*----------------------------------------------------------------------*
***INCLUDE ZXVVFF01 .
*&********************************************************************************************&*
*& OBJECT NAME          : ZXVVFF01                                                            &*
*  DEVELOPER            : PRADEEP KODINAGULA                                                  &*
*& MODULE NAME          : SD                                                                  &*
*& PROGRAM TYPE         : INCLUDE                                                             &*
*& CREATE DATE          : 07/07/2016                                                          &*
*& TRANSPORT NO         :                                                                     &*
* REVISION HISTORY----------------------------------------------------------------------------*
*                                                                                             *
*   CHANGED BY:                                                                               *
*   CHANGE ON:                                                                                *
*   REASON FOR CHANGE:                                                                        *
*                                                                                             *
* REVISION HISTORY----------------------------------------------------------------------------*
*{   INSERT         SBXK900028                                        1
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Sunday, October 07, 2018 23:47:54
* Changed By - 6010859 - SaurabhK
* Purpose    - Simplification list - 2267308 - konv replaced by prcd_elements
* Solution   - konv replaced by prcd_elements
* TR         - SBXK900028       6010859      S4H: S_K: Simplification List: 03.10.2018
*--------------------------------------------------------------------*
*}   INSERT


FORM spcd_email USING tvbrp TYPE ANY TABLE
                      tkomv TYPE ANY TABLE
                      wvbrk TYPE any.

*&*******************************************************************************************&
*&                             DATA DECLARATIONSS                                            &
*&*******************************************************************************************&
  DATA : BEGIN OF wa_vbrk,
           vbeln TYPE vbrk-vbeln,
           kunrg TYPE vbrk-kunrg,
           kunag TYPE vbrk-kunag,
           kalsm TYPE vbrk-kalsm,
           fkdat TYPE vbrk-fkdat,
           netwr TYPE vbrk-netwr,
           waerk TYPE vbrk-waerk,
           zterm TYPE vbrk-zterm,
           vkorg TYPE vbrk-vkorg,
           vtweg TYPE vbrk-vtweg,
           spart TYPE vbrk-spart,
           knumv TYPE vbrk-knumv,
           mwsbk TYPE vbrk-mwsbk,
         END OF wa_vbrk.

  DATA : BEGIN OF wa_final,
           matnr     TYPE vbrp-matnr,
           arktx     TYPE vbrp-arktx,
           charg     TYPE vbrp-charg,
           zfimg(18),
*       VRKME TYPE vbrp-VRKME,
         END OF wa_final.

  DATA : BEGIN OF wa_final_ibj,
           matnr     TYPE vbrp-matnr,
           arktx     TYPE vbrp-arktx,
*       CHARG TYPE VBRP-CHARG,
           vrkme     TYPE vbrp-vrkme,
           zfimg(18),

         END OF wa_final_ibj.


*DATA : BEGIN OF WA_KONV,
*{   REPLACE        SBXK900028                                        5
*\*       KSCHL TYPE KONV-KSCHL,
*\*       KWERT TYPE KONV-KWERT,
*       KSCHL TYPE PRCD_ELEMENTS-KSCHL,
*       KWERT TYPE PRCD_ELEMENTS-KWERT,
*}   REPLACE
*       END OF WA_KONV.

  DATA : BEGIN OF wa_pa0105 ,
           pernr      TYPE pa0105-pernr,
           usrid_long TYPE pa0105-usrid_long,
         END OF wa_pa0105.

  DATA : BEGIN OF wa_adr6,
           addrnumber TYPE adr6-addrnumber,
           smtp_addr  TYPE adr6-smtp_addr,
         END OF wa_adr6.

  DATA : BEGIN OF wa_exc,
*{   REPLACE        SBXK900270                                        6
*\         J_1ICSTNO TYPE J_1IMOCUST-J_1ICSTNO,
*\         J_1ILSTNO TYPE J_1IMOCUST-J_1ILSTNO,
*---------------------------------------------------------------------*
*--------------------------- << S/4HANA >> ---------------------------*
*---------------------------------------------------------------------*
* Changed On - Wensday, November 14, 2018
* Changed By - 10106 - Bhushan Mehta
* Purpose    - Table Replacement
* Solution   - Replace Table KAN1 from J_1IPANNO
* TR         - SBXK900270 - BM:Replace Table Excise to General Master Data:14.11.2018
*--------------------------------------------------------------------*
           j_1icstno TYPE kna1-j_1icstno,
           j_1ilstno TYPE kna1-j_1ilstno,
*}   REPLACE
         END OF wa_exc.

  DATA : wa_exc1      LIKE wa_exc,
         it_adr6      LIKE TABLE OF wa_adr6,
         it_pa0105    LIKE TABLE OF wa_pa0105,
         it_konv      LIKE TABLE OF komv,
         wa_konv      LIKE LINE OF it_konv,
         it_final     LIKE TABLE OF wa_final,
         it_final_ibj LIKE TABLE OF wa_final_ibj,
         it_vbrp      LIKE TABLE OF vbrp,
         wa_vbrp      LIKE LINE OF it_vbrp.

  DATA : it_fcat TYPE lvc_t_fcat,
         wa_fcat LIKE LINE OF it_fcat. " FIELDCATALOG

*{   REPLACE        SBXK900028                                        4
*\  DATA : S_JEXP TYPE KONV-KWERT, S_JCEP TYPE KONV-KWERT, S_ZCEP TYPE KONV-KWERT,
*\         S_ZACD TYPE KONV-KWERT, S_JCST TYPE KONV-KWERT, S_UTX1 TYPE KONV-KWERT.
  DATA : s_jexp TYPE prcd_elements-kwert, s_jcep TYPE prcd_elements-kwert, s_zcep TYPE prcd_elements-kwert,
         s_zacd TYPE prcd_elements-kwert, s_jcst TYPE prcd_elements-kwert, s_utx1 TYPE prcd_elements-kwert.
*}   REPLACE

  DATA : cust_name  TYPE kna1-name1,
         plant      TYPE vbrp-werks,
         cplant     TYPE kna1-kunnr,
         lv_vgbel   TYPE vbrp-vgbel,
         exnum      TYPE  j_1iexcnum,
         wrk_netwr  TYPE char17,
         wrk_fkdat  TYPE char10,
         zlfimg(18).

  DATA : t_header   TYPE STANDARD TABLE OF w3head,   "HEADER
         t_fields   TYPE STANDARD TABLE OF w3fields, "FIELDS
         t_html1    TYPE STANDARD TABLE OF w3html,
         s_html1    LIKE LINE OF t_html1,             "HTML
         wa_header  TYPE w3head,
         w_head     TYPE w3head,
         table_attr TYPE w3html.

*&*********************************************************************&*
*&                DATA DECLARATION FOR EMAIL                           &*
*&*********************************************************************&*

* CL_BCS CLASS FOR SENDING MAIL
  DATA: send_request TYPE REF TO cl_bcs,
        sender       TYPE REF TO if_sender_bcs,
        receiver     TYPE REF TO if_recipient_bcs,
        document     TYPE REF TO cl_document_bcs,
        t_html       TYPE bcsy_text,
        sent_to_all  TYPE os_boolean,
        email_sts    TYPE string.

* DATA DECLARATIONS FOR ERROR HANDLING.
  DATA : bcs_exception  TYPE  REF TO cx_bcs,
         send_exception TYPE  REF TO cx_bcs,
         addr_exception TYPE  REF TO cx_bcs.


  DATA : fm_name  TYPE rs38l_fnam,
         l_attach TYPE TABLE OF solix WITH HEADER LINE.







*&*******************************************************************************************&
*&                             FETCH AND PROCESS DATA                                        &
*&*******************************************************************************************&

*SELECT SINGLE VBELN KUNAG KUNRG KALSM FKDAT NETWR WAERK ZTERM
*VKORG VTWEG SPART KNUMV FROM VBRK  INTO WA_VBRK  WHERE VBELN = VBELN.

  MOVE-CORRESPONDING wvbrk TO wa_vbrk.

  IF wa_vbrk IS NOT INITIAL.

    it_vbrp[] = tvbrp[].

*SELECT * FROM VBRP INTO TABLE IT_VBRP WHERE  VBELN = VBELN.

    IF wa_vbrk-kalsm = 'ZDOMPR' .

*  SELECT SINGLE VGBEL FROM VBRP INTO LV_VGBEL
*  WHERE VBELN = WA_VBRK-VBELN.

      READ TABLE it_vbrp INTO wa_vbrp INDEX 2.
      IF sy-subrc = 0.
        lv_vgbel = wa_vbrp-vgbel.

        SELECT SINGLE depexnum FROM j_1irg23d INTO exnum
        WHERE  vbeln   = lv_vgbel.

        CLEAR plant.
        plant = wa_vbrp-werks.

        CLEAR wa_vbrp.
      ENDIF.

    ELSE.

      SELECT SINGLE exnum FROM j_1iexchdr INTO exnum
                              WHERE  trntyp = 'DLFC'
                              AND    rdoc   = wa_vbrk-vbeln
                              AND    rind   = 'N'
                              AND    status = 'C'.
    ENDIF.


*{   REPLACE        SBXK900028                                        3
*\*SELECT KSCHL KWERT FROM KONV INTO TABLE IT_KONV WHERE KNUMV = WA_VBRK-KNUMV.
*SELECT KSCHL KWERT FROM prcd_elements INTO TABLE IT_KONV WHERE KNUMV = WA_VBRK-KNUMV.
*}   REPLACE

    it_konv[] = tkomv[].

    IF it_konv IS NOT INITIAL.

      CLEAR : s_jexp , s_jcep, s_zcep, s_zacd ,s_jcst, s_utx1.

      LOOP AT it_konv INTO wa_konv.

        IF wa_konv-kschl = 'JEXP'.

          s_jexp = s_jexp + wa_konv-kwert.

        ELSEIF wa_konv-kschl = 'JCEP'.

          s_jcep = s_jcep + wa_konv-kwert.

        ELSEIF wa_konv-kschl = 'ZCEP'.

          s_zcep = s_zcep + wa_konv-kwert.

        ELSEIF wa_konv-kschl = 'ZACD'.

          s_zacd = s_zacd + wa_konv-kwert.

        ELSEIF wa_konv-kschl = 'JCST'.

          s_jcst = s_jcst + wa_konv-kwert.

        ELSEIF wa_konv-kschl = 'UTX1'.

          s_utx1 = s_utx1 + wa_konv-kwert.

        ENDIF.

        CLEAR wa_konv.

      ENDLOOP.

    ENDIF.
    BREAK 10106.

      SELECT pernr usrid_long INTO TABLE it_pa0105 FROM pa0105
      WHERE pernr IN ( SELECT pernr FROM knvp WHERE kunnr = wa_vbrk-kunrg AND parvw = 'ZM' )
      AND endda >= sy-datum ."= '99991231'. changed by varun on 15.07.2019 for Ignoring seperated employees

    SELECT addrnumber smtp_addr INTO TABLE it_adr6 FROM adr6
    WHERE addrnumber IN ( SELECT adrnr FROM kna1 WHERE kunnr = wa_vbrk-kunag ).

    CLEAR: cust_name.
    SELECT SINGLE name1 FROM kna1 INTO cust_name WHERE kunnr = wa_vbrk-kunag.

    CLEAR wa_exc.
*{   REPLACE        SBXK900270                                        7
*\    SELECT SINGLE J_1ICSTNO J_1ILSTNO FROM J_1IMOCUST INTO WA_EXC WHERE KUNNR = WA_VBRK-KUNAG.
    SELECT SINGLE j_1icstno
                  j_1ilstno
                  FROM kna1
                  INTO wa_exc
                 WHERE kunnr EQ wa_vbrk-kunag.
*}   REPLACE

    CLEAR cplant.
*SELECT SINGLE WERKS FROM ZSD_VBRKVBRP_MIS INTO PLANT WHERE VBELN = WA_VBRK-VBELN.

    CONCATENATE 'C' plant INTO cplant.


    CLEAR wa_exc1.
*{   REPLACE        SBXK900270                                        8
*\    SELECT SINGLE J_1ICSTNO J_1ILSTNO FROM J_1IMOCUST INTO WA_EXC1 WHERE KUNNR = CPLANT.
    SELECT SINGLE j_1icstno j_1ilstno
             FROM kna1
             INTO wa_exc1
            WHERE kunnr EQ cplant.
*}   REPLACE

    IF wa_vbrk-vkorg = '3000' AND wa_vbrk-vtweg = '10' AND  wa_vbrk-spart = '40'.


      READ TABLE it_vbrp INTO wa_vbrp INDEX 1.
      IF sy-subrc = 0.
        CLEAR plant.
        plant = wa_vbrp-werks.
        CLEAR wa_vbrp.
      ENDIF.


      CLEAR wa_exc1.

      SELECT SINGLE j_1icstno j_1ilstno FROM j_1imocomp INTO wa_exc1 WHERE werks = plant.

    ENDIF.

    CLEAR wrk_netwr.
    wrk_netwr = wa_vbrk-netwr + wa_vbrk-mwsbk.


    IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' OR wa_vbrk-vkorg = '3000' )
      AND wa_vbrk-vtweg = '10'
      AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' OR wa_vbrk-spart = '40').
      " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19

      LOOP AT it_vbrp INTO wa_vbrp WHERE fkimg <> 0.

        CLEAR: zlfimg.
        zlfimg =  wa_vbrp-fkimg.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_vbrp-matnr
          IMPORTING
            output = wa_vbrp-matnr.
        IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND wa_vbrk-vtweg = '10' AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ) . " for SPCD
          " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19

          wa_final-matnr  =  wa_vbrp-matnr.
          wa_final-arktx  =  wa_vbrp-arktx.
          wa_final-charg  =  wa_vbrp-charg.
          wa_final-zfimg  =  zlfimg.
*     WA_FINAL-VRKME  =  WA_VBRP-VRKME.

          APPEND wa_final TO it_final.
          CLEAR wa_final.

        ENDIF.
        IF wa_vbrk-vkorg = '3000' AND wa_vbrk-vtweg = '10' AND wa_vbrk-spart = '40' . " for SPCD

          wa_final_ibj-matnr  =  wa_vbrp-matnr.
          wa_final_ibj-arktx  =  wa_vbrp-arktx.
*     WA_FINAL-CHARG  =  WA_VBRP-CHARG.
          wa_final_ibj-zfimg  =  zlfimg.
          wa_final_ibj-vrkme  =  wa_vbrp-vrkme.

          APPEND wa_final_ibj TO it_final_ibj.
          CLEAR wa_final_ibj.

        ENDIF.




      ENDLOOP.

*********POPULATE FIELDCATALOG*********************************

      wa_fcat-coltext = 'Material Number'.
      APPEND wa_fcat TO it_fcat.
      wa_fcat-coltext = 'Material Description'.
      APPEND wa_fcat TO it_fcat.
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND wa_vbrk-vtweg = '10' AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ) . " for SPCD
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        wa_fcat-coltext = 'Batch '.
        APPEND wa_fcat TO it_fcat.
      ENDIF.
      IF wa_vbrk-vkorg = '3000' AND wa_vbrk-vtweg = '10' AND wa_vbrk-spart = '40' . " for IBJ
        wa_fcat-coltext = 'Uom'.
        APPEND wa_fcat TO it_fcat.
      ENDIF.

      wa_fcat-coltext = 'Qunatity'.
      APPEND wa_fcat TO it_fcat.

*******FILL THE COLUMN HEADINGS AND PROPERTIES*******

      LOOP AT it_fcat INTO wa_fcat.

        w_head-text = wa_fcat-coltext.

        CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'    "POPULATE THE COLUMN HEADINGS
          EXPORTING
            field_nr = sy-tabix
            text     = w_head-text
            fgcolor  = 'BLACK'
            bgcolor  = '#339999'
            size     = '1'
            font     = 'LUCIDA GRANDE'
          TABLES
            header   = t_header.

        CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'    "POPULATE COLUMN PROPERTIES
          EXPORTING
            field_nr  = sy-tabix
            fgcolor   = 'BLACK'
            justified = 'CENTER'
            size      = '1'
            font      = 'LUCIDA GRANDE'
          TABLES
            fields    = t_fields.

      ENDLOOP.

      table_attr = 'WIDTH=90% BORDER=0 CELLSPACING=5  CELLPADDING=0 ALIGN=CENTER BGCOLOR=#C6D2DB' .
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND wa_vbrk-vtweg = '10' AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ) . " for SPCD
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        CALL FUNCTION 'WWW_ITAB_TO_HTML'
          EXPORTING
            table_header     = wa_header
            table_attributes = table_attr
          TABLES
            html             = t_html1
            fields           = t_fields
            row_header       = t_header
            itable           = it_final.
      ENDIF.

      IF wa_vbrk-vkorg = '3000' AND wa_vbrk-vtweg = '10' AND wa_vbrk-spart = '40' . " for IBJ
        CALL FUNCTION 'WWW_ITAB_TO_HTML'
          EXPORTING
            table_header     = wa_header
            table_attributes = table_attr
          TABLES
            html             = t_html1
            fields           = t_fields
            row_header       = t_header
            itable           = it_final_ibj.

      ENDIF.

**************CREATE HTML BODY ************

      APPEND TEXT-401 TO t_html.
      APPEND TEXT-402 TO t_html.
      APPEND TEXT-403 TO t_html.
      APPEND TEXT-404 TO t_html.
      APPEND TEXT-405 TO t_html.
      APPEND TEXT-406 TO t_html.
      APPEND TEXT-407 TO t_html.
      APPEND TEXT-408 TO t_html.
      APPEND TEXT-409 TO t_html.
      APPEND TEXT-410 TO t_html.
      APPEND TEXT-411 TO t_html.
      APPEND TEXT-412 TO t_html.
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-413 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-626 TO t_html.
      ENDIF.
      APPEND TEXT-414 TO t_html.
      APPEND TEXT-415 TO t_html.
      APPEND TEXT-416 TO t_html.
      APPEND TEXT-417 TO t_html.
      APPEND TEXT-418 TO t_html.
      APPEND TEXT-419 TO t_html.
      APPEND TEXT-420 TO t_html.

      IF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-619 TO t_html.
      ELSEIF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-421 TO t_html.
      ENDIF.
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-422 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-625 TO t_html.
      ENDIF.

      APPEND TEXT-423 TO t_html.
      APPEND TEXT-424 TO t_html.
      APPEND TEXT-425 TO t_html.
      APPEND TEXT-426 TO t_html.
      APPEND TEXT-427 TO t_html.
      APPEND TEXT-428 TO t_html.
      APPEND TEXT-429 TO t_html.

      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-430 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-627 TO t_html.
      ENDIF.


      APPEND TEXT-431 TO t_html.
      APPEND TEXT-432 TO t_html.
      APPEND cust_name TO t_html.
      APPEND TEXT-433 TO t_html.
      APPEND TEXT-434 TO t_html.
      APPEND TEXT-435 TO t_html.
      APPEND TEXT-436 TO t_html.
      APPEND TEXT-437 TO t_html.
      APPEND TEXT-438 TO t_html.
      APPEND TEXT-439 TO t_html.

      APPEND TEXT-552 TO t_html.
      APPEND TEXT-553 TO t_html.
      APPEND TEXT-554 TO t_html.
      APPEND TEXT-555 TO t_html.
      APPEND TEXT-556 TO t_html.
      APPEND TEXT-557 TO t_html.
      APPEND wa_vbrk-kunag TO t_html.
      APPEND TEXT-559 TO t_html.
      APPEND TEXT-560 TO t_html.
      APPEND TEXT-561 TO t_html.
      APPEND TEXT-562 TO t_html.
      APPEND cust_name TO t_html.
      APPEND TEXT-564 TO t_html.

      APPEND TEXT-591 TO t_html.
      APPEND TEXT-592 TO t_html.
      APPEND TEXT-593 TO t_html.
      APPEND TEXT-594 TO t_html.
      CONDENSE wa_exc-j_1icstno.
      APPEND wa_exc-j_1icstno TO t_html.
      APPEND TEXT-596 TO t_html.
      APPEND TEXT-597 TO t_html.
      APPEND TEXT-598 TO t_html.
      APPEND TEXT-599 TO t_html.
      CONDENSE wa_exc-j_1ilstno.
      APPEND wa_exc-j_1ilstno TO t_html.
      APPEND TEXT-601 TO t_html.
      APPEND TEXT-605 TO t_html.
      APPEND TEXT-606 TO t_html.

      APPEND TEXT-565 TO t_html.
      APPEND TEXT-566 TO t_html.
      APPEND TEXT-567 TO t_html.
      APPEND TEXT-568 TO t_html.
      APPEND TEXT-569 TO t_html.
      APPEND TEXT-570 TO t_html.
*{   REPLACE        IRQK900193                                        1
*\      SHIFT WA_VBRK-VBELN LEFT DELETING LEADING 0.
      SHIFT wa_vbrk-vbeln LEFT DELETING LEADING '0'.
*}   REPLACE
      APPEND wa_vbrk-vbeln TO t_html.
      APPEND TEXT-572 TO t_html.
      APPEND TEXT-573 TO t_html.
      APPEND TEXT-574 TO t_html.
      APPEND TEXT-575 TO t_html.
      CONCATENATE wa_vbrk-fkdat+6(2)  '.'  wa_vbrk-fkdat+4(2)  '.'  wa_vbrk-fkdat+0(4)  INTO wrk_fkdat.
      APPEND wrk_fkdat TO t_html.
      APPEND TEXT-577 TO t_html.
      APPEND TEXT-578 TO t_html.
      APPEND TEXT-579 TO t_html.
      APPEND TEXT-580 TO t_html.
      IF exnum IS NOT INITIAL.
        APPEND TEXT-590 TO t_html.
      ENDIF.
      APPEND TEXT-581 TO t_html.
      APPEND TEXT-582 TO t_html.
*{   REPLACE        IRQK900193                                        2
*\      SHIFT EXNUM LEFT DELETING LEADING 0.
      SHIFT exnum LEFT DELETING LEADING '0'.
*}   REPLACE
      APPEND exnum TO t_html.
      APPEND TEXT-584 TO t_html.
      APPEND TEXT-585 TO t_html.
      APPEND TEXT-586 TO t_html.
      APPEND TEXT-587 TO t_html.
      APPEND TEXT-588 TO t_html.
      APPEND TEXT-589 TO t_html.
      APPEND TEXT-607 TO t_html.
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-608 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-623 TO t_html.
      ENDIF.

      APPEND TEXT-609 TO t_html.
      APPEND TEXT-610 TO t_html.
      CONDENSE wa_exc1-j_1icstno.
      APPEND wa_exc1-j_1icstno TO t_html.
      APPEND TEXT-612 TO t_html.
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-613 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-624 TO t_html.
      ENDIF.
      APPEND TEXT-614 TO t_html.
      APPEND TEXT-615 TO t_html.
      CONDENSE wa_exc1-j_1ilstno.
      APPEND wa_exc1-j_1ilstno TO t_html.
      APPEND TEXT-617 TO t_html.

      LOOP AT t_html1 INTO s_html1.         " HTML1 - SALES DOCUMENT ITEM DETAILS IN HTML FORMAT
        APPEND s_html1 TO t_html.
      ENDLOOP.
      APPEND TEXT-479 TO t_html.
      APPEND TEXT-480 TO t_html.


      IF s_jexp NE 0.
        APPEND TEXT-481 TO t_html.
        APPEND TEXT-482 TO t_html.
        APPEND TEXT-483 TO t_html.
        APPEND TEXT-484 TO t_html.
        APPEND s_jexp TO t_html.
        CLEAR s_jexp.
      ENDIF.

      IF s_jcep NE 0.
        APPEND TEXT-486 TO t_html.
        APPEND TEXT-487 TO t_html.
        APPEND TEXT-488 TO t_html.
        APPEND TEXT-489 TO t_html.
        APPEND s_jcep TO t_html.
        CLEAR s_jcep.
      ENDIF.


      IF s_zcep NE 0.
        APPEND TEXT-491 TO t_html.
        APPEND TEXT-492 TO t_html.
        APPEND TEXT-493 TO t_html.
        APPEND TEXT-494 TO t_html.
        APPEND s_zcep TO t_html.
        CLEAR s_zcep.
      ENDIF.

      APPEND TEXT-496 TO t_html.

      IF s_zacd NE 0.
        APPEND TEXT-497 TO t_html.
        APPEND TEXT-498 TO t_html.
        APPEND TEXT-499 TO t_html.
        APPEND TEXT-500 TO t_html.
        APPEND s_zacd TO t_html.
        CLEAR s_zacd.
      ENDIF.

      IF s_jcst NE 0.
        APPEND TEXT-502 TO t_html.
        APPEND TEXT-503 TO t_html.
        APPEND TEXT-504 TO t_html.
        APPEND TEXT-505 TO t_html.
        APPEND s_jcst TO t_html.
        CLEAR s_jcst.
      ENDIF.

      IF s_utx1 NE 0.
        APPEND TEXT-507 TO t_html.
        APPEND TEXT-508 TO t_html.
        APPEND TEXT-509 TO t_html.
        APPEND TEXT-510 TO t_html.
        APPEND s_utx1 TO t_html.
        CLEAR s_utx1.
      ENDIF.
      APPEND TEXT-512 TO t_html.
      APPEND TEXT-513 TO t_html.
      APPEND TEXT-514 TO t_html.
      APPEND TEXT-515 TO t_html.
      APPEND TEXT-516 TO t_html.
      APPEND TEXT-517 TO t_html.
      APPEND TEXT-518 TO t_html.
      CONDENSE wrk_netwr.
      APPEND wrk_netwr TO t_html.
      APPEND TEXT-520 TO t_html.
      APPEND TEXT-521 TO t_html.
      APPEND TEXT-522 TO t_html.
      APPEND TEXT-523 TO t_html.
      IF wa_vbrk-vkorg = '1000' AND wa_vbrk-spart = '10'.
        APPEND TEXT-524 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-620 TO t_html.
      ENDIF.
      APPEND TEXT-525 TO t_html.
      APPEND TEXT-526 TO t_html.
      APPEND TEXT-527 TO t_html.
      APPEND TEXT-528 TO t_html.
      APPEND TEXT-529 TO t_html.
      APPEND TEXT-530 TO t_html.
      APPEND TEXT-531 TO t_html.
      APPEND TEXT-532 TO t_html.
      APPEND TEXT-533 TO t_html.
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-534 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-621 TO t_html.
      ENDIF.
      APPEND TEXT-535 TO t_html.

      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-602 TO t_html.
        APPEND TEXT-603 TO t_html.
        APPEND TEXT-604 TO t_html.
      ENDIF.

      APPEND TEXT-536 TO t_html.
      APPEND TEXT-537 TO t_html.
      APPEND TEXT-538 TO t_html.
      APPEND TEXT-539 TO t_html.
      APPEND TEXT-540 TO t_html.
      APPEND TEXT-541 TO t_html.
      APPEND TEXT-542 TO t_html.
      APPEND TEXT-543 TO t_html.
      APPEND TEXT-544 TO t_html.
      APPEND TEXT-545 TO t_html.
      APPEND TEXT-546 TO t_html.
      APPEND TEXT-547 TO t_html.
      IF ( wa_vbrk-vkorg = '1000' OR wa_vbrk-vkorg = '2800' ) AND ( wa_vbrk-spart = '20' OR wa_vbrk-spart = '28' ).
        " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
        APPEND TEXT-548 TO t_html.
        APPEND TEXT-549 TO t_html.
        APPEND TEXT-550 TO t_html.
      ELSEIF wa_vbrk-vkorg = '3000' AND wa_vbrk-spart = '40'.
        APPEND TEXT-622 TO t_html.
      ENDIF.



      APPEND TEXT-551 TO t_html.
      APPEND TEXT-552 TO t_html.
      APPEND TEXT-553 TO t_html.


*      TRY.
*
*          SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).
*
**---------------------------------------------------------------------------------------" CREATING DOCUMENT
*          DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
*          I_TYPE = 'HTM'                                     "SINCE IT IS AN HTML BODY
*          I_IMPORTANCE = '5'
*          I_TEXT = T_HTML[]
*          I_SUBJECT = 'Invoice Summary' ).
*
*
*
*          IF WA_VBRK-VKORG = '3000' AND WA_VBRK-SPART = '40'. " send PDF attachment of IBJ invoice in mail
*            DATA: ZVBRP LIKE TABLE OF VBRP ,  ZVBRK TYPE VBRK , ZKOMV LIKE TABLE OF KOMV.
**CVBRP[] CKOMV[] CVBRK
*
*
*            ZVBRP = IT_VBRP.
*            MOVE-CORRESPONDING WVBRK TO ZVBRK.
*            ZKOMV = IT_KONV.
*
*            EXPORT ZVBRP FROM ZVBRP TO MEMORY ID 'VBRP_IBJ'.
*            EXPORT ZVBRK FROM ZVBRK TO MEMORY ID 'VBRK_IBJ'.
*            EXPORT ZKOMV FROM ZKOMV TO MEMORY ID 'KOMV_IBJ'.
*
*
*            CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*              EXPORTING
*                FORMNAME           = 'ZSD_SF_INVOICE_TAX'
*              IMPORTING
*                FM_NAME            = FM_NAME
*              EXCEPTIONS
*                NO_FORM            = 1
*                NO_FUNCTION_MODULE = 2
*                OTHERS             = 3.
*            IF SY-SUBRC <> 0.
*              MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*            ENDIF.
*
*            DATA:ZVBELN TYPE VBRK-VBELN.
*
*            ZVBELN = WA_VBRK-VBELN.
**DATA: L_ATTACH        TYPE TABLE OF SOLIX WITH HEADER LINE.
**EXPORT zvbeln FROM zvbeln to MEMORY ID 'IBJINVOICE'.
*
*            SUBMIT ZSD_INVOICE_TAX_CP
*            WITH  P_VBELN = ZVBELN
*            EXPORTING LIST TO MEMORY
*                AND RETURN.
*
*            IF SY-SUBRC = 0.
*
*              IMPORT L_ATTACH TO L_ATTACH FROM MEMORY ID 'IBJINV'.
*
*              CALL METHOD DOCUMENT->ADD_ATTACHMENT               " ADDING ATTACHMENT
*                  EXPORTING I_ATTACHMENT_TYPE     = 'PDF'
*                            I_ATTACHMENT_SUBJECT  =  'IBJ INVOICE'
*                            I_ATT_CONTENT_HEX     = L_ATTACH[].
*
*
*            ENDIF.
*
*
*          ENDIF.
*
*
*
*
**---------------------------------------------------------------------------------------" ADDING ATTACHMENT
**    call method document->add_attachment
**     exporting i_attachment_type     = 'XLS'
**               i_attachment_subject  = subject.
**               i_att_content_hex     = l_attach[].
*
**---------------------------------------------------------------------------------------" SET DOCUMENT
*          CALL METHOD SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).
*
**---------------------------------------------------------------------------------------" SET SENDER
*
*          IF WA_VBRK-VKORG = '1000' AND WA_VBRK-SPART = '20'.
*            SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*                     I_ADDRESS_STRING = 'donotreply@modi.com'
*                     I_ADDRESS_NAME   = 'Indofil Industries' ).
*          ELSEIF WA_VBRK-VKORG = '3000' AND WA_VBRK-SPART = '40'.
*
*            SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*                     I_ADDRESS_STRING = 'donotreply@modi.com'
*                     I_ADDRESS_NAME   = 'Indobaijin Chemicals pvt ltd.' ).
*
*          ENDIF.
*
*          CALL METHOD SEND_REQUEST->SET_SENDER
*            EXPORTING
*              I_SENDER = SENDER.
*
**---------------------------------------------------------------------------------------" ADD RECIPIENTS
*          LOOP AT IT_ADR6 INTO WA_ADR6.                           " ( CUSTOMER EMAILS )
*
*            IF WA_ADR6-SMTP_ADDR IS NOT INITIAL.
*
*              IF SY-UNAME = '10106' OR SY-UNAME = 'AMS_L1' OR SY-UNAME = 'IBMSUPPORT'.
*                RECEIVER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'pshinde-ICC@MODI.COM').
*              ELSE.
*                RECEIVER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( WA_ADR6-SMTP_ADDR ).
*              ENDIF.
*
*              CALL METHOD SEND_REQUEST->ADD_RECIPIENT
*                EXPORTING
*                  I_RECIPIENT = RECEIVER
*                  I_EXPRESS   = 'X'.
*
*            ENDIF.
*
*            CLEAR WA_ADR6.
*          ENDLOOP.
*
*          LOOP AT IT_PA0105 INTO WA_PA0105.                     "( USER EMAILS )
*
*            IF WA_PA0105-USRID_LONG IS NOT INITIAL.
*
*              IF SY-UNAME = '10106' OR SY-UNAME = 'AMS_L1' OR SY-UNAME = 'IBMSUPPORT'.
*                RECEIVER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( 'pshinde-icc@modi.com').
*              ELSE.
*                RECEIVER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( WA_PA0105-USRID_LONG  ).
*              ENDIF.
*
*              CALL METHOD SEND_REQUEST->ADD_RECIPIENT
*                EXPORTING
*                  I_RECIPIENT = RECEIVER
*                  I_EXPRESS   = 'X'
*                  I_COPY      = 'X'.
*
*            ENDIF.
*
*            CLEAR WA_PA0105.
*          ENDLOOP.
*
*
**---------------------------------------------------------------------------------------" SET SEND IMMEDIATELY
*          CALL METHOD SEND_REQUEST->SET_SEND_IMMEDIATELY( 'X' ).
*
**---------------------------------------------------------------------------------------" SEND EMAIL
*
*          CALL METHOD SEND_REQUEST->SEND(
*               EXPORTING
*                I_WITH_ERROR_SCREEN = 'X'
*               RECEIVING
*                RESULT              = SENT_TO_ALL ).
*
*          IF SENT_TO_ALL = 'X'.
*            MESSAGE 'Email was sent successfully' TYPE 'S'.
*          ENDIF.
*          COMMIT WORK.
**---------------------------------------------------------------------------------------" EXCEPTION HANDLING
*        CATCH CX_DOCUMENT_BCS INTO BCS_EXCEPTION.
*          EMAIL_STS = BCS_EXCEPTION->GET_TEXT( ).
*
*        CATCH CX_SEND_REQ_BCS INTO SEND_EXCEPTION.
*          EMAIL_STS = SEND_EXCEPTION->GET_TEXT( ).
*
*        CATCH CX_ADDRESS_BCS INTO ADDR_EXCEPTION.
*          EMAIL_STS = ADDR_EXCEPTION->GET_TEXT( ).
*
*      ENDTRY.
*
*
*
    ENDIF.

  ENDIF.
**********************************************************************

ENDFORM.                    " SPCD_EMAIL
