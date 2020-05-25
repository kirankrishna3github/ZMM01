class ZCL_IM_6MM019B_GR_BUYER_ML definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6MM019B_GR_BUYER_ML
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
*"* protected components of class ZCL_IM_6MM019B_GR_BUYER_ML
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6MM019B_GR_BUYER_ML
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6MM019B_GR_BUYER_ML IMPLEMENTATION.


METHOD if_ex_mb_document_badi~mb_document_before_update.
*  break ibmabap01.

  DATA : imseg_wa     TYPE mseg,
         ekko_wa      TYPE ekko,
         t024_wa      TYPE t024,
*         lfa1_wa type lfa1,
         idata_wa     TYPE mseg,
         makt_wa      TYPE makt,
         g_objcont_wa TYPE soli.

  DATA : wa_vendor    TYPE mseg-lifnr,
         wa_index     TYPE sy-index,
         wa_name1(35) TYPE c.

  DATA : idata     LIKE STANDARD TABLE OF idata_wa,
         imseg     LIKE STANDARD TABLE OF imseg_wa,
         g_objcont LIKE STANDARD TABLE OF g_objcont_wa.

  DATA: wa_mseg LIKE LINE OF xmseg.

  DATA: wa_packing_list TYPE sopcklsti1,
        wa_contents_txt TYPE solisti1,
        wa_reclist      TYPE somlreci1.
*-- VARIABLES USED FOR SENDING MAIL
  DATA: gv_smtp_addr  TYPE adr6-smtp_addr,
        sender        TYPE soextreci1-receiver,
        send_adr_type TYPE so_adr_typ.

  DATA: gv_count   TYPE sy-index,
        gv_line_no TYPE sy-index.

*-- MAIL RELATED INTERNAL TABLES
  DATA: wa_doc_chng     TYPE sodocchgi1,                   " DOCUMENT ATTRIBUTES
        it_packing_list TYPE STANDARD TABLE OF sopcklsti1, " ATTACHMENT TABLE
        it_contents_txt TYPE STANDARD TABLE OF solisti1,   " OBJECT TEXT
        it_reclist      TYPE STANDARD TABLE OF somlreci1.  " MAIL RECIPIENTS

  DATA : addr_usr     TYPE v_addr_usr,
         wa_smtp_addr TYPE adr6-smtp_addr.

  DATA: it_email TYPE TABLE OF zfi044_emailid,
        wa_email LIKE LINE OF it_email.

* Data Declarations
  DATA: lt_mailsubject     TYPE sodocchgi1.
  DATA: lt_mailrecipients TYPE STANDARD TABLE OF somlrec90,
        wa_mailrecipients LIKE LINE OF lt_mailrecipients.

  DATA: lt_mailtxt TYPE STANDARD TABLE OF soli,
        wa_mailtxt LIKE LINE OF lt_mailtxt.



*  if sy-tcode = 'MIGO'.
  CASE sy-ucomm.
    WHEN 'OK_POST' OR 'OK_POST1'.
      IF sy-tcode = 'MIGO'.
*        loop at xmseg into mseg_wa where BWART = '101'.
*          if not mseg_wa-ebeln is initial.
*            idata_wa-MBLNR = mseg_wa-MBLNR.
*            idata_wa-Mjahr = mseg_wa-Mjahr.
*            idata_wa-BWART = mseg_wa-BWART.
*            idata_wa-MENGE = mseg_wa-MENGE.
*            idata_wa-MEINS = mseg_wa-MEINS.
*            idata_wa-EBELN = mseg_wa-EBELN.
*            collect  idata_wa into idata.
*          endif.
*        endloop.


        idata = xmseg.
        imseg = xmseg.

        SORT idata BY mblnr mjahr .
        SORT imseg BY mblnr mjahr .
        DELETE ADJACENT DUPLICATES FROM idata COMPARING mblnr mjahr.
        DELETE idata WHERE ebeln = ''.

        LOOP AT idata INTO idata_wa WHERE bwart = '101'.
          CLEAR : ekko_wa,t024_wa.
          SELECT SINGLE * FROM ekko INTO ekko_wa
            WHERE ebeln = idata_wa-ebeln.

          SELECT SINGLE * FROM t024 INTO t024_wa
            WHERE ekgrp = ekko_wa-ekgrp.

*   ---get sender email adress

          CLEAR addr_usr.
          CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
            EXPORTING
              bname      = sy-uname
              mandt      = sy-mandt
              no_display = 'X'
            IMPORTING
*             OKCODE     =
              addr_usr   = addr_usr
*   CHANGING
*             USER_USR03 =
            .
        select single smtp_addr into wa_smtp_addr from adr6
        where addrnumber = addr_usr-addrnumber
                                       AND persnumber   = addr_usr-persnumber.
          sender  = wa_smtp_addr.
          send_adr_type = 'SMTP'.
          CLEAR: wa_reclist, it_reclist.

*-- POPULATE MAIL ID'S
          gv_smtp_addr = t024_wa-smtp_addr.

          wa_reclist-rec_type = 'U' ."INTERNET ADDRESS
          wa_reclist-express  = 'X' ."VALUE FOR ACTIVATED
          wa_reclist-receiver = gv_smtp_addr.
*    IF IZ6MMA_PARAMS_WA-PARAM1 = 'CC'.
*      WA_RECLIST-COPY = 'X'.
*    else.
*      WA_RECLIST-COPY = ''.
*    ENDIF.
          APPEND wa_reclist TO it_reclist.

          CLEAR: gv_smtp_addr, wa_reclist,g_objcont.
*---*-- MAIL SUBJECT LINE
          CLEAR: wa_doc_chng.

          IF NOT ekko_wa-lifnr IS INITIAL.
            SELECT SINGLE name1 FROM lfa1 INTO wa_name1
              WHERE lifnr = ekko_wa-lifnr.
          ELSE.
            SELECT SINGLE name1 FROM t001w INTO wa_name1
              WHERE werks = ekko_wa-reswk.
          ENDIF.

          wa_doc_chng-obj_name = 'Goods Receipt'.
*  WA_DOC_CHNG-OBJ_DESCR = L_OBJECT_HD_CHANGE-OBJDES.
          CONCATENATE 'Receipt from Vendor' wa_name1 INTO wa_doc_chng-obj_descr
          SEPARATED BY space.
          CLEAR: gv_line_no, gv_count.

*  ---mail body
          CLEAR : g_objcont_wa ,g_objcont .
          CONCATENATE 'PO No:  ' idata_wa-ebeln INTO g_objcont_wa SEPARATED BY space.
          APPEND g_objcont_wa TO g_objcont .
          CLEAR  g_objcont_wa.
          APPEND g_objcont_wa TO g_objcont .

          CONCATENATE 'GR No :' idata_wa-mblnr INTO g_objcont_wa SEPARATED BY space  .
          APPEND g_objcont_wa TO g_objcont .

          CLEAR  g_objcont_wa.
          APPEND g_objcont_wa TO g_objcont .

          CLEAR imseg_wa.
          READ TABLE imseg INTO imseg_wa WITH KEY mblnr = idata_wa-mblnr
                                                  mjahr = idata_wa-mjahr
                                                  BINARY SEARCH.
          IF sy-subrc = 0.
            wa_index = sy-tabix.
            LOOP AT imseg INTO imseg_wa FROM wa_index.
              IF ( imseg_wa-mblnr <> idata_wa-mblnr )
                AND ( imseg_wa-mjahr <> idata_wa-mjahr ).
                EXIT.
              ENDIF.

              CLEAR makt_wa.
              SELECT SINGLE * FROM makt INTO makt_wa
*{   REPLACE        SBXK900029                                        3
*\                WHERE matnr = imseg_wa-matnr.
                WHERE matnr = imseg_wa-matnr and spras eq 'E'.
*}   REPLACE

              g_objcont_wa+0(20) = imseg_wa-matnr.
              g_objcont_wa+20(40) = makt_wa-maktx.
              g_objcont_wa+60(15) = imseg_wa-menge.
              g_objcont_wa+75(5) = imseg_wa-meins.

              APPEND g_objcont_wa TO g_objcont .
              CLEAR  g_objcont_wa.
            ENDLOOP.
          ENDIF.

*----
          DESCRIBE TABLE g_objcont LINES gv_count.
          gv_line_no = gv_count.
          READ TABLE g_objcont INTO g_objcont_wa INDEX gv_count.
          wa_doc_chng-doc_size = ( gv_count - 1 ) * 255 + strlen( g_objcont_wa ).

*-- POPULATE PACKING LIST FOR BODY TEXT
          wa_packing_list-head_start  = 1.
          wa_packing_list-head_num    = 0.
          wa_packing_list-body_start  = 1.
          wa_packing_list-body_num    = gv_count."V_TABLE_LINES.
          wa_packing_list-doc_type    = 'RAW'.
          APPEND wa_packing_list TO it_packing_list.
          CLEAR wa_packing_list.

          DELETE it_reclist WHERE receiver  = ''.

          IF NOT it_reclist IS INITIAL.
            CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
              EXPORTING
                document_data              = wa_doc_chng
                put_in_outbox              = 'X'
                sender_address             = sender
                sender_address_type        = send_adr_type
*                commit_work                = 'X'
              TABLES
                packing_list               = it_packing_list
                contents_txt               = g_objcont
                receivers                  = it_reclist
              EXCEPTIONS
                too_many_receivers         = 1
                document_not_sent          = 2
                document_type_not_exist    = 3
                operation_no_authorization = 4
                parameter_error            = 5
                x_error                    = 6
                enqueue_error              = 7
                OTHERS                     = 8.

            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
**************        send mail on 344 transfer posting
      IF  sy-tcode = 'MIGO' OR sy-tcode = 'MIGO_TR'.
        READ TABLE xmseg INTO wa_mseg WITH KEY bwart = '344' shkzg = 'S' lgort = '4501'.
        IF sy-subrc = 0.
          SELECT * FROM zfi044_emailid INTO TABLE it_email
            WHERE egroup = 'MIGO_344'.
          LOOP AT it_email INTO wa_email.


            IF wa_email-email IS NOT INITIAL.

              CONDENSE wa_email-email.

              CLEAR: lt_mailrecipients, wa_mailrecipients , wa_mailtxt , lt_mailtxt , lt_mailsubject.
              wa_mailrecipients-rec_type  = 'U'.
              wa_mailrecipients-receiver = wa_email-email."'pshinde-icc@modi.com'.
              APPEND wa_mailrecipients TO lt_mailrecipients .
              CLEAR wa_mailrecipients.

              CLEAR: lt_mailsubject-obj_descr.
              lt_mailsubject-obj_name = 'MIGO_TR'.
              lt_mailsubject-obj_langu = sy-langu.

*{   REPLACE        SBXK900029                                        2
*\              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "#EC CI_FLDEXT_OK[2215424]
*}   REPLACE
            exporting
                  input  = wa_mseg-matnr
                IMPORTING
                  output = wa_mseg-matnr.

*{   REPLACE        SBXK900029                                        1
*\              CONCATENATE 'SHORT RECEIPT OF MATERIAL:' wa_mseg-matnr 'in Plant:' wa_mseg-werks
              CONCATENATE 'SHORT RECEIPT OF MATERIAL:' wa_mseg-matnr 'in Plant:' wa_mseg-werks  "#EC CI_FLDEXT_OK[2215424]
*}   REPLACE
            into lt_mailsubject-obj_descr separated by space.


*Material Document no/yyyy has been created in comp <1000> against 'short receipt'
*of Material <code> in the plant <xxxx> wide, batch <no>  (from stor loc 1501 to 4501)

              CONCATENATE 'Material Document/Year:' wa_mseg-mblnr '/' wa_mseg-mjahr 'has been created in Comp:' wa_mseg-bukrs
              'against ''Short receipt'' of Material:' wa_mseg-matnr ',in plant:' wa_mseg-werks
              'wide, Batch:' wa_mseg-umcha
              '( From Stor.Loc.:' wa_mseg-umlgo 'To' wa_mseg-lgort ')'
              INTO wa_mailtxt SEPARATED BY space .
*                  LT_MAILTXT = 'Outbound Created .
              APPEND wa_mailtxt TO lt_mailtxt. CLEAR wa_mailtxt.



*********** Send Mail
              CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
                EXPORTING
                  document_data              = lt_mailsubject
*                 COMMIT_WORK                = 'X'
                TABLES
                  object_content             = lt_mailtxt
                  receivers                  = lt_mailrecipients
                EXCEPTIONS
                  too_many_receivers         = 1
                  document_not_sent          = 2
                  document_type_not_exist    = 3
                  operation_no_authorization = 4
                  parameter_error            = 5
                  x_error                    = 6
                  enqueue_error              = 7
                  OTHERS                     = 8.
              IF sy-subrc EQ 0.
*                    COMMIT WORK.
*   Push mail out from SAP outbox
*                    SUBMIT RSCONN01 WITH MODE = 'INT' AND RETURN.
              ENDIF.

            ENDIF.


          ENDLOOP.
        ENDIF.
      ENDIF.
  ENDCASE.
*  endif.

*if sy-tcode = 'MIGO'.
*  READ TABLE XMSEG INTO wa_mseg WITH KEY BWART = '101' ."shkzg = 'S' LGORT = '4501'.
*           IF sy-subrc = 0.
*             MESSAGE 'ERROR' TYPE 'E'.
*           ENDIF.
*ENDIF.

ENDMETHOD.


method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.
*  break ibmabap01.
endmethod.
ENDCLASS.
