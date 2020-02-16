class ZCL_IM_ME28_RELEASE_MAIL definition
  public
  final
  create public .

*"* public components of class ZCL_IM_ME28_RELEASE_MAIL
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_ME_PURCHDOC_POSTED .
protected section.
*"* protected components of class ZCL_IM_ME28_RELEASE_MAIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_ME28_RELEASE_MAIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_ME28_RELEASE_MAIL IMPLEMENTATION.


METHOD IF_EX_ME_PURCHDOC_POSTED~POSTED.
  DATA : ADDR_USR TYPE V_ADDR_USR,
         WA_SMTP_ADDR TYPE ADR6-SMTP_ADDR.
*-- VARIABLES USED FOR SENDING MAIL
  DATA: GV_SMTP_ADDR  TYPE ADR6-SMTP_ADDR,
          SENDER        TYPE SOEXTRECI1-RECEIVER,
          SEND_ADR_TYPE TYPE SO_ADR_TYP.
  DATA : LV_BNAME TYPE USR03-BNAME.


  DATA: WA_PACKING_LIST TYPE SOPCKLSTI1,
        WA_CONTENTS_TXT TYPE SOLISTI1,
        WA_RECLIST      TYPE SOMLRECI1.

*-- MAIL RELATED INTERNAL TABLES
  DATA: WA_DOC_CHNG     TYPE SODOCCHGI1,                   " DOCUMENT ATTRIBUTES
        IT_PACKING_LIST TYPE STANDARD TABLE OF SOPCKLSTI1, " ATTACHMENT TABLE
        IT_CONTENTS_TXT TYPE STANDARD TABLE OF SOLISTI1,   " OBJECT TEXT
        IT_RECLIST      TYPE STANDARD TABLE OF SOMLRECI1.  " MAIL RECIPIENTS

  DATA : G_OBJCONT_WA TYPE SOLI , G_OBJCONT LIKE STANDARD TABLE OF G_OBJCONT_WA.

  DATA: IT_EMAIL TYPE TABLE OF ZFI044_EMAILID ,
        WA_EMAIL LIKE LINE OF IT_EMAIL,
        IT_USER TYPE TABLE OF ZUSERGROUP,
        WA_USER LIKE LINE OF IT_USER  .

  DATA: GV_COUNT    TYPE SY-INDEX,
        GV_LINE_NO  TYPE SY-INDEX.
  DATA: FLAG.

  DATA: IT_T16FS TYPE TABLE OF T16FS,
        WA_T16FS LIKE LINE OF IT_T16FS.

  DATA: IT_MAIL TYPE TABLE OF Z6MMA_PARAMS,
        WA_MAIL LIKE LINE OF IT_MAIL.



  DATA: USERID_LONG TYPE PA0105-USRID_LONG.

  BREAK 10106.
  IF SY-TCODE = 'ME28' or sy-tcode = 'ME29N'. " IRDK932122
    IF IM_EKKO-PROCSTAT = '05'.

*************************** send mail on PO complete release to PO creator ************************************
      CLEAR ADDR_USR.
      CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
        EXPORTING
          BNAME            = SY-UNAME
         MANDT            = SY-MANDT
         NO_DISPLAY       = 'X'
       IMPORTING
*     OKCODE           =
         ADDR_USR         = ADDR_USR
*   CHANGING
*     USER_USR03       =
                .
      SELECT SINGLE SMTP_ADDR INTO WA_SMTP_ADDR FROM ADR6
                                   WHERE ADDRNUMBER = ADDR_USR-ADDRNUMBER
                                   AND PERSNUMBER   = ADDR_USR-PERSNUMBER.
      TRANSLATE WA_SMTP_ADDR TO LOWER CASE.
      SENDER  = WA_SMTP_ADDR.
      SEND_ADR_TYPE = 'SMTP'.
      CLEAR ADDR_USR.
      LV_BNAME = IM_EKKO-ERNAM.
      CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
        EXPORTING
          BNAME            = LV_BNAME
         MANDT            = SY-MANDT
         NO_DISPLAY       = 'X'
       IMPORTING
*     OKCODE           =
         ADDR_USR         = ADDR_USR
*   CHANGING
*     USER_USR03       =
                .
      CLEAR: WA_RECLIST, IT_RECLIST.
*-- POPULATE MAIL ID'S
*    SELECT SINGLE smtp_addr INTO gv_smtp_addr FROM adr6
*                                 WHERE addrnumber = addr_usr-addrnumber
*                                 AND persnumber   = addr_usr-persnumber.
      CLEAR: USERID_LONG.
      SELECT SINGLE USRID_LONG FROM PA0105 INTO USERID_LONG WHERE PERNR = IM_EKKO-ERNAM AND SUBTY = '0010' AND ENDDA GE Sy-datum."ENDDA = '99991231'.


      WA_RECLIST-REC_TYPE = 'U' ."INTERNET ADDRESS
      WA_RECLIST-EXPRESS  = 'X' ."VALUE FOR ACTIVATED
      TRANSLATE USERID_LONG TO LOWER CASE.
      WA_RECLIST-RECEIVER = USERID_LONG."gv_smtp_addr.
      if USERID_LONG is NOT INITIAL.
       APPEND WA_RECLIST TO IT_RECLIST.
      endif.
      CLEAR: GV_SMTP_ADDR, WA_RECLIST,G_OBJCONT.

* ---- send mail to creator of purchase requisition: IRDK932083: Wednesday, May 16, 2018 12:04:52 ---- *
      data: it_ekpo like im_ekpo,
            wa_ekpo like line of im_ekpo.
      data: begin of pr_creator,
              ernam type eban-ernam,
            end of pr_creator,
            pr_creators like standard table of pr_creator.

      refresh it_ekpo.  " IRDK932124
      select *
        from ekpo
        into corresponding fields of table it_ekpo
        where ebeln = im_ekko-ebeln.

      if it_ekpo is not initial.
        refresh pr_creators.
        select distinct ernam           " pr creator
          from eban
          into table pr_creators
          for all entries in it_ekpo
          where banfn = it_ekpo-banfn   " pr no
          and   bnfpo = it_ekpo-bnfpo   " pr itm
          and   loekz <> abap_true.

        if pr_creators is not initial.
          clear pr_creator.
          loop at pr_creators into pr_creator.
            clear: wa_reclist.
            clear: userid_long.
            select single usrid_long from pa0105 into userid_long
              where pernr = pr_creator-ernam and subty = '0010' and endda ge sy-datum.  " IRDK932126
            if userid_long is not initial.
              wa_reclist-rec_type = 'U' ."internet address
              wa_reclist-express  = 'X' ."value for activated
              translate userid_long to lower case.
              wa_reclist-receiver = userid_long.
              append wa_reclist to it_reclist.
            endif.
            clear pr_creator.
          endloop.
        endif.
      endif.
* ---- end IRDK932083 ---- *

*************************** send mail on PO complete release to user group ************************************
****** send mail to users group  *********************
      DATA: ZGRP(11).

      CONCATENATE 'PO_' IM_EKKO-ERNAM INTO ZGRP. " create groups as per req of PO creator prefix with 'PO_'.
      CLEAR: FLAG ,IT_USER , WA_USER , IT_EMAIL , WA_EMAIL .

      SELECT * FROM ZFI044_EMAILID INTO TABLE IT_EMAIL WHERE EGROUP = ZGRP.

      LOOP AT IT_EMAIL INTO WA_EMAIL.
        CLEAR: WA_RECLIST.
        WA_RECLIST-REC_TYPE = 'U' ."INTERNET ADDRESS
        WA_RECLIST-EXPRESS  = 'X' ."VALUE FOR ACTIVATED
        TRANSLATE WA_EMAIL-EMAIL TO LOWER CASE.
        WA_RECLIST-RECEIVER = WA_EMAIL-EMAIL .
        APPEND WA_RECLIST TO IT_RECLIST.
      ENDLOOP.

      CLEAR: FLAG.

*---*-- MAIL SUBJECT LINE
      CLEAR: WA_DOC_CHNG.
      WA_DOC_CHNG-OBJ_NAME = 'Purchase Order'.
*  WA_DOC_CHNG-OBJ_DESCR = L_OBJECT_HD_CHANGE-OBJDES.
      CONCATENATE 'PO Number ' IM_EKKO-EBELN 'Completely Released.' INTO WA_DOC_CHNG-OBJ_DESCR
      SEPARATED BY SPACE.
      CLEAR: GV_LINE_NO, GV_COUNT.

*  ---mail body

* lv_tdname = im_ebeln.
      DATA:VEND_NAME TYPE LFA1-NAME1, PO_DATE(10) .

      CONCATENATE IM_EKKO-BEDAT+06(02) '.' IM_EKKO-BEDAT+04(02) '.' IM_EKKO-BEDAT(04) INTO PO_DATE.
      SELECT SINGLE NAME1 FROM LFA1 INTO VEND_NAME WHERE LIFNR = IM_EKKO-LIFNR.

      CONDENSE : VEND_NAME.
      CONCATENATE 'PO Number ' IM_EKKO-EBELN ', dated: ' PO_DATE  ', on Vendor: ' VEND_NAME '(' IM_EKKO-LIFNR ') - has been Released.' INTO G_OBJCONT_WA
      SEPARATED BY SPACE.
      APPEND G_OBJCONT_WA TO G_OBJCONT .
      CLEAR G_OBJCONT_WA.
*   ENDLOOP.
* ENDIF.

*----
      DESCRIBE TABLE G_OBJCONT LINES GV_COUNT.
      GV_LINE_NO = GV_COUNT.
      READ TABLE G_OBJCONT INTO G_OBJCONT_WA INDEX GV_COUNT.
      WA_DOC_CHNG-DOC_SIZE = ( GV_COUNT - 1 ) * 255 + STRLEN( G_OBJCONT_WA ).

*-- POPULATE PACKING LIST FOR BODY TEXT
      WA_PACKING_LIST-HEAD_START  = 1.
      WA_PACKING_LIST-HEAD_NUM    = 0.
      WA_PACKING_LIST-BODY_START  = 1.
      WA_PACKING_LIST-BODY_NUM    = GV_COUNT."V_TABLE_LINES.
      WA_PACKING_LIST-DOC_TYPE    = 'RAW'.
      APPEND WA_PACKING_LIST TO IT_PACKING_LIST.
      CLEAR WA_PACKING_LIST.


      SORT IT_RECLIST BY RECEIVER.
      DELETE IT_RECLIST WHERE RECEIVER  = ''.
      DELETE ADJACENT DUPLICATES FROM IT_RECLIST COMPARING RECEIVER.

      IF NOT IT_RECLIST IS INITIAL.
        CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
          EXPORTING
            DOCUMENT_DATA              = WA_DOC_CHNG
            PUT_IN_OUTBOX              = 'X'
            SENDER_ADDRESS             = SENDER
            SENDER_ADDRESS_TYPE        = SEND_ADR_TYPE
            COMMIT_WORK                = 'X'
          TABLES
            PACKING_LIST               = IT_PACKING_LIST
            CONTENTS_TXT               = G_OBJCONT
            RECEIVERS                  = IT_RECLIST
          EXCEPTIONS
            TOO_MANY_RECEIVERS         = 1
            DOCUMENT_NOT_SENT          = 2
            DOCUMENT_TYPE_NOT_EXIST    = 3
            OPERATION_NO_AUTHORIZATION = 4
            PARAMETER_ERROR            = 5
            X_ERROR                    = 6
            ENQUEUE_ERROR              = 7
            OTHERS                     = 8.

        IF SY-SUBRC <> 0.
          MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF SY-TCODE = 'ME21N' OR  SY-TCODE = 'ME22N'.
    IF IM_EKKO-MEMORY = '' AND IM_EKKO-MEMORYTYPE = ''."Purchase order not yet complete
      IF IM_EKKO-PROCSTAT = '03'.
        SELECT * FROM T16FS INTO TABLE IT_T16FS
          WHERE FRGGR = IM_EKKO-FRGGR
          AND FRGSX = IM_EKKO-FRGSX.
        IF SY-SUBRC = 0.
          READ TABLE IT_T16FS INTO WA_T16FS INDEX 1.

          SELECT * FROM Z6MMA_PARAMS
            INTO TABLE IT_MAIL
            WHERE PROGNAME = 'MM_PO_RELEASE'
            AND PARAM1 = IM_EKKO-FRGGR
*            AND PARAM2 = IM_EKKO-FRGSX
            AND PARAM3 = WA_T16FS-FRGC1. " First releaser

      ENDIF.
    ENDIF.
  ENDIF.
 ENDIF.


  IF SY-TCODE = 'ME29N' or SY-TCODE = 'ME28'.
    IF IM_EKKO-MEMORY = '' AND IM_EKKO-MEMORYTYPE = '' ."Purchase order not yet complete
      IF IM_EKKO-PROCSTAT = '03'.
        IF IM_EKKO-FRGZU IS  NOT INITIAL.	"	Release status
          IF IM_EKKO-FRGZU = 'X'.

          SELECT * FROM T16FS INTO TABLE IT_T16FS
                WHERE FRGGR = IM_EKKO-FRGGR
                AND FRGSX = IM_EKKO-FRGSX.
          IF SY-SUBRC = 0.
            READ TABLE IT_T16FS INTO WA_T16FS INDEX 1.

            SELECT * FROM Z6MMA_PARAMS
              INTO TABLE IT_MAIL
              WHERE PROGNAME = 'MM_PO_RELEASE'
              AND PARAM1 = IM_EKKO-FRGGR
*              AND PARAM2 = IM_EKKO-FRGSX
              AND PARAM3 = WA_T16FS-FRGC2. " First releaser

          ENDIF.
      ELSEIF IM_EKKO-FRGZU = 'XX'.

            SELECT * FROM T16FS INTO TABLE it_T16FS
            WHERE FRGGR = im_ekko-FRGGR
            AND FRGSX = im_ekko-FRGSX.
            IF sy-subrc = 0.
              READ TABLE it_T16FS INTO WA_T16FS INDEX 1.

              SELECT * FROM Z6MMA_PARAMS
                INTO TABLE it_mail
                WHERE PROGNAME = 'MM_PO_RELEASE'
                AND PARAM1 = im_ekko-FRGGR
*                AND PARAM2 = im_ekko-FRGSX
                AND PARAM3 = WA_T16FS-FRGC3. " First releaser

             ENDIF.

        ENDIF.
      ENDIF.
    ELSEIF IM_EKKO-PROCSTAT = '05'.
      IF IM_EKKO-FRGZU = 'XXX'.

      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

if it_mail IS NOT INITIAL.
    LOOP AT IT_MAIL INTO WA_MAIL.
*      CLEAR: WA_MAIL-paramval.
      CLEAR: wa_reclist.
      SELECT SINGLE usrid_long FROM pa0105 INTO wa_reclist-receiver
        WHERE pernr = wa_mail-param4
        AND subty = '0010'
        AND ENDDA GE SY-DATUM."endda = '99991231'.

      wa_reclist-rec_type = 'U' ."INTERNET ADDRESS
      wa_reclist-express  = 'X' ."VALUE FOR ACTIVATED
      TRANSLATE WA_MAIL-paramval TO LOWER CASE.
*      wa_reclist-receiver = WA_MAIL-paramval .
      APPEND wa_reclist TO it_reclist.
    ENDLOOP.

*      *---*-- MAIL SUBJECT LINE
    CLEAR: wa_doc_chng.
    wa_doc_chng-obj_name = 'Purchase Order'.
    CONCATENATE 'PO Number ' im_ekko-ebeln 'Waiting for your Release.' INTO wa_doc_chng-obj_descr
    SEPARATED BY space.
    CLEAR: gv_line_no, gv_count.

*  ---mail body

*DATA:vend_name TYPE lfa1-name1, po_date(10) .

    CONCATENATE im_ekko-BEDAT+06(02) '.' im_ekko-BEDAT+04(02) '.' im_ekko-BEDAT(04) INTO po_date.
    SELECT SINGLE name1 FROM lfa1 INTO vend_name WHERE lifnr = im_ekko-lifnr.

    CONDENSE : vend_name.
    CONCATENATE 'PO Number ' im_ekko-ebeln ', dated: ' po_date  ', on Vendor: ' vend_name '(' im_ekko-lifnr ') - is Waiting for Your Release.' INTO g_objcont_wa
    SEPARATED BY space.
    APPEND g_objcont_wa TO g_objcont .
    CLEAR g_objcont_wa.

    CLEAR g_objcont_wa.
    g_objcont_wa = ' '.
    APPEND g_objcont_wa TO g_objcont .
    CLEAR g_objcont_wa.


    CLEAR g_objcont_wa.
    g_objcont_wa = 'Thanks & Regards.'.
    APPEND g_objcont_wa TO g_objcont .
    CLEAR g_objcont_wa.

    DATA: it_usr21 TYPE TABLE OF usr21 , wa_usr21 like LINE OF it_usr21 .
    DATA: it_ADRP TYPE TABLE OF ADRP , wa_ADRP like LINE OF it_ADRP .

    SELECT * FROM usr21 INTO TABLE it_usr21 WHERE bname = sy-uname.
    IF  sy-subrc = 0 .
      READ TABLE it_usr21 INTO wa_usr21 INDEX 1.
      IF sy-subrc = 0 .
        SELECT SINGLE * FROM adrp INTO wa_ADRP WHERE PERSNUMBER = wa_usr21-PERSNUMBER.
         IF sy-subrc = 0 .
                CLEAR g_objcont_wa.
                g_objcont_wa = wa_ADRP-name_text.
                APPEND g_objcont_wa TO g_objcont .
                CLEAR g_objcont_wa.
         endif.
      endif.
    ENDIF.

*----
    DESCRIBE TABLE g_objcont LINES gv_count.
    gv_line_no = gv_count.
    READ TABLE g_objcont INTO g_objcont_wa INDEX gv_count.
    wa_doc_chng-doc_size = ( gv_count - 1 ) * 255 + STRLEN( g_objcont_wa ).

*-- POPULATE PACKING LIST FOR BODY TEXT
    wa_packing_list-head_start  = 1.
    wa_packing_list-head_num    = 0.
    wa_packing_list-body_start  = 1.
    wa_packing_list-body_num    = gv_count."V_TABLE_LINES.
    wa_packing_list-doc_type    = 'RAW'.
    APPEND wa_packing_list TO it_packing_list.
    CLEAR wa_packing_list.


   SORT IT_RECLIST BY RECEIVER.
   DELETE it_reclist WHERE receiver  = ''.
   DELETE ADJACENT DUPLICATES FROM IT_RECLIST COMPARING RECEIVER.

    IF NOT it_reclist IS INITIAL.
      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          document_data              = wa_doc_chng
          put_in_outbox              = 'X'
          sender_address             = sender
          sender_address_type        = send_adr_type
          commit_work                = 'X'
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

ENDIF.

ENDMETHOD.
ENDCLASS.
