*&---------------------------------------------------------------------*
*&  Include           ZMM_PO_SEND_MAIL_FD
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
FORM get_data.
*  SELECT EBELN BUKRS BSTYP BSART AEDAT LIFNR SPRAS FRGKE RLWRT
*    FROM EKKO
*    INTO TABLE GT_EKKO
*    WHERE AEDAT IN S_AEDAT
*    AND   LIFNR IN S_LIFNR
*    AND   BSART = P_BSART
*    AND   FRGKE = 'G'.
*
  SELECT ebeln bukrs bstyp bsart aedat lifnr spras frgke
  FROM ekko
  INTO TABLE gt_ekko
  WHERE aedat IN s_aedat
  AND   lifnr IN s_lifnr
  AND   bsart = p_bsart
  AND   frgke = 'G'.

  IF sy-subrc = 0.
    SELECT lifnr land1 name1 adrnr
      FROM lfa1
      INTO TABLE gt_lfa1
      FOR ALL ENTRIES IN gt_ekko
      WHERE lifnr = gt_ekko-lifnr.
    IF sy-subrc = 0.
      SELECT addrnumber persnumber date_from  consnumber smtp_addr
        FROM adr6
        INTO TABLE gt_adr6
        FOR ALL ENTRIES IN gt_lfa1
        WHERE addrnumber = gt_lfa1-adrnr.

      SELECT addrnumber persnumber date_from  consnumber tel_number dft_receiv
        FROM adr2
         INTO TABLE gt_adr2
         FOR ALL ENTRIES IN gt_lfa1
         WHERE addrnumber = gt_lfa1-adrnr
         AND   dft_receiv = 'X'.
    ENDIF.
  ENDIF.

  PERFORM process_data.
ENDFORM.                    "get_data
*&---------------------------------------------------------------------*
*&      Form  get_po_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_po_data USING ebeln TYPE ekko-ebeln.
  SELECT * INTO TABLE i_ekko
           FROM ekko
           WHERE ebeln = ebeln
           AND loekz = ''.
  SELECT * INTO TABLE i_ekpo
         FROM ekpo
         WHERE ebeln = ebeln
         AND   loekz = '' .
  READ TABLE i_ekko INDEX 1.
  IF sy-subrc = 0 .
    IF i_ekko-frgke = 'B'.
      MESSAGE e398(00) WITH 'Released the PO' '' '' ''..
    ENDIF.

    SELECT SINGLE * FROM lfm1 INTO wa_lfm1 WHERE lifnr = i_ekko-lifnr.
*{   REPLACE        SBXK900019                                        1
*\    SELECT * FROM konv
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************

    SELECT * FROM PRCD_ELEMENTS
*}   REPLACE
        INTO CORRESPONDING FIELDS OF TABLE i_konv
        WHERE knumv = i_ekko-knumv.

  ENDIF.

*  hide below conditions in PO Print
*  develeoper: Punam S
  DELETE i_konv WHERE kschl = 'ZBPC'. " BP CIF for Import
  DELETE i_konv WHERE kschl = 'ZBPL'. " Basic price
  DELETE i_konv WHERE kschl = 'ZBPP'. " basic price


  IF i_ekko-bsart <> 'ZSTO'.
    DELETE i_konv WHERE kschl = 'FRA2' AND lifnr <> i_ekko-lifnr." AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'FRB2' AND lifnr <> i_ekko-lifnr." AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'FRC2' AND lifnr <> i_ekko-lifnr." AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'ZFBT' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'FRA1' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'FRB1' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'FRC1' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'JOCM' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'ZJO2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'ZUN2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'ZDN2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'ZOC2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'ZLBT' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
    DELETE i_konv WHERE kschl = 'ZETX' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*   ----
  ENDIF.
  IF NOT i_ekpo[] IS INITIAL.
    SELECT *
         FROM ml_esll
         INTO CORRESPONDING FIELDS OF TABLE i_ml_esll
         FOR ALL ENTRIES IN i_ekpo
         WHERE ebeln = i_ekpo-ebeln AND
         del NE 'X'.
    SELECT *
         FROM mdsb
         INTO CORRESPONDING FIELDS OF TABLE i_mdsb
         FOR ALL ENTRIES IN i_ekpo
         WHERE ebeln = i_ekpo-ebeln AND
         xloek NE 'X'.
  ENDIF.

  CLEAR tt_komv.

  CALL FUNCTION 'Z6MM_PO_CAL_TAX'
    EXPORTING
      i_ebeln = ebeln
*     I_EBELP =
    IMPORTING
      tt_komv = tt_komv.


  IF NOT tt_komv[] IS INITIAL.

    LOOP AT tt_komv INTO wa_komv.
      MOVE-CORRESPONDING wa_komv TO i_komv.
      APPEND i_komv.
      CLEAR  i_komv.
    ENDLOOP.

  ENDIF.

  CLEAR: wa_komv_jvcs.
*  ---read jvcs
  READ TABLE i_komv INTO wa_komv WITH KEY
  kschl = 'JVCS'.
  IF sy-subrc = 0.
    wa_komv_jvcs = wa_komv.
    CLEAR wa_komv.
  ENDIF.

  DELETE i_komv WHERE kwert EQ 0
     OR kschl EQ 'JMX1'
     OR kschl EQ 'JMX2'
     OR kschl EQ 'JAX1'
     OR kschl EQ 'JAX2'
     OR kschl EQ 'JSX1'
     OR kschl EQ 'JSX2'
     OR kschl EQ 'JEX1'
     OR kschl EQ 'JEX2'
     OR kschl EQ 'JHX1'
     OR kschl EQ 'JHX2'
     OR kschl EQ 'NAVS'
     OR kschl EQ 'NAVM'.

  DELETE i_komv WHERE koaid EQ 'A'.
  CLEAR  i_komv.

  DELETE i_konv WHERE kwert EQ 0
      OR kschl EQ 'JEXS'.

  IF wa_lfm1-kalsk = '02'.
    IF wa_lfm1-lifnr = '0020000367'.
      READ TABLE i_ekpo INDEX 1.
      IF sy-subrc = 0.
        IF i_ekpo-werks = '1101'.
          DELETE i_konv WHERE NOT ( kschl EQ 'PB00'
              OR   kschl EQ 'PBXX'
              OR   kschl EQ 'R000'
              OR   kschl EQ 'R001'
              OR   kschl EQ 'R002'
              OR   kschl EQ 'R003'
              OR   kschl EQ 'KR00'
              OR   kschl EQ 'K000'
              OR   kschl EQ 'ZPK1'
              OR   kschl EQ 'ZPK2'
              OR   kschl EQ 'ZPK3'
              OR   kschl EQ 'FRC1'
              ).
        ENDIF.
      ENDIF.
    ELSE.
      DELETE i_konv WHERE NOT ( kschl EQ 'PB00'
          OR   kschl EQ 'PBXX'
          OR   kschl EQ 'R000'
          OR   kschl EQ 'R001'
          OR   kschl EQ 'R002'
          OR   kschl EQ 'R003'
          OR   kschl EQ 'KR00'
          OR   kschl EQ 'K000'
          OR   kschl EQ 'ZPK1'
          OR   kschl EQ 'ZPK2'
          OR   kschl EQ 'ZPK3'
          OR   kschl EQ 'IFR2'
          OR   kschl EQ 'IIN2'  " As per ZMM_PURCHASE_ORDER_PRG_GST, Added on 3.10.2017
          ).

    ENDIF.
  ENDIF.

  PERFORM print_form USING ebeln.
ENDFORM.                    "get_po_data
*&---------------------------------------------------------------------*
*&      Form  CLEAR_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM clear_refresh .
  CLEAR :
            i_ekko , i_ekpo , i_konv , i_komv , wa_lfm1.
  REFRESH :
            i_ekko , i_ekpo , i_konv , i_komv
            .
ENDFORM.                    " CLEAR_REFRESH
*&---------------------------------------------------------------------*
*&      Form  print_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->EBELN      text
*----------------------------------------------------------------------*
FORM print_form USING ebeln TYPE ekko-ebeln.
  CLEAR : fp_controlparams ,  fp_outputparams.
  fp_controlparams-getotf = 'X'.
  fp_controlparams-no_dialog = 'X'.
***  fp_controlparams-PREVIEW = ''.

  fp_outputparams-tdimmed = 'X'.
  fp_outputparams-tdnewid = 'X'.
**  fp_outputparams-tdnoprev = 'X'.
***  fp_outputparams-tdnoprint = 'X'.
  fp_outputparams-tddest = 'PDF'.

***  READ TABLE I_EKKO INTO WA_EKKO INDEX 1.
  READ TABLE i_ekko INTO wa_ekko WITH KEY ebeln = ebeln.
  IF sy-subrc = 0.
    IF wa_ekko-bsart = 'ZSTO'.
*      IF wa_ekko-bedat LE '20170630'.  " Call GST Specific form after 30.06.2017
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = 'Z6MM006S_STOCK_TRANS_ORDER'
*         variant            = ' '
*         direct_call        = ' '
        IMPORTING
          fm_name            = lf_fm_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      CALL FUNCTION lf_fm_name
        EXPORTING
          control_parameters = fp_controlparams
          output_options     = fp_outputparams
          user_settings      = space
          wa_komv_jvcs       = wa_komv_jvcs
        IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
          job_output_info    = it_job_output_info
        TABLES
          x_mdsb             = i_mdsb
          x_ekko             = i_ekko
          x_ekpo             = i_ekpo
          x_ml_esll          = i_ml_esll
          x_konv             = i_konv
          x_komv             = i_komv
* EXCEPTIONS
*         FORMATTING_ERROR   = 1
*         INTERNAL_ERROR     = 2
*         SEND_ERROR         = 3
*         USER_CANCELED      = 4
*         OTHERS             = 5
        .
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*      ELSE.
      " Call GST Specific form, In this case it is the same.
*      ENDIF.
    ELSEIF wa_ekko-bsart = 'ZIMP'.
      IF wa_ekko-bedat LE '20170630'.   " Call GST Specific form after 30.06.2017
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = 'Z6MM006S_IMPORT_PO_PRINT'
*           variant            = ' '
*           direct_call        = ' '
          IMPORTING
            fm_name            = lf_fm_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.

        CALL FUNCTION lf_fm_name
          EXPORTING
            control_parameters = fp_controlparams
            output_options     = fp_outputparams
            user_settings      = space
            wa_komv_jvcs       = wa_komv_jvcs
          IMPORTING
*           DOCUMENT_OUTPUT_INFO       =
            job_output_info    = it_job_output_info
          TABLES
            x_mdsb             = i_mdsb
            x_ekko             = i_ekko
            x_ekpo             = i_ekpo
            x_ml_esll          = i_ml_esll
            x_konv             = i_konv
            x_komv             = i_komv
* EXCEPTIONS
*           FORMATTING_ERROR   = 1
*           INTERNAL_ERROR     = 2
*           SEND_ERROR         = 3
*           USER_CANCELED      = 4
*           OTHERS             = 5
          .
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSE.
        " Call GST Specific form
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = 'ZMM_IMPORT_PO_GST_SF'
*           variant            = ' '
*           direct_call        = ' '
          IMPORTING
            fm_name            = lf_fm_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.

        CALL FUNCTION lf_fm_name  "'/1BCDWB/SF00000197'
          EXPORTING
            control_parameters = fp_controlparams
            output_options     = fp_outputparams
            user_settings      = space
            wa_komv_jvcs       = wa_komv_jvcs
          IMPORTING
*           DOCUMENT_OUTPUT_INFO       =
            job_output_info    = it_job_output_info
*           JOB_OUTPUT_OPTIONS =
          TABLES
            x_ekko             = i_ekko
            x_ekpo             = i_ekpo
            x_ml_esll          = i_ml_esll
            x_konv             = i_konv
            x_komv             = i_komv
            x_mdsb             = i_mdsb
*         EXCEPTIONS
*           FORMATTING_ERROR   = 1
*           INTERNAL_ERROR     = 2
*           SEND_ERROR         = 3
*           USER_CANCELED      = 4
*           OTHERS             = 5
          .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

      ENDIF.
    ELSE.
      IF wa_ekko-bedat LE '20170630'. " Call GST Specific form after 30.06.2017
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = 'Z6MM006S_PURCHASE_ORDER'
*           variant            = ' '
*           direct_call        = ' '
          IMPORTING
            fm_name            = lf_fm_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.

        CALL FUNCTION lf_fm_name
          EXPORTING
            control_parameters = fp_controlparams
            output_options     = fp_outputparams
            user_settings      = space
            wa_komv_jvcs       = wa_komv_jvcs
          IMPORTING
*           DOCUMENT_OUTPUT_INFO       =
            job_output_info    = it_job_output_info
          TABLES
            x_mdsb             = i_mdsb
            x_ekko             = i_ekko
            x_ekpo             = i_ekpo
            x_ml_esll          = i_ml_esll
            x_konv             = i_konv
            x_komv             = i_komv
* EXCEPTIONS
*           FORMATTING_ERROR   = 1
*           INTERNAL_ERROR     = 2
*           SEND_ERROR         = 3
*           USER_CANCELED      = 4
*           OTHERS             = 5
          .
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
      ELSE.

        " IHDK901733
        data(lv_formname) = cond tdsfname( when i_ekko-bukrs eq '2800'
                                           then 'ZMM_PURCHASE_ORDER_INDOREAGENS'
                                           else 'ZMM_PURCHASE_ORDER_FORM' ).

        " Call GST Specific form
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = lv_formname
*           variant            = ' '
*           direct_call        = ' '
          IMPORTING
            fm_name            = lf_fm_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.

        CALL FUNCTION lf_fm_name  "'/1BCDWB/SF00000196'
          EXPORTING
            control_parameters = fp_controlparams
            output_options     = fp_outputparams
            user_settings      = space
            wa_komv_jvcs       = wa_komv_jvcs
          IMPORTING
*           DOCUMENT_OUTPUT_INFO       =
            job_output_info    = it_job_output_info
*           JOB_OUTPUT_OPTIONS =
          TABLES
            x_ekko             = i_ekko
            x_ekpo             = i_ekpo
            x_ml_esll          = i_ml_esll
            x_konv             = i_konv
            x_komv             = i_komv
            x_mdsb             = i_mdsb
*         EXCEPTIONS
*           FORMATTING_ERROR   = 1
*           INTERNAL_ERROR     = 2
*           SEND_ERROR         = 3
*           USER_CANCELED      = 4
*           OTHERS             = 5
          .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

      ENDIF.
    ENDIF.


    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
*       ARCHIVE_INDEX         = ' '
*       COPYNUMBER            = 0
*       ASCII_BIDI_VIS2LOG    = ' '
*       PDF_DELETE_OTFTAB     = ' '
*       PDF_USERNAME          = ' '
      IMPORTING
        bin_filesize          = wrk_filesiz
*       BIN_FILE              =
      TABLES
        otf                   = it_job_output_info-otfdata[]
        lines                 = it_pdfdata
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ELSE.
    ENDIF.

  ENDIF.
ENDFORM.                    "print_form
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_data.

  DATA : it_komv TYPE komv_itab,
         wa_komv LIKE LINE OF it_komv.

  SORT : gt_ekko BY ebeln,
         gt_lfa1 BY lifnr,
         gt_adr6 BY addrnumber,
         gt_adr2 BY addrnumber.

  LOOP AT gt_ekko INTO gs_ekko.
    MOVE-CORRESPONDING : gs_ekko TO gs_final.

    CALL FUNCTION 'Z6MM_PO_CAL_TAX'
      EXPORTING
        i_ebeln = gs_ekko-ebeln
*       I_EBELP =
      IMPORTING
        tt_komv = it_komv.

    IF it_komv IS NOT INITIAL.

      DELETE it_komv[] WHERE kwert EQ 0
       OR kschl EQ 'JMX1'
       OR kschl EQ 'JMX2'
       OR kschl EQ 'JAX1'
       OR kschl EQ 'JAX2'
       OR kschl EQ 'JSX1'
       OR kschl EQ 'JSX2'
       OR kschl EQ 'JEX1'
       OR kschl EQ 'JEX2'
       OR kschl EQ 'JHX1'
       OR kschl EQ 'JHX2'
       OR kschl EQ 'NAVS'
       OR kschl EQ 'NAVM'.

*   DELETE IT_KOMV[] WHERE KOAID EQ 'A'.

      LOOP AT it_komv[] INTO wa_komv.
        gs_final-rlwrt = gs_final-rlwrt + wa_komv-kwert.
        CLEAR wa_komv.
      ENDLOOP.
    ENDIF.

    READ TABLE gt_lfa1 INTO gs_lfa1 WITH KEY lifnr = gs_ekko-lifnr BINARY SEARCH.
    IF sy-subrc = 0.
      gs_final-name1 = gs_lfa1-name1.
      READ TABLE gt_adr6 INTO gs_adr6 WITH KEY addrnumber = gs_lfa1-adrnr
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        gs_final-smtp_addr = gs_adr6-smtp_addr.
      ENDIF.
      READ TABLE gt_adr2 INTO gs_adr2 WITH KEY addrnumber = gs_lfa1-adrnr
                                 BINARY SEARCH.
      IF sy-subrc = 0.
        gs_final-tel_number = gs_adr2-tel_number.
      ENDIF.
    ENDIF.
    IF gs_final-smtp_addr IS INITIAL.
      ls_stylerow-fieldname = 'CHK' .
      ls_stylerow-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_stylerow INTO TABLE gs_final-field_style.
    ENDIF.
    APPEND  gs_final TO gt_final.
    CLEAR : gs_final , gs_lfa1 , gs_ekko , gs_adr6, it_komv[].
  ENDLOOP.
  SORT gt_final BY lifnr aedat.
ENDFORM.                    "process_data
*&---------------------------------------------------------------------*
*&      Form  mail_message_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EMPCODE  text
*      -->ENAME      text
*----------------------------------------------------------------------*
FORM mail_message_body USING vname TYPE lfa1-name1.
  DATA : str TYPE string.
  CLEAR : it_message , it_message[],str.
  REFRESH it_message.
  CONCATENATE 'M/s' vname INTO str SEPARATED BY space.
  it_message = str.
  APPEND it_message.

  it_message = ' '.
  APPEND it_message.
  it_message = ' '.
  APPEND it_message.
  it_message = 'Please Find the attached PDF file of PO.'.
  APPEND it_message.
  it_message = ' '.
  APPEND it_message.
  it_message = 'To download binding general T&C of this PO, browse URL:  http://goo.gl/o2Wf6C'.
  APPEND it_message.
  it_message = ' '.
  APPEND it_message.
  it_message = ' '.
  APPEND it_message.
  it_message = 'Indofil Purchase Team '.

  APPEND it_message.
  it_message = ' '.
  APPEND it_message.
  it_message = ' '.
  APPEND it_message.
  it_message = 'PLEASE NOTE :- THIS IS AN AUTO GENERATED MAIL, PLEASE DO NOT REPLY TO THIS MAIL.'.
  APPEND it_message.

ENDFORM.                    "mail_message_body
*&---------------------------------------------------------------------*
*&      Form  sending_mail_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LIFNR         text
*      -->EBELN         text
*      -->LV_EMAILADDR  text
*----------------------------------------------------------------------*
FORM sending_mail_new USING lifnr TYPE ekko-lifnr
                            ebeln TYPE ekko-ebeln
                            lv_emailaddr TYPE adr6-smtp_addr.

  DATA :
***          lv_emailaddr type adr6-smtp_addr,
          v_adrnr TYPE lfa1-adrnr.

  DATA: objpack LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.

  DATA: objhead LIKE solisti1   OCCURS  1 WITH HEADER LINE.

  DATA: objbin  LIKE solisti1   OCCURS 10 WITH HEADER LINE.

  DATA: objtxt  LIKE solisti1   OCCURS 10 WITH HEADER LINE.

  DATA: reclist LIKE somlreci1  OCCURS  5 WITH HEADER LINE.

  DATA: doc_chng LIKE sodocchgi1.

  DATA: tab_lines LIKE sy-tabix,
        ld_mtitle LIKE sodocchgi1-obj_descr.

  DATA: n TYPE i.

  DATA: it_params TYPE TABLE OF z6mma_params, wa_para LIKE LINE OF it_params.

***  DOC_CHNG-OBJ_NAME = 'INT'.
***
***  DOC_CHNG-OBJ_DESCR = 'Purchase Order Intimation Letter'.
***
***  OBJTXT = 'Please Find The Attached Document'.
***
***  APPEND OBJTXT.
  CLEAR : ld_sender_address , ld_sender_address_type.
  ld_sender_address      = 'sapautomail@indofil.com'.
  ld_sender_address_type = 'SMTP'.

  DESCRIBE TABLE it_message LINES tab_lines.

  READ TABLE it_message INDEX tab_lines.

  PERFORM : rem_zeros USING ebeln CHANGING ebeln,
            rem_zeros USING lifnr CHANGING lifnr.

  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( it_message ).
  CONCATENATE 'Indofil PO' '-' ebeln INTO ld_mtitle.
  doc_chng-obj_descr  = ld_mtitle.

  objpack-head_start = 1.

  objpack-head_num   = 0.

  objpack-body_start = 1.

  objpack-body_num   = tab_lines.

  objpack-doc_type   = 'RAW'.

  APPEND objpack.


  CALL FUNCTION 'QCE1_CONVERT'
    TABLES
      t_source_tab         = it_pdfdata
      t_target_tab         = objbin
    EXCEPTIONS
      convert_not_possible = 1
      OTHERS               = 2.



  DESCRIBE TABLE objbin LINES tab_lines.

  CONCATENATE lifnr '-' ebeln '.PDF' INTO objhead.
***  OBJHEAD = 'Attachment.PDF'.
  APPEND objhead.

* Creating the entry for the compressed attachment

  objpack-transf_bin = 'X'.

  objpack-head_start = 1.

  objpack-head_num   = 1.

  objpack-body_start = 1.

  objpack-body_num   = tab_lines.

  objpack-doc_type   = 'PDF'.

  objpack-obj_name   = 'ATTACHMENT'.

  objpack-obj_descr = 'PDF ATTACHMENT'.

  objpack-doc_size   = wrk_filesiz.

  APPEND objpack..

  CLEAR : reclist , reclist[].
  reclist-receiver =  lv_emailaddr.
  reclist-rec_type = 'U'.
  APPEND reclist.

* maintain email id in Z6MMA_PARAMS table in pgmname ZMM048 to receive mail
  IF p_bsart EQ 'ZIMP' OR p_bsart EQ 'YIMP'.
    CLEAR: it_params , wa_para.

    SELECT * FROM z6mma_params INTO TABLE it_params
      WHERE progname = 'ZMM048'
      AND ( param1 = 'ZIMP' OR param1 = 'YIMP' ).

    LOOP AT it_params INTO wa_para.
      reclist-receiver = wa_para-paramval. "'aabane@indofil.com'.
      reclist-rec_type = 'U'.
      reclist-copy = 'X'.
      APPEND reclist.
    ENDLOOP.

*    RECLIST-RECEIVER = 'aabane@indofil.com'.
****    reclist-receiver = 'raulnish09@gmail.com'.
*    RECLIST-REC_TYPE = 'U'.
*    RECLIST-COPY = 'X'.
*    APPEND RECLIST.
*
*    RECLIST-RECEIVER = 'rrane@indofil.com'.
****    reclist-receiver = 'nishankar.rahul@gmail.com'.
*    RECLIST-REC_TYPE = 'U'.
*    RECLIST-COPY = 'X'.
*    APPEND RECLIST.
**  ELSE.
**    RECLIST-RECEIVER = 'rjoshi@indofil.com'.
**    RECLIST-REC_TYPE = 'U'.
**    RECLIST-COPY = 'X'.
**    APPEND RECLIST.
**  ENDIF.
  ENDIF.
  break 10106.
  DATA: p_email TYPE somlreci1-receiver.
  SELECT SINGLE usrid_long FROM pa0105
    INTO p_email
    WHERE pernr = sy-uname
    AND subty = '0010'
    AND endda >= sy-datum."'99991231'.

  reclist-receiver = p_email.
  reclist-rec_type = 'U'.
  reclist-copy = 'X'.
  APPEND reclist.
*endif.
*above code is written on date 18.09.2015 , mail should trigger to user who executed this pgm

* 'sapautomail@indofil.com'
* stop mail if pgm executed in QA or developement ,
* only mail will trigger to user who is executing this report
  break 10106.

*  IF SY-SYSID = 'IRQ' OR SY-SYSID = 'IRD'.
*    DELETE RECLIST WHERE rec_type = 'U'.
*
*    CLEAR: RECLIST-RECEIVER, RECLIST.
*    SELECT SINGLE USRID_LONG
*      FROM PA0105
*      INTO RECLIST-RECEIVER
*      WHERE PERNR = SY-UNAME
*      AND SUBTY = '0010'
*      AND ENDDA = '99991231'.
*    RECLIST-REC_TYPE = 'U'.
*    RECLIST-COPY = 'X'.
*    APPEND RECLIST.
*  ENDIF.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      sender_address             = ld_sender_address         " sender's address
      sender_address_type        = ld_sender_address_type
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
***      contents_txt               = objtxt
      contents_txt               = it_message
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.


  IF sy-subrc = 0.
    cnt = cnt + 1.
  ENDIF.
ENDFORM.                    "sending_mail_new
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcatalog.
***  CLEAR layout.
  layout-cwidth_opt = 'X'.
  IF gt_final[] IS NOT INITIAL.                                 " added by Naren Karra on 20.10.2015
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_bypassing_buffer       = 'X'
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'SUB_ALV_USER_COMMAND'
        is_layout_lvc            = layout
        it_fieldcat_lvc          = fieldcatalog[]
        i_save                   = 'A'
        is_variant               = gx_variant
      TABLES
        t_outtab                 = gt_final[]
*     EXCEPTIONS
*       PROGRAM_ERROR            = 1
*       OTHERS                   = 2
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ELSE.
    MESSAGE ' No record found/ Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.                    "display_data
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UT_EXTAB   text
*----------------------------------------------------------------------*
FORM set_pf_status USING ut_extab TYPE slis_t_extab.
***  SET PF-STATUS 'ZRN_STD'.
  SET PF-STATUS 'ZRN_STATUS'.
ENDFORM.                    "set_pf_status
*&---------------------------------------------------------------------*
*&      Form  sub_alv_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM sub_alv_user_command  USING r_ucomm     LIKE sy-ucomm
                                 rs_selfield TYPE slis_selfield.

  DATA: str         TYPE string,
        lv_answer   TYPE c,
        lv_text(50) TYPE c,
        lv_lines    TYPE i.
***  break ibm_ams.
  CASE r_ucomm.
    WHEN '&IC1'.
      CASE rs_selfield-fieldname.
        WHEN 'EBELN'.
          CLEAR : gs_final1.
          READ TABLE gt_final INTO gs_final1 WITH KEY ebeln = rs_selfield-value.
          IF sy-subrc = 0.
            SET PARAMETER ID 'BES' FIELD rs_selfield-value.
            CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'LIFNR'.
          CLEAR : gs_final1.
          READ TABLE gt_final INTO gs_final1 WITH KEY lifnr = rs_selfield-value.
          IF sy-subrc = 0.
            SET PARAMETER ID 'LIF' FIELD rs_selfield-value.
***            SET PARAMETER ID 'BUK' FIELD '1000'.
***          SET PARAMETER ID 'EKO' FIELD wa_vend-ekorg.
***          SET PARAMETER ID 'KDY' FIELD kdy_val.
            CALL TRANSACTION 'MK02' ."AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    WHEN 'SEL'.
      PERFORM get_sel_data.
      LOOP AT gt_final INTO gs_final WHERE smtp_addr IS NOT INITIAL.
        gs_final-chk = 'X'.
        MODIFY gt_final FROM gs_final TRANSPORTING chk.
        CLEAR : gs_final.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
      rs_selfield-exit ='X'.
      PERFORM get_sel_data.
      PERFORM display_data.
    WHEN 'DESEL'.
      PERFORM get_sel_data.
      LOOP AT gt_final INTO gs_final WHERE smtp_addr IS NOT INITIAL.
        gs_final-chk = ''.
        MODIFY gt_final FROM gs_final TRANSPORTING chk.
        CLEAR : gs_final.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
      rs_selfield-exit ='X'.
      PERFORM get_sel_data.
      PERFORM display_data.
    WHEN 'SEND_MAIL'.
***      rs_selfield-refresh = 'X'.
***      rs_selfield-exit ='X'.
      PERFORM get_sel_data.
      PERFORM send_final_data.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDFORM.                    "sub_alv_user_command
*&---------------------------------------------------------------------*
*&      Form  get_sel_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_sel_data.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = e_grid.
  CALL METHOD e_grid->check_changed_data.
ENDFORM.                    "get_sel_data
*&---------------------------------------------------------------------*
*&      Form  send_final_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_final_data.
  LOOP AT gt_final INTO gs_final WHERE chk = 'X'.
    PERFORM : clear_refresh,
              get_po_data       USING gs_final-ebeln,
              mail_message_body USING gs_final-name1,
              sending_mail_new  USING gs_final-lifnr
                                      gs_final-ebeln
                                      gs_final-smtp_addr.

    cstr = cnt.
    CLEAR : gs_final.
  ENDLOOP.
  CONDENSE : cstr.
  IF cstr > 1.
    CONCATENATE cstr 'Mails Successfully Sent' INTO smsg SEPARATED BY space.
  ELSEIF cstr = 1.
    CONCATENATE cstr 'Mail Successfully Sent' INTO smsg SEPARATED BY space.
  ENDIF.
  MESSAGE smsg TYPE 'S'.
ENDFORM.                    "send_final_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcatalog.
  CLEAR : fieldcatalog , fieldcatalog[].
  DATA : pos TYPE i.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'CHK'.
  fieldcatalog-col_pos     = pos.
  fieldcatalog-checkbox    = 'X'.
  fieldcatalog-edit        = 'X'.
  fieldcatalog-scrtext_m   = 'Choose'.
  fieldcatalog-outputlen    = 2.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'EBELN'.
***  fieldcatalog-seltext_m   = 'Po Number'.
  fieldcatalog-scrtext_m   = 'Po Number'.
  fieldcatalog-col_pos     = pos.
  fieldcatalog-hotspot = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'AEDAT'.
***  fieldcatalog-seltext_m   = 'Po Date'.
  fieldcatalog-scrtext_m   = 'Po Date'.
  fieldcatalog-col_pos     = pos.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'LIFNR'.
***  fieldcatalog-seltext_m   = 'Vendor Code'.
  fieldcatalog-scrtext_m   = 'Vendor Code'.
  fieldcatalog-col_pos     = pos.
  fieldcatalog-hotspot = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'NAME1'.
***  fieldcatalog-seltext_m   = 'Vendor Name'.
  fieldcatalog-scrtext_m   = 'Vendor Name'.
  fieldcatalog-col_pos     = pos.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'SMTP_ADDR'.
***  fieldcatalog-seltext_m   = 'Mail ID'.
  fieldcatalog-scrtext_m   = 'Mail ID'.
  fieldcatalog-col_pos     = pos.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'TEL_NUMBER'.
***  fieldcatalog-seltext_m   = 'Mail ID'.
  fieldcatalog-scrtext_m   = 'Mob No'.
  fieldcatalog-col_pos     = pos.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'RLWRT'.
***  fieldcatalog-seltext_m   = 'Total PO Amt'.
  fieldcatalog-scrtext_m   = 'Total PO Amt'.
  fieldcatalog-col_pos     = pos.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
ENDFORM.                    "build_fieldcatalog

*&---------------------------------------------------------------------*
*&      Form  build_eventtab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EVENTS   text
*----------------------------------------------------------------------*
FORM build_eventtab USING p_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = p_events.

  READ TABLE p_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE formname_top_of_page TO ls_event-form.
    APPEND ls_event TO p_events.
  ENDIF.

ENDFORM.                               " BUILD_EVENTTAB

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LAYOUT   text
*----------------------------------------------------------------------*
FORM build_layout  USING    p_layout TYPE lvc_s_layo."slis_layout_alv."
  p_layout-zebra        = 'X'.
  p_layout-stylefname = 'FIELD_STYLE'.
ENDFORM.                    " build_layout

*&-------------------------------------------------------------------*
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
FORM initialize_variant.

  g_save = 'A'.
  CLEAR g_variant.
  g_variant-report = sy-repid.
  gx_variant = g_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.
*   layout-group_change_edit = 'X'.

ENDFORM.                               " INITIALIZE_VARIANT

*&---------------------------------------------------------------------*
*&      Form  f4_for_variant
*&---------------------------------------------------------------------*
FORM f4_for_variant .

  g_save = 'A'.
  g_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " f4_for_variant

*&---------------------------------------------------------------------*
*&      Form  rem_zeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->A          text
*      -->A1         text
*----------------------------------------------------------------------*
FORM rem_zeros USING a TYPE any
               CHANGING a1 TYPE any.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = a
    IMPORTING
      output = a1.
ENDFORM.                    "rem_zeros

*&---------------------------------------------------------------------*
*&      Form  add_zeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->A          text
*      -->A1         text
*----------------------------------------------------------------------*
FORM add_zeros USING a TYPE any
               CHANGING a1 TYPE any.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = a
    IMPORTING
      output = a1.
ENDFORM.                    "add_zeros
*&---------------------------------------------------------------------*
*&      Form  CHK_AUTH_OBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_auth_obj .

  AUTHORITY-CHECK OBJECT 'M_BEST_BSA'
                      ID 'BSART' FIELD p_bsart
                      ID 'ACTVT' FIELD '03'.

  IF sy-subrc NE 0.
*  IF GV_AUTH_BSART_FLG IS INITIAL.
*    GV_AUTH_BSART_FLG = 'X'.
*  ENDIF.
    MESSAGE 'Missing Authorization' TYPE 'I' DISPLAY LIKE 'E'.
*  MESSAGE 'Missing Authorization' TYPE 'A' DISPLAY LIKE 'I'.
*  STOP.
*LEAVE TO TRANSACTION 'ZMM048'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    " CHK_AUTH_OBJ
