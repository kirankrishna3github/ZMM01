*&---------------------------------------------------------------------*
*& Report ZMM_INFORM_VEND_INV_SUBMIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmm_inform_vend_inv_submit.
TABLES:bkpf , bseg ,bsis .


****Company code – BSIS-BUKRS
****Document Number – BSIS -BELNR
****Year - BSIS –GKAHR
****Document date – BSIS-BLDAT
****Plant – BSIS-WERKS

TYPES: BEGIN OF ty_bsis,
         bukrs TYPE bsis-bukrs,
         hkont TYPE bsis-hkont,
         belnr TYPE bsis-belnr,
         gjahr TYPE bsis-gjahr,
         BUZEI TYPE bsis-BUZEI,
         zuonr TYPE bsis-zuonr,
         blart TYPE bsis-blart,
         bldat TYPE bsis-bldat,
         xblnr TYPE bsis-xblnr,
         shkzg TYPE bsis-shkzg,
         dmbtr TYPE bsis-dmbtr,
         werks TYPE bsis-werks,
       END OF ty_bsis,
       BEGIN OF ty_bseg ,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         BUZEI TYPE bsis-BUZEI,
         zuonr TYPE bseg-zuonr,
         menge TYPE bseg-menge,
         meins TYPE bseg-meins,
         matnr TYPE bseg-matnr,
         ebeln TYPE bseg-ebeln,
         ebelp TYPE bseg-ebelp,
         lifnr TYPE bseg-lifnr,
         dmbtr TYPE bseg-dmbtr,
         ekgrp TYPE ekko-ekgrp,
       END OF ty_bseg,
       BEGIN OF ty_param,
         param1   TYPE z6mma_params-param1,
         paramval TYPE z6mma_params-paramval,
       END OF ty_param.

TYPES: BEGIN OF ty_final.
    INCLUDE TYPE zstr_vend_inv.
*types : vend_mail_id type adr6-smtp_addr,
*        user_mail_id type adr6-smtp_addr,
TYPES: END OF ty_final.

DATA: it_bsis   TYPE TABLE OF ty_bsis,
      ls_bsis   TYPE ty_bsis,
      it_bseg   TYPE TABLE OF ty_bseg,
      ls_bseg   TYPE ty_bseg,
      lt_final  TYPE TABLE OF ty_final,
      lt_final1 TYPE TABLE OF ty_final,
      ls_final1 TYPE ty_final,
      ls_final  TYPE ty_final.

DATA: it_param TYPE TABLE OF ty_param.

DATA: lo_table     TYPE REF TO cl_salv_table,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lo_display   TYPE REF TO cl_salv_display_settings,
      lo_columns   TYPE REF TO cl_salv_columns_table,
      lo_column    TYPE REF TO cl_salv_column_table,
      lo_layout    TYPE REF TO cl_salv_layout,
      ls_key       TYPE salv_s_layout_key,
      lo_selection TYPE REF TO cl_salv_selections,
      lo_event     TYPE REF TO cl_salv_events_table.

DATA : lv_save(1) TYPE c,
       lv_exit(1) TYPE c,
       lv_fm      TYPE rs38l_fnam,
       ls_variant TYPE disvariant,
       gs_variant TYPE disvariant.

DATA: control         TYPE ssfctrlop,
      job_output_info TYPE ssfcrescl,
      output_opt      TYPE ssfcompop,
      lt_pdf          TYPE TABLE OF tline,
      lt_doc          TYPE TABLE OF docs,
      ls_content      TYPE xstring,
      lt_solix        TYPE solix_tab.

DATA: attachment  TYPE zfi_s_vp_attachment,
      attachments TYPE TABLE OF zfi_s_vp_attachment,
      header      TYPE string,
      content     TYPE string,
      body        TYPE soli_tab,
      wa_body     LIKE LINE OF body,
      subject     TYPE string,
      sent        TYPE abap_bool,
      recipient   TYPE zfi_s_vp_recipient,
      recipients  LIKE STANDARD TABLE OF recipient.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_lifnr FOR bseg-lifnr,
                s_bukrs FOR bkpf-bukrs OBLIGATORY,
                s_belnr FOR bkpf-belnr,
                s_gjahr FOR bkpf-gjahr OBLIGATORY,
                s_bldat FOR bsis-bldat OBLIGATORY,
                s_werks FOR bsis-werks ,
                s_hkont FOR bsis-hkont NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

"ALV variant
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.


*&------------------------------------------------------------&*
*&                Field symbols Declaration
*&------------------------------------------------------------&*
FIELD-SYMBOLS <fs_x> TYPE x.

CLASS lcl_module DEFINITION.
  PUBLIC SECTION.
    METHODS: process, get_data , process_data , f4_variant,variant_exist,init_variant , call_sf, display_alv, send_mail ,
      change_col_text IMPORTING columnname TYPE lvc_fname
                                short      TYPE scrtext_s
                                medium     TYPE scrtext_m
                                long       TYPE scrtext_l,
      user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_module IMPLEMENTATION.
  METHOD f4_variant.

    lv_save = 'A'.
    ls_variant-report = sy-repid.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant = ls_variant
        i_save     = lv_save
      IMPORTING
        e_exit     = lv_exit
        es_variant = gs_variant
      EXCEPTIONS
        not_found  = 2.

    IF sy-subrc EQ 2.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF lv_exit EQ space.
        p_vari = gs_variant-variant.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD variant_exist.
    DATA : lv_save(1) TYPE c.

    IF NOT p_vari IS INITIAL.

      MOVE p_vari TO gs_variant-variant.

      lv_save = 'A'.
      gs_variant-report = sy-repid.
      CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
        EXPORTING
          i_save     = lv_save
        CHANGING
          cs_variant = gs_variant.

    ELSE.
      init_variant( ).
    ENDIF.
  ENDMETHOD.
  METHOD init_variant.
    DATA : lv_save(1) TYPE c.

    CLEAR : gs_variant.

    lv_save = 'A'.
    gs_variant-report = sy-repid.

    CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
      EXPORTING
        i_save        = lv_save
      CHANGING
        cs_variant    = gs_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.

    IF sy-subrc EQ 0.
      p_vari = gs_variant-variant.
    ENDIF.
  ENDMETHOD.
  METHOD process.
    get_data( ).
    process_data( ).

    CHECK lt_final[] IS NOT INITIAL.

    SORT lt_final[] BY lifnr werks.

    display_alv( ).

  ENDMETHOD.
  METHOD get_data.

    SELECT param1,paramval
    FROM z6mma_params
    INTO TABLE @it_param
    WHERE progname = 'ZMM026'.


    LOOP AT it_param INTO DATA(wa_param).
      s_hkont-option = 'EQ'.
      s_hkont-sign = 'I'.
      s_hkont-low = wa_param-param1.
      APPEND s_hkont TO s_hkont[].
    ENDLOOP.

    IF s_hkont IS NOT INITIAL.
      SELECT bukrs hkont belnr gjahr BUZEI blart
      zuonr bldat xblnr shkzg dmbtr werks
      FROM bsis INTO CORRESPONDING FIELDS OF TABLE it_bsis
        WHERE bukrs IN s_bukrs
        AND belnr IN s_belnr
        AND hkont IN s_hkont
        AND bldat IN s_bldat
        AND gjahr IN s_gjahr
        AND werks IN s_werks
        AND blart = 'WE'.
      IF it_bsis IS NOT INITIAL .

        SELECT a~bukrs a~belnr a~gjahr a~buzei a~zuonr a~menge a~meins
               a~matnr a~ebeln a~ebelp a~lifnr a~dmbtr b~ekgrp
          FROM bseg AS a
          JOIN ekko AS b
          ON a~ebeln = b~ebeln
          INTO CORRESPONDING FIELDS OF TABLE it_bseg
          FOR ALL ENTRIES IN it_bsis
          WHERE a~bukrs = it_bsis-bukrs
          AND a~belnr = it_bsis-belnr
          AND a~gjahr = it_bsis-gjahr
          AND a~BUZEI = it_bsis-BUZEI
          AND a~hkont IN s_hkont.

      ELSE.
        MESSAGE 'No Data Found' TYPE 'E'.

      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD process_data.

    LOOP AT it_bseg INTO ls_bseg  .

      ls_final-lifnr = ls_bseg-lifnr.
      ls_final-zuonr = ls_bseg-zuonr.
      ls_final-menge = ls_bseg-menge.
      ls_final-meins = ls_bseg-meins.
      ls_final-matnr = ls_bseg-matnr.
      ls_final-ebeln = ls_bseg-ebeln.
      ls_final-ebelp = ls_bseg-ebelp.
      ls_final-dmbtr = ls_bseg-dmbtr.
      ls_final-ekgrp = ls_bseg-ekgrp.
      SELECT SINGLE maktx FROM makt INTO ls_final-maktx WHERE matnr = ls_bseg-matnr.

      READ TABLE it_bsis INTO ls_bsis WITH KEY bukrs = ls_bseg-bukrs belnr = ls_bseg-belnr gjahr = ls_bseg-gjahr .
      IF sy-subrc = 0.
        ls_final-bukrs = ls_bsis-bukrs.
        ls_final-belnr = ls_bsis-belnr.
        ls_final-gjahr = ls_bsis-gjahr.
        ls_final-bldat = ls_bsis-bldat.
        ls_final-xblnr = ls_bsis-xblnr.
        ls_final-blart = ls_bsis-blart.
        ls_final-werks = ls_bsis-werks.
      ENDIF.

      SELECT SINGLE name1 ,adrnr FROM lfa1 INTO (  @ls_final-name1 , @DATA(zadrnr) ) WHERE lifnr = @ls_bseg-lifnr.
      IF zadrnr IS NOT INITIAL.

        SELECT SINGLE smtp_addr FROM adr6 INTO ls_final-vend_mail_id WHERE addrnumber = zadrnr  .
      ENDIF.

      SELECT SINGLE paramval FROM z6mma_params INTO ls_final-user_mail_id WHERE progname = 'ZMM026' AND param1 = ls_final-ekgrp.


      APPEND ls_final TO lt_final.
      CLEAR: ls_final.
    ENDLOOP.

  if s_lifnr is NOT INITIAL .
    delete lt_final WHERE lifnr NOT in s_lifnr.
  endif.

  ENDMETHOD.
  METHOD call_sf.

    "call smartform
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'ZMMS_INFORM_VEND_INV_SUBMIT'
*       VARIANT            = ' '
*       DIRECT_CALL        = ' '
      IMPORTING
        fm_name            = lv_fm
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    control-no_dialog = 'X'.
    control-getotf    = 'X'.

*    control-no_dialog = ' '.
*    control-preview   = 'X'.
*    control-no_open   = 'X'.
*    control-no_close  = 'X'.

    CALL FUNCTION lv_fm
      EXPORTING
        control_parameters = control
        output_options     = output_opt
      IMPORTING
        job_output_info    = job_output_info
      TABLES
        lt_final           = lt_final1
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'CONVERT_OTF'
        EXPORTING
          format                = 'PDF'
        TABLES
          otf                   = job_output_info-otfdata
          lines                 = lt_pdf
        EXCEPTIONS
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          err_bad_otf           = 4
          OTHERS                = 5.
      IF sy-subrc EQ 0.
        LOOP AT lt_pdf INTO DATA(ls_pdf).
          ASSIGN ls_pdf TO <fs_x> CASTING.
          CONCATENATE ls_content <fs_x> INTO ls_content IN BYTE MODE.
        ENDLOOP.

        CHECK ls_content IS NOT INITIAL.
        attachment-att_type =  'BIN'.
        attachment-att_subj = condense( |Indofil_Invoice_Submission({ ls_final-lifnr ALPHA = OUT }).pdf| )."-{ ls_final-werks }
        attachment-att_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = ls_content ).
        APPEND attachment TO attachments.
        CLEAR attachment.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD display_alv.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display   = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table   = lo_table
          CHANGING
            t_table        = lt_final
        ).

        IF lo_table IS BOUND.

          lo_table->set_screen_status(
            EXPORTING
              report        = 'ZMM_INFORM_VEND_INV_SUBMIT'
              pfstatus      = 'STANDARD'
              set_functions = lo_table->c_functions_all ).

          lo_functions = lo_table->get_functions( ).
          IF lo_functions IS BOUND.
            lo_functions->set_all( value = if_salv_c_bool_sap=>true ).
          ENDIF.

          lo_display = lo_table->get_display_settings( ).
          IF lo_display IS BOUND.
            lo_display->set_striped_pattern( value = if_salv_c_bool_sap=>true ).
          ENDIF.

          lo_columns = lo_table->get_columns( ).
          IF lo_columns IS BOUND.
*            CLEAR: columnname, short, medium, long.
            change_col_text( EXPORTING columnname = 'BUKRS' short      = 'Comp Code.'
                             medium     = 'Company Code'
                             long       = 'Company Code' ).
            change_col_text( EXPORTING columnname = 'BELNR' short      = 'Docmnt.No'
                             medium     = 'Document Number'
                             long       = 'Document Number' ).
            change_col_text( EXPORTING columnname = 'GJAHR' short      = 'FI Year'
                             medium     = 'Fiscal Year'
                             long       = 'Fiscal Year' ).
            change_col_text( EXPORTING columnname = 'BLART' short      = 'Doc.Type'
                             medium     = 'Document type'
                             long       = 'Document type' ).
            change_col_text( EXPORTING columnname = 'LIFNR' short      = 'Vendor'
                             medium     = ' Vendor Code'
                             long       = ' Vendor Code' ).
            change_col_text( EXPORTING columnname = 'WERKS' short      = 'Plant'
                             medium     = 'Plant'
                             long       = 'Plant' ).
            change_col_text( EXPORTING columnname = 'BLDAT' short      = 'Docmnt.Dt'
                             medium     = 'Document Date'
                             long       = 'Document Date' ).
            change_col_text( EXPORTING columnname = 'XBLNR' short      = 'Referance'
                             medium     = 'Referance Number'
                             long       = 'Referance Numner' ).
            change_col_text( EXPORTING columnname = 'ZUONR' short      = 'Assignment'
                             medium     = 'Assignment'
                             long       = 'Assignment' ).
            change_col_text( EXPORTING columnname = 'MENGE' short      = 'Quantity'
                             medium     = 'Quantity'
                             long       = 'Quantity' ).
            change_col_text( EXPORTING columnname = 'MEINS' short      = 'UOM'
                             medium     = 'UOM'
                             long       = 'UOM' ).
            change_col_text( EXPORTING columnname = 'MATNR' short      = 'Material'
                             medium     = 'Material'
                             long       = 'Material' ).
            change_col_text( EXPORTING columnname = 'MAKTX' short      = 'Mat.Descr.'
                             medium     = 'Material Descr.'
                             long       = 'Material Descr.' ).
            change_col_text( EXPORTING columnname = 'EBELN' short      = 'PO Number'
                             medium     = 'Purchase Order'
                             long       = 'Purchase Order' ).
            change_col_text( EXPORTING columnname = 'EBELP' short      = 'PO Item'
                             medium     = 'Purchase Order line '
                             long       = 'Purchase Order Line Item' ).
            change_col_text( EXPORTING columnname = 'VEND_MAIL_ID' short      = 'Vendor ID'
                             medium     = 'Vendor Mail ID'
                             long       = 'Vendor Mail ID' ).
            change_col_text( EXPORTING columnname = 'USER_MAIL_ID' short      = 'User ID'
                             medium     = 'User Mail ID'
                             long       = 'User Mail ID' ).
          ENDIF.

          lo_layout = lo_table->get_layout( ).
          IF lo_layout IS BOUND.
            ls_key-report = sy-cprog.

            lo_layout->set_key( value = ls_key ).

            lo_layout->set_save_restriction( value = if_salv_c_layout=>restrict_none ).
          ENDIF.

          lo_selection = lo_table->get_selections( ).
          IF lo_selection IS BOUND.
            lo_selection->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).
          ENDIF.

          lo_event = lo_table->get_event( ).
          IF lo_event IS BOUND.
            SET HANDLER user_command FOR lo_event.
          ENDIF.

          lo_table->display( ).
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.

  ENDMETHOD.
  METHOD change_col_text .
    TRY.
        lo_column ?= lo_columns->get_column( columnname = columnname ).
        lo_column->set_short_text( value = short ).
        lo_column->set_medium_text( value = medium ).
        lo_column->set_long_text( value = long ).

        IF columnname CS 'MAIL_ID'.
          lo_column->set_output_length( value = '35' ).
        ENDIF.
      CATCH cx_salv_not_found.
    ENDTRY.
  ENDMETHOD.
  METHOD send_mail.
    "mail body
    REFRESH body.
    CLEAR wa_body.
    wa_body-line = 'Dear Business Partner'.
    APPEND wa_body TO body.
    APPEND INITIAL LINE TO body.

    CLEAR wa_body.
    wa_body-line = 'Please find attached details of Invoice Submission'.
    APPEND wa_body TO body.
    APPEND INITIAL LINE TO body.

    "subject
    CLEAR subject.
    subject = condense( |Indofil Invoice Submission({ ls_final-lifnr ALPHA = OUT })| ).

    "add receipients
    "vendor's mail id
    CLEAR recipient.
    REFRESH recipients.

    recipient-recipient = ls_final-vend_mail_id.
    recipient-copy = abap_false.
    APPEND recipient TO recipients.

    "user mail id
    DATA(lt_mail_cc) = lt_final[].
    DELETE lt_mail_cc[] WHERE lifnr NE ls_final-lifnr.
    SORT lt_mail_cc[] BY ekgrp.
    DELETE ADJACENT DUPLICATES FROM lt_mail_cc[] COMPARING ekgrp.

    LOOP AT lt_mail_cc INTO DATA(ls_mail_cc).
      CLEAR recipient.
      TRY.
          recipient-recipient = it_param[ param1 = ls_mail_cc-ekgrp ]-paramval."ls_mail_cc-user_mail_id.
          recipient-copy = abap_true.
          APPEND recipient TO recipients.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDLOOP.

    IF subject IS NOT INITIAL AND recipients IS NOT INITIAL.
      CLEAR sent.
      NEW zcl_email( )->send_email( EXPORTING subject = subject
                                              sender         = 'sapautomail@indofil.com'
                                              body           = body
                                              body_obj_type  = 'RAW'
                                              recipients     = recipients
                                              attachments    = attachments
*                                                                     COND #( WHEN ls_content IS NOT INITIAL
*                                                                     THEN VALUE #( ( att_type  = 'BIN'
*                                                                     att_subj = |Returns_of_indofil.pdf|
*                                                                     att_solix =
*                                                                     cl_bcs_convert=>xstring_to_solix( iv_xstring = ls_content ) ) )
                                    IMPORTING
                                              sent           = sent ).

      MESSAGE COND #( WHEN sent EQ abap_true THEN 'Email sent successfully' ELSE 'Email sending failed' ) TYPE 'S'.
    ENDIF.
  ENDMETHOD.

  METHOD user_command.
    TYPES: BEGIN OF ty_lifnr,
             lifnr TYPE lfa1-lifnr,
           END OF ty_lifnr.
    DATA :lt_select TYPE salv_t_row,
          lrt_lifnr TYPE TABLE OF ty_lifnr.
    IF e_salv_function EQ 'MAIL'.
      CLEAR ls_final.
      REFRESH: lt_final1[],attachments[],lt_select,lrt_lifnr.

      lt_select = lo_selection->get_selected_rows( ).
      IF lt_select IS INITIAL.
        MESSAGE 'Please select a Vendor to send mail' TYPE 'E'.
      ENDIF.

      CLEAR ls_final.
      LOOP AT lt_select INTO DATA(ls_select).
        TRY.
            APPEND VALUE #( lifnr = lt_final[ ls_select ]-lifnr )
            TO lrt_lifnr[].
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDLOOP.
      SORT lrt_lifnr[] BY lifnr.
      DELETE ADJACENT DUPLICATES FROM lrt_lifnr[] COMPARING lifnr.

      LOOP AT lrt_lifnr[] INTO DATA(lrs_lifnr).
        REFRESH lt_final1[].
        lt_final1[] = lt_final[].
        CLEAR ls_final.
        READ TABLE lt_final INTO ls_final WITH KEY lifnr = lrs_lifnr-lifnr.
        DELETE lt_final1[] WHERE lifnr NE ls_final-lifnr.
        IF lt_final1[] IS NOT INITIAL.
          call_sf( ).
          IF attachments[] IS NOT INITIAL.
            send_mail( ).
            REFRESH: attachments[],lt_final1[].
          ENDIF.
        ENDIF.
      ENDLOOP.
*      TRY.
*            ls_final =  lt_final[ ls_select ] .
*
*            REFRESH lt_final1[].
*            lt_final1[] = lt_final[].
*            DELETE lt_final1[] WHERE lifnr NE ls_final-lifnr.
*            IF lt_final1[] IS NOT INITIAL.
*              call_sf( ).
*              IF attachments[] IS NOT INITIAL.
*                send_mail( ).
*                REFRESH: attachments[],lt_final1[].
*              ENDIF.
*            ENDIF.
*          CATCH cx_sy_itab_line_not_found.
*        ENDTRY.

*      SORT lrt_lifnr[] BY lifnr.
*      DELETE ADJACENT DUPLICATES FROM lrt_lifnr[] COMPARING lifnr.
*
*      SORT lt_final[] BY lifnr.

*      LOOP AT lt_final INTO ls_final.
*        ls_final1 = ls_final.
*        IF line_exists( lrt_lifnr[ lifnr = ls_final-lifnr ] ).
*          APPEND ls_final TO lt_final1.
*          at end of werks.
*            call_sf( ).
*          endat.
*          AT END OF lifnr.
*            call_sf( ).
*            IF attachments[] IS NOT INITIAL.
*              send_mail( ).
*              REFRESH: attachments[],lt_final1[].
*            ENDIF.
*          ENDAT.
*        ENDIF.
*      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

LOAD-OF-PROGRAM.
  DATA lo_mod TYPE REF TO lcl_module.
  lo_mod = NEW #( ).

START-OF-SELECTION.

  IF lo_mod IS BOUND.
    lo_mod->process( ).
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  lo_mod->f4_variant( ).

AT SELECTION-SCREEN.
  lo_mod->variant_exist( ).
