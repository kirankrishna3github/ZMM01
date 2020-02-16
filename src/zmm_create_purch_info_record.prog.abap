*&---------------------------------------------------------------------*
*& Report  ZMM_CREATE_PURCH_INFO_RECORD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmm_create_purch_info_record.

DATA: BEGIN OF wa_file,
        ekorg(4)  TYPE c,
        werks(4)  TYPE c,
        matnr(18) TYPE c,
        lifnr(10) TYPE c,
        ekgrp(3)  TYPE c,
        netpr     TYPE char20,
        mwskz(2)  TYPE c,
      END OF wa_file,
      it_file LIKE STANDARD TABLE OF wa_file,

      BEGIN OF wa_out,
        light(1) TYPE c,
        ekorg    TYPE ekorg,
        werks    TYPE werks_d,
        matnr    TYPE matnr,
        lifnr    TYPE lifnr,
        ekgrp    TYPE ekgrp,
        netpr    TYPE netpr,
        mwskz    TYPE mwskz,
        status   TYPE bapi_msg,
        comment  TYPE bapi_msg,
      END OF wa_out,
      it_out LIKE STANDARD TABLE OF wa_out,

      it_raw TYPE truxs_t_text_data.

DATA: ekorg TYPE bdcdata-fval,
      werks TYPE bdcdata-fval,
      matnr TYPE bdcdata-fval,
      lifnr TYPE bdcdata-fval,
      ekgrp TYPE bdcdata-fval,
      netpr TYPE bdcdata-fval,
      mwskz TYPE bdcdata-fval.

DATA:subrc    TYPE syst-subrc,
     messages TYPE STANDARD TABLE OF bdcmsgcoll,
     message  LIKE LINE OF messages.

DATA: o_table     TYPE REF TO cl_salv_table,
      o_container TYPE REF TO cl_gui_container,
      o_functions TYPE REF TO cl_salv_functions_list,
      o_columns   TYPE REF TO cl_salv_columns_table,
      o_column    TYPE REF TO cl_salv_column,
      o_layout    TYPE REF TO cl_salv_layout,
      o_layo      TYPE REF TO cl_salv_layout_service,
      o_key       TYPE salv_s_layout_key,
      o_info      TYPE salv_s_layout_info.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_file LIKE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

START-OF-SELECTION.
  PERFORM file_to_tab.

END-OF-SELECTION.
  PERFORM create_inforecord.
  PERFORM display_result.

*&---------------------------------------------------------------------*
*&      Form  FILE_TO_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_to_tab .
  IF p_file IS NOT INITIAL.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_field_seperator    = 'X'
        i_line_header        = 'X'
        i_tab_raw_data       = it_raw
        i_filename           = p_file
      TABLES
        i_tab_converted_data = it_file
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_INFORECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_inforecord .
  IF it_file IS NOT INITIAL.
    LOOP AT it_file INTO wa_file.
      CLEAR: ekorg,
             werks,
             matnr,
             lifnr,
             ekgrp,
             netpr,
             mwskz.

      MOVE wa_file-ekorg TO ekorg.
      MOVE wa_file-werks TO werks.
      MOVE wa_file-matnr TO matnr.
      MOVE wa_file-lifnr TO lifnr.
      MOVE wa_file-ekgrp TO ekgrp.
      MOVE wa_file-netpr TO netpr.
      MOVE wa_file-mwskz TO mwskz.

      MOVE-CORRESPONDING wa_file TO wa_out.

      CLEAR subrc.
      REFRESH messages.
      CALL FUNCTION 'ZFM_INFORECORD_ME11_PO'
        EXPORTING
*         mode      = 'N'
          lifnr_001 = lifnr  "'V1101'
          matnr_002 = matnr  "'20000096'
          ekorg_003 = ekorg  "'1000'
          werks_004 = werks  "'1201'
          ekgrp_011 = ekgrp  "'302'
          mwskz_012 = mwskz
          netpr_015 = netpr  "'             1'
        IMPORTING
          subrc     = subrc
        TABLES
          messtab   = messages.

      CLEAR message.
      READ TABLE messages INTO message WITH KEY msgtyp = 'E'.
      IF sy-subrc EQ 0.
        wa_out-light  = '1'.
        wa_out-status = 'Create failed'.
        CALL FUNCTION 'FORMAT_MESSAGE'
          EXPORTING
            id        = message-msgid
            lang      = '-D'
            no        = message-msgnr
            v1        = message-msgv1
            v2        = message-msgv2
            v3        = message-msgv3
            v4        = message-msgv4
          IMPORTING
            msg       = wa_out-comment
          EXCEPTIONS
            not_found = 1
            OTHERS    = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.
      ELSE.
        wa_out-light  = '3'.
        wa_out-status = 'Create successful'.
      ENDIF.
      APPEND wa_out TO it_out.
      CLEAR: wa_out.
      CLEAR wa_file.
    ENDLOOP.
  ELSE.
    MESSAGE 'No data uploaded' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_result .
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = o_table
        CHANGING
          t_table      = it_out.
    CATCH cx_salv_msg .
  ENDTRY.

  CALL METHOD o_table->get_columns
    RECEIVING
      value = o_columns.

  TRY .
      o_columns->set_exception_column( value = 'LIGHT' ).
    CATCH cx_salv_data_error.
  ENDTRY.

  CALL METHOD o_columns->set_optimize. " Default input bool true

  CALL METHOD o_table->get_functions
    RECEIVING
      value = o_functions.

  CALL METHOD o_functions->set_all. " Default input bool true

  CALL METHOD o_table->get_layout
    RECEIVING
      value = o_layout.

  o_key-report = sy-repid.

  CALL METHOD o_layout->set_key
    EXPORTING
      value = o_key.

  CALL METHOD o_layout->set_save_restriction.

  CALL METHOD o_table->display.
ENDFORM.
