*&---------------------------------------------------------------------*
*& Report  ZMM_UPD_PURCH_INFOREC_TAXCODE
*&
*&---------------------------------------------------------------------*
*& Developer: SaurabhK
*& Date: Wednesday, October 04, 2017 16:28:17
*& Descr: Update missing taxcode, purch grp in inforecords related to STO's created between 1 july 2017 and 31 aug 2017
*&---------------------------------------------------------------------*
REPORT zmm_upd_purch_inforec_taxcode.

TYPE-POOLS: icon.

TABLES: ekko.

TYPES: BEGIN OF ty_data,
         ekorg TYPE ekorg,
         reswk TYPE reswk,
         matnr TYPE matnr,
         werks TYPE werks_d,
       END OF ty_data,

       BEGIN OF ty_inforec,
         infnr TYPE infnr,
         ekorg TYPE ekorg,
         werks TYPE werks_d,
         matnr TYPE matnr,
         lifnr TYPE lifnr,
         ekgrp TYPE ekgrp,
         netpr TYPE netpr,
         mwskz TYPE mwskz,
       END OF ty_inforec,

       BEGIN OF ty_out,
         light(1) TYPE c.       "Status traffic lights
        INCLUDE TYPE ty_inforec.
TYPES:  status  TYPE bapi_msg,  "log
        comment TYPE bapi_msg,  "bdc error log
        END OF ty_out.

DATA: it_data        TYPE STANDARD TABLE OF ty_data,
      wa_data        TYPE ty_data,

      it_inforec_all TYPE STANDARD TABLE OF ty_inforec,
      wa_inforec_all TYPE ty_inforec,

      it_inforec_ok  TYPE STANDARD TABLE OF ty_inforec,
      wa_inforec_ok  TYPE ty_inforec,

      it_inforec     TYPE STANDARD TABLE OF ty_inforec,
      wa_inforec     TYPE ty_inforec,

      it_out         TYPE STANDARD TABLE OF ty_out,
      wa_out         TYPE ty_out,

      messages       LIKE TABLE OF bdcmsgcoll,
      message        LIKE LINE OF messages.

DATA: subrc LIKE syst-subrc.
DATA: lifnr LIKE bdcdata-fval,
      matnr LIKE bdcdata-fval,
      ekorg LIKE bdcdata-fval,
      werks LIKE bdcdata-fval,
      infnr LIKE bdcdata-fval,
      ekgrp LIKE bdcdata-fval.

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
SELECT-OPTIONS: s_bedat FOR ekko-bedat.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM fetch_data.

END-OF-SELECTION.
  PERFORM update_data.
  IF it_out IS NOT INITIAL.
    PERFORM display_result.
  ELSE.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_data .
  REFRESH: it_data.
  "Possible combinations of purch org, suppl plant, material and recv plant in STO's
  SELECT DISTINCT ekorg reswk matnr_i werks_i
    FROM wb2_v_ekko_ekpo2
    INTO TABLE it_data
    WHERE bsart IN ('YSTO', 'ZSTO')
    AND   bedat IN s_bedat.

  "Convert data to inforec table format
  IF it_data IS NOT INITIAL.
    LOOP AT it_data INTO wa_data.
      MOVE: wa_data-ekorg TO wa_inforec_all-ekorg,
            wa_data-werks TO wa_inforec_all-werks,
            wa_data-matnr TO wa_inforec_all-matnr.
      wa_inforec_all-lifnr = 'V' && wa_data-reswk.
      CONDENSE wa_inforec_all-lifnr.
      APPEND wa_inforec_all TO it_inforec_all.
      CLEAR: wa_data, wa_inforec_all.
    ENDLOOP.
  ELSE.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.


  IF it_inforec_all IS NOT INITIAL.
    "Get inforec for all above combinations where tax code is not maintained or purch grp is not maintained
    SELECT infnr ekorg werks matnr lifnr ekgrp netpr mwskz
    FROM zv_eina
    INTO TABLE it_inforec
    FOR ALL ENTRIES IN it_inforec_all
    WHERE ekorg EQ it_inforec_all-ekorg
    AND   werks EQ it_inforec_all-werks
    AND   matnr EQ it_inforec_all-matnr
    AND   lifnr EQ it_inforec_all-lifnr
    AND   ( mwskz EQ '' OR ekgrp EQ '' )
    AND   loekz NE 'X'.
    "Get inforec for all above combinations where inforecord is all ok!
    SELECT infnr ekorg werks matnr lifnr ekgrp netpr mwskz
    FROM zv_eina
    INTO TABLE it_inforec_ok
    FOR ALL ENTRIES IN it_inforec_all
    WHERE ekorg EQ it_inforec_all-ekorg
    AND   werks EQ it_inforec_all-werks
    AND   matnr EQ it_inforec_all-matnr
    AND   lifnr EQ it_inforec_all-lifnr
    AND   ( mwskz NE '' AND ekgrp NE '' )
    AND   loekz NE 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_data .
  IF  it_inforec IS NOT INITIAL.
    LOOP AT it_inforec INTO wa_inforec.
      MOVE-CORRESPONDING wa_inforec TO wa_out.
      IF wa_inforec-ekgrp IS INITIAL.
        wa_inforec-ekgrp = '301'.
      ENDIF.

      CLEAR subrc.
      CLEAR: lifnr,
             matnr,
             ekorg,
             werks,
             infnr,
             ekgrp.
      MOVE: wa_inforec-lifnr TO lifnr,
            wa_inforec-matnr TO matnr,
            wa_inforec-ekorg TO ekorg,
            wa_inforec-werks TO werks,
            wa_inforec-infnr TO infnr,
            wa_inforec-ekgrp TO ekgrp.
      REFRESH: messages.
      "Update inforecord via BDC
      CALL FUNCTION 'ZFM_INFORECORD_ME12_PO'
        EXPORTING
*         mode      = 'N'
          lifnr_001 = lifnr  "'V1101'
          matnr_002 = matnr  "'20000096'
          ekorg_003 = ekorg  "'1000'
          werks_004 = werks  "'1201'
          infnr_015 = infnr
          ekgrp_011 = ekgrp  "'302'
*         mwskz_012 = 'G3'
        IMPORTING
          subrc     = subrc
        TABLES
          messtab   = messages.
      READ TABLE messages INTO message WITH KEY msgtyp = 'E'.
      IF sy-subrc EQ 0.
        wa_out-light  = '1'.
        wa_out-status = 'Update failed'.
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
        wa_out-mwskz = 'G3'.
        wa_out-status = 'Update successful'.
      ENDIF.
      APPEND wa_out TO it_out.
      CLEAR: wa_inforec, wa_out.
    ENDLOOP.
  ENDIF.

  "Add status of inforecords/combinations that were not processed(either because they are correct or becasue no inforec exists)
  LOOP AT it_inforec_all INTO wa_inforec_all.
    "Do not display combinations for which inforecord is correctly maintained
    READ TABLE it_inforec_ok INTO wa_inforec_ok WITH KEY ekorg = wa_inforec_all-ekorg
                                                         werks = wa_inforec_all-werks
                                                         matnr = wa_inforec_all-matnr
                                                         lifnr = wa_inforec_all-lifnr.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    "Display combinations for which no inforecord exists
    READ TABLE it_inforec INTO wa_inforec WITH KEY ekorg = wa_inforec_all-ekorg
                                                   werks = wa_inforec_all-werks
                                                   matnr = wa_inforec_all-matnr
                                                   lifnr = wa_inforec_all-lifnr.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING wa_inforec_all TO wa_out.
      wa_out-light = '1'.
      wa_out-comment = 'Inforecord does not exist'.
      wa_out-status  = 'Cannot process'.
      APPEND wa_out TO it_out.
    ENDIF.
    CLEAR: wa_inforec_all, wa_inforec, wa_out.
  ENDLOOP.

  SORT it_out DESCENDING BY infnr.
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
