*&---------------------------------------------------------------------*
*& Include          ZMM_BAPI_MM02_BASICDATA1_FOEM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form F_CLEAR_VARIABLES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_clear_variables .

  CLEAR: lw_data,lw_headdata,lw_unitsofmeasure,lw_unitsofmeasurex,
         lw_clientdata,lw_clientdatax,lw_return,lw_msg,lw_materialdescription,
         lw_storagelocationdata,lw_storagelocationdatax.

  REFRESH: lt_data,lt_unitsofmeasure,lt_unitsofmeasurex,lt_clientdata,
           lt_clientdatax,lt_return,lt_msg,lt_materialdescription.

ENDFORM.

FORM set_title. " IHDK901136
  SELECT SINGLE ttext FROM tstct INTO sy-title WHERE tcode EQ sy-tcode AND sprsl EQ sy-langu.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_GET_FILEPATH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_filepath .

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = sy-repid
      dynpro_number = sy-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

  IF p_file IS INITIAL.
**    MESSAGE 'Please Choose the File Path' TYPE 'I'.
    MESSAGE TEXT-002 TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DWN_FILE_FMT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_dwn_file_fmt .

  IF sy-ucomm = 'FMT'.
    " IHDK903877
    CASE sy-tcode.
      WHEN 'ZMM090'.
        DATA(lv_url) = CONV char256( 'https://drive.google.com/file/d/1deR9QrUaJrzS77Fc4ZYRsT0Qi7HKBQUt/view' ).
      WHEN 'ZMM093'.
        lv_url = 'https://drive.google.com/file/d/1nElBU10sD3d-F24zXwXVn4Wr5T-WSKue/view'.
      WHEN OTHERS.
    ENDCASE.
    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = lv_url
        window_name            = CONV char100( |Download { to_upper( sy-tcode ) } File Format| ) " IHDK901028
        new_window             = 'X'
      EXCEPTIONS
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_REAT_FILE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_reat_file_data .

  IF p_file IS NOT INITIAL.

    DATA: i_filename TYPE rlgrap-filename,
          lt_rawdata TYPE truxs_t_text_data.

    i_filename = p_file.

    IF sy-tcode EQ 'ZMM090'.

      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_line_header        = 'X'
          i_tab_raw_data       = lt_rawdata[]
          i_filename           = i_filename
        TABLES
          i_tab_converted_data = lt_data[]
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.

      "added by varun on 15.11.19 as said by saurabh
      "for ZMM093 the upload file format is different.
      "so, a new structure is created and finally moved to lt_data for furture process.
    ELSEIF sy-tcode EQ 'ZMM093'.
      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_line_header        = 'X'
          i_tab_raw_data       = lt_rawdata[]
          i_filename           = i_filename
        TABLES
          i_tab_converted_data = it_file[]
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF it_file[] IS NOT INITIAL.
        lt_data[] = CORRESPONDING #( it_file[] ).
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_CALL_BAPI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_call_bapi .

  LOOP AT lt_data INTO lw_data.

*--- Header Data
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lw_data-matnr
      IMPORTING
        output = lw_headdata-material.

    lw_headdata-material_long = lw_headdata-material.
    lw_headdata-basic_view    = abap_true.            "'X'

    CLEAR lw_mara.
    READ TABLE lt_mara INTO lw_mara WITH KEY matnr = lw_headdata-material.

    IF sy-tcode = 'ZMM093'.
*--- Material Text
      IF lw_mara-mtart = 'ZEU2' OR lw_mara-mtart = 'ZMCO' OR lw_mara-mtart = 'ZSPR'.
        IF lw_data-maktx IS NOT INITIAL.  " IHDK901134
          CLEAR : lw_materialdescription.
          lw_materialdescription-langu     = sy-langu.
          lw_materialdescription-langu_iso = sy-langu.
          lw_materialdescription-matl_desc = lw_data-maktx.
          APPEND lw_materialdescription TO lt_materialdescription.
        ENDIF.

        IF lw_data-lgpbe IS NOT INITIAL.                  " Update / Change Storage Bin
          lw_headdata-storage_view         = abap_true.            "'X'
          lw_storagelocationdata-plant     = lw_data-werks.
          lw_storagelocationdata-stge_loc  = lw_data-lgort.
          lw_storagelocationdata-stge_bin  = lw_data-lgpbe.

          lw_storagelocationdatax-plant    = lw_data-werks.        "abap_true.            "'X'
          lw_storagelocationdatax-stge_loc = lw_data-lgort.        "abap_true.            "'X'
          lw_storagelocationdatax-stge_bin = abap_true.            "'X'
        ENDIF.

      ELSE.
        lw_msg-flag    = '1'.
        lw_msg-matnr   = lw_data-matnr.
        lw_msg-type    = 'E'.
        lw_msg-message = 'Not authorized to change material type:' && ` ` && lw_mara-mtart.
        APPEND lw_msg TO lt_msg.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF sy-tcode = 'ZMM090'.
*--- Net Wt., Volum and Volum unit AND Passing X parameters
      IF lw_data-brgew IS NOT INITIAL.
        lw_unitsofmeasure-gross_wt  = lw_data-brgew.

        lw_unitsofmeasure-alt_unit     = lw_mara-meins. "'KG'.
        lw_unitsofmeasure-alt_unit_iso = lw_mara-meins. "'KG'.
        lw_unitsofmeasure-unit_of_wt   = lw_mara-gewei. "'KG'.

        lw_unitsofmeasurex-alt_unit     = lw_mara-meins. "'KG'.
        lw_unitsofmeasurex-alt_unit_iso = lw_mara-meins. "'KG'.
        lw_unitsofmeasurex-gross_wt     = abap_true.            "'X'
        lw_unitsofmeasurex-unit_of_wt   = abap_true.            "'X'
      ENDIF.

      IF lw_data-volum IS NOT INITIAL.
        lw_unitsofmeasure-volume  = lw_data-volum.
        lw_unitsofmeasurex-volume = abap_true.            "'X'
      ENDIF.

      IF lw_data-voleh IS NOT INITIAL.
        lw_unitsofmeasure-volumeunit  = lw_data-voleh.
        lw_unitsofmeasurex-volumeunit = abap_true.            "'X'
      ENDIF.

      IF lw_unitsofmeasure IS NOT INITIAL AND lw_unitsofmeasurex IS NOT INITIAL.
        APPEND lw_unitsofmeasure TO lt_unitsofmeasure.
        APPEND lw_unitsofmeasurex TO lt_unitsofmeasurex.
      ENDIF.

*--- Net Wt. and Passing X Parameters
      IF lw_data-ntgew IS NOT INITIAL.
        lw_clientdata-net_weight  = lw_data-ntgew.
        lw_clientdata-unit_of_wt  = lw_unitsofmeasure-unit_of_wt. "'KG'.
        lw_clientdatax-net_weight = abap_true.
        lw_clientdatax-unit_of_wt = lw_unitsofmeasure-unit_of_wt. "'KG'.
      ENDIF.

*--- Material description and Passing X Parameters; IHDK901024
      IF lw_data-maktx IS NOT INITIAL.
        CLEAR : lw_materialdescription.
        lw_materialdescription-langu     = sy-langu.
        lw_materialdescription-langu_iso = sy-langu.
        lw_materialdescription-matl_desc = lw_data-maktx.
        APPEND lw_materialdescription TO lt_materialdescription.
      ENDIF.

*--- External material group and Passing X Parameters; IHDK901024
      IF lw_data-extwg IS NOT INITIAL.
        lw_clientdata-extmatlgrp  = lw_data-extwg.
        lw_clientdatax-extmatlgrp = abap_true.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata             = lw_headdata
        clientdata           = lw_clientdata
        clientdatax          = lw_clientdatax
        storagelocationdata  = lw_storagelocationdata
        storagelocationdatax = lw_storagelocationdatax
      IMPORTING
        return               = lw_return
      TABLES
        materialdescription  = lt_materialdescription[]
        unitsofmeasure       = lt_unitsofmeasure[]
        unitsofmeasurex      = lt_unitsofmeasurex[].

    IF lw_return-type = 'S' AND lw_return-number = '356'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      lw_msg-flag    = '3'.
      lw_msg-matnr   = lw_data-matnr.
      lw_msg-type    = lw_return-type.
      lw_msg-message = lw_return-message.
      APPEND lw_msg TO lt_msg.

    ELSE.
      lw_msg-flag    = '1'.
      lw_msg-matnr   = lw_data-matnr.
      lw_msg-type    = lw_return-type.
      lw_msg-message = lw_return-message.
      APPEND lw_msg TO lt_msg.
    ENDIF.

    CLEAR: lw_data,lw_headdata,lw_clientdata,lw_clientdatax,lw_msg,lw_return,
           lw_materialdescription,lw_mara.
    REFRESH: lt_unitsofmeasure,lt_unitsofmeasurex,lt_return,lt_materialdescription.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_FIELDCAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_fieldcat .

  CLEAR: lw_fieldcat.
  lw_fieldcat-fieldname = 'FLAG'.
  lw_fieldcat-seltext_l = 'Status'.
  APPEND lw_fieldcat TO lt_fieldcat.

  CLEAR: lw_fieldcat.
  lw_fieldcat-fieldname = 'MATNR'.
  lw_fieldcat-seltext_l = 'Material'.
  APPEND lw_fieldcat TO lt_fieldcat.

  CLEAR: lw_fieldcat.
  lw_fieldcat-fieldname = 'TYPE'.
  lw_fieldcat-seltext_l = 'Message Type'.
  APPEND lw_fieldcat TO lt_fieldcat.

  CLEAR: lw_fieldcat.
  lw_fieldcat-fieldname = 'MESSAGE'.
  lw_fieldcat-seltext_l = 'Message Text'.
  APPEND lw_fieldcat TO lt_fieldcat.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_DISPLAY_MSG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_display_msg .

  lw_layout-colwidth_optimize = 'X'.
  lw_layout-zebra             = 'X'.
  lw_layout-lights_fieldname  = 'FLAG'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      is_layout              = lw_layout
      it_fieldcat            = lt_fieldcat[]
    TABLES
      t_outtab               = lt_msg[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.

  DATA: lt_list TYPE slis_t_listheader,
        lw_list LIKE LINE OF lt_list.

  CLEAR lw_list.
  lw_list-typ   = 'H'.
  lw_list-info  = TEXT-004. "'Indofil Industries Ltd.'.
  APPEND lw_list TO lt_list.

  CLEAR lw_list.
  lw_list-typ   = 'S'.
  lw_list-info  = TEXT-005. "'Update Material Master Data'.
  APPEND lw_list TO lt_list.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_list[]
*     I_LOGO             =
    .
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_validation .
  BREAK abap01.
  " is this required??
*  IF sy-tcode = 'ZMM093'.
  REFRESH lt_mara.
  LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<lw_data>).
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <lw_data>-matnr
      IMPORTING
        output = <lw_data>-matnr.

    CLEAR lw_mara.
    lw_mara-matnr = <lw_data>-matnr.
    APPEND lw_mara TO lt_mara.

    CLEAR: lw_data.
  ENDLOOP.
  UNASSIGN <lw_data>.

  IF lt_mara IS NOT INITIAL.
    SELECT matnr
           mtart
           meins
           gewei
     FROM mara
     INTO TABLE lt_mara
     FOR ALL ENTRIES IN lt_mara
     WHERE matnr = lt_mara-matnr.

  ENDIF.
*  ENDIF.

  " auth check; IHDK901024
  CLEAR lw_data.
  LOOP AT lt_data INTO lw_data.
    TRY.
        DATA(lv_mtart) = lt_mara[ matnr = lw_data-matnr ]-mtart.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
    CLEAR lw_msg.
    AUTHORITY-CHECK OBJECT 'M_MATE_MAR'
      ID 'ACTVT' DUMMY
      ID 'BEGRU' FIELD lv_mtart.

    IF sy-subrc <> 0. " not auth for this material type
      CLEAR lw_msg.
      lw_msg-flag    = '1'.
      lw_msg-matnr   = lw_data-matnr.
      lw_msg-type    = 'E'.
      lw_msg-message = 'Not authorized to change material type:' && ` ` && lw_mara-mtart.
      APPEND lw_msg TO lt_msg.
      DELETE lt_data WHERE matnr = lw_data-matnr.
    ENDIF.

    "added by varun on 15.11.19
    "plant auth check.
    IF lw_data-werks IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
      ID 'ACTVT' DUMMY
      ID 'WERKS' FIELD lw_data-werks.
      IF sy-subrc NE 0.
        CLEAR lw_msg.
        lw_msg-flag    = '1'.
        lw_msg-matnr   = lw_data-werks.
        lw_msg-type    = 'E'.
        lw_msg-message = 'Not authorized to change Plant:' && ` ` && lw_data-werks.
        APPEND lw_msg TO lt_msg.
        DELETE lt_data WHERE werks = lw_data-werks.
      ENDIF.
    ENDIF.

    CLEAR lw_data.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_SEL_SCREEN
*&---------------------------------------------------------------------*
*& IHDK904429
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form modify_sel_screen .
  select single @abap_true
    from usr05
    where bname = @sy-uname
    and   parid = 'ZMM090_93_DWN'
    and   parva = @abap_true
    into @data(lv_enable_dwn).

  if lv_enable_dwn <> abap_true.
    loop at screen.
      if screen-group1 = 'DWN'.
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  endif.
endform.
