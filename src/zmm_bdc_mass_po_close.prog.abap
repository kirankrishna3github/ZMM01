report zmm_bdc_mass_po_close
       no standard page heading line-size 255.

* Include bdcrecx1_s:
* The call transaction using is called WITH AUTHORITY-CHECK!
* If you have own auth.-checks you can use include bdcrecx1 instead.
*include bdcrecx1_s.

tables: sscrfields.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*     Batchinputdata of single transaction
data: bdcdata like bdcdata occurs 0 with header line.
*     messages of call transaction
data: messtab like bdcmsgcoll occurs 0 with header line.
*     error session opened (' ' or 'X')
data: nodata type c length 1 value '/'.

data: gt_msg_settings type standard table of massmsgcfg with empty key.

data: gv_execute type flag.

selection-screen begin of screen 1001 title title as window.
selection-screen: skip,

                  begin of line,
                    pushbutton 5(20) dwn_btn user-command dwn,
                    comment 30(40) text-dwn,
                  end of line,

                  skip,

                  begin of line,
                    comment 5(70) text-wrn,
                  end of line.
selection-screen end of screen 1001.

call selection-screen 1001 starting at 50 10.

initialization.
  title = sy-title.

at selection-screen output.
  dwn_btn = '@49@ Download'.

at selection-screen.
  case sscrfields-ucomm.
    when 'DWN'.
      perform dwn_file_format.
    when 'CRET'.
      gv_execute = abap_true.
  endcase.

start-of-selection.
  if gv_execute eq abap_true.
    perform build_bdcdata.
    perform switch_off_warnings tables gt_msg_settings.
    perform call_bdc.
    perform reset_warnings tables gt_msg_settings.
    perform display_log.
  else.
    message 'Processing cancelled by user' type 'S' display like 'E'.
  endif.
  clear gv_execute.
*&---------------------------------------------------------------------*
*& Form DWN_FILE_FORMAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form dwn_file_format.
*  data: lv_answer type c length 1.
*
*  clear lv_answer.
*  call function 'POPUP_TO_CONFIRM'
*    exporting
*      titlebar              = 'Download file format?'
*      text_question         = 'Do you want to download the file format?'
*      display_cancel_button = abap_false
*    importing
*      answer                = lv_answer
*    exceptions
*      text_not_found        = 1
*      others                = 2.
*  if sy-subrc <> 0.
** Implement suitable error handling here
*  endif.
*
*  if lv_answer eq '1'.
  call function 'CALL_BROWSER'
    exporting
      url                    = 'https://tinyurl.com/y5zjz25z'
      window_name            = 'ZMM096_File_Format'
      new_window             = abap_true
    exceptions
      frontend_not_supported = 1
      frontend_error         = 2
      prog_not_found         = 3
      no_batch               = 4
      unspecified_error      = 5
      others                 = 6.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
*  endif.

*  message 'Press "Continue (Enter)" to select the upload file.' type 'I'.

endform.

*&---------------------------------------------------------------------*
*& Form BUILD_BDCDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form build_bdcdata .
*--------------------------------------------------------------------*
*   build bdcdata
*--------------------------------------------------------------------*
  refresh: bdcdata[].
  perform bdc_dynpro      using 'SAPMMSDL' '0100'.
  perform bdc_field       using 'BDC_CURSOR'
                                'MASSSCREEN-OBJECT'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=NEXT'.
  perform bdc_field       using 'MASSSCREEN-OBJECT'
                                'bus2012'.

  perform bdc_dynpro      using 'SAPMMSDL' '0200'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=UPLO'.
  perform bdc_field       using 'BDC_CURSOR'
                                'MASSTABS-TABTXT(02)'.
  perform bdc_field       using 'MASSTABS-MARK(02)'
                                'X'.

  perform bdc_dynpro      using 'SAPLMASS_SPREADSHEET_IMPORT' '0100'.
  perform bdc_field       using 'BDC_CURSOR'
                                'GV_HDRLT'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=CRET'.
  perform bdc_field       using 'GV_DECFM'
                                'X'.
  perform bdc_field       using 'GV_DATFM'
                                '1'.
  perform bdc_field       using 'GV_TIMFM'
                                '0'.
  perform bdc_field       using 'GV_LANGU'
                                'EN'.
  perform bdc_field       using 'GV_HDRLF'
                                ''.
  perform bdc_field       using 'GV_HDRLT'
                                'X'.
  perform bdc_field       using 'GV_CINIT'
                                '~'.

  perform bdc_dynpro      using 'SAPLMASS_SPREADSHEET_IMPORT' '0200'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=EXECUTE'.

  perform bdc_dynpro      using 'SAPLMASSINTERFACE' '0200'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=SAVE'.
  perform bdc_field       using 'BDC_CURSOR'
                                'HEADER_STRUC-FIELD2-VALUE-LEFT(01)'.
endform.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
endform.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
  if fval <> nodata.
    clear bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    append bdcdata.
  endif.
endform.

*&---------------------------------------------------------------------*
*& Form CALL_BDC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form call_bdc .
*--------------------------------------------------------------------*
*   call transaction for BDC
*--------------------------------------------------------------------*
  data(ls_options) = value ctu_params( dismode  = 'N'
                                       updmode  = 'L'
                                       racommit = 'X'
                                       nobinpt  = 'X' ). " IHDK904119
*  perform bdc_transaction using 'MASS'.
  refresh messtab[].
  call transaction 'MASS' without authority-check   " IHDK904417
                          using bdcdata[]
                          messages into messtab[]
                          options from ls_options.
endform.
*&---------------------------------------------------------------------*
*& Form DISPLAY_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form display_log .
*--------------------------------------------------------------------*
*   display log
*--------------------------------------------------------------------*
  if line_exists( messtab[ msgid = 'MASS_SPSH' msgnr = '019' ] ).
    message 'No file selected for upload.'  type 'S' display like 'E'.
    exit.
  endif.

  select single max( extnumber )
    from balhdr
    into @data(lv_log_ext_num)
    where object eq 'MASS'
    and   subobject eq 'BUS2012'
    and   aluser eq @sy-uname
    and   aldate eq @sy-datum.

  if lv_log_ext_num is not initial.
    data: lv_logs type i.
    clear lv_logs.
    call function 'APPL_LOG_READ_DB'
      exporting
        object          = 'MASS'
        subobject       = 'BUS2012'
        external_number = lv_log_ext_num
        user_id         = conv balhdr-aluser( sy-uname )
      importing
        number_of_logs  = lv_logs.

    check lv_logs ne 0.
    call function 'APPL_LOG_DISPLAY'
      exporting
        object                    = 'MASS'
        subobject                 = 'BUS2012'
        external_number           = lv_log_ext_num
        suppress_selection_dialog = abap_true
      exceptions
        no_authority              = 1
        others                    = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
  else.
    try.
        data(ls_message) = messtab[ 1 ].
        message id ls_message-msgid type ls_message-msgtyp number ls_message-msgnr
          with ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4.
      catch cx_sy_itab_line_not_found into data(lox_line_not_found).
        message 'No log generated' type 'S' display like 'E'.
    endtry.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form SWITCH_OFF_WARNINGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form switch_off_warnings tables rt_msg_settings structure massmsgcfg .
  " backup existing settings
  select *
    from massmsgcfg
    where uname = @sy-uname
    and   arbgb = 'M&'
    into table @rt_msg_settings.

  " delete existing settings
  submit massmsbk with arbgb = 'M&' and return.

  " switch off all popup messages
  insert massmsgcfg from table @( value #( ( uname = sy-uname arbgb = 'M&' msgnr = '302' )
                                           ( uname = sy-uname arbgb = 'M&' msgnr = '303' )
                                           ( uname = sy-uname arbgb = 'M&' msgnr = '304' )
                                           ( uname = sy-uname arbgb = 'M&' msgnr = '305' )
                                           ( uname = sy-uname arbgb = 'M&' msgnr = '308' ) ) ).
  if sy-dbcnt >= 1.
    commit work and wait.
  endif.
endform.
*&---------------------------------------------------------------------*
*& Form RESET_WARNINGS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_MSG_SETTINGS
*&---------------------------------------------------------------------*
form reset_warnings tables it_msg_settings structure massmsgcfg.
  " delete modified settings
  submit massmsbk with arbgb = 'M&' and return.

  " restore previous user settings
  if it_msg_settings is not initial.
    insert massmsgcfg from table it_msg_settings.
    if sy-dbcnt >= 1.
      commit work and wait.
    endif.
  endif.
endform.
