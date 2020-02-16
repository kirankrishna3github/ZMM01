*&---------------------------------------------------------------------*
*& Report  ZSEND_MAIL_WITH_ATTACHMENT
*&
*&---------------------------------------------------------------------*
*& Created By : Amol Bhagwat
*& Created Date : 07.04.2017
*&---------------------------------------------------------------------*
REPORT zsend_mail_with_attachment.

*&---------------------------------------------------------------------*
* VARIABLES
*&---------------------------------------------------------------------*
DATA: gv_repid TYPE sy-repid,
      gv_length TYPE i,
      gv_len TYPE so_obj_len,
      gv_file TYPE string,
      gv_result TYPE os_boolean,
      o_document TYPE REF TO cl_document_bcs,
      o_doc TYPE REF TO cl_bcs,
      o_recipient TYPE REF TO if_recipient_bcs.

*&---------------------------------------------------------------------*
* INTERNAL TABLES
*&---------------------------------------------------------------------*
DATA: BEGIN OF it_mailhex OCCURS 0,
        raw(255) TYPE x,
      END OF it_mailhex.

DATA: it_mail TYPE solix_tab,
      wa_mail TYPE LINE OF solix_tab.

DATA: it_textpool TYPE TABLE OF textpool,
      wa_textpool TYPE textpool OCCURS 0.

DATA: it_contents TYPE TABLE OF solisti1,
      wa_contents TYPE solisti1.

DATA: it_addr TYPE bcsy_resv,
      wa_addr TYPE LINE OF bcsy_resv.

DATA: BEGIN OF it_str OCCURS 0,
        str TYPE c LENGTH 50,
      END OF it_str.

*&---------------------------------------------------------------------*
* SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b.
  PARAMETERS: p_file TYPE ibipparms-path.
SELECTION-SCREEN END OF BLOCK b.

*&---------------------------------------------------------------------*
* AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

 CALL FUNCTION 'F4_FILENAME'
  EXPORTING
    program_name        = syst-cprog
    dynpro_number       = syst-dynnr
*    FIELD_NAME          = ' '
  IMPORTING
    file_name           = p_file.

  gv_file = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                      = gv_file
      filetype                      = 'BIN'
*     HAS_FIELD_SEPARATOR           = ' '
*     HEADER_LENGTH                 = 0
*     READ_BY_LINE                  = 'X'
*     DAT_MODE                      = ' '
*     CODEPAGE                      = ' '
*     IGNORE_CERR                   = ABAP_TRUE
*     REPLACEMENT                   = '#'
*     CHECK_BOM                     = ' '
*     VIRUS_SCAN_PROFILE            =
*     NO_AUTH_CHECK                 = ' '
    IMPORTING
      filelength                    = gv_length
*     HEADER                        =
    TABLES
      data_tab                      = it_mailhex
*   CHANGING
*     ISSCANPERFORMED               = ' '
    EXCEPTIONS
      file_open_error               = 1
      file_read_error               = 2
      no_batch                      = 3
      gui_refuse_filetransfer       = 4
      invalid_type                  = 5
      no_authority                  = 6
      unknown_error                 = 7
      bad_data_format               = 8
      header_not_allowed            = 9
      separator_not_allowed         = 10
      header_too_long               = 11
      unknown_dp_error              = 12
      access_denied                 = 13
      dp_out_of_memory              = 14
      disk_full                     = 15
      dp_timeout                    = 16
      OTHERS                        = 17
            .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ELSE.
    gv_len = gv_length.
    CONDENSE gv_len.
  ENDIF.

*&---------------------------------------------------------------------*
* INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

  gv_repid = sy-repid.

*&---------------------------------------------------------------------*
* START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  SPLIT gv_file AT '\' INTO TABLE it_str.
  DESCRIBE TABLE it_str LINES gv_len.
  READ TABLE it_str INDEX gv_len.

  CLEAR it_contents[].
  wa_contents-line = text-001.
  APPEND wa_contents TO it_contents.

  TRY.
    CALL METHOD cl_document_bcs=>create_document
      EXPORTING
        i_type         = 'HTM'
        i_subject      = text-003
        i_length       = gv_len
        i_language     = sy-langu
        i_importance   = '1'
*        i_sensitivity  =
        i_text         = it_contents
*        i_hex          =
*        i_header       =
*        i_sender       =
*        iv_vsi_profile =
      RECEIVING
        result         = o_document.
   CATCH cx_document_bcs .
  ENDTRY.

  LOOP AT it_mailhex.

    wa_mail-line = it_mailhex-raw.
    APPEND wa_mail TO it_mail.

  ENDLOOP.  "LOOP AT it_mailhex

  TRY.
    CALL METHOD o_document->add_attachment
      EXPORTING
        i_attachment_type     = 'EXT'
        i_attachment_subject  = it_str-str
*        i_attachment_size     =
*        i_attachment_language = SPACE
*        i_att_content_text    =
        i_att_content_hex     = it_mail
*        i_attachment_header   =
*        iv_vsi_profile        =
  .
     CATCH cx_document_bcs .
  ENDTRY.

  TRY.
    CALL METHOD cl_bcs=>create_persistent
      RECEIVING
        result = o_doc.
   CATCH cx_send_req_bcs .
  ENDTRY.

  TRY.
    CALL METHOD o_doc->set_status_attributes
      EXPORTING
        i_requested_status = 'N'
*        i_status_mail      = 'E'
      .
   CATCH cx_send_req_bcs .
  ENDTRY.

  TRY.
    CALL METHOD o_doc->set_send_immediately
      EXPORTING
        i_send_immediately = 'X'.
   CATCH cx_send_req_bcs .
  ENDTRY.

  TRY.
    CALL METHOD o_doc->set_document
      EXPORTING
        i_document = o_document.
   CATCH cx_send_req_bcs .
  ENDTRY.

  TRY.
    CALL METHOD cl_cam_address_bcs=>create_internet_address
      EXPORTING
        i_address_string = text-005
*        i_address_name   =
*        i_incl_sapuser   =
      RECEIVING
        result           = o_recipient.
   CATCH cx_address_bcs .
  ENDTRY.

  TRY.
    CALL METHOD o_doc->add_recipient
      EXPORTING
        i_recipient  = o_recipient
        i_express    = 'X'
*        i_copy       =
*        i_blind_copy =
*        i_no_forward =
        .
   CATCH cx_send_req_bcs .
  ENDTRY.

  TRY.
    CALL METHOD o_doc->send
      EXPORTING
        i_with_error_screen = 'X'
      RECEIVING
        result              = gv_result
      .
   CATCH cx_send_req_bcs .
  ENDTRY.

  COMMIT WORK.
