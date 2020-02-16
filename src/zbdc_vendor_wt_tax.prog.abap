REPORT zbdc_vendor_wt_tax
       NO STANDARD PAGE HEADING LINE-SIZE 255.

INCLUDE zbdcrecx1_xk01.
TYPES: BEGIN OF ty_file,
         lifnr(10),
         bukrs(4),
       END OF ty_file.

DATA: wa         TYPE ty_file,
      it         TYPE TABLE OF ty_file,
      it_type    TYPE truxs_t_text_data,
      it_bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE.

DATA: lv_file TYPE string.
DATA: lv_file1 TYPE rlgrap-filename.
DATA: gv_stcd3 TYPE lfa1-stcd3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.
  CLEAR file.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = file.

  lv_file  = file.
  lv_file1 = file.    " For Excel upload file type.

START-OF-SELECTION.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = it_type
      i_filename           = lv_file1
    TABLES
      i_tab_converted_data = it
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  PERFORM open_group.
  LOOP AT it INTO wa.
    PERFORM bdc_dynpro      USING 'SAPMF02K' '0101'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RF02K-D0610'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'RF02K-LIFNR'
                                  wa-lifnr."'10001963'.
    PERFORM bdc_field       USING 'RF02K-BUKRS'
                                  wa-bukrs."'1000'.
    PERFORM bdc_field       USING 'RF02K-D0610'
                                  'X'.
    PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=LDEL'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFBW-WITHT(03)'.
*    PERFORM bdc_field       USING 'LFB1-QLAND'
*                                  'IN'.
    PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=UPDA'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFBW-WITHT(03)'.
*    PERFORM bdc_field       USING 'LFB1-QLAND'
*                                  'IN'.
    PERFORM bdc_transaction USING 'XK02'.
  ENDLOOP.
  PERFORM close_group.
