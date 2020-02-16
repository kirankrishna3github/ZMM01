REPORT ybdc_jimd
       NO STANDARD PAGE HEADING LINE-SIZE 255.
TYPE-POOLS: truxs.

INCLUDE zbdcrecx1_vk11.

TYPES: BEGIN OF ty_file,
         kschl      TYPE kscha,     " UTX1
*         selkz      TYPE selkz_list," X
         lifnr      TYPE lifnr,     " IN
         werks      TYPE werks_d,     " 13 A797
         steuc      TYPE steuc,   " A797
*         kbetr      TYPE kbetr_kond,"amount
         mwskz      TYPE mwskz,     "A1
         kbetr(13),"      TYPE kbetr_kond,
         konwa      TYPE konwa,
*          datbi     TYPE kodatbi,
       END OF ty_file.


DATA: wa         TYPE ty_file,
      it         TYPE TABLE OF ty_file,
      it_type    TYPE truxs_t_text_data,
      it_bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE.

DATA: lv_file TYPE string.
DATA: lv_file1 TYPE rlgrap-filename.

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
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
 EXPORTING
   input        = wa-mwskz
 IMPORTING
   output       = wa-mwskz.

PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JIMD'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(05)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(05)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1806'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LIFNR'
                              wa-lifnr."'10000290'.
PERFORM bdc_field       USING 'KOMG-WERKS(01)'
                              wa-werks."'1101'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1806'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WERKS(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.
ENDLOOP.
PERFORM close_group.
