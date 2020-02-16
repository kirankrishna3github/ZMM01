REPORT zmm_condition_record_fv11_jiig
       NO STANDARD PAGE HEADING LINE-SIZE 255.
TYPE-POOLS: truxs.

INCLUDE zbdcrecx1_vk11.

TYPES: BEGIN OF ty_file,
         kschl      TYPE kscha,     " UTX1
*         selkz      TYPE selkz_list," X
         lland      TYPE lland,     " IN
         regio      TYPE regio,     " 13 A797
         wkreg      TYPE wkreg,     " 13 A797
         ven_class  TYPE j_1igtakld,"A797
         taxim      TYPE taxim1,    "A797
*         taxk1      TYPE taxk1,     " 1
*         taxm1      TYPE taxm1,     " 1
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
DATA: lv_file1 TYPE rlgrap-filename,
      len_r1,
      len_r2.

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

len_r1 = strlen( wa-regio ).
IF len_r1 NE '2'.
**  Only for single character region Eg: '6' becomes '006'

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
   input         = wa-regio
 IMPORTING
   output        = wa-regio.
SHIFT wa-regio LEFT BY 1 PLACES.
ENDIF.

len_r2 = strlen( wa-wkreg ).
IF len_r2 NE '2'.
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
   input         = wa-wkreg
 IMPORTING
   output        = wa-wkreg.
SHIFT wa-wkreg LEFT BY 1 PLACES.
ENDIF.
** KONP-KBETR(1) is longer the screen field
CONDENSE wa-kbetr.

IF wa-kschl = 'JIIG'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JIIG'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                              wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'0'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'   5'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.

ELSEIF wa-kschl = 'JICG'.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JICG'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                             wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'1'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.

ELSEIF wa-kschl = 'JISG'.

PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JISG'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                              wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'1'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.

ELSEIF wa-kschl = 'JIMR'.

  PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JIMR'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                              wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'1'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.

ELSEIF wa-kschl = 'JISR'.

PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JISR'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                              wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'1'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.

ELSEIF wa-kschl = 'JICR'.

PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JICR'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                              wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'1'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.


ELSEIF wa-kschl = 'JIMD'.
*** file format / structure is different !!
**  ZMM_CONDITION_RECORD_JIMD


ELSEIF wa-kschl = 'JIRS'.

PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JIRS'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                              wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'1'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.

ELSEIF wa-kschl = 'JIRC'.

PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV13A-KSCHL'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'RV13A-KSCHL'
                              wa-kschl."'JIRC'.
PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'RV130-SELKZ(06)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=WEIT'.
PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                              ''.
PERFORM bdc_field       USING 'RV130-SELKZ(06)'
                              'X'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KONP-KONWA(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '/00'.
PERFORM bdc_field       USING 'KOMG-LLAND'
                              wa-lland."'IN'.
PERFORM bdc_field       USING 'KOMG-REGIO'
                              wa-regio."'13'.
PERFORM bdc_field       USING 'KOMG-WKREG(01)'
                              wa-wkreg."'13'.
PERFORM bdc_field       USING 'KOMG-VEN_CLASS(01)'
                              wa-ven_class."'1'.
PERFORM bdc_field       USING 'KOMG-TAXIM(01)'
                              wa-taxim."'1'.
PERFORM bdc_field       USING 'KOMG-STEUC(01)'
                              wa-steuc."'28151200'.
PERFORM bdc_field       USING 'KOMG-MWSKZ(01)'
                              wa-mwskz."'01'.
PERFORM bdc_field       USING 'KONP-KBETR(01)'
                              wa-kbetr."'              10'.
PERFORM bdc_field       USING 'KONP-KONWA(01)'
                              wa-konwa."'INR'.
PERFORM bdc_dynpro      USING 'SAPMV13A' '1797'.
PERFORM bdc_field       USING 'BDC_CURSOR'
                              'KOMG-WKREG(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'
                              '=SICH'.
PERFORM bdc_transaction USING 'FV11'.

ENDIF.
ENDLOOP.
PERFORM close_group.
