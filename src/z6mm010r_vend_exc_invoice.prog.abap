*&---------------------------------------------------------------------*
*& Report  Z6MM004R_VEND_EX_INV
*&
*&---------------------------------------------------------------------*
REPORT  Z6MM010R_VEND_EXC_INVOICE.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Vendor Return Excise invoice
* OBJECT TYPE       : Report Program    FUNC. CONSULTANT: Girish
*          DEVELOPER: Ramakrishna Konda
*      CREATION DATE: 25.06.2010
*        DEV REQUEST: IRDK900207
*             TCODE : ZMM001
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
TABLES : j_1iexchdr,j_1iexcdtl.
*&---------------------------------------------------------------------*
selection-screen begin of block s01 with FRAME TITLE TEXT-S01.
PARAMETERS : p_exnum  LIKE j_1iexchdr-exnum OBLIGATORY,
             p_exyear  LIKE j_1iexchdr-exyear OBLIGATORY,
             p_werks  LIKE j_1iexchdr-werks OBLIGATORY.

SELECTION-SCREEN END OF BLOCK S01.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  SELECT  SINGLE *
    FROM j_1iexchdr
    WHERE exnum  = p_exnum
    AND   werks  = p_werks
    AND   exyear = p_exyear
    AND   trntyp = 'OTHR'
    AND   status NE 'D'.
  IF sy-subrc NE 0.
    MESSAGE e398(00) WITH  'Please Enter valid Document Number'.
  ENDIF.

*&---------------------------------------------------------------------*
*& start of selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: fm_name TYPE rs38l_fnam.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'Z6MM010S_VEND_EXC_NEW' "'Z6MM010S_VEND_EXC_INVOICEN'  "'Z6MM004S_VEND_EX_INV'
    IMPORTING
      fm_name            = fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION fm_name
    EXPORTING
      p_exnum  = p_exnum
      p_exyear = p_exyear
      p_werks  = p_werks.


*&---------------------------------------------------------------------*
*& end of selection
*&---------------------------------------------------------------------*
END-OF-SELECTION.
