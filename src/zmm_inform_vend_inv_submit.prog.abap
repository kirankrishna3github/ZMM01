*&---------------------------------------------------------------------*
*& Report ZMM_INFORM_VEND_INV_SUBMIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMM_INFORM_VEND_INV_SUBMIT.
TABLES:BKPF , BSEG ,BSIS .


****Company code – BSIS-BUKRS
****Document Number – BSIS -BELNR
****Year - BSIS –GKAHR
****Document date – BSIS-BLDAT
****Plant – BSIS-WERKS



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_Lifnr FOR bseg-lifnr,
                s_bukrs FOR bkpf-bukrs no-EXTENSION no INTERVALS OBLIGATORY,
                s_belnr FOR bkpf-belnr,
                s_GJAHR FOR bkpf-gjahr no-EXTENSION no INTERVALS OBLIGATORY,
                s_bldat FOR BSIS-bldat OBLIGATORY,
                s_werks FOR BSIS-werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

"ALV variant
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.
