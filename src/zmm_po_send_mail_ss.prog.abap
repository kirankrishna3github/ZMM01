*&---------------------------------------------------------------------*
*&  Include           ZMM_PO_SEND_MAIL_SS
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK detail WITH FRAME TITLE text-001.
  PARAMETERS     : p_BSART TYPE ekko-bsart OBLIGATORY.
  SELECT-OPTIONS : s_AEDAT FOR  ekko-AEDAT OBLIGATORY,
                   s_lifnr FOR  lfa1-lifnr .
SELECTION-SCREEN END OF BLOCK detail.

SELECTION-SCREEN BEGIN OF BLOCK layout WITH FRAME TITLE text-002.
PARAMETERS: p_vari LIKE disvariant-variant. " ALV Variant
SELECTION-SCREEN END OF BLOCK layout.
