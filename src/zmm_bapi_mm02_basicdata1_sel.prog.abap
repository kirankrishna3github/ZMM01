*&---------------------------------------------------------------------*
*& Include          ZMM_BAPI_MM02_BASICDATA1_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS P_FILE TYPE IBIPPARMS-PATH.
SELECTION-SCREEN END OF BLOCK B1.

*-- Sample File Format
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-006.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 2(15) TEXT-007 USER-COMMAND FMT modif id DWN. " IHDK904429
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.
