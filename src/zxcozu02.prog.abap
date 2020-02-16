*&---------------------------------------------------------------------*
*&  Include           ZXCOZU02
*&---------------------------------------------------------------------*


******Change by Monika on 14th Octtober 2011*************
*1. To Make field “BEDNR“ mandatory in Transaction “CJ20N”.
BREAK MM01.
DATA: C_E TYPE C VALUE 'E'.
IF  SY-TCODE EQ 'CJ20N'.
  IF RESBD_IMP-BEDNR IS INITIAL.
    MESSAGE TEXT-001 TYPE C_E DISPLAY LIKE C_E.
  ENDIF.
ENDIF.


*2.	To Make field Requirement qty <MENGE> non-editable
* in Transaction “CJ20N” if purchase requisition is released.

*IF EBAN_IMP-FRGKZ EQ '2'.
