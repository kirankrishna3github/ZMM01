*&---------------------------------------------------------------------*
*& Report  ZMM_CONSO_REG_HO_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_CONSO_REG_HO_DATA.


*&*******************************************************************************************&
*&                             DATA DECLARATIONS                                             &
*&*******************************************************************************************&

DATA : A TYPE ZMIS_SALES_DATA-VKORG,
       B TYPE ZMIS_SALES_DATA-VTWEG,
       C TYPE ZMIS_SALES_DATA-SPART,
       D TYPE ZMIS_SALES_DATA-werks,
       E TYPE ZMIS_SALES_DATA-ZMONTH,
       F TYPE ZMIS_SALES_DATA-ZYEAR.

DATA : WA_FINAL TYPE ZSD_REG_SALE_FC,
       IT_FINAL LIKE TABLE OF WA_FINAL,

       WA_FINAL1 TYPE ZSD_HO_SALE_FC,
       IT_FINAL1 LIKE TABLE OF WA_FINAL1.

 DATA R TYPE CHAR01.
 DATA ANSWER TYPE CHAR01.


*&*******************************************************************************************&
*&                             SELECTION SCREEN                                              &
*&*******************************************************************************************&

SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS : SO_VKORG  FOR A MODIF ID  PK  DEFAULT 1000,                           " SALES ORGAN
                 SO_VTWEG  FOR B MODIF ID  PK  DEFAULT 10,                             " DIST CHANNEL
                 SO_SPART  FOR C MODIF ID  PK  DEFAULT 10,                             " DIVISION
                 SO_WERKS  FOR D MODIF ID  PK, "NO INTERVALS NO-EXTENSION OBLIGATORY,  " PLANT
                 SO_ZMONT  FOR E MODIF ID  PK1 DEFAULT SY-DATUM+4(2) OBLIGATORY
                                               NO INTERVALS NO-EXTENSION ,             " MONTH
                 SO_ZYEAR  FOR F MODIF ID  PK1 DEFAULT SY-DATUM+0(4) OBLIGATORY
                                                NO INTERVALS NO-EXTENSION .            " YEAR


SELECTION-SCREEN END OF BLOCK B.
SELECTION-SCREEN END OF BLOCK A.

*----------------------------------- START OF SELECTION
START-OF-SELECTION.

CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
   TITLEBAR                    = 'Confirm '
   TEXT_QUESTION               = 'Are you sure to continue ?'
   TEXT_BUTTON_1               = 'Yes'
   TEXT_BUTTON_2               = 'No'
   DISPLAY_CANCEL_BUTTON       = ''
 IMPORTING
   ANSWER                      = ANSWER.

IF ANSWER = 1.
 PERFORM CONSOLIDATION.
ELSE.
  MESSAGE 'Cancelled by user' TYPE 'S' DISPLAY LIKE 'E'.
ENDIF.



*&*******************************************************************************************&
*&                             SUB ROUTIUNES                                                 &
*&*******************************************************************************************&

FORM CONSOLIDATION.

SELECT * FROM ZSD_REG_SALE_FC INTO TABLE IT_FINAL WHERE VKORG  IN SO_VKORG
                                                  AND   VTWEG  IN SO_VTWEG
                                                  AND   SPART  IN SO_SPART
                                                  AND   WERKS  IN SO_WERKS
                                                  AND   ZMONTH IN SO_ZMONT
                                                  AND   ZYEAR  IN SO_ZYEAR.

LOOP AT IT_FINAL INTO WA_FINAL.

MOVE-CORRESPONDING WA_FINAL TO WA_FINAL1.
COLLECT WA_FINAL1 INTO IT_FINAL1.
CLEAR : WA_FINAL, WA_FINAL1.

ENDLOOP.

IF IT_FINAL1 IS NOT INITIAL.

MODIFY ZSD_HO_SALE_FC FROM TABLE IT_FINAL1.
IF SY-SUBRC = 0.
MESSAGE 'Consolidation has been done' TYPE 'S'.
COMMIT WORK.
ENDIF.
ENDIF.

ENDFORM.
