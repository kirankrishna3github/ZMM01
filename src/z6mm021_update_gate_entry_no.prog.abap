*&---------------------------------------------------------------------*
*& REPORT  Z6MM_UPDATE_GATE_ENTRY_NO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6MM_UPDATE_GATE_ENTRY_NO.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION:
* OBJECT TYPE       :                 FUNC. CONSULTANT  :
*          DEVELOPER:
*      CREATION DATE:   DD.MM.YYYY
*        DEV REQUEST:
*  TCODE            :
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
TABLES Z6MMA_GT_ENT_HD.

DATA : IT_Z6MMA_GT_ENT_HD LIKE STANDARD TABLE OF Z6MMA_GT_ENT_HD.

START-OF-SELECTION.

  SELECT * FROM Z6MMA_GT_ENT_HD INTO TABLE IT_Z6MMA_GT_ENT_HD.

  CLEAR  Z6MMA_GT_ENT_HD.
  LOOP AT IT_Z6MMA_GT_ENT_HD INTO Z6MMA_GT_ENT_HD.
    IF Z6MMA_GT_ENT_HD-GATEN+4(1) = ''.
      Z6MMA_GT_ENT_HD-GATEN = ''.
    ELSEIF Z6MMA_GT_ENT_HD-GATEN+0(4) = Z6MMA_GT_ENT_HD-GATEN+4(4).
      Z6MMA_GT_ENT_HD-GATEN = Z6MMA_GT_ENT_HD-GATEN+4(14).
    ENDIF.
    CONDENSE Z6MMA_GT_ENT_HD-GATEN .
    MODIFY IT_Z6MMA_GT_ENT_HD FROM Z6MMA_GT_ENT_HD TRANSPORTING GATEN.
  ENDLOOP.

  UPDATE  Z6MMA_GT_ENT_HD FROM TABLE IT_Z6MMA_GT_ENT_HD.
  if sy-subrc = 0.
    message 'data saved' type 'S'.
  endif.
