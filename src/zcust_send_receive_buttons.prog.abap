*&---------------------------------------------------------------------*
*& Report  ZCUST_SEND_RECEIVE_BUTTONS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zcust_send_receive_buttons.
TYPE-POOLS:slis.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
PARAMETERS : p_radio1 RADIOBUTTON GROUP rad1,
             p_radio2 RADIOBUTTON GROUP rad1,
             p_radio3 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK blk1   .

START-OF-SELECTION.

  PERFORM call_screen.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_screen .
  IF p_radio1 = 'X'.
    CALL TRANSACTION 'ZCONF_SEND'  .
  ELSEIF p_radio2 = 'X'.
    CALL TRANSACTION 'ZCUST_CONFIRM'.
  ELSEIF p_radio3 = 'X'.
    CALL TRANSACTION 'ZCUST_CONF'." 'ZCUSTCONF'.
  ENDIF.
ENDFORM.                    " CALL_SCREEN
