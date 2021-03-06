*&---------------------------------------------------------------------*
*&  Include           ZINC_MEK1_CONDN_PROCESS
*&---------------------------------------------------------------------*

FORM a445_process .
  IF it_a445[] IS NOT INITIAL.
    LOOP AT it_a445 INTO wa_a445.
      MOVE-CORRESPONDING wa_a445 TO wa_tab_445.
      APPEND wa_tab_445 TO it_tab_445.
      CLEAR: wa_tab_445, wa_a445.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a505_process .
  IF it_a505[] IS NOT INITIAL.
    LOOP AT it_a505 INTO wa_a505.
      MOVE-CORRESPONDING wa_a505 TO wa_tab_505.
      APPEND wa_tab_505 TO it_tab_505.
      CLEAR: wa_tab_505, wa_a505.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a507_process .
  IF it_a507[] IS NOT INITIAL.
    LOOP AT it_a507 INTO wa_a507.
      MOVE-CORRESPONDING wa_a507 TO wa_tab_507.
      APPEND wa_tab_507 TO it_tab_507.
      CLEAR: wa_tab_507, wa_a507.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a516_process .
  IF it_a516[] IS NOT INITIAL.
    LOOP AT it_a516 INTO wa_a516.
      MOVE-CORRESPONDING wa_a516 TO wa_tab_516.
      APPEND wa_tab_516 TO it_tab_516.
      CLEAR: wa_tab_516, wa_a516.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a518_process .
  IF it_a518[] IS NOT INITIAL.
    LOOP AT it_a518 INTO wa_a518.
      MOVE-CORRESPONDING wa_a518 TO wa_tab_518.
      APPEND wa_tab_518 TO it_tab_518.
      CLEAR: wa_tab_518, wa_a518.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a540_process .
  IF it_a540[] IS NOT INITIAL.
    LOOP AT it_a540 INTO wa_a540.
      MOVE-CORRESPONDING wa_a540 TO wa_tab_540.
      APPEND wa_tab_540 TO it_tab_540.
      CLEAR: wa_tab_540, wa_a540.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a541_process .
  IF it_a541[] IS NOT INITIAL.
    LOOP AT it_a541 INTO wa_a541.
      MOVE-CORRESPONDING wa_a541 TO wa_tab_541.
      APPEND wa_tab_541 TO it_tab_541.
      CLEAR: wa_tab_541, wa_a541.
    ENDLOOP.
  ENDIF.
ENDFORM.
