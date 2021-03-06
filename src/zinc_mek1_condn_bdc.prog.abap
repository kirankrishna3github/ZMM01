*&---------------------------------------------------------------------*
*&  Include           ZINC_MEK1_CONDN_BDC
*&---------------------------------------------------------------------*

FORM a445_bdc .
  IF it_tab_445[] IS NOT INITIAL.
    SORT it_tab_445 ASCENDING BY kschl.

    LOOP AT it_tab_445 INTO wa_tab_445.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_445-kschl."'ZEX1'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(03)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                     ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(03)'
                                     'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A445' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_445-matnr."'000000000020000001'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1445'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'MEK2'.

      PERFORM close_group.

      CLEAR: wa_tab_445.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a505_bdc .
  IF it_tab_505[] IS NOT INITIAL.
    SORT it_tab_505 ASCENDING BY kschl.

    LOOP AT it_tab_505 INTO wa_tab_505.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_505-kschl."'ZEX1'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(01)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                     'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A505' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_505-werks."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_505-lifnr."'0010000027'.
      PERFORM bdc_field       USING 'F003-LOW'
                                    wa_tab_505-matnr."'000000000010001243'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1505'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'MEK2'.

      PERFORM close_group.

      CLEAR: wa_tab_505.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a507_bdc .
  IF it_tab_507[] IS NOT INITIAL.
    SORT it_tab_507 ASCENDING BY kschl.

    LOOP AT it_tab_507 INTO wa_tab_507.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_507-kschl."'ZEX1'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(02)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                     ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(02)'
                                     'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A507' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_507-lifnr."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_507-werks."'0010000027'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1507'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'MEK2'.

      PERFORM close_group.

      CLEAR: wa_tab_507.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a516_bdc .
  IF it_tab_516[] IS NOT INITIAL.
    SORT it_tab_516 ASCENDING BY kschl.

    LOOP AT it_tab_516 INTO wa_tab_516.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_516-kschl."'ZEX1'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(02)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                     ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(02)'
                                     'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A516' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_516-werks."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_516-matnr."'0010000027'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1516'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'MEK2'.

      PERFORM close_group.

      CLEAR: wa_tab_516.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a518_bdc .
  IF it_tab_518[] IS NOT INITIAL.
    SORT it_tab_518 ASCENDING BY kschl.

    LOOP AT it_tab_518 INTO wa_tab_518.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_518-kschl."'ZEX1'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(01)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                     'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A518' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_518-reswk."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_518-matnr."'000000000020000001'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1518'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'MEK2'.

      PERFORM close_group.

      CLEAR: wa_tab_518.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a540_bdc .
  IF it_tab_540[] IS NOT INITIAL.
    SORT it_tab_540 ASCENDING BY kschl.

    LOOP AT it_tab_540 INTO wa_tab_540.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_540-kschl."'ZEX1'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(04)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                     ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(04)'
                                     'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A540' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_540-reswk."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_540-matnr."'000000000020000001'.
      PERFORM bdc_field       USING 'F003-LOW'
                                    wa_tab_540-werks."'1101'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1540'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'MEK2'.

      PERFORM close_group.

      CLEAR: wa_tab_540.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM a541_bdc .
  IF it_tab_541[] IS NOT INITIAL.
    SORT it_tab_541 ASCENDING BY kschl.

    LOOP AT it_tab_541 INTO wa_tab_541.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_541-kschl."'ZEX1'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(08)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                     ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(08)'
                                     'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A541' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_541-reswk."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_541-werks."'1101'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1541'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'MEK2'.

      PERFORM close_group.

      CLEAR: wa_tab_541.
    ENDLOOP.
  ENDIF.
ENDFORM.
