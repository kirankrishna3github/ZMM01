*&---------------------------------------------------------------------*
*&  Include           ZINC_FV11_CONDN_BDC
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  A359_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM a359_bdc.
  IF it_tab_359[] IS NOT INITIAL.
    SORT it_tab_359 ASCENDING BY kschl werks.

    LOOP AT it_tab_359 INTO wa_tab_359.
      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_359-kschl."'JVRD'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(07)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                    ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(07)'
                                    'X'.

      PERFORM bdc_dynpro      USING 'RV13A359' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001'
                                    wa_tab_359-werks."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_359-matkl.       "'20005'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1359'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'FV12'.

      PERFORM close_group.

      CLEAR wa_tab_359.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  A504_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM a504_bdc .
  IF it_tab_504[] IS NOT INITIAL.

    SORT it_tab_504[] ASCENDING BY kschl.

    LOOP AT it_tab_504 INTO wa_tab_504.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_504-kschl."'JMOP'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                    'X'.

      PERFORM bdc_dynpro      USING 'RV13A504' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_504-werks."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_504-lifnr."'0010000027'.
      PERFORM bdc_field       USING 'F003-LOW'
                                    wa_tab_504-matnr."'000000000010001243'.
      PERFORM bdc_field       USING 'F004-LOW'
                                    wa_tab_504-mwskz."'05'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1504'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'FV12'.

      PERFORM close_group.

      CLEAR: wa_tab_504.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  A515_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM a515_bdc .
  IF it_tab_515[] IS NOT INITIAL.
    SORT it_tab_515[] ASCENDING BY kschl.

    LOOP AT it_tab_515 INTO wa_tab_515.
      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_515-kschl."'JMOP'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(02)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                    ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(02)'
                                    'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A515' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_515-werks."'1101'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_515-lifnr."'0010000027'.
      PERFORM bdc_field       USING 'F003-LOW'
                                    wa_tab_515-mwskz."'05'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1515'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'FV12'..

      PERFORM close_group.

      CLEAR: wa_tab_515.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  A519_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM a519_bdc .
  IF it_tab_519[] IS NOT INITIAL.
    SORT it_tab_519[] ASCENDING BY kschl.

    LOOP AT it_tab_519 INTO wa_tab_519.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_519-kschl."'JMOP'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(03)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                    ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(03)'
                                    'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A519' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_519-lifnr."'0010000027'.
      PERFORM bdc_field       USING 'F002-LOW'
                                    wa_tab_519-mwskz."'05'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1519'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'FV12'.

      PERFORM close_group.

      CLEAR: wa_tab_519.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  A536_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM a536_bdc .
  IF it_tab_536[] IS NOT INITIAL.
    SORT it_tab_536[] ASCENDING BY kschl.

    LOOP AT it_tab_536 INTO wa_tab_536.

      PERFORM open_group.

      PERFORM bdc_dynpro      USING 'SAPMV13A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-KSCHL'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ANTA'.
      PERFORM bdc_field       USING 'RV13A-KSCHL'
                                    wa_tab_536-kschl."'JMOP'.

      PERFORM bdc_dynpro      USING 'SAPLV14A' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV130-SELKZ(07)'.
      PERFORM bdc_field       USING 'RV130-SELKZ(01)'
                                    ''.
      PERFORM bdc_field       USING 'RV130-SELKZ(07)'
                                    'X'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=WEIT'.

      PERFORM bdc_dynpro      USING 'RV13A536' '1000'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SEL_DATE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ONLI'.
      PERFORM bdc_field       USING 'F001-LOW'
                                    wa_tab_536-mwskz."'05'.
      PERFORM bdc_field       USING 'SEL_DATE'
                                    v_datum."'30.03.2017'

      PERFORM bdc_dynpro      USING 'SAPMV13A' '1536'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'RV13A-DATBI(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'RV13A-DATBI(01)'
                                    '30.06.2017'.
      PERFORM bdc_transaction USING 'FV12'.

      PERFORM close_group.

      CLEAR: wa_tab_536.
    ENDLOOP.
  ENDIF.
ENDFORM.
