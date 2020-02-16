FUNCTION zfm_j_1ig_inv.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(P_BUKRS_001) LIKE  BDCDATA-FVAL
*"     VALUE(P_SWERKS_002) LIKE  BDCDATA-FVAL
*"     VALUE(P_WERKS_003) LIKE  BDCDATA-FVAL
*"     VALUE(R1_004) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(LOW_005) LIKE  BDCDATA-FVAL
*"     VALUE(LOW_006) LIKE  BDCDATA-FVAL
*"  EXPORTING
*"     REFERENCE(SUBRC) TYPE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL
*"----------------------------------------------------------------------
  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'J_1IG_INB_INV_STO' '1000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'S_DATE-LOW'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ONLI'.
  PERFORM bdc_field       USING 'P_BUKRS'
                                p_bukrs_001.
  PERFORM bdc_field       USING 'P_SWERKS'
                                p_swerks_002.
  PERFORM bdc_field       USING 'P_WERKS'
                                p_werks_003.
  PERFORM bdc_field       USING 'R1'
                                r1_004.
  PERFORM bdc_field       USING 'S_VBELN-LOW'
                                low_005.
  PERFORM bdc_field       USING 'S_DATE-LOW'
                                low_006.
  PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                '04/03'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=&GST'.
  PERFORM bdc_transaction TABLES messtab
  USING                         'J_1IG_INV'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.





ENDFUNCTION.
INCLUDE bdcrecxy .
