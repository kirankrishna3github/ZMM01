FUNCTION zfm_xk02_dms_doc.
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
*"     VALUE(LIFNR_001) LIKE  BDCDATA-FVAL DEFAULT '10001475'
*"     VALUE(DOKNR_01_013) LIKE  BDCDATA-FVAL DEFAULT '10000000075'
*"     VALUE(DOKVR_01_014) LIKE  BDCDATA-FVAL DEFAULT '00'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

* Auhtor: SaurabhK
* Date: Sunday, January 07, 2018 20:54:23
* TR: IRDK930677
* Purpose: Update DMS Doc in vendor master (used in ZNEW_VENDOR)

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF02K-D0110'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RF02K-LIFNR'
                                lifnr_001.
  PERFORM bdc_field       USING 'RF02K-D0110'
                                'X'.  " Address

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0110'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=DOKA'.

  PERFORM bdc_dynpro      USING 'SAPLCVOB' '0200'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'DRAW-DOKNR(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=GOON'.
  PERFORM bdc_field       USING 'DRAW-DOKAR(01)'
                                'Z13'.  " Doc type for Vendor
  PERFORM bdc_field       USING 'DRAW-DOKNR(01)'
                                doknr_01_013.
  PERFORM bdc_field       USING 'DRAW-DOKTL(01)'
                                '000'.
  PERFORM bdc_field       USING 'DRAW-DOKVR(01)'
                                dokvr_01_014.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0110'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPDA'.


  PERFORM bdc_transaction TABLES messtab
  USING                         'XK02'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.

ENDFUNCTION.
