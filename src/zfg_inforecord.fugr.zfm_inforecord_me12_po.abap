FUNCTION ZFM_INFORECORD_ME12_PO.
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
*"     VALUE(LIFNR_001) LIKE  BDCDATA-FVAL DEFAULT 'V1101'
*"     VALUE(MATNR_002) LIKE  BDCDATA-FVAL
*"     VALUE(EKORG_003) LIKE  BDCDATA-FVAL
*"     VALUE(WERKS_004) LIKE  BDCDATA-FVAL
*"     VALUE(NORMB_005) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(INFNR_015) LIKE  BDCDATA-FVAL
*"     VALUE(EKGRP_011) LIKE  BDCDATA-FVAL DEFAULT '302'
*"     VALUE(MWSKZ_012) LIKE  BDCDATA-FVAL DEFAULT 'G3'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0100'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-LIFNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINA-LIFNR'
                                lifnr_001.
  PERFORM bdc_field       USING 'EINA-MATNR'
                                matnr_002.
  PERFORM bdc_field       USING 'EINE-EKORG'
                                ekorg_003.
  PERFORM bdc_field       USING 'EINE-WERKS'
                                werks_004.
  PERFORM bdc_field       USING 'RM06I-NORMB'
                                normb_005.
  PERFORM bdc_field       USING 'EINA-INFNR'
                                infnr_015.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0101'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINA-MAHN1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.


  PERFORM bdc_dynpro      USING 'SAPMM06I' '0102'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINE-MWSKZ'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'EINE-EKGRP'
                                ekgrp_011.
  PERFORM bdc_field       USING 'EINE-MWSKZ'
                                mwskz_012.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0105'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'EINE-ANGNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_dynpro      USING 'SAPMM06I' '0103'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RM06I-LTEX1(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

  PERFORM bdc_dynpro      USING 'SAPLSPO1' '0100'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=YES'.
  PERFORM bdc_transaction TABLES messtab
  USING                         'ME12'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.
ENDFUNCTION.
*INCLUDE bdcrecxy .
