function zfm_inforecord_me12_po.
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

  perform bdc_nodata      using nodata.

  perform open_group      using group user keep holddate ctu.

  perform bdc_dynpro      using 'SAPMM06I' '0100'.
  perform bdc_field       using 'BDC_CURSOR'
                                'EINA-LIFNR'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.
  perform bdc_field       using 'EINA-LIFNR'
                                lifnr_001.
  perform bdc_field       using 'EINA-MATNR'
                                matnr_002.
  perform bdc_field       using 'EINE-EKORG'
                                ekorg_003.
  perform bdc_field       using 'EINE-WERKS'
                                werks_004.
  perform bdc_field       using 'RM06I-NORMB'
                                normb_005.
  perform bdc_field       using 'EINA-INFNR'
                                infnr_015.

  perform bdc_dynpro      using 'SAPMM06I' '0101'.
  perform bdc_field       using 'BDC_CURSOR'
                                'EINA-MAHN1'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.


  perform bdc_dynpro      using 'SAPMM06I' '0102'.
  perform bdc_field       using 'BDC_CURSOR'
                                'EINE-MWSKZ'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.
  perform bdc_field       using 'EINE-EKGRP'
                                ekgrp_011.
  perform bdc_field       using 'EINE-MWSKZ'
                                mwskz_012.

  perform bdc_dynpro      using 'SAPMM06I' '0105'.
  perform bdc_field       using 'BDC_CURSOR'
                                'EINE-ANGNR'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.

  perform bdc_dynpro      using 'SAPMM06I' '0103'.
  perform bdc_field       using 'BDC_CURSOR'
                                'RM06I-LTEX1(01)'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.

  perform bdc_dynpro      using 'SAPLSPO1' '0100'.
  perform bdc_field       using 'BDC_OKCODE'
                                '=YES'.

  data(ls_options) = value ctu_params( dismode = mode
                                       updmode = update
                                       racommit = abap_true ).

  perform bdc_transaction tables messtab
  using                          'ME12'
                                 ctu
                                 mode
                                 update
                                 ls_options.
  if sy-subrc <> 0.
    subrc = sy-subrc.
    exit.
  endif.

  perform close_group using     ctu.
endfunction.
*INCLUDE bdcrecxy .
