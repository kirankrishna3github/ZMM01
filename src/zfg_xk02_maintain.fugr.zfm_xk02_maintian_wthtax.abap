FUNCTION ZFM_XK02_MAINTIAN_WTHTAX.
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
*"     VALUE(LIFNR_001) LIKE  BDCDATA-FVAL
*"     VALUE(BUKRS_002) LIKE  BDCDATA-FVAL
*"     VALUE(D0610_003) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(QLAND_004) LIKE  BDCDATA-FVAL DEFAULT 'IN'
*"     VALUE(WITHT_03_005) LIKE  BDCDATA-FVAL DEFAULT 'IG'
*"     VALUE(WT_WITHCD_03_006) LIKE  BDCDATA-FVAL DEFAULT 'IG'
*"     VALUE(WT_SUBJCT_03_007) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(QLAND_008) LIKE  BDCDATA-FVAL DEFAULT 'IN'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPMF02K' '0101'.
perform bdc_field       using 'BDC_CURSOR'
                              'RF02K-D0610'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RF02K-LIFNR'
                              LIFNR_001.
perform bdc_field       using 'RF02K-BUKRS'
                              BUKRS_002.
perform bdc_field       using 'RF02K-D0610'
                              D0610_003.
perform bdc_dynpro      using 'SAPMF02K' '0610'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTR'.
perform bdc_field       using 'BDC_CURSOR'
                              'LFBW-WT_SUBJCT(01)'.
perform bdc_field       using 'LFB1-QLAND'
                              QLAND_004.
perform bdc_field       using 'LFBW-WITHT(03)'
                              WITHT_03_005.
perform bdc_field       using 'LFBW-WT_WITHCD(03)'
                              WT_WITHCD_03_006.
perform bdc_field       using 'LFBW-WT_SUBJCT(03)'
                              WT_SUBJCT_03_007.
perform bdc_dynpro      using 'SAPMF02K' '0610'.
perform bdc_field       using 'BDC_OKCODE'
                              '=UPDA'.
perform bdc_field       using 'BDC_CURSOR'
                              'LFBW-WT_SUBJCT(01)'.
perform bdc_field       using 'LFB1-QLAND'
                              QLAND_008.
perform bdc_transaction tables messtab
using                         'XK02'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
INCLUDE BDCRECXY .
