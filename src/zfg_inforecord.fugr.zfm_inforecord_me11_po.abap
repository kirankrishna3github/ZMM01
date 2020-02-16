function zfm_inforecord_me11_po.
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
*"     VALUE(MATNR_002) LIKE  BDCDATA-FVAL DEFAULT '20000096'
*"     VALUE(EKORG_003) LIKE  BDCDATA-FVAL DEFAULT '1000'
*"     VALUE(WERKS_004) LIKE  BDCDATA-FVAL DEFAULT '1201'
*"     VALUE(NORMB_005) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(URZLA_006) LIKE  BDCDATA-FVAL DEFAULT 'IN'
*"     VALUE(MEINS_007) LIKE  BDCDATA-FVAL DEFAULT 'KG'
*"     VALUE(UMREZ_008) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(UMREN_009) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(APLFZ_010) LIKE  BDCDATA-FVAL DEFAULT '2'
*"     VALUE(EKGRP_011) LIKE  BDCDATA-FVAL DEFAULT '302'
*"     VALUE(MWSKZ_012) LIKE  BDCDATA-FVAL DEFAULT 'G3'
*"     VALUE(MHDRZ_013) LIKE  BDCDATA-FVAL DEFAULT '15'
*"     VALUE(IPRKZ_014) LIKE  BDCDATA-FVAL DEFAULT 'D'
*"     VALUE(NETPR_015) LIKE  BDCDATA-FVAL DEFAULT '             1'
*"     VALUE(WAERS_016) LIKE  BDCDATA-FVAL DEFAULT 'INR'
*"     VALUE(PEINH_017) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(BPRME_018) LIKE  BDCDATA-FVAL DEFAULT 'KG'
*"     VALUE(BPUMZ_019) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(BPUMN_020) LIKE  BDCDATA-FVAL DEFAULT '1'
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

  perform bdc_dynpro      using 'SAPMM06I' '0101'.
  perform bdc_field       using 'BDC_CURSOR'
                                'EINA-MAHN1'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.
*  PERFORM bdc_field       USING 'EINA-URZLA'
*                                urzla_006.
*  PERFORM bdc_field       USING 'EINA-MEINS'
*                                meins_007.
*  PERFORM bdc_field       USING 'EINA-UMREZ'
*                                umrez_008.
*  PERFORM bdc_field       USING 'EINA-UMREN'
*                                umren_009.

  perform bdc_dynpro      using 'SAPMM06I' '0102'.
  perform bdc_field       using 'BDC_CURSOR'
                                'EINE-MWSKZ'.
  perform bdc_field       using 'BDC_OKCODE'
                                '/00'.

  " IHDK902028 - In case of SCR material since planned delv days/mrp view does not exist,
  " push default planned delv days as 1 as planned delv days is mandatory for creating inforecord
  " In other cases like FGM, the field is auto populated by the system from MM03 -> MRP view
  data(lv_matnr) = conv matnr( matnr_002 ).

  zcl_helper=>conv_exit( exporting iv = lv_matnr iv_mode = 'I' importing ev = lv_matnr ).
  select single mtart from mara into @data(lv_mtart) where matnr eq @lv_matnr.

  if lv_mtart eq 'ZSCR'.
    perform bdc_field       using 'EINE-APLFZ'
                                  '1'.  "aplfz_010.
  endif.
  " End IHDK902028

  perform bdc_field       using 'EINE-EKGRP'
                                ekgrp_011.
  perform bdc_field       using 'EINE-MWSKZ'
                                mwskz_012.
*  PERFORM bdc_field       USING 'EINE-MHDRZ'
*                                mhdrz_013.
*  PERFORM bdc_field       USING 'EINE-IPRKZ'
*                                iprkz_014.
  perform bdc_field       using 'EINE-NETPR'
                                netpr_015.
*  PERFORM bdc_field       USING 'EINE-WAERS'
*                                waers_016.
*  PERFORM bdc_field       USING 'EINE-PEINH'
*                                peinh_017.
*  PERFORM bdc_field       USING 'EINE-BPRME'
*                                bprme_018.
*  PERFORM bdc_field       USING 'EINE-BPUMZ'
*                                bpumz_019.
*  PERFORM bdc_field       USING 'EINE-BPUMN'
*                                bpumn_020.

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
  perform bdc_transaction tables messtab
  using                         'ME11'
                                ctu
                                mode
                                update.
  if sy-subrc <> 0.
    subrc = sy-subrc.
    exit.
  endif.

  perform close_group using     ctu.
endfunction.
include bdcrecxy .
