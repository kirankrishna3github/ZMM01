function zfm_drp_sto_vsart_update_bdc.
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
*"     VALUE(EXPPURCHASEORDER) LIKE  BAPIMEPOHEADER-PO_NUMBER
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"      POITEM STRUCTURE  BAPIMEPOITEM
*"----------------------------------------------------------------------
  data: vsart type bdcdata-fval,
        ebeln type bdcdata-fval.

  check poitem[] is not initial and exppurchaseorder is not initial.
  sort poitem by po_item ascending.
  clear: vsart, ebeln.
  ebeln = conv bdcdata-fval( exppurchaseorder ).
  subrc = 0.

  perform bdc_nodata      using nodata.

  perform open_group      using group user keep holddate ctu.

  perform bdc_dynpro      using 'SAPLMEGUI'   '0014'.
  perform bdc_field       using 'BDC_OKCODE'  '=MECHOB'.

  perform bdc_dynpro      using 'SAPLMEGUI'   '0002'.
  perform bdc_field       using 'BDC_OKCODE'  '=MEOK'.
  perform bdc_field       using 'BDC_CURSOR'  'MEPO_SELECT-EBELN'.
  perform bdc_field       using 'MEPO_SELECT-EBELN'  exppurchaseorder.


  perform bdc_dynpro      using 'SAPLMEGUI'   '0014'.
  perform bdc_field       using 'BDC_OKCODE'  '=TABIDT19'.
  perform bdc_field       using 'BDC_CURSOR'  'MEPO1319-MATKL'.

  do lines( poitem ) - 1 times.
    perform bdc_dynpro      using 'SAPLMEGUI'   '0014'.
    perform bdc_field       using 'BDC_OKCODE'  '=FORWARD3200'.
    perform bdc_field       using 'BDC_CURSOR'  'MEPO1331-VSART'.
    clear vsart.
    read table poitem into data(wa_poitem) index sy-index.
    if sy-subrc = 0 and wa_poitem-shiptype is not initial.
      vsart = conv bdcdata-fval( wa_poitem-shiptype ).
      perform bdc_field       using 'MEPO1331-VSART'  vsart.  "'9T'.
    endif.
    clear wa_poitem.
  enddo.

  clear vsart.
  perform bdc_dynpro      using 'SAPLMEGUI' '0014'.
  perform bdc_field       using 'BDC_OKCODE'  '=MESAVE'.
  read table poitem into wa_poitem index lines( poitem ).
  if sy-subrc = 0 and wa_poitem-shiptype is not initial.
    vsart = conv bdcdata-fval( wa_poitem-shiptype ).
    perform bdc_field       using 'MEPO1331-VSART'  vsart.  "'9T'.
  endif.

  perform bdc_transaction tables messtab
  using                         'ME22N'
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
