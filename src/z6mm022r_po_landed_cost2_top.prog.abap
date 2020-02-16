*&---------------------------------------------------------------------*
*&  Include           Z6MM022R_PO_LANDED_COST_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Dev. Class    : ZPP                                                *
*   Report Name   :  Z6MM022R_PO_LANDED_COST_TOP                       *
*   Program Type  : Include                                            *
*   Created by    : Kiruthika                                          *
*   Created on    : 26/06/2010                                         *
*   TCode         :                                                    *
*   Module Name   :                                                    *
*   Description   : PO landed cost report                              *
*----------------------------------------------------------------------*
*   S O U R C E   C O D E   C H A N G E   H I S T O R Y
* ---------------------------------------------------------------------*
*   CODE    | AUTHOR    | DATE     |  DESC
* ---------------------------------------------------------------------*
*
* ---------------------------------------------------------------------*

************************************************************************
* Declaration of type pools
************************************************************************
TYPE-POOLS: sdydo, slis.

************************************************************************
* Declaration of table types
************************************************************************
TYPES: BEGIN OF t_output,
  ebeln         TYPE ekko-ebeln,
  ebelp         TYPE  ekpo-ebelp,
  bedat         TYPE ekko-bedat,
  matnr         TYPE ekpo-matnr,
  txz01         TYPE ekpo-txz01,
  ekgrp         TYPE ekko-ekgrp,  " Purchasing Group Added by CS on 30.03.2016
  spart         TYPE mara-spart,  " Division Added by CS on 30.03.2016
  belnr         TYPE ekbe-belnr,
  budat         TYPE ekbe-budat,
  gjahr         TYPE ekbe-gjahr,
  buzei         TYPE ekbe-buzei,
  gr_menge      TYPE ekbe-menge,
  menge         TYPE ekpo-menge,
  meins         TYPE ekpo-meins,
  bamng         TYPE ekbe-bamng,  " Qty in SKU  - Added by CS on 04.05.2016
  bamng_meins   TYPE mseg-meins,  " SKU UOM  - Added by CS on 04.05.2016
  lifnr         TYPE ekko-lifnr,
  name1         TYPE lfa1-name1,
  werks         TYPE ekpo-werks,
  waers         TYPE ekko-waers,
  wkurs         TYPE ekko-wkurs,
  lcurr         TYPE p LENGTH 16 DECIMALS 2, "type ekpo-netwr,"
*  mprice        TYPE konv-kwert,
  bedamt        TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  bedamt1       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  ecsamt        TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  ecsamt1       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  secs          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  secs1         TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  bcustom       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  bcustom1      TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
*  bcustom      TYPE konv-kwert,
 incvd          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 incvd1         TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 incvd_prim     TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 incvd_prim1    TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 incvd_sec      TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 incvd_sec1     TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 educess_prim   TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 educess_prim1  TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 educess_sec    TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 educess_sec1   TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 custduty       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
 custduty1      TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  cha           TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  cha1          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  landing       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  landing1      TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  lcostpu       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  lcostpu1      TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  fri_amt       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  fri_amt1      TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  octori        TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  octori1       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  cst           TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  cst1          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  vat           TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  vat1          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  bstax         TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  ecstax        TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  shetax        TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  tot_price     TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  tot_price1    TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  land_cost     TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  land_costpu   TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  tot_exc       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  cenvare       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  tot_cd        TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  cust_pu       TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  lbt           TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  zetx          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  ztc3          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  zser          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert, " PO #: 12/1300
  zoc2          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  zfbt          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  zpk3          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  base_mat      TYPE ekpo-netwr,
  per_unit      TYPE ekpo-netwr,
  awkey         TYPE bkpf-awkey,
  kursf         TYPE bkpf-kursf,
  zterm         TYPE ekko-zterm,
  text1         TYPE t052u-text1,
  tot_net_val   TYPE netwr,
  rlwrt         TYPE ekpo-netwr , "type ekko-rlwrt,
  shkzg         TYPE ekbe-shkzg,
  grr_bas       TYPE  p LENGTH 16 DECIMALS 2,    "konv-kwert,
***** Start Code: Added by CS on 07.01.2016. *****
  ztag1         TYPE t052-ztag1,  " Days Limit
  zbpl          TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  zbpl_variance TYPE p LENGTH 16 DECIMALS 2,    "konv-kwert,
  zbpp TYPE p LENGTH 16 DECIMALS 2,
  zbpp_variance TYPE p LENGTH 16 DECIMALS 2,
  zbpc TYPE p LENGTH 16 DECIMALS 2,
  zbpc_variance TYPE p LENGTH 16 DECIMALS 2,
  avg_crval TYPE p LENGTH 16 DECIMALS 2,  " AVG credit Vaue i.e. Landed cost * PayTerms Days
  cr_days TYPE p LENGTH 16 DECIMALS 2, " Credit Days i.e. TOTAL of Avg Cr. Value / TOTAL of Landed Cost
***** End Code: Added by CS on 07.01.2016. *****
  END OF t_output.

TYPES: BEGIN OF t_ekko,
  ebeln TYPE ekko-ebeln,
  bedat TYPE ekko-bedat,
  bsart TYPE ekko-bsart,
  lifnr TYPE ekko-lifnr,
  waers TYPE ekko-waers,
  wkurs TYPE ekko-wkurs,
  knumv TYPE ekko-knumv,
  zterm TYPE ekko-zterm,  "Punam
  rlwrt TYPE ekko-rlwrt,
  ekgrp TYPE ekko-ekgrp,  " Purchasing Group Added by CS on 30.03.2016
  END OF t_ekko.

TYPES: BEGIN OF t_ekpo,
  ebeln TYPE ekpo-ebeln,
  ebelp TYPE ekpo-ebelp,
  txz01 TYPE ekpo-txz01,
  matnr TYPE ekpo-matnr,
  menge TYPE ekpo-menge,
  meins TYPE ekpo-meins,
  werks TYPE ekpo-werks,
  netpr TYPE ekpo-netpr,
  END OF t_ekpo.

TYPES: BEGIN OF t_lifnr,
  lifnr TYPE lfa1-lifnr,
  name1 TYPE lfa1-name1,
  END OF t_lifnr.

TYPES : BEGIN OF t_konv,
*{   REPLACE        SBXK900019                                        1
*\  knumv TYPE konv-knumv,
*\  kposn TYPE konv-kposn,
*\  kschl TYPE konv-kschl,
*\  kwert TYPE konv-kwert,
*\  kbetr TYPE konv-kbetr,  " Added by CS on 07.01.2016
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************
  knumv TYPE PRCD_ELEMENTS-knumv,
  kposn TYPE PRCD_ELEMENTS-kposn,
  kschl TYPE PRCD_ELEMENTS-kschl,
  kwert TYPE PRCD_ELEMENTS-kwert,
  kbetr TYPE PRCD_ELEMENTS-kbetr,  " Added by CS on 07.01.2016
*}   REPLACE
  END OF t_konv.

TYPES: BEGIN OF t_ekbe,
  ebeln    TYPE ekbe-ebeln,
  ebelp    TYPE ekbe-ebelp,
  vgabe    TYPE ekbe-vgabe, " Added by CS on 21.12.2015
  gjahr    TYPE ekbe-gjahr,
  belnr    TYPE ekbe-belnr,
  budat    TYPE ekbe-budat,
  dmbtr    TYPE dmbtr,
  buzei    TYPE ekbe-buzei,
  menge TYPE ekbe-menge,
  bamng TYPE ekbe-bamng,  " Qty in SKU -  Added by CS on 04.05.2016
  shkzg TYPE ekbe-shkzg,
  bewtp TYPE ekbe-bewtp,  " Added by CS on 18.12.2015
 END OF t_ekbe,
 BEGIN OF ty_mseg,   "  Added by CS on 04.05.2016
  mblnr TYPE mseg-mblnr,  " Number of Material Document
  mjahr TYPE mseg-mjahr,  " Material Document Year
  zeile TYPE mseg-zeile,  " Item in Material Document
  matnr TYPE mseg-matnr,  " Material Number
  werks TYPE mseg-werks,  " Plant
  menge TYPE mseg-menge,  " Quantity
  meins TYPE mseg-meins,  " Base Unit of Measure
  erfmg TYPE mseg-erfmg,  " Quantity in Unit of Entry
  erfme TYPE mseg-erfme,  " Unit of Entry
 END OF ty_mseg.
TYPES: BEGIN OF t_bkpf,
  awkey    TYPE bkpf-awkey,
  kursf    TYPE bkpf-kursf,
 END OF t_bkpf.

TYPES: BEGIN OF t_mara,  " Added by CS on 30.03.2016
        matnr TYPE mara-matnr,  " Material #
        spart TYPE mara-spart,  " Division
      END OF t_mara.

************************************************************************
* Declaration of internal tables
************************************************************************
DATA: it_ekko TYPE TABLE OF t_ekko,
      it_ekpo TYPE TABLE OF t_ekpo,
      it_lifnr TYPE TABLE OF t_lifnr,
      it_konv   TYPE TABLE OF t_konv,
      it_output TYPE TABLE OF t_output,
      it_ekbe_all   TYPE TABLE OF t_ekbe, " Added by CS
      it_ekbe   TYPE TABLE OF t_ekbe,
      it_ekbe_sto   TYPE TABLE OF t_ekbe,
      it_mseg TYPE TABLE OF ty_mseg, "  Added by CS on 04.05.2016
      wa_mseg TYPE ty_mseg,
      it_ekbz TYPE STANDARD TABLE OF ekbz,
      it_ekbz_sto TYPE STANDARD TABLE OF ekbz,
      wa_ekbz TYPE ekbz,
      wa_ekbz_sto TYPE ekbz,
      it_j_1igrxref TYPE STANDARD TABLE OF j_1igrxref,
      wa_j_1igrxref TYPE j_1igrxref,
      it_fieldcat TYPE slis_t_fieldcat_alv,
      wa_fieldcat TYPE slis_fieldcat_alv,
      it_events TYPE slis_t_event,
      wa_events TYPE slis_alv_event,
       layout   TYPE slis_layout_alv,
      it_komv TYPE komv_itab,
      it_bkpf TYPE STANDARD TABLE OF t_bkpf,
      it_mara TYPE TABLE OF t_mara, " Added by CS on 30.03.2016
      wa_mara TYPE t_mara.

DATA: alv_print        TYPE slis_print_alv.

************************************************************************
* Declaration of work areas
************************************************************************

DATA: wa_ekko TYPE t_ekko,
      wa_ekpo TYPE t_ekpo,
      wa_lifnr TYPE t_lifnr,
      wa_konv TYPE t_konv,
      wa_ekbe TYPE t_ekbe,
      wa_ekbe_sto TYPE t_ekbe,
      wa_output TYPE t_output,
      wa_temp TYPE t_output,
*      wa_fieldcat TYPE lvc_s_fcat,
      wa_komv TYPE komv,
      wa_bkpf TYPE t_bkpf,
      v_menge TYPE ekpo-menge.

DATA: tot_net TYPE netwr.
DATA: vat_total TYPE kwert.

************************************************************************
* Declaration of global fields
************************************************************************

DATA: w_container        TYPE REF TO cl_gui_custom_container,
      w_split_con        TYPE REF TO cl_gui_easy_splitter_container,
      w_top_container    TYPE REF TO cl_gui_container,   "Top Container
      w_bottom_container TYPE REF TO cl_gui_container,"Bottom Container
      w_document         TYPE REF TO cl_dd_document,         "Document
      w_grid             TYPE REF TO cl_gui_alv_grid,
      w_doc              TYPE REF TO cl_gui_docking_container,
      ok_code_100        TYPE sy-ucomm.

DATA : st_layout              TYPE slis_layout_alv.
DATA: g_save(1) TYPE c,
      g_exit(1) TYPE c,
      repname  LIKE sy-repid,
      g_variant LIKE disvariant,
      gx_variant LIKE disvariant,
      p_vairaint LIKE disvariant.

DATA: po, grr.
ranges: r_kschl for ekbz-kschl.

***** Start Code: Added by CS on 16.10.2015 for Authorization. *****
DATA: lv_ekorg_auth_flg TYPE c VALUE '',  " Auth. Flag for Purchase Org.
      lv_mtart_auth_flg TYPE c VALUE '',  " Auth. Flag for Material Type
      lv_werks_auth_flg TYPE c VALUE ''  " Auth. Flag for Receiving Plant
.
***** End Code: Added by CS on 16.10.2015 for Authorization. *****

***** Start Code: Added by CS on 07.01.2016. *****
DATA: lv_paytm_days TYPE t052-ztag1,  " Days Limit - Added by CS on 07.01.2016
      lv_tot_lcost TYPE p LENGTH 16 DECIMALS 2, " Total Landed Cost
      lv_tot_avgcr TYPE p LENGTH 16 DECIMALS 2, " Total Avg. Credit value
      lv_tot_crdays TYPE p LENGTH 16 DECIMALS 2,   " Total Avg. Credit value / Total Landed Cost
      lv_txt_crdays(20) TYPE c.
***** End Code: Added by CS on 07.01.2016. *****
