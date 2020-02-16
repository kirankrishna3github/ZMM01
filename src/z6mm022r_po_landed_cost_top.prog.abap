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
TYPE-POOLS: sdydo ,SLIS.

************************************************************************
* Declaration of table types
************************************************************************
TYPES: BEGIN OF t_output,
  ebeln         TYPE ekko-ebeln,
  ebelp         TYPE  ekpo-ebelp,
  bedat         TYPE ekko-bedat,
  matnr         TYPE ekpo-matnr,
  txz01         TYPE ekpo-txz01,
  belnr         TYPE ekbe-belnr,
  budat         TYPE ekbe-budat,
  gjahr         TYPE ekbe-gjahr,
  buzei         TYPE ekbe-buzei,
  gr_menge      TYPE ekbe-menge,
  menge         TYPE ekpo-menge,
  meins         TYPE ekpo-meins,
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
  base_mat      TYPE ekpo-netwr,
  per_unit      TYPE ekpo-netwr,
  awkey         TYPE bkpf-awkey,
  kursf         TYPE bkpf-kursf,
  zterm         type ekko-zterm,
  text1         type t052u-text1,
  tot_net_val   type NETWR,
  rlwrt         TYPE ekpo-NETWR , "type ekko-rlwrt,
  SHKZG         TYPE EKBE-SHKZG,
  GRR_BAS       TYPE  p LENGTH 16 DECIMALS 2,    "konv-kwert,
  END OF t_output.

TYPES: BEGIN OF t_ekko,
  ebeln TYPE ekko-ebeln,
  bedat TYPE ekko-bedat,
  bsart TYPE ekko-bsart,
  lifnr TYPE ekko-lifnr,
  waers TYPE ekko-waers,
  wkurs TYPE ekko-wkurs,
  knumv TYPE ekko-knumv,
  zterm type ekko-zterm,  "Punam
  rlwrt type ekko-rlwrt,
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
*}   REPLACE
  END OF t_konv.

TYPES: BEGIN OF t_ekbe,
  ebeln    TYPE ekbe-ebeln,
  ebelp    TYPE ekbe-ebelp,
  gjahr    TYPE ekbe-gjahr,
  belnr    TYPE ekbe-belnr,
  budat    TYPE ekbe-budat,
  dmbtr    TYPE dmbtr,
  buzei    TYPE ekbe-buzei,
  menge TYPE ekbe-menge,
  SHKZG TYPE EKBE-SHKZG,
 END OF t_ekbe.

TYPES: BEGIN OF t_bkpf,
  awkey    TYPE bkpf-awkey,
  kursf    TYPE bkpf-kursf,
 END OF t_bkpf.

************************************************************************
* Declaration of internal tables
************************************************************************
DATA: it_ekko TYPE TABLE OF t_ekko,
      it_ekpo TYPE TABLE OF t_ekpo,
      it_lifnr TYPE TABLE OF t_lifnr,
      it_konv   TYPE TABLE OF t_konv,
      it_output TYPE TABLE OF t_output,
      it_ekbe   TYPE TABLE OF t_ekbe,
      it_ekbe_STO   TYPE TABLE OF t_ekbe,
      it_ekbz TYPE STANDARD TABLE OF ekbz,
      IT_EKBZ_sto TYPE STANDARD TABLE OF ekbz,
      wa_ekbz TYPE ekbz,
      wa_ekbz_STO TYPE ekbz,
      it_j_1igrxref TYPE STANDARD TABLE OF j_1igrxref,
      wa_j_1igrxref TYPE j_1igrxref,
      IT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      WA_FIELDCAT TYPE SLIS_FIELDCAT_ALV,
      IT_EVENTS TYPE SLIS_T_EVENT,
      WA_EVENTS TYPE SLIS_ALV_EVENT,
       LAYOUT   TYPE SLIS_LAYOUT_ALV,
      it_komv TYPE komv_itab,
      it_bkpf TYPE STANDARD TABLE OF t_bkpf.

DATA: ALV_PRINT        TYPE SLIS_PRINT_ALV.

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

DATA: TOT_NET TYPE NETWR.
data: VAT_TOTAL type kwert.

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

DATA : ST_LAYOUT              TYPE SLIS_LAYOUT_ALV.
DATA: G_SAVE(1) TYPE C,
      G_EXIT(1) TYPE C,
      REPNAME  LIKE SY-REPID,
      G_VARIANT LIKE DISVARIANT,
      GX_VARIANT LIKE DISVARIANT,
      P_VAIRAINT LIKE DISVARIANT.

data: PO, GRR.

***** Start Code: Added by CS on 16.10.2015 for Authorization. *****
data: lv_ekorg_auth_flg type c value '',  " Auth. Flag for Purchase Org.
      lv_mtart_auth_flg type c value '',  " Auth. Flag for Material Type
      lv_werks_auth_flg type c value ''  " Auth. Flag for Receiving Plant
.
***** End Code: Added by CS on 16.10.2015 for Authorization. *****
