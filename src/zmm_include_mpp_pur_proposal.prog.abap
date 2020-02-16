*&---------------------------------------------------------------------*
*&  Include           ZMM_INCLUDE_MPP_PUR_PROPOSAL
*&---------------------------------------------------------------------*
*& TECHNICAL CONSULTANT : RAVINDRA SANAP


TYPE-POOLS: vrm.

TYPES: BEGIN OF ty_tab1,
       chk(1)                 TYPE  c,                       "consumed proposal
       prno                   TYPE  char10,                  "proposal number
       gjahr                  TYPE  gjahr,                   "fiscial year
       pr_date                TYPE  zmm_pur_proposal-pr_date, " proposal date
       ekgrp                  TYPE  ekgrp,
       eknam                  TYPE  eknam,                   "purchase group name
       bukrs                  TYPE  bukrs,
       werks                  TYPE  werks,
       plantname              TYPE  name1,                   "plant description
       erdat                  TYPE  erdat,                    "Date on Which Record Was Created
       ebelp                  TYPE  ebelp,
       matnr                  TYPE  matnr,                    "MATERIAL
       maktx                  TYPE  maktx,                    "DESC
       lifnr                  TYPE  lifnr,                    "VENDOR
       name1                  TYPE  name1,                    "VENDOR NAME
       qty(10)                TYPE  n,                        "quantity
       meins                  TYPE  ekpo-meins,                "unit
       netpr                  TYPE  netpr,                    "New BASIC PRICE
       waers                  TYPE  waers_curc,               "CURRENCY code
       mwskz                  TYPE  t007s-mwskz,              " tax code
       tax_desc               TYPE  t007s-text1,              " tax code description
       landed_price           TYPE  netpr,                    " new landed price
       landed_value           TYPE  netpr,                    " new landed value
       last_pur_basic_price   TYPE  netpr,                    " Last Purchase basic Price
       last_pur_land_price    TYPE  netpr,                     " Last Purchase landed Price
       business_price         TYPE  kbetr,                     "business plan price
       vari_last_pur_inr      TYPE  netpr,                     "variance last purchase inr per kg
       vari_last_pur_val      TYPE  netwr,                     "variance last purchase Value in Lakh
       vari_business_plan_inr TYPE  netpr,                     "variance business plan inr per kg
       vari_business_plan_val TYPE  netwr,                     "variance business plan Value in Lakh
       op_m1                  TYPE  zmm_pur_proposal-opening_m1,   "opening for month1
       req_m1                 TYPE  zmm_pur_proposal-requirment_m1,"requirment for month1
       app_vend_m1            TYPE  zmm_pur_proposal-app_vend_m1,  "app vendor for month1
       otr_vend_m1            TYPE  zmm_pur_proposal-other_vend_m1,"other vendor for month1
       clos_m1                TYPE  zmm_pur_proposal-closing_m1,   "closing for      month1
       op_m2                  TYPE  zmm_pur_proposal-opening_m2,   "opening for      month2
       req_m2                 TYPE  zmm_pur_proposal-requirment_m2,"requirment for month2
       app_vend_m2            TYPE  zmm_pur_proposal-app_vend_m2,  "app vendor for month2
       otr_vend_m2            TYPE  zmm_pur_proposal-other_vend_m2,"other vendor for month2
       clos_m2                TYPE  zmm_pur_proposal-closing_m2,   "closing for      month2
       op_m3                  TYPE  zmm_pur_proposal-opening_m3,   "opening for      month3
       req_m3                 TYPE  zmm_pur_proposal-requirment_m3,"requirment for month3
       app_vend_m3            TYPE  zmm_pur_proposal-app_vend_m3,  "app vendor for month3
       otr_vend_m3            TYPE  zmm_pur_proposal-other_vend_m3,"other vendor for month3
       clos_m3                TYPE  zmm_pur_proposal-closing_m3,   "closing for      month3
       costformula(4)         TYPE  c,                             "cost per formula
       comp_data_avl(4)       TYPE  c,                             "Competitor data availability
       text(500)              TYPE  c,                             " line item text
       remarks(500)           TYPE  c,                             " other remarks
       releaser1(8)           TYPE  c,
       releaser2(8)           TYPE  c,
       releaser3(8)           TYPE  c,
       releaser4(8)           TYPE  c,
       releaser_name1(30)     TYPE  c,
       releaser_name2(30)     TYPE  c,
       releaser_name3(30)     TYPE  c,
       releaser_name4(30)     TYPE  c,
       ch_date                TYPE  zmm_pur_proposal-ch_date,
       pr_user                TYPE  zmm_pur_proposal-pr_user,      "PROPOSED CREATED USER
       pr_time                TYPE  sy-timlo,                       "PROPOSED CREATED TIME
       flag(1)                TYPE  c,
       END OF ty_tab1.

TYPES: BEGIN OF ty_userlist,
       ekgrp  TYPE   ekgrp,
       pernr  TYPE   pa0001-pernr,
       ename  TYPE   pa0001-ename,
       END OF ty_userlist.

TYPES: BEGIN OF ty_pa0001,
       pernr  TYPE   pa0001-pernr,
       ename  TYPE   pa0001-ename,
       END OF ty_pa0001.

TYPES: BEGIN OF ty_pa0000,
       pernr  TYPE   pa0001-pernr,
       massn  TYPE   pa0000-massn,
       END OF ty_pa0000.

TYPES: BEGIN OF ty_a516,
       kschl  TYPE   a516-kschl,
       werks  TYPE   a516-werks,
       matnr  TYPE   a516-matnr,
       datbi  TYPE   a516-datbi,
       datab  TYPE   a516-datab,
       knumh  TYPE   a516-knumh,
       END OF ty_a516.

TYPES: BEGIN OF ty_ekpo,
       ebeln  TYPE   ekpo-ebeln,
       ebelp_i  TYPE   WB2_V_EKKO_EKPO2-ebelp_i,
       aedat  TYPE   ekpo-aedat,
       matnr_i  TYPE   WB2_V_EKKO_EKPO2-matnr_i,
       werks_i  TYPE   WB2_V_EKKO_EKPO2-werks_i,
       netpr_i  TYPE   WB2_V_EKKO_EKPO2-netpr_i,
       END OF ty_ekpo.

TYPES: BEGIN OF ty_matnr1,
       matnr TYPE marc-matnr,
       END OF ty_matnr1.

TYPES: BEGIN OF ty_pgrp,
       ekgrp TYPE t024-ekgrp,
       eknam TYPE t024-eknam,
       END OF ty_pgrp.

DATA:  lt_f4_pgrp TYPE TABLE OF ty_pgrp,
       it_ret     TYPE ddshretval OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF ty_werks,
       werks TYPE t001w-werks,
       name1 TYPE t001w-name1,
       END OF ty_werks.

DATA : lt_f4_werks TYPE TABLE OF ty_werks.

TYPES: BEGIN OF ty_matnr,
       matnr TYPE marc-matnr,
       END OF ty_matnr.

TYPES: BEGIN OF ty_matnr2,
       matnr TYPE marc-matnr,
       maktx TYPE makt-maktx,
       END OF ty_matnr2.

TYPES: BEGIN OF ty_mara,
       matnr TYPE mara-matnr,
       END OF ty_mara.

DATA: lt_f4_matnr  TYPE TABLE OF ty_matnr,
      lt_f4_matnr1 TYPE TABLE OF ty_matnr2,
      lt_mara      TYPE TABLE OF ty_mara,
      ls_mara      TYPE ty_mara.

TYPES: BEGIN OF ty_f4_waers,
       waers  TYPE waers_curc,
       ltext  TYPE ltext,
       END OF ty_f4_waers.

TYPES: BEGIN OF ty_f4_lifnr,
       lifnr  TYPE lifnr,
       name1  TYPE name1,
       END OF ty_f4_lifnr.

TYPES :BEGIN OF ty_lfb1,
       lifnr  TYPE lifnr,
       END OF ty_lfb1.

DATA: lt_f4_lifnr TYPE TABLE OF ty_f4_lifnr,
      ls_f4_lifnr TYPE ty_f4_lifnr,
      lt_lfb1     TYPE TABLE OF ty_lfb1,
      lv_lifnr    TYPE lifnr.

DATA: lt_f4_waers TYPE TABLE OF ty_f4_waers.

DATA: gt_tab1               TYPE TABLE OF ty_tab1,  "INTERNAL TABLE FOR TABLE CONTROL 1
      gs_tab1               TYPE ty_tab1,           "WORK ARE FOR TABLE TABLE CONTROL 1
      gt_tab2               TYPE TABLE OF ty_tab1,  "INTERNAL TABLE FOR TABLE CONTROL 2
      gt_tab3               TYPE TABLE OF ty_tab1,  "INTERNAL TABLE FOR TABLE CONTROL 3
      gt_tab4               TYPE TABLE OF ty_tab1,  "INTERNAL TABLE FOR TABLE CONTROL 4
      gt_tab5               TYPE TABLE OF ty_tab1,  "
      gs_tab2               TYPE ty_tab1,
      gs_tab22              TYPE ty_tab1,
      gs_tab44              TYPE ty_tab1,
      gs_tab3               TYPE ty_tab1,
      gs_tab4               TYPE ty_tab1,
      gt_pp                 TYPE TABLE OF zmm_pur_proposal,
      gs_pp                 TYPE zmm_pur_proposal,
      gt_zmm_pur_proposal   TYPE TABLE OF zmm_pur_proposal,
      gs_zmm_pur_proposal   TYPE zmm_pur_proposal,
      gt_user_list          TYPE TABLE OF ty_userlist,
      gs_user_list          TYPE ty_userlist,
      gt_ekpo               TYPE TABLE OF ty_ekpo,
      gs_ekpo               TYPE ty_ekpo,
      gt_pa0000             TYPE TABLE OF ty_pa0000,
      gs_pa0000             TYPE ty_pa0000,
      gt_pa0001             TYPE TABLE OF ty_pa0001,
      gs_pa0001             TYPE ty_pa0001,
      gt_a516               TYPE TABLE OF ty_a516,
      gs_a516               TYPE ty_a516,
      gt_stxh               TYPE STANDARD TABLE OF stxh,
      gs_stxh               TYPE stxh,
      lt_matnr              TYPE TABLE OF ty_matnr1,
      ls_matnr              TYPE ty_matnr1.

DATA: name                  TYPE vrm_id,
      value                 TYPE vrm_values,
      list                  TYPE vrm_values WITH HEADER LINE,
      list2                 TYPE vrm_values WITH HEADER LINE,
      list3                 TYPE vrm_values WITH HEADER LINE,
      list4                 TYPE vrm_values WITH HEADER LINE,
      list5                 TYPE vrm_values WITH HEADER LINE,
      list6                 TYPE vrm_values WITH HEADER LINE,
      list_final            TYPE vrm_values WITH HEADER LINE,
      list_box(8)           TYPE c,  "name of listbox
      list_box2(8)          TYPE c,  "name of listbox
      list_box3(8)          TYPE c,  "name of listbox
      list_box4(8)          TYPE c,  "name of listbox
      list_box5(5)          TYPE c,  "list box for attach doc
      list_box6(5)          TYPE c,  "listbox for view attachment
      lv_list_boxx(5)       TYPE c,  "listbox for view attachment
      lv_key3(8)            TYPE c,  "
      lv_key4(8)            TYPE c,  "
      costformula(4)        TYPE c,  "name of COSTFORMULA
      comp_data_avl(4)      TYPE c.  "name of comp data availability

DATA: lt_f4_itemno          TYPE TABLE OF zmm_proposal_doc,
      ls_f4_itemno          TYPE zmm_proposal_doc.

DATA: line_length           TYPE i VALUE 500,
      remarks_container     TYPE REF TO cl_gui_custom_container,
      remarks_editor        TYPE REF TO cl_gui_textedit,
      remarks               TYPE string,
      editor_container      TYPE REF TO cl_gui_custom_container,
      text_editor           TYPE REF TO cl_gui_textedit,
      g_mytable(255)        TYPE c OCCURS 0,
      flag                  TYPE c,
      flag1                 TYPE c,
      flag2                 TYPE c,
      flag3                 TYPE c,
      flag3_1               TYPE c,
      flag3_2               TYPE c,
      flag3_3               TYPE c,
      flag3_4               TYPE c,
      flag4                 TYPE c,
      flag5                 TYPE c,
      flag6                 TYPE c,
      flag7                 TYPE c,
      flag8                 TYPE c,
      flag9                 TYPE c,
      flag_final            TYPE c,
      no(8)                 TYPE c,
      g_lineno(2)           TYPE c,"sy-index,
      lv_curline            TYPE sy-tabix,
      text                  TYPE string,
      gv_releaser1(8)       TYPE c,
      gv_releaser2(8)       TYPE c,
      gv_releaser3(8)       TYPE c,
      gv_releaser4(8)       TYPE c,
      gv_releaser_name1(30) TYPE c,
      gv_releaser_name2(30) TYPE c,
      gv_releaser_name3(30) TYPE c,
      gv_releaser_name4(30) TYPE c,
      releaser_name1(40)    TYPE c,
      releaser_name2(40)    TYPE c,
      releaser_name3(40)    TYPE c,
      releaser_name4(40)    TYPE c,
      old_remarks(255)      TYPE  c,
      lv_msg                TYPE string,
      gv_ans                TYPE c,
      lv_ans                TYPE c,
      gv_msg                TYPE string,
      gv_fiscal             TYPE t009b-bdatj,
      gv_itemno             TYPE zmm_pur_proposal-pr_itemno.

DATA: r1                    TYPE  c,   "CREATE PROPOSAL
      r2                    TYPE  c,   "COPY/CHANGE PROPOSAL FROM EXISTING
      r3                    TYPE  c,   "DISPLAY PROPOSAL
      r4                    TYPE  c,   "
      r5                    TYPE  c,   "
      r6                    TYPE  c,   "
      r7                    TYPE  c.   "

DATA: gv_date               TYPE datum,
      gv_chdate             TYPE datum,
      gv_user               TYPE zmm_pur_proposal-pr_user,   "PROPOSED CREATED USER
      gv_time               TYPE sy-timlo,
      gv_ekgrp              TYPE t024-ekgrp,
      gv_eknam              TYPE t024-eknam,       "purchase group description
      gv_bukrs              LIKE vbak-bukrs_vf,
      gv_werks              TYPE t001w-werks,
      gv_plantname          TYPE t001w-name1,      "plant description
      lv_maktx              TYPE maktx,
      lv_name1              TYPE name1,
      lv_cons(4)            TYPE c,
      lv_kbetr              TYPE konp-kbetr,
      lv_year               TYPE inri-toyear,
      lv_no                 TYPE i,
      selline               TYPE i,
      selindex              TYPE i,
      lv_matnr              TYPE matnr,
      gv_prno(10)           TYPE c,
      gv_newprno(10)        TYPE c,
      lv_index              TYPE zmm_pur_proposal-pr_itemno,
      gv_pernr              TYPE persno,
      v_mode                type i.
DATA: gv_okcode             LIKE sy-ucomm.

CONTROLS: tc3 TYPE TABLEVIEW USING SCREEN 0103.

DATA:
      l_objkey          TYPE swo_typeid,
      lo_gos_service    TYPE REF TO cl_gos_document_service,
      ls_srgbtbrel      TYPE srgbtbrel,
      lt_srgbtbrel      TYPE srgbtbrel OCCURS 10,
      is_object         TYPE borident.
                                                             "EMAIL""
DATA: objpack           LIKE sopcklsti1 OCCURS 2 WITH HEADER LINE,
      objhead           LIKE solisti1 OCCURS 1 WITH HEADER LINE,
      objbin            LIKE solisti1 OCCURS 10 WITH HEADER LINE,
      objtxt            LIKE solisti1 OCCURS 10 WITH HEADER LINE,
      zreceivers        LIKE somlreci1 OCCURS 5 WITH HEADER LINE,
      doc_chng          LIKE sodocchgi1.

TYPES: t_body_msg       TYPE  solisti1,
       t_document_data  TYPE  sodocchgi1.
DATA : i_body_msg       TYPE STANDARD TABLE OF t_body_msg,
       g_tab_lines      TYPE i,
       w_body_msg       TYPE  t_body_msg,
       w_document_data  TYPE  t_document_data."

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*CONSTANTS: objtype TYPE borident-objtype VALUE 'ZGOS'.
*
*TYPES: BEGIN OF exclude_type,
*        fcode LIKE rsmpe-func,
* END OF exclude_type.
*
*DATA: manager      TYPE REF TO cl_gos_manager,
*      obj          TYPE borident,
**     OK_CODE      TYPE SY-UCOMM,
*      exclude_tab  TYPE STANDARD TABLE OF exclude_type,
*      exclude_wa   TYPE exclude_type.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
