*&***********************************************************************************************&*
*& OBJECT NAME          : ZMM_PROPOSAL_FINAL_CONFI                                                          &*
*& TECHNICAL CONSULTANT : RAVINDRA SANAP                                                         &*
*& TEAM LEAD            : POONAM SHINDE                                                          &*
*& FUCTIONAL            : VENU                                                                   &*
*& MODULE NAME          : MM                                                                     &*
*& PROGRAM TYPE         : REPORT                                                                 &*
*& TCODE                : ZMM016                                                                 &*
*& CREATE DATE          : Dec 08,2016                                                            &*
*& TRANSPORT NO         : IRDK926469                                                             &*
*& DESCRIPTION          : THIS REPORT IS DEVELOPED FOR PURCHASE PROPOSAL - TO DISPLAY            &*
*&                           ALL STATUS OF RELEASERS                                             &*
* REVISION HISTORY--------------------------------------------------------------------------------*
*                                                                                                 *
*   CHANGED BY:                                                                                   *
*   CHANGE ON:                                                                                    *
*   REASON FOR CHANGE:                                                                            *
*                                                                                                 *
*                                                                                                 *
* REVISION HISTORY--------------------------------------------------------------------------------*


REPORT  zmm_proposal_final_confi.
TYPE-POOLS: slis,icon.

DATA: W_VARIANT     TYPE DISVARIANT,
      ALV_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ALV_GRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: gt_fieldcat    TYPE STANDARD TABLE OF LVC_S_FCAT,
      gs_fieldcat    TYPE LVC_S_FCAT,
      gs_layout      TYPE LVC_S_layo,
      gs_head        TYPE slis_listheader,
      gt_head        TYPE slis_t_listheader.

TYPES : BEGIN OF ty_zmm_pur_proposal,
        prno             TYPE zmm_pur_proposal-prno,
        srno             TYPE i,
*        id               TYPE icon-id,
*        style            TYPE     lvc_t_styl,   "for disable
        gjahr            TYPE zmm_pur_proposal-gjahr,
        pr_itemno        TYPE zmm_pur_proposal-pr_itemno,
        level            TYPE  c,
        pr_date          TYPE zmm_pur_proposal-pr_date,
        ekgrp            TYPE zmm_pur_proposal-ekgrp,
        eknam            TYPE zmm_pur_proposal-eknam,
        bukrs            TYPE zmm_pur_proposal-bukrs,
        werks            TYPE zmm_pur_proposal-werks,
        plantname        TYPE zmm_pur_proposal-plantname,
        matnr            TYPE zmm_pur_proposal-matnr,
        maktx            TYPE zmm_pur_proposal-maktx,
        lifnr            TYPE zmm_pur_proposal-lifnr,
        name1            TYPE zmm_pur_proposal-name1,
        menge            TYPE zmm_pur_proposal-menge,
        netpr            TYPE zmm_pur_proposal-netpr,
        meins            TYPE zmm_pur_proposal-meins,
        waers            TYPE zmm_pur_proposal-waers,
        mwskz            TYPE zmm_pur_proposal-mwskz,
        tax_desc         TYPE zmm_pur_proposal-tax_desc,
        land_pr          TYPE zmm_pur_proposal-land_pr,
        land_value       TYPE zmm_pur_proposal-land_value,
        last_basicpr     TYPE zmm_pur_proposal-last_basicpr,
        last_landpr      TYPE zmm_pur_proposal-last_landpr,
        business_planpr  TYPE zmm_pur_proposal-business_planpr,
        vari_last_pur_in TYPE zmm_pur_proposal-vari_last_pur_in,
        vari_last_pur_va TYPE zmm_pur_proposal-vari_last_pur_va,
        vari_bus_pl_inr  TYPE zmm_pur_proposal-vari_bus_pl_inr,
        vari_bus_pl_val  TYPE zmm_pur_proposal-vari_bus_pl_val,
        opening_m1       TYPE zmm_pur_proposal-opening_m1,
        requirment_m1    TYPE zmm_pur_proposal-requirment_m1,
        app_vend_m1      TYPE zmm_pur_proposal-app_vend_m1,
        other_vend_m1    TYPE zmm_pur_proposal-other_vend_m1,
        closing_m1       TYPE zmm_pur_proposal-closing_m1,
        opening_m2       TYPE zmm_pur_proposal-opening_m2,
        requirment_m2    TYPE zmm_pur_proposal-requirment_m2,
        app_vend_m2      TYPE zmm_pur_proposal-app_vend_m2,
        other_vend_m2    TYPE zmm_pur_proposal-other_vend_m2,
        closing_m2       TYPE zmm_pur_proposal-closing_m2,
        opening_m3       TYPE zmm_pur_proposal-opening_m3,
        requirment_m3    TYPE zmm_pur_proposal-requirment_m3,
        app_vend_m3      TYPE zmm_pur_proposal-app_vend_m3,
        other_vend_m3    TYPE zmm_pur_proposal-other_vend_m3,
        closing_m3       TYPE zmm_pur_proposal-closing_m3,
        comp_data        TYPE zmm_pur_proposal-comp_data,
        comp_data1       TYPE zmm_pur_proposal-comp_data1,
        line_txt         TYPE zmm_pur_proposal-line_txt,
        final_price      TYPE zmm_pur_proposal-final_price,
        remark           TYPE zmm_pur_proposal-remark,
        releaser1        TYPE zmm_pur_proposal-releaser1,
        releaser_name1   TYPE zmm_pur_proposal-releaser_name1,
        releaser1_date   TYPE zmm_pur_proposal-releaser1_date,
        status1(10)      TYPE c,
        releaser2        TYPE zmm_pur_proposal-releaser2,
        releaser_name2   TYPE zmm_pur_proposal-releaser_name2,
        releaser2_date   TYPE zmm_pur_proposal-releaser2_date,
        status2(10)      TYPE c,
        releaser3        TYPE zmm_pur_proposal-releaser3,
        releaser_name3   TYPE zmm_pur_proposal-releaser_name3,
        releaser3_date   TYPE zmm_pur_proposal-releaser3_date,
        status3(10)      TYPE c,
        releaser4        TYPE zmm_pur_proposal-releaser4,
        releaser_name4   TYPE zmm_pur_proposal-releaser_name4,
        releaser4_date   TYPE zmm_pur_proposal-releaser4_date,
        status4(10)      TYPE c,
        erdat            TYPE zmm_pur_proposal-erdat,
        ch_date          TYPE zmm_pur_proposal-ch_date,
        pr_user          TYPE zmm_pur_proposal-pr_user,
        pr_time          TYPE zmm_pur_proposal-pr_time,
        app_vendor       TYPE zmm_pur_proposal-app_vendor,
        cons_proposal    TYPE zmm_pur_proposal-cons_proposal,
        color(4),

        END OF ty_zmm_pur_proposal.

"{ FOR DISABLE
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.
"} FOR DISABLE

DATA : gt_final             TYPE TABLE OF ty_zmm_pur_proposal,
       gs_final             LIKE LINE OF gt_final," ty_zmm_pur_proposal,
       gt_final2            TYPE TABLE OF ty_zmm_pur_proposal,
       gs_final2            LIKE LINE OF gt_final," ty_zmm_pur_proposal,
       gt_zmm_pur_proposal  TYPE TABLE OF zmm_pur_proposal,
       gs_zmm_pur_proposal  TYPE zmm_pur_proposal.


DATA : gv_prno(10) TYPE c," VALUE "'SET AND GET PARAMETER'.
       gv_prnr     TYPE persno,
       gv_user     TYPE zmm_pur_proposal-pr_user,
       lv_ans      TYPE c.

TYPES: BEGIN OF ty_f4_prno,
       prno TYPE zmm_pur_proposal-prno,
       END OF ty_f4_prno.

DATA : lt_f4_prno  TYPE TABLE OF ty_f4_prno,
       ls_f4_prno  TYPE ty_f4_prno,
       it_ret      TYPE ddshretval OCCURS 0 WITH HEADER LINE,
       gv_pernr    TYPE persno,
       flag1       TYPE c,
       flag_new    TYPE c.


TABLES : zmm_pur_proposal.

SELECTION-SCREEN : BEGIN OF BLOCK rad1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_prno FOR zmm_pur_proposal-prno MODIF ID b NO INTERVALS NO-EXTENSION .
SELECTION-SCREEN : END OF BLOCK rad1.

AT SELECTION-SCREEN on VALUE-REQUEST FOR s_prno-low.
PERFORM f4_help_prno.

START-OF-SELECTION.
  PERFORM select_data.
  CALL SCREEN 509.

FORM select_data .

  CLEAR :gv_pernr.
  REFRESH:gt_zmm_pur_proposal.

  SELECT *
      FROM zmm_pur_proposal INTO TABLE gt_zmm_pur_proposal
                      WHERE pr_user = sy-uname AND prno IN s_prno .
  SORT gt_zmm_pur_proposal BY prno.
  gv_pernr = sy-uname.
  DELETE ADJACENT DUPLICATES FROM gt_zmm_pur_proposal COMPARING ALL FIELDS.
  PERFORM final_data_approve.
ENDFORM.                    " SELECT_DATA


*&---------------------------------------------------------------------*
*&      Form  final_data_approve
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM final_data_approve.
  CLEAR gs_zmm_pur_proposal.
  LOOP AT  gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
    SHIFT gs_zmm_pur_proposal-releaser1_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser2_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser3_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser4_date LEFT DELETING LEADING '0'.
    gs_final-prno            = gs_zmm_pur_proposal-prno.
    gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
    gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno / 10.
    gs_final-pr_user         = gs_zmm_pur_proposal-pr_user.
    gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
    gs_final-ch_date         = gs_zmm_pur_proposal-ch_date.
    gs_final-pr_time         = gs_zmm_pur_proposal-pr_time.
    gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
    gs_final-eknam           = gs_zmm_pur_proposal-eknam.
    gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
    gs_final-werks           = gs_zmm_pur_proposal-werks.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-matnr
      IMPORTING
        output = gs_zmm_pur_proposal-matnr.

    gs_final-matnr           = gs_zmm_pur_proposal-matnr.
    gs_final-maktx           = gs_zmm_pur_proposal-maktx.
    gs_final-menge           = gs_zmm_pur_proposal-menge.
    gs_final-meins           = gs_zmm_pur_proposal-meins.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-lifnr
      IMPORTING
        output = gs_zmm_pur_proposal-lifnr.
    gs_final-lifnr            = gs_zmm_pur_proposal-lifnr.
    gs_final-name1            = gs_zmm_pur_proposal-name1.
    gs_final-netpr            = gs_zmm_pur_proposal-netpr.
    gs_final-waers            = gs_zmm_pur_proposal-waers.
    gs_final-land_pr          = gs_zmm_pur_proposal-land_pr.
    gs_final-mwskz            = gs_zmm_pur_proposal-mwskz.
    gs_final-name1            = gs_zmm_pur_proposal-name1.
    gs_final-land_value       = gs_zmm_pur_proposal-land_value.
    gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr .
    gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr.
    gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.
    gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.
    gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.
    gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
    gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
    gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
    gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
    gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
    gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
    gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
    gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
    gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
    gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
    gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
    gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
    gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
    gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
    gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
    gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
    gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
    gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
    gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
*          gs_final-level            = 1.
*          gs_final-color            = 'C410'.
    gs_final-releaser1        = gs_zmm_pur_proposal-releaser1.
    gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
    gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
    gs_final-releaser2        = gs_zmm_pur_proposal-releaser2.
    gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
    gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
    gs_final-releaser3        = gs_zmm_pur_proposal-releaser3.
    gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
    gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
    gs_final-releaser4        = gs_zmm_pur_proposal-releaser4.
    gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
    gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
    gs_final-app_vendor       = gs_zmm_pur_proposal-app_vendor.
    gs_final2 = gs_final.
    IF gs_zmm_pur_proposal-releaser1 NE ' '.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser1_date NE ' '.
          gs_final-level   = 1.
          gs_final-status1 = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser1_date EQ ' '.
              gs_final2-level            = 1.
              gs_final2-color            = 'C710'.
              gs_final2-status1         = 'Pending'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser2 NE ''.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser2_date NE ' '.
          gs_final-level            = 2.
          gs_final-status2          = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser2_date EQ ' '.
              gs_final2-level           = 2.
              gs_final2-status2         = 'Pending'.
              gs_final2-color            = 'C710'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser3 NE ' '.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser3_date NE ' '.
          gs_final-level            = 3.
          gs_final-status3          = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser3_date EQ ' '.
              gs_final2-level            = 3.
              gs_final2-status3          = 'Pending'.
              gs_final2-color            = 'C710'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser4 NE ' '.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser4_date NE ' '.
          gs_final-level            = 4.
          gs_final-status4          = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser4_date EQ ' '.
              gs_final2-level            = 4.
              gs_final2-status4          = 'Pending'.
              gs_final2-color            = 'C710'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: gs_final,gs_zmm_pur_proposal,gs_final2.
  ENDLOOP.


  SORT gt_final BY prno.

  REFRESH gt_final2..
  gt_final2[] = gt_final[].
  REFRESH gt_final.
  DATA : flag1 TYPE c.
  SORT gt_final2 BY prno.
  LOOP AT gt_final2 INTO gs_final2.
    SHIFT gs_final2-releaser1 LEFT DELETING LEADING '0'.
    SHIFT gs_final2-releaser2 LEFT DELETING LEADING '0'.
    SHIFT gs_final2-releaser3 LEFT DELETING LEADING '0'.
    SHIFT gs_final2-releaser4 LEFT DELETING LEADING '0'.
    gs_final = gs_final2.
    AT END OF prno.
      flag1 = 'X'.
      IF flag1 = 'X'.
        APPEND gs_final TO gt_final.
      ENDIF.
    ENDAT.
    CLEAR :gs_final2,flag1,gs_final.
  ENDLOOP.

 delete adjacent duplicates from gt_final comparing prno.
  refresh gt_final2.
  gt_final2[] = gt_final[].
  refresh gt_final.
  clear: gs_final,gs_final2.

  loop at gt_final2 into gs_final2.
    perform ok_pending.
  endloop.
  delete adjacent duplicates from gt_final comparing prno.
 data : lv_index type sy-tabix.
  clear flag_new.
  refresh:gt_final2.
  loop at gt_zmm_pur_proposal into gs_zmm_pur_proposal.
    if gs_zmm_pur_proposal-app_vendor = 'X'.
      lv_index = sy-tabix.
      flag_new = 'X'.
    endif.
    if lv_index ne ' '.
      loop at gt_final into gs_final from lv_index..
        if gs_final-prno = gs_zmm_pur_proposal-prno.
          gs_zmm_pur_proposal-pr_itemno = gs_zmm_pur_proposal-pr_itemno / 10.
          if gs_final-pr_itemno = gs_zmm_pur_proposal-pr_itemno.
            flag1 = 'X'.
            exit.
          endif.
        endif.
        clear gs_final.
      endloop.
    endif.
    if flag_new = 'X'.
      if flag1 ne 'X'.
        clear gs_final.
        perform multiple_app_vendor.
        append gs_final to gt_final2.
      endif.
    endif.
    clear: gs_zmm_pur_proposal,flag1,lv_index,flag_new.
  endloop.

  sort gt_final2 by prno pr_itemno.
  loop at gt_final into gs_final.
*    lv_index = sy-tabix.
    loop at gt_final2 into gs_final2." FROM lv_index.
      if gs_final2-prno = gs_final-prno and gs_final2-pr_itemno = gs_final-pr_itemno.
        delete gt_final2.
      endif.
      clear gs_final2.
    endloop.
    clear :gs_final,lv_index.
  endloop.
*  perform delete_extra.  "from gt_final2
  append lines of gt_final2 to gt_final.
 refresh : gt_final2.
  loop at gt_final into gs_final..
    shift gs_final-releaser1 left deleting leading '0'.
    shift gs_final-releaser2 left deleting leading '0'.
    shift gs_final-releaser3 left deleting leading '0'.
    shift gs_final-releaser4 left deleting leading '0'.
    shift gs_final-releaser1_date left deleting leading '0'.
    shift gs_final-releaser2_date left deleting leading '0'.
    shift gs_final-releaser3_date left deleting leading '0'.
    shift gs_final-releaser4_date left deleting leading '0'.
    if gs_final-releaser1_date ne ' '.
      gs_final-status1 = 'Ok'.
    endif.
    if gs_final-releaser2_date ne ' '.
      gs_final-status2 = 'Ok'.
    endif.
    if gs_final-releaser1_date eq ' '.
      gs_final-status1 = 'Pending'.
    endif.
    if gs_final-releaser2_date eq ' '.
      gs_final-status2 = 'Pending'.
    endif.
    if gs_final-releaser3 ne ' '.
      if gs_final-releaser3_date ne ' '.
        gs_final-status3 = 'Ok'.
      endif.
    endif.
    if gs_final-releaser3 ne ' '.
      if gs_final-releaser3_date eq ' '.
        gs_final-status3 = 'Pending'.
      endif.
    endif.
    if gs_final-releaser4 ne ' '.
      if gs_final-releaser4_date ne ' '.
        gs_final-status4 = 'Ok'.
      endif.
    endif.
    if gs_final-releaser4 ne ' '.
      if gs_final-releaser4_date eq ' '.
        gs_final-status4 = 'Pending'.
      endif.
    endif.
    modify gt_final from gs_final transporting status1 status2 status3 status4.
    clear gs_final.
  endloop.
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input         = gv_pernr
   importing
     output        = gv_pernr .

    loop at gt_final into gs_final.
    if gs_final-releaser1 eq gv_pernr.
      if gs_final-app_vendor = 'X'.
        gs_final-level            = 1.
      endif.
    endif.
    if gs_final-releaser2 eq gv_pernr.
      if gs_final-app_vendor = 'X'.
        gs_final-level            = 2.
      endif.
    endif.
    if gs_final-releaser3 eq gv_pernr.
      if gs_final-app_vendor = 'X'.
        gs_final-level            = 3.
      endif.
    endif.
    if gs_final-releaser4 eq gv_pernr.
      if gs_final-app_vendor = 'X'.
        gs_final-level            = 4.
      endif.
    endif.
    modify gt_final from gs_final transporting level.
    clear gs_final.
  endloop.
  sort gt_final by prno pr_itemno.
  if gt_final is initial.
    message 'No Data Found' type 'E'.
    exit.
  endif.
endform.                    " FINAL_DATA_U1

*&---------------------------------------------------------------------*
*&      Form  ok_pending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form ok_pending.
  shift gs_final2-releaser1 left deleting leading '0'.
  shift gs_final2-releaser2 left deleting leading '0'.
  shift gs_final2-releaser3 left deleting leading '0'.
  shift gs_final2-releaser4 left deleting leading '0'.
  shift gs_final2-releaser1_date left deleting leading '0'.
  shift gs_final2-releaser2_date left deleting leading '0'.
  shift gs_final2-releaser3_date left deleting leading '0'.
  shift gs_final2-releaser4_date left deleting leading '0'.
  gs_final = gs_final2.
  if gs_final2-releaser1 ne ' '.
    if gs_final2-app_vendor = 'X'.
      if gs_final2-releaser1_date ne ' '.
        gs_final2-level            = 1.
        gs_final2-status1          = 'Ok'.
        append gs_final2 to gt_final .
      else.
        if gs_final2-releaser1 ne ' '.
          if gs_final2-app_vendor = 'X'.
            if gs_final2-releaser1_date eq ' '.
              gs_final-level            = 1.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              append gs_final to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.
  if gs_final2-releaser2 ne ' '.
    if gs_final2-app_vendor = 'X'.
      if gs_final2-releaser2_date ne ' '.
        gs_final2-level            = 2.
        gs_final2-status1          = 'Ok'.
        append gs_final2 to gt_final .
      else.
        if gs_final2-releaser2 ne ' '.
          if gs_final2-app_vendor = 'X'.
            if gs_final2-releaser2_date eq ' '.
              gs_final-level            = 2.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              append gs_final to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.
  if gs_final2-releaser3 ne ' '.
    if gs_final2-app_vendor = 'X'.
      if gs_final2-releaser3_date ne ' '.
        gs_final2-level            = 3.
        gs_final2-status1          = 'Ok'.
        append gs_final2 to gt_final .
      else.
        if gs_final2-releaser3 ne ' '.
          if gs_final2-app_vendor = 'X'.
            if gs_final2-releaser3_date eq ' '.
              gs_final-level            = 3.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              append gs_final to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.
  if gs_final2-releaser4 ne ' '.
    if gs_final2-app_vendor = 'X'.
      if gs_final2-releaser4_date ne ' '.
        gs_final2-level            = 4.
        gs_final2-status1          = 'Ok'.
        append gs_final2 to gt_final .
      else.
        if gs_final2-releaser4 ne ' '.
          if gs_final2-app_vendor = 'X'.
            if gs_final2-releaser4_date eq ' '.
              gs_final-level            = 4.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              append gs_final to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
  endif.
endform.                    "ok_pending
*&---------------------------------------------------------------------*
*&      Form  MULTI_APP_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form multiple_app_vendor .
  gs_final-prno            = gs_zmm_pur_proposal-prno.
  gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
  gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno / 10 .
  gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
  gs_final-pr_user         = gs_zmm_pur_proposal-pr_user.
  gs_final-pr_time         = gs_zmm_pur_proposal-pr_time.
  gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
  gs_final-eknam           = gs_zmm_pur_proposal-eknam.
  gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
  gs_final-werks           = gs_zmm_pur_proposal-werks.
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = gs_zmm_pur_proposal-matnr
    importing
      output = gs_zmm_pur_proposal-matnr.

  gs_final-matnr           = gs_zmm_pur_proposal-matnr.
  gs_final-maktx           = gs_zmm_pur_proposal-maktx.
  gs_final-menge           = gs_zmm_pur_proposal-menge.
  gs_final-meins           = gs_zmm_pur_proposal-meins.
  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = gs_zmm_pur_proposal-lifnr
    importing
      output = gs_zmm_pur_proposal-lifnr.
  gs_final-lifnr            = gs_zmm_pur_proposal-lifnr.
  gs_final-name1            = gs_zmm_pur_proposal-name1.
  gs_final-netpr            = gs_zmm_pur_proposal-netpr.
  gs_final-waers            = gs_zmm_pur_proposal-waers.
  gs_final-land_pr          = gs_zmm_pur_proposal-land_pr.
  gs_final-mwskz            = gs_zmm_pur_proposal-mwskz.
  gs_final-name1            = gs_zmm_pur_proposal-name1.
  gs_final-land_value       = gs_zmm_pur_proposal-land_value.
  gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr .
  gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr.
  gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.
  gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.
  gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.
  gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
  gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
  gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
  gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
  gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
  gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
  gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
  gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
  gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
  gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
  gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
  gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
  gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
  gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
  gs_final-closing_m3       = gs_zmm_pur_proposal-closing_m3.
  gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
  gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
  gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
  gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
  gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
*          gs_final-level            = 1.
*          gs_final-color            = 'C410'.
  gs_final-releaser1        = gs_zmm_pur_proposal-releaser1.
  gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
  gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
  gs_final-releaser2        = gs_zmm_pur_proposal-releaser2.
  gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
  gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
  gs_final-releaser3        = gs_zmm_pur_proposal-releaser3.
  gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
  gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
  gs_final-releaser4        = gs_zmm_pur_proposal-releaser4.
  gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
  gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
  gs_final-app_vendor       = gs_zmm_pur_proposal-app_vendor.

endform.                    " MULTI_APP_VENDOR

"---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fieldcat .

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname  = 'PRNO'.
  gs_fieldcat-ref_table  = 'GT_FINAL'.
  gs_fieldcat-coltext    = 'Proposal Number'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_ITEMNO'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Best Proposal Sel.Line'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Releaser Name1'.
*  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Status 1'.
  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME2'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Releaser Name2'.
*  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS2'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Status 2'.
  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME3'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Releaser Name3'.
*  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS3'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Status 3'.
  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME4'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Releaser Name4'.
*  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS4'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Status 4'.
  gs_fieldcat-just = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.

  gs_fieldcat-fieldname = 'EKGRP'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Purchase Group'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'WERKS'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Plant'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_DATE'.
  gs_fieldcat-inttype   = 'D'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Proposal Date'.
  APPEND gs_fieldcat TO gt_fieldcat.
   CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'CH_DATE'.
  gs_fieldcat-inttype   = 'D'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Change Date'.
  APPEND gs_fieldcat TO gt_fieldcat.
   CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_USER'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Created By'.
  APPEND gs_fieldcat TO gt_fieldcat.
   CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_TIME'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Created Time'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MATNR'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Material No'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MAKTX'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Material Description'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LIFNR'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Vendor No'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NAME1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Vendor Name'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MENGE'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Quantity'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MEINS'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Unit'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NETPR'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'New Basic Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'WAERS'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Currency'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MWSKZ'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Tax Code'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_PR'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'New Landed Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_VALUE'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Purchase Value'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_BASICPR'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Last Purchase Basic PR(PO)'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_LANDPR'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Last Purchase Landed Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'BUSINESS_PLANPR'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Business Plan Price(ZBPL)'.
  APPEND gs_fieldcat TO gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_LAST_PUR_IN'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Variance_Last_Purchase_INR_Per_KG'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_LAST_PUR_VA'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Variance_Last_Purchase_Value'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_BUS_PL_INR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Variance_Business_Plan_INR_Per_KG'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_BUS_PL_VAL'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Variance_Business_Plan_Value'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OPENING_M1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Opening1(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'REQUIRMENT_M1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Requirment1(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'APP_VEND_M1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Approve Vendor1'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OTHER_VEND_M1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Other Vendors'.
  append gs_fieldcat to gt_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'CLOSING_M1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Closing1 (MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OPENING_M2'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext  = 'Opening2(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'REQUIRMENT_M2'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Requirment2(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'APP_VEND_M2'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Approve Vendor2'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OTHER_VEND_M2'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Other Vendors'.
  append gs_fieldcat to gt_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'CLOSING_M2'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Closing2 (MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OPENING_M3'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Opening3(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'REQUIRMENT_M3'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Requirment3(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'APP_VEND_M3'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Approve Vendor3'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OTHER_VEND_M3'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Other Vendors'.
  append gs_fieldcat to gt_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'CLOSING_M3'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Closing3 (MT)'.
  append gs_fieldcat to gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Cost Plus Formula'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA1'.
  gs_fieldcat-ref_table   = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Competitor Data '.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
*  ENDLOOP.

ENDFORM.                    " FIELDCAT

*
**&---------------------------------------------------------------------*
**&      Form  DISPLAY
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM display .
*
**  gs_layout-colwidth_optimize = 'X'.
**  gs_layout-colwidth_optimize = 'X'.
**  gs_layout-info_fieldname = 'COLOR'.
*  lc_s_glay-edt_cll_cb = 'X'.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*   EXPORTING
**    I_INTERFACE_CHECK                 = ' '
**    I_BYPASSING_BUFFER                = ' '
**    I_BUFFER_ACTIVE                   = ' '
*     i_callback_program                 = sy-repid
**    I_CALLBACK_PF_STATUS_SET          = ' '
**     i_callback_user_command            = form_callback
*     i_callback_top_of_page             = 'TOP_PAGE'
**    I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**    I_CALLBACK_HTML_END_OF_LIST       = ' '
**    I_STRUCTURE_NAME                  =
**    I_BACKGROUND_ID                   = ' '
**    I_GRID_TITLE                      =
*    i_grid_settings                   = lc_s_glay
*     is_layout                          = gs_layout
*     it_fieldcat                        = gt_fieldcat
*
*    TABLES
*      t_outtab                          = gt_final[]
**  EXCEPTIONS
**    PROGRAM_ERROR                     = 1
**    OTHERS                            = 2
*            .
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " DISPLAY
*
*
**&---------------------------------------------------------------------*
**&      Form  TOP_PAGE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM top_page.
*  DATA : dd(2) TYPE c,
*         mm(2) TYPE c,
*         yy(4) TYPE c,
*         lv_date(10) TYPE c.
*
*  CLEAR gt_head.
*  gs_head-typ = 'H'.
*  gs_head-info = 'Proposal - Final Confirmation Report'.
*  APPEND gs_head TO gt_head.
*
*  dd  = sy-datum+6(2).
*  mm  = sy-datum+4(2).
*  yy  = sy-datum+0(4).
*  CONCATENATE dd mm yy INTO lv_date SEPARATED BY '.'.
*  gs_head-typ   = 'S'.
*  gs_head-key   = 'Run Date :'.
*  gs_head-info  = lv_date.
*  APPEND gs_head TO gt_head.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      it_list_commentary = gt_head.
**     I_LOGO             =
**     I_END_OF_LIST_GRID =
**     I_ALV_FORM         =
*
*ENDFORM.                    "TOP_PAGE
*&---------------------------------------------------------------------*
*&      Module  STATUS_0509  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0509 OUTPUT.
  IF ALV_CONTAINER IS INITIAL.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITLE'.
 CREATE OBJECT alv_container
   EXPORTING
     container_name              = 'ALV_CONTAINER'.
 IF SY-SUBRC = 0.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent          = ALV_CONTAINER .
 ENDIF.
 PERFORM fieldcat.
   W_VARIANT-REPORT = SY-REPID.
   gs_layout-zebra = 'X'.
*   gs_layout-info_fname = 'COLOR'.
   gs_layout-grid_title = 'List'.
   gs_layout-cwidth_opt  = 'X'.
   CALL METHOD ALV_GRID->set_table_for_first_display
      EXPORTING
        is_variant                    =  W_VARIANT
        i_save                        = 'A'
        is_layout                      = gs_layout

      CHANGING
        it_outtab                     = GT_FINAL[]
        it_fieldcatalog               = GT_FIELDCAT[]. "it_fieldcatalog
*       PERFORM fieldcat.
 ENDIF.
ENDMODULE.                 " STATUS_0509  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0509  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0509 INPUT.
  DATA LV_UCOMM TYPE SY-UCOMM.

  LV_UCOMM = SY-UCOMM.
  CASE LV_UCOMM.
    WHEN 'CANCEl' OR 'EXIT'.
      PERFORM FREE_OBJECTS.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM FREE_OBJECTS.
      SET SCREEN '0'.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0509  INPUT
*&---------------------------------------------------------------------*
*&      Form  FREE_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FREE_OBJECTS .
  CALL METHOD ALV_GRID->FREE
    EXCEPTIONS
      CNTL_ERROR        = 1
      CNTL_SYSTEM_ERROR = 2
      OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  CALL METHOD ALV_CONTAINER->FREE
    EXCEPTIONS
      CNTL_ERROR        = 1
      CNTL_SYSTEM_ERROR = 2
      OTHERS            = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " FREE_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  F4_HELP_PRNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_HELP_PRNO .
 select prno from zmm_pur_proposal into table lt_f4_prno where pr_user = sy-uname.
  sort lt_f4_prno by prno.
  delete adjacent duplicates from lt_f4_prno comparing prno.
  refresh it_ret.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
   exporting
     retfield    = 'GV_PRNO'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0509'
     dynprofield = 'S_PRNO-LOW'
     tables
     value_tab   = lt_f4_prno
     return_tab     = it_ret
  exceptions
   parameter_error = 1
   no_values_found = 2
   others          = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3.
  else.
    read table it_ret index 1.
    S_PRNO-LOW = it_ret-fieldval.
  endif.
ENDFORM.                    " F4_HELP_PRNO
