*&---------------------------------------------------------------------*
*& Report  ZMM_PROPOSAL_FINAL_CONF_COMMON
*&***********************************************************************************************&*
*& OBJECT NAME          : ZMM_PROPOSAL_FINAL_CONF_COMMON                                                         &*
*& TECHNICAL CONSULTANT : RAVINDRA SANAP                                                         &*
*& TEAM LEAD            : POONAM SHINDE                                                          &*
*& FUCTIONAL            : VENU                                                                   &*
*& MODULE NAME          : MM                                                                     &*
*& PROGRAM TYPE         : REPORT                                                                 &*
*& TCODE                : ZMM019                                                                 &*
*& CREATE DATE          : Dec 26,2016                                                            &*
*& TRANSPORT NO         : IRDK926645                                                             &*
*& DESCRIPTION          : REPORT IS DEVELOPED FOR All Releasr's Status -Proposal                 &*
*&                                                                                               &*
* REVISION HISTORY--------------------------------------------------------------------------------*
*                                                                                                 *
*   CHANGED BY:                                                                                   *
*   CHANGE ON:                                                                                    *
*   REASON FOR CHANGE:                                                                            *
*                                                                                                 *
*                                                                                                 *
* REVISION HISTORY--------------------------------------------------------------------------------*

REPORT  ZMM_PROPOSAL_FINAL_CONF_COMMON.

TYPE-POOLS: SLIS.

DATA:  gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv,
       gs_layout   TYPE slis_layout_alv,
       gt_head     TYPE slis_t_listheader,
       gs_head     TYPE slis_listheader.

TYPES : begin of ty_zmm_pur_proposal,
        prno             type zmm_pur_proposal-prno,
        srno             type i,
*        id               TYPE icon-id,
*        style            TYPE     lvc_t_styl,   "for disable
        gjahr            type zmm_pur_proposal-gjahr,
        pr_itemno        type zmm_pur_proposal-pr_itemno,
        level            type  c,
        pr_date          type zmm_pur_proposal-pr_date,
        ekgrp            type zmm_pur_proposal-ekgrp,
        eknam            type zmm_pur_proposal-eknam,
        bukrs            type zmm_pur_proposal-bukrs,
        werks            type zmm_pur_proposal-werks,
        plantname        type zmm_pur_proposal-plantname,
        matnr            type zmm_pur_proposal-matnr,
        maktx            type zmm_pur_proposal-maktx,
        lifnr            type zmm_pur_proposal-lifnr,
        name1            type zmm_pur_proposal-name1,
        menge            type zmm_pur_proposal-menge,
        netpr            type zmm_pur_proposal-netpr,
        meins            type zmm_pur_proposal-meins,
        waers            type zmm_pur_proposal-waers,
        mwskz            type zmm_pur_proposal-mwskz,
        tax_desc         type zmm_pur_proposal-tax_desc,
        land_pr          type zmm_pur_proposal-land_pr,
        land_value       type zmm_pur_proposal-land_value,
        last_basicpr     type zmm_pur_proposal-last_basicpr,
        last_landpr      type zmm_pur_proposal-last_landpr,
        business_planpr  type zmm_pur_proposal-business_planpr,
        vari_last_pur_in type zmm_pur_proposal-vari_last_pur_in,
        vari_last_pur_va type zmm_pur_proposal-vari_last_pur_va,
        vari_bus_pl_inr  type zmm_pur_proposal-vari_bus_pl_inr,
        vari_bus_pl_val  type zmm_pur_proposal-vari_bus_pl_val,
        opening_m1       type zmm_pur_proposal-opening_m1,
        requirment_m1    type zmm_pur_proposal-requirment_m1,
        app_vend_m1      type zmm_pur_proposal-app_vend_m1,
        other_vend_m1    type zmm_pur_proposal-other_vend_m1,
        closing_m1       type zmm_pur_proposal-closing_m1,
        opening_m2       type zmm_pur_proposal-opening_m2,
        requirment_m2    type zmm_pur_proposal-requirment_m2,
        app_vend_m2      type zmm_pur_proposal-app_vend_m2,
        other_vend_m2    type zmm_pur_proposal-other_vend_m2,
        closing_m2       type zmm_pur_proposal-closing_m2,
        opening_m3       type zmm_pur_proposal-opening_m3,
        requirment_m3    type zmm_pur_proposal-requirment_m3,
        app_vend_m3      type zmm_pur_proposal-app_vend_m3,
        other_vend_m3    type zmm_pur_proposal-other_vend_m3,
        closing_m3       type zmm_pur_proposal-closing_m3,
        comp_data        type zmm_pur_proposal-comp_data,
        comp_data1       type zmm_pur_proposal-comp_data1,
        line_txt         type zmm_pur_proposal-line_txt,
        final_price      type zmm_pur_proposal-final_price,
        remark           type zmm_pur_proposal-remark,
        releaser1        type zmm_pur_proposal-releaser1,
        releaser_name1   type zmm_pur_proposal-releaser_name1,
        releaser1_date   type zmm_pur_proposal-releaser1_date,
        status1(10)      type c,
        releaser2        type zmm_pur_proposal-releaser2,
        releaser_name2   type zmm_pur_proposal-releaser_name2,
        releaser2_date   type zmm_pur_proposal-releaser2_date,
        status2(10)      type c,
        releaser3        type zmm_pur_proposal-releaser3,
        releaser_name3   type zmm_pur_proposal-releaser_name3,
        releaser3_date   type zmm_pur_proposal-releaser3_date,
        status3(10)      type c,
        releaser4        type zmm_pur_proposal-releaser4,
        releaser_name4   type zmm_pur_proposal-releaser_name4,
        releaser4_date   type zmm_pur_proposal-releaser4_date,
        status4(10)      type c,
        erdat            type zmm_pur_proposal-erdat,
        ch_date          type zmm_pur_proposal-ch_date,
        pr_user          type zmm_pur_proposal-pr_user,
        pr_time          type zmm_pur_proposal-pr_time,
        app_vendor       type zmm_pur_proposal-app_vendor,
        cons_proposal    type zmm_pur_proposal-cons_proposal,
        color(4),

        end of ty_zmm_pur_proposal.

DATA : gt_final             type table of ty_zmm_pur_proposal,
       gs_final             like line of gt_final," ty_zmm_pur_proposal,
       gt_final2            type table of ty_zmm_pur_proposal,
       gs_final2            like line of gt_final," ty_zmm_pur_proposal,
       gt_zmm_pur_proposal  type table of zmm_pur_proposal,
       gs_zmm_pur_proposal  type zmm_pur_proposal.


DATA  :gv_prno(10) type c," VALUE "'SET AND GET PARAMETER'.
       gv_prnr     type persno,
       gv_user     type zmm_pur_proposal-pr_user,
       lv_ans      type c.

TYPES : begin of ty_f4_prno,
       prno type zmm_pur_proposal-prno,
       end of ty_f4_prno.

DATA : lt_f4_prno  type table of ty_f4_prno,
       ls_f4_prno  type ty_f4_prno,
       it_ret      type ddshretval occurs 0 with header line,
       gv_pernr    type persno,
       flag1       type c,
       flag_new    type c,
       lv_index    type sy-tabix.


TABLES : zmm_pur_proposal.

selection-screen : begin of block rad1 with frame title text-001.
*PARAMETERS     : p_werks TYPE zmm_pur_proposal-werks.
*select-options : s_prno for zmm_pur_proposal-prno modif id b no intervals no-extension .
selection-screen : end of block rad1.
*
*at selection-screen on value-request for s_prno-low.
*perform f4_help_prno.

START-OF-SELECTION.
 PERFORM select_data.
 PERFORM fieldcat.
 PERFORM display.

form select_data .

  clear :gv_pernr.
  refresh:gt_zmm_pur_proposal.

  select *
      from zmm_pur_proposal into table gt_zmm_pur_proposal.
                               " WHERE  werks = p_werks
                               " AND    prno in s_prno .
*  IF p_werks IS INITIAL.
*   refresh:gt_zmm_pur_proposal.
*    select *
*      from zmm_pur_proposal into table gt_zmm_pur_proposal.
*  ENDIF.
  sort gt_zmm_pur_proposal by prno.
  gv_pernr = sy-uname.
  delete adjacent duplicates from gt_zmm_pur_proposal comparing all fields.
  perform final_data_approve.
endform.                    " SELECT_DATA


*&---------------------------------------------------------------------*
*&      Form  final_data_approve
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form final_data_approve.
  clear gs_zmm_pur_proposal.
  loop at  gt_zmm_pur_proposal into gs_zmm_pur_proposal.
    shift gs_zmm_pur_proposal-releaser1_date left deleting leading '0'.
    shift gs_zmm_pur_proposal-releaser2_date left deleting leading '0'.
    shift gs_zmm_pur_proposal-releaser3_date left deleting leading '0'.
    shift gs_zmm_pur_proposal-releaser4_date left deleting leading '0'.
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
    gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
    gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
    gs_final-closing_m3       = gs_zmm_pur_proposal-closing_m3.
    gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
    gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
    gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
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
    gs_final-remark           = gs_zmm_pur_proposal-remark.
    gs_final2 = gs_final.
    if gs_zmm_pur_proposal-releaser1 ne ' '.
      if gs_zmm_pur_proposal-app_vendor = 'X'.
        if gs_zmm_pur_proposal-releaser1_date ne ' '.
          gs_final-level   = 1.
          gs_final-status1 = 'Ok'.
          append gs_final to gt_final .
        else.
          if gs_zmm_pur_proposal-app_vendor = 'X'.
            if gs_zmm_pur_proposal-releaser1_date eq ' '.
              gs_final2-level            = 1.
              gs_final2-color            = 'C710'.
              gs_final2-status1         = 'Pending'.
              append gs_final2 to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
    if gs_zmm_pur_proposal-releaser2 ne ''.
      if gs_zmm_pur_proposal-app_vendor = 'X'.
        if gs_zmm_pur_proposal-releaser2_date ne ' '.
          gs_final-level            = 2.
          gs_final-status2          = 'Ok'.
          append gs_final to gt_final .
        else.
          if gs_zmm_pur_proposal-app_vendor = 'X'.
            if gs_zmm_pur_proposal-releaser2_date eq ' '.
              gs_final2-level           = 2.
              gs_final2-status2         = 'Pending'.
              gs_final2-color            = 'C710'.
              append gs_final2 to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
    if gs_zmm_pur_proposal-releaser3 ne ' '.
      if gs_zmm_pur_proposal-app_vendor = 'X'.
        if gs_zmm_pur_proposal-releaser3_date ne ' '.
          gs_final-level            = 3.
          gs_final-status3          = 'Ok'.
          append gs_final to gt_final .
        else.
          if gs_zmm_pur_proposal-app_vendor = 'X'.
            if gs_zmm_pur_proposal-releaser3_date eq ' '.
              gs_final2-level            = 3.
              gs_final2-status3          = 'Pending'.
              gs_final2-color            = 'C710'.
              append gs_final2 to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
    if gs_zmm_pur_proposal-releaser4 ne ' '.
      if gs_zmm_pur_proposal-app_vendor = 'X'.
        if gs_zmm_pur_proposal-releaser4_date ne ' '.
          gs_final-level            = 4.
          gs_final-status4          = 'Ok'.
          append gs_final to gt_final .
        else.
          if gs_zmm_pur_proposal-app_vendor = 'X'.
            if gs_zmm_pur_proposal-releaser4_date eq ' '.
              gs_final2-level            = 4.
              gs_final2-status4          = 'Pending'.
              gs_final2-color            = 'C710'.
              append gs_final2 to gt_final .
            endif.
          endif.
        endif.
      endif.
    endif.
    clear: gs_final,gs_zmm_pur_proposal,gs_final2.
  endloop.

  sort gt_final by prno.
  refresh gt_final2..
  gt_final2[] = gt_final[].
  refresh gt_final.
  data : flag1 type c.
  sort gt_final2 by prno.
  loop at gt_final2 into gs_final2.
    shift gs_final2-releaser1 left deleting leading '0'.
    shift gs_final2-releaser2 left deleting leading '0'.
    shift gs_final2-releaser3 left deleting leading '0'.
    shift gs_final2-releaser4 left deleting leading '0'.
    gs_final = gs_final2.
    at end of prno.
      flag1 = 'X'.
      if flag1 = 'X'.
        append gs_final to gt_final.
      endif.
    endat.
    clear :gs_final2,flag1,gs_final.
  endloop.

 delete adjacent duplicates from gt_final comparing prno.
  refresh gt_final2.
  gt_final2[] = gt_final[].
  refresh gt_final.
  clear: gs_final,gs_final2.

  loop at gt_final2 into gs_final2.
    perform ok_pending.
  endloop.
  delete adjacent duplicates from gt_final comparing prno.
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
  If gs_zmm_pur_proposal-pr_itemno EQ '1'.
  gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno .
  ENDIF.
  If gs_zmm_pur_proposal-pr_itemno NE '1'.
  gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno / 10 .
  ENDIF.
  gs_final-pr_user         = gs_zmm_pur_proposal-pr_user.
  gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
  gs_final-ch_date         = gs_zmm_pur_proposal-ch_date.
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
  gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
  gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
  gs_final-closing_m3       = gs_zmm_pur_proposal-closing_m3.
  gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
  gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
  gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
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
form fieldcat .

  clear gs_fieldcat.
  gs_fieldcat-fieldname  = 'PRNO'.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-seltext_l  = 'Proposal Number'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_ITEMNO'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Best Proposal Sel.Line No'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.

  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Releaser Name1'.
*  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Status 1'.
  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME2'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_M = 'Releaser Name2'.
*  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS2'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Status 2'.
  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME3'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Releaser Name3'.
*  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS3'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Status 3'.
  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME4'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Releaser Name4'.
*  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS4'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Status 4'.
  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'EKGRP'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Purchase Group'.
  gs_fieldcat-just = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'WERKS'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Plant'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_DATE'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Proposal Date'.
  append gs_fieldcat to gt_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'CH_DATE'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Change Date'.
  append gs_fieldcat to gt_fieldcat.

  gs_fieldcat-fieldname = 'PR_USER'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Created By'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_TIME'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Created Time'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'MATNR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Material No'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'MAKTX'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Material Description'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'LIFNR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Vendor No'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'NAME1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Vendor Name'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'MENGE'.
  gs_fieldcat-tabname  = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Quantity'.
*  gs_fieldcat-seltext_l = 'Quantity'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'MEINS'.
  gs_fieldcat-tabname  = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Unit'.
*  gs_fieldcat-seltext_l = 'Quantity'.
  append gs_fieldcat to gt_fieldcat.

  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'NETPR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'New Basic Price INR per KG'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
   gs_fieldcat-fieldname = 'WAERS'.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-seltext_l  = 'Currency'.
  gs_fieldcat-just       = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'MWSKZ'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Tax Code'.
  gs_fieldcat-just      = 'C'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_PR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l   = 'New Landed Price'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_VALUE'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Purchase Value'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_BASICPR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Last Purchase Basic PR(PO)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_LANDPR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Last Purchase Landed Price'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'BUSINESS_PLANPR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Business Plan Price(ZBPL)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_LAST_PUR_IN'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Variance_Last_Purchase_INR_Per_KG'.
  append gs_fieldcat to gt_fieldcat.

  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_LAST_PUR_VA'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Variance_Last_Purchase_Value'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_BUS_PL_INR'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Variance_Business_Plan_INR_Per_KG'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'VARI_BUS_PL_VAL'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Variance_Business_Plan_Value'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OPENING_M1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Opening1(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'REQUIRMENT_M1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Requirment1(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'APP_VEND_M1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Approve Vendor1'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OTHER_VEND_M1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Other Vendors'.
  append gs_fieldcat to gt_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'CLOSING_M1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Closing1 (MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OPENING_M2'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Opening2(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'REQUIRMENT_M2'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Requirment2(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'APP_VEND_M2'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Approve Vendor2'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OTHER_VEND_M2'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Other Vendors'.
  append gs_fieldcat to gt_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'CLOSING_M2'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Closing2 (MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OPENING_M3'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Opening3(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'REQUIRMENT_M3'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Requirment3(MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'APP_VEND_M3'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Approve Vendor3'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'OTHER_VEND_M3'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Other Vendors'.
  append gs_fieldcat to gt_fieldcat.
   clear gs_fieldcat.
  gs_fieldcat-fieldname = 'CLOSING_M3'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Closing3 (MT)'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Cost Plus Formula'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA1'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Competitor Data '.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.

  gs_fieldcat-fieldname = 'LINE_TXT'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Line Remarks '.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.

  gs_fieldcat-fieldname = 'REMARK'.
  gs_fieldcat-tabname   = 'GT_FINAL'.
  gs_fieldcat-seltext_l = 'Overall Remarks'.
  append gs_fieldcat to gt_fieldcat.
  clear gs_fieldcat.

endform.                    " FIELDCAT


*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display .

  gs_layout-colwidth_optimize = 'X'.
*  gs_layout-colwidth_optimize = 'X'.
*  gs_layout-info_fieldname = 'COLOR'.
*  lc_s_glay-edt_cll_cb = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*    I_INTERFACE_CHECK                 = ' '
*    I_BYPASSING_BUFFER                = ' '
*    I_BUFFER_ACTIVE                   = ' '
     i_callback_program                 = sy-repid
*    I_CALLBACK_PF_STATUS_SET          = ' '
*     i_callback_user_command            = form_callback
     i_callback_top_of_page             = 'TOP_PAGE'
*    I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*    I_CALLBACK_HTML_END_OF_LIST       = ' '
*    I_STRUCTURE_NAME                  =
*    I_BACKGROUND_ID                   = ' '
*    I_GRID_TITLE                      =
*    i_grid_settings                   = lc_s_glay
     is_layout                          = gs_layout
     it_fieldcat                        = gt_fieldcat

    TABLES
      t_outtab                          = gt_final[]
*  EXCEPTIONS
*    PROGRAM_ERROR                     = 1
*    OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY


*&---------------------------------------------------------------------*
*&      Form  TOP_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_page.
  DATA : dd(2) TYPE c,
         mm(2) TYPE c,
         yy(4) TYPE c,
         lv_date(10) TYPE c.

  CLEAR gt_head.
  gs_head-typ = 'H'.
  gs_head-info = 'Proposal - Releasers Status Report'.
  APPEND gs_head TO gt_head.

  dd  = sy-datum+6(2).
  mm  = sy-datum+4(2).
  yy  = sy-datum+0(4).
  CONCATENATE dd mm yy INTO lv_date SEPARATED BY '.'.
  gs_head-typ   = 'S'.
  gs_head-key   = 'Run Date :'.
  gs_head-info  = lv_date.
  APPEND gs_head TO gt_head.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_head.
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =

ENDFORM.                    "TOP_PAGE

*&---------------------------------------------------------------------*
*&      Form  F4_HELP_PRNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_help_prno .
*  select prno from zmm_pur_proposal into table lt_f4_prno ."where werks = p_werks.
*  sort lt_f4_prno by prno.
*  delete adjacent duplicates from lt_f4_prno comparing prno.
*  refresh it_ret.
*  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
*   exporting
*     retfield    = 'GV_PRNO'
*     value_org   = 'S'
**       dynpprog    = sy-repid
*     dynpnr      = sy-dynnr "'0509'
*     dynprofield = 'S_PRNO-LOW'
*     tables
*     value_tab   = lt_f4_prno
*     return_tab     = it_ret
*  exceptions
*   parameter_error = 1
*   no_values_found = 2
*   others          = 3.
*  if sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*    with sy-msgv1 sy-msgv2 sy-msgv3.
*  else.
*    read table it_ret index 1.
*    s_prno-low = it_ret-fieldval.
*  endif.
endform.                    " F4_HELP_PRNO
