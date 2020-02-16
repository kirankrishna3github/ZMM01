*&---------------------------------------------------------------------*
*& Report  ZMM_SUBCON_DELIVERY_CHALLAN                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Program Name        :  ZMM_SUBCON_DELIVERY_CHALLAN                  *
*& Creation Date       :  27.11.2017                                   *
*& Author              :  Naren Karra                                  *
*& Functional          :                                               *
*& Application Area    :  MM                                           *
*& Development Spec ID :                                               *
*&---------------------------------------------------------------------*
*& Description:        :                                               *
*& Inputs              :                                               *
*& Outputs             :                                               *
*& Scheduling          :  Back ground and foreground.                  *
*& External Routines   :  N/A                                          *
*& Assumptions/Restriction:                                            *
*& Change History:                                                     *
*&=====================================================================*
*& Date            Change #              | Changed By | Description    *
*& Nov 27, 2017   | IRDK930245           | ABAP01     | Initial        *
*&                                                      Development    *
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*   REVISION NO: 01
*   CHANGED BY: 6010859; SaurabhK
*   CHANGE ON:  Wednesday, July 25, 2018 10:08:47
*   REASON FOR CHANGE: MM: S_K: ZMM080/81: Fixes for region f4: 25.7.18
*   REQUEST #: IRDK932631, IRDK932950
* ---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Thursday, March 28, 2019 17:04:31
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : Mr Bhupesh Adoni
*& Rev. Request#              : IHDK901122
*& Rev. Description           : MM: S_K: ZMM080/81: Changes as per Adoni Sir: 28.3.19
*&---------------------------------------------------------------------*
*{   INSERT         SBXK900028                                        2
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Friday, October 05, 2018 20:22:47
* Changed By - 6010859 - SaurabhK
* Purpose    - Simplification list - 2267308 - konv replaced by prcd_elements
* Solution   - konv replaced by prcd_elements
* TR         - SBXK900028 - S4H: S_K: Simplification List: 03.10.2018
*--------------------------------------------------------------------*
*}   INSERT
report zmm_subcon_delivery_challan.
types: begin of ty_final,
         stcd3      type stcd3, " GSTN of Sub Contractor
         regio      type regio, " Sub contractor's state (As per GST)
         lifnr      type lifnr, "
         name1      type name1_gp,
         vbeln      type vbeln_vf, " Delivery Challan no
         fkdat      type fkdat,  " Delivery challan date
         matnr      type matnr,  " SAP item code
         arktx      type arktx,  " item text
         meins      type meins,  " UOM
         gstn_uom   type zgstn_uom-gstn_uom,  " IHDK901122; gstn uom
         fklmg      type fklmg,  " qty
         wavwr      type wavwr,  " Qty * rate(Rate-same as which is printed on Delivery doc.)
*          Type of Goods (capital /inputs)
         kbetr_cgst type kbetr, "  Tax Rate % IGST
         kbetr_igst type kbetr, "Tax Rate % CGST
         kbetr_ugst type kbetr, "Tax Rate % SGST
*          Cess
*        plant -- IHDK901122
         plant      type t001w-werks,
         plnt_desc  type t001w-name1,
         plnt_gstn  type kna1-stcd3,
         pono       type ekko-ebeln,
*        End IHDK901122
         division   type spart, " IHDK902476
       end of ty_final,

       begin of ty_t001w,
         werks type t001w-werks,
         name1 type t001w-name1,  " IHDK901122
         regio type t001w-regio,
         land1 type t001w-land1,
         stcd3 type kna1-stcd3,   " IHDK901122
       end of ty_t001w,

       begin of ty_mkpf_mseg,
         mblnr     type wb2_v_mkpf_mseg2-mblnr,
         mjahr     type wb2_v_mkpf_mseg2-mjahr,
         zeile     type wb2_v_mkpf_mseg2-zeile_i,
         line_id   type wb2_v_mkpf_mseg2-line_id_i,
         parent_id type wb2_v_mkpf_mseg2-parent_id_i,
         xauto     type wb2_v_mkpf_mseg2-xauto_i,
         vgart     type wb2_v_mkpf_mseg2-vgart,
         blart     type wb2_v_mkpf_mseg2-blart,
         bldat     type wb2_v_mkpf_mseg2-bldat,
         budat     type wb2_v_mkpf_mseg2-budat,
         xblnr     type wb2_v_mkpf_mseg2-xblnr,
         bwart     type wb2_v_mkpf_mseg2-bwart_i,
         matnr     type wb2_v_mkpf_mseg2-matnr_i,
         werks     type wb2_v_mkpf_mseg2-werks_i,
         lifnr     type wb2_v_mkpf_mseg2-lifnr_i,
         menge     type wb2_v_mkpf_mseg2-menge_i,
         meins     type wb2_v_mkpf_mseg2-meins_i,
         dmbtr     type wb2_v_mkpf_mseg2-dmbtr_i,
         ebeln     type wb2_v_mkpf_mseg2-ebeln_i,
         ebelp     type wb2_v_mkpf_mseg2-ebelp_i,
         spart     type mara-spart, " IHDK902476
       end of ty_mkpf_mseg.

data: gs_final     type ty_final,
      gt_final     type table of ty_final,
      gs_mkpf_mseg type ty_mkpf_mseg,
      gt_mkpf_mseg type table of ty_mkpf_mseg,
      gs_t001w     type ty_t001w,
      gt_t001w     type table of ty_t001w,
      gt_vbrk      type table of vbrk,
      gs_vbrk      type vbrk,
      gt_vbrp      type table of vbrp,
      gs_vbrp      type vbrp,
*{   REPLACE        SBXK900028                                        1
*\      gt_konv      TYPE TABLE OF konv,
*\      gs_konv      TYPE konv,
      gt_konv      type table of prcd_elements,
      gs_konv      type prcd_elements,
*}   REPLACE
      gt_mbew      type table of mbew,
      gs_mbew      type mbew.

data: gv_date  type mkpf-budat,
      gv_regio type t001w-regio,
      gv_werks type t001w-werks,
      gv_matnr type mseg-matnr,
      gv_fkdat type vbrk-fkdat,
      gv_spart type mara-spart.

data: gv_noauthorisation type flag.
*----------------------------------------------------------------------*
*  C O N S T A N T S     D E C L A R A T I O N S                       *
*----------------------------------------------------------------------*
constants: c_x        value 'X'.
data: gs_layout   type slis_layout_alv,
      layout      type slis_layout_alv,
      events      type slis_t_event,
      gv_repid    type sy-repid,
      g_variant   type disvariant,
      gx_variant  type disvariant,
      p_vairaint  type disvariant,
      alv_print   type slis_print_alv,
      gv_flg_cust,                                          "#EC NEEDED
      gv_flg_sale,                                          "#EC NEEDED
      g_save,                                               "#EC NEEDED
      g_exit,                                               "#EC NEEDED
      flag.                                                 "#EC NEEDED
*----------------------------------------------------------------------*
*   S E L E C T I O N    S C R E E N                                   *
*----------------------------------------------------------------------*
selection-screen begin of block b01 with frame title text-b01.

select-options: s_regio  for gv_regio obligatory,
                s_date   for gv_date obligatory,
                s_fkdat  for gv_fkdat,
                s_werks  for gv_werks,
                s_matnr  for gv_matnr,
                s_spart  for gv_spart.
selection-screen skip.
selection-screen begin of block b02 with frame title text-b02.
parameters: p_vari type disvariant-variant.
selection-screen end of block b02.
selection-screen end of block b01.

*--------------------------------------------------------------------*
* Local class for f4 callback - IHDK901122
*--------------------------------------------------------------------*
class lcl_f4_callback definition.
  public section.
    interfaces if_f4callback_value_request.
endclass.

class lcl_f4_callback implementation.
  method if_f4callback_value_request~f4_call_callback.
    data lt_dynpfields type standard table of dynpread.
    refresh lt_dynpfields.

    if s_regio[] is initial.  " f4 without an intermediate ok code
      call function 'DYNP_VALUES_READ'
        exporting
          dyname                   = cl_abap_syst=>get_current_program( )
          dynumb                   = sy-dynnr
          perform_conversion_exits = abap_true
          request                  = 'A'
        tables
          dynpfields               = lt_dynpfields
        exceptions
          invalid_abapworkarea     = 1
          invalid_dynprofield      = 2
          invalid_dynproname       = 3
          invalid_dynpronummer     = 4
          invalid_request          = 5
          no_fielddescription      = 6
          invalid_parameter        = 7
          undefind_error           = 8
          double_conversion        = 9
          stepl_not_found          = 10
          others                   = 11.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      loop at lt_dynpfields into data(ls_dynpfields)
        where ( fieldname eq 'S_REGIO-LOW' or fieldname eq 'S_REGIO-HIGH' )
        and fieldvalue is not initial.
        if s_regio[] is initial.
          append initial line to s_regio assigning field-symbol(<ls_regio>).
        else.
          read table s_regio assigning <ls_regio> index 1.
        endif.
        if <ls_regio> is assigned.
          <ls_regio>-sign = 'I'.
          <ls_regio>-option = cond #( when ls_dynpfields-fieldname cs 'HIGH' then 'BT' else 'EQ' ).
          <ls_regio>-low = cond #( when ls_dynpfields-fieldname cs 'LOW' then ls_dynpfields-fieldvalue else <ls_regio>-low ).
          <ls_regio>-high = cond #( when ls_dynpfields-fieldname cs 'HIGH' then ls_dynpfields-fieldvalue else <ls_regio>-high ).
        endif.
        clear ls_dynpfields.
        unassign <ls_regio>.
      endloop.
    endif.

    select werks as plant
      from t001w
      into table @data(lt_plant)
      where regio in @s_regio.

    check lt_plant is not initial.
    loop at lt_plant into data(ls_plant).
      append initial line to cs_shlp-selopt assigning field-symbol(<ls_selopt>).
      if <ls_selopt> is assigned.
        <ls_selopt>-shlpname = cs_shlp-shlpname.
        <ls_selopt>-shlpfield = 'WERKS'.
        <ls_selopt>-sign = 'I'.
        <ls_selopt>-option = 'EQ'.
        <ls_selopt>-low = ls_plant-plant.
      endif.
      clear ls_plant.
      unassign <ls_selopt>.
    endloop.
  endmethod.
endclass.
*--------------------------------------------------------------------*
* End IHDK901122
*--------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   I N I T I A L I Z A T I O N                                        *
*----------------------------------------------------------------------*
initialization.
  gv_repid = sy-repid.
  perform initialize_variant.
*----------------------------------------------------------------------*
*   A T   S E L E C T I O N    S C R E E N                             *
*----------------------------------------------------------------------*
at selection-screen on value-request for p_vari.
  perform f4_for_variant.

at selection-screen on value-request for s_regio-low.   " IRDK932631
  perform f4_regio using 'S_REGIO-LOW'.

at selection-screen on value-request for s_regio-high.  " IRDK932631
  perform f4_regio using 'S_REGIO-HIGH'.

at selection-screen on value-request for s_werks-low.   " IHDK901122
  perform f4_werks.

at selection-screen on value-request for s_werks-high.  " IHDK901122
  perform f4_werks.
*----------------------------------------------------------------------*
*   S T A R T   O F   S E L E C T I O N                                *
*----------------------------------------------------------------------*
start-of-selection.
  perform get_data.
*----------------------------------------------------------------------*
*   E N D   O F   S E L E C T I O N                                    *
*----------------------------------------------------------------------*
end-of-selection.
  if gt_final is not initial.
    perform final_display.
  else.
    message 'No data fetched for given selection' type 'S' display like 'W'.
  endif.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initialize_variant .
  g_save = 'A'.
  clear g_variant.
  g_variant-report = gv_repid.
  g_variant-variant = p_vari.
  gx_variant = g_variant.

  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save     = g_save
    changing
      cs_variant = gx_variant
    exceptions
      not_found  = 2.
  if sy-subrc = 0.
    p_vari = gx_variant-variant.
    g_variant = gx_variant.
  endif.

  layout-get_selinfos = 'X'.
  layout-group_change_edit = 'X'.
endform.
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_for_variant .
  g_save = 'A'.
  g_variant-report = gv_repid.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = g_variant
      i_save     = g_save
    importing
      e_exit     = g_exit
      es_variant = gx_variant
    exceptions
      not_found  = 2.
  if sy-subrc = 2.
    message id sy-msgid type 'S'      number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    if g_exit = space.
      p_vari = gx_variant-variant.
    endif.
    flag = 1.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       IHDK901122 - Restructured to change the base to vbrk - challan; selection date = vbrk-fkdat
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data .
  select a~werks    " IHDK901122 - query modified for plant details
           a~name1
           a~regio
           a~land1
           b~stcd3
          from t001w as a
          join kna1 as b
          on b~kunnr eq a~kunnr
          into table gt_t001w
          where a~werks in s_werks
          and   a~regio in s_regio.

*---  Authority Check Added by Sandeep for Plant on 05.07.2019 ---------*
  clear: gv_noauthorisation.

  if gt_t001w[] is not initial.
    loop at gt_t001w into gs_t001w.
      authority-check object 'M_MATE_WRK'
                id 'ACTVT' field '03'
                id 'WERKS' field gs_t001w-werks.
      if sy-subrc ne 0.
        delete gt_t001w where werks = gs_t001w-werks.
        gv_noauthorisation = 'X'. "GS_T001W-WERKS.
      endif.

      clear: gs_t001w.
    endloop.

    if gv_noauthorisation = 'X'.
      message 'Authorization is missing. Please check SU53' type 'I' display like 'W'.
    endif.
  endif.
*--- End-of-Addition ---------------------------------------------------*

  if gt_t001w is not initial.
    select a~mblnr
           a~mjahr
           a~zeile_i
           a~line_id_i
           a~parent_id_i
           a~xauto_i
           a~vgart
           a~blart
           a~bldat
           a~budat
           a~xblnr
           a~bwart_i
           a~matnr_i
           a~werks_i
           a~lifnr_i
           a~menge_i
           a~meins_i
           a~dmbtr_i
           a~ebeln_i
           a~ebelp_i
           b~spart      " IHDK902476
      from wb2_v_mkpf_mseg2 as a
      inner join mara as b
      on a~matnr_i eq b~matnr
      into table gt_mkpf_mseg
      for all entries in gt_t001w
      where a~vgart = 'WA'     " Good Issue, Transfer posting, other goods movements
      and   a~budat in s_date
      and   a~werks_i = gt_t001w-werks
      and   a~bwart_i eq '541' " GI whse to subc.stck
      and   a~matnr_i in s_matnr
      and   b~spart in s_spart.
*      and   xauto_i = ''.    " No automatically created lines

    " IHDK902476
    clear gv_noauthorisation.
    loop at gt_mkpf_mseg into gs_mkpf_mseg.
      authority-check object 'ZMM_SPART'
       id 'ACTVT' field '03'
       id 'SPART' field gs_mkpf_mseg-spart.
      if sy-subrc <> 0.
        delete gt_mkpf_mseg where spart = gs_mkpf_mseg-spart.
        gv_noauthorisation = abap_true.
      endif.
      clear gs_mkpf_mseg.
    endloop.

    if gv_noauthorisation eq abap_true.
      message 'Authorisation missing. Check SU53' type 'I' display like 'W'.
    endif.

    if gt_mkpf_mseg is not initial.
      select *
        from vbrp
        into table gt_vbrp
        for all entries in gt_mkpf_mseg
        where vgbel = gt_mkpf_mseg-mblnr .

      if gt_vbrp is not initial.
        select *
          from vbrk
          into table gt_vbrk
          for all entries in gt_vbrp
          where vbeln = gt_vbrp-vbeln
          and   fkdat in s_fkdat  " IHDK901122
          and   fkart = 'ZSN'.    " Subcon. Challan

        if gt_vbrk is not initial.
          select *
*{   REPLACE        SBXK900028                                        1
*\            FROM konv
            from prcd_elements
*}   REPLACE
            into table gt_konv
            for all entries in gt_vbrk
            where knumv = gt_vbrk-knumv.
        endif.
      endif.

      select *
        from mbew
        into table gt_mbew
        for all entries in gt_mkpf_mseg
        where matnr = gt_mkpf_mseg-matnr
        and   bwkey = gt_mkpf_mseg-werks.
    endif.
  endif.

  perform final_processing.
endform.
*&---------------------------------------------------------------------*
*&      Form  FINAL_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form final_display .
  data :ls_event_exit type slis_event_exit,                 "#EC NEEDED
        lt_event_exit type slis_t_event_exit.               "#EC NEEDED

  data: it_fieldcat type slis_t_fieldcat_alv,
        wa_fieldcat type slis_fieldcat_alv,
        lv_pos      type i value 1.

  gs_layout-zebra = c_x.
  gs_layout-colwidth_optimize = c_x.
  gs_layout-group_change_edit = c_x.
  gs_layout-allow_switch_to_list = c_x.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = gv_repid
      i_structure_name       = 'ZREP_541' " IHDK901122: Added plant related fields
      i_client_never_display = 'X'
    changing
      ct_fieldcat            = it_fieldcat.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  loop at it_fieldcat into wa_fieldcat.
    if wa_fieldcat-fieldname  = 'STCD3'.
      wa_fieldcat-fieldname  = 'STCD3'.
      wa_fieldcat-seltext_s  = 'GST No'.
      wa_fieldcat-seltext_m  = 'GST No of Sub-Con'.
      wa_fieldcat-seltext_l  = 'GST No of Sub-Contractor'.
*        wa_fieldcat-reptext_ddic  = 'Customer'.                             " heading (ddic)
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'REGIO'.
      wa_fieldcat-fieldname  = 'REGIO'.
      wa_fieldcat-seltext_s  = 'Sub-Contractors State(As per GST)'.
      wa_fieldcat-seltext_m  = 'Sub-Contractors State(As per GST)'.
      wa_fieldcat-seltext_l  = 'Sub-Contractors State(As per GST)'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'LIFNR'.
      wa_fieldcat-fieldname  = 'LIFNR'.
      wa_fieldcat-seltext_s  = 'SAP Vendor Code(Sub-Contractor)'.
      wa_fieldcat-seltext_m  = 'SAP Vendor Code(Sub-Contractor)'.
      wa_fieldcat-seltext_l  = 'SAP Vendor Code(Sub-Contractor)'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'NAME1'.
      wa_fieldcat-fieldname  = 'NAME1'.
      wa_fieldcat-seltext_s  = 'Vendor Name(Sub-Contractor)'.
      wa_fieldcat-seltext_m  = 'Vendor Name(Sub-Contractor)'.
      wa_fieldcat-seltext_l  = 'Vendor Name(Sub-Contractor)'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.

    if wa_fieldcat-fieldname  = 'VBELN'.
      wa_fieldcat-fieldname  = 'VBELN'.
      wa_fieldcat-seltext_s  = 'Delivery Challan No'.
      wa_fieldcat-seltext_m  = 'Delivery Challan No'.
      wa_fieldcat-seltext_l  = 'Delivery Challan No'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'FKDAT'.
      wa_fieldcat-fieldname  = 'FKDAT'.
      wa_fieldcat-seltext_s  = 'Delivery Challan Date'.
      wa_fieldcat-seltext_m  = 'Delivery Challan Date'.
      wa_fieldcat-seltext_l  = 'Delivery Challan Date'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'MATNR'.
      wa_fieldcat-fieldname  = 'MATNR'.
      wa_fieldcat-seltext_s  = 'SAP Item Code'.
      wa_fieldcat-seltext_m  = 'SAP Item Code'.
      wa_fieldcat-seltext_l  = 'SAP Item Code'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'ARKTX'.
      wa_fieldcat-fieldname  = 'ARKTX'.
      wa_fieldcat-seltext_s  = 'SAP Item Description'.
      wa_fieldcat-seltext_m  = 'SAP Item Description'.
      wa_fieldcat-seltext_l  = 'SAP Item Description'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'MEINS'.
      wa_fieldcat-fieldname  = 'MEINS'.
      wa_fieldcat-seltext_s  = 'UOM'.
      wa_fieldcat-seltext_m  = 'UOM'.
      wa_fieldcat-seltext_l  = 'Unit of Measurement'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    " IHDK901122
    if wa_fieldcat-fieldname  = 'GSTN_UOM'.
      wa_fieldcat-fieldname  = 'GSTN_UOM'.
      wa_fieldcat-seltext_s  = 'GSTN UOM'.
      wa_fieldcat-seltext_m  = 'GSTN UOM'.
      wa_fieldcat-seltext_l  = 'GSTN Unit of Measurement'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    " End IHDK901122
    if wa_fieldcat-fieldname  = 'FKLMG'.
      wa_fieldcat-fieldname  = 'FKLMG'.
      wa_fieldcat-seltext_s  = 'Quantity'.
      wa_fieldcat-seltext_m  = 'Quantity'.
      wa_fieldcat-seltext_l  = 'Quantity'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'WAVWR'.
      wa_fieldcat-fieldname  = 'WAVWR'.
      wa_fieldcat-seltext_s  = 'Taxable Value'.
      wa_fieldcat-seltext_m  = 'Taxable Value'.
      wa_fieldcat-seltext_l  = 'Taxable Value'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'KBETR_IGST'.
      wa_fieldcat-fieldname  = 'KBETR_IGST'.
      wa_fieldcat-seltext_s  = 'IGST'.
      wa_fieldcat-seltext_m  = 'Rate IGST'.
      wa_fieldcat-seltext_l  = 'Tax Rate % IGST'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'KBETR_CGST'.
      wa_fieldcat-fieldname  = 'KBETR_CGST'.
      wa_fieldcat-seltext_s  = 'CGST'.
      wa_fieldcat-seltext_m  = 'Rate CGST'.
      wa_fieldcat-seltext_l  = 'Tax Rate % CGST'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'KBETR_UGST'.
      wa_fieldcat-fieldname  = 'KBETR_UGST'.
      wa_fieldcat-seltext_s  = 'UGST'.
      wa_fieldcat-seltext_m  = 'Rate UGST'.
      wa_fieldcat-seltext_l  = 'Tax Rate % UGST'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    " IHDK901122
    if wa_fieldcat-fieldname  = 'PLANT'.
      wa_fieldcat-fieldname  = 'PLANT'.
      wa_fieldcat-seltext_s  = 'Plant'.
      wa_fieldcat-seltext_m  = 'Plant'.
      wa_fieldcat-seltext_l  = 'Plant'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'PLNT_DESC'.
      wa_fieldcat-fieldname  = 'PLNT_DESC'.
      wa_fieldcat-seltext_s  = 'Plant Name'.
      wa_fieldcat-seltext_m  = 'Plant Name'.
      wa_fieldcat-seltext_l  = 'Plant Name'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    if wa_fieldcat-fieldname  = 'PLNT_GSTN'.
      wa_fieldcat-fieldname  = 'PLNT_GSTN'.
      wa_fieldcat-seltext_s  = 'Plant GSTN'.
      wa_fieldcat-seltext_m  = 'Plant GSTN'.
      wa_fieldcat-seltext_l  = 'Plant GSTN'.
*      wa_fieldcat-outputlen  = 17.
      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endif.
    " End IHDK901122
  endloop.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = gv_repid
*     i_structure_name   = 'ZREP_541'
      is_layout          = gs_layout
      it_fieldcat        = it_fieldcat
      i_save             = 'A'
    tables
      t_outtab           = gt_final.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  FINAL_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form final_processing .
  check gt_mkpf_mseg is not initial.

  sort gt_vbrp by vgbel matnr vgpos ascending.
  sort gt_t001w by werks ascending.
  sort gt_mbew by matnr bwkey ascending.
  sort gt_vbrk by vbeln ascending.
  sort gt_konv by knumv kschl ascending.

  loop at gt_mkpf_mseg into gs_mkpf_mseg where xauto = abap_false.

    try.
        data(lv_zeile) = gt_mkpf_mseg[ mblnr = gs_mkpf_mseg-mblnr
                                       mjahr = gs_mkpf_mseg-mjahr
                                       parent_id = gs_mkpf_mseg-line_id ]-zeile.
      catch cx_sy_itab_line_not_found.
    endtry.

    clear:gs_t001w.
    read table gt_t001w into gs_t001w with key werks = gs_mkpf_mseg-werks.
    if sy-subrc eq 0.
      move: gs_t001w-regio to gs_final-regio.
      " IHDK901122
      move: gs_t001w-werks to gs_final-plant.
      move: gs_t001w-name1 to gs_final-plnt_desc.
      move: gs_t001w-stcd3 to gs_final-plnt_gstn.
      " End IHDK901122
    endif.

    move: gs_mkpf_mseg-lifnr to gs_final-lifnr,
          gs_mkpf_mseg-matnr to gs_final-matnr.

    " IHDK902476
    gs_final-division = gs_mkpf_mseg-spart.

    clear:gs_vbrp.
    read table gt_vbrp into gs_vbrp with key vgbel = gs_mkpf_mseg-mblnr
                                             matnr = gs_mkpf_mseg-matnr
                                             vgpos+2(4) = gs_mkpf_mseg-zeile binary search.
    if sy-subrc <> 0.
      read table gt_vbrp into gs_vbrp with key vgbel = gs_mkpf_mseg-mblnr
                                             matnr = gs_mkpf_mseg-matnr
                                             vgpos+2(4) = lv_zeile binary search. "gs_mkpf_mseg-zeile.
    endif.
    " IHDK901122; fix missing challans due to line item mismatch
    if gs_vbrp is not initial.

      gs_final-pono = gs_mkpf_mseg-ebeln.

      move:
        gs_vbrp-fkimg to gs_final-fklmg,
        gs_vbrp-meins to gs_final-meins,
*        gs_vbrp-wavwr TO gs_final-wavwr,
        gs_vbrp-arktx to gs_final-arktx.

      " IHDK901122
      select single gstn_uom from zgstn_uom into gs_final-gstn_uom where sap_uom = gs_final-meins.

      clear:gs_mbew.
      read table gt_mbew into gs_mbew with key matnr = gs_mkpf_mseg-matnr bwkey = gs_mkpf_mseg-werks binary search.
      if sy-subrc eq 0.
*        gs_vbrp-wavwr
        gs_final-wavwr = gs_mbew-verpr * gs_final-fklmg.
      endif.
      clear:gs_vbrk.
      read table gt_vbrk into gs_vbrk with key vbeln = gs_vbrp-vbeln binary search.
      if sy-subrc eq 0.
        move:
          gs_vbrk-vbeln to gs_final-vbeln,
          gs_vbrk-fkdat to gs_final-fkdat.
        clear:gs_konv.
        read table gt_konv into gs_konv with key knumv = gs_vbrk-knumv kschl = 'JOCG' binary search.
        if sy-subrc eq 0.
          move:
          gs_konv-kbetr to gs_final-kbetr_cgst.
        endif.
        clear:gs_konv.
        read table gt_konv into gs_konv with key knumv = gs_vbrk-knumv kschl = 'JOIG' binary search.
        if sy-subrc eq 0.
          move:
          gs_konv-kbetr to gs_final-kbetr_igst.
        endif.
        clear:gs_konv.
        read table gt_konv into gs_konv with key knumv = gs_vbrk-knumv kschl = 'JOUG' binary search.
        if sy-subrc eq 0.
          move:
          gs_konv-kbetr to gs_final-kbetr_ugst.
        endif.
        if gs_mkpf_mseg-lifnr is not initial.
          clear: gs_final-name1, gs_final-stcd3.
          select single name1 stcd3 from lfa1 into (gs_final-name1, gs_final-stcd3) where lifnr = gs_mkpf_mseg-lifnr.
        endif.
        append gs_final to gt_final.
        clear gs_final.
      endif.
    endif.
    clear: gs_mkpf_mseg, lv_zeile.
  endloop.
endform.

*&---------------------------------------------------------------------*
*&      Form  F4_REGIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_regio using dynprofield type help_info-dynprofld. " IRDK932631
  data: begin of regio,
          bezei type t005u-bezei,
          bland type t005u-bland,
        end of regio,
        regio_tab like standard table of regio.

  select bezei bland  " IRDK932950
    from t005u
    into table regio_tab
    where spras = 'E'
    and   land1 = 'IN'.

  delete adjacent duplicates from regio_tab comparing all fields.

  data return_tab type standard table of ddshretval.

  refresh return_tab.
  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'REGIO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = dynprofield
      window_title    = 'Select Region/State'
      value_org       = 'S'
    tables
      value_tab       = regio_tab
      return_tab      = return_tab
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
endform.

form f4_werks.  " IHDK901122
  data lt_return type standard table of ddshretval.

  refresh lt_return.
  data lo_f4_callback type ref to if_f4callback_value_request.
  free lo_f4_callback.
  lo_f4_callback ?= new lcl_f4_callback( ).
  call function 'F4IF_FIELD_VALUE_REQUEST'
    exporting
      tabname           = 'T001W'
      fieldname         = 'WERKS'
      callback_program  = cl_abap_syst=>get_current_program( )
      callback_method   = lo_f4_callback
    tables
      return_tab        = lt_return
    exceptions
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      others            = 5.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.
endform.
