class zcl_im_6mmb_po_validations definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6MMB_PO_VALIDATIONS
*"* do not include other source files here!!!
  public section.

    interfaces if_badi_interface .
    interfaces if_ex_me_process_po_cust .

    class-data gv_trtyp type trtyp .
    class-data flagt type c .
  protected section.
*"* protected components of class ZCL_IM_6MMB_PO_VALIDATIONS
*"* do not include other source files here!!!
  private section.
*"* private components of class ZCL_IM_6MMB_PO_VALIDATIONS
*"* do not include other source files here!!!

    class-data gv_lifnr type lifnr .
ENDCLASS.



CLASS ZCL_IM_6MMB_PO_VALIDATIONS IMPLEMENTATION.


  method if_ex_me_process_po_cust~check.
    types:begin of type_val_pro,
            ebelp     type ebelp,
            matnr     type matnr,
            prno      type zprno,
            pr_itemno type zpritemno,
          end of type_val_pro.
    data: lt_val_pro type table of type_val_pro,
          ls_val_pro type type_val_pro.
    data : l_items type purchase_order_items.
    data : l_single type purchase_order_item.
    data : l_item_cond type mmpur_tkomv.
    data : ls_cond type komv.
    data : ls_header type mepoheader.
    data : ls_shipping_data type ekpv. "Shipping Data For Stock Transfer of Purchasing Document Item
    data : l_items_header type mepoitem,
           wa_a505        type a505,
           wa_konp        type konp,
           ev_maktx       type makt-maktx,
           ls_mepoitem    type mepoitem,
           ls_proposal    type zmm_pur_proposal,
           lt_proposal    type table of zmm_pur_proposal.
    data: gv_index type sy-tabix.
    data: zknumh       type a017-knumh, zzterm type konp-zterm,
          header_ztag1 type dztage,
          info_ztag1   type dztage,
          header_zterm type t052-zterm, info_zterm type t052-zterm.

    data : gv_flg_matnr,
           gv_flg_lifnr,
           gv_flg_menge,
           gv_flg_netpr,
           gv_flg_werks,
           gv_flg_mwskz,
           gv_flg_bukrs,
           gv_flg_ekgrp.

    data: gv_rel1_lvl,
          gv_rel2_lvl,
          gv_rel3_lvl,
          gv_rel4_lvl.
    data:
      lv_stcd3         type stcd3,
      lv_ktokk         type ktokk,
      lv_j_1ipanno     type ktokk,
      lv_ven_class     type j_1igtakld,
      lv_taxtariffcode type esll-taxtariffcode,
      lv_wemng         type eket-wemng,
      lv_procstat      type ekko-procstat,
      lv_ebeln_grr     type ebeln,
      gv_taxtariffcode type esll-taxtariffcode.

    data:
      lt_esll type mmsrv_esll,
      ls_esll like line of lt_esll,
* Handle for Item of a Purchasing Document
      lr_item type ref to cl_po_item_handle_mm.
    data: ls_ekko_ekpo     type wb2_v_ekko_ekpo2,
          lt_ekko_ekpo     type table of wb2_v_ekko_ekpo2,
          lt_ekko_ekpo_tmp type table of wb2_v_ekko_ekpo2.
    data: n type sy-dbcnt.

    types: begin of type_ekbe,
             ebeln type ebeln,
             ebelp type ebelp,
             gjahr type mjahr,
             belnr type mblnr,
             buzei type mblpo,
             bewtp type bewtp,
             budat type budat,
             menge type menge_d,
             dmbtr type dmbtr,
             matnr type matnr,
             werks type werks_d,
           end of type_ekbe.
    data: lt_ekbe type table of type_ekbe,
          ls_ekbe type type_ekbe.

    data: tmp_spart type mara-spart.
    include mm_messages_mac.
    data: zmtart type mara-mtart.
*  DATA: msg1(100).
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Wednesday, October 10, 2018 19:44:52
* Changed By - 6010859 - SaurabhK
* Purpose    - Simplification list - 2215424 - MATNR length change 18 -> 40
* Solution   - Change msg field length to be compatible with matnr field length change
* TR         - SBXK900028 - S4H: S_K: Simplification List: 03.10.2018
*--------------------------------------------------------------------*
    data: msg1 type bapi_msg.
    data: zj_1kftbus type lfa1-j_1kftbus.
    data: ztel_extens type t024-tel_extens, zekgrp type marc-ekgrp.

    data: lv_kunnr_sto    type kunnr,
          lt_knvi         type table of knvi,
          ls_knvi         type knvi,
          lv_msg_sto(132),
          lv_spart_sto    type knvv-spart.

    types : begin of ty_proposal,
              matnr type matnr,
            end of ty_proposal.
    data : gt_proposal type table of ty_proposal,
           gs_proposal type ty_proposal.
    data: lv_hsn_tmp type t604f-steuc.
    data: vv_msg       type string.

    data: begin of div_chk, " IRDK932925
            div type spart,
          end of div_chk,
          div_chk_tab like standard table of div_chk.


**** below code is for DMS valid only for QA and DEV on date 12.12.2017 because open PO validation moveing to PRD
*********************************************************************Parmanand dms validation
*****  IF sy-sysid = 'IRD' OR sy-sysid = 'IRQ' .
*****    IF sy-tcode = 'ME21N' OR  sy-tcode = 'ME22N'.
*****      IF sy-ucomm = 'MESAVE'.
*****        DATA: doknr TYPE doknr.
*****        DATA: doknr1 TYPE doknr.
*****
*****        GET PARAMETER ID 'CV1' FIELD doknr.
*****        IF doknr IS NOT INITIAL.
*****          doknr1 = doknr.
*****        ENDIF.
*****        IF doknr1 IS INITIAL.
*****          MESSAGE 'DMS Attachment Mandatory' TYPE 'E'.
*****        ENDIF.
******FREE MEMORY ID 'CV1'.
******SET PARAMETER ID 'CV1' FIELD space.
*****      ENDIF.
*****    ENDIF.
*****  ENDIF.
************************************************ended
    refresh gt_proposal.
    ls_header = im_header->get_data( ).

    call method im_header->get_items
      receiving
        re_items = l_items.

    if ls_header-bsart = 'ZSTO' or ls_header-bsart = 'YSTO' .
      data(zlifnr) =  |V{ ls_header-reswk }|."INTO .
      condense zlifnr no-gaps.
    else.
      zlifnr = ls_header-lifnr.
    endif.

    import gv_flg_ekgrp to gv_flg_ekgrp from memory id 'ME21N_EKGRP'.
    import gv_flg_lifnr to gv_flg_lifnr from memory id 'ME21N_LIFNR2'.
    import gv_flg_bukrs to gv_flg_bukrs from memory id 'ME21N_BUKRS'.
    import gv_flg_werks to gv_flg_werks from memory id 'ME21N_WERKS'.
    import gv_flg_matnr to gv_flg_matnr from memory id 'ME21N_MATNR'.
    import gv_flg_menge to gv_flg_menge from memory id 'ME21N_MENGE'.
    import gv_flg_netpr to gv_flg_netpr from memory id 'ME21N_NETPR'.
    import gv_flg_mwskz to gv_flg_mwskz from memory id 'ME21N_MWSKZ'.

    import gv_rel1_lvl to gv_rel1_lvl from memory id 'REL1_LVL'.
    import gv_rel2_lvl to gv_rel2_lvl from memory id 'REL2_LVL'.
    import gv_rel3_lvl to gv_rel3_lvl from memory id 'REL3_LVL'.
    import gv_rel4_lvl to gv_rel4_lvl from memory id 'REL4_LVL'.
** Vendor Acct grp is needed on   '4th - PO  validation if already 3 similar type PO's exists'
**  & HSN code validation
    select single ktokk stcd3 from lfa1 into (lv_ktokk, lv_stcd3)
        where lifnr = ls_header-lifnr.
*  SORT l_items.
    sort lt_proposal.

    " IRDK933113
    " MM: S_K: PO_BADI: PO Short-close: 16.8.18
    " This is done to skip header level checks in PO's whose all line items are short-closed deleted
    loop at l_items into l_single.
      clear l_items_header.
      call method l_single-item->get_data
        receiving
          re_data = l_items_header.

      if l_items_header-loekz eq '' and l_items_header-elikz eq ''.
        data(all_close) = abap_false.
        exit.
      else.
        all_close = abap_true.  " this item is short-closed/deleted
      endif.
      free l_single.
    endloop.

    refresh div_chk_tab.  " IRDK932925
    loop at l_items into l_single.

      call method l_single-item->get_conditions
        importing
          ex_conditions = l_item_cond.

      call method l_single-item->get_data
        receiving
          re_data = l_items_header.

      " Exclude deleted and delivery complete items from validation as per mail from Venu Sir: 09.08.2018: PO ERR
      " IRDK933092: MM: S_K: PO_BADI: PO Short-close: 14.8.18

      if l_items_header-loekz eq '' and l_items_header-elikz eq ''.

        " IHDK905362
        if ls_header-bsart = 'YSTO' or ls_header-bsart = 'ZSTO'.
          if l_items_header-matnr is not initial.
            select single ktgrm
              from mvke
              where matnr = @l_items_header-matnr
              and   vkorg = @ls_header-bukrs
              into @data(lv_acc_assign_grp).

            if lv_acc_assign_grp is initial.
              " IHDK905398
              message condense( |{ l_items_header-matnr alpha = out }: Account assignment group not maintained| ) type 'E'.
            endif.
          endif.
        endif.
        " End IHDK905362

        " IRDK932925
        clear div_chk.
        select single spart from mara into div_chk-div where matnr = l_items_header-matnr.
        if div_chk-div is not initial.
          append div_chk to div_chk_tab.
        endif.
        " End IRDK932925
*--------------------------------------------------------------------*
        " IHDK906033
*--------------------------------------------------------------------*
        " IDT requirement: In case of vendor document, if vendor GST and...
        " business place GST have the same state code, then IGST tax code...
        " should not be allowed.
        " Refer mail from Prajay Bhansali [pbhansali@indofil.com]
        " Subj: RE: Data Punching - GST Tax code validation
        " Dated: Fri 03/04/2020 19:40
*--------------------------------------------------------------------*
        if l_items_header-mwskz is not initial.
          select single gstin
            from j_1bbranch
            where bukrs  = @ls_header-bukrs
            and   branch = ( select j_1bbranch from t001w where werks = @l_items_header-werks )
            into @data(lv_bupla_gstin).

          if lv_bupla_gstin <> '24AABCI4568D2ZR' and lv_bupla_gstin <> '24AACCI6317D1Z0'.  " SEZ excluded; IHDK906187

            select single stcd3
              from lfa1
              where lifnr = @zlifnr
              and   ktokk not in ( '2000', '4000' ) " import and one time vendors excluded; IHDK906039
              into @data(lv_vendor_gstin).

            if lv_bupla_gstin is not initial and lv_vendor_gstin is not initial.
              if lv_bupla_gstin+0(2) = lv_vendor_gstin+0(2).  " business place and vendor belong to the same state
                " prevent selection of igst tax code
                select single @abap_true
                  from t007s
                  where spras = @sy-langu
                  and   kalsm = 'ZTXINN'    " GST tax procedure
                  and   mwskz = @l_items_header-mwskz
                  and   text1 like '%Input%IGST%'
                  into @data(lv_igst_tax_code).

                if lv_igst_tax_code = abap_true.
                  message e023(zfi01).
                endif.
              endif.
            endif.
          endif.
        endif.
*--------------------------------------------------------------------*
        " End IHDK906033
*--------------------------------------------------------------------*

**    added by NK on 25.07.2017 - For Validation of Division(SPART) '00'
        call method l_single-item->get_shipping_data
          receiving
            re_ekpv = ls_shipping_data.

********************************************************************** proposal logic added by NK start
**** This is the Proposal Logic & only execute when Proposal No. is present !!
        if zcl_helper=>is_development( ) or zcl_helper=>is_quality( ).
          if ls_header-prno is not initial.
            select * from zmm_pur_proposal into table lt_proposal where prno = ls_header-prno and app_vendor = 'X'.
            read table lt_proposal into ls_proposal with key pr_itemno = l_items_header-ebelp matnr = l_items_header-matnr.
            if sy-subrc eq 0.
              gv_index = sy-tabix.
****      Deletion Indicator - if in case any User deletes a line item !!
              if l_items_header-loekz <> 'L'.
                if sy-tcode eq 'ME22N' or sy-tcode eq 'ZMMQUERY1' or sy-tcode eq 'ME23N'.

                  if l_items_header-netpr eq ls_proposal-netpr.
                    clear gv_flg_netpr.
                  else.
                    gv_flg_netpr = 'X'.
                  endif.
                  if  ls_proposal-menge ge l_items_header-menge.
                    clear gv_flg_menge.
                  else.
                    gv_flg_menge = 'X'.
                  endif.

                  if gv_flg_ekgrp is not initial.
                    message e008(zprno) with 'Maintain same Purchasing Group as in Proposal' ls_proposal-prno.
                  endif.
                  if gv_flg_lifnr is not initial.
                    message e007(zprno) with 'Maintain same Vendor as in Proposal for Line Item' l_items_header-ebelp.
                  endif.
                  if ls_header-bukrs eq ls_proposal-bukrs.
                    clear gv_flg_bukrs.
                  else.
                    gv_flg_bukrs = 'X'.
                    if gv_flg_bukrs is not initial.
                      message e006(zprno) with 'Maintain same Company Code as in Proposal' ls_proposal-prno.
                    endif.
                  endif.
                  if l_items_header-werks eq ls_proposal-werks.
                    clear gv_flg_werks.
                  else.
***   This flag is used for Import/Export to other methods
                    gv_flg_werks = 'X'.
                    if gv_flg_werks is not initial.
                      message e005(zprno) with 'Maintain same Plant as in Proposal for Line Item' l_items_header-ebelp.
                    endif.
                  endif.
                  if gv_flg_menge is not initial.
                    message e002(zprno) with 'Maintain same Quantity as in Proposal for' l_items_header-ebelp.
                  endif.

                  if gv_flg_netpr is not initial.
                    message e000(zprno) with 'Maintain same Price as in Proposal for Line Item' l_items_header-ebelp.
                  endif.

                  if l_items_header-mwskz eq ls_proposal-mwskz.
                    clear gv_flg_mwskz.
                  else.
                    gv_flg_mwskz = 'X'.
                    if gv_flg_mwskz is not initial.
                      message e004(zprno) with 'Maintain same Tax Code as in Proposal for Line Item' l_items_header-ebelp.
                    endif.
                  endif.

                  if l_items_header-matnr eq ls_proposal-matnr.
                    clear gv_flg_matnr.
                  else.
                    if gv_flg_matnr is not initial.
                      message e009(zprno) with 'Maintain same Material as in Proposal for Line Item' l_items_header-ebelp.
                    endif.
                  endif.
                endif.
              endif.
            else.
              if l_items_header-loekz <> 'L'.
                if gv_rel4_lvl = 'X'.
                  read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr." releaser2_date = '00000000'.
                  if sy-subrc eq 0.
*****      Means the material exists - now check whether it is Released by USER or not !!
                    read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr releaser4_date = '00000000'.
                    if sy-subrc eq 0.
*** If Material no. is mis match or changed in order based on line item EBELP  then this error msg !!
                      gv_flg_matnr = 'X'.
                      if gv_flg_matnr is not initial.
                        message e009(zprno) with 'Maintain same Material as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                    else.
                      clear gv_flg_matnr.
                      if l_items_header-netpr eq ls_proposal-netpr.
                        clear gv_flg_netpr.
                      else.
                        gv_flg_netpr = 'X'.
                      endif.
                      if  ls_proposal-menge ge l_items_header-menge.
                        clear gv_flg_menge.
                      else.
                        gv_flg_menge = 'X'.
                      endif.
                      if  ls_proposal-werks eq l_items_header-werks.
                        clear gv_flg_werks.
                      else.
                        gv_flg_werks = 'X'.
                      endif.
                      if  ls_proposal-ekgrp eq ls_header-ekgrp.
                        clear gv_flg_ekgrp.
                      else.
                        gv_flg_ekgrp = 'X'.
                      endif.
                      if  ls_proposal-bukrs eq ls_header-bukrs.
                        clear gv_flg_bukrs.
                      else.
                        gv_flg_bukrs = 'X'.
                      endif.
                      if  ls_proposal-mwskz eq l_items_header-mwskz.
                        clear gv_flg_mwskz.
                      else.
                        gv_flg_mwskz = 'X'.
                      endif.
                      if gv_flg_werks is not initial.
                        message e005(zprno) with 'Maintain same Plant as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_menge is not initial.
                        message e002(zprno) with 'Maintain same Quantity as in Proposal for' l_items_header-ebelp.
                      endif.
                      if gv_flg_netpr is not initial.
                        message e000(zprno) with 'Maintain same Price as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_mwskz is not initial.
                        message e004(zprno) with 'Maintain same Tax Code as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_ekgrp is not initial.
                        message e008(zprno) with 'Maintain same Purchasing Group as in Proposal' ls_proposal-prno.
                      endif.
                      if gv_flg_bukrs is not initial.
                        message e006(zprno) with 'Maintain same Company Code as in Proposal' ls_proposal-prno.
                      endif.
                    endif.
                  else.

                  endif.


                elseif gv_rel3_lvl = 'X'.
                  read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr." releaser2_date = '00000000'.
                  if sy-subrc eq 0.
*****      Means the material exists - now check whether it is Released by USER or not !!
                    read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr releaser3_date = '00000000'.
                    if sy-subrc eq 0.
*** If Material no. is mis match or changed in order based on line item EBELP  then this error msg !!
                      gv_flg_matnr = 'X'.
                      if gv_flg_matnr is not initial.
                        message e009(zprno) with 'Maintain same Material as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                    else.
                      clear gv_flg_matnr.
                      if l_items_header-netpr eq ls_proposal-netpr.
                        clear gv_flg_netpr.
                      else.
                        gv_flg_netpr = 'X'.
                      endif.
                      if  ls_proposal-menge ge l_items_header-menge.
                        clear gv_flg_menge.
                      else.
                        gv_flg_menge = 'X'.
                      endif.
                      if  ls_proposal-werks eq l_items_header-werks.
                        clear gv_flg_werks.
                      else.
                        gv_flg_werks = 'X'.
                      endif.
                      if  ls_proposal-ekgrp eq ls_header-ekgrp.
                        clear gv_flg_ekgrp.
                      else.
                        gv_flg_ekgrp = 'X'.
                      endif.
                      if  ls_proposal-bukrs eq ls_header-bukrs.
                        clear gv_flg_bukrs.
                      else.
                        gv_flg_bukrs = 'X'.
                      endif.
                      if  ls_proposal-mwskz eq l_items_header-mwskz.
                        clear gv_flg_mwskz.
                      else.
                        gv_flg_mwskz = 'X'.
                      endif.
                      if gv_flg_werks is not initial.
                        message e005(zprno) with 'Maintain same Plant as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_menge is not initial.
                        message e002(zprno) with 'Maintain same Quantity as in Proposal for' l_items_header-ebelp.
                      endif.
                      if gv_flg_netpr is not initial.
                        message e000(zprno) with 'Maintain same Price as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_mwskz is not initial.
                        message e004(zprno) with 'Maintain same Tax Code as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_ekgrp is not initial.
                        message e008(zprno) with 'Maintain same Purchasing Group as in Proposal' ls_proposal-prno.
                      endif.
                      if gv_flg_bukrs is not initial.
                        message e006(zprno) with 'Maintain same Company Code as in Proposal' ls_proposal-prno.
                      endif.
                    endif.
                  else.

                  endif.

                elseif gv_rel2_lvl = 'X'.
                  read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr." releaser2_date = '00000000'.
                  if sy-subrc eq 0.
*****      Means the material exists - now check whether it is Released by USER or not !!
                    read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr releaser2_date = '00000000'.
                    if sy-subrc eq 0.
*** If Material no. is mis match or changed in order based on line item EBELP  then this error msg !!
                      gv_flg_matnr = 'X'.
                      if gv_flg_matnr is not initial.
                        message e009(zprno) with 'Maintain same Material as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                    else.
                      clear gv_flg_matnr.
                      if l_items_header-netpr eq ls_proposal-netpr.
                        clear gv_flg_netpr.
                      else.
                        gv_flg_netpr = 'X'.
                      endif.
                      if  ls_proposal-menge ge l_items_header-menge.
                        clear gv_flg_menge.
                      else.
                        gv_flg_menge = 'X'.
                      endif.
                      if  ls_proposal-werks eq l_items_header-werks.
                        clear gv_flg_werks.
                      else.
                        gv_flg_werks = 'X'.
                      endif.
                      if  ls_proposal-ekgrp eq ls_header-ekgrp.
                        clear gv_flg_ekgrp.
                      else.
                        gv_flg_ekgrp = 'X'.
                      endif.
                      if  ls_proposal-bukrs eq ls_header-bukrs.
                        clear gv_flg_bukrs.
                      else.
                        gv_flg_bukrs = 'X'.
                      endif.
                      if  ls_proposal-mwskz eq l_items_header-mwskz.
                        clear gv_flg_mwskz.
                      else.
                        gv_flg_mwskz = 'X'.
                      endif.
                      if gv_flg_werks is not initial.
                        message e005(zprno) with 'Maintain same Plant as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_menge is not initial.
                        message e002(zprno) with 'Maintain same Quantity as in Proposal for' l_items_header-ebelp.
                      endif.
                      if gv_flg_netpr is not initial.
                        message e000(zprno) with 'Maintain same Price as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_mwskz is not initial.
                        message e004(zprno) with 'Maintain same Tax Code as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_ekgrp is not initial.
                        message e008(zprno) with 'Maintain same Purchasing Group as in Proposal' ls_proposal-prno.
                      endif.
                      if gv_flg_bukrs is not initial.
                        message e006(zprno) with 'Maintain same Company Code as in Proposal' ls_proposal-prno.
                      endif.
                    endif.
                  else.

                  endif.

                elseif gv_rel1_lvl = 'X'.
                  read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr." releaser2_date = '00000000'.
                  if sy-subrc eq 0.
*****      Means the material exists - now check whether it is Released by USER or not !!
                    read table lt_proposal into ls_proposal with key matnr = l_items_header-matnr releaser1_date = '00000000'.
                    if sy-subrc eq 0.
*** If Material no. is mis match or changed in order based on line item EBELP  then this error msg !!
                      gv_flg_matnr = 'X'.
                      if gv_flg_matnr is not initial.
                        message e009(zprno) with 'Maintain same Material as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                    else.
                      clear gv_flg_matnr.
                      if l_items_header-netpr eq ls_proposal-netpr.
                        clear gv_flg_netpr.
                      else.
                        gv_flg_netpr = 'X'.
                      endif.
                      if  ls_proposal-menge ge l_items_header-menge.
                        clear gv_flg_menge.
                      else.
                        gv_flg_menge = 'X'.
                      endif.
                      if  ls_proposal-werks eq l_items_header-werks.
                        clear gv_flg_werks.
                      else.
                        gv_flg_werks = 'X'.
                      endif.
                      if  ls_proposal-ekgrp eq ls_header-ekgrp.
                        clear gv_flg_ekgrp.
                      else.
                        gv_flg_ekgrp = 'X'.
                      endif.
                      if  ls_proposal-bukrs eq ls_header-bukrs.
                        clear gv_flg_bukrs.
                      else.
                        gv_flg_bukrs = 'X'.
                      endif.
                      if  ls_proposal-mwskz eq l_items_header-mwskz.
                        clear gv_flg_mwskz.
                      else.
                        gv_flg_mwskz = 'X'.
                      endif.
                      if gv_flg_werks is not initial.
                        message e005(zprno) with 'Maintain same Plant as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_menge is not initial.
                        message e002(zprno) with 'Maintain same Quantity as in Proposal for' l_items_header-ebelp.
                      endif.
                      if gv_flg_netpr is not initial.
                        message e000(zprno) with 'Maintain same Price as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_mwskz is not initial.
                        message e004(zprno) with 'Maintain same Tax Code as in Proposal for Line Item' l_items_header-ebelp.
                      endif.
                      if gv_flg_ekgrp is not initial.
                        message e008(zprno) with 'Maintain same Purchasing Group as in Proposal' ls_proposal-prno.
                      endif.
                      if gv_flg_bukrs is not initial.
                        message e006(zprno) with 'Maintain same Company Code as in Proposal' ls_proposal-prno.
                      endif.
                    endif.
                  else.

                  endif.
                endif.

              endif.
            endif.
          endif.
        endif.
********************************************************************** proposal changes added by NK end
**********************************************************************  GST related validation by NK Start on 10.07.2017
** Validation for JIIG, JISG, JICG, JIMD during PO creation - Atleast 1 of these conditions have to be maintained
        data: ls_taxcom type taxcom,
              ls_komv   type komv,
              lt_komv   type table of komv.
*CONSTANTS
        data: bstyp_info  value 'I',
              bstyp_ordr  value 'W',
              bstyp_banf  value 'B',
              bstyp_best  value 'F',
              bstyp_anfr  value 'A',
              bstyp_kont  value 'K',
              bstyp_lfpl  value 'L',
              bstyp_lerf  value 'Q',
              lv_jiig_flg,
              lv_jisg_flg,
              lv_jicg_flg,
              lv_jimd_flg.

* For GSTN number in Vendor master & GST Indicator
**  Vendor grp = '1000' & '3000'
        if sy-tcode = 'ME21N' or sy-tcode = 'ME22N' or sy-tcode = 'ME23N' or sy-tcode = 'ME29N'.
*      DATA:
*        lv_stcd3     TYPE stcd3,
*        lv_ktokk     TYPE ktokk,
*        lv_j_1ipanno TYPE ktokk,
*        lv_ven_class TYPE j_1igtakld.
*      SELECT SINGLE ktokk stcd3 FROM lfa1 INTO (lv_ktokk, lv_stcd3)
*        WHERE lifnr = ls_header-lifnr.

**      SELECT SINGLE j_1ipanno ven_class FROM j_1imovend INTO (lv_j_1ipanno, lv_ven_class)
**        WHERE lifnr = ls_header-lifnr.
*---------------------------------------------------------------------*
*--------------------------- << S/4HANA >> ---------------------------*
*---------------------------------------------------------------------*
* Changed On - Wensday, November 14, 2018
* Changed By - 10106 - Bhushan Mehta
* Purpose    - Table Replacement
* Solution   - Replace Table KAN1 from J_1IPANNO
* TR         - SBXK900270 - BM:Replace Table Excise to General Master Data:14.11.2018
*--------------------------------------------------------------------*
          select single j_1ipanno
                        ven_class
                        from lfa1
                        into (lv_j_1ipanno, lv_ven_class)
                       where lifnr eq ls_header-lifnr.

          "-------------------------MOD1----------------------"FOR STO PLANT AS VENDOR
          if  ls_header-bsart = 'ZSTO'  or ls_header-bsart = 'YSTO'.
            data : lv_lifnr type lfa1-lifnr.

            clear : lv_lifnr.

            concatenate 'V' ls_header-reswk into lv_lifnr.

            select single j_1ipanno
                         ven_class
                         from lfa1
                         into (lv_j_1ipanno, lv_ven_class)
                        where lifnr eq lv_lifnr.

          endif.
          "--------------------------END-------------------------"

          if lv_ktokk = '1000' or lv_ktokk = '3000'.
            if lv_ven_class is initial. " registered vendor
              if lv_stcd3 is initial. " tax code 3 in vendor master is blank i.e GSTN number is blank in vendor master

                message 'Kindly maintain GSTN number in Vendor master' type 'E'.

              endif.
            else. " unre gistered vendor

              message 'Please NOTE This Vendor is Unregistered !!' type 'W'.

            endif.
          endif.
*      IF lv_ktokk = '1000' OR lv_ktokk = '3000'.
*        IF lv_stcd3 IS NOT INITIAL.
*          IF lv_ven_class IS INITIAL.  "Ven_class = ''  - Registered, '1' - Not registered
***       Do nothing
*          ELSE.
*            MESSAGE 'GST Vendor Classification Not Registered' TYPE 'E'.
*          ENDIF.
*        ELSE.
*          MESSAGE 'Kindly maintain GSTN number in Vendor master' TYPE 'E'.
*        ENDIF.
*      ELSEIF lv_ktokk IS INITIAL.
***  Then Supplying Plant is maintained -- LS_HEADER-RESWK
***    Do nothing for now
*      ENDIF.

          "To Exclude Employee Vendor for Pan No. Validation
          if lv_ktokk ne 'EMPL'.
            if ls_header-bsart ne 'ZIMP' and ls_header-bsart ne 'YIMP' and ls_header-bsart ne 'IBIO' and ls_header-bsart ne 'IBPI' .
              " skip below validation for import vendor as mail from rakesh pamula on 16.05.2019 PS
              if lv_j_1ipanno is initial.
                message 'Kindly maintained PAN number for Vendor' type 'E'.
              endif.
            endif.
          endif.
        endif.
*}   INSERT
*
*  CALL METHOD im_header->get_items
*    RECEIVING
*      re_items = l_items.
*
*  LOOP AT l_items INTO l_single.
*
*    CALL METHOD l_single-item->get_conditions
*      IMPORTING
*        ex_conditions = l_item_cond.
*
*    CALL METHOD l_single-item->get_data
*      RECEIVING
*        re_data = l_items_header.


***    For GST HSN code validation
        if sy-tcode = 'ME21N' or sy-tcode = 'ME22N' or sy-tcode = 'ME23N' or sy-tcode = 'ME29N'.
***     Exclude Export PO i.e., ZIMP & YIMP
          if ls_header-bsart ne 'ZIMP' and ls_header-bsart ne 'YIMP'.
            data:
              lv_steuc  type steuc, " HSN code
              lv_steuc1 type steuc. " HSN code
**  For Service PO - Material no. does not exist - So as of now no Validation !!
            if l_items_header-matnr is not initial.
              select single steuc from marc into lv_steuc
                where matnr = l_items_header-matnr
                and   werks = l_items_header-werks.

              if lv_steuc is not initial.
                if l_items_header-j_1bnbm <> lv_steuc.  " IRDK932412
                  message 'Control code(India tab) does not match material HSN code.' type 'E'.
                endif.
**       Do nothing
              else.
                message 'HSN code for Material not maintained' type 'E'.
              endif.
              if ls_header-reswk is not initial.
                select single steuc from marc into lv_steuc1
                where matnr = l_items_header-matnr
                and   werks = ls_header-reswk.
                if lv_steuc1 is not initial.
**       Do nothing
                else.
                  message 'HSN code for Supplying Plant not maintained' type 'E'.
                endif.
              endif.
            else.
**   Added by NK on 13.07.2017 (If Material is INITIAL)
**         HSN code for Non-coded Material
**              Refer table T163Y  - If l_items_header-PSTYP = '9' means 'D' -  Service, l_items_header-PSTYP = '0' means - Standard PO
              if l_items_header-loekz ne 'L'.   " added by NK on 11.09.2017
                if l_items_header-pstyp = '9'."  This is logic is for Standard PO ---> l_items_header-pstyp = '0'.
***        Commented by NK on 24.07.2017 - As discussed with Mamata's - Account assignment Category
                  if l_items_header-knttp = 'A' or l_items_header-knttp = 'K' or l_items_header-knttp = 'P' or l_items_header-knttp = 'Q'.

* Macro to get item
*                    mmpur_dynamic_cast lr_item l_single-item. " cl_po_item_handle_mm
                    try.
                        lr_item ?= l_single-item.
                      catch cx_sy_move_cast_error.
                    endtry.

* Use method of IF_SERVICES_MM interface to fetch Service data
                    call method lr_item->if_services_mm~get_srv_data
                      exporting
                        im_packno = l_items_header-packno
                      importing
                        ex_esll   = lt_esll.

                    delete lt_esll where menge = '0.000'.
                    read table lt_esll into ls_esll with key taxtariffcode = ' '.
                    if sy-subrc eq 0 and ls_esll eq ''.
                      " Exclude deleted and delivery complete items from validation as per mail from Venu Sir: 09.08.2018: PO ERR
                      " IRDK933092: MM: S_K: PO_BADI: PO Short-close: 14.8.18

                      if sy-tcode = 'ME29N' or sy-tcode = 'ME23N' or sy-tcode = 'ME22N'.
                        l_items_header-packno = l_items_header-packno + 1.
**  In case of multiple service line items in Service tab - need to look for ESLL-INTROW / EXTROW field !!
                        select single taxtariffcode from esll into gv_taxtariffcode where packno = l_items_header-packno.
                        if sy-subrc ne 0.
                          select single taxtariffcode from esll into gv_taxtariffcode where packno = ls_esll-packno.
                          if sy-subrc ne 0.
                            message 'Tax Tariff Code for Non-coded Material not maintained' type 'E'.
                          endif.
                        endif.
                      else.
                        select single taxtariffcode from esll into gv_taxtariffcode where packno = ls_esll-packno.
                        if sy-subrc ne 0.
                          message 'Tax Tariff Code for Non-coded Material not maintained' type 'E'.
                        endif.
                      endif.
                    endif.
                  endif.
                else.
                  if l_items_header-j_1bnbm is initial.
                    if l_items_header-knttp = 'F'.  " F = Order
**              Don't give error message for HSN for Non-coded material. As discusssed with Varmaji on 06.10.2017
                    else.
**             Not a service PO
                      message 'HSN code for Non-coded Material not maintained' type 'E'.
                    endif.
                  else.
                    select single steuc from t604f into lv_hsn_tmp where land1 = 'IN' and steuc = l_items_header-j_1bnbm.
                    if sy-subrc ne 0.
***        Commented by NK on 24.07.2017 - As discussed with Mamata's - Account assignment Category
                      if l_items_header-knttp = 'A' or l_items_header-knttp = 'K' or l_items_header-knttp = 'P' or l_items_header-knttp = 'Q'.
                        message 'HSN code is not maintained in master table' type 'E'.
                      endif.
                    endif.
                  endif.
                endif.
              endif.
            endif.
          endif.
        endif.

*** Validation for JIIG, JISG, JICG, JIMD during PO creation - Atleast 1 of these conditions have to be maintained
        if sy-tcode = 'ME21N' or sy-tcode = 'ME22N' or sy-tcode = 'ME23N' or sy-tcode = 'ME29N'.
*** Unless & until Tax code is not filled up - Don't calculate conditions values

          if lv_ven_class is initial. " registered vendor
            if lv_stcd3 is initial. " tax code 3 in vendor master is blank i.e GSTN number is blank in vendor master

              if l_items_header-mwskz is not initial.
                " * << S/4HANA / 6010859 / SBXK900270 / Tuesday, November 20, 2018 12:19:38

                " IHDK900056
                data: ls_ekko type ekko,
                      ls_ekpo type ekpo.

                clear: ls_ekpo, ls_ekko.

                move-corresponding: ls_header to ls_ekko,
                                    l_items_header to ls_ekpo.

                refresh lt_komv.
                call method zcl_helper=>calc_po_item_tax
                  exporting
                    is_ekko  = ls_ekko
                    is_ekpo  = ls_ekpo
                  importing
                    et_taxes = lt_komv.

                read table lt_komv into ls_komv with key kschl = 'JIIG'.
                if sy-subrc eq 0.
                  clear lv_jiig_flg.
                else.
                  read table lt_komv into ls_komv with key kschl = 'JICG'.
                  if sy-subrc eq 0.
                    clear lv_jicg_flg.
                  else.
                    read table lt_komv into ls_komv with key kschl = 'JISG'.
                    if sy-subrc eq 0.
                      clear lv_jisg_flg.
                    else.
                      read table lt_komv into ls_komv with key kschl = 'JIMD'.
                      if sy-subrc eq 0.
                        clear lv_jimd_flg.
                      else.
                        lv_jimd_flg = 'X'.
                      endif.
*   lv_jisg_flg = 'X'.
                    endif.
*   lv_jicg_flg = 'X'.
                  endif.
*   lv_jiig_flg = 'X'.
                endif.
              endif.


            endif.
          endif.
          if lv_jimd_flg = 'X'."lv_jiig_flg = ' ' or lv_jisg_flg = ' ' or lv_jicg_flg = ' ' or lv_jimd_flg = ' '.
            if sy-mandt ne '120'.
              message 'GST related Tax Code is not maintained. Kindly maintain a proper Tax Code' type 'E'.
            endif.
          else.
            clear: lv_jiig_flg ,lv_jisg_flg, lv_jicg_flg, lv_jimd_flg.
          endif.

** For Vendor Account Group 1000 only
          if lv_ktokk = '1000'.
            data: gs_gst_tax type zgst_taxes,
                  gt_gst_tax type table of zgst_taxes.

            select single tax_cd from zgst_taxes
              into gs_gst_tax
              where tax_cd = l_items_header-mwskz.
            if sy-subrc eq 0.
** Do nothing
            else.
              if sy-mandt ne '120'.
                message 'Kindly maintain GST related Tax Code as in ZGST_TAXES table' type 'E'.
              endif.
            endif.
          endif.
        endif.
**********************************************************************  GST related validation by NK End on 10.07.2017

********************************************************************** Division 00 validaton by NK on 25.07.2017
*    ls_shipping_data
        if sy-tcode = 'ME21N' or sy-tcode = 'ME22N' or sy-tcode = 'ME23N' or sy-tcode = 'ME29N'.
          if ls_header-bsart eq 'ZSTO' or ls_header-bsart eq 'YSTO'.
            if ls_header-aedat ge '20170701'.
              if ls_shipping_data-spart ne '00'.
                message 'Kindly Assign Shipping Point to Division 00' type 'E'.
              endif.
            endif.
          endif.
        endif.
**********************************************************************
        if ls_header-bsart eq 'ZSTO'.
          read table l_item_cond into ls_cond with key kschl = 'ZSTR'.
          if sy-subrc <> 0.
            clear: vv_msg.
            concatenate 'Please maintain ZSTR condition type for item:' l_items_header-ebelp
            into vv_msg.
            message vv_msg type 'E'.
          elseif sy-subrc = 0 and ls_cond-kbetr le 0. " IRDK932302: Wednesday, May 30, 2018 12:18:34
            clear vv_msg.
            concatenate 'ZSTR condition value should be non-zero for item:' l_items_header-ebelp
            into vv_msg.
            message vv_msg type 'E'.
          endif.

*    Validate Transportation cost FRC1 condition type mandatory for SPCD division only
          clear : tmp_spart.
          select single spart from mara into tmp_spart where matnr = l_items_header-matnr and mtart = 'ZFGM' and ( spart = '20' or spart = '28' ).
          if sy-subrc = 0 .
            if tmp_spart = '20' or tmp_spart = '28'.  " IHDK900966: IndoReagans(2800): MM: S_K: Incorporate 2800/28: 19.3.19

              read table l_item_cond into ls_cond with key kschl = 'FRC1'.
              if sy-subrc <> 0.
                message 'Please maintain FRC1 condition type' type 'E'.
              endif.
            endif.
          else.
            clear tmp_spart .
          endif.

        endif.
        clear: ls_cond.

        loop at l_item_cond into ls_cond where kschl = 'ZCH1' or kschl = 'ZCH2' or kschl = 'ZCH3'.
          if ls_cond-waers <> 'INR'.
            message e398(00) with 'For item : ' ls_cond-kposn 'Please enter in INR only for cond.type' ls_cond-kschl.
          endif.
        endloop.

        if ls_header-bsart ne 'ZSED' and ls_header-bsart ne 'ZSEI'
          and ls_header-bsart ne 'YSED' and ls_header-bsart ne 'YSEI'.
          select single * from a505 into wa_a505 where kschl = 'ZCH3'
                                                   and werks = l_items_header-werks
                                                   and lifnr = ls_header-lifnr
                                                   and matnr = l_items_header-matnr
                                                   and datab <= sy-datum
                                                   and datbi >= sy-datum.
          if sy-subrc = 0.
            select single * from konp into wa_konp where knumh = wa_a505-knumh.
            loop at l_item_cond into ls_cond where kschl = 'ZCH3'.
              if ls_cond-kbetr > wa_konp-kbetr.
                message e398(00) with 'ZCH3 Value is Greater than MEK1 - Material:' l_items_header-matnr.
              endif.
            endloop.
          else.
            read table l_item_cond into ls_cond with key kschl = 'ZCH3'.
            if sy-subrc = 0.
              message e398(00) with 'Maintain Condition Type ZCH3 in MEK1 - Material:' l_items_header-matnr.
            endif.
          endif.
        endif.

        if ls_header-bsart eq 'YIMP' and ls_header-bsart eq 'ZIMP'.
          read table l_item_cond into ls_cond with key kschl = 'ZCH3'.
          if sy-subrc <> 0.
            message e398(00) with 'Maintain Condition Type ZCH3 for material - ' l_items_header-matnr.
          endif.
        endif.
******************************************************************************************************

*validation for ZBPP condition for raw and pkg
        if ls_header-bsart eq 'YDOM' or
             ls_header-bsart eq 'YIMP' or
             ls_header-bsart eq 'YSED' or
             ls_header-bsart eq 'YSEI' or
             ls_header-bsart eq 'ZDOM' or
             ls_header-bsart eq 'ZIMP' or
             ls_header-bsart eq 'ZSED' or
             ls_header-bsart eq 'ZSEI' or
             ls_header-bsart eq 'ZDTP' or
             ls_header-bsart eq 'ZENG' or
             ls_header-bsart eq 'YENG' .

          clear: zmtart.
          select single mtart from mara into zmtart where matnr = l_items_header-matnr.

          if zmtart = 'ZPKG' or zmtart = 'ZRAW'.
            if ls_header-ekgrp <> '205'.  "Exclude ZBPP ZBPL ZBPC validation for 205 Purchasing group
              if ls_header-bedat > '20150211'.
                "IF KOMP-MTART = 'ZRAW' OR KOMP-MTART = 'ZPKG'.
                clear msg1.
                "READ TABLE xkomv INTO wa_komv WITH KEY kschl = 'ZBPP'.
                read table l_item_cond into ls_cond with key kschl = 'ZBPP'.
                if sy-subrc <> 0.
                  concatenate 'Maintain ZBPP condition for Item :'
                   l_items_header-ebelp
                   'Material:'
                   l_items_header-matnr
                   into msg1 separated by space.
                  message msg1 type 'E'.
                endif.
*   READ TABLE xkomv INTO wa_komv WITH KEY kschl = 'ZBPL'.
                read table l_item_cond into ls_cond with key kschl = 'ZBPL'.
                if sy-subrc <> 0.
                  concatenate 'Maintain ZBPL condition for Item :'
                   l_items_header-ebelp
                  'Material:'
                  l_items_header-matnr
                  into msg1 separated by space.
                  message msg1 type 'E'.
                endif.
              endif.
            endif.
          endif.

* below validation is for first release PO to check purchasing group
* developed on : 16.02.2015 developer: Punam S
          if zmtart = 'ZPKG' or zmtart = 'ZRAW' or zmtart = 'ZTRD' .
            if ls_header-bedat > '20150304'.
              clear: ztel_extens , zekgrp.
              select single tel_extens from t024 into ztel_extens
                where ekgrp = ls_header-ekgrp.
              if ztel_extens = 'XX'.

                select single ekgrp
                  from marc into zekgrp
                  where matnr = l_items_header-matnr
                  and werks = l_items_header-werks.
                if ls_header-ekgrp <> zekgrp.

                  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'  "#EC CI_FLDEXT_OK[2215424]
                    exporting
                      input  = l_items_header-matnr
                    importing
                      output = l_items_header-matnr.


                  concatenate 'Purch.Group mismatch with header for Item :'
                    l_items_header-ebelp
                   ',Material:'
                   l_items_header-matnr
                   into msg1 separated by space.
                  message msg1 type 'E'.

                  call function 'CONVERSION_EXIT_ALPHA_INPUT' "#EC CI_FLDEXT_OK[2215424]
                    exporting
                      input  = l_items_header-matnr
                    importing
                      output = l_items_header-matnr.


*    ENDIF.

                endif.
              else.
*  below code to validate if header purc group is not XX grp i.e. B01 b02... etc , then if
*  user enter item which belongs to XX purchasing grp ,throw error msg.
*  date : 23.02.2015 PS

                clear: ztel_extens.
                select single ekgrp
                    from marc into zekgrp
                    where matnr = l_items_header-matnr
                    and werks = l_items_header-werks.
                if zekgrp is not initial.
                  select single tel_extens from t024 into ztel_extens
                    where ekgrp = zekgrp.

                  if ztel_extens = 'XX'.

                    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'  "#EC CI_FLDEXT_OK[2215424]
                      exporting
                        input  = l_items_header-matnr
                      importing
                        output = l_items_header-matnr.


                    concatenate 'Purch.Group mismatch with header for Item :'
                      l_items_header-ebelp
                     ',Material:'
                     l_items_header-matnr
                     into msg1 separated by space.
                    message msg1 type 'E'.

                    call function 'CONVERSION_EXIT_ALPHA_INPUT' "#EC CI_FLDEXT_OK[2215424]
                      exporting
                        input  = l_items_header-matnr
                      importing
                        output = l_items_header-matnr.

                  endif.
                  clear: ztel_extens.
                endif.
              endif.
            endif.
          endif.

        endif.


        if ls_header-bsart = 'ZIMP'.
          if ls_header-ekgrp <> '205'.  "Exclude ZBPP ZBPL ZBPC validation for 205 Purchasing group
            if zmtart = 'ZRAW' or zmtart = 'ZPKG'.
              if ls_header-bedat > '20150211'.
*READ TABLE xkomv INTO wa_komv WITH KEY kschl = 'ZBPC'.
                read table l_item_cond into ls_cond with key kschl = 'ZBPC'.
                if sy-subrc <> 0.
                  concatenate 'Maintain ZBPC condition for Item :'
                  l_items_header-ebelp
                  'Material:'
                   l_items_header-matnr
                  into msg1 separated by space.
                  message msg1 type 'E'.
                endif.
              endif.
            endif.
          endif.
        endif.

********************************************************************* " added by NK on 27.07.2017
*        if zcl_helper=>is_development( ) or zcl_helper=>is_quality( ). " IHDK903412
        if sy-tcode = 'ME21N' or sy-tcode = 'ME22N' or sy-tcode = 'ME23N' or sy-tcode = 'ME29N'.
          if lv_ktokk = '1000'. " Vendor Acct. grp (only Domestic vendors)
            if ls_header-bsart ne 'ZSTO' or ls_header-bsart ne 'YSTO'.
              if l_items_header-loekz <> 'L' . " Deletion Indicator
                if l_items_header-mtart = 'ZRAW' or l_items_header-mtart = 'ZPKG' or l_items_header-mtart = 'YROH' or l_items_header-mtart = 'ZPKU'.
*    *      Now on the combination was Vendor & Material - Check whether any other open PO's are present
*    *      Checking for open PO's means - (1) either GRR not done
*                                          (2) whether it is not short closed
*                                              Delivery Indicator Completed (ELIKZ)
*                                          (3) if GRR done, then check it's Quantity (pending quantity is there)
*    ** Use DB view WB2_V_EKKO_EKPO2 to get all the PO's based on Vendor & Material
*    **    then    EKBE-BEWTP = 'E'  - for GR done or not
*    *      For closed PO's means -        (1) if GRR done, look for check/Tik mark is there !
                  if l_items_header-matnr is not initial.
                    select * from wb2_v_ekko_ekpo2 into table lt_ekko_ekpo where bukrs = ls_header-bukrs
                                                                      and  bsart ne 'ZSTO'
                                                                      and  bsart ne 'YSTO'"NOT IN ('ZSTO','YSTO')
                                                                      and  aedat ge '20170401'
                                                                      and  lifnr = ls_header-lifnr
                                                                      and  matnr_i = l_items_header-matnr
                                                                      and  werks_i = l_items_header-werks " IHDK903474
                                                                      and  loekz_i not in ( 'L', 'S' )
                                                                      and  elikz_i ne abap_true.
                  else.
                    select * from wb2_v_ekko_ekpo2 into table lt_ekko_ekpo where bukrs = ls_header-bukrs
                                                                      and  bsart ne 'ZSTO'
                                                                      and  bsart ne 'YSTO'"NOT IN ('ZSTO','YSTO')
                                                                      and  aedat ge '20170401'
                                                                      and  lifnr = ls_header-lifnr
                                                                      and  werks_i = l_items_header-werks " IHDK903474
                                                                      and  loekz_i not in ( 'L', 'S' )
                                                                      and  elikz_i ne abap_true.
*                                                                    AND  matnr_i = l_items_header-matnr.
                  endif.
                  if lt_ekko_ekpo is not initial.
                    select ebeln ebelp gjahr belnr buzei bewtp budat menge dmbtr matnr werks
                       from ekbe into table lt_ekbe for all entries in lt_ekko_ekpo
                      where ebeln = lt_ekko_ekpo-ebeln_i
                      and   ebelp = lt_ekko_ekpo-ebelp_i
                      and   bewtp = 'E' " for GR done but here Quantity is not verified
                      and   bwart = '101'. " GR receipt ( in some cases BWART = 106 )
*                    and   elikz = 'X'.

                    if lt_ekbe is not initial.
                      lt_ekko_ekpo_tmp[] = lt_ekko_ekpo[].
*    **        EKBE-ELIKZ = 'X' (Delivery completed)
                      loop at lt_ekbe into ls_ekbe.
                        read table lt_ekko_ekpo into ls_ekko_ekpo with key ebeln_i = ls_ekbe-ebeln ebelp_i = ls_ekbe-ebelp elikz_i = 'X'.
                        if sy-subrc eq 0.
*    *                    Deleting those whose GR completed & Delivery Completed indicator also set
                          delete lt_ekko_ekpo where ebeln_i = ls_ekbe-ebeln and ebelp_i = ls_ekbe-ebelp.
*    *                  PO Short closed or not

                        else.
*    *                  PO Short closed or not

                        endif.

                      endloop.

*    *           Checking for Quantity with PO - i.e., EKET-WEMNG(GR quantity)
                      read table lt_ekko_ekpo into ls_ekko_ekpo with key ebeln_i = l_items_header-ebeln ebelp_i = l_items_header-ebelp.
                      if sy-subrc eq 0.
                        select single ebeln from ekbe into lv_ebeln_grr where ebeln = ls_ekko_ekpo-ebeln_i
                                                                         and  bewtp = 'E'
                                                                         and  bwart = '101'.
                        if sy-subrc eq 0.
                          select single wemng from eket into lv_wemng where ebeln = ls_ekko_ekpo-ebeln_i and ebelp = ls_ekko_ekpo-ebelp_i.
                          if l_items_header-menge eq lv_wemng.
*    *                     Do nothing
                          else.
                            " message commented IHDK903417
*                              message 'GR not done. Kindly check the Quantity' type 'E'.
                          endif.
                        endif.
                      endif.
*    **       If more than 3 Open PO's then don't allow to make 4th PO !!
                      if sy-tcode = 'ME21N'.
                        describe table lt_ekko_ekpo lines n.
                        if n ge '3'.
                          message '3 open POs for same Material/Vendor/Plant exists. 4th PO not possible' type 'E'.
                        endif.
                      endif.
                      if sy-tcode = 'ME22N' or sy-tcode = 'ME23N' or sy-tcode = 'ME29N'.
                        describe table lt_ekko_ekpo lines n.
                        if n gt '3'.
                          message '3 open POs for same Material/Vendor/Plant exists. 4th PO not possible' type 'E'.
                        endif.
                      endif.
                    else.
                      describe table lt_ekko_ekpo lines n.
                      if n gt '3'.
                        message '3 open POs for same Material/Vendor/Plant exists. 4th PO not possible' type 'E'.
                      endif.
                    endif.
                  endif.
                endif.
              endif.
            endif.
          endif.
        endif.
*        endif.
*********************************************************************
** added by NK on 13.09.2017 - start
**  This is validation is for ZSTO/YSTO doc. type & whatever the Tax type may flow while creating the PO i.e., IGST, SGST or CGST but if in the
**  tax classification tab in (Customer->Sales area->Billing doc.) is not maintained zero '0' - then DON'T allow to create PO for all the available
**  tax categaries in that Customer.
**  Also note the Division is 00(for STO) in this case !!

        if sy-tcode = 'ME21N' or sy-tcode = 'ME22N' or sy-tcode = 'ME23N'." OR sy-tcode = 'ME29N'.
          if ls_header-bsart eq 'ZSTO' or ls_header-bsart eq 'YSTO'.
*      ....if KNVI-TAXKD NE '0' for any TATYP ---> then throw an error !!
            concatenate 'C' l_items_header-werks into lv_kunnr_sto.
            condense lv_kunnr_sto.
**        Only for Division 00
            select single spart from knvv into lv_spart_sto where kunnr = lv_kunnr_sto and spart = '00'.
            if sy-subrc eq 0.
              if lv_kunnr_sto ne 'C' or lv_kunnr_sto ne '0'.
                select * from knvi into table lt_knvi where kunnr = lv_kunnr_sto.
                if sy-subrc eq 0.
                  loop at lt_knvi into ls_knvi where taxkd is initial.
                    concatenate 'Please Update Customer' lv_kunnr_sto 'Tax classification' into lv_msg_sto separated by space.
                    message lv_msg_sto type 'E'.
                  endloop.
                endif.
              endif.
            endif.
          endif.
        endif.
** added by NK on 13.09.2017 - end

*    gs_proposal-matnr = l_items_header-matnr.  """""ravi on 20.12.2016
*    APPEND gs_proposal TO gt_proposal.
*    CLEAR gs_proposal.
        clear: zmtart, zekgrp.
        if ls_header-bsart eq 'ZENG' or ls_header-bsart eq 'YENG'.
          if l_items_header-matnr is not initial. " this validation is only applicable for coded po .
            select single mtart from mara into zmtart where matnr = l_items_header-matnr.
            if zmtart ne 'ZSPR' and zmtart ne 'ZEU2' and zmtart ne 'ZCON' and zmtart ne 'ZMCO'.

              message e398(00) with 'Incorrect Material for this PO type:' l_items_header-ebelp ',' l_items_header-matnr.

            endif.

            select single ekgrp
                       from marc into zekgrp
                       where matnr = l_items_header-matnr
                       and werks = l_items_header-werks.
            if ls_header-ekgrp <> zekgrp.

              call function 'CONVERSION_EXIT_ALPHA_OUTPUT'  "#EC CI_FLDEXT_OK[2215424]
                exporting
                  input  = l_items_header-matnr
                importing
                  output = l_items_header-matnr.


              concatenate 'Purch.Group mismatch with header for Item :'
                l_items_header-ebelp
               ',Material:'
               l_items_header-matnr
               into msg1 separated by space.
              message msg1 type 'E'.

              call function 'CONVERSION_EXIT_ALPHA_INPUT' "#EC CI_FLDEXT_OK[2215424]
                exporting
                  input  = l_items_header-matnr
                importing
                  output = l_items_header-matnr.


            endif.


          endif.
        endif.
      endif.  " if l_items_header-loekz eq '' and l_items_header-elikz eq ''.

      clear:
        lv_bupla_gstin,
        lv_vendor_gstin,
        lv_igst_tax_code.
    endloop.

    " IRDK932925
    if div_chk_tab is not initial and ls_header-bsart eq 'ZSTO'.
      sort div_chk_tab.
      delete adjacent duplicates from div_chk_tab comparing div.

      if lines( div_chk_tab ) gt 1.
        message e398(00) with 'All items should belong to the same division'.
      endif.
    endif.
    " End IRDK932925

************ Developed By Punam
* check each pay term match with header.
* credit limit validation check for user ID other that below ids .
*
*

    clear: zmtart.
    if ls_header-ekorg = '1000' or ls_header-ekorg = '2800'."Extended validations for Indoreagens
      if ls_header-bedat > '20150211'.
        if ls_header-ekgrp <> '205'.
          if sy-uname ne '1805' and sy-uname ne '10032' and sy-uname ne '2866' and sy-uname ne '1788'.
            if ls_header-bsart eq 'YDOM' or
             ls_header-bsart eq 'YIMP' or
             ls_header-bsart eq 'YSED' or
             ls_header-bsart eq 'YSEI' or
             ls_header-bsart eq 'ZDOM' or
             ls_header-bsart eq 'ZIMP' or
             ls_header-bsart eq 'ZSED' or
             ls_header-bsart eq 'ZSEI' or
             ls_header-bsart eq 'ZDTP' or
             ls_header-bsart eq 'ZENG' or
             ls_header-bsart eq 'YENG' .

              clear: l_single ,l_items_header .
              loop at l_items into l_single.



                call method l_single-item->get_data
                  receiving
                    re_data = l_items_header.

                " Exclude deleted and delivery complete items from validation as per mail from Venu Sir: 09.08.2018: PO ERR
                " IRDK933092: MM: S_K: PO_BADI: PO Short-close: 14.8.18

                if l_items_header-loekz eq '' and l_items_header-elikz eq ''.

                  clear: zmtart.
                  select single mtart from mara into zmtart where matnr = l_items_header-matnr.
                  if zmtart = 'ZPKG' or zmtart = 'ZRAW'.

                    clear: zknumh , zzterm.
                    select single knumh
                     from a017
                     into zknumh
                     where matnr = l_items_header-matnr
                     and lifnr = ls_header-lifnr
                     and ekorg = ls_header-ekorg
                     and werks = l_items_header-werks
                     and datab <= ls_header-bedat
                     and datbi >= ls_header-bedat." '99991231'.

                    if sy-subrc = 0.

                      select single zterm
                        from konp
                        into zzterm
                        where knumh = zknumh.

                      if sy-subrc = 0.
                        if zzterm <> ls_header-zterm.

*        MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-006 '' '' ''.

                          select single ztag1 from t052 into header_ztag1
                             where zterm = ls_header-zterm.
                          select single ztag1 from t052 into info_ztag1
                            where zterm = zzterm.
                          if header_ztag1 <> info_ztag1.
                            if l_items_header-loekz <> 'L'.
                              message e398(00) with 'Credit Days are mismatch with Info record for' l_items_header-matnr.
                            endif.
*          MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-007 '' '' ''.
                          endif.
*            MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-006 '' '' ''.
                        endif.
                      endif.

                    else.
*                  DATA: msg(50).
                      data: msg type t100-text.
                      concatenate l_items_header-matnr 'Vendor :' ls_header-lifnr 'Plant:' l_items_header-werks into msg.
                      message e398(00) with 'Please maintain Plant Spec.Inforecord for Material' l_items_header-matnr 'Vendor :' ls_header-lifnr.
                      " 'Plant:' L_ITEMS_HEADER-werks.

                    endif.
                  endif.

                endif.  " if l_items_header-loekz eq '' and l_items_header-elikz eq ''.
              endloop.
*    ENDIF.
            endif.
          endif.
        endif.
      endif.
** Vendor Tagging - Check if Vendor Plant has been maintained or not
      if ls_header-bsart eq 'ZDOM' or ls_header-bsart eq 'YDOM' or ls_header-bsart eq 'ZDTP' or ls_header-bsart eq 'ZENG' or ls_header-bsart eq 'YENG'.
        constants: mmmfd_zzvendorplant type mmpur_metafield value 90000000.
        if ls_header-lifnr is not initial.
          if ls_header-zzvendorplant = space.
*LFA1-J_1KFTBUS
            clear: zj_1kftbus.
            select single j_1kftbus
              from lfa1 into zj_1kftbus
              where lifnr = ls_header-lifnr.
            if zj_1kftbus = 'V2 - Multi Plant Manufacturer'.
              mmpur_metafield  mmmfd_zzvendorplant.
              mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Vendor Supplying Plant Location ' '' '' ''.
            endif.
          endif.
        endif.
      endif.
    endif.
*  clear: zmtart.
*  select single mtart from mara into zmtart where matnr = L_ITEMS_HEADER-MATNR.
*
*
*   if zmtart = 'ZPKG' or zmtart = 'ZRAW'.
*  "IF KOMP-MTART = 'ZRAW' OR KOMP-MTART = 'ZPKG'.
*   CLEAR msg.
*   "READ TABLE xkomv INTO wa_komv WITH KEY kschl = 'ZBPP'.
*   READ TABLE L_ITEM_COND INTO LS_COND WITH KEY KSCHL = 'ZBPP'.
*   if sy-subrc <> 0.
*       CONCATENATE 'Please maintain ZBPP condition for Item :'
*        "L_ITEM_COND-kposn
*        'Material:'
*        "komp-matnr
*        INTO msg SEPARATED BY space.
*       MESSAGE msg TYPE 'E'.
*   endif.
**   READ TABLE xkomv INTO wa_komv WITH KEY kschl = 'ZBPL'.
*   READ TABLE L_ITEM_COND INTO LS_COND WITH KEY KSCHL = 'ZBPL'.
*   if sy-subrc <> 0.
*       CONCATENATE 'Please maintain ZBPL condition for Item :'
**       komp-kposn
*       'Material:'
**       komp-matnr
*       INTO msg SEPARATED BY space.
*       MESSAGE msg TYPE 'E'.
*   endif.
*ENDIF.
*
*if LS_HEADER-BSART = 'ZIMP'.
*IF zmtart = 'ZRAW' OR zmtart = 'ZPKG'.
**READ TABLE xkomv INTO wa_komv WITH KEY kschl = 'ZBPC'.
* READ TABLE L_ITEM_COND INTO LS_COND WITH KEY KSCHL = 'ZBPC'.
*   if sy-subrc <> 0.
*       CONCATENATE 'Please maintain ZBPC condition for Item :'
**       komp-kposn
*       'Material:'
**       komp-matnr
*       INTO msg SEPARATED BY space.
*       MESSAGE msg TYPE 'E'.
*   endif.
*endif.
*ENDIF.
*


*********** End code by Punam

***** Added by Naren Karra - 30.08.2016 -Start
    " IRDK932854: Commented unnecessary code
*  l_items_header = l_single-item->get_data( ).
*
*  IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N'.
**  l_items_header = l_single-item->get_data( ).
*    IF l_items_header-matnr IS NOT INITIAL.
*      SELECT SINGLE maktx FROM makt INTO ev_maktx WHERE matnr EQ l_items_header-matnr AND spras EQ sy-langu.
*      IF sy-subrc EQ 0.
*        l_items_header-txz01 = ev_maktx.
*
*        CALL METHOD l_single-item->set_data
*          EXPORTING
*            im_data = l_items_header.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.
***** Added by Naren Karra - 30.08.2016 -End


    if ls_header-bsart = 'ZSTO' or ls_header-bsart = 'YSTO' .

      data: it_eina type table of eina,
            wa_eina type eina,

            it_eine type table of eine,
            wa_eine type eine,

            v_gstno type kna1-stcd3.

      data: lv_reswk    type eina-lifnr,
            v_msg       type string,
            lifnr       type bdcdata-fval,
            matnr       type bdcdata-fval,
            ekorg       type bdcdata-fval,
            werks       type bdcdata-fval,
            ekgrp       type bdcdata-fval,
            netpr       type bdcdata-fval,
            subrc       like sy-subrc,
            messages    type table of bdcmsgcoll,
            lw_messages type bdcmsgcoll,
            v_supplnt   type kna1-kunnr,
            v_recplnt   type kna1-kunnr.

*Saurabh
      loop at l_items into l_single.
        clear: l_items_header, lv_reswk, v_supplnt, v_recplnt.
        call method l_single-item->get_data
          receiving
            re_data = l_items_header.

        " Exclude deleted and delivery complete items from validation as per mail from Venu Sir: 09.08.2018: PO ERR
        " IRDK933092: MM: S_K: PO_BADI: PO Short-close: 14.8.18

        if l_items_header-loekz eq '' and l_items_header-elikz eq ''.
* Make GST No mandatory for supplying, recieving plant *
          concatenate 'V' ls_header-reswk into lv_reswk.
          concatenate 'V' ls_header-reswk into v_supplnt.     " IHDK900966
          concatenate 'C' l_items_header-werks into v_recplnt.

          if v_supplnt is not initial.
            clear v_gstno.
            select single stcd3
              from lfa1
              into v_gstno
              where lifnr = v_supplnt.  " supplying plant = vendor (VXXXX); " IHDK900966
            if v_gstno is initial.
              clear msg.
              concatenate 'GSTIN No. not maintained for plant' ls_header-reswk into msg separated by space.
*          MESSAGE msg TYPE 'E'.
              mmpur_message_forced 'E' 'ZMM01' '000' msg '' '' ''.
            endif.
          endif.

          if v_recplnt is not initial.
            clear v_gstno.
            select single stcd3
              from kna1
              into v_gstno
              where kunnr = v_recplnt.  " recving plant = customer (CXXXX); " IHDK900966
            if v_gstno is initial.
              clear msg.
              concatenate 'GSTIN No. not maintained for plant' l_items_header-werks into msg separated by space.
*          MESSAGE msg TYPE 'E'.
              mmpur_message_forced 'E' 'ZMM01' '000' msg '' '' ''.
            endif.
          endif.
* Maintain inforecord for material, purch org, plnt before saving if not maintained *
          select *
            from eina
            into table it_eina
            where matnr = l_items_header-matnr
            and   lifnr = lv_reswk
            and   loekz ne 'X'.

          if sy-subrc = 0 and it_eina is not initial.
            select *
              from eine
              into table it_eine
              for all entries in it_eina
              where infnr = it_eina-infnr
              and   ekorg = ls_header-ekorg
              and   werks = l_items_header-werks
              and   loekz ne 'X'.
          endif.

          if it_eine[] is initial.
            clear v_msg.
            shift l_items_header-ebelp left deleting leading '0'.
            concatenate 'Item' l_items_header-ebelp 'Maintaining Inforecord for'
            ls_header-reswk '/' l_items_header-werks 'in STO...'
            into v_msg separated by space.
*        MESSAGE  v_msg TYPE 'S'.
            mmpur_message_forced 'S' 'ZMM01' '000' v_msg '' '' ''.

            clear: lifnr, matnr, ekorg, werks, ekgrp, netpr.
            move: lv_reswk to lifnr,
                  l_items_header-matnr to matnr,
                  ls_header-ekorg to ekorg,
                  l_items_header-werks to werks,
                  ls_header-ekgrp to ekgrp,
                  l_items_header-netpr to netpr.

            shift netpr left deleting leading space.

            call function 'ZFM_INFORECORD_ME11_PO'
              exporting
*               mode      = 'N'
                lifnr_001 = lifnr  "'V1101'
                matnr_002 = matnr  "'20000096'
                ekorg_003 = ekorg  "'1000'
                werks_004 = werks  "'1201'
                ekgrp_011 = ekgrp  "'302'
*               mwskz_012 = 'G3'
                netpr_015 = netpr  "'             1'
              importing
                subrc     = subrc
              tables
                messtab   = messages.

            refresh: it_eina[], it_eine[].

            select *
            from eina
            into table it_eina
            where matnr = l_items_header-matnr
            and   lifnr = lv_reswk.

            if sy-subrc = 0 and it_eina[] is not initial.
              select *
                from eine
                into table it_eine
                for all entries in it_eina
                where infnr = it_eina-infnr
                and   ekorg = ls_header-ekorg
                and   werks = l_items_header-werks.
            endif.

            if it_eine[] is not initial.
              clear v_msg.
              shift l_items_header-ebelp left deleting leading '0'.
              concatenate 'Item' l_items_header-ebelp 'Inforecord maintained for'
              ls_header-reswk '/' l_items_header-werks 'in STO...'
              into v_msg separated by space.
*          MESSAGE  v_msg TYPE 'S'.
              mmpur_message_forced 'S' 'ZMM01' '000' v_msg '' '' ''.
            else.
              clear v_msg.
              shift l_items_header-ebelp left deleting leading '0'.
              concatenate 'Item' l_items_header-ebelp 'Inforecord not maintained for'
              ls_header-reswk '/' l_items_header-werks 'in STO...'
              into v_msg separated by space.
*          MESSAGE  v_msg TYPE 'E'.
              mmpur_message_forced 'E' 'ZMM01' '000' v_msg '' '' ''.

              data: lv_msgid type symsgid,
                    lv_msgty type symsgty,
                    lv_msgno type symsgno,
                    lv_msgv1 type symsgv,
                    lv_msgv2 type symsgv,
                    lv_msgv3 type symsgv,
                    lv_msgv4 type symsgv.

              loop at messages into data(ls_message) where ( msgtyp eq 'E' or msgtyp eq 'A' ).
                clear: lv_msgid,
                       lv_msgty,
                       lv_msgno,
                       lv_msgv1,
                       lv_msgv2,
                       lv_msgv3,
                       lv_msgv4.

                lv_msgid = conv #( ls_message-msgid ).
                lv_msgty = conv #( ls_message-msgtyp ).
                lv_msgno = conv #( ls_message-msgnr ).
                lv_msgv1 = conv #( ls_message-msgv1 ).
                lv_msgv2 = conv #( ls_message-msgv2 ).
                lv_msgv3 = conv #( ls_message-msgv3 ).
                lv_msgv4 = conv #( ls_message-msgv4 ).

                mmpur_message_forced lv_msgty lv_msgid lv_msgno
                                     lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4.
                clear ls_message.
              endloop.
            endif.

          endif.

        endif. " if l_items_header-loekz eq '' and l_items_header-elikz eq ''.
        refresh: it_eina[], it_eine[], messages[].
      endloop.
    endif.

    " IRDK930553: MM: S_K: ME22N: PO_BADI: Check PO Line items with Q Info rec
*  IF sy-sysid EQ 'IRD' OR sy-sysid EQ 'IRQ'.  " IRDK931151, IRDK931155; commented IRDK932948: MM: S_K: PO_BADI: Check: Move QIR validation to PRD: 25.7.18
    if me->gv_trtyp eq 'V' and ( ls_header-ekgrp not between '301' and '304' ). " IRDK932296: exclude purch grp 301 to 304 relevant to STO only
      " Edit Mode => 22N, 29N? others?
      data: rel_until type qinf-frei_dat. " Release until date in QI info record
      data: rel_date_out(10) type c.  " rel until date in output format
      data: item_delv_date(10) type c.  " delv date in output format
      data: qm_proc_active type mara-qmpur. " Check whether qm procuerment is active for that material

      free l_single.
      loop at l_items into l_single.
        clear: l_items_header.
        call method l_single-item->get_data
          receiving
            re_data = l_items_header.

        " Exclude deleted and delivery complete items from validation as per mail from Venu Sir: 09.08.2018: PO ERR
        " IRDK933092: MM: S_K: PO_BADI: PO Short-close: 14.8.18

        if l_items_header-loekz eq '' and l_items_header-elikz eq ''.

          if l_items_header is not initial.
            " IRDK930980: MM: S_K: PO BADI: QI Info validation: Exclude non QM items
            select single qmpur
              from mara
              into qm_proc_active
              where matnr = l_items_header-matnr.
            " This validation is applicable only for items for which qm procurment is active in master
            if qm_proc_active = abap_true.
              " This validation is applicable only for items that are not deleted AND not completely delivered(all validations should be, ideally)
              if l_items_header-loekz ne abap_true.   " Not deleted
                if l_items_header-elikz ne abap_true " Delivery not yet complete
                  and l_items_header-matnr is not initial.  " Coded PO line items only  " IRDK933018
                  clear: rel_until, rel_date_out.
                  " Get QI info record for material, vendor, plant combination => get its release until date
                  " Compare that date with the delivery date of the po item, rel_until date should be greater than or = po item delivery date
                  select single frei_dat from qinf into rel_until
                    where matnr = l_items_header-matnr  " material
                    and   lieferant = ls_header-lifnr   " vendor
                    and   werk = l_items_header-werks   " plant
                    and   loekz ne abap_true. " record not marked for deletion

                  if sy-subrc is not initial. " not maintained or marked for deletion
                    clear v_msg.
                    concatenate l_items_header-ebelp ':' l_items_header-matnr ls_header-lifnr l_items_header-werks ':'
                    'QI info rec. not maintained ot deleted' into v_msg separated by space.
                    message v_msg type 'E'.
                  else. " QI info record found, now compare the rel until date with po item delv date
                    if rel_until lt l_items_header-eindt. " Delivery date, IRDK930737
                      " PO delivery date exceeds release until date in QI info record

                      call function 'CONVERT_DATE_TO_EXTERNAL'
                        exporting
                          date_internal            = rel_until
                        importing
                          date_external            = rel_date_out
                        exceptions
                          date_internal_is_invalid = 1
                          others                   = 2.
                      if sy-subrc <> 0.
*        Implement suitable error handling here
                      endif.

                      call function 'CONVERT_DATE_TO_EXTERNAL'
                        exporting
                          date_internal            = l_items_header-eindt " IRDK930737
                        importing
                          date_external            = item_delv_date
                        exceptions
                          date_internal_is_invalid = 1
                          others                   = 2.
                      if sy-subrc <> 0.
*        Implement suitable error handling here
                      endif.

                      clear v_msg.
                      concatenate l_items_header-ebelp ':' 'Release until date in QI:' rel_date_out 'exceeds item deliv. date :'
                      item_delv_date into v_msg separated by space.
                      message v_msg type 'E'.
                    endif.
                  endif.
                endif.
              endif.
            endif.
          endif.

        endif.  " if l_items_header-loekz eq '' and l_items_header-elikz eq ''.
        free l_single.
      endloop.
    endif.
*  ENDIF.
    " End IRDK930553


***********we observer that below validations were written in header hence not working correctly , hence shifted here in check
************** but some Po created incorrectly hence Po which will get created after 01.04.2018 will check this validations
*********** no effects on PO created earlier
************ Developer: 10106

    if all_close = abap_false.  " IRDK933113; perform header level checks if atleast one po item is not closed or deleted
      if ls_header-aedat ge '20180401'.
        if ls_header-bsart eq 'ZDOM' and ls_header-bukrs eq '2000'.
          mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Y* ' '' '' ''.
        endif.

        if ls_header-bsart eq 'ZENG' and ls_header-bukrs eq '2000'.
          mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Y* ' '' '' ''.
        endif.

        if ls_header-bsart eq 'ZENG' and ls_header-bukrs eq '2050'.
          mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Y* ' '' '' ''.
        endif.

        if ls_header-bsart eq 'YENG' and ( ls_header-bukrs eq '1000' or ls_header-bukrs eq '2800' ).  " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
          mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Z* ' '' '' ''.
        endif.

        if ls_header-bsart eq 'YDOM' and ( ls_header-bukrs eq '1000' or ls_header-bukrs eq '2800' ).  " IHDK900987: IndoReagans(2800): SD: S_K: Incorporate 2800/28: 19.3.19
          mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should Start with Y* ' '' '' ''.
        endif.

        "Check on Credit Days
        if ls_header-bsart ne 'YPTR' and ls_header-bsart ne 'YSTO' and ls_header-bsart ne 'ZSTO'
          and ls_header-bsart ne 'ZPTR' and ls_header-bsart ne 'YSTI'  .
          if ls_header-zterm is initial.
            message 'Enter Payment Terms' type 'E'.
          endif.
        endif.

        data : wa_t052 type t052.

        select single * from t052 into wa_t052
        where zterm = ls_header-zterm.
        if sy-subrc = 0.
          if wa_t052-koart = 'D'. " changes on 07.01.2014 by 10106
            message 'Select Correct Payment Term of Vendor (Purchasing)' type 'E'.
          endif.
        endif.
      endif.

      " IHDK906115 - moved under all_close if
**********************************************************************    ,
***    below validation is to check whether vendor is extended to company code mentined in PO header . -- 20.11.2019 mail from rajesh kubchandani / venu

      if ls_header-bsart ne 'ZSTO'. "added by varun on 19.12.2019 as said by punam
        if ls_header-bsart ne 'YSTO'.
          select single bukrs
            from lfb1 into @data(zbukrs)
            where lifnr = @ls_header-lifnr
            and bukrs =  @ls_header-bukrs.
          if sy-subrc <> 0.
            message 'Please Extend Vendor to desired Company code.' type 'E'.
          endif.
        endif.
      endif.

**********************************************************************
**********************************************************************
      "GST Validation for vendor added by varun on 09.12.2019
      if sy-tcode = 'ME21N' or sy-tcode = 'ME22N' or sy-tcode = 'ME23N' or sy-tcode = 'ME29N'.
        try.
            zcl_bupa_utilities=>validate_gst_number(
              exporting
                iv_entity = conv char10( zlifnr ) "ls_header-lifnr
              receiving
                rv_valid      = data(lv_gst_valid)
            ).

          catch zcx_generic into data(lox). " Generic Exception Class
            message lox->get_text( ) type 'E'.
        endtry.
        try .
            zcl_bupa_utilities=>validate_postal_code(
              exporting
                iv_entity      = conv char10( zlifnr ) "ls_header-lifnr
              receiving
                rv_valid       = data(lv_post_code_valid)
            ).
          catch zcx_generic into data(lox1). " Generic Exception Class
            message lox1->get_text( ) type 'E'.
        endtry.
      endif.
**********************************************************************
      " End IHDK906115
    endif.  " if all_close = abap_false.

*** below validation is to check if ZARP is maintained for receiveing plants region and resp. material
***  done on 29.05.2018 PS
    data: it_a605 type table of a605 .
    data: zspart type mara-spart, zmvgr5 type mvke-mvgr5, zvkorg type mvke-vkorg, zvtweg type mvke-vtweg,zregio type t001w-regio.
    break 10106.
    if ls_header-bsart eq 'ZSTO' .
      loop at l_items into l_single.
        clear: l_items_header.

        call method l_single-item->get_data
          receiving
            re_data = l_items_header.

        " Exclude deleted and delivery complete items from validation as per mail from Venu Sir: 09.08.2018: PO ERR
        " IRDK933092: MM: S_K: PO_BADI: PO Short-close: 14.8.18

        if l_items_header-loekz eq '' and l_items_header-elikz eq ''.

          clear: zmtart , zspart , zmvgr5 , zvkorg , zvtweg.
          select single mtart spart from mara into (zmtart , zspart ) where matnr = l_items_header-matnr.

          select single vkorg vtweg mvgr5
          from mvke
          into (zvkorg , zvtweg  ,zmvgr5 )
          where matnr = l_items_header-matnr.


          if ( zmtart = 'ZFGM' or zmtart = 'ZTRD' or zmtart = 'ZLFG' or zmtart = 'ZLTD' )
            and ( zspart = '10' or zspart = '15')
            and zvkorg = '1000'  and zvtweg = '10' and zmvgr5 = '002'.
            if ls_header-bedat >= '20180529'.

              clear: it_a605 , zregio.

              select single regio from t001w into zregio where werks = l_items_header-werks.


              select * from a605 into table it_a605
              where kschl = 'ZARP'
              and vkorg = zvkorg
              and vtweg = zvtweg
              and spart = zspart
              and regio = zregio "receiving plant region
              and ( kdgrp = '02' or kdgrp = '01' ) "komk-kdgrp
              and matnr = l_items_header-matnr
              and datab <= sy-datum
              and datbi >= sy-datum.
              if sy-subrc <> 0.
                clear: v_msg.
                concatenate 'Ask SD key user to Maintain ZARP(Region Apprvd price) fo line item:' l_items_header-ebelp into v_msg.
                message v_msg type 'E'.

              endif.

            endif.
          endif.

        endif.  " if l_items_header-loekz eq '' and l_items_header-elikz eq ''.
      endloop.
    endif.

**** error - dont allow to save ZSTO if purchasing group is not between 301 to 304

    if all_close = abap_false.  " IRDK933113; perform header level checks if atleast one po item is not closed or deleted
      if ls_header-bsart eq 'ZSTO' .
        if ls_header-ekgrp not between '301' and  '304'.
          message 'Please provide correct Purchasing Group for ZSTO.' type 'E'.
        endif.
      endif.
    endif.  " if all_close = abap_false.

    " Code commented: IRDK931062
*  " IRDK931002
*  DATA: lt_esuh TYPE mmsrv_esuh,
*        ls_esuh LIKE LINE OF lt_esuh.
*
*  FREE l_single.
*  LOOP AT l_items INTO l_single.
*    CLEAR: l_items_header.
*    CALL METHOD l_single-item->get_data
*      RECEIVING
*        re_data = l_items_header.
*
*    CHECK l_items_header-pstyp EQ '9'.  " Service line only
*    FREE lr_item.
*    mmpur_dynamic_cast lr_item l_single-item. " cl_po_item_handle_mm
*
** Use method of IF_SERVICES_MM interface to fetch Service data
*    REFRESH lt_esuh.
*    CALL METHOD lr_item->if_services_mm~get_srv_data  " CALL FUNCTION 'MS_READ_LIMITS' can also be used
*      EXPORTING
*        im_packno = l_items_header-packno
*      IMPORTING
*        ex_esuh   = lt_esuh.
*
*    IF lt_esuh IS NOT INITIAL.
*      READ TABLE lt_esuh INTO ls_esuh WITH KEY packno = l_items_header-packno.
*      IF sy-subrc = 0.
*        IF ls_esuh-sumlimit NE l_items_header-netwr.
*          " Message : limit should be equal to item netwr for service
*        ENDIF.
*      ELSE.
*        " Message : Limit not maintained
*      ENDIF.
*    ELSE.
*      " Message : Limit not maintained
*    ENDIF.
*
*    CHECK l_items_header IS NOT INITIAL.
*    FREE l_single.
*  ENDLOOP.
*  " End IRDK931002



*METHOD if_ex_me_process_po_cust~check.
*
*  DATA : l_items TYPE purchase_order_items.
*  DATA : l_single TYPE purchase_order_item.
*  DATA : l_item_cond TYPE mmpur_tkomv.
*  DATA : ls_cond TYPE komv.
*  DATA : ls_header TYPE mepoheader.
*  DATA : l_items_header TYPE mepoitem,
*         wa_a505 TYPE a505,
*         wa_konp TYPE konp.
*
*  DATA: tmp_spart TYPE mara-spart.
*
*  DATA: zknumh TYPE a017-knumh , zzterm TYPE konp-zterm ,
*          header_ztag1 TYPE dztage,
*          info_ztag1 TYPE dztage,
*          header_zterm TYPE t052-zterm , info_zterm TYPE t052-zterm.
*
*  INCLUDE mm_messages_mac. "useful macros for message handling
*
*  ls_header = im_header->get_data( ).
*
*  CALL METHOD im_header->get_items
*    RECEIVING
*      re_items = l_items.
*
*  LOOP AT l_items INTO l_single.
*
*    CALL METHOD l_single-item->get_conditions
*      IMPORTING
*        ex_conditions = l_item_cond.
*
*    CALL METHOD l_single-item->get_data
*      RECEIVING
*        re_data = l_items_header.
*
*    IF ls_header-bsart EQ 'ZSTO'.
*      READ TABLE l_item_cond INTO ls_cond WITH KEY kschl = 'ZSTR'.
*      IF sy-subrc <> 0.
*        MESSAGE 'Please maintain ZSTR condition type' TYPE 'E'.
*      ENDIF.
*
**    Validate Transportation cost FRC1 condition type mandatory for SPCD division only
*      CLEAR : tmp_spart.
*      SELECT SINGLE spart FROM mara INTO tmp_spart WHERE matnr = l_items_header-matnr AND mtart = 'ZFGM' AND spart = 20.
*      IF sy-subrc = 0 .
*        IF tmp_spart = '20' .
*
*          READ TABLE l_item_cond INTO ls_cond WITH KEY kschl = 'FRC1'.
*          IF sy-subrc <> 0.
*            MESSAGE 'Please maintain FRC1 condition type' TYPE 'E'.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        CLEAR tmp_spart .
*      ENDIF.
*
*    ENDIF.
*    CLEAR: ls_cond.
*
*    LOOP AT l_item_cond INTO ls_cond WHERE kschl = 'ZCH1' OR kschl = 'ZCH2' OR kschl = 'ZCH3'.
*      IF ls_cond-waers <> 'INR'.
*        MESSAGE e398(00) WITH 'For item : ' ls_cond-kposn 'Please enter in INR only for cond.type' ls_cond-kschl.
*      ENDIF.
*    ENDLOOP.
*
*    IF ls_header-bsart NE 'ZSED' "AND ls_header-bsart NE 'ZSEI'
*      AND ls_header-bsart NE 'YSED' ."AND ls_header-bsart NE 'YSEI'.
*      SELECT SINGLE * FROM a505 INTO wa_a505 WHERE kschl = 'ZCH3'
*                                               AND werks = l_items_header-werks
*                                               AND lifnr = ls_header-lifnr
*                                               AND matnr = l_items_header-matnr
*                                               AND datab <= sy-datum
*                                               AND datbi >= sy-datum.
*      IF sy-subrc = 0.
*        SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a505-knumh.
*        LOOP AT l_item_cond INTO ls_cond WHERE kschl = 'ZCH3'.
*          IF ls_cond-kbetr > wa_konp-kbetr.
*            MESSAGE e398(00) WITH 'ZCH3 Value is Greater than MEK1 - Material:' l_items_header-matnr.
*          ENDIF.
*        ENDLOOP.
*      ELSE.
*        READ TABLE l_item_cond INTO ls_cond WITH KEY kschl = 'ZCH3'.
*        IF sy-subrc = 0.
*          MESSAGE e398(00) WITH 'Maintain Condition Type ZCH3 in MEK1 - Material:' l_items_header-matnr.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
**   L_ITEMS_HEADER-matnr-zterm
**LS_HEADER-ZTERM
**
** check each pay term match with header.
** credit limit validation check for user ID other that below ids .
** BREAK-POINT.
* IF sy-uname NE '1805' AND sy-uname NE '10032' AND sy-uname NE '2866' AND sy-uname NE '1788'.
*  IF ls_header-bsart EQ 'YDOM' OR
*   ls_header-bsart EQ 'YIMP' OR
*   ls_header-bsart EQ 'YSED' OR
*   ls_header-bsart EQ 'YSEI' OR
*   ls_header-bsart EQ 'ZDOM' OR
*   ls_header-bsart EQ 'ZIMP' OR
*   ls_header-bsart EQ 'ZSED' OR
*   ls_header-bsart EQ 'ZSEI' .
*
*
*      CLEAR: zknumh , zzterm.
*      SELECT SINGLE knumh
*       FROM a017
*       INTO zknumh
*       WHERE matnr = l_items_header-matnr
*       AND lifnr = ls_header-lifnr
*       AND ekorg = ls_header-ekorg
*       AND werks = l_items_header-werks
*       AND datbi = '99991231'.
*
*      IF sy-subrc = 0.
*
*        SELECT SINGLE zterm
*          FROM konp
*          INTO zzterm
*          WHERE knumh = zknumh.
*
*        IF sy-subrc = 0.
*        IF zzterm <> ls_header-zterm.
*
**        MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-006 '' '' ''.
*
*        SELECT SINGLE ztag1 FROM t052 INTO header_ztag1
*           WHERE zterm = ls_header-zterm.
*        SELECT SINGLE ztag1 FROM t052 INTO info_ztag1
*          WHERE zterm = zzterm.
*        IF header_ztag1 < info_ztag1.
*          IF l_items_header-loekz <> 'L'.
*          MESSAGE e398(00) WITH 'Credit Days are Lesser than Info record for' l_items_header-matnr.
*          ENDIF.
**          MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-007 '' '' ''.
*        ENDIF.
**            MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-006 '' '' ''.
*        ENDIF.
*        ENDIF.
*
*      ELSE.
**        DATA: msg(50).
**        CONCATENATE L_ITEMS_HEADER-MATNR 'Vendor :' LS_HEADER-lifnr 'Plant:' L_ITEMS_HEADER-werks INTO msg.
*        MESSAGE e398(00) WITH 'Please maintain Plant Spec.Inforecord for Material' l_items_header-matnr 'Vendor :' ls_header-lifnr.
*        " 'Plant:' L_ITEMS_HEADER-werks.
*
*      ENDIF.
*
*
*    ENDIF.
*
* ENDIF.
*  ENDLOOP.
*
**  BREAK 10106." check.
*
** Vendor Tagging - Check if Vendor Plant has been maintained or not
* IF ls_header-bsart EQ 'ZDOM'.
*    CONSTANTS: mmmfd_zzvendorplant TYPE mmpur_metafield VALUE 90000000.
*   IF ls_header-lifnr IS NOT INITIAL.
*   IF ls_header-zzvendorplant = space.
**     mmpur_business_obj_id 90000000.
*    mmpur_metafield  mmmfd_zzvendorplant.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Vendor Supplying Plant Location ' '' '' ''.
*    ENDIF.
*    ENDIF.
*  ENDIF.
*ENDMETHOD.
  endmethod.


  method if_ex_me_process_po_cust~close.
  endmethod.


  method if_ex_me_process_po_cust~fieldselection_header.


    constants:  mmmfd_cust_01 type mmpur_metafield value 90000000.

    data : ls_header type mepoheader.
    data: l_persistent type mmpur_bool.
    field-symbols: <fs> like line of ch_fieldselection.

    ls_header = im_header->get_data( ).

    if me->gv_trtyp  =  'H' or   " CREATE OR CHANGE
       me->gv_trtyp =  'V' and sy-ucomm ne 'MESAVE'.

      read table ch_fieldselection assigning <fs> with table key metafield = mmmfd_cust_01.

      if sy-subrc is initial.
        <fs>-fieldstatus = '+'. " Input
      endif.

    elseif me->gv_trtyp  =  'H' or   " CREATE OR CHANGE
          me->gv_trtyp =  'V' and sy-ucomm eq 'MESAVE'.

      read table ch_fieldselection assigning <fs> with table key metafield = mmmfd_cust_01.

      if sy-subrc is initial.
        <fs>-fieldstatus = '*'. " Input
      endif.
    elseif   me->gv_trtyp = 'A'.

      read table ch_fieldselection assigning <fs> with table key metafield = mmmfd_cust_01.

      if sy-subrc is initial.
        <fs>-fieldstatus = '*'. " Display
      endif.

    endif.

* Flag = 'X' only if post method is trigerred and this method will be trigerred after post method
    if flagt = 'X'.
      read table ch_fieldselection assigning <fs> with table key metafield = mmmfd_cust_01.
      if sy-subrc is initial.
        <fs>-fieldstatus = '*'. " Display
      endif.
    endif.

  endmethod.


  method if_ex_me_process_po_cust~fieldselection_header_refkeys.
  endmethod.


  method if_ex_me_process_po_cust~fieldselection_item.
  endmethod.


  method if_ex_me_process_po_cust~fieldselection_item_refkeys.
  endmethod.


  method if_ex_me_process_po_cust~initialize.
  endmethod.


  method if_ex_me_process_po_cust~open.
    data : ls_header type mepoheader.
    include mm_messages_mac. "useful macros for message handling
    ls_header = im_header->get_data( ).

    me->gv_trtyp = im_trtyp.

    flagt = space.
  endmethod.


  METHOD if_ex_me_process_po_cust~post.

    DATA: ls_mepoitem   TYPE mepoitem,
          ls_mepoheader TYPE REF TO if_purchase_order_mm,
          ls_header     TYPE mepoheader,
          ls_customer   TYPE mepo_badi_exampl,
          ls_mmparams   TYPE z6mma_params,
          ls_tbsg       TYPE tbsg,
          g_objcont_wa  TYPE soli.

    DATA : g_objcont LIKE STANDARD TABLE OF g_objcont_wa.

    DATA: wa_packing_list TYPE sopcklsti1,
          wa_contents_txt TYPE solisti1,
          wa_reclist      TYPE somlreci1.
*-- VARIABLES USED FOR SENDING MAIL
    DATA: gv_smtp_addr  TYPE adr6-smtp_addr,
          sender        TYPE soextreci1-receiver,
          send_adr_type TYPE so_adr_typ.

    DATA: gv_count   TYPE sy-index,
          gv_line_no TYPE sy-index.

*-- MAIL RELATED INTERNAL TABLES
    DATA: wa_doc_chng     TYPE sodocchgi1,                   " DOCUMENT ATTRIBUTES
          it_packing_list TYPE STANDARD TABLE OF sopcklsti1, " ATTACHMENT TABLE
          it_contents_txt TYPE STANDARD TABLE OF solisti1,   " OBJECT TEXT
          it_reclist      TYPE STANDARD TABLE OF somlreci1.  " MAIL RECIPIENTS

    DATA : addr_usr     TYPE v_addr_usr,
           wa_smtp_addr TYPE adr6-smtp_addr.

    DATA : it_cdpos TYPE cdpos_tab.
    DATA  : wa_cdpos TYPE cdpos.
    DATA  : wa_cdhdr TYPE cdhdr.
    DATA : lv_bname TYPE usr03-bname.
    DATA : lv_tdname TYPE thead-tdname,
           it_lines  TYPE tlinetab,
           wa_lines  TYPE tline.
    DATA: it_email TYPE TABLE OF zfi044_emailid,
          wa_email LIKE LINE OF it_email,
          it_user  TYPE TABLE OF zusergroup,
          wa_user  LIKE LINE OF it_user.
    DATA: flag.

*    TYPES : BEGIN OF ty_werks ,
*          lifnr TYPE zmm_vendor_tag-lifnr,
*          werks TYPE  zmm_vendor_tag-werks,
*          city1 TYPE zmm_vendor_tag-city1,
*          country TYPE zmm_vendor_tag-country,
*          region TYPE zmm_vendor_tag-region,
*          bezei TYPE t005u-bezei,
*        END OF ty_werks.
* DATA : it_werks TYPE TABLE OF ty_werks.
*  DATA : wa_werks LIKE LINE OF it_werks.

    INCLUDE mm_messages_mac. "useful macros for message handling

*---------------------------------------------------------------------*
* here we retrieve the Item Data
*---------------------------------------------------------------------*
* The following code is written to default Valuation Type (BWTAR) field against given Order Type (BSART) and Material Type (MTART).
* The Default valuation Type against the Order Type and Material Type is being maintained in Z6MMA_PARAMS table by using TCode ZMMPARAM.
* Please maintain the data in the above table as given below.
* Z6MMA_PARAMS-PROGNAME = 'POVALIDATIONS'
* Z6MMA_PARAMS-PARAM1  = 'ZDOM' - PO Order Type
* Z6MMA_PARAMS-PARAM2 = 'ZPKG' - Material Type
* Z6MMA_PARAMS-SERNO = 'Serial No'. - 001
* Z6MMA_PARAMS-PARAMVAL = Default Valuation type - to be displayed on the screen.



    ls_header = im_header->get_data( ).
    IF ls_header-procstat EQ '08' AND ls_header-frgzu+0(1) EQ 'X'.

      SELECT * FROM  cdpos INTO CORRESPONDING FIELDS OF TABLE it_cdpos
      WHERE objectclas EQ 'EINKBELEG'
                                             AND objectid  EQ im_ebeln
                                             AND tabname EQ 'EKKO'
                                             AND fname EQ 'FRGZU'
                                             AND value_new EQ 'X'.
      SORT it_cdpos BY changenr DESCENDING.

      READ TABLE it_cdpos INTO wa_cdpos INDEX 1.
      IF sy-subrc EQ 0.
        SELECT SINGLE username FROM cdhdr INTO lv_bname WHERE objectclas EQ 'EINKBELEG'
                                               AND objectid  EQ im_ebeln
                                               AND changenr EQ wa_cdpos-changenr.
      ENDIF.
      CLEAR addr_usr.

      CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
        EXPORTING
          bname      = sy-uname
          mandt      = sy-mandt
          no_display = 'X'
        IMPORTING
*         OKCODE     =
          addr_usr   = addr_usr
*   CHANGING
*         USER_USR03 =
        .
      SELECT SINGLE smtp_addr INTO wa_smtp_addr FROM adr6
                                   WHERE addrnumber = addr_usr-addrnumber
                                   AND persnumber   = addr_usr-persnumber.
      sender  = wa_smtp_addr.

      send_adr_type = 'SMTP'.


      CLEAR addr_usr.

      CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
        EXPORTING
          bname      = lv_bname
          mandt      = sy-mandt
          no_display = 'X'
        IMPORTING
*         OKCODE     =
          addr_usr   = addr_usr
*   CHANGING
*         USER_USR03 =
        .

      CLEAR: wa_reclist, it_reclist.

*-- POPULATE MAIL ID'S

      SELECT SINGLE smtp_addr INTO gv_smtp_addr FROM adr6
                                   WHERE addrnumber = addr_usr-addrnumber
                                   AND persnumber   = addr_usr-persnumber.


      wa_reclist-rec_type = 'U' ."INTERNET ADDRESS
      wa_reclist-express  = 'X' ."VALUE FOR ACTIVATED
      wa_reclist-receiver = gv_smtp_addr.
*    IF IZ6MMA_PARAMS_WA-PARAM1 = 'CC'.
*      WA_RECLIST-COPY = 'X'.
*    else.
*      WA_RECLIST-COPY = ''.
*    ENDIF.
      APPEND wa_reclist TO it_reclist.

      CLEAR: gv_smtp_addr, wa_reclist,g_objcont.
*---*-- MAIL SUBJECT LINE
      CLEAR: wa_doc_chng.



      wa_doc_chng-obj_name = 'Purchase Order'.
*  WA_DOC_CHNG-OBJ_DESCR = L_OBJECT_HD_CHANGE-OBJDES.
      CONCATENATE 'Purchase Order Number ' im_ebeln 'Release Rejected' INTO wa_doc_chng-obj_descr
      SEPARATED BY space.
      CLEAR: gv_line_no, gv_count.

*  ---mail body

      lv_tdname = im_ebeln.
      CLEAR : it_lines.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'F03'
          language                = sy-langu
          name                    = lv_tdname
          object                  = 'EKKO'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
*  IMPORTING
*         HEADER                  =
        TABLES
          lines                   = it_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ELSE.
        LOOP AT it_lines INTO wa_lines.
          g_objcont_wa = wa_lines-tdline.
          APPEND g_objcont_wa TO g_objcont .
          CLEAR g_objcont_wa.
        ENDLOOP.
      ENDIF.
*----
      DESCRIBE TABLE g_objcont LINES gv_count.
      gv_line_no = gv_count.
      READ TABLE g_objcont INTO g_objcont_wa INDEX gv_count.
      wa_doc_chng-doc_size = ( gv_count - 1 ) * 255 + strlen( g_objcont_wa ).

*-- POPULATE PACKING LIST FOR BODY TEXT
      wa_packing_list-head_start  = 1.
      wa_packing_list-head_num    = 0.
      wa_packing_list-body_start  = 1.
      wa_packing_list-body_num    = gv_count."V_TABLE_LINES.
      wa_packing_list-doc_type    = 'RAW'.
      APPEND wa_packing_list TO it_packing_list.
      CLEAR wa_packing_list.

      DELETE it_reclist WHERE receiver  = ''.

      IF NOT it_reclist IS INITIAL.
        CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
          EXPORTING
            document_data              = wa_doc_chng
            put_in_outbox              = 'X'
            sender_address             = sender
            sender_address_type        = send_adr_type
            commit_work                = 'X'
          TABLES
            packing_list               = it_packing_list
            contents_txt               = g_objcont
            receivers                  = it_reclist
          EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.


* below code is to send mail to creator of PO on last release is complete
* developement date: 31.07.2015 developer: Punam S

    " IRDK932122: Code commented, since mails on release are now being sent through BADI ZME28_RELEASE_MAIL which works for ME29N as well
*if sy-tcode = 'ME29N'.
*IF ls_header-procstat EQ '05'." AND ls_header-frgzu+0(1) EQ 'X'.
*
**    SELECT * FROM  cdpos INTO CORRESPONDING FIELDS OF TABLE it_cdpos
**                                         WHERE objectclas EQ 'EINKBELEG'
**                                           AND objectid  EQ im_ebeln
**                                           AND tabname EQ 'EKKO'
**                                           AND fname EQ 'FRGZU'
**                                           AND value_new EQ 'X'.
**    SORT it_cdpos BY changenr DESCENDING.
*
**    READ TABLE it_cdpos INTO wa_cdpos INDEX 1.
**    IF sy-subrc EQ 0.
**      SELECT SINGLE username FROM cdhdr INTO lv_bname WHERE objectclas EQ 'EINKBELEG'
**                                             AND objectid  EQ im_ebeln
**                                             AND changenr EQ wa_cdpos-changenr.
**    ENDIF.
*    CLEAR addr_usr.
*
*    CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
*      EXPORTING
*        bname            = sy-uname
*       mandt            = sy-mandt
*       no_display       = 'X'
*     IMPORTING
**     OKCODE           =
*       addr_usr         = addr_usr
**   CHANGING
**     USER_USR03       =
*              .
*    SELECT SINGLE smtp_addr INTO wa_smtp_addr FROM adr6
*                                 WHERE addrnumber = addr_usr-addrnumber
*                                 AND persnumber   = addr_usr-persnumber.
*
*    TRANSLATE WA_SMTP_ADDR TO LOWER CASE.
*    sender  = wa_smtp_addr.
*    send_adr_type = 'SMTP'.
*
*    CLEAR addr_usr.
*
*    lv_bname = ls_header-ERNAM.
*
*    CALL FUNCTION 'SUSR_SHOW_USER_DETAILS'
*      EXPORTING
*        bname            = lv_bname
*       mandt            = sy-mandt
*       no_display       = 'X'
*     IMPORTING
**     OKCODE           =
*       addr_usr         = addr_usr
**   CHANGING
**     USER_USR03       =
*              .
*
*    CLEAR: wa_reclist, it_reclist.
*
**-- POPULATE MAIL ID'S
*
**    SELECT SINGLE smtp_addr INTO gv_smtp_addr FROM adr6
**                                 WHERE addrnumber = addr_usr-addrnumber
**                                 AND persnumber   = addr_usr-persnumber.
*DATA: userid_long TYPE pa0105-usrid_long.
*SELECT SINGLE usrid_long FROM pa0105 INTO userid_long WHERE pernr = ls_header-ernam AND subty = '0010' AND endda >= sy-datum ." '99991231'.
*
*    wa_reclist-rec_type = 'U' ."INTERNET ADDRESS
*    wa_reclist-express  = 'X' ."VALUE FOR ACTIVATED
*    TRANSLATE userid_long TO LOWER CASE.
*    wa_reclist-receiver = userid_long."gv_smtp_addr.
**    IF IZ6MMA_PARAMS_WA-PARAM1 = 'CC'.
**      WA_RECLIST-COPY = 'X'.
**    else.
**      WA_RECLIST-COPY = ''.
**    ENDIF.
*    APPEND wa_reclist TO it_reclist.
*
*    CLEAR: gv_smtp_addr, wa_reclist,g_objcont.
******* send mail to users group if purch. group is 101  *********************
**if ls_header-EKGRP = '101'.
*DATA: zgrp(11).
*
*  CONCATENATE 'PO_' ls_header-ERNAM INTO zgrp. " create groups as per req of PO creator prefix with 'PO_'.
*  CLEAR: FLAG ,IT_USER , WA_USER , it_email , WA_email .
**  SELECT * FROM ZUSERGROUP INTO TABLE IT_USER WHERE zegroup = '101_USER'.
**
**  LOOP AT IT_USER INTO WA_USER.
**    IF ls_header-ERNAM = WA_USER-USERID.
**      FLAG = 'X'.
**    ENDIF.
**  ENDLOOP.
**  IF  FLAG = 'X'.
*
*    SELECT * FROM ZFI044_EMAILID INTO TABLE it_email WHERE EGROUP = zgrp.
*
*      LOOP AT IT_EMAIL INTO WA_EMAIL.
*      CLEAR: wa_reclist.
*      wa_reclist-rec_type = 'U' ."INTERNET ADDRESS
*      wa_reclist-express  = 'X' ."VALUE FOR ACTIVATED
*      TRANSLATE WA_EMAIL-EMAIL TO LOWER CASE.
*      wa_reclist-receiver = WA_EMAIL-EMAIL .
*      APPEND wa_reclist TO it_reclist.
*      ENDLOOP.
*
*    CLEAR: FLAG.
**  ENDIF.
**endif.
*****************************************************************************
**---*-- MAIL SUBJECT LINE
*    CLEAR: wa_doc_chng.
*
*    wa_doc_chng-obj_name = 'Purchase Order'.
**  WA_DOC_CHNG-OBJ_DESCR = L_OBJECT_HD_CHANGE-OBJDES.
*    CONCATENATE 'PO Number ' im_ebeln 'Completely Released.' INTO wa_doc_chng-obj_descr
*    SEPARATED BY space.
*    CLEAR: gv_line_no, gv_count.
*
**  ---mail body
*
* lv_tdname = im_ebeln.
** CLEAR : it_lines.
** CALL FUNCTION 'READ_TEXT'
**   EXPORTING
***    CLIENT                        = SY-MANDT
**     id                            = 'F03'
**     language                      = sy-langu
**     name                          = lv_tdname
**     object                        = 'EKKO'
***    ARCHIVE_HANDLE                = 0
***    LOCAL_CAT                     = ' '
***  IMPORTING
***    HEADER                        =
**   TABLES
**     lines                         = it_lines
**  EXCEPTIONS
**    id                            = 1
**    language                      = 2
**    name                          = 3
**    not_found                     = 4
**    object                        = 5
**    reference_check               = 6
**    wrong_access_to_archive       = 7
**    OTHERS                        = 8
**           .
** IF sy-subrc <> 0.
*** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**
** ELSE.
**   LOOP AT it_lines INTO wa_lines.
*    DATA:vend_name TYPE lfa1-name1, po_date(10) .
*
*    CONCATENATE ls_header-BEDAT+06(02) '.' ls_header-BEDAT+04(02) '.' ls_header-BEDAT(04) INTO po_date.
*    SELECT SINGLE name1 FROM lfa1 INTO vend_name WHERE lifnr = ls_header-lifnr.
*
*    CONDENSE : vend_name.
*    CONCATENATE 'PO Number ' im_ebeln ', dated: ' po_date  ', on Vendor: ' vend_name '(' ls_header-lifnr ') - has been Released.' INTO g_objcont_wa
*    SEPARATED BY space.
*    APPEND g_objcont_wa TO g_objcont .
*    CLEAR g_objcont_wa.
**   ENDLOOP.
** ENDIF.
*
**----
*    DESCRIBE TABLE g_objcont LINES gv_count.
*    gv_line_no = gv_count.
*    READ TABLE g_objcont INTO g_objcont_wa INDEX gv_count.
*    wa_doc_chng-doc_size = ( gv_count - 1 ) * 255 + STRLEN( g_objcont_wa ).
*
**-- POPULATE PACKING LIST FOR BODY TEXT
*    wa_packing_list-head_start  = 1.
*    wa_packing_list-head_num    = 0.
*    wa_packing_list-body_start  = 1.
*    wa_packing_list-body_num    = gv_count."V_TABLE_LINES.
*    wa_packing_list-doc_type    = 'RAW'.
*    APPEND wa_packing_list TO it_packing_list.
*    CLEAR wa_packing_list.
*
*    SORT IT_RECLIST BY receiver.
*    DELETE ADJACENT DUPLICATES FROM IT_RECLIST COMPARING RECEIVER.
*    DELETE it_reclist WHERE receiver  = ''.
*
*    IF NOT it_reclist IS INITIAL.
*
*
*      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
*        EXPORTING
*          document_data              = wa_doc_chng
*          put_in_outbox              = 'X'
*          sender_address             = sender
*          sender_address_type        = send_adr_type
*          commit_work                = 'X'
*        TABLES
*          packing_list               = it_packing_list
*          contents_txt               = g_objcont
*          receivers                  = it_reclist
*        EXCEPTIONS
*          too_many_receivers         = 1
*          document_not_sent          = 2
*          document_type_not_exist    = 3
*          operation_no_authorization = 4
*          parameter_error            = 5
*          x_error                    = 6
*          enqueue_error              = 7
*          OTHERS                     = 8.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDIF.

**********************************************************************
    "GST Validation for vendor added by varun on 09.12.2019
    IF sy-tcode = 'ME21N' OR sy-tcode = 'ME22N' OR sy-tcode = 'ME23N' OR sy-tcode = 'ME29N'.
      TRY.
          zcl_bupa_utilities=>validate_gst_number(
            EXPORTING
              iv_entity = ls_header-lifnr
            RECEIVING
              rv_valid      = DATA(lv_gst_valid)
          ).
        CATCH zcx_generic INTO DATA(lox). " Generic Exception Class
          MESSAGE lox->get_text( ) TYPE 'E'.
      ENDTRY.
      TRY .
          zcl_bupa_utilities=>validate_postal_code(
            EXPORTING
              iv_entity      = ls_header-lifnr
            RECEIVING
              rv_valid       = DATA(lv_post_code_valid)
          ).
        CATCH zcx_generic INTO DATA(lox1). " Generic Exception Class
          MESSAGE lox1->get_text( ) TYPE 'E'.
      ENDTRY.
    ENDIF.
**********************************************************************

* Vendor Tagging - Check if Vendor Plant has been maintained or not
    IF ls_header-bsart EQ 'ZDOM' OR ls_header-bsart EQ 'YDOM' OR ls_header-bsart EQ 'ZENG' OR ls_header-bsart EQ 'YENG'.
      flagt = 'X'. " Make flag X this flag is checked in field selection method to make custom field readonly
      IF ls_header-lifnr IS NOT INITIAL.
        IF ls_header-zzvendorplant = space.
**   mmpur_business_obj_id mmmfd_zzvendorplant.
*    CONSTANTS: mmmfd_cust_01 TYPE mmpur_metafield VALUE 90000000.
*    mmpur_metafield mmmfd_cust_01.

          CONSTANTS: mmmfd_zzvendorplant TYPE mmpur_metafield VALUE 90000000.
          mmpur_metafield mmmfd_zzvendorplant.
          mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Vendor Supplying Plant Location ' '' '' ''.
        ELSE.


***Check if valid vendor supplying plant location has been maintained from user
*      SELECT lifnr
*           werks
*           city1
*           country
*      region
*      FROM zmm_vendor_tag
*      INTO TABLE it_werks WHERE lifnr = ls_header-lifnr.
*
*    READ TABLE it_werks INTO wa_werks WITH KEY werks = ls_header-zzvendorplant.
*         IF sy-subrc <> 0.
*            mmpur_metafield   mmmfd_cust_01.
*            mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Valid Vendor Supplying Plant Location ' '' '' ''.
*         ENDIF.
*
          im_header->set_data( ls_header ).
        ENDIF.
      ENDIF.


    ENDIF.
  ENDMETHOD.


method if_ex_me_process_po_cust~process_account.
endmethod.


  method if_ex_me_process_po_cust~process_header.
    data : ls_header type mepoheader.
    data : ls_mepoitem type ref to if_purchase_order_item_mm.  " added by nk on 04.05.2017
*  DATA : wa_t052 TYPE t052.
    data: zj_1kftbus type lfa1-j_1kftbus.
    types : begin of ty_werks ,
              lifnr   type zmm_vendor_tag-lifnr,
              werks   type  zmm_vendor_tag-werks,
              city1   type zmm_vendor_tag-city1,
              country type zmm_vendor_tag-country,
              region  type zmm_vendor_tag-region,
              bezei   type t005u-bezei,
            end of ty_werks.
    data : it_werks type table of ty_werks.
    data : wa_werks like line of it_werks.
    data : ls_proposal type zmm_pur_proposal,
           lt_proposal type table of zmm_pur_proposal.
*DATA: zknumh TYPE a018-knumh.
    data: ls_bdcdata type bdcdata,
          lt_bdcdata type table of bdcdata.
    data : gv_flg_ekgrp,
           gv_flg_lifnr,
           gp_prno  type zmm_pur_proposal-prno.
*  DATA: pay_term TYPE konp-zterm, wa_a018 type a018.
    include mm_messages_mac. "useful macros for message handling
*  BREAK 10106.

    ls_header = im_header->get_data( ).

**Validation For Company Code 1000 - Document Type should Start With Z*
**Validation For Company Code 2000 - Document Type should Start With Y*
*{   REPLACE        SBXK900102                                        1
*\ IF sy-sysid = 'IRD' or sy-sysid = 'IRQ' .
    if zcl_helper=>is_development( ) or zcl_helper=>is_quality( ).
*}   REPLACE
      if gp_prno is not initial.
        import gp_prno to gp_prno from memory id 'ME21N_ADD_TAB_PRNO'.
        ls_header-prno = gp_prno.

        data: lo_hd type ref to if_purchase_order_mm.
        call method lo_hd->set_data
          exporting
            im_data = ls_header.
      endif.
    endif.

***************** shifting below code in CHECK ************************************
*  IF ls_header-bsart EQ 'ZDOM' AND ls_header-bukrs EQ '2000'.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Y* ' '' '' ''.
*  ENDIF.
*
*  IF ls_header-bsart EQ 'ZENG' AND ls_header-bukrs EQ '2000'.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Y* ' '' '' ''.
*  ENDIF.
*
*  IF ls_header-bsart EQ 'ZENG' AND ls_header-bukrs EQ '2050'.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Y* ' '' '' ''.
*  ENDIF.
*
*  IF ls_header-bsart EQ 'YENG' AND ls_header-bukrs EQ '1000'.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Z* ' '' '' ''.
*  ENDIF.
*
*  IF ls_header-bsart EQ 'YDOM' AND ls_header-bukrs EQ '1000'.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should Start with Y* ' '' '' ''.
*  ENDIF.
*
*  "Check on Credit Days
*  IF ls_header-bsart NE 'YPTR' AND ls_header-bsart NE 'YSTO' AND ls_header-bsart NE 'ZSTO'
*    AND ls_header-bsart NE 'ZPTR' AND ls_header-bsart NE 'YSTI'  .
*    IF ls_header-zterm IS INITIAL.
*      MESSAGE 'Enter Payment Terms' TYPE 'E'.
*    ENDIF.
*  ENDIF.
*
*
*    SELECT SINGLE * FROM t052 INTO wa_t052
*    WHERE zterm = ls_header-zterm.
*  IF sy-subrc = 0.
*    IF wa_t052-koart = 'D'. " changes on 07.01.2014 by 10106
*      MESSAGE 'Select Correct Payment Term of Vendor (Purchasing)' TYPE 'E'.
*    ENDIF.
*  ENDIF.
****************************************************************************************



*  DATA: ZMATNR TYPE EKPO-MATNR.
* BREAK 10106.
*  IF LS_HEADER-ZTERM IS NOT INITIAL.
*
*    SELECT MATNR FROM EKPO UP TO 1 ROWS
*      INTO ZMATNR
*      WHERE EBELN = LS_HEADER-EBELN.
*    ENDSELECT.
*    if sy-subrc = 0 .
*
*    endif.
*
*  ENDIF.



    "Credit Days can be higher than the master but shall not be allowed to reduce it.
*  SELECT SINGLE * FROM lfm1 INTO wa_lfm1
*    WHERE lifnr = ls_header-lifnr
*    AND ekorg = ls_header-ekorg.


*  IF sy-subrc = 0.
*    IF wa_lfm1-zterm <> ls_header-zterm.
*      SELECT SINGLE ztag1 FROM t052 INTO prev_ztag1
*        WHERE zterm = wa_lfm1-zterm.
*      SELECT SINGLE ztag1 FROM t052 INTO new_ztag1
*        WHERE zterm = ls_header-zterm.
*      IF new_ztag1 < prev_ztag1.
*        MESSAGE 'Credit Days are Lesser than Vendor Master' TYPE 'E'.
*      ENDIF.
*    ENDIF.
*  ENDIF.

* Vendor Tagging - Check if Vendor Plant has been maintained or no
    if ls_header-bsart eq 'ZDOM' or ls_header-bsart eq 'YDOM' or ls_header-bsart eq 'ZENG' or ls_header-bsart eq 'YENG'.
      set cursor field 'EKKO_CI-ZZVENDORPLANT'.
***   BREAK ibmabap01.
      if ls_header-lifnr is not initial.
        if ls_header-zzvendorplant = space.

          clear: zj_1kftbus.
          select single j_1kftbus
            from lfa1 into zj_1kftbus
            where lifnr = ls_header-lifnr.
          if zj_1kftbus = 'V2 - Multi Plant Manufacturer'.

            gv_lifnr = ls_header-lifnr.
            constants:  mmmfd_cust_01 type mmpur_metafield value 90000000.
            mmpur_metafield  mmmfd_cust_01.
            mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Vendor Supplying Plant Location ' '' '' ''.
          endif.
        else.

**Check if valid vendor supplying plant location has been maintained from user
          select lifnr
              werks
              city1
              country
         region
         from zmm_vendor_tag
         into table it_werks where lifnr = ls_header-lifnr.

          clear: zj_1kftbus.
          select single j_1kftbus
            from lfa1 into zj_1kftbus
            where lifnr = ls_header-lifnr.
          if zj_1kftbus = 'V2 - Multi Plant Manufacturer'.

            read table it_werks into wa_werks with key werks = ls_header-zzvendorplant.
            if sy-subrc <> 0.
              mmpur_metafield  mmmfd_cust_01.
              mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Valid Vendor Supplying Plant Location ' '' '' ''.
            endif.
          endif.
        endif.
      endif.
    endif.

*{   REPLACE        SBXK900102                                        2
*\ IF sy-sysid = 'IRD' or sy-sysid = 'IRQ'.
    if zcl_helper=>is_development( ) or zcl_helper=>is_quality( ).
*}   REPLACE
      import ls_header-prno to ls_header-prno from memory id 'ME21N_PRNO'.
      import ls_header-lifnr to ls_header-lifnr from memory id 'ME21N_LIFNR'.
      if sy-tcode = 'ZMMQUERY1'.
        export ls_header-prno from ls_header-prno to memory id 'ME21N_PRNO3'.
        export ls_header-lifnr from ls_header-lifnr to memory id 'ME21N_LIFNR3'.
      endif.
      ls_proposal-prno  = ls_header-prno.
      ls_proposal-lifnr = ls_header-lifnr.
      if sy-tcode = 'ME22N' and ls_proposal-prno is not initial.
        import ls_header-prno to ls_header-prno from memory id 'ME21N_PRNO3'.
        import ls_header-lifnr to ls_header-lifnr from memory id 'ME21N_LIFNR3'.
        ls_proposal-lifnr = ls_header-lifnr.
        select single * from zmm_pur_proposal into ls_proposal where prno = ls_proposal-prno and lifnr = ls_proposal-lifnr.
        ls_header = im_header->get_data( ).
*    ls_header-ekorg = '1000'."ls_proposal-ekgrp.
        if ls_header-ekgrp ne ls_proposal-ekgrp.
          gv_flg_ekgrp = 'X'.
        endif.
*    ls_header-bukrs ne ls_proposal-bukrs.
        if ls_header-lifnr ne ls_proposal-lifnr.
          gv_flg_lifnr = 'X'.
        endif.

        export gv_flg_ekgrp from gv_flg_ekgrp to memory id 'ME21N_EKGRP'.
        export gv_flg_lifnr from gv_flg_lifnr to memory id 'ME21N_LIFNR2'.

      endif.

      if sy-tcode = 'ME22N'.
        call function 'SAPGUI_SET_FUNCTIONCODE'
          exporting
            functioncode           = 'MECHECKDOC' "'='
          exceptions
            function_not_supported = 1
            others                 = 2.
        if sy-subrc <> 0.
* Implement suitable error handling here
        endif.

        import gp_prno to gp_prno from memory id 'ME21N_ADD_TAB_PRNO_2'.
*    gp_prno = e_ci_ekko-prno.
        ls_header-prno = gp_prno.

      endif.
    endif.

***************************************************************************************************************************************
*DATA : ls_header TYPE mepoheader.
*DATA: wa_t052 TYPE t052.
*DATA: ZJ_1KFTBUS TYPE LFA1-J_1KFTBUS.
*  TYPES : BEGIN OF ty_werks ,
*          lifnr TYPE zmm_vendor_tag-lifnr,
*          werks TYPE  zmm_vendor_tag-werks,
*          city1 TYPE zmm_vendor_tag-city1,
*          country TYPE zmm_vendor_tag-country,
*          region TYPE zmm_vendor_tag-region,
*          bezei TYPE t005u-bezei,
*        END OF ty_werks.
* DATA : it_werks TYPE TABLE OF ty_werks.
*  DATA : wa_werks LIKE LINE OF it_werks.
*
**DATA: zknumh TYPE a018-knumh.
*
**  DATA: pay_term TYPE konp-zterm, wa_a018 type a018.
*  INCLUDE mm_messages_mac. "useful macros for message handling
**  BREAK 10106.
*  ls_header = im_header->get_data( ).
***Validation For Company Code 1000 - Document Type should Start With Z*
***Validation For Company Code 2000 - Document Type should Start With Y*
*
*
*  IF ls_header-bsart EQ 'ZDOM' AND ls_header-bukrs EQ '2000'.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should start with Y* ' '' '' ''.
*  ENDIF.
*  IF ls_header-bsart EQ 'YDOM' AND ls_header-bukrs EQ '1000'.
*    mmpur_message_forced 'E' 'ZMM01' '000' 'Document Type it should Start with Y* ' '' '' ''.
*  ENDIF.
*
*  "Check on Credit Days
*  IF ls_header-bsart NE 'YPTR' AND ls_header-bsart NE 'YSTO' AND ls_header-bsart NE 'ZSTO'
*    AND ls_header-bsart NE 'ZPTR' AND ls_header-bsart NE 'YSTI'  .
*    IF ls_header-zterm IS INITIAL.
*      MESSAGE 'Enter Payment Terms' TYPE 'E'.
*    ENDIF.
*  ENDIF.
*
**  DATA: ZMATNR TYPE EKPO-MATNR.
** BREAK 10106.
**  IF LS_HEADER-ZTERM IS NOT INITIAL.
**
**    SELECT MATNR FROM EKPO UP TO 1 ROWS
**      INTO ZMATNR
**      WHERE EBELN = LS_HEADER-EBELN.
**    ENDSELECT.
**    if sy-subrc = 0 .
**
**    endif.
**
**  ENDIF.
*
*  SELECT SINGLE * FROM t052 INTO wa_t052
*    WHERE zterm = ls_header-zterm.
*  IF sy-subrc = 0.
*    IF wa_t052-koart = 'D'. " changes on 07.01.2014 by 10106
*      MESSAGE 'Select Correct Payment Term of Vendor (Purchasing)' TYPE 'E'.
*    ENDIF.
*  ENDIF.
*
*  "Credit Days can be higher than the master but shall not be allowed to reduce it.
**  SELECT SINGLE * FROM lfm1 INTO wa_lfm1
**    WHERE lifnr = ls_header-lifnr
**    AND ekorg = ls_header-ekorg.
*
*
**  IF sy-subrc = 0.
**    IF wa_lfm1-zterm <> ls_header-zterm.
**      SELECT SINGLE ztag1 FROM t052 INTO prev_ztag1
**        WHERE zterm = wa_lfm1-zterm.
**      SELECT SINGLE ztag1 FROM t052 INTO new_ztag1
**        WHERE zterm = ls_header-zterm.
**      IF new_ztag1 < prev_ztag1.
**        MESSAGE 'Credit Days are Lesser than Vendor Master' TYPE 'E'.
**      ENDIF.
**    ENDIF.
**  ENDIF.
*
** Vendor Tagging - Check if Vendor Plant has been maintained or no
* IF ls_header-bsart EQ 'ZDOM' OR ls_header-bsart EQ 'YDOM'.
*   SET CURSOR FIELD 'EKKO_CI-ZZVENDORPLANT'.
****   BREAK ibmabap01.
*   IF ls_header-lifnr IS NOT INITIAL.
*   IF ls_header-zzvendorplant = space.
*
*     CLEAR: zJ_1KFTBUS.
*        SELECT SINGLE J_1KFTBUS
*          FROM lfa1 INTO zJ_1KFTBUS
*          WHERE lifnr = LS_HEADER-lifnr.
*        if ZJ_1KFTBUS = 'V2 - Multi Plant Manufacturer'.
*
*          gv_lifnr = ls_header-lifnr.
*          CONSTANTS:  mmmfd_cust_01 TYPE mmpur_metafield VALUE 90000000.
*         mmpur_metafield  mmmfd_cust_01.
*         mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Vendor Supplying Plant Location ' '' '' ''.
*         endif.
*    ELSE.
*
***Check if valid vendor supplying plant location has been maintained from user
*       SELECT lifnr
*           werks
*           city1
*           country
*      region
*      FROM zmm_vendor_tag
*      INTO TABLE it_werks WHERE lifnr = ls_header-lifnr.
*
*    CLEAR: zJ_1KFTBUS.
*        SELECT SINGLE J_1KFTBUS
*          FROM lfa1 INTO zJ_1KFTBUS
*          WHERE lifnr = LS_HEADER-lifnr.
*        if ZJ_1KFTBUS = 'V2 - Multi Plant Manufacturer'.
*
*    READ TABLE it_werks INTO wa_werks WITH KEY werks = ls_header-zzvendorplant.
*         IF sy-subrc <> 0.
*            mmpur_metafield  mmmfd_cust_01.
*            mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Valid Vendor Supplying Plant Location ' '' '' ''.
*         ENDIF.
*         endif.
*     ENDIF.
*     ENDIF.
*    ENDIF.
*
**  ENDIF.
*
*
  endmethod.


  method if_ex_me_process_po_cust~process_item.
    data: ls_mepoitem   type mepoitem,
          ls_mepoheader type ref to if_purchase_order_mm,
          ls_header     type mepoheader,
          ls_customer   type mepo_badi_exampl,
          ls_mmparams   type z6mma_params,
          ls_tbsg       type tbsg,
          ls_proposal   type zmm_pur_proposal,
          lt_proposal   type table of zmm_pur_proposal.
    types : begin of ty_werks ,
              lifnr   type zmm_vendor_tag-lifnr,
              werks   type  zmm_vendor_tag-werks,
              city1   type zmm_vendor_tag-city1,
              country type zmm_vendor_tag-country,
              region  type zmm_vendor_tag-region,
              bezei   type t005u-bezei,
            end of ty_werks.
    data : it_werks type table of ty_werks.
    data : wa_werks like line of it_werks.
    data : ev_maktx type mereq_item-txz01.
    data : cl_po type ref to cl_po_header_handle_mm.
    data : gv_flg_matnr,
           gv_flg_lifnr,
           gv_flg_menge,
           gv_flg_netpr,
           gv_flg_werks,
           gv_flg_mwskz,
           gv_flg_bukrs.
    include mm_messages_mac. "useful macros for message handling

*---------------------------------------------------------------------*
* here we retrieve the Item Data
*---------------------------------------------------------------------*
* The following code is written to default Valuation Type (BWTAR) field against given Order Type (BSART) and Material Type (MTART).
* The Default valuation Type against the Order Type and Material Type is being maintained in Z6MMA_PARAMS table by using TCode ZMMPARAM.
* Please maintain the data in the above table as given below.
* Z6MMA_PARAMS-PROGNAME = 'POVALIDATIONS'
* Z6MMA_PARAMS-PARAM1  = 'ZDOM' - PO Order Type
* Z6MMA_PARAMS-PARAM2 = 'ZPKG' - Material Type
* Z6MMA_PARAMS-SERNO = 'Serial No'. - 001
* Z6MMA_PARAMS-PARAMVAL = Default Valuation type - to be displayed on the screen.

    ls_mepoitem = im_item->get_data( ).
    ls_mepoheader = im_item->get_header( ).
    ls_header = ls_mepoheader->get_data( ).

    " Exclude deleted and delivery complete items from validation as per mail from Venu Sir: 09.08.2018: PO ERR
    if ls_mepoitem-loekz eq '' and ls_mepoitem-elikz eq ''. " IRDK933092: MM: S_K: PO_BADI: PO Short-close: 14.8.18

*******Change by monika on 13th Oct 2011****************************

* Following code provide the check for Free Item Tick Mark
* Free Item tick mark should be compulsory before saving the purchase order.
* Document type : 'IBIP'
* Purchasing Organisation : '3000'.


      if sy-tcode eq 'ME21N'.
        if ls_header-bsart = 'IBPI' and ls_header-ekorg = '3000'.
          if ls_mepoitem-umson is initial.
            data : c_e type c value 'E'.
            message text-005 type c_e display like c_e.
            mmpur_message_forced 'E' 'ZMM01' '000' text-005 '' '' ''.
          endif.
        endif.
      endif.

********End of change by monika****************************************


      if ls_header-bsart eq 'ZDOM' or ls_header-bsart eq 'ZIMP'
        or ls_header-bsart eq 'YDOM' or ls_header-bsart eq 'YIMP'.
*    or ls_header-bsart EQ 'ZENG' OR ls_header-bsart EQ 'YENG'.

        if ls_mepoitem-mtart eq 'ZROH' or ls_mepoitem-mtart eq 'ZPKG'.

          select single paramval from z6mma_params into ls_mmparams-paramval
                                              where progname = 'POVALIDATIONS'
                                                and param1 = ls_header-bsart
                                                and param2 = ls_mepoitem-mtart.
          if sy-subrc eq 0.

            ls_mepoitem-bwtar = ls_mmparams-paramval.
            call method im_item->set_data
              exporting
                im_data = ls_mepoitem.


          else.

            mmpur_message_forced 'E' 'ZMM01' '000' text-001 '' '' ''.

          endif.
        endif.
      endif.

* The following code provides validation for CIB Certification done against the material
* belonging to a particular material group and Purchase Order Document Type
* Please maintain the data in the above table as given below for the Warning message to appear
* Z6MMA_PARAMS-PROGNAME = 'POVALIDATIONS'
* Z6MMA_PARAMS-PARAM1  = 'ZIMP' - PO Order Type
* Z6MMA_PARAMS-PARAM2 = '110005' - Material Group
* Z6MMA_PARAMS-SERNO = 'Serial No'. - 001
* Z6MMA_PARAMS-PARAMVAL = 'X'

      select single paramval from z6mma_params into ls_mmparams-paramval
                                           where progname = 'POVALIDATIONS'
                                             and param1 = ls_header-bsart
                                             and param2 = ls_mepoitem-matkl.

      if sy-subrc eq 0 and not ls_mmparams-paramval is initial.

        mmpur_message_forced 'W' 'Z6MM01' '000' text-002 '' '' ''.


      endif.

      if ls_header-bsart+0(1) eq 'Z' and ls_header-bsart ne 'ZINT'. " intercompany STO; IHDK905761
        if ( ls_mepoitem-werks+0(1) eq '2' and ls_mepoitem-werks+0(2) ne '28' ). " IHDK900965; allow ‘Z’ PO type in 28XX plants(IndoReagans)
          mmpur_message_forced 'E' 'ZMM01' '000' text-003 '' '' ''.
        endif.
      endif.

      if ls_header-bsart+0(1) eq 'Y' and  ls_mepoitem-werks+0(1) eq '1'.

        mmpur_message_forced 'E' 'ZMM01' '000' text-003 '' '' ''.
      endif.

      if ls_header-bsart = 'ZSTO'.
        if ls_header-reswk+0(2) eq '14' and ls_header-reswk ne '1400'.
          if ls_mepoitem-bsgru is initial.
            mmpur_message_forced 'E' 'ZMM01' '000' text-004 '' '' ''.
          endif.

        endif.
        if ls_mepoitem-werks eq '1204'.

          if ls_mepoitem-bsgru is initial.
            mmpur_message_forced 'E' 'ZMM01' '000' text-004 '' '' ''.

          endif.
        endif.
      endif.

*  "******************** code added by 10106 Punam for
*  " "Credit Days can be higher than the info records but shall not be allowed to reduce it.
*    IF LS_HEADER-BSART EQ 'YDOM' OR
*   LS_HEADER-BSART EQ 'YIMP' OR
*   LS_HEADER-BSART EQ 'YSED' OR
*   LS_HEADER-BSART EQ 'YSEI' OR
*   LS_HEADER-BSART EQ 'ZDOM' OR
*   LS_HEADER-BSART EQ 'ZIMP' OR
*   LS_HEADER-BSART EQ 'ZSED' OR
*   LS_HEADER-BSART EQ 'ZSEI' .
      data: zknumh type a017-knumh, zzterm type konp-zterm.
*
*  SELECT SINGLE KNUMH
*    FROM A017
*    INTO ZKNUMH
*    WHERE MATNR = LS_MEPOITEM-MATNR
*    AND LIFNR = LS_HEADER-LIFNR
*    AND werks = LS_MEPOITEM-werks
*    AND EKORG = LS_HEADER-EKORG
*    AND DATBI = '99991231'.
*  IF SY-SUBRC = 0.
*    SELECT SINGLE ZTERM
*      FROM KONP
*      INTO ZZTERM
*      WHERE KNUMH = ZKNUMH.
*
*    IF SY-SUBRC = 0 .
*      IF ZZTERM <> LS_HEADER-ZTERM.
*        MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-009 '' '' ''.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*  ENDIF.

***********************************************************************
*** validate if user changes payment term , "Credit Days can be higher than the inforecords but shall not be allowed to reduce it.
*  DATA: HEADER_ZTERM TYPE T052-ZTERM , INFO_ZTERM TYPE T052-ZTERM,
*          HEADER_ZTAG1 TYPE DZTAGE,
*          INFO_ZTAG1 TYPE DZTAGE.
*
*  CLEAR: ZKNUMH , ZZTERM.
*  SELECT SINGLE KNUMH
*    FROM A017
*    INTO ZKNUMH
*    WHERE MATNR = LS_MEPOITEM-MATNR
*    AND LIFNR = LS_HEADER-LIFNR
*    AND werks = LS_MEPOITEM-werks
*    AND EKORG = LS_HEADER-EKORG
*    AND DATBI = '99991231'.
*  IF SY-SUBRC = 0.
*    SELECT SINGLE ZTERM
*      FROM KONP
*      INTO INFO_ZTERM
*      WHERE KNUMH = ZKNUMH.
*    IF SY-SUBRC = 0.
*
*      IF INFO_ZTERM <> LS_HEADER-ZTERM.
*
*        SELECT SINGLE ZTAG1 FROM T052 INTO HEADER_ZTAG1
*           WHERE ZTERM = LS_HEADER-ZTERM.
*        SELECT SINGLE ZTAG1 FROM T052 INTO INFO_ZTAG1
*          WHERE ZTERM = INFO_ZTERM.
*        IF HEADER_ZTAG1 < INFO_ZTAG1.
*          MESSAGE E398(00) WITH 'Credit Days are Lesser than Info records'." L_ITEMS_HEADER-MATNR..
**          MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-007 '' '' ''.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.

***IF ls_header-lifnr IS NOT INITIAL.
***   IF ls_header-zzvendorplant = space.
***     gv_lifnr = ls_header-lifnr.
***     CONSTANTS:  mmmfd_cust_01 TYPE mmpur_metafield VALUE 90000000.
***    mmpur_metafield  mmmfd_cust_01.
***    mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Vendor Supplying Plant Location ' '' '' ''.
***    ELSE.
***
*****Check if valid vendor supplying plant location has been maintained from user
***       SELECT lifnr
***           werks
***           city1
***           country
***      region
***      FROM zmm_vendor_tag
***      INTO TABLE it_werks WHERE lifnr = ls_header-lifnr.
***
***
***    READ TABLE it_werks INTO wa_werks WITH KEY werks = ls_header-zzvendorplant.
***         IF sy-subrc <> 0.
***            mmpur_metafield  mmmfd_cust_01.
***            mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Valid Vendor Supplying Plant Location ' '' '' ''.
***         ENDIF.
***     ENDIF.
***ENDIF.

***** Added by Naren Karra - 26.08.2016 -Start
      ls_mepoitem = im_item->get_data( ).

      if sy-tcode eq 'ME21N' or sy-tcode eq 'ME22N'.
        if ls_mepoitem-matnr is not initial.
          select single maktx from makt into ev_maktx where matnr eq ls_mepoitem-matnr and spras eq sy-langu.
          if sy-subrc eq 0.
            ls_mepoitem-txz01 = ev_maktx.

            call method im_item->set_data
              exporting
                im_data = ls_mepoitem.

          endif.
        endif.
      endif.

    endif.
***** Added by Naren Karra - 26.08.2016 -End
*{   REPLACE        SBXK900102                                        3
*\  if sy-sysid = 'IRD' or sy-sysid = 'IRQ'.
    if zcl_helper=>is_development( ) or zcl_helper=>is_quality( ).
*}   REPLACE
      import ls_header-prno  to ls_header-prno  from memory id 'ME21N_PRNO'.
      import ls_header-lifnr to ls_header-lifnr from memory id 'ME21N_LIFNR'.
      import ls_header-pr_itemno to ls_header-pr_itemno from memory id 'ME21N_PR_ITEMNO'.
      if ls_header-prno is not initial.
        if sy-tcode eq 'ME22N' or sy-tcode eq 'ZMMQUERY1' or sy-tcode eq 'ME23N'.

          select * from zmm_pur_proposal into table lt_proposal where prno = ls_header-prno and app_vendor = 'X'."AND pr_itemno = ls_header-pr_itemno.

          ls_mepoitem = im_item->get_data( ).
          loop at lt_proposal into ls_proposal.
*{   REPLACE        SBXK900102                                        1
*\          call function 'CONVERSION_EXIT_ALPHA_INPUT'
*--------------------------------------------------------------------*                                                                    *
*--<< S/4HANA >>-----------------------------------------------------*
*--------------------------------------------------------------------*                                                                    *
* Changed On - Wednesday, October 11
* Changed By - 6010859 - SaurabhK
* Purpose    - Simplification list - 2215424 - MATNR length change 18 -> 40
* Solution   - Pseudo comments
* TR         - SBXK900028 - S4H: S_K: Simplification List: 03.10.2018
*--------------------------------------------------------------------*                                                                    *
            call function 'CONVERSION_EXIT_ALPHA_INPUT'  "#EC CI_FLDEXT_OK[2215424]
*}   REPLACE
              exporting
                input  = ls_mepoitem-ematn
              importing
                output = ls_mepoitem-ematn.

*{   REPLACE        SBXK900102                                        2
*\          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            call function 'CONVERSION_EXIT_ALPHA_INPUT' "#EC CI_FLDEXT_OK[2215424]
*}   REPLACE
              exporting
                input  = ls_proposal-matnr
              importing
                output = ls_proposal-matnr.

            if ls_mepoitem-ematn ne ls_proposal-matnr.
              gv_flg_matnr = 'X'.
            else.
              clear gv_flg_matnr.
            endif.

            if ls_mepoitem-bukrs ne ls_proposal-bukrs.
              gv_flg_bukrs = 'X'.
            else.
              clear gv_flg_bukrs.
            endif.
            if ls_mepoitem-werks ne ls_proposal-werks.
              gv_flg_werks = 'X'.
            else.
              clear gv_flg_werks.
            endif.

            if ls_mepoitem-netpr ne ls_proposal-netpr.
              gv_flg_netpr = 'X'.
            else.
              clear gv_flg_netpr.
            endif.
            if ls_mepoitem-menge ne ls_proposal-menge.
              gv_flg_menge = 'X'.
            else.
              clear gv_flg_menge.
            endif.
            if ls_mepoitem-mwskz ne ls_proposal-mwskz.
              gv_flg_mwskz = 'X'.
            else.
              clear gv_flg_mwskz.
            endif.
            if ls_mepoitem-kolif is not initial.
              if ls_mepoitem-kolif ne ls_proposal-lifnr.
                gv_flg_lifnr = 'X'.
              endif.
            endif.

            export gv_flg_matnr from gv_flg_matnr to memory id 'ME21N_MATNR'.
            export gv_flg_bukrs from gv_flg_bukrs to memory id 'ME21N_BUKRS'.
            export gv_flg_werks from gv_flg_werks to memory id 'ME21N_WERKS'.
            export gv_flg_menge from gv_flg_menge to memory id 'ME21N_MENGE'.
            export gv_flg_netpr from gv_flg_netpr to memory id 'ME21N_NETPR'.
            export gv_flg_mwskz from gv_flg_mwskz to memory id 'ME21N_MWSKZ'.
            export gv_flg_lifnr from gv_flg_lifnr to memory id 'ME21N_LIFNR'.
          endloop.

** This is YES is of Button Delete i.e., SY-UCOMM = MEPO1211DELETE
          if sy-ucomm = 'YES'.
            data: wa_prno      type zmm_pur_proposal-prno,
                  wa_pr_itemno type zmm_pur_proposal-pr_itemno.
            if ls_mepoitem-loekz is not initial.
              wa_prno = ls_header-prno.
              wa_pr_itemno = ls_mepoitem-ebelp.

              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = wa_pr_itemno
                importing
                  output = wa_pr_itemno.
** DB update in EKKO header table CI_EKKO-PRNO
              update zmm_pur_proposal set cons_proposal = ' '
              where prno = wa_prno
              and   pr_itemno = wa_pr_itemno.
            endif.
          endif.
        endif.
      endif.
    endif.

*****************************************************************************************************************************************
*  DATA: ls_mepoitem TYPE mepoitem,
*        ls_mepoheader TYPE REF TO if_purchase_order_mm,
*        ls_header TYPE mepoheader,
*        ls_customer TYPE mepo_badi_exampl,
*        ls_mmparams TYPE z6mma_params,
*        ls_tbsg     TYPE tbsg.
*  TYPES : BEGIN OF ty_werks ,
*            lifnr TYPE zmm_vendor_tag-lifnr,
*            werks TYPE  zmm_vendor_tag-werks,
*            city1 TYPE zmm_vendor_tag-city1,
*            country TYPE zmm_vendor_tag-country,
*            region TYPE zmm_vendor_tag-region,
*            bezei TYPE t005u-bezei,
*          END OF ty_werks.
*  DATA : it_werks TYPE TABLE OF ty_werks.
*  DATA : wa_werks LIKE LINE OF it_werks.
*  DATA: ev_maktx TYPE mereq_item-txz01.
*
*  INCLUDE mm_messages_mac. "useful macros for message handling
*
**---------------------------------------------------------------------*
** here we retrieve the Item Data
**---------------------------------------------------------------------*
** The following code is written to default Valuation Type (BWTAR) field against given Order Type (BSART) and Material Type (MTART).
** The Default valuation Type against the Order Type and Material Type is being maintained in Z6MMA_PARAMS table by using TCode ZMMPARAM.
** Please maintain the data in the above table as given below.
** Z6MMA_PARAMS-PROGNAME = 'POVALIDATIONS'
** Z6MMA_PARAMS-PARAM1  = 'ZDOM' - PO Order Type
** Z6MMA_PARAMS-PARAM2 = 'ZPKG' - Material Type
** Z6MMA_PARAMS-SERNO = 'Serial No'. - 001
** Z6MMA_PARAMS-PARAMVAL = Default Valuation type - to be displayed on the screen.
*
*
*  ls_mepoitem = im_item->get_data( ).
*  ls_mepoheader = im_item->get_header( ).
*  ls_header = ls_mepoheader->get_data( ).
*
*
********Change by monika on 13th Oct 2011****************************
*
** Following code provide the check for Free Item Tick Mark
** Free Item tick mark should be compulsory before saving the purchase order.
** Document type : 'IBIP'
** Purchasing Organisation : '3000'.
*
*
*  IF sy-tcode EQ 'ME21N'.
*    IF ls_header-bsart = 'IBPI' AND ls_header-ekorg = '3000'.
*      IF ls_mepoitem-umson IS INITIAL.
*        DATA : c_e TYPE c VALUE 'E'.
*        MESSAGE text-005 TYPE c_e DISPLAY LIKE c_e.
*        mmpur_message_forced 'E' 'ZMM01' '000' text-005 '' '' ''.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*********End of change by monika****************************************
*
*
*  IF ls_header-bsart EQ 'ZDOM' OR ls_header-bsart EQ 'ZIMP'
*    OR ls_header-bsart EQ 'YDOM' OR ls_header-bsart EQ 'YIMP'.
*
*    IF ls_mepoitem-mtart EQ 'ZROH' OR ls_mepoitem-mtart EQ 'ZPKG'.
*
*      SELECT SINGLE paramval FROM z6mma_params INTO ls_mmparams-paramval
*                                          WHERE progname = 'POVALIDATIONS'
*                                            AND param1 = ls_header-bsart
*                                            AND param2 = ls_mepoitem-mtart.
*      IF sy-subrc EQ 0.
*
*        ls_mepoitem-bwtar = ls_mmparams-paramval.
*        CALL METHOD im_item->set_data
*          EXPORTING
*            im_data = ls_mepoitem.
*
*
*      ELSE.
*
*        mmpur_message_forced 'E' 'ZMM01' '000' text-001 '' '' ''.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
** The following code provides validation for CIB Certification done against the material
** belonging to a particular material group and Purchase Order Document Type
** Please maintain the data in the above table as given below for the Warning message to appear
** Z6MMA_PARAMS-PROGNAME = 'POVALIDATIONS'
** Z6MMA_PARAMS-PARAM1  = 'ZIMP' - PO Order Type
** Z6MMA_PARAMS-PARAM2 = '110005' - Material Group
** Z6MMA_PARAMS-SERNO = 'Serial No'. - 001
** Z6MMA_PARAMS-PARAMVAL = 'X'
*
*  SELECT SINGLE paramval FROM z6mma_params INTO ls_mmparams-paramval
*                                       WHERE progname = 'POVALIDATIONS'
*                                         AND param1 = ls_header-bsart
*                                         AND param2 = ls_mepoitem-matkl.
*
*  IF sy-subrc EQ 0 AND NOT ls_mmparams-paramval IS INITIAL.
*
*    mmpur_message_forced 'W' 'Z6MM01' '000' text-002 '' '' ''.
*
*
*  ENDIF.
*
*  IF ls_header-bsart+0(1) EQ 'Z' AND  ls_mepoitem-werks+0(1) EQ '2'.
*
*    mmpur_message_forced 'E' 'ZMM01' '000' text-003 '' '' ''.
*  ENDIF.
*
*  IF ls_header-bsart+0(1) EQ 'Y' AND  ls_mepoitem-werks+0(1) EQ '1'.
*
*    mmpur_message_forced 'E' 'ZMM01' '000' text-003 '' '' ''.
*  ENDIF.
*
*  IF ls_header-bsart = 'ZSTO'.
*    IF ls_header-reswk+0(2) EQ '14' AND ls_header-reswk NE '1400'.
*      IF ls_mepoitem-bsgru IS INITIAL.
*        mmpur_message_forced 'E' 'ZMM01' '000' text-004 '' '' ''.
*
*      ENDIF.
*
*    ENDIF.
*    IF ls_mepoitem-werks EQ '1204'.
*
*      IF ls_mepoitem-bsgru IS INITIAL.
*        mmpur_message_forced 'E' 'ZMM01' '000' text-004 '' '' ''.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
**  "******************** code added by 10106 Punam for
**  " "Credit Days can be higher than the info records but shall not be allowed to reduce it.
**    IF LS_HEADER-BSART EQ 'YDOM' OR
**   LS_HEADER-BSART EQ 'YIMP' OR
**   LS_HEADER-BSART EQ 'YSED' OR
**   LS_HEADER-BSART EQ 'YSEI' OR
**   LS_HEADER-BSART EQ 'ZDOM' OR
**   LS_HEADER-BSART EQ 'ZIMP' OR
**   LS_HEADER-BSART EQ 'ZSED' OR
**   LS_HEADER-BSART EQ 'ZSEI' .
*  DATA: zknumh TYPE a017-knumh , zzterm TYPE konp-zterm.
**
**  SELECT SINGLE KNUMH
**    FROM A017
**    INTO ZKNUMH
**    WHERE MATNR = LS_MEPOITEM-MATNR
**    AND LIFNR = LS_HEADER-LIFNR
**    AND werks = LS_MEPOITEM-werks
**    AND EKORG = LS_HEADER-EKORG
**    AND DATBI = '99991231'.
**  IF SY-SUBRC = 0.
**    SELECT SINGLE ZTERM
**      FROM KONP
**      INTO ZZTERM
**      WHERE KNUMH = ZKNUMH.
**
**    IF SY-SUBRC = 0 .
**      IF ZZTERM <> LS_HEADER-ZTERM.
**        MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-009 '' '' ''.
**      ENDIF.
**    ENDIF.
**
**  ENDIF.
**  ENDIF.
*
************************************************************************
**** validate if user changes payment term , "Credit Days can be higher than the inforecords but shall not be allowed to reduce it.
**  DATA: HEADER_ZTERM TYPE T052-ZTERM , INFO_ZTERM TYPE T052-ZTERM,
**          HEADER_ZTAG1 TYPE DZTAGE,
**          INFO_ZTAG1 TYPE DZTAGE.
**
**  CLEAR: ZKNUMH , ZZTERM.
**  SELECT SINGLE KNUMH
**    FROM A017
**    INTO ZKNUMH
**    WHERE MATNR = LS_MEPOITEM-MATNR
**    AND LIFNR = LS_HEADER-LIFNR
**    AND werks = LS_MEPOITEM-werks
**    AND EKORG = LS_HEADER-EKORG
**    AND DATBI = '99991231'.
**  IF SY-SUBRC = 0.
**    SELECT SINGLE ZTERM
**      FROM KONP
**      INTO INFO_ZTERM
**      WHERE KNUMH = ZKNUMH.
**    IF SY-SUBRC = 0.
**
**      IF INFO_ZTERM <> LS_HEADER-ZTERM.
**
**        SELECT SINGLE ZTAG1 FROM T052 INTO HEADER_ZTAG1
**           WHERE ZTERM = LS_HEADER-ZTERM.
**        SELECT SINGLE ZTAG1 FROM T052 INTO INFO_ZTAG1
**          WHERE ZTERM = INFO_ZTERM.
**        IF HEADER_ZTAG1 < INFO_ZTAG1.
**          MESSAGE E398(00) WITH 'Credit Days are Lesser than Info records'." L_ITEMS_HEADER-MATNR..
***          MMPUR_MESSAGE_FORCED 'E' 'ZMM01' '000' TEXT-007 '' '' ''.
**        ENDIF.
**      ENDIF.
**    ENDIF.
**  ENDIF.
*
****IF ls_header-lifnr IS NOT INITIAL.
****   IF ls_header-zzvendorplant = space.
****     gv_lifnr = ls_header-lifnr.
****     CONSTANTS:  mmmfd_cust_01 TYPE mmpur_metafield VALUE 90000000.
****    mmpur_metafield  mmmfd_cust_01.
****    mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Vendor Supplying Plant Location ' '' '' ''.
****    ELSE.
****
******Check if valid vendor supplying plant location has been maintained from user
****       SELECT lifnr
****           werks
****           city1
****           country
****      region
****      FROM zmm_vendor_tag
****      INTO TABLE it_werks WHERE lifnr = ls_header-lifnr.
****
****
****    READ TABLE it_werks INTO wa_werks WITH KEY werks = ls_header-zzvendorplant.
****         IF sy-subrc <> 0.
****            mmpur_metafield  mmmfd_cust_01.
****            mmpur_message_forced 'E' 'ZMM01' '000' 'Please Select Valid Vendor Supplying Plant Location ' '' '' ''.
****         ENDIF.
****     ENDIF.
****ENDIF.
****** Added by Naren Karra - 26.08.2016 -Start
*  ls_mepoitem = im_item->get_data( ).
*
*  IF sy-tcode EQ 'ME21N' OR sy-tcode EQ 'ME22N'.
*    IF ls_mepoitem-matnr IS NOT INITIAL.
*      SELECT SINGLE maktx FROM makt INTO ev_maktx WHERE matnr EQ ls_mepoitem-matnr AND spras EQ sy-langu.
*      IF sy-subrc EQ 0.
*        ls_mepoitem-txz01 = ev_maktx.
*
*        CALL METHOD im_item->set_data
*          EXPORTING
*            im_data = ls_mepoitem.
**      ELSE.
******  Their is a standard message by SAP if Material does not exist !!
**        MESSAGE 'Material does not exist. Kindly check' TYPE 'S' DISPLAY LIKE 'E'.
*      ENDIF.
*    ENDIF.
*  ENDIF.
****** Added by Naren Karra - 26.08.2016 -End

    " IRDK932298: Wednesday, May 30, 2018 11:41:17: Dev: 6010859 - SaurabhK
    " Auto add ZSTR condition type for ZSTO order type (purch grp for such orders is always between 301 and 304)
    " This is only applicable for agro/indolife domestic
    if ls_mepoitem is not initial and ls_header-bsart = 'ZSTO' and ( ls_header-ekgrp between '301' and '304' ).
      select single spart from mara into @data(material_division) where matnr = @ls_mepoitem-matnr.
      select single * from mvke into @data(material_sales_area) where matnr = @ls_mepoitem-matnr.
      if material_sales_area-vkorg = '1000' and material_sales_area-vtweg = '10'
        and ( material_division = '10' or material_division = '15' ).
        " Add ZSTR condition type: Value for the same is determined via routine based on ZARP value in condition record
        im_item->get_conditions( importing ex_conditions = data(conditions) ).
        read table conditions into data(condition) with key kschl = 'ZSTR'. " this check prevents repeat addition of same condition type in loop
        if sy-subrc <> 0.
          condition-kposn = ls_mepoitem-ebelp.
          condition-kschl = 'ZSTR'.
          append condition to conditions.
          " set method makes changes in the PO
          im_item->set_conditions( exporting im_conditions = conditions ).
        endif.

        " Set default 'Reason for order' to 054 - Others; this is done as per business requirement from Mr. Tapas Mitra/Mr. Manas Achrekar
        if ls_mepoitem-bsgru is initial.
          ls_mepoitem-bsgru = '054'.  " others
          " set method makes changes in the PO
          im_item->set_data( exporting im_data = ls_mepoitem ).
        endif.
      endif.
    endif.
    " End IRDK932298

  endmethod.


  method if_ex_me_process_po_cust~process_schedule.
  endmethod.
ENDCLASS.
