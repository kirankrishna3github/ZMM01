*&---------------------------------------------------------------------*
*& Report  ZMM_PRG_MASS_STO_SHORT_CLOSE
*&---------------------------------------------------------------------*
*& Transaction            : ZMM088
*& Creation Date          : Thursday, September 06, 2018 12:59:57
*& Author                 : 6010859 - SaurabhK
*& Functional             : Mr. Kamalakar Varma
*& Requested/Approved By  : Mr Narayan Kunder
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK933330
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&---------------------------------------------------------------------*
*& 1. Calculate date range as per option selected
*& 2. i. all STO option = 00.00.0000 to last date of prev month
*& 3. ii. prev month STO only = first day of prev month to last date of prev month
*& 4. Get STO open as per above date range and other filters
*&---------------------------------------------------------------------*
*& Following scenarios are considered, along with action taken:
*&---------------------------------------------------------------------*
*& 1. Item fully delivered and GR done – set delivery complete indicator if not auto-set by the system
*& 2. Item partially delivered
*&  2.1. not delivered at all – Mark the item as delivery complete without changing the quantity
*&  2.2. Item partially delivered, either in transit or GR done
*&    2.2.1. if item is in transit (delivery done but GR not done) or some part in transit and some part GR done -
*&           ...Reduce the item quantity proportionately; delivery complete indicator cannot be marked in such case.
*&           ...System will auto handle that when the GR is done.
*&    2.2.2. if item is partially delivered and GR done (none in transit) -
*&           ...Reduce the item quantity proportionately and mark delivery complete indicator.
*&---------------------------------------------------------------------*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Monday, September 10, 2018 17:52:00
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : Mr. Narayan Kunder
*& Rev. Request#              : IRDK933338, IRDK933372, IRDK933374, IRDK933423,
*&                              IRDK933427, IRDK933439, IRDK933443, IRDK933447, IRDK933452, IRDK933454, IRDK933456 = pre-production changes
*& Rev. Description           : MM: S_K: ZMM088: Mass STO short-close chng: 10.9.18
*&---------------------------------------------------------------------*
*& Message nos: 06 073, 06 280, ME 589, ME 039, 06 028 => E->W for short-closing old STO's
*&---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Monday, September 24, 2018 14:16:13
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : Mr. Narayan Kunder
*& Rev. Request#              : IRDK933460 - Fix logic for reverse entries, IRDK933474 - Allow closure of 'other' items of intransit sto
*& Rev. Description           : MM: S_K: ZMM088: Fixes/Changes: 24.09.2018
*&---------------------------------------------------------------------*

report zmm_prg_mass_sto_short_close.

* ---- Global data and selection screen ---- *
data: sto_no        type ekko-ebeln,
      sto_dt        type ekko-bedat,
      sending_plant type ekko-reswk,
      recving_plant type ekpo-werks,
      purch_org     type ekko-ekorg,
      purch_grp     type ekko-ekgrp,
      comp_code     type ekko-bukrs,
      division      type mara-spart.

selection-screen begin of block sel with frame title text-sel.
select-options: s_ebeln for sto_no,
                s_bedat for sto_dt,
                s_reswk for sending_plant,
                s_werks for recving_plant,
                s_ekorg for purch_org,
                s_ekgrp for purch_grp,
                s_bukrs for comp_code,
                s_spart for division.
selection-screen end of block sel.

selection-screen begin of block opt with frame title text-opt.
parameters: p_all  radiobutton group rad,
            p_mnth radiobutton group rad default 'X'.
selection-screen end of block opt.

selection-screen begin of block tst with frame title text-tst.
parameter: test_run as checkbox.
selection-screen end of block tst.

* ---- local class declaration ---- *
class lcl_application definition final.
  public section.
    methods: process.

  private section.
    methods: calc_dates, get_sto_data, process_sto, display_log,
      hotspot_click for event link_click of cl_salv_events_table  " event handler
        importing
            row
            column.

    data: first_day_of_previous_month type sy-datum,
          last_day_of_previous_month  type sy-datum.

    data: begin of sto,
            ebeln  type wb2_v_ekko_ekpo2-ebeln,
            ebelp  type wb2_v_ekko_ekpo2-ebelp_i,
            menge  type wb2_v_ekko_ekpo2-menge_i,
            elikz  type wb2_v_ekko_ekpo2-elikz_i,
            uebto  type wb2_v_ekko_ekpo2-uebto_i,
            memory type wb2_v_ekko_ekpo2-memory,
          end of sto,
          sto_tab like standard table of sto,

          begin of sto_hist,
            ebeln type wb2_v_ekko_ekpo2-ebeln,
            ebelp type wb2_v_ekko_ekpo2-ebelp_i,
            vgabe type ekbe-vgabe,
            menge type ekbe-menge,
            gjahr type ekbe-gjahr,
            belnr type ekbe-belnr,
            buzei type ekbe-buzei,
            shkzg type ekbe-shkzg,
          end of sto_hist,
          sto_hist_tab like standard table of sto_hist,

          begin of out,
            icon          type icon_d,
            ebeln         type wb2_v_ekko_ekpo2-ebeln,
            ebelp         type wb2_v_ekko_ekpo2-ebelp_i,
            menge         type wb2_v_ekko_ekpo2-menge_i,
            menge_c       type wb2_v_ekko_ekpo2-menge_i,
            delv_qty      type ekbe-menge,
            grr_qty       type ekbe-menge,
            intransit_qty type ekbe-menge,
            elikz         type wb2_v_ekko_ekpo2-elikz_i,
            elikz_c       type wb2_v_ekko_ekpo2-elikz_i,
            uebto_c       type wb2_v_ekko_ekpo2-uebto_i,
            on_hold       type wb2_v_ekko_ekpo2-memory,
            status(20)    type c,
            message       type bapi_msg,
          end of out,
          outtab like standard table of out,

          begin of msg_log,
            ebeln   type ekko-ebeln,
            msg_typ type bapiret2-type,
            msg     type bapi_msg,
          end of msg_log,
          msg_logs like standard table of msg_log with non-unique sorted key ebeln components ebeln.
endclass.

class main definition.
  public section.
    class-methods: start.
endclass.

* ---- local class implementation ---- *
class lcl_application implementation.
  method process.
    calc_dates( ).
    get_sto_data( ).

    if sto_tab is not initial.
      process_sto( ).
    else.
      message 'No processable data found' type 'S' display like 'E'.
      return.
    endif.

    display_log( ).
  endmethod.

  method calc_dates.
    data: today type sy-datum.

    clear: today, last_day_of_previous_month.
    today = sy-datum.
    call function 'OIL_LAST_DAY_OF_PREVIOUS_MONTH'  " get any one day of the previous month, here we get the last day
      exporting
        i_date_old = today
      importing
        e_date_new = last_day_of_previous_month.

    check last_day_of_previous_month is not initial.
    clear first_day_of_previous_month.
    call function 'HR_JP_MONTH_BEGIN_END_DATE'    " get the start and end date of the previous month using the date returned above
      exporting
        iv_date             = last_day_of_previous_month
      importing
        ev_month_begin_date = first_day_of_previous_month
        ev_month_end_date   = last_day_of_previous_month.

    if p_all eq abap_true.
      clear first_day_of_previous_month.  " get all data till end of previous month
    endif.
  endmethod.

  method get_sto_data.
    refresh: sto_tab.
    select distinct a~ebeln
      from wb2_v_ekko_ekpo2 as a
      join mara as b
      on a~matnr_i eq b~matnr
      into table sto_tab
      where a~ebeln in s_ebeln
      and   a~bedat in s_bedat
      and   ( a~bedat between first_day_of_previous_month and last_day_of_previous_month )
      and   a~reswk in s_reswk
      and   a~werks_i in s_werks
      and   a~bsart in ( 'YSTO', 'ZSTO' )
      and   a~bstyp eq 'F'  " purchase order
      and   a~ekorg in s_ekorg
      and   a~ekgrp in s_ekgrp
      and   a~bukrs in s_bukrs
      and   a~elikz_i ne abap_true " not already closed
      and   a~loekz_i eq ''        " not deleted, IRDK933338
      and   b~spart in s_spart
      order by ebeln ascending.

    check sto_tab is not initial.
    select distinct a~ebeln a~ebelp_i a~menge_i a~elikz_i a~uebto_i a~memory
      from wb2_v_ekko_ekpo2 as a
      join mara as b
      on a~matnr_i eq b~matnr
      into table sto_tab
      for all entries in sto_tab
      where a~ebeln eq sto_tab-ebeln
      and   a~bedat in s_bedat
      and   ( a~bedat between first_day_of_previous_month and last_day_of_previous_month )
      and   a~reswk in s_reswk
      and   a~werks_i in s_werks
      and   a~bsart in ( 'YSTO', 'ZSTO' )
      and   a~bstyp eq 'F'  " purchase order
      and   a~ekorg in s_ekorg
      and   a~ekgrp in s_ekgrp
      and   a~bukrs in s_bukrs
      and   a~loekz_i eq ''        " not deleted, IRDK933338
      and   b~spart in s_spart.

    check sto_tab is not initial.
    sort sto_tab by ebeln ebelp ascending.
    select ebeln ebelp vgabe menge gjahr belnr buzei shkzg  " belnr buzei gjahr included since key was repeated in some cases
      from ekbe
      into table sto_hist_tab
      for all entries in sto_tab
      where ebeln = sto_tab-ebeln
      and   ebelp = sto_tab-ebelp
      and   vgabe in ( '1', '8' )  " 1 = grr, 8 = deliv
      and   menge gt 0.
  endmethod.

  method process_sto.
    type-pools: icon.
    data: delv_qty      type ekbe-menge,
          grr_qty       type ekbe-menge,
          intransit_qty type ekbe-menge.

    data purchaseorder type bapimepoheader-po_number.
    data testrun       type bapiflag-bapiflag.
    data expheader     type bapimepoheader.
    data return        type standard table of bapiret2.
    data return_wa     type bapiret2.
    data poitem        type standard table of bapimepoitem.
    data poitem_wa     type bapimepoitem.
    data poitemx       type standard table of bapimepoitemx.
    data poitemx_wa    type bapimepoitemx.

    data index type i.
    data perc  type i.

    clear index.
    check sto_tab is not initial.
    data(sto_tab_copy) = sto_tab.
    sort sto_tab_copy by ebeln ebelp.
    delete adjacent duplicates from sto_tab_copy comparing ebeln.
    clear sto.
    loop at sto_tab_copy into data(sto_copy).
      index = index + 1.

      perc = conv i( conv percentage( ( index / lines( sto_tab_copy ) ) * 100 ) ).
      call function 'SAPGUI_PROGRESS_INDICATOR'
        exporting
          percentage = perc
          text       = |Processing STO { index } of { lines( sto_tab_copy ) }: { sto_copy-ebeln }|.

      clear: purchaseorder, testrun, expheader.
      refresh: return, poitem, poitemx.
      loop at sto_tab into sto where ebeln = sto_copy-ebeln.
        clear: delv_qty, grr_qty, intransit_qty, out.
        out-ebeln = sto-ebeln.
        out-ebelp = sto-ebelp.
        out-menge = sto-menge.
        out-elikz = sto-elikz.
        out-on_hold = sto-memory.

        loop at sto_hist_tab into sto_hist where ebeln = sto-ebeln and ebelp = sto-ebelp.
          if sto_hist-shkzg eq 'H'.
            sto_hist-menge = sto_hist-menge * -1.
          endif.
          if sto_hist-vgabe = '8'.
            delv_qty = delv_qty + sto_hist-menge.
          endif.
          if sto_hist-vgabe = '1'.
            grr_qty  = grr_qty  + sto_hist-menge.
          endif.
          clear sto_hist.
        endloop.

        intransit_qty = delv_qty - grr_qty.

        if intransit_qty ne 0.
          data(intransit_flag) = abap_true.
        endif.

        out-delv_qty      = delv_qty.
        out-grr_qty       = grr_qty.
        out-intransit_qty = intransit_qty.

        data(menge_bkp) = sto-menge.
        if sto-menge le grr_qty.  " item fully delivered and gr done, but delv complete indicator not set by system
          " set elikz = 'X', do not change qty
          sto-elikz = abap_true.
          try.
              out-uebto_c = ceil( abs( 100 - ( ( grr_qty + intransit_qty ) / sto-menge ) * 100 ) ).  " for overdelivery cases,
            catch cx_sy_arithmetic_overflow.
              out-icon = icon_red_light.
              out-status = 'Failure'.
              out-message = 'Overdelivery out of tolerance bounds.'.
          endtry.
          " calculate and set overdelivery limit; and set elikz = 'X' if not already set;
          " do not alter/increase qty
*          sto-menge = grr_qty + intransit_qty.  " for supporting overdelivery cases, this will increase order qty = grr + intransit
        elseif sto-menge gt grr_qty.  " item not at all dispatched or is in transit
          " set elikz = 'X', change item qty to grr_qty + intransit_qty
          sto-menge = grr_qty + intransit_qty.  " remaining qty = qty that is not yet dispatched is removed
          out-menge_c = sto-menge.

          if sto-menge = 0. " this will happen in case nothing has been dispatched i.e no sto history
            sto-menge = menge_bkp.  " this is required since system does not allow 0 qty line
            " so keep the qty as it is and just mark the item as deliv completed
          endif.

          if intransit_qty eq 0.
            sto-elikz = abap_true.  " deliv complete cannot be marked for items in transit
            " for such items, we just reduce the qty appropriately and let the system set the deliv compl indicator
            " as and when the intransit items are delivered/grr done at the recieving location
            " Note that the intransit qty for non-dispatched items will always be 0, so this works for those
            " items as well
          endif.
        endif.

        out-menge_c = sto-menge.
        out-elikz_c = sto-elikz.

        clear: return_wa, poitem_wa, poitemx_wa.

        if intransit_flag eq abap_false.
          if ( sto-elikz = abap_true and out-elikz eq abap_false ) or sto-menge <> menge_bkp or out-uebto_c is not initial.
            purchaseorder = sto-ebeln.

            poitem_wa-po_item = sto-ebelp.
            if sto-elikz = abap_true and out-elikz eq abap_false.
              poitem_wa-no_more_gr = sto-elikz.
            endif.
            if sto-menge <> menge_bkp.
              poitem_wa-quantity = sto-menge.
            endif.
            if out-uebto_c is not initial.
              poitem_wa-over_dlv_tol = out-uebto_c.
            endif.
            append poitem_wa to poitem.

            poitemx_wa-po_item = sto-ebelp.
            poitemx_wa-po_itemx = abap_true.
            if sto-elikz = abap_true and out-elikz eq abap_false.
              poitemx_wa-no_more_gr = abap_true.
            endif.
            if sto-menge <> menge_bkp.
              poitemx_wa-quantity = abap_true.
            endif.
            if out-uebto_c is not initial.
              poitemx_wa-over_dlv_tol = abap_true.
            endif.
            append poitemx_wa to poitemx.
          else.
            if out-icon is initial and out-status is initial and out-message is initial.
              out-icon = icon_yellow_light.
              out-status = 'Warning'.
              out-message = 'No changes required'.
            endif.
          endif.
        else.
          out-icon = icon_red_light.
          out-status = 'Failure'.
          out-message = 'Item is still in transit. Cannot be closed/processed.'.
        endif.
        if out-message <> 'No changes required'.
          append out to outtab.
        endif.

        clear: sto_hist, menge_bkp, sto, intransit_flag.
      endloop.

      if poitem is not initial and poitemx is not initial.
*      if intransit_flag eq abap_false.
        testrun = test_run.
        call function 'BAPI_PO_CHANGE'
          exporting
            purchaseorder = purchaseorder
            testrun       = testrun
          importing
            expheader     = expheader
          tables
            return        = return
            poitem        = poitem
            poitemx       = poitemx.

        read table return into data(ret_test) with key id = 'MMPUR_BASE' number = '053' type = 'I'.

        if return is not initial.
          delete return where id eq 'BAPI' or id eq 'MEPO' or ( type ne 'E' and type ne 'A' and type ne 'S' ).
        endif.

        read table return into return_wa with key type = 'E'.
        if sy-subrc <> 0 .
          read table return into return_wa with key type = 'A'.
          if sy-subrc <> 0.
            read table return into return_wa with key id = '06' number = '023'.  " success message
            if sy-subrc = 0.
              if testrun eq abap_false.
                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait = abap_true.
              endif.
              out-icon = icon_green_light.
              out-status = 'Success'.
              out-message = return_wa-message.
            else.
              if ret_test is not initial.
                out-icon = icon_green_light.
                out-status = 'Success'.
                out-message = ret_test-message.
              else.
                out-icon = icon_red_light.
                out-status = 'Failure'.
                out-message = 'Unknown error. Please retry manually'.
              endif.
            endif.
          endif.
        endif.

        if return_wa-type eq 'E' or return_wa-type eq 'A'.
          out-icon = icon_red_light.
          out-status = 'Failure'.
          out-message = return_wa-message.
        endif.

        loop at outtab into data(out_copy) where ebeln = sto_copy-ebeln.
          if out_copy-icon is initial and out_copy-status is initial and out_copy-message is initial.
            out_copy-icon = out-icon.
            out_copy-status = out-status.
            out_copy-message = out-message.
            modify outtab from out_copy transporting icon status message.
          endif.
          clear out_copy.
        endloop.

        " loop at return anyways
        clear msg_log.
        clear: return_wa.
        loop at return into return_wa.
          msg_log-ebeln = sto_copy-ebeln.
          msg_log-msg_typ = return_wa-type.
          msg_log-msg = return_wa-message.

          if out-status eq 'Success'.
            if return_wa-type = 'S' and return_wa-id = '06' and return_wa-number = '023'. " only add success message for successful creation
              append msg_log to msg_logs.
            endif.
          else. " in other cases add all messages
            append msg_log to msg_logs.
          endif.

          clear: msg_log, return_wa.
        endloop.
*      else.
*        clear out_copy.
*        loop at outtab into out_copy where ebeln = sto_copy-ebeln.
*          if out_copy-icon is initial and out_copy-status is initial and out_copy-message is initial.
*            out_copy-icon    = icon_red_light.
*            out_copy-status  = 'Failure'.
*            out_copy-message = 'Atleast one item of this STO is in transit. Cannot be closed/processed.'.
*            modify outtab from out_copy transporting icon status message.
*          endif.
*          clear out_copy.
*        endloop.
*      endif.
      endif.
      clear: sto_copy, intransit_flag, ret_test.
    endloop.

    check msg_logs is not initial.
    sort msg_logs ascending.
    delete adjacent duplicates from msg_logs comparing all fields.

    check outtab is not initial.
    sort outtab ascending by ebeln ebelp.

  endmethod.

  method display_log.
    data: o_table     type ref to cl_salv_table,
          o_container type ref to cl_gui_container,
          o_functions type ref to cl_salv_functions_list,
          o_columns   type ref to cl_salv_columns_table,
          o_column    type ref to cl_salv_column_table,
          o_col_list  type ref to cl_salv_column_list,
          o_layout    type ref to cl_salv_layout,
          o_layo      type ref to cl_salv_layout_service,
          o_key       type salv_s_layout_key,
          o_info      type salv_s_layout_info,
          o_display   type ref to cl_salv_display_settings,
          o_head_grid type ref to cl_salv_form_layout_grid,
          o_label     type ref to cl_salv_form_label,
          o_flow      type ref to cl_salv_form_layout_flow,
          o_events    type ref to cl_salv_events_table.


    free: o_table    ,
    o_container,
    o_functions,
    o_columns  ,
    o_column   ,
    o_col_list ,
    o_layout   ,
    o_layo     ,
    o_key      ,
    o_info     ,
    o_display  ,
    o_head_grid,
    o_label    ,
    o_flow     ,
    o_events   .

    if outtab is not initial.

      try.
          cl_salv_table=>factory(
          exporting
            list_display = if_salv_c_bool_sap=>false
          importing
            r_salv_table = o_table
          changing
            t_table      = outtab ).
        catch cx_salv_msg .
      endtry.

      check o_table is bound.

      o_columns = o_table->get_columns( ).

      if o_columns is bound.
        try.
            " Column procesing
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ICON' ).
            o_column->set_long_text( value = 'Status Icon' ).
            o_column->set_medium_text( value = 'Status Icon' ).
            o_column->set_short_text( value = 'Stat Icon' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EBELN' ).
            o_column->set_long_text( value = 'STO Number' ).
            o_column->set_medium_text( value = 'STO Number' ).
            o_column->set_short_text( value = 'STO' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EBELP' ).
            o_column->set_long_text( value = 'STO Item' ).
            o_column->set_medium_text( value = 'STO Item' ).
            o_column->set_short_text( value = 'Item' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MENGE' ).
            o_column->set_long_text( value = 'Original Quantity' ).
            o_column->set_medium_text( value = 'Original Quantity' ).
            o_column->set_short_text( value = 'Qty' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MENGE_C' ).
            o_column->set_long_text( value = 'Changed Quantity' ).
            o_column->set_medium_text( value = 'Chnged Quantity' ).
            o_column->set_short_text( value = 'Ch Qty' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'DELV_QTY' ).
            o_column->set_long_text( value = 'Delivery Quantity' ).
            o_column->set_medium_text( value = 'Delivery Quantity' ).
            o_column->set_short_text( value = 'Del Qty' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'GRR_QTY' ).
            o_column->set_long_text( value = 'Goods Reciept Quantity' ).
            o_column->set_medium_text( value = 'GR Quantity' ).
            o_column->set_short_text( value = 'GR Qty' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'INTRANSIT_QTY' ).
            o_column->set_long_text( value = 'Intransit Quantity' ).
            o_column->set_medium_text( value = 'Intransit Quantity' ).
            o_column->set_short_text( value = 'Intr Qty' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ELIKZ' ).
            o_column->set_long_text( value = 'Delivery Complete Indicator' ).
            o_column->set_medium_text( value = 'Del. Compl. Ind.' ).
            o_column->set_short_text( value = 'DelCmpl' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ELIKZ_C' ).
            o_column->set_long_text( value = 'Changed Delivery Complete Indicator' ).
            o_column->set_medium_text( value = 'Ch. Del. Compl. Ind.' ).
            o_column->set_short_text( value = 'Ch DelCmpl' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'UEBTO_C' ).
            o_column->set_long_text( value = 'Changed Over Delivery Tolerance' ).
            o_column->set_medium_text( value = 'Ch. Ovr. Del. Tol.' ).
            o_column->set_short_text( value = 'Ch OvDlTol' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ON_HOLD' ).
            o_column->set_long_text( value = 'STO Held / Incomplete?' ).
            o_column->set_medium_text( value = 'STO Held?' ).
            o_column->set_short_text( value = 'Held?' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'STATUS' ).
            o_column->set_long_text( value = 'Processing Status' ).
            o_column->set_medium_text( value = 'Processing Status' ).
            o_column->set_short_text( value = 'Proc Stat' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MESSAGE' ).
            o_column->set_long_text( value = 'Message' ).
            o_column->set_medium_text( value = 'Message' ).
            o_column->set_short_text( value = 'Message' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

          catch: cx_salv_not_found.
        endtry.
        o_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
      endif.

      o_functions = o_table->get_functions( ).

      if o_functions is bound.
        o_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
      endif.

      o_layout = o_table->get_layout( ).

      o_key-report = sy-repid.

      if o_layout is bound.
        o_layout->set_key( exporting value = o_key ).

        o_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

        o_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
      endif.

      o_display = o_table->get_display_settings( ).

      if o_display is bound.
        o_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
      endif.

      " Build report header
      create object o_head_grid.
      if o_head_grid is bound.
        o_label = o_head_grid->create_label( exporting row = 1 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'Mass STO Short Close: Processing Log' ).
        endif.

        o_label = o_head_grid->create_label( exporting row = 2 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'No. of rows:' ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 2 column = 2 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = | { lines( outtab ) }| ).
        endif.

        o_label = o_head_grid->create_label( exporting row = 3 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'Time-stamp:' ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 3 column = 2 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = |{ sy-datum date = user } { sy-uzeit time = user }| ).
        endif.

        o_table->set_top_of_list( exporting value = o_head_grid ).
        o_table->set_top_of_list_print( exporting value = o_head_grid ).
      endif.

      " Event handling - hotpost click
      o_events = o_table->get_event( ).
      if o_events is bound.
        set handler hotspot_click for o_events.
      endif.

      o_table->display( ).
    else.
      message 'No log generated' type 'S' display like 'E'.
    endif.

  endmethod.

  method hotspot_click.
    data: lo_table     type ref to cl_salv_table,
          lo_columns   type ref to cl_salv_columns_table,
          lo_functions type ref to cl_salv_functions_list,
          lo_column    type ref to cl_salv_column_table,
          lo_display   type ref to cl_salv_display_settings.

    case column.
      when 'ICON' or 'STATUS' or 'MESSAGE'. " IRDK932867
        clear out.
        read table outtab into out index row.
        if sy-subrc = 0.
          data(index_log) = filter #( msg_logs using key ebeln where ebeln = out-ebeln ).
          sort index_log.
          try.
              cl_salv_table=>factory(
              exporting
                list_display = abap_false
              importing
                r_salv_table = lo_table
              changing
                t_table      = index_log ).
            catch cx_salv_msg.
          endtry.

          if lo_table is bound.
            lo_columns = lo_table->get_columns( ).
            if lo_columns is bound.
              lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
            endif.

            lo_functions = lo_table->get_functions( ).

            if lo_functions is bound.
              lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
            endif.

            lo_display = lo_table->get_display_settings( ).

            if lo_display is bound.
              lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
              lo_display->set_list_header( exporting value = 'Processing log' ).
            endif.

* ALV as Popup
            lo_table->set_screen_popup(
            start_column = 5
            end_column   = 80
            start_line   = 1
            end_line     = 10 ).

* Display
            lo_table->display( ).
          endif.
        endif.
      when 'EBELN'.
        clear out.
        read table outtab into out index row.
        if sy-subrc = 0 and out-ebeln is not initial.
          set parameter id 'BES' field out-ebeln.
          call transaction 'ME23N' and skip first screen.
        endif.
      when others.
    endcase.
  endmethod.
endclass.

class main implementation.
  method start.
    data: lo_app type ref to lcl_application.

    free lo_app.
    lo_app = new lcl_application( ).

    check lo_app is bound.
    lo_app->process( ).
  endmethod.
endclass.

* ---- events ---- *
initialization.

at selection-screen output.

at selection-screen.
  if sy-ucomm eq 'ONLI' and sy-batch ne abap_true.
    if p_all eq abap_true and s_ebeln is initial and s_bedat is initial.
      message 'Selected option cannot be executed in foreground mode!' type 'I' display like 'E'.
      stop.
    endif.
  endif.

start-of-selection.
  main=>start( ).

end-of-selection.
