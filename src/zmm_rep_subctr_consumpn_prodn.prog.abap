*&---------------------------------------------------------------------*
*& Report  ZMM_REP_SUBCTR_CONSUMPN_PRODN
*&---------------------------------------------------------------------*
*& Transaction            : ZMM080
*& Creation Date          : Thursday, November 23, 2017 09:45:04
*& Author                 : ABAP02 - SaurabhK
*& Functional             : Kamalakar Varma
*& Requested/Approved By  : Mr Bhupesh Adoni
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK930205
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&
*======================================================================*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Wednesday, July 25, 2018 10:06:31
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : KV
*& Rev. Request#              : IRDK932631, IRDK932950
*& Rev. Description           : MM: S_K: ZMM080/81: Fixes for region f4: 25.7.18
*&---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Thursday, September 20, 2018 12:10:34
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : Mr Gyan Pandey
*& Rev. Request#              : IRDK933433, IRDK933437
*& Rev. Description           : MM: S_K: ZMM080: Chngs: Mail frm Gyan Pandey: 20.09.2018
*&---------------------------------------------------------------------*
*& Revision #                 : 03
*& Rev. Date                  : Monday, September 24, 2018 12:12:08
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : Mr Gyan Pandey/Mr Belkar
*& Rev. Request#              : IRDK933470
*& Rev. Description           : MM: S_K: ZMM080: Add gr No: 24.09.2018
*&---------------------------------------------------------------------*
*& Revision #                 : 04
*& Rev. Date                  : Thursday, March 28, 2019 17:04:31
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : Mr Bhupesh Adoni
*& Rev. Request#              : IHDK901122
*& Rev. Description           : MM: S_K: ZMM080/81: Changes as per Adoni Sir: 28.3.19
*&---------------------------------------------------------------------*
report zmm_rep_subctr_consumpn_prodn.

* ---- Global data and selection screen ---- *
data: gr_date  type mkpf-budat,
      state    type t001w-regio,
      vendor   type ekko-lifnr,
      material type mseg-matnr,
      plant    type t001w-werks,
      division type mara-spart.

selection-screen begin of block sel with frame title text-001.
select-options: s_date for gr_date obligatory,
                s_state for state,
                s_vend  for vendor,
                s_matnr for material,
                s_plant for plant,
                s_divs  for division.
selection-screen end of block sel.

selection-screen begin of block opt with frame title text-002.
parameters: p_recpt radiobutton group opt default 'X',
            p_consm radiobutton group opt,
            p_both  radiobutton group opt.
selection-screen end of block opt.

selection-screen begin of block lay with frame title text-lay.
parameters: p_var type disvariant-variant.
selection-screen end of block lay.

* ---- local class definitions ---- *
* ---- begin local exception class definition ---- *
class lcx_generic_error definition inheriting from cx_static_check.
  " Used in throwing a generic exception, Error displayed is based on context of the exception
endclass.
* ---- end exception class definition ---- *

* ---- Local class definitions ---- *
class lcl_sel_screen_events definition.
  public section.
    interfaces: if_f4callback_value_request.  " IHDK901122

    methods: variant_f4_selection, check_variant_existence, variant_init,
      f4_state importing dynprofield type help_info-dynprofld, " IRDK932631.
      f4_plant. " IHDK901122
endclass.

class lcl_sel_screen_events implementation.
  method variant_f4_selection.
    data: ls_layout type salv_s_layout_info,
          ls_key    type salv_s_layout_key.

    clear: ls_key, ls_layout.
    ls_key-report = sy-repid.

    ls_layout = cl_salv_layout_service=>f4_layouts(
    s_key    = ls_key
    restrict = if_salv_c_layout=>restrict_none  ).

    p_var = ls_layout-layout.
  endmethod.

  method check_variant_existence.
    check sy-ucomm eq 'ONLI'.
    data: lt_layout type salv_t_layout_info,
          ls_layout type salv_s_layout_info,
          ls_key    type salv_s_layout_key.

    clear ls_key.
    ls_key-report = sy-repid.
    if not p_var is initial.
      refresh lt_layout.
      lt_layout = cl_salv_layout_service=>get_layouts( s_key = ls_key ).
      if lt_layout is not initial.
        read table lt_layout into ls_layout with key layout = p_var.
        if sy-subrc <> 0.
          message 'No such variant' type 'S' display like 'E'.
          leave screen.
        endif.
      else.
        message 'No such variant' type 'S' display like 'E'.
        leave screen.
      endif.
    else.
      variant_init( ).
    endif.
  endmethod.

  method variant_init.
    data: value  type salv_s_layout_info,
          ls_key type salv_s_layout_key.

    clear: value, ls_key.
    ls_key-report = sy-repid.
    value = cl_salv_layout_service=>get_default_layout( s_key = ls_key ).

    if value is not initial.
      p_var = value-layout.
    endif.
  endmethod.

  method f4_state.  " IRDK932631
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
  endmethod.

  method f4_plant.  " IHDK901122
    data lt_return type standard table of ddshretval.

    refresh lt_return.
    data lo_f4_callback type ref to if_f4callback_value_request.
    free lo_f4_callback.
    lo_f4_callback ?= new lcl_sel_screen_events( ).
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
  endmethod.

  method if_f4callback_value_request~f4_call_callback.  " IHDK901122
    data lt_dynpfields type standard table of dynpread.
    refresh lt_dynpfields.

    if s_state[] is initial.  " f4 without an intermediate ok code
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
        where ( fieldname eq 'S_STATE-LOW' or fieldname eq 'S_STATE-HIGH' )
        and fieldvalue is not initial.
        if s_state[] is initial.
          append initial line to s_state assigning field-symbol(<ls_state>).
        else.
          read table s_state assigning <ls_state> index 1.
        endif.
        if <ls_state> is assigned.
          <ls_state>-sign = 'I'.
          <ls_state>-option = cond #( when ls_dynpfields-fieldname cs 'HIGH' then 'BT' else 'EQ' ).
          <ls_state>-low = cond #( when ls_dynpfields-fieldname cs 'LOW' then ls_dynpfields-fieldvalue else <ls_state>-low ).
          <ls_state>-high = cond #( when ls_dynpfields-fieldname cs 'HIGH' then ls_dynpfields-fieldvalue else <ls_state>-high ).
        endif.
        clear ls_dynpfields.
        unassign <ls_state>.
      endloop.
    endif.

    select werks as plant
      from t001w
      into table @data(lt_plant)
      where regio in @s_state.

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

class lcl_alv definition.
  public section.
    methods:
      constructor,
      execute.

  protected section.

  private section.
    data: begin of plant,
            werks type t001w-werks,
            name1 type t001w-name1,   " plant name
            regio type t001w-regio,   " state code
            land1 type t001w-land1,   " country key
            stcd3 type kna1-stcd3,    " plant gstn number
          end of plant,
          plants like standard table of plant,

          begin of state,
            bland type t005u-bland,
            bezei type t005u-bezei,
          end of state,
          states like standard table of state,

          begin of migo,  " query with xauto_i = ''
            mblnr type wb2_v_mkpf_mseg2-mblnr,    " migo doc no
            mjahr type wb2_v_mkpf_mseg2-mjahr,    " year
            zeile type wb2_v_mkpf_mseg2-zeile_i,  " line item
            vgart type wb2_v_mkpf_mseg2-vgart,    " should be 'WE' => Goods Reciept, transaction type
            blart type wb2_v_mkpf_mseg2-blart,    " should be 'WE' => Goods Reciept, document type
            bldat type wb2_v_mkpf_mseg2-bldat,    " document date
            budat type wb2_v_mkpf_mseg2-budat,    " posting date,  subcontractor delivery challan date
            xblnr type wb2_v_mkpf_mseg2-xblnr,    " delivery note, subcontractors delivery challan no
            bwart type wb2_v_mkpf_mseg2-bwart_i,  " movement type, 101, 543
            matnr type wb2_v_mkpf_mseg2-matnr_i,  " material
            werks type wb2_v_mkpf_mseg2-werks_i,  " plant
            lifnr type wb2_v_mkpf_mseg2-lifnr_i,  " vendor
            menge type wb2_v_mkpf_mseg2-menge_i,  " quantity
            meins type wb2_v_mkpf_mseg2-meins_i,  " uom
            dmbtr type wb2_v_mkpf_mseg2-dmbtr_i,  " amount in local curreny, display this or calculate?
            charg type wb2_v_mkpf_mseg2-charg_i,  " batch no
            hsdat type wb2_v_mkpf_mseg2-hsdat_i,  " date of manufacture
            vfdat type wb2_v_mkpf_mseg2-vfdat_i,  " expiry date
            ebeln type wb2_v_mkpf_mseg2-ebeln_i,  " purchase order,
            ebelp type wb2_v_mkpf_mseg2-ebelp_i,  " po item no
            spart type mara-spart,                " Division
          end of migo,
          migo_table like standard table of migo,

          begin of purch_order,
            ebeln type ekpo-ebeln,
            bedat type ekko-bedat,  " po date
          end of purch_order,
          purch_orders like standard table of purch_order,

          begin of vendor,
            lifnr type lfa1-lifnr,
            name1 type lfa1-lifnr,  " vendor name
            stcd3 type lfa1-stcd3,  " gstn no
            regio type lfa1-regio,  " state code
          end of vendor,
          vendors like standard table of vendor,

          begin of description,
            matnr type makt-matnr,
            maktx type makt-maktx,
          end of description,
          descriptions like standard table of description,

          begin of output,
            plant        like plant-werks,  " plant
            plnt_desc    like plant-name1,  " plant name
            plnt_gstn    like plant-stcd3,  " plant gstn; IHDK901122
            vendor       like vendor-lifnr, " vendor code
            vend_name    like vendor-name1, " vendor name
            vend_gstn    like vendor-stcd3, " vendor gstn no
            vend_state   like state-bezei,  " vendor state
            tran_typ(30) type c,            " trans type (receipt, consumption)
            gr_no        like migo-mblnr,   " gr number
            post_date    like migo-budat,   " posting date
            org_del_no   like migo-xblnr,   " original delivery challan no
            org_del_date like migo-budat,   " original delivery challan date
            sub_del_no   like migo-xblnr,   " subcontractors delivery challan no
            sub_del_date like migo-budat,   " subcontracttors delivery challan date
            po_no        like purch_order-ebeln,  " purchase order number
            po_date      like purch_order-bedat,  " purhcase order date
            material     like migo-matnr,   " material
            mat_desc     like description-maktx,  " material description
            unit         like migo-meins,   " unit of measure
            gstn_unit    type zgstn_uom-gstn_uom,    " IHDK901122; equivalent gstn uom
            quantity     like migo-menge,   " quantity
            value        like migo-dmbtr,   " taxable value, calculation based on moving avg price?
            batch        like migo-charg,   " batch number
            mfg_date     like migo-hsdat,   " mfg date
            exp_date     like migo-vfdat,   " expiry date
            pk_lf        type i,            " pack life in days = exp date - today
            division     like migo-spart,    " division - IHDK902476
          end of output,
          output_table like standard table of output.

    methods:
      fetch,
      fetch_plants,
      fetch_states,

      deduce_mvt_type
        returning
          value(mvt_types) type bwart_t_range,

      fetch_migo_gr
        raising lcx_generic_error,

      fetch_sub_ctr_po,

      filter_subctr_migo
        raising lcx_generic_error,

      fetch_vendor_gstn,
      fetch_material_desc,

      process
        changing
          output_table like output_table,

      display
        changing
          output_table like output_table.

    data: gv_noauthorisation type flag.

endclass.     " lcl_alv

class lcl_main definition.
  public section.
    class-methods: main.

  protected section.

  private section.
    class-data: lo_alv type ref to lcl_alv.
endclass.     " lcl_main

* ---- local class implementation ---- *
class lcl_alv implementation.
  method constructor.
    " Some initilization if required
  endmethod.

  method execute.
    try.
        fetch( ).
      catch lcx_generic_error.
        return.
    endtry.
    process( changing output_table = output_table ).
    display( changing output_table = output_table ).
  endmethod.

  method fetch.
    fetch_plants( ).
    fetch_states( ).
    try.
        fetch_migo_gr( ).
      catch lcx_generic_error.
        return.
    endtry.
    fetch_sub_ctr_po( ).
    try.
        filter_subctr_migo( ).
      catch lcx_generic_error.
        return.
    endtry.
    fetch_vendor_gstn( ).
    fetch_material_desc( ).
  endmethod.

  method process.
    " IHDK901122
    select *
      from zgstn_uom
      into table @data(lt_gstn_uom).

    check migo_table is not initial.
    clear: migo, output.
    refresh output_table.
    loop at migo_table into migo.
      clear plant.
      read table plants into plant with key werks = migo-werks.
      if sy-subrc = 0.
        output-plant = plant-werks.
        output-plnt_desc = plant-name1.
        output-plnt_gstn = plant-stcd3. " IHDK901122
      endif.
      clear vendor.
      read table vendors into vendor with key lifnr = migo-lifnr.
      if sy-subrc is initial.
        output-vendor    = vendor-lifnr.
        output-vend_name = vendor-name1.
        output-vend_gstn = vendor-stcd3.

        clear state.
        read table states into state with key bland = vendor-regio.
        if sy-subrc is initial.
          output-vend_state = state-bezei.
        endif.
      endif.

      if migo-bwart = '101'.
        output-tran_typ = 'Receipt'.
      elseif migo-bwart = '543'.
        output-tran_typ = 'Consumption'.
      endif.

      output-gr_no     = migo-mblnr.
      output-post_date = migo-budat.

*      output_table-org_del_no   =
*      output_table-org_del_date =

      clear purch_order.
      read table purch_orders into purch_order with key ebeln = migo-ebeln.
      if sy-subrc = 0.
        output-po_no = purch_order-ebeln.
        output-po_date = purch_order-bedat.
      endif.

      output-sub_del_no   = migo-xblnr.
      output-sub_del_date = migo-budat.
      output-material     = migo-matnr.

      clear description.
      read table descriptions into description with key matnr = migo-matnr.
      if sy-subrc is initial.
        output-mat_desc = description-maktx.
      endif.

      output-unit         = migo-meins.

      " IHDK901122
      try.
          output-gstn_unit = lt_gstn_uom[ sap_uom = output-unit ]-gstn_uom.
        catch cx_sy_itab_line_not_found.
      endtry.

      output-quantity     = migo-menge.
      output-value        = migo-dmbtr.

      output-batch    = migo-charg.
      output-mfg_date = migo-hsdat.
      output-exp_date = migo-vfdat.
      if migo-vfdat is not initial.
        output-pk_lf = migo-vfdat - sy-datum.
      endif.

      output-division = migo-spart. " IHDK902476

      append output to output_table.
      clear: migo, output.
    endloop.
  endmethod.

  method display.
    data: lines(5) type c.  " No. of rows in output_table
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
          o_flow      type ref to cl_salv_form_layout_flow.

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
          o_flow     .

    if output_table is not initial.

      try.
          cl_salv_table=>factory(
            exporting
            list_display = if_salv_c_bool_sap=>false
            importing
            r_salv_table = o_table
            changing
            t_table      = output_table ).
        catch cx_salv_msg .
      endtry.

      check o_table is bound.

      o_columns = o_table->get_columns( ).

      if o_columns is bound.
        try.
            " Column procesing
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PLANT' ).
            o_column->set_long_text( value = 'Plant' ).
            o_column->set_medium_text( value = 'Plant' ).
            o_column->set_short_text( value = 'Plant' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PLNT_DESC' ).
            o_column->set_long_text( value = 'Plant Name' ).
            o_column->set_medium_text( value = 'Plant Name' ).
            o_column->set_short_text( value = 'Plant Name' ).

            " IHDK901122
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PLNT_GSTN' ).
            o_column->set_long_text( value = 'Plant GST Number' ).
            o_column->set_medium_text( value = 'Plant GSTN' ).
            o_column->set_short_text( value = 'Plant GST' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VENDOR' ).
            o_column->set_long_text( value = 'Vendor' ).
            o_column->set_medium_text( value = 'Vendor' ).
            o_column->set_short_text( value = 'Vend' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VEND_NAME' ).
            o_column->set_long_text( value = 'Vendor Name' ).
            o_column->set_medium_text( value = 'Vendor Name' ).
            o_column->set_short_text( value = 'Vend Name' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VEND_GSTN' ).
            o_column->set_long_text( value = 'Vendor GSTN' ).
            o_column->set_medium_text( value = 'Vendor GSTN' ).
            o_column->set_short_text( value = 'Vend GSTN' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VEND_STATE' ).
            o_column->set_long_text( value = 'Vendor State' ).
            o_column->set_medium_text( value = 'Vendor State' ).
            o_column->set_short_text( value = 'Vend State' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TRAN_TYP' ).
            o_column->set_long_text( value = 'Transaction Type' ).
            o_column->set_medium_text( value = 'Trans. Type' ).
            o_column->set_short_text( value = 'Tr. Typ.' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'GR_NO' ).
            o_column->set_long_text( value = 'Goods Receipt Number' ).
            o_column->set_medium_text( value = 'Goods Receipt No.' ).
            o_column->set_short_text( value = 'GR No' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'POST_DATE' ).
            o_column->set_long_text( value = 'Posting Date' ).
            o_column->set_medium_text( value = 'Posting Date' ).
            o_column->set_short_text( value = 'Post. Date' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ORG_DEL_NO' ).
            o_column->set_long_text( value = 'Original Delivery Challan No' ).
            o_column->set_medium_text( value = 'Orig. Del. No' ).
            o_column->set_short_text( value = 'Org Del No' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ORG_DEL_DATE' ).
            o_column->set_long_text( value = 'Original Delivery Challan Date' ).
            o_column->set_medium_text( value = 'Orig. Del. Date' ).
            o_column->set_short_text( value = 'Org Del Dt' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SUB_DEL_NO' ).
            o_column->set_long_text( value = 'SubContracting Delivery Challan No' ).
            o_column->set_medium_text( value = 'SubCtr. Del. No' ).
            o_column->set_short_text( value = 'Sub Del No' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SUB_DEL_DATE' ).
            o_column->set_long_text( value = 'SubContracting Delivery Challan Date' ).
            o_column->set_medium_text( value = 'SubCtr. Del. Date' ).
            o_column->set_short_text( value = 'Sub Del Dt' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_NO' ).
            o_column->set_long_text( value = 'Purchase Order Number' ).
            o_column->set_medium_text( value = 'PO Number' ).
            o_column->set_short_text( value = 'PO No.' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_DATE' ).
            o_column->set_long_text( value = 'Purchase Order Date' ).
            o_column->set_medium_text( value = 'PO Date' ).
            o_column->set_short_text( value = 'PO Date' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MATERIAL' ).
            o_column->set_long_text( value = 'Item Code' ).
            o_column->set_medium_text( value = 'Item Code' ).
            o_column->set_short_text( value = 'Item Code' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MAT_DESC' ).
            o_column->set_long_text( value = 'Item Description' ).
            o_column->set_medium_text( value = 'Item Desc.' ).
            o_column->set_short_text( value = 'Item Desc' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'UNIT' ).
            o_column->set_long_text( value = 'Unit of Measure' ).
            o_column->set_medium_text( value = 'Unit of Measure' ).
            o_column->set_short_text( value = 'UoM' ).

            " IHDK901122
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'GSTN_UNIT' ).
            o_column->set_long_text( value = 'GSTN Unit of Measure' ).
            o_column->set_medium_text( value = 'GSTN Unit of Measure' ).
            o_column->set_short_text( value = 'GSTN UoM' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'QUANTITY' ).
            o_column->set_long_text( value = 'Quantity' ).
            o_column->set_medium_text( value = 'Quantity' ).
            o_column->set_short_text( value = 'Quantity' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VALUE' ).
            o_column->set_long_text( value = 'Taxable Value' ).
            o_column->set_medium_text( value = 'Taxable Value' ).
            o_column->set_short_text( value = 'Value' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BATCH' ).
            o_column->set_long_text( value = 'Batch Number' ).
            o_column->set_medium_text( value = 'Batch No.' ).
            o_column->set_short_text( value = 'Batch No.' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MFG_DATE' ).
            o_column->set_long_text( value = 'Manufacturing Date' ).
            o_column->set_medium_text( value = 'Mfg Date' ).
            o_column->set_short_text( value = 'Mfg Date' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EXP_DATE' ).
            o_column->set_long_text( value = 'Expiry Date' ).
            o_column->set_medium_text( value = 'Expiry Date' ).
            o_column->set_short_text( value = 'Exp Date' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PK_LF' ).
            o_column->set_long_text( value = 'Pack Life in Days' ).
            o_column->set_medium_text( value = 'Pack Life (Days)' ).
            o_column->set_short_text( value = 'Pack Life' ).

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

      clear lines.
      describe table output_table lines lines.
      condense lines.

      " Build report header
      create object o_head_grid.
      if o_head_grid is bound.
        o_label = o_head_grid->create_label( exporting row = 1 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'Subcontracting Receipt(101), Consumption(543)' ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 2 column = 1 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = 'No. of rows' && ` ` && lines ).
        endif.

        o_table->set_top_of_list( exporting value = o_head_grid ).
        o_table->set_top_of_list_print( exporting value = o_head_grid ).
      endif.

      o_table->display( ).
    else.
      message 'No data found' type 'S' display like 'E'.
    endif.
  endmethod.

  method fetch_plants.
    " get plants based on state code
    refresh plants.
    select a~werks
           a~name1
           a~regio
           a~land1
           b~stcd3
      from t001w as a
      join kna1 as b
      on b~kunnr = a~kunnr
      into table plants
      where a~werks in s_plant
      and   a~regio in s_state.

*---  Authority Check Added by Sandeep for Plant on 05.07.2019 ---------*
    clear: gv_noauthorisation.
    if plants is not initial. " IHDK902465
      loop at plants into plant.
        authority-check object 'M_MATE_WRK'
                  id 'ACTVT' field '03'
                  id 'WERKS' field plant-werks.
        if sy-subrc ne 0.
          delete plants where werks = plant-werks.  " IHDK902474
          gv_noauthorisation = 'X'. "PLANT-WERKS.
        endif.

        clear plant.
      endloop.

      if gv_noauthorisation = 'X'.
        message 'Authorization is missing. Please check SU53' type 'I' display like 'W'.
      endif.
    endif.
*--- End-of-Addition ---------------------------------------------------*

  endmethod.

  method fetch_states.
    " get state code description, will be used for plants as well as vendors(mostly vendors)
    refresh states.
    select bland
           bezei
      from t005u
      into table states
      where land1 = 'IN'  " India
      and   spras = 'E'.  " English
  endmethod.

  method deduce_mvt_type.
    " create a movement type range for migo selection based on option selected by user
    data: mvt_type like line of mvt_types.

    refresh mvt_types.
    clear mvt_type.
    if p_recpt eq abap_true.      " recpt/incoming production only (101)
      mvt_type-sign = 'I'.
      mvt_type-option = 'EQ'.
      mvt_type-low = '101'.
      append mvt_type to mvt_types.
    elseif p_consm eq abap_true.  " consumption only (543)
      mvt_type-sign = 'I'.
      mvt_type-option = 'EQ'.
      mvt_type-low = '543'.
      append mvt_type to mvt_types.
    elseif p_both eq abap_true.   " both 101 and 543
      mvt_type-sign = 'I'.
      mvt_type-option = 'EQ'.
      mvt_type-low = '101'.
      append mvt_type to mvt_types.

      clear mvt_type.
      mvt_type-sign = 'I'.
      mvt_type-option = 'EQ'.
      mvt_type-low = '543'.
      append mvt_type to mvt_types.
    endif.
  endmethod.

  method fetch_migo_gr.
    data: mvt_types type bwart_t_range.
    refresh mvt_types.
    mvt_types = deduce_mvt_type( ).

    " get migo data based on plants and date
    check plants is not initial.
    refresh migo_table.
    select a~mblnr
           a~mjahr
           a~zeile_i
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
           a~charg_i
           a~hsdat_i
           a~vfdat_i
           a~ebeln_i
           a~ebelp_i
           b~spart
      from wb2_v_mkpf_mseg2 as a
      join mara as b
      on a~matnr_i eq b~matnr
      into table migo_table
      for all entries in plants
      where a~vgart = 'WE'  " Goods receipt,   also filter on tcode = 'MIGO_GR' ?
      and   a~budat in s_date
      and   a~werks_i = plants-werks
      and   a~werks_i in s_plant
      and   a~bwart_i in mvt_types " 101 - receipt/incom. production, 543 - consumption
      and   a~lifnr_i in s_vend
      and   a~matnr_i in s_matnr
      and   b~spart in s_divs
      and   a~xauto_i = ''. " no auto generated lines

*---  Authority Check Added by Sandeep for Division on 05.07.2019 ---------*
    clear: gv_noauthorisation.

    if migo_table is not initial.
      loop at migo_table into migo.
         authority-check object 'ZMM_SPART' " IHDK902466
          id 'ACTVT' field '03'
          id 'SPART' field migo-spart.
        if sy-subrc ne 0.
          delete migo_table where spart = migo-spart.
          gv_noauthorisation = 'X'. "MIGO-SPART.
        endif.
        clear: migo.
      endloop.

      if gv_noauthorisation = 'X'.
        message 'Authorization is missing. Please check SU53' type 'I' display like 'W'.
      endif.
    endif.
*--- End-of-Addition ---------------------------------------------------*

    " rasie exception if migo table is initial
    if  migo_table is initial.
      message 'No data found' type 'S' display like 'E'.
      raise exception type lcx_generic_error.
    endif.
  endmethod.

  method fetch_sub_ctr_po.
    " get only subcontracting po's out of all po's in migo table
    check migo_table is not initial.
    refresh purch_orders.
    select distinct ebeln bedat
      from wb2_v_ekko_ekpo2       ##DB_FEATURE_MODE[TABLE_LEN_MAX1]
      into table purch_orders
      for all entries in migo_table
      where ebeln = migo_table-ebeln
      and   pstyp_i = '3'.  " item category(L), 3 = sub contracting = L
  endmethod.

  method filter_subctr_migo.
    " delete migo's that are not of a subcontracting po(consider only subcontracting migo)
    if purch_orders is initial. " no sub contracting po's found
      refresh migo_table. " delete all material docs since none of them are SubCtr type
    else.
      loop at migo_table into migo.
        clear purch_order.
        read table purch_orders into purch_order with key ebeln = migo-ebeln.
        if sy-subrc is not initial. " if its not a subcontracting po
          delete migo_table where mblnr = migo-mblnr. " delete that material document
        endif.
        clear migo.
      endloop.
    endif.

    " rasie exception if migo table is initial
    if  migo_table is initial.
      message 'No data found' type 'S' display like 'E'.
      raise exception type lcx_generic_error.
    endif.
  endmethod.

  method fetch_vendor_gstn.
    check migo_table is not initial.
    refresh vendors.
    select lifnr
           name1
           stcd3
           regio
      from lfa1
      into table vendors
      for all entries in migo_table
      where lifnr = migo_table-lifnr. " vendor
  endmethod.

  method fetch_material_desc.
    check migo_table is not initial.
    refresh descriptions.
    select matnr
           maktx
      from makt
      into table descriptions
      for all entries in migo_table
      where matnr = migo_table-matnr " material
      and   spras = 'E'.
  endmethod.
endclass.     " lcl_alv

class lcl_main implementation.
  method main.
    lo_alv = new lcl_alv( ).
    lo_alv->execute( ).
  endmethod.
endclass.     " lcl_main

* ---- Selection screen events ---- *
* ---- initialisation ---- *
initialization.
  data: lo_sel type ref to lcl_sel_screen_events.

  free lo_sel.
  lo_sel = new lcl_sel_screen_events( ).

  if lo_sel is bound.
    lo_sel->variant_init( ).
  endif.

* ---- value request ---- *
at selection-screen on value-request for p_var.
  if lo_sel is bound.
    lo_sel->variant_f4_selection( ).
  endif.

at selection-screen on value-request for s_state-low.   " IRDK932631
  if lo_sel is bound.
    lo_sel->f4_state( exporting dynprofield = 'S_STATE-LOW' ).
  endif.

at selection-screen on value-request for s_state-high.  " IRDK932631
  if lo_sel is bound.
    lo_sel->f4_state( exporting dynprofield = 'S_STATE-HIGH' ).
  endif.

  " IHDK901122 ---->

at selection-screen on value-request for s_plant-low.
  if lo_sel is bound.
    lo_sel->f4_plant( ).
  endif.

at selection-screen on value-request for s_plant-high.
  if lo_sel is bound.
    lo_sel->f4_plant( ).
  endif.
  " End IHDK901122

* ---- at sel screen ---- *
at selection-screen.
  if lo_sel is bound.
    lo_sel->check_variant_existence( ).
  endif.

start-of-selection.
  try.
      lcl_main=>main( ).
    catch cx_root.
  endtry.
