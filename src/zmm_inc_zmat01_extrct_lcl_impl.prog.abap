*&---------------------------------------------------------------------*
*&  Include           ZMM_INC_MAT_EXT_DATA_LCL_IMPL
*&---------------------------------------------------------------------*
class lcl_alv_helper implementation.
  method execute.
    unassign <lt_output>.
    assign c_tab to <lt_output>.
    if <lt_output> is assigned.
      fetch_data( ).
      process_data( ).
      display_output( ).
    endif.
  endmethod.
  method fetch_data.
    read_mara( ).
    read_marc( ).
    read_makt( ).
    read_mard( ).
    read_mbew( ).
    read_mvke( ).
    read_marm( ).
    read_mlan( ).
  endmethod.

  method read_mara.
    refresh material_table.
    select *
      from mara
      into table material_table
      where matnr in s_matnr
      and   spart in s_spart
      and   mtart in r_mtart.      " Report output restricted to these material types

    " Rev 04
    check material_table is not initial.
    sort material_table ascending.
    " IHDK902332
    loop at material_table into material.
      authority-check object 'M_MATE_MAR'
          id 'ACTVT' dummy
          id 'BEGRU' field material-mtart.
      if sy-subrc <> 0.
        delete material_table where mtart eq material-mtart.
        data(lv_no_auth) = abap_true.
      endif.

      authority-check object 'V_VBAK_VKO'
       id 'VKORG' dummy
       id 'VTWEG' dummy
       id 'SPART' field material-spart
       id 'ACTVT' dummy.
      if sy-subrc <> 0.
        delete material_table where spart eq material-spart.
        lv_no_auth = abap_true.
      endif.

      clear material.
    endloop.
    if lv_no_auth eq abap_true.
      message 'Authorisation missing. Check SU53.' type 'I' display like 'W'.
    endif.
    " End Rev 04
  endmethod.

  method read_marc.
    check material_table is not initial.
    refresh plantdata_table.
    select *
      from marc
      into table plantdata_table
      for all entries in material_table
      where matnr eq material_table-matnr
      and werks in s_werks.

    " Rev 04
    check plantdata_table is not initial.
    sort plantdata_table ascending.
    if p_rad3 eq abap_true or p_rad6 eq abap_true. " depot
      delete plantdata_table where werks not between '1301' and '1499'.
    endif.
    if p_rad2 eq abap_true or p_rad5 eq abap_true or p_rad10 eq abap_true or p_rad12 eq abap_true or p_rad14 eq abap_true.  " plant
      delete plantdata_table where werks between '1301' and '1499'.
    endif.

    " IHDK902332
    loop at plantdata_table into plantdata.
      authority-check object 'M_MATE_WRK'
          id 'ACTVT' dummy
          id 'WERKS' field plantdata-werks.
      if sy-subrc <> 0.
        delete plantdata_table where werks eq plantdata-werks.
        data(lv_no_auth) = abap_true.
      endif.
      clear plantdata.
    endloop.
    if lv_no_auth eq abap_true.
      message 'Authorisation missing. Check SU53.' type 'I' display like 'W'.
    endif.
    " End Rev 04
  endmethod.

  method read_makt.
    check plantdata_table is not initial.
    refresh description_table.
    select *
      from makt
      into table description_table
      for all entries in plantdata_table
      where matnr eq plantdata_table-matnr
      and   spras eq 'E'.

    " Rev 04
    check description_table is not initial.
    sort description_table ascending.
    " End Rev 04
  endmethod.

  method read_mard.
    check plantdata_table is not initial.
    refresh strg_loc_table.
    select *
      from mard
      into table strg_loc_table
      for all entries in plantdata_table
      where matnr = plantdata_table-matnr
      and   werks = plantdata_table-werks
      and   lgort in s_lgort.

    " Rev 04
    check strg_loc_table is not initial.
    sort strg_loc_table ascending.
    " End Rev 04
  endmethod.

  method read_mbew.
    check plantdata_table is not initial.
    refresh valuationdata_table.
    select *
      from mbew
      into table valuationdata_table
      for all entries in plantdata_table
      where matnr = plantdata_table-matnr
      and   ( bwkey in s_bwkey and bwkey = plantdata_table-werks ).

    " Rev 04
    check valuationdata_table is not initial.
    sort valuationdata_table ascending.
  endmethod.

  method read_mvke.
    check plantdata_table is not initial.
    refresh salesdata_table.
    select *
      from mvke
      into table salesdata_table
      for all entries in material_table
      where matnr eq material_table-matnr
      and   vkorg in s_vkorg
      and   vtweg in s_vtweg.

    check salesdata_table is not initial.
    sort salesdata_table ascending.

    loop at salesdata_table into salesdata.
      authority-check object 'M_MATE_VKO'
        id 'ACTVT' dummy
        id 'VKORG' field salesdata-vkorg
        id 'VTWEG' field salesdata-vtweg.
      if sy-subrc <> 0.
        delete salesdata_table where vkorg eq salesdata-vkorg and vtweg eq salesdata-vtweg.
        data(lv_no_auth) = abap_true.
      endif.

      authority-check object 'V_VBAK_VKO'
       id 'VKORG' field salesdata-vkorg
       id 'VTWEG' field salesdata-vtweg
       id 'SPART' dummy
       id 'ACTVT' dummy.
      if sy-subrc <> 0.
        delete salesdata_table where vkorg eq salesdata-vkorg and vtweg eq salesdata-vtweg.
        lv_no_auth = abap_true.
      endif.
      clear salesdata.
    endloop.
    if lv_no_auth eq abap_true.
      message 'Authorisation missing. Check SU53.' type 'I' display like 'W'.
    endif.
  endmethod.

  method read_marm.
    check plantdata_table is not initial.
    refresh uomdata_table.
    select *
      from marm
      into table uomdata_table
      for all entries in plantdata_table
      where matnr = plantdata_table-matnr.

    check uomdata_table is not initial.
    sort uomdata_table ascending.
  endmethod.

  method read_mlan.
    check plantdata_table is not initial.
    refresh taxclass_table.
    select *
      from mlan
      into table taxclass_table
      for all entries in plantdata_table
      where matnr = plantdata_table-matnr.

    check taxclass_table is not initial.
    sort taxclass_table ascending.
  endmethod.

  method process_data.
    check material_table is not initial.
    refresh <lt_output>.
    do lines( plantdata_table ) times.
      append initial line to <lt_output> assigning field-symbol(<ls_output>).
      if <ls_output> is assigned.
        try.
            plantdata = plantdata_table[ sy-index ].
            move-corresponding plantdata to <ls_output>.
          catch cx_sy_itab_line_not_found.
        endtry.
        try.
            material = material_table[ matnr = plantdata-matnr ].
            move-corresponding material to <ls_output>.
          catch cx_sy_itab_line_not_found.
        endtry.
        try.
            description = description_table[ matnr = plantdata-matnr ].
            move-corresponding description to <ls_output>.
          catch cx_sy_itab_line_not_found.
        endtry.
        try.
            strg_loc = strg_loc_table[ matnr = plantdata-matnr werks = plantdata-werks ].
            move-corresponding strg_loc to <ls_output>.
          catch cx_sy_itab_line_not_found.
        endtry.
        try.
            valuationdata = valuationdata_table[ matnr = plantdata-matnr bwkey = plantdata-werks ].
            move-corresponding valuationdata to <ls_output>.
            loop at valuationdata_table into valuationdata where matnr = plantdata-matnr
                                                             and bwkey = plantdata-werks
                                                             and bwtar is not initial.
              if valuationdata-bwtar cs 'DOM'.
                assign component 'BWTAR_DOM' of structure <ls_output> to field-symbol(<lv_val_typ>).
                if sy-subrc = 0.
                  <lv_val_typ> = valuationdata-bwtar.
                endif.
              endif.
              unassign <lv_val_typ>.
              if valuationdata-bwtar cs 'IMP'.
                assign component 'BWTAR_IMP' of structure <ls_output> to <lv_val_typ>.
                if sy-subrc = 0.
                  <lv_val_typ> = valuationdata-bwtar.
                endif.
              endif.
              unassign <lv_val_typ>.
              clear valuationdata.
            endloop.
          catch cx_sy_itab_line_not_found.
        endtry.
        try.
            salesdata = salesdata_table[ matnr = plantdata-matnr ].
            move-corresponding salesdata to <ls_output>.
          catch cx_sy_itab_line_not_found.
        endtry.
        try.
            uomdata = uomdata_table[ matnr = plantdata-matnr ].
            move-corresponding uomdata to <ls_output>.
          catch cx_sy_itab_line_not_found.
        endtry.
        try.
            taxclass = taxclass_table[ matnr = plantdata-matnr ].
            move-corresponding taxclass to <ls_output>.
          catch cx_sy_itab_line_not_found.
        endtry.

        assign component 'VERPR' of structure <ls_output> to field-symbol(<lv_mvg_price>).
        if <lv_mvg_price> is assigned.
          clear <lv_mvg_price>.
        endif.

        assign component 'STPRS' of structure <ls_output> to field-symbol(<lv_std_price>).
        if <lv_std_price> is assigned.
          clear <lv_std_price>.
        endif.
      endif.

      clear: plantdata, material, description, strg_loc, valuationdata, salesdata, uomdata, taxclass.
    enddo.

    try.
        delete <lt_output> where ('MATNR NOT IN S_MATNR').
      catch cx_sy_itab_dyn_loop.
    endtry.
    try.
        delete <lt_output> where ('WERKS NOT IN S_WERKS').
      catch cx_sy_itab_dyn_loop.
    endtry.
    try.
        delete <lt_output> where ('LGORT NOT IN S_LGORT').
      catch cx_sy_itab_dyn_loop.
    endtry.
    try.
        delete <lt_output> where ('VKORG NOT IN S_VKORG').
      catch cx_sy_itab_dyn_loop.
    endtry.
    try.
        delete <lt_output> where ('VTWEG NOT IN S_VTWEG').
      catch cx_sy_itab_dyn_loop.
    endtry.
    try.
        delete <lt_output> where ('SPART NOT IN S_SPART').
      catch cx_sy_itab_dyn_loop.
    endtry.
    try.
        delete <lt_output> where ('BWKEY NOT IN S_BWKEY').
      catch cx_sy_itab_dyn_loop.
    endtry.
  endmethod.

  method get_alv.
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
          o_flow      type ref to cl_salv_form_layout_flow,
          col_tab     type salv_t_column_ref.

    field-symbols: <col> type salv_s_column_ref.

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

    if c_outtab is not initial.

      try.
          cl_salv_table=>factory(
            exporting
            list_display = if_salv_c_bool_sap=>false
            importing
            r_salv_table = o_table
            changing
            t_table      = c_outtab ).
        catch cx_salv_msg .
      endtry.

      check o_table is bound.
      e_alv = o_table.

      o_columns = o_table->get_columns( ).

      if o_columns is bound.
        try.
            free o_column.
            o_column ?= o_columns->get_column( exporting columnname = 'MATNR' ).
            o_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
            o_col_list ?= o_column.
            o_col_list->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( exporting columnname = 'WERKS' ).
            o_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
            o_col_list ?= o_column.
            o_col_list->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( exporting columnname = 'INDEX' ).
            o_column->set_technical( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( exporting columnname = 'SEL' ).
            o_column->set_technical( exporting value = if_salv_c_bool_sap=>true ).

*            free o_column.
*            o_column ?= o_columns->get_column( exporting columnname = 'MTART' ).
*            o_column->set_visible( exporting value = if_salv_c_bool_sap=>false ).
*
*            free o_column.
*            o_column ?= o_columns->get_column( exporting columnname = 'BWKEY' ).
*            o_column->set_visible( exporting value = if_salv_c_bool_sap=>false ).
*
*            free o_column.
*            o_column ?= o_columns->get_column( exporting columnname = 'BWTAR' ).
*            o_column->set_visible( exporting value = if_salv_c_bool_sap=>false ).
*
*            free o_column.
*            o_column ?= o_columns->get_column( exporting columnname = 'BKLAS' ).
*            o_column->set_visible( exporting value = if_salv_c_bool_sap=>false ).

            " Rev 01, Do not display 0.00 for quant, currency fields, display blank instead
            refresh col_tab.
            col_tab = o_columns->get( ).
            data: dtype type inttype.
            if col_tab is not initial.
              unassign <col>.
              loop at col_tab assigning <col>.
                clear dtype.
                dtype = <col>-r_column->get_ddic_inttype( ).
                if dtype eq 'P' or dtype eq 'F'.
                  <col>-r_column->set_zero( if_salv_c_bool_sap=>false ).
                endif.
              endloop.
            endif.

            " Alternate logic
            " loop at col_tab assigning <col>.
            " o_column ?= o_columns->get_column( exporting columnname = <col>-columnname ).
            " dtype = o_column->get_ddic_inttype( ).
            " if dtype 'P' or 'F'
            " o_column->set_zero( false )
            " endloop.

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
      describe table c_outtab lines lines.
      condense lines.

      " Build report header
      create object o_head_grid.
      if o_head_grid is bound.
        o_label = o_head_grid->create_label( exporting row = 1 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = i_header ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 2 column = 1 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = 'No. of rows' && ` ` && lines ).
        endif.

        o_table->set_top_of_list( exporting value = o_head_grid ).
        o_table->set_top_of_list_print( exporting value = o_head_grid ).
      endif.
    else.
      message 'No data found' type 'S' display like 'E'.
    endif.
  endmethod.

  method lvl2_out.
    data: lines(5) type c.  " No. of rows in output_table
    data: o_head_grid type ref to cl_salv_form_layout_grid,
          o_label     type ref to cl_salv_form_label,
          o_flow      type ref to cl_salv_form_layout_flow.
    data: o_table type ref to cl_salv_table.
    data: o_functions type ref to cl_salv_functions_list.

    check c_outtab[] is not initial.
    free: o_head_grid,
          o_label    ,
          o_flow     .
    free: o_table.
    free: o_functions.
    try.
        cl_salv_table=>factory(
          exporting
            list_display = if_salv_c_bool_sap=>false
          importing
            r_salv_table = o_table
          changing
            t_table      = c_outtab ).
      catch cx_salv_msg .
    endtry.

    check o_table is bound.

    data(o_columns) = o_table->get_columns( ).
    if o_columns is bound.
      o_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
    endif.

    o_functions = o_table->get_functions( ).

    if o_functions is bound.
      o_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
    endif.

    clear lines.
    describe table c_outtab lines lines.
    condense lines.

    " Build report header
    create object o_head_grid.
    o_label = o_head_grid->create_label( exporting row = 1 column = 1 ).
    if o_label is bound.
      o_label->set_text( exporting value = i_header ).
    endif.

    o_flow = o_head_grid->create_flow( exporting row = 2 column = 1 ).
    if  o_flow is bound.
      o_flow->create_text( exporting text = 'No. of rows' && ` ` && lines ).
    endif.

    o_table->set_top_of_list( exporting value = o_head_grid ).
    o_table->set_top_of_list_print( exporting value = o_head_grid ).

    o_table->display( ).
  endmethod.

  method display_output.
    data: head_text type string.
    data: alv type ref to cl_salv_table.
    data: events type ref to cl_salv_events_table.
    data: objectname type rs38m-programm.
    data: tpool type standard table of textpool.

    refresh tpool.
    clear objectname.

    objectname = sy-cprog.
    call function 'RS_TEXTPOOL_READ'
      exporting
        objectname           = objectname
        action               = space
      tables
        tpool                = tpool
      exceptions
        object_not_found     = 1
        permission_failure   = 2
        invalid_program_type = 3
        error_occured        = 4
        action_cancelled     = 5
        others               = 6.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    case abap_true.
      when p_rad1.
        read table tpool into data(wpool) with key key = 'R01'.
      when p_rad2.
        read table tpool into wpool with key key = 'R02'.
      when p_rad3.
        read table tpool into wpool with key key = 'R03'.
      when p_rad4.
        read table tpool into wpool with key key = 'R04'.
      when p_rad5.
        read table tpool into wpool with key key = 'R05'.
      when p_rad6.
        read table tpool into wpool with key key = 'R06'.
      when p_rad7.
        read table tpool into wpool with key key = 'R07'.
      when p_rad8.
        read table tpool into wpool with key key = 'R08'.
      when p_rad9.
        read table tpool into wpool with key key = 'R09'.
      when p_rad10.
        read table tpool into wpool with key key = 'R10'.
      when p_rad11.
        read table tpool into wpool with key key = 'R11'.
      when p_rad12.
        read table tpool into wpool with key key = 'R12'.
      when p_rad13.
        read table tpool into wpool with key key = 'R13'.
      when p_rad14.
        read table tpool into wpool with key key = 'R14'.
      when others.
    endcase.
    condense wpool-entry.

    clear head_text.
    head_text = |Material data for { wpool-entry } in creation/extension format|.

    free: alv, events.
    get_alv(
      exporting
        i_header = head_text
      importing
        e_alv = alv
      changing
        c_outtab = <lt_output> ).

    if alv is bound.
      events = alv->get_event( ).
      set handler on_line_click for events.

      call method alv->display.
    endif.
  endmethod.

  method on_line_click. " hotspot_click implementation
    case column.
      when 'MATNR'.
        read table <lt_output> assigning field-symbol(<ls_output>) index row.
        if sy-subrc eq 0.
          assign component 'MATNR' of structure <ls_output> to field-symbol(<lv_matnr>).
          if <lv_matnr> is assigned and <lv_matnr> is not initial.
            set parameter id 'MAT' field <lv_matnr>.
            call transaction 'MM03' and skip first screen.
          endif.
        endif.
      when 'WERKS'.
        field-symbols: <lt_lvl2> type standard table.
        read table <lt_output> assigning <ls_output> index row.
        if sy-subrc eq 0.
          call selection-screen 1001 starting at 10 10.
          if gv_1001_ucomm eq 'CRET'.
            data: head_text type string.
            data: matnr type mara-matnr.
            clear: head_text.
            assign component 'MATNR' of structure <ls_output> to <lv_matnr>.
            matnr = <lv_matnr>.
            shift matnr left deleting leading '0'.
            assign component 'WERKS' of structure <ls_output> to field-symbol(<lv_werks>).
            if <lv_matnr> is assigned and <lv_werks> is assigned.
              case abap_true.
                when p_stloc.
                  head_text = 'Storage location details for material:' && ` ` && matnr && ` ` && 'Plant:' && ` ` && <lv_werks>.
                  select matnr,
                         werks,
                         lgort
                    from mard
                    into table @data(lt_strg_loc)
                    where matnr = @<lv_matnr>
                    and   werks = @<lv_werks>.

                  assign lt_strg_loc to <lt_lvl2>.
                when p_sales.
                  head_text = 'Sales area details for material:' && ` ` && matnr.
                  select a~matnr,
                         b~vkorg,
                         b~vtweg,
                         a~spart
                    from mvke as b
                    inner join mara as a
                    on a~matnr = b~matnr
                    into table @data(lt_sales_area)
                    where b~matnr = @<lv_matnr>.

                  assign lt_sales_area to <lt_lvl2>.
                when p_valtn.
                  head_text = 'Valuation area details for material:' && ` ` && matnr && ` ` && 'Plant:' && ` ` && <lv_werks>.
                  select matnr,
                         bwkey,
                         bwtar,
                         bklas
                    from mbew
                    into table @data(lt_valuation)
                    where matnr = @<lv_matnr>
                    and   bwkey = @<lv_werks>.

                  assign lt_valuation to <lt_lvl2>.
                when others.
              endcase.
              if <lt_lvl2> is assigned and <lt_lvl2> is not initial.
                lvl2_out(
                  exporting
                    i_header = head_text
                  changing
                    c_outtab = <lt_lvl2> ).
              endif.
            endif.
          endif.
        endif.
      when others.
        message 'Choose a valid function...' type 'S'.
    endcase.
  endmethod.
endclass.

*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_fg_create
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_fg_create implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZFGM' )
                       ( sign = 'I' option = 'EQ' low = 'ZLFG' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_fg_plt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_fg_plt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZFGM' )
                       ( sign = 'I' option = 'EQ' low = 'ZLFG' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.

*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_fg_dpt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_fg_dpt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZFGM' )
                       ( sign = 'I' option = 'EQ' low = 'ZLFG' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_trd_create
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_trd_create implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZTRD' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_trd_plt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_trd_plt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZTRD' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_trd_dpt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_trd_dpt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZTRD' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_raw_create
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_raw_create implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZRAW' )
                       ( sign = 'I' option = 'EQ' low = 'ZPKG' )
                       ( sign = 'I' option = 'EQ' low = 'ZPKU' )
                       ( sign = 'I' option = 'EQ' low = 'ZCON' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_raw_plt_dpt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_raw_plt_dpt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZRAW' )
                       ( sign = 'I' option = 'EQ' low = 'ZPKG' )
                       ( sign = 'I' option = 'EQ' low = 'ZPKU' )
                       ( sign = 'I' option = 'EQ' low = 'ZCON' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_sfg_create
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_sfg_create implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZSFG' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_sfg_plt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_sfg_plt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZSFG' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_mco_create
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_mco_create implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZMCO' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_mco_plt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_mco_plt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'ZMCO' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_spr_create
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_spr_create implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'YSPR' )
                       ( sign = 'I' option = 'EQ' low = 'ZSPR' )
                       ( sign = 'I' option = 'EQ' low = 'YSPI' )
                       ( sign = 'I' option = 'EQ' low = 'ZEU2' ) ).

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.
*&---------------------------------------------------------------------*
*& Class (Implementation) lcl_spr_plt_ext
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
class lcl_spr_plt_ext implementation.
  method constructor.
    super->constructor( ).

    refresh r_mtart.
    r_mtart = value #( ( sign = 'I' option = 'EQ' low = 'YSPR' )
                       ( sign = 'I' option = 'EQ' low = 'ZSPR' )
                       ( sign = 'I' option = 'EQ' low = 'YSPI' )
                       ( sign = 'I' option = 'EQ' low = 'ZEU2' ) )..

    execute(
      changing
        c_tab = output_table ).
  endmethod.
endclass.

* ---- main class implementation ---- *
class lcl_main implementation.
  method main.
* 1. class name = name of class based on selected radio button
    clear: cl_name. " type seoclsname
    free: lo.       " type ref to object(generic object ref)
    case abap_true.
      when p_rad1.
        cl_name = 'LCL_FG_CREATE'.
      when p_rad2.
        cl_name = 'LCL_FG_PLT_EXT'.
      when p_rad3.
        cl_name = 'LCL_FG_DPT_EXT'.
      when p_rad4.
        cl_name = 'LCL_TRD_CREATE'.
      when p_rad5.
        cl_name = 'LCL_TRD_PLT_EXT'.
      when p_rad6.
        cl_name = 'LCL_TRD_DPT_EXT'.
      when p_rad7.
        cl_name = 'LCL_RAW_CREATE'.
      when p_rad8.
        cl_name = 'LCL_RAW_PLT_DPT_EXT'.
      when p_rad9.
        cl_name = 'LCL_SFG_CREATE'.
      when p_rad10.
        cl_name = 'LCL_SFG_PLT_EXT'.
      when p_rad11.
        cl_name = 'LCL_MCO_CREATE'.
      when p_rad12.
        cl_name = 'LCL_MCO_PLT_EXT'.
      when p_rad13.
        cl_name = 'LCL_SPR_CREATE'.
      when p_rad14.
        cl_name = 'LCL_SPR_PLT_EXT'.
    endcase.
    if cl_name is not initial.
      try.
          create object lo type (cl_name). " Class name resolved @ runtime,
          " constructor of respective class is called implicitly
*        CATCH cx_sy_create_object_error.
*          RETURN.
        catch cx_root into lo_cx.
          message lo_cx->get_text( ) type 'S' display like 'E'.
          return.
      endtry.
    endif.
  endmethod.
endclass.
* ---- end main class implementation ---- *
