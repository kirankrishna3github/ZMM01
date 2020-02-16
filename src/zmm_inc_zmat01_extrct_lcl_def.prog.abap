*&---------------------------------------------------------------------*
*&  Include           ZMM_INC_MAT_EXT_DATA_LCL_DEF
*&---------------------------------------------------------------------*
class lcl_alv_helper definition.
  public section.
    data: r_mtart type fip_t_mtart_range.

    methods:
      execute
        changing
          c_tab      type standard table.

  private section.
    data: material            type mara,
          material_table      type standard table of mara,

          description         type makt,
          description_table   type standard table of makt,

          plantdata           type marc,
          plantdata_table     type standard table of marc,

          strg_loc            type mard,
          strg_loc_table      type standard table of mard,

          valuationdata       type mbew,
          valuationdata_table type standard table of mbew,

          salesdata           type mvke,
          salesdata_table     type standard table of mvke,

          uomdata             type marm,
          uomdata_table       type standard table of marm,

          taxclass            type mlan,
          taxclass_table      type standard table of mlan.

    methods:
      fetch_data,

      read_mara,

      read_makt,

      read_marc,

      read_mard,

      read_mbew,

      read_mvke,

      read_marm,

      read_mlan,

      get_alv
        importing i_header type string
        exporting e_alv    type ref to cl_salv_table
        changing  c_outtab type standard table,

      lvl2_out
        importing i_header type string
        changing  c_outtab type standard table,

      process_data,   " gather queried data in a single table for output

      display_output, " display that table in alv format

      on_line_click for event link_click of cl_salv_events_table
        importing
            row
            column. " event handler method, imports row and column of clicked value
endclass.

* ---- Class definitions ---- *
class lcl_fg_create definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_fg_create_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_fg_plt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_fg_plt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_fg_dpt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_fg_dpt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_trd_create definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_trd_create_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_trd_plt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_trd_plt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_trd_dpt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_trd_dpt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_raw_create definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_raw_create_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_raw_plt_dpt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_raw_plt_dpt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_sfg_create definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_sfg_create_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_sfg_plt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_sfg_plt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_mco_create definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_mco_create_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_mco_plt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_mco_plt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_spr_create definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_spr_create_data.
    data: end of output,
    output_table like standard table of output.
endclass.

class lcl_spr_plt_ext definition inheriting from lcl_alv_helper.

  public section.
    methods: constructor.

  protected section.
    " protected content

  private section.
    data: begin of output.
        include type zif_mm_mat01=>ty_spr_plt_ext_data.
    data: end of output,
    output_table like standard table of output.
endclass.

* ---- begin main class definition, main method ---- *
class lcl_main definition.
  public section.
* Dynamic object creation OF dynamic CLASS TYPE
    class-data:
      cl_name type seoclsname,    " holds class/reference type eg: 'LCL_FGM_ALV'
      lo      type ref to object. " object of any reference type,
    " we will make it refer to class in cl_name @ runtime

    " static main method
    class-methods:
      main.

  protected section.

  private section.

endclass.
* ---- end main class definition ---- *
