*&---------------------------------------------------------------------*
*& Report ZMM_PRG_MASS_MAT_BOM_CREATE
*&---------------------------------------------------------------------*
*& Transaction            : ZMM091
*& Creation Date          : 28.01.2019
*& Author                 : 6010859 - SaurabhK
*& Functional             : VG
*& Requested/Approved By  : VG
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IHDK900372
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&---------------------------------------------------------------------*
*& 1. Input format - Excel file - Format downloadable from selection screen
*& 2. Validation on plant existence, material/component existence and material/component existence in given plant + authority check
*& 3. Mass Material BOM creation using CSAP_MAT_BOM_CREATE
*& 4. Detailed log is displayed
*&---------------------------------------------------------------------*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Monday, February 11, 2019 00:37:49
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK900568
*& Rev. Description           : MM: S_K: ZMM091: Improve error log: 10.2.19
*&---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Wednesday, February 27, 2019 10:50:38
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK900747
*& Rev. Description           : MM: S_K: ZMM091: Fixes for header Qty UOM: 27.2.19
*&---------------------------------------------------------------------*
*& Revision #                 : 03
*& Rev. Date                  : Friday, December 13, 2019 17:38:09
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK904247 | Add authority check + resolve CX_SY_CONVERSION_NO_NUMBER runtime error
*& Rev. Description           : MM: S_K: ZMM091: BOM add auth check: 13.12.19
*&---------------------------------------------------------------------*
report zmm_prg_mass_mat_bom_create.

* ---- global data(if any) ---- *

* ---- selection screen ---- *
selection-screen begin of block fil with frame title text-fil.
parameters: p_file type rlgrap-filename.
selection-screen end of block fil.

selection-screen begin of block dwn with frame title text-dwn.
selection-screen: begin of line,
  pushbutton 1(20) dwn_btn user-command dwn,
  comment 25(70) text-wrn,
end of line.
selection-screen end of block dwn.

* ---- local class definition ---- *
class lcl_application definition final.
  public section.
    class-methods:
      file_open_dialog
        importing
          field type dynpread-fieldname
        changing
          file  type rlgrap-filename,

      dwn_file_format.

    methods: process.

  protected section.

  private section.
    types: excelcell type c length 50.
    data: begin of gs_excel,
            " header start
            index type excelcell,
            matnr type excelcell,
            werks type excelcell,
            bmeng type excelcell,
            " item start
            idnrk type excelcell,
            menge type excelcell,
            meins type excelcell,
          end of gs_excel,
          gt_excel like standard table of gs_excel,

          begin of gs_data,
            " header start
            index type i,
            matnr type csap_mbom-matnr,
            werks type csap_mbom-werks,
            bmeng type stko_api01-base_quan,

            " item start
            idnrk type stpo_api01-component,
            menge type stpo_api01-comp_qty,
            meins type stpo_api01-comp_unit,

            " validation check flag
            error type bapi_flag,
          end of gs_data,
          gt_data like standard table of gs_data,

          begin of gs_log,
            index    type i,
            matnr    type csap_mbom-matnr,
            werks    type csap_mbom-werks,
            maktx    type makt-maktx,
            stlnr    type stko_api02-bom_no,
            procstat type icon_d,
            msg      type bapi_msg,
          end of gs_log,
          gt_log like standard table of gs_log,

          begin of gs_index_log,
            index type i,
            msg   type bapi_msg,
          end of gs_index_log,
          gt_index_log like standard table of gs_index_log with non-unique sorted key index components index,

          gv_run_time  type i.

    methods:
      file_to_itab, convert_excel_to_bapi_format, validation, create_bom_mass, display_log,
      hotspot_click for event link_click of cl_salv_events_table
        importing
            row
            column.
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.

  private section.
endclass.

* ---- local class implementation ---- *
class lcl_application implementation.
  method file_open_dialog.
    data field_name    type dynpread-fieldname.
    data file_name     type ibipparms-path.

    check field is not initial.
    clear: file_name.

    call function 'F4_FILENAME'
      exporting
        field_name = field
      importing
        file_name  = file_name.

    if file_name is not initial.
      file = file_name.
    endif.
  endmethod.

  method dwn_file_format.
    call function 'CALL_BROWSER'
      exporting
        url                    = 'https://goo.gl/oyt42H'     " File download url, google drive link
        window_name            = 'Download ZMM091 File Format'
        new_window             = 'X'
      exceptions
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        others                 = 6.
    if sy-subrc <> 0.
* Implement suitable error handling here
    else.
      message 'Opening your default web browser...'  type 'S'.
    endif.
  endmethod.

  method process.
    clear gv_run_time.
    get run time field data(begin_processing).
    file_to_itab( ).

    if gt_excel is not initial.
      call method zcl_helper=>check_file_format( changing ctab = gt_excel exceptions file_format_altered = 1 ).
      if sy-subrc <> 0.
        return.
      endif.
      convert_excel_to_bapi_format( ).
      if gt_data is not initial.
        validation( ).
        if gt_data is not initial.
          create_bom_mass( ).
        endif.
        get run time field data(end_processing).
        gv_run_time = end_processing - begin_processing.
        if gt_log is not initial.
          sort gt_log by index ascending.
          display_log( ).
        else.
          message 'No log generated' type 'S' display like 'E'.
        endif.
      else.
        message 'Error in conversion' type 'S' display like 'E'.
        return.
      endif.
    else.
      message 'No data read' type 'S' display like 'E'.
      return.
    endif.
  endmethod.

  method file_to_itab.
    data i_tab_raw_data type truxs_t_text_data.
    refresh gt_data.
    call function 'TEXT_CONVERT_XLS_TO_SAP'
      exporting
        i_field_seperator    = abap_true
        i_line_header        = abap_true
        i_tab_raw_data       = i_tab_raw_data
        i_filename           = p_file
      tables
        i_tab_converted_data = gt_excel
      exceptions
        conversion_failed    = 1
        others               = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
      message id sy-msgid type 'S' number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    endif.
  endmethod.

  method convert_excel_to_bapi_format.
    check gt_excel is not initial.
    loop at gt_excel assigning field-symbol(<fs_excel>).
      " condense and dynamic conversion exits after move corresponding
      append initial line to gt_data assigning field-symbol(<fs_data>).
      zcl_helper=>format_excel_to_bapi( changing is_excel = <fs_excel> is_data = <fs_data> ).
      try.
          <fs_data>-menge = round( val = <fs_data>-menge dec = 3 ).
          condense <fs_data>-menge.
        catch cx_sy_conversion_no_number.
      endtry.
    endloop.
  endmethod.

  method validation.
    check gt_data is not initial.
    include icons.

    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = 'Validating data...'.

    select werks
      from t001w
      into table @data(lt_plt)
      for all entries in @gt_data
      where werks = @gt_data-werks.

    select distinct matnr, mtart, spart
     from mara
     into table @data(lt_mat)
     for all entries in @gt_data
     where ( matnr = @gt_data-matnr or matnr = @gt_data-idnrk ).

    select distinct matnr, werks
      from marc
      into table @data(lt_mat_plt)
      for all entries in @gt_data
      where ( matnr = @gt_data-matnr or matnr = @gt_data-idnrk ).

    select distinct matnr, maktx    " IHDK900568
      from makt
      into table @data(lt_mat_desc)
      for all entries in @gt_data
      where matnr = @gt_data-matnr
      and spras   = @sy-langu.

    sort gt_data by index matnr werks bmeng ascending.
    clear gs_data.
    loop at gt_data into gs_data.
      data(lv_index) = sy-tabix.

      clear gs_index_log.
      gs_index_log-index = gs_data-index.
*--------------------------------------------------------------------*
      " check if plant exists
      read table lt_plt into data(ls_plt) with key werks = gs_data-werks.
      if sy-subrc <> 0.
        data(lv_error) = abap_true.
        gs_index_log-msg = condense( |Plant { gs_data-werks } does not exist.| ).
        append gs_index_log to gt_index_log.
      else.
        " authority check
        " plant
        authority-check object 'M_MATE_WRK'
          id 'ACTVT' dummy
          id 'WERKS' field gs_data-werks.
        if sy-subrc <> 0.
          lv_error = abap_true.
          gs_index_log-msg = condense( |No authority for plant { gs_data-werks }| ).
          append gs_index_log to gt_index_log.
        endif.

      endif.
*--------------------------------------------------------------------*
      " check if material exists
      read table lt_mat into data(ls_mat) with key matnr = gs_data-matnr.
      if sy-subrc <> 0.
        lv_error = abap_true.
        gs_index_log-msg = condense( |Material { gs_data-matnr alpha = out } does not exist.| ).
        append gs_index_log to gt_index_log.
      else.
        " authority check
        " material type
        authority-check object 'M_MATE_MAT'
          id 'ACTVT' dummy
          id 'BEGRU' field ls_mat-mtart.
        if sy-subrc <> 0.
          lv_error = abap_true.
          gs_index_log-msg = condense( |No authority for material type { ls_mat-mtart } / { ls_mat-matnr }| ).
          append gs_index_log to gt_index_log.
        endif.

        " division
        authority-check object 'ZMM_SPART'
          id 'ACTVT' dummy
          id 'SPART' field ls_mat-spart.
        if sy-subrc <> 0.
          lv_error = abap_true.
          gs_index_log-msg = condense( |No authority for materials of division { ls_mat-spart } / { ls_mat-matnr }| ).
          append gs_index_log to gt_index_log.
        endif.

      endif.
*--------------------------------------------------------------------*
      clear ls_mat.
      " Check if material exists in plant
      read table lt_mat_plt into data(ls_mat_plt) with key matnr = gs_data-matnr werks = gs_data-werks.
      if sy-subrc <> 0.
        lv_error = abap_true.
        gs_index_log-msg = condense( |Material { gs_data-matnr alpha = out } does not exist in plant { gs_data-werks }.| ).
        append gs_index_log to gt_index_log.
      endif.
*--------------------------------------------------------------------*
      clear ls_mat.
      " check if component exists
      read table lt_mat into ls_mat with key matnr = gs_data-idnrk.
      if sy-subrc <> 0.
        lv_error = abap_true.
        gs_index_log-msg = condense( |Component { gs_data-idnrk alpha = out } does not exist.| ).
        append gs_index_log to gt_index_log.
      else.
        " authority check
        " material type
        authority-check object 'M_MATE_MAT'
          id 'ACTVT' dummy
          id 'BEGRU' field ls_mat-mtart.
        if sy-subrc <> 0.
          lv_error = abap_true.
          gs_index_log-msg = condense( |No authority for component type { ls_mat-mtart } / { ls_mat-matnr }| ).
          append gs_index_log to gt_index_log.
        endif.

        " division
        authority-check object 'ZMM_SPART'
          id 'ACTVT' dummy
          id 'SPART' field ls_mat-spart.
        if sy-subrc <> 0.
          lv_error = abap_true.
          gs_index_log-msg = condense( |No authority for component of division { ls_mat-spart } / { ls_mat-matnr }| ).
          append gs_index_log to gt_index_log.
        endif.
      endif.
*--------------------------------------------------------------------*
      clear ls_mat_plt.
      " Check if component exists in plant
      read table lt_mat_plt into ls_mat_plt with key matnr = gs_data-idnrk werks = gs_data-werks.
      if sy-subrc <> 0.
        lv_error = abap_true.
        gs_index_log-msg = condense( |Component { gs_data-idnrk alpha = out } does not exist in plant { gs_data-werks }.| ).
        append gs_index_log to gt_index_log.
      endif.
*--------------------------------------------------------------------*
      if lv_error eq abap_true.
        gs_data-error = abap_true.
        modify gt_data from gs_data index lv_index transporting error.
      endif.
      clear: gs_data, lv_error, lv_index.
    endloop.

    if gt_index_log is not initial.
      sort gt_index_log by index msg ascending.
      delete adjacent duplicates from gt_index_log comparing index msg.
    endif.

    sort gt_data by index matnr werks bmeng ascending.
    clear gs_data.
    loop at gt_data into gs_data where error eq abap_true.
      clear: gs_log.
      move-corresponding gs_data to gs_log.
      read table lt_mat_desc into data(ls_mat_desc) with key matnr = gs_data-matnr. " IHDK900568
      if sy-subrc = 0.
        gs_log-maktx = ls_mat_desc-maktx.
      endif.
      gs_log-procstat = icon_red_light.
      gs_log-msg = 'Some errors occured. Click for details'.
      append gs_log to gt_log.

      delete gt_data where index = gs_data-index.
      clear gs_data.
    endloop.

  endmethod.

  method create_bom_mass.
    check gt_data is not initial.
    sort gt_data by index idnrk ascending.
    data(lt_header) = gt_data[].
    delete adjacent duplicates from lt_header comparing index.
    data(lt_item) = gt_data[].
    data: lt_mast   type standard table of mast_api02,
          ls_stko   type stko_api01,
          lv_bom_no type stko_api02-bom_no,
          lt_stpo   type standard table of stpo_api01.
    constants: lc_bom_usage   type csap_mbom-stlan value '1',
               lc_commit_true type capiflag-comm_wait value abap_true,
               lc_item_cat    type stpo_api01-item_categ value 'L'.

    include: icons.

    loop at lt_header into data(ls_header).
      clear gs_log.
      move-corresponding ls_header to gs_log. " log preparation

      select single maktx from makt into @data(lv_mat_desc) where matnr = @ls_header-matnr and spras eq @sy-langu.
      gs_log-maktx = lv_mat_desc.

      call function 'CALO_INIT_API'
        exporting
          external_log_no          = conv balhdri-extnumber( ls_header-matnr && '_' && ls_header-werks )
        exceptions
          log_object_not_found     = 1
          log_sub_object_not_found = 2
          other_error              = 3
          others                   = 4.

      refresh lt_mast.
      call function 'CSAP_MAT_BOM_SELECT'
        exporting
          material      = ls_header-matnr
          plant         = ls_header-werks
          bom_usage     = lc_bom_usage
        tables
          t_mast        = lt_mast
        exceptions
          error         = 1
          error_message = 2
          others        = 3.
      if sy-subrc <> 0 or lt_mast is initial.
* Implement suitable error handling here
        clear: ls_stko, lv_bom_no.
        ls_stko-base_quan = ls_header-bmeng.
*        ls_stko-base_unit = ls_header-meins. " IHDK900747
        ls_stko-bom_text  = lv_mat_desc.

        refresh lt_stpo.
        data: lv_item_no type stpo_api01-item_no.
        clear lv_item_no.
        loop at lt_item into data(ls_item) where index = ls_header-index.
          append initial line to lt_stpo assigning field-symbol(<ls_stpo>).
          if sy-subrc = 0 and <ls_stpo> is assigned.
            <ls_stpo>-item_categ = lc_item_cat.
            lv_item_no = lv_item_no + 10.
            <ls_stpo>-item_no = |{ lv_item_no alpha = in }|.
            <ls_stpo>-component = ls_item-idnrk.
            <ls_stpo>-comp_qty = ls_item-menge.
            <ls_stpo>-comp_unit = ls_item-meins.
          endif.
          unassign <ls_stpo>.
          clear ls_item.
        endloop.

        call function 'CSAP_MAT_BOM_CREATE'
          exporting
            material           = ls_header-matnr
            plant              = ls_header-werks
            bom_usage          = lc_bom_usage
            valid_from         = conv csap_mbom-datuv( |{ sy-datum date = user }| )
            i_stko             = ls_stko
            fl_commit_and_wait = lc_commit_true
          importing
            bom_no             = lv_bom_no
          tables
            t_stpo             = lt_stpo
          exceptions
            error              = 1
            others             = 2.
        if sy-subrc <> 0.
* Implement suitable error handling here
          gs_log-procstat = icon_red_light.
          message id sy-msgid type 'S' number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 into gs_log-msg.
          gs_log-msg = condense( |{ gs_log-msg } - BOM creation failed. Click for details( SLG1 ).| ).
        elseif lv_bom_no is not initial.  " success
          gs_log-stlnr = lv_bom_no.
          gs_log-procstat = icon_green_light.
          gs_log-msg = condense( |BOM { lv_bom_no alpha = out } | &&
                       |created for material { ls_header-matnr alpha = out } | &&
                       |in plant { ls_header-werks }.| ).
        endif.
      elseif lt_mast is not initial.
        try.
            gs_log-stlnr = lt_mast[ 1 ]-bom_no.
            gs_log-procstat = icon_red_light.
            gs_log-msg = condense( |BOM { lt_mast[ 1 ]-bom_no alpha = out } | &&
                         |already exists for material { ls_header-matnr alpha = out } | &&
                         |in plant { ls_header-werks }.| ).
          catch cx_sy_itab_line_not_found.
        endtry.
      endif.

      append gs_log to gt_log.
      clear: ls_header, lv_mat_desc.
    endloop.
  endmethod.

  method display_log.
    check gt_log is not initial.
    " display log in alv
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = data(lo_table)
          changing
            t_table      = gt_log ).

        check lo_table is bound.

        data(lo_columns) = lo_table->get_columns( ).

        if lo_columns is bound.
          try.
              data(lo_column) = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'PROCSTAT' ) ).
              if lo_column is bound.
                lo_column->set_long_text( exporting value = 'Processing Status' ).
                lo_column->set_medium_text( exporting value = 'Proc. Status' ).
                lo_column->set_short_text( exporting value = 'ProcStat' ).
                lo_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
              endif.

              free lo_column.
              lo_column = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'MSG' ) ).
              if lo_column is bound.
                lo_column->set_long_text( exporting value = 'Log Message' ).
                lo_column->set_medium_text( exporting value = 'Log Msg' ).
                lo_column->set_short_text( exporting value = 'LogMsg' ).
                lo_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
              endif.

              lo_column = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'INDEX' ) ).
              if lo_column is bound.
                lo_column->set_long_text( exporting value = 'Index' ).
                lo_column->set_medium_text( exporting value = 'Index' ).
                lo_column->set_short_text( exporting value = 'Index' ).
              endif.

              lo_column = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'STLNR' ) ).
              if lo_column is bound.
                lo_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
              endif.

              lo_column = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'ERROR' ) ).
              if lo_column is bound.
                lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).
              endif.
*          data(lt_col) = lo_columns->get( ).
*          if lt_col is not initial.
*            loop at lt_col into data(ls_col).
*              translate ls_col-columnname using '_ '.
*              ls_col-r_column->set_long_text( exporting value = conv scrtext_l( ls_col-columnname ) ).
*              ls_col-r_column->set_medium_text( exporting value = conv scrtext_m( ls_col-columnname ) ).
*              ls_col-r_column->set_short_text( exporting value = conv scrtext_s( ls_col-columnname ) ).
*
*              clear ls_col.
*            endloop.
*          endif.
            catch cx_salv_not_found.
          endtry.
          lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_functions) = lo_table->get_functions( ).
        if lo_functions is bound.
          lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_layout) = lo_table->get_layout( ).

        data(lo_key) = value salv_s_layout_key( report = sy-repid ).

        if lo_layout is bound.
          lo_layout->set_key( exporting value = lo_key ).

          lo_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

          lo_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_display) = lo_table->get_display_settings( ).

        if lo_display is bound.
          lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
          lo_display->set_list_header( exporting value = conv lvc_title( 'Mass Material BOM Creation Log' ) ).
        endif.

        data(lo_head_grid) = new cl_salv_form_layout_grid( ).
        if lo_head_grid is bound.
          data(lo_label) = lo_head_grid->create_label( exporting row = 1 column = 1 ).
          if lo_label is bound.
            lo_label->set_text( exporting value = 'Mass Create Material BOM' ).
          endif.

          lo_label = lo_head_grid->create_label( exporting row = 2 column = 1 ).
          if lo_label is bound.
            lo_label->set_text( exporting value = 'No. of BOM''s processed:' ).
          endif.

          data(lo_flow) = lo_head_grid->create_flow( exporting row = 2 column = 2 ).
          if lo_flow is bound.
            lo_flow->create_text( exporting text = | { lines( gt_log ) }| ).
          endif.

          lo_label = lo_head_grid->create_label( exporting row = 3 column = 1 ).
          if lo_label is bound.
            lo_label->set_text( exporting value = 'Processing time(in minutes):' ).
          endif.

          lo_flow = lo_head_grid->create_flow( exporting row = 3 column = 2 ).
          if lo_flow is bound.
            lo_flow->create_text( exporting text = |{ round( val = gv_run_time * ( '1.67' * ( 10 ** -8 ) ) dec = 2 ) }| ).
          endif.

          lo_table->set_top_of_list( exporting value = lo_head_grid ).
          lo_table->set_top_of_list_print( exporting value = lo_head_grid ).
        endif.

        data(lo_event) = lo_table->get_event( ).
        if lo_event is bound.
          set handler hotspot_click for lo_event.
        endif.

        lo_table->display( ).
      catch cx_salv_msg.
    endtry.
  endmethod.

  method hotspot_click.
    clear gs_log.
    read table gt_log into gs_log index row.
    if sy-subrc = 0.
      case column.
        when 'PROCSTAT' or 'MSG'.
          if gs_log-stlnr is not initial. " already exists or newly created; redirect to cs03
            set parameter id 'MAT' field gs_log-matnr.
            set parameter id 'WRK' field gs_log-werks.
            set parameter id 'CSV' field '1'.
            call transaction 'CS03' with authority-check and skip first screen.
          else. " display slg1
            if gs_log-msg cs 'SLG1'.
              constants: lc_object    type balhdr-object value 'CAPI',
                         lc_subobject type balhdr-subobject value 'CAPI_LOG'.
              data: lv_external_number type balhdr-extnumber.

              clear lv_external_number.
              lv_external_number =  conv balhdr-extnumber( gs_log-matnr && '_' && gs_log-werks ).

              data: lv_logs type i.
              clear lv_logs.
              call function 'APPL_LOG_READ_DB'
                exporting
                  object           = lc_object
                  subobject        = lc_subobject
                  external_number  = lv_external_number
                  program_name     = conv balhdr-alprog( sy-repid )
                  transaction_code = conv balhdr-altcode( sy-tcode )
                  user_id          = conv balhdr-aluser( sy-uname )
                importing
                  number_of_logs   = lv_logs.

              check lv_logs ne 0.
              call function 'APPL_LOG_DISPLAY'
                exporting
                  object                    = lc_object
                  subobject                 = lc_subobject
                  external_number           = lv_external_number
                  suppress_selection_dialog = abap_true
                exceptions
                  no_authority              = 1
                  others                    = 2.
              if sy-subrc <> 0.
* Implement suitable error handling here
              endif.
            else. " local log
              data(index_log) = filter #( gt_index_log using key index where index = gs_log-index ).
              sort index_log.
              try.
                  cl_salv_table=>factory(
                  exporting
                    list_display = abap_false
                  importing
                    r_salv_table = data(lo_table)
                  changing
                    t_table      = index_log ).
                catch cx_salv_msg.
              endtry.

              if lo_table is bound.
                try.
                    data(lo_columns) = lo_table->get_columns( ).
                    if lo_columns is bound.
                      data(lo_column) = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'INDEX' ) ).
                      if lo_column is bound.
                        lo_column->set_visible( value  = if_salv_c_bool_sap=>false ).
                      endif.
                      lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
                    endif.
                  catch cx_salv_not_found.
                endtry.

                data(lo_functions) = lo_table->get_functions( ).

                if lo_functions is bound.
                  lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
                endif.

                data(lo_display) = lo_table->get_display_settings( ).

                if lo_display is bound.
                  lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
                  lo_display->set_list_header( exporting value = |Processing log for index { gs_log-index }| ).
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
          endif.
        when 'STLNR'.
          if gs_log-stlnr is not initial. " already exists or newly created; redirect to cs03
            set parameter id 'MAT' field gs_log-matnr.
            set parameter id 'WRK' field gs_log-werks.
            set parameter id 'CSV' field '1'.
            call transaction 'CS03' with authority-check and skip first screen.
          endif.
        when others.
      endcase.
    endif.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    new lcl_application( )->process( ).
  endmethod.
endclass.

* ---- program events ---- *

* ---- load of program ---- *

* ---- initialisation ---- *

* ---- selection screen events ---- *
at selection-screen output.
  dwn_btn = '@49@ Download'.

at selection-screen on value-request for p_file.
  lcl_application=>file_open_dialog( exporting field = 'P_FILE' changing file = p_file ).

at selection-screen.
  if sy-ucomm eq 'DWN'.
    lcl_application=>dwn_file_format( ).
  endif.

  if sy-ucomm eq 'ONLI'.
    if p_file is initial.
      set cursor field 'P_FILE'.
      message s055(00) display like 'E'.
      stop. " this is me being lazy; should have used a method with excecption instead
    endif.
  endif.

* ---- start of selection ---- *
start-of-selection.
  lcl_main=>start( ).
