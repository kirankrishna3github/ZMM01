*&---------------------------------------------------------------------*
*& Report ZMM_PRG_MAINTAIN_CLASSIF_CHARS
*&---------------------------------------------------------------------*
*& In future, if this program is to be used for batch based classification,
*& just add batch to the input file format after descr column and make changes at required places
*&---------------------------------------------------------------------*
report zmm_prg_maintain_classif_chars.

*--------------------------------------------------------------------*
* Global data
*--------------------------------------------------------------------*
tables: sscrfields.
field-symbols: <gt_table> type standard table,
               <gt_log>   type standard table.
*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
selection-screen begin of block sel with frame title text-sel.
parameters: p_clsnum type klah-class obligatory default 'ZIIS_MAT_CLASS' modif id sel,
            p_clstyp type klah-klart obligatory default '001' modif id sel.

selection-screen skip.
parameters: p_test as checkbox default 'X'.
selection-screen end of block sel.

selection-screen begin of block dwn with frame title text-dwn.
selection-screen:
  begin of line,
    pushbutton 2(15) text-001 user-command dwn,
  end of line.
selection-screen end of block dwn.

selection-screen begin of block fil with frame title text-fil.
selection-screen:
  begin of line,
    comment 1(79) text-002,
  end of line.
parameters: p_file type string.
selection-screen end of block fil.
*--------------------------------------------------------------------*
* local class definitions
*--------------------------------------------------------------------*
class lcl_app definition.
  public section.
    class-data: gt_classcharacteristics type standard table of bapi_char,
                gs_objecttable          type bapi1003_key-objecttable.

    class-methods:
      screen_modif, f4_clsnum, f4_file, handle_sel_ucomm, validate_input,
      set_object_table, build_table_from_char, prep_dwn_file_format,
      char_to_fltp
        importing
          value(iv) type any
        exporting
          value(ev) type any.
    methods: process.

  protected section.
    " placeholder

  private section.
    data: begin of gs_index_log,
            matnr type mara-matnr,
            type  type bapiret2-type,
            msg   type bapi_msg,
          end of gs_index_log,
          gt_index_log like standard table of gs_index_log with non-unique sorted key matnr components matnr.

    methods: read_file_data, material_validation, maintain_classif_chars, create_log_table, display_log,
      hotspot_click for event link_click of cl_salv_events_table
        importing
            row
            column.
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.
    " placeholder

  private section.
    " placeholder
endclass.
*--------------------------------------------------------------------*
* local class implementation
*--------------------------------------------------------------------*
class lcl_app implementation.
  method screen_modif.  " IHDK901242
    loop at screen.
      if screen-group1 eq 'SEL'.
        screen-input = 0.
        modify screen.
      endif.
    endloop.
  endmethod.

  method f4_clsnum.
    select a~klart, a~class, b~kschl
      from klah as a
      join swor as b
      on a~clint eq b~clint
      into table @data(lt_class)
      where a~class like 'Z%'
      and   b~spras eq @sy-langu.

    data lt_return type standard table of ddshretval.

    refresh lt_return.
    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'CLASS'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'P_CLSNUM'
        window_title    = 'Select Class'
        value_org       = 'S'
      tables
        value_tab       = lt_class
        return_tab      = lt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    data: lt_dynpfields type standard table of dynpread.
    refresh lt_dynpfields.
    try.
        lt_dynpfields = value #( ( fieldname = 'P_CLSTYP' fieldvalue = lt_class[ class = lt_return[ 1 ]-fieldval ]-klart )
                                 ( fieldname = 'P_CLSNUM' fieldvalue = lt_return[ 1 ]-fieldval ) ).

        call function 'DYNP_VALUES_UPDATE'
          exporting
            dyname               = conv d020s-prog( sy-repid ) ##operator
            dynumb               = conv d020s-dnum( sy-dynnr ) ##operator
          tables
            dynpfields           = lt_dynpfields
          exceptions
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            others               = 8.
        if sy-subrc <> 0.
* Implement suitable error handling here
        endif.
      catch cx_sy_itab_line_not_found.

    endtry.
  endmethod.

  method f4_file.
    data: lv_file_filter type string,
          lt_filein      type filetable,
          lv_rc          type i,
          lv_user_action type i.

    clear: lv_file_filter, lv_rc, lv_user_action.
    refresh: lt_filein.
    move 'Excel files(*.xls,*.xlsx)|*.xls*' to lv_file_filter. " description|*.extension

    cl_gui_frontend_services=>file_open_dialog(
      exporting
        file_filter             = lv_file_filter
        multiselection          = space
      changing
        file_table              = lt_filein
        rc                      = lv_rc         " no of files selected
        user_action             = lv_user_action
      exceptions
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        others                  = 5 ).

    if sy-subrc <> 0.
      message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
      return.
    endif.

    if lv_user_action eq 9.
      " action cancelled
      message 'Action cancelled by user' type 'S' display like 'E'.
      return.
    endif.

    if lt_filein is not initial and lv_rc gt 0 and lv_user_action eq 0.
      try.
          p_file = conv #( lt_filein[ 1 ]-filename ).
        catch cx_sy_itab_line_not_found.
      endtry.
    else.
      message 'Input file not selected' type 'I' display like 'E'.
      clear p_file.
      return.
    endif.
  endmethod.

  method handle_sel_ucomm.
    if sscrfields-ucomm eq 'DWN'.
      lcl_app=>validate_input( ).
      if gt_classcharacteristics is not initial.
        lcl_app=>prep_dwn_file_format( ).
      endif.
    endif.
    if sscrfields-ucomm eq 'ONLI'.
      if p_file is initial.
        set cursor field 'P_FILE'.
        message s055(00) display like 'E'.
      endif.
    endif.
  endmethod.

  method process.
    validate_input( ).

    check gt_classcharacteristics is not initial.
    build_table_from_char( ).
    read_file_data( ).

    check <gt_table> is not initial.
    create_log_table( ).
    material_validation( ).
    maintain_classif_chars( ).

    check <gt_log> is not initial.
    display_log( ).
  endmethod.

  method validate_input.

    data: ls_return      type bapireturn1,
          lt_char_values type standard table of bapi_char_values.

    clear: ls_return.
    refresh: gt_classcharacteristics, lt_char_values.
    call function 'BAPI_CLASS_GET_CHARACTERISTICS'
      exporting
        classnum        = conv bapi_class_key-classnum( p_clsnum ) ##operator
        classtype       = conv bapi_class_key-classtype( p_clstyp ) ##operator
      importing
        return          = ls_return
      tables
        characteristics = gt_classcharacteristics
        char_values     = lt_char_values.
    if ls_return-type eq 'E' or ls_return-type eq 'A'.
      message ls_return-message type 'S' display like 'E'.
      return.
    else.
      set_object_table( ).
    endif.

  endmethod.

  method set_object_table.
    clear gs_objecttable.
    select single obtab from tcla into gs_objecttable where klart eq p_clstyp.
  endmethod.

  method prep_dwn_file_format.
    build_table_from_char( ).

    " populate table for download format
    check <gt_table> is assigned.
    refresh <gt_table>.
    do 2 times.
      data(lv_index) = sy-index.
      append initial line to <gt_table> assigning field-symbol(<ls_table>).
      if <ls_table> is assigned.
        case lv_index.
          when 1.
            assign component 1 of structure <ls_table> to field-symbol(<lv>).
            if sy-subrc = 0.
              <lv> = 'MATNR'.
            endif.
            unassign <lv>.
            assign component 2 of structure <ls_table> to <lv>.
            if sy-subrc = 0.
              <lv> = 'MAKTX'.
            endif.
          when 2.
            unassign <lv>.
            assign component 1 of structure <ls_table> to <lv>.
            if sy-subrc = 0.
              <lv> = to_mixed('MATERIAL').
            endif.
            unassign <lv>.
            assign component 2 of structure <ls_table> to <lv>.
            if sy-subrc = 0.
              <lv> = to_mixed('DESCRIPTION').
            endif.
          when others.  " for batch in future
            exit.
        endcase.
        do.
          unassign <lv>.
          assign component sy-index + 2 of structure <ls_table> to <lv>.  " make sy-index + 3 if batch is added in future
          " to ensure backwards compatibility with MARA use a variable instead
          " eg: lv_no_of_fixed_cols = 2, 3 etc
          if sy-subrc <> 0.
            exit.
          endif.
          if <lv> is assigned.
            try.
                case lv_index.
                  when 1.
                    <lv> = gt_classcharacteristics[ sy-index ]-name_char.
                  when 2.
                    <lv> = gt_classcharacteristics[ sy-index ]-descr_char.
                  when others.
                endcase.
              catch cx_sy_itab_line_not_found.
            endtry.
          endif.
        enddo.
        unassign <ls_table>.
      endif.
      clear lv_index.
    enddo.

    check <gt_table> is not initial.
    data: lv_file_path type string,
          lv_filename  type string,
          lv_path      type string.

    clear: lv_path, lv_filename, lv_file_path.
    cl_gui_frontend_services=>file_save_dialog(
      exporting
        window_title      = conv #( |Download file format| )
        default_file_name = conv #( |File_format_for_class_{ p_clsnum }_type_{ p_clstyp }| )
        default_extension = conv #( |xls| )
      changing
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_file_path
      exceptions
        cntl_error                = 1
        error_no_gui              = 2
        not_supported_by_gui      = 3
        invalid_default_file_name = 4
        others = 5 ).

    if sy-subrc <> 0.
      message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    endif.

    check lv_file_path is not initial and sy-subrc = 0.
    cl_gui_frontend_services=>gui_download(
      exporting
        filename              = lv_file_path
        write_field_separator = abap_true
      changing
        data_tab              = <gt_table>
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        others = 25 ).

    refresh <gt_table>.
    if sy-subrc <> 0.
      message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
    else.
      message 'File format downloaded successfully at' && ` ` && lv_file_path type 'S'.
    endif.

  endmethod.

  method build_table_from_char.
    check gt_classcharacteristics is not initial.
    data: lt_component type cl_abap_structdescr=>component_table.
    data: lv_excellcell type c length 50.

    append value #( name = 'MATNR' type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_excellcell ) ) ) to lt_component.
    append value #( name = 'MAKTX' type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_excellcell ) ) ) to lt_component.
    " add field for batch in future

    loop at gt_classcharacteristics into data(ls_classcharacteristics).
      append value #( name = conv #( ls_classcharacteristics-name_char )
                      type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_excellcell ) ) ) to lt_component.
      clear ls_classcharacteristics.
    endloop.

    try.
        data(lo_struct) = cl_abap_structdescr=>create(
                               exporting
                                 p_components = lt_component ).
      catch cx_sy_struct_creation
            cx_sy_unknown_type.
    endtry.

    data: ls_table type ref to data.
    if lo_struct is bound.
      create data ls_table type handle lo_struct.

      if ls_table is bound.
        try.
            data(lo_tabletype) = cl_abap_tabledescr=>create(
                                  exporting
                                    p_line_type = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ls_table ) )
                                    p_table_kind = cl_abap_tabledescr=>tablekind_std ).
          catch cx_sy_table_creation
                cx_sy_unknown_type.
        endtry.

        if lo_tabletype is bound.
          data: lt_table type ref to data.
          create data lt_table type handle lo_tabletype.

          if lt_table is bound.
            unassign <gt_table>.
            assign lt_table->* to <gt_table>.
          endif.
        endif.
      endif.
    endif.
  endmethod.

  method read_file_data.
    check <gt_table> is assigned and p_file is not initial.
    data: lt_raw type truxs_t_text_data.
    call function 'TEXT_CONVERT_XLS_TO_SAP'
      exporting
        i_field_seperator    = abap_true
        i_line_header        = abap_false
        i_tab_raw_data       = lt_raw
        i_filename           = conv rlgrap-filename( p_file )
      tables
        i_tab_converted_data = <gt_table>
      exceptions
        conversion_failed    = 1
        others               = 2.
    if sy-subrc <> 0.
      message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
      return.
    endif.

    zcl_helper=>check_file_format( changing ctab = <gt_table> exceptions file_format_altered = 1 others = 2 ).
    if sy-subrc <> 0.
      refresh <gt_table>.
      return.
    endif.

    check <gt_table> is not initial.
    loop at <gt_table> assigning field-symbol(<ls_table>).
      zcl_helper=>format_excel_to_bapi( changing is_excel = <ls_table> is_data = <ls_table> ).
    endloop.

  endmethod.

  method material_validation.
    data: lv_matnr type mara-matnr.
    check <gt_log> is assigned and <gt_table> is not initial.
    loop at <gt_table> assigning field-symbol(<ls_table>).
      data(lv_index) = sy-tabix.
      append initial line to <gt_log> assigning field-symbol(<ls_log>).
      if <ls_log> is assigned.
        assign component 'MATNR' of structure <ls_table> to field-symbol(<lv_matnr>).
        if <lv_matnr> is assigned.
          lv_matnr = <lv_matnr>.
          zcl_helper=>conv_exit( exporting iv = lv_matnr importing ev = lv_matnr ).

          assign component 'MAKTX' of structure <ls_table> to field-symbol(<lv_maktx>).
          if <lv_maktx> is assigned and <lv_maktx> is initial.
            select single maktx from makt into <lv_maktx> where matnr eq lv_matnr and spras eq sy-langu.
          endif.
        endif.
        move-corresponding <ls_table> to <ls_log>.
        select single mtart, spart from mara into ( @data(lv_mtart), @data(lv_spart) ) where matnr eq @lv_matnr.
        if sy-subrc = 0.
          select single * from zmm_t_mat_class into @data(ls_mat_class)
            where class eq @p_clsnum
            and   klart eq @p_clstyp
            and   mtart eq @lv_mtart
            and   spart eq @lv_spart.

          if sy-subrc <> 0.
            assign component 'MESSAGE' of structure <ls_log> to field-symbol(<lv_msg>).
            if <lv_msg> is assigned.
              <lv_msg> = |{ p_clsnum }:{ p_clstyp } is not applicable to mat. type: { lv_mtart }, div: { lv_spart }|.
            endif.
            assign component 'PROC_STAT' of structure <ls_log> to field-symbol(<lv_icon>).
            if <lv_icon> is assigned.
              <lv_icon> = icon_red_light.
            endif.
            delete <gt_table> index lv_index.
          endif.
        else.
          assign component 'MESSAGE' of structure <ls_log> to <lv_msg>.
          if <lv_msg> is assigned.
            <lv_msg> = |Material { lv_matnr alpha = out } does not exist|.
          endif.
          assign component 'PROC_STAT' of structure <ls_log> to <lv_icon>.
          if <lv_icon> is assigned.
            <lv_icon> = icon_red_light.
          endif.
          delete <gt_table> index lv_index. " IHDK902648
        endif.
      endif.
      unassign: <lv_matnr>, <lv_msg>, <lv_icon>, <ls_log>.
    endloop.

    delete <gt_log> where ('PROC_STAT IS INITIAL').
    " check if material exists

    " check if selected class is correct/applicable as per material type and division using a z table

    " add batch check in case of mcha/b => whether batch is supplied; whether batch exists

    " verify material description??
  endmethod.

  method create_log_table.
    check <gt_table> is assigned.
    data(lo_table_descr) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <gt_table> ) ).
    data(lo_struct_descr) = cast cl_abap_structdescr( lo_table_descr->get_table_line_type( ) ).

    data(lt_component) = lo_struct_descr->get_components( ).

    data: lv_msg  type bapi_msg,
          lv_icon type icon_d.
    append value #( name = 'MESSAGE'   type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_msg ) ) )  to lt_component.
    append value #( name = 'PROC_STAT' type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_icon ) ) ) to lt_component.

    data(lo_struct) = cl_abap_structdescr=>create(
                           exporting
                             p_components = lt_component ).

    data: ls_table type ref to data.
    create data ls_table type handle lo_struct.

    data(lo_tabletype) = cl_abap_tabledescr=>create(
                          exporting
                            p_line_type = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ls_table ) )
                            p_table_kind = cl_abap_tabledescr=>tablekind_std ).

    data: lt_table type ref to data.
    create data lt_table type handle lo_tabletype.

    unassign <gt_log>.
    assign lt_table->* to <gt_log>.

    refresh <gt_log>.

    " push 2nd line from excel to log table -> this is the description line
    try.
        append initial line to <gt_log> assigning field-symbol(<ls_log>).
        if <ls_log> is assigned.
          move-corresponding <gt_table>[ 1 ] to <ls_log>.
          assign component 'MESSAGE' of structure <ls_log> to field-symbol(<lv_msg>).
          if <lv_msg> is assigned.
            <lv_msg> = 'Message'.
          endif.
          assign component 'PROC_STAT' of structure <ls_log> to field-symbol(<lv_icon>).
          if <lv_icon> is assigned.
            <lv_icon> = 'PrSt'.
          endif.
        endif.
      catch cx_sy_itab_line_not_found.
    endtry.

    delete <gt_table> index 1.
  endmethod.

  method maintain_classif_chars.
    data: lv_objectkey          type bapi1003_key-object,
          lv_classif_status     type bapi1003_key-status,
          lt_allocvaluesnumnew  type standard table of bapi1003_alloc_values_num,
          lt_allocvaluescharnew type standard table of bapi1003_alloc_values_char,
          lt_allocvaluescurrnew type standard table of bapi1003_alloc_values_curr,
          lt_return             type standard table of bapiret2,
          lv_matnr              type mara-matnr,
          lv_date               type sy-datum.

    check <gt_table> is not initial.
    data(lo_table_descr) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <gt_table> ) ).
    data(lo_struct_descr) = cast cl_abap_structdescr( lo_table_descr->get_table_line_type( ) ).

    data(lt_component) = lo_struct_descr->get_components( ).
    loop at <gt_table> assigning field-symbol(<ls_table>).
      data(lv_index) = sy-tabix.

      refresh: lt_allocvaluesnumnew, lt_allocvaluesnumnew, lt_allocvaluescurrnew, lt_return.
      clear: lv_objectkey, lv_classif_status, lv_matnr, lv_date.
      assign component 'MATNR' of structure <ls_table> to field-symbol(<lv_matnr>).
      if <lv_matnr> is assigned.
        lv_matnr = <lv_matnr>.
        zcl_helper=>conv_exit( exporting iv = lv_matnr importing ev = lv_matnr ).
      endif.
      unassign <lv_matnr>.

      do.
        if sy-index gt 2. " first 2 columns are fixed; will be 3 for batch; must be variable based for backwards compatibilty
          assign component sy-index of structure <ls_table> to field-symbol(<lv>).
          if sy-subrc <> 0.
            exit.
          endif.
          if <lv> is assigned and <lv> is not initial.
            case gt_classcharacteristics[ name_char = conv #( lt_component[ sy-index ]-name ) ]-data_type.
              when 'CHAR'.
                append initial line to lt_allocvaluescharnew assigning field-symbol(<ls_allocvaluescharnew>).
                if <ls_allocvaluescharnew> is assigned.
                  <ls_allocvaluescharnew>-charact = gt_classcharacteristics[ name_char = conv #( lt_component[ sy-index ]-name ) ]-name_char.
                  <ls_allocvaluescharnew>-value_char = <lv>.
                endif.
                unassign <ls_allocvaluescharnew>.
              when 'DATE'.
                append initial line to lt_allocvaluesnumnew assigning field-symbol(<ls_allocvaluesnumnew>).
                if <ls_allocvaluesnumnew> is assigned.
                  <ls_allocvaluesnumnew>-charact = gt_classcharacteristics[ name_char = conv #( lt_component[ sy-index ]-name ) ]-name_char.

                  char_to_fltp( exporting iv = <lv> importing ev = <ls_allocvaluesnumnew>-value_from ).
                  zcl_helper=>conv_exit( exporting iv = <lv> iv_mode = 'O' importing ev = <lv> ).
                endif.
                unassign <ls_allocvaluesnumnew>.
              when 'NUM'.
                append initial line to lt_allocvaluesnumnew assigning <ls_allocvaluesnumnew>.
                if <ls_allocvaluesnumnew> is assigned.
                  <ls_allocvaluesnumnew>-charact = gt_classcharacteristics[ name_char = conv #( lt_component[ sy-index ]-name ) ]-name_char.

                  char_to_fltp( exporting iv = <lv> importing ev = <ls_allocvaluesnumnew>-value_from ).
                endif.
                unassign <ls_allocvaluesnumnew>.
              when 'CURR'.
                append initial line to lt_allocvaluesnumnew assigning field-symbol(<ls_allocvaluescurrnew>).
                if <ls_allocvaluescurrnew> is assigned.
                  <ls_allocvaluescurrnew>-charact = gt_classcharacteristics[ name_char = conv #( lt_component[ sy-index ]-name ) ]-name_char.

                  char_to_fltp( exporting iv = <lv> importing ev = <ls_allocvaluescurrnew>-value_from ).
                endif.
                unassign <ls_allocvaluescurrnew>.
              when others.
            endcase.
          endif.
        endif.
      enddo.

      lv_objectkey = conv #( lv_matnr ).  " in case of batch( identifiable based on gs_objecttable = MCHB ), this must matnr + batch
      if lt_allocvaluesnumnew[] is not initial
        or lt_allocvaluescharnew[] is not initial
        or lt_allocvaluescurrnew[] is not initial.
        call function 'BAPI_OBJCL_CHANGE'   " Read docu for details
          exporting
            objectkey          = lv_objectkey
            objecttable        = gs_objecttable
            classnum           = conv bapi_class_key-classnum( p_clsnum ) ##operator
            classtype          = conv bapi_class_key-classtype( p_clstyp ) ##operator
          importing
            classif_status     = lv_classif_status
          tables
            allocvaluesnumnew  = lt_allocvaluesnumnew
            allocvaluescharnew = lt_allocvaluescharnew
            allocvaluescurrnew = lt_allocvaluescurrnew
            return             = lt_return.

        append initial line to <gt_log> assigning field-symbol(<ls_log>).
        if <ls_log> is assigned.
          move-corresponding <ls_table> to <ls_log>.
          assign component 'MESSAGE' of structure <ls_log> to field-symbol(<lv_msg>).
          if <lv_msg> is assigned.
            try.
                <lv_msg> = lt_return[ id = 'CL' number = '735' type = 'S' ]-message.
              catch cx_sy_itab_line_not_found.
            endtry.
            try.
                <lv_msg> = lt_return[ id = 'CL' number = '737' type = 'S' ]-message.
              catch cx_sy_itab_line_not_found.
            endtry.
            try.
                <lv_msg> = lt_return[ id = 'CL' number = '738' type = 'S' ]-message.
              catch cx_sy_itab_line_not_found.
            endtry.
          endif.
          if <lv_msg> is not initial.
            assign component 'PROC_STAT' of structure <ls_log> to field-symbol(<lv_icon>).
            if <lv_icon> is assigned.
              <lv_icon> = icon_green_light.
            endif.

            if p_test is initial.
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true.
            endif.
          endif.
          try.
              if line_exists( lt_return[ type = 'E' ] ).
                assign component 'MESSAGE' of structure <ls_log> to <lv_msg>.
                if <lv_msg> is assigned.
                  <lv_msg> = |Some errors occured. Click for details.|.
                endif.
                assign component 'PROC_STAT' of structure <ls_log> to <lv_icon>.
                if <lv_icon> is assigned.
                  <lv_icon> = icon_red_light.
                endif.
                if p_test is initial.
                  call function 'BAPI_TRANSACTION_ROLLBACK'.
                endif.
                loop at lt_return into data(ls_return).
                  append initial line to gt_index_log assigning field-symbol(<ls_index_log>).
                  if <ls_index_log> is assigned.
                    <ls_index_log>-matnr = lv_matnr.
                    <ls_index_log>-type  = ls_return-type.
                    <ls_index_log>-msg   = ls_return-message.
                  endif.
                  clear ls_return.
                endloop.
              endif.
            catch cx_sy_itab_line_not_found.
          endtry.
          unassign: <lv_msg>, <lv_icon>.
        endif.
      endif.
    endloop.
  endmethod.

  method char_to_fltp.
    data: lv_fltp type cha_class_data-sollwert.
    clear: ev, lv_fltp.
    check iv is not initial.
    call function 'QSS0_CHAR_FLTP_CONVERSION'
      exporting
        i_character_string = conv qfltp-sollwert( iv )
        i_number_of_digits = conv plmk-stellen( 16 )
      importing
        e_fltp_value       = lv_fltp
      exceptions
        others             = 1.

    move lv_fltp to ev.
  endmethod.

  method display_log.
    check <gt_log> is not initial.
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = data(lo_table)
          changing
            t_table      = <gt_log> ).

        check lo_table is bound.

        data(lo_columns) = lo_table->get_columns( ).

        if lo_columns is bound.
          try.
              data(lt_col) = lo_columns->get( ).
              if lt_col is not initial.
                loop at lt_col into data(ls_col).
*                  translate ls_col-columnname using '_ '.
                  ls_col-r_column->set_long_text( exporting value = conv scrtext_l( ls_col-columnname ) ).
                  ls_col-r_column->set_medium_text( exporting value = conv scrtext_m( ls_col-columnname ) ).
                  ls_col-r_column->set_short_text( exporting value = conv scrtext_s( ls_col-columnname ) ).

                  clear ls_col.
                endloop.
              endif.

              data(lo_column) = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'PROC_STAT' ) ).
              if lo_column is bound.
                lo_column->set_long_text( exporting value = 'Processing Status' ).
                lo_column->set_medium_text( exporting value = 'Proc. Status' ).
                lo_column->set_short_text( exporting value = 'ProcStat' ).
                lo_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
              endif.

              free lo_column.
              lo_column = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'MESSAGE' ) ).
              if lo_column is bound.
                lo_column->set_long_text( exporting value = 'Log Message' ).
                lo_column->set_medium_text( exporting value = 'Log Msg' ).
                lo_column->set_short_text( exporting value = 'LogMsg' ).
                lo_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
              endif.

              free lo_column.
              lo_column = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'MATNR' ) ).
              if lo_column is bound.
                lo_column->set_cell_type( exporting value = if_salv_c_cell_type=>hotspot ).
              endif.
            catch cx_salv_not_found.
          endtry.
          lo_columns->set_column_position( exporting columnname = 'PROC_STAT' position = 1 ).
          lo_columns->set_column_position( exporting columnname = 'MESSAGE'   position = 2 ).
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
          lo_display->set_list_header( exporting value = conv lvc_title( 'Maintain Material Classification Characteristics' ) ).
        endif.

        data(lo_head_grid) = new cl_salv_form_layout_grid( ).
        if lo_head_grid is bound.
          data(lo_label) = lo_head_grid->create_label( exporting row = 1 column = 1 ).
          if lo_label is bound.
            lo_label->set_text( exporting value = 'Maintain Material Classification Characteristics' ).
          endif.

          lo_label = lo_head_grid->create_label( exporting row = 2 column = 1 ).
          if lo_label is bound.
            lo_label->set_text( exporting value = 'No. of materials processed:' ).
          endif.

          data(lo_flow) = lo_head_grid->create_flow( exporting row = 2 column = 2 ).
          if lo_flow is bound.
            lo_flow->create_text( exporting text = | { lines( <gt_log> ) - 1 }| ).
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
    data: lv_matnr type mara-matnr.
    clear lv_matnr.
    read table <gt_log> assigning field-symbol(<ls_log>) index row.
    if sy-subrc = 0.
      assign component 'MATNR' of structure <ls_log> to field-symbol(<lv_matnr>).
      if <lv_matnr> is assigned and <lv_matnr> is not initial.
        lv_matnr = <lv_matnr>.
        zcl_helper=>conv_exit( exporting iv = lv_matnr importing ev = lv_matnr ).
      endif.
      case column.
        when 'MATNR'.
          if lv_matnr is not initial.
            call function 'BAPI_MATERIAL_DISPLAY'
              exporting
                material = conv bapimatall-material( lv_matnr ).
          endif.
        when others.
          data(lt_index_log) = filter #( gt_index_log using key matnr where matnr = lv_matnr ).
          if lt_index_log is not initial.
            sort lt_index_log.
            try.
                cl_salv_table=>factory(
                exporting
                  list_display = abap_false
                importing
                  r_salv_table = data(lo_table)
                changing
                  t_table      = lt_index_log ).
              catch cx_salv_msg.
            endtry.

            if lo_table is bound.
              try.
                  data(lo_columns) = lo_table->get_columns( ).
                  if lo_columns is bound.
                    data(lo_column) = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'MATNR' ) ).
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
                lo_display->set_list_header( exporting value = |Processing log for index { lv_matnr }| ).
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
      endcase.
    endif.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    try.
        new lcl_app( )->process( ).
      catch cx_root into data(lox_root).
        message lox_root->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
endclass.
*--------------------------------------------------------------------*
* pre-selection screen events
*--------------------------------------------------------------------*
load-of-program.
  " placeholder

initialization.
  " placeholder
*--------------------------------------------------------------------*
* selection screen events
*--------------------------------------------------------------------*
at selection-screen output.
  lcl_app=>screen_modif( ).

at selection-screen on value-request for p_clsnum.
  lcl_app=>f4_clsnum( ).

at selection-screen on value-request for p_file.
  lcl_app=>f4_file( ).

at selection-screen.
  lcl_app=>handle_sel_ucomm( ).
  " placeholder
*--------------------------------------------------------------------*
* start-of-selection
*--------------------------------------------------------------------*
start-of-selection.
  lcl_main=>start( ).
*--------------------------------------------------------------------*
* end-of-selection
*--------------------------------------------------------------------*
end-of-selection.
