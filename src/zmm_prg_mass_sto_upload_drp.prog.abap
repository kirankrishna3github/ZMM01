*&---------------------------------------------------------------------*
*& Report  ZMM_PRG_MASS_STO_UPLOAD_DRP
*&---------------------------------------------------------------------*
*& Transaction            : ZMM085
*& Creation Date          : Tuesday, July 17, 2018 11:02:15
*& Author                 : 6010859 - SaurabhK
*& Functional             : Diwakar Ramji(Accenture)
*& Requested/Approved By  : DRP project team/Mr Sunil Kolambkar
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK932785
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&---------------------------------------------------------------------*
*& 1.
*& 2.
*& 3.
*& 4.
*&---------------------------------------------------------------------*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Tuesday, July 17, 2018 23:52:40
*& Rev. Author                : 6010859
*& Rev. Requested/Approved By : Diwakar Ramji(Accenture)
*& Rev. Request#              : IRDK932867, IRDK932879, IRDK932909, IRDK932911
*& Rev. Description           : MM: S_K: ZMM085: Pre-production changes: 17.07.18
*&---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Monday, July 30, 2018 12:07:24
*& Rev. Author                : 6010859
*& Rev. Requested/Approved By : Diwakar Ramji(Accenture)
*& Rev. Request#              : IRDK932970
*& Rev. Description           : MM: S_K: ZMM085: Add STO No. column in o/p: 30.7.18
*&---------------------------------------------------------------------*
*& Revision #                 : 03
*& Rev. Date                  : Wednesday, August 01, 2018 08:52:38
*& Rev. Author                : 6010859
*& Rev. Requested/Approved By : Diwakar Ramji(Accenture)
*& Rev. Request#              : IRDK932987
*& Rev. Description           : MM: S_K: ZMM085: Add mat. desc. to output: 1.8.18
*&---------------------------------------------------------------------*
report zmm_prg_mass_sto_upload_drp.
*&---------------------------------------------------------------------*
*---<< S/4HANA >>---*
*&---------------------------------------------------------------------*
* Changed On - Tuesday, October 16, 2018 ::00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - Functionality not available
* Solution   - Add Material External Field
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*&---------------------------------------------------------------------*

* <--- Selection screen and global data --->
tables: sscrfields.
type-pools: icon.

" Data for selection screen - select options
data: ord_typ type ekko-bsart,
      co_code type ekko-bukrs,
      pur_org type ekko-ekorg,
      pur_grp type ekko-ekgrp.

" selection screen
selection-screen begin of block sel with frame title text-sel.
parameters: p_bsart like ord_typ default 'ZSTO' obligatory,
            p_bukrs like co_code obligatory default '1000',
            p_ekorg like pur_org obligatory default '1000',
            p_ekgrp like pur_grp obligatory default '301'.
selection-screen end of block sel.

selection-screen begin of block fil with frame title text-fil.
parameters: p_file type string obligatory default 'Drive:\Path_of_your_upload_file.xls'.
selection-screen end of block fil.

selection-screen begin of block dwn with frame title text-dwn.
selection-screen: begin of line,
  pushbutton 1(20) dwn_btn user-command dwn_format,
  comment 25(70) text-wrn,
end of line.
selection-screen end of block dwn.

" local class definition
* ---- begin local exception class definition ---- *
class lcx_generic_error definition inheriting from cx_static_check.
  " Used in throwing a generic exception, Error displayed is based on context of the exception
endclass.
* ---- end exception class definition ---- *

class lcl_application definition final.
  public section.
    class-methods: file_select raising lcx_generic_error.
    methods: process.

  protected section.
    " place holder

  private section.
    types: excelcell(50) type c.
    data: begin of excel_line,
            id      type excelcell,
            reswk   type excelcell, " supplying plant
            matnr   type excelcell, " material
            bstmg   type excelcell, " quantity
            eeind   type excelcell, " delivery date
            ewerk   type excelcell, " dest plant
            versart type excelcell, " ship. type
            text    type excelcell, " item text
          end of excel_line,
          excel like standard table of excel_line,

          begin of data,
            id      type i,
            reswk   type reswk,
            matnr   type matnr,
            bstmg   type bstmg,
            eeind   type eeind,
            ewerk   type ewerk,
            versart type versart,
            text    type tdline,
          end of data,
          data_tab like standard table of data,

          begin of out,
            icon    type icon_d,  " signal
            id      type i,
            bsart   type bsart,
            bukrs   type bukrs,
            ekorg   type ekorg,
            ekgrp   type ekgrp,
            reswk   type reswk,
            matnr   type matnr,
            maktx   type maktx,
            bstmg   type bstmg,
            eeind   type eeind,
            ewerk   type ewerk,
            versart type versart,
            text    type tdline,
            ebeln   type ebeln,
            status  type char20,
            messg   type bapi_msg,
          end of out,
          outtab like standard table of out.

    data: begin of msg_log,
            index   type syindex,
            msg_typ type bapiret2-type,
            msg     type bapi_msg,
          end of msg_log,
          msg_logs like standard table of msg_log with non-unique sorted key index components index.

    methods:
      read_file,

      check_file_format raising lcx_generic_error,

      convert_to_bapi_format raising lcx_generic_error,

      create_sto,

      fill_bapix
        importing
          value(bapi) type any
        changing
          bapix       type any,

      display_log,

      hotspot_click for event link_click of cl_salv_events_table  " event handler
        importing
            row
            column.
endclass.

class main definition.
  public section.
    class-methods: start.
endclass.

" local class implementation
class lcl_application implementation.
  method file_select.
* parameters used in :
* cl_gui_frontend_services=>file_open_dialog *
* cl_gui_frontend_services=>file_save_dialog *
    data: file        type string,  " filename
          path        type string,  " directory path
          file_path   type string,  " full path: directory + filename with ext
          file_filter type string,  " save as
          title       type string,  " dialog title
          defname     type string,  " default file name
          defext      type string,  " default extension
          useraction  type i.       " button clicked by user

* file table containing selected files, used in file_open *
    data: it_filein type filetable,
          wa_filein type file_table,

          " No of files selected
          rc        type i.

    clear: title, file_filter, rc, useraction.
    refresh: it_filein.

    move 'Select STO upload file' to title.
    move '(*.xls,*.xlsx)|*.xls*' to file_filter.
    call method cl_gui_frontend_services=>file_open_dialog
      exporting
        window_title            = title
        file_filter             = file_filter
        multiselection          = space
      changing
        file_table              = it_filein
        rc                      = rc
        user_action             = useraction
      exceptions
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        others                  = 5.
    if sy-subrc <> 0.
      message id 'ZMM' type 'S' number '002' display like 'E'.
      message id sy-msgid
      type 'I'
      number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      raise exception type lcx_generic_error.
    endif.

    if useraction eq '9'.
      message id 'ZMM' type 'S' number '001' display like 'E'.
      raise exception type lcx_generic_error.
    else.
      clear: wa_filein, p_file.
      check it_filein is not initial and rc eq 1.
      read table it_filein into wa_filein index 1.
      if sy-subrc = 0.
        move wa_filein-filename to p_file.
      endif.
    endif.
  endmethod.

  method process.
    if p_file is not initial.
      read_file( ).
    else.
      message 'No file selected' type 'S' display like 'E'.
      return.
    endif.

    if excel is not initial.
      try.
          check_file_format( ).
          if excel is not initial.
            convert_to_bapi_format( ).
          endif.
        catch lcx_generic_error.
          return.
      endtry.

      if data_tab is not initial.
        create_sto( ).
      else.
        message 'No data/invalid data' type 'S' display like 'E'.
        return.
      endif.
    else.
      message 'No data could be read' type 'I' display like 'E'.
      return.
    endif.

    if outtab is not initial.
      display_log( ).
    endif.
  endmethod.

  method read_file.
    data i_tab_raw_data type truxs_t_text_data.
    data lv_msg type string.
    clear lv_msg.
    refresh excel.
    call function 'TEXT_CONVERT_XLS_TO_SAP' " IHDK900159
      exporting
        i_field_seperator    = abap_true
        i_line_header        = abap_true
        i_tab_raw_data       = i_tab_raw_data
        i_filename           = conv rlgrap-filename( p_file )
      tables
        i_tab_converted_data = excel
      exceptions
        conversion_failed    = 1
        others               = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
      if sy-msgid eq 'UX' and sy-msgno eq '893'.  " Replace file cannot be processed with a meaningful error
        clear lv_msg.
        concatenate 'Is file' p_file 'still open? Please close the file.' into lv_msg separated by space.
        message lv_msg type 'S' display like 'E'.
      else.
        message id sy-msgid
        type 'S'
        number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
      endif.
    endif.

    if excel is initial.
      message 'No data read from file' type 'I' display like 'E'.
      return.
    endif.
  endmethod.

  method check_file_format.
    data: lo_struct type ref to cl_abap_structdescr,
          lo_table  type ref to cl_abap_tabledescr,
          lt_comp   type cl_abap_structdescr=>component_table,
          ls_comp   like line of lt_comp.

    free: lo_table, lo_struct.
    refresh lt_comp.
    lo_table  ?=  cl_abap_structdescr=>describe_by_data( excel[] ).
    lo_struct ?=  lo_table->get_table_line_type( ).
    lt_comp   =   lo_struct->get_components( ).

    clear excel_line.
    read table excel into excel_line index 1.
    if sy-subrc = 0.
      do.
        assign component sy-index of structure excel_line to field-symbol(<fs>).
        if sy-subrc <> 0.
          exit.
        endif.
        if <fs> is assigned.
          condense <fs>.
          <fs> = shift_left( val = <fs> sub = '' ).

          clear ls_comp.
          read table lt_comp into ls_comp index sy-index.
          if sy-subrc = 0 and to_upper( ls_comp-name ) <> to_upper( <fs> ).
            message ls_comp-name && ': Field sequence altered. Please check your file format' type 'S' display like 'E'.
            raise exception type lcx_generic_error.
          endif.
        endif.
        unassign <fs>.
      enddo.
    endif.
    delete excel index 1.
  endmethod.

  method convert_to_bapi_format.  " IHDK900081: MM: S_K: ZMM085: Fixes post S4H: MATNR length: 6.1.19
* local variables for method: convert to bapi format *  " RTTS: ABAP Run Time Type Services
    data: lo_descr          type ref to cl_abap_typedescr.  " type info of supplied data
    data: lv_relative_name  type string.    " data element of supplied variable
    data: lv_data_element   type rollname.
    data: lv_function       type funcname.  " CONV. EXIT Name
    data: wa_dd04l          type dd04l.     " Data elements - get converion exit for data element
    data: lv_index          type sy-index.  " Loop Index

    try.
        refresh data_tab.
        clear: data.
        loop at excel assigning field-symbol(<excel_line>).
          append initial line to data_tab assigning field-symbol(<data>).
          do.
            assign component sy-index of structure <excel_line> to field-symbol(<fs_excel>).
            if sy-subrc <> 0.
              exit.
            endif.
            assign component sy-index of structure <data> to field-symbol(<fs_data>).
            if sy-subrc <> 0.
              exit.
            endif.
            if <fs_excel> is assigned.
              <fs_excel> = condense( <fs_excel> ).
              <fs_excel> = shift_left( val = <fs_excel> sub = space ).
              move <fs_excel> to <fs_data>.
              free lo_descr.
              lo_descr = cl_abap_typedescr=>describe_by_data( p_data = <fs_data> ).

              if lo_descr is bound.
                clear: lv_relative_name, lv_data_element, wa_dd04l, lv_function.

                lv_relative_name = lo_descr->get_relative_name( ).

                if lv_relative_name is not initial.
                  move lv_relative_name to lv_data_element.

                  select single *
                  from dd04l
                  into wa_dd04l
                  where rollname = lv_data_element.

                  if wa_dd04l-convexit is not initial.
                    concatenate 'CONVERSION_EXIT_' wa_dd04l-convexit '_INPUT' into lv_function.

                    call function 'FUNCTION_EXISTS'
                      exporting
                        funcname           = lv_function
                      exceptions
                        function_not_exist = 1
                        others             = 2.
                    if sy-subrc = 0.
                      try.
                          call function lv_function
                            exporting
                              input  = <fs_data>
                            importing
                              output = <fs_data>.
                        catch cx_sy_dyn_call_illegal_func.
                          " catch-block
                      endtry.
                    endif.
                  endif.
                endif.
              endif.
*              <fs_data> = |{ <fs_data> alpha = in }|.
            endif.
          enddo.
        endloop.
      catch cx_root into data(lo_cx).
    endtry.
  endmethod.

  method create_sto.
    data poheader         type bapimepoheader.
    data poheaderx        type bapimepoheaderx.
    data exppurchaseorder type bapimepoheader-po_number.
    data return           type standard table of bapiret2.
    data wa_return        like line of return.
    data poitem           type standard table of bapimepoitem.
    data wa_poitem        like line of poitem.
    data poitemx          type standard table of bapimepoitemx.
    data wa_poitemx       like line of poitemx.
    data poschedule       type standard table of bapimeposchedule.
    data wa_poschedule    like line of poschedule.
    data poschedulex      type standard table of bapimeposchedulx.
    data wa_poschedulex   like line of poschedulex.
    data potextitem       type standard table of bapimepotext.
    data wa_potextitem    like line of potextitem.
    data msg              type bapiret2-message.
    data item             like wa_poitem-po_item.

    check data_tab is not initial.
    data(loop_tab) = data_tab[].

    loop at loop_tab into data(loop).
      clear: poheader, poheaderx, exppurchaseorder, wa_poitem, wa_poitemx,
             wa_poschedule, wa_poschedulex, wa_potextitem, wa_return, msg, item.
      refresh: return, poitem, poitemx, poschedule, poschedulex, potextitem.

      " prepare for log output
      clear out.
      out-bukrs = p_bukrs.
      out-bsart = p_bsart.
      out-ekorg = p_ekorg.
      out-ekgrp = p_ekgrp.
      out-reswk = loop-reswk.

      poheader-comp_code = p_bukrs.
      poheader-doc_type  = p_bsart.
      poheader-purch_org = p_ekorg.
      poheader-pur_group = p_ekgrp.
      poheader-suppl_plnt = loop-reswk.

      " Fill poheaderx
      fill_bapix(
      exporting
        bapi = poheader
      changing
        bapix = poheaderx ).

      clear data.
      loop at data_tab into data where id = loop-id.
        " prepare for log output
        move-corresponding data to out.
        item = item + 10.
        " fill poitem
        wa_poitem-po_item = item.
        wa_poitem-material = data-matnr.
        wa_poitem-material_external = data-matnr.

        select single maktx from makt into out-maktx where matnr = data-matnr and spras = 'E'.

        wa_poitem-plant    = data-ewerk.
        wa_poitem-quantity = data-bstmg.
        wa_poitem-shiptype = data-versart.

        " Fill poitemx
        fill_bapix(
        exporting
          bapi = wa_poitem
        changing
          bapix = wa_poitemx ).

        wa_poitemx-po_itemx = abap_true.

        " fill poschedule
        wa_poschedule-po_item = wa_poitem-po_item.
        wa_poschedule-sched_line = wa_poschedule-sched_line + 1.
        wa_poschedule-delivery_date = data-eeind.

        " fill poschedulex
        fill_bapix(
        exporting
          bapi = wa_poschedule
        changing
          bapix = wa_poschedulex ).

        wa_poschedulex-po_itemx    = abap_true.
        wa_poschedulex-sched_linex = abap_true.

        " fill potextitem
        wa_potextitem-po_item = wa_poitem-po_item.
        wa_potextitem-text_id = 'F01'.
        wa_potextitem-text_form = '*'.
        wa_potextitem-text_line = data-text.

        " no x structure for potextitem

        append: wa_poitem to poitem,
                wa_poitemx to poitemx,
                wa_poschedule to poschedule,
                wa_poschedulex to poschedulex,
                wa_potextitem to potextitem,
                out to outtab.
        clear: wa_poitem, wa_poitemx, wa_poschedule, wa_poschedulex, wa_potextitem.
        clear data.
      endloop.

      if poheader is not initial and poitem is not initial.
        data(poitem_bdc) = poitem[].  " IRDK932911
        call function 'BAPI_PO_CREATE1'
          exporting
            poheader         = poheader
            poheaderx        = poheaderx
          importing
            exppurchaseorder = exppurchaseorder
          tables
            return           = return
            poitem           = poitem
            poitemx          = poitemx
            poschedule       = poschedule
            poschedulex      = poschedulex
            potextitem       = potextitem.
      endif.

      clear: wa_return, msg.
      if exppurchaseorder is not initial. " success
        call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true.
        read table return into wa_return with key type = 'S' id = '06' number = '017'.
        if sy-subrc = 0.
          msg = wa_return-message.
        endif.

        " since the above bapi could not update the ship type field for some reason(we have raised this to SAP), we are using bdc here as a post update work-around
        call function 'ZFM_DRP_STO_VSART_UPDATE_BDC'  " IRDK932879
          exporting
            exppurchaseorder = exppurchaseorder
          tables
            poitem           = poitem_bdc.  " IRDK932911

      else.
        call function 'BAPI_TRANSACTION_ROLLBACK'.
        " read return for error
        read table return into wa_return with key type = 'E'. " Error
        if sy-subrc <> 0.
          read table return into wa_return with key type = 'A'. " Abend
        endif.
        if wa_return is not initial.
          msg = wa_return-message.
        endif.
      endif.

      clear out.
      loop at outtab into out where id = loop-id.
        if exppurchaseorder is not initial. " success
          out-icon   = icon_green_light.
          out-status = 'Success'.
          out-ebeln  = exppurchaseorder.
        else. " failure
          out-icon   = icon_red_light.
          out-status = 'Failure'.
        endif.
        out-messg  = msg.
        modify outtab from out transporting icon status messg ebeln.
        clear out.
      endloop.

      " loop at return anyways
      clear msg_log.
      clear: wa_return, msg.
      loop at return into wa_return.
        msg_log-index = loop-id.
        msg_log-msg_typ = wa_return-type.
        msg = wa_return-message.
        msg_log-msg = msg.

        " IRDK932867
        if exppurchaseorder is not initial.
          if wa_return-type = 'S' and wa_return-id = '06' and wa_return-number = '017'. " only add success message for successful creation
            append msg_log to msg_logs.
          endif.
        else. " in other cases add all messages
          append msg_log to msg_logs.
        endif.

        clear: msg_log, wa_return.
      endloop.

      delete loop_tab where id = loop-id.
      clear: loop, data.
    endloop.
  endmethod.

  method fill_bapix.
    " This works when there's one to one mapping between bapi and bapix structures
    " Local field symbols
    field-symbols: <fs_bapi>  type any,
                   <fs_bapix> type any,
                   <fs_x>     type any,
                   <fs>       type any.

    constants: c_bapix_type type rollname value 'BAPIUPDATE'.

    data: lo_struct type ref to cl_abap_structdescr,
          lo_table  type ref to cl_abap_tabledescr,
          lt_comp   type cl_abap_structdescr=>component_table,
          ls_comp   like line of lt_comp,
          lt_compx  type cl_abap_structdescr=>component_table,
          ls_compx  like line of lt_comp.

    try.
        unassign: <fs_bapi>, <fs_bapix>, <fs_x>, <fs>.

        assign: bapi  to <fs_bapi>,
        bapix to <fs_bapix>.

        free: lo_table, lo_struct.
        refresh lt_comp.
        lo_struct  ?=  cl_abap_structdescr=>describe_by_data( <fs_bapi> ).
*        lo_struct ?=  lo_table->get_table_line_type( ).
        lt_comp   =   lo_struct->get_components( ).

        free: lo_table, lo_struct.
        refresh lt_compx.
        lo_struct  ?=  cl_abap_structdescr=>describe_by_data( <fs_bapix> ).
*        lo_struct ?=  lo_table->get_table_line_type( ).
        lt_compx   =   lo_struct->get_components( ).

        if <fs_bapi> is assigned and <fs_bapix> is assigned.
          clear: <fs_bapix>.
          move 0 to sy-subrc.
          if lt_comp is not initial.
            clear ls_comp.
            loop at lt_comp into ls_comp.
              unassign <fs>.
              assign component sy-tabix of structure <fs_bapi> to <fs>.
              read table lt_compx into ls_compx with key name = ls_comp-name.
              if sy-subrc = 0.
                unassign <fs_x>.
                assign component sy-tabix of structure <fs_bapix> to <fs_x>.
                if <fs> is assigned and <fs> is not initial and <fs_x> is assigned.
                  data(lo_descr) = cl_abap_typedescr=>describe_by_data( p_data = <fs_x> ).

                  if lo_descr is bound.
                    data(lv_relative_name) = lo_descr->get_relative_name( ).
                    if lv_relative_name eq c_bapix_type.
                      move abap_true to <fs_x>.
                    else.
                      move <fs> to <fs_x>.
                    endif.
                  endif.
                endif.
              endif.
            endloop.
          endif.
        endif.
      catch cx_root into data(lo_cx).
    endtry.
  endmethod.

  method display_log.
*    check outtab is not initial.
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
            o_column ?= o_columns->get_column( columnname = 'ID' ).
            o_column->set_long_text( value = 'Line ID' ).
            o_column->set_medium_text( value = 'Line ID' ).
            o_column->set_short_text( value = 'Line ID' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BSART' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BUKRS' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EKORG' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EKGRP' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'RESWK' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'STATUS' ).
            o_column->set_long_text( value = 'Processing Status' ).
            o_column->set_medium_text( value = 'Proc. Status' ).
            o_column->set_short_text( value = 'Proc Stat' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EBELN' ).
            o_column->set_long_text( value = 'STO Number' ).
            o_column->set_medium_text( value = 'STO No.' ).
            o_column->set_short_text( value = 'STO' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            " IRDK932867
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TEXT' ).
            o_column->set_long_text( value = 'PO Item Text' ).
            o_column->set_medium_text( value = 'PO Item Text' ).
            o_column->set_short_text( value = 'Item Text' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MESSG' ).
            o_column->set_long_text( value = 'Message' ).
            o_column->set_medium_text( value = 'Message' ).
            o_column->set_short_text( value = 'Message' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

          catch: cx_salv_not_found.
        endtry.
        o_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
        o_columns->set_key_fixation( exporting value = if_salv_c_bool_sap=>true ).
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
          o_label->set_text( exporting value = 'Mass STO Creation: Processing Log' ).
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
      when 'ICON' or 'STATUS' or 'MESSG' or 'ID'. " IRDK932867
        clear out.
        read table outtab into out index row.
        if sy-subrc = 0.
          data(index_log) = filter #( msg_logs using key index where index = out-id ).
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

    if lo_app is bound.
      lo_app->process( ).
    endif.
  endmethod.
endclass.

" selection screen events

at selection-screen output.
  " image and text for download button
  dwn_btn = '@49@ Download'.

at selection-screen on value-request for p_file.
  try.
      lcl_application=>file_select( ).
    catch lcx_generic_error into data(lo_cx).
      return.
  endtry.

at selection-screen.
  if sscrfields-ucomm eq 'ONLI'.
    if ( p_bsart ne 'YSTO' and p_bsart ne 'ZSTO' ).
      message 'Invalid document type for STO creation.' type 'S' display like 'E'.
      return.
    endif.

    if p_file is initial.
      set cursor field 'P_FILE'.
      message id '00' type 'S' number '055' display like 'E'.
      stop.
    endif.
  endif.

  if sscrfields-ucomm eq 'DWN_FORMAT'.
    call function 'CALL_BROWSER'
      exporting
        url                    = 'https://goo.gl/tfvs5g'      " File download url, google drive link
        window_name            = 'ZMM085_File_format'
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
  endif.

  " start of selection - begin main

start-of-selection.
  main=>start( ).
