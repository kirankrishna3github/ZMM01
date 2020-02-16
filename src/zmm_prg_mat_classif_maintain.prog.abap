*&---------------------------------------------------------------------*
*& Program ZMM_PRG_MAT_CLASSIF_MAINTAIN
*&---------------------------------------------------------------------*
*& Tuesday, January 07, 2020 12:16:17
*& 6010859 - SaurabhK
*& ZMM097: Fetch materials for which classif data has not been maintained and update
*& IHDK904487: MM: S_K: Maintain material classif: 6.1.20
*&---------------------------------------------------------------------*
program zmm_prg_mat_classif_maintain.

*--------------------------------------------------------------------*
* Global data
*--------------------------------------------------------------------*
" for select options
data:
  gv_matnr type mara-matnr,
  gv_mtart type mara-mtart.
*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
selection-screen begin of block sel with frame title text-sel.
select-options: s_matnr for gv_matnr,
                s_mtart for gv_mtart obligatory default 'ZFGM'.
parameters: p_class type klasse_d obligatory default 'ZSFG_FG',
            p_cltyp type klassenart obligatory default '023'.
selection-screen end of block sel.
*--------------------------------------------------------------------*
* local class definitions
*--------------------------------------------------------------------*
class lcl_app definition.
  public section.
    methods: process.

  protected section.
    " placeholder

  private section.
    data:
      begin of ms_log,
        pstat type icon_d,
        matnr type mara-matnr,
        messg type bapi_msg,
      end of ms_log,
      mt_log like standard table of ms_log with empty key.

    methods:
      get_materials_wo_classif
        returning
          value(rt_material) type matnr_tty,

      maintain_classif_data
        importing
          value(it_material) type matnr_tty,

      display_log
        changing
          ct_data type standard table,

      on_link_click
      for event
            link_click of cl_salv_events_table
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
  method process.
    data(lt_material) = get_materials_wo_classif( ).
    if lt_material is not initial.
      maintain_classif_data(
        exporting
          it_material = lt_material ).
    else.
      message 'No data found!' type 'S' display like 'E'.
    endif.
  endmethod.

  method get_materials_wo_classif.
    clear rt_material.
    select matnr
      from mara
      where matnr in @s_matnr
      and   mtart in @s_mtart
      and   pstat not like '%C%'      " classification view does not exist
      into table @rt_material.
  endmethod.

  method maintain_classif_data.
    data:
* BAPI_OBJCL_CHANGE * " Material Classification View
      " Header data, key fields
      lv_objectkey          type bapi1003_key-object,       " Material
      lv_objecttable        type bapi1003_key-objecttable,  " MARA - CONSTANT
      lv_classnum           type bapi1003_key-classnum,     " Class name/number - > Eg ZSFG
      lv_classtype          type bapi1003_key-classtype,    " Class type -> Eg 023, Batch -> TCLAT
      lv_classif_status     type bapi1003_key-status,       " Classification status after BAPI call: 1 = ok
      " Maintain specific characteristic values if required
      lt_allocvaluesnumnew  type table of bapi1003_alloc_values_num,
      lt_allocvaluescharnew type table of bapi1003_alloc_values_char,
      lt_allocvaluescurrnew type table of bapi1003_alloc_values_curr,
      lt_mat_class_ret      type table of bapiret2.

    clear mt_log.
    if it_material is not initial.
      loop at it_material into data(lv_material).
        cl_progress_indicator=>progress_indicate(
          exporting
            i_text               = |Processing material { lv_material alpha = out }|  " Progress Text (If no message transferred in I_MSG*)
            i_processed          = sy-tabix                 " Number of Objects Already Processed
            i_total              = lines( it_material )     " Total Number of Objects to Be Processed
            i_output_immediately = abap_true ).             " X = Display Progress Immediately

        clear:
          lv_objectkey,
          lv_objecttable,
          lv_classnum,
          lv_classtype,
          lv_classif_status,
          ms_log,
          lt_allocvaluesnumnew,
          lt_allocvaluescharnew,
          lt_allocvaluescurrnew,
          lt_mat_class_ret.

        move:
          lv_material to lv_objectkey,
          lv_material to ms_log-matnr,
          'MARA'      to lv_objecttable,
          p_class     to lv_classnum,
          p_cltyp     to lv_classtype.

        call function 'BAPI_OBJCL_CHANGE'
          exporting
            objectkey          = lv_objectkey
            objecttable        = lv_objecttable
            classnum           = lv_classnum
            classtype          = lv_classtype
          importing
            classif_status     = lv_classif_status
          tables
            allocvaluesnumnew  = lt_allocvaluesnumnew
            allocvaluescharnew = lt_allocvaluescharnew
            allocvaluescurrnew = lt_allocvaluescurrnew
            return             = lt_mat_class_ret.

        if line_exists( lt_mat_class_ret[ type = 'E' ] ) or lv_classif_status ne 1.
          call function 'BAPI_TRANSACTION_ROLLBACK'.
          ms_log-pstat = icon_red_light.
          try.
              ms_log-messg = lt_mat_class_ret[ type = 'E' ]-message.
            catch cx_sy_itab_line_not_found ##no_handler.
          endtry.
        else.
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847
          ms_log-pstat = icon_green_light.
          ms_log-messg = 'OK'.
        endif.

        append ms_log to mt_log.
        clear lv_material.
      endloop.

      if mt_log is not initial.
        display_log(
          changing
            ct_data = mt_log ).
      endif.
    endif.
  endmethod.

  method display_log.
    if ct_data is not initial.
      try.
          cl_salv_table=>factory(
            importing
              r_salv_table   = data(lo_alv)              " Basis Class Simple ALV Tables
            changing
              t_table        = ct_data ).

          if lo_alv is bound.
            " enable alv functions/buttons
            lo_alv->get_functions( )->set_all( exporting value = if_salv_c_bool_sap=>true ).

            " set display settings/alv look-n-feel
            data(lo_display_settings) = lo_alv->get_display_settings( ).
            if lo_display_settings is bound.
              lo_display_settings->set_striped_pattern( exporting value = if_salv_c_bool_sap=>true ).
              lo_display_settings->set_list_header( exporting value = 'Processing log...' ).
            endif.

            " modifications in auto-generated fieldcatalog
            data(lo_columns) = lo_alv->get_columns( ).
            if lo_columns is bound.
              try.
                  data(lo_column) = cast cl_salv_column_table( lo_columns->get_column(
                                                                 exporting
                                                                   columnname = 'PSTAT' ) ).
                  if lo_column is bound.
                    lo_column->set_long_text( exporting value = 'Processing Status' ).
                    lo_column->set_medium_text( exporting value = 'Proc. Status' ).
                    lo_column->set_short_text( exporting value = 'Proc.Stat.' ).
                  endif.
                catch cx_salv_not_found ##no_handler
                      cx_sy_move_cast_error ##no_handler. " ALV: General Error Class (Checked During Syntax Check)
              endtry.

              try.
                  clear lo_column.
                  lo_column = cast cl_salv_column_table( lo_columns->get_column(
                                                            exporting
                                                              columnname = 'MATNR' ) ).
                  if lo_column is bound.
                    lo_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).
                  endif.
                catch cx_salv_not_found ##no_handler
                      cx_sy_move_cast_error ##no_handler. " ALV: General Error Class (Checked During Syntax Check)
              endtry.

              try.
                  clear lo_column.
                  lo_column = cast cl_salv_column_table( lo_columns->get_column( exporting columnname = 'Message' ) ).
                  if lo_column is bound.
                    lo_column->set_long_text( exporting value = 'Processing Message' ).
                    lo_column->set_medium_text( exporting value = 'Proc. Message' ).
                    lo_column->set_short_text( exporting value = 'Proc.Msg' ).
                  endif.
                catch cx_salv_not_found ##no_handler
                      cx_sy_move_cast_error ##no_handler. " ALV: General Error Class (Checked During Syntax Check)
              endtry.
              " optimise column width as per content length
              lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
            endif.

            " set up event handler method for handling alv events like hotspot/link click
            set handler on_link_click for lo_alv->get_event( ).

            " display the alv to the user
            lo_alv->display( ).
          endif.
        catch cx_salv_msg ##no_handler. " ALV: General Error Class with Message
      endtry.
    endif.
  endmethod.

  method on_link_click.
    try.
        data(ls_log) = mt_log[ row ].

        case column.
          when 'MATNR'.
            data: lv_material type bapimatall-material,
                  ls_return   type bapiret1.

            lv_material = ls_log-matnr.
            call function 'BAPI_MATERIAL_DISPLAY'
              exporting
                material = lv_material
              importing
                return   = ls_return.

            if ls_return is not initial.
              message ls_return-message type 'S' display like 'E'.
            endif.
        endcase.
      catch cx_sy_itab_line_not_found ##no_handler.
    endtry.
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
  " placeholder

at selection-screen.
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
