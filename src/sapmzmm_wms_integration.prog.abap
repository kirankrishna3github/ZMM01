*&---------------------------------------------------------------------*
*& Modulpool SAPMZMM_WMS_INTEGRATION
*&---------------------------------------------------------------------*
*&  To-do create structure in SAP DDIC: date, time, user, plant, stor-loc, host_to_wms
*&---------------------------------------------------------------------*
program sapmzmm_wms_integration message-id z_wms.

" top
class lcl_application definition deferred.

data: gv_entry_date type mkpf-cpudt,
      gv_material   type mara-matnr,
      ok_code       type sy-ucomm,  " ucomm for 100 and 1001
      ok_code_1002  type sy-ucomm,
      ok_code_1003  type sy-ucomm,
      lo_app        type ref to lcl_application,
      gs_usr05      type usr05.

selection-screen begin of screen 1001 as subscreen.
selection-screen begin of block sel with frame title text-sel.
parameters: p_werks type marc-werks modif id sel, " obligatory,
            p_lgort type mard-lgort modif id sel. " obligatory.
selection-screen end of block sel.
selection-screen begin of block opt with frame title text-opt.
parameters: r1 radiobutton group rad modif id mst,              " Material Master SAP
            r2 radiobutton group rad modif id rad,              " Warehouse Data
*            r3 radiobutton group rad modif id rad,              " Inventory Stock -> Required?
            r3 radiobutton group rad modif id usr.              " WMS User Master
selection-screen end of block opt.
selection-screen end of screen 1001.

selection-screen begin of screen 1002 as window.
selection-screen begin of block cdt with frame title text-cdt.
select-options: s_cpudt for gv_entry_date obligatory.
selection-screen end of block cdt.
selection-screen end of screen 1002.

selection-screen begin of screen 1003 as window.
selection-screen begin of block mat with frame title text-mat.
select-options: s_matnr for gv_material obligatory.
parameters: p_labor type mara-labor obligatory default '011',
            p_meinh type marm-meinh obligatory default 'EA',
            p_umrez type marm-umrez obligatory.
selection-screen end of block mat.
selection-screen begin of line.
selection-screen comment 1(78) text-con.
selection-screen end of line.
selection-screen end of screen 1003.

class lcl_application definition final.
  public section.
    methods: constructor, modif_screen, get_ucomm, process,
      add_log
        importing
          msgty type bal_s_msg-msgty
          msgid type bal_s_msg-msgid
          msgno type bal_s_msg-msgno
          msgv1 type bal_s_msg-msgv1 optional
          msgv2 type bal_s_msg-msgv2 optional
          msgv3 type bal_s_msg-msgv3 optional
          msgv4 type bal_s_msg-msgv4 optional,
      add_log_text
        importing
          msgty   type bal_s_msg-msgty
          msg_txt type string,
      display_log.

  protected section.

  private section.
    methods: get_log_handle,
      init_wms_connection,
      update_master_data,
      update_wms_in_out,
      update_wms_user_master,
      get_next_sequence returning value(rv_next_seq) type zhost_to_wms-msg_rec_id,
      get_messages,
      update_wms_log.

    types: c3 type c length 3.

    data: gs_log_handle type balloghndl,   "  Application Log: Log Handle
          gs_log        type bal_s_log,    "  Log header data
          gt_msg_handle type standard table of balmsghndl.  " Table of added messages

    data: gv_con_name    type dbcon-con_name,
          gv_schema_name type adbc_schema_name,
          gv_table_name  type adbc_name.

    data: go_wms_con type ref to zcl_sql_wms.

    data: gv_processed  type flag,
          gv_ok_execute type flag.

    " constants
    constants: gc_host      type zhost_to_wms-msg_src value 'HOST',
               gc_def       type zhost_to_wms-msg_stat value 'DEF',
               gc_container type zhost_to_wms-msg_trans_type value 'CON',
               gc_master    type zhost_to_wms-msg_trans_type value 'MASTER',
               gc_inward    type zhost_to_wms-msg_trans_type value 'IN',
               gc_outward   type zhost_to_wms-msg_trans_type value 'OUT'.

endclass.

class lcl_application implementation.
  method constructor.
    get_log_handle( ).
  endmethod.

  method get_log_handle.
    clear: gs_log_handle, gs_log.
    refresh gt_msg_handle.
    " create log handle
    gs_log-extnumber  = 'WMS Integration: Application Log'.
    gs_log-object     = 'Z_WMS'.
    gs_log-subobject  = 'Z_WMS_SUB'.
    gs_log-aldate     = sy-datum.
    gs_log-altime     = sy-uzeit.
    gs_log-aluser     = sy-uname.
    gs_log-alprog     = sy-repid.

    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log                 = gs_log
      importing
        e_log_handle            = gs_log_handle
      exceptions
        log_header_inconsistent = 1
        others                  = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endmethod.

  method init_wms_connection.
    free go_wms_con.
    if gv_con_name is not initial and gv_schema_name is not initial and gv_table_name is not initial.

      cl_progress_indicator=>progress_indicate(
        exporting
          i_text = 'Connecting to WMS system...'
          i_processed = 1
          i_total = 2 ).

      " IHDK903408
      try.
          go_wms_con = new zcl_sql_wms(
                            iv_con_name   = to_upper( gv_con_name )
                            iv_schema     = to_upper( gv_schema_name )
                            iv_db_tabname = to_upper( gv_table_name ) ).
        catch cx_sql_exception.
          free go_wms_con.
      endtry.

      if not go_wms_con is bound.
        message s003 with gv_con_name display like 'E'.
      else.
        message s004 with gv_con_name.
      endif.
    endif.
  endmethod.

  method add_log.
    data: ls_msg           type bal_s_msg,
          ls_msg_handle    type balmsghndl,
          lv_msg_logged    type boolean,
          lv_msg_displayed type boolean.
* define data of message for Application Log
    clear ls_msg.
    ls_msg-msgty = msgty.
    ls_msg-msgid = msgid.
    ls_msg-msgno = msgno.
    ls_msg-msgv1 = msgv1.
    ls_msg-msgv2 = msgv2.
    ls_msg-msgv3 = msgv3.
    ls_msg-msgv4 = msgv4.

    case ls_msg-msgty.
      when 'E' or 'A'.
        ls_msg-probclass = 1.
      when 'W'.
        ls_msg-probclass = 2.
      when others.
        ls_msg-probclass = 3.
    endcase.

    clear: ls_msg_handle, lv_msg_logged, lv_msg_displayed.
    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle        = gs_log_handle
        i_s_msg             = ls_msg
      importing
        e_s_msg_handle      = ls_msg_handle
        e_msg_was_logged    = lv_msg_logged
        e_msg_was_displayed = lv_msg_displayed
      exceptions
        log_not_found       = 1
        msg_inconsistent    = 2
        log_is_full         = 3
        others              = 4.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      append ls_msg_handle to gt_msg_handle.
    endif.
  endmethod.

  method add_log_text.
    data: i_probclass      type balprobcl,
          ls_msg_handle    type balmsghndl,
          lv_msg_logged    type boolean,
          lv_msg_displayed type boolean.

    clear i_probclass.
    case msgty.
      when 'E' or 'A'.
        i_probclass = 1.
      when 'W'.
        i_probclass = 2.
      when others.
        i_probclass = 3.
    endcase.

    clear: ls_msg_handle, lv_msg_logged, lv_msg_displayed.
    call function 'BAL_LOG_MSG_ADD_FREE_TEXT'
      exporting
        i_log_handle        = gs_log_handle
        i_msgty             = msgty
        i_probclass         = i_probclass
        i_text              = conv bapi_msg( msg_txt )
      importing
        e_s_msg_handle      = ls_msg_handle
        e_msg_was_logged    = lv_msg_logged
        e_msg_was_displayed = lv_msg_displayed
      exceptions
        log_not_found       = 1
        msg_inconsistent    = 2
        log_is_full         = 3
        others              = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      append ls_msg_handle to gt_msg_handle.
    endif.

  endmethod.

  method display_log.
    data:ls_display_profile type bal_s_prof,
         lt_log_handle      type bal_t_logh.

    " first save the log to db, then display it to the user
    refresh lt_log_handle.
    append gs_log_handle to lt_log_handle.

    call function 'BAL_DB_SAVE'
      exporting
        i_save_all       = abap_true
        i_t_log_handle   = lt_log_handle
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

* get standard display profile
    clear ls_display_profile.
    call function 'BAL_DSP_PROFILE_STANDARD_GET'
      importing
        e_s_display_profile = ls_display_profile
      exceptions
        others              = 1.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    ls_display_profile-use_grid = abap_true.
    ls_display_profile-disvariant-report = sy-repid.

    call function 'BAL_DSP_LOG_DISPLAY'
      exporting
        i_s_display_profile  = ls_display_profile
        i_t_log_handle       = lt_log_handle
      exceptions
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        others               = 5.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    refresh gt_msg_handle.
    call function 'BAL_LOG_MSG_DELETE_ALL'
      exporting
        i_log_handle  = gs_log_handle
      exceptions
        log_not_found = 1
        others        = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    call function 'BAL_LOG_REFRESH'
      exporting
        i_log_handle  = gs_log_handle
      exceptions
        log_not_found = 1
        others        = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    clear gs_log_handle.

  endmethod.

  method modif_screen.
    case sy-dynnr.
      when 1001.
        loop at screen.
          if ( p_werks is initial or p_lgort is initial ).
            if screen-group1 eq 'RAD' or screen-group1 eq 'MST'.
              screen-active = 0.
              screen-input = 0.
              screen-invisible = 1.
              modify screen.
            endif.
          else.
            if screen-group1 eq 'SEL'.
              screen-input = 0.
              modify screen.
            endif.
          endif.

          if gs_usr05 is initial.
            if screen-group1 eq 'MST' or screen-group1 eq 'USR'.
              screen-active = 0.
              screen-input = 0.
              screen-invisible = 1.
              modify screen.
            endif.
          endif.
        endloop.
      when 1002.
      when 1003.
      when others.
    endcase.
  endmethod.

  method get_ucomm.
    clear: ok_code_1002, ok_code_1003.
    case sy-dynnr.
      when 1001.
        if not r3 eq abap_true or ( p_werks is not initial and p_lgort is not initial ).
          if p_werks is initial.
            set cursor field 'P_WERKS'.
            message id '00' type 'E' number '055'.
          endif.
          if p_lgort is initial.
            set cursor field 'P_LGORT'.
            message id '00' type 'E' number '055'.
          endif.

          " execution reaches this point only when both werks and lgort are filled
          select single *
            from zmm_t_wms_master
            into @data(ls_wms_master)
            where werks = @p_werks
            and   lgort = @p_lgort
            and   uname = @sy-uname.

          if ls_wms_master is initial.
            select single *
              from zmm_t_wms_master
              into @data(ls_check_wms_existence)
              where werks = @p_werks
              and   lgort = @p_lgort.
            if ls_check_wms_existence is not initial.
              message e005 with p_werks p_lgort.
            else.
              if r3 eq abap_true.
                message i016.
              endif.
              message e014 with p_werks p_lgort.
            endif.
          elseif ls_wms_master is not initial.
            clear: gv_con_name, gv_schema_name, gv_table_name.
            gv_con_name    = ls_wms_master-con_name.
            gv_schema_name = ls_wms_master-db_schema.
            gv_table_name  = ls_wms_master-tab_name.
          endif.
        elseif r3 eq abap_true and
          ( ( p_werks is not initial and p_lgort is initial ) or ( p_werks is initial and p_lgort is not initial ) ).
          message i016 display like 'E'.
          message e015.
        endif.
      when 1002.
        ok_code_1002 = sy-ucomm.
      when 1003.
        ok_code_1003 = sy-ucomm.
      when others.
    endcase.
  endmethod.

  method process.
    clear: gv_processed, gv_ok_execute.
    try.  " IHDK900177
        case abap_true.
          when r1.
            init_wms_connection( ).
            update_master_data( ).
          when r2.
            init_wms_connection( ).
            update_wms_in_out( ).
          when r3.
            update_wms_user_master( ).
          when others.
        endcase.

        if go_wms_con is bound.
          get_messages( ).
          go_wms_con->cleanup( ).
          free go_wms_con.
          if gv_processed eq abap_false and gv_ok_execute eq abap_true.
            message s017 display like 'E'.
          endif.
          update_wms_log( ).
        endif.

        if gt_msg_handle is not initial.
          display_log( ).
        endif.
      catch cx_root.
    endtry.
  endmethod.

  method update_master_data.
    " Flow logic -----
    " first update container master, and push to oracle
    " then update material master sap(mm02)
    " once that is successful, update material master wms and push to oracle

    data: ls_headdata             type bapimathead,               " View Selection
          ls_clientdata           type bapi_mara,                 " MARA
          ls_clientdatax          type bapi_marax,                " Checkbox
          ls_storagelocationdata  type bapi_mard,   " MARD
          ls_storagelocationdatax type bapi_mardx,  " Checkbox
          lt_uom                  type table of bapi_marm,        " MARM
          ls_uom                  type bapi_marm,
          lt_uomx                 type table of bapi_marmx,       " Checkbox
          ls_uomx                 type bapi_marmx,
          lt_return_mat           type table of bapi_matreturn2,  " Return messages from BAPI
          ls_return_mat           type bapi_matreturn2,
          ls_return               type bapiret2.

    data: ls_wms_main    type zmm_t_wms_main,
          ls_host_to_wms type zhost_to_wms.

    call selection-screen 1003 starting at 10 10.
    check ok_code_1003 eq 'CRET'.
    gv_ok_execute = abap_true.

    " first update container master, and push to oracle and equivalent sap z table
    " Flow logic ----
    " No need to check for repeat entry since even if we push duplicates from SAP, WMS system ignores such entries or updates wherever required
    " So pushing duplicates is fine
    clear: ls_wms_main, ls_host_to_wms.

    " fill log table key fields
    ls_wms_main-werks = p_werks.
    ls_wms_main-lgort = p_lgort.
    ls_wms_main-datum = sy-datum.
    ls_wms_main-uzeit = sy-uzeit.
    get time stamp field ls_wms_main-timestamp.
    ls_wms_main-ernam = sy-uname.

    " fill container specific fields
    ls_wms_main-msg_src = gc_host.
    ls_wms_main-msg_rec_id = get_next_sequence( ).
    ls_wms_main-msg_trans_type = gc_container.
    ls_wms_main-msg_dt_def = zcl_sql_wms=>convert_date_to_oracle( exporting iv_sap_date = sy-datum ).
    ls_wms_main-msg_stat = gc_def.
    ls_wms_main-container = |K{ conv c3( p_umrez ) alpha = in }|.
    ls_wms_main-container_wt = p_umrez.

    move-corresponding ls_wms_main to ls_host_to_wms.

    select single container
      from zmm_t_wms_main
      into @data(container_pushed)
      where werks = @p_werks
      and   lgort = @p_lgort
      and   container = @ls_wms_main-container
      and   msg_stat eq 'TRM'.

    if container_pushed is initial.
      data(lv_rows_processed) = go_wms_con->execute( exporting is_data = ls_host_to_wms ).

      if lv_rows_processed = 1.
        gv_processed = abap_true.
        insert zmm_t_wms_main from ls_wms_main.
        if sy-dbcnt eq 1.
          commit work.
        endif.

        add_log(
          exporting
            msgty = 'S'
            msgid = 'Z_WMS'
            msgno = '012'
            msgv1 = conv bal_s_msg-msgv1( ls_host_to_wms-msg_rec_id )
            msgv2 = conv bal_s_msg-msgv2( ls_host_to_wms-container )
            ).

      else.

        add_log(
          exporting
            msgty = 'E'
            msgid = 'Z_WMS'
            msgno = '013'
            msgv1 = conv bal_s_msg-msgv1( ls_host_to_wms-msg_rec_id )
            msgv2 = conv bal_s_msg-msgv2( ls_host_to_wms-container )
            ).
      endif.
    endif.
    clear container_pushed. " IHDK900181

*    msg_src = HOST
*    msg_rec_id = generate using sequence
*    msg_trans_type = CON
*    msg_dt_def = DD-MM-YYYY use class
*    msg_stat = DEF
*    container = K && alpha in 3 char p_umrez
*    container_wt = p_umrez

    " then update material master sap(mm02)
    if condense( s_matnr-low ) ne '*'.
      select a~matnr as material, a~mtart as mat_type, a~labor as lab_office, b~maktx as descr
        from mara as a
        inner join makt as b
        on a~matnr = b~matnr
        into table @data(lt_material)
        where a~matnr in @s_matnr
        and   b~spras eq @sy-langu
        order by material.

      if lt_material is not initial.

        select matnr as material, meinh as alt_unit, umrez as num, umren as den
          from marm
          into table @data(lt_mat_uom)
          for all entries in @lt_material
          where matnr = @lt_material-material
          order by primary key.

        " add logic for storage location(from sel screen) if not yet extended
        select matnr as material, werks as plant, lgort as sloc
          from mard
          into table @data(lt_mat_sloc)
          for all entries in @lt_material
          where matnr = @lt_material-material
          order by primary key.

        loop at lt_material into data(ls_material).
          try.
              data(ls_mat_uom) = lt_mat_uom[ material = ls_material-material alt_unit = p_meinh num = p_umrez den = 1 ] .
            catch cx_sy_itab_line_not_found.  " read failure
          endtry.

          " headdata
          clear ls_headdata.
          ls_headdata-material = ls_material-material.
          move 'C'            to ls_headdata-ind_sector.    " MBRSH
          ls_headdata-matl_type = ls_material-mat_type.
          move abap_true      to ls_headdata-basic_view.
          move 'I'            to ls_headdata-inp_fld_check.

          if ls_material-lab_office ne p_labor.
            " mara
            clear ls_clientdata.
            ls_clientdata-dsn_office = p_labor.
            clear ls_clientdatax.
            ls_clientdatax-dsn_office = abap_true.
          endif.

          if ls_mat_uom is initial.
            " marm
            refresh: lt_uom, lt_uomx.
            clear ls_uom.
            ls_uom-alt_unit = p_meinh.
            ls_uom-numerator = p_umrez.
            ls_uom-denominatr = 1.
            append ls_uom to lt_uom.

            clear ls_uomx.
            ls_uomx-alt_unit = p_meinh.
            ls_uomx-numerator = abap_true.
            ls_uomx-denominatr = abap_true.
            append ls_uomx to lt_uomx.
          endif.

          try .
              data(ls_mat_sloc) = lt_mat_sloc[ material = ls_material-material plant = p_werks sloc = p_lgort ].
            catch cx_sy_itab_line_not_found.  " not yet extended to p_lgort
              " mark the required views
              ls_headdata-mrp_view     = abap_true.
              ls_headdata-storage_view = abap_true.
              clear ls_headdata-basic_view.

              ls_storagelocationdata-plant = p_werks.
              ls_storagelocationdata-stge_loc = p_lgort.

              zcl_helper=>fill_x_fields(
                exporting
                  data = ls_storagelocationdata
                changing
                  datax = ls_storagelocationdatax ).
          endtry.

          refresh: lt_return_mat.
          clear: ls_return_mat, ls_return.
          if ls_clientdata is not initial or ls_storagelocationdata is not initial or lt_uom is not initial.
            call function 'BAPI_MATERIAL_SAVEDATA'
              exporting
                headdata             = ls_headdata
                storagelocationdata  = ls_storagelocationdata   " IHDK901861
                storagelocationdatax = ls_storagelocationdatax
              importing
                return          = ls_return
              tables
                returnmessages  = lt_return_mat.
            try.
                if line_exists( lt_return_mat[ type = 'E' ] ) or line_exists( lt_return_mat[ type = 'A' ] ).
                  call function 'BAPI_TRANSACTION_ROLLBACK'.
                elseif ls_return-number = '356' and ls_return-type = 'S'.
                  call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true.
                  add_log(
                    exporting
                      msgty = ls_return-type
                      msgid = ls_return-id
                      msgno = ls_return-number
                      msgv1 = ls_return-message_v1
                      msgv2 = ls_return-message_v2
                      msgv3 = ls_return-message_v3
                      msgv4 = ls_return-message_v4 ).
                endif.
                loop at lt_return_mat into ls_return_mat.
                  add_log(
                    exporting
                      msgty = ls_return_mat-type
                      msgid = ls_return_mat-id
                      msgno = ls_return_mat-number
                      msgv1 = ls_return_mat-message_v1
                      msgv2 = ls_return_mat-message_v2
                      msgv3 = ls_return_mat-message_v3
                      msgv4 = ls_return_mat-message_v4 ).

                  clear: ls_return_mat.
                endloop.
              catch cx_sy_itab_line_not_found.
            endtry.
          else.
            add_log(
              exporting
                msgty = 'W'
                msgid = 'Z_WMS'
                msgno = '001'
                msgv1 = conv bal_s_msg-msgv1( ls_material-material )
              ).
          endif.

          " once that is done, update material master wms and push to oracle and equivalent sap z table
          clear: ls_wms_main, ls_host_to_wms, lv_rows_processed.

          " fill log table key fields
          ls_wms_main-werks = p_werks.
          ls_wms_main-lgort = p_lgort.
          ls_wms_main-datum = sy-datum.
          ls_wms_main-uzeit = sy-uzeit.
          get time stamp field ls_wms_main-timestamp.
          ls_wms_main-ernam = sy-uname.

          " fill material master specific fields
          ls_wms_main-msg_src = gc_host.
          ls_wms_main-msg_rec_id = get_next_sequence( ).
          ls_wms_main-msg_trans_type = gc_master.
          ls_wms_main-msg_dt_def = zcl_sql_wms=>convert_date_to_oracle( exporting iv_sap_date = sy-datum ).
          ls_wms_main-msg_stat = gc_def.
          ls_wms_main-item_code = ls_material-material.
          ls_wms_main-description = ls_material-descr.
          ls_wms_main-mat_cat = '00'.
          ls_wms_main-mat_type = ls_material-mat_type.

          move-corresponding ls_wms_main to ls_host_to_wms.

          select single item_code
            from zmm_t_wms_main
            into @data(item_code_pushed)
            where werks = @p_werks
            and   lgort = @p_lgort
            and   item_code = @ls_wms_main-item_code
            and   msg_stat eq 'TRM'.

          if item_code_pushed is initial.
            lv_rows_processed = go_wms_con->execute( exporting is_data = ls_host_to_wms ).

            if lv_rows_processed = 1.
              gv_processed = abap_true.
              insert zmm_t_wms_main from ls_wms_main.
              if sy-dbcnt eq 1.
                commit work.
              endif.

              add_log(
                  exporting
                    msgty = 'S'
                    msgid = 'Z_WMS'
                    msgno = '010'
                    msgv1 = conv bal_s_msg-msgv1( ls_host_to_wms-msg_rec_id )
                    msgv2 = conv bal_s_msg-msgv2( ls_material-material )
                    ).
            else.
              add_log(
                  exporting
                    msgty = 'E'
                    msgid = 'Z_WMS'
                    msgno = '011'
                    msgv1 = conv bal_s_msg-msgv1( ls_host_to_wms-msg_rec_id )
                    msgv2 = conv bal_s_msg-msgv2( ls_material-material )
                    ).
            endif.
          endif.

*            msg_src = HOST
*            msg_rec_id = generate using sequence
*            msg_trans_type = MASTER
*            msg_dt_def = DD-MM-YYYY use class
*            msg_stat = DEF
*            item_code =
*            description
*            mat_cat
*            mat_type

          clear: ls_material, ls_mat_uom, item_code_pushed. " IHDK900181
        endloop.
      else.
        add_log(
          exporting
            msgty = 'E'
            msgid = 'Z_WMS'
            msgno = '002'
            ).
      endif.
    endif.
  endmethod.

  method update_wms_in_out.
    data: ls_wms_main    type zmm_t_wms_main,
          ls_host_to_wms type zhost_to_wms.
    call selection-screen 1002 starting at 10 10.
    check ok_code_1002 eq 'CRET'.
    gv_ok_execute = abap_true.

    " Only consider materials for which master data has been maintained
    select a~matnr as material, a~mtart as mat_type, a~labor as lab_office, b~maktx as descr
      from mara as a
      inner join makt as b
      on a~matnr = b~matnr
      into table @data(lt_material)
      where a~matnr in @s_matnr
      and   a~labor in ( '011' )  " might need to add more values here in future
      and   b~spras eq @sy-langu
      order by material.

    if lt_material is not initial.

      select matnr as material, meinh as alt_unit, umrez as num, umren as den
        from marm
        into table @data(lt_mat_uom)
        for all entries in @lt_material
        where matnr = @lt_material-material
        and   meinh in ( 'EA' ) " might need to add more values here in future
        and   umrez is not null
        order by primary key.

      data: lr_matnr type range of matnr,
            ls_matnr like line of lr_matnr.

      refresh lr_matnr. clear ls_matnr.

      loop at lt_mat_uom into data(ls_mat_uom).
        clear ls_matnr.
        ls_matnr-sign   = 'I'.
        ls_matnr-option = 'EQ'.
        ls_matnr-low    = ls_mat_uom-material.
        ls_matnr-high   = ''.
        append ls_matnr to lr_matnr.
        clear ls_mat_uom.
      endloop.

      sort lr_matnr by low.
      delete adjacent duplicates from lr_matnr comparing low.

      if lr_matnr is not initial. " at least some materials with correct master data found
        select mblnr,
               mjahr,
               budat,
               cpudt,
               zeile,
               bwart,
               matnr,
               lgort,
               charg,
               shkzg,
               menge,
               meins,
               sjahr,
               smbln
          from matdoc
          into table @data(lt_mat_doc)
          where cpudt in @s_cpudt
          and   matnr in @lr_matnr
          and   werks eq @p_werks
          and   lgort eq @p_lgort
          and   smbln eq ''
          and   sjahr eq ''
          and   bwart in ( '311', '601', '641', '701', '702' ).

        if lt_mat_doc is not initial.
          select distinct matnr, charg, vfdat
            from mch1
            into table @data(lt_batch)
            for all entries in @lt_mat_doc
            where matnr = @lt_mat_doc-matnr
            and   charg = @lt_mat_doc-charg.

          loop at lt_mat_doc into data(ls_mat_doc).
            clear: ls_wms_main, ls_host_to_wms.

            " fill log table key fields
            ls_wms_main-werks = p_werks.
            ls_wms_main-lgort = p_lgort.
            ls_wms_main-datum = sy-datum.
            ls_wms_main-uzeit = sy-uzeit.
            get time stamp field ls_wms_main-timestamp.
            ls_wms_main-ernam = sy-uname.

            " identify inward or outward
            if ( ls_mat_doc-bwart eq '311' or ls_mat_doc-bwart eq '701' ) and ls_mat_doc-shkzg eq 'S'.  " this is inward
              data(lv_inward_flg) = abap_true.
            endif.

            if ( ls_mat_doc-bwart eq '601' or ls_mat_doc-bwart eq '641' or ls_mat_doc-bwart eq '702' ) or
              ( ls_mat_doc-bwart eq '311' and ls_mat_doc-shkzg eq 'H' ).  " this is outward
              data(lv_outward_flg) = abap_true.
            endif.

            " update common fields
            ls_wms_main-msg_src = gc_host.
            ls_wms_main-msg_rec_id = get_next_sequence( ).
            ls_wms_main-msg_dt_def = zcl_sql_wms=>convert_date_to_oracle( exporting iv_sap_date = sy-datum ).
            ls_wms_main-msg_stat = gc_def.

            " item_code, batch_no, qty, container
            ls_wms_main-item_code = ls_mat_doc-matnr.
            ls_wms_main-batch = ls_mat_doc-charg.
            ls_wms_main-quantity = ls_mat_doc-menge.

            try.
                ls_wms_main-container = |K{ conv c3( lt_mat_uom[ material = ls_mat_doc-matnr ]-num ) alpha = in }|.
              catch cx_sy_itab_line_not_found.  " #EC_NO_HANDLER
            endtry.

            " fields specific to transaction type
            if lv_inward_flg eq abap_true.

              ls_wms_main-msg_trans_type = gc_inward.
              ls_wms_main-gr_no = ls_mat_doc-mblnr && ls_mat_doc-zeile && ls_mat_doc-mjahr.
              try.
                  ls_wms_main-expiry_date =
                  zcl_sql_wms=>convert_date_to_oracle( exporting iv_sap_date = lt_batch[ matnr = ls_mat_doc-matnr charg = ls_mat_doc-charg ]-vfdat ).
                catch cx_sy_itab_line_not_found.  " #EC_NO_HANDLER
              endtry.

              select single gr_no
                from zmm_t_wms_main
                into @data(gr_no_pushed)
                where werks = @p_werks
                and   lgort = @p_lgort
                and   gr_no = @ls_wms_main-gr_no
                and   msg_stat eq 'TRM'.
            elseif lv_outward_flg eq abap_true.

              ls_wms_main-msg_trans_type = gc_outward.
              ls_wms_main-docu_no = ls_mat_doc-mblnr && ls_mat_doc-zeile && ls_mat_doc-mjahr.

              select single docu_no
                from zmm_t_wms_main
                into @data(docu_no_pushed)
                where werks = @p_werks
                and   lgort = @p_lgort
                and   docu_no = @ls_wms_main-docu_no
                and   msg_stat eq 'TRM'.
            endif.

            " suggest mseg key concat for grno or docu no
            " what happens if duplicate entries are pushed
            " no key field defined
            " what happens if unknown/new material or container is pushed in the transaction without pushing master first
            if gr_no_pushed is initial and docu_no_pushed is initial.
              move-corresponding ls_wms_main to ls_host_to_wms.

              data(lv_rows_processed) = go_wms_con->execute( exporting is_data = ls_host_to_wms ).

              if lv_rows_processed = 1.
                gv_processed = abap_true.
                insert zmm_t_wms_main from ls_wms_main.
                if sy-dbcnt eq 1.
                  commit work.
                endif.

                add_log(
                  exporting
                    msgty = 'S'
                    msgid = 'Z_WMS'
                    msgno = '008'
                    msgv1 = conv bal_s_msg-msgv1( ls_host_to_wms-msg_rec_id )
                    msgv2 = conv bal_s_msg-msgv2( ls_mat_doc-mblnr && ls_mat_doc-zeile && ls_mat_doc-mjahr )
                    msgv3 = conv bal_s_msg-msgv3( ls_host_to_wms-msg_trans_type )
                    ).
              else.
                add_log(
                  exporting
                    msgty = 'E'
                    msgid = 'Z_WMS'
                    msgno = '009'
                    msgv1 = conv bal_s_msg-msgv1( ls_host_to_wms-msg_rec_id )
                    msgv2 = conv bal_s_msg-msgv2( ls_mat_doc-mblnr && ls_mat_doc-zeile && ls_mat_doc-mjahr )
                    msgv3 = conv bal_s_msg-msgv3( ls_host_to_wms-msg_trans_type )
                    ).
              endif.
            endif.

            clear: ls_mat_doc, lv_inward_flg, lv_outward_flg, lv_rows_processed, docu_no_pushed, gr_no_pushed.  " IHDK900181
          endloop.
        else.
          " log error here for no material documents found
          message s007 display like 'E'.
        endif.
      else.
        " log error here for no materials found
        message s006 display like 'E'.
      endif.
    else.
      " log error here for no materials found
      message s006 display like 'E'.
    endif.
    " in
*      msg_src = HOST
*      msg_rec_id = generate using sequence
*      msg_trans_type = IN
*      msg_dt_def = DD-MM-YYYY use class
*      msg_stat = DEF
*      gr_no
*      item_code
*      batch_no
*      qty
*      exp_date
*      container
    " push to oracle and update equivalent sap z table

    " out
*      msg_src = HOST
*      msg_rec_id = generate using sequence
*      msg_trans_type = OUT
*      msg_dt_def = DD-MM-YYYY use class
*      msg_stat = DEF
*      docu_no
*      item_code
*      batch_no
*      qty
*      container
    " push to oracle and update equivalent sap z table
  endmethod.

  method update_wms_user_master.

    call function 'VIEW_MAINTENANCE_CALL'
      exporting
        action                       = 'U'
        view_name                    = 'ZMM_T_WMS_MASTER'
      exceptions
        client_reference             = 1
        foreign_lock                 = 2
        invalid_action               = 3
        no_clientindependent_auth    = 4
        no_database_function         = 5
        no_editor_function           = 6
        no_show_auth                 = 7
        no_tvdir_entry               = 8
        no_upd_auth                  = 9
        only_show_allowed            = 10
        system_failure               = 11
        unknown_field_in_dba_sellist = 12
        view_not_found               = 13
        maintenance_prohibited       = 14
        others                       = 15.
    if sy-subrc <> 0.
* Implement suitable error handling here
      add_log(
        exporting
          msgty = sy-msgty
          msgid = sy-msgid
          msgno = sy-msgno
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4 ).
    endif.

  endmethod.

  method get_next_sequence.
    check go_wms_con is bound.
    clear rv_next_seq.

    data(lo_next_seq) = go_wms_con->get_next_sequence( ).

    check lo_next_seq is bound and lo_next_seq is not initial.
    assign lo_next_seq->* to field-symbol(<fv_next_seq>).

    check <fv_next_seq> is assigned and <fv_next_seq> is not initial.
    rv_next_seq = conv zhost_to_wms-msg_rec_id( <fv_next_seq> ).
  endmethod.

  method get_messages.
    check go_wms_con is bound.
    data(lt_sql_message) = go_wms_con->get_messages( ).
    if lt_sql_message is not initial.
      loop at lt_sql_message into data(ls_sql_message).
        add_log_text(
          exporting
            msg_txt = conv string( ls_sql_message-method_name && ` - ` && condense( ls_sql_message-message ) )
            msgty   = ls_sql_message-msgtype ).

        clear ls_sql_message.
      endloop.
    endif.
  endmethod.

  method update_wms_log.
    data: l_jobcount type tbtcjob-jobcount,
          l_jobname  type tbtcjob-jobname.

    l_jobname = |UPDATE_WMS_LOG|.

    call function 'JOB_OPEN'  " create a job
      exporting
        jobname          = l_jobname
      importing
        jobcount         = l_jobcount
      exceptions
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        others           = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno into data(lv_msg)
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.

      submit zmm_prg_update_wms_log " submit report via job created above
            user sy-uname via job l_jobname
            number l_jobcount and return.

      call function 'JOB_CLOSE' " release the job
        exporting
          jobcount             = l_jobcount
          jobname              = l_jobname
          strtimmed            = 'X'
        exceptions
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          others               = 8.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno into lv_msg
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.
  endmethod.
endclass.

*&---------------------------------------------------------------------*
*& Module SET_STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module set_status_0100 output.
  data: lt_fcode type standard table of sy-ucomm.
  refresh lt_fcode.
  append 'ONLI' to lt_fcode.

  if ( p_werks is initial or p_lgort is initial ) and gs_usr05 is initial.
    set pf-status 'ZPF_WMS_0100' excluding lt_fcode.
  else.
    set pf-status 'ZPF_WMS_0100'.
  endif.
  set titlebar 'ZTITLE_WMS_0100'.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  HANDLE_STATUS_UCOMM_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module handle_status_ucomm_0100 input.
  case ok_code.
    when 'ONLI'.
      lo_app->process( ).
    when others.
  endcase.
  clear ok_code.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  HANDLE_EXIT_UCOMM_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module handle_exit_ucomm_0100 input.
  leave program.
endmodule.

" program events

load-of-program.
  free lo_app.
  lo_app = new lcl_application( ).
  if not lo_app is bound.
    exit.
  else.
    clear gs_usr05.
    select single * from usr05 into @gs_usr05 where bname = @sy-uname and parid = 'Z_WMS_MASTER' and parva = @abap_true.
  endif.

  " selection screen events

at selection-screen output.
  lo_app->modif_screen( ).

at selection-screen.
  lo_app->get_ucomm( ).
