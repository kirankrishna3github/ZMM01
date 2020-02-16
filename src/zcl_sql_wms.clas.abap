class ZCL_SQL_WMS definition
  public
  final
  create public .

public section.

  types:
    begin of ty_message,
        method_name type seomtdname,
        message     type bapiret2-message,
        msgtype     type bapiret2-type,
      end of   ty_message .
  types:
    tty_messages type standard table of ty_message with default key .

  methods CONSTRUCTOR
    importing
      value(IV_DB_TABNAME) type ADBC_NAME
      value(IV_CON_NAME) type DBCON-CON_NAME
      value(IV_SCHEMA) type ADBC_SCHEMA_NAME
    raising
      CX_SQL_EXCEPTION .
  methods EXECUTE
    importing
      value(IS_DATA) type ANY
    returning
      value(RV_ROWS_PROCESSED) type I .
  methods GET_MESSAGES
    returning
      value(RT_MESSAGES) type TTY_MESSAGES .
  methods CLEANUP .
  class-methods CONVERT_DATE_TO_ORACLE
    importing
      value(IV_SAP_DATE) type SYST-DATUM
    returning
      value(RV_ORACLE_DATE) type CHAR20 .
  methods SELECT_ALL
    returning
      value(RT_DATA) type ref to DATA .
  class-methods FORMAT_METADATA
    importing
      value(IT_COL_METADATA) type ADBC_TABCOL_DESCR_TAB optional
    changing
      value(IT_METADATA) type ADBC_RS_METADATA_DESCR_TAB .
  class-methods CONVERT_DATE_TO_WMS
    importing
      value(IV_SAP_DATE) type SYST-DATUM
    returning
      value(RV_WMS_DATE) type CHAR20 .
  methods GET_NEXT_SEQUENCE
    returning
      value(RO_REC_ID) type ref to DATA .
  methods DELETE
    importing
      value(IS_DATA) type ANY
    returning
      value(RV_DELETE_OK) type ABAP_BOOL .
  methods SELECT_WHERE
    importing
      value(IS_DATA) type ANY
    returning
      value(RT_DATA) type ref to DATA .
  methods UPDATE
    importing
      value(IS_DATA) type ANY
    returning
      value(RV_UPDATE_OK) type ABAP_BOOL .
  class-methods CHECK_WMS_TRANS_STATUS .
  protected section.
private section.

  types:
    begin of ls_col_name,
      column_name type adbc_name,
    end of ls_col_name .

  constants:
    begin of gc_wms,
      oracle_dbms type dbcon-dbms value if_mdc_sql=>dbms-oracle,
      select      type string value if_mdc_sql=>statement-select,
      from        type string value if_mdc_sql=>statement-from,
      where       type string value if_mdc_sql=>statement-where,
      insert      type string value if_mdc_sql=>statement-insert,
      update      type string value if_mdc_sql=>statement-update,
      delete      type string value if_mdc_sql=>statement-delete,
      and         type string value 'and',
      into        type string value 'into',
      values      type string value 'values',
      set         type string value 'set',
      nextval     type string value 'nextval',
      sequence    type string value 'SEQ_REC_ID',
    end of gc_wms .
  data:
    begin of gs_wms,
          con_name    type dbcon_name,
          schema      type adbc_schema_name,
        end of gs_wms .
  data GO_CON type ref to CL_SQL_CONNECTION .
  data GO_META type ref to CL_SQL_METADATA .
  data GV_DB_RELEASE type STRING .
  data GS_MESSAGE type TY_MESSAGE .
  data GT_MESSAGES type TTY_MESSAGES .
  data GS_TABLE_DESCR type ADBC_TABLE_DESCR .
  data GV_DB_TABNAME type ADBC_NAME .
  data GT_COLUMN type ADBC_TABCOL_DESCR_TAB .
  data GT_KEY type ADBC_COLUMN_TAB .
  data:
    gt_col_name type standard table of ls_col_name .
  data:
    gt_key_col_name type standard table of ls_col_name .
  data:
    gt_non_key_col_name type standard table of ls_col_name .
  data GT_METADATA type ADBC_RS_METADATA_DESCR_TAB .
  data GT_KEY_METADATA type ADBC_RS_METADATA_DESCR_TAB .
  data GT_NON_KEY_METADATA type ADBC_RS_METADATA_DESCR_TAB .
  data GO_STRUCT type ref to DATA .
  data GO_KEY_STRUCT type ref to DATA .
  data GO_TABLE type ref to DATA .
  data GO_NON_KEY_STRUCT type ref to DATA .
  constants:
    begin of gc_operation,
      where  type c length 1 value 'W',
      insert type c length 1 value 'I',
      update type c length 1 value 'U',
    end of gc_operation .

  methods GET_CONNECTION .
  methods CLEAR_GLOBALS .
  methods GET_METADATA_REF .
  methods HANDLE_EXCEPTION
    importing
      value(IX) type ref to CX_ROOT .
  methods CHECK_AUTHORITY
    returning
      value(RV_AUTHORISED) type ABAP_BOOL
    raising
      CX_DEMO_T100 .
  methods GET_COLUMNS .
  methods GET_PRIMARY_KEYS .
  methods GET_TABLE_DESCRIPTOR .
  methods GENERATE_METADATA
    importing
      value(IV_DB_TABNAME) type ADBC_NAME .
  methods GENERATE_STRUCTS .
  methods GET_FIELD_LIST
    importing
      value(IT_COL_NAME) like GT_COL_NAME
      value(IS_DATA) type ANY optional
    returning
      value(RS_FIELD_LIST) type STRING .
  methods ADD_MESSAGE
    importing
      value(IV_MESSAGE) type BAPI_MSG
      value(IV_MSGTYP) type BAPIRET2-TYPE .
  methods INSERT
    importing
      value(IS_DATA) type ANY
    exporting
      value(EV_ROWS_PROCESSED) type I
    returning
      value(RV_DUPLICATE_KEY) type ABAP_BOOL .
  methods GET_PARAM_MARKERS
    importing
      value(IT_COL_NAME) like GT_COL_NAME
      value(IV_WITH_EQUALITY_OP) type ABAP_BOOL default ABAP_FALSE
      value(IV_OPERATION) type C
      value(IS_DATA) type ANY optional
    returning
      value(RV_PARAM_MARKERS) type STRING .
  methods MOVE_CORRESPONDING
    importing
      value(IS_DATA) type ANY
    changing
      value(CS_DATA) type ANY
    returning
      value(RV_ERROR) type ABAP_BOOL .
  methods CHECK_STRUCT_INDENTICALITY
    importing
      value(IS_DATA) type ANY
    returning
      value(RV_COMPAT_OK) type ABAP_BOOL .
  methods GET_STRUCT_REF
    importing
      value(IS_DATA) type ANY
    returning
      value(RO_STRUCT) type ref to DATA .
ENDCLASS.



CLASS ZCL_SQL_WMS IMPLEMENTATION.


  method add_message.
    data(lt_abap_callstack) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).

    split lt_abap_callstack[ 2 ]-event at '=>' into data(lv_class_name) data(lv_method_name).

    clear gs_message.
    gs_message-method_name = lv_method_name.
    gs_message-message = iv_message.
    gs_message-msgtype = iv_msgtyp.
    append gs_message to gt_messages.
  endmethod.


  method check_authority.
    clear rv_authorised.
    check gs_wms-con_name is not initial.
    select single * from dbcon into @data(ls_dbcon) where con_name = @gs_wms-con_name.
    if sy-subrc = 0.
      try.
          data(lo_dbcon) = cl_dba_dbcon=>get_con_ref( gs_wms-con_name ).

          data(lo_auth) = cl_dba_dbcon_authority=>get_auth_ref( lo_dbcon ).

          if lo_dbcon is bound.
            rv_authorised = lo_auth->is_extmain( ).

            lo_dbcon->reset_dbcon( ).
          else.
            add_message( exporting iv_message = 'DB Con reference could not be instantiated' iv_msgtyp = 'E' ).
            return.
          endif.
        catch cx_db6_con into data(lox_db6_con).
          handle_exception( exporting ix = lox_db6_con ).
      endtry.
    else.
      raise exception type cx_demo_t100
        exporting
          textid = cx_demo_t100=>demo
          text1  = 'Connection' && ` ` && gs_wms-con_name
          text2  = 'not in table DBCON'
          text3  = space
          text4  = space.
      " or you can use add_message( ) and return
    endif.

    if rv_authorised eq abap_false.
      raise exception type cx_demo_t100
        exporting
          textid = cx_demo_t100=>demo
          text1  = 'No Authority'
          text2  = space
          text3  = space
          text4  = space.
      " or you can use add_message( ) and return
    endif.
  endmethod.


  method check_struct_indenticality.
    clear rv_compat_ok.
    if go_struct is bound.
      assign is_data to field-symbol(<ls_data>).

      data(lo_struct_descr_abap) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <ls_data> ) ).
      data(lo_struct_descr_db) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( exporting p_data_ref = go_struct ) ).
      if lo_struct_descr_db is bound and lo_struct_descr_abap is bound.
        try.
            do.
              rv_compat_ok =
                cond #( when lo_struct_descr_db->components[ sy-index ]-name eq lo_struct_descr_abap->components[ sy-index ]-name
                        then abap_true else abap_false ).
              if rv_compat_ok eq abap_false.
                add_message(
                  exporting
                    iv_message = 'DB/ABAP Structures not identical'
                    iv_msgtyp  = 'E' ).
                exit.
              endif.
            enddo.
          catch cx_sy_itab_line_not_found.  " denotes end of loop
        endtry.
      else.
        add_message(
          exporting
            iv_message = 'Structures descriptors not generated'
            iv_msgtyp  = 'E' ).
        return.
      endif.
    else.
      add_message(
        exporting
          iv_message = 'Structures not generated'
          iv_msgtyp  = 'E' ).
      return.
    endif.
  endmethod.


  method check_wms_trans_status.
    data: lt_message type zcl_sql_wms=>tty_messages.
    data: ls_host_to_wms type zhost_to_wms.
    field-symbols: <lt_res> type standard table.

    select distinct werks, lgort, con_name, db_schema, tab_name
      from zmm_t_wms_master
      into table @data(lt_master).

    check lt_master is not initial.
    refresh lt_message.
    loop at lt_master into data(ls_master).
      select *
        from zmm_t_wms_main
        into table @data(lt_pending)
        where msg_stat = 'DEF'
        and   werks = @ls_master-werks
        and   lgort = @ls_master-lgort
        and   msg_rec_id gt 200.

      if lt_pending is not initial.
        try.
            data(lo_con) = new zcl_sql_wms( iv_con_name = ls_master-con_name iv_schema = ls_master-db_schema iv_db_tabname = ls_master-tab_name ).
          catch cx_sql_exception.
        endtry.
        if lo_con is bound.
          loop at lt_pending into data(ls_pending).
            clear ls_host_to_wms.
            move-corresponding ls_pending to ls_host_to_wms.
            data(lo_res_tab) = lo_con->select_where( exporting is_data = ls_host_to_wms ).
            if lo_res_tab is bound.
              assign lo_res_tab->* to <lt_res>.
              if <lt_res> is assigned and <lt_res> is not initial.
                read table <lt_res> assigning field-symbol(<ls_res>) index 1.
                if sy-subrc = 0 and <ls_res> is assigned and <ls_res> is not initial.
                  assign component 'MSG_REC_ID' of structure <ls_res> to field-symbol(<fs>).
                  if sy-subrc = 0 and <fs> is not initial and <fs> = ls_pending-msg_rec_id.
                    unassign <fs>.
                    assign component 'MSG_STAT' of structure <ls_res> to <fs>.
                    if sy-subrc = 0 and <fs> is assigned.
                      ls_pending-msg_stat = <fs>.
                    endif.
                    unassign <fs>.
                    assign component 'MSG_DT_TRM' of structure <ls_res> to <fs>.
                    if sy-subrc = 0 and <fs> is assigned.
                      ls_pending-msg_dt_trm = <fs>.
                    endif.
                    unassign <fs>.
                    assign component 'MSG_ERR_DESC' of structure <ls_res> to <fs>.
                    if sy-subrc = 0 and <fs> is assigned.
                      ls_pending-msg_err_desc = <fs>.
                    endif.
                    unassign <fs>.
                    update zmm_t_wms_main set msg_stat = ls_pending-msg_stat
                                              msg_dt_trm = ls_pending-msg_dt_trm
                                              msg_err_desc = ls_pending-msg_err_desc
                                          where werks     = ls_pending-werks
                                            and lgort     = ls_pending-lgort
                                            and datum     = ls_pending-datum
                                            and uzeit     = ls_pending-uzeit
                                            and timestamp = ls_pending-timestamp.
                    if sy-dbcnt = 1.
                      commit work.
                      write: / ls_pending-msg_rec_id, 'processing status updated'.
                    endif.
                  endif.
                endif.
              endif.
            endif.
            clear ls_pending.
            free lo_res_tab.
            unassign: <lt_res>, <ls_res>.
          endloop.

          append lines of lo_con->get_messages( ) to lt_message.
          lo_con->cleanup( ).
          free lo_con.
        endif.
      endif.
      clear ls_master.
    endloop.

    write: / 'Log updated' color = 1.
  endmethod.


  method cleanup.
    if go_con is bound. " IHDK903408
      try.
          if not go_con->is_closed( ).
            go_con->close( ).
          endif.
        catch cx_sql_exception into data(lox_sql).
          handle_exception( exporting ix = lox_sql ).
      endtry.
    endif.

    clear_globals( ).
  endmethod.


  method clear_globals.
    free:
      go_con,
      go_meta,
      gv_db_release,
      gs_message,
      gt_messages,
      gs_table_descr,
      gv_db_tabname,
      gt_column,
      gt_key,
      gt_col_name,
      gt_key_col_name,
      gt_non_key_col_name,
      gt_metadata,
      go_struct,
      go_key_struct,
      go_non_key_struct,
      go_table,
      gs_wms.
  endmethod.


  method constructor.
    try.
        clear_globals( ).
        gs_wms-con_name = to_upper( iv_con_name ).
        gs_wms-schema = to_upper( iv_schema ).
*        if check_authority( ).
        get_connection( ).
        if go_con is not bound. " IHDK903408
          raise exception type cx_sql_exception.
        endif.
        if go_con is bound and go_meta is bound and go_con->ping( ).
          generate_metadata( exporting iv_db_tabname = to_upper( iv_db_tabname ) ).
        endif.
*        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
      catch cx_demo_t100 into data(lox_demo_t100).
        handle_exception( exporting ix = lox_demo_t100 ).
    endtry.
  endmethod.


  method convert_date_to_oracle.
    check iv_sap_date is not initial.
    clear rv_oracle_date.
    call function 'CONVERSION_EXIT_SDATE_OUTPUT'
      exporting
        input  = iv_sap_date
      importing
        output = rv_oracle_date.

    split rv_oracle_date at '.' into data(dd) data(mon) data(yyyy).

    clear rv_oracle_date.
    rv_oracle_date = |{ dd }-{ mon }-{ yyyy+2(*) }|.
  endmethod.


  method convert_date_to_wms.
    check iv_sap_date is not initial.
    clear rv_wms_date.

    call function 'CONVERT_DATE_TO_EXTERNAL'
      exporting
        date_internal            = iv_sap_date
      importing
        date_external            = rv_wms_date
      exceptions
        date_internal_is_invalid = 1
        others                   = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    if rv_wms_date ca '/'.
      data(splitter) = '/'.
    elseif rv_wms_date ca '.'.
      splitter = '.'.
    elseif rv_wms_date ca '-'.
      splitter = '-'.
    endif.

    split rv_wms_date at splitter into data(dd) data(mm) data(yyyy).

    clear rv_wms_date.
    rv_wms_date = |{ dd }-{ mm }-{ yyyy }|.
  endmethod.


  method delete.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        clear rv_delete_ok.
        if go_key_struct is bound.
          assign go_key_struct->* to field-symbol(<ls_key_data>).
          if <ls_key_data> is assigned.
            data(lv_move_error) = move_corresponding(
                                    exporting
                                      is_data = is_data
                                    changing
                                      cs_data = <ls_key_data> ).
*            move-corresponding is_data to <ls_key_data>.
            if lv_move_error eq abap_false and <ls_key_data> is not initial.
              data(ls_param_markers) = get_param_markers(
                                        exporting
                                          it_col_name = gt_key_col_name
                                          iv_operation = gc_operation-where
                                          iv_with_equality_op = abap_true
                                          is_data = <ls_key_data> ).
              if ls_param_markers is not initial.
                data(lv_sql_stmt) = gc_wms-delete && ` `
                      && gc_wms-from && ` `
                      && cl_db6_tool=>qualified_name(
                          exporting
                            schema_name = gs_table_descr-schema
                            object_name = gs_table_descr-table_name ) && ` `
                      && gc_wms-where && ` `
                      && ls_param_markers.

                data(lo_sql_stmt) = go_con->create_statement( exporting tab_name_for_trace = gs_table_descr-table_name ).

                if lv_sql_stmt is not initial and lo_sql_stmt is bound.
                  data(lo_key_struct) = get_struct_ref( exporting is_data = <ls_key_data> ).
                  if lo_key_struct is bound.
                    assign lo_key_struct->* to field-symbol(<fs_key_data>).
                    if <fs_key_data> is assigned.
                      move-corresponding <ls_key_data> to <fs_key_data>.
                    endif.

                    lo_sql_stmt->set_param_struct( exporting struct_ref = lo_key_struct ).
                    data(lv_rows_processed) = lo_sql_stmt->execute_update( exporting statement = lv_sql_stmt ).

                    if lv_rows_processed = 1.
                      go_con->commit( ).
                      rv_delete_ok = abap_true.
                      add_message(
                        exporting
                          iv_message = '1 row deleted from' && ` ` && gs_table_descr-table_name
                          iv_msgtyp  = 'S' ).
                    endif.
                  else.
                    add_message(
                      exporting
                        iv_message = 'Statement object not generated'
                        iv_msgtyp  = 'E' ).
                    return.
                  endif.
                else.
                  add_message(
                    exporting
                      iv_message = 'Could not create structure from metadata'
                      iv_msgtyp  = 'E' ).
                endif.
              else.
                add_message(
                  exporting
                    iv_message = 'Marker list not generated'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              add_message(
                exporting
                  iv_message = 'Move error: No transferable data'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Field symbol error'
                iv_msgtyp  = 'E' ).
            return.
          endif.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
        try.
            go_con->rollback( ).
          catch cx_sql_exception into lox_sql.
            handle_exception( exporting ix = lox_sql ).
        endtry.
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( exporting ix = lox_parameter_invalid ).
    endtry.
  endmethod.


  method execute.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        clear rv_rows_processed.
        " get table info + primary key info
        " select one row for extracting metadata => might not return any result but result object helps in extracting metadata
        " generate a primary key structure using input data row and table metadata
        " insert row into table
        " if execution throws duplicate key exception, delete the row using primary key and insert again
        " for any other exception handle appropriately
        " in case of date field format appropriately using to_date function of oracle => this has to be taken care of by the caller
        if is_data is not initial.
          if go_struct is bound and check_struct_indenticality( exporting is_data = is_data ).
            " either(alternate logic)
*            data(lo_data) = select_where( exporting is_data = is_data ).
*            assign lo_data->* to field-symbol(<ls_data>).
*            if <ls_data> is assigned and <ls_data> is not initial.
*              data(lv_update_ok) = update( exporting is_data = is_data ).
*            else.
*              data(lv_duplicate_key) = insert( exporting is_data = is_data ).
*              if lv_duplicate_key eq abap_true.
*                add_message(
*                  exporting
*                    iv_message = 'Duplicate key detected'
*                    iv_msgtyp  = 'E' ).
*              endif.
*            endif.
*            return.
            " or
            data(lv_duplicate_key) = insert( exporting is_data = is_data importing ev_rows_processed = rv_rows_processed ).
            if lv_duplicate_key eq abap_true and go_key_struct is bound.
              add_message(
                exporting
                  iv_message = 'Duplicate key detected, delete ->> insert'
                  iv_msgtyp  = 'W' ).
              data(lv_delete_ok) = delete( exporting is_data = is_data ).
              if lv_delete_ok eq abap_true.
                lv_duplicate_key = insert( exporting is_data = is_data importing ev_rows_processed = rv_rows_processed ).
              else.
                add_message(
                  exporting
                    iv_message = 'Entry with duplicate key could not be deleted'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              return.
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Metadata not generated'
                iv_msgtyp  = 'E' ).
            return.
          endif.
        else.
          add_message( exporting iv_message = 'Data is initial' iv_msgtyp = 'E' ).
          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
    endtry.
  endmethod.


  method format_metadata.
    data: lv_len          type i,
          lv_off          type i,
          lv_tabix_n(4)   type n,
          lt_column_names type hashed table of adbc_name with unique key table_line.

    clear: lv_tabix_n.
    refresh: lt_column_names.
    loop at it_metadata into data(ls_metadata).
      lv_tabix_n = sy-tabix.
      clear lv_off.
      clear lv_len.

      translate ls_metadata-column_name to upper case.
      lv_len = strlen( ls_metadata-column_name ).

      while lv_off < lv_len.
        if ls_metadata-column_name+lv_off(1) cn
*                  'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789#$%&*-/;<=>?@^{|}'. "#EC NOTEXT
        'ABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789'.            "#EC NOTEXT
          ls_metadata-column_name+lv_off(1) = ' '.
        endif.
        lv_off = lv_off + 1.
      endwhile.

      condense ls_metadata-column_name no-gaps.

*       prevent empty column names
      if ls_metadata-column_name is initial.
        ls_metadata-column_name = lv_tabix_n.
      endif.

      if ls_metadata-column_name(1) ca '0123456789'.
        concatenate '_' ls_metadata-column_name into ls_metadata-column_name.
      endif.

*       ensure unique column names
      insert ls_metadata-column_name into table lt_column_names.
      if sy-subrc <> 0.
        if strlen( ls_metadata-column_name ) <= 26.
          concatenate ls_metadata-column_name lv_tabix_n into ls_metadata-column_name.
        else.
          ls_metadata-column_name+26 = lv_tabix_n.
        endif.
        insert ls_metadata-column_name into table lt_column_names.
      endif.

*       prevent shortdump with '' (results in C with length 0)
      if ls_metadata-length = 0.
        ls_metadata-length = 1.
      endif.

      if it_col_metadata is supplied and it_col_metadata is not initial.
        try.
            data(ls_col_metadata) = it_col_metadata[ column_name = ls_metadata-column_name ].
            if ls_col_metadata-ddic_type eq 'DATS' or ls_col_metadata-data_type eq 'DATE'.
              ls_metadata-length = 40.
            endif.
            if ls_col_metadata-ddic_type eq 'DEC'.  " IHDK900177
              ls_metadata-length = ls_col_metadata-ddic_length.
              ls_metadata-decimals = ls_col_metadata-ddic_decimals.
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.
      endif.

      modify it_metadata from ls_metadata.
      clear: ls_metadata, ls_col_metadata.
    endloop.
  endmethod.


  method generate_metadata.
    try .
        check go_con is bound and go_meta is bound and go_con->ping( ).
        if iv_db_tabname ne gv_db_tabname or gs_table_descr is initial.
          gv_db_tabname = iv_db_tabname.
          get_table_descriptor( ).
          get_columns( ).
          get_primary_keys( ).
          generate_structs( ).
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
    endtry.
  endmethod.


  method generate_structs.
    try .
        check go_con is bound and go_meta is bound and go_con->ping( ).
        refresh gt_col_name.
        if gt_column is not initial.
          gt_col_name[] = corresponding #( gt_column mapping column_name = column_name ).
          data(ls_field_list) = get_field_list( exporting it_col_name = gt_col_name ).  " for dummy select
          if ls_field_list is not initial.
            data(lv_sql_stmt) =   gc_wms-select && ` ` && ls_field_list && ` `
                                  && gc_wms-from && ` ` &&
                                  cl_db6_tool=>qualified_name(
                                    exporting
                                      schema_name = gs_table_descr-schema
                                      object_name = gs_table_descr-table_name ) && ` `
                                  && gc_wms-where && ` rownum = 1` .  " this is the select single equivalent in oracle

            data(lo_sql_stmt) = go_con->create_statement( exporting tab_name_for_trace = gs_table_descr-table_name ).

            if lv_sql_stmt is not initial and lo_sql_stmt is bound.
              data(lo_res_set) = lo_sql_stmt->execute_query( exporting statement = lv_sql_stmt ).
            else.
              add_message(
                exporting
                  iv_message = 'Statement object not generated'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
            if lo_res_set is bound.
              data(lt_metadata) = lo_res_set->get_metadata( ).

              if lt_metadata is not initial.
                format_metadata( exporting it_col_metadata = gt_column changing it_metadata = lt_metadata ).
                refresh gt_metadata.
                gt_metadata = lt_metadata.

                try.
                    data: lo_type_descr   type ref to cl_abap_typedescr,
                          lo_struct_descr type ref to cl_abap_structdescr.
                    free: lo_struct_descr, lo_type_descr.

                    go_struct = lo_res_set->get_struct_ref( exporting md_tab = lt_metadata ).

                    lo_res_set->close( ).
                    call method cl_abap_typedescr=>describe_by_data_ref
                      exporting
                        p_data_ref           = go_struct
                      receiving
                        p_descr_ref          = lo_type_descr
                      exceptions
                        reference_is_initial = 1
                        others               = 2.
                    if sy-subrc <> 0.
*                 Implement suitable error handling here
                    else.
                      lo_struct_descr ?= lo_type_descr.
                    endif.

                    data(lo_tabletype) = cl_abap_tabledescr=>create(
                                          exporting
                                            p_line_type = lo_struct_descr
                                            p_table_kind = cl_abap_tabledescr=>tablekind_std ).

                    create data go_table type handle lo_tabletype.

                  catch cx_sy_table_creation into data(lox_table_creation).
                    handle_exception( exporting ix = lox_table_creation ).
                  catch cx_sy_struct_creation into data(lox_struct_create).
                    handle_exception( exporting ix = lox_struct_create ).
                  catch cx_parameter_invalid_range into data(lox_parameter_invalid_range).
                    handle_exception( exporting ix = lox_parameter_invalid_range ).
                endtry.
              else.
                add_message(
                  exporting
                    iv_message = 'Result column metadata not generated'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              add_message(
                exporting
                  iv_message = 'Result set not generated'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Field list not generated'
                iv_msgtyp  = 'E' ).
            return.
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Column metadata not generated'
              iv_msgtyp  = 'E' ).
          return.
        endif.
        refresh gt_key_col_name.
        if gt_key is not initial.
          gt_key_col_name[] = corresponding #( gt_key mapping column_name = table_line ).

          if lt_metadata is not initial.
            " generate key metadata from lt_metadata
            data: lt_key_metadata like lt_metadata.

            refresh lt_key_metadata.
            loop at lt_metadata into data(ls_metadata).
              read table gt_key_col_name into data(ls_key_col_name) with key column_name = ls_metadata-column_name.
              if sy-subrc = 0.
                append ls_metadata to lt_key_metadata.
              endif.
              clear: ls_metadata, ls_key_col_name.
            endloop.

            if lt_key_metadata is not initial.
              try.
                  go_key_struct = lo_res_set->get_struct_ref( exporting md_tab = lt_key_metadata ).
                catch cx_sy_struct_creation into lox_struct_create.
                  handle_exception( exporting ix = lox_struct_create ).
                catch cx_parameter_invalid_range into lox_parameter_invalid_range.
                  handle_exception( exporting ix = lox_parameter_invalid_range ).
              endtry.
              refresh gt_key_metadata.
              gt_key_metadata = lt_key_metadata.
            else.
              add_message(
                exporting
                  iv_message = 'Result key column metadata not generated'
                  iv_msgtyp  = 'E' ). " return skipped on pupose
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Result column metadata not generated'
                iv_msgtyp  = 'E' ). " return skipped on pupose
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Key column metadata not generated'
              iv_msgtyp  = 'E' ). " return skipped on pupose
        endif.
        " generate structure for non key columns
        refresh gt_non_key_col_name.
        if gt_column is not initial and lt_metadata is not initial.
          data: lt_non_key_metadata like lt_metadata.
          refresh lt_non_key_metadata.
          if gt_key is not initial.
            loop at lt_metadata into ls_metadata.
              if not line_exists( lt_key_metadata[ column_name = ls_metadata-column_name ] ).
                append corresponding #( ls_metadata ) to lt_non_key_metadata.
              endif.
              clear ls_metadata.
            endloop.
            gt_non_key_col_name = corresponding #( lt_non_key_metadata mapping column_name = column_name ).
          else.
            lt_non_key_metadata = lt_metadata.
            gt_non_key_col_name = gt_col_name.
          endif.
          if lt_non_key_metadata is not initial.
            try.
                go_non_key_struct = lo_res_set->get_struct_ref( exporting md_tab = lt_non_key_metadata ).
              catch cx_sy_struct_creation into lox_struct_create.
                handle_exception( exporting ix = lox_struct_create ).
              catch cx_parameter_invalid_range into lox_parameter_invalid_range.
                handle_exception( exporting ix = lox_parameter_invalid_range ).
            endtry.
            refresh gt_non_key_metadata.
            gt_non_key_metadata = lt_non_key_metadata.
          else.
            add_message(
            exporting
              iv_message = 'Non-key result column metadata not generated'
              iv_msgtyp  = 'E' ). " return skipped on pupose
          endif.
          if gt_non_key_col_name is initial.
            add_message(
              exporting
                iv_message = 'Non-key column metadata not generated'
                iv_msgtyp  = 'E' ). " return skipped on pupose
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Column/Result column metadata not generated'
              iv_msgtyp  = 'E' ). " return skipped on pupose
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( exporting ix = lox_parameter_invalid ).
    endtry.
  endmethod.


  method get_columns.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ) and gs_table_descr is not initial.
        refresh gt_column.
        go_meta->get_columns(
          exporting
            schema_name      = gs_table_descr-schema
            table_name       = gs_table_descr-table_name
          importing
            column_tab       = gt_column ).
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
    endtry.
  endmethod.


  method get_connection.
    try.
        if go_con is bound.
          if not go_con->is_closed( ).
            go_con->close( ).
          endif.
          free go_con.
        endif.

        go_con = cl_sql_connection=>get_connection( exporting con_name = gs_wms-con_name ).

        if go_con is bound and go_con->ping( ).
          get_metadata_ref( ).
        else.
          add_message(
            exporting
              iv_message = `Connection ` && gs_wms-con_name && ` could not be established/ping failed`
              iv_msgtyp  = 'E' ).

          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
    endtry.
  endmethod.


  method get_field_list.
    if is_data is supplied.
      assign is_data to field-symbol(<fs_data>).
    endif.
    clear rs_field_list.
    check it_col_name is not initial.
    loop at it_col_name into data(ls_col_name).
      if is_data is supplied and <fs_data> is assigned.
        assign component sy-tabix of structure <fs_data> to field-symbol(<fs>).
        if <fs> is assigned and <fs> is not initial.
          if sy-tabix > 1.
            concatenate rs_field_list `, ` into rs_field_list.
          endif.
          concatenate rs_field_list ls_col_name-column_name into rs_field_list.
        endif.
      else.
        if sy-tabix > 1.
          concatenate rs_field_list `, ` into rs_field_list.
        endif.
        concatenate rs_field_list ls_col_name-column_name into rs_field_list.
      endif.
      clear ls_col_name.
    endloop.
  endmethod.


  method get_messages.
    check gt_messages[] is not initial.
    rt_messages[] = gt_messages[].
  endmethod.


  method get_metadata_ref.
    check go_con is bound and go_con->get_dbms( ) = gc_wms-oracle_dbms.
    gv_db_release = go_con->db_get_release( ).
    try.
        go_meta = go_con->get_metadata( ).
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( ix = lox_parameter_invalid ).
    endtry.
  endmethod.


  method get_next_sequence.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        data(lv_sql_stmt) = gc_wms-select && ` ` &&
                            cl_db6_tool=>qualified_name(
                              exporting
                                schema_name = gs_table_descr-schema
                                object_name = gc_wms-sequence ) && `.` && gc_wms-nextval && ` as NEXT_SEQ_NO ` &&
                            gc_wms-from && ` DUAL`.

        data(lo_sql_stmt) = go_con->create_statement( exporting tab_name_for_trace = 'DUAL' ).
        if lv_sql_stmt is not initial and lo_sql_stmt is bound.
          data(lo_res_set) = lo_sql_stmt->execute_query( exporting statement = lv_sql_stmt ).
          if lo_res_set is bound.
            data(lt_metadata) = lo_res_set->get_metadata( ).
            if lt_metadata is not initial.
              format_metadata( changing it_metadata = lt_metadata ).
              data(lo_struct) = lo_res_set->get_struct_ref( exporting md_tab = lt_metadata ).
              if lo_struct is bound.
                assign lo_struct->* to field-symbol(<fs>).
                lo_res_set->set_param_struct( exporting struct_ref = lo_struct ).
                lo_res_set->next( ).
                lo_res_set->close( ).

                if <fs> is assigned and <fs> is not initial.
                  assign component 1 of structure <fs> to field-symbol(<fv>).
                  if <fv> is assigned and <fv> is not initial.
                    ro_rec_id = ref #( <fv> ).
                  endif.
                else.
                  add_message(
                    exporting
                      iv_message = 'Empty result set'
                      iv_msgtyp  = 'E' ).
                  return.
                endif.
              else.
                add_message(
                  exporting
                    iv_message = 'Structure could not be generated'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              add_message(
                exporting
                  iv_message = 'Result metadata not generated'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Result set not generated'
                iv_msgtyp  = 'E' ).
            return.
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Statement/Statement object not generated'
              iv_msgtyp  = 'E' ).
          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( exporting ix = lox_parameter_invalid ).
      catch cx_sy_struct_creation into data(lox_struct_creation).
        handle_exception( exporting ix = lox_struct_creation ).
    endtry.

  endmethod.


  method get_param_markers.
    check gt_col_name is not initial.
    if iv_with_equality_op eq abap_true.
      data(lv_marker) = ` = ?`. " 'AND' separated list of 'col_name = ?' for use in where clauses of selects, updates and deletes
    else.
      lv_marker = `?`. " comma separated list of '?' for use in inserts and updates
    endif.

    case iv_operation.
      when gc_operation-where.
        data(lv_separator) = `and `.
      when gc_operation-insert.
        lv_separator = `, `.
      when gc_operation-update.
        lv_separator = `, `.
      when others.
    endcase.

    if is_data is supplied.
      assign is_data to field-symbol(<fs_data>).
    endif.

    loop at it_col_name into data(ls_col_name).
      if is_data is supplied and <fs_data> is assigned.
        assign component sy-tabix of structure <fs_data> to field-symbol(<fs>).
        if <fs> is assigned and <fs> is not initial.
          if sy-tabix = 1.
            if iv_with_equality_op eq abap_true.
              rv_param_markers = ls_col_name-column_name && lv_marker.
            else.
              rv_param_markers = lv_marker.
            endif.
          else.
            if iv_with_equality_op eq abap_true.
              rv_param_markers = rv_param_markers && lv_separator && ls_col_name-column_name && lv_marker.
            else.
              rv_param_markers = rv_param_markers && lv_separator && lv_marker.
            endif.
          endif.
        endif.
      else.
        if sy-tabix = 1.
          if iv_with_equality_op eq abap_true.
            rv_param_markers = ls_col_name-column_name && lv_marker.
          else.
            rv_param_markers = lv_marker.
          endif.
        else.
          if iv_with_equality_op eq abap_true.
            rv_param_markers = rv_param_markers && lv_separator && ls_col_name-column_name && lv_marker.
          else.
            rv_param_markers = rv_param_markers && lv_separator && lv_marker.
          endif.
        endif.
      endif.
      clear ls_col_name.
    endloop.
  endmethod.


  method get_primary_keys.
    data: lv_key_name type adbc_name.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ) and gs_table_descr is not initial.
        refresh gt_key.
        clear lv_key_name.
        go_meta->get_primary_keys(
          exporting
            schema_name      = gs_table_descr-schema
            table_name       = gs_table_descr-table_name
          importing
            primary_key_name = lv_key_name
            primary_key_tab  = gt_key ).

        " in case no key is specified, oracle uses rownum as key
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
    endtry.
  endmethod.


  method get_struct_ref.
    data: md_ref      type ref to adbc_rs_metadata_descr,
          structdescr type ref to cl_abap_structdescr,
          comp_wa     type abap_componentdescr,
          comp_tab    type abap_component_tab.

    if is_data is not initial.
      assign is_data to field-symbol(<fs_data>).
    endif.

    loop at gt_metadata reference into md_ref.
      if is_data is not initial and <fs_data> is assigned.
        assign component md_ref->column_name of structure <fs_data> to field-symbol(<fs>).
        if <fs> is assigned and <fs> is not initial.
          comp_wa-name = md_ref->column_name.
          case md_ref->data_type.
            when cl_sql_result_set=>c_md_type_p.
*       Might throw CX_PARAMETER_INVALID_RANGE => will be propagated to caller
              comp_wa-type = cl_abap_elemdescr=>get_p( p_length   = md_ref->length
                                                       p_decimals = md_ref->decimals ).
            when cl_sql_result_set=>c_md_type_f.
              comp_wa-type = cl_abap_elemdescr=>get_f( ).
            when cl_sql_result_set=>c_md_type_i.
              if md_ref->length <= 1.
                comp_wa-type = cl_abap_elemdescr=>get_int1( ).
              elseif md_ref->length <= 2.
                comp_wa-type = cl_abap_elemdescr=>get_int2( ).
              elseif md_ref->length <= 4.
                comp_wa-type = cl_abap_elemdescr=>get_i( ).
              else.
                comp_wa-type = cl_abap_elemdescr=>get_int8( ).
              endif.
            when cl_sql_result_set=>c_md_type_c.
*              if string_only = abap_true.
*                comp_wa-type = cl_abap_elemdescr=>get_string( ).
*              else.
*         Might throw CX_PARAMETER_INVALID_RANGE => will be propagated to caller
              comp_wa-type = cl_abap_elemdescr=>get_c( p_length = md_ref->length ).
*              endif.
            when cl_sql_result_set=>c_md_type_x.
*              if string_only = abap_true.
*                comp_wa-type = cl_abap_elemdescr=>get_xstring( ).
*              else.
*         Might throw CX_PARAMETER_INVALID_RANGE => will be propagated to caller
              comp_wa-type = cl_abap_elemdescr=>get_x( p_length = md_ref->length ).
*              endif.
            when cl_sql_result_set=>c_md_type_string.
              comp_wa-type = cl_abap_elemdescr=>get_string( ).
            when cl_sql_result_set=>c_md_type_xstring.
              comp_wa-type = cl_abap_elemdescr=>get_xstring( ).
            when cl_sql_result_set=>c_md_type_decfloat16.
              comp_wa-type = cl_abap_elemdescr=>get_decfloat16( ).
            when cl_sql_result_set=>c_md_type_decfloat34.
              comp_wa-type = cl_abap_elemdescr=>get_decfloat34( ).
            when others.
*       unknown data type
              raise exception type cx_sy_struct_comp_type
                exporting
                  component_name   = comp_wa-name
                  component_number = sy-tabix.
          endcase.
          append comp_wa to comp_tab.
        endif.
      endif.
    endloop.

* Might throw CX_SY_STRUCT_CREATION => will be propagated to caller
    structdescr = cl_abap_structdescr=>create( p_components = comp_tab
                                                p_strict    = abap_true ).
    create data ro_struct type handle structdescr.
  endmethod.


  method get_table_descriptor.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        data: table_descr_tab type adbc_table_descr_tab,
              table_descr     like line of table_descr_tab,
              table_rgtab     type adbc_name_rgtab,
              table_rgwa      like line of table_rgtab,
              schema_rgtab    type adbc_name_rgtab,
              schema_rgwa     like line of schema_rgtab,
              table_types     type string.

        refresh: table_descr_tab, table_rgtab, schema_rgtab.
        clear: table_descr, table_rgwa, schema_rgwa, gs_table_descr.
        concatenate cl_sql_metadata=>c_table_type_table cl_sql_metadata=>c_table_type_view
        into table_types.

        table_descr-schema = gs_wms-schema. " Need to know this before hand
        table_descr-table_name = gv_db_tabname. " Need to know this before hand
        table_descr-table_type = cl_sql_metadata=>c_table_type_table.

        table_rgwa-sign = 'I'.
        table_rgwa-option = 'EQ'.
        table_rgwa-low = table_descr-table_name.
        append table_rgwa to table_rgtab.

        schema_rgwa-sign = 'I'.
        schema_rgwa-option = 'EQ'.
        schema_rgwa-low = table_descr-schema.
        append schema_rgwa to schema_rgtab.

        go_meta->get_tables(
          exporting
            schema_rgtab = schema_rgtab
            table_rgtab  = table_rgtab
            table_types  = table_types
          importing
            table_descr_tab = table_descr_tab ).

        if table_descr_tab is not initial.
          read table table_descr_tab into data(table_descr_wa) index 1.
          if sy-subrc = 0 and table_descr_wa = table_descr and lines( table_descr_tab ) eq 1.
            gs_table_descr = table_descr_wa.
          else.
            clear gs_table_descr.
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Could not load table descriptor'
              iv_msgtyp  = 'E' ).
          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
    endtry.
  endmethod.


  method handle_exception.
    check ix is bound.
    constants: lc_sql_exc_class type seoclsname value 'CX_SQL_EXCEPTION'.

    data(lt_abap_callstack) = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).

    split lt_abap_callstack[ 2 ]-event at '=>' into data(lv_class_name) data(lv_method_name).

    data(lo_type_descr) = cl_abap_typedescr=>describe_by_object_ref( ix ).
    if lo_type_descr is bound.
      data(lv_exception_class) = lo_type_descr->get_relative_name( ).
    endif.

    data: lox_sql type ref to cx_sql_exception.
    clear gs_message.
    free lox_sql.
    try.
        case lv_exception_class.
          when lc_sql_exc_class.
            lox_sql ?= ix.
            if lox_sql is bound.
              gs_message-method_name = lv_method_name.
              if lox_sql->db_error eq abap_true.
                " SQL error occurred (includes the cases 'duplicate_key', 'dbobject_exists',
                "'dbobject_not_exists'). Only in this case the exception attributes
                "SQL_CODE and SQL_MESSAGE are set.
                gs_message-message = |SQL error { lox_sql->sql_code }: { lox_sql->sql_message }| ##NO_TEXT.
                case abap_true.
                  when lox_sql->duplicate_key.
                    gs_message-message = gs_message-message && `, Duplicate Key`.
                  when lox_sql->dbobject_exists.
                    gs_message-message = gs_message-message && `, DB Object Already Exists`.
                  when lox_sql->dbobject_not_exists.
                    gs_message-message = gs_message-message && `, DB Object Does Not Exist`.
                  when others.
                endcase.
              else.
                gs_message-message = |ADBC error: |.
                case abap_true.
                  when lox_sql->invalid_cursor.
                    gs_message-message = gs_message-message && 'Invalid Cursor'.
                  when lox_sql->external_error.
                    gs_message-message = gs_message-message && 'External Error'.
                  when lox_sql->no_more_memory.
                    gs_message-message = gs_message-message && 'No More Memory'.
                  when lox_sql->unknown_connection.
                    gs_message-message = gs_message-message && 'Unknown Connection'.
                  when lox_sql->connection_open_error.
                    gs_message-message = gs_message-message && 'Connection Open Error'.
                  when others.
                    gs_message-message = gs_message-message && 'Internal Error Code' && ` ` && conv string( lox_sql->internal_error ).
                endcase.
                " get db trace file
                gs_message-message = gs_message-message && `, Trace File: dev_w` && cl_dmc_utilities=>get_own_wp_no( ).
              endif.
              gs_message-msgtype = 'E'.
            endif.
          when others.
            gs_message-method_name = lv_method_name.
            gs_message-message = ix->get_text( ).
            gs_message-msgtype = 'E'.
        endcase.
      catch cx_sy_create_object_error.  " #EC NO_HANDLER
    endtry.

    append gs_message to gt_messages.
  endmethod.


  method insert.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        clear: rv_duplicate_key, ev_rows_processed.
        if go_struct is bound.
          assign go_struct->* to field-symbol(<ls_data>).
          if <ls_data> is assigned.
            data(lv_move_error) = move_corresponding(
                                    exporting
                                      is_data = is_data
                                    changing
                                      cs_data = <ls_data> ).
*            move-corresponding is_data to <ls_data>.
            if lv_move_error eq abap_false and <ls_data> is not initial.
              data(ls_field_list) = get_field_list( exporting it_col_name = gt_col_name is_data = is_data ).
              data(ls_param_markers) = get_param_markers(
                                        exporting
                                          it_col_name = gt_col_name
                                          iv_operation = gc_operation-insert
                                          is_data = is_data ).
              if ls_field_list is not initial and ls_param_markers is not initial.
                data(lv_sql_stmt) = gc_wms-insert && ` `
                                    && gc_wms-into && ` `
                                    && cl_db6_tool=>qualified_name(
                                        exporting
                                          schema_name = gs_table_descr-schema
                                          object_name = gs_table_descr-table_name ) && ` ( `
                                    && ls_field_list && ` ) `
                                    && gc_wms-values && ` ( `
                                    && ls_param_markers && ` ) `.

                data(lo_sql_stmt) = go_con->create_statement( exporting tab_name_for_trace = gs_table_descr-table_name ).

                if lv_sql_stmt is not initial and lo_sql_stmt is bound.
                  data(lo_struct) = get_struct_ref( exporting is_data = is_data ).
                  if lo_struct is bound.
                    assign lo_struct->* to field-symbol(<fs_data>).
                    if <fs_data> is assigned.
                      move-corresponding <ls_data> to <fs_data>.
                    endif.
                    lo_sql_stmt->set_param_struct( exporting struct_ref = lo_struct ).
                    data(lv_rows_processed) = lo_sql_stmt->execute_update( exporting statement = lv_sql_stmt ).
                    ev_rows_processed = lv_rows_processed.

                    if lv_rows_processed = 1.
                      go_con->commit( ).
                      add_message(
                        exporting
                          iv_message = '1 row inserted into' && ` ` && gs_table_descr-table_name
                          iv_msgtyp  = 'S' ).
                    endif.
                  else.
                    add_message(
                      exporting
                        iv_message = 'Could not create structure from metadata'
                        iv_msgtyp  = 'E' ).
                  endif.
                else.
                  add_message(
                    exporting
                      iv_message = 'Statement object not generated'
                      iv_msgtyp  = 'E' ).
                  return.
                endif.
              else.
                add_message(
                  exporting
                    iv_message = 'Column list/marker list not generated'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              add_message(
                exporting
                  iv_message = 'Move error: No transferable data'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
          else.
            add_message(
             exporting
               iv_message = 'Field symbol error'
               iv_msgtyp  = 'E' ).
            return.
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Structures not generated'
              iv_msgtyp  = 'E' ).
          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
        try.
            go_con->rollback( ).
          catch cx_sql_exception into lox_sql.
            handle_exception( exporting ix = lox_sql ).
        endtry.
        if lox_sql->duplicate_key eq abap_true.
          rv_duplicate_key = abap_true.
        endif.
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( exporting ix = lox_parameter_invalid ).
    endtry.
  endmethod.


  method move_corresponding.
    try.
        if is_data is not initial.
          assign is_data to field-symbol(<lis_data>).
          assign cs_data to field-symbol(<lcs_data>).

          data(lo_is_struct_descr) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <lis_data> ) ).
          data(lo_cs_struct_descr) = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( exporting p_data = <lcs_data> ) ).

          if lo_is_struct_descr is bound and lo_cs_struct_descr is bound.
            loop at lo_cs_struct_descr->components into data(ls_component).
              assign component ls_component-name of structure <lis_data> to field-symbol(<lis>).
              if sy-subrc <> 0.
                rv_error = abap_true.
                exit.
              endif.
              assign component ls_component-name of structure <lcs_data> to field-symbol(<lcs>).
              if sy-subrc <> 0.
                rv_error = abap_true.
                exit.
              endif.
              if rv_error <> abap_true and <lis> is assigned and <lcs> is assigned.
                <lcs> = <lis>.
              endif.

              unassign: <lis>, <lcs>.
              clear ls_component.
            endloop.

            if cs_data is initial.
              add_message(
                exporting
                  iv_message = 'Move error: No data moved'
                  iv_msgtyp  = 'E' ).

              return.
            endif.
          else.
            rv_error = abap_true.
            add_message(
              exporting
                iv_message = 'Move error: Struct descriptor not generated'
                iv_msgtyp  = 'E' ).

            return.
          endif.
        else.
          rv_error = abap_true.
          add_message(
            exporting
              iv_message = 'Move error: Empty data'
              iv_msgtyp  = 'E' ).

          return.
        endif.
      catch cx_root into data(lox_root).
        handle_exception( exporting ix = lox_root ).
    endtry.
  endmethod.


  method select_all.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        if go_table is bound.
          data(ls_field_list) = get_field_list( exporting it_col_name = gt_col_name ).
          if ls_field_list is not initial.
            data(lv_sql_stmt) = gc_wms-select && ` ` &&
                                ls_field_list && ` ` &&
                                gc_wms-from && ` ` &&
                                cl_db6_tool=>qualified_name(
                                  exporting
                                    schema_name = gs_table_descr-schema
                                    object_name = gs_table_descr-table_name ).

            data(lo_sql_stmt) = go_con->create_statement( exporting tab_name_for_trace = gs_table_descr-table_name ).
            if lo_sql_stmt is bound and lv_sql_stmt is not initial.
              data(lo_res_set) = lo_sql_stmt->execute_query( exporting statement = lv_sql_stmt ).

              if lo_res_set is bound.
                field-symbols: <lt_data> type standard table.
                assign go_table->* to <lt_data>.
                if <lt_data> is assigned.
                  refresh <lt_data>.
                endif.
                lo_res_set->set_param_table( exporting itab_ref = go_table ).
                lo_res_set->next_package( ).
                lo_res_set->close( ).

                if <lt_data> is not initial.
                  add_message(
                    exporting
                      iv_message = |{ lo_res_set->rows_fetched } rows fetched|
                      iv_msgtyp  = 'S' ).
                endif.

                get reference of <lt_data> into rt_data.
              else.
                add_message(
                  exporting
                    iv_message = 'Result set not returned'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              add_message(
                exporting
                  iv_message = 'Statement object not generated'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Field list not generated'
                iv_msgtyp  = 'E' ).
            return.
          endif.
        else.
          add_message(
           exporting
             iv_message = 'Structures not generated'
             iv_msgtyp  = 'E' ).
          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( exporting ix = lox_parameter_invalid ).
*      catch cx_parameter_invalid_type into data(lox_parameter_invalid).
*        handle_exception( exporting ix = lox_parameter_invalid_type ).
    endtry.
  endmethod.


  method select_where.
    try.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        if go_struct is bound and go_key_struct is bound and go_table is bound.
          assign go_key_struct->* to field-symbol(<ls_key_data>).
          if <ls_key_data> is assigned.
            data(lv_move_error) = move_corresponding(
                                    exporting
                                      is_data = is_data
                                    changing
                                      cs_data = <ls_key_data> ).

            if lv_move_error eq abap_false and <ls_key_data> is not initial.
              data(ls_field_list) = get_field_list( exporting it_col_name = gt_col_name ).
              data(ls_param_markers) = get_param_markers(
                                        exporting
                                          it_col_name = gt_key_col_name
                                          iv_operation = gc_operation-where
                                          iv_with_equality_op = abap_true
                                          is_data = <ls_key_data> ).
              if ls_field_list is not initial and ls_param_markers is not initial.
                data(lv_sql_stmt) = gc_wms-select && ` ` &&
                                    ls_field_list && ` ` &&
                                    gc_wms-from && ` ` &&
                                    cl_db6_tool=>qualified_name(
                                      exporting
                                        schema_name = gs_table_descr-schema
                                        object_name = gs_table_descr-table_name ) && ` ` &&
                                    gc_wms-where && ` ` &&
                                    ls_param_markers.
                data(lo_sql_stmt) = go_con->create_statement( exporting tab_name_for_trace = gs_table_descr-table_name ).
                if lv_sql_stmt is not initial and lo_sql_stmt is bound.
                  data(lo_key_struct) = get_struct_ref( exporting is_data = <ls_key_data> ).
                  if lo_key_struct is bound.
                    assign lo_key_struct->* to field-symbol(<fs_key_data>).
                    if <fs_key_data> is assigned.
                      move-corresponding <ls_key_data> to <fs_key_data>.
                    endif.
                    lo_sql_stmt->set_param_struct( exporting struct_ref = lo_key_struct ).
                    data(lo_res_set) = lo_sql_stmt->execute_query( exporting statement = lv_sql_stmt ).
                    if lo_res_set is bound.
                      field-symbols: <lt_data> type standard table.
                      assign go_table->* to <lt_data>.
                      if <lt_data> is assigned.
                        refresh <lt_data>.
                      endif.
                      lo_res_set->set_param_table( exporting itab_ref = go_table ).
                      lo_res_set->next_package( ).
                      lo_res_set->close( ).

                      if <lt_data> is not initial.
                        add_message(
                          exporting
                            iv_message = |{ lo_res_set->rows_fetched } rows fetched|
                            iv_msgtyp  = 'S' ).
                      endif.

                      get reference of <lt_data> into rt_data.
                    else.
                      add_message(
                        exporting
                          iv_message = 'Result set not returned'
                          iv_msgtyp  = 'E' ).
                      return.
                    endif.
                  else.
                    add_message(
                      exporting
                        iv_message = 'Statement object not generated'
                        iv_msgtyp  = 'E' ).
                    return.
                  endif.
                else.
                  add_message(
                    exporting
                      iv_message = 'Could not create structure from metadata'
                      iv_msgtyp  = 'E' ).
                endif.
              else.
                add_message(
                  exporting
                    iv_message = 'Column list/marker list not generated'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              add_message(
                exporting
                  iv_message = 'Move error: No transferable data'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Field symbol error'
                iv_msgtyp  = 'E' ).
            return.
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Structures not generated'
              iv_msgtyp  = 'E' ).
          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( exporting ix = lox_parameter_invalid ).
    endtry.
  endmethod.


  method update.
    try.
        clear rv_update_ok.
        check go_con is bound and go_meta is bound and go_con->ping( ).
        if go_non_key_struct is bound and go_key_struct is bound and go_struct is bound.
          assign go_non_key_struct->* to field-symbol(<ls_non_key_data>).
          assign go_key_struct->* to field-symbol(<ls_key_data>).
          if <ls_non_key_data> is assigned and <ls_key_data> is assigned.
            data(lv_move_error_non_key) = move_corresponding(
                                            exporting
                                              is_data = is_data
                                            changing
                                              cs_data = <ls_non_key_data> ).

            data(lv_move_error_key) = move_corresponding(
                                        exporting
                                          is_data = is_data
                                        changing
                                          cs_data = <ls_key_data> ).
            if lv_move_error_non_key eq abap_false and lv_move_error_key eq abap_false
            and <ls_non_key_data> is not initial and <ls_key_data> is not initial.
              data(ls_param_markers_set) = get_param_markers(
                                            exporting
                                              it_col_name = gt_non_key_col_name
                                              iv_operation = gc_operation-update
                                              iv_with_equality_op = abap_true
                                              is_data = <ls_non_key_data> ).
              data(ls_param_markers_where) = get_param_markers(
                                              exporting
                                                it_col_name = gt_key_col_name
                                                iv_operation = gc_operation-update
                                                iv_with_equality_op = abap_true
                                                is_data = <ls_key_data> ).
              if ls_param_markers_set is not initial and ls_param_markers_where is not initial.
                data(lv_sql_stmt) = gc_wms-update && ` ` &&
                                    cl_db6_tool=>qualified_name(
                                      exporting
                                        schema_name = gs_table_descr-schema
                                        object_name = gs_table_descr-table_name ) && ` ` &&
                                    gc_wms-set && ` ` &&
                                    ls_param_markers_set && ` ` &&
                                    gc_wms-where && ` ( ` &&
                                    ls_param_markers_where && ` ) `.
                data(lo_sql_stmt) = go_con->create_statement( exporting tab_name_for_trace = gs_table_descr-table_name ).
                if lv_sql_stmt is not initial and lo_sql_stmt is bound.
                  data(lo_non_key_struct) = get_struct_ref( exporting is_data = <ls_non_key_data> ).
                  data(lo_key_struct) = get_struct_ref( exporting is_data = <ls_key_data> ).

                  if lo_non_key_struct is bound and lo_key_struct is bound.
                    assign lo_non_key_struct->* to field-symbol(<fs_non_key_data>).
                    assign lo_key_struct->* to field-symbol(<fs_key_data>).
                    if <fs_non_key_data> is assigned and <fs_key_data> is assigned.
                      move-corresponding <ls_non_key_data> to <fs_non_key_data>.
                      move-corresponding <ls_key_data> to <fs_key_data>.
                    endif.
                    lo_sql_stmt->set_param_struct( exporting struct_ref = lo_non_key_struct ).
                    lo_sql_stmt->set_param_struct( exporting struct_ref = lo_key_struct ).

                    data(lv_rows_processed) = lo_sql_stmt->execute_update( exporting statement = lv_sql_stmt ).
                    if lv_rows_processed ge 1.
                      rv_update_ok = abap_true.
                      go_con->commit( ).
                      add_message(
                        exporting
                          iv_message = '1 row updated in' && ` ` && gs_table_descr-table_name
                          iv_msgtyp  = 'S' ).
                    endif.
                  else.
                    add_message(
                      exporting
                        iv_message = 'Statement object not generated'
                        iv_msgtyp  = 'E' ).
                    return.
                  endif.
                else.
                  add_message(
                      exporting
                        iv_message = 'Could not create structure from metadata'
                        iv_msgtyp  = 'E' ).
                endif.
              else.
                add_message(
                  exporting
                    iv_message = 'Column list/marker list not generated'
                    iv_msgtyp  = 'E' ).
                return.
              endif.
            else.
              add_message(
                exporting
                  iv_message = 'Move error: No transferable data'
                  iv_msgtyp  = 'E' ).
              return.
            endif.
          else.
            add_message(
              exporting
                iv_message = 'Field symbol error'
                iv_msgtyp  = 'E' ).
            return.
          endif.
        else.
          add_message(
            exporting
              iv_message = 'Structures not generated'
              iv_msgtyp  = 'E' ).
          return.
        endif.
      catch cx_sql_exception into data(lox_sql).
        handle_exception( exporting ix = lox_sql ).
      catch cx_parameter_invalid into data(lox_parameter_invalid).
        handle_exception( exporting ix = lox_parameter_invalid ).
    endtry.
  endmethod.
ENDCLASS.
