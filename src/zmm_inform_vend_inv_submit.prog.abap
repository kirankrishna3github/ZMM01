*&---------------------------------------------------------------------*
*& Report ZMM_INFORM_VEND_INV_SUBMIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmm_inform_vend_inv_submit.
tables:bkpf , bseg ,bsis .


****Company code – BSIS-BUKRS
****Document Number – BSIS -BELNR
****Year - BSIS –GKAHR
****Document date – BSIS-BLDAT
****Plant – BSIS-WERKS

types: begin of ty_bsis,
         bukrs type bsis-bukrs,
         hkont type bsis-hkont,
         belnr type bsis-belnr,
         gjahr type bsis-gjahr,
         zuonr type bsis-zuonr,
         blart type bsis-blart,
         bldat type bsis-bldat,
         xblnr type bsis-xblnr,
         shkzg type bsis-shkzg,
         dmbtr type bsis-dmbtr,
         werks type bsis-werks,
       end of ty_bsis,
       begin of ty_bseg ,
         bukrs type bseg-bukrs,
         belnr type bseg-belnr,
         gjahr type bseg-gjahr,
         zuonr type bseg-zuonr,
         menge type bseg-menge,
         meins type bseg-meins,
         matnr type bseg-matnr,
         ebeln type bseg-ebeln,
         ebelp type bseg-ebelp,
         lifnr type bseg-lifnr,
         dmbtr type bseg-dmbtr,
       end of ty_bseg.

types: begin of ty_final.
    include type zstr_vend_inv.
types : vend_mail_id type adr6-smtp_addr,
        user_mail_id type adr6-smtp_addr,
        end of ty_final.

data: it_bsis  type table of ty_bsis,
      ls_bsis  type ty_bsis,
      it_bseg  type table of ty_bseg,
      ls_bseg  type ty_bseg,
      lt_final type table of ty_final,
      ls_final type ty_final.

DATA: lo_table     TYPE REF TO cl_salv_table,
      lo_functions TYPE REF TO cl_salv_functions_list,
      lo_display   TYPE REF TO cl_salv_display_settings,
      lo_columns   TYPE REF TO cl_salv_columns_table,
      lo_column    TYPE REF TO cl_salv_column_table,
      lo_layout    TYPE REF TO cl_salv_layout,
      ls_key       TYPE salv_s_layout_key,
      lo_selection TYPE REF TO cl_salv_selections,
      lo_event     TYPE REF TO cl_salv_events_table.

data : lv_save(1) type c,
       lv_exit(1) type c,
       ls_variant type disvariant,
       gs_variant type disvariant.

selection-screen begin of block b1 with frame title text-001.
select-options: s_lifnr for bseg-lifnr,
                s_bukrs for bkpf-bukrs no-extension no intervals obligatory,
                s_belnr for bkpf-belnr,
                s_gjahr for bkpf-gjahr no-extension no intervals obligatory,
                s_bldat for bsis-bldat OBLIGATORY,
                s_werks for bsis-werks ,
                s_hkont for bsis-hkont no-display.
selection-screen end of block b1.

"ALV variant
selection-screen begin of block b2 with frame title text-002.
parameters p_vari type disvariant-variant.
selection-screen end of block b2.


*&------------------------------------------------------------&*
*&                Field symbols Declaration
*&------------------------------------------------------------&*
field-symbols <fs_x> type x.

class lcl_module definition.
  public section.
    methods: process, get_data , process_data , f4_variant,variant_exist,init_variant , call_sf, display_alv, send_mail ,
      change_col_text importing columnname type lvc_fname
                                short      type scrtext_s
                                medium     type scrtext_m
                                long       type scrtext_l,
      user_command for event added_function of cl_salv_events
        importing e_salv_function.
endclass.

class lcl_module implementation.
  method f4_variant.

    lv_save = 'A'.
    ls_variant-report = sy-repid.

    call function 'REUSE_ALV_VARIANT_F4'
      exporting
        is_variant = ls_variant
        i_save     = lv_save
      importing
        e_exit     = lv_exit
        es_variant = gs_variant
      exceptions
        not_found  = 2.

    if sy-subrc eq 2.
      message id sy-msgid type 'S' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      if lv_exit eq space.
        p_vari = gs_variant-variant.
      endif.
    endif.
  endmethod.
  method variant_exist.
    data : lv_save(1) type c.

    if not p_vari is initial.

      move p_vari to gs_variant-variant.

      lv_save = 'A'.
      gs_variant-report = sy-repid.
      call function 'REUSE_ALV_VARIANT_EXISTENCE'
        exporting
          i_save     = lv_save
        changing
          cs_variant = gs_variant.

    else.
      init_variant( ).
    endif.
  endmethod.
  method init_variant.
    data : lv_save(1) type c.

    clear : gs_variant.

    lv_save = 'A'.
    gs_variant-report = sy-repid.

    call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
      exporting
        i_save        = lv_save
      changing
        cs_variant    = gs_variant
      exceptions
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        others        = 4.

    if sy-subrc eq 0.
      p_vari = gs_variant-variant.
    endif.
  endmethod.
  method process.
    get_data( ).
    process_data( ).

    CHECK lt_final[] IS NOT INITIAL.

    SORT lt_final[] BY lifnr werks.

    display_alv( ).



  endmethod.
  method get_data.

    select param1
    from z6mma_params
    into table @data(it_param)
    where progname = 'ZMM026'.


    loop at it_param into data(wa_param).
      s_hkont-option = 'EQ'.
      s_hkont-sign = 'I'.
      s_hkont-low = wa_param-param1.
      append s_hkont to s_hkont[].
    endloop.

    if s_hkont is not initial.
      select bukrs hkont belnr gjahr blart
      zuonr bldat xblnr shkzg dmbtr werks
      from bsis into table it_bsis
        where bukrs in s_bukrs
        and belnr in s_belnr
        and hkont in s_hkont
        and bldat in s_bldat
        and gjahr in s_gjahr
        and werks in s_werks
        AND blart = 'WE'.
      if it_bsis is not initial .

        select bukrs belnr gjahr zuonr menge meins matnr ebeln ebelp lifnr dmbtr
          from bseg into corresponding fields of table it_bseg
          for all entries in it_bsis
          where bukrs = it_bsis-bukrs
          and belnr = it_bsis-belnr
          and gjahr = it_bsis-gjahr
          and hkont in s_hkont.

      else.
        message 'No Data Found' type 'E'.

      endif.
    endif.

  endmethod.
  method process_data.

    loop at it_bSEG into ls_bseg  .

      ls_final-LIFNR = ls_bseg-lifnr.
      ls_final-ZUONR = ls_bseg-ZUONR.
      ls_final-MENGE = ls_bseg-MENGE.
      ls_final-MEINS = ls_bseg-MEINS.
      ls_final-MATNR = ls_bseg-MATNR.
      ls_final-EBELN = ls_bseg-EBELN.
      ls_final-EBELP = ls_bseg-EBELP.
      ls_final-DMBTR = ls_bseg-DMBTR.

      SELECT SINGLE maktx FROM makt INTO ls_final-MAKTX WHERE matnr = ls_bseg-matnr.

      READ TABLE it_bsis INTO ls_bsis WITH KEY bukrs = ls_bseg-bukrs belnr = ls_bseg-belnr gjahr = ls_bseg-gjahr .
      if sy-subrc = 0.
      ls_final-BUKRS = ls_bsis-BUKRS.
      ls_final-BELNR = ls_bsis-BELNR.
      ls_final-GJAHR = ls_bsis-GJAHR.
      ls_final-BLDAT = ls_bsis-bldat.
      ls_final-XBLNR = ls_bsis-xblnr.
      ls_final-BLART = ls_bsis-blart.
      ls_final-werks = ls_bsis-werks.
      endif.

      append ls_final to LT_FINAL.

    endloop.


  endmethod.
  method call_sf.

  endmethod.
  method display_alv.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            list_display   = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table   = lo_table
          CHANGING
            t_table        = lt_final
        ).

        IF lo_table IS BOUND.

          lo_table->set_screen_status(
            EXPORTING
              report        = 'ZMM_INFORM_VEND_INV_SUBMIT'
              pfstatus      = 'STANDARD'
              set_functions = lo_table->c_functions_all ).

          lo_functions = lo_table->get_functions( ).
          IF lo_functions IS BOUND.
            lo_functions->set_all( value = if_salv_c_bool_sap=>true ).
          ENDIF.

          lo_display = lo_table->get_display_settings( ).
          IF lo_display IS BOUND.
            lo_display->set_striped_pattern( value = if_salv_c_bool_sap=>true ).
          ENDIF.

          lo_columns = lo_table->get_columns( ).
          IF lo_columns IS BOUND.
*            CLEAR: columnname, short, medium, long.
            change_col_text( EXPORTING columnname = 'BUKRS' short      = 'Comp Code.'
                             medium     = 'Company Code'
                             long       = 'Company Code' ).
            change_col_text( EXPORTING columnname = 'BELNR' short      = 'Docmnt.No'
                             medium     = 'Document Number'
                             long       = 'Document Number' ).
            change_col_text( EXPORTING columnname = 'GJAHR' short      = 'FI Year'
                             medium     = 'Fiscal Year'
                             long       = 'Fiscal Year' ).
            change_col_text( EXPORTING columnname = 'BLART' short      = 'Doc.Type'
                             medium     = 'Document type'
                             long       = 'Document type' ).
            change_col_text( EXPORTING columnname = 'LIFNR' short      = 'Vendor'
                             medium     = ' Vendor Code'
                             long       = ' Vendor Code' ).
            change_col_text( EXPORTING columnname = 'WERKS' short      = 'Plant'
                             medium     = 'Plant'
                             long       = 'Plant' ).
            change_col_text( EXPORTING columnname = 'BLDAT' short      = 'Docmnt.Dt'
                             medium     = 'Document Date'
                             long       = 'Document Date' ).
            change_col_text( EXPORTING columnname = 'XBLNR' short      = 'Referance'
                             medium     = 'Referance Number'
                             long       = 'Referance Numner' ).
            change_col_text( EXPORTING columnname = 'ZUONR' short      = 'Assignment'
                             medium     = 'Assignment'
                             long       = 'Assignment' ).
            change_col_text( EXPORTING columnname = 'MENGE' short      = 'Quantity'
                             medium     = 'Quantity'
                             long       = 'Quantity' ).
            change_col_text( EXPORTING columnname = 'MEINS' short      = 'UOM'
                             medium     = 'UOM'
                             long       = 'UOM' ).
            change_col_text( EXPORTING columnname = 'MATNR' short      = 'Material'
                             medium     = 'Material'
                             long       = 'Material' ).
            change_col_text( EXPORTING columnname = 'MAKTX' short      = 'Mat.Descr.'
                             medium     = 'Material Descr.'
                             long       = 'Material Descr.' ).
            change_col_text( EXPORTING columnname = 'EBELN' short      = 'PO Number'
                             medium     = 'Purchase Order'
                             long       = 'Purchase Order' ).
            change_col_text( EXPORTING columnname = 'EBELP' short      = 'PO Item'
                             medium     = 'Purchase Order line '
                             long       = 'Purchase Order Line Item' ).
            change_col_text( EXPORTING columnname = 'VEND_MAIL_ID' short      = 'Vendor ID'
                             medium     = 'Vendor Mail ID'
                             long       = 'Vendor Mail ID' ).
            change_col_text( EXPORTING columnname = 'USER_MAIL_ID' short      = 'User ID'
                             medium     = 'User Mail ID'
                             long       = 'User Mail ID' ).
          ENDIF.

          lo_layout = lo_table->get_layout( ).
          IF lo_layout IS BOUND.
            ls_key-report = sy-cprog.

            lo_layout->set_key( value = ls_key ).

            lo_layout->set_save_restriction( value = if_salv_c_layout=>restrict_none ).
          ENDIF.

          lo_selection = lo_table->get_selections( ).
          IF lo_selection IS BOUND.
            lo_selection->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).
          ENDIF.

          lo_event = lo_table->get_event( ).
          IF lo_event IS BOUND.
            SET HANDLER user_command FOR lo_event.
          ENDIF.

          lo_table->display( ).
        ENDIF.
      CATCH cx_salv_msg.
    ENDTRY.

  endmethod.
  method send_mail.

  endmethod.
  method change_col_text.

  endmethod.
  method user_command.

  endmethod.
endclass.

load-of-program.
  data lo_mod type ref to lcl_module.
  lo_mod = new #( ).

start-of-selection.

  if lo_mod is bound.
    lo_mod->process( ).
  endif.

at selection-screen on value-request for p_vari.
  lo_mod->f4_variant( ).

at selection-screen.
  lo_mod->variant_exist( ).
