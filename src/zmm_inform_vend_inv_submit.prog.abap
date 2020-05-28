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
         ekgrp TYPE ekko-ekgrp,
       end of ty_bseg.

types: begin of ty_final.
         include type zstr_vend_inv.
*types : vend_mail_id type adr6-smtp_addr,
*        user_mail_id type adr6-smtp_addr,
 types: end of ty_final.

data: it_bsis   type table of ty_bsis,
      ls_bsis   type ty_bsis,
      it_bseg   type table of ty_bseg,
      ls_bseg   type ty_bseg,
      lt_final  type table of ty_final,
      lt_final1 type table of ty_final,
      ls_final1 type ty_final,
      ls_final  type ty_final.

data: lo_table     type ref to cl_salv_table,
      lo_functions type ref to cl_salv_functions_list,
      lo_display   type ref to cl_salv_display_settings,
      lo_columns   type ref to cl_salv_columns_table,
      lo_column    type ref to cl_salv_column_table,
      lo_layout    type ref to cl_salv_layout,
      ls_key       type salv_s_layout_key,
      lo_selection type ref to cl_salv_selections,
      lo_event     type ref to cl_salv_events_table.

data : lv_save(1) type c,
       lv_exit(1) type c,
       lv_fm      type rs38l_fnam,
       ls_variant type disvariant,
       gs_variant type disvariant.

data: control         type ssfctrlop,
      job_output_info type ssfcrescl,
      output_opt      type ssfcompop,
      lt_pdf          type table of tline,
      lt_doc          type table of docs,
      ls_content      type xstring,
      lt_solix        type solix_tab.

data: attachment  type zfi_s_vp_attachment,
      attachments type table of zfi_s_vp_attachment,
      header      type string,
      content     type string,
      body        type soli_tab,
      wa_body     like line of body,
      subject     type string,
      sent        type abap_bool,
      recipient   type zfi_s_vp_recipient,
      recipients  like standard table of recipient.


selection-screen begin of block b1 with frame title text-001.
select-options: s_lifnr for bseg-lifnr,
                s_bukrs for bkpf-bukrs no-extension no intervals obligatory,
                s_belnr for bkpf-belnr,
                s_gjahr for bkpf-gjahr no-extension no intervals obligatory,
                s_bldat for bsis-bldat obligatory,
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

    check lt_final[] is not initial.

    sort lt_final[] by lifnr werks.

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
        and blart = 'WE'.
      if it_bsis is not initial .

        select a~bukrs a~belnr a~gjahr a~zuonr a~menge a~meins
               a~matnr a~ebeln a~ebelp a~lifnr a~dmbtr b~ekgrp
          from bseg as a
          JOIN ekko as b
          on a~ebeln = b~ebeln
          into corresponding fields of table it_bseg
          for all entries in it_bsis
          where a~bukrs = it_bsis-bukrs
          and a~belnr = it_bsis-belnr
          and a~gjahr = it_bsis-gjahr
          and a~hkont in s_hkont.

      else.
        message 'No Data Found' type 'E'.

      endif.
    endif.

  endmethod.
  method process_data.

    loop at it_bseg into ls_bseg  .

      ls_final-lifnr = ls_bseg-lifnr.
      ls_final-zuonr = ls_bseg-zuonr.
      ls_final-menge = ls_bseg-menge.
      ls_final-meins = ls_bseg-meins.
      ls_final-matnr = ls_bseg-matnr.
      ls_final-ebeln = ls_bseg-ebeln.
      ls_final-ebelp = ls_bseg-ebelp.
      ls_final-dmbtr = ls_bseg-dmbtr.
      ls_final-ekgrp = ls_bseg-ekgrp.
      select single maktx from makt into ls_final-maktx where matnr = ls_bseg-matnr.

      read table it_bsis into ls_bsis with key bukrs = ls_bseg-bukrs belnr = ls_bseg-belnr gjahr = ls_bseg-gjahr .
      if sy-subrc = 0.
        ls_final-bukrs = ls_bsis-bukrs.
        ls_final-belnr = ls_bsis-belnr.
        ls_final-gjahr = ls_bsis-gjahr.
        ls_final-bldat = ls_bsis-bldat.
        ls_final-xblnr = ls_bsis-xblnr.
        ls_final-blart = ls_bsis-blart.
        ls_final-werks = ls_bsis-werks.
      endif.

      select single name1 ,adrnr from lfa1 into (  @ls_final-name1 , @data(zadrnr) ) where lifnr = @ls_bseg-lifnr.
      if zadrnr is not initial.

        select single smtp_addr from adr6 into ls_final-vend_mail_id where addrnumber = zadrnr  .
      endif.

      SELECT SINGLE paramval FROM Z6MMA_PARAMS INTO ls_final-user_mail_id WHERE progname = 'ZMM026' AND param1 = ls_final-ekgrp.


      append ls_final to lt_final.

    endloop.


  endmethod.
  method call_sf.

    "call smartform
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = 'ZMMS_INFORM_VEND_INV_SUBMIT'
*       VARIANT            = ' '
*       DIRECT_CALL        = ' '
      importing
        fm_name            = lv_fm
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    control-no_dialog = 'X'.
    control-getotf    = 'X'.

*    control-no_dialog = ' '.
*    control-preview   = 'X'.
*    control-no_open   = 'X'.
*    control-no_close  = 'X'.

    call function lv_fm
      exporting
        control_parameters = control
        output_options     = output_opt
      importing
        job_output_info    = job_output_info
      tables
        lt_final           = lt_final1
      exceptions
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        others             = 5.
    if sy-subrc eq 0.
      call function 'CONVERT_OTF'
        exporting
          format                = 'PDF'
        tables
          otf                   = job_output_info-otfdata
          lines                 = lt_pdf
        exceptions
          err_max_linewidth     = 1
          err_format            = 2
          err_conv_not_possible = 3
          err_bad_otf           = 4
          others                = 5.
      if sy-subrc eq 0.
        loop at lt_pdf into data(ls_pdf).
          assign ls_pdf to <fs_x> casting.
          concatenate ls_content <fs_x> into ls_content in byte mode.
        endloop.

        check ls_content is not initial.
        attachment-att_type =  'BIN'.
        attachment-att_subj = condense( |Indofil_Invoice_Submission({ ls_final-lifnr alpha = out })-{ ls_final-werks }.pdf| ).
        attachment-att_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = ls_content ).
        append attachment to attachments.
        clear attachment.
      endif.
    endif.



  endmethod.
  method display_alv.
    try.
        cl_salv_table=>factory(
          exporting
            list_display   = if_salv_c_bool_sap=>false
          importing
            r_salv_table   = lo_table
          changing
            t_table        = lt_final
        ).

        if lo_table is bound.

          lo_table->set_screen_status(
            exporting
              report        = 'ZMM_INFORM_VEND_INV_SUBMIT'
              pfstatus      = 'STANDARD'
              set_functions = lo_table->c_functions_all ).

          lo_functions = lo_table->get_functions( ).
          if lo_functions is bound.
            lo_functions->set_all( value = if_salv_c_bool_sap=>true ).
          endif.

          lo_display = lo_table->get_display_settings( ).
          if lo_display is bound.
            lo_display->set_striped_pattern( value = if_salv_c_bool_sap=>true ).
          endif.

          lo_columns = lo_table->get_columns( ).
          if lo_columns is bound.
*            CLEAR: columnname, short, medium, long.
            change_col_text( exporting columnname = 'BUKRS' short      = 'Comp Code.'
                             medium     = 'Company Code'
                             long       = 'Company Code' ).
            change_col_text( exporting columnname = 'BELNR' short      = 'Docmnt.No'
                             medium     = 'Document Number'
                             long       = 'Document Number' ).
            change_col_text( exporting columnname = 'GJAHR' short      = 'FI Year'
                             medium     = 'Fiscal Year'
                             long       = 'Fiscal Year' ).
            change_col_text( exporting columnname = 'BLART' short      = 'Doc.Type'
                             medium     = 'Document type'
                             long       = 'Document type' ).
            change_col_text( exporting columnname = 'LIFNR' short      = 'Vendor'
                             medium     = ' Vendor Code'
                             long       = ' Vendor Code' ).
            change_col_text( exporting columnname = 'WERKS' short      = 'Plant'
                             medium     = 'Plant'
                             long       = 'Plant' ).
            change_col_text( exporting columnname = 'BLDAT' short      = 'Docmnt.Dt'
                             medium     = 'Document Date'
                             long       = 'Document Date' ).
            change_col_text( exporting columnname = 'XBLNR' short      = 'Referance'
                             medium     = 'Referance Number'
                             long       = 'Referance Numner' ).
            change_col_text( exporting columnname = 'ZUONR' short      = 'Assignment'
                             medium     = 'Assignment'
                             long       = 'Assignment' ).
            change_col_text( exporting columnname = 'MENGE' short      = 'Quantity'
                             medium     = 'Quantity'
                             long       = 'Quantity' ).
            change_col_text( exporting columnname = 'MEINS' short      = 'UOM'
                             medium     = 'UOM'
                             long       = 'UOM' ).
            change_col_text( exporting columnname = 'MATNR' short      = 'Material'
                             medium     = 'Material'
                             long       = 'Material' ).
            change_col_text( exporting columnname = 'MAKTX' short      = 'Mat.Descr.'
                             medium     = 'Material Descr.'
                             long       = 'Material Descr.' ).
            change_col_text( exporting columnname = 'EBELN' short      = 'PO Number'
                             medium     = 'Purchase Order'
                             long       = 'Purchase Order' ).
            change_col_text( exporting columnname = 'EBELP' short      = 'PO Item'
                             medium     = 'Purchase Order line '
                             long       = 'Purchase Order Line Item' ).
            change_col_text( exporting columnname = 'VEND_MAIL_ID' short      = 'Vendor ID'
                             medium     = 'Vendor Mail ID'
                             long       = 'Vendor Mail ID' ).
            change_col_text( exporting columnname = 'USER_MAIL_ID' short      = 'User ID'
                             medium     = 'User Mail ID'
                             long       = 'User Mail ID' ).
          endif.

          lo_layout = lo_table->get_layout( ).
          if lo_layout is bound.
            ls_key-report = sy-cprog.

            lo_layout->set_key( value = ls_key ).

            lo_layout->set_save_restriction( value = if_salv_c_layout=>restrict_none ).
          endif.

          lo_selection = lo_table->get_selections( ).
          if lo_selection is bound.
            lo_selection->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).
          endif.

          lo_event = lo_table->get_event( ).
          if lo_event is bound.
            set handler user_command for lo_event.
          endif.

          lo_table->display( ).
        endif.
      catch cx_salv_msg.
    endtry.

  endmethod.
  method change_col_text .
    try.
        lo_column ?= lo_columns->get_column( columnname = columnname ).
        lo_column->set_short_text( value = short ).
        lo_column->set_medium_text( value = medium ).
        lo_column->set_long_text( value = long ).

        if columnname cs 'MAIL_ID'.
          lo_column->set_output_length( value = '35' ).
        endif.
      catch cx_salv_not_found.
    endtry.
  endmethod.
  method send_mail.
    "mail body
    refresh body.
    clear wa_body.
    wa_body-line = 'Dear Business Partner'.
    append wa_body to body.
    append initial line to body.

    clear wa_body.
    wa_body-line = 'Please find attached details of Invoice Submission'.
    append wa_body to body.
    append initial line to body.

    "subject
    clear subject.
    subject = condense( |Indofil Invoice Submission({ ls_final-lifnr alpha = out })| ).

    "add receipients
    "vendor's mail id
    clear recipient.
    refresh recipients.

    recipient-recipient = ls_final1-vend_mail_id.
    recipient-copy = abap_false.
    append recipient to recipients.

    "user mail id
    recipient-recipient = ls_final1-user_mail_id.
    recipient-copy = abap_true.
    append recipient to recipients.

    if subject is not initial and recipients is not initial.
      clear sent.
      new zcl_email( )->send_email( exporting subject = subject
                                              sender         = 'sapautomail@indofil.com'
                                              body           = body
                                              body_obj_type  = 'RAW'
                                              recipients     = recipients
                                              attachments    = attachments
*                                                                     COND #( WHEN ls_content IS NOT INITIAL
*                                                                     THEN VALUE #( ( att_type  = 'BIN'
*                                                                     att_subj = |Returns_of_indofil.pdf|
*                                                                     att_solix =
*                                                                     cl_bcs_convert=>xstring_to_solix( iv_xstring = ls_content ) ) )
                                    importing
                                              sent           = sent ).

      message cond #( when sent eq abap_true then 'Email sent successfully' else 'Email sending failed' ) type 'S'.
    endif.
  endmethod.

  method user_command.
    types: begin of ty_lifnr,
             lifnr type lfa1-lifnr,
           end of ty_lifnr.
    data :lt_select type salv_t_row,
          lrt_lifnr type table of ty_lifnr.
    if e_salv_function eq 'MAIL'.
      clear ls_final.
      refresh: lt_final1[],attachments[],lt_select,lrt_lifnr.

      lt_select = lo_selection->get_selected_rows( ).
      if lt_select is initial.
        message 'Please select a Vendor to send mail' type 'E'.
      endif.

      loop at lt_select into data(ls_select).
        try.
            append value #( lifnr = lt_final[ ls_select ]-lifnr ) to lrt_lifnr.
          catch cx_sy_itab_line_not_found.
        endtry.
      endloop.

      sort lrt_lifnr[] by lifnr.
      delete adjacent duplicates from lrt_lifnr[] comparing lifnr.

      sort lt_final[] by lifnr.

      loop at lt_final into ls_final.
        ls_final1 = ls_final.
        if line_exists( lrt_lifnr[ lifnr = ls_final-lifnr ] ).
          append ls_final to lt_final1.
          at end of werks.
            call_sf( ).
          endat.
          at end of lifnr.
            if attachments[] is not initial.
              send_mail( ).
              refresh: attachments[],lt_final1[].
            endif.
          endat.
        endif.
      endloop.
    endif.

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
