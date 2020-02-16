*--------------------------------------------------------------------*
* Author: 6010859; SaurabhK
* Thursday, October 17, 2019 23:40:14
* This program fetches all vendors for which default partner functions(OA, PI, VN) are not maintained
* These default partner functions are then created for each of those vendors using a BDC of XK02 -> partner functions
*--------------------------------------------------------------------*
program zmm_bdc_vend_partner_default
       no standard page heading line-size 255.

" global data
data: gv_lifnr type lfm1-lifnr,
      gv_ekorg type lfm1-ekorg.

" selection screen
selection-screen begin of block sel with frame title text-sel.
select-options: s_lifnr for gv_lifnr,
                s_ekorg for gv_ekorg.
selection-screen end of block sel.

selection-screen begin of block opt with frame title text-opt.
parameters: p_fore radiobutton group opt,
            p_back radiobutton group opt default 'X',
            p_err  radiobutton group opt.
selection-screen end of block opt.

parameters: nodata default '/' lower case no-display. " fields that should not be altered in BDC call can be filled with this character

" local class definitions
class lcl_app definition.
  public section.
    methods: process.

  protected section.

  private section.
    data: begin of gs_vendor,
            lifnr type lfm1-lifnr,
            ekorg type lfm1-ekorg,
          end of gs_vendor,
          gt_vendor like standard table of gs_vendor,

          begin of gs_log,
            pstat type icon_d,
            lifnr type lfm1-lifnr,
            ekorg type lfm1-ekorg,
            messg type bapi_msg,
          end of gs_log,
          gt_log        like standard table of gs_log,

          gt_bdcdata    type standard table of bdcdata,
          gt_bdcmessage type standard table of bdcmsgcoll.

    methods:
      get_vendors,

      build_and_run_bdc,

      pf_input_conv
        importing
          iv_parvw        type parvw
        returning
          value(rv_parvw) type parvw,

      bdc_dynpro
        importing
          iv_program type bdcdata-program
          iv_dynpro  type bdcdata-dynpro,

      bdc_field
        importing
          iv_fnam type bdcdata-fnam
          iv_fval type bdcdata-fval,

      display_log.
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.

  private section.
endclass.

" local class implementations
class lcl_app implementation.
  method process.
    get_vendors( ).

    if gt_vendor is not initial.
      build_and_run_bdc( ).

      display_log( ).
    else.
      message 'No data found.' type 'S' display like 'E'.
      return.
    endif.
  endmethod.

  method get_vendors.
    cl_progress_indicator=>progress_indicate(
      exporting
        i_text               = 'Fetching vendors with missing partner data...'               " Progress Text (If no message transferred in I_MSG*)
        i_processed          = 50           " Number of Objects Already Processed
        i_total              = 100          " Total Number of Objects to Be Processed
        i_output_immediately = abap_true ). " X = Display Progress Immediately

    " get vendor data: purchasing org
    refresh gt_vendor.
    select lifnr, ekorg
      from lfm1
      into table @gt_vendor
      where lifnr in @s_lifnr
      and   ekorg in @s_ekorg
      order by lifnr, ekorg ascending.

    data(lv_oa) = pf_input_conv( exporting iv_parvw = 'OA' ). " BA
    data(lv_pi) = pf_input_conv( exporting iv_parvw = 'PI' ). " RS
    data(lv_vn) = pf_input_conv( exporting iv_parvw = 'VN' ). " LF

    " get vendor data: default partner functions
    select lifnr, ekorg, parvw
      from wyt3
      into table @data(lt_partners)
      where parvw in ( @lv_oa, @lv_pi, @lv_vn )
      order by lifnr, ekorg ascending.

    " for each vendor - purchasing org check whether any of the default partner functions is missing
    " if all the default partner functions are maintained; we exclude such vendors as no further processing is required
    " if even one default partner function is missing; we process the vendor via XK02 BDC to maintain the missing default partner functions
    loop at gt_vendor into data(ls_vendor).
      try.
          data(ls_partner) = lt_partners[ lifnr = ls_vendor-lifnr ekorg = ls_vendor-ekorg parvw = lv_oa ].
        catch cx_sy_itab_line_not_found.
          data(lv_missing) = abap_true. " oa not maintained
      endtry.
      try.
          clear ls_partner.
          ls_partner = lt_partners[ lifnr = ls_vendor-lifnr ekorg = ls_vendor-ekorg parvw = lv_pi ].
        catch cx_sy_itab_line_not_found.
          lv_missing = abap_true. " pi not maintained
      endtry.
      try.
          clear ls_partner.
          ls_partner = lt_partners[ lifnr = ls_vendor-lifnr ekorg = ls_vendor-ekorg parvw = lv_vn ].
        catch cx_sy_itab_line_not_found.
          lv_missing = abap_true. " vn not maintained
      endtry.

      " all 3 default partners already maintained
      if lv_missing eq abap_false.
        delete gt_vendor where lifnr = ls_vendor-lifnr and ekorg = ls_vendor-ekorg.
      endif.

      clear: ls_vendor, ls_partner, lv_missing.
    endloop.
  endmethod.

  method build_and_run_bdc.
    refresh: gt_bdcdata, gt_log.
    check gt_vendor is not initial.

    " set bdc execution mode
    case abap_true.
      when p_fore.
        data(lv_mode) = 'A'.
      when p_back.
        lv_mode = 'N'.
      when p_err.
        lv_mode = 'E'.
    endcase.

    " set bdc update mode
    data(lv_update) = 'A'.  " Asynchronous - commit work without wait

    loop at gt_vendor into data(ls_vendor).

      if sy-tabix mod 50 eq 0.  " display progress indicator every 50 records processed
        cl_progress_indicator=>progress_indicate(
          exporting
            i_text               = |Processing vendor { ls_vendor-lifnr alpha = out } / { ls_vendor-ekorg }...|  " Progress Text (If no message transferred in I_MSG*)
            i_processed          = sy-tabix           " Number of Objects Already Processed
            i_total              = lines( gt_vendor[] )          " Total Number of Objects to Be Processed
            i_output_immediately = abap_true ). " X = Display Progress Immediately

        wait up to 1 seconds.
      endif.

      refresh gt_bdcdata.
      clear gs_log.

      move-corresponding ls_vendor to gs_log.

      " inital xk02 screen - enter vendor, purchasing org, select partner functions and press enter
      bdc_dynpro( exporting iv_program = 'SAPMF02K' iv_dynpro = '0101' ).
      bdc_field( exporting iv_fnam = 'BDC_CURSOR' iv_fval = 'WRF02K-D0320' ).
      bdc_field( exporting iv_fnam =  'BDC_OKCODE' iv_fval = '/00' ).
      bdc_field( exporting iv_fnam =  'RF02K-LIFNR' iv_fval = conv #( ls_vendor-lifnr ) ).
      bdc_field( exporting iv_fnam =  'RF02K-EKORG' iv_fval = conv #( ls_vendor-ekorg ) ).
      bdc_field( exporting iv_fnam =  'WRF02K-D0320' iv_fval = 'X' ).

      " partner functions screen; in edit mode default partner are auto-loaded by system if not maintained
      " we just press save to save the loaded default partner functions
      bdc_dynpro( exporting iv_program = 'SAPMF02K' iv_dynpro = '0320' ).
      bdc_field( exporting iv_fnam = 'BDC_CURSOR' iv_fval = 'WYT3-PARVW(01)' ).
      bdc_field( exporting iv_fnam =  'BDC_OKCODE' iv_fval = '=UPDA' ).

      refresh gt_bdcmessage.
      try.
          call transaction 'XK02'
                           without authority-check
                           using gt_bdcdata
                           mode   lv_mode
                           update lv_update
                           messages into gt_bdcmessage.

          try.
              data(ls_bdcmessage) = gt_bdcmessage[ msgtyp = 'E' ].
              gs_log-pstat = icon_red_light.
            catch cx_sy_itab_line_not_found.
              try.
                  ls_bdcmessage = gt_bdcmessage[ msgid = 'F2' msgnr = '035' ].  " no changes made
                  gs_log-pstat = icon_yellow_light.
                catch cx_sy_itab_line_not_found.
                  try.
                      ls_bdcmessage = gt_bdcmessage[ msgid = 'F2' msgnr = '056' ].  " changes saved
                      gs_log-pstat = icon_green_light.
                    catch cx_sy_itab_line_not_found.
                  endtry.
              endtry.
          endtry.

          message id     ls_bdcmessage-msgid
                  type   ls_bdcmessage-msgtyp
                  number ls_bdcmessage-msgnr
                  into   gs_log-messg
                  with   ls_bdcmessage-msgv1
                         ls_bdcmessage-msgv2
                         ls_bdcmessage-msgv3
                         ls_bdcmessage-msgv4.

        catch cx_sy_authorization_error into data(lox_auth_check).
      endtry.

      append gs_log to gt_log.

      wait up to 1 seconds. " IHDK903749

      clear: ls_vendor, ls_bdcmessage.
    endloop.
  endmethod.

  method pf_input_conv.
    check iv_parvw is not initial.
    clear rv_parvw.
    call function 'CONVERSION_EXIT_PARVW_INPUT'
      exporting
        input  = iv_parvw
      importing
        output = rv_parvw.
  endmethod.

  method display_log.
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table   = data(lo_alv)              " Basis Class Simple ALV Tables
          changing
            t_table        = gt_log ).

        check lo_alv is bound.

        data(lo_columns) = lo_alv->get_columns( ).
        if lo_columns is bound.
          try.
              data(lo_column) = lo_columns->get_column( exporting columnname = 'PSTAT' ).
              if lo_column is bound.
                lo_column->set_short_text( exporting value = 'Proc Stat' ).
                lo_column->set_medium_text( exporting value = 'Proc Stat' ).
                lo_column->set_long_text( exporting value = 'Processing Status' ).
              endif.
            catch cx_salv_not_found. " ALV: General Error Class (Checked During Syntax Check)
          endtry.
          lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
        endif.


        data(lo_functions) = lo_alv->get_functions( ).
        if lo_functions is bound.
          lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_layout) = lo_alv->get_layout( ).

        data(lo_key) = value salv_s_layout_key( report = sy-repid ).

        if lo_layout is bound.
          lo_layout->set_key( exporting value = lo_key ).

          lo_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

          lo_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_display) = lo_alv->get_display_settings( ).

        if lo_display is bound.
          lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
        endif.

        lo_alv->display( ).
      catch cx_salv_msg. " ALV: General Error Class with Message
    endtry.
  endmethod.

  method bdc_dynpro.
    data: ls_bdcdata type bdcdata.
    clear ls_bdcdata.
    ls_bdcdata-program  = iv_program.
    ls_bdcdata-dynpro   = iv_dynpro.
    ls_bdcdata-dynbegin = abap_true.
    append ls_bdcdata to gt_bdcdata.
  endmethod.

  method bdc_field.
    data: ls_bdcdata type bdcdata.
    clear ls_bdcdata.

    if iv_fval <> nodata.
      ls_bdcdata-fnam = iv_fnam.
      ls_bdcdata-fval = iv_fval.
      append ls_bdcdata to gt_bdcdata.
    endif.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    try.
        new lcl_app( )->process( ).
      catch cx_root.
    endtry.
  endmethod.
endclass.

start-of-selection.
  lcl_main=>start( ).
