*&---------------------------------------------------------------------*
*& Report  ZMM_J_1IG_INV_MASS
*&
*&---------------------------------------------------------------------*
*& IRDK932010, IRDK932012: SD: S_K: ZMM_J_1IG_INV_MASS: Fixes after duplicate odn issue
*&
*&---------------------------------------------------------------------*
report zmm_j_1ig_inv_mass.

data: begin of wa_sto,
        ebeln type ekko-ebeln,
        reswk type ekko-reswk,
      end of wa_sto,
      it_sto like standard table of wa_sto,

      begin of wa_po_hist,
        ebeln type ekbe-ebeln,
        belnr type ekbe-belnr,
        gjahr type ekbe-gjahr,
      end of wa_po_hist,
      it_po_hist like standard table of wa_po_hist,

      begin of wa_gr,
        bukrs    type mseg-bukrs,
        mblnr    type mseg-mblnr,
        mjahr    type mseg-mjahr,
        werks    type mseg-werks,
        ebeln    type mseg-ebeln,
        vbeln_im type mseg-vbeln_im,
      end of wa_gr,
      it_gr like standard table of wa_gr,

      begin of wa_delv_flow,
        vbelv type vbfa-vbelv,
        vbeln type vbfa-vbeln,
      end of wa_delv_flow,
      it_delv_flow like standard table of wa_delv_flow,

      begin of wa_invoice,
        vbeln type vbrk-vbeln,
        erdat type vbrk-erdat,
        bukrs type vbrk-bukrs,
      end of wa_invoice,
      it_invoice like standard table of wa_invoice,

      begin of wa_inb_inv,
        bukrs type bkpf-bukrs,
        belnr type bkpf-belnr,
        gjahr type bkpf-gjahr,
      end of wa_inb_inv,

      begin of wa_inv_acc,
        bukrs type bkpf-bukrs,
        belnr type bkpf-belnr,
        gjahr type bkpf-gjahr,
        xblnr type bkpf-xblnr,
        bktxt type bkpf-bktxt,
      end of wa_inv_acc,
      it_inv_acc like standard table of wa_inv_acc,

      begin of wa_bdcdata,
        p_bukrs_001  type bdcdata-fval, " Comp Code
        p_swerks_002 type bdcdata-fval, " Suppl Plant
        p_werks_003  type bdcdata-fval, " Recv. Plant
        low_005      type bdcdata-fval, " STO Inv
        low_006      type bdcdata-fval, " STO Inv date
      end of wa_bdcdata,

      begin of wa_out,
        light(1)    type c,
        sto_po      type mseg-ebeln,
        sto_delv    type mseg-vbeln_im,
        sto_inv     type vbfa-vbeln,
        sto_inv_dt  type vbrk-erdat,
        sto_gr      type mseg-mblnr,
        sto_gr_yr   type mseg-mjahr,
        suppl_plant type ekko-reswk,
        recv_plant  type mseg-werks,
        comp_code   type mseg-bukrs,
        message     type bapi_msg,
      end of wa_out,
      it_out      like standard table of wa_out,

      messtab     like standard table of bdcmsgcoll,
      wa_mess     like bdcmsgcoll,

      wa_bapimess like bapiret2.

data: subrc like syst-subrc.

data: o_table     type ref to cl_salv_table,
      o_container type ref to cl_gui_container,
      o_functions type ref to cl_salv_functions_list,
      o_columns   type ref to cl_salv_columns_table,
      o_column    type ref to cl_salv_column,
      o_layout    type ref to cl_salv_layout,
      o_layo      type ref to cl_salv_layout_service,
      o_key       type salv_s_layout_key,
      o_info      type salv_s_layout_info.

*selection-screen begin of block b1 with frame title text-001.
*parameters: p_date LIKE ekko-bedat.
*selection-screen end of block b1.

start-of-selection.
  perform fetch_data.

end-of-selection.
  perform j_1ig_inv_bdc.
  perform display_results.
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fetch_data .
  select ebeln reswk
    from ekko
    into table it_sto
    where bedat ge '20170701' " sy-datum
    and   bsart in ('YSTO', 'ZSTO')
    and   loekz <> 'X'.

  check it_sto is not initial.
  select distinct ebeln belnr gjahr
    from ekbe
    into table it_po_hist
    for all entries in it_sto
    where ebeln = it_sto-ebeln
    and   vgabe = '1' " Goods Reciept
    and   bewtp = 'E' " WE, Goods Reciept
    and   bwart = '101'.

  check it_po_hist is not initial.
  select distinct bukrs mblnr mjahr werks ebeln vbeln_im  " Delivery number
    from mseg
    into table it_gr
    for all entries in it_po_hist
    where mblnr = it_po_hist-belnr
    and   mjahr = it_po_hist-gjahr
    and   ebeln = it_po_hist-ebeln
    and   vgart_mkpf = 'WE'  " WE, Goods Reciept
    and   bwart      = '101'.

  check it_gr is not initial.
  select distinct vbelv vbeln
    from vbfa
    into table it_delv_flow
    for all entries in it_gr
    where vbelv = it_gr-vbeln_im
    and   vbtyp_n = 'M'.  " Invoice

  check it_delv_flow is not initial.
  select distinct vbeln erdat bukrs
    from vbrk
    into table it_invoice
    for all entries in it_delv_flow
    where vbeln = it_delv_flow-vbeln
    and   fkart = 'ZSTO'
    and   rfbsk = 'C'
    and   fksto = ' '.

  check it_invoice is not initial.
  select bukrs belnr gjahr xblnr bktxt
    from bkpf
    into table it_inv_acc
    for all entries in it_invoice
    where bukrs = it_invoice-bukrs
    and   belnr = it_invoice-vbeln
    and   blart = 'RV'.
endform.
*&---------------------------------------------------------------------*
*&      Form  J_1IG_INV_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form j_1ig_inv_bdc .
  data: bktxt_invref type bkpf-bktxt.
  if it_invoice is not initial.
    loop at it_invoice into wa_invoice.
      clear: wa_out, wa_bdcdata.

      clear wa_inv_acc.
      read table it_inv_acc into wa_inv_acc with key belnr = wa_invoice-vbeln.
      if sy-subrc = 0.
        clear wa_delv_flow.
        read table it_delv_flow into wa_delv_flow with key vbeln = wa_invoice-vbeln.
        if sy-subrc = 0.
          clear wa_gr.
          read table it_gr into wa_gr with key vbeln_im = wa_delv_flow-vbelv.
          if sy-subrc = 0.
            clear wa_po_hist.
            read table it_po_hist into wa_po_hist with key belnr = wa_gr-mblnr
                                                           gjahr = wa_gr-mjahr.
            if sy-subrc = 0.
              clear wa_sto.
              read table it_sto into wa_sto with key ebeln = wa_po_hist-ebeln.
              if sy-subrc = 0.
                wa_out-sto_po      = wa_po_hist-ebeln.
                wa_out-sto_delv    = wa_gr-vbeln_im.
                wa_out-sto_inv     = wa_invoice-vbeln.
                wa_out-sto_inv_dt  = wa_invoice-erdat.
                wa_out-sto_gr      = wa_gr-mblnr.
                wa_out-sto_gr_yr   = wa_gr-mjahr.
                wa_out-suppl_plant = wa_sto-reswk.
                wa_out-recv_plant  = wa_gr-werks.
                wa_out-comp_code   = wa_gr-bukrs.

                clear bktxt_invref.
                concatenate 'GINB/' wa_invoice-vbeln '/' wa_invoice-erdat into bktxt_invref.
                "Check for existing inb gst inv for ob sto inv
                select single bukrs belnr gjahr
                  from bkpf
                  into wa_inb_inv
                  where bukrs = wa_gr-bukrs
                  and   blart = 'RE'
                  and   tcode eq 'J_1IG_INV'
                  and   ( xblnr eq wa_invoice-vbeln or xblnr eq wa_inv_acc-xblnr )
                  and   bktxt = bktxt_invref
                  and   xblnr ne ''
                  and   stblg = ' '
                  and   stjah = ' '.

                if sy-subrc = 0.
                  wa_out-light   = '2'. "Error
                  wa_out-message = 'GST Inbound Invoice' && ` ` && wa_inb_inv-belnr && ` ` && 'already exists.'.
                else.
                  wa_bdcdata-p_bukrs_001  = wa_out-comp_code.
                  wa_bdcdata-p_swerks_002 = wa_out-suppl_plant.
                  wa_bdcdata-p_werks_003  = wa_out-recv_plant.
                  wa_bdcdata-low_005      = wa_out-sto_inv.

                  call function 'CONVERT_DATE_TO_EXTERNAL'
                    exporting
                      date_internal            = wa_out-sto_inv_dt
                    importing
                      date_external            = wa_bdcdata-low_006
                    exceptions
                      date_internal_is_invalid = 1
                      others                   = 2.
                  if sy-subrc <> 0.
                  endif.

                  " Create GST IB. Inv. via BDC
                  clear subrc.
                  refresh: messtab.
                  call function 'ZFM_J_1IG_INV'
                    exporting
                      p_bukrs_001  = wa_bdcdata-p_bukrs_001  " Comp Code
                      p_swerks_002 = wa_bdcdata-p_swerks_002 " Suppl Plant
                      p_werks_003  = wa_bdcdata-p_werks_003  " Recv. Plant
                      low_005      = wa_bdcdata-low_005      " STO Inv
                      low_006      = wa_bdcdata-low_006      " STO Inv date
                    importing
                      subrc        = subrc
                    tables
                      messtab      = messtab.

                  clear wa_mess.
                  read table messtab into wa_mess with key msgtyp = 'E'.
                  if sy-subrc = 0.
                    wa_out-light  = '1'. "Error
                    clear wa_bapimess.
                    wa_bapimess-id         = wa_mess-msgid.
                    wa_bapimess-number     = wa_mess-msgnr.
                    wa_bapimess-message_v1 = wa_mess-msgv1.
                    wa_bapimess-message_v2 = wa_mess-msgv2.
                    wa_bapimess-message_v3 = wa_mess-msgv3.
                    wa_bapimess-message_v4 = wa_mess-msgv4.
                    call function 'BAPI_MESSAGE_GETDETAIL'
                      exporting
                        id         = wa_bapimess-id
                        number     = wa_bapimess-number
                        language   = sy-langu
                        textformat = 'HTM'
                        message_v1 = wa_bapimess-message_v1
                        message_v2 = wa_bapimess-message_v2
                        message_v3 = wa_bapimess-message_v3
                        message_v4 = wa_bapimess-message_v4
                      importing
                        message    = wa_out-message.
                  else.
                    clear wa_inb_inv.
                    select single bukrs belnr gjahr
                      from bkpf
                      into wa_inb_inv
                      where bukrs = wa_gr-bukrs
                      and   blart = 'RE'
                      and   tcode eq 'J_1IG_INV'
                      and   ( xblnr eq wa_invoice-vbeln or xblnr eq wa_inv_acc-xblnr )
                      and   bktxt = bktxt_invref
                      and   xblnr ne ''
                      and   stblg = ' '
                      and   stjah = ' '.
                    if sy-subrc = 0.
                      wa_out-light   = '3'. "Success
                      wa_out-message = 'GST Inbound Invoice' && ` ` && wa_inb_inv-belnr && ` ` && 'posted in year' && ` ` && wa_inb_inv-gjahr.
                    else.
                      wa_out-light = '1'. " Error
                      wa_out-message = 'Error/s occured while creating document.'.
                    endif.  "Check if gst inb. inv was created
                  endif.  "READ TABLE messtab, for error
                endif.  "Check for existing inb gst inv for ob sto inv
                append wa_out to it_out.
              endif.  "READ TABLE it_sto
            endif.  "READ TABLE it_po_hist
          endif.  "READ TABLE it_gr
        endif.  "READ TABLE it_delv_flow
      else. " read table it_inv_acc
        wa_out-light = '1'. " Error
        wa_out-message = 'Accounting document not posted'.
      endif.  " read table it_inv_acc
      clear wa_invoice.
    endloop.
  else.
    message 'No data selected' type 'S' display like 'E'.
  endif.  "Check it_invoice is not empty
endform.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_results .
  if it_out is not initial.
    try.
        call method cl_salv_table=>factory
          exporting
            list_display = if_salv_c_bool_sap=>false
          importing
            r_salv_table = o_table
          changing
            t_table      = it_out.
      catch cx_salv_msg .
    endtry.

    call method o_table->get_columns
      receiving
        value = o_columns.

    try .
        o_columns->set_exception_column( value = 'LIGHT' ).
      catch cx_salv_data_error.
    endtry.

* change column headers
    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'STO_PO' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'STO' ).
    o_column->set_medium_text( value = 'STO' ).
    o_column->set_short_text( value = 'STO' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'STO_DELV' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'OBD No.' ).
    o_column->set_medium_text( value = 'OBD No.' ).
    o_column->set_short_text( value = 'OBD No.' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'STO_INV' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'Invoice No.' ).
    o_column->set_medium_text( value = 'Invoice No.' ).
    o_column->set_short_text( value = 'Inv.' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'STO_INV_DT' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'Invoice Date.' ).
    o_column->set_medium_text( value = 'Invoice Dt.' ).
    o_column->set_short_text( value = 'In. Dt.' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'STO_GR' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'GR No.' ).
    o_column->set_medium_text( value = 'GR No.' ).
    o_column->set_short_text( value = 'GR No.' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'STO_GR_YR' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'Year' ).
    o_column->set_medium_text( value = 'Year' ).
    o_column->set_short_text( value = 'Year' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'SUPPL_PLANT' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'Supplying Plant' ).
    o_column->set_medium_text( value = 'Suppl. Plant' ).
    o_column->set_short_text( value = 'Sup. Pl.' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'RECV_PLANT' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'Recieving Plant' ).
    o_column->set_medium_text( value = 'Recvg. Plant' ).
    o_column->set_short_text( value = 'Rec. Pl.' ).

    free o_column.
    try.
        o_column ?= o_columns->get_column( columnname = 'COMP_CODE' ).
      catch cx_salv_not_found.
    endtry.
    o_column->set_long_text( value = 'Company Code' ).
    o_column->set_medium_text( value = 'Comp. Code' ).
    o_column->set_short_text( value = 'Comp. Code' ).

    call method o_columns->set_optimize. " Default input bool true

    call method o_table->get_functions
      receiving
        value = o_functions.

    call method o_functions->set_all. " Default input bool true

    call method o_table->get_layout
      receiving
        value = o_layout.

    o_key-report = sy-repid.

    call method o_layout->set_key
      exporting
        value = o_key.

    call method o_layout->set_save_restriction.

    call method o_table->display.
  endif.
endform.
