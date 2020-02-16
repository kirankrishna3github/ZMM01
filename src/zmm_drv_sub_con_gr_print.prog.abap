*&---------------------------------------------------------------------*
*& Report ZMM_DRV_SUB_CON_GR_PRINT
*&---------------------------------------------------------------------*
*& Transaction            : ZMM082
*& Creation Date          : Thursday, December 14, 2017 11:21:22
*& Author                 : ABAP02 - SaurabhK
*& Functional             : Kamalakar Varma
*& Requested/Approved By  : Kamalakar Varma
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK930451
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&---------------------------------------------------------------------*
*& 1. Sub Con Goods reciept print program -> mvt types 101, 543
*&---------------------------------------------------------------------*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Wednesday, July 17, 2019 23:18:15
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : VG/KV
*& Rev. Request#              : IHDK902509
*& Rev. Description           : MM: S_K: ZMM082: Sub con GR print: re-created in S4: 17.7.19
*&---------------------------------------------------------------------*
report zmm_drv_sub_con_gr_print.

" For selection screen types
data: matdoc type mkpf-mblnr.
data: year   type mkpf-mjahr.

* ---- Selection Screen and Global Data ---- *
selection-screen begin of block sel with frame title text-sel.
select-options: s_matdoc for matdoc,
                s_year   for year.
selection-screen end of block sel.

class lcl_application definition.
  public section.
    methods: process.

  protected section.

  private section.
    methods:
      get_matdocs
        exporting
          matdocs type any table,
      bapi_getdata
        importing
          matdocs type any table,
      print
        importing
          matdoc_head  type any
          matdoc_items type any table
          fm_name      type rs38l_fnam,

      initialize_form
        exporting
          fm_name type rs38l_fnam.
endclass.

class lcl_application implementation.
  method process.
    data: begin of matdoc,
            mblnr type mkpf-mblnr,
            mjahr type mkpf-mjahr,
          end of matdoc,
          matdocs like standard table of matdoc.

    refresh matdocs.
    get_matdocs( importing matdocs = matdocs ).

    if matdocs is not initial.
      bapi_getdata( exporting matdocs = matdocs ).
    else.
      message 'No data found' type 'S' display like 'E'.
      return.
    endif.
  endmethod.

  method get_matdocs.
    select mblnr mjahr
      from mkpf
      into table matdocs
      where mblnr in s_matdoc
      and   mjahr in s_year
      and   vgart eq 'WE'
      and   blart eq 'WE'.
  endmethod.

  method bapi_getdata.
    check matdocs is not initial.

    data fm_name type rs38l_fnam.
    clear fm_name.
    initialize_form( importing fm_name = fm_name ).

    field-symbols: <matdoc> type any,
                   <doc>    type any,
                   <year>   type any.

    data: doc  type bapi2017_gm_head_02-mat_doc,
          year type bapi2017_gm_head_02-doc_year.

    data matdoc_head  type bapi2017_gm_head_02.
    data matdoc_items type standard table of bapi2017_gm_item_show.
    data return       type standard table of bapiret2.
    data w_return     type bapiret2.

    loop at matdocs assigning <matdoc>.
      clear: doc, year.
      check <matdoc> is assigned and <matdoc> is not initial.
      assign component 'MBLNR' of structure <matdoc> to <doc>.
      if <doc> is assigned and <doc> is not initial.
        doc = <doc>.
      endif.
      assign component 'MJAHR' of structure <matdoc> to <year>.
      if <year> is assigned and <year> is not initial.
        year = <year>.
      endif.

      check doc is not initial and year is not initial.
      clear: matdoc_head, w_return.
      refresh: matdoc_items, return.

      call function 'BAPI_GOODSMVT_GETDETAIL'
        exporting
          materialdocument = doc
          matdocumentyear  = year
        importing
          goodsmvt_header  = matdoc_head
        tables
          goodsmvt_items   = matdoc_items
          return           = return.

      read table return into w_return with key type = 'E'.
      check sy-subrc is not initial and matdoc_items is not initial.

      " exclude non-subcontracting items
      select ebeln, ebelp, pstyp
        from ekpo
        into table @data(lt_po)
        for all entries in @matdoc_items
        where ebeln = @matdoc_items-po_number
        and   ebelp = @matdoc_items-po_item
        and   pstyp eq '3'. " L = 3 = subcontracting item

      " exclude cancelled items
      select mblnr, mjahr, zeile, smbln, sjahr, smblp
        from mseg
        into table @data(lt_cancel)
        for all entries in @matdoc_items
        where smbln = @matdoc_items-mat_doc
        and   sjahr = @matdoc_items-doc_year
        and   smblp = @matdoc_items-matdoc_itm.

      loop at matdoc_items into data(ls_item).
        if not line_exists( lt_po[ ebeln = ls_item-po_number ebelp = ls_item-po_item ] ).
          delete matdoc_items where po_number = ls_item-po_number and po_item = ls_item-po_item.
        endif.

        if line_exists( lt_cancel[ smbln = ls_item-mat_doc sjahr = ls_item-doc_year smblp = ls_item-matdoc_itm ] ).
          delete matdoc_items where po_number = ls_item-po_number and po_item = ls_item-po_item.
        endif.

        " auth check for plant
        authority-check object 'M_MATE_WRK'
         id 'ACTVT' field '03'
         id 'WERKS' field ls_item-plant.
        if sy-subrc <> 0.
          delete matdoc_items where plant = ls_item-plant.
        endif.

        clear ls_item.
      endloop.

      check matdoc_items is not initial.
      print(
        exporting
          matdoc_head  = matdoc_head
          matdoc_items = matdoc_items
          fm_name      = fm_name ).

    endloop.
  endmethod.

  method print.
    data: head  type bapi2017_gm_head_02,
          items type standard table of bapi2017_gm_item_show.

    check matdoc_head is not initial and matdoc_items is not initial and fm_name is not initial.
    head = matdoc_head.
    items[] = matdoc_items[].

    call function fm_name
      exporting
        head             = head
      tables
        items            = items
      exceptions
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4.

  endmethod.

  method initialize_form.
    data formname type tdsfname value 'ZMM_SMF_SUB_CON_GR'.

    clear fm_name.
    call function 'SSF_FUNCTION_MODULE_NAME'
      exporting
        formname           = formname
      importing
        fm_name            = fm_name
      exceptions
        no_form            = 1
        no_function_module = 2
        others             = 3.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
  endmethod.
endclass.

start-of-selection.
  data: lo_app type ref to lcl_application.

  free lo_app.
  lo_app = new lcl_application( ).
  if lo_app is bound.
    lo_app->process( ).
  endif.
