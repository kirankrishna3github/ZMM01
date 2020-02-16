class ZCL_IM_6MM021B_PR_ENHANCE definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6MM021B_PR_ENHANCE
*"* do not include other source files here!!!
public section.

  interfaces IF_EX_ME_PROCESS_REQ_CUST .
protected section.
*"* protected components of class ZCL_IM_6MM021B_PR_ENHANCE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6MM021B_PR_ENHANCE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6MM021B_PR_ENHANCE IMPLEMENTATION.


METHOD if_ex_me_process_req_cust~check.
**Added by NK on 02.08.2017 for HSN code validation for Acct. caty.= K & Service = D
  DATA : ls_header      TYPE mereq_header.
  DATA : l_items        TYPE mmpur_requisition_items.
  DATA : l_single       TYPE mmpur_requisition_item.
  DATA : l_items_header TYPE mereq_item,
         im_item1       TYPE REF TO if_purchase_requisition_item.

  DATA:
    lt_esll          TYPE mmsrv_esll,
    ls_esll          LIKE LINE OF lt_esll,
    lr_item          TYPE REF TO cl_req_item_proxy_mm,
    lv_taxtariffcode TYPE esll-taxtariffcode.

  ls_header = im_header->get_data( ).

  l_items = im_header->get_items( ).
  DATA:
    lv_steuc  TYPE steuc. " HSN code

  DATA: lo_mm TYPE REF TO if_services_mm .
  LOOP AT l_items INTO l_single.
    IF sy-tcode = 'ME51N' OR sy-tcode = 'ME52N' OR sy-tcode = 'ME53N'." OR sy-tcode = 'ME54N'.
      CALL METHOD l_single-item->get_data
        RECEIVING
          re_data = l_items_header.

      IF l_items_header-matnr IS NOT INITIAL.
        SELECT SINGLE steuc FROM marc INTO lv_steuc
          WHERE matnr = l_items_header-matnr
          AND   werks = l_items_header-werks.

        IF lv_steuc IS NOT INITIAL.
**       Do nothing
        ELSE.
          MESSAGE 'HSN code for Material not maintained' TYPE 'E'.
        ENDIF.
      ENDIF.

      IF l_items_header-pstyp = '9'.
        IF l_items_header-knttp = 'K'.

*    lr_item ?= l_single-item.
          mmpur_dynamic_cast lo_mm l_single-item. " cl_po_item_handle_mm

          CALL METHOD lo_mm->get_srv_data
            EXPORTING
              im_packno = l_items_header-packno
*             im_limit  = MMPUR_NO
*             im_ebeln  =
            IMPORTING
              ex_esll   = lt_esll
*             ex_esuh   =
*             ex_esuc   =
*             ex_eskl   =
*                          EXCEPTIONS
*             failure   = 1
*             others    = 2
            .
          IF sy-subrc <> 0.
*                         Implement suitable error handling here
          ENDIF.
          DELETE lt_esll WHERE menge = '0.000'.
          READ TABLE lt_esll INTO ls_esll WITH KEY taxtariffcode = ' '.
          IF sy-subrc EQ 0.
            MESSAGE 'Tax Tariff Code for Non-coded Material not maintained' TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF sy-tcode = 'ME54N'.
      CALL METHOD l_single-item->get_data
        RECEIVING
          re_data = l_items_header.

      IF l_items_header-pstyp = '9'.
        IF l_items_header-knttp = 'K'.

          l_items_header-packno = l_items_header-packno + 1.
          SELECT SINGLE taxtariffcode FROM esll INTO lv_taxtariffcode WHERE packno = l_items_header-packno.
          IF sy-subrc NE 0.
*** added by NK on 23.08.2017
*** This below logic is added again since in some cases the data is not found in ESLL with above condition !!
*    lr_item ?= l_single-item.
            l_items_header-packno = l_items_header-packno - 1.
            mmpur_dynamic_cast lo_mm l_single-item. " cl_po_item_handle_mm

            CALL METHOD lo_mm->get_srv_data
              EXPORTING
                im_packno = l_items_header-packno
*               im_limit  = MMPUR_NO
*               im_ebeln  =
              IMPORTING
                ex_esll   = lt_esll
*               ex_esuh   =
*               ex_esuc   =
*               ex_eskl   =
*                          EXCEPTIONS
*               failure   = 1
*               others    = 2
              .
            IF sy-subrc <> 0.
*                         Implement suitable error handling here
            ENDIF.
            DELETE lt_esll WHERE menge = '0.000'.
            READ TABLE lt_esll INTO ls_esll INDEX 1.
**        reading the PACKNO only - so no SY-SUBRC check is required !!
            SELECT SINGLE taxtariffcode FROM esll INTO lv_taxtariffcode WHERE packno = ls_esll-packno.
            IF sy-subrc NE 0.
              MESSAGE 'Tax Tariff Code for Non-coded Material not maintained' TYPE 'E'.
            ENDIF.
***
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDMETHOD.


method IF_EX_ME_PROCESS_REQ_CUST~CLOSE.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_HEADER_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM.
  DATA: l_preq TYPE mereq_item.

  CALL METHOD IM_ITEM->get_data
    RECEIVING
      re_data = l_preq.

* Convert Requisitioner name to upper case
  l_preq-BEDNR = sy-uname.
  TRANSLATE l_preq-BEDNR TO UPPER CASE.

  CALL METHOD IM_ITEM->set_data
    EXPORTING
      im_data = l_preq.

endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~FIELDSELECTION_ITEM_REFKEYS.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~INITIALIZE.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~OPEN.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~POST.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_ACCOUNT.
endmethod.


method IF_EX_ME_PROCESS_REQ_CUST~PROCESS_HEADER.

endmethod.


METHOD if_ex_me_process_req_cust~process_item.

  DATA: ev_maktx TYPE mereq_item-txz01.
  DATA: l_preq TYPE mereq_item.

  CALL METHOD im_item->get_data
    RECEIVING
      re_data = l_preq.

* Convert Requisitioner name to upper case
  l_preq-bednr = sy-uname.
***** Added by Naren Karra - 25.08.2016 -Start
  IF l_preq-matnr IS NOT INITIAL.
    SELECT SINGLE maktx FROM makt INTO ev_maktx WHERE matnr EQ l_preq-matnr AND spras EQ sy-langu.
    IF sy-subrc EQ 0.
      l_preq-txz01 = ev_maktx.
    ENDIF.
  ENDIF.
***** Added by Naren Karra - 25.08.2016 -End
  CALL METHOD im_item->set_data
    EXPORTING
      im_data = l_preq.

ENDMETHOD.
ENDCLASS.
