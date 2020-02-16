class ZCL_ZMM_PO_COND_DPC_EXT definition
  public
  inheriting from ZCL_ZMM_PO_COND_DPC
  create public .

public section.
protected section.

  methods PO_CONDSET_GET_ENTITYSET
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZMM_PO_COND_DPC_EXT IMPLEMENTATION.


  method PO_CONDSET_GET_ENTITYSET.
    try.
        data(lv_purchaseorder) =
          conv bapimepoheader-po_number( it_filter_select_options[ property = 'Purchaseorder' ]-select_options[ 1 ]-low ).
      catch cx_sy_itab_line_not_found into data(lox_line_not_found).
    endtry.

    data: lt_return       type standard table of bapiret2,
          lt_pocond       type standard table of bapimepocond.

    refresh: lt_return, lt_pocond.
    check lv_purchaseorder is not initial.
    call function 'BAPI_PO_GETDETAIL1'
      exporting
        purchaseorder = lv_purchaseorder
      tables
        return        = lt_return
        pocond        = lt_pocond.

    " populate entity set from po cond along with po number
    et_entityset = value #( for ls_pocond in lt_pocond
                            " fill ls_entity with po number only
                            " then use that as the base and merge with rest of the values from ls_pocond
                            " append the resulting entity set row to entityset
                            let ls_entity = value zcl_zmm_po_cond_mpc=>ts_po_cond( purchaseorder = lv_purchaseorder )
                            in ( corresponding #( base ( ls_entity ) ls_pocond ) ) ).
  endmethod.
ENDCLASS.
