FUNCTION Z6MM014F_UPDATE_HEADER.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_MIGO_BADI_HEADER_FIELDS) TYPE  ZST_MIGO_GATE_ENTRY_HEAD
*"----------------------------------------------------------------------

data: ls_Z6MMA_GT_ENT_HD type Z6MMA_GT_ENT_HD.

* Database update:
  move-corresponding is_migo_badi_header_fields to ls_Z6MMA_GT_ENT_HD.
  INSERT Z6MMA_GT_ENT_HD FROM ls_Z6MMA_GT_ENT_HD.
  IF sy-subrc <> 0.
    MESSAGE a398(00) WITH 'Error update Z6MMA_GT_ENT_HD'.
  ENDIF.



ENDFUNCTION.
