FUNCTION Z6MM014F_GET_HEADER_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(ES_MIGO_BADI_HEADER_FIELDS) TYPE
*"        ZST_MIGO_GATE_ENTRY_HEAD
*"----------------------------------------------------------------------
* Get all data from fields of external screen
  MOVE-CORRESPONDING z6mma_gt_ent_hd TO es_migo_badi_header_fields.




ENDFUNCTION.
