FUNCTION Z6MM014F_PUT_HEADER_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_MIGO_BADI_HEADER_FIELDS) TYPE
*"        ZST_MIGO_GATE_ENTRY_HEAD
*"----------------------------------------------------------------------
* Put all data from fields to external screen
  MOVE-CORRESPONDING is_migo_badi_header_fields TO Z6mma_gt_ent_hd.




ENDFUNCTION.
