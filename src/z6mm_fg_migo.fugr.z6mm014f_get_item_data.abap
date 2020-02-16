FUNCTION Z6MM014F_GET_ITEM_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(ES_MIGO_BADI_SCREEN_FIELDS) TYPE
*"        ZST_MIGO_GATE_ENTRY_ITEM
*"----------------------------------------------------------------------


* Get all data from fields of external screen
* First move all customer-fields
  MOVE-CORRESPONDING z6mma_gt_ent_dt TO es_migo_badi_screen_fields.
* Second move all GOITEM-fields (displayed on external screen)
*es_migo_badi_screen_fields-sgtxt = goitem-sgtxt.



ENDFUNCTION.
