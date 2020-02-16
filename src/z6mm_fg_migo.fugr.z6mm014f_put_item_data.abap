FUNCTION Z6MM014F_PUT_ITEM_DATA.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_MIGO_BADI_SCREEN_FIELDS) TYPE
*"        ZST_MIGO_GATE_ENTRY_ITEM
*"----------------------------------------------------------------------
* Put all data from fields to external screen
* First fill all customer-fields
  MOVE-CORRESPONDING is_migo_badi_screen_fields TO z6mma_gt_ent_dt.
* second fill all GOITEM-fields (displayed on external screen)
*  goitem-sgtxt = is_migo_badi_screen_fields-sgtxt.




ENDFUNCTION.
