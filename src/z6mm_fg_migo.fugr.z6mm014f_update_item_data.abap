FUNCTION Z6MM014F_UPDATE_ITEM_DATA.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  TABLES
*"      IT_MIGO_BADI_EXAMPLE STRUCTURE  Z6MMA_GT_ENT_DT
*"----------------------------------------------------------------------
* Databse update:
  INSERT z6mma_gt_ent_dt FROM TABLE IT_MIGO_BADI_EXAMPLE.
  IF sy-subrc <> 0.
    MESSAGE a398(00) WITH 'Error update MIGO_BADI_EXAMPL'.
  ENDIF.





ENDFUNCTION.
