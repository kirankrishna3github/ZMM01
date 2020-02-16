*&---------------------------------------------------------------------*
*& Report ZMM_PRG_CHECK_WMS_STATUS
*&---------------------------------------------------------------------*
*& Class methods cannot be directly submitted to background jobs, so this program was created
*& IHDK900139
*&---------------------------------------------------------------------*
report zmm_prg_update_wms_log.

start-of-selection.
  zcl_sql_wms=>check_wms_trans_status( ).
