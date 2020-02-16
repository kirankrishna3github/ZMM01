FUNCTION-POOL Z6MM_FG_MIGO.                 "MESSAGE-ID ..


TABLES: Z6MMA_GT_ENT_HD,
        Z6MMA_Gt_ENT_DT,
        goitem.

TYPES:
  BEGIN OF ty_s_exdata_header,
    line_id       TYPE mb_line_id,
    mandt         TYPE mandt,
    mblnr         TYPE mblnr,
    mjahr         TYPE mjahr,
    badi_nummer   TYPE num10,
  END OF ty_s_exdata_header.
