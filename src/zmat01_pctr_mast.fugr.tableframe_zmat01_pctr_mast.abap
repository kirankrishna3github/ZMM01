*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMAT01_PCTR_MAST
*   generation date: 22.01.2019 at 12:49:16
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMAT01_PCTR_MAST   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
