*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMAT01_SLOC_MAST
*   generation date: 28.02.2019 at 11:16:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMAT01_SLOC_MAST   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
