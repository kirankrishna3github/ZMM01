*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMM_T_MAT_CLAS
*   generation date: 02.04.2019 at 13:01:28
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMM_T_MAT_CLAS     .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
