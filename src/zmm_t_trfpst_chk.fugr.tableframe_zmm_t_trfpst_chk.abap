*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMM_T_TRFPST_CHK
*   generation date: 26.06.2020 at 17:51:51
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMM_T_TRFPST_CHK   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
