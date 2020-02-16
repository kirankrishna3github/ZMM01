*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z_WMS_TMG
*   generation date: 12.12.2018 at 10:00:15
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z_WMS_TMG          .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
