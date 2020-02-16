*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z6MM_TAX_CODE
*   generation date: 27.06.2011 at 20:05:33 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z6MM_TAX_CODE      .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
