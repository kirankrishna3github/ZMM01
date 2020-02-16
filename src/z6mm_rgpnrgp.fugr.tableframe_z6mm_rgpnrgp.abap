*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_Z6MM_RGPNRGP
*   generation date: 15.06.2010 at 13:31:38 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_Z6MM_RGPNRGP       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
