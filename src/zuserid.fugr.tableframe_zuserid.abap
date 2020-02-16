*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZUSERID
*   generation date: 09.08.2010 at 18:36:53 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZUSERID            .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
