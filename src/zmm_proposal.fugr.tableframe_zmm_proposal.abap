*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZMM_PROPOSAL
*   generation date: 16.12.2016 at 14:35:06 by user IBM_AMS
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZMM_PROPOSAL       .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
