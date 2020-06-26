*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 26.06.2020 at 17:51:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM_T_TRFPST_CHK................................*
DATA:  BEGIN OF STATUS_ZMM_T_TRFPST_CHK              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_T_TRFPST_CHK              .
CONTROLS: TCTRL_ZMM_T_TRFPST_CHK
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_T_TRFPST_CHK              .
TABLES: ZMM_T_TRFPST_CHK               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
