*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 12.12.2018 at 10:00:16
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM_T_WMS_MASTER................................*
DATA:  BEGIN OF STATUS_ZMM_T_WMS_MASTER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_T_WMS_MASTER              .
CONTROLS: TCTRL_ZMM_T_WMS_MASTER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_T_WMS_MASTER              .
TABLES: ZMM_T_WMS_MASTER               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
