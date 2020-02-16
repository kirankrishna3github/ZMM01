*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 02.04.2019 at 13:01:28
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM_T_MAT_CLASS.................................*
DATA:  BEGIN OF STATUS_ZMM_T_MAT_CLASS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_T_MAT_CLASS               .
CONTROLS: TCTRL_ZMM_T_MAT_CLASS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_T_MAT_CLASS               .
TABLES: ZMM_T_MAT_CLASS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
