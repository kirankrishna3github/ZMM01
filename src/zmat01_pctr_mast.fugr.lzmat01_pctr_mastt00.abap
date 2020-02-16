*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 22.01.2019 at 12:49:16
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMAT01_PCTR_MAST................................*
DATA:  BEGIN OF STATUS_ZMAT01_PCTR_MAST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMAT01_PCTR_MAST              .
CONTROLS: TCTRL_ZMAT01_PCTR_MAST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMAT01_PCTR_MAST              .
TABLES: ZMAT01_PCTR_MAST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
