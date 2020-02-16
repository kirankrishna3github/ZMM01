*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 28.02.2019 at 11:16:10
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMAT01_SLOC_MAST................................*
DATA:  BEGIN OF STATUS_ZMAT01_SLOC_MAST              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMAT01_SLOC_MAST              .
CONTROLS: TCTRL_ZMAT01_SLOC_MAST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMAT01_SLOC_MAST              .
TABLES: ZMAT01_SLOC_MAST               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
