*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 29.05.2018 at 11:25:55
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6MMA_PARAMS....................................*
DATA:  BEGIN OF STATUS_Z6MMA_PARAMS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6MMA_PARAMS                  .
CONTROLS: TCTRL_Z6MMA_PARAMS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *Z6MMA_PARAMS                  .
TABLES: Z6MMA_PARAMS                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
