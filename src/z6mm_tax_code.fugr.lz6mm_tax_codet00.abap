*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 27.06.2011 at 20:05:34 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6MMA_TAX_CODES.................................*
DATA:  BEGIN OF STATUS_Z6MMA_TAX_CODES               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6MMA_TAX_CODES               .
CONTROLS: TCTRL_Z6MMA_TAX_CODES
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *Z6MMA_TAX_CODES               .
TABLES: Z6MMA_TAX_CODES                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
