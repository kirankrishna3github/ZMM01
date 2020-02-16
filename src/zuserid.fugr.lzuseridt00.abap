*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 09.08.2010 at 18:36:54 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMA0USERID......................................*
DATA:  BEGIN OF STATUS_ZMA0USERID                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMA0USERID                    .
CONTROLS: TCTRL_ZMA0USERID
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMA0USERID                    .
TABLES: ZMA0USERID                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
