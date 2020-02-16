*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 19.02.2015 at 11:48:33 by user 10106
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM_VENDOR_TAG..................................*
DATA:  BEGIN OF STATUS_ZMM_VENDOR_TAG                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_VENDOR_TAG                .
CONTROLS: TCTRL_ZMM_VENDOR_TAG
            TYPE TABLEVIEW USING SCREEN '0003'.
*.........table declarations:.................................*
TABLES: *ZMM_VENDOR_TAG                .
TABLES: ZMM_VENDOR_TAG                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
