*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 16.12.2016 at 14:35:07 by user IBM_AMS
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZMM_PP_USER_AUTH................................*
DATA:  BEGIN OF STATUS_ZMM_PP_USER_AUTH              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_PP_USER_AUTH              .
CONTROLS: TCTRL_ZMM_PP_USER_AUTH
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_PP_USER_AUTH              .
TABLES: ZMM_PP_USER_AUTH               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
