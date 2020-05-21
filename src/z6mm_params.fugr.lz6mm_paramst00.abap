*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 21.05.2020 at 16:15:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6MMA_PARAMS....................................*
DATA:  BEGIN OF STATUS_Z6MMA_PARAMS                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6MMA_PARAMS                  .
CONTROLS: TCTRL_Z6MMA_PARAMS
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZFI_V_VEN_DT_VAL................................*
TABLES: ZFI_V_VEN_DT_VAL, *ZFI_V_VEN_DT_VAL. "view work areas
CONTROLS: TCTRL_ZFI_V_VEN_DT_VAL
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZFI_V_VEN_DT_VAL. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZFI_V_VEN_DT_VAL.
* Table for entries selected to show on screen
DATA: BEGIN OF ZFI_V_VEN_DT_VAL_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZFI_V_VEN_DT_VAL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFI_V_VEN_DT_VAL_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZFI_V_VEN_DT_VAL_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZFI_V_VEN_DT_VAL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZFI_V_VEN_DT_VAL_TOTAL.

*.........table declarations:.................................*
TABLES: *Z6MMA_PARAMS                  .
TABLES: Z6MMA_PARAMS                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
