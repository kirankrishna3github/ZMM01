*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 15.06.2010 at 13:35:23 by user IBMABAP01
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: Z6MMA_RGPNRGP_AT................................*
DATA:  BEGIN OF STATUS_Z6MMA_RGPNRGP_AT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6MMA_RGPNRGP_AT              .
CONTROLS: TCTRL_Z6MMA_RGPNRGP_AT
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: Z6MMA_RGPNRGP_NR................................*
DATA:  BEGIN OF STATUS_Z6MMA_RGPNRGP_NR              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_Z6MMA_RGPNRGP_NR              .
CONTROLS: TCTRL_Z6MMA_RGPNRGP_NR
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *Z6MMA_RGPNRGP_AT              .
TABLES: *Z6MMA_RGPNRGP_NR              .
TABLES: Z6MMA_RGPNRGP_AT               .
TABLES: Z6MMA_RGPNRGP_NR               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
