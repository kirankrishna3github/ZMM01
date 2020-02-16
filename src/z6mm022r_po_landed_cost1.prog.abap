*&---------------------------------------------------------------------*
*& Report  Z6MM022R_PO_LANDED_COST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6mm022r_po_landed_cost1 NO STANDARD PAGE HEADING
                                MESSAGE-ID z6mm022r_msg.

*----------------------------------------------------------------------*
*   Dev. Class    : ZMM                                                *
*   Report Name   :  Z6MM022R_PO_LANDED_COST                           *
*   Program Type  : Report                                             *
*   Created by    : Kiruthika                                          *
*   Created on    : 26/06/2010                                         *
*   TCode         : ZMM_PO_LAND_COST                                   *
*   Module Name   : Z6MM022R                                           *
*   Description   : PO landed cost report                              *
*----------------------------------------------------------------------*
*   S O U R C E   C O D E   C H A N G E   H I S T O R Y
* ---------------------------------------------------------------------*
*   CODE    | AUTHOR    | DATE     |  DESC
* ---------------------------------------------------------------------*
*
* ---------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 16/10/2015
*   REASON FOR CHANGE: Add Authorization
*   REQUEST #: IRDK921100
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 07/01/2016
*   REASON FOR CHANGE: Added Fields for Value & Variance of ZBPL, ZBPP, ZBPC,Credit Days
*   REQUEST #: IRDK922099
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 30/03/2016
*   REASON FOR CHANGE: Added Fields for Purchasing Group & Division
*   REQUEST #: IRDK923091
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 16/01/2017
*   REASON FOR CHANGE: Added SPART in selection & field
*   REQUEST #: IRDK926781
* --------------------------------------------------------------------------------------------*
************************************************************************
* INCLUDES
************************************************************************
INCLUDE z6mm022r_po_landed_cost1_top.
INCLUDE z6mm022r_po_landed_cost1_s01.
INCLUDE z6mm022r_po_landed_cost1_f01.


INITIALIZATION.
  repname = sy-repid.
  PERFORM initialize_variant.

************************************************************************
* AT SELECTION SCREEN EVENT
************************************************************************
AT SELECTION-SCREEN.

  PERFORM pai_of_selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

*  Validates the user entry
  PERFORM validation.

************************************************************************
* START OF SELECTION EVENT
************************************************************************

START-OF-SELECTION.
  PERFORM check_auth_obj.  " Added by CS on 16.10.2015 for Authorization.
* Fetches data from database
  IF r_po = 'X'.
    IF so_bedat IS INITIAL .
      MESSAGE 'Please Enter PO Date' TYPE 'I'.
      po = 'X'.
      EXIT.
      STOP.
    ENDIF.
  ELSE.
    IF so_budat IS INITIAL .
      MESSAGE 'Please Enter GRR Date' TYPE 'I'.
      grr = 'X'.
      EXIT.
      STOP.
    ENDIF.
  ENDIF.

  PERFORM select_data.
* Gets the condition value from KNOV
  PERFORM get_condition_value.
* Fills the output table
*  PERFORM fill_data.
  IF r_gr = 'X'.
    PERFORM fill_data_gl.
  ELSE.
    PERFORM fill_data_po.
  ENDIF.
***** Start Code: Added by CS on 07.01.2016 *****
  IF lv_tot_lcost IS NOT INITIAL.

    wa_output-cr_days = lv_tot_avgcr / lv_tot_lcost.  " TOTAL of Avg Cr Value / Total of Landed Cost
    APPEND wa_output TO it_output.
    CLEAR: wa_output.
*  lv_tot_crdays = lv_tot_avgcr / lv_tot_lcost.  " TOTAL of Avg Cr Value / Total of Landed Cost
*  MOVE lv_tot_crdays to lv_txt_crdays.
  ENDIF.
***** End Code: Added by CS on 07.01.2016 *****
* Fills the field catalog for output table
  PERFORM fill_fieldcat.
***** Start Code: Added by CS on 16.10.2015 for Authorization. *****
  IF it_output[] IS NOT INITIAL.
    IF lv_werks_auth_flg = 'X' OR lv_mtart_auth_flg = 'X' OR lv_ekorg_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Pur. Org./Plant/Material Type.' TYPE 'I' DISPLAY LIKE 'W'.
*      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.

    IF lv_werks_auth_flg = 'X' OR lv_mtart_auth_flg = 'X' OR lv_ekorg_auth_flg = 'X'.
      MESSAGE 'No record found./ Missing Authorization for Pur. Org./Plant/Material Type.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
***** End Code: Added by CS on 16.10.2015 for Authorization. *****

************************************************************************
* END OF SELECTION EVENT
************************************************************************
END-OF-SELECTION.
* Displays the data in ALV grid display using OOPs
  PERFORM display_data.
