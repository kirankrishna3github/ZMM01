*&---------------------------------------------------------------------*
*& Report  Z6MM022R_PO_LANDED_COST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6mm022r_po_landed_cost NO STANDARD PAGE HEADING
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

************************************************************************
* INCLUDES
************************************************************************
INCLUDE z6mm022r_po_landed_cost_top.
INCLUDE z6mm022r_po_landed_cost_s01.
INCLUDE z6mm022r_po_landed_cost_f01.


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
