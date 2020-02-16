*&---------------------------------------------------------------------*
*& Report  ZMM_STOCK_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_STOCK_DETAILS_MAIL_EXP90.

TYPE-POOLS : slis.

*INCLUDE ZMM_STOCK_DETAILS_M_DD.
INCLUDE ZMM_STOCK_DETAILS_M_DD_exp90.
*INCLUDE ZMM_STOCK_DETAILS_M_FINAL.
INCLUDE ZMM_STOCK_DETAILS_M_FINAL_exp.
*INCLUDE ZMM_STOCK_DETAILS_M_FD.
INCLUDE ZMM_STOCK_DETAILS_M_FD_exp90.

INITIALIZATION.
  PERFORM build_eventtab USING gt_events[].
  PERFORM build_layout USING layout.

START-OF-SELECTION.
PERFORM get_data.
PERFORM send_mail.
PERFORM display_data.
