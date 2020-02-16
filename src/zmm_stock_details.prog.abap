*&---------------------------------------------------------------------*
*& Report  ZMM_STOCK_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_STOCK_DETAILS.

TYPE-POOLS : slis.

INCLUDE : ZMM_STOCK_DETAILS_DD,
          ZMM_STOCK_DETAILS_final,
          ZMM_STOCK_DETAILS_FD.

INITIALIZATION.
  PERFORM build_eventtab USING gt_events[].
  PERFORM build_layout USING layout.

START-OF-SELECTION.
PERFORM get_data.
PERFORM send_mail.
PERFORM display_data.
