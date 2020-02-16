*&---------------------------------------------------------------------*
*& Report  ZMM_PO_SEND_MAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT ZMM_PO_SEND_MAIL.

TYPE-POOLS : slis.
TABLES : ekko,ekpo,eket,lfa1,adr6,adrc.

INCLUDE : ZMM_PO_SEND_MAIL_DD,
          ZMM_PO_SEND_MAIL_final,
          ZMM_PO_SEND_MAIL_SS,
          ZMM_PO_SEND_MAIL_FD.

*-----------------------------------------------------------------------
*   Initialization
*
*-----------------------------------------------------------------------
INITIALIZATION.
  PERFORM build_eventtab USING gt_events[].
  PERFORM build_layout USING layout.
  PERFORM initialize_variant.

*-----------------------------------------------------------------------
*   At Selection Screen for Variant Selection
*
*-----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

START-OF-SELECTION.

  PERFORM get_data.

  PERFORM display_data.

END-OF-SELECTION.
