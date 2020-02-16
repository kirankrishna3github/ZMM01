*&---------------------------------------------------------------------*
*&  Include           Z6MM022R_PO_LANDED_COST_S01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Dev. Class    : ZPP                                                *
*   Report Name   :  Z6MM022R_PO_LANDED_COST_S01                       *
*   Program Type  : Include                                            *
*   Created by    : Kiruthika                                          *
*   Created on    : 26/06/2010                                         *
*   TCode         :                                                    *
*   Module Name   :                                                    *
*   Description   : PO landed cost report                              *
*----------------------------------------------------------------------*
*   S O U R C E   C O D E   C H A N G E   H I S T O R Y
* ---------------------------------------------------------------------*
*   CODE    | AUTHOR    | DATE     |  DESC
* ---------------------------------------------------------------------*
*
* ---------------------------------------------------------------------*
TABLES: EKKO, EKPO, EKBE.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS:
    SO_LIFNR FOR EKKO-LIFNR, " Vendor
    SO_EKORG FOR EKKO-EKORG, " pur org
    SO_WERKS FOR EKPO-WERKS, " plant
    SO_EBELN FOR EKKO-EBELN, " pur doc
    SO_MATNR FOR EKPO-MATNR, " material
    SO_MATKL FOR EKPO-MATKL, " material group
    SO_MTART FOR EKPO-MTART, " Mat type
    SO_BSART FOR EKKO-BSART,  " Pur doc type
    SO_LLIEF FOR EKKO-LLIEF,  " Suppl vendor
    SO_RESWK FOR EKKO-RESWK,  " Supplying plant
    SO_BEDAT FOR EKKO-BEDAT,  "PO Date
    SO_BUDAT FOR EKBE-BUDAT.  "GRR Date

SELECT-OPTIONS:S_FRGKE FOR EKKO-FRGKE.
SELECT-OPTIONS:S_RLWRT FOR EKkO-RLWRT NO-EXTENSION NO INTERVALS.

*    so_magrp FOR ekpo-matkl.  " Mat group
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T02.
PARAMETERS: R_PO RADIOBUTTON GROUP G1 DEFAULT 'X',
            R_GR RADIOBUTTON GROUP G1.
SELECTION-SCREEN END OF BLOCK B2.
*

SELECTION-SCREEN BEGIN OF BLOCK A01 WITH FRAME TITLE TEXT-T03.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT. " ALV Variant
SELECTION-SCREEN END OF BLOCK A01.
