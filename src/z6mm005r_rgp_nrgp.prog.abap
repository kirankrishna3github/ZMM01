*&---------------------------------------------------------------------*
*& Report  Z6MM005R_RGP_NRGP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6MM005R_RGP_NRGP.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: RGP / NRGP Menu Program
* OBJECT TYPE       : Report              FUNC. CONSULTANT:Girish
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 29.06.2010
*        DEV REQUEST: IRDK900078
*              TCODE: ZMM005_0
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

DATA : V_OK_CODE TYPE SY-UCOMM.

start-of-selection.

 CALL SCREEN 100.

end-of-SELECTION.

INCLUDE Z6MM005R_RGP_NRGP_01O01.

INCLUDE Z6MM005R_RGP_NRGP_I01.
