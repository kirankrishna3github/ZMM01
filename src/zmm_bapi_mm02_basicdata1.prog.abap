*&------ INDOFIELD INDUSTRIES LTI. ------------------------------------*
*&---------------------------------------------------------------------*
*& TRANSACTION : ZMM090                                                *
*& PROGRAM NAME: ZMM_BAPI_MM02_BASICDATA1                              *
*& TECHENICAL  : Sandeep Pore                                          *
*& FUNCTIONAL  : Kamalakar Varma                                       *
*& MODULE      : MM                                                    *
*& TR          : IHDK900155                                            *
*&---------------------------------------------------------------------*
*& CHANGE HISTORY |CHANGED BY |                                        *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Report ZMM_BAPI_MM02_BASICDATA1
*&---------------------------------------------------------------------*

REPORT ZMM_BAPI_MM02_BASICDATA1.

INCLUDE ZMM_BAPI_MM02_BASICDATA1_TOP.
INCLUDE ZMM_BAPI_MM02_BASICDATA1_SEL.
INCLUDE ZMM_BAPI_MM02_BASICDATA1_FORM.

*&---------------------------------------------------------------------*
*& I N I T I A L I Z A T I O N
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM F_CLEAR_VARIABLES.
  perform set_title.  " IHDK901136

" IHDK904429
at selection-screen output.
  perform modify_sel_screen.

*&---------------------------------------------------------------------*
*& A T S E L E C T I O N - S C R E E N O N V A L U E - R E Q U E S T
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM F_GET_FILEPATH.

*&---------------------------------------------------------------------*
*& A T S E L E C T I O N - S C R E E N
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM F_DWN_FILE_FMT.

*&---------------------------------------------------------------------*
*& S T A R T - O F - S E L E C T I O N
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM F_REAT_FILE_DATA.

    IF LT_DATA IS NOT INITIAL.
      PERFORM F_VALIDATION.
      PERFORM F_CALL_BAPI.
    ELSE.
      MESSAGE TEXT-003 TYPE 'I'.
    ENDIF.

    IF LT_MSG IS NOT INITIAL.
      PERFORM F_FIELDCAT.
      PERFORM F_DISPLAY_MSG.
    ENDIF.
