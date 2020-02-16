*&---------------------------------------------------------------------*
*& Report  ZME_MATERIAL_REQUISITION_SLIP
*&
*&---------------------------------------------------------------------*
*&  Developed By : Anooj Mohandas
*&  Functional   : Ashish Kothari
*&---------------------------------------------------------------------*

REPORT  zme_material_requisition_slip MESSAGE-ID zmsg_slip.

DATA : it_resb  TYPE ztt_resb,
       it_t001l TYPE ztt_t001l,
       it_makt  TYPE ztt_makt,
       ls_resb  TYPE zst_resb,
       fm_name  TYPE rs38l_fnam.


CONSTANTS : c_form TYPE tdsfname VALUE 'ZSF_MATERIAL_REQUISITION_SLIP'.


SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_rsnum TYPE resb-rsnum.
SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN.

  SELECT SINGLE rsnum
                rspos
                matnr
                werks
                lgort
                bdter
                bdmng
                meins
                sgtxt
                umlgo
                erfmg
                erfme
           FROM resb
           INTO ls_resb
           WHERE rsnum = p_rsnum.
  IF sy-subrc <> 0.
    MESSAGE e007.
  ENDIF.

START-OF-SELECTION.

  PERFORM get_data.

  PERFORM get_fname.

  PERFORM call_sf.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  SELECT rsnum
         rspos
         matnr
         werks
         lgort
         bdter
         bdmng
         meins
         sgtxt
         umlgo
         erfmg
         erfme
      FROM resb
      INTO TABLE it_resb
      WHERE rsnum EQ p_rsnum.
  IF sy-subrc = 0.

    SELECT werks
           lgort
           lgobe
      FROM t001l
      INTO TABLE it_t001l
      FOR ALL ENTRIES IN it_resb
      WHERE werks = it_resb-werks
        AND ( lgort = it_resb-lgort
         or   lgort = it_resb-umlgo ).
    IF sy-subrc = 0.
      SORT it_t001l BY werks lgort.
    ENDIF.

    SELECT matnr
           maktx
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_resb
      WHERE matnr = it_resb-matnr
        AND spras = sy-langu.
    IF sy-subrc = 0.
      SORT it_makt BY matnr.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fname .
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname                 = c_form
*     VARIANT                  = ' '
*     DIRECT_CALL              = ' '
   IMPORTING
     fm_name                  = fm_name
   EXCEPTIONS
     no_form                  = 1
     no_function_module       = 2
     OTHERS                   = 3
            .
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e006.
      WHEN 2.
        MESSAGE e005.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.                    " GET_FNAME
*&---------------------------------------------------------------------*
*&      Form  CALL_SF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_sf .
  CALL FUNCTION fm_name
    EXPORTING
      ls_resb          = ls_resb
    TABLES
      it_resb          = it_resb
      it_t001l         = it_t001l
      it_makt          = it_makt
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e001.
      WHEN 2.
        MESSAGE e002.
      WHEN 3.
        MESSAGE e003.
      WHEN 4.
        MESSAGE e004.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDFORM.                    " CALL_SF
