*&---------------------------------------------------------------------*
*& Report  ZME_MATERIAL_ISSUE_SLIP
*&---------------------------------------------------------------------*
*&  Developer : Anooj Mohandas
*&  Functional : Ashish Kothari
*&  Created On : 31.03.2016
*&  Transport Request Details :
*&  - V1.0  - IRDK923104 - IBJ: Material Issue Slip V1.0 Dt: 30.03.2016
*&---------------------------------------------------------------------*
REPORT  zme_material_issue_slip1 MESSAGE-ID zmsg_slip.

TABLES : nast.

DATA : lf_fm_name            TYPE rs38l_fnam,         "FM Name for Smartforms
       lv_mblnr              TYPE mkpf-mblnr,         "Material Document Number
       lv_mjahr              TYPE mkpf-mjahr,         "Material Document Year
       wa_mkpf               TYPE mkpf,               "Material Document Header
       it_mseg               TYPE ztt_mseg1,          "Material Document Item
       it_makt               TYPE ztt_makt,           "Material Description
       it_rsnum              TYPE ztt_rsnum,          "Requisition Details
       it_t001l              TYPE ztt_t001l.          "Location Details
DATA : gv_mblnr TYPE mkpf-mblnr,
       gv_flag.

CONSTANTS : c_form TYPE tdsfname VALUE 'ZSF_MATERIAL_ISSUE_SLIP1'. "Smartform Name

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-010.
PARAMETERS: p_mjahr TYPE mkpf-mjahr OBLIGATORY.
SELECT-OPTIONS: s_mblnr FOR gv_mblnr OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  entry
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RETURN_CODE  text
*      -->US_SCREEN    text
*----------------------------------------------------------------------*
*FORM entry                                                  "#EC CALLED
*     USING return_code TYPE i                               "#EC NEEDED
*           us_screen   TYPE c.                              "#EC NEEDED

*  lv_mblnr = nast-objky+0(10).  "Material Document Number
*  lv_mjahr = nast-objky+10(4).  "Document Year

  PERFORM get_data.   "Obtaining Details for Binding

  IF gv_flag IS INITIAL.
    PERFORM processing. "Calling Layout / Smartforms
  ELSEIF gv_flag = 'X'.
    MESSAGE 'For the above selection criteria no data found for Movement Type 701' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

*ENDFORM.                    "entry
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  CLEAR gv_flag.
***Obtaining Material Document Header Details
  SELECT SINGLE mblnr   "Material Document Number
                mjahr   "Fiscal Year
                bldat   "Material Document Date
           FROM mkpf
           INTO (wa_mkpf-mblnr,
                 wa_mkpf-mjahr,
                 wa_mkpf-bldat)
           WHERE mblnr IN s_mblnr"lv_mblnr   "Document Number
             AND mjahr EQ p_mjahr."lv_mjahr.  "Document Year
  IF sy-subrc <> 0.
    CLEAR wa_mkpf.
  ELSE.  "IF sy-subrc <> 0.
***Obtaining Material Document Item Details
    SELECT mblnr  AS mblnr  "Material Document Number
           mjahr  AS mjahr  "Document Year
           xauto  AS xauto  "Identifier for Location From / To
           matnr  AS matnr  "Material Number
           werks  AS werks  "Plant
           lgort  AS lgort  "Location
           charg  AS charg  "Batch
           erfmg  AS erfmg  "Quantity Issued
           erfme  AS erfme  "Unit
           rsnum  AS rsnum  "Requisition Number
           rspos  AS rspos  "Requisition Item
          FROM mseg
          INTO TABLE it_mseg
               WHERE mblnr IN s_mblnr"wa_mkpf-mblnr  "Document Number
                 AND mjahr EQ p_mjahr"wa_mkpf-mjahr. "Document Year
                 AND bwart EQ '701'.

    IF it_mseg[] IS NOT INITIAL.
***Obtaining Requisition Details
      SELECT rsnum  AS rsnum  "Requisition Number
             rspos  AS rspos  "Requisition Item
             bdmng  AS bdmng  "Quantity Required
             meins  AS meins  "Unit
             erfmg  AS erfmg  "QUANTITY REQUIRED
             erfme  AS erfme  "UNIT
          FROM resb
          INTO TABLE it_rsnum
          FOR ALL ENTRIES IN it_mseg
                       WHERE rsnum EQ it_mseg-rsnum  "Requisition Number
                         AND rspos EQ it_mseg-rspos. "Requisition Item
      IF sy-subrc = 0.
        SORT it_rsnum BY rsnum rspos.
      ENDIF.  "IF sy-subrc = 0.
***Obtaining Location Details
      SELECT werks  AS werks  "Plant
             lgort  AS lgort  "Location
             lgobe  AS lgobe  "Location Description
          FROM t001l
          INTO TABLE it_t001l
          FOR ALL ENTRIES IN it_mseg
                        WHERE werks EQ it_mseg-werks   "Plant
                          AND lgort EQ it_mseg-lgort.  "Location
      IF sy-subrc = 0.
        SORT it_t001l BY werks lgort.
      ENDIF.  "IF sy-subrc = 0.
***Obtaining Material Details
      SELECT matnr  AS matnr  "Material Number
             maktx  AS maktx  "Material Description
          FROM makt
          INTO TABLE it_makt
          FOR ALL ENTRIES IN it_mseg
                       WHERE matnr EQ it_mseg-matnr "Material Number
                         AND spras EQ sy-langu.     "Language
      IF sy-subrc = 0.
        SORT it_makt BY matnr.
      ENDIF.  "IF sy-subrc = 0.

    ELSE.
      gv_flag = 'X'.
    ENDIF.  "IF it_mseg[] IS NOT INITIAL.

  ENDIF.  "IF sy-subrc <> 0.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processing .

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = c_form "Form Name
    IMPORTING
      fm_name            = lf_fm_name  "Generated FM Name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e006. "Form Does not Exist
      WHEN 2.
        MESSAGE e005. "Function Module Generation Error
      WHEN OTHERS.
    ENDCASE.    "CASE sy-subrc.
  ENDIF.    "IF sy-subrc <> 0.


CALL FUNCTION lf_fm_name"'/1BCDWB/SF00000368'
  EXPORTING
    wa_mkpf                    = wa_mkpf  "Material Document Header Details
  tables
    it_mseg                    = it_mseg  "Material Document Item Details
    it_makt                    = it_makt  "Material Details
    it_rsnum                   = it_rsnum "Requisition Details
    it_t001l                   = it_t001l "Location Details
 EXCEPTIONS
   FORMATTING_ERROR           = 1
   INTERNAL_ERROR             = 2
   SEND_ERROR                 = 3
   USER_CANCELED              = 4
   OTHERS                     = 5
          .
IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e001. "Formatting Error
      WHEN 2.
        MESSAGE e002. "Internal Error
      WHEN 3.
        MESSAGE e003. "Send Error
      WHEN 4.
        MESSAGE e004. "User Canceled
      WHEN OTHERS.
    ENDCASE.    "CASE sy-subrc.
ENDIF.


*  CALL FUNCTION lf_fm_name"'/1BCDWB/SF00000368'
*    EXPORTING
*      wa_mkpf                    = wa_mkpf  "Material Document Header Details
*    TABLES
*      it_mseg                    = it_mseg  "Material Document Item Details
*      it_makt                    = it_makt  "Material Details
*      it_rsnum                   = it_rsnum "Requisition Details
*      it_t001l                   = it_t001l "Location Details
*   EXCEPTIONS
*     formatting_error           = 1
*     internal_error             = 2
*     send_error                 = 3
*     user_canceled              = 4
*     OTHERS                     = 5.
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN 1.
*        MESSAGE e001. "Formatting Error
*      WHEN 2.
*        MESSAGE e002. "Internal Error
*      WHEN 3.
*        MESSAGE e003. "Send Error
*      WHEN 4.
*        MESSAGE e004. "User Canceled
*      WHEN OTHERS.
*    ENDCASE.    "CASE sy-subrc.
*  ENDIF.


*  CALL FUNCTION lf_fm_name  "Generated FM Name
*    EXPORTING
*      wa_mkpf          = wa_mkpf  "Material Document Header Details
*    TABLES
*      it_mseg          = it_mseg  "Material Document Item Details
*      it_makt          = it_makt  "Material Details
*      it_rsnum         = it_rsnum "Requisition Details
*      it_t001l         = it_t001l "Location Details
*    EXCEPTIONS
*      formatting_error = 1
*      internal_error   = 2
*      send_error       = 3
*      user_canceled    = 4
*      OTHERS           = 5.
*  IF sy-subrc <> 0.
*    CASE sy-subrc.
*      WHEN 1.
*        MESSAGE e001. "Formatting Error
*      WHEN 2.
*        MESSAGE e002. "Internal Error
*      WHEN 3.
*        MESSAGE e003. "Send Error
*      WHEN 4.
*        MESSAGE e004. "User Canceled
*      WHEN OTHERS.
*    ENDCASE.    "CASE sy-subrc.
*  ENDIF.    "IF sy-subrc <> 0.


ENDFORM.                    " PROCESSING
