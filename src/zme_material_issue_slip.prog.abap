*&---------------------------------------------------------------------*
*& Report  ZME_MATERIAL_ISSUE_SLIP
*&---------------------------------------------------------------------*
*&  Developer : Anooj Mohandas
*&  Functional : Ashish Kothari
*&  Created On : 31.03.2016
*&  Transport Request Details :
*&  - V1.0  - IRDK923104 - IBJ: Material Issue Slip V1.0 Dt: 30.03.2016
*&---------------------------------------------------------------------*
REPORT  zme_material_issue_slip MESSAGE-ID zmsg_slip.

TABLES : nast.

DATA : lf_fm_name            TYPE rs38l_fnam,         "FM Name for Smartforms
       lv_mblnr              TYPE mkpf-mblnr,         "Material Document Number
       lv_mjahr              TYPE mkpf-mjahr,         "Material Document Year
       wa_mkpf               TYPE mkpf,               "Material Document Header
       it_mseg               TYPE ztt_mseg,           "Material Document Item
       it_makt               TYPE ztt_makt,           "Material Description
       it_rsnum              TYPE ztt_rsnum,          "Requisition Details
       it_t001l              TYPE ztt_t001l.          "Location Details

DATA: LW_MAKT TYPE ZST_MAKT,    "Added by Sandeep on 25.02.2019 for DivisioN
      LW_MSEG TYPE ZST_MSEG,
      LV_SPART TYPE MARA-SPART,
      LV_ADRNR TYPE ADRC-addrnumber,
      LW_HEADER TYPE ZST_HEADER.

FIELD-SYMBOLS <FS_MSEG> TYPE ZST_MSEG.

DATA: LW_SADRC TYPE ADRC,
      LW_BADRC TYPE ADRC.

DATA: LV_PARAM2 TYPE Z6MMA_PARAMS-PARAM2,
      LV_PARAMVAL TYPE Z6MMA_PARAMS-PARAMVAL.

**CONSTANTS : c_form TYPE tdsfname VALUE 'ZSF_MATERIAL_ISSUE_SLIP'. "Smartform Name
DATA: c_form TYPE tdsfname VALUE 'ZSF_MATERIAL_ISSUE_SLIP'. "Smartform Name

*&---------------------------------------------------------------------*
*&      Form  entry
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RETURN_CODE  text
*      -->US_SCREEN    text
*----------------------------------------------------------------------*
FORM entry                                                  "#EC CALLED
     USING return_code TYPE i                               "#EC NEEDED
           us_screen   TYPE c.                              "#EC NEEDED

  lv_mblnr = nast-objky+0(10).  "Material Document Number
  lv_mjahr = nast-objky+10(4).  "Document Year

  PERFORM get_data.   "Obtaining Details for Binding

  PERFORM processing. "Calling Layout / Smartforms

ENDFORM.                    "entry
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

CLEAR: LW_SADRC,LW_BADRC.

***Obtaining Material Document Header Details
  SELECT SINGLE mblnr   "Material Document Number
                mjahr   "Fiscal Year
                bldat   "Material Document Date
           FROM mkpf
           INTO (wa_mkpf-mblnr,
                 wa_mkpf-mjahr,
                 wa_mkpf-bldat)
           WHERE mblnr EQ lv_mblnr   "Document Number
             AND mjahr EQ lv_mjahr.  "Document Year
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
           erfmg  AS erfmg  "Quantity Issued
           erfme  AS erfme  "Unit
           rsnum  AS rsnum  "Requisition Number
           rspos  AS rspos  "Requisition Item
           LBKUM  AS LBKUM
           WAERS  AS WAERS
          FROM mseg
          INTO CORRESPONDING FIELDS OF TABLE it_mseg
          WHERE mblnr EQ wa_mkpf-mblnr  "Document Number
            AND mjahr EQ wa_mkpf-mjahr. "Document Year

    IF it_mseg[] IS NOT INITIAL.
***Obtaining Requisition Details
      SELECT rsnum  AS rsnum  "Requisition Number
             rspos  AS rspos  "Requisition Item
             bdmng  AS bdmng  "Quantity Required
             meins  AS meins  "Unit
             ERFMG  AS ERFMG  "QUANTITY REQUIRED
             ERFME  AS ERFME  "UNIT
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

*---- Added by Sandeep on 25.02.2019 for Division -----------------------*
        CLEAR: LW_MAKT,LV_SPART.
        READ TABLE it_makt INTO LW_MAKT INDEX 1.
          IF SY-SUBRC = 0.
             SELECT SINGLE SPART FROM MARA INTO LV_SPART WHERE MATNR = LW_MAKT-MATNR.
          ENDIF.
      ENDIF.  "IF sy-subrc = 0.
**BREAK:10106, ABAP01.
    IF LV_SPART NE '40'.
      CLEAR: LW_MSEG,LV_ADRNR,LW_SADRC,LV_PARAMVAL,LV_PARAM2.
        READ TABLE IT_MSEG INTO LW_MSEG WITH KEY XAUTO ='X'.
          IF SY-SUBRC = 0.
              SELECT SINGLE ADRNR FROM T001W INTO LV_ADRNR WHERE WERKS = LW_MSEG-WERKS.
                IF LV_ADRNR IS NOT INITIAL.
                  SELECT SINGLE * FROM ADRC INTO LW_SADRC WHERE ADDRNUMBER = LV_ADRNR.

                  SELECT SINGLE PARAM2 PARAMVAL FROM Z6MMA_PARAMS
                    INTO ( LV_PARAM2,LV_PARAMVAL ) WHERE PROGNAME EQ 'STATE_MAP' AND PARAM1 EQ LW_SADRC-REGION.

                  CONCATENATE LV_PARAMVAL '(' LV_PARAM2 ')' INTO LW_HEADER-ZSSTATE.
                  CONDENSE LW_HEADER-ZSSTATE.
                ENDIF.

            CLEAR: LV_ADRNR,LV_PARAMVAL,LV_PARAM2.
            SELECT SINGLE ADRNR FROM TWLAD INTO LV_ADRNR WHERE WERKS = LW_MSEG-WERKS AND LGORT = LW_MSEG-LGORT.
              IF LV_ADRNR IS NOT INITIAL.
                SELECT SINGLE * FROM ADRC INTO LW_BADRC WHERE ADDRNUMBER = LV_ADRNR.

                SELECT SINGLE PARAM2 PARAMVAL FROM Z6MMA_PARAMS
                  INTO  ( LV_PARAM2,LV_PARAMVAL ) WHERE PROGNAME EQ 'STATE_MAP' AND PARAM1 EQ LW_BADRC-REGION.

                CONCATENATE LV_PARAMVAL '(' LV_PARAM2 ')' INTO LW_HEADER-ZBSTATE.
                  CONDENSE LW_HEADER-ZBSTATE.
              ENDIF.
          ENDIF.

         LW_HEADER-MBLNR = WA_MKPF-MBLNR.
         LW_HEADER-MJAHR = WA_MKPF-MJAHR.

         DELETE IT_MSEG WHERE XAUTO NE 'X'.
         LOOP AT IT_MSEG ASSIGNING <FS_MSEG>.
           READ TABLE IT_MAKT INTO LW_MAKT WITH KEY MATNR = <FS_MSEG>-MATNR.
            IF SY-SUBRC = 0.
              <FS_MSEG>-MAKTX = LW_MAKT-MAKTX.
            ENDIF.

            <FS_MSEG>-PRICE = ( <FS_MSEG>-LBKUM / <FS_MSEG>-ERFMG ).

            LW_HEADER-WAERS = <FS_MSEG>-WAERS.
            LW_HEADER-TQTY  = ( LW_HEADER-TQTY + <FS_MSEG>-ERFMG ).
            LW_HEADER-TVALUE = ( LW_HEADER-TVALUE + <FS_MSEG>-LBKUM ).
            LW_HEADER-TOT_AMT = LW_HEADER-TOT_AMT +  LW_HEADER-TVALUE.

            CLEAR: LW_MAKT.
         ENDLOOP.
         UNASSIGN <FS_MSEG>.
      ENDIF.


*---- End of Addition ---------------------------------------------------*

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

  IF LV_SPART NE '40'.
    CLEAR: c_form.
    c_form = 'Z6SD001S_TAX_INVOICE_GST_ZWE1'.

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

      CALL FUNCTION lf_fm_name  "'/1BCDWB/SF00000210'
        EXPORTING
          lw_sadrc                   = LW_SADRC
          lw_badrc                   = LW_BADRC
          LW_HEADER                  = LW_HEADER
        TABLES
          lt_mseg                    = IT_MSEG[]
       EXCEPTIONS
         FORMATTING_ERROR           = 1
         INTERNAL_ERROR             = 2
         SEND_ERROR                 = 3
         USER_CANCELED              = 4
         OTHERS                     = 5.
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
      ENDIF.    "IF sy-subrc <> 0.

  ELSE.
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

      CALL FUNCTION lf_fm_name  "Generated FM Name
        EXPORTING
          wa_mkpf          = wa_mkpf  "Material Document Header Details
        TABLES
          it_mseg          = it_mseg  "Material Document Item Details
          it_makt          = it_makt  "Material Details
          it_rsnum         = it_rsnum "Requisition Details
          it_t001l         = it_t001l "Location Details
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          user_canceled    = 4
          OTHERS           = 5.
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
      ENDIF.    "IF sy-subrc <> 0.

  ENDIF.

ENDFORM.                    " PROCESSING
