************************************************************************
*     REPORT RM07MLBS   (Transaction MB52)
*
*     History:
* February 2009 RR                                          "1311282
* added field LGOBE into output list with parameter         "1311282
* "c-no-out" in form F0300_FIELDCAT_FLAT.                   "1311282
*
* July 2006 MS                                              "n960980
* changed output of first line in print modus. If you enter "n960980
* user commands after function print, you get the list in   "n960980
* print modus view.                                         "n960980
*
* Jan. 2006 MS                                              "n912093
* added fields GLGMG,WGLGM,UMLMC,WUMLC,TRAME,WTRAM          "n912093
* into output list with parameter "c-no-out"                "n912093
* corrected documentation for field "Stock in transfer"     "n912093
* down with release 6.0                                     "n912093
* added dynamic break-points from cp-group MMIM_REP_MB52    "n912093

* März 2005 TW                                              "n829722
* authorization check should be always processed            "n829722
*
* Nov. 2004 MM :                                         "AC0K020254
* origin acceptance : process valuated block GR          "AC0K020254
* stock MARC-BWESB as hidden field                       "AC0K020254

* July 2004 MM                                              "n759412
*  Avoid shortdump DBIF_RSQL_INVALID_RSQL
*
* Improvements :                       Nov 2003 TW          "n667256
* - enable Webreporting                                     "n667256
* - print the page numbers                                  "n667256
* - use function module for database commit for the update  "n667256
*   of the parameters in table ESDUS. This allows to record "n667256
*   this transaction for a batch input session using        "n667256
*   transaction SHDB                                        "n667256

* Feb. 2003 MM                                              "n577268
* ignore stocks or special stocks from plant level when     "n577268
* user selects via storage location                         "n577268

* Jan. 2003 MM                                              "n591618
* report displayed stocks for plant although the user has   "n591618
* not the required authorization for that plant             "n591618

* Dec. 2002 MM                                              "n579976
* M7 393 when user deletes the initial display variant      "n579976

*-----------------------------------------------------------"n546707
* note 546707                         August 19th, 2002 MM  "n546707
* wrong parameters for AUTHORITY-CHECK 'F_BKPF_BUK' fixed   "n546707
*-----------------------------------------------------------"n531604
*     performance improved  4.6 and higher   July 2002 MM   "n531604
*     - consider settings from view V_MMIM_REP_PRINT        "n531604
*     - choose flat or hierarchic list                      "n531604
*     - show the individual line of special stocks E and Q  "n531604
*     - authority-check for values per company code         "n531604
*     - work with a lean table ORGAN                        "n531604
*     - new function processing special stocks              "n531604
*     - new function of parameter XLVORM                    "n531604
*     - evaluate and show the flag for deletion             "n531604
*     - contents of parameters :                            "n531604
*       - in dialog mode : preset parameters from last run  "n531604
*       - save the parameters during each run               "n531604
*-----------------------------------------------------------"n531604
*
*     note 494306  FI help dokumentation for parameters improved
*                  Feb. 11th 2002 XJD
*
*     Note 155853: First version
*     Note 161442: Display variante possible again
*     Note 177898: MSKA/MSPR with duplicate entires, SELECT error
*     Note 182322: Error during authorization check for plant
*     Note 307852: Jump back to initial screen for empty selection
*     Note 311770: Batch selection
*     Note 353428: Value even for valuation area with 0 stock
*     Note 304353: New selection fields into the standard.
*     Note 388735: Abort COLLECT_OVERFLOW_TYPE_P due to note 353428
*     Note 407810: Typo-error causes unprecise values (follow up
*                  of note 388735).
*
************************************************************************
*     List of stock quantities and values
************************************************************************
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 20.10.2015
*   REASON FOR CHANGE: Add Authorization for Plant and Sales District
*   REQUEST #: IRDK921177
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 30.12.2015
*   REASON FOR CHANGE: Remove Authorization for ZCITYC
*   REQUEST #: IRDK922030
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 30.12.2015
*   REASON FOR CHANGE: Added 2 fields Loc Received Date,Last movement Date
*   REQUEST #: IRDK922032
* --------------------------------------------------------------------------------------------*

REPORT zrm07mlbs_new MESSAGE-ID m7 NO STANDARD PAGE HEADING LINE-SIZE 170.
*ENHANCEMENT-POINT RM07MLBS_G4 SPOTS ES_RM07MLBS STATIC.
*ENHANCEMENT-POINT RM07MLBS_G5 SPOTS ES_RM07MLBS.
*ENHANCEMENT-POINT RM07MLBS_G6 SPOTS ES_RM07MLBS STATIC.
*ENHANCEMENT-POINT RM07MLBS_G7 SPOTS ES_RM07MLBS.

************************************************************************
* Data declarations
************************************************************************

* Type pools
TYPE-POOLS: slis, imrep.

* Database tables
TABLES: mara, makt, mard, mchb, mkol, mslb, mska, msku, mssa, mspr,
        mssq, mbew, ebew, qbew, t134m, t001w, t001l, marc, t001, t001k,
        t023, t024 , t005g.

TABLES : sscrfields.         "for the user-commands

* working table for the entries of all stock tables
DATA: BEGIN OF collector OCCURS 0,
       matnr LIKE mara-matnr,
       werks LIKE t001w-werks,
       lgort LIKE mard-lgort,
       sobkz LIKE mkol-sobkz,
       pspnr               LIKE  mspr-pspnr,
       vbeln               LIKE  mska-vbeln,
       posnr               LIKE  mska-posnr,
       lifnr LIKE mslb-lifnr,
       kunnr LIKE msku-kunnr,
       lvorm               LIKE  mard-lvorm,

       kzbws LIKE mssa-kzbws,
       charg LIKE mchb-charg,
       labst LIKE mard-labst,
       insme LIKE mard-insme,
       speme LIKE mard-speme,
       einme LIKE mard-einme,
       retme LIKE mard-retme,
       umlme LIKE mard-umlme,
       bwesb               LIKE  marc-bwesb,                "AC0K020254
       glgmg LIKE marc-glgmg,                               "n912093
       trame LIKE marc-trame,                               "n912093
       umlmc LIKE marc-umlmc,                               "n912093
*ENHANCEMENT-POINT EHP604_RM07MLBS_01 SPOTS ES_RM07MLBS STATIC .
* CHANGES BY PUNAM TO ADD FOLLOWING THREE FIELDS
       hsdat TYPE mch1-hsdat, " Date of Manufacture                  "PUNAM
       vfdat TYPE mch1-vfdat, "Shelf Life Exp. Date                  "PUNAM
       atwrt TYPE auspc_v1-atwrt, "Old Batch number                  "PUNAM
       spart LIKE mara-spart, " DIVISION                             "PUNAM
       vtext LIKE tspat-vtext, " DIVISION DESCRIPTION                "PUNAM
* END BY PUNAM
       cityc LIKE t001w-cityc,
       bezei LIKE t005h-bezei,
       ersda TYPE mchb-ersda, " Loc Received Date - Added by CS on 30.12.2015
       laeda TYPE	mchb-laeda " Last movement Date - Added by CS on 30.12.2015
       .
DATA: END OF collector.

* Internal tables
DATA: BEGIN OF header OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        werks LIKE t001w-werks,
        name1 LIKE t001w-name1,
        mtart LIKE mara-mtart,
        matkl LIKE mara-matkl,
        spart LIKE mara-spart, " DIVISION  "PUNAM
        vtext LIKE tspat-vtext, " DIVISION DESCRIPTION                "PUNAM
        cityc LIKE t001w-cityc, " sales district "Punam
        extwg LIKE mara-extwg, " external material grp
        bezei LIKE t005h-bezei.
*ENHANCEMENT-POINT EHP604_RM07MLBS_02 SPOTS ES_RM07MLBS STATIC .
DATA: END OF header.

DATA: BEGIN OF bestand OCCURS 0,
*        Key fields
         matnr LIKE mara-matnr,
         werks LIKE t001w-werks,
         lgort LIKE mard-lgort,
         sobkz LIKE mkol-sobkz,
         ssnum               LIKE  bickey-ssnum,            "n531604
         pspnr               LIKE  mspr-pspnr,              "n531604
         vbeln               LIKE  mska-vbeln,              "n531604
         posnr               LIKE  mska-posnr,              "n531604
         lifnr LIKE mkol-lifnr,
         kunnr LIKE msku-kunnr,
         kzbws LIKE mssa-kzbws,
         charg LIKE mchb-charg,
*        Additional data (texts, unit, ...)
         maktx LIKE marav-maktx,
         bwkey LIKE mbew-bwkey,
         mtart LIKE marav-mtart,
         matkl LIKE marav-matkl,
         meins LIKE marav-meins,
         bwtty LIKE marc-bwtty,
         xchar LIKE marc-xchar,
         lgobe LIKE t001l-lgobe,
         bwtar LIKE mcha-bwtar,
         waers LIKE t001-waers,
         name1 LIKE t001w-name1,
*        Quantities and currencies
         labst LIKE mard-labst,
         wlabs LIKE mbew-salk3,
         insme LIKE mard-insme,
         winsm LIKE mbew-salk3,
         speme LIKE mard-speme,
         wspem LIKE mbew-salk3,
         einme LIKE mard-einme,
         weinm LIKE mbew-salk3,
         retme LIKE mard-retme,
         wretm LIKE mbew-salk3,
         umlme LIKE mard-umlme,
         wumlm LIKE mbew-salk3,
         glgmg LIKE marc-glgmg,                             "n912093
         wglgm LIKE mbew-salk3,                             "n912093
         trame LIKE marc-trame,                             "n912093
         wtram LIKE mbew-salk3,                             "n912093
         umlmc LIKE marc-umlmc,                             "n912093
         wumlc LIKE mbew-salk3,                             "n912093

*        Dummy field
         dummy               TYPE  alv_dummy,
*        Colour
         farbe TYPE slis_t_specialcol_alv,
         lvorm               LIKE  mard-lvorm,

*        valuated blocked GR stock                       "AC0K020254
         bwesb               LIKE  marc-bwesb,              "AC0K020254
         wbwesb              LIKE  mbew-salk3,              "AC0K020254
*ENHANCEMENT-POINT EHP604_RM07MLBS_03 SPOTS ES_RM07MLBS STATIC .

* CHANGES BY PUNAM TO ADD FOLLOWING THREE FIELDS
         hsdat TYPE mch1-hsdat, " Date of Manufacture                  "PUNAM
         vfdat TYPE mch1-vfdat, "Shelf Life Exp. Date                  "PUNAM
         cuobj_bm TYPE mch1-cuobj_bm,
         atwrt TYPE auspc_v1-atwrt, "Old Batch number                      "PUNAM
         spart LIKE mara-spart, " DIVISION                      "PUNAM
         vtext LIKE tspat-vtext, " DIVISION DESCRIPTION                "PUNAM
         sl_days TYPE i, "Shelf Life-Days
         status(50),
         cityc LIKE t001w-cityc,
         extwg LIKE mara-extwg,
         bezei LIKE t005h-bezei,
         ersda TYPE mchb-ersda, " Loc Received Date - Added by CS on 30.12.2015
         laeda TYPE	mchb-laeda " Last movement Date - Added by CS on 30.12.2015
         .
* END BY PUNAM
DATA:  END OF bestand.

* define a lean table organ
TYPES : BEGIN OF stype_organ,
          werks              LIKE  t001w-werks,
          bwkey              LIKE  t001w-bwkey,
          name1              LIKE  t001w-name1,
          bukrs              LIKE  t001-bukrs,
          waers              LIKE  t001-waers,
        END OF stype_organ,

        stab_organ           TYPE STANDARD TABLE OF
                             stype_organ
                             WITH DEFAULT KEY.

DATA: g_t_organ              TYPE  stab_organ,
      g_s_organ              TYPE  stype_organ.

* define a buffer table for the MARD entries with flag
* for deletion
TYPES : BEGIN OF stype_mard_lv,
          matnr              LIKE  mard-matnr,
          werks              LIKE  mard-werks,
          lgort              LIKE  mard-lgort,
          lvorm              LIKE  mard-lvorm,
        END OF stype_mard_lv,

        htab_mard_lv         TYPE HASHED TABLE OF
                             stype_mard_lv
                   WITH UNIQUE KEY matnr werks lgort.

DATA : g_s_mard_lv           TYPE  stype_mard_lv,
       g_t_mard_lv           TYPE  htab_mard_lv.

* define a buffer table for the storage bins
TYPES : BEGIN OF stype_t001l,
          werks              LIKE  t001l-werks,
          lgort              LIKE  t001l-lgort,
          lgobe              LIKE  t001l-lgobe,
        END OF stype_t001l,

        htab_t001l           TYPE HASHED TABLE OF
                             stype_t001l
                             WITH UNIQUE KEY werks lgort.

DATA : g_s_t001l             TYPE  stype_t001l,
       g_t_t001l             TYPE  htab_t001l.

* define working areas for access table organ               "n531604
TYPES : BEGIN OF stype_buffer,                              "n531604
         werks               LIKE  t001w-werks,
         bukrs               LIKE  t001-bukrs,
         subrc               LIKE  syst-subrc,
       END OF stype_buffer,

       stab_buffer           TYPE STANDARD TABLE OF
                             stype_buffer
                             WITH DEFAULT KEY.

DATA : g_s_buffer          TYPE  stype_buffer,
       g_t_buffer          TYPE  stab_buffer.

* Data for listviewer
DATA: repid     LIKE sy-repid.
DATA: fieldcat  TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: keyinfo   TYPE slis_keyinfo_alv.
DATA: color     TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA: layout    TYPE slis_layout_alv.

DATA: sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE.
DATA: excluding TYPE slis_t_extab WITH HEADER LINE.

* internal working table for events / for the headlines     "n667256
DATA: gs_events     TYPE slis_alv_event.                    "n667256
DATA: gt_events     TYPE slis_t_event.                      "n667256

                                                            "n667256
* for the header of the list, when alv grid is in use       "n667256
DATA : gt_ueb                TYPE  slis_t_listheader,       "n667256
       gs_ueb                TYPE  slis_listheader.         "n667256

* Variants
DATA: variante        LIKE disvariant,
      variante_flat   LIKE disvariant,
      def_variante    LIKE disvariant,
      def_variante_f4 LIKE disvariant,
      variant_exit(1) TYPE c.

*ENHANCEMENT-POINT RM07MLBS_01 SPOTS ES_RM07MLBS STATIC.
*ENHANCEMENT-POINT RM07MLBS_13 SPOTS ES_RM07MLBS STATIC .
DATA : g_f_vari_hsq          LIKE  disvariant-variant,
       g_f_vari_flt          LIKE  disvariant-variant.

* working fields to save the initial display variants       "n579976
DATA : g_f_vari_hsq_initial  LIKE  disvariant-variant,      "n579976
       g_f_vari_flt_initial  LIKE  disvariant-variant.      "n579976

* Global variables for handling ALV functionality
TABLES: mmim_rep_print.

DATA: alv_keyinfo      TYPE slis_keyinfo_alv.
DATA: alv_variant      LIKE disvariant.
DATA: alv_layout       TYPE slis_layout_alv.
DATA: alv_repid        LIKE sy-repid.
DATA: alv_print        TYPE slis_print_alv.
DATA: alv_detail_func(30)    TYPE  c,
      alv_color              LIKE      mmim_rep_print-color.

* User settings for the checkboxes
DATA: oref_settings TYPE REF TO cl_mmim_userdefaults.

* define working fields
DATA : g_cnt_col_pos         TYPE i,
       g_cnt_spos            TYPE i.

DATA : g_flag_ok(01)         TYPE c,
       g_flag_mess_333(01)   TYPE c,
       g_flag_t001l(01)      TYPE c.

DATA : g_cnt_variant_error   TYPE i.                        "n667256

* does the user want to suppress objects from plant level ? "n577268
DATA : g_flag_suppress_init_lgort(01)  TYPE c.              "n577268

DATA : BEGIN OF g_flag_sobkz,
         vbeln(01)           TYPE c,
         pspnr(01)           TYPE c,
         lifnr(01)           TYPE c,
         kunnr(01)           TYPE c,
       END OF g_flag_sobkz.

CONSTANTS : c_no_out(01)     TYPE c    VALUE 'X',
            c_out(01)        TYPE c    VALUE space.

* flag to be set when INITIALIZATION was processed          "n667256
DATA g_flag_initialization(01) TYPE c.                      "n667256

* authorization check should be always processed            "n829722
DATA t_flag_launched(01) TYPE c.                            "n829722

*data : novalues   like      am07m-mb52_noval value 'X'.

DATA: lv_werks_auth_flg TYPE c VALUE '',  " Plant Authorization Flag, Added by CS on 20.10.2015 For Authorization Check
      lv_cityc_auth_flg TYPE c VALUE ''. " Sales District Auth Flag, Added by CS on 20.10.2015 For Authorization Check

*{   INSERT         SBXK900030                                        1
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Wednesday, October 10, 2018 20:57:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - Field length extensions
* Solution   - Replace Field
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*

DATA : lv_objek TYPE ausp-objek.
*}   INSERT
DATA: VALUE_DISPLAY(10).
************************************************************************
* Selection screen
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK abgrenzung WITH FRAME TITLE text-001.

SELECT-OPTIONS:
  matnr  FOR mara-matnr MEMORY ID mat MATCHCODE OBJECT mat1,
  werks  FOR t001l-werks  MEMORY ID wrk,                    "718285
  cityc  FOR t005g-cityc,
  lgort  FOR t001l-lgort  MEMORY ID lag,
  charg  FOR mchb-charg  MEMORY ID cha MATCHCODE OBJECT mch1.

SELECTION-SCREEN END OF BLOCK abgrenzung.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK lbs WITH FRAME TITLE text-070.

SELECT-OPTIONS:
  matart FOR mara-mtart,
  matkla FOR mara-matkl,
  extwg FOR mara-extwg,
  ekgrup FOR marc-ekgrp.
*ENHANCEMENT-POINT RM07MLBS_3 SPOTS ES_RM07MLBS STATIC .

SELECTION-SCREEN END OF BLOCK lbs.

*----------------------------------------------------------------------*

* for the selection os special stocks
SELECTION-SCREEN BEGIN OF BLOCK lb2 WITH FRAME TITLE text-071.

* for the selection os special stocks
PARAMETERS : pa_sond       LIKE      rmmmb-kzlso
                           DEFAULT   'X'.

SELECT-OPTIONS:
  so_sobkz                 FOR  mkol-sobkz.

SELECTION-SCREEN END OF BLOCK lb2.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK lb1 WITH FRAME TITLE text-080.

* select only lines who contain at least one negative stock
PARAMETERS: negativ LIKE am07m-seneg.

* Documentation for parameter XMCHB improved                "n494306
PARAMETERS: xmchb            LIKE      am07m-mb52_xmchb     "n494306
                             DEFAULT   'X'.

* Checkbox to eliminate lines with zero stocks
PARAMETERS: nozero   LIKE rmmmb-kznul DEFAULT   'X'.

* Checkbox to disable value processing.
* Documentation for parameter NOVALUES improved             "n494306
PARAMETERS: novalues  LIKE      am07m-mb52_noval." default 'X'." MODIF ID mod. "n494306


SELECTION-SCREEN END OF BLOCK lb1.

*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK liste WITH FRAME TITLE text-005.

* choose flat or hierarchic list                            "n531604
SELECTION-SCREEN BEGIN OF LINE.                             "n531604
SELECTION-SCREEN         POSITION 1.                        "n531604
PARAMETERS : pa_hsq      LIKE  am07m-mb52_alv_hsq           "n531604
                         DEFAULT  'X'                       "n531604
                         RADIOBUTTON GROUP alvv             "n531604
                         USER-COMMAND alvv.                 "n531604
SELECTION-SCREEN         COMMENT 3(40)  text-006            "n531604
                         FOR FIELD pa_hsq.                  "n531604
SELECTION-SCREEN END OF LINE.                               "n531604
                                                            "n531604
SELECTION-SCREEN BEGIN OF LINE.                             "n531604
SELECTION-SCREEN         POSITION 1.                        "n531604
PARAMETERS : pa_flt      LIKE  am07m-mb52_alv_flt           "n531604
                         RADIOBUTTON GROUP alvv.            "n531604
SELECTION-SCREEN         COMMENT 3(40)  text-007            "n531604
                         FOR FIELD pa_flt.                  "n531604
SELECTION-SCREEN END OF LINE.                               "n531604

*  parameters :
*    pa_grid                  type  MB_XFELD
*                             default 'X'
*                             radiobutton group alv1,
*    pa_class                 type  MB_XFELD
*                             radiobutton group alv1.

PARAMETERS: p_vari LIKE disvariant-variant.

SELECTION-SCREEN END OF BLOCK liste.
*ENHANCEMENT-POINT EHP604_RM07MLBS_04 SPOTS ES_RM07MLBS STATIC .

*----------------------------------------------------------------------*

* F4-Help for variant
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

AT SELECTION-SCREEN.

* check radiobuttons                                          "n667256
  IF  pa_hsq IS INITIAL.                                    "n667256
    IF  pa_flt IS INITIAL.                                  "n667256
*     not allowed                                             "n667256
      MOVE  'X'              TO  pa_hsq.                    "n667256
      ADD  1                 TO  g_cnt_variant_error.       "n667256
    ENDIF.                                                  "n667256
  ELSE.                                                     "n667256
    IF  pa_flt IS INITIAL.                                  "n667256
    ELSE.                                                   "n667256
*     not allowed                                             "n667256
      CLEAR                  pa_flt.                        "n667256
      ADD  1                 TO  g_cnt_variant_error.       "n667256
    ENDIF.                                                  "n667256
  ENDIF.                                                    "n667256
                                                            "n667256
* the user will get the info about the old variant only once  "n667256
  IF  g_cnt_variant_error = 1.                              "n667256
    IF  NOT sy-slset IS INITIAL.                            "n667256
      MESSAGE i634(db)       WITH  sy-slset sy-repid.       "n667256
    ENDIF.                                                  "n667256
  ENDIF.                                                    "n667256

* has the user changed the radiobuttons for the mode of the "n531604
* SAP-LIST-VIEWER ?                                         "n531604
  IF  sscrfields-ucomm  =  'ALVV'.
*   yes, restore the old entry if extists                   "n531604
    IF      NOT pa_hsq IS INITIAL.
      MOVE  g_f_vari_hsq     TO  p_vari.
    ELSEIF  NOT  pa_flt IS INITIAL.
*     for flat ( simple ) list
      MOVE  g_f_vari_flt     TO  p_vari.
    ENDIF.

  ELSE.
*   save the display variant depending on the selected mode "n531604
*   of the SAP-LIST-VIEWER
    IF      NOT pa_hsq IS INITIAL.
*     for hierarchic seq. list
      MOVE p_vari            TO  g_f_vari_hsq.

    ELSEIF  NOT  pa_flt IS INITIAL.
*     for flat ( simple ) list
      MOVE p_vari            TO  g_f_vari_flt.

    ENDIF.
  ENDIF.

* it is necessary to set flag xmchb if batch has been entered because
* otherwise MCHB will not be read and non suitable items can't be
* be removed later on in form data_selection
  IF NOT charg[] IS INITIAL. xmchb = 'X'. ENDIF.    "note 311770



START-OF-SELECTION.
VALUE_DISPLAY = 'ZMB52_VAL'.
EXPORT VALUE_DISPLAY FROM VALUE_DISPLAY to MEMORY id 'VALUE_DISPLAY'.
PERFORM CALL_STD_MB52.

*&---------------------------------------------------------------------*
*& Form CALL_STD_MB52
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CALL_STD_MB52 .

*call TRANSACTION 'MB52' WITH AUTHORITY-CHECK .
SUBMIT RM07MLBS
WITH matnr in matnr " Material
WITH werks in werks " Plant
WITH cityc IN cityc " Sales district
WITH lgort IN lgort " Storage Location
WITH charg in charg " Batch
WITH matart in matart " Material Type
WITH matkla in matkla " Material Group
WITH extwg in extwg " Purchasing Group
WITH ekgrup in ekgrup " Ext. Material Group
WITH pa_sond = pa_sond " Read Special Stocks
WITH so_sobkz IN so_sobkz " Special Stock Indicator
WITH negativ = negativ " Negative stocks
WITH xmchb = xmchb " Display batch stocks
WITH nozero = nozero " No zero stock lines
WITH novalues = novalues  " Do not display values
WITH pa_hsq = pa_hsq " Generate Hierarchical List
WITH pa_flt = pa_flt " Generate Non-Hierarchical List
WITH p_vari = p_vari " Layout
AND RETURN.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       F4-Hilfe für Reportvariante                                    *
*----------------------------------------------------------------------*
FORM f4_for_variant.

* look for the available display variant depending on the   "n531604
* selected mode of the SAP-LIST-VIEWER
  IF      NOT pa_hsq IS INITIAL.
*     for hierarchic seq. list

    variante-REPORT = sy-repid.
    variante-USERNAME = sy-uname.
    MOVE variante          TO  def_variante_f4.

  ELSEIF  NOT  pa_flt IS INITIAL.
*     for flat ( simple ) list
    variante_flat-REPORT = sy-repid.
    variante_flat-USERNAME = sy-uname.
    variante_flat-HANDLE = 'FLAT'.

    MOVE variante_flat     TO  def_variante_f4.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            is_variant          = def_variante_f4
            i_save              = 'A'
*           it_default_fieldcat =
       IMPORTING
            e_exit              = variant_exit
            es_variant          = def_variante
       EXCEPTIONS
            not_found = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF variant_exit = space.
*     save the selected display variant
      p_vari = def_variante-variant.

      IF      NOT pa_hsq IS INITIAL.
*       for hierarchic seq. list
        MOVE  p_vari         TO  g_f_vari_hsq.

      ELSEIF  NOT  pa_flt IS INITIAL.
*       for flat ( simple ) list
        MOVE  p_vari         TO  g_f_vari_flt.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " F4_FOR_VARIANT



************************************************************************
* Main program
************************************************************************

*AT SELECTION-SCREEN.
*
** check radiobuttons                                          "n667256
*  IF  pa_hsq IS INITIAL.                                    "n667256
*    IF  pa_flt IS INITIAL.                                  "n667256
**     not allowed                                             "n667256
*      MOVE  'X'              TO  pa_hsq.                    "n667256
*      ADD  1                 TO  g_cnt_variant_error.       "n667256
*    ENDIF.                                                  "n667256
*  ELSE.                                                     "n667256
*    IF  pa_flt IS INITIAL.                                  "n667256
*    ELSE.                                                   "n667256
**     not allowed                                             "n667256
*      CLEAR                  pa_flt.                        "n667256
*      ADD  1                 TO  g_cnt_variant_error.       "n667256
*    ENDIF.                                                  "n667256
*  ENDIF.                                                    "n667256
*                                                            "n667256
** the user will get the info about the old variant only once  "n667256
*  IF  g_cnt_variant_error = 1.                              "n667256
*    IF  NOT sy-slset IS INITIAL.                            "n667256
*      MESSAGE i634(db)       WITH  sy-slset sy-repid.       "n667256
*    ENDIF.                                                  "n667256
*  ENDIF.                                                    "n667256
*
** has the user changed the radiobuttons for the mode of the "n531604
** SAP-LIST-VIEWER ?                                         "n531604
*  IF  sscrfields-ucomm  =  'ALVV'.
**   yes, restore the old entry if extists                   "n531604
*    IF      NOT pa_hsq IS INITIAL.
*      MOVE  g_f_vari_hsq     TO  p_vari.
*    ELSEIF  NOT  pa_flt IS INITIAL.
**     for flat ( simple ) list
*      MOVE  g_f_vari_flt     TO  p_vari.
*    ENDIF.
*
*  ELSE.
**   save the display variant depending on the selected mode "n531604
**   of the SAP-LIST-VIEWER
*    IF      NOT pa_hsq IS INITIAL.
**     for hierarchic seq. list
*      MOVE p_vari            TO  g_f_vari_hsq.
*
*    ELSEIF  NOT  pa_flt IS INITIAL.
**     for flat ( simple ) list
*      MOVE p_vari            TO  g_f_vari_flt.
*
*    ENDIF.
*  ENDIF.
*
** it is necessary to set flag xmchb if batch has been entered because
** otherwise MCHB will not be read and non suitable items can't be
** be removed later on in form data_selection
*  IF NOT charg[] IS INITIAL. xmchb = 'X'. ENDIF.    "note 311770
*
** send a warning if the user starts this report without any "n531604
** restrictions for the database selection                   "n531604
** only when this report is started                          "n531604
*  IF matnr IS INITIAL AND                                   "n531604
*     werks IS INITIAL AND                                   "n531604
*     lgort IS INITIAL AND                                   "n531604
*     charg IS INITIAL.                                      "n531604
*    IF  sy-ucomm  =  'ONLI'  OR                             "n531604
*        sy-ucomm  =  'PRIN'.                                "n531604
**ENHANCEMENT-SECTION EHP604_RM07MLBS_43 SPOTS ES_RM07MLBS .
*      MESSAGE  w689.    "The selection was not restricted   "n531604
**END-ENHANCEMENT-SECTION.
*    ENDIF.                                                  "n531604
*  ENDIF.                                                    "n531604
** go on only if the user wants to launch this report        "n667256
** the authorization check should be always processed        "n829722
*  IF sy-ucomm = 'ONLI' OR                                   "n829722
*     sy-ucomm = 'PRIN' OR                                   "n829722
*     sy-ucomm = 'SJOB' OR                                   "n829722
*     sy-ucomm = space.                                      "n829722
*    MOVE 'X' TO t_flag_launched.                            "n829722
*  ELSE.                                                     "n829722
*    IF sy-ucomm <> space.                                   "n829722
*      CLEAR t_flag_launched.                                "n829722
*    ENDIF.                                                  "n829722
*  ENDIF.                                                    "n829722
*  CHECK t_flag_launched = 'X'.                              "n829722
*  PERFORM check_entry.
*  PERFORM organisation.
*  PERFORM check_authorization.
*
** save the parameters of this run in database table ESDUS   "n531604
*  PERFORM                    f0200_settings_save.           "n531604
*
**------------------------ Initialisierung -----------------------------*
*INITIALIZATION.
** novalues = 'X'.
**ENHANCEMENT-POINT RM07MLBS_05 SPOTS ES_RM07MLBS.
*  PERFORM                    f0000_get_print_settings.      "n531604
*
** look for the setting of the parameters from the last run  "n531604
*  PERFORM                    f0100_settings_init.           "n531604
*
*  PERFORM initialisierung.
*
** set flag when INITILIZATION is processed                  "n667256
*  MOVE  'X'        TO  g_flag_initialization.               "n667256
*                                                            "n667256
**-----------------------------------------------------------"n667256
*                                                            "n667256
**ENHANCEMENT-POINT RM07MLBS_07 SPOTS ES_RM07MLBS.
*START-OF-SELECTION.
*
** it makes no sence to carry out this report                "n667256
*  IF  g_cnt_variant_error > 0.                              "n667256
*    IF  NOT sy-slset IS INITIAL.                            "n667256
*      MESSAGE e634(db)       WITH  sy-slset sy-repid.       "n667256
*    ENDIF.                                                  "n667256
*  ENDIF.                                                    "n667256
*
** does the user restrict the storage locations and want to  "n577268
** suppress stock objects from plant level ?                 "n577268
*  CLEAR                      collector-lgort.               "n577268
*                                                            "n577268
*  IF  collector-lgort IN lgort.                             "n577268
*    CLEAR                    g_flag_suppress_init_lgort.    "n577268
*  ELSE.                                                     "n577268
*    MOVE  'X'                TO  g_flag_suppress_init_lgort. "n577268
*  ENDIF.                                                    "n577268
*
*  PERFORM check_auth_obj. " Added by Chirag Shah 20.10.2015 Check Authority of Plant & Sales District.
*
*  PERFORM data_selection.
****** Start Code: Added Code by CS on 19.10.2015 for Plant & Sales District Auth. ******
*  IF bestand[] IS NOT INITIAL.
*    IF lv_werks_auth_flg EQ 'X'." OR lv_cityc_auth_flg EQ 'X'.
*      MESSAGE 'Missing Authorization for Plant.' TYPE 'S' DISPLAY LIKE 'W'.
*    ENDIF.
*  ENDIF.
****** End Code: Added Code by CS on 19.10.2015 for Plant & Sales District Auth. ******
*
**-------------------------- Datenausgabe-------------------------------*
*END-OF-SELECTION.
*  READ TABLE bestand INDEX 1 TRANSPORTING NO FIELDS.
*
*  IF sy-subrc = 0.
**   the fieldcatalog depends on the type of list            "n531604
*    IF      NOT pa_hsq IS INITIAL.                          "n531604
**     create hierarchic list                                "n531604
*      PERFORM                fieldcatalog.                  "n531604
*    ELSEIF  NOT  pa_flt IS INITIAL.                         "n531604
*      PERFORM                f0300_fieldcat_flat.           "n531604
*    ENDIF.                                                  "n531604
*
*    IF  g_flag_mess_333 = 'X'.
**     "The list is incomplete due to lacking authorization
*      MESSAGE                s333.
*    ENDIF.
*
*    PERFORM list_output.
*  ELSE.
****** Start Code: Added Code by CS on 20.10.2015 for Plant & Sales District Auth. ******
*    IF lv_werks_auth_flg EQ 'X."'." OR lv_cityc_auth_flg EQ 'X'.
*      MESSAGE 'No record found / Missing Authorization for Plant.' TYPE 'I'.
*      LEAVE LIST-PROCESSING.
*    ELSE.
*      MESSAGE s843.
*    ENDIF.
**    MESSAGE s843.
****** End Code: Added Code by CS on 20.10.2015 for Plant & Sales District Auth. ******
**   Zu den vorgegebenen Daten ist kein Bestand vorhanden
*    IF NOT sy-calld IS INITIAL.                             "307852
*      LEAVE.                                                "307852
*    ELSE.                                                   "307852
*      LEAVE TO TRANSACTION sy-tcode.                        "307852
*    ENDIF.                                                  "307852
*  ENDIF.
*
*************************************************************************
** Read organisation
*************************************************************************
*FORM organisation.
*
** get all existing storage bins of the required plants
*  REFRESH : g_t_t001l, g_t_organ.
*  CLEAR   : g_s_t001l, g_s_organ, g_flag_t001l.
*
*  SELECT  werks lgort lgobe  FROM t001l
*                   INTO CORRESPONDING FIELDS OF TABLE g_t_t001l
*                   WHERE  werks IN werks
*                     AND  lgort IN lgort
*                     AND  lgobe NE space.
*
*  IF  sy-subrc IS INITIAL.
*    MOVE  'X'                TO  g_flag_t001l.
*  ENDIF.
*
*  SELECT DISTINCT werks name1 bwkey
*                  INTO CORRESPONDING FIELDS OF TABLE g_t_organ
*                  FROM t001w
*                  WHERE werks IN werks.
*
*  SORT g_t_organ             BY bwkey.
*
*  LOOP AT g_t_organ          INTO  g_s_organ.
*    ON CHANGE OF g_s_organ-bwkey.
*      CLEAR                  g_flag_ok.
*
*      SELECT SINGLE * FROM t001k
*                   WHERE bwkey EQ g_s_organ-bwkey.
*
*      IF sy-subrc IS INITIAL.
*        SELECT SINGLE * FROM t001
*                   WHERE bukrs EQ t001k-bukrs.
*
*        IF sy-subrc IS INITIAL.
*          MOVE  'X'          TO  g_flag_ok.
*        ENDIF.
*      ENDIF.
*    ENDON.
*
*    IF  g_flag_ok = 'X'.
*      MOVE-CORRESPONDING t001  TO  g_s_organ.
*      MODIFY  g_t_organ        FROM  g_s_organ.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                               " ORGANISATION
*
*************************************************************************
** Check authorization on plant level for all selected plants
*************************************************************************
*FORM check_authorization.
*
** define local working areas
*  DATA : l_s_bukrs           TYPE  stype_buffer,
*         l_t_bukrs           TYPE  stab_buffer.
*
*  SORT   g_t_organ           BY werks.
*
** report displayed stocks for plant although the user has   "n591618
** not the required authorization for that plant             "n591618
*  CLEAR                      g_s_buffer.                    "n591618
*
*  LOOP AT g_t_organ          INTO  g_s_organ.
**   check the authority only after the plant has changed    "n531604
*    IF  g_s_organ-werks NE g_s_buffer-werks.
*      MOVE  g_s_organ-werks  TO  g_s_buffer-werks.
*
**      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
**                     ID 'ACTVT' FIELD '03'
**                     ID 'WERKS' FIELD g_s_organ-werks.
**
**      IF NOT sy-subrc IS INITIAL.
**        set  cursor          field  'WERKS-LOW'.
**        MESSAGE e120         WITH  g_s_organ-werks.
**      ENDIF.
*    ENDIF.
*
*    IF  novalues IS INITIAL.                                "n531604
**     the user wants to see the values
*      IF  g_s_organ-bukrs NE g_s_buffer-bukrs.
**       check the authority after the company code changed
*
*        AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
**           parameters for AUTHORITY-CHECK command fixed    "n546707
*            ID 'BUKRS' FIELD g_s_organ-bukrs                "n546707
*            ID 'ACTVT' FIELD '03'.                          "n546707
*
*        MOVE : g_s_organ-bukrs    TO  g_s_buffer-bukrs,     "n667256
*               sy-subrc           TO  g_s_buffer-subrc.     "n667256
*
*        IF sy-subrc <> 0.
**         no authorization -> save the company code
*          MOVE : 'X'              TO  g_flag_mess_333,
*                 g_s_organ-bukrs  TO  l_s_bukrs-bukrs.
*          COLLECT l_s_bukrs       INTO  l_t_bukrs.
*        ENDIF.
*      ENDIF.
*
**     use the result from the buffer
*      IF  g_s_buffer-subrc <> 0.
*        CLEAR                g_s_organ-waers.
*        MODIFY  g_t_organ    FROM  g_s_organ
*                             TRANSPORTING  waers.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*
** send the info for each missing autority
*  IF  g_flag_mess_333  =  'X'.
*    SORT                     l_t_bukrs.
*    SET CURSOR               FIELD  'WERKS-LOW'.
*
*    LOOP AT l_t_bukrs        INTO  l_s_bukrs.
**     No authorization to display data for company code &
*      MESSAGE  i862(m3)      WITH  l_s_bukrs-bukrs.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.                    "check_authorization
*
*************************************************************************
** Check data on selection screen
*************************************************************************
*FORM check_entry.
*
** Check some entered data for consistency
*  CALL FUNCTION 'MMIM_ENTRYCHECK_MAIN'
*    TABLES
*      it_matnr = matnr
*      it_werks = werks
*      it_lgort = lgort
*      it_ekgrp = ekgrup
*      it_sobkz = so_sobkz.
*
** Material type
*  IF NOT matart-low IS INITIAL OR NOT matart-high IS INITIAL.
*    SELECT SINGLE * FROM t134m WHERE mtart IN matart.
*    IF NOT sy-subrc IS INITIAL.
*      MESSAGE e104(m3) WITH matart-low.
*    ENDIF.
*  ENDIF.
*
** Material class
*  IF NOT matkla-low IS INITIAL OR NOT matart-high IS INITIAL.
*    SELECT SINGLE * FROM t023 WHERE matkl IN matkla.
*    IF NOT sy-subrc IS INITIAL.
*      MESSAGE e883 WITH matkla-low.
*    ENDIF.
*  ENDIF.
*
** Display variant
*  IF NOT p_vari IS INITIAL.
*    SET CURSOR               FIELD  'P_VARI'.
*
*    IF      NOT pa_hsq IS INITIAL.
**     for hierarchic seq. list
*      MOVE : p_vari          TO  variante-variant,
*             variante        TO  def_variante.
*
*    ELSEIF  NOT  pa_flt IS INITIAL.
**     for flat ( simple ) list
*      MOVE : p_vari          TO  variante_flat-variant,
*             variante_flat   TO  def_variante.
*    ENDIF.
*
*    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
*      EXPORTING
*        i_save     = 'A'
*      CHANGING
*        cs_variant = def_variante.
*  ELSE.                                                     "n579976
**   the user wants no initial display variant               "n579976
*                                                            "n579976
*    IF      NOT pa_hsq IS INITIAL.                          "n579976
**     for hierarchic seq. list                              "n579976
*      IF  NOT g_f_vari_hsq_initial IS INITIAL.              "n579976
**       but the SAP-LIST-VIEWER will apply the existing     "n579976
**       initial display variant for hierarchic lists        "n579976
*        PERFORM  f3000_send_warning_m7_393                  "n579976
*                             USING  g_f_vari_hsq_initial.   "n579976
*      ENDIF.                                                "n579976
*                                                            "n579976
*    ELSEIF  NOT  pa_flt IS INITIAL.                         "n579976
**     for flat ( simple ) list                              "n579976
*      IF  NOT g_f_vari_flt_initial IS INITIAL.              "n579976
**       but the SAP-LIST-VIEWER will apply the existing     "n579976
**       initial display variant for flat lists              "n579976
*        PERFORM  f3000_send_warning_m7_393                  "n579976
*                             USING  g_f_vari_flt_initial.   "n579976
*      ENDIF.                                                "n579976
*    ENDIF.                                                  "n579976
*  ENDIF.
*
*ENDFORM.                               "check_entry
*
*************************************************************************
** Initialization: Read default variant
*************************************************************************
*FORM initialisierung.
*
** prepare the areas for the different display variants
*  repid = sy-repid.
*  CLEAR : variante,          variante_flat.
*  MOVE  : repid              TO  variante-report,
*          repid              TO  variante_flat-report,
*          'FLAT'             TO  variante_flat-handle.
*
** the display variant is depending on the seleted mode of   "n531604
** the SAP-LIST-VIEWER : look for both variants              "n531604
*
** a) Get default variant for the hierarchic list
*  def_variante = variante.
*
*  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
*    EXPORTING
*      i_save     = 'A'
*    CHANGING
*      cs_variant = def_variante
*    EXCEPTIONS
*      not_found  = 2.
*
*  IF sy-subrc = 0.
*    MOVE  def_variante-variant    TO  g_f_vari_hsq.
**   save the initial display variant for the hierseq. list  "n579976
*    MOVE  def_variante-variant    TO  g_f_vari_hsq_initial. "n579976
*  ENDIF.
*
** b) Get default variant for the non-hierarchic list
*  def_variante = variante_flat.
*
*  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
*    EXPORTING
*      i_save     = 'A'
*    CHANGING
*      cs_variant = def_variante
*    EXCEPTIONS
*      not_found  = 2.
*
*  IF sy-subrc = 0.
*    MOVE  def_variante-variant    TO  g_f_vari_flt.
**   save the initial display variant for the flat list      "n579976
*    MOVE  def_variante-variant    TO  g_f_vari_flt_initial. "n579976
*  ENDIF.
*
** take the required variant
*  IF      NOT pa_hsq IS INITIAL.
**   for hierarchic seq. list
*    p_vari =  g_f_vari_hsq.
*
*  ELSEIF  NOT  pa_flt IS INITIAL.
**   for flat ( simple ) list
*    p_vari = g_f_vari_flt.
*  ENDIF.
**ENHANCEMENT-POINT EHP604_RM07MLBS_05 SPOTS ES_RM07MLBS .
*
*ENDFORM.                               " INITIALISIERUNG
*
*************************************************************************
** Main data selection routine
*************************************************************************
*FORM data_selection.
*
** Materials to be processed
*  TYPES: BEGIN OF ty_mat,
*           matnr LIKE mara-matnr,
*           werks LIKE marc-werks,
*           xchar LIKE marc-xchar,
*           mtart LIKE mara-mtart,
*           matkl LIKE mara-matkl,
*           meins LIKE mara-meins,
*           trame LIKE marc-trame,
*           umlmc LIKE marc-umlmc,
*           glgmg LIKE marc-glgmg,                           "n912093
*           spart LIKE mara-spart, "DIVISION                 "BY PUNAM
*           vtext LIKE tspat-vtext, " DIVISION DESCRIPTION                "PUNAM
*           cityc LIKE t001w-cityc,
*           bezei LIKE t005h-bezei,
*           extwg LIKE mara-extwg,
*           bwesb             LIKE  marc-bwesb,              "AC0K020254
*           lvorm_mara        LIKE  mara-lvorm,
*           lvorm_marc        LIKE  marc-lvorm.
*
**ENHANCEMENT-POINT EHP604_RM07MLBS_06 SPOTS ES_RM07MLBS STATIC .
*  TYPES:  END OF ty_mat.
*
*  DATA: t_mat     TYPE ty_mat OCCURS 0 WITH HEADER LINE,
*        t_batch   TYPE ty_mat OCCURS 0 WITH HEADER LINE,
*        t_nobatch TYPE ty_mat OCCURS 0 WITH HEADER LINE.
**ENHANCEMENT-POINT EHP604_RM07MLBS_07 SPOTS ES_RM07MLBS STATIC .
*
** buffer for reading working tables
*  DATA : l_s_mat             TYPE  ty_mat,
*         l_f_matnr           LIKE  makt-matnr.
*
*  RANGES: r_sobkz FOR mkol-sobkz.
*
*  DATA: l_cnt_matnr_i_eq     TYPE i.                        "n759412
*  DATA: l_cnt_matnr_total    TYPE i.                        "n759412
*
*************************************************************************
** Read material master data (MARA and MARC)
*************************************************************************
*  REFRESH collector.
*
** take all matching entries, do not consider the deletion
** indicator
*
**** BEGIN INSERT n_759412
** analyse the select-option table for material
** numbers
*  CLEAR : l_cnt_matnr_total, l_cnt_matnr_i_eq.
*
*  LOOP AT matnr.
*    ADD  1             TO  l_cnt_matnr_total.
*
*    IF  NOT matnr-low     IS INITIAL  AND
*            matnr-sign    =  'I'      AND
*            matnr-option  =  'EQ'     AND
*            matnr-high    IS INITIAL.
**     the table contains single a material number
*      ADD  1           TO  l_cnt_matnr_i_eq.
*    ELSE.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
** added dynamic break-point ID MMIM_REP_MB52              "n912093
*  BREAK-POINT ID mmim_rep_mb52.                             "n912093
** process SELECT command depending on the
** required material selection
*  IF  l_cnt_matnr_total  > 0                 AND
*      l_cnt_matnr_total  = l_cnt_matnr_i_eq.
**ENHANCEMENT-SECTION EHP604_RM07MLBS_08 SPOTS ES_RM07MLBS .
*    SELECT mara~matnr werks xchar mtart matkl meins trame umlmc
*           bwesb glgmg spart                      "AC0K020254  "n912093
*         mara~lvorm AS lvorm_mara
*         marc~lvorm AS lvorm_marc
*         INTO CORRESPONDING FIELDS OF TABLE t_mat
*         FROM mara INNER JOIN marc
*         ON mara~matnr = marc~matnr
*         FOR ALL entries IN matnr
*         WHERE mara~matnr = matnr-low
*           AND werks IN werks
*           AND mtart IN matart
*           AND matkl IN matkla
*           AND extwg IN  extwg
*           AND ekgrp IN ekgrup.
**END-ENHANCEMENT-SECTION.
*  ELSE.
** END INSERT n_759412
**ENHANCEMENT-SECTION EHP604_RM07MLBS_09 SPOTS ES_RM07MLBS .
*    SELECT mara~matnr werks xchar mtart matkl meins trame umlmc
*           bwesb glgmg spart                         "AC0K020254 "912093
*         mara~lvorm AS lvorm_mara
*         marc~lvorm AS lvorm_marc
*         INTO CORRESPONDING FIELDS OF TABLE t_mat
*         FROM mara INNER JOIN marc
*         ON mara~matnr = marc~matnr
*         WHERE mara~matnr IN matnr
*           AND werks IN werks
*           AND mtart IN matart
*           AND matkl IN matkla
*           AND extwg IN extwg
*           AND ekgrp IN ekgrup.
**END-ENHANCEMENT-SECTION.
*  ENDIF.                                                    "n759412
**ENHANCEMENT-POINT EHP604_RM07MLBS_10 SPOTS ES_RM07MLBS .
*************************************************************************
** Get "normal" stocks.
** If no detailed batch display is required,
** all data come from MARD. Otherwise, materials with batch
** management are extracted from MCHB, the rest from MARD.
*************************************************************************
*  REFRESH: t_batch, t_nobatch.
** Split the worklist into the parts for each table...
*  IF xmchb IS INITIAL.
*    t_nobatch[] = t_mat[].
*  ELSE.
*    LOOP AT t_mat.
*      IF t_mat-xchar IS INITIAL.
*        APPEND t_mat TO t_nobatch.
*      ELSE.
*        APPEND t_mat TO t_batch.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
**ENHANCEMENT-POINT EHP604_RM07MLBS_11 SPOTS ES_RM07MLBS .
** Access MARD
** I you think that instead of SELECT-APPEND we could have used
** an array fetch, please wait for the table names to become different
** from the internal fields. B.T.W.: The DB-interface also buffers.
*  CLEAR collector.
*  READ TABLE t_nobatch INDEX 1 TRANSPORTING NO FIELDS.
*  IF sy-subrc = 0.
**ENHANCEMENT-SECTION EHP604_RM07MLBS_12 SPOTS ES_RM07MLBS .
*    SELECT matnr werks lgort
*           labst umlme insme einme speme retme lvorm
*           INTO (collector-matnr, collector-werks, collector-lgort,
*                 collector-labst, collector-umlme, collector-insme,
*                 collector-einme, collector-speme, collector-retme,
*                 collector-lvorm)
*           FROM mard
*           FOR ALL ENTRIES IN t_nobatch
*           WHERE matnr = t_nobatch-matnr
*             AND werks = t_nobatch-werks
*             AND lgort IN lgort.
*
**     save the MARD Key and deletion indicator for later
**     in table G_T_MARD_LV for use with special stocks
*      IF NOT pa_sond         IS INITIAL  AND
*         NOT collector-lvorm IS INITIAL.
*        MOVE-CORRESPONDING  collector
*                             TO  g_s_mard_lv.
*        INSERT  g_s_mard_lv  INTO  TABLE  g_t_mard_lv.
*      ENDIF.
*
*      PERFORM                f2000_collect_collector.
*    ENDSELECT.
**END-ENHANCEMENT-SECTION.
*  ENDIF.
*
** Access MCHB
*  CLEAR collector.
*  READ TABLE t_batch INDEX 1 TRANSPORTING NO FIELDS.
*  IF sy-subrc = 0.
**ENHANCEMENT-SECTION EHP604_RM07MLBS_13 SPOTS ES_RM07MLBS .
*    SELECT matnr werks lgort charg
*           clabs cumlm cinsm ceinm cspem cretm lvorm
*            ersda laeda " Added by CS on 30.12.2015
*           INTO (collector-matnr, collector-werks, collector-lgort,
*                 collector-charg,
*                 collector-labst, collector-umlme, collector-insme,
*                 collector-einme, collector-speme, collector-retme,
*                 collector-lvorm,
*                 collector-ersda, collector-laeda ) " Added by CS on 30.12.2015
*           FROM mchb
*           FOR ALL ENTRIES IN t_batch
*           WHERE matnr = t_batch-matnr
*             AND werks = t_batch-werks
*             AND lgort IN lgort
*             AND charg IN charg.
*
*      PERFORM                f2000_collect_collector.
*    ENDSELECT.
**END-ENHANCEMENT-SECTION.
*  ENDIF.
*
*************************************************************************
** Transfer stocks from MARC (TRAME, UMLMC)
*************************************************************************
*  CLEAR collector.
*
**ENHANCEMENT-SECTION EHP604_RM07MLBS_14 SPOTS ES_RM07MLBS .
*  LOOP AT t_mat WHERE umlmc <> 0 OR trame <> 0 OR           "AC0K020254
*                      bwesb <> 0 OR                         "AC0K020254
*                      glgmg <> 0.                           "912093
*
**     there are no lines with stock = zero
*
**     take the stocks from plant level only when the user   "n577268
**     does not restrict the storage location;               "n577268
*    CHECK : g_flag_suppress_init_lgort IS INITIAL.          "n577268
*
*    IF negativ = 'X'.
**       ignore entry if all stocks are zero or greater
*      IF  t_mat-trame >= 0 AND
*          t_mat-umlmc >= 0 AND
*          t_mat-glgmg >= 0.                                 "n912093
*        CONTINUE.          "take the next entry
*      ENDIF.
*    ENDIF.
*
*    collector-matnr = t_mat-matnr.
*    collector-werks = t_mat-werks.
*    collector-umlme = t_mat-trame + t_mat-umlmc.
*    collector-lvorm = t_mat-lvorm_marc.
*    collector-bwesb = t_mat-bwesb.                          "AC0K020254
*    collector-glgmg = t_mat-glgmg.                          "n912093
*    collector-trame = t_mat-trame.                          "n912093
*    collector-umlmc = t_mat-umlmc.                          "n912093
*    collector-spart = t_mat-spart. " DIVISION  "PUNAM
*
*    APPEND collector.
*  ENDLOOP.
*
*
*
**END-ENHANCEMENT-SECTION.
*
*************************************************************************
** Consignment from vendor (MKOL)
** Read only if requested by one of the
** flags on the selection screen. Absolutely inconsistent, but
** due to compatibility...
** MKOL has a flag for deletion
*************************************************************************
*
*  IF  NOT pa_sond IS INITIAL  AND
*      NOT t_mat[] IS INITIAL.
*    IF  'K' IN so_sobkz  OR
*        'M' IN so_sobkz.
*      CLEAR                  collector.
*
**ENHANCEMENT-SECTION EHP604_RM07MLBS_15 SPOTS ES_RM07MLBS .
*      SELECT matnr werks lgort charg sobkz lifnr
*             slabs sinsm seinm sspem lvorm
*             INTO (collector-matnr, collector-werks, collector-lgort,
*                   collector-charg, collector-sobkz, collector-lifnr,
*                   collector-labst, collector-insme, collector-einme,
*                   collector-speme, collector-lvorm)
*             FROM mkol
*             FOR ALL ENTRIES IN t_mat
*             WHERE matnr = t_mat-matnr
*               AND werks = t_mat-werks
*               AND lgort IN lgort
*               AND charg IN charg
*               AND sobkz IN so_sobkz.
*
*        PERFORM              f2000_collect_collector.
*      ENDSELECT.
**END-ENHANCEMENT-SECTION.
*    ENDIF.
*  ENDIF.
*
*************************************************************************
** Special stocks at customer side (MSKU)
** MSKU has no flag for deletion
*************************************************************************
*
*  IF  NOT  pa_sond IS INITIAL  AND
*      NOT  t_mat[] IS INITIAL.
*    IF  'V' IN so_sobkz  OR
*        'W' IN so_sobkz.
*      CLEAR collector.
*
**ENHANCEMENT-SECTION EHP604_RM07MLBS_16 SPOTS ES_RM07MLBS .
*      SELECT matnr werks charg sobkz kunnr
*           kulab kuins kuein kuuml                          " OM EHP604
*           INTO (collector-matnr, collector-werks, collector-charg,
*                 collector-sobkz, collector-kunnr,
*                 collector-labst, collector-insme, collector-einme,
*                 collector-umlme)                           " OM EHP604
*           FROM msku
*           FOR ALL ENTRIES IN t_mat
*           WHERE matnr = t_mat-matnr
*             AND werks = t_mat-werks
*             AND charg IN charg
*             AND sobkz  IN  so_sobkz.
*
*        PERFORM              f2000_collect_collector.
*      ENDSELECT.
**END-ENHANCEMENT-SECTION.
*    ENDIF.
*  ENDIF.
*
*************************************************************************
** Special stocks at vendor provision (MSLB)
** MSLB has no flag for deletion
*************************************************************************
*
*  IF  NOT  pa_sond IS INITIAL  AND
*      NOT  t_mat[] IS INITIAL  AND
*           'O'     IN so_sobkz.
*    CLEAR collector.
*
**ENHANCEMENT-SECTION EHP604_RM07MLBS_17 SPOTS ES_RM07MLBS .
*    SELECT matnr werks charg sobkz lifnr
*           lblab lbins lbein lbuml                          " OM EHP604
*           INTO (collector-matnr, collector-werks, collector-charg,
*                 collector-sobkz, collector-lifnr,
*                 collector-labst, collector-insme,
*                 collector-einme, collector-umlme)          " OM EHP604
*           FROM mslb
*           FOR ALL ENTRIES IN t_mat
*           WHERE matnr = t_mat-matnr
*             AND werks = t_mat-werks
*             AND charg IN charg
*             AND sobkz  IN  so_sobkz.
*
*      PERFORM                f2000_collect_collector.
*    ENDSELECT.
**END-ENHANCEMENT-SECTION.
*  ENDIF.
*
*************************************************************************
** Customer order stock (MSKA) and sum segment (MSSA) for valuation.
** Sum on the database and FOR ALL ENTRIES is not allowed from
** release 4.5 onwards, so the summation has to be done
** on the application server (here!).
** MSKA has no flag for deletion
*************************************************************************
*
*  IF  NOT  pa_sond IS INITIAL  AND
*      NOT  t_mat[] IS INITIAL  AND
*           'E'     IN so_sobkz.
*    CLEAR collector.
*
**ENHANCEMENT-SECTION EHP604_RM07MLBS_18 SPOTS ES_RM07MLBS .
*    SELECT mska~matnr mska~werks lgort charg mska~sobkz
*           mska~vbeln mska~posnr
*           kalab kains kaspe kaein kzbws
*           INTO (collector-matnr, collector-werks, collector-lgort,
*                 collector-charg, collector-sobkz,
*                 collector-vbeln, collector-posnr,
*                 collector-labst, collector-insme, collector-speme,
*                 collector-einme, collector-kzbws)
*           FROM mska INNER JOIN mssa
*           ON   mska~matnr = mssa~matnr
*            AND mska~werks = mssa~werks
*            AND mska~sobkz = mssa~sobkz
*            AND mska~vbeln = mssa~vbeln
*            AND mska~posnr = mssa~posnr
*           FOR ALL entries IN t_mat
*           WHERE mska~matnr = t_mat-matnr
*             AND mska~werks = t_mat-werks
*             AND mska~lgort IN lgort
*             AND mska~charg IN charg.
*
*      PERFORM                f2000_collect_collector.
*    ENDSELECT.
**END-ENHANCEMENT-SECTION.
*
**   Transfer stocks for customer order (SATRA in MSSA)
*    CLEAR collector.
**ENHANCEMENT-SECTION EHP604_RM07MLBS_19 SPOTS ES_RM07MLBS .
*    SELECT matnr werks sobkz vbeln posnr kzbws satra
*           INTO (collector-matnr, collector-werks, collector-sobkz,
*                 collector-vbeln, collector-posnr,
*                 collector-kzbws, collector-umlme)
*           FROM mssa
*           FOR ALL ENTRIES IN t_mat
*           WHERE matnr = t_mat-matnr
*             AND werks = t_mat-werks
*             AND sobkz  IN  so_sobkz
*             AND satra <> 0.
*
*      PERFORM                f2000_collect_collector.
*    ENDSELECT.
**END-ENHANCEMENT-SECTION.
*  ENDIF.
*
*************************************************************************
** The same game for project stocks (MSPR/MSSQ).
** MSPR has no flag for deletion
*************************************************************************
*
*  IF  NOT  pa_sond IS INITIAL  AND
*      NOT  t_mat[] IS INITIAL  AND
*           'Q'     IN so_sobkz.
*    CLEAR collector.
*
**ENHANCEMENT-SECTION EHP604_RM07MLBS_20 SPOTS ES_RM07MLBS .
*    SELECT mspr~matnr mspr~werks lgort charg mspr~sobkz mspr~pspnr
*           prlab prins prspe prein kzbws
*           INTO (collector-matnr, collector-werks, collector-lgort,
*                 collector-charg, collector-sobkz,
*                 collector-pspnr,
*                 collector-labst, collector-insme, collector-speme,
*                 collector-einme, collector-kzbws)
*           FROM mspr INNER JOIN mssq
*           ON   mspr~matnr = mssq~matnr
*            AND mspr~werks = mssq~werks
*            AND mspr~sobkz = mssq~sobkz
*            AND mspr~pspnr = mssq~pspnr
*           FOR ALL entries IN t_mat
*           WHERE mspr~matnr = t_mat-matnr
*             AND mspr~werks = t_mat-werks
*             AND mspr~lgort IN lgort
*             AND mspr~charg IN charg.
*
*      PERFORM                f2000_collect_collector.
*    ENDSELECT.
**END-ENHANCEMENT-SECTION.
*
**   Transfer stocks for projects (SQTRA in MSSQ)
*    CLEAR collector.
**ENHANCEMENT-SECTION EHP604_RM07MLBS_21 SPOTS ES_RM07MLBS .
*    SELECT matnr werks sobkz pspnr kzbws sqtra
*           INTO (collector-matnr, collector-werks, collector-sobkz,
*                 collector-pspnr,
*                 collector-kzbws, collector-umlme)
*           FROM mssq
*           FOR ALL ENTRIES IN t_mat
*           WHERE matnr = t_mat-matnr
*             AND werks = t_mat-werks
*             AND sobkz  IN  so_sobkz
*             AND sqtra <> 0.
*
*      PERFORM                f2000_collect_collector.
*    ENDSELECT.
**END-ENHANCEMENT-SECTION.
*  ENDIF.
**ENHANCEMENT-POINT RM07MLBS_02 SPOTS ES_RM07MLBS.
*
*************************************************************************
** Extract key-data for other tables.
*************************************************************************
*  DATA: BEGIN OF t_maktkey OCCURS 0,
*          matnr LIKE makt-matnr,
*        END OF t_maktkey,
*
** working area for the material description
*        BEGIN OF l_s_makt,
*          matnr LIKE makt-matnr,
*          maktx LIKE makt-maktx,
*        END OF l_s_makt,
*
*        BEGIN OF t_makt OCCURS 0,
*          matnr LIKE makt-matnr,
*          maktx LIKE makt-maktx,
*        END OF t_makt,
*
*        BEGIN OF t_mchakey OCCURS 0,
*          matnr LIKE mcha-matnr,
*          werks LIKE mcha-werks,
*          charg LIKE mcha-charg,
*        END OF t_mchakey,
*        BEGIN OF t_mcha OCCURS 0,
*          matnr LIKE mcha-matnr,
*          werks LIKE mcha-werks,
*          charg LIKE mcha-charg,
*          bwtar LIKE mcha-bwtar,
*        END OF t_mcha.
*
*  REFRESH: t_maktkey, t_mchakey, bestand.
** remove all items in bestand with wrong batch number. If we would
** not remove this, report will e.g. show materials which has
** only MARD entries, too.
*  LOOP AT collector WHERE charg IN charg.               "note 311770
*    MOVE-CORRESPONDING collector TO bestand.
*
**   fill the key of the special stocks into the field
**   assigment
**ENHANCEMENT-SECTION     RM07MLBS_03 SPOTS ES_RM07MLBS.
*    CASE    collector-sobkz.
*      WHEN  'E'.
*        MOVE  : 'X'               TO  g_flag_sobkz-vbeln.
*        WRITE : collector-vbeln   TO  bestand-ssnum.
*        MOVE  : '/'               TO  bestand-ssnum+10(01).
*        WRITE : collector-posnr   TO  bestand-ssnum+12(08)
*                                  NO-ZERO.
*        CONDENSE                  bestand-ssnum.
*
*      WHEN  'K'.
*        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
*        WRITE : collector-lifnr   TO  bestand-ssnum.
*
*      WHEN  'M'.
*        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
*        WRITE : collector-lifnr   TO  bestand-ssnum.
*
*      WHEN  'O'.
*        MOVE  : 'X'               TO  g_flag_sobkz-lifnr.
*        WRITE : collector-lifnr   TO  bestand-ssnum.
*
*      WHEN  'Q'.
*        MOVE  : 'X'               TO  g_flag_sobkz-pspnr.
*        WRITE : collector-pspnr   TO  bestand-ssnum.
*
*      WHEN  'V'.
*        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
*        WRITE : collector-kunnr   TO  bestand-ssnum.
*
*      WHEN  'W'.
*        MOVE  : 'X'               TO  g_flag_sobkz-kunnr.
*        WRITE : collector-kunnr   TO  bestand-ssnum.
*
*      WHEN  OTHERS.
*        CLEAR                     bestand-ssnum.
*    ENDCASE.
**END-ENHANCEMENT-SECTION.
*
**ENHANCEMENT-POINT RM07MLBS_12 SPOTS ES_RM07MLBS .
*    APPEND bestand.
*
*    t_maktkey-matnr = bestand-matnr.
*    COLLECT t_maktkey.
*
*    IF bestand-charg <> space.
*      t_mchakey-matnr = bestand-matnr.
*      t_mchakey-werks = bestand-werks.
*      t_mchakey-charg = bestand-charg.
*      COLLECT t_mchakey.
*    ENDIF.
*  ENDLOOP.
*
*  FREE                       collector.
*
*************************************************************************
** Read additional tables
*************************************************************************
*  READ TABLE t_maktkey INDEX 1 TRANSPORTING NO FIELDS.
*  IF sy-subrc = 0.
*    SELECT matnr maktx INTO CORRESPONDING FIELDS OF TABLE t_makt
*           FROM makt
*           FOR ALL ENTRIES IN t_maktkey
*           WHERE matnr = t_maktkey-matnr
*             AND spras = sy-langu.
*    SORT t_makt BY matnr.
*  ENDIF.
*
** Read batch data only if values are requested
*  READ TABLE t_mchakey INDEX 1 TRANSPORTING NO FIELDS.
*  IF sy-subrc = 0 AND novalues IS INITIAL.
*    SELECT matnr werks charg bwtar
*           INTO CORRESPONDING FIELDS OF TABLE t_mcha
*           FROM mcha
*           FOR ALL ENTRIES IN t_mchakey
*           WHERE matnr = t_mchakey-matnr
*             AND werks = t_mchakey-werks
*             AND charg = t_mchakey-charg.
*    SORT t_mcha BY matnr werks charg.
*  ENDIF.
*
*************************************************************************
** Data definitions for the valuation extraction
*************************************************************************
*  DATA: BEGIN OF t_mbewkey OCCURS 0,
*          matnr LIKE mbew-matnr,
*          bwkey LIKE mbew-bwkey,
*          bwtar LIKE mbew-bwtar,
*        END OF t_mbewkey,
*
**       workin table for the material stock valuation
*        BEGIN OF t_mbew OCCURS 0,
*          matnr LIKE mbew-matnr,
*          bwkey LIKE mbew-bwkey,
*          bwtar LIKE mbew-bwtar,
**         consider the valuation of the special stocks E, Q "n531604
*          sobkz              LIKE  ebew-sobkz,              "n531604
*          vbeln              LIKE  ebew-vbeln,              "n531604
*          posnr              LIKE  ebew-posnr,              "n531604
*          pspnr              LIKE  qbew-pspnr,              "n531604
*
*          lbkum(12)  TYPE p DECIMALS 3,                     "407810
*          salk3(12)  TYPE p DECIMALS 2,                     "388735
*          vprsv LIKE mbew-vprsv,                            "353428
*          verpr LIKE mbew-verpr,                            "353428
*          stprs LIKE mbew-stprs,                            "353428
*          peinh LIKE mbew-peinh,                            "353428
*        END OF t_mbew.
*
*  DATA: t_ebewkey LIKE t_mbewkey OCCURS 0 WITH HEADER LINE.
*  DATA: t_qbewkey LIKE t_mbewkey OCCURS 0 WITH HEADER LINE.
*  DATA: t_ebew    LIKE t_mbew    OCCURS 0 WITH HEADER LINE.
*  DATA: t_qbew    LIKE t_mbew    OCCURS 0 WITH HEADER LINE.
*
*  DATA: BEGIN OF t_t134mkey OCCURS 0,
*          bwkey LIKE t134m-bwkey,
*          mtart LIKE t134m-mtart,
*        END OF t_t134mkey,
*        BEGIN OF t_t134m OCCURS 0,
*          bwkey LIKE t134m-bwkey,
*          mtart LIKE t134m-mtart,
*          wertu LIKE t134m-wertu,
*        END OF t_t134m.
*
*************************************************************************
** Fill in additional data (first round) and extract the data
** for the access to the valuation tables.
*************************************************************************
*  SORT t_mat BY matnr werks.
*  SORT g_t_organ             BY werks.
*  CLEAR : g_s_t001l,         g_s_organ.
*
*  LOOP AT bestand.
**   get the information per plant and storage location
**   with buffer
*    IF  g_flag_t001l = 'X'.
*      IF  bestand-werks = g_s_t001l-werks  AND
*          bestand-lgort = g_s_t001l-lgort.
*      ELSE.
**       read with plant and storage location
*        READ TABLE g_t_t001l   INTO  g_s_t001l
*          WITH TABLE KEY werks = bestand-werks
*                         lgort = bestand-lgort.
*
*        MOVE : bestand-werks TO g_s_t001l-werks,
*               bestand-lgort TO g_s_t001l-lgort.
*
*        IF sy-subrc <> 0.
*          CLEAR              g_s_t001l-lgobe.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
**   take the storage bin from the buffer
*    MOVE : g_s_t001l-lgobe   TO  bestand-lgobe.
*
*
**   get the information per plant with buffer
*    IF  bestand-werks  NE  g_s_organ-werks.
*      READ TABLE g_t_organ   INTO  g_s_organ
*          WITH KEY werks = bestand-werks
*                             BINARY SEARCH.
*
*      IF sy-subrc <> 0.
**       sorry nothing found
*        CLEAR                g_s_organ.
*        MOVE : bestand-werks TO  g_s_organ-werks.
*      ENDIF.
*    ENDIF.
*
**   take the following fields from the buffer
*    MOVE : g_s_organ-name1   TO  bestand-name1,
*           g_s_organ-waers   TO  bestand-waers,
*           g_s_organ-bwkey   TO  bestand-bwkey.
*
*
**   get the information from the material master MARC
**   with buffer
*    IF  bestand-matnr = l_s_mat-matnr  AND
*        bestand-werks = l_s_mat-werks.
**     results are in the buffer
*    ELSE.
*      CLEAR                  l_s_mat.
*      MOVE : bestand-matnr   TO  l_s_mat-matnr,
*             bestand-werks   TO  l_s_mat-werks.
*
*      READ TABLE t_mat       INTO  l_s_mat
*         WITH KEY matnr = bestand-matnr
*                  werks = bestand-werks
*                             BINARY SEARCH.
*
*      IF  sy-subrc <> 0.
**       sorry nothing found
*        CLEAR                l_s_mat.
*        MOVE : bestand-matnr TO  l_s_mat-matnr,
*               bestand-werks TO  l_s_mat-werks.
*      ENDIF.
*    ENDIF.
*
**   take the results the buffer
*    MOVE : l_s_mat-mtart     TO  bestand-mtart,
*           l_s_mat-matkl     TO  bestand-matkl,
*           l_s_mat-meins     TO  bestand-meins,
*           l_s_mat-extwg     TO  bestand-extwg.
*
*
**   if this entry has no deletion flag, take the
**   deletion flag from a higher level like MARA, MARC,
**   or MARDA
*    IF  bestand-lvorm IS INITIAL.
*      IF      NOT  l_s_mat-lvorm_marc IS INITIAL.
*        MOVE  l_s_mat-lvorm_marc
*                             TO  bestand-lvorm.
*      ELSEIF  NOT  l_s_mat-lvorm_mara IS INITIAL.
*        MOVE  l_s_mat-lvorm_mara
*                             TO  bestand-lvorm.
*
*      ELSEIF  NOT g_t_mard_lv[] IS INITIAL  AND
*              NOT bestand-lgort IS INITIAL  AND
*              NOT bestand-sobkz IS INITIAL.
**       look for deletion flag in working table
**       g_t_mard_lv for a line with special stock
*        IF  bestand-matnr = g_s_mard_lv-matnr  AND
*            bestand-werks = g_s_mard_lv-werks  AND
*            bestand-lgort = g_s_mard_lv-lgort.
*        ELSE.
**         read table only after the key has changed
*          READ TABLE g_t_mard_lv  INTO  g_s_mard_lv
*          WITH TABLE KEY matnr = bestand-matnr
*                         werks = bestand-werks
*                         lgort = bestand-lgort.
*
*          IF sy-subrc <> 0.
**           fill the buffer in case the entry does not exist
*            MOVE : bestand-matnr  TO  g_s_mard_lv-matnr,
*                   bestand-werks  TO  g_s_mard_lv-werks,
*                   bestand-lgort  TO  g_s_mard_lv-lgort.
*            CLEAR                 g_s_mard_lv-lvorm.
*          ENDIF.
*
**         take the result from the buffer
*          MOVE  g_s_mard_lv-lvorm   TO  bestand-lvorm.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*
**   read the material short description after the material
**   number has changed
*    IF  bestand-matnr NE l_s_makt-matnr.
*      READ TABLE t_makt      INTO  l_s_makt
*                   WITH KEY matnr = bestand-matnr
*                             BINARY SEARCH.
*
*      IF sy-subrc <> 0.
**       sorry nothing found
*        CLEAR                l_s_makt-maktx.
*        MOVE  bestand-matnr  TO  l_s_makt-matnr.
*      ENDIF.
*    ENDIF.
*
**   take the results the buffer
*    MOVE : l_s_makt-maktx    TO  bestand-maktx.
*
** added dynamic break-point ID MMIM_REP_MB52              "n912093
*    BREAK-POINT ID mmim_rep_mb52.                           "n912093
*
*    IF bestand-charg <> space AND novalues IS INITIAL.
*      READ TABLE t_mcha WITH KEY matnr = bestand-matnr
*                                 werks = bestand-werks
*                                 charg = bestand-charg
*                                 BINARY SEARCH.
*      IF sy-subrc = 0.
*        bestand-bwtar = t_mcha-bwtar.
*      ENDIF.
*    ENDIF.
*    MODIFY bestand.
** Valuation keys
*    IF novalues IS INITIAL.
*      IF  bestand-sobkz = ' ' OR bestand-sobkz = 'O' OR
*          bestand-sobkz = 'W' OR bestand-sobkz = 'V' OR
*          bestand-kzbws = 'A'.
*        t_mbewkey-matnr = bestand-matnr.
*        t_mbewkey-bwkey = bestand-bwkey.
*        t_mbewkey-bwtar = bestand-bwtar.
*        COLLECT t_mbewkey.
*      ELSEIF bestand-sobkz = 'E' AND bestand-kzbws = 'M'.
*        t_ebewkey-matnr = bestand-matnr.
*        t_ebewkey-bwkey = bestand-bwkey.
*        t_ebewkey-bwtar = bestand-bwtar.
*        COLLECT t_ebewkey.
*      ELSEIF bestand-sobkz = 'Q' AND bestand-kzbws = 'M'.
*        t_qbewkey-matnr = bestand-matnr.
*        t_qbewkey-bwkey = bestand-bwkey.
*        t_qbewkey-bwtar = bestand-bwtar.
*        COLLECT t_qbewkey.
*      ENDIF.
*      t_t134mkey-bwkey = bestand-bwkey.
*      t_t134mkey-mtart = bestand-mtart.
*      COLLECT t_t134mkey.
*    ENDIF.                             " novalues is initial
*  ENDLOOP.
*
** release the space of global working tables after use
*  FREE : g_t_mard_lv, g_t_t001l, g_t_organ.
*
*************************************************************************
** Read the valuation tables
*************************************************************************
*  IF novalues IS INITIAL.
*    READ TABLE t_mbewkey INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      SELECT matnr bwkey bwtar lbkum salk3
*             vprsv verpr stprs peinh                        "353428
*             INTO CORRESPONDING FIELDS OF TABLE t_mbew
*             FROM mbew
*             FOR ALL ENTRIES IN t_mbewkey
*             WHERE matnr = t_mbewkey-matnr
*               AND bwkey = t_mbewkey-bwkey
*               AND bwtar = t_mbewkey-bwtar.
*      SORT t_mbew BY matnr bwkey bwtar.
*    ENDIF.
*
*    READ TABLE t_ebewkey INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
**     "Unfortunately", EBEW and QBEW do not have sum segments over
**     the valuation types. Therefore, without batch data, another
**     SELECT-statement is needed.
*      IF xmchb = 'X'.
*        SELECT matnr bwkey bwtar sobkz vbeln posnr lbkum salk3
*               vprsv verpr stprs peinh
*               INTO (t_ebew-matnr, t_ebew-bwkey, t_ebew-bwtar,
*                     t_ebew-sobkz,  t_ebew-vbeln, t_ebew-posnr,
*                     t_ebew-lbkum, t_ebew-salk3,
*                     t_ebew-vprsv, t_ebew-verpr,
*                     t_ebew-stprs, t_ebew-peinh)
*               FROM ebew
*               FOR ALL ENTRIES IN t_ebewkey
*               WHERE matnr = t_ebewkey-matnr
*                 AND bwkey = t_ebewkey-bwkey
*                 AND bwtar = t_ebewkey-bwtar.
*          COLLECT t_ebew.
*        ENDSELECT.
*      ELSE.
*        SELECT matnr bwkey bwtar sobkz vbeln posnr lbkum salk3
*               vprsv verpr stprs peinh
*               INTO (t_ebew-matnr, t_ebew-bwkey,  t_ebew-bwtar,
*                     t_ebew-sobkz,  t_ebew-vbeln, t_ebew-posnr,
*                     t_ebew-lbkum, t_ebew-salk3,
*                     t_ebew-vprsv, t_ebew-verpr,
*                     t_ebew-stprs, t_ebew-peinh)
*               FROM ebew
*               FOR ALL ENTRIES IN t_ebewkey
*               WHERE matnr = t_ebewkey-matnr
*                 AND bwkey = t_ebewkey-bwkey.
*          COLLECT t_ebew.
*        ENDSELECT.
*      ENDIF.
*
*      SORT t_ebew BY matnr bwkey bwtar sobkz vbeln posnr.
*    ENDIF.
*
*    READ TABLE t_qbewkey INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      IF xmchb = 'X'.
*        SELECT matnr bwkey bwtar sobkz pspnr lbkum salk3
*               vprsv verpr stprs peinh
*               INTO (t_qbew-matnr, t_qbew-bwkey, t_qbew-bwtar,
*                     t_qbew-sobkz, t_qbew-pspnr,
*                     t_qbew-lbkum, t_qbew-salk3,
*                     t_qbew-vprsv, t_qbew-verpr,
*                     t_qbew-stprs, t_qbew-peinh)
*               FROM qbew
*               FOR ALL ENTRIES IN t_qbewkey
*               WHERE matnr = t_qbewkey-matnr
*                 AND bwkey = t_qbewkey-bwkey
*                AND bwtar = t_qbewkey-bwtar.
*          COLLECT t_qbew.
*        ENDSELECT.
*      ELSE.
*        SELECT matnr bwkey bwtar sobkz pspnr lbkum salk3
*               vprsv verpr stprs peinh
*               INTO (t_qbew-matnr, t_qbew-bwkey, t_qbew-bwtar,
*                     t_qbew-sobkz, t_qbew-pspnr,
*                     t_qbew-lbkum, t_qbew-salk3,
*                     t_qbew-vprsv, t_qbew-verpr,
*                     t_qbew-stprs, t_qbew-peinh)
*               FROM qbew
*               FOR ALL ENTRIES IN t_qbewkey
*               WHERE matnr = t_qbewkey-matnr
*                 AND bwkey = t_qbewkey-bwkey.
*          COLLECT t_qbew.
*        ENDSELECT.
*      ENDIF.
*
*      SORT t_qbew BY matnr bwkey bwtar sobkz pspnr.
*    ENDIF.
*
*    READ TABLE t_t134mkey INDEX 1 TRANSPORTING NO FIELDS.
*    IF sy-subrc = 0.
*      SELECT bwkey mtart wertu
*             INTO CORRESPONDING FIELDS OF TABLE t_t134m
*             FROM t134m
*             FOR ALL ENTRIES IN t_t134mkey
*             WHERE bwkey = t_t134mkey-bwkey
*               AND mtart = t_t134mkey-mtart.
*      SORT t_t134m BY bwkey mtart.
*    ENDIF.
*************************************************************************
** Fill the valuation data
*************************************************************************
*    DATA: factor TYPE f.
*    LOOP AT bestand.
*      CHECK bestand-waers <> space.  "Do nothing for failed Auth-Checks
*      READ TABLE t_t134m WITH KEY bwkey = bestand-bwkey
*                                  mtart = bestand-mtart
*                                  BINARY SEARCH.
*      CHECK sy-subrc = 0 AND t_t134m-wertu = 'X'.
**     Set SY-SUBRC = 4. A successful table read resets it an starts
**     the value filling.
*      sy-subrc = 4.
*      IF  bestand-sobkz = ' ' OR bestand-sobkz = 'O' OR
*          bestand-sobkz = 'W' OR bestand-sobkz = 'V' OR
*          bestand-kzbws = 'A'.
*        READ TABLE t_mbew WITH KEY matnr = bestand-matnr
*                                   bwkey = bestand-bwkey
*                                   bwtar = bestand-bwtar
*                                   BINARY SEARCH.
*
*      ELSEIF bestand-sobkz = 'E' AND bestand-kzbws = 'M'.
*        READ TABLE t_ebew WITH KEY matnr = bestand-matnr
*                                   bwkey = bestand-bwkey
*                                   bwtar = bestand-bwtar
*                                   sobkz = bestand-sobkz
*                                   vbeln = bestand-vbeln    "n531604
*                                   posnr = bestand-posnr    "n531604
*                                   BINARY SEARCH.
*        MOVE-CORRESPONDING t_ebew TO t_mbew.
*
*      ELSEIF bestand-sobkz = 'Q' AND bestand-kzbws = 'M'.
*        READ TABLE t_qbew WITH KEY matnr = bestand-matnr
*                                   bwkey = bestand-bwkey
*                                   bwtar = bestand-bwtar
*                                   sobkz = bestand-sobkz
*                                   pspnr = bestand-pspnr
*                                   BINARY SEARCH.
*        MOVE-CORRESPONDING t_qbew TO t_mbew.
*      ENDIF.
*
*      IF sy-subrc = 0.
*        IF t_mbew-lbkum = 0.
**         Cannot happen, but in R/3 this does not hold in all cases...
*          IF t_mbew-peinh = 0.                              "353428
*            t_mbew-peinh = 1.                               "353428
*          ENDIF.                                            "353428
**         Calculation of value in case of LBKUM = 0 only possible
**         for MBEW. EBEW and QBEW are collected over all subitems
**         (VBELN...), so the data are not available.
*          IF bestand-sobkz = 'E' OR bestand-sobkz = 'Q'.    "388735
*            factor = 0.                                     "388735
*            CLEAR bestand-waers.                            "388735
*          ELSE.                                             "388735
*            CASE t_mbew-vprsv.
*              WHEN 'V'. factor = t_mbew-verpr / t_mbew-peinh.
*              WHEN 'S'. factor = t_mbew-stprs / t_mbew-peinh.
*            ENDCASE.
*          ENDIF.                                            "388735
*        ELSE.
*          factor = t_mbew-salk3 / t_mbew-lbkum.
*        ENDIF.
*
*        bestand-wlabs = bestand-labst * factor.
*        bestand-winsm = bestand-insme * factor.
*        bestand-wspem = bestand-speme * factor.
*        bestand-weinm = bestand-einme * factor.
*        bestand-wumlm = bestand-umlme * factor.
*        bestand-wbwesb = bestand-bwesb * factor.            "AC0K020254
*        bestand-wglgm = bestand-glgmg * factor.             "n912093
*        bestand-wtram = bestand-trame * factor.             "n912093
*        bestand-wumlc = bestand-umlmc * factor.             "n912093
**ENHANCEMENT-POINT EHP604_RM07MLBS_22 SPOTS ES_RM07MLBS .
*        MODIFY bestand.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.                               "novalues is initial
**ENHANCEMENT-POINT EHP604_RM07MLBS_23 SPOTS ES_RM07MLBS .
*
**START  CODE BY PUNAM
*  LOOP AT bestand.
*    SELECT SINGLE hsdat  " Date of Manufacture
*           vfdat "Shelf Life Exp. Date
*           cuobj_bm
*      FROM mch1 INTO (bestand-hsdat , bestand-vfdat , bestand-cuobj_bm )
*      WHERE matnr = bestand-matnr
*      AND charg = bestand-charg.
*
**    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
**      EXPORTING
**        INPUT        = bestand-CUOBJ_BM
**     IMPORTING
**       OUTPUT        = bestand-CUOBJ_BM.
*
*    SELECT SINGLE a~spart vtext
*      FROM mara AS a
*      JOIN tspat AS b
*      ON a~spart = b~spart
*      INTO (bestand-spart , bestand-vtext)
*      WHERE matnr =  bestand-matnr
*      AND spras = 'EN'.
*
**{   REPLACE        SBXK900030                                        1
**\    SELECT SINGLE atwrt FROM ausp
**\    INTO bestand-atwrt
**\    WHERE objek = bestand-cuobj_bm
**\    AND atinn = '0000000810'. "'0000000837'."
*
*    lv_objek = bestand-cuobj_bm.
*
*    SELECT SINGLE atwrt
*             FROM ausp
*             INTO bestand-atwrt
*            WHERE objek EQ lv_objek
*              AND atinn EQ '0000000810'.
*
**}   REPLACE
*
*    IF  bestand-vfdat IS NOT INITIAL.
*      bestand-sl_days = bestand-vfdat - sy-datum.
*    ENDIF.
**If<=90 ,then-"Near Expiry"
**IF<=0,"Expired"
**If>90<180,then"90 to 180 days"
**If>180<360,then"180 to 360 days expiry"
**If>360,then "More than 1 year"
*
*    IF bestand-vfdat IS NOT INITIAL.
*      IF bestand-sl_days <= 90 AND bestand-sl_days > 0.
*        bestand-status = 'Near Expiry'.
*      ELSEIF bestand-sl_days <= 0.
*        bestand-status = 'Expired'.
*      ELSEIF bestand-sl_days > 90 AND bestand-sl_days <= 180.
*        bestand-status = '90 to 180 days'.
*      ELSEIF bestand-sl_days > 180 AND bestand-sl_days <= 360.
*        bestand-status = '180 to 360 days expiry'.
*      ELSEIF bestand-sl_days > 360.
*        bestand-status = 'More than 1 year'.
*      ENDIF.
*    ELSE.
*      bestand-status = 'Not Applicable'.
*    ENDIF.
*
*    SELECT SINGLE cityc FROM t001w INTO bestand-cityc WHERE werks = bestand-werks
*      AND cityc IN cityc.
*
*    IF sy-subrc <> 0 . CLEAR  bestand-cityc. ENDIF.
*
*    IF  bestand-cityc IS NOT INITIAL.
*      SELECT SINGLE bezei FROM t005h INTO bestand-bezei WHERE spras = 'EN'
*        AND land1 = 'IN' AND cityc = bestand-cityc.
*    ENDIF.
*
*    MODIFY  bestand.
*  ENDLOOP.
**end CODE BY PUNAM
*
*  IF cityc IS NOT INITIAL.
*    IF bestand[] IS NOT INITIAL.
*      DELETE bestand[] WHERE cityc NOT IN cityc.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    "data_selection
*
*************************************************************************
** Build fieldcatalog for list viewer
*************************************************************************
*FORM fieldcatalog.
** HIRARCHICAL REPRESENTATION
** Header fields
*  CLEAR fieldcat.
**ENHANCEMENT-SECTION     RM07MLBS_08 SPOTS ES_RM07MLBS.
*  fieldcat-fieldname     = 'MATNR'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'MARA'.
**END-ENHANCEMENT-SECTION.
*  APPEND fieldcat.
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'MAKTX'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'MAKT'.
*  APPEND fieldcat.
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'WERKS'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'T001W'.
*  APPEND fieldcat.
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'NAME1'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'T001W'.
*  APPEND fieldcat.
**ENHANCEMENT-POINT RM07MLBS_1 SPOTS ES_RM07MLBS .
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'MTART'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'MARA'.
*  fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'MATKL'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'MARA'.
*  fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*  fieldcat-fieldname     = 'EXTWG'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'MARA'.
*  fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*  "PUNAM
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'SPART'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'MARA'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'VTEXT'.
*  fieldcat-tabname       = 'HEADER'.
*  fieldcat-ref_tabname   = 'TSPAT'.
*  APPEND fieldcat.
*  "PUNAM
*
** List body
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'LGORT'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARD'.
*  fieldcat-outputlen     = 5.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'LGOBE'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'T001L'.
*  fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*  IF NOT pa_sond IS INITIAL.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'SOBKZ'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MKOL'.
*    APPEND fieldcat.
*
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'KZBWS'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MSSA'.
*    fieldcat-outputlen     = 1.
*    APPEND fieldcat.
*
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'SSNUM'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'BICKEY'.
*    APPEND fieldcat.
*  ENDIF.
*
*  IF  xmchb = 'X'.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'CHARG'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MCHB'.
*    APPEND fieldcat.
*  ENDIF.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'LVORM'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARD'.
*  fieldcat-outputlen     = 3.
*  APPEND fieldcat.
*
** Quantities
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'LABST'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARD'.
*  fieldcat-qfieldname    = 'MEINS'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'MEINS'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARA'.
*  fieldcat-outputlen     = '5'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.                                           "n912093
*  fieldcat-fieldname     = 'UMLME'.                         "n912093
*  fieldcat-tabname       = 'BESTAND'.                       "n912093
*  fieldcat-ref_tabname   = 'AM07M'.                         "n912093
*  fieldcat-ref_fieldname = 'MB52_TRAUML'.                   "n912093
*  fieldcat-qfieldname    = 'MEINS'.                         "n912093
*  APPEND fieldcat.                                          "n912093
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'INSME'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARD'.
*  fieldcat-qfieldname    = 'MEINS'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'EINME'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARD'.
*  fieldcat-qfieldname    = 'MEINS'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'SPEME'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARD'.
*  fieldcat-qfieldname    = 'MEINS'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'RETME'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MARD'.
*  fieldcat-qfieldname    = 'MEINS'.
*  APPEND fieldcat.
*
** process MARC-BWESB as hidden field                     "AC0K020254
*  CLEAR fieldcat.                                           "AC0K020254
*  fieldcat-fieldname     = 'BWESB'.                         "AC0K020254
*  fieldcat-tabname       = 'BESTAND'.                       "AC0K020254
*  fieldcat-ref_tabname   = 'MARC'.                          "AC0K020254
*  fieldcat-qfieldname    = 'MEINS'.                         "AC0K020254
*  fieldcat-no_out        = 'X'.                             "AC0K020254
*  APPEND fieldcat.                                          "AC0K020254
*
** tied empties stock                                     "n912093
*  fieldcat-fieldname     = 'GLGMG'.                         "n912093
*  fieldcat-tabname       = 'BESTAND'.                       "n912093
*  fieldcat-ref_tabname   = 'MARC'.                          "n912093
*  fieldcat-qfieldname    = 'MEINS'.                         "n912093
*  fieldcat-no_out      = 'X'.                               "n912093
*  APPEND fieldcat.                                          "n912093
*  CLEAR fieldcat.                                           "n912093
*
** stock in transit                                       "n912093
*  fieldcat-fieldname     = 'TRAME'.                         "n912093
*  fieldcat-tabname       = 'BESTAND'.                       "n912093
*  fieldcat-ref_tabname   = 'MARC'.                          "n912093
*  fieldcat-qfieldname    = 'MEINS'.                         "n912093
*  fieldcat-no_out      = 'X'.                               "n912093
*  APPEND fieldcat.                                          "n912093
*  CLEAR fieldcat.                                           "n912093
*
** stock in uml                                           "n912093
*  fieldcat-fieldname     = 'UMLMC'.                         "n912093
*  fieldcat-tabname       = 'BESTAND'.                       "n912093
*  fieldcat-ref_tabname   = 'MARC'.                          "n912093
*  fieldcat-qfieldname    = 'MEINS'.                         "n912093
*  fieldcat-no_out      = 'X'.                               "n912093
*  APPEND fieldcat.                                          "n912093
*  CLEAR fieldcat.                                           "n912093
*
*
***START PUNAM
** CLEAR fieldcat.
**    fieldcat-fieldname     = 'HSDAT'.
**    fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
**    fieldcat-no_out        = 'X'.
** APPEND fieldcat.
**
**  CLEAR fieldcat.
**    fieldcat-fieldname     = 'VFDAT'.
**    fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
**    fieldcat-no_out        = 'X'.
** APPEND fieldcat.
**
**  CLEAR fieldcat.
**    fieldcat-fieldname     = 'ATWRT'.
**    fieldcat-tabname       = 'BESTAND'.
***    fieldcat-ref_tabname   = 'AUSP'.
**    fieldcat-seltext_M     = 'Legacy batch No'.
**    fieldcat-no_out        = 'X'.
** APPEND fieldcat.
*** END PUNAM
*
*
*
*
** set the key fields of the special stock as hidden fields
*  IF  g_flag_sobkz-lifnr = 'X'.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'LIFNR'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MKOL'.
*    fieldcat-no_out        = 'X'.
*    APPEND fieldcat.
*  ENDIF.
*
*  IF  g_flag_sobkz-kunnr = 'X'.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'KUNNR'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MSKU'.
*    fieldcat-no_out        = 'X'.
*    APPEND fieldcat.
*  ENDIF.
*
*  IF  g_flag_sobkz-vbeln = 'X'.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'VBELN'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MSKA'.
*    fieldcat-no_out        = 'X'.
*    APPEND fieldcat.
*
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'POSNR'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MSKA'.
*    fieldcat-no_out        = 'X'.
*    APPEND fieldcat.
*  ENDIF.
*
*  IF  g_flag_sobkz-pspnr = 'X'.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'PSPNR'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_tabname   = 'MSPR'.
*    fieldcat-no_out        = 'X'.
*    APPEND fieldcat.
*  ENDIF.
*
** here starts the second row
** Values
*  IF novalues IS INITIAL.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'DUMMY'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-outputlen     = 9.
*
**   calculate the length of the dummy field
*    IF xmchb = 'X'.
*      ADD 11 TO fieldcat-outputlen.
*    ENDIF.
*
*    IF NOT pa_sond IS INITIAL.
*      ADD 29 TO fieldcat-outputlen.
*    ENDIF.
*
*    APPEND fieldcat.
*
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'WLABS'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-ref_tabname   = 'MBEW'.
*    fieldcat-cfieldname    = 'WAERS'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-seltext_s     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-do_sum        = 'X'.
*    APPEND fieldcat.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'WAERS'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_fieldname = 'WAERS'.
*    fieldcat-ref_tabname   = 'T001'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-outputlen     = 5.
*    APPEND fieldcat.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'WUMLM'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-ref_tabname   = 'MBEW'.
*    fieldcat-cfieldname    = 'WAERS'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-seltext_s     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-outputlen     = '18'.
*    fieldcat-do_sum        = 'X'.
*    APPEND fieldcat.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'WINSM'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-ref_tabname   = 'MBEW'.
*    fieldcat-cfieldname    = 'WAERS'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-seltext_s     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-outputlen     = '18'.
*    fieldcat-do_sum        = 'X'.
*    APPEND fieldcat.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'WEINM'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-ref_tabname   = 'MBEW'.
*    fieldcat-cfieldname    = 'WAERS'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-seltext_s     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-outputlen     = '18'.
*    fieldcat-do_sum        = 'X'.
*    APPEND fieldcat.
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'WSPEM'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-ref_tabname   = 'MBEW'.
*    fieldcat-cfieldname    = 'WAERS'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-seltext_s     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-outputlen     = '18'.
*    fieldcat-do_sum        = 'X'.
*    APPEND fieldcat.
*
*    CLEAR fieldcat.
*    fieldcat-fieldname     = 'WRETM'.
*    fieldcat-tabname       = 'BESTAND'.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-ref_tabname   = 'MBEW'.
*    fieldcat-cfieldname    = 'WAERS'.
*    fieldcat-row_pos       = '2'.
*    fieldcat-seltext_s     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-seltext_m     = text-020.
*    fieldcat-outputlen     = '18'.
*    fieldcat-do_sum        = 'X'.
*    APPEND fieldcat.
*
**  process estimated value MARC-BWESB as hidden field    "AC0K020254
*    CLEAR fieldcat.                                         "AC0K020254
*    fieldcat-fieldname     = 'WBWESB'.                      "AC0K020254
*    fieldcat-tabname       = 'BESTAND'.                     "AC0K020254
*    fieldcat-ref_tabname   = 'MBEW'.                        "AC0K020254
*    fieldcat-ref_fieldname = 'SALK3'.                       "AC0K020254
*    fieldcat-cfieldname    = 'WAERS'.                       "AC0K020254
*    fieldcat-row_pos       = '2'.                           "AC0K020254
*    fieldcat-seltext_s     = text-020.                      "AC0K020254
*    fieldcat-seltext_m     = text-020.                      "AC0K020254
*    fieldcat-seltext_m     = text-020.                      "AC0K020254
*    fieldcat-outputlen     = '18'.                          "AC0K020254
*    fieldcat-do_sum        = 'X'.                           "AC0K020254
*    fieldcat-no_out        = 'X'.                           "AC0K020254
*    APPEND fieldcat.                                        "AC0K020254
*
**   value for tied empties                               "n912093
*    fieldcat-no_out      = 'X'.                             "n912093
*    fieldcat-fieldname     = 'WGLGM'.                       "n912093
*    fieldcat-tabname       = 'BESTAND'.                     "n912093
*    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
*    fieldcat-ref_tabname   = 'MBEW'.                        "n912093
*    fieldcat-cfieldname    = 'WAERS'.                       "n912093
*    fieldcat-row_pos       = '2'.                           "n912093
*    fieldcat-seltext_s     = text-020.                      "n912093
*    fieldcat-seltext_m     = text-020.                      "n912093
*    fieldcat-seltext_m     = text-020.                      "n912093
*    fieldcat-outputlen     = '18'.                          "n912093
*    fieldcat-do_sum        = 'X'.                           "n912093
*    APPEND fieldcat.                                        "n912093
*
**   value for transit                                    "n912093
*    fieldcat-no_out      = 'X'.                             "n912093
*    fieldcat-fieldname     = 'WTRAM'.                       "n912093
*    fieldcat-tabname       = 'BESTAND'.                     "n912093
*    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
*    fieldcat-ref_tabname   = 'MBEW'.                        "n912093
*    fieldcat-cfieldname    = 'WAERS'.                       "n912093
*    fieldcat-row_pos       = '2'.                           "n912093
*    fieldcat-seltext_s     = text-020.                      "n912093
*    fieldcat-seltext_m     = text-020.                      "n912093
*    fieldcat-seltext_m     = text-020.                      "n912093
*    fieldcat-outputlen     = '18'.                          "n912093
*    fieldcat-do_sum        = 'X'.                           "n912093
*    APPEND fieldcat.
*
**   value for uml at plant                               "n912093
*    fieldcat-no_out      = 'X'.                             "n912093
*    fieldcat-fieldname     = 'WUMLC'.                       "n912093
*    fieldcat-tabname       = 'BESTAND'.                     "n912093
*    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
*    fieldcat-ref_tabname   = 'MBEW'.                        "n912093
*    fieldcat-cfieldname    = 'WAERS'.                       "n912093
*    fieldcat-row_pos       = '2'.                           "n912093
*    fieldcat-seltext_s     = text-020.                      "n912093
*    fieldcat-seltext_m     = text-020.                      "n912093
*    fieldcat-seltext_m     = text-020.                      "n912093
*    fieldcat-outputlen     = '18'.                          "n912093
*    fieldcat-do_sum        = 'X'.                           "n912093
*    APPEND fieldcat.
*
*  ENDIF.                               "novalues is initial
*
**START PUNAM
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'HSDAT'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MCH1'.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'VFDAT'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-ref_tabname   = 'MCH1'.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
****** Start Code: Added by CS on 31.12.2015 for Loc Received Date, Last movement Date. *****
* CLEAR fieldcat.
*  fieldcat-fieldname     = 'ERSDA'.   "
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-seltext_m     = 'Loc Received Date'.
*  fieldcat-ref_tabname   = 'MCHB'.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'LAEDA'.
*  fieldcat-tabname       = 'BESTAND'.
*  fieldcat-seltext_m     = 'Last movement Date'.
*  fieldcat-ref_tabname   = 'MCHB'.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
****** End Code: Added by CS on 31.12.2015 for Loc Received Date, Last movement Date. *****
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'ATWRT'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'AUSP'.
*  fieldcat-seltext_m     = 'Legacy batch No'.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'SL_DAYS'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'AUSP'.
*  fieldcat-seltext_m     = 'Shelf Life-Days'.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'STATUS'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'AUSP'.
*  fieldcat-seltext_m     = 'STATUS'.
*  fieldcat-outputlen     = 22.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'CITYC'.
*  fieldcat-tabname       = 'BESTAND'.
**  fieldcat-ref_tabname   = 'T001W'.
*  fieldcat-seltext_m     = 'Sales Dist.'.
*  APPEND fieldcat.
*
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'BEZEI'.
*  fieldcat-tabname       = 'BESTAND'.
**  fieldcat-ref_tabname   = 'T001W'.
*  fieldcat-seltext_m     = 'Sales Dist.Text'.
*  APPEND fieldcat.
*
*
** END PUNAM
*
*
**ENHANCEMENT-POINT EHP604_RM07MLBS_24 SPOTS ES_RM07MLBS .
*
*ENDFORM.                               " FELDKATALOG_AUFBAUEN
*
*************************************************************************
** Show the result list
*************************************************************************
*FORM list_output.
*
**  set pf-status 'STANDARD'.
*
**ENHANCEMENT-SECTION     RM07MLBS_09 SPOTS ES_RM07MLBS.
*  keyinfo-header01 = 'MATNR'.
*  keyinfo-header02 = 'WERKS'.
*  keyinfo-item01   = 'MATNR'.
*  keyinfo-item02   = 'WERKS'.
*  keyinfo-item03   = 'LGORT'.
*
** new sort order
*  SORT bestand BY matnr werks lgort
*                  sobkz kzbws
*                  lifnr kunnr vbeln  posnr  pspnr charg.
**END-ENHANCEMENT-SECTION.
*
*
*  REFRESH : sort.
*  CLEAR   : sort, g_cnt_spos.
*
** create the sort table for the ALV depending on the
** list type
*  IF      NOT pa_hsq IS INITIAL.
**   for hierarchic seq. list
*
*  ELSEIF  NOT  pa_flt IS INITIAL.
**   for flat ( simple ) list
**ENHANCEMENT-SECTION     RM07MLBS_10 SPOTS ES_RM07MLBS.
*    PERFORM  f0400_create_sort    USING  'MATNR'.
**END-ENHANCEMENT-SECTION.
*    PERFORM  f0400_create_sort    USING  'WERKS'.
*  ENDIF.
*
*  PERFORM  f0400_create_sort      USING  'LGORT'.
*  PERFORM  f0400_create_sort      USING  'SOBKZ'.
*  PERFORM  f0400_create_sort      USING  'KZBWS'.
*  PERFORM  f0400_create_sort      USING  'LIFNR'.
*  PERFORM  f0400_create_sort      USING  'KUNNR'.
*
*  PERFORM  f0400_create_sort      USING  'VBELN'.
*  PERFORM  f0400_create_sort      USING  'POSNR'.
*  PERFORM  f0400_create_sort      USING  'PSPNR'.
*
*
*  DEFINE colourize.
*    clear color.
*    color-fieldname = &1.
*    color-color-int = '0'.
*    if &2 > 0.
*      color-color-col = '5'.
*    elseif &2 < 0.
*      color-color-col = '6'.
*    endif.
*    append color.
*    case &1.
*      when 'LABST'.
*        color-fieldname = 'MEINS'.
*        append color.
*      when 'WLABS'.
*        color-fieldname = 'WAERS'.
*        append color.
*    endcase.
*  END-OF-DEFINITION.
**ENHANCEMENT-POINT EHP604_RM07MLBS_26 SPOTS ES_RM07MLBS .
*
** skip this loop when the user wants a flat list without
** colors
*  IF  NOT pa_hsq     IS INITIAL  OR
*      NOT alv_color  =  'X'.
*    LOOP AT bestand.
*      IF  NOT pa_hsq IS INITIAL.                            "n531604
**       create working table header only if a hierarchic    "n531604
**       list is required                                    "n531604
*        ON CHANGE OF bestand-matnr OR bestand-werks.
*          MOVE-CORRESPONDING bestand TO header.
**ENHANCEMENT-POINT RM07MLBS_2 SPOTS ES_RM07MLBS .
*          APPEND header.
*        ENDON.
*      ENDIF.                                                "n531604
*
**     create the table with the colour information          "n531604
**     depending on the customizing settings in table        "n531604
**     V_MMIM_REP_PRINT   'X' = no colors                    "n531604
*      IF  alv_color NE 'X'.                                 "n531604
*        REFRESH color.
*        colourize 'LABST' bestand-labst.
*        colourize 'UMLME' bestand-umlme.
*        colourize 'EINME' bestand-einme.
*        colourize 'SPEME' bestand-speme.
*        colourize 'RETME' bestand-retme.
*        colourize 'INSME' bestand-insme.
*        colourize 'WLABS' bestand-wlabs.
*        colourize 'WUMLM' bestand-wumlm.
*        colourize 'WEINM' bestand-weinm.
*        colourize 'WSPEM' bestand-wspem.
*        colourize 'WRETM' bestand-wretm.
*        colourize 'WINSM' bestand-winsm.
*        colourize 'BWESB' bestand-bwesb.                    "AC0K020254
*        colourize 'WBWESB' bestand-wbwesb.                  "AC0K020254
*        colourize 'GLGMG' bestand-glgmg.                    "n912093
*        colourize 'WGLGM' bestand-wglgm.                    "n912093
*        colourize 'TRAME' bestand-trame.                    "n912093
*        colourize 'WTRAM' bestand-wtram.                    "n912093
*        colourize 'UMLMC' bestand-umlmc.                    "n912093
*        colourize 'WUMLC' bestand-wumlc.                    "n912093
**ENHANCEMENT-POINT EHP604_RM07MLBS_27 SPOTS ES_RM07MLBS .
*        bestand-farbe = color[].
*        MODIFY bestand.
*      ENDIF.                                                "n531604
*    ENDLOOP.
*  ENDIF.                                                    "n531604
*
** set the name for color table when required                "n531604
*  IF  alv_color = 'X'.                                      "n531604
*    CLEAR                    layout-coltab_fieldname.       "n531604
*  ELSE.                                                     "n531604
*    MOVE  'FARBE'            TO  layout-coltab_fieldname.   "n531604
*  ENDIF.                                                    "n531604
*
*  layout-group_change_edit = 'X'.
*
*  DATA : l_f_check(01)       TYPE c.
**ENHANCEMENT-POINT EHP604_RM07MLBS_28 SPOTS ES_RM07MLBS STATIC .
*
** added dynamic break-point ID MMIM_REP_MB52                "n912093
*  BREAK-POINT ID mmim_rep_mb52.                             "n912093
*
** process the list according the parameters                 "n531604
*  IF      NOT pa_hsq IS INITIAL.                            "n531604
**     create a hierarchic list                              "n531604
*
**     assign form routine for page numbering                "n667256
*    gs_events-name         =       'TOP_OF_PAGE'.           "n667256
*    gs_events-form         = 'F4000_TOP_OF_PAGE'.           "n667256
*    APPEND  gs_events      TO gt_events.                    "n667256
*
**ENHANCEMENT-POINT EHP604_RM07MLBS_29 SPOTS ES_RM07MLBS .
*
*
*    CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
*      EXPORTING
*        i_interface_check  = l_f_check
*        i_callback_program = repid
*        is_layout          = layout
*        it_fieldcat        = fieldcat[]
*        i_default          = 'X'
*        i_save             = 'A'
*        is_variant         = variante
*        it_events          = gt_events[]                    "n667256
*        i_tabname_header   = 'HEADER'
*        i_tabname_item     = 'BESTAND'
*        is_keyinfo         = keyinfo
*        is_print           = alv_print
*        it_sort            = sort[]
*        it_excluding       = excluding[]
*      TABLES
*        t_outtab_header    = header
*        t_outtab_item      = bestand
*      EXCEPTIONS
*        OTHERS             = 2.
*
*  ELSEIF  NOT  pa_flt IS INITIAL.                           "n531604
**   create a flat non-hierarchic list                     "n531604
*
**   assign form routine for page numbering only for       "n667256
**   classic ALV                                           "n667256
*    IF  alv_detail_func = 'REUSE_ALV_LIST_DISPLAY'.         "n667256
*      gs_events-name       =       'TOP_OF_PAGE'.           "n667256
*      gs_events-form       = 'F4000_TOP_OF_PAGE'.           "n667256
*      APPEND  gs_events    TO gt_events.                    "n667256
*    ENDIF.
*
**ENHANCEMENT-POINT EHP604_RM07MLBS_30 SPOTS ES_RM07MLBS .
*
*    CALL FUNCTION alv_detail_func
*      EXPORTING
*        i_interface_check  = l_f_check
*        i_callback_program = repid
*        is_layout          = layout
*        it_fieldcat        = fieldcat[]
*        it_sort            = sort[]
*        i_default          = 'X'
*        i_save             = 'A'
*        is_variant         = variante_flat
*        it_events          = gt_events[]                    "n667256
*        is_print           = alv_print
*      TABLES
*        t_outtab           = bestand
*      EXCEPTIONS
*        OTHERS             = 2.
*  ENDIF.                                                    "n531604
*
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                               " LISTAUSGABE
*
**&---------------------------------------------------------------------*
**&      Form  F4_FOR_VARIANT
**&---------------------------------------------------------------------*
**       F4-Hilfe für Reportvariante                                    *
**----------------------------------------------------------------------*
*FORM f4_for_variant.
*
** look for the available display variant depending on the   "n531604
** selected mode of the SAP-LIST-VIEWER
*  IF      NOT pa_hsq IS INITIAL.
**     for hierarchic seq. list
*    MOVE variante          TO  def_variante_f4.
*
*  ELSEIF  NOT  pa_flt IS INITIAL.
**     for flat ( simple ) list
*    MOVE variante_flat     TO  def_variante_f4.
*  ENDIF.
*
*  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
*       EXPORTING
*            is_variant          = def_variante_f4
*            i_save              = 'A'
**           it_default_fieldcat =
*       IMPORTING
*            e_exit              = variant_exit
*            es_variant          = def_variante
*       EXCEPTIONS
*            not_found = 2.
*
*  IF sy-subrc = 2.
*    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    IF variant_exit = space.
**     save the selected display variant
*      p_vari = def_variante-variant.
*
*      IF      NOT pa_hsq IS INITIAL.
**       for hierarchic seq. list
*        MOVE  p_vari         TO  g_f_vari_hsq.
*
*      ELSEIF  NOT  pa_flt IS INITIAL.
**       for flat ( simple ) list
*        MOVE  p_vari         TO  g_f_vari_flt.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                               " F4_FOR_VARIANT
*
**-----------------------------------------------------------"n531604
**    f0000_get_print_settings
**-----------------------------------------------------------"n531604
*
*FORM f0000_get_print_settings.
*
** get the settings for the SAP-LIST-VIEWER
*  SELECT SINGLE * FROM mmim_rep_print
*                             WHERE report = sy-repid.
*
*  IF sy-subrc <> 0.
**   if this entry is missing, set default values and insert
**   a new entry into database tabel MMIM_REP_PRINT
*    CLEAR                    mmim_rep_print.
*    MOVE : sy-repid          TO  mmim_rep_print-report,
*           'X'               TO  mmim_rep_print-selinfo.
*    INSERT                   mmim_rep_print.
*
*    IF  sy-subrc IS INITIAL.
*      COMMIT                 WORK.
*    ELSE.
**     error during insert into table MMIN_RE_PRINT
*      MESSAGE  s895          WITH  text-895.
*    ENDIF.
*  ENDIF.
*
** set the parameter for this run
*  IF mmim_rep_print-selinfo = 'X'.
*    alv_print-no_print_selinfos = ' '.
*  ELSE.
*    alv_print-no_print_selinfos = 'X'.
*  ENDIF.
*
*  IF mmim_rep_print-coverpage = 'X'.
*    alv_print-no_coverpage = ' '.
*  ELSE.
*    alv_print-no_coverpage = 'X'.
*  ENDIF.
*
*  IF mmim_rep_print-listinfo = 'X'.
*    alv_print-no_print_listinfos = ' '.
*  ELSE.
*    alv_print-no_print_listinfos = 'X'.
*  ENDIF.
*
*  IF mmim_rep_print-gridcontrol = 'X'.
*    alv_detail_func = 'REUSE_ALV_GRID_DISPLAY'.
*  ELSE.
*    alv_detail_func = 'REUSE_ALV_LIST_DISPLAY'.
*  ENDIF.
*
*  IF mmim_rep_print-color = 'X'.
*    alv_color       = 'X'.
*  ELSE.
*    alv_color       = space.
*  ENDIF.
*
*ENDFORM.                     "f0000_get_print_settings
*
**-----------------------------------------------------------"n531604
** Initialization of the user defaults for the checkboxes
**    f0100_settings_init.                                   "n531604
**-----------------------------------------------------------"n531604
*
*FORM f0100_settings_init.                                   "n531604
*
** only in dialog mode
*  CHECK : sy-batch IS INITIAL.
*
*  IF oref_settings IS INITIAL.
*    CREATE OBJECT oref_settings
*      EXPORTING
*        i_action = 'RM07MLBS'.
*  ENDIF.
*
** get the parameters from the last run
*  pa_sond   = oref_settings->get( 'PA_SOND' ).
*  pa_hsq    = oref_settings->get( 'PA_HSQ' ).
*
*  pa_flt    = oref_settings->get( 'PA_FLT' ).
*  negativ   = oref_settings->get( 'NEGATIV' ).
*  xmchb     = oref_settings->get( 'XMCHB'   ).
*  nozero    = oref_settings->get( 'NOZERO'  ).
*  novalues  = oref_settings->get( 'NOVALUES' ).
*
** check radiobuttons
*  IF  pa_hsq IS INITIAL.
*    IF  pa_flt IS INITIAL.
**     not allowed
*      MOVE  'X'              TO  pa_hsq.
*    ENDIF.
*  ELSE.
*    IF  pa_flt IS INITIAL.
*    ELSE.
**     not allowed
*      CLEAR                  pa_flt.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                     "f0100_settings_init           "n531604
*
*************************************************************************
** Save the user settings
*************************************************************************
*
*FORM f0200_settings_save.                                   "n531604
*
** only in dialog mode
*  CHECK : sy-batch IS INITIAL.
*
** go on when the FORM routines of INITILIZATION             "n667256
** were processed                                            "n667256
*  CHECK : g_flag_initialization = 'X'.                      "n667256
*
** Save the settings
*  CALL METHOD oref_settings->set( i_element = 'PA_SOND'
*    i_active = pa_sond ).
*  CALL METHOD oref_settings->set( i_element = 'PA_HSQ'
*    i_active = pa_hsq ).
*  CALL METHOD oref_settings->set( i_element = 'PA_FLT'
*    i_active = pa_flt ).
*  CALL METHOD oref_settings->set( i_element = 'NEGATIV'
*    i_active = negativ ).
*  CALL METHOD oref_settings->set( i_element = 'XMCHB'
*    i_active = xmchb ).
*
*  CALL METHOD oref_settings->set( i_element = 'NOZERO'
*    i_active = nozero ).
*  CALL METHOD oref_settings->set( i_element = 'NOVALUES'
*    i_active = novalues ).
*
*  CALL METHOD oref_settings->flush.
*
** carry out the database updates only; the normal commit    "n667256
** command does not allow to record this transaction for     "n667256
** a batch input session using transaction SHDB              "n667256
*  CALL FUNCTION 'DB_COMMIT'.                                "n667256
*
*ENDFORM.                     "f0200_settings_save           "n531604
*
**----------------------------------------------------------------------*
**    f0300_fieldcat_flat
**----------------------------------------------------------------------*
*
*FORM f0300_fieldcat_flat.
*
** define macro
*  DEFINE macro_fill_fieldcat.
*    add  : 1                 to  g_cnt_col_pos.
*    move : g_cnt_col_pos     to  fieldcat-col_pos,
*           &1                to  fieldcat-fieldname,
*           'BESTAND'         to  fieldcat-tabname,
*           &2                to  fieldcat-ref_tabname,
*           &3                to  fieldcat-no_out.
*
*    if  not fieldcat-seltext_l is initial.
*      move : fieldcat-seltext_l   to  fieldcat-seltext_m,
*             fieldcat-seltext_l   to  fieldcat-seltext_s.
*    endif.
*
*    append                   fieldcat.
*    clear                    fieldcat.
*  END-OF-DEFINITION.
*
**ENHANCEMENT-SECTION     RM07MLBS_11 SPOTS ES_RM07MLBS.
*  macro_fill_fieldcat 'MATNR'  'MARA'   c_out.
**END-ENHANCEMENT-SECTION.
*  macro_fill_fieldcat 'MAKTX'  'MAKT'   c_no_out.
*  macro_fill_fieldcat 'WERKS'  'T001W'  c_out.
*  macro_fill_fieldcat 'NAME1'  'T001W'  c_no_out.
*  macro_fill_fieldcat 'MTART'  'MARA'   c_no_out.
*  macro_fill_fieldcat 'MATKL'  'MARA'   c_no_out.
*  macro_fill_fieldcat 'EXTWG'  'MARA'   c_no_out.
*  macro_fill_fieldcat 'LGORT'  'MARD'   c_out.
*  macro_fill_fieldcat 'LGOBE'  'T001L'  c_no_out.           "1311282
*
*  IF NOT pa_sond IS INITIAL.
*    macro_fill_fieldcat 'SOBKZ'  'MKOL'   c_out.
*
*    IF  novalues IS INITIAL.
*      macro_fill_fieldcat 'KZBWS'  'MSSA'   c_out.
*    ENDIF.
*
*    MOVE : 'SSNUM'           TO  fieldcat-ref_fieldname.
*    macro_fill_fieldcat 'SSNUM' 'BICKEY' c_out.
*  ENDIF.
*
*  macro_fill_fieldcat 'LVORM'  'MARD'   c_out.
*
*  IF xmchb = 'X'.
*    macro_fill_fieldcat 'CHARG'  'MCHB'   c_out.
*  ENDIF.
*
**START PUNAM
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'ATWRT'.
*  fieldcat-col_pos       = 12.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'AUSP'.
*  fieldcat-seltext_m     = 'Legacy batch No'.
**    fieldcat-no_out        = 'X'.
*  APPEND fieldcat.
*
*  macro_fill_fieldcat 'HSDAT'  'MCH1'   c_out.
*  macro_fill_fieldcat 'VFDAT'  'MCH1'   c_out.
*
****** Start Code: Added by CS on 31.12.2015 for Loc Received Date, Last movement Date. *****
*  MOVE : 'Loc Received Date'           TO  fieldcat-seltext_l.
*  macro_fill_fieldcat 'ERSDA'  'MCHB'   c_out.
*
*  MOVE : 'Last movement Date'           TO  fieldcat-seltext_l.
*  macro_fill_fieldcat 'LAEDA'  'MCHB'   c_out.
****** End Code: Added by CS on 31.12.2015 for Loc Received Date, Last movement Date. *****
*
**macro_fill_fieldcat 'ATWRT'  'AUSP'   c_out.
**end punam
*
*  macro_fill_fieldcat 'MEINS'  'MARA'   c_out.
** Stock and value for stock unrestrestricted use
*  fieldcat-qfieldname    = 'MEINS'.
*  macro_fill_fieldcat 'LABST'  'MARD'   c_out.
**ENHANCEMENT-POINT EHP604_RM07MLBS_31 SPOTS ES_RM07MLBS .
*
*  IF novalues IS INITIAL.
*    fieldcat-outputlen     = 5.
*    macro_fill_fieldcat 'WAERS'  'T001'   c_out.
*
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-cfieldname    = 'WAERS'.
**   'Wert frei verwend.'.
*    fieldcat-seltext_l     = text-021.
*    fieldcat-do_sum        = 'X'.
*    fieldcat-outputlen     = '18'.
*    macro_fill_fieldcat 'WLABS'  'MBEW'   c_out.
*  ENDIF.
*
** stock and value for stock in transfer
*  fieldcat-qfieldname    = 'MEINS'.
*  fieldcat-ref_fieldname = 'MB52_TRAUML'.                   "n912093
*  macro_fill_fieldcat 'UMLME'  'AM07M'   c_out.             "n912093
**ENHANCEMENT-POINT EHP604_RM07MLBS_32 SPOTS ES_RM07MLBS .
*
*  IF novalues IS INITIAL.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-cfieldname    = 'WAERS'.
**   'Wert in Umlagerung'. // Wert Umlag u. Transit     "n912093
**   'Wert Umlag.Bestand'. // n912093                   "n912093
*    fieldcat-seltext_l     = text-030.                      "n912093
*    fieldcat-do_sum        = 'X'.
*    fieldcat-outputlen     = '18'.
*    macro_fill_fieldcat 'WUMLM'  'MBEW'   c_out.
*  ENDIF.
*
** stock and value for stock in quality inspection
*  fieldcat-qfieldname    = 'MEINS'.
*  macro_fill_fieldcat 'INSME'  'MARD'   c_out.
**ENHANCEMENT-POINT EHP604_RM07MLBS_33 SPOTS ES_RM07MLBS .
*
*  IF novalues IS INITIAL.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-cfieldname    = 'WAERS'.
**   "Wert in QualPrüfng'
*    fieldcat-seltext_l     = text-023.
*    fieldcat-do_sum        = 'X'.
*    fieldcat-outputlen     = '18'.
*    macro_fill_fieldcat 'WINSM'  'MBEW'   c_out.
*  ENDIF.
*
** stock and value for restricted stock
*  fieldcat-qfieldname    = 'MEINS'.
*  macro_fill_fieldcat 'EINME'  'MARD'   c_out.
**ENHANCEMENT-POINT EHP604_RM07MLBS_34 SPOTS ES_RM07MLBS .
*
*  IF novalues IS INITIAL.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-cfieldname    = 'WAERS'.
**   "Wert nicht frei'
*    fieldcat-seltext_l     = text-024.
*    fieldcat-do_sum        = 'X'.
*    fieldcat-outputlen     = '18'.
*    macro_fill_fieldcat 'WEINM'  'MBEW'   c_out.
*  ENDIF.
*
** stock and value for blocked stock
*  fieldcat-qfieldname    = 'MEINS'.
*  macro_fill_fieldcat 'SPEME'  'MARD'   c_out.
**ENHANCEMENT-POINT EHP604_RM07MLBS_35 SPOTS ES_RM07MLBS .
*
*  IF novalues IS INITIAL.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-cfieldname    = 'WAERS'.
**   'Wert Sperrbestand'
*    fieldcat-seltext_l     = text-025.
*    fieldcat-do_sum        = 'X'.
*    fieldcat-outputlen     = '18'.
*    macro_fill_fieldcat 'WSPEM'  'MBEW'   c_out.
*  ENDIF.
*
** stock and value for blocked returns
*  fieldcat-qfieldname    = 'MEINS'.
*  macro_fill_fieldcat 'RETME'  'MARD'   c_out.
**ENHANCEMENT-POINT EHP604_RM07MLBS_36 SPOTS ES_RM07MLBS .
*
** Values
*  IF novalues IS INITIAL.
*    fieldcat-ref_fieldname = 'SALK3'.
*    fieldcat-cfieldname    = 'WAERS'.
**   "Wert RetourenSperr'.
*    fieldcat-seltext_l     = text-026.
*    fieldcat-do_sum        = 'X'.
*    fieldcat-outputlen     = '18'.
*    macro_fill_fieldcat 'WRETM'  'MBEW'   c_out.
*  ENDIF.                               "novalues is initial
*
** process valuated block GR stock as hidden field        "AC0K020254
*  fieldcat-qfieldname    = 'MEINS'.                         "AC0K020254
*  macro_fill_fieldcat 'BWESB'  'MARC'   c_no_out.           "AC0K020254
**ENHANCEMENT-POINT EHP604_RM07MLBS_37 SPOTS ES_RM07MLBS .
*                                                            "AC0K020254
** the estimated value for the valuated block GR stock    "AC0K020254
** as hidden field, too                                   "AC0K020254
*  IF novalues IS INITIAL.                                   "AC0K020254
*    fieldcat-ref_fieldname = 'SALK3'.                       "AC0K020254
*    fieldcat-cfieldname    = 'WAERS'.                       "AC0K020254
**   value blocked GR stock                               "AC0K020254
*    fieldcat-seltext_l     = text-027.                      "AC0K020254
*    fieldcat-do_sum        = 'X'.                           "AC0K020254
*    fieldcat-outputlen     = '18'.                          "AC0K020254
*    macro_fill_fieldcat 'WBWESB'  'MBEW'   c_no_out.        "AC0K020254
*  ENDIF.
*
** stock and value for tied empties                    "n912093
*  fieldcat-qfieldname    = 'MEINS'.                         "n912093
*  macro_fill_fieldcat 'GLGMG'  'MARC'   c_no_out.           "n912093
**ENHANCEMENT-POINT EHP604_RM07MLBS_38 SPOTS ES_RM07MLBS .
*                                                            "n912093
** Values                                              "n912093
*  IF novalues IS INITIAL.                                   "n912093
*    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
*    fieldcat-cfieldname    = 'WAERS'.                       "n912093
**   'Wert gebundenes Leergut'                         "n912093
*    fieldcat-seltext_l     = text-028.                      "n912093
*    fieldcat-do_sum        = 'X'.                           "n912093
*    fieldcat-outputlen     = '18'.                          "n912093
*    macro_fill_fieldcat 'WGLGM'  'MBEW'   c_no_out.         "n912093
*  ENDIF.                                                    "n912093
*
*
** stock and value for stock in transit                "n912093
*  fieldcat-qfieldname    = 'MEINS'.                         "n912093
*  macro_fill_fieldcat 'TRAME'  'MARC'   c_no_out.           "n912093
**ENHANCEMENT-POINT EHP604_RM07MLBS_39 SPOTS ES_RM07MLBS .
*                                                            "n912093
** Values                                              "n912093
*  IF novalues IS INITIAL.                                   "n912093
*    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
*    fieldcat-cfieldname    = 'WAERS'.                       "n912093
**   'Wert Transitbestand'                             "n912093
*    fieldcat-seltext_l     = text-029.                      "n912093
*    fieldcat-do_sum        = 'X'.                           "n912093
*    fieldcat-outputlen     = '18'.                          "n912093
*    macro_fill_fieldcat 'WTRAM'  'MBEW'   c_no_out.         "n912093
*  ENDIF.                                                    "n912093
*
** stock and value for stock in transit                "n912093
*  fieldcat-qfieldname    = 'MEINS'.                         "n912093
*  macro_fill_fieldcat 'UMLMC'  'MARC'   c_no_out.           "n912093
**ENHANCEMENT-POINT EHP604_RM07MLBS_40 SPOTS ES_RM07MLBS .
*                                                            "n912093
** Values                                              "n912093
*  IF novalues IS INITIAL.                                   "n912093
*    fieldcat-ref_fieldname = 'SALK3'.                       "n912093
*    fieldcat-cfieldname    = 'WAERS'.                       "n912093
**   'Wert Umlagerung an Werk'                         "n912093
*    fieldcat-seltext_l     = text-022.                      "n912093
*    fieldcat-do_sum        = 'X'.                           "n912093
*    fieldcat-outputlen     = '18'.                          "n912093
*    macro_fill_fieldcat 'WUMLC'  'MBEW'   c_no_out.         "n912093
*  ENDIF.                                                    "n912093
**START PUNAM
**macro_fill_fieldcat 'HSDAT'  'MCH1'   c_out.
**macro_fill_fieldcat 'VFDAT'  'MCH1'   c_out.
***macro_fill_fieldcat 'ATWRT'  'AUSP'   c_out.
*
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'SL_DAYS'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
*  fieldcat-seltext_m     = 'Shelf Life-Days'.
**    fieldcat-no_out        = 'X'.
*  fieldcat-col_pos       = 27.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'STATUS'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
*  fieldcat-seltext_m     = 'STATUS'.
**    fieldcat-no_out        = 'X'.
*  fieldcat-outputlen     = 22.
*  fieldcat-col_pos       = 28.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'SPART'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
*  fieldcat-seltext_m     = 'Division'.
**    fieldcat-no_out        = 'X'.
**     fieldcat-outputlen     = 22.
*  fieldcat-col_pos       = 29.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'VTEXT'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
*  fieldcat-seltext_m     = 'Division'.
*  fieldcat-no_out        = 'X'.
**     fieldcat-outputlen     = 22.
*  fieldcat-col_pos       = 30.
*  APPEND fieldcat.
*
*
**  CLEAR fieldcat.
**    fieldcat-fieldname     = 'ATWRT'.
***    fieldcat-tabname       = 'BESTAND'.
***    fieldcat-ref_tabname   = 'AUSP'.
**    fieldcat-seltext_M     = 'Legacy batch No'.
***    fieldcat-no_out        = 'X'.
** APPEND fieldcat.
** END PUNAM
*
*
** set the key fields of the special stock as hidden fields
*  IF  g_flag_sobkz-lifnr = 'X'.
*    macro_fill_fieldcat 'LIFNR'  'MKOL'   c_no_out.
*  ENDIF.
*
*  IF  g_flag_sobkz-kunnr = 'X'.
*    macro_fill_fieldcat 'KUNNR'  'MSKU'   c_no_out.
*  ENDIF.
*
*  IF  g_flag_sobkz-vbeln = 'X'.
*    macro_fill_fieldcat 'VBELN'  'MSKA'   c_no_out.
*    macro_fill_fieldcat 'POSNR'  'MSKA'   c_no_out.
*  ENDIF.
*
*  IF  g_flag_sobkz-pspnr = 'X'.
*    macro_fill_fieldcat 'PSPNR'  'MSPR'   c_no_out.
*  ENDIF.
*
*
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'CITYC'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
*  fieldcat-seltext_m     = 'Sales Dist.'.
*  fieldcat-no_out        = 'X'.
**     fieldcat-outputlen     = 22.
**    fieldcat-COL_POS       = 30.
*  APPEND fieldcat.
*
*  CLEAR fieldcat.
*  fieldcat-fieldname     = 'BEZEI'.
*  fieldcat-tabname       = 'BESTAND'.
**    fieldcat-ref_tabname   = 'MCH1'.
*  fieldcat-seltext_m     = 'Sales Dist.Text'.
*  fieldcat-no_out        = 'X'.
**     fieldcat-outputlen     = 22.
**    fieldcat-COL_POS       = 30.
*  APPEND fieldcat.
*
*ENDFORM.                     "f0300_fieldcat_flat
*
**----------------------------------------------------------------------*
**    f0400_create_sort
**----------------------------------------------------------------------*
*
*FORM f0400_create_sort
*                   USING     l_f_fieldname LIKE sort-fieldname.
*
** create the table with the alv sort information
*
*  IF      NOT pa_hsq IS INITIAL.
**   for hierarchic seq. list
**   check whether this is an active field is in the
**   fieldcat
*    READ TABLE fieldcat WITH KEY
*                             fieldname = l_f_fieldname
*                             no_out    = space.
*
*  ELSEIF  NOT  pa_flt IS INITIAL.
**   for flat ( simple ) list
**   check whether this field is in the fieldcat
*    READ TABLE fieldcat WITH KEY
*                             fieldname = l_f_fieldname.
*  ENDIF.
*
*  IF sy-subrc IS INITIAL.
*    ADD  1                   TO  g_cnt_spos.
*    MOVE : g_cnt_spos        TO  sort-spos,
*           l_f_fieldname     TO  sort-fieldname,
*           'X'               TO  sort-up,
*           'BESTAND'         TO  sort-tabname.
*    APPEND                   sort.
*    CLEAR                    sort.
*  ENDIF.
*
*ENDFORM.                     "f0400_create_sort
*
**----------------------------------------------------------------------*
**    f2000_COLLECT_collector.
**----------------------------------------------------------------------*
*
*FORM f2000_collect_collector.
*
** does the user want to suppress stock objects from plant   "n577268
** level ?                                                   "n577268
*  IF  g_flag_suppress_init_lgort = 'X'.                     "n577268
*    IF  collector-lgort IS INITIAL.                         "n577268
**     ignore stock objects without storage location         "n577268
*      EXIT.                  "--> go to exit                "n577268
*    ENDIF.                                                  "n577268
*  ENDIF.                                                    "n577268
*
*************************************************************************
** process the functions "No zero stocks",
** "Negative stocks only", "Without batches" here
*************************************************************************
**ENHANCEMENT-POINT EHP604_RM07MLBS_41 SPOTS ES_RM07MLBS .
*
*  IF negativ = 'X'.
**   ignore entry if all stocks are zero or greater
*    IF collector-labst >= 0 AND collector-einme >= 0 AND
*       collector-insme >= 0 AND collector-retme >= 0 AND
*       collector-speme >= 0 AND collector-umlme >= 0.
*      EXIT.                  "--> go to exit
*    ENDIF.
*  ENDIF.
*
*  IF nozero = 'X'.
**   ignore all entries without stock
*    IF collector-labst = 0 AND collector-einme = 0 AND
*       collector-insme = 0 AND collector-retme = 0 AND
*       collector-speme = 0 AND collector-umlme = 0.
*      EXIT.                  "--> go to exit
*    ENDIF.
*  ENDIF.
*
*  IF xmchb IS INITIAL.
*    CLEAR collector-charg.
*  ENDIF.
*
*  COLLECT                    collector.
*
*ENDFORM.                     "f2000_COLLECT_collector.
*
**-----------------------------------------------------------"n579976
**     F3000_SEND_WARNING_M7_393                             "n579976
**-----------------------------------------------------------"n579976
*                                                            "n579976
**&---------------------------------------------------------------------*
**&      Form  F3000_SEND_WARNING_M7_393
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->L_VARIANT  text
**----------------------------------------------------------------------*
*FORM f3000_send_warning_m7_393                              "n579976
*         USING     l_variant LIKE  disvariant-variant.      "n579976
*                                                            "n579976
** check the customising settings : emerge warning 393 ?     "n579976
*  CALL FUNCTION              'ME_CHECK_T160M'               "n579976
*    EXPORTING                                               "n579976
*      i_arbgb                = 'M7'                         "n579976
*      i_msgnr                = '393'                        "n579976
*    EXCEPTIONS                                              "n579976
*      nothing                = 0                            "n579976
*      OTHERS                 = 1.                           "n579976
*                                                            "n579976
*  IF sy-subrc <> 0.                                         "n579976
**   list will be created using the initial layout &         "n579976
*    MESSAGE i393             WITH  l_variant.               "n667256
*  ENDIF.                                                    "n579976
*                                                            "n579976
*ENDFORM.                     "F3000_SEND_WARNING_M7_393     "n579976
*                                                            "n579976
**-----------------------------------------------------------"n579976
*
*
**-----------------------------------------------------------"n667256
**  F4000_TOP_OF_PAGE.                                       "n667256
**-----------------------------------------------------------"n667256
*                                                            "n667256
*
*AT SELECTION-SCREEN OUTPUT.                                 "n667256
*                                                            "n667256
**ENHANCEMENT-POINT RM07MLBS_04 SPOTS ES_RM07MLBS.
*  IF  g_flag_initialization IS INITIAL.                     "n667256
**   the process time INITIALIZATION was not done, so        "n667256
**   carry out the functions here                            "n667256
*    MOVE  'X'                TO g_flag_initialization.      "n667256
*                                                            "n667256
*    PERFORM                  f0000_get_print_settings.      "n667256
*                                                            "n667256
**   look for the setting of the parameters from the         "n667256
**   last run                                                "n667256
*    PERFORM                  f0100_settings_init.           "n667256
*                                                            "n667256
*    PERFORM                  initialisierung.
*                                                            "n667256
*  ENDIF.
*  PERFORM                  modify_screen.                   "n667256
*                                                            "n667256
**-----------------------------------------------------------"n667256
*
**------------------------- Datenselektion -----------------------------*
*FORM f4000_top_of_page.                                     "n667256
*                                                            "n667256
** go on if there is a print destination set                 "n667256
*  CHECK   NOT sy-prdsn IS INITIAL.                          "n667256
*
** go on if it is in print modus, only                       "n960980
*  CHECK sy-ucomm = '&RNT' OR sy-ucomm IS INITIAL.           "n960980
*
*                                                            "n667256
**     classic ALV : use the simple write command            "n667256
*  WRITE : sy-datlo DD/MM/YYYY, sy-title, sy-pagno.          "n667256
*                                                            "n667256
*                                                            "n667256
*ENDFORM.                     "F4000_TOP_OF_PAGE.            "n667256
*                                                            "n667256
**-----------------------------------------------------------"n667256
**ENHANCEMENT-POINT EHP604_RM07MLBS_42 SPOTS ES_RM07MLBS STATIC .
**&---------------------------------------------------------------------*
**&      Form  MODIFY_SCREEN
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM modify_screen .
**  novalues = 'X'.
*  LOOP AT SCREEN.
*    IF screen-group1 = 'MOD'.
*      screen-active = '1'.
*      screen-input  = '0'.
*      screen-output = '1'.
*      screen-invisible = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
*
*ENDFORM.                    " MODIFY_SCREEN
**&---------------------------------------------------------------------*
**&      Form  CHECK_AUTH_OBJ
**&---------------------------------------------------------------------*
** Added by Chirag Shah 20.10.2015 Check Authority of Plant & Sales District..
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM check_auth_obj.
*  TYPES: BEGIN OF type_t001w_check,
*          werks TYPE t001w-werks, " Plant
*        END OF type_t001w_check
**        BEGIN OF type_t005h_check,
**          cityc TYPE t005h-cityc, " Sales District
**        END OF type_t005h_check
*        .
*  DATA: t_t001w_check TYPE TABLE OF type_t001w_check, " For Plant
*        w_t001w_check TYPE type_t001w_check
**        t_t005h_check TYPE TABLE OF type_t005h_check, " For Sales District
**        w_t005h_check TYPE type_t005h_check.
*.
*  DATA: lv_auth_flg TYPE c VALUE ''.
*
*  FREE: t_t001w_check[].", t_t005h_check[].
*  CLEAR: w_t001w_check.", w_t005h_check.
*
*  break test1.
****** Start Code: Added by CS on 20.10.2015 for Plant Authorization. *****
*  SELECT werks  " Fetch values of Plants
*    FROM t001w
*    INTO TABLE t_t001w_check
*    WHERE werks IN werks.
*  IF t_t001w_check[] IS NOT INITIAL.
*    CLEAR: werks.
*    REFRESH: werks.
*    LOOP AT t_t001w_check INTO w_t001w_check. " Check Authorizations for Plant
*      AUTHORITY-CHECK OBJECT 'M_MATE_WRK' " 'M_MSEG_WWA'
*                          ID 'ACTVT' FIELD '03'
*                          ID 'WERKS' FIELD w_t001w_check-werks.
*      IF sy-subrc EQ 0.
*        werks-sign = 'I'.
*        werks-option = 'EQ'.
*        werks-low = w_t001w_check-werks.
*        APPEND werks TO werks.
*        CLEAR: werks.
*      ELSE.
*        IF lv_werks_auth_flg IS INITIAL.  " Authorization Flag
*          lv_werks_auth_flg = 'X'.
*        ENDIF.
*      ENDIF.
*      CLEAR: w_t001w_check.
*    ENDLOOP.
*    FREE: t_t001w_check[].
*  ENDIF.
*  IF werks[] IS INITIAL.
*    werks-sign = 'I'.
*    werks-option = 'EQ'.
*    werks-low = ''.
*    APPEND werks.
*    CLEAR: werks.
*  ENDIF.
****** End Code: Added by CS on 20.10.2015 for Plant Authorization. *****
*
******* Start Code: Added by CS on 20.10.2015 for Sales District/City - 4 Char Authorization. *****
**  SELECT cityc  " Fetch values of Sales District i.e. City
**    FROM t005h
**    INTO TABLE t_t005h_check
**    WHERE cityc IN cityc
**      AND spras EQ sy-langu.
**  IF t_t005h_check[] IS NOT INITIAL.
**    CLEAR: cityc.
**    REFRESH: cityc.
**    LOOP AT t_t005h_check INTO w_t005h_check. " Check Authorizations for Sales District
**      AUTHORITY-CHECK OBJECT 'ZCITYC'
**                          ID 'ACTVT' FIELD '03'
**                          ID 'CITYC' FIELD w_t005h_check-cityc.
**
**      IF sy-subrc EQ 0.
**        cityc-sign = 'I'.
**        cityc-option = 'EQ'.
**        cityc-low = w_t005h_check-cityc.
**        APPEND cityc TO cityc.
**        CLEAR: cityc.
**      ELSE.
**        IF lv_cityc_auth_flg IS INITIAL.  " Authorization Flag
**          lv_cityc_auth_flg = 'X'.
**        ENDIF.
**      ENDIF.
**      CLEAR: w_t005h_check.
**    ENDLOOP.
**    FREE: t_t005h_check[].
**  ENDIF.
**  IF cityc[] IS INITIAL.
**    cityc-sign = 'I'.
**    cityc-option = 'EQ'.
**    cityc-low = ''.
**    APPEND cityc TO cityc.
**    CLEAR cityc.
**  ENDIF.
**  IF lv_auth_flg EQ 'X'.
**    MESSAGE 'Authorization Missing.' TYPE 'S' DISPLAY LIKE 'W'.
**  ENDIF.
****** End Code: Added by CS on 20.10.2015 for Sales District/City - 4 Char Authorization. *****
*ENDFORM.                    " CHECK_AUTH_OBJ
