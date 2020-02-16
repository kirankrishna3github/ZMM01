*&---------------------------------------------------------------------*
*& Report  ZINVENTORY_AGING_REPORT_T
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 06.11.2015
*   REASON FOR CHANGE: Add Authorization
*   REQUEST #: IRDK921561
* --------------------------------------------------------------------------------------------*
*   CHANGED BY: Saurabh Khare
*   CHANGE ON:  23.08.2017
*   REASON FOR  CHANGE: Resolve Runtime error ITAB_STRUC_ACCESS_VIOLATION in FM's using wertetab due to type mismatch
*   REQUEST #:  IRDK928916
* --------------------------------------------------------------------------------------------*
REPORT  zinventory_aging_report MESSAGE-ID ziage.

TYPE-POOLS: slis.

TABLES: tvko,
        t024e,
        t001w,
        marc,
        mara,
        mkpf,
        mard,
        msku,
        mslb.

DATA: BEGIN OF xv134w OCCURS 100.
        INCLUDE STRUCTURE v134w.
DATA: END OF xv134w.

DATA: BEGIN OF xt134m OCCURS 100.
        INCLUDE STRUCTURE t134m.
DATA: END OF xt134m.

DATA: BEGIN OF xt156  OCCURS 100.
        INCLUDE STRUCTURE t156.
DATA: END OF xt156.

DATA: BEGIN OF xt156s OCCURS 100.
        INCLUDE STRUCTURE v156s.
DATA: END OF xt156s.

DATA: BEGIN OF xt156m OCCURS 100.
        INCLUDE STRUCTURE t156m.
DATA: END OF xt156m.

DATA: BEGIN OF xt156c OCCURS 100.
        INCLUDE STRUCTURE t156c.
DATA: END OF xt156c.

DATA: xt156b LIKE t156b OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF mjahr OCCURS 1.
        INCLUDE STRUCTURE mcbmjahr.
DATA: END OF mjahr.

DATA: BEGIN OF wertetab OCCURS 500.
        INCLUDE STRUCTURE bco_werte.
DATA    bestand LIKE mard-labst.
DATA: END OF wertetab.
* ---- Modification on Wednesday, August 23, 2017 14:56:56 to avoid Runtime Error in FM's using wertetab due to type mismatch---- *
* Developer: Saurabh Khare *
* Req: IRDK928916 *
DATA: wertewa LIKE LINE OF wertetab.

DATA: wertetab_fm TYPE TABLE OF bco_werte,
      wertewa_fm  TYPE bco_werte.
* ---- End of Modification ---- *
DATA: BEGIN OF verbr_mat OCCURS 100.
        INCLUDE STRUCTURE bco_werte.
DATA: END OF verbr_mat.

DATA: aobj,
      vondatum LIKE sy-datlo,
      bisdatum LIKE sy-datlo,
      bestand  LIKE mard-labst.


RANGES: ekgrp FOR marc-ekgrp,
        dismm FOR marc-dismm,
        dispo FOR marc-dispo,
        sobkz FOR mseg-sobkz.

TYPES: BEGIN OF behi_type,
         perio   TYPE d,
         zugang  LIKE mard-labst,
         abgang  LIKE mard-labst,
         labst   LIKE mard-labst,
         zugangk LIKE mard-labst,
         abgangk LIKE mard-labst,
       END OF behi_type.

DATA: berid_range LIKE berid_range OCCURS 0 WITH HEADER LINE,
      behi        TYPE behi_type OCCURS 0 WITH HEADER LINE,
      xmseg       LIKE mseg OCCURS 0 WITH HEADER LINE,
      behi_maxrow LIKE sy-tabix.


TYPES: BEGIN OF bind_type,            "interner Belegindex
         budat LIKE mkpf-budat,           "Buchungsdatum
         mblnr LIKE mkpf-mblnr,           "Belegnummer
         mjahr LIKE mkpf-mjahr,           "Belegjahr
         zeile LIKE mseg-zeile,           "Position
         werks LIKE mseg-werks,           "Werk
         lgort LIKE mseg-lgort,           "Lagerort
         menge LIKE mseg-menge,           "Menge
       END OF bind_type.

DATA: bind TYPE bind_type OCCURS 0 WITH HEADER LINE.

TYPES : BEGIN OF ty_mbew,
          matnr TYPE matnr,
          bwkey TYPE bwkey,
          lbkum TYPE lbkum,
          salk3 TYPE salk3,
          verpr TYPE verpr,
        END OF ty_mbew.

DATA : it_mbew TYPE STANDARD TABLE OF ty_mbew,
       wa_mbew TYPE ty_mbew.

DATA : BEGIN OF it_finaltab OCCURS 10,
         werks        LIKE bco_werte-werks,
         matnr        LIKE bco_werte-matnr,
         pendays      LIKE bco_werte-hlp_ltzbew,
         pendstk      LIKE bco_werte-akt_menge,
         period       LIKE mkpf-budat,
         issue        LIKE mard-labst,
         recpt        LIKE mard-labst,
*          recpt   LIKE mard-labst,
         stock1       LIKE mard-labst,
         stock2       LIKE mard-labst,
         stock3       LIKE mard-labst,
         stock4       LIKE mard-labst,
         stock5       LIKE mard-labst,
         stock6       LIKE mard-labst,
         stock7       LIKE mard-labst,
         stock8       LIKE mard-labst,
         stock9       LIKE mard-labst,
         stock10      LIKE mard-labst,
         stock11      LIKE mard-labst,
         stock12      LIKE mard-labst,
         stock13      LIKE mard-labst,
         stock14      LIKE mard-labst,
         txt_days(14),
       END OF it_finaltab.

DATA : BEGIN OF it_displaytab OCCURS 10,
         col(4)       TYPE c,
         werks        LIKE bco_werte-werks,
         matnr        LIKE bco_werte-matnr,
         pendays      LIKE bco_werte-hlp_ltzbew,
         pendstk      LIKE bco_werte-akt_menge,
         verpr        TYPE verpr,
         stock1       LIKE mard-labst,
         value1       LIKE mbew-salk3,
         stock2       LIKE mard-labst,
         value2       LIKE mbew-salk3,
         stock3       LIKE mard-labst,
         value3       LIKE mbew-salk3,
         stock4       LIKE mard-labst,
         value4       LIKE mbew-salk3,
         stock5       LIKE mard-labst,
         value5       LIKE mbew-salk3,
         stock6       LIKE mard-labst,
         value6       LIKE mbew-salk3,
         stock7       LIKE mard-labst,
         value7       LIKE mbew-salk3,
*          stock8  LIKE mard-labst,
*          value8  LIKE mbew-salk3,
*          stock9  LIKE mard-labst,
*          value9  LIKE mbew-salk3,
*          stock10  LIKE mard-labst,
*          value10  LIKE mbew-salk3,
*          stock11  LIKE mard-labst,
*          value11  LIKE mbew-salk3,
*          stock12  LIKE mard-labst,
*          value12  LIKE mbew-salk3,
*          stock13  LIKE mard-labst,
*          value13  LIKE mbew-salk3,
*          stock14  LIKE mard-labst,
*          value14  LIKE mbew-salk3,
         txt_days(14),
       END OF  it_displaytab,

       BEGIN OF it_displaytab1 OCCURS 10,
         col(4)       TYPE c,
         werks        LIKE bco_werte-werks,
         matnr        LIKE bco_werte-matnr,
         pendays      LIKE bco_werte-hlp_ltzbew,
         pendstk      TYPE string,
         verpr        LIKE mbew-verpr,
         stock1       TYPE string,
         value1       LIKE mbew-salk3,
         stock2       TYPE string,
         value2       LIKE mbew-salk3,
         stock3       TYPE string,
         value3       LIKE mbew-salk3,
         stock4       TYPE string,
         value4       LIKE mbew-salk3,
         stock5       TYPE string,
         value5       LIKE mbew-salk3,
         stock6       TYPE string,
         value6       LIKE mbew-salk3,
         stock7       TYPE string,
         value7       LIKE mbew-salk3,
*          stock8   TYPE string,
*          value8   LIKE mbew-salk3,
*          stock9   TYPE string,
*          value9   LIKE mbew-salk3,
*          stock10  TYPE string,
*          value10  LIKE mbew-salk3,
*          stock11  TYPE string,
*          value11  LIKE mbew-salk3,
*          stock12  TYPE string,
*          value12  LIKE mbew-salk3,
*          stock13  TYPE string,
*          value13  LIKE mbew-salk3,
*          stock14  TYPE string,
*          value14  LIKE mbew-salk3,
         txt_days(14),
       END OF  it_displaytab1,

       BEGIN OF it_displaytabf OCCURS 10,
*          col(4)   TYPE c,
         werks        TYPE werks , "LIKE bco_werte-werks,
         matnr        TYPE matnr,  " bco_werte-matnr,
         maktx        TYPE maktx,

         name2        TYPE t001w-name2, "short plant Name
         spart        TYPE spart, "divion
         vtext        TYPE tspat-vtext, "division Name
         mtart        TYPE mara-mtart, "MATERIAL TYPE
         mtbez        TYPE t134t-mtbez,

*          PENDAYS(10) TYPE C,"" LIKE  BCO_WERTE-HLP_LTZBEW,
*          PENDAYS LIKE  BCO_WERTE-HLP_LTZBEW,
         pendays      TYPE i,
*          pendstk   TYPE string,
         pendstk      LIKE bco_werte-akt_menge,

         verpr        TYPE verpr,  " mbew-verpr,

         value1       LIKE mbew-salk3,

         value2       LIKE mbew-salk3,

         value3       LIKE mbew-salk3,

         value4       LIKE mbew-salk3,

         value5       LIKE mbew-salk3,

         value6       LIKE mbew-salk3,

         value7       LIKE mbew-salk3,
         txt_days(14),
         flg(1)       TYPE c,

       END OF  it_displaytabf.
DATA: wa_makt TYPE makt.

DATA : ls_displaytab  LIKE LINE OF it_displaytab1,
       ls_displaytab1 LIKE LINE OF it_displaytab1.

*
DATA: wt_fieldcat  TYPE slis_t_fieldcat_alv,
      wt_fieldcatc TYPE slis_t_fieldcat_alv.

DATA: wt_sort     TYPE slis_t_sortinfo_alv,
      ws_sort     TYPE slis_t_sortinfo_alv,
      wt_layout   TYPE slis_layout_alv,
      t_dat       LIKE sy-datum,
      t_dat1      LIKE sy-datum,
      t_dat2      LIKE sy-datum,
      t_dat3      LIKE sy-datum,
      t_dat4      LIKE sy-datum,
      t_dat5      LIKE sy-datum,
      t_dat6      LIKE sy-datum,
      t_dat7      LIKE sy-datum,
      t_dat8      LIKE sy-datum,
      t_dat9      LIKE sy-datum,
      t_dat10     LIKE sy-datum,
      t_dat11     LIKE sy-datum,
      t_dat12     LIKE sy-datum,
      t_dat13     LIKE sy-datum,
      t_dat14     LIKE sy-datum,
      txt_days(1),
      t_days(3),
      t1(10)      TYPE c,
      t2(10)      TYPE c,
      t3(10)      TYPE c,
      t4(10)      TYPE c,
      t5(10)      TYPE c,
      t6(10)      TYPE c,
      t7(10)      TYPE c,

      v1          TYPE i,
      v2          TYPE i,
      v3          TYPE i,
      v4          TYPE i,
      v5          TYPE i,
      v6          TYPE i,

      s1          TYPE i,
      s2          TYPE i,
      s3          TYPE i,
      s4          TYPE i,
      s5          TYPE i,
      s6          TYPE i.

DATA: repid TYPE sy-repid.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr TYPE mara-matnr, "MATERIAL
        spart TYPE mara-spart, "DIVISION
        vtext TYPE tspat-vtext, "DIVISION DESCRIPTION
        mtart TYPE mara-mtart, "MATERIAL TYPE
        mtbez TYPE t134t-mtbez, "TYPE NAME
      END OF it_matnr.

DATA: BEGIN OF it_plant OCCURS 0,
        werks TYPE t001w-werks,
        name2 TYPE t001w-name2,
      END OF it_plant.
***** Start Code: Added by CS on 06.11.2015 for Authorization. *****
DATA: lv_mtart_auth_flg TYPE c VALUE '',  " Auth. Flag for Material Type
      lv_werks_auth_flg TYPE c VALUE '',  " Auth. Flag for Plant
      lv_spart_auth_flg TYPE c VALUE ''  " Auth. Flag for Division
      .
***** End Code: Added by CS on 06.11.2015 for Authorization. *****
SELECTION-SCREEN BEGIN OF BLOCK objekte WITH FRAME TITLE text-001.

SELECT-OPTIONS vkorg FOR tvko-vkorg.
SELECT-OPTIONS ekorg FOR t024e-ekorg.
SELECT-OPTIONS werke FOR t001w-werks OBLIGATORY.

SELECTION-SCREEN END OF BLOCK objekte.

SELECTION-SCREEN BEGIN OF BLOCK zeitraum WITH FRAME TITLE text-004.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(31) text-002 FOR FIELD anztage.
PARAMETERS: anztage LIKE rmcb0-anata DEFAULT '365'.         "'180'.
SELECTION-SCREEN COMMENT  41(4) text-003.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN : COMMENT 1(11) text-786,
                   POSITION 33.
PARAMETERS p_c1 TYPE c LENGTH 4 DEFAULT '30' .
PARAMETERS p_c2 TYPE c LENGTH 4 DEFAULT '60'.
PARAMETERS p_c3 TYPE c LENGTH 4 DEFAULT '90'.
PARAMETERS p_c4 TYPE c LENGTH 4 DEFAULT '120'.
PARAMETERS p_c5 TYPE c LENGTH 4 DEFAULT '150'.
PARAMETERS p_c6 TYPE c LENGTH 4 DEFAULT  '180'.

SELECTION-SCREEN END OF LINE.



SELECTION-SCREEN END OF BLOCK zeitraum.


SELECTION-SCREEN BEGIN OF BLOCK bereich WITH FRAME TITLE text-005.

SELECT-OPTIONS material FOR marc-matnr MATCHCODE OBJECT mat1
                                       MEMORY ID mat.
SELECT-OPTIONS matkl FOR mara-matkl.

SELECT-OPTIONS mtart FOR mara-mtart OBLIGATORY .

SELECT-OPTIONS spart FOR mara-spart . "DIVISION

SELECT-OPTIONS maabc FOR marc-maabc.

SELECTION-SCREEN END OF BLOCK bereich.

*INCLUDE YINC_LAYOUT.

INITIALIZATION.

  repid = sy-repid.

*  PERFORM INITIALISIERUNG USING REPID.

AT SELECTION-SCREEN.
*  PERFORM PAI_OF_SELECTION_SCREEN.
  PERFORM validation.
*  perform val.
  PERFORM val1.

  PERFORM text_create.


AT SELECTION-SCREEN ON BLOCK objekte.

  PERFORM select_objekte.

AT SELECTION-SCREEN ON BLOCK zeitraum.
  PERFORM select_zeitraum.






START-OF-SELECTION.
  PERFORM check_auth_obj.  " Added by CS on 06.11.2015 for Authorization.

  PERFORM fuellen_tabellen.


* ---- Modification on Wednesday, August 23, 2017 14:56:56 to avoid Runtime Error in FM's using wertetab due to type mismatch---- *
* Developer: Saurabh Khare *
* Req: IRDK928916 *

* ---- Changed wertetab(bestang is an additional field) TO ==> wertetab_fm(structure(bco_werte) same as that of FM parameter) ---- *
  CALL FUNCTION 'AUFBAU_WERTETABELLE'
    EXPORTING
      flg_aobj     = aobj
*     alager       = xalager
*     lvorm        = xlvorm
    TABLES
      wertetabelle = wertetab_fm       " IRDK928916
      vkorg        = vkorg
      ekorg        = ekorg
      material     = material
      werk         = werke
      mtart        = mtart
      matkl        = matkl
      ekgrp        = ekgrp
      dismm        = dismm
      dispo        = dispo
      maabc        = maabc
    EXCEPTIONS
      not_found    = 01.

  CALL FUNCTION 'LESEN_MATERIALKURZTEXTE'
    TABLES
      wertetabelle = wertetab_fm.       " IRDK928916


  CALL FUNCTION 'BERECHNEN_BESTAND'
    EXPORTING
      flg_verbr    = 'X'
      flg_akt      = 'X'
      flg_mit      = ' '
      flg_min      = ' '
      datum_ab     = vondatum
      datum_bis    = bisdatum
    TABLES
      wertetabelle = wertetab_fm       " IRDK928916
*     xv134w       = xv134w
      x134m        = xt134m
      xt156c       = xt156c
      xt156m       = xt156m
      xt156s       = xt156s
      mjahr        = mjahr              " Pass this for performance improvement, need to create screen parameter
      gesverbrauch = verbr_mat.

  CALL FUNCTION 'VERBRAUCH_BCO'
    EXPORTING
      flg_verbr      = 'X'
      flg_kz         = 'L'
      flg_progn      = ' '
      vondatum       = vondatum
      bisdatum       = bisdatum
      flg_dispoverbr = 'X'
    TABLES
      wertetabelle   = wertetab_fm       " IRDK928916
      gesverbrauch   = verbr_mat
      berid_range    = berid_range.


  CALL FUNCTION 'WERTE_ZU_WERKSMATERIAL'
    EXPORTING
      mandantenwaehrung = space
      flg_aobj          = aobj
      flg_verbr         = 'S'
    TABLES
      wertetabelle      = wertetab_fm       " IRDK928916
    EXCEPTIONS
      gesamtwert_null   = 01.

* ---- Populate wertetab from wertetab_fm so that rest of the processing is unaffected(keep on using wertetab)---- *
  IF wertetab_fm IS NOT INITIAL.
    LOOP AT wertetab_fm INTO wertewa_fm.
      MOVE-CORRESPONDING wertewa_fm TO wertewa.
      APPEND wertewa TO wertetab.
      CLEAR: wertewa_fm, wertewa.
    ENDLOOP.
  ENDIF.
* ---- End of Modification ---- *

  LOOP AT wertetab.
    CLEAR behi.
    REFRESH behi.
*    PERFORM calculate_stock.
*    LOOP AT behi.
    it_finaltab-werks   = wertetab-werks.
    it_finaltab-matnr   = wertetab-matnr.
    IF wertetab-kein_verbrauch EQ 'X'.
      CLEAR it_finaltab-txt_days.
      it_finaltab-txt_days = text-030.
      it_finaltab-pendays = wertetab-hlp_ltzbew.
    ELSE.
      it_finaltab-pendays = wertetab-hlp_ltzbew.
    ENDIF.
    it_finaltab-pendstk = wertetab-akt_menge.

    CHECK wertetab-k_menge LE anztage.
    IF wertetab-k_menge LE p_c1.                            "'30'.
      it_finaltab-stock1   = wertetab-akt_menge.
    ELSEIF ( wertetab-k_menge GT p_c1 )                     "'30')
       AND ( wertetab-k_menge LE p_c2 ).                    "'60').
      it_finaltab-stock2   = wertetab-akt_menge.
    ELSEIF ( wertetab-k_menge GT p_c2 )                     "'60')
       AND ( wertetab-k_menge LE p_c3 ).                    "'90').
      it_finaltab-stock3   = wertetab-akt_menge.
    ELSEIF ( wertetab-k_menge GT p_c3 )                     "'90')
       AND ( wertetab-k_menge LE p_c4 ).                    "'120').
      it_finaltab-stock4   = wertetab-akt_menge.
    ELSEIF ( wertetab-k_menge GT p_c4 )                     "'120')
       AND ( wertetab-k_menge LE p_c5 ).                    "'150').
      it_finaltab-stock5   = wertetab-akt_menge.
    ELSEIF ( wertetab-k_menge GT p_c5 )                     "'150')
       AND ( wertetab-k_menge LE p_c6 ).                    "'180').
      it_finaltab-stock6   = wertetab-akt_menge.
    ELSEIF ( wertetab-k_menge GT p_c6 ) .                   "'180')
*       AND ( wertetab-k_menge LE '210').
      it_finaltab-stock7   = wertetab-akt_menge.
    ENDIF.

    APPEND it_finaltab.
    CLEAR it_finaltab.
  ENDLOOP.

  LOOP AT it_finaltab
  WHERE NOT ( stock1 IS INITIAL
              AND stock2 IS INITIAL
              AND stock3 IS INITIAL
              AND stock4 IS INITIAL
              AND stock5 IS INITIAL
              AND stock6 IS INITIAL
              AND stock7 IS INITIAL
              AND stock8 IS INITIAL
              AND stock9 IS INITIAL
              AND stock10 IS INITIAL
              AND stock11 IS INITIAL
              AND stock12 IS INITIAL
              AND stock13 IS INITIAL
              AND stock14 IS INITIAL ).
    MOVE-CORRESPONDING it_finaltab TO it_displaytab.
    APPEND it_displaytab.
    CLEAR : it_finaltab , it_displaytab.
  ENDLOOP.

  IF it_displaytab[] IS NOT INITIAL.
    SELECT matnr bwkey lbkum
           salk3 verpr
           FROM mbew
           INTO TABLE it_mbew
           FOR ALL ENTRIES IN it_displaytab
           WHERE matnr = it_displaytab-matnr
           AND   bwkey = it_displaytab-werks.
  ENDIF.



  CLEAR it_displaytab.
  LOOP AT it_displaytab.
    CLEAR wa_mbew.
    READ TABLE it_mbew INTO wa_mbew
      WITH KEY matnr = it_displaytab-matnr
               bwkey = it_displaytab-werks.
    IF sy-subrc = 0.
      it_displaytab-verpr = wa_mbew-verpr.   " Moving Avg Price

      IF it_displaytab-pendstk IS NOT INITIAL.
        ls_displaytab-pendstk = ls_displaytab-pendstk +
                                wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-pendstk * wa_mbew-verpr ).
      IF it_displaytab-stock1 IS NOT INITIAL.
*        ls_displaytab-stock1 = ls_displaytab-stock1 +
*                                wa_mbew-salk3.
        it_displaytab-value1 = wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-stock1 * wa_mbew-verpr ).
      IF it_displaytab-stock2 IS NOT INITIAL.
*        ls_displaytab-stock2 = ls_displaytab-stock2 +
*                                wa_mbew-salk3.
        it_displaytab-value2 = wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-stock2 * wa_mbew-verpr ).
      IF it_displaytab-stock3 IS NOT INITIAL.
*        ls_displaytab-stock3 = ls_displaytab-stock3 +
*                              wa_mbew-salk3.
        it_displaytab-value3 = wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-stock3 * wa_mbew-verpr ).
      IF it_displaytab-stock4 IS NOT INITIAL.
*        ls_displaytab-stock4 = ls_displaytab-stock4 +
*                              wa_mbew-salk3.
        it_displaytab-value4 = wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-stock4 * wa_mbew-verpr ).
      IF it_displaytab-stock5 IS NOT INITIAL.
*        ls_displaytab-stock5 = ls_displaytab-stock5 +
*                              wa_mbew-salk3.
        it_displaytab-value5 = wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-stock5 * wa_mbew-verpr ).
      IF it_displaytab-stock6 IS NOT INITIAL.
*        ls_displaytab-stock6 = ls_displaytab-stock6 +
*                              wa_mbew-salk3.
        it_displaytab-value6 = wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-stock6 * wa_mbew-verpr ).
      IF it_displaytab-stock7 IS NOT INITIAL.
*        ls_displaytab-stock7 = ls_displaytab-stock7 +
*                              wa_mbew-salk3.
        it_displaytab-value7 = wa_mbew-salk3.
      ENDIF.
      "( it_displaytab-stock7 * wa_mbew-verpr ).
*      IF it_displaytab-stock8 IS NOT INITIAL.
*        ls_displaytab-stock8 = ls_displaytab-stock8 +
*                              wa_mbew-salk3.
*        it_displaytab-value8 = wa_mbew-salk3.
*      ENDIF.
*      "( it_displaytab-stock8 * wa_mbew-verpr ).
**      IF it_displaytab-stock9 IS NOT INITIAL.
**        ls_displaytab-stock9 = ls_displaytab-stock9 +
**                              wa_mbew-salk3.
*        it_displaytab-value9 = wa_mbew-salk3.
**      ENDIF.
*      "( it_displaytab-stock9 * wa_mbew-verpr ).
**      IF it_displaytab-stock10 IS NOT INITIAL.
**        ls_displaytab-stock10 = ls_displaytab-stock10 +
**                              wa_mbew-salk3.
*        it_displaytab-value10 = wa_mbew-salk3.
**      ENDIF.
*      "( it_displaytab-stock10 * wa_mbew-verpr ).
*      IF it_displaytab-stock11 IS NOT INITIAL.
*        ls_displaytab-stock11 = ls_displaytab-stock11 +
*                              wa_mbew-salk3.
*        it_displaytab-value11 = wa_mbew-salk3.
*      ENDIF.
*      "( it_displaytab-stock11 * wa_mbew-verpr ).
*      IF it_displaytab-stock12 IS NOT INITIAL.
*        ls_displaytab-stock12 = ls_displaytab-stock12 +
*                              wa_mbew-salk3.
*        it_displaytab-value12 = wa_mbew-salk3.
*      ENDIF.
*      "( it_displaytab-stock12 * wa_mbew-verpr ).
*      IF it_displaytab-stock13 IS NOT INITIAL.
*        ls_displaytab-stock13 = ls_displaytab-stock13 +
*                              wa_mbew-salk3.
*        it_displaytab-value13 = wa_mbew-salk3.
*      ENDIF.
*      "( it_displaytab-stock13 * wa_mbew-verpr ).
*      IF it_displaytab-stock14 IS NOT INITIAL.
*        ls_displaytab-stock14 = ls_displaytab-stock14 +
*                              wa_mbew-salk3.
*        it_displaytab-value14 = wa_mbew-salk3.
*      ENDIF.
*      "( it_displaytab-stock14 * wa_mbew-verpr ).

      MODIFY it_displaytab
       TRANSPORTING verpr   value1  value2  value3  value4  value5
                    value6  value7
*                    value8  value9  value10 value11
*                    value12 value13 value14
           WHERE matnr = it_displaytab-matnr
             AND werks = it_displaytab-werks.
    ENDIF.
  ENDLOOP.

  REFRESH it_displaytab1[].
  LOOP AT it_displaytab.
    CLEAR ls_displaytab1.
    MOVE-CORRESPONDING it_displaytab TO ls_displaytab1.
    APPEND ls_displaytab1 TO it_displaytab1.
  ENDLOOP.


  REFRESH it_displaytabf.
  LOOP AT it_displaytab1.

    MOVE-CORRESPONDING it_displaytab1 TO it_displaytabf.

*    WRITE IT_DISPLAYTAB1-PENDAYS TO IT_DISPLAYTABF-PENDAYS.
*    CONDENSE IT_DISPLAYTABF-PENDAYS.


    SELECT SINGLE * FROM makt INTO wa_makt
    WHERE matnr = it_displaytabf-matnr AND
          spras = 'EN'.

    IF sy-subrc = 0.
      it_displaytabf-maktx = wa_makt-maktx.

    ENDIF.


    APPEND it_displaytabf.

    CLEAR: wa_makt,it_displaytab1,it_displaytabf.
  ENDLOOP.



*  IF ls_displaytab IS NOT INITIAL.
**    refresh it_displaytab1.
*    MOVE 'Tot:' TO ls_displaytab-werks.
*    ls_displaytab-col = 'C311'.
*    CONDENSE : ls_displaytab-pendstk,
*               ls_displaytab-stock1,
*               ls_displaytab-stock12,
*               ls_displaytab-stock3,
*               ls_displaytab-stock4,
*               ls_displaytab-stock5,
*               ls_displaytab-stock6,
*               ls_displaytab-stock7,
*               ls_displaytab-stock8,
*               ls_displaytab-stock9,
*               ls_displaytab-stock10,
*               ls_displaytab-stock11,
*               ls_displaytab-stock12,
*               ls_displaytab-stock13,
*               ls_displaytab-stock14.
*    APPEND ls_displaytab TO it_displaytab1.
*  ENDIF.


***** Start Code: Added by CS on 06.11.2015 for Authorization Message. *****
*  PERFORM display_alv. " Commented by CS
  IF it_displaytabf[] IS NOT INITIAL.
    IF lv_werks_auth_flg EQ 'X' OR lv_mtart_auth_flg EQ 'X'.
      MESSAGE 'Missing Authorization.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
    PERFORM display_alv.
  ELSE.
    IF lv_werks_auth_flg EQ 'X' OR lv_mtart_auth_flg EQ 'X'.
      MESSAGE 'No Record Found! or Missing Authorization.' TYPE 'I'.
    ELSE.
      MESSAGE 'No Record Found.' TYPE 'I'.
    ENDIF.
    LEAVE LIST-PROCESSING.
  ENDIF.
***** End Code: Added by CS on 06.11.2015 for Authorization Message. *****


*&---------------------------------------------------------------------*
*&      Form  FUELLEN_TABELLEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fuellen_tabellen .
  CLEAR xt134m. REFRESH xt134m.
  CLEAR xt156s. REFRESH xt156s.
  CLEAR xt156m. REFRESH xt156m.
  CLEAR xt156c. REFRESH xt156c.
  CLEAR mjahr.  REFRESH mjahr.

* select * from v134w into table xv134w order by mandt werks mtart.
  SELECT * FROM t134m INTO TABLE xt134m ORDER BY PRIMARY KEY.
* SELECT * FROM T156S INTO TABLE XT156S ORDER BY PRIMARY KEY.
  SELECT * FROM v156s INTO TABLE xt156s ORDER BY
     mandt bwart wertu mengu sobkz kzbew kzzug kzvbr.
  SELECT * FROM t156m INTO TABLE xt156m ORDER BY PRIMARY KEY.
  SELECT * FROM t156c INTO TABLE xt156c ORDER BY PRIMARY KEY.

ENDFORM.                    " FUELLEN_TABELLEN

*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJEKTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_objekte .

  CLEAR aobj.

  READ TABLE vkorg INDEX 1.
  IF sy-subrc EQ 0.
    IF NOT aobj IS INITIAL.
      MESSAGE e200.
*   Bitte nur ein Analyse-Objekt wählen
    ENDIF.
    aobj = 'V'.
  ENDIF.
  READ TABLE ekorg INDEX 1.
  IF sy-subrc EQ 0.
    IF NOT aobj IS INITIAL.
      MESSAGE e200.
*   Bitte nur ein Analyse-Objekt wählen
    ENDIF.
    aobj = 'E'.
  ENDIF.
  READ TABLE werke INDEX 1.
  IF sy-subrc EQ 0.
    IF NOT aobj IS INITIAL.
      MESSAGE e200.
*   Bitte nur ein Analyse-Objekt wählen
    ENDIF.
    aobj = 'W'.
*    dreizlg = true.
  ENDIF.

  IF aobj IS INITIAL.
    MESSAGE e001.
  ENDIF.

ENDFORM.                    " SELECT_OBJEKTE

*&---------------------------------------------------------------------*
*&      Form  SELECT_ZEITRAUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_zeitraum .
  IF anztage > 36500.
    anztage = 36500.
  ENDIF.

  IF anztage > 0.
    bisdatum = sy-datlo.
*    vondatum = sy-datlo - anztage.
    vondatum = '20080101'.
  ENDIF.

ENDFORM.                    " SELECT_ZEITRAUM

*&---------------------------------------------------------------------*
*&      Form  CALCULATE_STOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_stock .

*  PERFORM fuellen_tabellen.
*  PERFORM belegsegment_lesen.
*  PERFORM lagerbestand_ermitteln.
*  PERFORM lagerbestandsverlauf.
*  PERFORM werte_kumulieren.
*
ENDFORM.                    " CALCULATE_STOCK

*&---------------------------------------------------------------------*
*&      Form  BELEGSEGMENT_LESEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM belegsegment_lesen .
  DATA:   ls_mcmseg   LIKE mcmseg,
          ls_mcmseg_l LIKE mcmseg,
          ls_mcmseg_k LIKE mcmseg,
          lv_modus    LIKE mccontrol-modus VALUE 'B'.

  CLEAR xmseg.
*  IF bzb_start = true.
  PERFORM sobkz_fuellen.
*    bzb_start = false.
*  ENDIF.

*  LOOP AT wertetab.
  SELECT * FROM mseg
    INTO TABLE xmseg
*     FOR ALL ENTRIES IN wertetab
   WHERE matnr = wertetab-matnr
     AND werks EQ wertetab-werks
     AND sobkz IN sobkz.
*  ENDLOOP.

  READ TABLE xmseg INDEX 1.
  IF sy-subrc = 0.
    LOOP AT xmseg.
      CLEAR ls_mcmseg.
      CLEAR ls_mcmseg_l.
      CLEAR ls_mcmseg_k.
      MOVE-CORRESPONDING xmseg TO ls_mcmseg_l.

      CALL FUNCTION 'MCB_DETERMINE_STOCKTYPE'
        EXPORTING
          i_mseg     = xmseg
*         i_mcmseg   = ls_mcmseg
          i_mcmseg   = ls_mcmseg_l
          i_modus    = lv_modus
        IMPORTING
*         e_mcmseg_l = ls_mcmseg
          e_mcmseg_l = ls_mcmseg_l
          e_mcmseg_k = ls_mcmseg_k
        TABLES
          zt156      = xt156
          zt156b     = xt156b
          zt156s     = xt156s
          zt156m     = xt156m
          zt156c     = xt156c
          zmseg      = xmseg.

      DO 2 TIMES.
        IF sy-index EQ 1.
          MOVE ls_mcmseg_l TO ls_mcmseg.
        ELSE.
          MOVE ls_mcmseg_k TO ls_mcmseg.
        ENDIF.

        CHECK NOT ls_mcmseg IS INITIAL.

*--- Alle verbrauchsrelevanten Bewegungen
        IF NOT ls_mcmseg-xlabst IS INITIAL OR
           NOT ls_mcmseg-xinsme IS INITIAL OR
           NOT ls_mcmseg-xumlmc IS INITIAL OR
           NOT ls_mcmseg-xtrame IS INITIAL OR
           NOT ls_mcmseg-xspeme IS INITIAL OR
           NOT ls_mcmseg-xmklkk IS INITIAL OR
           NOT ls_mcmseg-xmklkl IS INITIAL OR
           NOT ls_mcmseg-xmkqkk IS INITIAL OR
           NOT ls_mcmseg-xmkqkl IS INITIAL OR
           NOT ls_mcmseg-xmslbo IS INITIAL OR
           NOT ls_mcmseg-xmsqbo IS INITIAL OR
           NOT ls_mcmseg-xeinme IS INITIAL OR
           NOT ls_mcmseg-xmkekk IS INITIAL OR
           NOT ls_mcmseg-xmkekl IS INITIAL OR
           NOT ls_mcmseg-xmsebo IS INITIAL OR
           NOT ls_mcmseg-xglgmg IS INITIAL.

          SELECT SINGLE * FROM mkpf WHERE mblnr = xmseg-mblnr
                                     AND  mjahr = xmseg-mjahr.

          CLEAR behi.
          IF ls_mcmseg-shkzg = 'S'.
            MOVE xmseg-menge TO behi-zugang.
          ELSE.
            MOVE xmseg-menge TO behi-abgang.
          ENDIF.
          IF NOT ls_mcmseg-xstbw IS INITIAL.
            IF behi-zugang > 0.
              behi-abgang = - behi-zugang.
              CLEAR behi-zugang.
            ELSEIF behi-abgang > 0.
              behi-zugang = - behi-abgang.
              CLEAR behi-abgang.
            ENDIF.
          ENDIF.

          MOVE: mkpf-budat TO behi-perio.
          COLLECT behi.
          MOVE: mkpf-budat  TO bind-budat,
                mkpf-mblnr  TO bind-mblnr,
                mkpf-mjahr  TO bind-mjahr,
                xmseg-zeile TO bind-zeile,
                xmseg-lgort TO bind-lgort,
                xmseg-werks TO bind-werks,
                xmseg-menge TO bind-menge.
          APPEND bind.
        ENDIF.
      ENDDO.
    ENDLOOP.
*    DESCRIBE TABLE behi LINES behi_maxrow.
    SORT behi BY perio.
    SORT bind BY budat.
  ELSE.
*    RAISE no_material_movements.
  ENDIF.

ENDFORM.                    " BELEGSEGMENT_LESEN

*&---------------------------------------------------------------------*
*&      Form  SOBKZ_FUELLEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sobkz_fuellen .
  CLEAR sobkz. REFRESH sobkz.

  sobkz-low     = space.
  sobkz-high    = space.
  sobkz-option  = 'EQ'.
  sobkz-sign    = 'I'.
  APPEND sobkz.

  sobkz-low     = 'O'.
  sobkz-high    = 'O'.
  sobkz-option  = 'EQ'.
  sobkz-sign    = 'I'.
  APPEND sobkz.

  sobkz-low     = 'V'.
  sobkz-high    = 'V'.
  sobkz-option  = 'EQ'.
  sobkz-sign    = 'I'.
  APPEND sobkz.

  sobkz-low     = 'W'.
  sobkz-high    = 'W'.
  sobkz-option  = 'EQ'.
  sobkz-sign    = 'I'.
  APPEND sobkz.

  sobkz-low     = 'K'.
  sobkz-high    = 'K'.
  sobkz-option  = 'EQ'.
  sobkz-sign    = 'I'.
  APPEND sobkz.

ENDFORM.                    " SOBKZ_FUELLEN

*&---------------------------------------------------------------------*
*&      Form  LAGERBESTAND_ERMITTELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lagerbestand_ermitteln .


  DATA: le_v VALUE 'V',
        le_w VALUE 'W',
        le_o VALUE 'O'.

  bestand = 0.
*  LOOP AT wertetab.
* Lagerortbestaende
  SELECT * FROM mard WHERE matnr = wertetab-matnr
                      AND  werks = wertetab-werks.
    bestand = bestand + mard-labst.
    bestand = bestand + mard-insme.
    bestand = bestand + mard-speme.
    bestand = bestand + mard-umlme.
    bestand = bestand + mard-einme.
  ENDSELECT.
* Umlagerungsbestand
  SELECT * FROM marc WHERE matnr = wertetab-matnr
                      AND  werks = wertetab-werks.
    bestand = bestand + marc-umlmc.
    bestand = bestand + marc-trame.
  ENDSELECT.
* Kundensonderbestaende bewertet
  SELECT * FROM msku WHERE matnr = wertetab-matnr
                      AND  werks = wertetab-werks
                      AND ( sobkz = le_v OR
                            sobkz = le_w ).
    bestand = bestand + msku-kulab.
    bestand = bestand + msku-kuins.
    bestand = bestand + msku-kuein.
  ENDSELECT.
* Lieferantensonderbestaende bewertet
  SELECT * FROM mslb WHERE matnr = wertetab-matnr
                      AND  werks = wertetab-werks
                      AND  sobkz = le_o.
    bestand = bestand + mslb-lblab.
    bestand = bestand + mslb-lbins.
    bestand = bestand + mslb-lbein.
  ENDSELECT.
  wertetab-bestand = bestand.

  MODIFY wertetab
  TRANSPORTING bestand
  WHERE matnr = wertetab-matnr
  AND werks = wertetab-werks.

*  ENDLOOP.
ENDFORM.                    " LAGERBESTAND_ERMITTELN

*&---------------------------------------------------------------------*
*&      Form  LAGERBESTANDSVERLAUF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lagerbestandsverlauf .
  DATA : behiindex TYPE p.

  DESCRIBE TABLE behi LINES behi_maxrow.
  behiindex = behi_maxrow.
  READ TABLE behi INDEX behiindex.

  IF behi-perio <> sy-datlo.
    CLEAR behi.
*   behi-labst = labst.
    behi-labst = bestand.
*   behi-perio = sy-datum.
    behi-perio = sy-datlo.
    APPEND behi.
    behi_maxrow = behi_maxrow + 1.
    behiindex = behiindex + 1.
  ELSE.
*   BEHI-LABST = LABST.
    behi-labst = bestand.
    MODIFY behi INDEX behiindex.
*   LABST = LABST - BEHI-ZUGANG + BEHI-ABGANG.
    bestand = bestand - behi-zugang + behi-abgang.
  ENDIF.

*-------- Lagerbestaende der Vorperioden berechnen ---------------------
  DO.
    behiindex = behiindex - 1.
    IF behiindex  = 0.
      EXIT.
    ENDIF.
    READ TABLE behi INDEX behiindex.
*   BEHI-LABST = LABST.
    behi-labst = bestand.
    MODIFY behi INDEX behiindex.
*   LABST = LABST - BEHI-ZUGANG + BEHI-ABGANG.
    bestand = bestand - behi-zugang + behi-abgang.
  ENDDO.
ENDFORM.                    " LAGERBESTANDSVERLAUF

*&---------------------------------------------------------------------*
*&      Form  WERTE_KUMULIEREN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM werte_kumulieren .
  DATA: abgangv LIKE mard-labst VALUE 0,
        zugangv LIKE mard-labst VALUE 0,
        hnvalue TYPE f,
        hcvalue TYPE f.

  LOOP AT behi.
    behi-zugangk = behi-zugang + zugangv.
    behi-abgangk = behi-abgang + abgangv.
    MODIFY behi.

**-------- hoechste Werte fuer Skaliereung ermitteln--------------------
    IF behi-zugang  >= hnvalue. hnvalue = behi-zugang.  ENDIF.
    IF behi-abgang  >= hnvalue. hnvalue = behi-abgang.  ENDIF.
    IF behi-labst   >= hnvalue. hnvalue = behi-labst.   ENDIF.
    IF behi-zugangk >= hcvalue. hcvalue = behi-zugangk. ENDIF.
    IF behi-abgangk >= hcvalue. hcvalue = behi-abgangk. ENDIF.

    zugangv = behi-zugangk.
    abgangv = behi-abgangk.
  ENDLOOP.

ENDFORM.                    " WERTE_KUMULIEREN

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
  PERFORM get_fieldcat.
*  PERFORM get_sorttab.
  PERFORM get_layout.
  PERFORM display_grid.
ENDFORM.                    " DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fieldcat .
  DATA: ls_fieldcat  TYPE slis_fieldcat_alv,
        ls_fieldcatc TYPE slis_fieldcat_alv.

**
**  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
**    EXPORTING
**      i_program_name         = 'ZINVENTORY_AGING_REPORT_T'
**      i_internal_tabname     = 'IT_DISPLAYTABF'
**      i_inclname             = 'ZINVENTORY_AGING_REPORT_T'
**    CHANGING
**      ct_fieldcat            = wt_fieldcat
**    EXCEPTIONS
**      inconsistent_interface = 1
**      program_error          = 2
**      OTHERS                 = 3.
**  IF sy-subrc <> 0.
**    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**  ENDIF.

*  LOOP AT wt_fieldcat INTO ls_fieldcat.
*    CASE ls_fieldcat-fieldname.


**        clear LS_FIELDCAT.
**        LS_FIELDCAT-FIELDNAME = 'werks'. "PLANT
**        LS_FIELDCAT-TABNAME = 'it_displaytabf'.
**        LS_FIELDCAT-REPTEXT_DDIC = 'pLANT'(006).
**        LS_FIELDCAT-SELTEXT_L = 'pLANT'(006).
**        LS_FIELDCAT-SELTEXT_M = 'pLANT'(006).
**        LS_FIELDCAT-SELTEXT_S = 'pLANT'(006).
**        LS_FIELDCAT-COL_POS = 1.
**        LS_FIELDCAT-OUTPUTLEN = 8.
**        append ls_fieldcat to wt_fieldcat.

  ls_fieldcat-fieldname = 'WERKS'. "plant Name
  ls_fieldcat-outputlen = 8.
  ls_fieldcat-seltext_s = 'Plant'.
  ls_fieldcat-seltext_m = 'Plant'.
  ls_fieldcat-seltext_l = 'Plant'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-key = 'X'.
  APPEND ls_fieldcat TO wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NAME2'. "plant Name
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-seltext_s = 'Plant Name'.
  ls_fieldcat-seltext_m = 'Plant Name'.
  ls_fieldcat-seltext_l = 'Plant Name'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-key = 'X'.
  APPEND ls_fieldcat TO wt_fieldcat.

*        CLEAR ls_fieldcat.
*        ls_fieldcat-fieldname = 'MATNR'.
*        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
*        ls_fieldcat-reptext_ddic = 'Material No'(007).
*        ls_fieldcat-seltext_l = 'Material No'(007).
*        ls_fieldcat-seltext_m = 'Material No'(007).
*        ls_fieldcat-seltext_s = 'Material No'(007).
*        ls_fieldcat-col_pos = 2.
*        ls_fieldcat-outputlen = 14.
*        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATNR'. "plant Name
  ls_fieldcat-outputlen = 14.
  ls_fieldcat-seltext_s = 'Material No'.
  ls_fieldcat-seltext_m = 'Material No'.
  ls_fieldcat-seltext_l = 'Material No'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
  ls_fieldcat-key = 'X'.
  APPEND ls_fieldcat TO wt_fieldcat.


**        CLEAR ls_fieldcat.
**        ls_fieldcat-fieldname = 'MAKTX'.
**        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
**        ls_fieldcat-reptext_ddic = 'Material Description'.
**        ls_fieldcat-seltext_l = 'Material Description'.
**        ls_fieldcat-seltext_m = 'Material Description'.
**        ls_fieldcat-seltext_s = 'Material Description'.
**        ls_fieldcat-col_pos = 3.
**        ls_fieldcat-outputlen = 40.
**        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MAKTX'.
  ls_fieldcat-outputlen = 40.
  ls_fieldcat-seltext_s =  'Material Description'.
  ls_fieldcat-seltext_m =  'Material Description'.
  ls_fieldcat-seltext_l =  'Material Description'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
*        LS_FIELDCAT-KEY = 'X'.
  APPEND ls_fieldcat TO wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MTART'.
  ls_fieldcat-outputlen = 4.
  ls_fieldcat-seltext_s =  'Type'.
  ls_fieldcat-seltext_m =  'Mat Type'.
  ls_fieldcat-seltext_l =  'Material Type'.
  ls_fieldcat-ddictxt   = 'S'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
  APPEND ls_fieldcat TO wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MTBEZ'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-seltext_s =  'Type'.
  ls_fieldcat-seltext_m =  'Mat Type'.
  ls_fieldcat-seltext_l =  'Material Type'.
  ls_fieldcat-ddictxt   = 'L'.
  ls_fieldcat-no_out   = 'X'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
  APPEND ls_fieldcat TO wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SPART'.
  ls_fieldcat-outputlen = 2.
  ls_fieldcat-seltext_s =  'Div'.
  ls_fieldcat-seltext_m =  'Division'.
  ls_fieldcat-seltext_l =  'Division'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
  APPEND ls_fieldcat TO wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VTEXT'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-seltext_s =  'Division'.
  ls_fieldcat-seltext_m =  'Division'.
  ls_fieldcat-seltext_l =  'Division'.
  ls_fieldcat-ddictxt   = 'L'.
  ls_fieldcat-no_out   = 'X'.
  ls_fieldcat-inttype   = 'C'.
  ls_fieldcat-datatype  = 'CHAR'.
  APPEND ls_fieldcat TO wt_fieldcat.

*      WHEN 'PENDAYS'.
**        ls_fieldcat-reptext_ddic = 'Pending for days'(008).
*        ls_fieldcat-seltext_l = 'Pending for days'(008).
**        ls_fieldcat-seltext_m = 'Pending for days'(008).
**        ls_fieldcat-seltext_s = 'Pending for days'(008).
*        ls_fieldcat-col_pos = 3.
*        ls_fieldcat-outputlen = 16.
*
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PENDAYS'.
*        LS_FIELDCAT-OUTPUTLEN = 10.
  ls_fieldcat-seltext_s =  'Pending for days'.
  ls_fieldcat-seltext_m =  'Pending for days'.
  ls_fieldcat-seltext_l =  'Pending for days'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'INT2'.
  ls_fieldcat-ref_tabname   = 'WERTETAB'.
  ls_fieldcat-ref_fieldname = 'HLP_LTZBEW'.
  APPEND ls_fieldcat TO wt_fieldcat.

*        CLEAR ls_fieldcat.
*        ls_fieldcat-fieldname = 'TXT_DAYS'.
*        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
*        ls_fieldcat-reptext_ddic = 'Consumption Status'(031).
*        ls_fieldcat-seltext_l = 'Consumption Status'(031).
*        ls_fieldcat-seltext_m = 'Consumption Status'(031).
*        ls_fieldcat-seltext_s = 'Consumption Status'(031).
*        ls_fieldcat-col_pos = 5.
*        ls_fieldcat-outputlen = 16.
*        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TXT_DAYS'.
  ls_fieldcat-outputlen = 16.
  ls_fieldcat-seltext_s =  'Consumption Status'.
  ls_fieldcat-seltext_m =  'Consumption Status'.
  ls_fieldcat-seltext_l =  'Consumption Status'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CHAR'.
  APPEND ls_fieldcat TO wt_fieldcat.


*        CLEAR ls_fieldcat.
*        ls_fieldcat-fieldname = 'PENDSTK'.
*        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
*        ls_fieldcat-reptext_ddic = 'Pending Stock Qty'(009).
*        ls_fieldcat-seltext_l = 'Pending Stock Qty'(009).
*        ls_fieldcat-seltext_m = 'Pending Stock Qty'(009).
*        ls_fieldcat-seltext_s = 'Pending Stock Qty'(009).
*        ls_fieldcat-col_pos = 6.
*        ls_fieldcat-outputlen = 20.
*        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PENDSTK'.
  ls_fieldcat-outputlen = 20.
  ls_fieldcat-seltext_s =  'Pending Stock Qty'.
  ls_fieldcat-seltext_m =  'Pending Stock Qty'.
  ls_fieldcat-seltext_l =  'Pending Stock Qty'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'QUAN'.
  APPEND ls_fieldcat TO wt_fieldcat.


**        CLEAR ls_fieldcat.
**        ls_fieldcat-fieldname = 'VERPR'.
**        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
**        ls_fieldcat-reptext_ddic = 'Moving Avg Price'.
**        ls_fieldcat-seltext_l = 'Moving Avg Price'.
**        ls_fieldcat-seltext_m = 'Moving Avg Price'.
**        ls_fieldcat-seltext_s = 'Moving Avg Price'.
**        ls_fieldcat-col_pos = 7.
**        ls_fieldcat-outputlen = 20.
**        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VERPR'.
  ls_fieldcat-outputlen = 20.
  ls_fieldcat-seltext_s =  'Moving Avg Price'.
  ls_fieldcat-seltext_m =  'Moving Avg Price'.
  ls_fieldcat-seltext_l =  'Moving Avg Price'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.

*if not  p_c1 is initial.
*        CLEAR ls_fieldcat.
*        ls_fieldcat-fieldname = 'VALUE1'.
*        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
*        ls_fieldcat-reptext_ddic = t1. "p_c1..  " '0-30 Value'.
*        ls_fieldcat-seltext_l = t1. "p_c1.  "'0-30 Value'.
*        ls_fieldcat-seltext_m = t1 .  "p_c1.   "'0-30 Value'.
*        ls_fieldcat-seltext_s = t1. "p_c1.  "'0-30 Value'.
*        ls_fieldcat-col_pos = 8.
*        ls_fieldcat-outputlen = 13.
*        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE1'.
  ls_fieldcat-outputlen = 13.
  ls_fieldcat-seltext_l = t1. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-seltext_m = t1 .  "P_C1.   "'0-30 VALUE'.
  ls_fieldcat-seltext_s = t1. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.
*endif.

*if not  p_c2 is initial.
*        CLEAR ls_fieldcat.
*        ls_fieldcat-fieldname = 'VALUE2'.
*        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
*        ls_fieldcat-reptext_ddic = t2.  "'31-60 Value'.
*        ls_fieldcat-seltext_l = t2.  "'31-60 Value'.
*        ls_fieldcat-seltext_m = t2.  "'31-60 Value'.
*        ls_fieldcat-seltext_s = t2 . "'31-60 Value'.
*        ls_fieldcat-col_pos = 9.
*        ls_fieldcat-outputlen = 13.
*        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE2'.
  ls_fieldcat-outputlen = 13.
  ls_fieldcat-seltext_l = t2. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-seltext_m = t2 .  "P_C1.   "'0-30 VALUE'.
  ls_fieldcat-seltext_s = t2. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.
*endif.

*if not  p_c3 is initial.
**        CLEAR ls_fieldcat.
**        ls_fieldcat-fieldname = 'VALUE3'.
**        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
**        ls_fieldcat-reptext_ddic = t3.  "'61-90 Value'.
**        ls_fieldcat-seltext_l = t3.  "'61-90 Value'.
**        ls_fieldcat-seltext_m = t3.  "'61-90 Value'.
**        ls_fieldcat-seltext_s = t3.  "'61-90 Value'.
**        ls_fieldcat-col_pos = 10.
**        ls_fieldcat-outputlen = 13.
**        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE3'.
  ls_fieldcat-outputlen = 13.
  ls_fieldcat-seltext_l = t3. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-seltext_m = t3 .  "P_C1.   "'0-30 VALUE'.
  ls_fieldcat-seltext_s = t3. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.
*endif.
*if not  p_c4 is initial.
**        CLEAR ls_fieldcat.
**        ls_fieldcat-fieldname = 'VALUE4'.
**        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
**        ls_fieldcat-reptext_ddic = t4.  "'91-120 Value'.
**        ls_fieldcat-seltext_l = t4.  "'91-120 Value'.
**        ls_fieldcat-seltext_m = t4.  "'91-120 Value'.
**        ls_fieldcat-seltext_s = t4.  "'91-120 Value'.
**        ls_fieldcat-col_pos = 11.
**        ls_fieldcat-outputlen = 13.
**        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE4'.
  ls_fieldcat-outputlen = 13.
  ls_fieldcat-seltext_l = t4. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-seltext_m = t4 .  "P_C1.   "'0-30 VALUE'.
  ls_fieldcat-seltext_s = t4. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.
*endif.
*if not  p_c5 is initial.
**        CLEAR ls_fieldcat.
**        ls_fieldcat-fieldname = 'VALUE5'.
**        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
**        ls_fieldcat-reptext_ddic = p_c5.   "'121-150 Value'.
**        ls_fieldcat-seltext_l = t5.   "'121-150 Value'.
**        ls_fieldcat-seltext_m = t5.   "'121-150 Value'.
**        ls_fieldcat-seltext_s = t5.   "'121-150 Value'.
**        ls_fieldcat-col_pos = 12.
**        ls_fieldcat-outputlen = 13.
**        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE5'.
  ls_fieldcat-outputlen = 13.
  ls_fieldcat-seltext_l = t5. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-seltext_m = t5 .  "P_C1.   "'0-30 VALUE'.
  ls_fieldcat-seltext_s = t5. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.
*endif.
*if not  p_c6 is initial.
**        CLEAR ls_fieldcat.
**        ls_fieldcat-fieldname = 'VALUE6'.
**        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
**        ls_fieldcat-reptext_ddic = p_c6.  "'151-180 Value'.
**        ls_fieldcat-seltext_l = t6.  "'151-180 Value'.
**        ls_fieldcat-seltext_m = t6.  "'151-180 Value'.
**        ls_fieldcat-seltext_s = t6.  "'151-180 Value'.
**        ls_fieldcat-col_pos = 13.
**        ls_fieldcat-outputlen = 13.
**        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE6'.
  ls_fieldcat-outputlen = 13.
  ls_fieldcat-seltext_l = t6. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-seltext_m = t6 .  "P_C1.   "'0-30 VALUE'.
  ls_fieldcat-seltext_s = t6. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.
*endif.
*if not  p_c6 is initial.
*        CLEAR ls_fieldcat.
*        ls_fieldcat-fieldname = 'VALUE7'.
*        ls_fieldcat-tabname = 'IT_DISPLAYTABF'.
*        ls_fieldcat-reptext_ddic = t7.  "'181-210 Value'.
*        ls_fieldcat-seltext_l = t7.   "181-210 Value'.
*        ls_fieldcat-seltext_m = t7 .  "'181-210 Value'.
*        ls_fieldcat-seltext_s = t7 . "'181-210 Value'.
*        ls_fieldcat-col_pos = 14.
*        ls_fieldcat-outputlen = 13.
*        append ls_fieldcat to wt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VALUE7'.
  ls_fieldcat-outputlen = 13.
  ls_fieldcat-seltext_l = t7. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-seltext_m = t7 .  "P_C1.   "'0-30 VALUE'.
  ls_fieldcat-seltext_s = t7. "P_C1.  "'0-30 VALUE'.
  ls_fieldcat-ddictxt   = 'M'.
  ls_fieldcat-datatype  = 'CURR'.
  APPEND ls_fieldcat TO wt_fieldcat.

*endif
*    ENDCASE.
*    MODIFY wt_fieldcat
*    FROM ls_fieldcat.
*  ENDLOOP.



  IF p_c1 IS INITIAL.
*delete TABLE wt_fieldcat with table key FIELDNAME = 'VALUE1'.
    DELETE  wt_fieldcat WHERE fieldname = 'VALUE1'.
  ENDIF.


  IF p_c2 IS INITIAL.
    DELETE  wt_fieldcat WHERE fieldname = 'VALUE2'.
  ENDIF.

  IF  p_c3 IS INITIAL.
    DELETE  wt_fieldcat WHERE fieldname = 'VALUE3'.
  ENDIF.

  IF p_c4 IS INITIAL.
    DELETE  wt_fieldcat WHERE fieldname = 'VALUE4'.
  ENDIF.

  IF  p_c5 IS INITIAL.
    DELETE  wt_fieldcat WHERE fieldname = 'VALUE5'.
  ENDIF.

  IF  p_c6 IS INITIAL.
    DELETE  wt_fieldcat WHERE fieldname = 'VALUE6'.

  ENDIF.

  IF  p_c1 IS INITIAL AND
      p_c2 IS INITIAL AND
      p_c3 IS INITIAL AND
      p_c4 IS INITIAL AND
      p_c5 IS INITIAL AND
      p_c6 IS INITIAL.


    DELETE  wt_fieldcat WHERE fieldname = 'VALUE7'.

  ENDIF.




*  wt_fieldcatc[] = wt_fieldcat[].
*
*  loop at wt_fieldcatc INTO  ls_fieldcatc.
*
*    read table wt_fieldcat into  ls_fieldcat
*    with key fieldname = ls_fieldcatc-fieldname.
*    if sy-subrc = 0.
*
*      if ls_fieldcat-fieldname = 'VALUE1' AND P_C1 = SPACE.
*        DELETE wt_fieldcat index sy-tabix. "from ls_fieldcat.
*      ENDIF.
*
*      if ls_fieldcat-fieldname = 'VALUE2' AND P_C2 = SPACE.
*        DELETE wt_fieldcat index sy-tabix. "from ls_fieldcat.
*      ENDIF.
*
*      if ls_fieldcat-fieldname = 'VALUE3' AND P_C3 = SPACE.
*        DELETE wt_fieldcat index sy-tabix. "from ls_fieldcat.
*      ENDIF.
*      if ls_fieldcat-fieldname = 'VALUE4' AND P_C4 = SPACE.
*        DELETE wt_fieldcat index sy-tabix. "from ls_fieldcat.
*      ENDIF.
*      if ls_fieldcat-fieldname = 'VALUE5' AND P_C5 = SPACE.
*        DELETE wt_fieldcat index sy-tabix. "from ls_fieldcat.
*      ENDIF.
*      if ls_fieldcat-fieldname = 'VALUE6' AND P_C6 = SPACE.
*        DELETE wt_fieldcat index sy-tabix. "from ls_fieldcat.
*      ENDIF.
*
*   endif.
*    clear: ls_fieldcat,ls_fieldcatc.
*  endloop.


ENDFORM.                    " GET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_grid .
  DATA:lwa_layout TYPE slis_layout_alv.
  lwa_layout-zebra             = 'X'.
  lwa_layout-colwidth_optimize = 'X'.

  break bil-abap01.

  IF it_displaytabf[] IS NOT INITIAL.
    CLEAR it_matnr[].
    SELECT
            a~matnr
            a~spart
            d~vtext
            a~mtart
            t~mtbez
        FROM mara AS a
        INNER JOIN tspat AS d
          ON a~spart EQ d~spart
          AND d~spras EQ 'E'
        INNER JOIN t134t AS t
          ON a~mtart EQ t~mtart
          AND t~spras EQ 'E'
       INTO TABLE it_matnr
        WHERE a~spart IN spart.


*************    GETTING PLANT NAME ************************
    SELECT werks
           name2
        FROM t001w
      INTO TABLE it_plant
    WHERE werks IN werke.

    LOOP AT it_displaytabf.
      READ TABLE it_matnr WITH KEY matnr = it_displaytabf-matnr .
      IF sy-subrc IS NOT INITIAL.
        it_displaytabf-flg = 'X'.
        MODIFY it_displaytabf TRANSPORTING flg.
      ELSE.
        it_displaytabf-spart = it_matnr-spart.
        it_displaytabf-vtext = it_matnr-vtext.
        it_displaytabf-mtart = it_matnr-mtart.
        it_displaytabf-mtbez = it_matnr-mtbez.

        READ TABLE it_plant WITH KEY werks  = it_displaytabf-werks.
        IF sy-subrc IS INITIAL.
          it_displaytabf-name2 = it_plant-name2.
        ENDIF.
        MODIFY it_displaytabf TRANSPORTING spart vtext mtart mtbez name2.
      ENDIF.
    ENDLOOP.
    DELETE it_displaytabf WHERE flg EQ 'X'.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     i_bypassing_buffer      = 'X'
*     i_buffer_active         = ' '
      i_callback_program      = sy-repid  "sy-cprog
      i_callback_user_command = 'SUB_USER_COMMAND'
      is_layout               = lwa_layout  "wt_layout
      it_fieldcat             = wt_fieldcat[]
*     I_SAVE                  = VARIANT_SAVE
*     IS_VARIANT              = VARIANTE
*     it_sort                 = ws_sort[]
*     i_save                  = 'A'
    TABLES
      t_outtab                = it_displaytabf
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    " DISPLAY_GRID

*&---------------------------------------------------------------------*
*&      Form  GET_SORTTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sorttab .

  DATA: ls_sort LIKE LINE OF wt_sort.

  ls_sort-spos = 1.
  ls_sort-fieldname = 'WERKS'.
  ls_sort-down  = ' '.
  ls_sort-tabname = 'IT_DISPLAYTABF'.
  APPEND ls_sort TO ws_sort.
  ls_sort-spos = 2.
  ls_sort-fieldname = 'MATNR'.
  ls_sort-down  = ' '.
  ls_sort-tabname = 'IT_DISPLAYTABF'.
  APPEND ls_sort TO ws_sort.
  ls_sort-spos = 3.
  ls_sort-fieldname = 'PENDAYS'.
  ls_sort-down  = ' '.
  ls_sort-tabname = 'IT_DISPLAYTABF'.
  APPEND ls_sort TO ws_sort.
  ls_sort-spos = 4.
  ls_sort-fieldname = 'PENDSTK'.
  ls_sort-down  = ' '.
  ls_sort-tabname = 'IT_DISPLAYTABF'.
  APPEND ls_sort TO ws_sort.

ENDFORM.                    " GET_SORTTAB
*&---------------------------------------------------------------------*
*&      Form  GET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_layout .
*  wt_layout-info_fieldname = 'COL'.

ENDFORM.                    " GET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  TEXT_CREATE
*&---------------------------------------------------------------------*

FORM text_create .

  DATA: p1(10) TYPE c,
        p2(10) TYPE c,
        p3(10) TYPE c,
        p4(10) TYPE c,
        p5(10) TYPE c,
        p6(10) TYPE c,
        p7(10) TYPE c.

  WRITE s1 TO p1.
  s1 = p_c1. s1 = s1 + 1. "31
  s2 = p_c2. s2 = s2 + 1. "61
  s3 = p_c3. s3 = s3 + 1. "91
  s4 = p_c4. s4 = s4 + 1. "121
  s5 = p_c5. s5 = s5 + 1. "151
  s6 = p_c6. s6 = s6 + 1. "181

  WRITE s1 TO p1. CONDENSE p1.
  WRITE s2 TO p2. CONDENSE p2.
  WRITE s3 TO p3. CONDENSE p3.
  WRITE s4 TO p4. CONDENSE p4.
  WRITE s5 TO p5. CONDENSE p5.
  WRITE s6 TO p6. CONDENSE p6.


  IF NOT p_c1 IS INITIAL.
    CONCATENATE '0-' p_c1 INTO t1.    "
  ENDIF.
  IF NOT p_c2 IS INITIAL.
    CONCATENATE p1 '-' p_c2 INTO t2.
  ENDIF.
  IF NOT p_c3 IS INITIAL.
    CONCATENATE p2 '-' p_c3 INTO t3.
  ENDIF.
  IF NOT p_c4 IS INITIAL.
    CONCATENATE p3 '-' p_c4 INTO t4.
  ENDIF.
  IF NOT p_c5 IS INITIAL.
    CONCATENATE p4 '-' p_c5 INTO t5.
  ENDIF.
  IF NOT p_c6 IS INITIAL.
    CONCATENATE p5 '-' p_c6 INTO t6.
  ENDIF.

  CONCATENATE ' ' 'Above' INTO t7.

ENDFORM.                    " TEXT_CREATE
*&---------------------------------------------------------------------*
*&      Form  VALIDATION
*&---------------------------------------------------------------------*

FORM validation .

  IF anztage < p_c1.
    MESSAGE e899(f2) WITH text-044.
  ENDIF.

  IF anztage < p_c2.
    MESSAGE e899(f2) WITH text-044.
  ENDIF.

  IF anztage < p_c3.
    MESSAGE e899(f2) WITH text-044.
  ENDIF.

  IF anztage < p_c4.
    MESSAGE e899(f2) WITH text-044.
  ENDIF.

  IF anztage < p_c5.
    MESSAGE e899(f2) WITH text-044.
  ENDIF.

  IF anztage < p_c6.
    MESSAGE e899(f2) WITH text-044.
  ENDIF.





ENDFORM.                    " VALIDATION
*&---------------------------------------------------------------------*
*&      Form  VAL
*&---------------------------------------------------------------------*

FORM val .
  v1 = p_c1.
  IF NOT p_c1 IS INITIAL AND
     NOT p_c2 IS INITIAL.



    IF p_c1 >= p_c2.
      MESSAGE e899(f2) WITH  text-046 p_c2 text-045  p_c1 .

    ENDIF.
  ENDIF.

  IF NOT p_c2 IS INITIAL AND
     NOT p_c3 IS INITIAL.
    IF p_c2 >= p_c3.
      MESSAGE e899(f2) WITH  text-046 p_c3 text-045  p_c2 .

    ENDIF.
  ENDIF.

  IF NOT p_c3 IS INITIAL AND
     NOT p_c4 IS INITIAL.
    IF p_c3 >= p_c4.
      MESSAGE e899(f2) WITH  text-046 p_c4 text-045 p_c3 .

    ENDIF.
  ENDIF.

  IF NOT p_c4 IS INITIAL AND
     NOT p_c5 IS INITIAL.

    IF p_c4 >= p_c5.
      MESSAGE e899(f2) WITH  text-046 p_c5 text-045 p_c4 .

    ENDIF.
  ENDIF.

  IF NOT p_c5 IS INITIAL AND
     NOT p_c6 IS INITIAL.

    IF p_c5 >= p_c6.
      MESSAGE e899(f2) WITH  text-046 p_c6 text-045  p_c5 .

    ENDIF.
  ENDIF.

ENDFORM.                    " VAL
*&---------------------------------------------------------------------*
*&      Form  VAL1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM val1 .

  v1 = p_c1.
  v2 = p_c2.
  v3 = p_c3.
  v4 = p_c4.
  v5 = p_c5.
  v6 = p_c6.

  IF NOT p_c1 IS INITIAL AND
     NOT p_c2 IS INITIAL.



    IF v1 >= v2.
      MESSAGE e899(f2) WITH  text-046 p_c2 text-045  p_c1 .

    ENDIF.
  ENDIF.

  IF NOT p_c2 IS INITIAL AND
     NOT p_c3 IS INITIAL.
    IF v2 >= v3.
      MESSAGE e899(f2) WITH  text-046 p_c3 text-045  p_c2 .

    ENDIF.
  ENDIF.

  IF NOT p_c3 IS INITIAL AND
     NOT p_c4 IS INITIAL.
    IF v3 >= v4.
      MESSAGE e899(f2) WITH  text-046 p_c4 text-045 p_c3 .

    ENDIF.
  ENDIF.

  IF NOT p_c4 IS INITIAL AND
     NOT p_c5 IS INITIAL.

    IF v4 >= v5.
      MESSAGE e899(f2) WITH  text-046 p_c5 text-045 p_c4 .

    ENDIF.
  ENDIF.

  IF NOT p_c5 IS INITIAL AND
     NOT p_c6 IS INITIAL.

    IF v5 >= v6.
      MESSAGE e899(f2) WITH  text-046 p_c6 text-045  p_c5 .

    ENDIF.
  ENDIF.

ENDFORM.                    "VAL1

**************************************************
FORM sub_user_command USING ld_ucomm TYPE syucomm
                   sobj TYPE slis_selfield.
  DATA : bdoc(18) TYPE c.
  IF sobj-fieldname = 'MATNR'.
    bdoc = sobj-value.

    SET PARAMETER ID 'MAT' FIELD bdoc.
    CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

  ENDIF.
ENDFORM.                    "SUB_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 06.11.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth_obj .
  TYPES:
    BEGIN OF ty_tspat, " Division
      spart TYPE tspat-spart,
    END OF ty_tspat,
    BEGIN OF ty_t001w,
      werks TYPE t001w-werks, " Plants
    END OF ty_t001w,
    BEGIN OF ty_t134t,  " Material Type
      mtart TYPE t134t-mtart,
    END OF ty_t134t
    .
  DATA: t_tspat TYPE TABLE OF ty_tspat, " Division
        w_tspat TYPE ty_tspat,
        t_t001w TYPE TABLE OF ty_t001w, " Plants
        w_t001w TYPE ty_t001w,
        t_t134t TYPE TABLE OF ty_t134t, " Material Type
        w_t134t TYPE ty_t134t
        .
  FREE : t_tspat[], t_t001w[], t_t134t[].
  CLEAR: w_tspat, w_t001w, w_t134t.

  break test1.
***** Start Code: Added by CS on 06.11.2015 for Division Authorization. *****
  SELECT spart  " Fetch values of Division
    FROM tspat
    INTO TABLE t_tspat
    WHERE spart IN spart
      AND spras EQ sy-langu.

  CLEAR: spart, lv_spart_auth_flg.
  REFRESH: spart[].
  IF t_tspat[] IS NOT INITIAL.
    LOOP AT t_tspat INTO w_tspat.
      AUTHORITY-CHECK OBJECT 'V_VBAK_VKO' " Division
                     ID 'ACTVT' FIELD '03'
                     ID 'SPART' FIELD w_tspat-spart.
      IF sy-subrc EQ 0.
        spart-sign = 'I'.
        spart-option = 'EQ'.
        spart-low = w_tspat-spart.
        APPEND spart.
        CLEAR: spart.
      ELSE.
        IF lv_spart_auth_flg IS INITIAL.  " Authorization Flag
          lv_spart_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_tspat.
    ENDLOOP.
  ENDIF.
  IF spart[] IS INITIAL.
    spart-sign = 'I'.
    spart-option = 'EQ'.
    spart-low = ''.
    APPEND spart.
    CLEAR: spart.
  ENDIF.
***** End Code: Added by CS on 06.11.2015 for Division Authorization. *****


****** Start Code: Added by CS on 06.11.2015 for Plant Authorization. *****
  SELECT werks  " Fetch values of Plant
    FROM t001w
    INTO TABLE t_t001w
    WHERE werks IN werke.
**
  CLEAR: werke, lv_werks_auth_flg.
  REFRESH: werke[].
  IF t_t001w[] IS NOT INITIAL.
    LOOP AT t_t001w INTO w_t001w.
      AUTHORITY-CHECK OBJECT 'M_MSEG_WWA' " Plant
                     ID 'ACTVT' FIELD '03'
                     ID 'WERKS' FIELD w_t001w-werks.
      IF sy-subrc EQ 0.
        werke-sign = 'I'.
        werke-option = 'EQ'.
        werke-low = w_t001w-werks.
        APPEND werke.
        CLEAR: werke.
      ELSE.
        IF lv_werks_auth_flg IS INITIAL.  " Authorization Flag
          lv_werks_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t001w.
    ENDLOOP.
  ENDIF.
  IF werke[] IS INITIAL.
    werke-sign = 'I'.
    werke-option = 'EQ'.
    werke-low = ''.
    APPEND werke.
    CLEAR: werke.
  ENDIF.
***** End Code: Added by CS on 06.11.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 06.11.2015 for Material Type Authorization. *****
  SELECT mtart  " Fetch values of Material Type
    FROM t134t
    INTO TABLE t_t134t
    WHERE mtart IN mtart
      AND spras EQ sy-langu.

  CLEAR: mtart, lv_mtart_auth_flg.
  REFRESH: mtart[].
  IF t_t134t[] IS NOT INITIAL.
    LOOP AT t_t134t INTO w_t134t.
      AUTHORITY-CHECK OBJECT 'K_ML_MTART' " Material Type
                     ID 'ACTVT' FIELD '03'
                     ID 'MTART' FIELD w_t134t-mtart.
      IF sy-subrc EQ 0.
        mtart-sign = 'I'.
        mtart-option = 'EQ'.
        mtart-low = w_t134t-mtart.
        APPEND mtart.
        CLEAR: mtart.
      ELSE.
        IF lv_mtart_auth_flg IS INITIAL.  " Authorization Flag
          lv_mtart_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t134t.
    ENDLOOP.
  ENDIF.
  IF mtart[] IS INITIAL.
    mtart-sign = 'I'.
    mtart-option = 'EQ'.
    mtart-low = ''.
    APPEND mtart.
    CLEAR: mtart.
  ENDIF.
***** End Code: Added by CS on 06.11.2015 for Material Type Authorization. *****
ENDFORM.                    " CHECK_AUTH_OBJ
