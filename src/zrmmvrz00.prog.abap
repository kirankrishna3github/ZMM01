* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 23/10/2015
*   REASON FOR CHANGE: Add Authorization, layout and Variant
*   REQUEST #: IRDK913105
* --------------------------------------------------------------------------------------------*
REPORT zrmmvrz00
                LINE-COUNT 0         "neu zu 3.1H /ch -> H: 75343
                LINE-SIZE 132 MESSAGE-ID m3.
*ENHANCEMENT-POINT RMMVRZ00_G4 SPOTS ES_RMMVRZ00 STATIC.
*ENHANCEMENT-POINT RMMVRZ00_G5 SPOTS ES_RMMVRZ00.
*ENHANCEMENT-POINT RMMVRZ00_G6 SPOTS ES_RMMVRZ00 STATIC.
*ENHANCEMENT-POINT RMMVRZ00_G7 SPOTS ES_RMMVRZ00.

* Datenbanktabellen
TABLES:
    marav,
    marc,
    marcv,        "Notwendig für Selektionsbild !!
    mbew,
    t001w,
    t001k,
    tcurm,
    t001,
    mmvr1.        "DDIC-Struktur für Mattab

* interne Tabellen
DATA: BEGIN OF mattab OCCURS 50.
        INCLUDE STRUCTURE mmvr1.
DATA: END OF mattab.

DATA: BEGIN OF ibwkey_werk OCCURS 50,
        bwkey LIKE mbew-bwkey,
        werks LIKE marcv-werks,
      END OF ibwkey_werk.

DATA: BEGIN OF bwkeytab OCCURS 20, "enth. BWKEYs der einzulesenden MBEWs
        bwkey LIKE mbew-bwkey,
      END OF bwkeytab.

*---> Ranges
RANGES: ibwkey FOR mbew-bwkey.


* Reportspezifische Abgrenzungen
SELECT-OPTIONS:
    mtart     FOR  marav-mtart,
    matkl     FOR  marav-matkl,
    ernam     FOR  marav-ernam.

* Hilfsfelder
DATA:
    ktext      LIKE marav-maktx,   "Kurztext
    hmatnr     LIKE marav-matnr,   "Hilfsfeld für Leerzeile pro Material
    xmatnr     LIKE marav-matnr,
    flg_intensified,               "Flag --> Format INTENISFIED
    flgwerks_exist TYPE c,         "Flag --> Werk existiert
    flgbwkey_exist TYPE c,         "Flag --> Bewertungskreis existiert
    flgwerk_zum_bwkey TYPE c,      "Flag --> Werk zum Bwkey existiert
    flgber_mbew    TYPE c,         "Flag --> Berechtigung für MBEW-Satz
    zaehler1(4) TYPE n VALUE 0,    "Zähler, ob  ein Material selektiert
    zaehler2(4) TYPE n VALUE 0,    "Zähler für getrennte Bewertung
    zaehler    LIKE sy-tabix,      "Zähler für DB-Zugriffs-Range

* Konstanten
    grenze     LIKE sy-tabix VALUE 20,  "Max. Zeilen für DB-Zugr.-Range
    space109(109) TYPE c,
    xfeld                    VALUE 'X'.

* siehe SAPDBMSM
DATA:
    kreuz     TYPE c    VALUE 'X',
    auth00    TYPE c,
    auth01    TYPE c,                  "Kz. Werksberechtigung fehlt
    auth02    TYPE c.                  "Kz. Bukrs-Berechtigung fehlt

*---- Bewertungsebene-------------------------------------------------
DATA:    bewwerks LIKE tcurm-bwkrs_cus VALUE '1',
         bewbukrs LIKE tcurm-bwkrs_cus VALUE '3'.
DATA:  l_t134 LIKE t134,
       l_t023 LIKE t023.
* Parameters
PARAMETERS: bewflg TYPE mit_status_b   DEFAULT 'X'
              AS CHECKBOX      .  "Flag zur Anzeige bew. Materialien
PARAMETERS : p_vari TYPE disvariant-variant MODIF ID pk. " Added by CS on 23.10.2015 for Layout

***** Start Code: Added by CS on 23.10.2015 for layout. *****
DATA: s_repid   LIKE sy-repid,
      g_save(1) TYPE c,
      g_exit(1) TYPE c,                         " ALV VARIANT
      gx_variant TYPE disvariant.
***** End Code: Added by CS on 23.10.2015 for layout. *****
***** Start Code: Added by CS on 23.10.2015 for Authorization. *****
DATA: lv_mtart_auth_flg TYPE c VALUE '',  " Auth. Flag for Material Type
      lv_werks_auth_flg TYPE c VALUE ''  " Auth. Flag for Plant
.
***** End Code: Added by CS on 23.10.2015 for Authorization. *****

* Feldgruppen
FIELD-GROUPS:
    header,
    material.


* Felder werden den Feldgruppen zugeordnet
INSERT  marav-matnr mattab-werks mattab-bwtar           INTO header.

INSERT  marav-ernam  marav-mtart  marav-matkl  marav-meins
        marav-laeda  ktext
       mattab-ekgrp mattab-maabc mattab-dismm
       mattab-vprsv mattab-preis mattab-waers mattab-peinh
       mattab-bklas                                     INTO material.

** Accessibility 5.0
INCLUDE zirmmvrz00_alt.

*&---------------------------------------------------------------------*
*&  Include           IRMMVRZ00_ALT
*&---------------------------------------------------------------------*
*& Global Data Declaration for ALV display
*&---------------------------------------------------------------------*

* type-pools ***********************************************************
*TYPE-POOLS: slis.
*
** structures types *****************************************************
*TYPES:
*  BEGIN OF listwa_type,
*INCLUDE TYPE zzplm_alv_230.
*
*TYPES:
**  MATNR  type PLM_ALV_230-MATNR,
**WERKS  type PLM_ALV_230-WERKS,
**BWTAR  type PLM_ALV_230-BWTAR,
**KTEXT  type PLM_ALV_230-KTEXT,
**LAEDA  type PLM_ALV_230-LAEDA,
**MTART  type PLM_ALV_230-MTART,
**MATKL  type PLM_ALV_230-MATKL,
**MEINS  type PLM_ALV_230-MEINS,
**EKGRP  type PLM_ALV_230-EKGRP,
**MAABC  type PLM_ALV_230-MAABC,
**DISMM  type PLM_ALV_230-DISMM,
**BKLAS  type PLM_ALV_230-BKLAS,
**VPRSV  type PLM_ALV_230-VPRSV,
**PREIS  type PLM_ALV_230-PREIS,
**WAERS  type PLM_ALV_230-WAERS,
**PEINH  type PLM_ALV_230-PEINH,
**ERNAM type PLM_ALV_230-ERNAM,
**spart TYPE tspa-spart,
**VTEXT TYPE tspat-VTEXT,
*  END   OF listwa_type.
*
** tables types *********************************************************
*TYPES: listtab_type TYPE STANDARD TABLE OF
*  listwa_type WITH DEFAULT KEY.
*
** data *****************************************************************
*DATA: gt_list TYPE listtab_type,
*      listwa LIKE LINE OF gt_list.
*
** constants ************************************************************
*CONSTANTS: c_marked(1)        TYPE c VALUE 'X',
*           c_structure_name   TYPE tabname VALUE 'ZZPLM_ALV_230',
*           c_save(1)          TYPE c VALUE 'A',
*           c_werk(5)          TYPE c VALUE 'WERKS',
*           c_matnr(5)          TYPE c VALUE 'MATNR'.
*CONSTANTS:
*  BEGIN OF cs_callback,
*    program       TYPE syrepid VALUE 'RMMVRZ00',
*    pf_status_set TYPE slis_formname VALUE '',
*    user_command  TYPE slis_formname VALUE '',
*    top_of_page   TYPE slis_formname VALUE '',
*  END   OF cs_callback.
*
***** Start Code: Added by CS on 23.10.2015 for layout. *****
*PERFORM get_default_variant.                     " GETTING DEFAULT VARIANT SAVED

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM variant_f4_selection.                    " F4 HELP FOR VARIANT ON SELECTION SCREEN

AT SELECTION-SCREEN.
  s_repid = sy-repid.
  PERFORM check_variant_existance.

INITIALIZATION.
  s_repid = sy-repid.
  PERFORM variant_init.
***** End Code: Added by CS on 23.10.2015 for layout. *****



  INCLUDE zirmmvrz00_alf.


START-OF-SELECTION.
  PERFORM check_auth_obj.  " Added by CS on 23.10.2015 for Authorization.
*--------------------------------------------------------------------*
* Lesen Materialien                                                  *
*--------------------------------------------------------------------*

* wg. Performance werden die selektierten Materialarten in der
* log DB MSM bekannt gemacht.                 "
  IF ( ms_matnr[] IS INITIAL ) AND                "ch/4.5b - H:112292
     ( NOT matkl[] IS INITIAL ).                  "
    EXPORT matkl TO MEMORY                        "
                 ID 'MATKL'.                      "
  ELSEIF                                          "
     ( ms_matnr[] IS INITIAL ) AND              "
     ( NOT mtart[] IS INITIAL ).                "
    EXPORT mtart TO MEMORY                      "
                 ID 'MTART'.                    "ch zu 4.0 wg. TODO
  ENDIF.                                        "

  PERFORM lesen_tcurm.

* Prüfen, ob überhaupt eines der eingegebenen Werke existiert  /ch 4.6C
  SELECT * FROM t001w UP TO 1 ROWS            "
         WHERE werks IN ms_werks.             "
  ENDSELECT.                                  "
  IF sy-subrc <> 0.                           "
    MESSAGE s102 WITH ms_werks-low.           "
    EXIT .                                    "
  ENDIF.                                      "


GET marav.
  CHECK SELECT-OPTIONS.

  MOVE marav-maktx TO ktext.
  CLEAR flgwerks_exist.                       "ch zu 3.1G ->H: 62812

GET marcv.      "neu zu 3.0d -> Bem in Form Lesen_MARC    /ch
  PERFORM lesen_marc.

GET marav LATE.     "ch zu 3.0E -> bessere Performance
  "           -> Beseitigung mehrfacher Einräge
  PERFORM bwkey_zu_werk_ermitteln.
* PERFORM LESEN_MBEW.     "wird nun von Form BWKEY_ZU_WERK_ERMITTELN
  "aus aufgerufen (wg. Umstellung Array-Select)
  "28.12.93 / CH
  IF NOT bewflg IS INITIAL.
*--- nur bewertete Werke zum Material erwünscht -----------------------
    PERFORM loeschen_werk_unbw.
  ENDIF.

  LOOP AT mattab.
* Sowohl Zeile der MATTAB als auch die int. Tab. MARAV wird extrahiert
    sy-subrc = 0.                                      "Note 1070614
    PERFORM authority_check_on_client USING marav-matnr.
    IF sy-subrc = 0.
      EXTRACT material.
    ENDIF.
  ENDLOOP.

* Int. Tabelle MATTAB ist leer und unabhängig von den Berechtigungen
* sind weder Werksdaten noch Buchhaltungsdaten zum Material vorhanden.
  IF sy-subrc NE 0
     AND flgwerks_exist IS INITIAL         "kein Werk vorhanden
     AND flgbwkey_exist IS INITIAL         "kein Bewertungskreis vorh.
     AND bewflg IS INITIAL.
* Feldgruppe MATERIAL soll nur dann extrahiert werden, wenn auf dem
* Selektionsdynpro kein Werk eingegeben wurde.
    CHECK ms_werks IS INITIAL.            "22.04.94 WEWK11K076569
    sy-subrc = 0.                                      "Note 1070614
    PERFORM authority_check_on_client USING marav-matnr.
    IF sy-subrc = 0.
      EXTRACT material.
    ENDIF.
  ENDIF.
  CLEAR mattab.
  REFRESH mattab.

* Selektionsende
END-OF-SELECTION.

  SORT.
  LOOP.
    zaehler1 = zaehler1 + 1.

    AT material.
      MOVE-CORRESPONDING mattab TO mmvr1.
      IF mattab-bwtar IS INITIAL.

** Accessibility 5.0
*        SELECT SINGLE spart FROM mara INTO listwa-spart WHERE matnr = listwa-matnr.
*        SELECT SINGLE vtext FROM tspat INTO listwa-vtext WHERE spart = listwa-spart AND spras = 'EN'.
        MOVE ktext TO listwa-ktext.
        MOVE-CORRESPONDING mmvr1 TO listwa.
        MOVE-CORRESPONDING marav TO listwa.
        MOVE-CORRESPONDING mattab TO listwa.

        APPEND listwa TO gt_list.

      ELSE.
* MBEW-Detaildaten werden dann ausgegeben, wenn die Bewertungsart
* ungleich Space ist
        PERFORM mbew_details_ausgeben.
*       MOVE MARAV-MATNR TO XMATNR.
      ENDIF.
    ENDAT.
    MOVE marav-matnr TO hmatnr.
  ENDLOOP.
*note 1070614
  IF auth00 IS NOT INITIAL OR
     auth01 IS NOT INITIAL OR
     auth02 IS NOT INITIAL OR
    lv_mtart_auth_flg EQ 'X'. " Added by CS on 26.10.2015 for Authorization
    IF auth01 EQ 'X' OR lv_mtart_auth_flg EQ 'X'. " Added by CS on 26.10.2015 for Authorization
      MESSAGE 'Missing Authorization for Plant/Material Type.' TYPE 'I' DISPLAY LIKE 'W'.
    ELSE.
      MESSAGE s781.
    ENDIF.

  ENDIF.
* Wenn kein Material gefunden wird ---> entsprechende Fehlermeldung
  break test1.
  IF zaehler1 < 1.
    MESSAGE s780.
    IF NOT sy-binpt IS INITIAL OR NOT sy-calld IS INITIAL. "note 374028
      LEAVE.
    ELSE.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
  ENDIF.

****** Start Code: Added by CS on 23.10.2015 for Authorization. *****
*  IF gt_list[] IS NOT INITIAL.
*    IF auth01 EQ 'X' OR lv_mtart_auth_flg = 'X'.
*      MESSAGE 'Missing Authorization for Plant/Material Type.' TYPE 'I' DISPLAY LIKE 'W'.
*    ENDIF.
*  ELSE.
*    IF auth01 = 'X' OR lv_mtart_auth_flg = 'X'.
*      MESSAGE 'No data found/ Missing Authorization for Plant/Material Type.' TYPE 'I'.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDIF.
****** End Code: Added by CS on 23.10.2015 for Authorization. *****

* ALV output
  PERFORM alv_list_output.


* note 551074
  PERFORM protokoll_daten.


*eject
*----------------------------------------------------------------------*
*  Form-Routinen                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Bei Bewertung auf Buchungskreisebene wird die Buchungskreis-
* Berechtigung geprüft
*----------------------------------------------------------------------*
FORM berechtigung_bukrs.

  IF tcurm-bwkrs_cus EQ bewbukrs.
*---- Bewertung liegt auf Buchungskreisebene -------------------------
    AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
                  ID 'ACTVT' FIELD '03'
                  ID 'BUKRS' FIELD mbew-bwkey.
    IF sy-subrc NE 0.
      auth02 = kreuz.
      CLEAR flgber_mbew.
    ENDIF.
  ENDIF.

*   Neu zu 3.1I: Prüfung ob Berechtigung zum Anzeigen der Buchhaltungs-
*   bzw. Kalkulationsdaten                    ch/13.08.97  H: 81273
  AUTHORITY-CHECK OBJECT 'M_MATE_STA'
           ID 'ACTVT' FIELD '03'
           ID 'STATM' FIELD 'B'.
  IF sy-subrc NE 0.
    AUTHORITY-CHECK OBJECT 'M_MATE_STA'
             ID 'ACTVT' FIELD '03'
             ID 'STATM' FIELD 'G'.
  ENDIF.
  IF sy-subrc NE 0.
    auth02 = kreuz.
    CLEAR flgber_mbew.
  ENDIF.

ENDFORM.                    "BERECHTIGUNG_BUKRS

*----------------------------------------------------------------------*
* MBEW-Detaildaten werden ausgegeben ---> getrennte Bewertung existiert*
*----------------------------------------------------------------------*
FORM mbew_details_ausgeben.

  IF xmatnr NE marav-matnr.

    MOVE-CORRESPONDING mmvr1 TO listwa.

    APPEND listwa TO gt_list.
  ENDIF.

ENDFORM.                    "MBEW_DETAILS_AUSGEBEN

*--------------------------------------------------------------------*
* Interne Tabelle IBWKEY der möglichen Bewertungskreise gemäß        *
* Werksselektion aufbauen.                                           *
* Zusätzlich festhalten der Zuordnung Bewertungskreis zu Werk        *
* in der internen Tabelle IBWKEY_WERK                                *
* Änderung 28.12.93 / CH:
* Um beim Array-Select auf die MBEW einen SQL-Fehler wegen Überlauf
* in der DB-Schnittstelle zu vermeiden, werden die für das aktuelle
* Material relevante BWKEYs zunächs in BWKEYTAB eingelesen und an-
* schließend daraus evtl. mehrere kleinere IBWKEYs für die DB-Selek-
* tion erzeugt.
*--------------------------------------------------------------------*
FORM bwkey_zu_werk_ermitteln.

  CLEAR flgbwkey_exist.
  CLEAR flgwerk_zum_bwkey.

  CLEAR bwkeytab.    REFRESH bwkeytab.
  CLEAR ibwkey_werk. REFRESH ibwkey_werk.
  CLEAR ibwkey.      REFRESH ibwkey.

  SELECT * FROM t001w
    WHERE werks IN ms_werks.
*  Eintrag BWKEYTAB ----------------------------------------------------
    READ TABLE bwkeytab WITH KEY t001w-bwkey BINARY SEARCH.
    IF sy-subrc NE 0.
      bwkeytab-bwkey = t001w-bwkey.
      INSERT bwkeytab INDEX sy-tabix.
    ENDIF.

*  Eintrag Tabelle IBWKEY_WERK -----------------------------------------
    CLEAR ibwkey_werk.
    ibwkey_werk-werks = t001w-werks.
    ibwkey_werk-bwkey = t001w-bwkey.
    APPEND ibwkey_werk.
  ENDSELECT.

* Massenzugriff auf die MBEW
  CLEAR zaehler.
  ibwkey-sign   = 'I'.
  ibwkey-option = 'EQ'.
  CLEAR ibwkey-high.
  LOOP AT bwkeytab.
    ibwkey-low    = bwkeytab-bwkey.
    APPEND ibwkey.
    ADD 1 TO zaehler.
*   IF ZAEHLER = GRENZE.               "ch zu 4.0: Umstellung auf
*     PERFORM LESEN_MBEW.              "Select for all entries
*     CLEAR ZAEHLER.
*     REFRESH IBWKEY.
*   ENDIF.
  ENDLOOP.
  IF zaehler > 0.
    PERFORM lesen_mbew.
    CLEAR zaehler.
    REFRESH ibwkey.
  ENDIF.

*--- Falls zum Material kein passendes Werk zur MBEW vorhanden ist,----
*--- zurücksetzen des Kennzeichens, daß MBEW existiert, damit --------
*--- für den Fall, daß kein MARC vorhanden ist, die MARA-Daten -------
*--  ausgegeben werden -----------------------------------------------
  IF flgwerk_zum_bwkey IS INITIAL.
    CLEAR flgbwkey_exist.
  ENDIF.

ENDFORM.                    "BWKEY_ZU_WERK_ERMITTELN

*--------------------------------------------------------------------*
* Lesen Werksdaten zum Material gemäß Werksselektion                 *
*--------------------------------------------------------------------*
FORM lesen_marc.

*  CLEAR FLGWERKS_EXIST.                       "ch zu 3.1G ->H: 62812
  CLEAR marc.

*  SELECT * FROM MARC                           "ch zu 3.0d
*       WHERE MATNR EQ  MARAV-MATNR             "sonst wird MARC durch
*       AND   WERKS IN MS_WERKS.                "log DB MSM und durch
  MOVE-CORRESPONDING marcv TO marc.             "diesen Report gelesen
  "->schlechte Performance
  flgwerks_exist = 'X'.

* Es wird geprüft, ob die Material_Werks-Berechtigung vorliegt
  AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
                 ID 'ACTVT' FIELD '03'
                 ID 'WERKS' FIELD marc-werks.
  IF sy-subrc NE 0.
    auth01 = kreuz.
  ELSE.
    CLEAR mattab.
    MOVE-CORRESPONDING marc TO mattab.
    APPEND mattab.
  ENDIF.
*  ENDSELECT.                                "ch zu 3.0d

ENDFORM.                    "LESEN_MARC

*--------------------------------------------------------------------*
*   Lesen Bewertungsdaten zum Material gemäß Werksselektion          *
*--------------------------------------------------------------------*
FORM lesen_mbew.

  SELECT * FROM mbew
       FOR ALL ENTRIES IN ibwkey                            "ch zu 4.0
       WHERE  matnr EQ   marav-matnr
*      AND    BWKEY IN   IBWKEY.              "ch zu 4.0
       AND    bwkey =    ibwkey-low.                        "ch zu 4.0

*   note 914636
    DATA ls_t134 TYPE t134.
    CALL FUNCTION 'T134_SINGLE_READ'
      EXPORTING
        t134_mtart = marav-mtart
      IMPORTING
        wt134      = ls_t134
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0 OR ls_t134-pstat NA 'B'.
      CONTINUE.
    ENDIF.

    flgbwkey_exist = 'X'.                   "Bewertungskreis existiert
    flgber_mbew    = 'X'.
    PERFORM berechtigung_bukrs.
    CHECK NOT flgber_mbew IS INITIAL.

    LOOP AT ibwkey_werk
      WHERE bwkey EQ  mbew-bwkey.
*--- Aktualisieren bzw. Erzeugen der Einträge für alle Werke -----
*--- zum Bewertungskreis ------------------------------------------
      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
                    ID 'ACTVT' FIELD '03'
                    ID 'WERKS' FIELD ibwkey_werk-werks.
      IF sy-subrc NE 0.
*-Keine Material-Werks-Berechtigung -----------------------------
        auth01 = kreuz.
      ELSE.
        LOOP AT mattab
          WHERE werks      EQ ibwkey_werk-werks
          AND   bwtar      EQ space.
          flgwerk_zum_bwkey = 'X'.
* MARC-Daten sind schon vorhanden -----------------------------------
          IF mbew-bwtar EQ space .
* Bewertungskopfsatz festhalten -------------------------------------
            PERFORM waehrung_ermitteln USING mbew-bwkey.
            PERFORM preisermitteln.
            PERFORM kzstb_ermitteln.
            MOVE-CORRESPONDING mbew TO mattab.
            MODIFY mattab.
          ELSE.
* Bewertungs-Detailsatz festhalten -----------------------------------
            CLEAR mattab.
            MOVE ibwkey_werk-werks  TO mattab.
            MOVE-CORRESPONDING mbew TO mattab.
            PERFORM waehrung_ermitteln USING mbew-bwkey.
            PERFORM preisermitteln.
            mattab-kzstb = xfeld.
            APPEND mattab.
          ENDIF.
        ENDLOOP.
        IF sy-subrc NE 0 AND tcurm-bwkrs_cus = bewwerks.  "neu zu 3.0E
          flgwerk_zum_bwkey = 'X'.                          "und 2.2F/G
          CLEAR mattab.                                     "->H: 21839
          MOVE ibwkey_werk-werks  TO mattab.             "
          MOVE-CORRESPONDING mbew TO mattab.             "
          PERFORM waehrung_ermitteln USING mbew-bwkey.   "
          PERFORM preisermitteln.                        "
          PERFORM kzstb_ermitteln.                       "
          APPEND mattab.                                 "
        ENDIF.                                            "
      ENDIF.
    ENDLOOP.
  ENDSELECT.

ENDFORM.                    "LESEN_MBEW

*--------------------------------------------------------------------*
*   Lesen TCURM                                                      *
*--------------------------------------------------------------------*
FORM lesen_tcurm.

  SELECT * FROM tcurm.
  ENDSELECT.
  IF sy-subrc NE 0 OR tcurm-bwkrs_cus IS INITIAL.
    MESSAGE e046.
  ENDIF.

ENDFORM.                    "LESEN_TCURM

*----------------------------------------------------------------------*
* Wenn BEWFLG von außen mitgegeben wird, dann werden nur bewertete
* Materialien angezeigt; ansonsten alle Materialien
*----------------------------------------------------------------------*
FORM loeschen_werk_unbw.

  IF flgbwkey_exist IS INITIAL.        "kein Bewertungskreis vorh.
    CLEAR mattab.
    REFRESH mattab.
  ELSE.
    LOOP AT mattab WHERE kzstb EQ space.
      DELETE mattab.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "LOESCHEN_WERK_UNBW

*--------------------------------------------------------------------*
*   Preisermittlung                                                  *
*--------------------------------------------------------------------*
FORM preisermitteln.

  IF mbew-vprsv EQ 'S'.
    MOVE mbew-stprs TO mattab-preis.
  ELSE.
    MOVE mbew-verpr TO mattab-preis.
  ENDIF.

ENDFORM.                    "PREISERMITTELN

*--------------------------------------------------------------------*
*   Lesen der T001W                                                  *
*--------------------------------------------------------------------*
FORM lesen_t001w.

  SELECT SINGLE * FROM t001w
     WHERE werks EQ marc-werks.

ENDFORM.                    "LESEN_T001W

*--------------------------------------------------------------------*
*   Einlesen des Währungsschlüssels                                  *
*--------------------------------------------------------------------*
FORM waehrung_ermitteln USING bewertungskreis.

  IF t001k-bwkey NE bewertungskreis.
    SELECT SINGLE * FROM t001k
       WHERE bwkey EQ bewertungskreis.
  ELSE.
    sy-subrc = 0.
  ENDIF.
  IF sy-subrc = 0.
    IF t001-bukrs NE t001k-bukrs.
      SELECT SINGLE * FROM t001
         WHERE bukrs EQ t001k-bukrs.
      MOVE t001-waers TO mattab-waers.
    ELSE.
      MOVE t001-waers TO mattab-waers.
    ENDIF.
  ENDIF.

ENDFORM.                    "WAEHRUNG_ERMITTELN

*--------------------------------------------------------------------*
*   Ermitteln, ob die Sicht Buchhaltung zum Material gepflegt ist    *
*--------------------------------------------------------------------*
FORM kzstb_ermitteln.

  IF mbew-pstat IS INITIAL OR
     mbew-pstat CA 'B'.
    mattab-kzstb = xfeld.
  ELSE.
    CLEAR mattab-kzstb.
  ENDIF.

ENDFORM.                    "KZSTB_ERMITTELN
*&---------------------------------------------------------------------*
*&      Form  PROTOKOLL_DATEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM protokoll_daten .

  DATA: funcname_ca_write_log LIKE rs38l-name VALUE 'CA_WRITE_LOG',
        funcname_ca_user_exists LIKE rs38l-name VALUE 'CA_USER_EXISTS',
        progname TYPE progname VALUE 'RMMVRZ00',
        selections(80) TYPE c OCCURS 0 WITH HEADER LINE.

  REFRESH selections. CLEAR selections.

  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = funcname_ca_write_log
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc = 0.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = funcname_ca_user_exists
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 0.
      CALL FUNCTION funcname_ca_user_exists
        EXPORTING
          i_user       = sy-uname
        EXCEPTIONS
          user_missing = 1
          OTHERS       = 2.
    ENDIF.
    IF sy-subrc = 0.
      CALL FUNCTION funcname_ca_write_log
        EXPORTING
          i_program     = progname
        TABLES
          it_selections = selections
        EXCEPTIONS
          write_error   = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " PROTOKOLL_DATEN
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK_ON_CLIENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM authority_check_on_client USING l_matnr LIKE mara-matnr.
*Note 1070614
  STATICS : s_matnr LIKE mara_matnr,
            s_subrc LIKE sy-subrc.

  IF s_matnr NE l_matnr.

    s_matnr = l_matnr.

    SELECT SINGLE * FROM t134 INTO l_t134
      WHERE  mtart = marav-mtart.

    IF NOT l_t134-begru IS INITIAL.
      AUTHORITY-CHECK OBJECT 'M_MATE_MAR'
                        ID 'ACTVT' FIELD '03'
                        ID 'BEGRU' FIELD l_t134-begru.
      IF sy-subrc NE 0.
        auth00 = kreuz.
        s_subrc = sy-subrc.
        CHECK sy-subrc = 0.
      ENDIF.
    ENDIF.
    IF NOT marav-begru IS INITIAL.
      CALL FUNCTION 'BEGRU_MAT_AUTHORITY_CHECK'
        EXPORTING
          aktyp        = 'A'
          begru        = marav-begru
        EXCEPTIONS
          no_authority = 1.
      IF sy-subrc NE 0.
        auth00 = kreuz.
        s_subrc = sy-subrc.
        CHECK sy-subrc = 0.
      ENDIF.
    ENDIF.

    IF NOT marav-matkl IS INITIAL.
      CALL FUNCTION 'T023_SINGLE_READ'
        EXPORTING
          t023_matkl = marav-matkl
        IMPORTING
          wt023      = l_t023
        EXCEPTIONS
          not_found  = 01.
      IF sy-subrc NE 0.
        CLEAR l_t023.
      ENDIF.
      IF NOT l_t023-begru IS INITIAL.
        CALL FUNCTION 'BEGRU_WGR_AUTHORITY_CHECK'
          EXPORTING
            aktyp        = 'A'
            begru        = l_t023-begru
          EXCEPTIONS
            no_authority = 1.
        IF sy-subrc NE 0.
          auth00 = kreuz.
          s_subrc = sy-subrc.
          CHECK sy-subrc = 0.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
    sy-subrc = s_subrc.
  ENDIF.

ENDFORM.                    " AUTHORITY_CHECK_ON_CLIENT
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Added by CS on 23.10.2015 for layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_init .

  g_save = 'A'.
  CLEAR gx_variant.
  gx_variant-report = s_repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.

ENDFORM.                    " VARIANT_INIT
*&---------------------------------------------------------------------*
*&      Form  VARIANT_F4_SELECTION
*&---------------------------------------------------------------------*
* Addd by CS on 23.10.2015 for F4 Help Layout.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_f4_selection .

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = gx_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.

  ENDIF.
ENDFORM.                    " VARIANT_F4_SELECTION
**&---------------------------------------------------------------------*
**&      Form  GET_DEFAULT_VARIANT
**&---------------------------------------------------------------------*
** Addd by CS on 23.10.2015 for Default Layout.
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
*FORM get_default_variant .
*  CLEAR gx_variant.
*  gx_variant-report = sy-repid.
*
*  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
*    EXPORTING
*      i_save     = 'U'
*    CHANGING
*      cs_variant = gx_variant
*    EXCEPTIONS
*      not_found  = 2.
*
*  IF sy-subrc = 0.
*    p_vari = gx_variant-variant.
*  ENDIF.
*
*ENDFORM.                    " GET_DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
* Added by CS on 23.10.2015 for validate existance of layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_variant_existance .
  IF NOT p_vari IS INITIAL.
    MOVE p_vari TO gx_variant-variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant.
  ELSE.
    PERFORM variant_init.
  ENDIF.

ENDFORM.                    " CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 23.10.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth_obj .
  TYPES:
        BEGIN OF ty_t001w,
          werks TYPE t001w-werks, " Plants
        END OF ty_t001w,
        BEGIN OF ty_t134t,  " Material Type
          mtart TYPE t134t-mtart,
        END OF ty_t134t
          .
  DATA: t_t001w TYPE TABLE OF ty_t001w, " Plants
        w_t001w TYPE ty_t001w,
        t_t134t TYPE TABLE OF ty_t134t, " Material Type
        w_t134t TYPE ty_t134t
        .
  FREE : t_t001w[], t_t134t[].
  CLEAR: w_t001w, w_t134t.

  break test1.
****** Start Code: Added by CS on 23.10.2015 for Plant Authorization. *****
*  SELECT werks  " Fetch values of Plant
*    FROM t001w
*    INTO TABLE t_t001w
*    WHERE werks IN ms_werks.
***
*  CLEAR: ms_werks, lv_werks_auth_flg.
*  REFRESH: ms_werks[].
*  IF t_t001w[] IS NOT INITIAL.
*    LOOP AT t_t001w INTO w_t001w.
*      AUTHORITY-CHECK OBJECT 'M_MSEG_WWA' " Plant
*                     ID 'ACTVT' FIELD '03'
*                     ID 'WERKS' FIELD w_t001w-werks.
*      IF sy-subrc EQ 0.
*        ms_werks-sign = 'I'.
*        ms_werks-option = 'EQ'.
*        ms_werks-low = w_t001w-werks.
*        APPEND ms_werks.
*        CLEAR: ms_werks.
*      ELSE.
*        IF lv_werks_auth_flg IS INITIAL.  " Authorization Flag
*          lv_werks_auth_flg = 'X'.
*        ENDIF.
*      ENDIF.
*      CLEAR: w_t001w.
*    ENDLOOP.
*  ENDIF.
*  IF ms_werks[] IS INITIAL.
*    ms_werks-sign = 'I'.
*    ms_werks-option = 'EQ'.
*    ms_werks-low = ''.
*    APPEND ms_werks.
*    CLEAR: ms_werks.
*  ENDIF.
****** End Code: Added by CS on 23.10.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 23.10.2015 for Material Type Authorization. *****
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
***** End Code: Added by CS on 23.10.2015 for Material Type Authorization. *****
ENDFORM.                    " CHECK_AUTH_OBJ
