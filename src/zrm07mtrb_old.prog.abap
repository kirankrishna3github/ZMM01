*&---------------------------------------------------------------------*
*& Report ZRM07MTRB_OLD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*REPORT ZRM07MTRB_OLD.


************************************************************************
*     REPORT RM07MTRB   (TA MB5T)                                      *
************************************************************************
*     Last modification: 22.03.2001, vio
************************************************************************
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 28/10/2015
*   REASON FOR CHANGE: Add Authorization
*   REQUEST #: IRDK915462
* --------------------------------------------------------------------------------------------*

REPORT zrm07mtrb MESSAGE-ID m7.
*ENHANCEMENT-POINT rm07mtrb_g4 SPOTS es_rm07mtrb STATIC.
*ENHANCEMENT-POINT rm07mtrb_g5 SPOTS es_rm07mtrb.
*ENHANCEMENT-POINT rm07mtrb_g6 SPOTS es_rm07mtrb STATIC.
*ENHANCEMENT-POINT rm07mtrb_g7 SPOTS es_rm07mtrb.

TYPE-POOLS:  slis.
CLASS cl_mmim_auth DEFINITION LOAD.

TABLES: ekko, ekpo, t001w, makt, mara, t001, t001w_ext,     "sdp462
        t001l, mdlg.                                                         "dfps

* EA-DFPS
*     active?
DATA: gv_dfps_active TYPE c.                                                 "dfps

* Interface structure for function module
DATA: BEGIN OF xtab6 OCCURS 0,
         werks LIKE ekpo-werks,
         matnr LIKE ekpo-matnr,
         menge LIKE ekbe-menge,
         meins LIKE ekpo-meins,
         dmbtr LIKE ekbe-dmbtr,
         waers LIKE ekbe-waers,
         netwr LIKE ekpo-netwr,
         bwaer LIKE ekko-waers,
         ebeln LIKE ekbe-ebeln,
         ebelp LIKE ekbe-ebelp,
         sobkz LIKE mdbs-sobkz,
         pstyp LIKE mdbs-pstyp,
         bstmg LIKE ekbe-menge,
         bstme LIKE ekpo-meins,
         reswk LIKE ekko-reswk,
         bsakz LIKE ekko-bsakz,                             "sdp462
         lgort LIKE ekpo-lgort,                             "DFPS
         reslo LIKE ekpo-reslo,                             "DFPS
         budat LIKE mkpf-budat,"EINDT,
         cpudt LIKE mkpf-cpudt,
         ststk LIKE zzst_sto_details-NDAYS,"ststk,
         chng(01), "FLAG
      END OF xtab6.

DATA: doc_year TYPE ekbe-gjahr.
DATA: mat_doc TYPE ekbe-belnr.

DATA: BEGIN OF bestand OCCURS 0.
        INCLUDE STRUCTURE xtab6.
DATA:   maktx LIKE makt-maktx,
        name1 LIKE t001w-name1,
        color TYPE slis_t_specialcol_alv,
        longnum TYPE /sappspro/longnum.
*ENHANCEMENT-POINT rm07mtrb_01 SPOTS es_rm07mtrb STATIC.
DATA: END OF bestand.

* Data for plant, name and company code
DATA: BEGIN OF organ OCCURS 0,
        werks LIKE t001w-werks,
        name1 LIKE t001w-name1,
        bukrs LIKE t001k-bukrs,
      END OF organ.

DATA: BEGIN OF header OCCURS 0,
        matnr LIKE mara-matnr,
        maktx LIKE makt-maktx,
        werks LIKE t001w-werks,
        name1 LIKE t001w-name1,
        lgort LIKE t001l-lgort,                             "dfps
      END OF header.

DATA: fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: detail.
***** Start Code: Added by CS on 28.10.2015 for Authorization. *****
DATA: lv_reswk_auth_flg TYPE c VALUE '',  " Auth. Flag for Issue Plant
      lv_werks_auth_flg TYPE c VALUE ''.  " Auth. Flag for Receiving Plant
***** End Code: Added by CS on 28.10.2015 for Authorization. *****

*********************** selection screen *******************************

SELECTION-SCREEN BEGIN OF BLOCK abgrenzungen WITH FRAME TITLE text-001.
SELECT-OPTIONS: matnr FOR mara-matnr  MEMORY ID mat,
                berid FOR mdlg-berid  MODIF ID dfp,         "dfps
                werks FOR t001w-werks MEMORY ID wrk,
                lgort FOR t001l-lgort MODIF ID dfp,         "dfps
                bukrs FOR t001-bukrs,
                resbi FOR mdlg-berid  MODIF ID dfp,         "dfps
                reswk FOR ekko-reswk,                       "sdp462
                reslo FOR ekpo-reslo  MODIF ID dfp,         "dfps
                sobkz FOR ekpo-sobkz.
PARAMETERS:
                xtram LIKE am07m-mb5t_sel DEFAULT 'X',
                xnlcc LIKE am07m-mb5t_sel DEFAULT 'X',
                xelik LIKE am07m-also_elikz,
                xloek LIKE am07m-also_deleted,
                xsper LIKE am07m-also_elikz,                "n1268193
                det_list LIKE am07m-XSELK.
SELECTION-SCREEN END OF BLOCK abgrenzungen.

SELECTION-SCREEN BEGIN OF BLOCK liste WITH FRAME TITLE text-002.
PARAMETERS: alv_def LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK liste.

INCLUDE rm07alvi.
*INCLUDE zrm07alvi.

INITIALIZATION.
*ENHANCEMENT-POINT rm07mtrb_02 SPOTS es_rm07mtrb.
  PERFORM alv_init.
* Check if EA-DFPS is active
  CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
    EXPORTING
      i_structure_package = 'EA-DFPS'
    IMPORTING
      e_active            = gv_dfps_active
    EXCEPTIONS
      not_existing        = 1
      object_not_existing = 2
      no_extension_object = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    CLEAR gv_dfps_active.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  IF gv_dfps_active IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'DFP'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR alv_def.
  PERFORM alv_f4.

AT SELECTION-SCREEN.
  CALL FUNCTION 'MMIM_ENTRYCHECK_MAIN'
    TABLES
      it_matnr = matnr
      it_werks = werks
      it_bukrs = bukrs
      it_reswk = reswk
      it_sobkz = sobkz.
  PERFORM alv_check.
  PERFORM organisation.

START-OF-SELECTION.
  PERFORM check_auth_obj.  " Added by CS on 28.10.2015 for Authorization.

  PERFORM datenselektion.

END-OF-SELECTION.
  PERFORM feldkatalog_aufbauen USING fieldcat[].
  PERFORM listausgabe.
************************************************************************
FORM organisation.

  SELECT werks name1 bukrs
         INTO CORRESPONDING FIELDS OF TABLE organ
         FROM t001w INNER JOIN t001k
         ON t001w~bwkey = t001k~bwkey
         WHERE werks IN werks
           AND bukrs IN bukrs.
  SORT organ BY werks.
ENDFORM.                    " ORGANISATION


************************************************************************
FORM datenselektion.
  DATA: color TYPE slis_t_specialcol_alv WITH HEADER LINE.

  DATA: BEGIN OF makt_key OCCURS 0,
          matnr LIKE makt-matnr,
        END OF makt_key.
  DATA: BEGIN OF imakt OCCURS 0,
          matnr LIKE makt-matnr,
          maktx LIKE makt-maktx,
        END OF imakt.

  RANGES: elikz FOR ekpo-elikz,
          loekz FOR ekpo-loekz.
*ENHANCEMENT-POINT RM07MTRB_16 SPOTS ES_RM07MTRB STATIC .
  REFRESH: elikz, loekz.

  elikz-sign = 'I'. elikz-option = 'EQ'. CLEAR elikz-low. APPEND elikz.
  IF NOT xelik IS INITIAL. elikz-low = 'X'. APPEND elikz. ENDIF.

  loekz-sign = 'I'. loekz-option = 'EQ'. CLEAR loekz-low. APPEND loekz.
  IF NOT xloek IS INITIAL. loekz-low = 'X'. APPEND loekz. ENDIF.
* note 1268193 introduces the new field xsper for del. po               "n1268193
  IF NOT xsper IS INITIAL. loekz-low = 'S'. APPEND loekz. ENDIF. "n1268193


  IF gv_dfps_active IS INITIAL.
*ENHANCEMENT-SECTION     rm07mtrb_07 SPOTS es_rm07mtrb.
    CALL FUNCTION 'MB_ADD_TRANSFER_QUANTITY'
      EXPORTING
        cumulate            = ' '
        i_cross_company     = xnlcc
        i_non_cross_company = xtram
      TABLES
        xmatnr              = matnr
        xwerks              = werks
        xreswk              = reswk
        xsobkz              = sobkz
        xelikz              = elikz
        xloekz              = loekz
        xtab6               = xtab6
      EXCEPTIONS
        OTHERS              = 1.
    REFRESH bestand.
    LOOP AT xtab6.
*   Eliminate data not in CC-selection
      READ TABLE organ WITH KEY werks = xtab6-werks TRANSPORTING NO FIELDS
                                                    BINARY SEARCH.
      CHECK sy-subrc = 0.
*--------------------------------------------------------------------------------
      CLEAR: mat_doc , doc_year , xtab6-budat , xtab6-ststk , xtab6-cpudt.

      SELECT SINGLE belnr gjahr
        FROM ekbe
        INTO (mat_doc , doc_year )
        WHERE ebeln = xtab6-ebeln
      AND ebelp = xtab6-ebelp
      AND vgabe = 6
      AND bwart = 641.
      IF sy-subrc <> 0. CLEAR: mat_doc , doc_year. ENDIF.

      SELECT SINGLE budat cpudt
       FROM mkpf INTO (xtab6-budat , xtab6-cpudt)
       WHERE mblnr = mat_doc
       AND mjahr = doc_year.
      IF sy-subrc <> 0. CLEAR: xtab6-budat , xtab6-cpudt. ENDIF.

      IF xtab6-budat <> xtab6-cpudt.
        xtab6-chng = 'X'.
      ENDIF.

      IF xtab6-budat IS NOT INITIAL.
        xtab6-ststk = sy-datum - xtab6-budat.
      ENDIF.
*--------------------------------------------------------------------------------

      MOVE-CORRESPONDING xtab6 TO bestand.
      APPEND bestand.
      CLEAR: mat_doc , doc_year , xtab6-budat , xtab6-ststk , xtab6-cpudt.
    ENDLOOP.
*END-ENHANCEMENT-SECTION.
  ELSE.
*ENHANCEMENT-SECTION RM07MTRB_17 SPOTS ES_RM07MTRB .
    CALL FUNCTION 'MB_ADD_TRANSFER_QUANTITY'
      EXPORTING
        cumulate            = ' '
        i_cross_company     = xnlcc
        i_non_cross_company = xtram
      TABLES
        xmatnr              = matnr
        xberid              = berid
        xwerks              = werks
        xlgort              = lgort
        xresbi              = resbi
        xreswk              = reswk
        xreslo              = reslo
        xsobkz              = sobkz
        xelikz              = elikz
        xloekz              = loekz
        xtab6               = xtab6
      EXCEPTIONS
        OTHERS              = 1.
    REFRESH bestand.
    LOOP AT xtab6.
*   Eliminate data not in CC-selection
      READ TABLE organ WITH KEY werks = xtab6-werks TRANSPORTING NO FIELDS
                                                    BINARY SEARCH.
      CHECK sy-subrc = 0.
*--------------------------------------------------------------------------------
      CLEAR: mat_doc , doc_year , xtab6-budat , xtab6-ststk , xtab6-cpudt.

      SELECT SINGLE belnr gjahr
        FROM ekbe
        INTO (mat_doc , doc_year )
        WHERE ebeln = xtab6-ebeln
      AND ebelp = xtab6-ebelp
      AND vgabe = 6
      AND bwart = 641.
      IF sy-subrc <> 0. CLEAR: mat_doc , doc_year. ENDIF.

      SELECT SINGLE budat cpudt
        FROM mkpf INTO (xtab6-budat , xtab6-cpudt)
        WHERE mblnr = mat_doc
        AND mjahr = doc_year.
      IF sy-subrc <> 0. CLEAR: xtab6-budat , xtab6-cpudt. ENDIF.

      IF xtab6-budat <> xtab6-cpudt.
        xtab6-chng = 'X'.
      ENDIF.

      IF xtab6-budat IS NOT INITIAL.
        xtab6-ststk = sy-datum - xtab6-budat.
      ENDIF.
*--------------------------------------------------------------------------------
      MOVE-CORRESPONDING xtab6 TO bestand.
      APPEND bestand.
      CLEAR: mat_doc , doc_year , xtab6-budat , xtab6-ststk , xtab6-cpudt.
    ENDLOOP.
*END-ENHANCEMENT-SECTION.
  ENDIF.

  SORT organ BY werks.
  REFRESH makt_key.
* First round: Check authorizations, colorize, collect material numbers
  LOOP AT bestand.
* Color
    CLEAR color.
    REFRESH color.
    IF bestand-bstmg < 0.
      color-color-col = '6'.
      color-color-int = '0'.
    ELSEIF bestand-bstmg >= 0.
      color-color-col = '5'.
      color-color-int = '0'.
    ENDIF.
    color-fieldname = 'BSTMG'. APPEND color.
    color-fieldname = 'BSTME'. APPEND color.
    color-fieldname = 'NETWR'. APPEND color.
    color-fieldname = 'BWAER'. APPEND color.
    color-fieldname = 'MENGE'. APPEND color.
    color-fieldname = 'MEINS'. APPEND color.
    color-fieldname = 'DMBTR'. APPEND color.
    color-fieldname = 'WAERS'. APPEND color.
*ENHANCEMENT-POINT rm07mtrb_05 SPOTS es_rm07mtrb.
    bestand-color = color[].
* Authorizations
    IF NOT cl_mmim_auth=>check( i_object = 'M_MATE_WRK'
                                i_value1 = bestand-werks ) IS INITIAL.
      DELETE bestand.
      CONTINUE.
    ENDIF.
    READ TABLE organ WITH KEY werks = bestand-werks BINARY SEARCH.
    IF NOT cl_mmim_auth=>check( i_object = 'F_BKPF_BUK'
                                i_level  = cl_mmim_auth=>c_warning
                                i_value1 = organ-bukrs ) IS INITIAL.
      CLEAR: bestand-dmbtr, bestand-waers, bestand-netwr, bestand-bwaer.
    ENDIF.
* Collect
    makt_key-matnr = bestand-matnr.
    COLLECT makt_key.
    MODIFY bestand.
  ENDLOOP.
* Read materal short texts
  READ TABLE makt_key INDEX 1.
  IF sy-subrc = 0.
    SELECT matnr maktx INTO CORRESPONDING FIELDS OF TABLE imakt
     FROM makt FOR ALL ENTRIES IN makt_key WHERE matnr = makt_key-matnr
                                             AND spras = sy-langu.
    SORT imakt BY matnr.
  ENDIF.
* Second round: Fill data
  LOOP AT bestand.
    READ TABLE imakt WITH KEY matnr = bestand-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      bestand-maktx = imakt-maktx.
    ENDIF.
    READ TABLE organ WITH KEY werks = bestand-werks.
    IF sy-subrc = 0.
      bestand-name1 = organ-name1.
    ENDIF.
* check longnumber is active and fill the content
    DATA cl_change TYPE REF TO /sappspro/cl_numbers.
    DATA lf_active TYPE boolean.

    CLEAR: lf_active.
    CREATE OBJECT cl_change.

    CALL METHOD cl_change->is_active
      RECEIVING
        rv_active = lf_active.

    IF lf_active = 'X'.
      TRY.
          bestand-longnum =
          /sappspro/cl_numbers=>lookup( bestand-ebeln ).
        CATCH /sappspro/cx_number_not_found.
*             number does not exist.
      ENDTRY.
    ENDIF.

    MODIFY bestand.
  ENDLOOP.
ENDFORM.                    " DATENSELEKTION

************************************************************************
FORM feldkatalog_aufbauen USING p_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: fieldcat TYPE slis_fieldcat_alv.

* Kopffelder:
  CLEAR fieldcat.
  fieldcat-fieldname     = 'MATNR'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'EKPO'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'MAKTX'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'MAKT'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'WERKS'.
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'EKPO'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'NAME1'.  "umbenennen
  fieldcat-tabname       = 'HEADER'.
  fieldcat-ref_tabname   = 'T001W'.
  APPEND fieldcat TO p_fieldcat.
  IF NOT gv_dfps_active IS INITIAL.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'LGORT'.
    fieldcat-tabname       = 'HEADER'.
    fieldcat-ref_tabname   = 'EKPO'.
    APPEND fieldcat TO p_fieldcat.
  ENDIF.

* Positionsfelder:
  CLEAR fieldcat.
  fieldcat-fieldname     = 'EBELN'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKPO'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'EBELP'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKPO'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'RESWK'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKKO'.
  APPEND fieldcat TO p_fieldcat.
  IF NOT gv_dfps_active IS INITIAL.
    CLEAR fieldcat.
    fieldcat-fieldname     = 'RESLO'.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-ref_tabname   = 'EKPO'.
    APPEND fieldcat TO p_fieldcat.
  ENDIF.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'SOBKZ'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKPO'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'MENGE'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MSEG'.
  fieldcat-ref_fieldname = 'MENGE'.
  fieldcat-qfieldname    = 'MEINS'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'MEINS'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'MARA'.
  APPEND fieldcat TO p_fieldcat.
*ENHANCEMENT-POINT rm07mtrb_06 SPOTS es_rm07mtrb.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'DMBTR'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKBE'.
  fieldcat-cfieldname    = 'WAERS'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'WAERS'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKBE'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'BSTMG'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKPO'.
  fieldcat-ref_fieldname = 'MENGE'.
  fieldcat-qfieldname    = 'BSTME'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'BSTME'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKPO'.
  fieldcat-ref_fieldname = 'MEINS'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'NETWR'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_tabname   = 'EKPO'.
  fieldcat-cfieldname    = 'BWAER'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'BWAER'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-ref_fieldname = 'WAERS'.
  fieldcat-ref_tabname   = 'EKKO'.
  APPEND fieldcat TO p_fieldcat.
  CLEAR fieldcat.
  fieldcat-fieldname     = 'BUDAT'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-seltext_l     = 'Despatch Date'.
  fieldcat-seltext_m     = 'Despatch Date'.
  fieldcat-seltext_s     = 'Despatch Date'.
*  fieldcat-ref_fieldname = 'AERS'.
*  fieldcat-ref_tabname   = 'MKPF'.
  APPEND fieldcat TO p_fieldcat.


  CLEAR fieldcat.
  fieldcat-fieldname     = 'STSTK'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-seltext_l     = 'Days in Transit'.
  fieldcat-seltext_m     = 'Days in Transit'.
  fieldcat-seltext_s     = 'Days in Transit'.
*  fieldcat-ref_fieldname = 'AERS'.
*  fieldcat-ref_tabname   = 'EKET'.
  APPEND fieldcat TO p_fieldcat.


  CLEAR fieldcat.
  fieldcat-fieldname     = 'CHNG'.
  fieldcat-tabname       = 'BESTAND'.
  fieldcat-seltext_l     = 'Date Changed'.
  fieldcat-seltext_m     = 'Date Changed'.
  fieldcat-seltext_s     = 'Date Changed'.
*  fieldcat-ref_fieldname = 'AERS'.
*  fieldcat-ref_tabname   = 'EKET'.
  APPEND fieldcat TO p_fieldcat.
* insert longnum to fieldcat
  DATA cl_change TYPE REF TO /sappspro/cl_numbers.
  DATA lf_active TYPE boolean.

  CLEAR: lf_active.
  CREATE OBJECT cl_change.

  CALL METHOD cl_change->is_active
    RECEIVING
      rv_active = lf_active.

  IF lf_active = 'X'.
    CLEAR fieldcat.
    fieldcat-tabname       = 'BESTAND'.
    fieldcat-fieldname     = 'LONGNUM'.
    fieldcat-ref_tabname   = 'GOITEM'.
    fieldcat-ref_fieldname = 'LONGNUM'.
    APPEND fieldcat TO p_fieldcat.
  ENDIF.

ENDFORM.                    " FELDKATALOG_AUFBAUEN

************************************************************************
FORM listausgabe.

  SET TITLEBAR '100'.

  alv_keyinfo-header01 = 'MATNR'.
  alv_keyinfo-header02 = 'WERKS'.
  IF NOT gv_dfps_active IS INITIAL.
    alv_keyinfo-header03 = 'LGORT'.
  ENDIF.

  alv_keyinfo-item01   = 'MATNR'.
  alv_keyinfo-item02   = 'WERKS'.
  IF gv_dfps_active IS INITIAL.
    alv_keyinfo-item03   = 'EBELN'.
    alv_keyinfo-item04   = 'EBELP'.
  ELSE.
    alv_keyinfo-item03   = 'LGORT'.
    alv_keyinfo-item04   = 'EBELN'.
    alv_keyinfo-item05   = 'EBELP'.
  ENDIF.

  IF gv_dfps_active IS INITIAL.
    SORT bestand BY matnr werks ebeln ebelp.
    LOOP AT bestand.
      ON CHANGE OF bestand-matnr OR bestand-werks.
        MOVE-CORRESPONDING bestand TO header.
        APPEND header.
      ENDON.
    ENDLOOP.
  ELSE.
    SORT bestand BY matnr werks lgort ebeln ebelp.
    LOOP AT bestand.
      ON CHANGE OF bestand-matnr OR bestand-werks OR bestand-lgort.
        MOVE-CORRESPONDING bestand TO header.
        APPEND header.
      ENDON.
    ENDLOOP.
  ENDIF.
***** Start Code: Added by CS on 28.10.2015 for Authorization Object. *****
  IF header[] IS NOT INITIAL.
    IF lv_reswk_auth_flg = 'X' OR lv_werks_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Receiving Plant/Issue Plant.' TYPE 'I' DISPLAY LIKE 'W'.
*      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.
    IF lv_reswk_auth_flg = 'X' OR lv_werks_auth_flg = 'X'.
      MESSAGE 'No data found or Missing Authorization for Receiving Plant/Issue Plant.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
***** End Code: Added by CS on 28.10.2015 for Authorization Object. *****

  alv_layout-coltab_fieldname = 'COLOR'.
  detail = ' '.

 IF DET_LIST = 'X'.
 PERFORM detail_list.
 else.

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program       = alv_repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = alv_layout
      it_fieldcat              = fieldcat[]
      i_default                = 'X'
      i_save                   = 'A'
      is_variant               = alv_variant
      i_tabname_header         = 'HEADER'
      i_tabname_item           = 'BESTAND'
      is_keyinfo               = alv_keyinfo
      is_print                 = alv_print
    TABLES
      t_outtab_header          = header
      t_outtab_item            = bestand.
  ENDIF.
ENDFORM.                    " LISTAUSGABE

************************************************************************
FORM set_status USING rt_extab TYPE slis_t_extab.
  DATA: wa TYPE slis_extab.
  IF cl_mmim_auth=>level( ) IS INITIAL.
    APPEND 'AUTH' TO rt_extab.
  ENDIF.
  IF cl_mmim_auth=>level( ) = cl_mmim_auth=>c_error.
    MESSAGE s124(m7).
  ENDIF.
  IF detail = 'X'.
    wa-fcode = 'DETAIL'.
    APPEND wa TO rt_extab.
  ENDIF.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.                    "set_status

************************************************************************
FORM user_command USING rf_ucomm LIKE sy-ucomm
                        rs TYPE slis_selfield.
* Catch detail list
  IF rs-tabname = '1'.
    rs-tabname = 'BESTAND'.
  ENDIF.
* Double click
  IF rf_ucomm = '&IC1'.
    rf_ucomm = 'BEST'.
  ENDIF.
* Read table.
  CASE rs-tabname.
    WHEN 'HEADER'.
      CLEAR bestand.
      READ TABLE header INDEX rs-tabindex.
      MOVE-CORRESPONDING header TO bestand.
    WHEN 'BESTAND'.
      READ TABLE bestand INDEX rs-tabindex.
  ENDCASE.
  CHECK sy-subrc = 0.
* Execute command
  CASE rf_ucomm.
    WHEN 'BEST' OR 'HIST'.
      IF rs-tabname = 'BESTAND'.
        IF rf_ucomm = 'BEST'.
          CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
            EXPORTING
              i_ebeln      = bestand-ebeln
              i_enjoy      = 'X'
            EXCEPTIONS
              not_found    = 1
              no_authority = 2
              invalid_call = 3
              OTHERS       = 4.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ELSE.
          SUBMIT rm06ehbe AND RETURN
            WITH s_ebeln = bestand-ebeln
            WITH s_ebelp = bestand-ebelp
            WITH lesen    = 'Y'.
        ENDIF.
      ELSE.
        MESSAGE s131(m8).
      ENDIF.
    WHEN 'AUTH'.
      CALL METHOD cl_mmim_auth=>display.
    WHEN 'DETAIL'.
      PERFORM detail_list.
    WHEN 'MMBE'.
      SET PARAMETER ID 'MAT' FIELD bestand-matnr.
      SET PARAMETER ID 'WRK' FIELD bestand-werks.
      CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.
  ENDCASE.
  CLEAR rf_ucomm.
ENDFORM.                    "user_command

************************************************************************
FORM detail_list.
  DATA: fc_detail TYPE slis_fieldcat_alv OCCURS 0 WITH HEADER LINE.
  DATA: variant_detail      LIKE disvariant.
  DATA: lt_base_list LIKE bestand[].                        "401421
* The detail ALV may modify the list (sorting). If returned to the
* base list, the original list needs to be restored.
  lt_base_list[] = bestand[].                               "401421
  REFRESH fc_detail.
  LOOP AT fieldcat.
    MOVE-CORRESPONDING fieldcat TO fc_detail.
    CLEAR fc_detail-tabname.
    APPEND fc_detail.
  ENDLOOP.
  CLEAR variant_detail.
  variant_detail-report = alv_repid.
  variant_detail-handle = 'DETA'.
  detail = 'X'. "needed for GUI status differentiation

  CALL FUNCTION alv_detail_func
    EXPORTING
      i_callback_program       = alv_repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = fc_detail[]
      i_default                = 'X'
      is_variant               = variant_detail
      i_save                   = 'A'
      is_layout                = alv_layout
      is_print                 = alv_print
    TABLES
      t_outtab                 = bestand[].
  CLEAR detail.
  bestand[] = lt_base_list[].                               "401421
ENDFORM.                    "detail_list
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 23.10.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth_obj .
  TYPES: BEGIN OF ty_t001w,
          werks TYPE t001w-werks, " Plants
        END OF ty_t001w.
  DATA: t_t001w TYPE TABLE OF ty_t001w, " Receiving Plants
        w_t001w TYPE ty_t001w,
        ts_t001w TYPE TABLE OF ty_t001w, " Issue Plants
        ws_t001w TYPE ty_t001w.

  FREE : t_t001w[], ts_t001w[].
  CLEAR: w_t001w, ws_t001w.

  break test1.

***** Start Code: Added by CS on 23.10.2015 for Receiving Plant Authorization. *****
  SELECT werks  " Fetch values of Plant
    FROM t001w
    INTO TABLE t_t001w
    WHERE werks IN werks.

  CLEAR: werks, lv_werks_auth_flg.
  REFRESH: werks[].
  IF t_t001w[] IS NOT INITIAL.
    LOOP AT t_t001w INTO w_t001w.
      AUTHORITY-CHECK OBJECT 'ZRPLANT' " Receiving Plant
                     ID 'ACTVT' FIELD '03'
                     ID 'WERKS' FIELD w_t001w-werks.

      IF sy-subrc EQ 0.
        werks-sign = 'I'.
        werks-option = 'EQ'.
        werks-low = w_t001w-werks.
        APPEND werks.
        CLEAR: werks.
      ELSE.
        IF lv_werks_auth_flg IS INITIAL.  " Authorization Flag
          lv_werks_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t001w.
    ENDLOOP.
  ENDIF.
  IF werks[] IS INITIAL.
    werks-sign = 'I'.
    werks-option = 'EQ'.
    werks-low = ''.
    APPEND werks.
    CLEAR: werks.
  ENDIF.
***** End Code: Added by CS on 23.10.2015 for Receiving Plant Authorization. *****

***** Start Code: Added by CS on 28.10.2015 for Issue Plant Authorization. *****
  SELECT werks  " Fetch values of Issue Plant
    FROM t001w
    INTO TABLE ts_t001w
    WHERE werks IN reswk.

  CLEAR: reswk, lv_reswk_auth_flg.
  REFRESH: reswk[].
  IF ts_t001w[] IS NOT INITIAL.
    LOOP AT ts_t001w INTO ws_t001w.
      AUTHORITY-CHECK OBJECT 'ZSPLANT' " Issue Plant
                     ID 'ACTVT' FIELD '03'
                     ID 'RESWK' FIELD ws_t001w-werks.

      IF sy-subrc EQ 0.
        reswk-sign = 'I'.
        reswk-option = 'EQ'.
        reswk-low = ws_t001w-werks.
        APPEND reswk.
        CLEAR: reswk.
      ELSE.
        IF lv_reswk_auth_flg IS INITIAL.  " Authorization Flag
          lv_reswk_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: ws_t001w.
    ENDLOOP.
  ENDIF.
  IF reswk[] IS INITIAL.
    reswk-sign = 'I'.
    reswk-option = 'EQ'.
    reswk-low = ''.
    APPEND reswk.
    CLEAR: reswk.
  ENDIF.
***** End Code: Added by CS on 28.10.2015 for Issue Plant Authorization. *****

ENDFORM.                    " CHECK_AUTH_OBJ
