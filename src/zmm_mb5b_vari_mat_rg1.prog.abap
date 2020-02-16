*&---------------------------------------------------------------------*
*& Report  ZMM_MB5B_VARI_MAT_RG1                                       *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Program Name        :  ZMM_MB5B_VARI_MAT_RG1                        *
*& Creation Date       :  Nov 24, 2016                                 *
*& Author              :  Naren Karra                                  *
*& Functional          :  Ashish Kothari(KPMG)                         *
*& Application Area    :  MM                                           *
*& Development Spec ID :                                               *
*&---------------------------------------------------------------------*
*& Description:        :  Material Document posted in RG1 register     *
*& Inputs              :                                               *
*& Outputs             :                                               *
*& Scheduling          :  Back ground and foreground.                  *
*& External Routines   :  N/A                                          *
*& Assumptions/Restriction:                                            *
*& Change History:                                                     *
*&=====================================================================*
*& Date          | Change #              | Changed By | Description    *
*& Nov 24, 2016  | IRDK926365            | IBM_AMS    | Initial        *
*&                                                      Development    *
*----------------------------------------------------------------------*
REPORT  zmm_mb5b_vari_mat_rg1.
TYPE-POOLS:slis.
*----------------------------------------------------------------------*
*  I N C L U D E S                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*  T Y P E S    D E C L A R A T I O N S                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_var_view,
        matnr     TYPE matnr,
        maktx     TYPE maktx,
        steuc     TYPE steuc,
        meins     TYPE meins,
        menge_sap TYPE menge_d,
        menge_rg1 TYPE menge_d,
        menge_dif TYPE menge_d,
       END OF type_var_view,

       tt_var_view TYPE STANDARD TABLE OF type_var_view,

      BEGIN OF type_var_detail,
        mblnr      TYPE mblnr,
        cpudt      TYPE cpudt,
        matnr      TYPE matnr,
        maktx      TYPE maktx,
        bwart      TYPE bwart,
        mat_im(3)  TYPE c,
        mat_rg1(3) TYPE c,
       END OF type_var_detail,

       tt_var_detail TYPE STANDARD TABLE OF type_var_detail,

      BEGIN OF type_mkpf,
        mandt TYPE mandt,
        mblnr TYPE mblnr,
        mjahr TYPE mjahr,
        vgart TYPE vgart,
        budat TYPE budat,
        cpudt TYPE cpudt,
       END OF type_mkpf,

       tt_mkpf TYPE STANDARD TABLE OF type_mkpf,

       BEGIN OF type_mseg,
         mandt TYPE mandt,
         mblnr TYPE mblnr,
         mjahr TYPE mjahr,
         zeile TYPE mblpo,
         bwart TYPE bwart,
         xauto TYPE mb_xauto,
         matnr TYPE matnr,
         werks TYPE werks_d,
         lgort TYPE lgort_d,
         charg TYPE charg_d,
       END OF type_mseg,

       tt_mseg TYPE STANDARD TABLE OF type_mseg,

         BEGIN OF type_asci,
           line(200),
         END OF type_asci,

         BEGIN OF type_falv1,
          matnr TYPE matnr,
         END OF type_falv1,

         BEGIN OF type_falv2,
          menge TYPE menge_d,
          meins TYPE meins,
         END OF type_falv2,

         BEGIN OF type_j_2irg1bal,
          exgrp     TYPE j_2irg1bal-exgrp,
          datum     TYPE j_2irg1bal-datum,
          matnr     TYPE j_2irg1bal-matnr,
          werks     TYPE j_2irg1bal-werks,
          cb_finish TYPE j_2irg1bal-cb_finish,
         END OF type_j_2irg1bal,

         BEGIN OF type_j_1irg1,
         exgrp TYPE j_1irg1-exgrp,
         syear TYPE j_1irg1-syear,
         serialno TYPE j_1irg1-serialno,
         mblnr TYPE j_1irg1-mblnr,
         mjahr TYPE j_1irg1-mjahr,
         zeile TYPE j_1irg1-zeile,
         bwart TYPE j_1irg1-bwart,
         matnr TYPE j_1irg1-matnr,
         END OF type_j_1irg1,

         BEGIN OF type_zvari_det,
           bwart TYPE zvari_det-bwart,
           xauto TYPE zvari_det-xauto,
         END OF type_zvari_det,

         BEGIN OF type_j_1imtchid,
        mandt TYPE mandt,
        matnr TYPE matnr,
        werks TYPE werks_d,
        j_1ichid TYPE	j_1ichid,
        j_1isubind TYPE	j_1isubind,
        j_1icapind TYPE	j_1icapind,
        j_1igrxref TYPE	j_1igrex,
        j_1idecflag TYPE j_1ideclag,
        j_1idecdate TYPE j_1idecdate,
        aedat TYPE aedat,
        usnam TYPE j_1iusnamc,
      END OF type_j_1imtchid.

*----------------------------------------------------------------------*
*  D A T A    D E C L A R A T I O N S                                  *
*----------------------------------------------------------------------*
DATA: gv_datum    TYPE sy-datum,                            "#EC NEEDED
      gv_total    TYPE i,                                   "#EC NEEDED
      gv_success  TYPE i,                                   "#EC NEEDED
      gv_failure  TYPE i,                                   "#EC NEEDED
      gv_werks    TYPE ekpo-werks,                          "#EC NEEDED
      gv_budat    TYPE mkpf-budat,                          "#EC NEEDED
      gv_date_low(10),                                      "#EC NEEDED
      gv_date_high(10).                                     "#EC NEEDED

DATA: gt_var_view TYPE STANDARD TABLE OF type_var_view,     "#EC NEEDED
      gt_mkpf     TYPE STANDARD TABLE OF type_mkpf,         "#EC NEEDED
      gt_mseg     TYPE STANDARD TABLE OF type_mseg,         "#EC NEEDED
      gt_var_detail TYPE STANDARD TABLE OF type_var_detail, "#EC NEEDED
      gt_j_1irg1  TYPE STANDARD TABLE OF type_j_1irg1,      "#EC NEEDED
      gt_vari_det TYPE STANDARD TABLE OF type_zvari_det.    "#EC NEEDED

DATA: gs_j_1imtchid TYPE type_j_1imtchid,
      gt_j_1imtchid TYPE STANDARD TABLE OF type_j_1imtchid.
*----------------------------------------------------------------------*
*  C O N S T A N T S     D E C L A R A T I O N S                       *
*----------------------------------------------------------------------*
CONSTANTS: formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
DATA: fieldtab   TYPE slis_t_fieldcat_alv,
      fieldtab1  TYPE slis_t_fieldcat_alv,
      p_heading  TYPE slis_t_listheader,
      layout     TYPE slis_layout_alv,
      events     TYPE slis_t_event,
      repname    LIKE sy-repid,
      g_variant  TYPE disvariant,
      gx_variant TYPE disvariant,
      p_vairaint TYPE disvariant,
      alv_print  TYPE slis_print_alv,
      g_save,                                               "#EC NEEDED
      g_exit,                                               "#EC NEEDED
      flag.                                                 "#EC NEEDED
*----------------------------------------------------------------------*
*   S E L E C T I O N    S C R E E N                                   *
*----------------------------------------------------------------------*
* Selection Screen Block 1
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-b01.
PARAMETERS: p_over   TYPE c NO-DISPLAY ."RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND usr1.
PARAMETERS: p_detail TYPE c NO-DISPLAY DEFAULT 'X'."RADIOBUTTON GROUP g1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE text-b05.
PARAMETERS: p_sku1   TYPE c NO-DISPLAY."RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND usr2 MODIF ID m1.
PARAMETERS: p_sku2   TYPE c NO-DISPLAY."RADIOBUTTON GROUP g2 MODIF ID m1.
SELECTION-SCREEN END OF BLOCK b05.
SELECTION-SCREEN END OF BLOCK b01.

* Selection Screen Block 2
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-b02.
PARAMETERS: p_werks1 TYPE ekpo-werks NO-DISPLAY ."MODIF ID m1.
SELECT-OPTIONS: s_budat1 FOR gv_budat MODIF ID m1 NO-DISPLAY." OBLIGATORY .
SELECTION-SCREEN END OF BLOCK b02.

* Selection Screen Block 3-1

* Selection Screen Block 3
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03.
PARAMETERS: p_matnr  TYPE mara-matnr MODIF ID m2 NO-DISPLAY,
            p_werks2 TYPE ekpo-werks MODIF ID m2 OBLIGATORY.
SELECT-OPTIONS: s_budat2 FOR gv_budat MODIF ID m2 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b03.

* Selection Screen Block 4
SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-b04.
PARAMETERS: p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b04.
*----------------------------------------------------------------------*
*   I N I T I A L I Z A T I O N                                        *
*----------------------------------------------------------------------*
INITIALIZATION.
  repname = sy-repid.
  PERFORM initialize_variant.
  PERFORM build_eventtab USING events[].
*----------------------------------------------------------------------*
*   A T   S E L E C T I O N    S C R E E N    O U T P U T              *
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
** Modify the selection screen based on the radiobutton control
*  PERFORM sub_modify_selscreen.
*----------------------------------------------------------------------*
*   A T   S E L E C T I O N    S C R E E N                             *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.
*----------------------------------------------------------------------*
*   S T A R T   O F   S E L E C T I O N                                *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM build_comment USING p_heading[].
  PERFORM sub_fetch_zmb5b_data.
  PERFORM initialize_fieldcat  USING fieldtab[].
  PERFORM initialize_fieldcat1 USING fieldtab1[].
*----------------------------------------------------------------------*
*   E N D   O F   S E L E C T I O N                                    *
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_var_view[] IS NOT INITIAL.
    PERFORM display_list_view.
  ELSEIF gt_var_detail[] IS NOT INITIAL.
    PERFORM display_list_detail.
  ELSE.
    MESSAGE 'No data found. Kindly check the selection' TYPE 'S' DISPLAY LIKE 'W'. "#EC NOTEXT
  ENDIF.
*----------------------------------------------------------------------*
*   S U B R O U T I N E S                                              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_MODIFY_SELSCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_modify_selscreen .
*  LOOP AT SCREEN.
*    CASE screen-group1.
*      WHEN 'M1'.
*        IF p_over IS INITIAL.
*          screen-name = s_budat1.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*
*        IF p_over IS INITIAL.
*          screen-name = p_werks1.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*
*        IF p_over IS INITIAL.
*          screen-name = p_sku1.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*
*        IF p_over IS INITIAL.
*          screen-name = p_sku2.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*      WHEN 'M2'.
*        IF p_detail IS INITIAL.
*          screen-name = p_matnr.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*
*        IF p_detail IS INITIAL.
*          screen-name = p_werks2.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*
*        IF p_detail IS INITIAL.
*          screen-name = s_budat2.
*          screen-active = '0'.
*          MODIFY SCREEN.
*        ENDIF.
*    ENDCASE.
*  ENDLOOP.
ENDFORM.                    " SUB_MODIFY_SELSCREEN
*&---------------------------------------------------------------------*
*&      Form  SUB_FETCH_ZMB5B_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_fetch_zmb5b_data .
* Stocks for Posting Date
  DATA: lt_seltab     TYPE TABLE OF rsparams,               "#EC NEEDED
        lt_listtab    TYPE TABLE OF abaplist,
        lt_asci       TYPE STANDARD TABLE OF type_asci,
        lt_j_2irg1bal TYPE STANDARD TABLE OF type_j_2irg1bal,
        ls_seltab     TYPE rsparams,                        "#EC NEEDED
        ls_listtab    TYPE abaplist,                        "#EC NEEDED
        ls_asci       TYPE type_asci,                       "#EC NEEDED
        ls_var_view   TYPE type_var_view,
        ls_mkpf       TYPE type_mkpf,
        ls_mseg       TYPE type_mseg,
        ls_var_detail TYPE type_var_detail,
        ls_j_2irg1bal TYPE type_j_2irg1bal,
        ls_j_1irg1    TYPE type_j_1irg1,
        ls_vari_det   TYPE type_zvari_det,
        lv_index      TYPE sy-tabix,
        lv_j_1iexcgrp TYPE j_1iexgrps-j_1iexcgrp,
        lv_serialno   TYPE j_1irg1-serialno,
        lv_text(8),                                         "#EC NEEDED
* Import - Export memory IDs parameters
        i_falv1       TYPE STANDARD TABLE OF type_falv1,
        s_falv1       TYPE type_falv1,
        i_falv2       TYPE STANDARD TABLE OF type_falv2,
        s_falv2       TYPE type_falv2.
********************************************************************** start ~nk on 24.11.2016
  SELECT mandt
         mblnr
         mjahr
         vgart
         budat
         cpudt
    FROM mkpf
    INTO TABLE gt_mkpf
    WHERE cpudt IN s_budat2 ORDER BY PRIMARY KEY.       "#EC CI_NOFIELD

  SELECT mandt
         matnr
         werks
         j_1ichid
         j_1isubind
         j_1icapind
         j_1igrxref
         j_1idecflag
         j_1idecdate
         aedat
         usnam
    FROM j_1imtchid
    INTO TABLE gt_j_1imtchid
    WHERE werks EQ p_werks2
    AND j_1icapind EQ 'F' .

  SORT gt_mkpf BY mblnr.
  IF gt_mkpf IS NOT INITIAL.
    SELECT mandt
           mblnr
           mjahr
           zeile
           bwart
           xauto
           matnr
           werks
           lgort
           charg
      FROM mseg
      INTO TABLE gt_mseg
      FOR ALL ENTRIES IN gt_mkpf
      WHERE mblnr EQ gt_mkpf-mblnr
      AND   xauto EQ ''
      AND   bwart IN ('601', '602', '641', '642', '651', '701', '702', '911')
      AND   werks EQ p_werks2
      AND   lgort EQ '1501' ORDER BY PRIMARY KEY.
  BREAK 10106.

  SELECT mandt
           mblnr
           mjahr
           zeile
           bwart
           xauto
           matnr
           werks
           lgort
           charg
      FROM mseg
      APPENDING TABLE gt_mseg
      FOR ALL ENTRIES IN gt_mkpf
      WHERE mblnr EQ gt_mkpf-mblnr
      AND   xauto EQ 'X'
      AND   bwart = '311'
      AND   werks EQ p_werks2
      AND   lgort EQ '1501' ORDER BY PRIMARY KEY.


  ENDIF.

  CLEAR lv_j_1iexcgrp.
  SELECT SINGLE j_1iexcgrp FROM j_1iexgrps INTO lv_j_1iexcgrp WHERE j_1iwerks = p_werks2. "#EC *

  IF gt_mseg IS NOT INITIAL.
    IF lv_j_1iexcgrp IS NOT INITIAL.
      SELECT exgrp
             syear
             serialno
             mblnr
             mjahr
             zeile
             bwart
             matnr
         FROM j_1irg1
         INTO TABLE gt_j_1irg1
         FOR ALL ENTRIES IN gt_mseg
         WHERE exgrp EQ lv_j_1iexcgrp
         AND   mblnr EQ gt_mseg-mblnr
         AND   mjahr EQ gt_mseg-mjahr
         AND   zeile EQ gt_mseg-zeile.                      "#EC *
    ENDIF.
  ENDIF.
********************************************************************** end   ~nk on 24.11.2016
*  P_OVER = 'X' - Variance Overview
  IF p_over = 'X'.
    IF p_werks1 IS NOT INITIAL.
      ls_seltab-selname = 'WERKS'.
      ls_seltab-kind    = 'S'.
      ls_seltab-sign    = 'I'.
      ls_seltab-option  = 'EQ'.
      ls_seltab-low     = p_werks1.
      APPEND ls_seltab TO lt_seltab.
    ELSEIF p_werks2 IS NOT INITIAL.
      ls_seltab-selname = 'WERKS'.
      ls_seltab-kind    = 'S'.
      ls_seltab-sign    = 'I'.
      ls_seltab-option  = 'EQ'.
      ls_seltab-low     = p_werks2.
      APPEND ls_seltab TO lt_seltab.
    ENDIF.

    ls_seltab-selname = 'LGORT'.
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = '1501'.
    APPEND ls_seltab TO lt_seltab.

    ls_seltab-selname = 'DATUM'.  " mkpf-budat
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'BT'.
    ls_seltab-low     = s_budat1-low.
    ls_seltab-high    = s_budat1-high.
    APPEND ls_seltab TO lt_seltab.

*{   REPLACE        SBXK900339                                        1
*\    SUBMIT zrm07mlbd WITH SELECTION-TABLE lt_seltab EXPORTING LIST TO MEMORY AND RETURN .
    SUBMIT rm07mlbd WITH SELECTION-TABLE lt_seltab EXPORTING LIST TO MEMORY AND RETURN .
*}   REPLACE

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = lt_listtab
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'LIST_TO_ASCI'
      TABLES
        listasci           = lt_asci
        listobject         = lt_listtab
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IMPORT i_falv1 TO i_falv1 FROM MEMORY ID 'ZMB5B_NK1'.
    IMPORT i_falv2 TO i_falv2 FROM MEMORY ID 'ZMB5B_NK2'.

    LOOP AT i_falv1 INTO s_falv1.
      lv_index = sy-tabix.
      MOVE:
            s_falv1-matnr TO ls_var_view-matnr.
      SELECT SINGLE maktx FROM makt INTO ls_var_view-maktx WHERE spras = sy-langu AND matnr = s_falv1-matnr.

      SELECT SINGLE steuc FROM marc INTO ls_var_view-steuc WHERE matnr = s_falv1-matnr AND werks = p_werks1.

      READ TABLE i_falv2 INTO s_falv2 INDEX lv_index.
      IF sy-subrc EQ 0.
        MOVE:
            s_falv2-menge TO ls_var_view-menge_sap,
            s_falv2-meins TO ls_var_view-meins.
      ENDIF.

      CLEAR lv_j_1iexcgrp.
      SELECT SINGLE j_1iexcgrp FROM j_1iexgrps INTO lv_j_1iexcgrp WHERE j_1iwerks = p_werks1. "#EC *

      SELECT exgrp
             datum
             matnr
             werks
             cb_finish
        FROM j_2irg1bal
        INTO TABLE lt_j_2irg1bal
        WHERE exgrp = lv_j_1iexcgrp
        AND   matnr = ls_var_view-matnr.

      SORT lt_j_2irg1bal DESCENDING.
*  Fetching the entry with the latest date and it's CB_FINISH quantity
      READ TABLE lt_j_2irg1bal INTO ls_j_2irg1bal INDEX 1.
      IF sy-subrc EQ 0.
        MOVE:
             ls_j_2irg1bal-cb_finish TO ls_var_view-menge_rg1.
      ENDIF.

      ls_var_view-menge_dif = ls_var_view-menge_sap - ls_var_view-menge_rg1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = ls_var_view-matnr
        IMPORTING
          output = ls_var_view-matnr.

      APPEND ls_var_view TO gt_var_view.
      CLEAR: ls_var_view.
    ENDLOOP.
* This is to delete entires with null Variance column (column 7)
* Since this report is going to be scheduled in Bckgrd and sent to users - null data is not required !

    IF p_sku2 = 'X'.
      DELETE gt_var_view WHERE menge_dif = ''.
    ENDIF.

*  P_DETAIL = 'X' - Variance Detail
  ELSEIF p_detail = 'X'.
*    SELECT mandt
*           mblnr
*           mjahr
*           vgart
*           budat
*           cpudt
*      FROM mkpf
*      INTO TABLE gt_mkpf
*      WHERE cpudt IN s_budat2 ORDER BY PRIMARY KEY.     "#EC CI_NOFIELD
*
*    SORT gt_mkpf BY mblnr.
*
*    IF gt_mkpf IS NOT INITIAL.
*      IF p_matnr IS INITIAL.
*        SELECT mandt
*               mblnr
*               mjahr
*               zeile
*               bwart
*               xauto
*               matnr
*               werks
*               lgort
*               charg
*          FROM mseg
*          INTO TABLE gt_mseg
*          FOR ALL ENTRIES IN gt_mkpf
*          WHERE mblnr EQ gt_mkpf-mblnr
*          AND   bwart IN ('311', '601', '602', '641', '642', '651', '701', '702', '911')
*          AND   werks EQ p_werks2
*          AND   lgort EQ '1501' ORDER BY PRIMARY KEY.
*
*      ELSEIF p_matnr IS NOT INITIAL.
*        SELECT mandt
*               mblnr
*               mjahr
*               zeile
*               bwart
*               xauto
*               matnr
*               werks
*               lgort
*               charg
*          FROM mseg
*          INTO TABLE gt_mseg
*          FOR ALL ENTRIES IN gt_mkpf
*          WHERE mblnr EQ gt_mkpf-mblnr
*          AND   bwart IN ('311', '601', '602', '641', '642', '651', '701', '702', '911')
*          AND   matnr EQ p_matnr
*          AND   werks EQ p_werks2
*          AND   lgort EQ '1501' ORDER BY PRIMARY KEY.
*      ENDIF.
*    ENDIF.
*
*    CLEAR lv_j_1iexcgrp.
*    SELECT SINGLE j_1iexcgrp FROM j_1iexgrps INTO lv_j_1iexcgrp WHERE j_1iwerks = p_werks2. "#EC *
*
*    IF gt_mseg IS NOT INITIAL.
*      IF lv_j_1iexcgrp IS NOT INITIAL.
*        SELECT exgrp
*               syear
*               serialno
*               mblnr
*               mjahr
*               zeile
*               bwart
*               matnr
*           FROM j_1irg1
*           INTO TABLE gt_j_1irg1
*           FOR ALL ENTRIES IN gt_mseg
*           WHERE exgrp EQ lv_j_1iexcgrp
*           AND   mblnr EQ gt_mseg-mblnr
*           AND   mjahr EQ gt_mseg-mjahr
*           AND   zeile EQ gt_mseg-zeile.                    "#EC *
*      ENDIF.
*    ENDIF.

    SELECT bwart
           xauto
      FROM zvari_det
      INTO TABLE gt_vari_det.                               "#EC *
    DATA : lv_maktx TYPE makt-maktx,
           lv_bwart TYPE mseg-bwart.

    SORT gt_mkpf BY mblnr.
    SORT gt_mseg BY mblnr.
    SORT gt_vari_det BY bwart xauto.
    SORT gt_j_1irg1 BY exgrp mblnr mjahr zeile.
    CLEAR ls_mkpf.
    LOOP AT gt_mkpf INTO ls_mkpf.
      MOVE: ls_mkpf-mblnr TO ls_var_detail-mblnr.

      ls_var_detail-cpudt = ls_mkpf-cpudt.

      CLEAR ls_mseg.
      READ TABLE gt_mseg INTO ls_mseg WITH KEY mblnr = ls_mkpf-mblnr BINARY SEARCH.
      IF sy-subrc EQ 0.
* Material document (Column 1), Material no in Column 2 and Tick / yes in column 3.
        MOVE:
        ls_mseg-matnr TO ls_var_detail-matnr.
        ls_var_detail-bwart = ls_mseg-bwart.

        CLEAR lv_maktx.
        SELECT SINGLE maktx FROM makt INTO lv_maktx WHERE matnr = ls_var_detail-matnr AND spras = sy-langu.
        ls_var_detail-maktx = lv_maktx.

        ls_var_detail-mat_im = 'Yes'.
        READ TABLE gt_vari_det INTO ls_vari_det WITH KEY bwart = ls_mseg-bwart.
        IF sy-subrc EQ 0.
          READ TABLE gt_mseg INTO ls_mseg WITH KEY mblnr = ls_mkpf-mblnr bwart = ls_vari_det-bwart xauto = ls_vari_det-xauto.
          IF sy-subrc EQ 0.

            READ TABLE gt_j_1irg1 INTO ls_j_1irg1 WITH KEY exgrp = lv_j_1iexcgrp
                                                            mblnr = ls_mseg-mblnr
                                                            mjahr = ls_mseg-mjahr
                                                            zeile = ls_mseg-zeile BINARY SEARCH.
            IF sy-subrc EQ 0.
              ls_var_detail-mat_rg1 = 'Yes'.
            ELSE.
              CLEAR ls_var_detail-mat_rg1.
            ENDIF.
          ENDIF.
        ENDIF.


        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = ls_var_detail-matnr
          IMPORTING
            output = ls_var_detail-matnr.

        APPEND ls_var_detail TO gt_var_detail.
        CLEAR: ls_var_detail, ls_var_detail-mat_im, ls_var_detail-mat_rg1.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SUB_FETCH_ZMB5B_DATA
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED
  DATA : rs_variant TYPE disvariant,
         pline TYPE slis_listheader,
         v_lines TYPE i.
  CLEAR rs_variant.
*  IMPORT rs_variant FROM MEMORY ID 'VARIANT'.

  IF NOT rs_variant-text IS INITIAL.
    pline-typ = 'S'.
    pline-info = rs_variant-text.
    APPEND pline TO p_heading.
  ENDIF.

  CALL FUNCTION 'Z6XX_REUSE_ALV_COMMENTARY_WR'
    EXPORTING
      it_list_commentary = p_heading.

  IF NOT rs_variant-text IS INITIAL.
    DESCRIBE TABLE p_heading LINES v_lines.
    DELETE p_heading INDEX v_lines.
  ENDIF.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_HEADING[]  text
*----------------------------------------------------------------------*
FORM build_comment  USING    p_p_heading TYPE slis_t_listheader. "#EC NEEDED
  DATA: s_variant LIKE p_vairaint.                          "#EC NEEDED
  DATA: hline     TYPE slis_listheader,
        text(60)  TYPE c,
        sep(20)   TYPE c.                                   "#EC NEEDED
*        lv_date_low(10),
*        lv_date_high(10).

  IF p_over = 'X'.
    CLEAR hline.
    hline-typ  = 'H'.
    hline-info = '       Variance Overview  '.              "#EC NOTEXT
    APPEND hline TO p_heading.                              "#EC *
    CLEAR text.
  ELSEIF p_detail = 'X'.
    CLEAR hline.
    hline-typ  = 'H'.
    hline-info = '       Material Document Posted in RG1 Register'. "#EC NOTEXT
    APPEND hline TO p_heading.
    CLEAR text.
  ENDIF.
  IF p_over = 'X'.
    CLEAR: gv_date_low, gv_date_high.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = s_budat1-low
      IMPORTING
        output = gv_date_low.

    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = s_budat1-high
      IMPORTING
        output = gv_date_high.

    CONCATENATE 'Date From:   ' gv_date_low ' To ' gv_date_high INTO text SEPARATED BY space. "#EC NOTEXT
    hline-typ  = 'S'.
    hline-info = text.
    APPEND hline TO p_heading.
    CLEAR text.
  ELSEIF p_detail = 'X'.
    CLEAR: gv_date_low, gv_date_high.
    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = s_budat2-low
      IMPORTING
        output = gv_date_low.

    CALL FUNCTION 'CONVERSION_EXIT_PDATE_OUTPUT'
      EXPORTING
        input  = s_budat2-high
      IMPORTING
        output = gv_date_high.

    CONCATENATE 'Date From:  ' gv_date_low ' To ' gv_date_high INTO text SEPARATED BY space. "#EC NOTEXT
    hline-typ  = 'S'.
    hline-info = text.
    APPEND hline TO p_heading.
    CLEAR text.
  ENDIF.
ENDFORM.                    " BUILD_COMMENT
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDTAB[]  text
*----------------------------------------------------------------------*
FORM initialize_fieldcat  USING    p_fieldtab TYPE slis_t_fieldcat_alv.
  DATA:
        fieldcat TYPE slis_fieldcat_alv,
        lv_pos TYPE i.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MATNR'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'ITEM CODE'.
  fieldcat-outputlen  = 18.
  APPEND fieldcat TO p_fieldtab.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MAKTX'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'ITEM DESCRIPTION'.
  fieldcat-outputlen  = 35.
  APPEND fieldcat TO p_fieldtab.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'STEUC'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'CHAPTER ID'.
  fieldcat-outputlen  = 16.
  APPEND fieldcat TO p_fieldtab.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MEINS'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'UOM'.
  fieldcat-outputlen  = 3.
  APPEND fieldcat TO p_fieldtab.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MENGE_SAP'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'QTY. AS PER SAP'.
  fieldcat-outputlen  = 13.
  APPEND fieldcat TO p_fieldtab.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MENGE_RG1'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'QTY. AS PER RG1'.
  fieldcat-outputlen  = 13.
  APPEND fieldcat TO p_fieldtab.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MENGE_DIF'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'DIFFERENCE'.
  fieldcat-outputlen  = 13.
  APPEND fieldcat TO p_fieldtab.
  CLEAR fieldcat.
ENDFORM.                    " INITIALIZE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list_view .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = repname
      is_layout          = layout
      i_buffer_active    = ' '
      it_fieldcat        = fieldtab[]
      i_save             = g_save
      is_variant         = g_variant
      it_events          = events[]
      is_print           = alv_print
    TABLES
      t_outtab           = gt_var_view.
ENDFORM.                    " DISPLAY_LIST_VIEW
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EVENTS[]  text
*----------------------------------------------------------------------*
FORM build_eventtab  USING    p_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = p_events.
  READ TABLE p_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE formname_top_of_page TO ls_event-form.
    APPEND ls_event TO p_events.
  ENDIF.
ENDFORM.                    " BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_variant .
  IF p_over = 'X'.
    g_save = 'A'.
    CLEAR g_variant.
    g_variant-report = repname.
    g_variant-variant = p_vari.
    gx_variant = g_variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant
      EXCEPTIONS
        not_found  = 2.
    IF sy-subrc = 0.
      p_vari = gx_variant-variant.
      g_variant = gx_variant.
    ENDIF.

    layout-get_selinfos = 'X'.
    layout-group_change_edit = 'X'.
  ELSEIF p_detail = 'X'.
    g_save = 'A'.
    CLEAR g_variant.
    g_variant-report = repname.
    g_variant-variant = p_vari.
    gx_variant = g_variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant
      EXCEPTIONS
        not_found  = 2.
    IF sy-subrc = 0.
      p_vari = gx_variant-variant.
      g_variant = gx_variant.
    ENDIF.

    layout-get_selinfos = 'X'.
    layout-group_change_edit = 'X'.
  ENDIF.
  alv_print-no_print_selinfos  = 'X'.
  alv_print-no_coverpage       = 'X'.
  alv_print-no_print_listinfos = 'X'.

ENDFORM.                    " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_for_variant .
  IF p_over = 'X'.
    g_save = 'A'.
    g_variant-report = sy-repid.
    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant = g_variant
        i_save     = g_save
      IMPORTING
        e_exit     = g_exit
        es_variant = gx_variant
      EXCEPTIONS
        not_found  = 2.
    IF sy-subrc = 2.
      MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF g_exit = space.
        p_vari = gx_variant-variant.
      ENDIF.
      flag = 1.
    ENDIF.
  ELSEIF p_detail = 'X'.
    g_save = 'A'.
    g_variant-report = sy-repid.
    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant = g_variant
        i_save     = g_save
      IMPORTING
        e_exit     = g_exit
        es_variant = gx_variant
      EXCEPTIONS
        not_found  = 2.
    IF sy-subrc = 2.
      MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF g_exit = space.
        p_vari = gx_variant-variant.
      ENDIF.
      flag = 1.
    ENDIF.
  ENDIF.
ENDFORM.                    " F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_list_detail .
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = repname
      is_layout          = layout
      i_buffer_active    = ' '
      it_fieldcat        = fieldtab1[]
      i_save             = g_save
      is_variant         = g_variant
      it_events          = events[]
      is_print           = alv_print
    TABLES
      t_outtab           = gt_var_detail.
ENDFORM.                    " DISPLAY_LIST_DETAIL
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELDTAB1[]  text
*----------------------------------------------------------------------*
FORM initialize_fieldcat1  USING    p_fieldtab1 TYPE slis_t_fieldcat_alv.
  DATA:
        fieldcat TYPE slis_fieldcat_alv,
        lv_pos TYPE i.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MBLNR'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_s  = 'Mat. Doc.'.
  fieldcat-seltext_m  = 'Mat. Document'.
  fieldcat-seltext_l  = 'Material Document'.
  fieldcat-outputlen  = 10.
  APPEND fieldcat TO p_fieldtab1.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'CPUDT'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_s  = 'Mat. Date'.
  fieldcat-seltext_m  = 'Mat. Doc. Dat'.
  fieldcat-seltext_l  = 'Material Document Date'.
  fieldcat-outputlen  = 10.
  APPEND fieldcat TO p_fieldtab1.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MATNR'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'Item Code'.
  fieldcat-seltext_l  = 'Material Number'.
  fieldcat-outputlen  = 18.
  APPEND fieldcat TO p_fieldtab1.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MAKTX'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'Mat. text'.
  fieldcat-seltext_l  = 'Material Description'.
  fieldcat-outputlen  = 35.
  APPEND fieldcat TO p_fieldtab1.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'BWART'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_m  = 'Mov. Type'.
  fieldcat-seltext_l  = 'Movement Type'.
  fieldcat-outputlen  = 5.
  APPEND fieldcat TO p_fieldtab1.
  CLEAR fieldcat.


  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MAT_IM'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_s  = 'Posted in IM'.
  fieldcat-seltext_m  = 'Mat.Posted in IM'.
  fieldcat-seltext_l  = 'Material Document Posted in IM'.
  fieldcat-outputlen  = 5.
  APPEND fieldcat TO p_fieldtab1.
  CLEAR fieldcat.

  lv_pos = lv_pos + 1.
  fieldcat-fieldname  = 'MAT_RG1'.
  fieldcat-col_pos    = lv_pos.
  fieldcat-seltext_s  = 'Posted in RG1 Register'.
  fieldcat-seltext_m  = 'Mat.Posted in RG1 Register'.
  fieldcat-seltext_l  = 'Material Document Posted in RG1 Register'.
  fieldcat-outputlen  = 5.
  APPEND fieldcat TO p_fieldtab1.
  CLEAR fieldcat.
ENDFORM.                    " INITIALIZE_FIELDCAT1
