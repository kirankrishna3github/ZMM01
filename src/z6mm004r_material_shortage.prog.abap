*&---------------------------------------------------------------------*
*& REPORT  Z6MM004R_MATERIAL_SHORTAGE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6mm004r_material_shortage.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION:
* OBJECT TYPE       :                 FUNC. CONSULTANT  :GIRISH
*          DEVELOPER:SUPRIYA
*      CREATION DATE:   13.10.2010
*        DEV REQUEST:
*  TCODE            :
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 23/10/2015
*   REASON FOR CHANGE: Add Authorization and layout
*   REQUEST #: IRDK913079 (Already Created in Past)
* --------------------------------------------------------------------------------------------*

TABLES : mkpf,
         mseg,
         z6mma_gt_ent_hd.

*&---------------------------------------------------------------------*
*&                      TYPE POOLS
*&---------------------------------------------------------------------*

TYPE-POOLS : slis.

DATA : BEGIN OF wa_mkpf ,
       mblnr TYPE mkpf-mblnr,
       mjahr TYPE mkpf-mjahr,
       bldat TYPE mkpf-bldat,
       END OF wa_mkpf.

DATA : BEGIN OF wa_mseg,
       mblnr TYPE mseg-mblnr,
       mjahr TYPE mseg-mjahr,
       matnr TYPE mseg-matnr,
       werks TYPE mseg-werks,
       charg TYPE mseg-charg,
       lifnr TYPE mseg-lifnr,
       menge TYPE mseg-menge,
       meins TYPE mseg-meins,
       ebeln TYPE mseg-ebeln,
       END OF wa_mseg.

DATA : BEGIN OF wa_ge,
       mblnr  TYPE z6mma_gt_ent_hd-mblnr,
       mjahr  TYPE z6mma_gt_ent_hd-mjahr,
       gaten  TYPE z6mma_gt_ent_hd-gaten,
       gtdat  TYPE z6mma_gt_ent_hd-gtdat,
       gr_wt  TYPE z6mma_gt_ent_hd-gr_wt,
       tr_wt  TYPE z6mma_gt_ent_hd-tr_wt,
       net_wt TYPE z6mma_gt_ent_hd-net_wt,
       END OF wa_ge.

DATA : BEGIN OF wa_final,
       mblnr  TYPE mseg-mblnr,
       mjahr  TYPE mseg-mjahr,
       matnr  TYPE mseg-matnr,
       bldat  TYPE mkpf-bldat,
       werks  TYPE mseg-werks,
       charg  TYPE mseg-charg,
       lifnr  TYPE mseg-lifnr,
       menge  TYPE mseg-menge,
       meins  TYPE mseg-meins,
       ebeln  TYPE mseg-ebeln,

       gaten  TYPE z6mma_gt_ent_hd-gaten,
       gtdat  TYPE z6mma_gt_ent_hd-gtdat,
       gr_wt  TYPE z6mma_gt_ent_hd-gr_wt,
       tr_wt  TYPE z6mma_gt_ent_hd-tr_wt,
       net_wt TYPE z6mma_gt_ent_hd-net_wt,
       name1  TYPE lfa1-name1,
       bedat TYPE ekko-bedat,
       quan_diff TYPE mseg-menge,
       maktx TYPE makt-maktx,
       END OF wa_final.

DATA: BEGIN OF wa_lfa1,
      lifnr TYPE lfa1-lifnr,
      name1 TYPE lfa1-name1,
      END OF wa_lfa1.

DATA : BEGIN OF wa_mara,
       matnr TYPE mara-matnr,
       END OF wa_mara.

DATA :i_mkpf    LIKE STANDARD TABLE OF wa_mkpf,
      i_mseg    LIKE STANDARD TABLE OF wa_mseg,
      i_ge      LIKE STANDARD TABLE OF wa_ge,
      it_lfa1   LIKE STANDARD TABLE OF wa_lfa1,
      i_final   LIKE STANDARD TABLE OF wa_final,
      it_mara          LIKE STANDARD TABLE OF wa_mara.

DATA : r_matnr TYPE RANGE OF mara-matnr,
       wa_matnr LIKE LINE OF r_matnr.

DATA :  prg                  LIKE  sy-repid               ,
        layout               TYPE  slis_layout_alv        ,
        gt_list_top_of_page  TYPE  slis_t_listheader,
        gt_events            TYPE  slis_t_event           ,
        fieldcat             TYPE  slis_t_fieldcat_alv  WITH HEADER LINE  .

***** Start Code: Added by CS on 23.10.2015 for layout. *****
DATA: s_repid   LIKE sy-repid,
      g_save(1) TYPE c,
      g_exit(1) TYPE c,                         " ALV VARIANT
      gx_variant TYPE disvariant.
***** End Code: Added by CS on 23.10.2015 for layout. *****
***** Start Code: Added by CS on 23.10.2015 for Authorization. *****
DATA: lv_werks_auth_flg TYPE c VALUE ''  " Auth. Flag for Plant
.
***** End Code: Added by CS on 23.10.2015 for Authorization. *****

SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_mblnr FOR mkpf-mblnr,
                 s_mjahr FOR mkpf-mjahr,
                 s_werks FOR mseg-werks,
                 s_lifnr FOR mseg-lifnr,
*                 s_matnr FOR mseg-matnr,
                 s_budat FOR mkpf-budat,
                 s_blart FOR mkpf-blart.
PARAMETERS : p_var TYPE disvariant-variant MODIF ID pk. " Added by CS on 23.10.2015 for Layout
SELECTION-SCREEN END OF BLOCK a01.

***** Start Code: Added by CS on 23.10.2015 for layout. *****
*PERFORM get_default_variant.                     " GETTING DEFAULT VARIANT SAVED

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM variant_f4_selection.                    " F4 HELP FOR VARIANT ON SELECTION SCREEN

AT SELECTION-SCREEN.
  s_repid = sy-repid.
  PERFORM check_variant_existance.

INITIALIZATION.
  s_repid = sy-repid.
  PERFORM variant_init.
***** End Code: Added by CS on 23.10.2015 for layout. *****

*&---------------------------------------------------------------------*
*&     INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION .
  prg = sy-repid .

*&---------------------------------------------------------------------*
*&                      START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM check_auth_obj.  " Added by CS on 23.10.2015 for Authorization.

  PERFORM get_data.
  PERFORM build_fieldcat.
  PERFORM build_layout.
  PERFORM build_comment USING gt_list_top_of_page[].
***** Start Code: Added by CS on 23.10.2015 for Authorization. *****
*    PERFORM display_alv. " Commented by CS
  IF i_final[] IS NOT INITIAL.
    IF lv_werks_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Plant.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
    PERFORM display_alv.
  ELSE.
    IF lv_werks_auth_flg = 'X'.
      MESSAGE 'No data found/ Missing Authorization for Plant.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.
      MESSAGE 'No data found.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.
***** End Code: Added by CS on 23.10.2015 for Authorization. *****



*&---------------------------------------------------------------------*
*&      FORM  GET_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM get_data .

*  ---logiv for wb material
  SELECT matnr
       INTO TABLE it_mara FROM mara
       WHERE labor = '012'.

  IF sy-subrc = 0.
    LOOP AT it_mara INTO wa_mara.
      wa_matnr-sign   = 'I'.
      wa_matnr-option = 'EQ'.
      wa_matnr-low    = wa_mara-matnr.
      wa_matnr-high   = ''.
      APPEND wa_matnr TO r_matnr.
    ENDLOOP.
  ENDIF.
  CLEAR wa_matnr.


*  -----

  SELECT  mblnr
          mjahr
          bldat FROM mkpf INTO TABLE i_mkpf
          WHERE     mblnr IN s_mblnr
                AND mjahr IN s_mjahr
                AND blart IN s_blart
                AND budat IN s_budat.
  IF sy-subrc = 0.
    SORT i_mkpf BY mblnr mjahr.
  ENDIF.

  IF NOT i_mkpf IS INITIAL.
    IF NOT r_matnr IS INITIAL.
      SELECT mblnr
             mjahr
             matnr
             werks
             charg
             lifnr
             menge
             meins
             ebeln FROM mseg INTO TABLE i_mseg
             FOR ALL ENTRIES IN i_mkpf
             WHERE mblnr EQ i_mkpf-mblnr
             AND   mjahr EQ i_mkpf-mjahr
             AND   werks IN s_werks
             AND   lifnr IN s_lifnr
             AND   matnr IN r_matnr.
      IF sy-subrc = 0.
        SORT i_mseg BY mblnr mjahr.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT i_mseg IS INITIAL.
    SELECT mblnr
           mjahr
           gaten
           gtdat
           gr_wt
           tr_wt
           net_wt FROM z6mma_gt_ent_hd INTO TABLE i_ge
           FOR ALL ENTRIES IN i_mseg
           WHERE mblnr EQ i_mseg-mblnr
           AND   mjahr EQ i_mseg-mjahr.
    IF sy-subrc = 0.
      SORT i_ge BY mblnr mjahr.
    ENDIF.

    SELECT lifnr
           name1 FROM lfa1 INTO TABLE it_lfa1
           FOR ALL ENTRIES IN i_mseg
           WHERE lifnr EQ i_mseg-lifnr.
    IF sy-subrc = 0.
      SORT it_lfa1 BY lifnr name1.
    ENDIF.

    LOOP AT i_mseg INTO wa_mseg.
      MOVE-CORRESPONDING wa_mseg TO wa_final.
      APPEND wa_final TO i_final.
      CLEAR wa_final.
    ENDLOOP.

    LOOP AT i_final INTO wa_final.
      CLEAR wa_ge.
      READ TABLE i_ge INTO wa_ge WITH KEY mblnr = wa_final-mblnr
                                          mjahr = wa_final-mjahr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-gaten  =  wa_ge-gaten.
        wa_final-gtdat  =  wa_ge-gtdat.
        wa_final-gr_wt  =  wa_ge-gr_wt.
        wa_final-tr_wt  =  wa_ge-tr_wt.
        wa_final-net_wt =  wa_ge-net_wt.
      ENDIF.

      CLEAR :wa_final-name1,wa_lfa1.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_final-lifnr  BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-name1 =  wa_lfa1-name1 .
      ENDIF.

      CLEAR :wa_mkpf,wa_final-bldat.
      READ TABLE i_mkpf INTO wa_mkpf WITH KEY mblnr = wa_final-mblnr
                                          mjahr = wa_final-mjahr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-bldat = wa_mkpf-bldat.
      ENDIF.

      CLEAR wa_final-bedat .
      SELECT SINGLE bedat FROM ekko INTO wa_final-bedat WHERE ebeln = wa_final-ebeln.

      CLEAR wa_final-quan_diff.
      wa_final-quan_diff = wa_final-menge - wa_final-net_wt.

*        ---MAT DESP
      CLEAR wa_final-maktx .
      SELECT SINGLE maktx FROM makt INTO wa_final-maktx WHERE matnr = wa_final-matnr.

      MODIFY i_final FROM wa_final TRANSPORTING bldat gaten gtdat gr_wt tr_wt net_wt name1 bedat quan_diff maktx.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat .

  fieldcat-fieldname   = 'MBLNR'.
  fieldcat-seltext_m   = 'Docu No'.
  fieldcat-col_pos     = 1.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'MJAHR'.
  fieldcat-seltext_m   = 'Docu Year'.
  fieldcat-col_pos     = 2.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'BLDAT'.
  fieldcat-seltext_m   = 'Docu Date'.
  fieldcat-col_pos     = 2.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'LIFNR'.
  fieldcat-seltext_m   = 'Vendor'.
  fieldcat-col_pos     = 3.
  fieldcat-no_zero    = 'X'.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'NAME1'.
  fieldcat-seltext_m   = 'Vendor Name'.
  fieldcat-col_pos     = 4.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'WERKS'.
  fieldcat-seltext_m   = 'Plant'.
  fieldcat-col_pos     = 5.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'GATEN'.
  fieldcat-seltext_m   = 'Gate Entry No'.
  fieldcat-col_pos     = 6.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'GTDAT'.
  fieldcat-seltext_m   = 'Gate Entry Date'.
  fieldcat-col_pos     = 7.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'EBELN'.
  fieldcat-seltext_m   = 'PO No.'.
  fieldcat-col_pos     = 8.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'BEDAT'.
  fieldcat-seltext_m   = 'PO Dt.'.
  fieldcat-col_pos     = 8.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'MATNR'.
  fieldcat-seltext_m   = 'MATERIAL'.
  fieldcat-col_pos     = 9.
  fieldcat-no_zero    = 'X'.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'MAKTX'.
  fieldcat-seltext_m   = 'MATERIAL DESC'.
  fieldcat-col_pos     = 10.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'MENGE'.
  fieldcat-seltext_m   = 'GR QTY'.
  fieldcat-col_pos     = 11.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'MEINS'.
  fieldcat-seltext_m   = 'GR UOM'.
  fieldcat-col_pos     = 12.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'GR_WT'.
  fieldcat-seltext_m   = 'Gross Weight'.
  fieldcat-col_pos     = 13.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'TR_WT'.
  fieldcat-seltext_m   = 'Tare Weight'.
  fieldcat-col_pos     = 14.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'NET_WT'.
  fieldcat-seltext_m   = 'Net Weight'.
  fieldcat-col_pos     = 15.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'QUAN_DIFF'.
  fieldcat-seltext_m   = 'Quan Diff'.
  fieldcat-col_pos     = 16.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_layout .
  layout-no_input          = 'X'.
  layout-colwidth_optimize = 'X'.
  layout-zebra             = 'X'.
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING

     i_callback_program                = prg
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
     i_callback_top_of_page            = 'TOP_OF_PAGE'
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
     is_layout                         = layout
     it_fieldcat                       = fieldcat[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
*     IT_SORT                           =
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
     i_save                            = 'A' " 'X' Added by CS on 23.10.2015 for Layout
*     IS_VARIANT                        =
*     IT_EVENTS                         =

    TABLES
      t_outtab                          = i_final
   EXCEPTIONS
     program_error                     = 1
     OTHERS                            = 2
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page .

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM build_comment  USING p_heading TYPE slis_t_listheader.

  DATA: line        TYPE  slis_listheader ,
       rs_variant  TYPE  disvariant      ,
       c_date(10)                        .

  CLEAR : gt_list_top_of_page[], gt_list_top_of_page .

  line-info = 'INDOFIL CHEMICALS COMPANY' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  line-info = 'REPORT : Material shortage' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  IMPORT rs_variant FROM MEMORY ID 'VARIANT'.
  IF NOT rs_variant IS INITIAL .
    CONCATENATE 'Layout : ' rs_variant-text INTO line-info
                                             SEPARATED BY space .
    line-typ  = 'S' .
    APPEND line TO gt_list_top_of_page .
    CLEAR line .
  ENDIF .

  line-typ = 'S' .
  WRITE sy-datum TO c_date .
  CONCATENATE 'Date : ' c_date INTO line-info SEPARATED BY space .
  APPEND line TO gt_list_top_of_page .
  CLEAR line .

ENDFORM.                    " BUILD_COMMENT
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
    p_var = gx_variant-variant.
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
      p_var = gx_variant-variant.
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
*    p_var = gx_variant-variant.
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
  IF NOT p_var IS INITIAL.
    MOVE p_var TO gx_variant-variant.
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
        END OF ty_t001w
          .
  DATA:
        t_t001w TYPE TABLE OF ty_t001w, " Plants
        w_t001w TYPE ty_t001w.

  FREE : t_t001w[].
  CLEAR: w_t001w.

  break test1.

***** Start Code: Added by CS on 23.10.2015 for Plant Authorization. *****
  SELECT werks  " Fetch values of Plant
    FROM t001w
    INTO TABLE t_t001w
    WHERE werks IN s_werks.

  CLEAR: s_werks, lv_werks_auth_flg.
  REFRESH: s_werks[].
  IF t_t001w[] IS NOT INITIAL.
    LOOP AT t_t001w INTO w_t001w.
      AUTHORITY-CHECK OBJECT 'M_MSEG_WWA' " Plant
                     ID 'ACTVT' FIELD '03'
                     ID 'WERKS' FIELD w_t001w-werks.
      IF sy-subrc EQ 0.
        s_werks-sign = 'I'.
        s_werks-option = 'EQ'.
        s_werks-low = w_t001w-werks.
        APPEND s_werks.
        CLEAR: s_werks.
      ELSE.
        IF lv_werks_auth_flg IS INITIAL.  " Authorization Flag
          lv_werks_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t001w.
    ENDLOOP.
  ENDIF.
  IF s_werks[] IS INITIAL.
    s_werks-sign = 'I'.
    s_werks-option = 'EQ'.
    s_werks-low = ''.
    APPEND s_werks.
    CLEAR: s_werks.
  ENDIF.
***** End Code: Added by CS on 23.10.2015 for Plant Authorization. *****
ENDFORM.                    " CHECK_AUTH_OBJ
