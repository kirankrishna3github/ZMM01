*&---------------------------------------------------------------------*
*& REPORT  copy of Z6MM004R_MATERIAL_SHORTAGE
*& GRR Register for Logistics
*&---------------------------------------------------------------------*
*&Developed by: Mrs. Punam S
*&Date: 03.10.2011
*& Requirement By: Mr. Kamlakar Verma.
*&---------------------------------------------------------------------*

REPORT  z6mm005r_new_mat_shortage MESSAGE-ID ZNK1.
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
*        REVISION NO:ASHISH PATNAIK
*          DEVELOPER:                        DATE:   16.02.2014
*        DESCRIPTION:
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:
*        DEVELOPER:   Naren Karra             DATE:   05.10.2015
*        DESCRIPTION: New Authorization object added for Plant
*----------------------------------------------------------------------*
TABLES : mkpf,MARA,
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
*       gr_wt  TYPE z6mma_gt_ent_hd-gr_wt,
*       tr_wt  TYPE z6mma_gt_ent_hd-tr_wt,
*       net_wt TYPE z6mma_gt_ent_hd-net_wt,
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
*       gr_wt  TYPE z6mma_gt_ent_hd-gr_wt,
*       tr_wt  TYPE z6mma_gt_ent_hd-tr_wt,
*       net_wt TYPE z6mma_gt_ent_hd-net_wt,
       name1  TYPE lfa1-name1,
       bedat TYPE ekko-bedat,
*       quan_diff TYPE mseg-menge,
       maktx TYPE makt-maktx,
       LMENGE01 TYPE QALS-LMENGE01, " ACCEPTED QTY
       LMENGE04 TYPE QALS-LMENGE04, " REJECTED QTY
       BSART TYPE ekko-BSART, " purchasing doc type
       RESWK TYPE ekko-RESWK, " supplying plant
       END OF wa_final.

DATA: BEGIN OF wa_lfa1,
      lifnr TYPE lfa1-lifnr,
      name1 TYPE lfa1-name1,
      END OF wa_lfa1.

DATA : BEGIN OF WA_MARA,
       MATNR TYPE MARA-MATNR,
       END OF WA_MARA.

DATA :i_mkpf    LIKE STANDARD TABLE OF wa_mkpf,
      i_mseg    LIKE STANDARD TABLE OF wa_mseg,
      i_mseg1   LIKE STANDARD TABLE OF wa_mseg,        " added by Naren Karra on 06.10.2015
      i_ge      LIKE STANDARD TABLE OF wa_ge,
      it_lfa1   LIKE STANDARD TABLE OF wa_lfa1,
      i_final   LIKE STANDARD TABLE OF wa_final,
      IT_MARA          LIKE STANDARD TABLE OF WA_MARA.

DATA : R_MATNR TYPE RANGE OF MARA-MATNR,
       WA_MATNR LIKE LINE OF R_MATNR.

DATA :  prg                  LIKE  sy-repid               ,
        layout               TYPE  slis_layout_alv        ,
        gt_list_top_of_page  TYPE  slis_t_listheader,
        gt_events            TYPE  slis_t_event           ,
        fieldcat             TYPE  slis_t_fieldcat_alv  WITH HEADER LINE  .

DATA: REPID TYPE SY-REPID.
DATA: G_SAVE(1) TYPE C,
      G_EXIT(1) TYPE C,
*      REPNAME  LIKE SY-REPID,
      G_VARIANT LIKE DISVARIANT,
      GX_VARIANT LIKE DISVARIANT,
      P_VAIRAINT LIKE DISVARIANT.

TYPES: BEGIN OF T_VARINFO,
       FLAG TYPE C,
       OLENGTH TYPE X,
       LINE LIKE RALDB-INFOLINE,
END OF T_VARINFO.
DATA: INFOTAB TYPE T_VARINFO OCCURS 0 WITH HEADER LINE,
VARIANT_INFO TYPE RSVARADMIN OCCURS 0 WITH HEADER LINE .

SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_mblnr FOR mkpf-mblnr,
                 s_mjahr FOR mkpf-mjahr,
                 s_werks FOR mseg-werks OBLIGATORY,
                 s_lifnr FOR mseg-lifnr,
                 s_matnr FOR mseg-matnr,
                 s_mtart FOR MARA-MTART OBLIGATORY,
                 s_budat FOR mkpf-budat OBLIGATORY,
                 s_blart FOR mkpf-blart.
SELECTION-SCREEN END OF BLOCK a01.

SELECTION-SCREEN BEGIN OF BLOCK A02 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT. " ALV Variant
SELECTION-SCREEN END OF BLOCK A02.

AT SELECTION-SCREEN.
PERFORM PAI_OF_SELECTION_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
PERFORM F4_FOR_VARIANT.
*&---------------------------------------------------------------------*
*&     INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION .
 prg = sy-repid .
 REPID = SY-REPID.
 PERFORM INITIALIZE_VARIANT.
*&---------------------------------------------------------------------*
*&                      START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM get_data.
  if i_final is NOT INITIAL.

  PERFORM build_fieldcat.
  PERFORM build_layout.
  PERFORM build_comment USING gt_list_top_of_page[].
  PERFORM display_alv.
  ELSE.
*     MESSAGE 'No Records Found !' TYPE 'I'.
     MESSAGE 'No Records Found / User not authorized for this Plant !' TYPE 'I'.                 " added by NK on 06.10.2015
  ENDIF.

*  START-OF-SELECTION.

  CALL FUNCTION 'PRINT_SELECTIONS'
    EXPORTING
      MODE      = 'X'
      RNAME     = SY-REPID "program name
      RVARIANTE = VARIANT_INFO-VARIANT "varient name
    TABLES
      INFOTAB   = INFOTAB.

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
    SELECT MATNR
         INTO TABLE IT_MARA FROM MARA WHERe mtart in s_mtart and matnr in s_matnr.
*         WHERE LABOR = '012'.
*
  IF SY-SUBRC = 0.
    LOOP AT IT_MARA INTO WA_MARA.
      WA_MATNR-SIGN   = 'I'.
      WA_MATNR-OPTION = 'EQ'.
      WA_MATNR-LOW    = WA_MARA-MATNR.
      WA_MATNR-HIGH   = ''.
      APPEND WA_MATNR TO R_MATNR.
    ENDLOOP.
  ENDIF.
  CLEAR WA_MATNR.
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
*    if not r_matnr is initial.
    SELECT mblnr mjahr matnr werks charg lifnr menge meins ebeln
    FROM mseg INTO TABLE i_mseg
           FOR ALL ENTRIES IN i_mkpf
           WHERE mblnr EQ i_mkpf-mblnr
           AND   mjahr EQ i_mkpf-mjahr
           AND   werks IN s_werks
           AND   lifnr IN s_lifnr
           AND   matnr IN r_matnr
           AND BWART IN ('101' , '102' ,'122' , '161' ,'162') .
    IF sy-subrc = 0.
      SORT i_mseg BY mblnr mjahr.
    ENDIF.
*    endif.
************************Start********************************     " added by NK on 05.10.2015

LOOP AT I_MSEG INTO WA_MSEG.
    AUTHORITY-CHECK OBJECT 'M_MSEG_WWA'
                        ID 'WERKS' FIELD WA_MSEG-WERKS
                        ID 'ACTVT' FIELD '03'.

    IF SY-SUBRC EQ 0.
    APPEND WA_MSEG TO I_MSEG1.
    CLEAR WA_MSEG.
    ELSE.
     MESSAGE 'User does not have authorisation for few Plants' type 'S'.
    ENDIF.

ENDLOOP.
*************************End*********************************
  ENDIF.

  IF NOT i_mseg1 IS INITIAL.
    SELECT mblnr
           mjahr
           gaten
           gtdat
*           gr_wt
*           tr_wt
*           net_wt
      FROM z6mma_gt_ent_hd INTO TABLE i_ge
           FOR ALL ENTRIES IN i_mseg1
           WHERE mblnr EQ i_mseg1-mblnr
           AND   mjahr EQ i_mseg1-mjahr.
    IF sy-subrc = 0.
      SORT i_ge BY mblnr mjahr.
    ENDIF.

    SELECT lifnr
           name1 FROM lfa1 INTO TABLE it_lfa1
           FOR ALL ENTRIES IN i_mseg1
           WHERE lifnr EQ i_mseg1-lifnr.
    IF sy-subrc = 0.
      SORT it_lfa1 BY lifnr name1.
    ENDIF.

    LOOP AT i_mseg1 INTO wa_mseg.
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
*        wa_final-gr_wt  =  wa_ge-gr_wt.
*        wa_final-tr_wt  =  wa_ge-tr_wt.
*        wa_final-net_wt =  wa_ge-net_wt.
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
      SELECT SINGLE bedat bsart RESWK FROM ekko INTO (wa_final-bedat, wa_final-bsart , wa_final-RESWK) WHERE ebeln = wa_final-ebeln.

      if wa_final-bsart = 'ZSTO'. " IF Document type is zsto then in plase of vendor supplying plant will come
        wa_final-lifnr = wa_final-RESWK.
        SELECT SINGLE NAME1 FROM t001w INTO  wa_final-name1 WHERE werks =  wa_final-RESWK.
      ENDIF.
*      CLEAR wa_final-quan_diff.
*      wa_final-quan_diff = wa_final-menge - wa_final-net_wt.

*        ---MAT DESP
      CLEAR wa_final-maktx .
      SELECT SINGLE maktx FROM makt INTO wa_final-maktx WHERE matnr = wa_final-matnr.

      MODIFY i_final FROM wa_final TRANSPORTING bldat gaten gtdat bsart"gr_wt tr_wt net_wt quan_diff
      name1 bedat  maktx lifnr name1.

      SELECT SINGLE LMENGE01 LMENGE04
        FROM QALS INTO (WA_FINAL-LMENGE01 , WA_FINAL-LMENGE04 )
        WHERE MATNR = wa_final-MATNR
        AND CHARG = wa_final-CHARG
        AND MBLNR = wa_final-MBLNR
        AND MJAHR = wa_final-mjahr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT        = wa_final-MATNR
      IMPORTING
       OUTPUT        = wa_final-MATNR        .



      MODIFY i_final FROM wa_final TRANSPORTING matnr LMENGE01 LMENGE04 .

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

  fieldcat-fieldname   = 'LMENGE01'.
  fieldcat-seltext_m   = 'Accept Qty'.
  fieldcat-col_pos     = 13.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'LMENGE04'.
  fieldcat-seltext_m   = 'Rejected Qty'.
  fieldcat-col_pos     = 14.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

  fieldcat-fieldname   = 'BSART'.
  fieldcat-seltext_m   = 'Doc.Type'.
  fieldcat-col_pos     = 15.
  APPEND fieldcat TO fieldcat.
  CLEAR  fieldcat.

*  fieldcat-fieldname   = 'GR_WT'.
*  fieldcat-seltext_m   = 'Gross Weight'.
*  fieldcat-col_pos     = 13.
*  APPEND fieldcat TO fieldcat.
*  CLEAR  fieldcat.
*
*  fieldcat-fieldname   = 'TR_WT'.
*  fieldcat-seltext_m   = 'Tare Weight'.
*  fieldcat-col_pos     = 14.
*  APPEND fieldcat TO fieldcat.
*  CLEAR  fieldcat.
*
*  fieldcat-fieldname   = 'NET_WT'.
*  fieldcat-seltext_m   = 'Net Weight'.
*  fieldcat-col_pos     = 15.
*  APPEND fieldcat TO fieldcat.
*  CLEAR  fieldcat.
*
*  fieldcat-fieldname   = 'QUAN_DIFF'.
*  fieldcat-seltext_m   = 'Quan Diff'.
*  fieldcat-col_pos     = 16.
*  APPEND fieldcat TO fieldcat.
*  CLEAR  fieldcat.

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
*     i_save                            = 'A'
                    I_SAVE             = G_SAVE
                    IS_VARIANT         = G_VARIANT

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

  line-info = 'REPORT : GRR Register for Logistics' .
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
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE_VARIANT.
    G_SAVE = 'A'.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = REPID.
  G_VARIANT-VARIANT = P_VARI.
  GX_VARIANT = G_VARIANT.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      I_SAVE     = G_SAVE
    CHANGING
      CS_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 0.
    P_VARI = GX_VARIANT-VARIANT.
    G_VARIANT = GX_VARIANT.
  ENDIF.
ENDFORM.                    " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PAI_OF_SELECTION_SCREEN .
PERFORM INITIALIZE_VARIANT.
ENDFORM.                    " PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_FOR_VARIANT.
    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      IS_VARIANT = G_VARIANT
      I_SAVE     = G_SAVE
    IMPORTING
      E_EXIT     = G_EXIT
      ES_VARIANT = GX_VARIANT
    EXCEPTIONS
      NOT_FOUND  = 2.
  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF G_EXIT = SPACE.
      P_VARI = GX_VARIANT-VARIANT.
    ENDIF.
  ENDIF.
ENDFORM.                    " F4_FOR_VARIANT
