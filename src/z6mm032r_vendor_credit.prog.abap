*&---------------------------------------------------------------------*
*& Report  Z6MM031R_MOV_AVG_PRICE
*&
*&---------------------------------------------------------------------*
*&Developer : Punam
*&Developement Date : 19.08.2011
*&Requirement from: Mr.Subhash Palkar
*&---------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 05.11.2015
*   REASON FOR CHANGE: Add Authorization & Layout
*   REQUEST #: IRDK913085
* --------------------------------------------------------------------------------------------*


REPORT  z6mm031r_mov_avg_price.
TABLES: ekko , ekpo , mbew , mara.

TYPE-POOLS:slis.
*ALV DATA DECLARATIONS
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      "GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      gd_layout    TYPE slis_layout_alv,
"      GD_REPID     LIKE SY-REPID,
      g_variant LIKE disvariant,
      gt_events     TYPE slis_t_event,
      gd_prntparams TYPE slis_print_alv.


DATA: it_sort TYPE  slis_t_sortinfo_alv." WITH HEADER LINE.
DATA: wa_sort LIKE LINE OF it_sort.
*COMPANY CODE
*PLANT
*MATERIAL TYPE
*MATERIAL
*DESCRIPTION
*PO NO
*PO ITEM NO
*PO DATE
*NET LANDED COST
*MOVING AVF PRICE
*DIFFERENCE
CONSTANTS: formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
DATA:p_heading  TYPE slis_t_listheader.
DATA: BEGIN OF it_out OCCURS 0,
        bukrs TYPE ekpo-bukrs,
        werks TYPE ekpo-werks,
        mtart TYPE ekpo-mtart,
        matnr TYPE ekpo-matnr,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
        bedat TYPE ekko-bedat,
        netpr TYPE ekpo-netpr, "NLC
        peinh TYPE ekpo-peinh,
        verpr TYPE mbew-verpr, "MAP
        lbkum TYPE mbew-lbkum,
        waers TYPE ekko-waers, " CURRUNCY CODE FOR NLC
        lifnr TYPE ekko-lifnr,
        ekorg TYPE ekko-ekorg,
        zterm TYPE lfm1-zterm,

        name1 TYPE lfa1-name1,

        diff TYPE mbew-verpr,
        maktg TYPE makt-maktg,
        cell TYPE lvc_t_scol,"SLIS_T_SPECIALCOL_ALV,

        diff_perc TYPE i,
        l_netpr TYPE ekpo-netpr,"LPC
        diff2 TYPE mbew-verpr,
        diff_perc2 TYPE i,
        last_ebeln TYPE ekpo-ebeln, " LAST PO
        last_ebelp TYPE ekpo-ebelp,
        lpc_waers TYPE ekko-waers, " CURRUNCY CODE FOR LPC
        map_waers TYPE t001-waers, " CURRUNCY CODE FOR MAP
        nlc_lpc TYPE mbew-verpr, " LPC- NLC
        diff_perc3 TYPE i,

        m_zterm TYPE lfm1-zterm,
        text1 TYPE t052u-text1, " PO
        text2 TYPE t052u-text1, " MASTER
        ztag1 TYPE t052-ztag1,
        m_ztag1 TYPE t052-ztag1,
        difference TYPE i,
      END OF it_out.
*      IT_OUT LIKE TABLE OF WA_OUT.

*DATA: FNAME1(10) TYPE C , COLR TYPE C VALUE '6'.

***** Start Code: Added by CS on 05.11.2015 for layout. *****
DATA: s_repid   LIKE sy-repid,
      g_save(1) TYPE c,
      g_exit(1) TYPE c,                         " ALV VARIANT
      gx_variant TYPE disvariant.
***** End Code: Added by CS on 05.11.2015 for layout. *****
***** Start Code: Added by CS on 05.11.2015 for Authorization. *****
DATA: lv_bukrs_auth_flg TYPE c VALUE '',  " Auth. Flag for Company Code
      lv_werks_auth_flg TYPE c VALUE ''  " Auth. Flag for Plant
.
***** End Code: Added by CS on 05.11.2015 for Authorization. *****

DATA: fname1(10) TYPE c,
          int TYPE c ,
          colr TYPE c.
*********************
DATA:  p_vairaint LIKE disvariant.

DATA:f_cell LIKE LINE OF  it_out-cell.
DATA:f_cell2 LIKE LINE OF  it_out-cell.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs FOR ekpo-bukrs DEFAULT '1000' OBLIGATORY,
                s_werks FOR ekpo-werks,
                s_bedat FOR ekko-bedat,
                s_mtart FOR mara-mtart,
                s_matnr FOR mara-matnr,
                s_ebeln FOR ekko-ebeln,
                s_lifnr FOR ekko-lifnr.
PARAMETERS : p_var TYPE disvariant-variant MODIF ID pk. " Added by CS on 05.11.2015 for Layout
SELECTION-SCREEN END OF BLOCK blk1.
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-002.

PARAMETERS : p_all RADIOBUTTON GROUP gp1,
            p_match RADIOBUTTON GROUP gp1,
            p_miss RADIOBUTTON GROUP gp1.

SELECTION-SCREEN END OF BLOCK blk2.

***** Start Code: Added by CS on 05.11.2015 for layout. *****
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM variant_f4_selection.                    " F4 HELP FOR VARIANT ON SELECTION SCREEN

AT SELECTION-SCREEN.
  s_repid = sy-repid.
  PERFORM check_variant_existance.

INITIALIZATION.
  s_repid = sy-repid.
  PERFORM variant_init.
***** End Code: Added by CS on 05.11.2015 for layout. *****

START-OF-SELECTION.
  PERFORM check_auth_obj.  " Added by CS on 05.11.2015 for Authorization.

  PERFORM get_data.
  PERFORM build_comment USING p_heading[].
  PERFORM build_eventtab USING gt_events[].
  IF it_out[] IS NOT INITIAL .
***** Start Code: Added by CS on 05.11.2015 for Authorization Message. *****
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Company Code/Plant.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
***** End Code: Added by CS on 05.11.2015 for Authorization Message. *****
    PERFORM display.
  ELSE.
***** Start Code: Added by CS on 05.11.2015 for Authorization Message. *****
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'No Records Found or Missing Authorization for Company Code/Plant.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.
      MESSAGE 'No Records Found !' TYPE 'I'.
    ENDIF.
***** End Code: Added by CS on 05.11.2015 for Authorization Message. *****
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

*SELECT A~BUKRS WERKS MTART MATNR A~EBELN EBELP BEDAT NETPR PEINH
*FROM EKPO AS A
*JOIN EKKO AS B
*ON A~EBELN = B~EBELN
*INTO TABLE IT_OUT
*WHERE A~LOEKZ = ''
*AND A~BSTYP = 'F'
*AND B~BEDAT IN S_BEDAT
*AND A~BUKRS IN S_BUKRS
*AND A~WERKS IN S_WERKS
*AND A~MATNR IN S_MATNR
*AND A~MTART IN S_MTART.

  SELECT a~bukrs a~werks a~mtart a~matnr a~ebeln a~ebelp b~bedat a~netpr a~peinh c~verpr
    c~lbkum b~waers b~lifnr b~ekorg b~zterm
  FROM ekpo AS a
  JOIN ekko AS b
  ON a~ebeln = b~ebeln
  JOIN mbew AS c
  ON a~matnr = c~matnr
  AND a~werks = c~bwkey
  AND a~bwtar = c~bwtar
  INTO CORRESPONDING FIELDS OF TABLE it_out
  WHERE a~loekz = ''
  AND a~bstyp = 'F'
  AND b~bedat IN s_bedat
  AND a~bukrs IN s_bukrs
  AND a~werks IN s_werks
  AND a~matnr IN s_matnr
  AND a~mtart IN s_mtart
  AND c~lvorm <> 'X'
  AND b~bsart IN ('DB','NB','YDOM','YIMP','ZDOM','ZIMP' )
  AND a~elikz <> 'X'
  AND a~ebeln IN s_ebeln
  AND c~vprsv = 'V' AND b~lifnr IN s_lifnr  .

  LOOP AT it_out." INTO WA_OUT.

    SELECT SINGLE maktg
    FROM makt
    INTO it_out-maktg
    WHERE matnr = it_out-matnr
    AND spras = 'EN'.

    SELECT SINGLE name1
    FROM lfa1
    INTO it_out-name1
    WHERE lifnr = it_out-lifnr
    AND spras = 'EN'.


    " TO RETRIVE LAST PURCHASE COST
    SELECT SINGLE netpr a~ebeln a~ebelp b~waers
      FROM ekpo AS a
      JOIN ekko AS b
      ON a~ebeln = b~ebeln
      INTO (it_out-l_netpr , it_out-last_ebeln , it_out-last_ebelp ,  it_out-lpc_waers)
      WHERE matnr = it_out-matnr
      AND werks = it_out-werks
      AND a~bukrs = it_out-bukrs
      AND bedat IN ( SELECT MAX( bedat )
      FROM ekko AS c
      JOIN ekpo AS d
      ON c~ebeln = d~ebeln
      WHERE matnr = it_out-matnr
      AND werks = it_out-werks
      AND a~bukrs = it_out-bukrs ).

*    AND BEDAT IN SELECT MAX( [DISTINCT] BEDAT ).

*  SELECT MAX( BEDAT )  FROM EKKO JOIN

*    SELECT SINGLE WAERS FROM T001 INTO IT_OUT-MAP_WAERS WHERE BUKRS =  IT_OUT-BUKRS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_out-matnr
      IMPORTING
        output = it_out-matnr.

    SELECT SINGLE zterm FROM lfm1 INTO it_out-m_zterm WHERE lifnr = it_out-lifnr.

    IF it_out-m_zterm IS INITIAL.

      SELECT SINGLE zterm FROM lfb1 INTO it_out-m_zterm WHERE lifnr = it_out-lifnr.

    ENDIF.

    SELECT SINGLE ztag1 FROM t052 INTO it_out-ztag1 WHERE zterm = it_out-zterm.
    SELECT SINGLE ztag1 FROM t052 INTO it_out-m_ztag1 WHERE zterm = it_out-m_zterm.

    SELECT SINGLE text1 FROM t052u INTO it_out-text1 WHERE spras = 'EN' AND zterm = it_out-zterm.
    SELECT SINGLE text1 FROM t052u INTO it_out-text2 WHERE spras = 'EN' AND zterm = it_out-m_zterm.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_out-lifnr
      IMPORTING
        output = it_out-lifnr.

    it_out-difference = it_out-ztag1 - it_out-m_ztag1.


    MODIFY it_out[] FROM it_out TRANSPORTING matnr lifnr maktg name1 last_ebeln
    last_ebelp lpc_waers zterm text1 m_zterm text2 ztag1  m_ztag1 difference.
  ENDLOOP.

  SORT it_out[] BY bukrs werks mtart matnr ebeln ebelp.

**********LOOP*******
  LOOP AT it_out .
    IF p_match = 'X'.
      IF it_out-zterm NE it_out-m_zterm.
        DELETE it_out[] INDEX sy-tabix.
      ENDIF.
    ELSEIF p_miss = 'X'  .
      IF it_out-zterm EQ it_out-m_zterm.
        DELETE it_out[] INDEX sy-tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_out .
*    IF  IT_OUT-ZTERM IS NOT INITIAL.

    IF it_out-difference < 0.
      fname1 = 'EBELN'.
      colr = 6.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.

    IF it_out-difference < 0.
      fname1 = 'LIFNR'.
      colr = 6.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.

    IF it_out-difference > 0.
      fname1 = 'EBELN'.
      colr = 5.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.
    IF it_out-difference > 0.
      fname1 = 'LIFNR'.
      colr = 5.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.

    IF it_out-cell IS NOT INITIAL.
      MODIFY it_out  TRANSPORTING cell.
    ENDIF.
*    ENDIF.

  ENDLOOP.

  gd_layout-info_fieldname = 'COLOR'.
  gd_layout-coltab_fieldname = 'CELL'.

ENDFORM.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  SET_CELL_COLOURS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_cell_colours .
  DATA: wa_cellcolor TYPE lvc_s_scol.

  wa_cellcolor-fname = fname1.
  wa_cellcolor-color-col = colr.  "color code 1-7, if outside rage defaults to 7
*  WA_CELLCOLOR-COLOR-INT = INT.  "1 = Intensified on, 0 = Intensified off
  wa_cellcolor-color-inv = '0'.  "1 = text colour, 0 = background colour
  APPEND wa_cellcolor TO it_out-cell.

ENDFORM.                    "SET_CELL_COLOURS
**&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display .
  PERFORM build_fieldcatalog.

  wa_sort-spos = '01' .
  wa_sort-fieldname = 'BUKRS'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = '02' .
  wa_sort-fieldname = 'WERKS'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.


  wa_sort-spos = '03' .
  wa_sort-fieldname = 'MTART'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = '04' .
  wa_sort-fieldname = 'MATNR'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.


  wa_sort-spos = '05' .
  wa_sort-fieldname = 'MAKTG'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = '06' .
  wa_sort-fieldname = 'BEDAT'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = '07' .
  wa_sort-fieldname = 'LIFNR'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.

  wa_sort-spos = '08' .
  wa_sort-fieldname = 'NAME1'.
  wa_sort-tabname = 'IT_OUT'.
  wa_sort-up = 'X' .
  wa_sort-subtot = 'X'.
  APPEND wa_sort TO it_sort.

*GD_LAYOUT-COLWIDTH_OPTIMIZE  = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_bypassing_buffer      = 'X'
            i_callback_program      = sy-repid
*            i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
            i_callback_user_command = 'USER_COMMAND'
*            i_grid_title           = outtext
            is_layout               = gd_layout
            it_fieldcat             = fieldcatalog[]
*            it_special_groups       = gd_tabgroup
            is_variant               = g_variant
            it_events               = gt_events
            is_print                = gd_prntparams
            i_save                  = 'A' "'X'
            it_sort                 = it_sort
*            is_variant              = z_template
       TABLES
            t_outtab                = it_out[].

* CLEAR IT_OUT.

ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  fieldcatalog-fieldname   = 'BUKRS'.
  fieldcatalog-seltext_m   = 'Comp'.
  fieldcatalog-col_pos     = 0.
*  FIELDCATALOG-KEY         = 'X'.
  fieldcatalog-outputlen   = 04.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'WERKS'.
  fieldcatalog-seltext_m   = 'Plant'.
  fieldcatalog-col_pos     = 1.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-seltext_m   = 'TEST'.
  fieldcatalog-outputlen   = 04.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'MTART'.
  fieldcatalog-seltext_m   = 'MatType'.
  fieldcatalog-col_pos     = 2.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-seltext_m   = 'TEST'.
  fieldcatalog-outputlen   = 05.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'MATNR'.
  fieldcatalog-seltext_m   = 'Material'.
  fieldcatalog-col_pos     = 3.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'MAKTG'.
  fieldcatalog-seltext_m   = 'Descr.'.
  fieldcatalog-col_pos     = 4.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'LIFNR'.
  fieldcatalog-seltext_m   = 'Vendor'.
  fieldcatalog-col_pos     = 5.
*  FIELDCATALOG-KEY         = 'X'.
  fieldcatalog-outputlen   = 7.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'NAME1'.
  fieldcatalog-seltext_m   = 'Vendor'.
  fieldcatalog-col_pos     = 6.

  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

*  FIELDCATALOG-FIELDNAME   = 'LBKUM'.
*  FIELDCATALOG-SELTEXT_M   = 'Stk.In Hand'.
*  FIELDCATALOG-COL_POS     = 7.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.


  fieldcatalog-fieldname   = 'EBELN'.
  fieldcatalog-seltext_m   = 'PO No.'.
  fieldcatalog-col_pos     = 8.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'EBELP'.
  fieldcatalog-seltext_m   = 'Item'.
  fieldcatalog-col_pos     = 9.
  fieldcatalog-outputlen   = 02.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'BEDAT'.
  fieldcatalog-seltext_m   = 'PO DATE'.
  fieldcatalog-col_pos     = 10.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'ZTERM'.
  fieldcatalog-seltext_m   = 'PO PTM'.
  fieldcatalog-col_pos     = 11.
*  FIELDCATALOG-KEY         = 'X'.
  fieldcatalog-outputlen   = 05.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'ZTAG1'.
  fieldcatalog-seltext_m   = 'PO PTM Days'.
  fieldcatalog-col_pos     = 12.
*  FIELDCATALOG-KEY         = 'X'.
  fieldcatalog-outputlen   = 04.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'TEXT1'.
  fieldcatalog-seltext_m   = 'PO PTM'.
  fieldcatalog-col_pos     = 13.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-do_sum  = 'X'.
  fieldcatalog-outputlen   = 10.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'M_ZTERM'.
  fieldcatalog-seltext_m   = 'Master PTM'.
  fieldcatalog-col_pos     = 14.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-do_sum  = 'X'.
  fieldcatalog-outputlen   = 05.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'M_ZTAG1'.
  fieldcatalog-seltext_m   = 'Master PTM Days'.
  fieldcatalog-col_pos     = 15.
*  FIELDCATALOG-KEY         = 'X'.
  fieldcatalog-outputlen   = 04.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'TEXT2'.
  fieldcatalog-seltext_m   = 'Master PTM'.
  fieldcatalog-col_pos     = 16.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-do_sum  = 'X'.
  fieldcatalog-outputlen   = 10.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'DIFFERENCE'.
  fieldcatalog-seltext_m   = 'PTM DIFF'.
  fieldcatalog-col_pos     = 17.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-do_sum  = 'X'.
  fieldcatalog-outputlen   = 04.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

*  FIELDCATALOG-FIELDNAME   = 'NETPR'.
*  FIELDCATALOG-SELTEXT_M   = 'NLC'.
*  FIELDCATALOG-SELTEXT_L   = 'Net Landed Cost'.
*  FIELDCATALOG-COL_POS     = 11.
*  FIELDCATALOG-OUTPUTLEN   = 08.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
*
**  FIELDCATALOG-FIELDNAME   = 'WAERS'.
**  FIELDCATALOG-SELTEXT_M   = 'CURR'.
**  FIELDCATALOG-SELTEXT_L   = 'Currency Code'.
**  FIELDCATALOG-COL_POS     = 10.
***  FIELDCATALOG-KEY         = 'X'.
**   FIELDCATALOG-OUTPUTLEN   = 03.
**  APPEND FIELDCATALOG TO FIELDCATALOG.
**  CLEAR  FIELDCATALOG.
*
*
*  FIELDCATALOG-FIELDNAME   = 'VERPR'.
*  FIELDCATALOG-SELTEXT_M   = 'MAP'.
*  FIELDCATALOG-COL_POS     = 12.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
*
*
**  FIELDCATALOG-FIELDNAME   = 'MAP_WAERS'.
**  FIELDCATALOG-SELTEXT_M   = 'CURR'.
**  FIELDCATALOG-SELTEXT_L   = 'Currency Code'.
**  FIELDCATALOG-COL_POS     = 12.
**   FIELDCATALOG-OUTPUTLEN   = 03.
***  FIELDCATALOG-KEY         = 'X'.
**  APPEND FIELDCATALOG TO FIELDCATALOG.
**  CLEAR  FIELDCATALOG.
*
*  FIELDCATALOG-FIELDNAME   = 'DIFF'.
*  FIELDCATALOG-SELTEXT_M   = 'NLC-MAP'."'Var.wrt NLC'.
*  FIELDCATALOG-SELTEXT_L   = 'Var.wrt NLC'.
*  FIELDCATALOG-COL_POS     = 13.
*  FIELDCATALOG-OUTPUTLEN   = 08.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
*
*
*  FIELDCATALOG-FIELDNAME   = 'DIFF_PERC'.
*  FIELDCATALOG-SELTEXT_M   = 'NLC-MAP %'."Var.% wrt NLC'.
*  FIELDCATALOG-SELTEXT_L   = 'Var.% wrt NLC'.
*  FIELDCATALOG-COL_POS     = 14.
*  FIELDCATALOG-OUTPUTLEN   = 05.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
*
*  FIELDCATALOG-FIELDNAME   = 'L_NETPR'.
*  FIELDCATALOG-SELTEXT_M   = 'LPC'.
*  FIELDCATALOG-SELTEXT_L   = 'Last.Purchase.Cost'.
*  FIELDCATALOG-COL_POS     = 15.
*  FIELDCATALOG-OUTPUTLEN   = 08.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
**
**  FIELDCATALOG-FIELDNAME   = 'LPC_WAERS'.
**  FIELDCATALOG-SELTEXT_M   = 'CURR'.
**  FIELDCATALOG-SELTEXT_L   = 'Currency Code'.
**  FIELDCATALOG-COL_POS     = 16.
**   FIELDCATALOG-OUTPUTLEN   = 03.
***  FIELDCATALOG-KEY         = 'X'.
**  APPEND FIELDCATALOG TO FIELDCATALOG.
**  CLEAR  FIELDCATALOG.
*
*
*  FIELDCATALOG-FIELDNAME   = 'DIFF2'.
*  FIELDCATALOG-SELTEXT_M   = 'LPC-MAP'."Var.wrt LPC'.
*  FIELDCATALOG-SELTEXT_L   = 'Var.wrt LPC'.
*  FIELDCATALOG-COL_POS     = 17.
*  FIELDCATALOG-OUTPUTLEN   = 08.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
*
*
*  FIELDCATALOG-FIELDNAME   = 'DIFF_PERC2'.
*  FIELDCATALOG-SELTEXT_M   = 'LPC-MAP %'."Var.% wrt LPC'.
*  FIELDCATALOG-SELTEXT_L   = 'Var.% wrt LPC'.
*  FIELDCATALOG-COL_POS     = 18.
*  FIELDCATALOG-OUTPUTLEN   = 05.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
*
*  FIELDCATALOG-FIELDNAME   = 'NLC_LPC'.
*  FIELDCATALOG-SELTEXT_M   = 'NLC-LPC'."Var.wrt LPC'.
*  FIELDCATALOG-SELTEXT_L   = 'Var.wrt NLC-LPC'.
*  FIELDCATALOG-COL_POS     = 19.
*  FIELDCATALOG-OUTPUTLEN   = 08.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.
*
*
*  FIELDCATALOG-FIELDNAME   = 'DIFF_PERC3'.
*  FIELDCATALOG-SELTEXT_M   = 'NLC-LPC %'."Var.% wrt NLC'.
*  FIELDCATALOG-SELTEXT_L   = 'Var.% wrt NLC-MAP'.
*  FIELDCATALOG-COL_POS     = 20.
*  FIELDCATALOG-OUTPUTLEN   = 05.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.



ENDFORM.                    " BUILD_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM    text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                  selfield TYPE slis_selfield.

  READ TABLE it_out INDEX selfield-tabindex.
  CASE selfield-sel_tab_field.
    WHEN '1-EBELN'.
      SET PARAMETER ID 'BES' FIELD it_out-ebeln.
      IF NOT it_out-ebeln IS INITIAL.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN '1-LIFNR'.
*Terms of Payment
      SET PARAMETER ID 'LIF' FIELD it_out-lifnr.
      SET PARAMETER ID 'BUK' FIELD it_out-bukrs.
      SET PARAMETER ID 'EKO' FIELD it_out-ekorg.

      IF NOT it_out-lifnr IS INITIAL.
        CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN.
      ENDIF.
**     READ TABLE IT_OUT WITH KEY SELFIELD-TABINDEX.
**      SET PARAMETER ID 'BES' FIELD WA_OUT-AGR_NAME.
*      SET PARAMETER ID 'MAT' FIELD IT_OUT-MATNR.
*      IF NOT IT_OUT-MATNR IS INITIAL.
*
*        CALL TRANSACTION 'MB52' AND SKIP FIRST SCREEN.
*      ENDIF.
  ENDCASE.
ENDFORM.                    "USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.
  DATA : rs_variant LIKE disvariant,
         pline TYPE slis_listheader,
         v_lines TYPE i.
  CLEAR rs_variant.
  IMPORT rs_variant FROM MEMORY ID 'VARIANT'.
*  FREE MEMORY ID 'VARIANT'.
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
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EVENTS   text
*----------------------------------------------------------------------*
FORM build_eventtab USING    p_events TYPE slis_t_event.
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

ENDFORM.                               " BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_HEADING[]  text
*----------------------------------------------------------------------*
FORM build_comment  USING  p_heading TYPE slis_t_listheader.
  DATA gt_stack-is_layout-s_variant LIKE p_vairaint.
  DATA: hline TYPE slis_listheader,
        text(60) TYPE c,
        sep(20) TYPE c.
  CLEAR: hline, text.
*  IF option IS INITIAL.
*    hline-typ  = 'H'.
* ELSE.
  hline-typ  = 'S'.
* ENDIF.
  READ TABLE it_out .

  WRITE: 'No of Entries:' TO text,
         sy-tfill TO text+16.
*         '' TO TEXT+23.

  hline-info = text.

  APPEND hline TO p_heading.
  CLEAR text.

  WRITE: 'Company:' TO text,
          s_bukrs-low TO text+9.
*  write  S_bukrs-low TO TEXT+31.
**         '' TO TEXT+23.


  IF s_bukrs-high IS NOT INITIAL.
    WRITE 'and' TO text+14.
    WRITE  s_bukrs-high TO text+18.
  ENDIF.

  hline-info = text.

  APPEND hline TO p_heading.
  CLEAR text.



ENDFORM.                    " BUILD_COMMENT
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Added by CS on 05.11.2015 for layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_init .
*  BREAK-POINT.
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
* Addd by CS on 05.11.2015 for F4 Help Layout.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_f4_selection .
*  BREAK-POINT.
  gx_variant-report = sy-repid.
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
*&---------------------------------------------------------------------*
*&      Form  CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
* Added by CS on 05.11.2015 for validate existance of layout
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
*   Added by CS on 05.11.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth_obj .
  TYPES: BEGIN OF ty_t001w,
          werks TYPE t001w-werks, " Plants
        END OF ty_t001w,
        BEGIN OF ty_t001,
          bukrs TYPE t001-bukrs,  " Company Code
        END OF ty_t001
          .
  DATA: t_t001w TYPE TABLE OF ty_t001w, " Plants
        w_t001w TYPE ty_t001w,
        t_t001 TYPE TABLE OF ty_t001, " Company Code
        w_t001 TYPE ty_t001.

  FREE : t_t001w[], t_t001[].
  CLEAR: w_t001w, w_t001.

  break test1.

***** Start Code: Added by CS on 05.11.2015 for Plant Authorization. *****
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
***** End Code: Added by CS on 05.11.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 05.11.2015 for Company Code Authorization. *****
  SELECT bukrs  " Fetch values of Company Code
    FROM t001
    INTO TABLE t_t001
    WHERE bukrs IN s_bukrs.

  CLEAR: s_bukrs, lv_bukrs_auth_flg.
  REFRESH: s_bukrs[].
  IF t_t001[] IS NOT INITIAL.
    LOOP AT t_t001 INTO w_t001.
      AUTHORITY-CHECK OBJECT 'F_BKPF_BUK' " Company Code
                     ID 'ACTVT' FIELD '03'
                     ID 'BUKRS' FIELD w_t001-bukrs.
      IF sy-subrc EQ 0.
        s_bukrs-sign = 'I'.
        s_bukrs-option = 'EQ'.
        s_bukrs-low = w_t001-bukrs.
        APPEND s_bukrs.
        CLEAR: s_bukrs.
      ELSE.
        IF lv_bukrs_auth_flg IS INITIAL.  " Authorization Flag
          lv_bukrs_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t001.
    ENDLOOP.
  ENDIF.
  IF s_bukrs[] IS INITIAL.
    s_bukrs-sign = 'I'.
    s_bukrs-option = 'EQ'.
    s_bukrs-low = ''.
    APPEND s_bukrs.
    CLEAR: s_bukrs.
  ENDIF.
***** End Code: Added by CS on 05.11.2015 for Company Code Authorization. *****
ENDFORM.                    " CHECK_AUTH_OBJ
