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
*   CHANGE ON: 04.11.2015
*   REASON FOR CHANGE: Add Authorization & Layout
*   REQUEST #: IRDK907627
* --------------------------------------------------------------------------------------------*


REPORT  z6mm031r_mov_avg_price.
TABLES: ekko , ekpo , mbew , mara.

TYPE-POOLS:slis.
*ALV DATA DECLARATIONS
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      "GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      gd_layout    TYPE slis_layout_alv,
"      GD_REPID     LIKE SY-REPID,
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
      END OF it_out.
*      IT_OUT LIKE TABLE OF WA_OUT.

*DATA: FNAME1(10) TYPE C , COLR TYPE C VALUE '6'.


DATA: fname1(10) TYPE c,
          int TYPE c ,
          colr TYPE c.
*********************


DATA:f_cell LIKE LINE OF  it_out-cell.
DATA:f_cell2 LIKE LINE OF  it_out-cell.

***** Start Code: Added by CS on 04.11.2015 for layout. *****
DATA: s_repid   LIKE sy-repid,
      g_save(1) TYPE c,
      g_exit(1) TYPE c,                         " ALV VARIANT
      gx_variant TYPE disvariant.
***** End Code: Added by CS on 04.11.2015 for layout. *****
***** Start Code: Added by CS on 04.11.2015 for Authorization. *****
DATA: lv_bukrs_auth_flg TYPE c VALUE '',  " Auth. Flag for Company Code
      lv_werks_auth_flg TYPE c VALUE ''  " Auth. Flag for Plant
.
***** End Code: Added by CS on 04.11.2015 for Authorization. *****

SELECTION-SCREEN BEGIN OF BLOCK blk1.
SELECT-OPTIONS: s_bukrs FOR ekpo-bukrs DEFAULT '1000' OBLIGATORY,
                s_werks FOR ekpo-werks,
                s_bedat FOR ekko-bedat,
                s_mtart FOR mara-mtart,
                s_matnr FOR mara-matnr,
                s_ebeln FOR ekko-ebeln,
                s_lifnr FOR ekko-lifnr.
PARAMETERS : p_var TYPE disvariant-variant MODIF ID pk. " Added by CS on 04.11.2015 for Layout
SELECTION-SCREEN END OF BLOCK blk1.

***** Start Code: Added by CS on 04.11.2015 for layout. *****
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM variant_f4_selection.                    " F4 HELP FOR VARIANT ON SELECTION SCREEN

AT SELECTION-SCREEN.
  s_repid = sy-repid.
  PERFORM check_variant_existance.

INITIALIZATION.
  s_repid = sy-repid.
  PERFORM variant_init.
***** End Code: Added by CS on 04.11.2015 for layout. *****


START-OF-SELECTION.
  PERFORM check_auth_obj.  " Added by CS on 04.11.2015 for Authorization.

  PERFORM get_data.
  IF it_out[] IS NOT INITIAL .
***** Start Code: Added by CS on 04.11.2015 for Authorization Message. *****
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Company Code/Plant.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
***** End Code: Added by CS on 04.11.2015 for Authorization Message. *****
    PERFORM display.
  ELSE.
***** Start Code: Added by CS on 04.11.2015 for Authorization Message. *****

    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'No Records Found or Missing Authorization for Company Code/Plant.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.
***** End Code: Added by CS on 04.11.2015 for Authorization Message. *****
      MESSAGE 'No Records Found !' TYPE 'I'.
    ENDIF.
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
    c~lbkum b~waers b~lifnr
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

    SELECT SINGLE waers FROM t001 INTO it_out-map_waers WHERE bukrs =  it_out-bukrs.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_out-matnr
      IMPORTING
        output = it_out-matnr.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = it_out-lifnr
      IMPORTING
        output = it_out-lifnr.

*--------- convert NLC tO INR
    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*     CLIENT                  = SY-MANDT
        date                    = it_out-bedat
        foreign_amount          = it_out-netpr
        foreign_currency        = it_out-waers
        local_currency          = 'INR'
*     RATE                    = 0
       type_of_rate            = 'M'
       read_tcurr              = 'X'
     IMPORTING
*     EXCHANGE_RATE           =
*     FOREIGN_FACTOR          =
       local_amount            = it_out-netpr
*     LOCAL_FACTOR            =
*     EXCHANGE_RATEX          =
*     FIXED_RATE              =
*     DERIVED_RATE_TYPE       =
*   EXCEPTIONS
*     NO_RATE_FOUND           = 1
*     OVERFLOW                = 2
*     NO_FACTORS_FOUND        = 3
*     NO_SPREAD_FOUND         = 4
*     DERIVED_2_TIMES         = 5
*     OTHERS                  = 6
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*--------- convert LPC tO INR
    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*     CLIENT                  = SY-MANDT
        date                    = it_out-bedat
        foreign_amount          = it_out-l_netpr
        foreign_currency        = it_out-lpc_waers
        local_currency          = 'INR'
*     RATE                    = 0
       type_of_rate            = 'M'
       read_tcurr              = 'X'
     IMPORTING
*     EXCHANGE_RATE           =
*     FOREIGN_FACTOR          =
       local_amount            = it_out-l_netpr
*     LOCAL_FACTOR            =
*     EXCHANGE_RATEX          =
*     FIXED_RATE              =
*     DERIVED_RATE_TYPE       =
*   EXCEPTIONS
*     NO_RATE_FOUND           = 1
*     OVERFLOW                = 2
*     NO_FACTORS_FOUND        = 3
*     NO_SPREAD_FOUND         = 4
*     DERIVED_2_TIMES         = 5
*     OTHERS                  = 6
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    it_out-diff = it_out-netpr - it_out-verpr.

*  DIFF_PERC2
    IF it_out-diff > 0.
      IF it_out-netpr <> 0.
        it_out-diff_perc = ( it_out-diff * 100 ) / it_out-netpr. "( IT_OUT-VERPR / IT_OUT-NETPR ) * 100.
      ENDIF.
    ELSEIF it_out-diff < 0.
      IF it_out-verpr <> 0.
        it_out-diff_perc = ( it_out-diff * 100 ) / it_out-verpr. "( IT_OUT-VERPR / IT_OUT-NETPR ) * 100.
      ENDIF.
    ENDIF.


    it_out-diff2 = it_out-l_netpr - it_out-verpr.

    IF it_out-diff2 > 0.
      IF it_out-l_netpr <> 0.
        it_out-diff_perc2 = ( it_out-diff2 * 100 ) / it_out-l_netpr. "( IT_OUT-VERPR / IT_OUT-NETPR ) * 100.
      ENDIF.
    ELSEIF it_out-diff2 < 0.
      IF it_out-verpr <> 0.
        it_out-diff_perc2 = ( it_out-diff2 * 100 ) / it_out-verpr. "( IT_OUT-VERPR / IT_OUT-NETPR ) * 100.
      ENDIF.
    ENDIF.

    it_out-nlc_lpc = it_out-netpr - it_out-l_netpr.

    IF it_out-nlc_lpc > 0.
      IF it_out-netpr <> 0.
        it_out-diff_perc3 = ( it_out-nlc_lpc * 100 ) / it_out-netpr.
      ENDIF.
    ELSEIF it_out-nlc_lpc < 0.
      IF it_out-l_netpr <> 0.
        it_out-diff_perc3 = ( it_out-nlc_lpc * 100 ) / it_out-l_netpr.
      ENDIF.
    ENDIF.


    MODIFY it_out[] FROM it_out.
  ENDLOOP.

  SORT it_out[] BY bukrs werks mtart matnr ebeln ebelp.


*"Here we define which column we want to give
*"color, in this case it's column CITYFROM
*CLEAR f_cell.
*f_cell-fname = 'DIFF' .
*"This is a number to give color RED
*f_cell-COLOR-COL = 6.
*f_cell-NOKEYCOL = 'X'.
*APPEND f_cell TO it_OUT-CELL.
*MODIFY it_OUT TRANSPORTING CELL
*WHERE DIFF > 0.
*"This is a number to give color GREEN
*CLEAR: f_cell , it_OUT-CELL.
*f_cell-fname = 'DIFF' .
*f_cell-COLOR-COL = 5.
*f_cell-NOKEYCOL = 'X'.
*APPEND f_cell TO it_OUT-CELL.
*MODIFY it_OUT TRANSPORTING CELL
*WHERE DIFF < 0.
***---------------------------------------------------------------------
**CLEAR: f_cell2 .", it_OUT-CELL.
**f_cell2-fname = 'DIFF2' .
**"This is a number to give color RED
**f_cell2-COLOR-COL = 6.
**f_cell2-NOKEYCOL = 'X'.
**APPEND f_cell2 TO it_OUT-CELL.
**MODIFY it_OUT TRANSPORTING CELL
**WHERE DIFF2 > 0.
**"This is a number to give color GREEN
**CLEAR: f_cell2 .", it_OUT-CELL.
**f_cell2-fname = 'DIFF2' .
**f_cell2-COLOR-COL = 5.
**f_cell2-NOKEYCOL = 'X'.
**APPEND f_cell2 TO it_OUT-CELL.
**MODIFY it_OUT TRANSPORTING CELL
**WHERE DIFF2 < 0.





*GD_LAYOUT-INFO_FIELDNAME = 'COLOR'.
*GD_LAYOUT-COLTAB_FIELDNAME = 'CELL2'.


*GD_LAYOUT-F2CODE = '&ETA'.

*GT_GRID-S_LAYOUT-F2CODE



**********LOOP*******
  LOOP AT it_out .

    IF it_out-diff > 0.
      fname1 = 'DIFF'.
      colr = 6.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.

    IF it_out-diff2 > 0.
      fname1 = 'DIFF2'.
      colr = 6.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.

    IF it_out-nlc_lpc > 0.
      fname1 = 'NLC_LPC'.
      colr = 6.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.



    IF it_out-diff < 0.
      fname1 = 'DIFF'.
      colr = 5.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.

    IF it_out-diff2 < 0.
      fname1 = 'DIFF2'.
      colr = 5.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.

    IF it_out-nlc_lpc < 0.
      fname1 = 'NLC_LPC'.
      colr = 5.
      int = '0'.
      PERFORM set_cell_colours.
    ENDIF.



    IF it_out-cell IS NOT INITIAL.
      MODIFY it_out  TRANSPORTING cell.
    ENDIF.

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
            it_events               = gt_events
            is_print                = gd_prntparams
            i_save                  = 'A' " 'X'
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
  fieldcatalog-seltext_m   = 'Description'.
  fieldcatalog-col_pos     = 4.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'LIFNR'.
  fieldcatalog-seltext_m   = 'Vendor'.
  fieldcatalog-col_pos     = 5.
  fieldcatalog-outputlen   = 7.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'NAME1'.
  fieldcatalog-seltext_m   = 'Vendor'.
  fieldcatalog-col_pos     = 6.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'LBKUM'.
  fieldcatalog-seltext_m   = 'Stk.In Hand'.
  fieldcatalog-col_pos     = 7.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'EBELN'.
  fieldcatalog-seltext_m   = 'PO No.'.
  fieldcatalog-col_pos     = 8.
  fieldcatalog-key         = 'X'.
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
  fieldcatalog-col_pos     = 11.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'NETPR'.
  fieldcatalog-seltext_m   = 'NLC'.
  fieldcatalog-seltext_l   = 'Net Landed Cost'.
  fieldcatalog-col_pos     = 11.
  fieldcatalog-outputlen   = 08.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

*  FIELDCATALOG-FIELDNAME   = 'WAERS'.
*  FIELDCATALOG-SELTEXT_M   = 'CURR'.
*  FIELDCATALOG-SELTEXT_L   = 'Currency Code'.
*  FIELDCATALOG-COL_POS     = 10.
**  FIELDCATALOG-KEY         = 'X'.
*   FIELDCATALOG-OUTPUTLEN   = 03.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.


  fieldcatalog-fieldname   = 'VERPR'.
  fieldcatalog-seltext_m   = 'MAP'.
  fieldcatalog-col_pos     = 12.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


*  FIELDCATALOG-FIELDNAME   = 'MAP_WAERS'.
*  FIELDCATALOG-SELTEXT_M   = 'CURR'.
*  FIELDCATALOG-SELTEXT_L   = 'Currency Code'.
*  FIELDCATALOG-COL_POS     = 12.
*   FIELDCATALOG-OUTPUTLEN   = 03.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.

  fieldcatalog-fieldname   = 'DIFF'.
  fieldcatalog-seltext_m   = 'NLC-MAP'."'Var.wrt NLC'.
  fieldcatalog-seltext_l   = 'Var.wrt NLC'.
  fieldcatalog-col_pos     = 13.
  fieldcatalog-outputlen   = 08.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'DIFF_PERC'.
  fieldcatalog-seltext_m   = 'NLC-MAP %'."Var.% wrt NLC'.
  fieldcatalog-seltext_l   = 'Var.% wrt NLC'.
  fieldcatalog-col_pos     = 14.
  fieldcatalog-outputlen   = 05.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'L_NETPR'.
  fieldcatalog-seltext_m   = 'LPC'.
  fieldcatalog-seltext_l   = 'Last.Purchase.Cost'.
  fieldcatalog-col_pos     = 15.
  fieldcatalog-outputlen   = 08.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
*
*  FIELDCATALOG-FIELDNAME   = 'LPC_WAERS'.
*  FIELDCATALOG-SELTEXT_M   = 'CURR'.
*  FIELDCATALOG-SELTEXT_L   = 'Currency Code'.
*  FIELDCATALOG-COL_POS     = 16.
*   FIELDCATALOG-OUTPUTLEN   = 03.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.


  fieldcatalog-fieldname   = 'DIFF2'.
  fieldcatalog-seltext_m   = 'LPC-MAP'."Var.wrt LPC'.
  fieldcatalog-seltext_l   = 'Var.wrt LPC'.
  fieldcatalog-col_pos     = 17.
  fieldcatalog-outputlen   = 08.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'DIFF_PERC2'.
  fieldcatalog-seltext_m   = 'LPC-MAP %'."Var.% wrt LPC'.
  fieldcatalog-seltext_l   = 'Var.% wrt LPC'.
  fieldcatalog-col_pos     = 18.
  fieldcatalog-outputlen   = 05.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'NLC_LPC'.
  fieldcatalog-seltext_m   = 'NLC-LPC'."Var.wrt LPC'.
  fieldcatalog-seltext_l   = 'Var.wrt NLC-LPC'.
  fieldcatalog-col_pos     = 19.
  fieldcatalog-outputlen   = 08.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'DIFF_PERC3'.
  fieldcatalog-seltext_m   = 'NLC-LPC %'."Var.% wrt NLC'.
  fieldcatalog-seltext_l   = 'Var.% wrt NLC-MAP'.
  fieldcatalog-col_pos     = 20.
  fieldcatalog-outputlen   = 05.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.



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
*   WHEN '2-MATNR'.
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
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Added by CS on 04.11.2015 for layout
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
* Addd by CS on 04.11.2015 for F4 Help Layout.
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
* Added by CS on 04.11.2015 for validate existance of layout
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
*   Added by CS on 04.11.2015 for Authorization
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

***** Start Code: Added by CS on 04.11.2015 for Plant Authorization. *****
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
***** End Code: Added by CS on 04.11.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 04.11.2015 for Company Code Authorization. *****
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
***** End Code: Added by CS on 04.11.2015 for Company Code Authorization. *****
ENDFORM.                    " CHECK_AUTH_OBJ
