*&---------------------------------------------------------------------*
*& Report  Z6MM034R_FG_STOCK_RECO
*& Developed By: Mrs. Punam Shinde
*&---------------------------------------------------------------------*
*& Requirement From : Kamalakar Verma.
*& Date of Developement Start: 26.07.2012
*&---------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 05.11.2015
*   REASON FOR CHANGE: Add Authorization & Layout
*   REQUEST #:
* --------------------------------------------------------------------------------------------*

REPORT  z6mm034r_fg_stock_reco_chapid.
TYPE-POOLS:slis.
TABLES: mara , mseg, mkpf , t001w , t001.

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

DATA: BEGIN OF wa_m,
      mblnr TYPE mkpf-mblnr,
      mjahr TYPE mseg-mjahr,
      budat TYPE mkpf-budat,
      werks TYPE mseg-werks,
      matnr TYPE mseg-matnr,
      charg TYPE mseg-charg,
      bwart TYPE mseg-bwart,
      lgort TYPE mseg-lgort,
      menge TYPE mseg-menge,
      shkzg TYPE mseg-shkzg,
      xblnr TYPE mkpf-xblnr,
      le_vbeln TYPE mkpf-le_vbeln,
      bukrs TYPE mseg-bukrs,
      ebeln TYPE mseg-ebeln,
      line_id TYPE mseg-line_id,
      parent_id TYPE mseg-parent_id,
      END OF wa_m,
      it_m LIKE TABLE OF wa_m,
      wa_temp LIKE wa_m.


DATA:BEGIN OF wa_mm,
*      VKORG TYPE MVKE-VKORG ,
      werks TYPE mard-werks,
      matnr TYPE mard-matnr,
      j_1ichid TYPE j_1imtchid-j_1ichid,
      maktx TYPE makt-maktx,
      menge TYPE  mseg-menge,"PRAVIN ADDED 08/01/16
      prd_qty2 TYPE mseg-menge,
      sale_qty3 TYPE mseg-menge,
      exp_sale4 TYPE mseg-menge,
      sto_qty5 TYPE mseg-menge,
      sales_ret TYPE mseg-menge,
      back_prd6 TYPE mseg-menge,
      701_qty7 TYPE mseg-menge,
      702_qty8 TYPE mseg-menge,
      in_process9 TYPE mseg-menge,
      clos_qty10 TYPE mseg-menge,
     END OF wa_mm,
      it_mm LIKE TABLE OF wa_mm.

DATA:BEGIN OF wa_out,
        werks TYPE t001w-werks,
        matnr TYPE mara-matnr,
        charg TYPE mseg-charg,
       opning1 TYPE mseg-menge,
       prd_qty2 TYPE mseg-menge,
       sale_qty3 TYPE mseg-menge,
       exp_sale4 TYPE mseg-menge,
       sto_qty5 TYPE mseg-menge,
   sales_ret TYPE mseg-menge,
       back_prd6 TYPE mseg-menge,
       701_qty7 TYPE mseg-menge,
       702_qty8 TYPE mseg-menge,
       block_qty9 TYPE mseg-menge,
       closing10 TYPE mseg-menge,
     END OF wa_out,
     it_out LIKE TABLE OF wa_out.

DATA: p_anfmenge TYPE stko-bmeng.

*data: begin of SELTAB_WA,
*      SELNAME type char12,
*      KIND type RSPARAMS-KIND,
*      SIGN type RSPARAMS-SIGN,
*      OPTION type RSPARAMS-OPTION,
*      LOW type RSPARAMS-LOW,
*      HIGH type RSPARAMS-high,
*      end of SELTAB_WA,
*      SELTAB like TABLE OF SELTAB_WA.
*
DATA: seltab TYPE TABLE OF rsparams,
         seltab_wa LIKE LINE OF seltab.

DATA: list_tab LIKE STANDARD TABLE OF abaplist.

DATA: BEGIN OF wa_mb5b,
      werks TYPE t001w-werks,
      matnr TYPE mara-matnr,
      maktx type makt-maktx,"pravin added 12/01/16
      from_date TYPE char10,
      to_date TYPE char10,
      open_menge TYPE char30,"MSEG-MENGE,
      rec_menge TYPE char30,"MSEG-MENGE,
     issu_menge TYPE char30,"MSEG-MENGE,
     clos_menge TYPE char30,"MSEG-MENGE,
     meins TYPE mseg-meins,
     END OF wa_mb5b,
     it_mb5b LIKE TABLE OF wa_mb5b.


DATA: BEGIN OF vlist OCCURS 0,
             line(1024) TYPE c,
         END OF vlist.

DATA: blank TYPE char8,
         lines TYPE i.

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

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME.

SELECT-OPTIONS: s_bukrs FOR t001-bukrs,
                s_werks FOR t001w-werks,
                s_matnr FOR mara-matnr,
                s_budat FOR mkpf-budat OBLIGATORY,
                s_lgort FOR mseg-lgort NO-DISPLAY.
PARAMETERS : p_var TYPE disvariant-variant MODIF ID pk. " Added by CS on 05.11.2015 for Layout
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME.
* Checkbox to eliminate lines with zero stocks
PARAMETERS: nozero   LIKE rmmmb-kznul.

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

  IF it_mm IS NOT INITIAL.
***** Start Code: Added by CS on 05.11.2015 for Authorization Message. *****
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Company Code/Plant.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
***** End Code: Added by CS on 05.11.2015 for Authorization Message. *****
    PERFORM process_data.
    PERFORM display.
  ELSE.
***** Start Code: Added by CS on 05.11.2015 for Authorization Message. *****
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'No Records Found or Missing Authorization for Company Code/Plant.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.
      MESSAGE 'No Records Found!' TYPE 'I'.
    ENDIF.
***** End Code: Added by CS on 05.11.2015 for Authorization Message. *****
  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  SELECT a~mblnr b~mjahr a~budat b~werks b~matnr b~charg b~bwart b~lgort
    b~menge b~shkzg a~xblnr a~le_vbeln b~bukrs b~ebeln b~line_id b~parent_id
    FROM mkpf AS a
    JOIN mseg AS b
    ON a~mblnr = b~mblnr
    AND a~mjahr = b~mjahr
    JOIN mara AS c
    ON b~matnr = c~matnr
    INTO TABLE it_m
    WHERE a~budat IN s_budat AND b~bukrs IN s_bukrs
    AND b~werks IN s_werks AND b~matnr IN s_matnr
    AND b~bwart IN ('311','601','641','701','702','101','261','262','531','532' , '561', '602','642',
                    '651', '652', '103' , '105')
    AND b~lgort IN ('1501','1500','1401' , '1515')
    AND c~mtart IN ('ZFGM' , 'ZSFG').

  IF it_m IS NOT INITIAL.

    SORT it_m BY mblnr mjahr werks matnr budat.


    SELECT DISTINCT b~werks b~matnr
      FROM mard AS b
      JOIN mara AS c
      ON b~matnr = c~matnr
      INTO TABLE it_mm
      WHERE  b~werks IN s_werks
*    and A~BUDAT IN S_BUDAT
      AND b~lgort = '1501'
      AND b~matnr IN s_matnr AND c~mtart IN ('ZFGM' , 'ZSFG').
*    AND A~VKORG IN S_BUKRS.

    SORT it_mm BY werks matnr.

    DELETE ADJACENT DUPLICATES FROM it_mm.

  ENDIF.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  DATA: vtweg TYPE vtweg.

  LOOP AT it_mm INTO wa_mm.

********************************************************OPENING QTY***********************************
*    CALL FUNCTION 'ZGET_OPENING_STOCK'
*      EXPORTING
*        WRK_MATNR       = WA_MM-MATNR
*        WRK_WERKS       = WA_MM-WERKS
*        WRK_DATE        = S_BUDAT-LOW
**       WRK_LGBST       = 'X'
**       WRK_BWBST       = ' '
*        WA_LGORT        = '1501'
**        WRK_BWKEY       = WA_MM-VKORG
*     IMPORTING
*       ANFMENGE        = WA_MM-MENGE
**       ENDMENGE        =
**       SOLL            =
**       HABEN           =
**     EXCEPTIONS
**       MANDATORY       = 1
**       OTHERS          = 2
*              .
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    MODIFY IT_MM FROM WA_MM TRANSPORTING MENGE.
****************Production QTY recved after QA Approval***************************************************************************


    LOOP AT it_m INTO wa_m
      WHERE werks = wa_mm-werks "AND BUKRS = WA_MM-VKORG
      AND matnr = wa_mm-matnr." AND BWART = '311' AND LGORT = '1500'.

      IF wa_m-shkzg = 'H'.
        wa_m-menge = wa_m-menge * -1.
      ENDIF.

      CASE wa_m-bwart.
        WHEN '311'.
          IF wa_m-lgort = '1500' OR wa_m-lgort = '1515'.
            READ TABLE it_m INTO wa_temp WITH KEY mblnr = wa_m-mblnr lgort = '1501' mjahr = wa_m-mjahr
            charg = wa_m-charg parent_id =  wa_m-line_id.
            IF sy-subrc = 0.
              IF wa_temp-shkzg = 'H'.
                wa_temp-menge = wa_temp-menge * -1.
              ENDIF.
              wa_mm-prd_qty2 = wa_mm-prd_qty2 + wa_temp-menge .
              CLEAR wa_temp-menge.
            ELSE.
              CLEAR wa_temp-menge.
            ENDIF.
          ELSEIF wa_m-lgort = '1501'.
************QTY issued back to Production(Captive Consumption) *******************************************************************
            READ TABLE it_m INTO wa_temp WITH KEY mblnr = wa_m-mblnr  mjahr = wa_m-mjahr lgort = '1401'
             charg = wa_m-charg.
            IF sy-subrc = 0.
              IF wa_temp-shkzg = 'H'.
                wa_temp-menge = wa_temp-menge * -1.
              ENDIF.

              wa_mm-back_prd6 = wa_mm-back_prd6 + wa_temp-menge .
              CLEAR wa_temp-menge.
            ELSE.
              CLEAR wa_temp-menge.
            ENDIF.

            READ TABLE it_m INTO wa_temp WITH KEY mblnr = wa_m-mblnr  mjahr = wa_m-mjahr lgort = '1500'
             charg = wa_m-charg parent_id =  wa_m-line_id.
            IF sy-subrc = 0.
              IF wa_temp-shkzg = 'H'.
                wa_temp-menge = wa_temp-menge * -1.
              ENDIF.

              wa_mm-in_process9 = wa_mm-in_process9 + wa_temp-menge .
              CLEAR wa_temp-menge.
            ELSE.
              CLEAR wa_temp-menge.
            ENDIF.

            READ TABLE it_m INTO wa_temp WITH KEY mblnr = wa_m-mblnr  mjahr = wa_m-mjahr lgort = '1515'
             charg = wa_m-charg parent_id =  wa_m-line_id.
            IF sy-subrc = 0.
              IF wa_temp-shkzg = 'H'.
                wa_temp-menge = wa_temp-menge * -1.
              ENDIF.

              wa_mm-back_prd6 = wa_mm-back_prd6 + wa_temp-menge .
              CLEAR wa_temp-menge.
            ELSE.
              CLEAR wa_temp-menge.
            ENDIF.

          ENDIF.
        WHEN '101' OR '531' OR '532' OR '561'.
          READ TABLE it_m INTO wa_temp WITH KEY mblnr = wa_m-mblnr lgort = '1501' mjahr = wa_m-mjahr
            charg = wa_m-charg .
          IF sy-subrc = 0.
            IF wa_temp-bwart = '101' OR wa_temp-bwart = '531'
              OR  wa_temp-bwart = '532' OR wa_temp-bwart = '561'.
              IF wa_temp-shkzg = 'H'.
                wa_temp-menge = wa_temp-menge * -1.
              ENDIF.
              wa_mm-prd_qty2 = wa_mm-prd_qty2 + wa_temp-menge .
              CLEAR wa_temp-menge.
            ELSE.
              CLEAR wa_temp-menge.
            ENDIF.
          ELSE.
            CLEAR wa_temp-menge.
          ENDIF.
        WHEN '261' OR '262'.
*        IF WA_M-LGORT = '1501'.
************QTY issued back to Production(Captive Consumption) *******************************************************************
          READ TABLE it_m INTO wa_temp WITH KEY mblnr = wa_m-mblnr  mjahr = wa_m-mjahr lgort = '1501'
           charg = wa_m-charg.
          IF sy-subrc = 0.
            IF wa_temp-shkzg = 'H'.
              wa_temp-menge = wa_temp-menge * -1.
            ENDIF.

            wa_mm-back_prd6 = wa_mm-back_prd6 + wa_temp-menge .
            CLEAR wa_temp-menge.
          ELSE.
            CLEAR wa_temp-menge.
          ENDIF.
*        endif.

        WHEN '601' OR '641' OR '602' OR '642'.
****************Sales QTY**********************************************************************
**************Export Sales QTY*********************************************************************
          IF wa_m-lgort = '1501'.
            IF wa_m-bwart = '601' OR wa_m-bwart = '602' AND wa_m-le_vbeln <> ''.

              CLEAR: vtweg.
              SELECT SINGLE vtweg FROM mvke INTO vtweg WHERE matnr =  wa_m-matnr ."AND VKORG = WA_M-BUKRS .
              IF sy-subrc <> 0 . CLEAR vtweg. ENDIF.
              IF vtweg = '10'.
                wa_mm-sale_qty3 = wa_mm-sale_qty3 + wa_m-menge.
              ELSEIF vtweg = '20'.
                wa_mm-exp_sale4 = wa_mm-exp_sale4 + wa_m-menge.
              ENDIF.
            ELSEIF  wa_m-bwart = '641' OR wa_m-bwart = '642'  AND wa_m-ebeln <> ''.
              wa_mm-sto_qty5 = wa_mm-sto_qty5 + wa_m-menge.
            ENDIF.
          ENDIF.
        WHEN  '701' OR '702'.
************QTY added for Physical stock Correction*********************************************

          IF wa_m-lgort = '1501'.
            IF wa_m-bwart = '701'.
              wa_mm-701_qty7 = wa_mm-701_qty7 + wa_m-menge .
            ELSEIF wa_m-bwart = '702'.
              wa_mm-702_qty8 = wa_mm-702_qty8 + wa_m-menge .
            ENDIF.
          ENDIF.

*        when '103' or '105'.
*          READ TABLE IT_M INTO WA_TEMP WITH KEY MBLNR = WA_M-MBLNR LGORT = '1501' MJAHR = WA_M-MJAHR
*            CHARG = WA_M-CHARG.
*          CLEAR WA_TEMP-MENGE.
        WHEN '651' OR '652'.
          IF wa_m-lgort = '1501'.
            IF wa_m-bwart = '651'.
              wa_mm-sales_ret = wa_mm-sales_ret + wa_m-menge .
            ELSEIF wa_m-bwart = '652'.
              wa_mm-sales_ret = wa_mm-sales_ret + wa_m-menge .
            ENDIF.
          ENDIF.


      ENDCASE.

    ENDLOOP.



    SELECT SINGLE maktx  FROM makt INTO wa_mm-maktx WHERE matnr = wa_mm-matnr AND spras = sy-langu.

    SELECT SINGLE j_1ichid
      FROM j_1imtchid
      INTO wa_mm-j_1ichid
      WHERE matnr = wa_mm-matnr
      AND werks = wa_mm-werks.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_mm-matnr
      IMPORTING
        output = wa_mm-matnr.

    PERFORM get_mb5b.

    IF wa_mm-in_process9 < 0.
      wa_mm-in_process9 =  wa_mm-in_process9 * -1.
    ENDIF.



    MODIFY it_mm FROM wa_mm TRANSPORTING menge matnr maktx prd_qty2 sale_qty3 exp_sale4 sto_qty5 sales_ret back_prd6
    701_qty7 702_qty8 in_process9 clos_qty10 j_1ichid.
    CLEAR wa_mm.
*     endif.
  ENDLOOP.

  IF nozero = 'X'.
    DELETE it_mm WHERE  menge = 0 AND prd_qty2 = 0 AND  sale_qty3 = 0 AND
      exp_sale4 = 0 AND sto_qty5 = 0 AND back_prd6 = 0 AND 701_qty7 = 0 AND 702_qty8 = 0
      AND in_process9 = 0 AND clos_qty10 = 0.
  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display .

  PERFORM build_fieldcatalog.

  wa_sort-fieldname = 'J_1ICHID'.
  wa_sort-spos = 1.
  wa_sort-subtot = 'X'.
  wa_sort-up = 'X' .
  APPEND wa_sort TO it_sort.

  wa_sort-fieldname = 'MATNR'.
  wa_sort-spos = 2.
  wa_sort-subtot = 'X'.
  wa_sort-up = 'X' .
  APPEND wa_sort TO it_sort.




  gd_layout-colwidth_optimize  = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
             i_bypassing_buffer      = 'X'
             i_callback_program      = sy-repid
*            i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
*            I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*            i_grid_title           = outtext
             is_layout               = gd_layout
             it_fieldcat             = fieldcatalog[]
*            it_special_groups       = gd_tabgroup
             is_variant               = g_variant
             it_events               = gt_events
             is_print                = gd_prntparams
             i_save                  = 'A' " 'X'
            it_sort                 = it_sort
*            is_variant              = z_template
        TABLES
             t_outtab                = it_mm[].

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

*  FIELDCATALOG-FIELDNAME   = 'VKORG'.
*  FIELDCATALOG-SELTEXT_M   = 'COMPANY'.
*  FIELDCATALOG-COL_POS     = 0.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.

  fieldcatalog-fieldname   = 'WERKS'.
  fieldcatalog-seltext_m   = 'Plant'.
  fieldcatalog-col_pos     = 0.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'J_1ICHID'.
  fieldcatalog-seltext_m   = 'Chapter ID'.
  fieldcatalog-col_pos     = 1.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'MATNR'.
  fieldcatalog-seltext_m   = 'Material'.
  fieldcatalog-col_pos     = 2.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'MAKTX'.
  fieldcatalog-seltext_m   = 'Material'.
  fieldcatalog-col_pos     = 3.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'MENGE'.
  fieldcatalog-seltext_m   = 'Opening Stk.'.
  fieldcatalog-col_pos     = 4.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'PRD_QTY2'.
  fieldcatalog-seltext_m   = 'Prod.QTY'.
  fieldcatalog-col_pos     = 5.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'SALE_QTY3'.
  fieldcatalog-seltext_m   = 'Domestic Sales'.
  fieldcatalog-col_pos     = 6.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'EXP_SALE4'.
  fieldcatalog-seltext_m   = 'Export Sales'.
  fieldcatalog-col_pos     = 7.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'STO_QTY5'.
  fieldcatalog-seltext_m   = 'STO QTY'.
  fieldcatalog-col_pos     = 8.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
* Sales Return SALES_RET

  fieldcatalog-fieldname   = 'SALES_RET'.
  fieldcatalog-seltext_m   = 'Sales Return QTY'.
  fieldcatalog-col_pos     = 9.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'BACK_PRD6'.
  fieldcatalog-seltext_m   = 'Captive Consm QTY'.
  fieldcatalog-col_pos     = 10.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = 'IN_PROCESS9'.
  fieldcatalog-seltext_m   = 'Issued to InProc.'.
  fieldcatalog-col_pos     = 11.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


  fieldcatalog-fieldname   = '701_QTY7'.
  fieldcatalog-seltext_m   = 'Add Phy.stk'.
  fieldcatalog-col_pos     = 12.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = '702_QTY8'.
  fieldcatalog-seltext_m   = 'Redu.Phy.stk'.
  fieldcatalog-col_pos     = 13.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  fieldcatalog-fieldname   = 'CLOS_QTY10'.
  fieldcatalog-seltext_m   = 'Closing Stock'.
  fieldcatalog-col_pos     = 14.
  fieldcatalog-do_sum = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.


ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  GET_MB5B
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mb5b .

  CLEAR: seltab_wa.
  REFRESH: seltab.
  CLEAR : it_mb5b, wa_mb5b, vlist , list_tab.
*
*  PERFORM SELTAB_FILL.
*
*  SUBMIT RM07MLBD
*     WITH SELECTION-TABLE SELTAB
*     EXPORTING LIST TO MEMORY
*     AND RETURN.
*
*  S_lgort-SELNAME = 'MATNR'.
*  SELTAB_WA-KIND = 'S'.
  s_lgort-sign    = 'I'.
  s_lgort-option  = 'BT'.
  s_lgort-low    = '1501'.
  s_lgort-high   = '1502'.
  APPEND s_lgort TO s_lgort.
  CLEAR s_lgort.

  SUBMIT rm07mlbd
     WITH matnr = wa_mm-matnr
     WITH werks = wa_mm-werks
     WITH datum IN s_budat
     WITH lgort IN s_lgort
     WITH lgbst = 'X'
     with bwbst = ''  " Added by CS on 05.11.2015 for Default Radio Button Selection
     with sbbst = ''  " Added by CS on 05.11.2015 for Default Radio Button Selection
     WITH pa_sumfl = 'X'
     WITH xchar = ''
     EXPORTING LIST TO MEMORY
     AND RETURN.

* From memory transfer the program output into internal table through below FM :
  IF sy-subrc = 0.
    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = list_tab.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Convert a (Saved) List Object to ASCI by using below FM.
    IF list_tab[] IS NOT INITIAL.

      CALL FUNCTION 'LIST_TO_ASCI'
        EXPORTING
          list_index = -1
        TABLES
          listasci   = vlist
          listobject = list_tab.

      IF sy-subrc <> '0'.
        WRITE:/ 'LIST_TO_ASCI error !! ', sy-subrc.
      ELSE.

      ENDIF.
    ENDIF.

    LOOP AT vlist WHERE NOT LINE CS '-----' .
      SPLIT vlist-line AT '|' INTO  blank
                                    wa_mb5b-werks
                                    wa_mb5b-matnr
                                    wa_mb5b-maktx
                                    wa_mb5b-from_date
                                    wa_mb5b-to_date
                                    wa_mb5b-open_menge
                                    wa_mb5b-rec_menge
                                    wa_mb5b-issu_menge
                                    wa_mb5b-clos_menge
                                    wa_mb5b-meins.
      REPLACE ALL OCCURRENCES OF ',' IN wa_mb5b-clos_menge WITH ''.
      REPLACE ALL OCCURRENCES OF ',' IN wa_mb5b-open_menge WITH ''.
      CONDENSE : wa_mb5b-werks,
                 wa_mb5b-matnr,
                 wa_mb5b-maktx,
                 wa_mb5b-from_date,
                 wa_mb5b-to_date,
                 wa_mb5b-open_menge,
                 wa_mb5b-rec_menge,
                 wa_mb5b-issu_menge,
                 wa_mb5b-clos_menge,
                 wa_mb5b-meins.
      APPEND wa_mb5b TO it_mb5b.
      CLEAR wa_mb5b.
    ENDLOOP.
    DELETE it_mb5b INDEX 1.
    DESCRIBE TABLE it_mb5b LINES lines.

    READ TABLE it_mb5b INTO wa_mb5b WITH KEY matnr = wa_mm-matnr werks = wa_mm-werks.
    IF sy-subrc <> 0.
      CLEAR wa_mb5b.

    ENDIF.

    MOVE wa_mb5b-clos_menge TO wa_mm-clos_qty10.
    MOVE wa_mb5b-open_menge TO wa_mm-menge.
  ENDIF.
ENDFORM.                                                    " GET_MB5B
*&---------------------------------------------------------------------*
*&      Form  SELTAB_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seltab_fill .
  "Material
  seltab_wa-selname = 'MATNR'.
  seltab_wa-kind = 'S'.
  seltab_wa-sign    = 'I'.
  seltab_wa-option  = 'EQ'.
  seltab_wa-low    = wa_mm-matnr.
  seltab_wa-high    = wa_mm-matnr.
  APPEND seltab_wa TO seltab.
  CLEAR seltab_wa.

  "Plant
  seltab_wa-selname = 'WERKS'.
  seltab_wa-kind = 'S'.
  seltab_wa-sign    = 'I'.
  seltab_wa-option  = 'EQ'.
  seltab_wa-low    = wa_mm-werks.
  APPEND seltab_wa TO seltab.
  CLEAR seltab_wa.

  seltab_wa-selname = 'LGORT'.
  seltab_wa-kind = 'S'.
  seltab_wa-sign    = 'I'.
  seltab_wa-option  = 'EQ'.
  seltab_wa-low    = '1501'."wa_mm-LGORT.
  APPEND seltab_wa TO seltab.
  CLEAR seltab_wa.

  seltab_wa-selname = 'DATUM'.
  seltab_wa-kind = 'S'.
  seltab_wa-sign    = 'I'.
  seltab_wa-option  = 'EQ'.
  seltab_wa-low    = s_budat-low.
  seltab_wa-high    = s_budat-high.
  APPEND seltab_wa TO seltab.
  CLEAR seltab_wa.


*  SELTAB_WA-SELNAME = 'DATUM-LOW'.
*  SELTAB_WA-KIND = 'P'.
*  SELTAB_WA-SIGN    = 'I'.
*  SELTAB_WA-OPTION  = 'EQ'.
*  SELTAB_WA-LOW    = S_BUDAT-LOW.
**  SELTAB_WA-HIGH    = S_BUDAT-HIGH.
*  APPEND SELTAB_WA TO SELTAB.
*  CLEAR SELTAB_WA.
*
*  SELTAB_WA-SELNAME = 'DATUM-HIGH'.
*  SELTAB_WA-KIND = 'P'.
*  SELTAB_WA-SIGN    = 'I'.
*  SELTAB_WA-OPTION  = 'EQ'.
**  SELTAB_WA-LOW    = S_BUDAT-LOW.
*  SELTAB_WA-HIGH    = S_BUDAT-HIGH.
*  APPEND SELTAB_WA TO SELTAB.
*  CLEAR SELTAB_WA.

*   seltab_wa-selname = 'LGBST'.
*   seltab_wa-kind = 'P'.
*   seltab_wa-sign    = 'I'.
*   seltab_wa-option  = 'EQ'.
*   seltab_wa-low    = wa_mm-WERKS.
*   APPEND seltab_wa TO seltab.
*   CLEAR seltab_wa.



ENDFORM.                    " SELTAB_FILL
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Added by CS on 05.11.2015 for layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form variant_init .
*  BREAK-POINT.
  g_save = 'A'.
  clear gx_variant.
  gx_variant-report = s_repid.
  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save     = g_save
    changing
      cs_variant = gx_variant
    exceptions
      not_found  = 2.
  if sy-subrc = 0.
    p_var = gx_variant-variant.
  endif.

endform.                    " VARIANT_INIT
*&---------------------------------------------------------------------*
*&      Form  VARIANT_F4_SELECTION
*&---------------------------------------------------------------------*
* Addd by CS on 05.11.2015 for F4 Help Layout.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form variant_f4_selection .
*  BREAK-POINT.
  gx_variant-report = sy-repid.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = gx_variant
      i_save     = 'A'
    importing
      e_exit     = g_exit
      es_variant = gx_variant
    exceptions
      not_found  = 2.

  if sy-subrc = 2.
    message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.

    if g_exit = space.
      p_var = gx_variant-variant.
    endif.

  endif.
endform.                    " VARIANT_F4_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
* Added by CS on 05.11.2015 for validate existance of layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_variant_existance .
  if not p_var is initial.
    move p_var to gx_variant-variant.
    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = g_save
      changing
        cs_variant = gx_variant.
  else.
    perform variant_init.
  endif.

endform.                    " CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 05.11.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_auth_obj .
  types: begin of ty_t001w,
          werks type t001w-werks, " Plants
        end of ty_t001w,
        begin of ty_t001,
          bukrs type t001-bukrs,  " Company Code
        end of ty_t001
          .
  data: t_t001w type table of ty_t001w, " Plants
        w_t001w type ty_t001w,
        t_t001 type table of ty_t001, " Company Code
        w_t001 type ty_t001.

  free : t_t001w[], t_t001[].
  clear: w_t001w, w_t001.

  break test1.

***** Start Code: Added by CS on 05.11.2015 for Plant Authorization. *****
  select werks  " Fetch values of Plant
    from t001w
    into table t_t001w
    where werks in s_werks.

  clear: s_werks, lv_werks_auth_flg.
  refresh: s_werks[].
  if t_t001w[] is not initial.
    loop at t_t001w into w_t001w.
      authority-check object 'M_MSEG_WWA' " Plant
                     id 'ACTVT' field '03'
                     id 'WERKS' field w_t001w-werks.
      if sy-subrc eq 0.
        s_werks-sign = 'I'.
        s_werks-option = 'EQ'.
        s_werks-low = w_t001w-werks.
        append s_werks.
        clear: s_werks.
      else.
        if lv_werks_auth_flg is initial.  " Authorization Flag
          lv_werks_auth_flg = 'X'.
        endif.
      endif.
      clear: w_t001w.
    endloop.
  endif.
  if s_werks[] is initial.
    s_werks-sign = 'I'.
    s_werks-option = 'EQ'.
    s_werks-low = ''.
    append s_werks.
    clear: s_werks.
  endif.
***** End Code: Added by CS on 05.11.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 05.11.2015 for Company Code Authorization. *****
  select bukrs  " Fetch values of Company Code
    from t001
    into table t_t001
    where bukrs in s_bukrs.

  clear: s_bukrs, lv_bukrs_auth_flg.
  refresh: s_bukrs[].
  if t_t001[] is not initial.
    loop at t_t001 into w_t001.
      authority-check object 'F_BKPF_BUK' " Company Code
                     id 'ACTVT' field '03'
                     id 'BUKRS' field w_t001-bukrs.
      if sy-subrc eq 0.
        s_bukrs-sign = 'I'.
        s_bukrs-option = 'EQ'.
        s_bukrs-low = w_t001-bukrs.
        append s_bukrs.
        clear: s_bukrs.
      else.
        if lv_bukrs_auth_flg is initial.  " Authorization Flag
          lv_bukrs_auth_flg = 'X'.
        endif.
      endif.
      clear: w_t001.
    endloop.
  endif.
  if s_bukrs[] is initial.
    s_bukrs-sign = 'I'.
    s_bukrs-option = 'EQ'.
    s_bukrs-low = ''.
    append s_bukrs.
    clear: s_bukrs.
  endif.
***** End Code: Added by CS on 05.11.2015 for Company Code Authorization. *****
endform.                    " CHECK_AUTH_OBJ
