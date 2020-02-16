*&---------------------------------------------------------------------*
*& Report  Z6MM031R_ALL_RECEIPTS
*&
*&---------------------------------------------------------------------*
*& DEVELOPER: PUNAM SHINDE
*& DEVELOPEMENT DATE: 17.08.2011
*& REPORT: Daily Receipt and Cumulative Receipt for the Month
*&---------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:
*        DEVELOPER:   Naren Karra             DATE:   04.11.2015
*        DESCRIPTION: MM: Authorization code added
*        REQUEST :    IRDK921499
*----------------------------------------------------------------------*
REPORT  Z6MM032R_ALL_RECEIPTS.
TYPE-POOLS:KKBLO, SLIS.


TABLES : MSEG , MKPF, MARA.
*ALV DATA DECLARATIONS
DATA: FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      "GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT    TYPE SLIS_LAYOUT_ALV,
"      GD_REPID     LIKE SY-REPID,
      GT_EVENTS     TYPE SLIS_T_EVENT,
      GD_PRNTPARAMS TYPE SLIS_PRINT_ALV.


*ALV DATA DECLARATIONS
DATA: FIELDCATALOG2 TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      "GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT2    TYPE SLIS_LAYOUT_ALV,
"      GD_REPID     LIKE SY-REPID,
      GT_EVENTS2     TYPE SLIS_T_EVENT,
      GD_PRNTPARAMS2 TYPE SLIS_PRINT_ALV.

DATA : ST_LAYOUT              TYPE SLIS_LAYOUT_ALV.
DATA: G_SAVE(1) TYPE C,
      G_EXIT(1) TYPE C,
      REPNAME  LIKE SY-REPID,
      G_VARIANT LIKE DISVARIANT,
      GX_VARIANT LIKE DISVARIANT,
      P_VAIRAINT LIKE DISVARIANT.

DATA: IT_SORT TYPE  SLIS_T_SORTINFO_ALV." WITH HEADER LINE.
DATA: WA_SORT LIKE LINE OF IT_SORT.

DATA: BEGIN OF WA_OUT,
        MBLNR TYPE MSEG-MBLNR,
        MJAHR TYPE MSEG-MJAHR,
        ZEILE TYPE MSEG-ZEILE,
        WERKS TYPE MSEG-WERKS,
        PLANT TYPE T001W-NAME1,
        MTART TYPE MARA-MTART,
        MATNR TYPE MARA-MATNR,
        MAKTX TYPE MAKT-MAKTX,
*  BUDAT TYPE MKPF-BUDAT,
        MENGE_D TYPE MSEG-MENGE,
        MEINS_D TYPE MSEG-MEINS,

        DMBTR_D TYPE MSEG-DMBTR,
        WAERS_D TYPE MSEG-WAERS,
*      Landed Rate for current date
        LR_D TYPE MSEG-DMBTR,

        MENGE_M TYPE MSEG-MENGE,
        MEINS_M TYPE MSEG-MEINS,

        DMBTR_M TYPE MSEG-DMBTR,
        WAERS_M TYPE MSEG-WAERS,
*      Landed Rate for current month
        LR_M TYPE MSEG-DMBTR,

        MENGE_P TYPE MSEG-MENGE,
        MEINS_P TYPE MSEG-MEINS,

        DMBTR_P TYPE MSEG-DMBTR,
        WAERS_P TYPE MSEG-WAERS,
*      Landed Rate for Current Period
        LR_P TYPE MSEG-DMBTR,

*      BP Rate
      END OF WA_OUT,
      IT_OUT LIKE TABLE OF WA_OUT.

DATA: BEGIN OF WA,
        MBLNR TYPE MSEG-MBLNR,
        MJAHR TYPE MSEG-MJAHR,
        ZEILE TYPE MSEG-ZEILE,
        WERKS TYPE MSEG-WERKS,
        MATNR TYPE MARA-MATNR,
        BUDAT TYPE MKPF-BUDAT,
        MENGE TYPE MSEG-MENGE,
        MEINS TYPE MSEG-MEINS,
        MTART TYPE MARA-MTART,
        SHKZG TYPE MSEG-SHKZG,
        DMBTR TYPE MSEG-DMBTR,
        EBELN TYPE MSEG-EBELN,
        LIFNR TYPE MSEG-LIFNR,
        BEDAT TYPE EKKO-BEDAT,
        NAME1 TYPE LFA1-NAME1,
        WAERS TYPE MSEG-WAERS,
*      Landed Rate for current date
        MAKTX TYPE MAKT-MAKTX,
        PLANT TYPE T001W-NAME1,
      END OF WA,
      IT LIKE TABLE OF WA,
      WA_P LIKE WA,
      IT_P LIKE TABLE OF WA_P,
      WA_D LIKE WA_P,
      IT_D LIKE TABLE OF WA_D,
      WA_M LIKE WA_P,
      IT_M LIKE TABLE OF WA_M,
      WA_DETAIL LIKE WA,
      IT_DETAIL LIKE TABLE OF WA_DETAIL,
      WA_DET_M LIKE WA,
      IT_DET_M LIKE TABLE OF WA_DET_M ,
      WA_DET_D LIKE WA,
      IT_DET_D LIKE TABLE OF WA_DET_D.

DATA: RATE TYPE EKPO-NETPR.

DATA: MON TYPE I,MON2 TYPE I , YER TYPE I , YER2 TYPE I.

DATA: START_DT(10) , FYEAR(04).
DATA: P TYPE I.
DATA: P1(02).
DATA: GV_AUTH_WERKS_FLG.                        " added by Naren Karra on 04.11.2015

CONSTANTS : FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                 VALUE 'TOP_OF_PAGE'.
TYPES: BEGIN OF T_VARINFO,
       FLAG TYPE C,
       OLENGTH TYPE X,
       LINE LIKE RALDB-INFOLINE,
END OF T_VARINFO.
DATA: INFOTAB TYPE T_VARINFO OCCURS 0 WITH HEADER LINE,
VARIANT_INFO TYPE RSVARADMIN OCCURS 0 WITH HEADER LINE .

DATA: IT_LISTHEADER TYPE SLIS_T_LISTHEADER.

*DATA: PERIOD TYPE CE21000-PERBL.
SELECTION-SCREEN BEGIN OF BLOCK A01 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT. " ALV Variant
SELECTION-SCREEN END OF BLOCK A01.

SELECTION-SCREEN BEGIN OF BLOCK  BLK1.
SELECT-OPTIONS:
*        S_MJAHR FOR MSEG-MJAHR NO INTERVALS NO-EXTENSION OBLIGATORY,
        S_BUDAT FOR MKPF-BUDAT OBLIGATORY ,"default start_dt to sy-datum ,
        S_WERKS FOR MSEG-WERKS,
        S_MTART FOR MARA-MTART,
        S_MATNR FOR MARA-MATNR.
SELECTION-SCREEN END OF BLOCK BLK1.

INITIALIZATION.
  REPNAME = SY-REPID.
  PERFORM INITIALIZE_VARIANT.

  CLEAR: P, P1.
  P = SY-DATUM+04(02).

* CONCATENATE S_DATUM-LOW(04) '0' S_DATUM-LOW+04(02) INTO PERIOD.
  IF P >= 4 AND P <= 12.
    CONCATENATE SY-DATUM(04) '0401' INTO START_DT.
  ELSEIF P <= 3 AND P >= 1 .
    FYEAR = SY-DATUM(04) - 1.
    CONCATENATE  FYEAR '0401' INTO START_DT.
  ENDIF.
  S_BUDAT-SIGN = 'I'.
  S_BUDAT-OPTION = 'BT'.
  S_BUDAT-LOW = START_DT.
  S_BUDAT-HIGH = SY-DATUM.
  APPEND S_BUDAT.


AT SELECTION-SCREEN.
  PERFORM PAI_OF_SELECTION_SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_FOR_VARIANT.


START-OF-SELECTION.
  PERFORM CHECK_AUTH.                                                     " added by Naren Karra on 04.11.2015

  CALL FUNCTION 'PRINT_SELECTIONS'
    EXPORTING
      MODE      = 'X'
      RNAME     = SY-REPID "program name
      RVARIANTE = VARIANT_INFO-VARIANT "varient name
    TABLES
      INFOTAB   = INFOTAB.

  IF S_BUDAT-HIGH IS  INITIAL.
    S_BUDAT-HIGH = SY-DATUM.
    APPEND S_BUDAT.
  ENDIF.

  PERFORM GET_DATA.
************************Start********************************     " added by NK on 04.11.2015
IF GV_AUTH_WERKS_FLG = 'X'.
  MESSAGE 'Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.
ENDIF.
*************************End*********************************
  IF IT_OUT IS NOT INITIAL .
    PERFORM DISPLAY.
  ELSE.
    MESSAGE 'No Records Found /Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.            " modified by Naren Karra on 04.11.2015
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

  SELECT B~MBLNR B~MJAHR B~ZEILE B~WERKS B~MATNR A~BUDAT B~MENGE B~MEINS C~MTART B~SHKZG B~DMBTR
    B~EBELN B~LIFNR
  FROM MKPF AS A
  JOIN MSEG AS B
  ON A~MBLNR =  B~MBLNR
  JOIN MARA AS C
  ON B~MATNR = C~MATNR
  INTO TABLE IT
  WHERE A~BUDAT IN S_BUDAT
  AND B~BWART IN ('101' , '102' ,'122' , '161' ,'162')
  AND B~MATNR IN S_MATNR
  AND B~WERKS IN S_WERKS
  AND C~MTART IN S_MTART.
*AND A~MJAHR IN S_MJAHR.

  SORT IT BY MBLNR.

  LOOP AT IT INTO WA.
    CLEAR: RATE.
    SELECT SINGLE MAKTG
    FROM MAKT
    INTO WA-MAKTX
    WHERE MATNR = WA-MATNR
    AND SPRAS = 'EN'.

    SELECT SINGLE BEDAT FROM EKKO INTO WA-BEDAT WHERE EBELN = WA-EBELN.
    SELECT SINGLE NAME1 FROM LFA1 INTO WA-NAME1 WHERE LIFNR = WA-LIFNR AND SPRAS = 'EN'.
    SELECT SINGLE NAME1 FROM T001W INTO WA-PLANT WHERE WERKS = WA-WERKS.
    IF WA-EBELN IS NOT INITIAL.
      IF WA-DMBTR = 0.
        SELECT SINGLE NETPR
          FROM EKPO INTO RATE
          WHERE EBELN = WA-EBELN
          AND MATNR = WA-MATNR.
        IF SY-SUBRC = 0.
          WA-DMBTR = WA-MENGE * RATE.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA-MATNR
      IMPORTING
        OUTPUT = WA-MATNR.

    CLEAR: MON , MON2.



    IF WA-SHKZG = 'H'.
      WA-MENGE = WA-MENGE * -1.
      WA-DMBTR = WA-DMBTR * -1.
    ENDIF.

    CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
      EXPORTING
        I_DATE  = WA-BUDAT
      IMPORTING
        E_MONTH = MON
        E_YEAR  = YER.

    CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
      EXPORTING
        I_DATE  = S_BUDAT-HIGH
      IMPORTING
        E_MONTH = MON2
        E_YEAR  = YER2.

    IF WA-BUDAT = S_BUDAT-HIGH. "FOR ON DATE
      WA_D = WA.
      MOVE-CORRESPONDING WA TO WA_DET_D.
      APPEND WA_DET_D TO IT_DET_D.

      CLEAR: WA_DET_D ,WA_D-MBLNR , WA_D-SHKZG ,WA_D-ZEILE , WA_D-BUDAT, WA_P-MJAHR
             ,WA_D-EBELN  ,WA_D-LIFNR ,WA_D-BEDAT ,WA_D-NAME1 .
      COLLECT WA_D INTO IT_D.
    ENDIF.
    IF MON = MON2 AND YER = YER2." FOR IN MONTH
      WA_M = WA.
      MOVE-CORRESPONDING WA TO WA_DET_M.
      APPEND WA_DET_M TO IT_DET_M.


      CLEAR: WA_DET_M ,WA_M-MBLNR , WA_M-SHKZG ,WA_M-ZEILE , WA_M-BUDAT, WA_P-MJAHR
             ,WA_M-EBELN  ,WA_M-LIFNR ,WA_M-BEDAT ,WA_M-NAME1 .
      COLLECT WA_M INTO IT_M.

*   APPEND WA TO IT_M.
    ENDIF.
    IF WA-BUDAT IN S_BUDAT.
      WA_P = WA.
      CLEAR: WA_P-MBLNR , WA_P-SHKZG ,WA_P-ZEILE , WA_P-BUDAT, WA_P-MJAHR
            ,WA_P-EBELN	,WA_P-LIFNR	,WA_P-BEDAT	,WA_P-NAME1 .
      COLLECT WA_P INTO IT_P.
*   APPEND WA TO IT_P.
    ENDIF.



    MODIFY IT FROM WA TRANSPORTING MATNR MENGE DMBTR BEDAT NAME1 MAKTX PLANT.
  ENDLOOP.

  CLEAR: WA, WA_P , WA_D , WA_M.
  SORT IT_M BY MATNR.
  SORT IT_D BY MATNR.
  SORT IT_P BY MATNR.

*MBLNR TYPE MSEG-MBLNR,
*MJAHR TYPE MSEG-MJAHR,
*ZEILE TYPE MSEG-ZEILE,
*WERKS TYPE MSEG-WERKS,
*MATNR TYPE MARA-MATNR,
*BUDAT TYPE MKPF-BUDAT,
*MENGE TYPE MSEG-MENGE,
*MEINS TYPE MSEG-MEINS,
*MTART TYPE MARA-MTART,
*SHKZG TYPE MSEG-SHKZG,
**      Landed Rate for current date
*MAKTX TYPE MAKT-MAKTX,


  LOOP AT IT_P INTO WA_P.
    MOVE WA_P-MJAHR TO WA_OUT-MJAHR.
    MOVE WA_P-WERKS TO WA_OUT-WERKS.
    MOVE WA_P-MATNR TO WA_OUT-MATNR.
    MOVE WA_P-MENGE TO WA_OUT-MENGE_P.
    MOVE WA_P-MEINS TO WA_OUT-MEINS_P.
    MOVE WA_P-MTART TO WA_OUT-MTART.
    MOVE WA_P-DMBTR TO WA_OUT-DMBTR_P.
    MOVE WA_P-WAERS TO WA_OUT-WAERS_P.
    MOVE WA_P-MAKTX TO WA_OUT-MAKTX.
    MOVE WA_P-PLANT TO WA_OUT-PLANT.

    READ TABLE IT_M INTO WA_M WITH KEY MATNR = WA_P-MATNR WERKS = WA_P-WERKS. "MJAHR = WA_P-MJAHR.
    IF SY-SUBRC <> 0. CLEAR WA_M. ENDIF.
    MOVE WA_M-MENGE TO WA_OUT-MENGE_M.
    MOVE WA_M-MEINS TO WA_OUT-MEINS_M.
    MOVE WA_M-DMBTR TO WA_OUT-DMBTR_M.
    MOVE WA_M-WAERS TO WA_OUT-WAERS_M.


    READ TABLE IT_D INTO WA_D WITH KEY MATNR = WA_P-MATNR WERKS = WA_P-WERKS. "MJAHR = WA_P-MJAHR.
    IF SY-SUBRC <> 0. CLEAR WA_D. ENDIF.
    MOVE WA_D-MENGE TO WA_OUT-MENGE_D.
    MOVE WA_D-MEINS TO WA_OUT-MEINS_D.
    MOVE WA_D-DMBTR TO WA_OUT-DMBTR_D.
    MOVE WA_D-WAERS TO WA_OUT-WAERS_D.

*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT  = WA_OUT-MATNR
*      IMPORTING
*        OUTPUT = WA_OUT-MATNR.
*

*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT  = WA_OUT-MATNR
*      IMPORTING
*        OUTPUT = WA_OUT-MATNR.

    IF WA_OUT-DMBTR_D <> 0.
      IF WA_OUT-MENGE_D <> 0.
        WA_OUT-LR_D =  WA_OUT-DMBTR_D / WA_OUT-MENGE_D.
      ENDIF.
    ENDIF.
    IF WA_OUT-DMBTR_M <> 0.
      IF WA_OUT-MENGE_M <> 0.
        WA_OUT-LR_M =  WA_OUT-DMBTR_M / WA_OUT-MENGE_M .
      ENDIF.
    ENDIF.
    IF WA_OUT-DMBTR_P <> 0.
      IF WA_OUT-MENGE_P <> 0.
        WA_OUT-LR_P =  WA_OUT-DMBTR_P / WA_OUT-MENGE_P.
      ENDIF.
    ENDIF.
    APPEND WA_OUT TO IT_OUT.
    CLEAR WA_OUT.
  ENDLOOP.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY .
  PERFORM BUILD_FIELDCATALOG.
*  WA_SORT-SPOS = '01' .
*  WA_SORT-FIELDNAME = 'MTART'.
*  WA_SORT-TABNAME = 'IT_OUT'.
*  WA_SORT-UP = 'X' .
*  WA_SORT-SUBTOT = 'X'.
*  APPEND WA_SORT TO IT_SORT.

*WA_SORT-FIELDNAME = 'MATNR'.
*WA_SORT-up = 'X' .
*Append WA_SORT TO it_sort.
*
*WA_SORT-FIELDNAME = 'MAKTX'.
*WA_SORT-up = 'X' .
*Append WA_SORT TO it_sort.


  PERFORM SUB_GET_EVENT.
  PERFORM BUILD_COMMENT CHANGING IT_LISTHEADER[].

  WA_SORT-FIELDNAME = 'WERKS'.
  WA_SORT-UP = 'X' .
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.

*  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  DATA VARIANT  LIKE  DISVARIANT. CLEAR VARIANT.
*  VARIANT-REPORT    = SY-REPID.
  GD_LAYOUT-ZEBRA        = 'X'.
  "GD_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_BYPASSING_BUFFER      = 'X'
            I_CALLBACK_PROGRAM      = SY-REPID
*            i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
            I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*            i_grid_title           = outtext
            IS_LAYOUT               = GD_LAYOUT
            IT_FIELDCAT             = FIELDCATALOG[]
*            it_special_groups       = gd_tabgroup
            IT_EVENTS               = GT_EVENTS
            IS_PRINT                = GD_PRNTPARAMS
            I_SAVE                  = G_SAVE
            I_DEFAULT               = 'A'
            IS_VARIANT              = G_VARIANT
            IT_SORT                 = IT_SORT

*            is_variant              = z_template
       TABLES
            T_OUTTAB                = IT_OUT.
ENDFORM.                    " DISPLAY
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG .

  FIELDCATALOG-FIELDNAME   = 'MTART'.
  FIELDCATALOG-SELTEXT_M   = 'Mat.Type'.
  FIELDCATALOG-COL_POS     = 0.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.


  FIELDCATALOG-FIELDNAME   = 'WERKS'.
  FIELDCATALOG-SELTEXT_M   = 'Plant'.
  FIELDCATALOG-COL_POS     = 1.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-seltext_m   = 'TEST'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'PLANT'.
  FIELDCATALOG-SELTEXT_M   = 'Plant'.
  FIELDCATALOG-COL_POS     = 2.
*  FIELDCATALOG-KEY         = 'X'.
*  FIELDCATALOG-seltext_m   = 'TEST'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

*  FIELDCATALOG-FIELDNAME   = 'MJAHR'.
*  FIELDCATALOG-SELTEXT_M   = 'Fy.Year'.
*  FIELDCATALOG-COL_POS     = 2.
**  FIELDCATALOG-KEY         = 'X'.
**  FIELDCATALOG-seltext_m   = 'TEST'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.


  FIELDCATALOG-FIELDNAME   = 'MATNR'.
  FIELDCATALOG-SELTEXT_M   = 'Material'.
  FIELDCATALOG-COL_POS     = 3.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MAKTX'.
  FIELDCATALOG-SELTEXT_M   = 'Description'.
  FIELDCATALOG-COL_POS     = 4.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MENGE_D'.
  FIELDCATALOG-SELTEXT_M   = 'GRR QTY on Dt.'.
  FIELDCATALOG-COL_POS     = 5.
  FIELDCATALOG-DO_SUM  = 'X'.
  FIELDCATALOG-KEY         = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

*  FIELDCATALOG-FIELDNAME   = 'MEINS_D'.
*  FIELDCATALOG-SELTEXT_M   = 'UOM'.
*  FIELDCATALOG-COL_POS     = 6.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'LR_D'.
  FIELDCATALOG-SELTEXT_M   = 'Rate for Dt.'.
  FIELDCATALOG-COL_POS     = 7.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.


  FIELDCATALOG-FIELDNAME   = 'MENGE_M'.
  FIELDCATALOG-SELTEXT_M   = 'GRR QTY on Mon'.
  FIELDCATALOG-COL_POS     = 8.
  FIELDCATALOG-KEY         = 'X'.
  FIELDCATALOG-DO_SUM  = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

*  FIELDCATALOG-FIELDNAME   = 'MEINS_M'.
*  FIELDCATALOG-SELTEXT_M   = 'UOM'.
*  FIELDCATALOG-COL_POS     = 9.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'LR_M'.
  FIELDCATALOG-SELTEXT_M   = 'Rate for Month'.
  FIELDCATALOG-COL_POS     = 10.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MENGE_P'.
  FIELDCATALOG-SELTEXT_M   = 'GRR QTY in Period'.
  FIELDCATALOG-COL_POS     = 11.
  FIELDCATALOG-KEY         = 'X'.
  FIELDCATALOG-DO_SUM  = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MEINS_P'.
  FIELDCATALOG-SELTEXT_M   = 'UOM'.
  FIELDCATALOG-COL_POS     = 12.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.


  FIELDCATALOG-FIELDNAME   = 'LR_P'.
  FIELDCATALOG-SELTEXT_M   = 'Rate for Period'.
  FIELDCATALOG-COL_POS     = 13.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

*  FIELDCATALOG-FIELDNAME   = 'GLTRP'.
*  FIELDCATALOG-SELTEXT_M   = 'Prod.Date'.
*  FIELDCATALOG-COL_POS     = 9.
**  FIELDCATALOG-KEY         = 'X'.
*  APPEND FIELDCATALOG TO FIELDCATALOG.
*  CLEAR  FIELDCATALOG.

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  SUB_GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUB_GET_EVENT .
  CONSTANTS : C_FORMNAME_SUBTOTAL_TEXT TYPE SLIS_FORMNAME VALUE
  'SUBTOTAL_TEXT'.
  DATA: L_S_EVENT TYPE SLIS_ALV_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE     = 4
    IMPORTING
      ET_EVENTS       = GT_EVENTS
    EXCEPTIONS
      LIST_TYPE_WRONG = 0
      OTHERS          = 0.
* Subtotal
  READ TABLE GT_EVENTS  INTO L_S_EVENT
                    WITH KEY NAME = SLIS_EV_SUBTOTAL_TEXT.
  IF SY-SUBRC = 0.
    MOVE C_FORMNAME_SUBTOTAL_TEXT TO L_S_EVENT-FORM.
    MODIFY GT_EVENTS FROM L_S_EVENT INDEX SY-TABIX.
  ENDIF.

  READ TABLE GT_EVENTS INTO L_S_EVENT
                    WITH KEY NAME = SLIS_EV_TOP_OF_PAGE.
  IF SY-SUBRC = 0.
    MOVE FORMNAME_TOP_OF_PAGE TO L_S_EVENT-FORM.
    APPEND L_S_EVENT TO GT_EVENTS.
  ENDIF.




ENDFORM.                    " SUB_GET_EVENT

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM LIKE SY-UCOMM SELFIELD TYPE SLIS_SELFIELD.

  CLEAR : IT_DETAIL , WA_DETAIL.

  READ TABLE IT_OUT INTO WA_OUT INDEX SELFIELD-TABINDEX.
  CASE SELFIELD-SEL_TAB_FIELD.
    WHEN '1-MENGE_P'.
      LOOP AT IT INTO WA WHERE MATNR =  WA_OUT-MATNR AND WERKS =  WA_OUT-WERKS.
        MOVE-CORRESPONDING WA TO WA_DETAIL.
        APPEND WA_DETAIL TO IT_DETAIL.
        CLEAR WA_DETAIL.
      ENDLOOP.
    WHEN '1-MENGE_M'.
      LOOP AT IT_DET_M INTO WA_DET_M WHERE MATNR =  WA_OUT-MATNR AND WERKS =  WA_OUT-WERKS.
        MOVE-CORRESPONDING WA_DET_M TO WA_DETAIL.
        APPEND WA_DETAIL TO IT_DETAIL.
        CLEAR WA_DETAIL.
      ENDLOOP.

    WHEN '1-MENGE_D'.
      LOOP AT IT_DET_D INTO WA_DET_D WHERE MATNR =  WA_OUT-MATNR AND WERKS =  WA_OUT-WERKS.
        MOVE-CORRESPONDING WA_DET_D TO WA_DETAIL.
        APPEND WA_DETAIL TO IT_DETAIL.
        CLEAR WA_DETAIL.
      ENDLOOP.
*      SET PARAMETER ID 'BES' FIELD WA_OUT-EBELN.
*      IF NOT WA_OUT-EBELN IS INITIAL.
*        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*      ENDIF.
  ENDCASE.

  IF IT_DETAIL IS NOT INITIAL .

    PERFORM NEXT_DISPLAY.

  ENDIF.
  CLEAR: WA_OUT,WA_DETAIL, IT_DETAIL , FIELDCATALOG2[] , FIELDCATALOG2.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  NEXT_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NEXT_DISPLAY .
  PERFORM FIELDCATAL.

  GD_LAYOUT2-COLWIDTH_OPTIMIZE = 'X'.
*  DATA VARIANT  LIKE  DISVARIANT. CLEAR VARIANT.
*  VARIANT-REPORT    = SY-REPID.

  "GD_REPID = SY-REPID.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_BYPASSING_BUFFER                = 'X'
            I_CALLBACK_PROGRAM      = SY-REPID
*            i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
            I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*            i_grid_title           = outtext
            IS_LAYOUT               = GD_LAYOUT2
            IT_FIELDCAT             = FIELDCATALOG2[]
*            it_special_groups       = gd_tabgroup
            IT_EVENTS               = GT_EVENTS2
            IS_PRINT                = GD_PRNTPARAMS2
            I_SAVE                  = 'X'
*            IT_SORT                 = IT_SORT

*            is_variant              = z_template
       TABLES
            T_OUTTAB                = IT_DETAIL.



ENDFORM.                    " NEXT_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCATAL .


  FIELDCATALOG2-FIELDNAME   = 'MATNR'.
  FIELDCATALOG2-SELTEXT_M   = 'Material'.
  FIELDCATALOG2-COL_POS     = 0.
*  FIELDCATALOG-KEY         = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.

  FIELDCATALOG2-FIELDNAME   = 'MAKTX'.
  FIELDCATALOG2-SELTEXT_M   = 'Description'.
  FIELDCATALOG2-COL_POS     = 1.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.

  FIELDCATALOG2-FIELDNAME   = 'MBLNR'.
  FIELDCATALOG2-SELTEXT_M   = 'Document'.
  FIELDCATALOG2-COL_POS     = 2.
  FIELDCATALOG2-DO_SUM  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.

  FIELDCATALOG2-FIELDNAME   = 'MJAHR'.
  FIELDCATALOG2-SELTEXT_M   = 'Year'.
  FIELDCATALOG2-COL_POS     = 3.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.

  FIELDCATALOG2-FIELDNAME   = 'BUDAT'.
  FIELDCATALOG2-SELTEXT_M   = 'Date'.
  FIELDCATALOG2-COL_POS     = 4.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.


  FIELDCATALOG2-FIELDNAME   = 'MENGE'.
  FIELDCATALOG2-SELTEXT_M   = 'QTY'.
  FIELDCATALOG2-COL_POS     = 5.
  FIELDCATALOG2-DO_SUM  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.


  FIELDCATALOG2-FIELDNAME   = 'MEINS'.
  FIELDCATALOG2-SELTEXT_M   = 'UOM'.
  FIELDCATALOG2-COL_POS     = 6.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.


  FIELDCATALOG2-FIELDNAME   = 'MTART'.
  FIELDCATALOG2-SELTEXT_M   = 'Mat.Type'.
  FIELDCATALOG2-COL_POS     = 7.
*  FIELDCATALOG-do_sum  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.


  FIELDCATALOG2-FIELDNAME   = 'DMBTR'.
  FIELDCATALOG2-SELTEXT_M   = 'Value'.
  FIELDCATALOG2-COL_POS     = 8.
  FIELDCATALOG2-DO_SUM  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.


  FIELDCATALOG2-FIELDNAME   = 'EBELN'.
  FIELDCATALOG2-SELTEXT_M   = 'PO Num'.
  FIELDCATALOG2-COL_POS     = 9.
  FIELDCATALOG2-DO_SUM  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.

  FIELDCATALOG2-FIELDNAME   = 'BEDAT'.
  FIELDCATALOG2-SELTEXT_M   = 'PO DATE'.
  FIELDCATALOG2-COL_POS     = 10.
  FIELDCATALOG2-DO_SUM  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.


  FIELDCATALOG2-FIELDNAME   = 'LIFNR'.
  FIELDCATALOG2-SELTEXT_M   = 'Vendor'.
  FIELDCATALOG2-COL_POS     = 11.
  FIELDCATALOG2-DO_SUM  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.

  FIELDCATALOG2-FIELDNAME   = 'NAME1'.
  FIELDCATALOG2-SELTEXT_M   = 'Vendor'.
  FIELDCATALOG2-COL_POS     = 12.
  FIELDCATALOG2-DO_SUM  = 'X'.
  APPEND FIELDCATALOG2 TO FIELDCATALOG2.
  CLEAR  FIELDCATALOG2.


*        MTART TYPE MARA-MTART,
*        SHKZG TYPE MSEG-SHKZG,
*        DMBTR TYPE MSEG-DMBTR,
*        WAERS TYPE MSEG-WAERS,
**      Landed Rate for current date
*        MAKTX TYPE MAKT-MAKTX,
ENDFORM.                    " FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE_VARIANT .
  G_SAVE = 'A'.
  CLEAR G_VARIANT.
  G_VARIANT-REPORT = REPNAME.
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
FORM F4_FOR_VARIANT .



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

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_LISTHEADER.
ENDFORM.                    " top_of_page.
*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_LISTHEADER[]  text
*----------------------------------------------------------------------*
FORM BUILD_COMMENT CHANGING LT_LISTHEADER TYPE SLIS_T_LISTHEADER.

  DATA : LINE TYPE SLIS_LISTHEADER.
*       text(100).
  DATA: FLG(01).

*  CLEAR LINE.
*  LINE-TYP = 'H'.
*  LINE-INFO = TEXT.
*  APPEND LINE TO LT_LISTHEADER.

  IF NOT GX_VARIANT-TEXT IS INITIAL.
    CLEAR LINE.
    LINE-TYP = 'S'.
    LINE-INFO = GX_VARIANT-TEXT.
    APPEND LINE TO LT_LISTHEADER.
  ENDIF.
  DELETE INFOTAB WHERE LINE IS INITIAL.
  LOOP AT INFOTAB.


    CLEAR LINE.
    LINE-TYP = 'S'.
    CONDENSE INFOTAB-LINE.

    IF INFOTAB-LINE = 'G/L Account'.
*      LINE-TYP = 'D'.
      FLG = 'X'.
    ENDIF.
    IF FLG = ''.
      LINE-INFO = INFOTAB-LINE.
      APPEND LINE TO LT_LISTHEADER.
    ENDIF.
  ENDLOOP.

*clear line.
*clear text.
*line-typ = 'S'.
*line-key = ''.
*line-info = ''.
*append line to lt_listheader.

ENDFORM.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AUTH .
TYPES: BEGIN OF TY_T001W,
        WERKS TYPE WERKS_D,
       END OF TY_T001W.
DATA : WA_T001W TYPE TY_T001W, I_T001W TYPE STANDARD TABLE OF TY_T001W.

SELECT WERKS
  FROM T001W
  INTO TABLE I_T001W
  WHERE WERKS IN S_WERKS.

  CLEAR: GV_AUTH_WERKS_FLG, S_WERKS.
  REFRESH: S_WERKS[].
 IF I_T001W[] IS NOT INITIAL.
  LOOP AT I_T001W INTO WA_T001W.
    AUTHORITY-CHECK OBJECT 'M_MSEG_WWA'
                        ID 'WERKS' FIELD WA_T001W-WERKS
                        ID 'ACTVT' FIELD '03'.
    IF SY-SUBRC EQ 0.
      S_WERKS-SIGN = 'I'.
      S_WERKS-OPTION = 'EQ'.
      S_WERKS-LOW = WA_T001W-WERKS.
      APPEND S_WERKS.
      CLEAR S_WERKS.
    ELSE.
      IF GV_AUTH_WERKS_FLG IS INITIAL.
        GV_AUTH_WERKS_FLG = 'X'.
      ENDIF.
    ENDIF.
    CLEAR WA_T001W.
  ENDLOOP.
 ENDIF.

  IF S_WERKS[] IS INITIAL.
    S_WERKS-SIGN = 'I'.
    S_WERKS-OPTION = 'EQ'.
    S_WERKS-LOW = ''.
    APPEND S_WERKS.
  ENDIF.
ENDFORM.                    " CHECK_AUTH
