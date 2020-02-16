*&---------------------------------------------------------------------*
*& Report  Z6MM035R_BP_DSI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:
*        DEVELOPER:   Naren Karra             DATE:   28.10.2015
*        DESCRIPTION: FI,MM: Authorization code added
*        REQUEST :    IRDK921306
*----------------------------------------------------------------------*

REPORT  Z6MM035R_BP_DSI.
TYPE-POOLS:SLIS.
TABLES: MARA , CE21000 , MVKE, BSEG, T001K, MSEG.


*ALV DATA DECLARATIONS
DATA: FIELDCATALOG TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE,
      "GD_TAB_GROUP TYPE SLIS_T_SP_GROUP_ALV,
      GD_LAYOUT    TYPE SLIS_LAYOUT_ALV,
"      GD_REPID     LIKE SY-REPID,
      G_VARIANT LIKE DISVARIANT,
      GT_EVENTS     TYPE SLIS_T_EVENT,
      GD_PRNTPARAMS TYPE SLIS_PRINT_ALV.


DATA: IT_SORT TYPE  SLIS_T_SORTINFO_ALV." WITH HEADER LINE.
DATA: WA_SORT LIKE LINE OF IT_SORT.

TYPES : BEGIN OF TY_OUT,
          MATNR TYPE MARA-MATNR,
          MAKTX TYPE MAKT-MAKTX,
          MTART TYPE MARA-MTART,
          OPEN_MENGE TYPE MSEG-MENGE,
          MATKL TYPE MARA-MATKL,
          SPART TYPE MARA-SPART,
          VTWEG TYPE MVKE-VTWEG,
          BUKRS TYPE T001K-BUKRS,
          WGBEZ TYPE T023T-WGBEZ,
          TILL_MONTH TYPE p DECIMALS 0,"MSEG-MENGE,
          DSI(20),
          MONTH1 TYPE MSEG-MENGE,
          MONTH2 TYPE MSEG-MENGE,
          MONTH3 TYPE MSEG-MENGE,
          MONTH4 TYPE MSEG-MENGE,
          MONTH5 TYPE MSEG-MENGE,
          MONTH6 TYPE MSEG-MENGE,
          MONTH7 TYPE MSEG-MENGE,
          MONTH8 TYPE MSEG-MENGE,
          MONTH9 TYPE MSEG-MENGE,
          MONTH10 TYPE MSEG-MENGE,
          MONTH11 TYPE MSEG-MENGE,
          MONTH12 TYPE MSEG-MENGE,
*          MONTH13 TYPE MSEG-MENGE,
*          MONTH14 TYPE MSEG-MENGE,
*          MONTH15 TYPE MSEG-MENGE,
*          MONTH16 TYPE MSEG-MENGE,
*          MONTH17 TYPE MSEG-MENGE,
*          MONTH18 TYPE MSEG-MENGE,
        END OF TY_OUT.

DATA: WA_OUT TYPE TY_OUT ,
      IT_OUT LIKE TABLE OF WA_OUT.

DATA: LIST_TAB LIKE STANDARD TABLE OF ABAPLIST.

DATA: BEGIN OF VLIST OCCURS 0,
             LINE(1024) TYPE C,
         END OF VLIST.

DATA: BLANK TYPE CHAR8,
         LINES TYPE I.

DATA: BEGIN OF WA_MB5B,
      WERKS TYPE T001W-WERKS,
      MATNR TYPE MARA-MATNR,
      FROM_DATE TYPE CHAR10,
      TO_DATE TYPE CHAR10,
      OPEN_MENGE TYPE CHAR30,"MSEG-MENGE,
      REC_MENGE TYPE CHAR30,"MSEG-MENGE,
     ISSU_MENGE TYPE CHAR30,"MSEG-MENGE,
     CLOS_MENGE TYPE CHAR30,"MSEG-MENGE,
     MEINS TYPE MSEG-MEINS,
     END OF WA_MB5B,
     IT_MB5B LIKE TABLE OF WA_MB5B.

DATA: PERIOD TYPE CE21000-PERBL.
DATA: NO_OF_D TYPE P.

DATA: BEGIN OF WA_BP,
        PERBL TYPE CE21000-PERBL,
        ARTNR TYPE CE21000-ARTNR ,
*        BZIRK TYPE CE21000-BZIRK ,
        ABSMG001 TYPE CE21000-ABSMG001,
        DATE TYPE SY-DATUM,
      END OF WA_BP,
      IT_BP LIKE TABLE OF WA_BP,
      WA_BP_FINAL LIKE WA_BP,
      IT_BP_FINAL LIKE TABLE OF WA_BP_FINAL.

DATA: MON1 TYPE T247-LTX,
      MON2 TYPE T247-LTX,
      MON3 TYPE T247-LTX,
      MON4 TYPE T247-LTX,
      MON5 TYPE T247-LTX,
      MON6 TYPE T247-LTX,
      MON7 TYPE T247-LTX,
      MON8 TYPE T247-LTX,
      MON9 TYPE T247-LTX,
      MON10 TYPE T247-LTX,
      MON11 TYPE T247-LTX,
      MON12 TYPE T247-LTX,
      MON13 TYPE T247-LTX,
      MON14 TYPE T247-LTX,
      MON15 TYPE T247-LTX,
      MON16 TYPE T247-LTX,
      MON17 TYPE T247-LTX,
      MON18 TYPE T247-LTX,
      MON19 TYPE T247-LTX,
      MON20 TYPE T247-LTX,
      MON21 TYPE T247-LTX,
      MON22 TYPE T247-LTX,
      MON23 TYPE T247-LTX,
      MON24 TYPE T247-LTX.

************************Start********************************     " added by NK on 28.10.2015
DATA: GV_AUTH_BUKRS_FLG,
      GV_AUTH_SPART_FLG,
      GV_AUTH_VTWEG_FLG,
      GV_AUTH_MTART_FLG.

DATA: G_SAVE,
      G_EXIT,
      REPID LIKE SY-REPID,
*      G_VARIANT LIKE DISVARIANT,
      GX_VARIANT LIKE DISVARIANT.

*************************End*********************************

SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME.
SELECT-OPTIONS: S_BUKRS FOR T001K-BUKRS NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT '1000',
                S_MATNR FOR MARA-MATNR,
                S_SPART FOR MARA-SPART,
                S_VTWEG FOR MVKE-VTWEG,
                S_MTART FOR MARA-MTART,
                S_GJAHR FOR BSEG-GJAHR obligatory,
                S_DATUM FOR SY-DATUM NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT SY-DATUM.

SELECT-OPTIONS:  S_LGORT FOR MSEG-LGORT .

SELECTION-SCREEN END OF BLOCK BLK.
************************Start********************************     " added by NK on 28.10.2015
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
  PARAMETERS P_VARI TYPE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK B3.
*************************End*********************************
SELECTION-SCREEN : BEGIN OF BLOCK TEXTNOTE  WITH FRAME TITLE TEXT-I01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(79) TEXT-I02 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN : END OF BLOCK TEXTNOTE.

INITIALIZATION.                                                           " added by Naren Karra on 28.10.2015
REPID = SY-REPID.
PERFORM INITIAL_VARIANT.

AT SELECTION-SCREEN.
  PERFORM PAI_OF_SELECTION-SCREEN.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM F4_FOR_VARIANT.

START-OF-SELECTION.
  PERFORM CHECK_AUTH.                                                      " added by Naren Karra on 28.10.2015
  PERFORM GET_MATERIALS.
  PERFORM GET_PERIOD.

  IF IT_OUT[] IS NOT INITIAL.

*  PERFORM GET_DATE.
    PERFORM GET_OPN_STK.
************************Start********************************     " added by NK on 28.10.2015
   IF GV_AUTH_BUKRS_FLG = 'X' OR GV_AUTH_SPART_FLG = 'X' OR GV_AUTH_VTWEG_FLG = 'X' OR GV_AUTH_MTART_FLG = 'X'.
     MESSAGE 'Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.
   ENDIF.
*************************End*********************************
    PERFORM DISPLAY.
  ELSE.
    MESSAGE 'No records found/ Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.      " added by Naren Karra on 28.10.2015
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_MATERIALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MATERIALS .

  IF S_MTART IS INITIAL.
    S_MTART-SIGN    = 'I'.
    S_MTART-OPTION  = 'EQ'.
    S_MTART-LOW    = 'ZFGM'.
    APPEND S_MTART TO S_MTART.
    CLEAR S_MTART.

    S_MTART-SIGN    = 'I'.
    S_MTART-OPTION  = 'EQ'.
    S_MTART-LOW    = 'ZTRD'.
    APPEND S_MTART TO S_MTART.
    CLEAR S_MTART.
  ELSE.
    IF S_MTART-LOW <> 'ZFGM' AND S_MTART-LOW <> 'ZTRD'.
      CLEAR: S_MTART , S_MTART[].
    ENDIF.
  ENDIF.
  IF S_MTART[] IS NOT INITIAL.
    SELECT A~MATNR B~MAKTX A~MTART A~MATKL A~SPART C~VTWEG F~BUKRS
    FROM MARA AS A
      JOIN MAKT AS B
      ON A~MATNR = B~MATNR
      JOIN MVKE AS C
      ON A~MATNR = C~MATNR
      JOIN MARD AS D
      ON A~MATNR = D~MATNR
      JOIN MBEW AS E
      ON D~MATNR = E~MATNR
      AND D~WERKS = E~BWKEY
      JOIN T001K AS F
      ON E~BWKEY = F~BWKEY
      INTO CORRESPONDING FIELDS OF TABLE IT_OUT
      WHERE A~MATNR IN S_MATNR
      AND SPART IN S_SPART
      AND VTWEG IN S_VTWEG
      AND MTART IN S_MTART
      AND BUKRS IN S_BUKRS.
    SORT IT_OUT BY BUKRS MATNR MAKTX MTART MATKL SPART VTWEG  .

    DELETE ADJACENT DUPLICATES FROM IT_OUT.
  ENDIF.
*  DELETE IT_OUT WHERE MTART not in ('ZFGM' , 'ZTRD' ).

ENDFORM.                    " GET_MATERIALS
*&---------------------------------------------------------------------*
*&      Form  GET_OPN_STK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_OPN_STK .

  CLEAR : IT_MB5B, WA_MB5B, VLIST , LIST_TAB.

*  S_LGORT-SIGN    = 'I'.
*  S_LGORT-OPTION  = 'EQ'.
*  S_LGORT-LOW    = '1501'.
**  S_LGORT-HIGH   = '1501'.
*  APPEND S_LGORT TO S_LGORT.
*  CLEAR S_LGORT.
*
*  S_LGORT-SIGN    = 'I'.
*  S_LGORT-OPTION  = 'EQ'.
*  S_LGORT-LOW    = '1601'.
**  S_LGORT-HIGH   = '1601'.
*  APPEND S_LGORT TO S_LGORT.
*  CLEAR S_LGORT.

  LOOP AT IT_OUT INTO WA_OUT.

    SUBMIT RM07MLBD
       WITH MATNR = WA_OUT-MATNR
*     WITH WERKS = WA_MM-WERKS
       WITH DATUM IN S_DATUM
       WITH LGORT IN S_LGORT
       WITH LGORT IN S_LGORT
       WITH BUKRS IN S_BUKRS
       WITH LGBST = 'X'
       WITH PA_SUMFL = 'X'
       WITH XCHAR = ''
       EXPORTING LIST TO MEMORY
       AND RETURN.

************************Start********************************     " added by NK on 28.10.2015
  IF SY-SUBRC EQ 0.                     " To overwrite the message which is displayed from that program unnecessarily -> 'RM07MLBD' text-083
   MESSAGE ' ' TYPE 'S'.
  ENDIF.
*************************End*********************************

************* From memory transfer the program output into internal table through below FM :
    IF SY-SUBRC = 0.
      CLEAR: LIST_TAB , VLIST , VLIST[] , IT_MB5B, WA_MB5B .
      CALL FUNCTION 'LIST_FROM_MEMORY'
        TABLES
          LISTOBJECT = LIST_TAB.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

* Convert a (Saved) List Object to ASCI by using below FM.
      IF LIST_TAB[] IS NOT INITIAL.

        CALL FUNCTION 'LIST_TO_ASCI'
          EXPORTING
            LIST_INDEX = -1
          TABLES
            LISTASCI   = VLIST
            LISTOBJECT = LIST_TAB.

        IF SY-SUBRC <> '0'.
          WRITE:/ 'LIST_TO_ASCI error !! ', SY-SUBRC.
        ELSE.

        ENDIF.
      ENDIF.

      LOOP AT VLIST WHERE NOT LINE CS '-----' .
        SPLIT VLIST-LINE AT '|' INTO  BLANK
                                      WA_MB5B-WERKS
                                      WA_MB5B-MATNR
                                      WA_MB5B-FROM_DATE
                                      WA_MB5B-TO_DATE
                                      WA_MB5B-OPEN_MENGE
                                      WA_MB5B-REC_MENGE
                                      WA_MB5B-ISSU_MENGE
                                      WA_MB5B-CLOS_MENGE
                                      WA_MB5B-MEINS.
        REPLACE ALL OCCURRENCES OF ',' IN WA_MB5B-CLOS_MENGE WITH ''.
        REPLACE ALL OCCURRENCES OF ',' IN WA_MB5B-OPEN_MENGE WITH ''.
        CONDENSE : WA_MB5B-WERKS,
                   WA_MB5B-MATNR,
                   WA_MB5B-FROM_DATE,
                   WA_MB5B-TO_DATE,
                   WA_MB5B-OPEN_MENGE,
                   WA_MB5B-REC_MENGE,
                   WA_MB5B-ISSU_MENGE,
                   WA_MB5B-CLOS_MENGE,
                   WA_MB5B-MEINS.
        APPEND WA_MB5B TO IT_MB5B.
        CLEAR WA_MB5B.
      ENDLOOP.
      DELETE IT_MB5B INDEX 1.
      DESCRIBE TABLE IT_MB5B LINES LINES.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_OUT-MATNR
        IMPORTING
          OUTPUT = WA_OUT-MATNR.

      LOOP AT IT_MB5B INTO WA_MB5B WHERE MATNR = WA_OUT-MATNR.
        WA_OUT-OPEN_MENGE = WA_OUT-OPEN_MENGE + WA_MB5B-OPEN_MENGE.
      ENDLOOP.

      SELECT SINGLE WGBEZ FROM T023T INTO WA_OUT-WGBEZ WHERE SPRAS = SY-LANGU AND MATKL = WA_OUT-MATKL.

      PERFORM BP_DATA.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_OUT-MATNR
        IMPORTING
          OUTPUT = WA_OUT-MATNR.

      MODIFY IT_OUT FROM WA_OUT TRANSPORTING MATNR OPEN_MENGE WGBEZ TILL_MONTH DSI
             MONTH1 MONTH2 MONTH3 MONTH4 MONTH5 MONTH6 MONTH7 MONTH8 MONTH9 MONTH10  MONTH11  MONTH12.
      CLEAR: WA_OUT .

    ENDIF.

  ENDLOOP.
ENDFORM.                    " GET_OPN_STK
*&---------------------------------------------------------------------*
*&      Form  GET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_DATE .
**  DATA: MON_LNAME TYPE T247-LTX,
**        MON_SNAME TYPE T247-KTX,
**        MONTH_NO TYPE T247-MNR.
*  data: next_year(04).
*
*  CONCATENATE S_DATUM-LOW(04) '0' S_DATUM-LOW+04(02) INTO PERIOD.
*
*  MONTH_NO = S_DATUM-LOW+04(02).
*
*  MONTH_NO_TO = MONTH_NO + 12.
*
*  next_year = S_DATUM-LOW(04) + 1.
*
*  DO 12  TIMES.
*
**    IF MONTH_NO <= 12.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = MONTH_NO
*        IMPORTING
*          OUTPUT = MONTH_NO.
*
**      CALL FUNCTION 'ISP_GET_MONTH_NAME'
**      EXPORTING
***     DATE               = S_DATUM-LOW
**        LANGUAGE           = SY-LANGU
**        MONTH_NUMBER       = MONTH_NO
**     IMPORTING
**        LANGU_BACK         = SY-LANGU
**        LONGTEXT           = MON_LNAME
**        SHORTTEXT          = MON_SNAME
***  EXCEPTIONS
***     CALENDAR_ID        = 1
***     DATE_ERROR         = 2
***     NOT_FOUND          = 3
***     WRONG_INPUT        = 4
***     OTHERS             = 5
**              .
**      IF SY-SUBRC <> 0.
*** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**      ENDIF.
*
*
*
*      CASE MONTH_NO.
*        WHEN 01.
*          concatenate 'JAN' S_DATUM-LOW(04) into MON1.
**          MON1 = 'JAN'."MON_LNAME.
*        WHEN 02.
*          concatenate 'FEB' S_DATUM-LOW(04) into MON2.
**          MON2 = 'FEB'.
*        WHEN 03.
*          concatenate 'MAR' S_DATUM-LOW(04) into MON3.
**          MON3 = 'MAR'.
*        WHEN 04.
*          concatenate 'APR' S_DATUM-LOW(04) into MON4.
**          MON4 = 'APR'.
*        WHEN 05.
*          concatenate 'MAY' S_DATUM-LOW(04) into MON5.
**          MON5 = 'MAY'.
*        WHEN 06.
*          concatenate 'JUN' S_DATUM-LOW(04) into MON6.
**          MON6 = 'JUN'.
*        WHEN 07.
*          concatenate 'JUL' S_DATUM-LOW(04) into MON7.
**          MON7 = 'JUL'.
*        WHEN 08.
*          concatenate 'AUG' S_DATUM-LOW(04) into MON8.
**          MON8 = 'AUG'.
*        WHEN 09.
*          concatenate 'SEP' S_DATUM-LOW(04) into MON9.
**          MON9 = 'SEP'.
*        WHEN 10.
*          concatenate 'OCT' S_DATUM-LOW(04) into MON10.
**          MON10 = 'OCT'.
*        WHEN 11.
*          concatenate 'NOV' S_DATUM-LOW(04) into MON11.
**          MON11 = 'NOV'.
*        WHEN 12.
*          concatenate 'DEC' S_DATUM-LOW(04) into MON12.
**          MON12 = 'DEC'.
*        WHEN 13.
*          concatenate 'JAN' next_year into MON13.
**          MON13 = 'JAN'.
*        WHEN 14.
*          concatenate 'FEB' next_year into MON14.
**          MON14 = 'FEB'.
*        WHEN 15.
*          concatenate 'MAR' next_year into MON15.
**          MON15 = 'MAR'.
*        WHEN 16.
*          concatenate 'APR' next_year into MON16.
**          MON16 = 'APR'.
*        WHEN 17.
*          concatenate 'MAY' next_year into MON17.
**          MON17 = 'MAY'.
*        WHEN 18.
*          concatenate 'JUN' next_year into MON18.
**          MON18 = 'JUN'.
*        WHEN 19.
*          concatenate 'JUL' next_year into MON19.
**          MON19 = 'JUL'.
*        WHEN 20.
*          concatenate 'AUG' next_year into MON20.
**          MON20 = 'AUG'.
*        WHEN 21.
*          concatenate 'SEP' next_year into MON21.
**          MON21 = 'SEP'.
*        WHEN 22.
*          concatenate 'OCT' next_year into MON22.
**          MON22 = 'OCT'.
*        WHEN 23.
*          concatenate 'NOV' next_year into MON23.
**          MON23 = 'NOV'.
*        WHEN 24.
*          concatenate 'DEC' next_year into MON24.
**          MON24 = 'DEC'.
*
*        WHEN OTHERS.
*      ENDCASE.
*
*      MONTH_NO = MONTH_NO + 1.
*
**    ENDIF.
*  ENDDO.
*
*
*
*
*ENDFORM.                    " GET_DATE
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

  GD_LAYOUT-COLWIDTH_OPTIMIZE  = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
             I_BYPASSING_BUFFER      = 'X'
             I_CALLBACK_PROGRAM      = SY-REPID
*            i_callback_top_of_page   = 'TOP-OF-PAGE'  "see FORM
*            I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
*            i_grid_title           = outtext
             IS_LAYOUT               = GD_LAYOUT
             IT_FIELDCAT             = FIELDCATALOG[]
*            it_special_groups       = gd_tabgroup
             IS_VARIANT               = G_VARIANT
             IT_EVENTS               = GT_EVENTS
             IS_PRINT                = GD_PRNTPARAMS
             I_SAVE                  = 'A'       "'X'                          " modified by Naren Karra on 28.10.2015
*            IT_SORT                 = IT_SORT
*            is_variant              = z_template
        TABLES
             T_OUTTAB                = IT_OUT[].

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

  FIELDCATALOG-FIELDNAME   = 'BUKRS'.
  FIELDCATALOG-SELTEXT_M   = 'Company'.
  FIELDCATALOG-COL_POS     = 0.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MATNR'.
  FIELDCATALOG-SELTEXT_M   = 'Product'.
  FIELDCATALOG-COL_POS     = 1.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MAKTX'.
  FIELDCATALOG-SELTEXT_M   = 'Description'.
  FIELDCATALOG-COL_POS     = 2.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'OPEN_MENGE'.
  FIELDCATALOG-SELTEXT_M   = 'As on Stock'.
  FIELDCATALOG-COL_POS     = 3.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MTART'.
  FIELDCATALOG-SELTEXT_M   = 'Material Type'.
  FIELDCATALOG-COL_POS     = 4.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'MATKL'.
  FIELDCATALOG-SELTEXT_M   = 'Material Grp.'.
  FIELDCATALOG-COL_POS     = 5.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'WGBEZ'.
  FIELDCATALOG-SELTEXT_M   = 'Material Grp.'.
  FIELDCATALOG-COL_POS     = 6.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'SPART'.
  FIELDCATALOG-SELTEXT_M   = 'Division'.
  FIELDCATALOG-COL_POS     = 7.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'VTWEG'.
  FIELDCATALOG-SELTEXT_M   = 'Distr.Chnl.'.
  FIELDCATALOG-COL_POS     = 8.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'TILL_MONTH '.
  FIELDCATALOG-SELTEXT_M   = 'Stock - BP'.
  FIELDCATALOG-COL_POS     = 9.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  FIELDCATALOG-FIELDNAME   = 'DSI'.
  FIELDCATALOG-SELTEXT_M   = 'DSI'.
  FIELDCATALOG-COL_POS     = 10.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  IF MON1 IS NOT INITIAL.
    FIELDCATALOG-FIELDNAME   = 'MONTH1'.
    FIELDCATALOG-SELTEXT_M   = MON1.
    FIELDCATALOG-COL_POS     = 11.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON2 IS NOT INITIAL.
    FIELDCATALOG-FIELDNAME   = 'MONTH2'.
    FIELDCATALOG-SELTEXT_M   = MON2.
    FIELDCATALOG-COL_POS     = 12.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON3 IS NOT INITIAL.
    FIELDCATALOG-FIELDNAME   = 'MONTH3'.
    FIELDCATALOG-SELTEXT_M   = MON3.
    FIELDCATALOG-COL_POS     = 13.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON4 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH4'.
    FIELDCATALOG-SELTEXT_M   = MON4.
    FIELDCATALOG-COL_POS     = 14.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON5 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH5'.
    FIELDCATALOG-SELTEXT_M   = MON5.
    FIELDCATALOG-COL_POS     = 15.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON6 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH6'.
    FIELDCATALOG-SELTEXT_M   = MON6.
    FIELDCATALOG-COL_POS     = 16.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON7 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH7'.
    FIELDCATALOG-SELTEXT_M   = MON7.
    FIELDCATALOG-COL_POS     = 17.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON8 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH8'.
    FIELDCATALOG-SELTEXT_M   = MON8.
    FIELDCATALOG-COL_POS     = 18.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON9 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH9'.
    FIELDCATALOG-SELTEXT_M   = MON9.
    FIELDCATALOG-COL_POS     = 19.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON10 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH10'.
    FIELDCATALOG-SELTEXT_M   = MON10.
    FIELDCATALOG-COL_POS     = 20.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON11 IS NOT INITIAL.

    FIELDCATALOG-FIELDNAME   = 'MONTH11'.
    FIELDCATALOG-SELTEXT_M   = MON11.
    FIELDCATALOG-COL_POS     = 21.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.

  IF MON12 IS NOT INITIAL.
    FIELDCATALOG-FIELDNAME   = 'MONTH12'.
    FIELDCATALOG-SELTEXT_M   = MON12.
    FIELDCATALOG-COL_POS     = 22.
    APPEND FIELDCATALOG TO FIELDCATALOG.
    CLEAR  FIELDCATALOG.
  ENDIF.
*
*  IF MON13 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH13'.
*    FIELDCATALOG-SELTEXT_M   = MON13.
*    FIELDCATALOG-COL_POS     = 23.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*  IF MON14 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH14'.
*    FIELDCATALOG-SELTEXT_M   = MON14.
*    FIELDCATALOG-COL_POS     = 24.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*
* IF MON15 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH15'.
*    FIELDCATALOG-SELTEXT_M   = MON15.
*    FIELDCATALOG-COL_POS     = 25.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*
*  IF MON16 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH16'.
*    FIELDCATALOG-SELTEXT_M   = MON16.
*    FIELDCATALOG-COL_POS     = 26.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*    IF MON17 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH17'.
*    FIELDCATALOG-SELTEXT_M   = MON17.
*    FIELDCATALOG-COL_POS     = 27.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*    IF MON18 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH18'.
*    FIELDCATALOG-SELTEXT_M   = MON18.
*    FIELDCATALOG-COL_POS     = 28.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*    IF MON19 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH19'.
*    FIELDCATALOG-SELTEXT_M   = MON19.
*    FIELDCATALOG-COL_POS     = 29.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*    IF MON20 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH20'.
*    FIELDCATALOG-SELTEXT_M   = MON20.
*    FIELDCATALOG-COL_POS     = 30.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*    IF MON21 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH21'.
*    FIELDCATALOG-SELTEXT_M   = MON21.
*    FIELDCATALOG-COL_POS     = 31.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*
*    IF MON22 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH22'.
*    FIELDCATALOG-SELTEXT_M   = MON22.
*    FIELDCATALOG-COL_POS     = 32.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*
*    IF MON23 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH23'.
*    FIELDCATALOG-SELTEXT_M   = MON23.
*    FIELDCATALOG-COL_POS     = 33.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*
*    IF MON24 IS NOT INITIAL.
*    FIELDCATALOG-FIELDNAME   = 'MONTH24'.
*    FIELDCATALOG-SELTEXT_M   = MON24.
*    FIELDCATALOG-COL_POS     = 34.
*    APPEND FIELDCATALOG TO FIELDCATALOG.
*    CLEAR  FIELDCATALOG.
*  ENDIF.
*
*


ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  BP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BP_DATA .
  DATA: D1(03) , D2(02) , D4(04) , D3 TYPE I .

  DATA: TOTAL TYPE MSEG-MENGE.
  DATA: DSI TYPE p decimals 0."MSEG-MENGE.
  DATA: M1 TYPE P,
        M2 TYPE P,
        M3 TYPE P,
        M4 TYPE P,
        M5 TYPE P,
        M6 TYPE P,
        M7 TYPE P,
        M8 TYPE P,
        M9 TYPE P,
        M10 TYPE P,
        M11 TYPE P,
        M12 TYPE P.


  CLEAR : IT_BP , IT_BP_FINAL , NO_OF_D ,WA_BP_FINAL , WA_BP,
         M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12 , DSI.

*{   REPLACE        SBXK900030                                        1
*\  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\    EXPORTING
*\      INPUT  = WA_OUT-MATNR
*\    IMPORTING
*\      OUTPUT = WA_OUT-MATNR.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Monday, November 13, 2018
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - CONVERSION_EXIT_ALPHA_INPUT Changes in Material
* Solution   - commenting below code as no need to convert material
*              number after converting it is checking for 40 digits
*              material code which which returns sy-subrc 4
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
  EXPORTING
    input  = wa_out-matnr
  IMPORTING
    output = wa_out-matnr.

*}   REPLACE

*  CONCATENATE S_DATUM-LOW(04) '0' S_DATUM-LOW+04(02) INTO PERIOD.

  SELECT ABSMG001 ARTNR PERBL
    FROM CE21000
    INTO CORRESPONDING FIELDS OF TABLE IT_BP
    WHERE VRGAR = 'F'
    AND PERBL >= PERIOD
    AND ARTNR = WA_OUT-MATNR.

  SORT IT_BP BY ARTNR PERBL.

  LOOP AT IT_BP INTO WA_BP.

    CONCATENATE WA_BP-PERBL(04) WA_BP-PERBL+05(02) '01' INTO WA_BP-DATE.

    COLLECT WA_BP INTO IT_BP_FINAL.

  ENDLOOP.

  CLEAR : TOTAL, NO_OF_D.

*  PERFORM GET_DATE.


  LOOP AT IT_BP_FINAL INTO WA_BP_FINAL.

    TOTAL = TOTAL + WA_BP_FINAL-ABSMG001.
*    WA_OUT-OPEN_MENGE = 1000.
    WA_OUT-TILL_MONTH = WA_OUT-OPEN_MENGE - TOTAL.


    IF WA_BP_FINAL-PERBL+05(02) = '01'.
      D3 = S_GJAHR-LOW.
      D2 = '04'.
      D1 = 'APR'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '02'.
      D3 = S_GJAHR-LOW.
      D2 = '05'.
      D1 = 'MAY'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '03'.
      D3 = S_GJAHR-LOW.
      D2 = '06'.
      D1 = 'JUN'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '04'.
      D3 = S_GJAHR-LOW.
      D2 = '07'.
      D1 = 'JUL'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '05'.
      D3 = S_GJAHR-LOW.
      D2 = '08'.
      D1 = 'AUG'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '06'.
      D3 = S_GJAHR-LOW.
      D2 = '09'.
      D1 = 'SEP'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '07'.
      D3 = S_GJAHR-LOW.
      D2 = '10'.
      D1 = 'OCT'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '08'.
      D3 = S_GJAHR-LOW.
      D2 = '11'.
      D1 = 'NOV'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '09'.
      D3 = S_GJAHR-LOW.
      D2 = '12'.
      D1 = 'DEC'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '10'.
      D3 = S_GJAHR-LOW + 1.
      D2 = '01'.
      D1 = 'JAN'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '11'.
      D3 = S_GJAHR-LOW + 1.
      D2 = '02'.
      D1 = 'FEB'.
    ELSEIF WA_BP_FINAL-PERBL+05(02) = '12'.
      D3 = S_GJAHR-LOW + 1.
      D2 = '03'.
      D1 = 'MAR'.
    ENDIF.

    D4 = D3.

    CONCATENATE D4 D2 '01' INTO WA_BP_FINAL-DATE.

    CALL FUNCTION 'HR_E_NUM_OF_DAYS_OF_MONTH'
      EXPORTING
        P_FECHA        = WA_BP_FINAL-DATE
      IMPORTING
        NUMBER_OF_DAYS = NO_OF_D.



    CASE SY-TABIX.
      WHEN 1.
        M1 = NO_OF_D.
        WA_OUT-MONTH1 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON1."WA_BP_FINAL-PERBL+05(02) WA_BP_FINAL-PERBL(04)
*        concatenate 'JAN' S_DATUM-LOW(04) into MON1.
      WHEN 2.
        M2 = NO_OF_D.
        WA_OUT-MONTH2 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON2.
      WHEN 3.
        M3 = NO_OF_D.
        WA_OUT-MONTH3 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON3.
      WHEN 4.
        M4 = NO_OF_D.
        WA_OUT-MONTH4 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON4.

      WHEN 5.
        M5 = NO_OF_D.
        WA_OUT-MONTH5 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON5.
      WHEN 6.
        M6 = NO_OF_D.
        WA_OUT-MONTH6 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON6.
      WHEN 7.
        M7 = NO_OF_D.
        WA_OUT-MONTH7 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON7.
      WHEN 8.
        M8 = NO_OF_D.
        WA_OUT-MONTH8 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON8.

      WHEN 9.
        M9 = NO_OF_D.
        WA_OUT-MONTH9 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON9.
      WHEN 10.
        M10 = NO_OF_D.
        WA_OUT-MONTH10 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON10.
      WHEN 11.
        M11 = NO_OF_D.
        WA_OUT-MONTH11 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON11.

      WHEN 12.
        M12 = NO_OF_D.
        WA_OUT-MONTH12 = WA_BP_FINAL-ABSMG001.
        CONCATENATE d1 '.' D4 INTO MON12.

      WHEN OTHERS.
    ENDCASE.

    IF WA_BP_FINAL-ABSMG001 <> 0.
      IF WA_OUT-TILL_MONTH <= 0.
        CASE SY-TABIX.
          WHEN 1.
            CLEAR M1.
          WHEN 2.
            CLEAR M2.
          WHEN 3.
            CLEAR M3.
          WHEN 4.
            CLEAR M4.
          WHEN 5.
            CLEAR M5.
          WHEN 6.
            CLEAR M6.
          WHEN 7.
            CLEAR M7.
          WHEN 8.
            CLEAR M8.
          WHEN 9.
            CLEAR M9.
          WHEN 10.
            CLEAR M10.
          WHEN 11.
            CLEAR M11.
          WHEN 12.
            CLEAR M12.
          WHEN OTHERS.
        ENDCASE.

        WA_OUT-TILL_MONTH = WA_OUT-TILL_MONTH + WA_BP_FINAL-ABSMG001.

        DSI = ( ( WA_OUT-TILL_MONTH * NO_OF_D ) / WA_BP_FINAL-ABSMG001 ) + M1 + M2 + M3 + M4 + M5
                     + M6 + M7 + M8 + M9 + M10 + M11 + M12 .
        WA_OUT-DSI = DSI.

        EXIT.
      ELSEIF WA_OUT-TILL_MONTH > 0.
*       CASE SY-TABIX.
*        WHEN 1.
*          CLEAR M1.
*        WHEN 2.
*          CLEAR M2.
*        WHEN 3.
*          CLEAR M3.
*        WHEN 4.
*          CLEAR M4.
*        WHEN 5.
*          CLEAR M5.
*        WHEN 6.
*          CLEAR M6.
*        WHEN 7.
*          CLEAR M7.
*        WHEN 8.
*          CLEAR M8.
*        WHEN 9.
*          CLEAR M9.
*        WHEN 10.
*          CLEAR M10.
*        WHEN 11.
*          CLEAR M11.
*        WHEN 12.
*          CLEAR M12.
*        WHEN OTHERS.
*      ENDCASE.


        WA_OUT-DSI = 'Excess Stock'.
*      EXIT.
      ENDIF.

    ELSEIF WA_BP_FINAL-ABSMG001 = 0.
      CASE SY-TABIX.
        WHEN 1.
          CLEAR M1.
        WHEN 2.
          CLEAR M2.
        WHEN 3.
          CLEAR M3.
        WHEN 4.
          CLEAR M4.
        WHEN 5.
          CLEAR M5.
        WHEN 6.
          CLEAR M6.
        WHEN 7.
          CLEAR M7.
        WHEN 8.
          CLEAR M8.
        WHEN 9.
          CLEAR M9.
        WHEN 10.
          CLEAR M10.
        WHEN 11.
          CLEAR M11.
        WHEN 12.
          CLEAR M12.
        WHEN OTHERS.
      ENDCASE.

*     WA_OUT-TILL_MONTH = WA_OUT-TILL_MONTH + WA_BP_FINAL-ABSMG001.

*     WA_OUT-DSI = ( ( WA_OUT-TILL_MONTH * NO_OF_D ) / 1 ) + M1 + M2 + M3 + M4 + M5
*                  + M6 + M7 + M8 + M9 + M10 + M11 + M12 .

      WA_OUT-DSI = 'Excess Stock'.
      EXIT.
    ENDIF.



  ENDLOOP.

  LOOP AT IT_BP_FINAL INTO WA_BP_FINAL.
    CASE SY-TABIX.
      WHEN 1.

        WA_OUT-MONTH1 = WA_BP_FINAL-ABSMG001.

      WHEN 2.

        WA_OUT-MONTH2 = WA_BP_FINAL-ABSMG001.

      WHEN 3.

        WA_OUT-MONTH3 = WA_BP_FINAL-ABSMG001.

      WHEN 4.

        WA_OUT-MONTH4 = WA_BP_FINAL-ABSMG001.

      WHEN 5.

        WA_OUT-MONTH5 = WA_BP_FINAL-ABSMG001.

      WHEN 6.

        WA_OUT-MONTH6 = WA_BP_FINAL-ABSMG001.

      WHEN 7.

        WA_OUT-MONTH7 = WA_BP_FINAL-ABSMG001.

      WHEN 8.

        WA_OUT-MONTH8 = WA_BP_FINAL-ABSMG001.

      WHEN 9.

        WA_OUT-MONTH9 = WA_BP_FINAL-ABSMG001.

      WHEN 10.

        WA_OUT-MONTH10 = WA_BP_FINAL-ABSMG001.

      WHEN 11.

        WA_OUT-MONTH11 = WA_BP_FINAL-ABSMG001.

      WHEN 12.

        WA_OUT-MONTH12 = WA_BP_FINAL-ABSMG001.

      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.


ENDFORM.                    " BP_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PERIOD .
  DATA: P TYPE I.
  DATA: P1(02).
  CLEAR: P, P1.
  P = S_DATUM-LOW+04(02).

* CONCATENATE S_DATUM-LOW(04) '0' S_DATUM-LOW+04(02) INTO PERIOD.
  IF P >= 4 AND P <= 12.

    P = P - 3.
    P1 = P.
    CONCATENATE  S_DATUM-LOW(04) '00' P1 INTO PERIOD.
  ELSEIF P <= 3 AND P >= 1 .

    P = P + 9.
    P1 = P.
    CONCATENATE  S_DATUM-LOW(04) '0' P1 INTO PERIOD.
  ENDIF.



ENDFORM.                    " GET_PERIOD
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AUTH .
TYPES: BEGIN OF TY_T001,
        BUKRS TYPE BUKRS,
       END OF TY_T001,
       BEGIN OF TY_TSPAT,
        SPART TYPE SPART,
       END OF TY_TSPAT,
       BEGIN OF TY_TVTWT,
        VTWEG TYPE VTWEG,
       END OF TY_TVTWT,
       BEGIN OF TY_T134,
        MTART TYPE MTART,
       END OF TY_T134.
DATA: WA_T001  TYPE TY_T001,  I_T001  TYPE STANDARD TABLE OF TY_T001,
      WA_TSPAT TYPE TY_TSPAT, I_TSPAT TYPE STANDARD TABLE OF TY_TSPAT,
      WA_TVTWT TYPE TY_TVTWT, I_TVTWT TYPE STANDARD TABLE OF TY_TVTWT,
      WA_T134  TYPE TY_T134,  I_T134  TYPE STANDARD TABLE OF TY_T134.

AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                    ID 'BUKRS' FIELD S_BUKRS-LOW
                    ID 'ACTVT' FIELD '03'.
  IF SY-SUBRC NE 0.
    MESSAGE 'Not Authorized for Company Code' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

SELECT SPART
  FROM TSPAT
  INTO TABLE I_TSPAT
  WHERE SPART IN S_SPART
  AND   SPRAS EQ SY-LANGU.

  CLEAR: GV_AUTH_SPART_FLG, S_SPART.
  REFRESH S_SPART[].
  IF I_TSPAT[] IS NOT INITIAL.
    LOOP AT I_TSPAT INTO WA_TSPAT.
      AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
                          ID 'SPART' FIELD WA_TSPAT-SPART
                          ID 'ACTVT' FIELD '03'.
      IF SY-SUBRC EQ 0.
        S_SPART-SIGN = 'I'.
        S_SPART-OPTION = 'EQ'.
        S_SPART-LOW = WA_TSPAT-SPART.
        APPEND S_SPART.
      ELSE.
        IF GV_AUTH_SPART_FLG IS INITIAL.
          GV_AUTH_SPART_FLG = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR WA_TSPAT.
  ENDIF.

  IF S_SPART[] IS INITIAL.
    S_SPART-SIGN = 'I'.
    S_SPART-OPTION = 'EQ'.
    S_SPART-LOW = ''.
    APPEND S_SPART.
  ENDIF.

  SELECT VTWEG
    FROM TVTWT
    INTO TABLE I_TVTWT
    WHERE VTWEG IN S_VTWEG
    AND   SPRAS EQ SY-LANGU.

    CLEAR: S_VTWEG, GV_AUTH_VTWEG_FLG.
    REFRESH S_VTWEG[].
    IF I_TVTWT[] IS NOT INITIAL.
      LOOP AT I_TVTWT INTO WA_TVTWT.
        AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
                            ID 'VTWEG' FIELD WA_TVTWT-VTWEG
                            ID 'ACTVT' FIELD '03'.
        IF SY-SUBRC EQ 0 .
          S_VTWEG-SIGN = 'I'.
          S_VTWEG-OPTION = 'EQ'.
          S_VTWEG-LOW = WA_TVTWT-VTWEG.
          APPEND S_VTWEG.
        ELSE.
         IF GV_AUTH_VTWEG_FLG IS INITIAL.
           GV_AUTH_VTWEG_FLG = 'X'.
         ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR WA_TVTWT.
    ENDIF.

  IF S_VTWEG[] IS INITIAL.
    S_VTWEG-SIGN = 'I'.
    S_VTWEG-OPTION = 'EQ'.
    S_VTWEG-LOW = ''.
    APPEND S_VTWEG.
  ENDIF.

  SELECT MTART
    FROM T134
    INTO TABLE I_T134
    WHERE MTART IN S_MTART.

    CLEAR:S_MTART, GV_AUTH_MTART_FLG.
    REFRESH S_MTART[].
    IF I_T134[] IS NOT INITIAL.
      LOOP AT I_T134 INTO WA_T134.
        AUTHORITY-CHECK OBJECT 'K_ML_MTART'
                            ID 'MTART' FIELD WA_T134-MTART
                            ID 'ACTVT' FIELD '03'.
        IF SY-SUBRC EQ 0.
          S_MTART-SIGN = 'I'.
          S_MTART-OPTION = 'EQ'.
          S_MTART-LOW = WA_T134-MTART.
          APPEND S_MTART.
        ELSE.
          IF GV_AUTH_MTART_FLG IS INITIAL.
            GV_AUTH_MTART_FLG = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR WA_T134.
    ENDIF.

    IF S_MTART[] IS INITIAL.
      S_MTART-SIGN = 'I'.
      S_MTART-OPTION = 'EQ'.
      S_MTART-LOW = ''.
      APPEND S_MTART.
    ENDIF.
ENDFORM.                    " CHECK_AUTH
*&---------------------------------------------------------------------*
*&      Form  PAI_OF_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PAI_OF_SELECTION-SCREEN .
PERFORM INITIAL_VARIANT.
ENDFORM.                    " PAI_OF_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*&      Form  INITIAL_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIAL_VARIANT .

CLEAR G_VARIANT.
G_SAVE = 'A'.
G_VARIANT-REPORT = REPID.
G_VARIANT-VARIANT = P_VARI.
GX_VARIANT = G_VARIANT.

CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
 EXPORTING
   I_SAVE              = G_SAVE
  CHANGING
    cs_variant          = GX_VARIANT
 EXCEPTIONS
   NOT_FOUND           = 2.
IF sy-subrc EQ 0.
  P_VARI = GX_VARIANT-VARIANT.
  G_VARIANT = GX_VARIANT.
ENDIF.

ENDFORM.                    " INITIAL_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F4_FOR_VARIANT .

CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
  EXPORTING
    is_variant               = G_VARIANT
   I_SAVE                    = G_SAVE
 IMPORTING
   E_EXIT                    = G_EXIT
   ES_VARIANT                = GX_VARIANT
 EXCEPTIONS
   NOT_FOUND                 = 1.
IF sy-subrc EQ 2.
 MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ELSE.
  IF G_EXIT EQ SPACE.
    P_VARI = GX_VARIANT-VARIANT.
  ENDIF.
ENDIF.

ENDFORM.                    " F4_FOR_VARIANT
