*&---------------------------------------------------------------------*
*& Report  Z6MM013R_WMS_INVENTORY_STOCK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION:
* OBJECT TYPE       :                 FUNC. CONSULTANT  :
*          DEVELOPER:SUPRIYA
*      CREATION DATE:   07.09.2010
*        DEV REQUEST:
*  TCODE            :Z6MM013_INV
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

REPORT  Z6MM013R_WMS_INVENTORY_STOCK.

TABLES:MARA,
       Z6MMA_WMS_CM,
       Z6MMA_WMS_CM_HIS.

DATA : BEGIN OF WA_MARA,
       MATNR TYPE MARA-MATNR,
       END OF WA_MARA.

DATA : BEGIN OF WA_MAKT,
       MATNR TYPE MAKT-MATNR,
       MAKTX TYPE MAKT-MAKTX,
       END OF WA_MAKT.

DATA :BEGIN OF WA_MARD,
      MATNR TYPE MARD-MATNR,
      WERKS TYPE MARD-WERKS,
      LGORT TYPE MARD-LGORT,
      LABST TYPE MARD-LABST,
      END OF WA_MARD.

DATA : BEGIN OF WA_MARM,
       MATNR TYPE MARM-MATNR,
       MEINH TYPE MARM-MEINH,
       UMREZ TYPE MARM-UMREZ,
       UMREN TYPE MARM-UMREN,
       END OF WA_MARM.

DATA: WA_Z6MMA_WMS_INVT   TYPE Z6MMA_WMS_INVT.
*      WA_Z6MMA_WMS_INV_HI TYPE Z6MMA_WMS_INV_HI.

DATA: IT_Z6MMA_WMS_INVT   LIKE STANDARD TABLE OF WA_Z6MMA_WMS_INVT,
*      IT_Z6MMA_WMS_INV_HI LIKE STANDARD TABLE OF WA_Z6MMA_WMS_INV_HI,
      IT_MARD             LIKE STANDARD TABLE OF WA_MARD,
      IT_MARM             LIKE STANDARD TABLE OF WA_MARM,
      IT_MARA             LIKE STANDARD TABLE OF WA_MARA,
      IT_MAKT             LIKE STANDARD TABLE OF WA_MAKT.
**---------------------------------------------------------------------*
*       START OF SELECTION.
**---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM CHK_HISTORY.
  PERFORM GET_DATA.
  PERFORM UPDATE_TABLE.
*&---------------------------------------------------------------------*
*&      Form  CHK_HISTORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_HISTORY .

*  SELECT ITEM_CODE
*         CONTAINER INTO TABLE IT_Z6MMA_WMS_INV_HI FROM  Z6MMA_WMS_MM_HIS.
*  IF SY-SUBRC = 0.
*    SORT IT_Z6MMA_WMS_INV_HI BY ITEM_CODE
*                                CONTAINER .
*  ENDIF.

ENDFORM.                    " CHK_HISTORY
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  SELECT MATNR
       INTO TABLE IT_MARA FROM MARA
       WHERE LABOR = '011'.
  IF SY-SUBRC = 0.
    SORT IT_MARA.
  ENDIF.

  IF NOT IT_MARA IS INITIAL.

    SELECT MATNR
           MEINH
           UMREZ
           UMREN FROM MARM INTO TABLE IT_MARM
           FOR ALL ENTRIES IN IT_MARA
           WHERE MATNR = IT_MARA-MATNR.
    IF SY-SUBRC = 0.
      SORT IT_MARM BY MATNR.
    ENDIF.

    SELECT MATNR
           WERKS
           LGORT
           LABST FROM MARD INTO TABLE IT_MARD
           FOR ALL ENTRIES IN IT_MARA
           WHERE MATNR =  IT_MARA-MATNR
           AND   WERKS = '2101'
           AND   LGORT = '1502'."'1501'.
*           AND   LGORT NE ''.
    IF SY-SUBRC = 0.
      SORT IT_MARD BY MATNR.
    ENDIF.
"Start of Anees
    SELECT MATNR
           MAKTX FROM MAKT INTO TABLE IT_MAKT
           FOR ALL ENTRIES IN IT_MARA
           WHERE MATNR =  IT_MARA-MATNR
           AND   spras = sy-langu.
    IF SY-SUBRC = 0.
      SORT IT_MAKT BY MATNR.
    ENDIF.
"End of Anees
  ENDIF.


  LOOP AT IT_MARD INTO WA_MARD.
*    READ TABLE IT_Z6MMA_WMS_INV_HI INTO WA_Z6MMA_WMS_INV_HI WITH KEY ITEM_CODE = WA_MARD-MATNR
*                                                                     BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      CLEAR WA_Z6MMA_WMS_INVT.
*    ELSE.
      WA_Z6MMA_WMS_INVT-ITEM_CODE = WA_MARD-MATNR.
      WA_Z6MMA_WMS_INVT-LABST     = WA_MARD-LABST.
      WA_Z6MMA_WMS_INVT-lgort     = '1502'."pravin aded 17/02/16
*    ENDIF.
    COLLECT WA_Z6MMA_WMS_INVT INTO IT_Z6MMA_WMS_INVT.
  ENDLOOP.

  LOOP AT IT_Z6MMA_WMS_INVT INTO WA_Z6MMA_WMS_INVT.
    CLEAR WA_MARM.
    READ TABLE IT_MARM INTO WA_MARM WITH KEY MATNR = WA_Z6MMA_WMS_INVT-ITEM_CODE
                                             MEINH = 'EA'
                                             BINARY SEARCH.
    IF SY-SUBRC = 0.
*      IF WA_MARM-UMREN NE WA_MARM-UMREZ.
        SELECT SINGLE * FROM Z6MMA_WMS_CM_HIS
          WHERE CONTAINER_WT  = WA_MARM-UMREZ.
        IF SY-SUBRC = 0.
          WA_Z6MMA_WMS_INVT-CONTAINER = Z6MMA_WMS_CM_HIS-CONTAINER.
        ENDIF.
*      ENDIF.
    ENDIF.

    CLEAR WA_MAKT.
    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_Z6MMA_WMS_INVT-ITEM_CODE
    BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_Z6MMA_WMS_INVT-DESCRIPTION =  WA_MAKT-MAKTX.
    ENDIF.

    MODIFY IT_Z6MMA_WMS_INVT FROM WA_Z6MMA_WMS_INVT TRANSPORTING CONTAINER DESCRIPTION.

*    MOVE-CORRESPONDING WA_Z6MMA_WMS_INVT  TO WA_Z6MMA_WMS_INV_HI.
*    WA_Z6MMA_WMS_INV_HI-UNAME = SY-UNAME.
*    WA_Z6MMA_WMS_INV_HI-ERDAT = SY-DATUM.
*    WA_Z6MMA_WMS_INV_HI-UZEIT = SY-TIMLO.
*    APPEND WA_Z6MMA_WMS_INV_HI TO IT_Z6MMA_WMS_INV_HI.
  ENDLOOP.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE .

  IF NOT IT_Z6MMA_WMS_INVT IS INITIAL.
    MODIFY Z6MMA_WMS_INVT FROM TABLE IT_Z6MMA_WMS_INVT.
    IF sy-subrc = 0.
      MESSAGE 'Update of WMS_INVT is Successful' TYPE 'S'.
    ENDIF.
  ENDIF.

*  IF NOT IT_Z6MMA_WMS_INV_HI IS INITIAL.
*    MODIFY Z6MMA_WMS_INV_HI FROM TABLE IT_Z6MMA_WMS_INV_HI.
*  ENDIF.
ENDFORM.                    " UPDATE_TABLE
