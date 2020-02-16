*&---------------------------------------------------------------------*
*& Report  ZMM_UPD_BOM_LOCATION_STPO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_UPD_BOM_LOCATION_STPO.
TABLES: MAST , STPO, MARA , MVKE.

DATA: BEGIN OF WA_MARA ,
        MATNR TYPE MARA-MATNR,
        WERKS TYPE MAST-WERKS,
        STLAN TYPE MAST-STLAN,
        STLNR TYPE MAST-STLNR,
        MTART TYPE MARA-MTART,
        STLKN TYPE STPO-STLKN,
*        STPOZ TYPE STPO-STPOZ,
        IDNRK TYPE STPO-IDNRK,
      END OF WA_MARA,
      IT_MARA LIKE STANDARD TABLE OF WA_MARA.
*      IT_MARA2 LIKE STANDARD TABLE OF WA_MARA.

*DATA: BEGIN OF WA_STPO,
*        STLNR TYPE STPO-STLNR,
*        STLKN TYPE STPO-STLKN,
*        IDNRK TYPE STPO-IDNRK,
*        LGORT TYPE STPO-LGORT,
*      END OF WA_STPO,
DATA: IT_STPO LIKE TABLE OF STPO, WA_STPO LIKE LINE OF IT_STPO .


SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME.

SELECT-OPTIONS : S_WERKS FOR MAST-WERKS NO INTERVALS NO-EXTENSION OBLIGATORY,
                 S_MTART FOR MARA-MTART NO INTERVALS NO-EXTENSION OBLIGATORY,
                 S_VTWEG FOR MVKE-VTWEG.

SELECT-OPTIONS:  S_STLAN FOR MAST-STLAN NO INTERVALS NO-EXTENSION,
                 S_LGORT FOR STPO-LGORT NO INTERVALS NO-EXTENSION OBLIGATORY,
                 S_MATNR FOR MAST-MATNR,
                 S_STLNR FOR MAST-STLNR,
                 S_IDNRK FOR STPO-IDNRK.

SELECTION-SCREEN END OF BLOCK BLK.

START-OF-SELECTION.

  PERFORM GET_DATA.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: LGPRO TYPE MARC-LGPRO.
  IF S_MTART IS NOT INITIAL.
    IF S_MTART-LOW = 'ZFGM'.
      SELECT B~MATNR B~WERKS B~STLAN B~STLNR A~MTART
        FROM MARA AS A
        JOIN MAST AS B
        ON A~MATNR = B~MATNR
        JOIN MVKE AS C
        ON A~MATNR = C~MATNR
        INTO TABLE IT_MARA
        WHERE A~MATNR IN S_MATNR
        AND B~WERKS IN S_WERKS
        AND B~STLAN IN S_STLAN
        AND A~MTART IN S_MTART
        AND C~VTWEG IN S_VTWEG
        AND B~STLNR IN S_STLNR.
    ELSE.
      SELECT B~MATNR B~WERKS B~STLAN B~STLNR A~MTART
        FROM MARA AS A
        JOIN MAST AS B
        ON A~MATNR = B~MATNR
*       JOIN MVKE AS C
*       ON A~MATNR = C~MATNR
        INTO TABLE IT_MARA
        WHERE A~MATNR IN S_MATNR
        AND B~WERKS IN S_WERKS
        AND B~STLAN IN S_STLAN
        AND A~MTART IN S_MTART
*       AND C~VTWEG IN S_VTWEG
        AND B~STLNR IN S_STLNR.
    ENDIF .
  ENDIF.

*  SELECT B~MATNR B~WERKS B~STLAN B~STLNR A~MTART
*    FROM MARA AS A
*    JOIN MAST AS B
*    ON A~MATNR = B~MATNR
**    JOIN MVKE AS C
**    ON A~MATNR = C~MATNR
*    INTO TABLE IT_MARA
*    WHERE A~MATNR IN S_MATNR
*    AND B~WERKS IN S_WERKS
*    AND B~STLAN IN S_STLAN
*    AND A~MTART IN S_MTART
**    AND C~VTWEG IN S_VTWEG
*    AND B~STLNR IN S_STLNR.
*
**  IF S_VTWEG IS NOT INITIAL.
**    LOOP AT IT_MARA INTO WA_MARA.
**      IF WA_MARA-MTART = 'ZFGM'.
**
**      ENDIF.
**    ENDLOOP.
**  ENDIF.


  IF IT_MARA IS NOT INITIAL.

    SELECT *
      FROM STPO
      INTO CORRESPONDING FIELDS OF TABLE IT_STPO
      FOR ALL ENTRIES IN IT_MARA
      WHERE STLNR = IT_MARA-STLNR
      AND IDNRK IN S_IDNRK.

    SORT IT_STPO BY STLNR IDNRK.

    IF IT_STPO IS NOT INITIAL.

      LOOP AT IT_STPO INTO WA_STPO.
        CLEAR: LGPRO.

        SELECT SINGLE LGPRO
          FROM MARC INTO LGPRO
          WHERE MATNR = WA_STPO-IDNRK.
        IF SY-SUBRC = 0 .
          IF LGPRO BETWEEN '1201' AND '1299'.
            WA_STPO-LGORT = LGPRO.
          ELSE .
            WA_STPO-LGORT = S_LGORT-LOW.
          ENDIF.
        ELSE.
          WA_STPO-LGORT = S_LGORT-LOW.
        ENDIF.

*        WA_STPO-LGORT = S_LGORT-LOW.
        MODIFY STPO FROM WA_STPO.
        IF SY-SUBRC = 0 .
          COMMIT WORK.
          FORMAT COLOR 5 INTENSIFIED OFF.
          WRITE : /'STORAGE LOCATION Updated for BOM: ' , WA_STPO-STLNR , 'Component :' , WA_STPO-IDNRK.
        ELSE.
          FORMAT COLOR 6 INTENSIFIED OFF.
          WRITE : /'Error while Updating STORAGE LOCATION for BOM: ' , WA_STPO-STLNR , 'Component :' , WA_STPO-IDNRK.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDIF.
ENDFORM.                    " GET_DATA
