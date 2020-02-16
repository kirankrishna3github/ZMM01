FUNCTION ZGET_OPENING_STOCK.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(WRK_MATNR) TYPE  MSEG-MATNR
*"     REFERENCE(WRK_WERKS) TYPE  MSEG-WERKS
*"     REFERENCE(WRK_DATE) TYPE  MKPF-BUDAT
*"     REFERENCE(WRK_LGBST) TYPE  AM07M-LGBST DEFAULT 'X'
*"     REFERENCE(WRK_BWBST) TYPE  AM07M-BWBST DEFAULT ' '
*"     REFERENCE(WA_LGORT) TYPE  MSEG-LGORT
*"  EXPORTING
*"     REFERENCE(ANFMENGE) TYPE  P
*"     REFERENCE(ENDMENGE) TYPE  P
*"     REFERENCE(SOLL) TYPE  P
*"     REFERENCE(HABEN) TYPE  P
*"  EXCEPTIONS
*"      MANDATORY
*"----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*---<< S/4HANA >>---*
*&---------------------------------------------------------------------*
* Changed On - Tuesday, October 16, 2018 12:30:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - The client field "MANDT" cannot be specified in the ON condition.
* Solution   - Comment MANDT from Select Query
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*&---------------------------------------------------------------------*

  DATA: BEGIN OF WEG_MAT OCCURS 100,
          WERKS LIKE MSEG-WERKS,
          LGORT LIKE MSEG-LGORT,
          MATNR LIKE MSEG-MATNR,
          SHKZG LIKE MSEG-SHKZG,
          MENGE(09) TYPE P DECIMALS 3,
        END OF WEG_MAT.

  DATA : G_F_DCOBJDEF_NAME     LIKE DCOBJDEF-NAME,
         G_FLAG_IS_OIL_ACTIVE(01)        TYPE C,
         G_CNT_IS_OIL          TYPE I,
         OIGLCALC(01)         TYPE  C,
         OIGLSKU(07)          TYPE  P  DECIMALS 3.

  DATA: BEGIN OF MAT_WEG OCCURS 100,
       BWKEY LIKE MBEW-BWKEY,
       WERKS LIKE MSEG-WERKS,
       MATNR LIKE MSEG-MATNR,
       SHKZG LIKE MSEG-SHKZG,
       MENGE(09) TYPE P DECIMALS 3,
      DMBTR(09)            TYPE P    DECIMALS 3,
     END OF MAT_WEG.

  DATA: BEGIN OF MAT_SUM_BUK OCCURS 100,
          BWKEY LIKE MBEW-BWKEY,
          MATNR LIKE MSEG-MATNR,
          SHKZG LIKE MSEG-SHKZG,
          MENGE(09) TYPE P DECIMALS 3,
          DMBTR(09)            TYPE P    DECIMALS 3,
        END OF MAT_SUM_BUK.

  TYPES :BEGIN OF STYPE_MSEG_LEAN,
         MBLNR             LIKE      MKPF-MBLNR,
         MJAHR             LIKE      MKPF-MJAHR,
         BLART             LIKE      MKPF-BLART,
         BUDAT             LIKE      MKPF-BUDAT,
         MATNR             LIKE      MSEG-MATNR,
         WERKS             LIKE      MSEG-WERKS,
         SOBKZ             LIKE      MSEG-SOBKZ,
         SHKZG             LIKE      MSEG-SHKZG,
         MENGE             LIKE      MSEG-MENGE,
         MEINS             LIKE      MSEG-MEINS,
         XAUTO             LIKE      MSEG-XAUTO,
         BUSTM             LIKE      MSEG-BUSTM,
         LGORT             LIKE      MSEG-LGORT,
         OIGLCALC(01)      TYPE      C,
         OIGLSKU(07)       TYPE      P  DECIMALS 3,
         INSMK             LIKE      MSEG-INSMK,
         RETAIL(01)        TYPE C,                          "n497992
         BUSTW             LIKE      MSEG-BUSTW,
         ZEILE             LIKE      MSEG-ZEILE,
         KZVBR                 LIKE    MSEG-KZVBR ,
         KZBEW             LIKE      MSEG-KZBEW,


END OF STYPE_MSEG_LEAN.

  DATA: BEGIN OF SUM_MAT OCCURS 100,
          WERKS LIKE MSEG-WERKS,
          MATNR LIKE MSEG-MATNR,
          SHKZG LIKE MSEG-SHKZG,
          MENGE(09) TYPE P DECIMALS 3,
        END OF SUM_MAT.


  TYPES: STAB_MSEG_LEAN TYPE STANDARD TABLE OF STYPE_MSEG_LEAN.


  DATA: BEGIN OF BESTAND OCCURS 100,
          BWKEY LIKE MBEW-BWKEY,
          WERKS LIKE MSEG-WERKS,
          MATNR LIKE MSEG-MATNR,
          CHARG LIKE MSEG-CHARG,

          ENDMENGE(09) TYPE P DECIMALS 3,
          ANFMENGE(09) TYPE P DECIMALS 3,
          MEINS LIKE MARA-MEINS,
          ENDWERT(09)          TYPE P    DECIMALS 2,
          ANFWERT(09)          TYPE P    DECIMALS 2,

          SOLL(09) TYPE P DECIMALS 3,
          HABEN(09) TYPE P DECIMALS 3,
          SOLLWERT(09)         TYPE P    DECIMALS 2,
          HABENWERT(09)        TYPE P    DECIMALS 2,
          WAERS LIKE T001-WAERS,
        END OF BESTAND.

  DATA: BEGIN OF IMARA OCCURS 100,
          MATNR LIKE MARA-MATNR,
          MEINS LIKE MARA-MEINS,
          MTART LIKE MARA-MTART,
        END OF IMARA.

  DATA: BEGIN OF IT134M OCCURS 100,
          BWKEY LIKE T134M-BWKEY,
          MTART LIKE T134M-MTART,
          MENGU LIKE T134M-MENGU,
          WERTU LIKE T134M-WERTU,
        END OF IT134M.


  DATA:  G_T_MSEG_LEAN         TYPE    STAB_MSEG_LEAN,
         G_S_MSEG_LEAN         TYPE    STYPE_MSEG_LEAN,
         WERKS                 LIKE    MSEG-WERKS,
         AKTDAT                LIKE    SY-DATLO,
         L_F_BWKEY             LIKE      T001K-BWKEY.

  RANGES: G_RA_LGORT          FOR       MSEG-LGORT,   "storage location
          WRK_LGORT           FOR       MSEG-LGORT,
          G_RA_XAUTO          FOR       MSEG-XAUTO,
          G_RA_BWKEY          FOR       T001K-BWKEY,   "valuation area
          G_0000_RA_BWKEY     FOR       T001K-BWKEY  .


  DATA:   BEGIN OF IMARD OCCURS 100,
           WERKS TYPE MARD-WERKS,
           MATNR TYPE MARD-MATNR,
           LGORT TYPE MARD-LGORT,
           LABST TYPE MARD-LABST,
           UMLME TYPE MARD-UMLME,
           EINME TYPE MARD-EINME,
           SPEME TYPE MARD-SPEME,
           INSME LIKE MARD-INSME,
           RETME TYPE MARD-RETME,
           KLABS TYPE MARD-KLABS,
           LBKUM TYPE MBEW-LBKUM,
           SALK3(09) TYPE P    DECIMALS 2,
           WAERS LIKE T001-WAERS,
         END OF IMARD.

  DATA: BEGIN OF MAT_SUM OCCURS 100,
          BWKEY LIKE MBEW-BWKEY,
          WERKS LIKE MSEG-WERKS,
          MATNR LIKE MSEG-MATNR,
          SHKZG LIKE MSEG-SHKZG,
         MENGE(09) TYPE P DECIMALS 3,
          DMBTR(09) TYPE P DECIMALS 3,
        END OF MAT_SUM.

  TYPES : BEGIN OF STYPE_MSEG_XAUTO,
             MBLNR             LIKE  MSEG-MBLNR,
             MJAHR             LIKE  MSEG-MJAHR,
             ZEILE             LIKE  MSEG-ZEILE,
             MATNR             LIKE  MSEG-MATNR,
             XAUTO             LIKE  MSEG-XAUTO,
          END OF STYPE_MSEG_XAUTO,

          STAB_MSEG_XAUTO      TYPE STANDARD TABLE OF
                               STYPE_MSEG_XAUTO
                               WITH DEFAULT KEY.

* working area for the previous entry
  DATA : G_S_MSEG_PR           TYPE  STYPE_MSEG_XAUTO,

* table for the original MM doc posting lines
         G_S_MSEG_OR           TYPE  STYPE_MSEG_XAUTO,
         G_T_MSEG_OR           TYPE  STAB_MSEG_XAUTO.

* definition of working area for valuation tables improved
  TYPES : BEGIN OF STYPE_MBEW,
            MATNR              LIKE      MBEW-MATNR,
            BWKEY              LIKE      MBEW-BWKEY,
            BWTAR              LIKE      MBEW-BWTAR,
            LBKUM(09)          TYPE P    DECIMALS 3,
            SALK3(09)          TYPE P    DECIMALS 2,
            MEINS              LIKE      MARA-MEINS,
            WAERS              LIKE      T001-WAERS,
          END OF STYPE_MBEW,

          STAB_MBEW               TYPE STANDARD TABLE OF
                                  STYPE_MBEW WITH DEFAULT KEY.

  DATA: G_S_MBEW                  TYPE  STYPE_MBEW,
        G_T_MBEW                  TYPE  STAB_MBEW,
        G_CUST_TIED_EMPTIES(01)   TYPE C    VALUE ' ',
        G_FLAG_DELETE(01)         TYPE C.

  DATA   : G_F_ZEILE           LIKE  MSEG-ZEILE.

  TYPES : BEGIN OF STYPE_ORGAN,
            KEYTYPE(01)        TYPE C,
            KEYFIELD           LIKE      T001W-WERKS,
            BWKEY              LIKE      T001K-BWKEY,
            WERKS              LIKE      T001W-WERKS,
            BUKRS              LIKE      T001-BUKRS,
            WAERS              LIKE      T001-WAERS,
          END OF STYPE_ORGAN,

          STAB_ORGAN           TYPE STANDARD TABLE OF STYPE_ORGAN
                               WITH KEY KEYTYPE KEYFIELD BWKEY WERKS.

  DATA : G_S_ORGAN             TYPE      STYPE_ORGAN,
         G_T_ORGAN             TYPE      STAB_ORGAN
                                         WITH HEADER LINE.



  DATA: BEGIN OF MAT_WEG_BUK OCCURS 100,
          BWKEY LIKE MBEW-BWKEY,
          MATNR LIKE MSEG-MATNR,
          SHKZG LIKE MSEG-SHKZG,
          MENGE(09) TYPE P DECIMALS 3,
          DMBTR(09)            TYPE P    DECIMALS 3,
        END OF MAT_WEG_BUK.
  DATA: BEGIN OF IT156W OCCURS 100,
          BUSTW LIKE T156W-BUSTW,
          XBGBB LIKE T156W-XBGBB,
        END OF IT156W.


  DATA:   BEGIN OF IMSWEG OCCURS 1000,
          MBLNR LIKE MSEG-MBLNR,
          MJAHR LIKE MSEG-MJAHR,
          ZEILE LIKE MSEG-ZEILE,
          MATNR LIKE MSEG-MATNR,
          CHARG LIKE MSEG-CHARG,
          BWTAR LIKE MSEG-BWTAR,
          WERKS LIKE MSEG-WERKS,
          LGORT LIKE MSEG-LGORT,
          SOBKZ LIKE MSEG-SOBKZ,
          BWART LIKE MSEG-BWART,
          SHKZG LIKE MSEG-SHKZG,
          XAUTO LIKE MSEG-XAUTO,
          MENGE LIKE MSEG-MENGE,
          MEINS LIKE MSEG-MEINS,
          DMBTR LIKE MSEG-DMBTR,
          DMBUM LIKE MSEG-DMBUM,
          BUSTM LIKE MSEG-BUSTM,
          BUSTW LIKE MSEG-BUSTW,
          OIGLCALC(01) TYPE  C,
          OIGLSKU(07)  TYPE  P  DECIMALS 3,
          INSMK        LIKE  MSEG-INSMK,
          END OF IMSWEG.


  IF WRK_MATNR IS INITIAL OR WRK_DATE IS INITIAL OR WRK_WERKS IS INITIAL.
    MESSAGE S289(M7) RAISING MANDATORY  .
  ENDIF.
***********************************************************************
* change of storage location property depending up on the lgbst & bwbst.
* lgbst  = storage location / batch stock.
* bwbst  = valuated stock.
***********************************************************************
  REFRESH                    G_RA_LGORT.
  CLEAR                      G_RA_LGORT.

*  IF    wrk_lgbst = 'X'.                "only pericular Storage loc./batch stock
*    MOVE wa_lgort TO  g_ra_lgort.
  IF WA_LGORT IS NOT INITIAL.
    MOVE : WA_LGORT TO  G_RA_LGORT-LOW,
           'I'               TO  G_RA_LGORT-SIGN,
           'EQ'              TO  G_RA_LGORT-OPTION.
    APPEND                   G_RA_LGORT.
  ELSE.
    IF    WRK_LGBST = 'X'.                "only Storage loc./batch stock
      MOVE WRK_LGORT[]               TO  G_RA_LGORT[].

      MOVE : 'E'               TO  G_RA_LGORT-SIGN,
             'EQ'              TO  G_RA_LGORT-OPTION.
      APPEND                   G_RA_LGORT.
    ELSEIF  WRK_BWBST = 'X'.       "only valuated stocks
*   copy the existing select-options
      MOVE WRK_LGORT[]               TO  G_RA_LGORT[].

    ENDIF.


  ENDIF.

*  if wrk_bwkey is not initial.
*    MOVE : wrk_bwkey TO  G_RA_BWKEY-LOW,
*           'I'               TO  G_RA_BWKEY-SIGN,
*           'EQ'              TO  G_RA_BWKEY-OPTION.
*    APPEND                   G_RA_BWKEY.
*
*  endif.
*  ELSEIF  wrk_bwbst = 'X'.       "only valuated stocks
**   copy the existing select-options
*    MOVE wrk_lgort[]               TO  g_ra_lgort[].
*
*  ENDIF.

*---------------------------------------------------------------------
* To find postings with valuation string, but without relevance for
* the valuated stock.
* Take lines from MSEG where for the combination BUSTW/XAUTO=XBGBB
* there is an entry in T156W with key BSX.
*---------------------------------------------------------------------
  SELECT BUSTW XBGBB FROM T156W
                     INTO CORRESPONDING FIELDS OF TABLE IT156W
                     WHERE VORSL = 'BSX'.
  SORT IT156W BY BUSTW XBGBB.
  DELETE ADJACENT DUPLICATES FROM IT156W.
  DELETE IT156W WHERE BUSTW = SPACE.


  IF WRK_LGBST = 'X'.

    IF WA_LGORT IS   NOT INITIAL.
      SELECT * FROM MARD INTO CORRESPONDING FIELDS OF TABLE IMARD
                                               WHERE  MATNR = WRK_MATNR
                                               AND     WERKS = WRK_WERKS AND LGORT = WA_LGORT .
    ELSE.
      SELECT * FROM MARD INTO CORRESPONDING FIELDS OF TABLE IMARD
                                             WHERE  MATNR = WRK_MATNR
                                             AND     WERKS = WRK_WERKS.
    ENDIF.

    IF SY-SUBRC <> 0.
      MESSAGE S289(M7).
    ENDIF.


  ELSEIF WRK_BWBST = 'X'.

* define local working areas  / for the result of the
* database selections and the control break
    DATA : L_T_MBEW              TYPE  STAB_MBEW,
           L_S_MBEW              TYPE  STYPE_MBEW,

           L_S_MBEW_SPLIT        TYPE  STYPE_MBEW,
           L_S_MBEW_NORMAL       TYPE  STYPE_MBEW,
           L_FLAG_SPLIT(01)      TYPE C.
* read the matching valuation entries
    SELECT MATNR BWKEY BWTAR LBKUM SALK3  FROM MBEW
           INTO CORRESPONDING FIELDS OF TABLE L_T_MBEW
           WHERE  MATNR  = WRK_MATNR
             AND  BWKEY  IN  G_RA_BWKEY .

    IF SY-SUBRC NE 0.
      MESSAGE S289(M7).
    ENDIF.

* read the matching valuation records of the valuated
* special stock sales order
    SELECT MATNR BWKEY BWTAR
           SUM( LBKUM ) AS LBKUM
           SUM( SALK3 ) AS SALK3        FROM  EBEW
           APPENDING CORRESPONDING FIELDS OF TABLE L_T_MBEW
          WHERE  MATNR  =  WRK_MATNR
             AND  BWKEY  IN  G_RA_BWKEY
*           AND  BWTAR  IN  BWTAR
           GROUP BY  MATNR  BWKEY BWTAR.

* read the matching valuation records of the valuated
* special stock projects
    SELECT MATNR BWKEY BWTAR
           SUM( LBKUM ) AS LBKUM
           SUM( SALK3 ) AS SALK3        FROM  QBEW
           APPENDING CORRESPONDING FIELDS OF TABLE L_T_MBEW
           WHERE  MATNR  = WRK_MATNR
             AND  BWKEY  IN  G_RA_BWKEY
*           AND  BWTAR  IN  BWTAR
           GROUP BY  MATNR  BWKEY BWTAR.

* read the matching valuation records of the valuated
* special subcontractor stock OBEW
    SELECT MATNR BWKEY BWTAR
           SUM( LBKUM ) AS LBKUM
           SUM( SALK3 ) AS SALK3         FROM  OBEW
           APPENDING CORRESPONDING FIELDS OF TABLE L_T_MBEW
           WHERE  MATNR  = WRK_MATNR
             AND  BWKEY  IN  G_RA_BWKEY
*           AND  BWTAR  IN  BWTAR
           GROUP BY  MATNR  BWKEY BWTAR.

* create table g_t_organ if it is still empty
    IF  G_T_ORGAN[] IS INITIAL.
*   create working table G_0000_RA_BWKEY with the valuation areas
      LOOP AT L_T_MBEW         INTO  L_S_MBEW.
        ON CHANGE OF L_S_MBEW-BWKEY.
          MOVE : L_S_MBEW-BWKEY
                               TO  G_0000_RA_BWKEY-LOW,
                 'I'           TO  G_0000_RA_BWKEY-SIGN,
                 'EQ'          TO  G_0000_RA_BWKEY-OPTION.
          COLLECT              G_0000_RA_BWKEY.
        ENDON.
      ENDLOOP.


    ENDIF.

    SORT  L_T_MBEW             BY  MATNR  BWKEY.

    LOOP AT L_T_MBEW           INTO  L_S_MBEW.
*   process a single entry / add the stock and value
      IF  L_S_MBEW-BWTAR IS INITIAL.
        MOVE : L_S_MBEW-MATNR  TO  L_S_MBEW_NORMAL-MATNR,
               L_S_MBEW-BWKEY  TO  L_S_MBEW_NORMAL-BWKEY.
        ADD :  L_S_MBEW-LBKUM  TO  L_S_MBEW_NORMAL-LBKUM,
               L_S_MBEW-SALK3  TO  L_S_MBEW_NORMAL-SALK3.
      ELSE.
*     material has split valuation
        MOVE : 'X'             TO  L_FLAG_SPLIT,
               L_S_MBEW-MATNR  TO  L_S_MBEW_SPLIT-MATNR,
               L_S_MBEW-BWKEY  TO  L_S_MBEW_SPLIT-BWKEY.
        ADD :  L_S_MBEW-LBKUM  TO  L_S_MBEW_SPLIT-LBKUM,
               L_S_MBEW-SALK3  TO  L_S_MBEW_SPLIT-SALK3.
      ENDIF.

*   control break after material and valuation area
      AT END OF BWKEY.
*     create a entry for the next working table
        IF  L_FLAG_SPLIT = 'X'.
*       if the material has split valuation, take only
*       the sums from the entries with valuation type
          MOVE-CORRESPONDING  L_S_MBEW_SPLIT  TO  G_S_MBEW.
        ELSE.
          MOVE-CORRESPONDING  L_S_MBEW_NORMAL TO  G_S_MBEW.
        ENDIF.
      ENDAT.
      APPEND  G_S_MBEW     TO  G_T_MBEW.
    ENDLOOP.
  ENDIF.

  SELECT
      MKPF~MBLNR MKPF~MJAHR MKPF~BLART MKPF~BUDAT MSEG~MATNR
      MSEG~MENGE MSEG~SHKZG MSEG~LGORT MSEG~SOBKZ MSEG~WERKS
      MSEG~XAUTO MSEG~BUSTW MSEG~BUSTM
        INTO CORRESPONDING FIELDS OF TABLE G_T_MSEG_LEAN
        FROM MKPF AS MKPF  JOIN MSEG AS MSEG
**                   ON MKPF~MANDT  =  MSEG~MANDT  AND
                   ON MKPF~MBLNR  =  MSEG~MBLNR  AND
                   MKPF~MJAHR  =  MSEG~MJAHR
        WHERE MSEG~MATNR  =  WRK_MATNR
        AND MSEG~WERKS = WRK_WERKS
        AND MSEG~LGORT IN G_RA_LGORT
        AND MKPF~BUDAT  GE  WRK_DATE ."and mseg~bukrs in G_RA_BWKEY .
*  IF  sy-subrc  <>  0.
**   no material documents found
*    MESSAGE s842(m7). " RAISING MANDATORY .
*  ENDIF.
*

  IF WRK_BWBST = 'X'.
    LOOP AT G_T_MSEG_LEAN      INTO  G_S_MSEG_LEAN
                                 WHERE  KZVBR <> SPACE
                                   AND ( KZBEW = 'B' OR KZBEW = 'F' ).

*   get the valuation area

      MOVE  G_S_MSEG_LEAN-WERKS   TO  L_F_BWKEY.


      READ TABLE IMARA WITH KEY MATNR = G_S_MSEG_LEAN-MATNR
                               BINARY SEARCH.

      IF  SY-SUBRC IS INITIAL.
        READ TABLE IT134M      WITH KEY  BWKEY = L_F_BWKEY
                                         MTART = IMARA-MTART
                               BINARY SEARCH.

        IF  SY-SUBRC IS INITIAL.
          IF NOT IT134M-MENGU IS INITIAL AND
             NOT IT134M-WERTU IS INITIAL.
            DELETE              G_T_MSEG_LEAN.
          ENDIF.
        ENDIF.
      ELSE.
        DELETE                  G_T_MSEG_LEAN.
      ENDIF.
    ENDLOOP.


* Eliminate material documents with valuation string, but without
* relevance to the valuated stock.

    LOOP AT G_T_MSEG_LEAN      INTO  G_S_MSEG_LEAN.
      IF  NOT G_CUST_TIED_EMPTIES IS INITIAL.

*     look for MM documents with xauto = L and change
*     indicators

        CASE  G_S_MSEG_LEAN-XAUTO.
          WHEN  'X'.
          WHEN  SPACE.

          WHEN  OTHERS.

*  range table g_ra_xauto contains the special
* indicators for the transfer movements of the
*  tied empties
            IF  G_S_MSEG_LEAN-XAUTO IN G_RA_XAUTO.
              MOVE  G_S_MSEG_LEAN-XAUTO
                               TO  G_S_MSEG_LEAN-RETAIL.
              CLEAR              G_S_MSEG_LEAN-XAUTO.
              MODIFY  G_T_MSEG_LEAN    FROM  G_S_MSEG_LEAN
                               TRANSPORTING XAUTO RETAIL.
            ENDIF.
        ENDCASE.
      ENDIF.

      READ TABLE IT156W        WITH KEY
                               BUSTW = G_S_MSEG_LEAN-BUSTW
                               XBGBB = G_S_MSEG_LEAN-XAUTO
                               TRANSPORTING NO FIELDS
                               BINARY SEARCH.
      IF SY-SUBRC <> 0.
        DELETE                 G_T_MSEG_LEAN.
      ENDIF.
    ENDLOOP.
  ENDIF.


*  screening the dates which is greater than budat

  AKTDAT = SY-DATLO + 30.
  IF NOT ( WRK_DATE IS INITIAL OR WRK_DATE > AKTDAT ).
    LOOP AT G_T_MSEG_LEAN    INTO  G_S_MSEG_LEAN
    WHERE BUDAT > WRK_DATE.
      MOVE-CORRESPONDING G_S_MSEG_LEAN TO IMSWEG.
      APPEND IMSWEG.
      DELETE   G_T_MSEG_LEAN.
    ENDLOOP.
  ENDIF.

*storageLocation/batchstock is 'X'

  IF NOT WRK_LGBST IS INITIAL.
    SORT IMSWEG BY WERKS MATNR SHKZG.
    LOOP AT IMSWEG.
      IF ( IMSWEG-XAUTO IS INITIAL ) OR
         ( IMSWEG-BUSTM <> 'MA02' AND IMSWEG-BUSTM <> 'MA05' ).
        MOVE-CORRESPONDING IMSWEG TO WEG_MAT.
        COLLECT WEG_MAT.
      ELSE.
        DELETE IMSWEG.
      ENDIF.
    ENDLOOP.

*sumation of quantity based on 'S' and 'H' indicators for valuated
*stock

  ELSEIF WRK_BWBST = 'X'.
    SORT IMSWEG BY WERKS MATNR SHKZG.
    LOOP AT IMSWEG.
      IF  G_FLAG_IS_OIL_ACTIVE = 'X'.
        IF ( IMSWEG-BUSTM <> 'MEU1' )    OR
           ( IMSWEG-BUSTM = 'MEU1'
             AND NOT IMSWEG-OIGLCALC IS INITIAL
             AND NOT IMSWEG-OIGLSKU IS INITIAL ).
          MOVE-CORRESPONDING IMSWEG TO MAT_WEG.
          COLLECT MAT_WEG.
        ELSE.
          DELETE           IMSWEG.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING IMSWEG TO MAT_WEG.
        COLLECT MAT_WEG.
      ENDIF.

    ENDLOOP.

    LOOP AT MAT_WEG.

      MAT_WEG-BWKEY = MAT_WEG-WERKS.
      MODIFY MAT_WEG.
    ENDLOOP.

  ENDIF.

*sumation of quantity based on 'S' and 'H' indicators for local/batch
*stock
  IF NOT WRK_LGBST IS INITIAL.

    SORT  G_T_MSEG_LEAN    BY WERKS MATNR SHKZG DESCENDING.

    LOOP AT G_T_MSEG_LEAN  INTO  G_S_MSEG_LEAN.
      IF ( G_S_MSEG_LEAN-XAUTO IS INITIAL ) OR
         ( G_S_MSEG_LEAN-BUSTM <> 'MA02' AND
           G_S_MSEG_LEAN-BUSTM <> 'MA05' ).
        MOVE-CORRESPONDING G_S_MSEG_LEAN   TO  SUM_MAT.
        COLLECT            SUM_MAT.
      ELSE.
        DELETE             G_T_MSEG_LEAN.
      ENDIF.
    ENDLOOP.

*

  ELSEIF WRK_BWBST = 'X'.
    SORT  G_T_MSEG_LEAN      BY WERKS MATNR SHKZG DESCENDING.
    LOOP AT G_T_MSEG_LEAN    INTO  G_S_MSEG_LEAN.
      IF  G_FLAG_IS_OIL_ACTIVE = 'X'.
        IF ( G_S_MSEG_LEAN-BUSTM <> 'MEU1' )    OR
           ( G_S_MSEG_LEAN-BUSTM = 'MEU1'
           AND NOT G_S_MSEG_LEAN-OIGLCALC IS INITIAL
           AND NOT G_S_MSEG_LEAN-OIGLSKU IS INITIAL ).
          MOVE-CORRESPONDING  G_S_MSEG_LEAN
                             TO  MAT_SUM.
          COLLECT            MAT_SUM.
        ELSE.
          DELETE             G_T_MSEG_LEAN.
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING  G_S_MSEG_LEAN
                             TO  MAT_SUM.
        COLLECT              MAT_SUM.
      ENDIF.
    ENDLOOP.

    LOOP AT MAT_SUM.

      MAT_SUM-BWKEY = MAT_SUM-WERKS.
      MODIFY MAT_SUM.
    ENDLOOP.
  ENDIF.

*for valuated stock closing balance

  IF WRK_BWBST = 'X'.
    SORT MAT_WEG     BY BWKEY MATNR SHKZG.
    SORT MAT_WEG_BUK BY BWKEY MATNR SHKZG.

    LOOP AT G_T_MBEW         INTO  G_S_MBEW.
      CLEAR: MAT_WEG, MAT_WEG_BUK.
      MOVE-CORRESPONDING G_S_MBEW      TO BESTAND.


      READ TABLE MAT_WEG WITH KEY BWKEY = G_S_MBEW-BWKEY
MATNR = G_S_MBEW-MATNR
                                  SHKZG = 'S' BINARY SEARCH.
      BESTAND-ENDMENGE = G_S_MBEW-LBKUM - MAT_WEG-MENGE.
      BESTAND-ENDWERT  = G_S_MBEW-SALK3 - MAT_WEG-DMBTR.

      CLEAR: MAT_WEG, MAT_WEG_BUK.

      READ TABLE MAT_WEG WITH KEY BWKEY = G_S_MBEW-BWKEY
                                  MATNR = G_S_MBEW-MATNR
                                  SHKZG = 'H' BINARY SEARCH.
      BESTAND-ENDMENGE = BESTAND-ENDMENGE + MAT_WEG-MENGE.
      BESTAND-ENDWERT  = BESTAND-ENDWERT  + MAT_WEG-DMBTR.

      COLLECT BESTAND.
    ENDLOOP.
    FREE                     G_S_MBEW.

*for location/batch stock closing balance
  ELSEIF WRK_LGBST = 'X'.
    LOOP AT IMARD.

      CLEAR WEG_MAT-MENGE.

      MOVE-CORRESPONDING IMARD TO BESTAND.
      READ TABLE WEG_MAT WITH KEY WERKS = IMARD-WERKS
                                  LGORT = IMARD-LGORT
                                  MATNR = IMARD-MATNR
                                  SHKZG = 'S'.
      BESTAND-ENDMENGE = IMARD-LABST + IMARD-INSME + IMARD-SPEME
                       + IMARD-EINME +               IMARD-RETME
                       - WEG_MAT-MENGE.
      CLEAR WEG_MAT-MENGE.
      READ TABLE WEG_MAT WITH KEY WERKS = IMARD-WERKS
                                  LGORT = IMARD-LGORT
                                  MATNR = IMARD-MATNR
                                  SHKZG = 'H'.
      BESTAND-ENDMENGE = BESTAND-ENDMENGE + WEG_MAT-MENGE.
      COLLECT BESTAND.
    ENDLOOP.
  ENDIF.

*for valuated stock opening balance.

  IF WRK_BWBST = 'X'.
    SORT MAT_SUM     BY BWKEY MATNR SHKZG.
    SORT MAT_SUM_BUK BY BWKEY MATNR SHKZG.
    LOOP AT BESTAND.
      CLEAR: MAT_SUM, MAT_SUM_BUK.

      READ TABLE MAT_SUM WITH KEY BWKEY = BESTAND-BWKEY
                                  MATNR = BESTAND-MATNR
                                  SHKZG = 'S' BINARY SEARCH.
      MOVE MAT_SUM-MENGE TO BESTAND-SOLL.
      MOVE MAT_SUM-DMBTR TO BESTAND-SOLLWERT.

      CLEAR: MAT_SUM, MAT_SUM_BUK.

      READ TABLE MAT_SUM WITH KEY BWKEY = BESTAND-BWKEY
                                  MATNR = BESTAND-MATNR
                                  SHKZG = 'H' BINARY SEARCH.
      MOVE MAT_SUM-MENGE TO BESTAND-HABEN.
      MOVE MAT_SUM-DMBTR TO BESTAND-HABENWERT.

      BESTAND-ANFMENGE = BESTAND-ENDMENGE - BESTAND-SOLL
                                          + BESTAND-HABEN.
      BESTAND-ANFWERT = BESTAND-ENDWERT - BESTAND-SOLLWERT
                                        + BESTAND-HABENWERT.
      MODIFY BESTAND.

    ENDLOOP.


*for location/batch stock opening balance

  ELSEIF WRK_LGBST = 'X'.

    LOOP AT BESTAND.
      CLEAR SUM_MAT-MENGE.
      READ TABLE SUM_MAT WITH KEY WERKS = BESTAND-WERKS
                                  MATNR = BESTAND-MATNR
                                  SHKZG = 'S'.
      MOVE SUM_MAT-MENGE TO BESTAND-SOLL.
      CLEAR SUM_MAT-MENGE.
      READ TABLE SUM_MAT WITH KEY WERKS = BESTAND-WERKS
                                  MATNR = BESTAND-MATNR
                                  SHKZG = 'H'.
      MOVE SUM_MAT-MENGE TO BESTAND-HABEN.
      BESTAND-ANFMENGE = BESTAND-ENDMENGE - BESTAND-SOLL
                                          + BESTAND-HABEN.
      MODIFY BESTAND.
    ENDLOOP.
  ENDIF.

* exporting Opening,closing & receipt/issue values
  ENDMENGE =  BESTAND-ENDMENGE.    " opening balance
  ANFMENGE = BESTAND-ANFMENGE.     " closing balance
  SOLL = BESTAND-SOLL.             " Receipt total
  HABEN = BESTAND-HABEN.           " Issue total

ENDFUNCTION.
