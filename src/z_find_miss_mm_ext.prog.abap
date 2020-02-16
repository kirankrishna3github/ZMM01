*&---------------------------------------------------------------------*
*& Report  Z_FIND_MISS_MM_EXT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*REPORT Z_FIND_MISS_MM_EXT.

*$*$----------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000570839                     $*
*$--------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          All Support Package Levels                   $*
*$  Release 500          Fm SAPKH50001                                $*
*$  Release 600          Fm SAPKH60001                                $*
*$  Release 602          All Support Package Levels                   $*
*$  Release 603          All Support Package Levels                   $*
*$  Release 604          Fm SAPKH60401                                $*
*$  Release 605          All Support Package Levels                   $*
*$  Release 606          Fm SAPKH60601                                $*
*$  Release 616          All Support Package Levels                   $*
*$  Release 617          All Support Package Levels                   $*
*$*$----------------------------------------------------------------$*$*
*&--------------------------------------------------------------------*
*& Object          REPS Z_FIND_MISS_MM_EXT
*& Object Header   PROG Z_FIND_MISS_MM_EXT
*&--------------------------------------------------------------------*
*& REPORT Z_FIND_MISS_MM_EXT
*&--------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report  Z_FIND_MISS_MM_EXT                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
*& This report searches for existing FI documents without              *
*& corresponding MM documents and other follow up documents            *
*&                                                                     *
*&---------------------------------------------------------------------*

* Change history
* 04.06.03: Version 0, Responsible: SvSi, note 628468
* 17.06.03: Version 1, corrected SORT criteria

REPORT  z_find_miss_mm NO STANDARD PAGE HEADING LINE-SIZE 123.

TABLES: bkpf.

TYPES: BEGIN OF rbkp_miss,
       bukrs TYPE bkpf-bukrs,
       belnr_fi TYPE bkpf-belnr,
       belnr_mm TYPE rbkp-belnr,
       gjahr TYPE rbkp-gjahr,
       f_header TYPE c,
       f_pos TYPE c,
       f_po_hist TYPE c,
       f_text_mm(30) TYPE c,
       f_text_co(30) TYPE c,
       f_text_ml(30) TYPE c,
       f_text_fm(30) TYPE c,
       END OF rbkp_miss.

TYPES: BEGIN OF t_bkpf,
       bukrs TYPE bkpf-bukrs,
       belnr TYPE bkpf-belnr,
       gjahr TYPE bkpf-belnr,
       awkey TYPE bkpf-awkey,
       bstat TYPE bkpf-bstat,
       END OF t_bkpf.

DATA: tab_bkpf TYPE TABLE OF t_bkpf,
      tab_rbkp TYPE TABLE OF rbkp WITH HEADER LINE,
      tab_rseg TYPE TABLE OF rseg,
      tab_rseg_acc TYPE TABLE OF rseg,
      tab_rbco TYPE TABLE OF rbco,
      tab_rbma TYPE TABLE OF rbma,
      tab_ekbe TYPE TABLE OF ekbe,
      tab_ekbz TYPE TABLE OF ekbz,
      tab_cobk TYPE TABLE OF cobk,
      tab_t001k TYPE TABLE OF t001k,
      tab_mlhd TYPE TABLE OF mlhd,
      tab_rbkp_miss TYPE TABLE OF rbkp_miss.

DATA: s_bkpf TYPE t_bkpf,
      s_rbkp TYPE rbkp,
      s_rbkp_miss TYPE rbkp_miss,
      s_rseg TYPE rseg,
      s_rbco TYPE rbco,
      s_rbma TYPE rbma,
      s_ekbe TYPE ekbe,
      s_ekbz TYPE ekbz,
      s_cobk TYPE cobk,
      s_mlhd TYPE mlhd,
      s_fmifihd TYPE fmifihd,
      s_t001k TYPE t001k.

DATA: "f_bkpf-belnr TYPE bkpf-belnr,
      "f_bkpf-gjahr TYPE bkpf-gjahr,
      f_lines TYPE I,
      f_rseg_not_found TYPE c,
      f_rseg_acc TYPE c,
      f_ekbe_acc TYPE c,
      f_rbco_found TYPE c,
      f_rbma_found TYPE c,
      f_rseg_acc_check TYPE rbkp-belnr,
      f_ekbe_acc_check TYPE ekbe-ebeln,
      f_fm_found TYPE c,
      i_applc TYPE c VALUE 'A',
      l_flg_active TYPE c.

DATA: c_found(5) TYPE c VALUE 'found',
      c_not_found(9) TYPE c VALUE 'not found',
      c_co_nodata(7) TYPE c VALUE 'no data'.

SELECT-OPTIONS: so_bukrs FOR bkpf-bukrs,
                so_belnr FOR bkpf-belnr,
                so_gjahr FOR bkpf-gjahr.

SELECT bukrs belnr gjahr awkey bstat FROM bkpf
                     INTO CORRESPONDING FIELDS OF TABLE tab_bkpf
                                     WHERE bukrs IN so_bukrs
                                       AND belnr IN so_belnr
                                       AND gjahr IN so_gjahr
                                       AND awtyp EQ 'RMRP'
                                       AND bstat EQ space
                                       AND ausbk EQ space.
READ TABLE tab_bkpf INTO s_bkpf INDEX 1.
IF sy-subrc NE 0.
  WRITE: 'No document to repair.'.
  EXIT.
ENDIF.

LOOP AT tab_bkpf INTO s_bkpf.
  MOVE: s_bkpf-bukrs TO s_rbkp_miss-bukrs,
        s_bkpf-belnr TO s_rbkp_miss-belnr_fi,
        s_bkpf-awkey(10) TO s_rbkp_miss-belnr_mm,
        s_bkpf-awkey+10(4) TO s_rbkp_miss-gjahr.
  IF s_bkpf-gjahr = s_rbkp_miss-gjahr.
    APPEND s_rbkp_miss TO tab_rbkp_miss.
  ENDIF.
ENDLOOP.

SELECT * FROM rbkp INTO TABLE tab_rbkp
                          FOR ALL ENTRIES IN tab_rbkp_miss
                          WHERE belnr = tab_rbkp_miss-belnr_mm
                            AND gjahr = tab_rbkp_miss-gjahr.

LOOP AT tab_rbkp_miss INTO s_rbkp_miss.
  READ TABLE tab_rbkp INTO s_rbkp WITH KEY belnr = s_rbkp_miss-belnr_mm
                                           gjahr = s_rbkp_miss-gjahr.
  IF sy-subrc = 0 AND s_rbkp-ivtyp = '2'.
    DELETE TABLE tab_rbkp_miss FROM s_rbkp_miss.
  ENDIF.
ENDLOOP.

CLEAR: s_rbkp, s_rbkp_miss.

READ TABLE tab_rbkp_miss INTO s_rbkp_miss INDEX 1.

IF sy-subrc NE 0.
  WRITE: 'No documents found.'.
  EXIT.
ELSE.
  SELECT * FROM rseg INTO TABLE tab_rseg
                            FOR ALL ENTRIES IN tab_rbkp_miss
                            WHERE belnr = tab_rbkp_miss-belnr_mm
                              AND gjahr = tab_rbkp_miss-gjahr.

  SELECT * FROM rbco INTO TABLE tab_rbco
                            FOR ALL ENTRIES IN tab_rbkp_miss
                            WHERE belnr = tab_rbkp_miss-belnr_mm
                              AND gjahr = tab_rbkp_miss-gjahr.

  SELECT * FROM rbma INTO TABLE tab_rbma
                            FOR ALL ENTRIES IN tab_rbkp_miss
                            WHERE belnr = tab_rbkp_miss-belnr_mm
                              AND gjahr = tab_rbkp_miss-gjahr.

  DESCRIBE TABLE tab_rseg LINES f_lines.
  IF f_lines NE 0.
    SELECT * FROM ekbe INTO TABLE tab_ekbe
                              FOR ALL ENTRIES IN tab_rseg
                              WHERE ebeln = tab_rseg-ebeln
                                AND ebelp = tab_rseg-ebelp
                                AND gjahr = tab_rseg-gjahr
                                AND belnr = tab_rseg-belnr.

    SELECT * FROM ekbz INTO TABLE tab_ekbz
                               FOR ALL ENTRIES IN tab_rseg
                              WHERE ebeln = tab_rseg-ebeln
                                AND ebelp = tab_rseg-ebelp
                                AND belnr = tab_rseg-belnr
                                AND gjahr = tab_rseg-gjahr.
    CLEAR f_lines.
  ELSE.
    SELECT * FROM ekbe INTO TABLE tab_ekbe
                            FOR ALL ENTRIES IN tab_rbkp_miss
                            WHERE belnr = tab_rbkp_miss-belnr_mm
                              AND gjahr = tab_rbkp_miss-gjahr.

    SELECT * FROM ekbz INTO TABLE tab_ekbz
                            FOR ALL ENTRIES IN tab_rbkp_miss
                            WHERE belnr = tab_rbkp_miss-belnr_mm
                              AND gjahr = tab_rbkp_miss-gjahr.
  ENDIF.
  SELECT * FROM cobk INTO TABLE tab_cobk
                              FOR ALL ENTRIES IN tab_rbkp_miss
                             WHERE gjahr = tab_rbkp_miss-gjahr
                               AND refbt = 'R'
                               AND refbn = tab_rbkp_miss-belnr_mm.

  SELECT * FROM t001k INTO TABLE tab_t001k
                                FOR ALL ENTRIES IN tab_rbkp_miss
                                WHERE bukrs = tab_rbkp_miss-bukrs.

*{   REPLACE        SBXK900030                                        1
*\  SELECT * FROM mlhd INTO TABLE tab_mlhd
*\                            FOR ALL ENTRIES IN tab_rbkp_miss
*\                            WHERE kjahr = tab_rbkp_miss-gjahr
*\                            AND vgart = 'UP'
*\                            AND awtyp = 'RMRP'
*\                            AND awref = tab_rbkp_miss-belnr_mm.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Thursday, October 04, 2018 17:00:00
* Changed By - ABAP01 - BhushanM
* Purpose    - Simplification list - 2220005 - S/4 HANA: Data Model Changes in Pricing and Condition Technic
* Solution   - Used alternate way to Get Data
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
"Need to Check after GRN
"If not working corrently, then go with CKML_F_DOCUMENT_READ_MLHD FM
*BREAK indo_basis.
BREAK abap01.

    SELECT * FROM mldoc
             INTO CORRESPONDING FIELDS OF TABLE tab_mlhd
             FOR ALL ENTRIES IN tab_rbkp_miss
             WHERE kjahr EQ tab_rbkp_miss-gjahr
               AND vgart EQ 'UP'
               AND awtyp EQ 'RMRP'
               AND awref EQ tab_rbkp_miss-belnr_mm.

    SELECT * FROM mlhd
             APPENDING CORRESPONDING FIELDS OF TABLE tab_mlhd
             FOR ALL ENTRIES IN tab_rbkp_miss
             WHERE kjahr EQ tab_rbkp_miss-gjahr
               AND vgart EQ 'UP'
               AND awtyp EQ 'RMRP'
               AND awref EQ tab_rbkp_miss-belnr_mm.

    SORT : tab_mlhd by belnr kjahr.
    DELETE ADJACENT DUPLICATES FROM tab_mlhd COMPARING belnr kjahr.

*}   REPLACE
  ENDIF.

* For better performance: sort all tables
SORT tab_rbkp BY belnr gjahr.
SORT tab_rseg BY belnr gjahr.
SORT tab_rbco BY belnr gjahr.
SORT tab_rbma BY belnr gjahr.
SORT tab_ekbe BY belnr gjahr.
SORT tab_ekbz BY belnr gjahr.
SORT tab_cobk BY gjahr.
SORT tab_mlhd BY kjahr.

LOOP AT tab_rbkp_miss INTO s_rbkp_miss.
  READ TABLE tab_rbkp INTO s_rbkp WITH KEY belnr = s_rbkp_miss-belnr_mm
                                           gjahr = s_rbkp_miss-gjahr
                                           BINARY SEARCH.
  IF sy-subrc NE 0.
    s_rbkp_miss-f_header = 'X'.
  ENDIF.

  READ TABLE tab_rseg INTO s_rseg WITH KEY belnr = s_rbkp_miss-belnr_mm
                                           gjahr = s_rbkp_miss-gjahr
                                           BINARY SEARCH.
  IF sy-subrc NE 0.
    f_rseg_not_found = 'X'.
  ENDIF.
  READ TABLE tab_rbco INTO s_rbco WITH KEY belnr = s_rbkp_miss-belnr_mm
                                           gjahr = s_rbkp_miss-gjahr
                                           BINARY SEARCH.
  IF sy-subrc = 0.
    f_rbco_found = 'X'.
  ENDIF.

  READ TABLE tab_rbma INTO s_rbma WITH KEY
                                  belnr = s_rbkp_miss-belnr_mm
                                  gjahr = s_rbkp_miss-gjahr
                                  BINARY SEARCH.
  IF sy-subrc = 0.
    f_rbma_found = 'X'.
  ENDIF.

  IF ( f_rseg_not_found NE space AND
       f_rbco_found EQ space AND
       f_rbma_found EQ space ).
    s_rbkp_miss-f_pos = 'X'.
  ENDIF.

  READ TABLE tab_ekbe INTO s_ekbe WITH KEY belnr = s_rbkp_miss-belnr_mm
                                            gjahr = s_rbkp_miss-gjahr
                                            BINARY SEARCH.
  IF sy-subrc NE 0.
  READ TABLE tab_ekbz INTO s_ekbz WITH KEY belnr = s_rbkp_miss-belnr_mm
                                              gjahr = s_rbkp_miss-gjahr
                                                          BINARY SEARCH.
    IF sy-subrc NE 0.
      IF ( f_rbco_found EQ space AND f_rbma_found EQ space ).
        s_rbkp_miss-f_po_hist = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

* Check if MM doument is okay, if not read follow up documents
  IF ( s_rbkp_miss-f_header = 'X' OR
       s_rbkp_miss-f_pos = 'X'    OR
       s_rbkp_miss-f_po_hist = 'X' ).

* Check if there are accountings for CO document
* PO data
    IF f_rseg_not_found = ' '.
      LOOP AT tab_rseg INTO s_rseg WHERE belnr = s_rbkp_miss-belnr_mm
                                     AND gjahr = s_rbkp_miss-gjahr
                                     AND knttp NE space.
        MOVE s_rseg-belnr TO f_rseg_acc_check.
        IF f_rseg_acc_check NE space.
          f_rseg_acc = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      IF s_rbkp_miss-f_pos = 'X'.
        IF NOT s_rbkp_miss-f_po_hist = 'X'.
        LOOP AT tab_ekbe INTO s_ekbe WHERE belnr = s_rbkp_miss-belnr_mm
                                          AND gjahr = s_rbkp_miss-gjahr
                                                     AND zekkn NE space.
            MOVE s_ekbe-ebeln TO f_ekbe_acc_check.
            IF f_ekbe_acc NE space.
              f_ekbe_acc = 'X'.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    IF ( f_rseg_acc = 'X' OR
         f_ekbe_acc = 'X' OR
         f_rbco_found = 'X' ).

      READ TABLE tab_cobk INTO s_cobk WITH KEY
                                      gjahr = s_rbkp_miss-gjahr
                                      refbt = 'R'
                                      refbn = s_rbkp_miss-belnr_mm
                                      orgvg = 'RMRP'.
      IF sy-subrc EQ 0.
        s_rbkp_miss-f_text_co = c_found.
      ELSE.
        s_rbkp_miss-f_text_co = c_not_found.
      ENDIF.
    ELSEIF ( s_rbkp_miss-f_pos = 'X' AND
             s_rbkp_miss-f_po_hist = 'X' ).
      s_rbkp_miss-f_text_co = c_co_nodata.
    ENDIF.

    READ TABLE tab_t001k INTO s_t001k WITH KEY
                                      bukrs = s_rbkp_miss-bukrs.
    IF sy-subrc EQ 0 AND s_t001k-mlbwa NE space.
      READ TABLE tab_mlhd INTO s_mlhd WITH KEY
                                      kjahr = s_rbkp_miss-gjahr
                                      vgart = 'UP'
                                      awtyp = 'RMRP'
                                      awref = s_rbkp_miss-belnr_mm.
      IF sy-subrc EQ 0.
        s_rbkp_miss-f_text_ml = c_found.
      ELSE.
        s_rbkp_miss-f_text_ml = c_not_found.
      ENDIF.
    ENDIF.

* Is the company code FM activ?
    CALL FUNCTION 'FMFK_BUKRS_CHECK_FMAKTIV'
         EXPORTING
              ip_bukrs           = s_rbkp_miss-bukrs
              ip_applc           = i_applc
         IMPORTING
              op_is_active       = l_flg_active
         EXCEPTIONS
              no_fikrs_for_bukrs = 1
              wrong_input_flag   = 2.

    IF l_flg_active = 'J'.
      SELECT SINGLE * FROM fmifihd INTO s_fmifihd
                              WHERE awref = s_rbkp_miss-belnr_mm
                                AND aworg = s_rbkp_miss-gjahr.
      IF sy-subrc EQ 0.
        s_rbkp_miss-f_text_fm = 'found'.
      ELSE.
        s_rbkp_miss-f_text_fm = 'not found'.
      ENDIF.
    ENDIF.
  ENDIF.

* analysis and/or finish
  IF ( s_rbkp_miss-f_header NE space
     OR s_rbkp_miss-f_pos NE space
     OR s_rbkp_miss-f_po_hist NE space ).
    MODIFY tab_rbkp_miss FROM s_rbkp_miss.
  ELSE.
    DELETE tab_rbkp_miss WHERE
                         belnr_fi = s_rbkp_miss-belnr_fi AND
                         belnr_mm = s_rbkp_miss-belnr_mm AND
                         gjahr = s_rbkp_miss-gjahr AND
                         bukrs = s_rbkp_miss-bukrs.
  ENDIF.

*Clear all data for next run
  CLEAR: s_rbkp, s_rseg, s_rbma, s_ekbe, s_ekbz, s_cobk, s_rbkp_miss,
       f_rbco_found, f_rbma_found, f_rseg_acc, f_ekbe_acc,
       f_rseg_acc_check, f_ekbe_acc_check, f_rseg_not_found.
ENDLOOP.

READ TABLE tab_rbkp_miss INTO s_rbkp_miss INDEX 1.
IF sy-subrc EQ 0.

  WRITE: 'Following MM-documents are missing or archived.'.
  SKIP.
  WRITE AT:/1  'comp. code', 15 'FI doc.', 27 'MM doc.',
          41 'fisc.year', 53 'head', 58 'pos',
          63 'hist', 69 'CO document', 82 'ML document',
          95 'FM document', 107 'comment'.
  ULINE.

  LOOP AT tab_rbkp_miss INTO s_rbkp_miss.
    IF ( s_rbkp_miss-f_header NE space AND
         s_rbkp_miss-f_pos EQ space AND
         s_rbkp_miss-f_po_hist EQ space ).
      s_rbkp_miss-f_text_mm = 'Note 609655'.
    ELSE.
      s_rbkp_miss-f_text_mm = 'Send OSS message'.
    ENDIF.

    WRITE AT: /1  s_rbkp_miss-bukrs COLOR COL_NORMAL,
               15 s_rbkp_miss-belnr_fi COLOR COL_HEADING,
               27 s_rbkp_miss-belnr_mm COLOR COL_POSITIVE,
               41 s_rbkp_miss-gjahr COLOR COL_POSITIVE, 52 '|',
               53 s_rbkp_miss-f_header, 57 '|',
               58 s_rbkp_miss-f_pos, 62 '|',
               63 s_rbkp_miss-f_po_hist, 67 '|',
               69 s_rbkp_miss-f_text_co, 81 '|',
               82 s_rbkp_miss-f_text_ml, 94 '|',
               95 s_rbkp_miss-f_text_fm, 106 '|',
               107 s_rbkp_miss-f_text_mm.
    ULINE.
  ENDLOOP.
ELSE.
  WRITE: 'No documents to repair.'.
ENDIF.

*>>>> END OF INSERTION <<<<<<
...
*&--------------------------------------------------------------------*
