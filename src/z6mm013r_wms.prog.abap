*&---------------------------------------------------------------------*
*& REPORT  Z6MM013R_WMS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6mm013r_wms.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION:
* OBJECT TYPE       :                 FUNC. CONSULTANT  :GIRISH
*          DEVELOPER:SUPRIYA
*      CREATION DATE:03.09.2010
*        DEV REQUEST:
*  TCODE            :Z6MM013
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

TABLES:mara,
       marm,
       z6mma_wms_cm,
       z6mma_wms_cm_his,
       qals.

DATA : BEGIN OF wa_mara,
       matnr TYPE mara-matnr,
       END OF wa_mara.

DATA : BEGIN OF wa_marm,
       matnr TYPE marm-matnr,
       meinh TYPE marm-meinh,
       umrez TYPE marm-umrez,
       umren TYPE marm-umren,
       END OF wa_marm.

DATA : BEGIN OF wa_makt,
       matnr TYPE makt-matnr,
       maktx TYPE makt-maktx,
       END OF wa_makt.

DATA : BEGIN OF wa_mkpf,
       mblnr TYPE mkpf-mblnr,
       mjahr TYPE mkpf-mjahr,
       budat TYPE mkpf-budat,
       cpudt TYPE mkpf-cpudt,
*       CPUTM type mkpf-CPUTM,
       END OF wa_mkpf.

DATA : BEGIN OF wa_mseg,
       mblnr TYPE mseg-mblnr,
       mjahr TYPE mseg-mjahr,
       zeile TYPE mseg-zeile,
       bwart TYPE mseg-bwart,
       matnr TYPE mseg-matnr,
*       WERKS TYPE MSEG-WERKS,
       lgort type mseg-lgort,"pravin aded 17/02/16
       charg TYPE mseg-charg,
       shkzg TYPE mseg-shkzg,
       menge TYPE mseg-menge,
       meins TYPE mseg-meins,
       sjahr TYPE mseg-sjahr,
       smbln TYPE mseg-smbln,
       END OF wa_mseg.

DATA : BEGIN OF wa_mseg1,
       mblnr TYPE mseg-mblnr,
       mjahr TYPE mseg-mjahr,
       zeile TYPE mseg-zeile,
       bwart TYPE mseg-bwart,
       matnr TYPE mseg-matnr,
*       WERKS TYPE MSEG-WERKS,
       lgort type mseg-lgort,"pravin added 17/02/16
       charg TYPE mseg-charg,
       shkzg TYPE mseg-shkzg,
       menge TYPE mseg-menge,
       meins TYPE mseg-meins,
       sjahr TYPE mseg-sjahr,
       smbln TYPE mseg-smbln,
       rev,
       END OF wa_mseg1.

DATA :wa_z6mma_wms_mm  TYPE z6mma_wms_mm,
      wa_z6mma_wms_out TYPE z6mma_wms_out,
      wa_z6mma_wms_in  TYPE z6mma_wms_in.

DATA : BEGIN OF wa_mseg_in,
       mblnr TYPE mseg-mblnr,
       mjahr TYPE mseg-mjahr,
       zeile TYPE mseg-zeile,
       bwart TYPE mseg-bwart,
       matnr TYPE mseg-matnr,
*       WERKS TYPE MSEG-WERKS,
       lgort type mseg-lgort,"pravin aded 17/02/16
       charg TYPE mseg-charg,
       shkzg TYPE mseg-shkzg,
       menge TYPE mseg-menge,
       meins TYPE mseg-meins,
       END OF wa_mseg_in.

DATA : BEGIN OF wa_mseg_in1,
       mblnr TYPE mseg-mblnr,
       mjahr TYPE mseg-mjahr,
       zeile TYPE mseg-zeile,
       bwart TYPE mseg-bwart,
       matnr TYPE mseg-matnr,
*       WERKS TYPE MSEG-WERKS,
       lgort type mseg-lgort,"pravin aded 17/02/16
       charg TYPE mseg-charg,
       shkzg TYPE mseg-shkzg,
       menge TYPE mseg-menge,
       meins TYPE mseg-meins,
       indi,
       END OF wa_mseg_in1.

DATA : BEGIN OF wa_qamb,
       prueflos TYPE qals-prueflos,
*       AUFNR    TYPE QALS-AUFNR,
       mblnr    TYPE qals-mblnr,
       mjahr    TYPE qals-mjahr,
       zeile    TYPE qals-zeile,
       END OF wa_qamb.

*data : begin of wa_wms_mm ,
*       mandt type sy-mandt,
*       ITEM_CODE type mara-matnr,
*       CONTAINER type Z6MMA_WMS_MM-container,
*       MAT_CAT type Z6MMA_WMS_MM-mat_cat,
*       MAT_TYPE type Z6MMA_WMS_MM-MAT_TYPE,
*       DESCRIPTION type makt-maktx,
*       end of wa_wms_mm.

DATA : it_mara          LIKE STANDARD TABLE OF wa_mara,
*       IT_MARA1         LIKE STANDARD TABLE OF WA_MARA,
       it_marm          LIKE STANDARD TABLE OF wa_marm,
       it_z6mma_wms_mm  LIKE STANDARD TABLE OF wa_z6mma_wms_mm,
       it_makt          LIKE STANDARD TABLE OF wa_makt,
       it_mkpf          LIKE STANDARD TABLE OF wa_mkpf,
       it_mseg          LIKE STANDARD TABLE OF wa_mseg,
       it_mseg1         LIKE STANDARD TABLE OF wa_mseg1,
       it_z6mma_wms_out LIKE STANDARD TABLE OF wa_z6mma_wms_out,
       it_z6mma_wms_in  LIKE STANDARD TABLE OF wa_z6mma_wms_in,
       it_mseg_in       LIKE STANDARD TABLE OF wa_mseg_in,
       it_mseg_in1      LIKE STANDARD TABLE OF wa_mseg_in1,
       it_qamb          LIKE STANDARD TABLE OF wa_qamb.
*       it_wms_mm LIKE STANDARD TABLE OF wa_wms_mm.

DATA : r_matnr TYPE RANGE OF mara-matnr,
       wa_matnr LIKE LINE OF r_matnr.
DATA : temp_mblnr TYPE char10.
DATA : wa_vfdat TYPE mch1-vfdat.

*---HISTORY TABLES
DATA :BEGIN OF wa_mm_chk,
      item_code TYPE z6mma_wms_mm_his-item_code,
      container TYPE z6mma_wms_mm_his-container,
      END OF wa_mm_chk.

DATA :BEGIN OF wa_out_chk,
      item_code TYPE z6mma_wms_out_hi-item_code,
      container TYPE z6mma_wms_out_hi-container,
      batch     TYPE z6mma_wms_out_hi-batch,
      docu_no   TYPE z6mma_wms_out_hi-docu_no,
      mjahr     TYPE z6mma_wms_out_hi-mjahr,
      END OF wa_out_chk.

DATA :BEGIN OF wa_in_chk,
      gr_no     TYPE z6mma_wms_in_his-gr_no,
      mjahr     TYPE z6mma_wms_in_his-mjahr,
      item_code TYPE z6mma_wms_in_his-item_code,
      container TYPE z6mma_wms_in_his-container,
      batch     TYPE z6mma_wms_in_his-batch,
      END OF wa_in_chk.

DATA : wa_mm_hist  TYPE z6mma_wms_mm_his,
       wa_out_hist TYPE z6mma_wms_out_hi,
       wa_in_hist  TYPE z6mma_wms_in_his.

DATA : it_mm_chk   LIKE STANDARD TABLE OF wa_mm_chk,
       it_out_chk  LIKE STANDARD TABLE OF wa_out_chk,
       it_in_chk   LIKE STANDARD TABLE OF wa_in_chk,
       it_mm_hist  LIKE STANDARD TABLE OF wa_mm_hist,
       it_out_hist LIKE STANDARD TABLE OF wa_out_hist,
       it_in_hist  LIKE STANDARD TABLE OF  wa_in_hist.

DATA: it_in_hist_ck  LIKE STANDARD TABLE OF  wa_in_hist,
      wa_in_hist_ck LIKE wa_in_hist,
      it_out_hist_ck LIKE STANDARD TABLE OF wa_out_hist,
      wa_out_hist_ck LIKE wa_out_hist,
      it_mm_hist_ck  LIKE STANDARD TABLE OF wa_mm_hist,
      wa_mm_hist_ck  LIKE wa_mm_hist.

DATA: BEGIN OF message,
        mm_msg TYPE char6,
        in_msg TYPE char6,
        out_msg TYPE char7,
        mm_msg_his TYPE char10,
        in_msg_his TYPE char10,
        out_msg_his TYPE char11,
      END OF message.
DATA: final_msg TYPE char70.
*----------------------------------------------------------------------*
*     SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-b01 .
SELECT-OPTIONS:  s_ersda FOR mara-ersda OBLIGATORY DEFAULT sy-datum.
PARAMETERS    :  p_werks LIKE mseg-werks DEFAULT '2101' MODIF ID mod.
SELECTION-SCREEN END OF BLOCK b1 .

**---------------------------------------------------------------------*
*       AT SELECTION-SCREEN OUTPUT.
**---------------------------------------------------------------------*



AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.
**---------------------------------------------------------------------*
*       START OF SELECTION.
**---------------------------------------------------------------------*

START-OF-SELECTION.
  perform chk_date.
  PERFORM history_data.
  PERFORM gather_data.
  PERFORM mm_data.
  PERFORM get_data_outward.
  PERFORM get_data_in.
  PERFORM table_update.
*&---------------------------------------------------------------------*
*&      FORM  GATHER_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM gather_data.

  SELECT matnr
         INTO TABLE it_mara FROM mara
         WHERE labor = '011'.

  IF sy-subrc = 0.
    LOOP AT it_mara INTO wa_mara.
      wa_matnr-sign   = 'I'.
      wa_matnr-option = 'EQ'.
      wa_matnr-low    = wa_mara-matnr.
      wa_matnr-high   = ''.
      APPEND wa_matnr TO r_matnr.
    ENDLOOP.
  ENDIF.
  CLEAR wa_matnr.

*  SELECT MATNR FROM MARA INTO TABLE IT_MARA
*         WHERE ERSDA IN S_ERSDA
*         AND   LABOR = '011'.
*  IF SY-SUBRC = 0.
*    SORT IT_MARA BY MATNR.
*  ENDIF.

  IF NOT r_matnr IS INITIAL.
    SELECT matnr
           meinh
           umrez
           umren FROM marm INTO TABLE it_marm
*           FOR ALL ENTRIES IN IT_MARA
*           WHERE MATNR = IT_MARA-MATNR.
            WHERE matnr IN r_matnr.
    IF sy-subrc = 0.
      SORT it_marm BY matnr meinh.
    ENDIF.
    SELECT matnr
           maktx FROM makt INTO TABLE it_makt
*           FOR ALL ENTRIES IN IT_MARA
*           WHERE MATNR = IT_MARA-MATNR.
            WHERE matnr IN r_matnr.
    IF sy-subrc = 0.
      SORT it_makt BY matnr.
    ENDIF.
  ENDIF.

*  CLEAR WA_MARA.
*  SELECT MATNR
*         INTO TABLE IT_MARA1 FROM MARA
*         WHERE LABOR = '011'.
*
*  IF SY-SUBRC = 0.
*    LOOP AT IT_MARA1 INTO WA_MARA.
*      WA_MATNR-SIGN   = 'I'.
*      WA_MATNR-OPTION = 'EQ'.
*      WA_MATNR-LOW    = WA_MARA-MATNR.
*      WA_MATNR-HIGH   = ''.
*      APPEND WA_MATNR TO R_MATNR.
*    ENDLOOP.
*  ENDIF.
*  CLEAR WA_MATNR.
ENDFORM.                    " GATHER_DATA
*&---------------------------------------------------------------------*
*&      FORM  MM_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM mm_data .

  CLEAR :it_z6mma_wms_mm,wa_z6mma_wms_mm,wa_mara.

  LOOP AT it_mara INTO wa_mara.
    wa_z6mma_wms_mm-mandt     = sy-mandt.
    wa_z6mma_wms_mm-item_code = wa_mara-matnr.

    READ TABLE it_mm_chk INTO wa_mm_chk WITH KEY item_code    = wa_z6mma_wms_mm-item_code
*                                              CONTAINER    = WA_Z6MMA_WMS_MM-CONTAINER
                                                    BINARY SEARCH.
    IF sy-subrc = 0 .
      CLEAR wa_z6mma_wms_mm.
    ELSE.
      APPEND wa_z6mma_wms_mm TO it_z6mma_wms_mm.
    ENDIF.
  ENDLOOP.
  CLEAR wa_z6mma_wms_mm.

  LOOP AT it_z6mma_wms_mm INTO wa_z6mma_wms_mm.

    CLEAR: wa_marm.
    READ TABLE it_marm INTO wa_marm WITH KEY matnr = wa_z6mma_wms_mm-item_code
                                             meinh = 'EA'
                                              BINARY SEARCH.
    IF sy-subrc = 0.
*      IF WA_MARM-UMREN NE WA_MARM-UMREZ.
      "Anees
      SELECT SINGLE * FROM z6mma_wms_cm_his
        WHERE container_wt  = wa_marm-umrez.
*        SELECT SINGLE * FROM Z6MMA_WMS_CM
*          WHERE CONTAINER_WT  = WA_MARM-UMREZ.
      "Anees
      IF sy-subrc = 0.
        wa_z6mma_wms_mm-container = z6mma_wms_cm_his-container.
*          WA_Z6MMA_WMS_MM-CONTAINER = Z6MMA_WMS_CM-CONTAINER.
*          wa_wms_mm-CONTAINER = Z6MMA_WMS_CM-CONTAINER.
      ENDIF.
*      ENDIF.
    ENDIF.

    CLEAR wa_makt.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_z6mma_wms_mm-item_code
    BINARY SEARCH.
    IF sy-subrc = 0.
      wa_z6mma_wms_mm-description =  wa_makt-maktx.
*      wa_wms_mm-DESCRIPTION =  WA_MAKT-MAKTX.
    ENDIF.

*    READ TABLE IT_MM_CHK INTO WA_MM_CHK WITH KEY ITEM_CODE = WA_Z6MMA_WMS_MM-ITEM_CODE
*                                                 CONTAINER = WA_Z6MMA_WMS_MM-CONTAINER
*                                                 BINARY SEARCH.
*    IF SY-SUBRC = 0 .
*      CLEAR WA_Z6MMA_WMS_MM.
*    ELSE.
    wa_z6mma_wms_mm-lgort = '1502'."pravin added 17/02/16
    MODIFY it_z6mma_wms_mm FROM wa_z6mma_wms_mm TRANSPORTING container description lgort.
*    ENDIF.
  ENDLOOP.

  DELETE it_z6mma_wms_mm WHERE container IS INITIAL.
  "Anees
  LOOP AT it_z6mma_wms_mm INTO wa_z6mma_wms_mm.
    READ TABLE it_mm_hist_ck INTO wa_mm_hist_ck
              WITH KEY item_code = wa_z6mma_wms_mm-item_code.
    IF sy-subrc = 0.
      DELETE it_z6mma_wms_mm WHERE item_code = wa_z6mma_wms_mm-item_code.
    ENDIF.
  ENDLOOP.
  "End

  CLEAR : wa_z6mma_wms_mm,wa_mm_hist,it_mm_hist.
  LOOP AT it_z6mma_wms_mm INTO wa_z6mma_wms_mm.
    MOVE-CORRESPONDING wa_z6mma_wms_mm TO wa_mm_hist.
    wa_mm_hist-uname = sy-uname.
    wa_mm_hist-erdat = sy-datum.
    wa_mm_hist-uzeit = sy-timlo.
    APPEND wa_mm_hist TO it_mm_hist.
  ENDLOOP.
ENDFORM.                    "MM_DATA
*&---------------------------------------------------------------------*
*&      FORM  GET_DATA_OUTWARD
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM get_data_outward .

  CLEAR:wa_mkpf,it_mkpf,wa_mseg,it_mseg.

  IF NOT r_matnr IS INITIAL.
    SELECT mblnr
           mjahr
           budat
           cpudt FROM mkpf INTO TABLE it_mkpf
           WHERE cpudt IN s_ersda.
*           and CPUTM >= '134530'.
*    MBLNR IN (4900000126,4900000128,4900000130,4900000131,4900000132,4900000133,4900000134,4900000137) AND
    IF sy-subrc = 0.
      SORT it_mkpf BY mblnr mjahr.
    ENDIF.

    IF NOT it_mkpf IS INITIAL.
      SELECT mblnr
             mjahr
             zeile
             bwart
             matnr
             lgort "pravin aded 17/02/16
             charg
             shkzg
             menge
             meins
             sjahr
             smbln FROM mseg INTO TABLE it_mseg
             FOR ALL ENTRIES IN it_mkpf
             WHERE mblnr = it_mkpf-mblnr
             AND   mjahr = it_mkpf-mjahr
             AND   matnr IN r_matnr
             AND   werks = p_werks
*      '1302' "'2010'
*             AND   BWART IN (601,602,641)
             AND   bwart IN (311, 601,602,641,702)
             AND lgort = '1502'."'1501'.
      IF sy-subrc = 0.
        DELETE it_mseg WHERE bwart = 311
                        AND shkzg = 'S'.
        SORT it_mseg BY mblnr mjahr smbln sjahr.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR :wa_mseg1,it_mseg1.
  it_mseg1 = it_mseg.
  IF it_mseg1 IS NOT INITIAL.
  LOOP AT it_mseg1 INTO wa_mseg1.
*    IF WA_MSEG1-SHKZG = 'H'.
*      WA_MSEG1-MENGE = WA_MSEG1-MENGE * -1.
*    ENDIF.
    READ TABLE it_mseg INTO wa_mseg WITH KEY smbln = wa_mseg1-mblnr
                                             sjahr = wa_mseg1-mjahr
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_mseg1-rev = 'R'.
    ENDIF.
    MODIFY it_mseg1 FROM wa_mseg1 TRANSPORTING rev.
  ENDLOOP.

  DELETE it_mseg1 WHERE rev  = 'R'.
  DELETE it_mseg1 WHERE bwart = '602'.

*  SORT IFINAL BY MBLNR MJAHR.
  CLEAR :wa_mseg1,it_z6mma_wms_out,wa_z6mma_wms_out.

  LOOP AT it_mseg1 INTO wa_mseg1.
    READ TABLE it_out_chk INTO wa_out_chk WITH KEY  item_code  = wa_mseg1-matnr
                                                    batch      = wa_mseg1-charg
                                                    docu_no    = wa_mseg1-mblnr
                                                    mjahr      = wa_mseg1-mjahr
                                                    BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR wa_z6mma_wms_out .
      CLEAR wa_out_hist.
    ELSE.
      wa_z6mma_wms_out-item_code = wa_mseg1-matnr.
      wa_z6mma_wms_out-batch     = wa_mseg1-charg.
      " Anees
      CONCATENATE wa_mseg1-mblnr wa_mseg1-zeile INTO wa_z6mma_wms_out-docu_no.
*      WA_Z6MMA_WMS_OUT-DOCU_NO   = WA_MSEG1-MBLNR.
      " Anees
      wa_z6mma_wms_out-quantity  = wa_mseg1-menge.
    ENDIF.
    COLLECT wa_z6mma_wms_out INTO it_z6mma_wms_out.

  ENDLOOP.

  DELETE it_z6mma_wms_out WHERE item_code IS INITIAL.
  "Anees
  LOOP AT it_z6mma_wms_out INTO wa_z6mma_wms_out.
    READ TABLE it_out_hist_ck INTO wa_out_hist_ck
              WITH KEY docu_no = wa_z6mma_wms_out-docu_no.
    IF sy-subrc = 0.
      DELETE it_z6mma_wms_out WHERE docu_no = wa_z6mma_wms_out-docu_no.
    ENDIF.
  ENDLOOP.
  "End
  SORT it_mseg1 BY mblnr matnr charg.
  LOOP AT it_z6mma_wms_out INTO wa_z6mma_wms_out.
    CLEAR wa_marm.
    READ TABLE it_marm INTO wa_marm WITH KEY matnr = wa_z6mma_wms_out-item_code
                                                 meinh = 'EA'
                                                 BINARY SEARCH.
    IF sy-subrc = 0.
*      IF WA_MARM-UMREN NE WA_MARM-UMREZ.
      "Anees
      SELECT SINGLE * FROM z6mma_wms_cm_his
        WHERE container_wt  = wa_marm-umrez.
*        SELECT SINGLE * FROM Z6MMA_WMS_CM
*          WHERE CONTAINER_WT  = WA_MARM-UMREZ.
      "Anees
      IF sy-subrc = 0.
        wa_z6mma_wms_out-container = z6mma_wms_cm_his-container.
*          WA_Z6MMA_WMS_OUT-CONTAINER = Z6MMA_WMS_CM-CONTAINER.
      ENDIF.
*      ENDIF.
    ENDIF.

    CLEAR wa_makt.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_z6mma_wms_out-item_code
    BINARY SEARCH.
    IF sy-subrc = 0.
      wa_z6mma_wms_out-description =  wa_makt-maktx.
    ENDIF.
    wa_z6mma_wms_out-lgort = '1502'."pravin aded 17/02/16
    MODIFY it_z6mma_wms_out FROM wa_z6mma_wms_out TRANSPORTING container description lgort.

    MOVE-CORRESPONDING wa_z6mma_wms_out TO wa_out_hist.
    "Anees
    CLEAR temp_mblnr.
    temp_mblnr = wa_z6mma_wms_out-docu_no+0(10).
    READ TABLE it_mseg1 INTO wa_mseg1 WITH KEY mblnr = temp_mblnr
                                             matnr   = wa_z6mma_wms_out-item_code
                                             charg   = wa_z6mma_wms_out-batch
                                             BINARY SEARCH.
    "Anees
*    READ TABLE IT_MSEG1 INTO WA_MSEG1 WITH KEY MBLNR = WA_Z6MMA_WMS_OUT-DOCU_NO
*                                             MATNR   = WA_Z6MMA_WMS_OUT-ITEM_CODE
*                                             CHARG   = WA_Z6MMA_WMS_OUT-BATCH
*                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_out_hist-mjahr = wa_mseg1-mjahr.
      wa_out_hist-meins = wa_mseg1-meins.
      wa_out_hist-lgort = wa_mseg1-lgort. "pravin aded 17/02/16
    ENDIF.
    wa_out_hist-uname = sy-uname.
    wa_out_hist-erdat = sy-datum.
    wa_out_hist-uzeit = sy-timlo.
    APPEND wa_out_hist TO it_out_hist.
  ENDLOOP.
  delete it_z6mma_wms_out where container is initial.
  delete it_out_hist where container is initial.

*  ELSE.
*    MESSAGE 'No Records Found !!' type 'I'.
  ENDIF.
ENDFORM.                    " GET_DATA_OUTWARD
*&---------------------------------------------------------------------*
*&      FORM  GET_DATA_IN
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM get_data_in .

  CLEAR:wa_mseg_in ,it_mseg_in .
  IF NOT r_matnr IS INITIAL.
    IF NOT it_mkpf IS INITIAL.
      SELECT mblnr
             mjahr
             zeile
             bwart
             matnr
             lgort  "pravin aded 17/02/16
             charg
             shkzg
             menge
             meins FROM mseg INTO TABLE it_mseg_in
             FOR ALL ENTRIES IN it_mkpf
             WHERE mblnr = it_mkpf-mblnr
             AND   mjahr = it_mkpf-mjahr
             AND   matnr IN r_matnr
             AND   werks = p_werks
             AND   bwart IN (311,701)
             AND lgort = '1502' "'1501'
             AND shkzg = 'S'.
*      '1302' "'2010'
*             AND   BWART = '321'.

      IF sy-subrc = 0.
        SORT it_mseg_in BY mblnr mjahr .
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR :wa_mseg_in1,it_mseg_in1.
  it_mseg_in1 = it_mseg_in.

  "Anees
*  CLEAR :WA_QAMB,IT_QAMB.
*  IF NOT IT_MSEG_IN IS INITIAL.
**    SELECT PRUEFLOS AUFNR MJAHR MBLNR ZEILE FROM QALS INTO TABLE IT_QALS
**    FOR ALL ENTRIES IN IT_MSEG_IN WHERE MBLNR   = IT_MSEG_IN-MBLNR
**                                  AND MJAHR     = IT_MSEG_IN-MJAHR
**                                  AND ZEILE     = IT_MSEG_IN-ZEILE
**                                  AND WERK      = P_WERKS
**                                  AND AUFNR NE ''.
*    SELECT PRUEFLOS MBLNR MJAHR ZEILE FROM QAMB INTO TABLE IT_QAMB
*    FOR ALL ENTRIES IN IT_MSEG_IN WHERE MBLNR     = IT_MSEG_IN-MBLNR
*                                  AND   MJAHR     = IT_MSEG_IN-MJAHR
*                                  AND   ZEILE     = IT_MSEG_IN-ZEILE.
*  ENDIF.
*
*  SORT IT_QAMB BY MBLNR MJAHR ZEILE.
*  CLEAR WA_MSEG_IN1.
*  LOOP AT IT_MSEG_IN1 INTO WA_MSEG_IN1.
*    CLEAR WA_MSEG_IN1-INDI.
*    READ TABLE IT_QAMB INTO WA_QAMB WITH KEY MBLNR = WA_MSEG_IN1-MBLNR
*                                             MJAHR = WA_MSEG_IN1-MJAHR
*                                             ZEILE = WA_MSEG_IN1-ZEILE
*                                             BINARY SEARCH.
*    IF SY-SUBRC = 0.
*      SELECT SINGLE * FROM QALS WHERE PRUEFLOS = WA_QAMB-PRUEFLOS
*                                      AND AUFNR NE ''.
*      IF SY-SUBRC = 0.
*        WA_MSEG_IN1-INDI = ''.
*      ELSE .
*        WA_MSEG_IN1-INDI = 'X'.
*      ENDIF.
*    ELSE .
*      WA_MSEG_IN1-INDI = 'X'.
*    ENDIF.
*    MODIFY IT_MSEG_IN1 FROM WA_MSEG_IN1 TRANSPORTING INDI.
*  ENDLOOP.
  "End
  DELETE it_mseg_in1 WHERE indi = 'X'.

  CLEAR :wa_mseg_in1,wa_mseg_in1,it_z6mma_wms_in.
  LOOP AT it_mseg_in1 INTO wa_mseg_in1.
    READ TABLE it_in_chk INTO wa_in_chk WITH KEY gr_no       = wa_mseg_in1-mblnr
                                                 mjahr       = wa_mseg_in1-mjahr
                                                 item_code   = wa_mseg_in1-matnr
                                                 batch       = wa_mseg_in1-charg
                                                 BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR wa_z6mma_wms_in .
    ELSE.
      " Anees
      CONCATENATE wa_mseg_in1-mblnr wa_mseg_in1-zeile INTO wa_z6mma_wms_in-gr_no.
*      WA_Z6MMA_WMS_IN-GR_NO     = WA_MSEG_IN1-MBLNR.
      " Anees
      wa_z6mma_wms_in-item_code = wa_mseg_in1-matnr.
      wa_z6mma_wms_in-batch     = wa_mseg_in1-charg.
      wa_z6mma_wms_in-quantity  = wa_mseg_in1-menge.
    ENDIF.
    COLLECT wa_z6mma_wms_in INTO it_z6mma_wms_in.
  ENDLOOP.

  DELETE it_z6mma_wms_in  WHERE item_code IS INITIAL.
  CLEAR wa_z6mma_wms_in.
  "Anees
  LOOP AT it_z6mma_wms_in INTO wa_z6mma_wms_in.
    READ TABLE it_in_hist_ck INTO wa_in_hist_ck
              WITH KEY gr_no = wa_z6mma_wms_in-gr_no.
    IF sy-subrc = 0.
      DELETE it_z6mma_wms_in WHERE gr_no = wa_z6mma_wms_in-gr_no.
    ENDIF.
  ENDLOOP.
  "End
  LOOP AT it_z6mma_wms_in INTO wa_z6mma_wms_in.
    CLEAR wa_marm.
    READ TABLE it_marm INTO wa_marm WITH KEY matnr = wa_z6mma_wms_in-item_code
                                                 meinh = 'EA'
                                                 BINARY SEARCH.
    IF sy-subrc = 0.
*      IF WA_MARM-UMREN NE WA_MARM-UMREZ.
      "Anees
      SELECT SINGLE * FROM z6mma_wms_cm_his
        WHERE container_wt  = wa_marm-umrez.
*        SELECT SINGLE * FROM Z6MMA_WMS_CM
*          WHERE CONTAINER_WT  = WA_MARM-UMREZ.
      "Anees
      IF sy-subrc = 0.
        wa_z6mma_wms_in-container = z6mma_wms_cm_his-container.
*          WA_Z6MMA_WMS_IN-CONTAINER = Z6MMA_WMS_CM-CONTAINER.
      ENDIF.
*      ENDIF.
    ENDIF.

    CLEAR wa_makt.
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_z6mma_wms_in-item_code
    BINARY SEARCH.
    IF sy-subrc = 0.
      wa_z6mma_wms_in-description =  wa_makt-maktx.
    ENDIF.

    SELECT SINGLE vfdat INTO wa_vfdat FROM mch1
      WHERE matnr = wa_z6mma_wms_in-item_code
      AND charg = wa_z6mma_wms_in-batch.
    IF sy-subrc = 0.
      wa_z6mma_wms_in-expiry_date = wa_vfdat.
    ENDIF.

    wa_z6mma_wms_in-lgort = '1502'."pravin aded 17/02/16
    MODIFY it_z6mma_wms_in FROM wa_z6mma_wms_in TRANSPORTING container description expiry_date lgort.

*  ---FOR HISTORY TABLE
    MOVE-CORRESPONDING wa_z6mma_wms_in TO wa_in_hist.
    "Anees
    CLEAR temp_mblnr.
    temp_mblnr = wa_z6mma_wms_in-gr_no+0(10).
    READ TABLE it_mseg_in1 INTO wa_mseg_in1 WITH KEY mblnr = temp_mblnr
                                         matnr   = wa_z6mma_wms_in-item_code
                                         charg   = wa_z6mma_wms_in-batch
                                         BINARY SEARCH.
    "Anees
*    READ TABLE IT_MSEG_IN1 INTO WA_MSEG_IN1 WITH KEY MBLNR = WA_Z6MMA_WMS_IN-GR_NO
*                                             MATNR   = WA_Z6MMA_WMS_IN-ITEM_CODE
*                                             CHARG   = WA_Z6MMA_WMS_IN-BATCH
*                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_in_hist-mjahr = wa_mseg_in1-mjahr.
      wa_in_hist-meins = wa_mseg_in1-meins.
      wa_in_hist-lgort = wa_mseg_in1-lgort."pravin aded 17/02/16
    ENDIF.
    wa_in_hist-uname = sy-uname.
    wa_in_hist-erdat = sy-datum.
    wa_in_hist-uzeit = sy-timlo.
    APPEND wa_in_hist TO it_in_hist.
*    ----
  ENDLOOP.
  delete it_z6mma_wms_in where container is initial.
  delete it_in_hist where container is initial.
ENDFORM.                    " GET_DATA_IN
*&---------------------------------------------------------------------*
*&      FORM  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-group1 = 'MOD'.
      screen-active = '1'.
      screen-input  = '0'.
      screen-output = '1'.
      screen-invisible = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      FORM  HISTORY_DATA
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM history_data .
  "Anees
  SELECT * FROM z6mma_wms_out_hi
    INTO TABLE it_out_hist_ck
    WHERE erdat IN s_ersda.

  SELECT * FROM z6mma_wms_in_his
    INTO TABLE it_in_hist_ck
    WHERE erdat IN s_ersda.

  SELECT * FROM z6mma_wms_mm_his
    INTO TABLE it_mm_hist_ck.
  "End

  SELECT item_code
         container INTO TABLE it_mm_chk FROM  z6mma_wms_mm_his.
  IF sy-subrc = 0.
    SORT it_mm_chk BY item_code
                      container .
  ENDIF.

  SELECT item_code
         container
         batch
         docu_no
         mjahr INTO TABLE it_out_chk FROM  z6mma_wms_out_hi.
  IF sy-subrc = 0.
    SORT it_out_chk BY item_code
                       batch
                       docu_no
                       mjahr.
  ENDIF.

  SELECT gr_no
         mjahr
         item_code
         container
         batch INTO TABLE it_in_chk FROM  z6mma_wms_in_his.
  IF sy-subrc = 0.
    SORT it_in_chk  BY gr_no
                       mjahr
                       item_code
                       batch .
  ENDIF.

ENDFORM.                    " HISTORY_DATA
*&---------------------------------------------------------------------*
*&      FORM  TABLE_UPDATE
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM table_update .

  IF NOT it_z6mma_wms_mm IS INITIAL.
    MODIFY z6mma_wms_mm     FROM TABLE it_z6mma_wms_mm .
    IF sy-subrc = 0.
      message-mm_msg = 'WMS_MM'.
    ENDIF.
  ENDIF.

  IF NOT it_z6mma_wms_out IS INITIAL.
    MODIFY z6mma_wms_out     FROM TABLE it_z6mma_wms_out .
    IF sy-subrc = 0.
      message-out_msg = 'WMS_OUT'.
    ENDIF.
  ENDIF.

  IF NOT it_z6mma_wms_in IS INITIAL.
    MODIFY z6mma_wms_in      FROM TABLE it_z6mma_wms_in .
    IF sy-subrc = 0.
      message-in_msg = 'WMS_IN'.
    ENDIF.
  ENDIF.

  IF NOT it_mm_hist IS INITIAL.
    MODIFY z6mma_wms_mm_his FROM TABLE it_mm_hist.
    IF sy-subrc = 0.
      message-mm_msg_his = 'WMS_MM_HIS'.
    ENDIF.
  ENDIF.

  IF NOT it_out_hist IS INITIAL.
    MODIFY z6mma_wms_out_hi  FROM TABLE it_out_hist .
    IF sy-subrc = 0.
      message-out_msg_his = 'WMS_OUT_HIS'.
    ENDIF.
  ENDIF.

  IF NOT it_in_hist IS INITIAL.
    MODIFY z6mma_wms_in_his  FROM TABLE it_in_hist .
    IF sy-subrc = 0.
      message-in_msg_his = 'WMS_IN_HIS'.
    ENDIF.
  ENDIF.

  IF message IS NOT INITIAL.
    final_msg = 'Update of'.
    IF message-mm_msg IS NOT INITIAL.
      CONCATENATE final_msg message-mm_msg INTO final_msg SEPARATED BY ' '.
    ENDIF.
    IF message-mm_msg_his IS NOT INITIAL.
      CONCATENATE final_msg message-mm_msg_his INTO final_msg SEPARATED BY ' '.
    ENDIF.
    IF message-in_msg IS NOT INITIAL.
      CONCATENATE final_msg message-in_msg INTO final_msg SEPARATED BY ' '.
    ENDIF.
    IF message-in_msg_his IS NOT INITIAL.
      CONCATENATE final_msg message-in_msg_his INTO final_msg SEPARATED BY ' '.
    ENDIF.
    IF message-out_msg IS NOT INITIAL.
      CONCATENATE final_msg message-out_msg INTO final_msg SEPARATED BY ' '.
    ENDIF.
    IF message-out_msg_his IS NOT INITIAL.
      CONCATENATE final_msg message-out_msg_his INTO final_msg SEPARATED BY ' '.
    ENDIF.
    CONCATENATE final_msg 'is successful.' INTO final_msg SEPARATED BY ' '.
    MESSAGE final_msg TYPE 'S'.
  ENDIF.
ENDFORM.                    " TABLE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  CHK_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_DATE .
if s_ersda-low < '20130410'."'20130320'.
  message 'Please Enter date On or After "10.04.2013".' type 'E'.
endif.
ENDFORM.                    " CHK_DATE
