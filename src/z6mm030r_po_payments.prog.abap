*&---------------------------------------------------------------------*
*& Report  Z6MM024R_PO_PAYMENTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


REPORT z6mm024r_po_payments   NO STANDARD PAGE HEADING
                           LINE-SIZE  210
                           MESSAGE-ID zmm01.

*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Purchase Order Payments
* OBJECT TYPE       : Report               FUNC. CONSULTANT  : Girish/Ambar
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 06.12.2010
*        DEV REQUEST: IRDK903645
*  TCODE            : ZMM024
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 03.11.2015
*   REASON FOR CHANGE: Authorization
*   REQUEST #: IRDK921459
* --------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       1 :   Tables Defination                                        *
*----------------------------------------------------------------------*
TABLES:
         bkpf,
         ekko ,
*         bseg,
         t001,                     "Company Master
         t001w,                    "Plant master
         t134,                     "Material type master
         t007a,
*         rbkp,
         rseg,
         bset,
         mseg,
         ekbe.
*----------------------------------------------------------------------*
*       2 :   Internal Table Declaration                               *
*----------------------------------------------------------------------*
DATA:   BEGIN OF itab OCCURS 1,
           bukrs  LIKE  bseg-bukrs,           "IR Docno
           werks  LIKE t001w-werks,
           mwskz  LIKE t007a-mwskz,          "tax code
           bedat  LIKE sy-datum,             "Purchase order date
           ebeln  LIKE ekko-ebeln,           "PO No
           ebelp  LIKE ekpo-ebelp,           "PO Item No
           netwr  LIKE ekpo-netwr,           "PO Item Value
           lifnr  LIKE ekko-lifnr,           "Vendor code
           name1  LIKE lfa1-name1,           "Vendor Name
           matnr  LIKE ekpo-matnr,           "Material code
           mtart  LIKE mara-mtart,           "Material type
* GR related info
           gramt  LIKE ekpo-netwr,           "GR Value
           mblnr  LIKE mseg-mblnr,           "GR No
           mjahr  LIKE mseg-mjahr,           "GR YEAR
           zeile  LIKE mseg-zeile,           "GR Item
* IR related info
*           bukrs LIKE  bseg-bukrs,           "IR Docno
           gjahr LIKE  rbkp-gjahr,           "IR YEAR
           belnr LIKE  rbkp-belnr,           "IR Docno
           buzei LIKE  bseg-buzei,           "IR ITEM
           dmbtr LIKE  bseg-dmbtr,           "IR ITEM
           meins LIKE  mara-meins,
        END   OF itab.
DATA : it_bseg TYPE bseg OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF iekbe OCCURS 0,
        ebeln   LIKE  ekbe-ebeln,
        ebelp   LIKE  ekbe-ebelp,
        lifnr  LIKE ekko-lifnr,           "Vendor code
           name1  LIKE lfa1-name1,           "Vendor Name
           matnr  LIKE ekpo-matnr,           "Material code
           mtart  LIKE mara-mtart,           "Material type
        maktx   LIKE  makt-maktx,
        vgabe   LIKE  ekbe-vgabe,
*GR
        gjahr   LIKE  ekbe-gjahr,
        belnr   LIKE  ekbe-belnr,
        buzei   LIKE  ekbe-buzei,
        dmbtr   LIKE  ekbe-dmbtr,
        menge   LIKE  ekbe-menge,
        meins   LIKE  mara-meins,
        budat   LIKE  ekbe-budat,
        bewtp   LIKE  ekbe-bewtp,
*IR
        gjahr1  LIKE  ekbe-gjahr,
        belnr1  LIKE  ekbe-belnr,
        buzei1  LIKE  ekbe-buzei,
*Invoice
        gjahr2  LIKE  bkpf-gjahr,
        belnr2  LIKE  bkpf-belnr,
        buzei2  LIKE  bseg-buzei,
        invdt   LIKE  bkpf-budat,

* Invoice details
        irbvl  LIKE  bseg-wrbtr,       "IR Basic value
        waers  LIKE  ekbe-waers,
        irlvl  LIKE  bseg-dmbtr,
        paymt  LIKE  bseg-dmbtr,
        paydt  LIKE  bkpf-budat,
        inddt  LIKE  bkpf-budat,
        indudt  LIKE  bkpf-budat,
        accqt  TYPE  mseg-menge,
        shpdt  TYPE  mkpf-budat,
      END   OF iekbe.
DATA : tekbe LIKE iekbe OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF iheader  OCCURS 1,
           bukrs  LIKE  bseg-bukrs,           "Company code
           gjahr  LIKE  ekbe-gjahr,           "GR YEAR
           werks  LIKE ekpo-werks,
      END   OF iheader.

*----------------------------------------------------------------------*
*       3 :   Data Declaration                                         *
*----------------------------------------------------------------------*
TYPE-POOLS : slis.
DATA: fieldtab    TYPE slis_t_fieldcat_alv,
       heading    TYPE slis_t_listheader,
       layout     TYPE slis_layout_alv,
       events     TYPE slis_t_event,
       repname    LIKE sy-repid,
*       f2code     LIKE sy-ucomm VALUE  '&ETA',
       g_save(1)  TYPE c,
       g_variant  LIKE disvariant,
       gx_variant LIKE disvariant,
       g_exit(1)  TYPE c.
DATA: keyinfo     TYPE slis_keyinfo_alv.

CONSTANTS:formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE',
          formname_user_command TYPE slis_formname VALUE 'USER_COMMAND'.

DATA: alv_print        TYPE slis_print_alv.
DATA: alv_detail_func(30).
DATA : p_vari LIKE disvariant-variant.
DATA : it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       is_layout TYPE slis_layout_alv,
        i_repid   LIKE sy-repid,
       it_events TYPE slis_t_event,
       it_listheader TYPE slis_t_listheader.
DATA : text(100).
TYPES: BEGIN OF t_varinfo,
       flag TYPE c,

       olength TYPE x,

        line LIKE raldb-infoline,

END OF t_varinfo.
DATA: tables TYPE trdir-name OCCURS 0 WITH HEADER LINE ,

infotab TYPE t_varinfo OCCURS 0 WITH HEADER LINE,

variant_info TYPE rsvaradmin OCCURS 0 WITH HEADER LINE ,

variant_names TYPE rsvarrange OCCURS 0 WITH HEADER LINE .

***** Start Code: Added by CS on 03.11.2015 for Authorization. *****
DATA: lv_bukrs_auth_flg TYPE c VALUE '',  " Auth. Flag for Company Code
      lv_werks_auth_flg TYPE c VALUE ''  " Auth. Flag for Plant
.
***** End Code: Added by CS on 03.11.2015 for Authorization. *****

*----------------------------------------------------------------------*
*       4 :   Selection Screen                                         *
*----------------------------------------------------------------------*
SELECTION-SCREEN    BEGIN OF BLOCK  s1 WITH FRAME TITLE text-001 .
SELECT-OPTIONS:   s_bukrs FOR t001-bukrs OBLIGATORY,
                  s_werks FOR t001w-werks,
                  s_mtart FOR t134-mtart,
                  s_bedat FOR sy-datum,
                  s_ebeln FOR ekko-ebeln ,
                  s_lifnr FOR ekko-lifnr ,
                  s_mwskz FOR t007a-mwskz,
                  s_budat FOR ekbe-budat.
SELECTION-SCREEN    END    OF BLOCK  s1 .

SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-002.
PARAMETERS: p_var  LIKE disvariant-variant.    " ALV Variant
SELECTION-SCREEN END OF BLOCK s2.

*----------------------------------------------------------------------*
*         :   Initialization                                           *
*----------------------------------------------------------------------*
INITIALIZATION.


  i_repid = sy-repid.
  PERFORM initialize_variant.


*----------------------------------------------------------------------*
*                   at selection-screen.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  p_vari = p_var.
  PERFORM f4_for_variant.
  p_var = p_vari.

AT SELECTION-SCREEN.
  i_repid = sy-repid.
  p_vari = p_var.
  PERFORM pai_of_selection_screen.


*----------------------------------------------------------------------*
*         :   Start of Selection                                       *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM check_auth_obj.  " Added by CS on 03.11.2015 for Authorization.

  PERFORM get_data.

*  PERFORM display-output TABLES itab.
***** Start Code: Added by CS on 03.11.2015 for Authorization Message. *****
*  PERFORM display_data. " Commented by CS
  IF iekbe[] IS NOT INITIAL.
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Company Code/Plant.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
    PERFORM display_data.
  ELSE.
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'No Records Found or Missing Authorization for Company Code/Plant.' TYPE 'I'.
    ELSE.
      MESSAGE 'No Records Found !' TYPE 'I'.
    ENDIF.
    LEAVE LIST-PROCESSING.
  ENDIF.
***** End Code: Added by CS on 03.11.2015 for Authorization Message. *****

END-OF-SELECTION.
*  PERFORM write_data.
*----------------------------------------------------------------------*
*         :   End of Selection                                         *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*         :   Top of Page                                              *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM write_header.
*********************************************************** ************
*   F o r m     R o u t i n e s     S t a r t s     H e r e            *
*********************************************************** ************
FORM get_data.
  DATA: lv_name1  LIKE lfa1-name1,
        lv_subrc  LIKE sy-subrc.

  REFRESH: itab,iekbe.
  CLEAR  : itab,iekbe.

  SELECT ekko~ebeln ekko~lifnr ekko~bedat ekko~bukrs
         ekpo~ebelp ekpo~netwr ekpo~matnr ekpo~mwskz ekpo~werks
         mara~mtart mara~meins
         INTO CORRESPONDING FIELDS OF TABLE itab
         FROM ekko INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
         INNER JOIN mara ON ekpo~matnr = mara~matnr
                         WHERE ekko~ebeln IN s_ebeln
                         AND   ekko~bukrs IN s_bukrs
                         AND   ekko~bedat IN s_bedat
                         AND   ekko~lifnr IN s_lifnr
                         AND   ekpo~werks IN s_werks
                         AND   ekpo~mwskz IN s_mwskz
                         AND   mara~mtart IN s_mtart
                         AND   ekpo~loekz EQ space.

  IF sy-subrc NE 0.
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'. " Added by CS on 23.11.2015 for Disp Msg.
      MESSAGE 'No Records Found or Missing Authorization for Company Code/Plant.' TYPE 'I'.
    ELSE.
      MESSAGE i000 WITH 'No list generated'.
    ENDIF.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  SORT itab.
  LOOP AT itab.
*    REFRESH: iekbe.
*    CLEAR:   iekbe,ekbe.
    ON CHANGE OF itab-lifnr.
      SELECT SINGLE name1 INTO lv_name1 FROM lfa1
                                    WHERE lifnr EQ itab-lifnr.
    ENDON.



    MOVE lv_name1 TO itab-name1.
    SELECT * FROM ekbe APPENDING CORRESPONDING FIELDS OF TABLE iekbe
                              WHERE ebeln EQ itab-ebeln
                              AND   ebelp EQ itab-ebelp
                              AND   vgabe EQ '1'
                              AND   bwart EQ '101'           "FOR GR
                              AND budat IN s_budat.

    SELECT * FROM ekbe APPENDING CORRESPONDING FIELDS OF TABLE tekbe
                             WHERE ebeln EQ itab-ebeln
                             AND   ebelp EQ itab-ebelp
                             AND   vgabe EQ '2'.

    MODIFY itab.
    CLEAR itab.
  ENDLOOP.
  PERFORM modify_iekbe.

*--------GR date logic
  IF NOT s_budat IS INITIAL.
    DELETE iekbe WHERE budat IS INITIAL.
  ENDIF.

ENDFORM.                    "get_data

*---------------------------------------------------------------------*
*       FORM Write_Data                                               *
*---------------------------------------------------------------------*
FORM write_data.

*  SORT itab.
*  SORT iekbe.
*  LOOP AT itab.
*    on change of itab-bukrs or itab-werks.
*      FORMAT COLOR 5 ON RESET INTENSIFIED OFF.
*       write:/2 'Company code:' No-gap, ITAB-BUKRS NO-GAP,
*              30  'Plant       :' No-gap, ITAB-WERKS NO-GAP.
*       WRITE:/ SY-ULINE.
*    endon.
*      FORMAT COLOR 2 ON INTENSIFIED OFF.
*    WRITE:/1 '|' NO-GAP,
*          (2) itab-mwskz NO-GAP, '|' NO-GAP,
*         (10) itab-bedat NO-GAP, '|' NO-GAP,
*         (10) itab-ebeln NO-GAP, '|' NO-GAP,
*          (5) itab-ebelp NO-GAP, '|' NO-GAP,
*         (17) itab-netwr NO-GAP, '|' NO-GAP,
*         (10) itab-lifnr NO-GAP, '|' NO-GAP,
*         (35) itab-name1 NO-GAP, '|' NO-GAP.
*    PERFORM write_grir_det .
*    FORMAT COLOR 2 OFF.
*    AT LAST.
*      WRITE:/ sy-uline.
*    ENDAT.
*  ENDLOOP.

ENDFORM.                    "write_data

*---------------------------------------------------------------------*
*       FORM Write_Header                                             *
*---------------------------------------------------------------------*
FORM write_header.
  SELECT SINGLE * FROM t001 WHERE bukrs EQ itab-bukrs.
  WRITE:/2 'Purchase Payments'.
  WRITE:/2 'Company code:', (5) t001-bukrs,
           50 'Name :', t001-butxt.
  WRITE:/2 'User        :', sy-uname,
           50 'Report run date: ', sy-datum.

  WRITE:/ sy-uline.
  FORMAT COLOR 1 ON.
  WRITE:/1 '|' NO-GAP,
         text-005  NO-GAP, '|' NO-GAP,
         text-011  NO-GAP,
*         text-012  NO-GAP, '|' NO-GAP,
         text-013  NO-GAP, '|' NO-GAP,
         text-014  NO-GAP, '|' NO-GAP,
         text-015  NO-GAP, 209 '|' NO-GAP.

  WRITE:/1 '|' NO-GAP,
         97 '|' NO-GAP,  108 '|' NO-GAP,
        126 '|' NO-GAP,  137 '|' NO-GAP,
        text-016  NO-GAP, '|' NO-GAP,
        text-017  NO-GAP, '|' NO-GAP,
        text-018  NO-GAP, '|' NO-GAP,
        text-019  NO-GAP, '|' NO-GAP.
  FORMAT COLOR 1 OFF.
  WRITE:/ sy-uline.
ENDFORM.                    "write_header
*&---------------------------------------------------------------------*
*&      Form  WRITE_GRIR_DET
*&---------------------------------------------------------------------*
FORM write_grir_det.


ENDFORM.                    " WRITE_GRIR_DET
*&---------------------------------------------------------------------*
*&      Form  MODIFY_IEKBE
*&---------------------------------------------------------------------*
FORM modify_iekbe.
  DATA: lv_subrc  LIKE sy-subrc,
        lv_awkey LIKE bkpf-awkey.
  DATA : v_tabix TYPE sy-tabix.
  SORT iekbe.
  LOOP AT iekbe.
    SELECT SINGLE * FROM rseg CLIENT SPECIFIED
                           WHERE ebeln EQ iekbe-ebeln
                           AND   ebelp EQ iekbe-ebelp
                           AND   lfbnr EQ iekbe-belnr
                           AND   lfgja EQ iekbe-gjahr
                           AND   lfpos EQ iekbe-buzei.
    IF sy-subrc EQ 0.
      MOVE rseg-belnr TO iekbe-belnr1.
      MOVE rseg-gjahr TO iekbe-gjahr1.
      MOVE rseg-buzei TO iekbe-buzei1.
      MODIFY iekbe .
      CLEAR  iekbe.
    ENDIF.
  ENDLOOP.
  LOOP AT iekbe.
    IF NOT iekbe-belnr1 IS INITIAL.
      READ TABLE tekbe WITH KEY belnr = iekbe-belnr1
                                gjahr = iekbe-gjahr1
                                buzei = iekbe-buzei1.
      IF sy-subrc EQ 0.
        DELETE tekbe INDEX sy-tabix.
        CLEAR  tekbe.

      ENDIF.
    ENDIF.
  ENDLOOP.
  APPEND LINES OF tekbe TO iekbe.
  SORT iekbe.
  LOOP AT iekbe.
    v_tabix = sy-tabix.
* CHECK FOR GR Cancellation
    lv_subrc = 2.
    PERFORM check_gr_cancel CHANGING lv_subrc.
    IF lv_subrc EQ 0.
      DELETE iekbe.
      CHECK lv_subrc EQ 2.
    ENDIF.
    CLEAR itab.
    READ TABLE itab WITH KEY ebeln = iekbe-ebeln
                             ebelp = iekbe-ebelp.
    IF sy-subrc EQ 0.
      MOVE : itab-lifnr TO iekbe-lifnr,
             itab-name1 TO iekbe-name1,
             itab-meins TO iekbe-meins,
             itab-mtart TO iekbe-mtart.

    ENDIF.
    CLEAR: rseg,bkpf,bset.
    SELECT SINGLE maktx FROM makt INTO iekbe-maktx
                                  WHERE matnr EQ itab-matnr
                                    AND spras EQ sy-langu.

    SELECT SINGLE lmenge01 FROM qals INTO iekbe-accqt
                                     WHERE mblnr EQ iekbe-belnr
                                       AND mjahr EQ iekbe-gjahr
                                       AND zeile EQ iekbe-buzei.

    SELECT SINGLE * FROM rseg CLIENT SPECIFIED
                            WHERE ebeln EQ iekbe-ebeln
                            AND   ebelp EQ iekbe-ebelp
                            AND   lfbnr EQ iekbe-belnr
                            AND   lfgja EQ iekbe-gjahr
                            AND   lfpos EQ iekbe-buzei.
    IF sy-subrc EQ 0.
      MOVE rseg-belnr TO iekbe-belnr1.
      MOVE rseg-gjahr TO iekbe-gjahr1.
      MOVE rseg-buzei TO iekbe-buzei1.
*      MOVE rseg-wrbtr TO iekbe-irbvl.


      CLEAR bkpf.
      CONCATENATE rseg-belnr rseg-gjahr INTO lv_awkey.
    ELSE.

      CONCATENATE iekbe-belnr iekbe-gjahr INTO lv_awkey.

    ENDIF.
    IF iekbe-vgabe EQ '2'.
      CLEAR : iekbe-belnr,
              iekbe-gjahr,
              iekbe-buzei,
              iekbe-dmbtr,
              iekbe-budat,
              iekbe-menge.
    ENDIF.
    SELECT SINGLE * FROM bkpf CLIENT SPECIFIED
                          WHERE mandt EQ sy-mandt
                          AND   awtyp EQ 'RMRP'
                          AND   awkey EQ lv_awkey
                          AND   stblg EQ space.


    MOVE bkpf-belnr TO iekbe-belnr2.
    MOVE bkpf-gjahr TO iekbe-gjahr2.
    SELECT *  FROM bseg INTO CORRESPONDING FIELDS OF TABLE it_bseg
                         WHERE bukrs EQ bkpf-bukrs
                           AND belnr EQ iekbe-belnr2
                           AND gjahr EQ iekbe-gjahr2
                           AND koart EQ 'K'.
    LOOP AT it_bseg.
      iekbe-irlvl = it_bseg-dmbtr + iekbe-irlvl.
      iekbe-irbvl = it_bseg-wrbtr + iekbe-irbvl.
      IF NOT it_bseg-augbl IS INITIAL.
        iekbe-paymt = iekbe-paymt + it_bseg-dmbtr.
        iekbe-paydt = it_bseg-augdt.
      ENDIF.
      iekbe-inddt = it_bseg-zfbdt.

*    -----inv.due date = baseline date + payment tems

      iekbe-indudt  = iekbe-inddt + it_bseg-zbd1t.
    ENDLOOP.

    MOVE bkpf-budat TO iekbe-invdt.



*    ENDIF.
    MODIFY iekbe INDEX v_tabix..
    CLEAR  iekbe.
  ENDLOOP.


ENDFORM.                    " MODIFY_IEKBE
*&---------------------------------------------------------------------*
*&      Form  CHECK_GR_CANCEL
*&---------------------------------------------------------------------*
FORM check_gr_cancel CHANGING pc_subrc LIKE sy-subrc.
  CLEAR: mseg.
  pc_subrc = 2.
  SELECT SINGLE * FROM mseg
         WHERE mseg~smbln EQ iekbe-belnr
           AND mseg~sjahr EQ iekbe-gjahr
           AND mseg~smblp EQ iekbe-buzei.

  pc_subrc = sy-subrc.
*           AND matnr IN matnr
*           AND werks IN werks
*           AND budat IN budat
*           AND usnam IN usnam.

ENDFORM.                    " CHECK_GR_CANCEL
*&---------------------------------------------------------------------*
*&      Form  display-output
*&---------------------------------------------------------------------*
FORM display-output TABLES   p_itab STRUCTURE itab.

ENDFORM.                    " display-output

*&---------------------------------------------------------------------*
*&      Form  collect_fieldtab
*&---------------------------------------------------------------------*
FORM display_data.


  PERFORM fill_fieldtabnew(z6sdinc_alv1) USING :
    'IEKBE' 'EBELN' 'Purchase Order'   'EBELN'   'EKKO'  1 fieldtab,
    'IEKBE' 'EBELP' 'PO.Item  '   'EBELP'   'EKPO' 2  fieldtab,
    'IEKBE' 'LIFNR' 'Vendor'   'LIFNR'   'EKKO' 3 fieldtab,
    'IEKBE' 'NAME1' 'Vendor Name'   'NAME1'   'LFA1' 4 fieldtab,
    'IEKBE' 'MATNR' 'Material Code'   'MATNR'   'MARA' 5 fieldtab,
    'IEKBE' 'MAKTX' 'Material Desc.'   'MAKTX'   'MAKT' 6 fieldtab,
    'IEKBE' 'MTART' 'Material Type'   'MTART'   'MARA' 7 fieldtab,
    'IEKBE' 'BELNR' 'GR.Doc'   'MBLNR'   'MKPF' 8 fieldtab,
    'IEKBE' 'BUZEI' 'GR.Item'   'ZEILE'   'MSEG' 9 fieldtab,
    'IEKBE' 'GJAHR' 'GR.Year'   'MJAHR'   'MKPF' 10 fieldtab,
    'IEKBE' 'DMBTR' 'GR.Value'   'DMBTR'   'MSEG' 11 fieldtab,
    'IEKBE' 'MENGE' 'GR.Qty'   'MENGE'   'MSEG' 12 fieldtab,
    'IEKBE' 'ACCQT' 'Accp.Qty'   'MENGE'   'MSEG' 13 fieldtab,
    'IEKBE' 'SHPDT' 'Ship.Date'   'BUDAT'   'MKPF' 14 fieldtab,
    'IEKBE' 'BUDAT' 'GR.Date'   'BUDAT'   'EKBE' 15 fieldtab,
    'IEKBE' 'BELNR2' 'Invoice.Doc'   'BELNR'   'BKPF' 16 fieldtab,
    'IEKBE' 'GJAHR2' 'Invoice Year'   'GJAHR'   'BKPF' 17 fieldtab,
    'IEKBE' 'INVDT' 'Inv.Post date'   'BUDAT'   'EKBE' 18 fieldtab,
    'IEKBE' 'IRBVL' 'Inv.Amt(DOC.Curr)'   'WRBTR'   'EKBE' 19 fieldtab,
    'IEKBE' 'IRLVL' 'Inv.Amt(Loc.Curr)'   'DMBTR'   'EKBE' 20 fieldtab,
    'IEKBE' 'INDDT' 'Inv.Base Dt'   'BUDAT'   'EKBE' 21 fieldtab,
    'IEKBE' 'INDUDT' 'Inv.Due Dt'   'BUDAT'   'EKBE' 21 fieldtab,
    'IEKBE' 'PAYDT' 'Payment.Date'   'BUDAT'   'EKBE' 22 fieldtab,
    'IEKBE' 'PAYMT' 'Payment.Amount'   'DMBTR'   'EKBE' 23 fieldtab,
    'IEKBE' 'WAERS' 'Curr'   'WAERS'   'MSEG' 24 fieldtab,
    'IEKBE' 'MEINS' 'UOM'   'MEINS'   'MSEG' 25 fieldtab.

  text = 'Purchase Order Payments'.

  PERFORM build_layout.
  PERFORM build_events CHANGING it_events[].
  PERFORM build_comment CHANGING it_listheader[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                = ' '
*   I_BUFFER_ACTIVE                   = ' '
      i_callback_program                = i_repid
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
*   I_STRUCTURE_NAME                  =
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
      is_layout                         = is_layout
      it_fieldcat                       = fieldtab
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*    I_DEFAULT                         = 'A'
      i_save                            = 'A'
      is_variant                        = g_variant
      it_events                         = it_events
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   I_HTML_HEIGHT_TOP                 = 0
*   I_HTML_HEIGHT_END                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_HYPERLINK                      =
*   IT_ADD_FIELDCAT                   =
*   IT_EXCEPT_QINFO                   =
*   IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab                          = iekbe
* EXCEPTIONS
*   PROGRAM_ERROR                     = 1
*   OTHERS                            = 2
              .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " collect_fieldtab

*&---------------------------------------------------------------------*
*&      Form  pai_of_selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pai_of_selection_screen.
*
  IF NOT p_vari IS INITIAL.
    MOVE g_variant TO gx_variant.
    MOVE p_vari TO gx_variant-variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant.
    g_variant = gx_variant.
  ELSE.
    PERFORM initialize_variant.
  ENDIF.
ENDFORM.                               " PAI_OF_SELECTION_SCREEN


*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events CHANGING lt_events TYPE slis_t_event.

  DATA : it_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = lt_events.

  READ TABLE it_events WITH KEY name = slis_ev_top_of_page
                                       INTO it_event.

  IF sy-subrc = 0.
    MOVE formname_top_of_page TO it_event-form.
    APPEND it_event TO lt_events.
  ENDIF.



ENDFORM.                    " build_events

*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_comment CHANGING lt_listheader TYPE slis_t_listheader.

  DATA : line TYPE slis_listheader.
*       text(100).


  CLEAR line.
  line-typ = 'H'.
  line-info = text.
  APPEND line TO lt_listheader.

  IF NOT gx_variant-text IS INITIAL.
    CLEAR line.
    line-typ = 'S'.
    line-info = gx_variant-text.
    APPEND line TO lt_listheader.
  ENDIF.

  DELETE infotab WHERE LINE IS INITIAL.
  LOOP AT infotab.


    CLEAR line.
    line-typ = 'S'.
    CONDENSE infotab-line.
    line-info = infotab-line.

    APPEND line TO lt_listheader.


  ENDLOOP.

*clear line.
*clear text.
*line-typ = 'S'.
*line-key = ''.
*line-info = ''.
*append line to lt_listheader.

ENDFORM.                    " build_comment

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_listheader.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_layout.
  is_layout-zebra = 'X'.
  is_layout-detail_popup = 'X'.
  is_layout-colwidth_optimize = 'X'.
ENDFORM.                    "build_layout
*&---------------------------------------------------------------------*
*&      Form  initialize_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM initialize_variant.
  g_save = 'A'.
  CLEAR g_variant.
  g_variant-report = i_repid.
  gx_variant = g_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.
ENDFORM.                               " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form F4_FOR_VARIANT.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
FORM f4_for_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
*    MESSAGE ID SY-MSGID TYPE 'S'      NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.                    "f4_for_variant

*I001     Basic Selection Criteria                                                                       24
*I002     Layout Selection                                                                               16
*I005     Tx                                                                                              2
*I006                                                                                                     1
*I007                                                                                                     1
*I008                                                                                                     1
*I009                                                                                                     1
*I010                                                                                                     1
*I011     PO Dt.    | PO Number| Item|PO item value    | Vendor   | Vendor Name                         104 oc   |
*I012                                                                                                     1
*I013     GR Amount                                                                                       9
*I014     IR Doc                                                                                          6
*I015                  Invoice Amount Break - Up                                                         38
*I016     Basic Amount                                                                                   12
*I017     Excise value                                                                                   12
*I018     Sales tax                                                                                       9
*I019     Transport Amt                                                                                  13
*R        Purchase register for RM and PM                                                                31
*SP_VARI  D       Layout                                                                                 14
*SS_BEDAT D       Current date                                                                           20
*SS_BUKRS D       Company code                                                                           20
*SS_EBELN D       Purchasing document                                                                    27
*SS_LIFNR D       Vendor                                                                                 14
*SS_MTART D       Material type                                                                          21
*SS_MWSKZ D       Tax code                                                                               16
*SS_WERKS D       Plant                                                                                  13
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 03.11.2015 for Authorization
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

***** Start Code: Added by CS on 03.11.2015 for Plant Authorization. *****
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
***** End Code: Added by CS on 03.11.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 03.11.2015 for Company Code Authorization. *****
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
***** End Code: Added by CS on 03.11.2015 for Company Code Authorization. *****
ENDFORM.                    " CHECK_AUTH_OBJ
