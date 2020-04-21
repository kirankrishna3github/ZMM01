*&---------------------------------------------------------------------*
*& Report  Z6PP007C_TG_STK_UPL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6mm020r_port_qty_details.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Import Purchase Status
* OBJECT TYPE       : Report            FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 10.08.2010
*        DEV REQUEST: IIRDK900911
*             TCODE : ZMM0020
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
*   CHANGE ON: 04.11.2015
*   REASON FOR CHANGE: Add Authorization & Layout
*   REQUEST #: IRDK913076
* --------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
*     TABLES6
*----------------------------------------------------------------------*
TABLES : ekko,ekpo.
TYPE-POOLS : slis,truxs.

*----------------------------------------------------------------------*
*     SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK s1 WITH FRAME TITLE text-001 .
PARAMETERS: p_qty  RADIOBUTTON GROUP rad1 DEFAULT 'X' ,
            p_val   RADIOBUTTON GROUP rad1 .
SELECTION-SCREEN END   OF BLOCK s1 .
SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-002 .

SELECT-OPTIONS: s_bukrs FOR ekko-bukrs,
                s_lifnr FOR ekko-lifnr  ,
                s_ekgrp FOR ekko-ekgrp,
                s_matnr FOR ekpo-matnr,
                s_werks FOR ekpo-werks,
                s_bedat FOR ekko-bedat,
                s_ebeln FOR ekko-ebeln.
PARAMETERS : p_var TYPE disvariant-variant MODIF ID pk. " Added by CS on 04.11.2015 for Layout
SELECTION-SCREEN END OF BLOCK s2 .


*----------------------------------------------------------------------*
*     INTERNAL TABLES
*----------------------------------------------------------------------*


DATA :  v_message TYPE lvc_title.
DATA : it_ekko TYPE ekko_tty.
DATA : wa_ekko TYPE ekko.
DATA : it_itab TYPE zztt_imp_po_status.
DATA : wa_itab TYPE zzst_imp_po_status.
DATA : it_mard TYPE  mard_tt.
DATA : wa_mard TYPE mard.
DATA : v_tabix TYPE sy-tabix.
DATA : lv_stock TYPE labst,
       lv_exch_rate TYPE bapi1093_0,
       lv_kzwi2 TYPE kzwi2.
DATA : lv_ebeln TYPE bapimepoheader-po_number,
       lv_header TYPE bapimepoheader,
       it_poitem LIKE bapimepoitem OCCURS 0 WITH HEADER LINE,
       it_pocond LIKE bapimepocond OCCURS 0 WITH HEADER LINE,
       it_poconfirmation LIKE bapiekes OCCURS 0  WITH HEADER LINE,
       it_pohistory_totals LIKE  bapiekbes OCCURS 0 WITH HEADER LINE,
       it_pohistory LIKE bapiekbe OCCURS 0 WITH HEADER LINE.
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

*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION .
  PERFORM check_auth_obj.  " Added by CS on 04.11.2015 for Authorization.

*  IF p_fore EQ 'X' .
*    v_mode = 'A' .
*  ELSE .
*    v_mode = 'E' .
*  ENDIF .
  PERFORM  f_get_data  .

  IF NOT it_itab IS INITIAL.
***** Start Code: Added by CS on 04.11.2015 for Authorization Message. *****
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Company Code/Plant.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
***** End Code: Added by CS on 04.11.2015 for Authorization Message. *****
    PERFORM  disp_records.
***** Start Code: Added by CS on 04.11.2015 for Authorization Message. *****
  ELSE.
    IF lv_werks_auth_flg = 'X' OR lv_bukrs_auth_flg = 'X'.
      MESSAGE 'No Records Found or Missing Authorization for Company Code/Plant.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.
      MESSAGE 'No Records Found !' TYPE 'I'.
    ENDIF.

***** End Code: Added by CS on 04.11.2015 for Authorization Message. *****
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  I_UPLOAD
*&---------------------------------------------------------------------*
FORM f_get_data.

  IF NOT s_bedat[] IS INITIAL.

    SELECT ebeln bsart FROM ekko INTO CORRESPONDING FIELDS OF TABLE it_ekko
                                WHERE bstyp = 'F'
                                  AND bedat IN s_bedat
                                  AND bukrs IN s_bukrs
                                  AND lifnr IN s_lifnr
                                  AND ebeln IN s_ebeln.
  ELSEIF NOT s_lifnr[] IS INITIAL.

    SELECT ebeln  bsart FROM ekko INTO CORRESPONDING FIELDS OF TABLE it_ekko
                             WHERE lifnr IN s_lifnr
                               AND ekorg EQ '1000'
                               AND ekgrp IN s_ekgrp
                               AND bedat IN s_bedat
                               AND bukrs IN s_bukrs
                               AND ebeln IN s_ebeln.
  ELSEIF NOT s_ebeln[] IS INITIAL.
    SELECT ebeln bsart FROM ekko INTO CORRESPONDING FIELDS OF TABLE it_ekko
                         WHERE ebeln IN s_ebeln
                           AND bukrs IN s_bukrs
                           AND lifnr IN s_lifnr
                           AND ekgrp IN s_ekgrp
                           AND bedat IN s_bedat
                           AND ebeln IN s_ebeln.

  ELSE.
    SELECT ebeln  bsart FROM ekko INTO CORRESPONDING FIELDS OF TABLE it_ekko
                                WHERE ebeln IN s_ebeln
                                  AND bukrs IN s_bukrs
                                  AND lifnr IN s_lifnr
                                  AND ekgrp IN s_ekgrp
                                  AND bedat IN s_bedat
                                  AND ebeln IN s_ebeln.

  ENDIF.
  DELETE it_ekko WHERE bsart NE 'ZIMP' AND bsart NE 'YIMP'.

  LOOP AT it_ekko INTO wa_ekko.

    lv_ebeln = wa_ekko-ebeln.
    REFRESH : it_poitem,it_pohistory_totals,it_poconfirmation.
    CLEAR : it_poitem,it_pohistory_totals,it_poconfirmation.
    CALL FUNCTION 'BAPI_PO_GETDETAIL1'
      EXPORTING
        purchaseorder            = lv_ebeln
*          ACCOUNT_ASSIGNMENT       = ' '
*          ITEM_TEXT                = ' '
*          HEADER_TEXT              = ' '
*          DELIVERY_ADDRESS         = ' '
*          VERSION                  = ' '
*          SERVICES                 = ' '
*          SERIALNUMBERS            = ' '
*          INVOICEPLAN              = ' '
      IMPORTING
        poheader                 = lv_header
*          POEXPIMPHEADER           =
      TABLES
*          RETURN                   =
        poitem                   = it_poitem
*          POADDRDELIVERY           =
*          POSCHEDULE               =
*          POACCOUNT                =
*          POCONDHEADER             =
       pocond                   = it_pocond
*          POLIMITS                 =
*          POCONTRACTLIMITS         =
*          POSERVICES               =
*          POSRVACCESSVALUES        =
*          POTEXTHEADER             =
*          POTEXTITEM               =
*          POEXPIMPITEM             =
*          POCOMPONENTS             =
*          POSHIPPINGEXP            =
        pohistory                = it_pohistory
        pohistory_totals         = it_pohistory_totals
        poconfirmation           = it_poconfirmation
*          ALLVERSIONS              =
*          POPARTNER                =
*          EXTENSIONOUT             =
*          SERIALNUMBER             =
*          INVPLANHEADER            =
*          INVPLANITEM              =
*          POHISTORY_MA             =
              .
    LOOP AT it_poitem WHERE plant IN s_werks
                        AND material IN s_matnr.
      MOVE : lv_ebeln TO wa_itab-ebeln,
             it_poitem-po_item TO wa_itab-ebelp,
             it_poitem-material TO wa_itab-matnr,
             it_poitem-short_text TO wa_itab-txz01,
             it_poitem-quantity TO wa_itab-menge,
             it_poitem-po_unit TO wa_itab-meins,
             it_poitem-plant TO wa_itab-werks,
             lv_header-currency TO wa_itab-waers,
             lv_header-vendor TO wa_itab-lifnr.

      SELECT SINGLE name1 FROM lfa1 INTO wa_itab-name1
                               WHERE lifnr = wa_itab-lifnr.
      CLEAR wa_itab-quantity.
      LOOP AT it_poconfirmation WHERE   po_item = wa_itab-ebelp.

        wa_itab-quantity = wa_itab-quantity +  it_poconfirmation-quantity.

      ENDLOOP.

**      Quantity at port 107 movement .
**      Quantity rejected at port 108 movement - rev of 107 .
**      Balance(107-108) Balance at Port
**      Qty Received at Plant (109)
**      Qty Rej at plant (110) - rev of 109
**      Balance Qty at Port (107-108) + 110

        LOOP AT it_pohistory WHERE po_item = wa_itab-ebelp
                                   AND move_type EQ '107'
                                    OR move_type EQ '108'
                                    OR move_type EQ '109'
                                    OR move_type EQ '110'.

        IF it_pohistory-db_cr_ind = 'H'.
          it_pohistory-deliv_qty = it_pohistory-deliv_qty * -1.
          it_pohistory-quantity = it_pohistory-quantity * -1.

        ENDIF.
        CASE it_pohistory-move_type.
          WHEN '107'.
            wa_itab-qty_107 = wa_itab-qty_107 + it_pohistory-deliv_qty.
          WHEN '108'.
            wa_itab-qty_108 = wa_itab-qty_108 + it_pohistory-deliv_qty.
          WHEN '109'.
            wa_itab-qty_109 = wa_itab-qty_109 + it_pohistory-quantity.
          WHEN '110'.
            wa_itab-qty_110 = wa_itab-qty_110 + it_pohistory-quantity.
          WHEN OTHERS.
        ENDCASE.

        endloop.

        wa_itab-blocked_qy = wa_itab-qty_107_108 = wa_itab-qty_107 + wa_itab-qty_108.
        wa_itab-qty_109_110 = wa_itab-qty_109 + wa_itab-qty_110.
        wa_itab-balpt = wa_itab-qty_107_108 - wa_itab-qty_109_110.
*        wa_itab-quantity =  wa_itab-quantity + wa_itab-qty_107_108.
        wa_itab-deliv_qty = wa_itab-qty_109.

*********************************************************************************
      READ TABLE it_pohistory_totals WITH KEY po_item = wa_itab-ebelp.
      IF sy-subrc EQ 0.


*        wa_itab-blocked_qy = it_pohistory_totals-blocked_qy + it_pohistory_totals-deliv_qty.
*        wa_itab-deliv_qty =  it_pohistory_totals-deliv_qty.
        IF NOT wa_itab-quantity IS INITIAL.
          wa_itab-balpo = wa_itab-menge - wa_itab-quantity.
        ELSE.
          wa_itab-balpo = wa_itab-menge - wa_itab-blocked_qy.
        ENDIF.
*        wa_itab-balpt = it_pohistory_totals-blocked_qy .
        SELECT SINGLE kzwi2 FROM ekpo INTO lv_kzwi2 WHERE ebeln = wa_ekko-ebeln
                                                      AND ebelp = wa_itab-ebelp.
        CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
          EXPORTING
            rate_type        = 'M'
            from_curr        = lv_header-currency
            to_currncy       = 'INR'
            date             = sy-datum
          IMPORTING
            exch_rate        = lv_exch_rate
*                     RETURN           =
                  .
        wa_itab-wkurs = lv_exch_rate-exch_rate.

        wa_itab-cifvl = ( wa_itab-balpt / wa_itab-menge ) * lv_kzwi2.

        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
         EXPORTING
*           CLIENT                  = SY-MANDT
           date                    =  sy-datum
           foreign_amount          =  wa_itab-cifvl
           foreign_currency        =  wa_itab-waers
           local_currency          =  'INR'
*           RATE                    = 0
           type_of_rate            = 'M'
*           READ_TCURR              = 'X'
         IMPORTING
           exchange_rate           =  wa_itab-wkurs
*           FOREIGN_FACTOR          =
           local_amount            = wa_itab-cifrl
*           LOCAL_FACTOR            =
*           EXCHANGE_RATEX          =
*           FIXED_RATE              =
*           DERIVED_RATE_TYPE       =
*         EXCEPTIONS
*           NO_RATE_FOUND           = 1
*           OVERFLOW                = 2
*           NO_FACTORS_FOUND        = 3
*           NO_SPREAD_FOUND         = 4
*           DERIVED_2_TIMES         = 5
*           OTHERS                  = 6
                 .
        IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

*        WA_ITAB-CIFRL = WA_ITAB-WKURS * WA_ITAB-BALPT.
      ENDIF.
      LOOP AT it_pohistory WHERE po_item = wa_itab-ebelp
                                   AND move_type EQ '103'
                                    OR move_type EQ '104'.


        SELECT SINGLE portd FROM z6mma_gt_ent_dt INTO wa_itab-portd
                                 WHERE mblnr EQ it_pohistory-mat_doc
                                   AND mjahr EQ it_pohistory-doc_year
                                   AND zeile EQ it_pohistory-matdoc_itm.

        IF sy-subrc EQ 0.
          SELECT SINGLE name1 FROM t001w INTO wa_itab-pname WHERE werks = wa_itab-portd.

        ENDIF.




      ENDLOOP.




      SELECT SINGLE portd FROM z6mma_gt_ent_dt INTO wa_itab-portd
                               WHERE mblnr EQ it_pohistory-mat_doc
                                 AND mjahr EQ it_pohistory-doc_year
                                 AND zeile EQ it_pohistory-matdoc_itm.

      IF sy-subrc EQ 0.
        SELECT SINGLE name1 FROM t001w INTO wa_itab-pname WHERE werks = wa_itab-portd.

      ENDIF.




    ENDLOOP.
    APPEND wa_itab TO it_itab.
    CLEAR wa_itab.

  ENDLOOP.



ENDFORM.                    " I_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
FORM check_data.





ENDFORM.                    " check_data

" run_bdc

*&---------------------------------------------------------------------*
*&      Form  disp_err
*&---------------------------------------------------------------------*
FORM disp_records.


  DATA: it_fieldcat TYPE slis_t_fieldcat_alv,

  wa_fieldcat TYPE LINE OF slis_t_fieldcat_alv,

  wa_layout TYPE slis_layout_alv,



  it_keyinfo TYPE TABLE OF slis_keyinfo_alv WITH HEADER LINE.



  v_message = 'Import PO Status'.

*wa_LAYOUT-BOX_FIELDNAME        = 'SELECTED'.  "field for checkbox

*  wa_LAYOUT-LIGHTS_FIELDNAME     = 'LIGHTS'.  "field for lights
* wa_LAYOUT-LIGHTS_ROLLNAME      = 'QVLOTSLIGHTS'.  "F1 help for lights

  wa_layout-get_selinfos         = 'X'. "show selection screen criteria
  wa_layout-detail_popup         = 'X'. "show detail via popup
  wa_layout-group_change_edit    = 'X'. "allows data to be grouped
  wa_layout-zebra                = 'X'. "striped pattern


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
    i_program_name               = sy-repid
*    I_INTERNAL_TABNAME           = 'IT_ITAB'
       i_structure_name             = 'ZZST_IMP_PO_STATUS'
*   I_CLIENT_NEVER_DISPLAY       = 'X'
*   I_INCLNAME                   =
     i_bypassing_buffer           = 'X'
      i_buffer_active              ='X'
    CHANGING
      ct_fieldcat                  = it_fieldcat[]
   EXCEPTIONS
     inconsistent_interface       = 1
     program_error                = 2
     OTHERS                       = 3
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = sy-repid
*      i_internal_tabname     = 'ID_T438R'
**      i_inclname             = sy-repid
*    CHANGING
*      ct_fieldcat            = it_fieldcat[]
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*
*  IF sy-subrc <> 0.
*
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ELSE.

    LOOP AT it_fieldcat INTO wa_fieldcat.

      IF wa_fieldcat-fieldname = 'BALPT'.
        wa_fieldcat-seltext_l = 'Balance Qty at Port'.
        wa_fieldcat-seltext_m = 'Balance Qty at Port'.
        wa_fieldcat-seltext_s = 'Balance Qty at Port'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.

      IF wa_fieldcat-fieldname = 'QUANTITY'.
        wa_fieldcat-seltext_l = 'Shipped from Vendor'.
        wa_fieldcat-seltext_m = 'Shipped from Vendor'.
        wa_fieldcat-seltext_s = 'Shipped from Vendor'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.

      IF wa_fieldcat-fieldname = 'BALPO'.
        wa_fieldcat-seltext_l = 'Balance PO Qty'.
        wa_fieldcat-seltext_m = 'Balance PO Qty'.
        wa_fieldcat-seltext_s = 'Balance PO Qty'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.

      IF wa_fieldcat-fieldname = 'BLOCKED_QY'.
        wa_fieldcat-seltext_l = 'Bal.Qty at Port(107-108)'.
        wa_fieldcat-seltext_m = 'Bal.Qty at Port(107-108)'.
        wa_fieldcat-seltext_s = 'Bal.Qty at Port(107-108)'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.
*
*      IF wa_fieldcat-fieldname = 'DELIV_QTY'.
*        wa_fieldcat-seltext_l = 'Qty Rec.at Plant'.
*        wa_fieldcat-seltext_m = 'Qty Rec.at Plant'.
*        wa_fieldcat-seltext_s = 'Qty Rec.at Plant'.
*        wa_fieldcat-ddictxt = 'M'.
*      ENDIF.

      IF wa_fieldcat-fieldname = 'CIFVL'.
        wa_fieldcat-seltext_l = 'CIF Value in Doc.Curr'.
        wa_fieldcat-seltext_m = 'CIF Value in Doc.Curr'.
        wa_fieldcat-seltext_s = 'CIF Value in Doc.Curr'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.

      IF wa_fieldcat-fieldname = 'CIFRL'.
        wa_fieldcat-seltext_l = 'CIF Value in loc.Curr'.
        wa_fieldcat-seltext_m = 'CIF Value in Loc.Curr'.
        wa_fieldcat-seltext_s = 'CIF Value in Loc.Curr'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.

      IF wa_fieldcat-fieldname = 'QTY_107'.
        wa_fieldcat-seltext_l = 'Qty at Port(107)'.
        wa_fieldcat-seltext_m = 'Qty at Port(107)'.
        wa_fieldcat-seltext_s = 'Qty at Port(107)'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.

      IF wa_fieldcat-fieldname = 'QTY_108'.
        wa_fieldcat-seltext_l = 'Qty Rejected(108)'.
        wa_fieldcat-seltext_m = 'Qty Rejected(108)'.
        wa_fieldcat-seltext_s = 'Qty Rejected(108)'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.

      IF wa_fieldcat-fieldname = 'QTY_109'.
        wa_fieldcat-seltext_l = 'Qty Received at Plant(109)'.
        wa_fieldcat-seltext_m = 'Qty Rec.at Plant(109)'.
        wa_fieldcat-seltext_s = 'Qty Rec.at Plant(109)'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.


      IF wa_fieldcat-fieldname = 'QTY_110'.
        wa_fieldcat-seltext_l = 'Qty Rejected at Plant(110)'.
        wa_fieldcat-seltext_m = 'Qty Rej.at Plant(110)'.
        wa_fieldcat-seltext_s = 'Qty Rej.at Plant(110)'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.


      IF wa_fieldcat-fieldname = 'QTY_109_110'.
        wa_fieldcat-seltext_l = 'Bal.Qty at Plant(109-110)'.
        wa_fieldcat-seltext_m = 'Bal.Qty at Plant(109-110)'.
        wa_fieldcat-seltext_s = 'Bal.Qty at Plant(109-110)'.
        wa_fieldcat-ddictxt = 'M'.
      ENDIF.




      IF p_qty IS INITIAL.
        IF wa_fieldcat-fieldname EQ 'QUANTITY' OR wa_fieldcat-fieldname EQ 'BLOCKED_QY'
                           OR wa_fieldcat-fieldname EQ 'DELIV_QTY'.

          wa_fieldcat-no_out = 'X'.


        ENDIF.
      ELSE.
        IF wa_fieldcat-fieldname EQ 'CIFVL' OR wa_fieldcat-fieldname EQ 'CIFRL'
                          OR wa_fieldcat-fieldname EQ 'WKURS'.

          wa_fieldcat-no_out = 'X'.


        ENDIF.

      ENDIF.

      MODIFY it_fieldcat FROM wa_fieldcat.
      CLEAR wa_fieldcat.
    ENDLOOP.

    delete it_fieldcat WHERE fieldname = 'QTY_107_108'.
*    delete it_fieldcat WHERE fieldname = 'QTY_109_110'.
    delete it_fieldcat WHERE fieldname = 'DELIV_QTY'.
*    delete it_fieldcat WHERE fieldname = 'BALPT'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
*       I_INTERFACE_CHECK                 = ' '
        i_bypassing_buffer                = 'X'
*       I_BUFFER_ACTIVE                   = ' '
       i_callback_program                = sy-repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME                  =
*       I_BACKGROUND_ID                   = ' '
        i_grid_title                      = v_message
*       I_GRID_SETTINGS                   =
        is_layout                         = wa_layout
        it_fieldcat                       = it_fieldcat[]
*       IT_EXCLUDING                      =
*       IT_SPECIAL_GROUPS                 =
*       IT_SORT                           =
*       IT_FILTER                         =
*       IS_SEL_HIDE                       =
        i_default                         = 'X'
        i_save                            = 'A'
*       IS_VARIANT                        =
*       IT_EVENTS                         =
*       IT_EVENT_EXIT                     =
*       IS_PRINT                          =
*       IS_REPREP_ID                      =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE                 = 0
*       I_HTML_HEIGHT_TOP                 = 0
*       I_HTML_HEIGHT_END                 = 0
*       IT_ALV_GRAPHICS                   =
*       IT_HYPERLINK                      =
*       IT_ADD_FIELDCAT                   =
*       IT_EXCEPT_QINFO                   =
*       IR_SALV_FULLSCREEN_ADAPTER        =
*     IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab                          = it_itab
     EXCEPTIONS
       program_error                     = 1
       OTHERS                            = 2
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


  ENDIF.

ENDFORM.                    " disp_err
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


*R        BDC For Create Strategy Plan (IP42)                                                            35
*SP_BACK          Back Ground                                                                            19
*SP_FORE          Fore Ground                                                                            19
