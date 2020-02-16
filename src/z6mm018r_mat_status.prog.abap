
REPORT  z6mm018r_mat_status.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Plant Material Stock Status
* OBJECT TYPE       : Report            FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 09.08.2010
*        DEV REQUEST: IRDK900884
*             TCODE : ZMM0018
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*        Changes made by :   Mrs.Puanam S   DATE:   28.07.2011
*        DESCRIPTION:   Status indicators cahnges as per requirement of Mr.D Srinivas
*----------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 12/10/2015
*   REASON FOR CHANGE: Add Authorization, layout and Variant
*   REQUEST #: IRDK913070
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 16/10/2015
*   REASON FOR CHANGE: Authorization Changes
*   REQUEST #: IRDK921102
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 27/10/2015
*   REASON FOR CHANGE: Layout changes
*   REQUEST #: IRDK921286
* --------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
*     TABLES6
*----------------------------------------------------------------------*
TABLES : marc,mara.
TYPE-POOLS : slis.",truxs.


SELECTION-SCREEN BEGIN OF BLOCK s2 WITH FRAME TITLE text-002 .
SELECT-OPTIONS: s_matnr FOR marc-matnr  ,
                s_werks FOR marc-werks,
                s_dismm FOR marc-dismm,
                s_mtart FOR mara-mtart.
SELECTION-SCREEN END OF BLOCK s2 .

SELECTION-SCREEN BEGIN OF BLOCK s3 WITH FRAME TITLE text-003 .
PARAMETERS : p_lgort TYPE mard-lgort OBLIGATORY.
SELECTION-SCREEN END OF BLOCK s3 .

***** Start Code: Added by CS on 13.10.2015 for Layout. *****
SELECTION-SCREEN BEGIN OF BLOCK s4 WITH FRAME TITLE text-004 .
PARAMETERS : p_var TYPE disvariant-variant MODIF ID pk.
SELECTION-SCREEN END OF BLOCK s4 .
***** End Code: Added by CS on 13.10.2015 for Layout. *****

*----------------------------------------------------------------------*
*     INTERNAL TABLES
*----------------------------------------------------------------------*


TYPES : BEGIN OF s_itab,
        lights(1),
        werks TYPE werks_d,
        matnr TYPE matnr,
        maktx TYPE maktx,
        meins  TYPE meins,
        minbe TYPE minbe, "Reorder Point
        dismm TYPE dismm,
        labst TYPE labst, "Unrestricted stock
        bstmi TYPE bstmi,
        diqty TYPE labst,
        ekgrp TYPE ekgrp,
        mtart TYPE mtart,
        matkl TYPE matkl,
        eisbe TYPE eisbe, "Safety Stock

       END   OF s_itab .
DATA :  v_message TYPE lvc_title.
DATA : it_itab TYPE TABLE OF s_itab.
DATA : wa_itab TYPE s_itab.
DATA : it_mard TYPE  mard_tt.
DATA : wa_mard TYPE mard.
*data : v_tabix type sy-tabix.
DATA : lv_stock TYPE labst.

***** Start Code: Added by CS on 13.10.2015 for layout. *****
DATA: s_repid   LIKE sy-repid,
      g_save(1) TYPE c,
      g_exit(1) TYPE c,                         " ALV VARIANT
      gx_variant TYPE disvariant.
***** End Code: Added by CS on 13.10.2015 for layout. *****

***** Start Code: Added by CS on 13.10.2015 for Authorization. *****
DATA: lv_mtart_auth_flg TYPE c VALUE '',  " Auth. Flag for Material Type
      lv_werks_auth_flg TYPE c VALUE ''  " Auth. Flag for Plant
.
***** End Code: Added by CS on 09.10.2015 for Authorization. *****


***** Start Code: Added by CS on 13.10.2015 for layout. *****
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_var.
  PERFORM variant_f4_selection.                    " F4 HELP FOR VARIANT ON SELECTION SCREEN

AT SELECTION-SCREEN.
  s_repid = sy-repid.
  PERFORM check_variant_existance.

INITIALIZATION.
  s_repid = sy-repid.
  PERFORM variant_init.
***** End Code: Added by CS on 13.10.2015 for layout. *****


*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION .
  PERFORM check_auth_obj.  " Added by CS on 13.10.2015 for Authorization.!
*  IF p_fore EQ 'X' .
*    v_mode = 'A' .
*  ELSE .
*    v_mode = 'E' .
*  ENDIF .
  PERFORM  f_get_data  .

***** Start Code: Added by CS on 13.10.2015 for Authorization. *****
*  IF NOT it_itab IS INITIAL. " Commented by CS on 13.10.2015
*    PERFORM  disp_records.
*  ENDIF.
  IF NOT it_itab IS INITIAL.
    IF lv_werks_auth_flg = 'X' OR lv_mtart_auth_flg = 'X'.
      MESSAGE 'Missing Authorization for Plant/Material Type.' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.
    PERFORM  disp_records.
  ELSE.
    IF lv_werks_auth_flg = 'X' OR lv_mtart_auth_flg = 'X'.
      MESSAGE 'No data found/ Missing Authorization for Plant/Material Type.' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
***** End Code: Added by CS on 13.10.2015 for Authorization. *****


*&---------------------------------------------------------------------*
*&      Form  I_UPLOAD
*&---------------------------------------------------------------------*
FORM f_get_data.

  IF NOT s_werks[] IS INITIAL AND s_matnr[] IS INITIAL.

    SELECT * FROM marc CLIENT SPECIFIED
       INTO CORRESPONDING FIELDS OF TABLE it_itab

                            WHERE mandt EQ sy-mandt
                              AND werks IN s_werks
                              AND matnr IN s_matnr
                              AND dismm IN s_dismm.
  ELSEIF NOT s_matnr IS INITIAL AND s_werks[] IS INITIAL.


    SELECT * FROM marc
                         INTO CORRESPONDING FIELDS OF TABLE it_itab
                            WHERE matnr IN s_matnr
                              AND werks IN s_werks.


  ELSE.
    SELECT * FROM marc INTO CORRESPONDING FIELDS OF TABLE it_itab
                           WHERE matnr IN s_matnr
                             AND werks IN s_werks.


  ENDIF.
  DELETE it_itab WHERE dismm NE 'VB' AND dismm NE 'PD'.


  LOOP AT it_itab INTO wa_itab.
    CLEAR lv_stock.
    SELECT werks matnr labst insme FROM mard INTO CORRESPONDING FIELDS OF TABLE it_mard
                                      WHERE matnr = wa_itab-matnr
                                        AND werks = wa_itab-werks
                                        AND lgort NE p_lgort.
    LOOP AT it_mard INTO wa_mard.
      lv_stock = lv_stock + wa_mard-labst + wa_mard-insme.
    ENDLOOP.


    IF wa_itab-minbe IS INITIAL AND wa_itab-dismm EQ 'PD'.
      wa_itab-minbe = wa_itab-eisbe + ( ( wa_itab-eisbe * 10 ) / 100 ).
    ENDIF.
    wa_itab-diqty =  lv_stock - wa_itab-minbe.

    IF lv_stock >= wa_itab-minbe.
      wa_itab-lights = '3'. " GREEN
    ELSEIF lv_stock > wa_itab-eisbe AND lv_stock < wa_itab-minbe.
      wa_itab-lights = '2'. "YELLOW
    ELSEIF lv_stock <= wa_itab-eisbe.
      wa_itab-lights = '1'. "RED
    ENDIF.


*    IF WA_ITAB-DIQTY EQ 0.
*      WA_ITAB-LIGHTS = '2'.
*    ELSEIF WA_ITAB-DIQTY LT 0.
*      WA_ITAB-LIGHTS = '1'.
*    ELSE.
*      WA_ITAB-LIGHTS = '3'.
*    ENDIF.

    SELECT SINGLE maktx FROM makt INTO wa_itab-maktx
                                WHERE spras = sy-langu
                                  AND matnr = wa_itab-matnr.

    SELECT SINGLE mtart matkl meins FROM mara INTO (wa_itab-mtart,wa_itab-matkl,wa_itab-meins)
                                WHERE matnr = wa_itab-matnr.


    wa_itab-labst = lv_stock.
    MODIFY it_itab FROM wa_itab.
    CLEAR wa_itab.
  ENDLOOP.

  IF NOT s_mtart[] IS INITIAL.
    LOOP AT it_itab INTO wa_itab.
      IF NOT wa_itab-mtart IN s_mtart.
        DELETE TABLE it_itab FROM wa_itab.
        CLEAR wa_itab.
      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT it_itab BY lights.
ENDFORM.                    " I_UPLOAD


*&---------------------------------------------------------------------*
*&      Form  disp_err
*&---------------------------------------------------------------------*
FORM disp_records.


  DATA: it_fieldcat TYPE slis_t_fieldcat_alv,

  wa_fieldcat TYPE LINE OF slis_t_fieldcat_alv,

  wa_layout TYPE slis_layout_alv,



  it_keyinfo TYPE TABLE OF slis_keyinfo_alv WITH HEADER LINE.



  v_message = 'Plant Material Stock Status'.

*wa_LAYOUT-BOX_FIELDNAME        = 'SELECTED'.  "field for checkbox

  wa_layout-lights_fieldname     = 'LIGHTS'.  "field for lights
  wa_layout-lights_rollname      = 'QVLOTSLIGHTS'.  "F1 help for lights

  wa_layout-get_selinfos         = 'X'. "show selection screen criteria
  wa_layout-detail_popup         = 'X'. "show detail via popup
  wa_layout-group_change_edit    = 'X'. "allows data to be grouped
  wa_layout-zebra                = 'X'. "striped pattern


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
    i_program_name               = sy-repid
*    I_INTERNAL_TABNAME           = 'IT_ITAB'
       i_structure_name             = 'ZZST_MAT_STATUS'
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

      IF wa_fieldcat-fieldname = 'BSTMI'.
        wa_fieldcat-seltext_l = 'Lot Size'.
        wa_fieldcat-seltext_m = 'Lot Size'.
        wa_fieldcat-seltext_s = 'Lot Size'.
        wa_fieldcat-ddictxt = 'M'.


      ENDIF.

      IF wa_fieldcat-fieldname = 'LABST'.
        wa_fieldcat-seltext_l = 'Stock'.
        wa_fieldcat-seltext_m = 'Stock'.
        wa_fieldcat-seltext_s = 'Stock'.
        wa_fieldcat-ddictxt = 'M'.


      ENDIF.
      IF wa_fieldcat-fieldname = 'DIQTY'.
        wa_fieldcat-seltext_l = 'Diff.Quantity'.
        wa_fieldcat-seltext_m = 'Diff.Quantity'.
        wa_fieldcat-seltext_s = 'Diff.Quantity'.
        wa_fieldcat-ddictxt = 'M'.


      ENDIF.
      MODIFY it_fieldcat FROM wa_fieldcat.
      CLEAR wa_fieldcat.
    ENDLOOP.

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
*       Added by CS on 13.10.2015 for layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_init .

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
* Addd by CS on 13.10.2015 for F4 Help Layout.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM variant_f4_selection .
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
* Added by CS on 13.10.2015 for validate existance of layout
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
*   Added by CS on 09.10.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth_obj .
  TYPES:
        BEGIN OF ty_t001w,
          werks TYPE t001w-werks, " Plants
        END OF ty_t001w,
        BEGIN OF ty_t134t,  " Material Type
          mtart TYPE t134t-mtart,
        END OF ty_t134t
          .
  DATA: t_t001w TYPE TABLE OF ty_t001w, " Plants
        w_t001w TYPE ty_t001w,
        t_t134t TYPE TABLE OF ty_t134t, " Material Type
        w_t134t TYPE ty_t134t
        .
  FREE : t_t001w[], t_t134t[].
  CLEAR: w_t001w, w_t134t.

  break test1.

***** Start Code: Added by CS on 13.10.2015 for Plant Authorization. *****
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
***** End Code: Added by CS on 13.10.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 13.10.2015 for Material Type Authorization. *****
  SELECT mtart  " Fetch values of Material Type
    FROM t134t
    INTO TABLE t_t134t
    WHERE mtart IN s_mtart
      AND spras eq sy-langu.

  CLEAR: s_mtart, lv_mtart_auth_flg.
  REFRESH: s_mtart[].
  IF t_t134t[] IS NOT INITIAL.
    LOOP AT t_t134t INTO w_t134t.
      AUTHORITY-CHECK OBJECT 'K_ML_MTART' " Material Type
                     ID 'ACTVT' FIELD '03'
                     ID 'MTART' FIELD w_t134t-mtart.
      IF sy-subrc EQ 0.
        s_mtart-sign = 'I'.
        s_mtart-option = 'EQ'.
        s_mtart-low = w_t134t-mtart.
        APPEND s_mtart.
        CLEAR: s_mtart.
      ELSE.
        IF lv_mtart_auth_flg IS INITIAL.  " Authorization Flag
          lv_mtart_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t134t.
    ENDLOOP.
  ENDIF.
  IF s_mtart[] IS INITIAL.
    s_mtart-sign = 'I'.
    s_mtart-option = 'EQ'.
    s_mtart-low = ''.
    APPEND s_mtart.
    CLEAR: s_mtart.
  ENDIF.
***** End Code: Added by CS on 13.10.2015 for Material Type Authorization. *****


ENDFORM.                    " CHECK_AUTH_OBJ


*R        BDC For Create Strategy Plan (IP42)                                                            35
*SP_BACK          Back Ground                                                                            19
*SP_FORE          Fore Ground                                                                            19
