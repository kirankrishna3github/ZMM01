*&---------------------------------------------------------------------*
*& Report  Z6HR007R_WAGE_REGISTER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:
*        DEVELOPER:   Naren Karra             DATE:   29.10.2015
*        DESCRIPTION: MM: Authorization code added
*        REQUEST :    IRDK921345
*{   INSERT         SBXK900038                                        1
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/07/2018
* Changed By - Rahul Shukla
* Purpose  - Simplification List: HANA Migration
* Solution - Table konv Conditions (Obsolete - replaced by PRCD_ELEMENTS)
* TR - SBXK900038
****************************************************************************************************
*}   INSERT
*----------------------------------------------------------------------*
REPORT  z6mm022r_po_condtions.


*&---------------------------------------------------------------------*
*&                      TABLES
*&---------------------------------------------------------------------*

TABLES : ekko, ekpo, t685t,t683s.


*&---------------------------------------------------------------------*
*&                      TYPE POOLS
*&---------------------------------------------------------------------*

TYPE-POOLS : slis.

*&---------------------------------------------------------------------*
*&                      Selection Screen
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .
PARAMETERS : p_vari  LIKE  disvariant-variant .
SELECTION-SCREEN END   OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-016.
PARAMETERS: p_bsart LIKE ekko-bsart OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-015.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln,
                 s_bedat FOR ekko-bedat,
                 s_lifnr FOR ekko-lifnr,
                 s_matnr FOR ekpo-matnr,
                 s_mtart FOR ekpo-mtart,
                 s_werks FOR ekpo-werks.
SELECTION-SCREEN END OF BLOCK a.

*&---------------------------------------------------------------------*
*&                      FIELD SYMBOLS
*&---------------------------------------------------------------------*

FIELD-SYMBOLS : <table>    TYPE  table ,     " Main Internal Table
                <struc> ,
                <table1>   TYPE  table,
                <struc1>,
                <table2>   TYPE  table,
                <struc2>,

                <table3>   TYPE  table,
                <struc3>,
                <wstruc>,                    " Header Struct for <table>
                <field> ,
                <component> .

DATA      :   alv_fieldcat TYPE              slis_t_fieldcat_alv ,
              lt_alv_cat   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat1   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat2   TYPE  TABLE OF    lvc_s_fcat ,
              lt_alv_cat3   TYPE  TABLE OF    lvc_s_fcat ,
              it_fieldcat  LIKE  LINE  OF    lt_alv_cat .

DATA      :   i_table      TYPE  REF   TO    data ,
              i_struct     TYPE  REF   TO    data ,
              tabix        TYPE sy-tabix.
*
DATA : v_recnt TYPE i.
DATA : BEGIN OF it_field_dynamic OCCURS 0,
      fieldname LIKE dd03l-fieldname,
      reftab  LIKE dd03l-tabname,
      reffield  LIKE dd03l-reffield,
      desc(35),
      END OF it_field_dynamic.
DATA : it_komv TYPE komv_itab.
DATA : wa_komv TYPE komv.
DATA : im_komv TYPE komv_itab.
*&---------------------------------------------------------------------*
*&                      ALV Declarations
*&---------------------------------------------------------------------*


**-- FOR TREE ALV
DATA : g_save(1)            TYPE  c                      ,
       g_exit(1)            TYPE  c                      ,
       g_variant            LIKE  disvariant             ,
       gx_variant           LIKE  disvariant             ,
       f2code               LIKE  sy-ucomm VALUE  '&ETA' ,
       layout               TYPE  slis_layout_alv        ,
       prg                  LIKE  sy-repid               ,
       keyinfo  TYPE slis_keyinfo_alv,
       gt_list_top_of_page  TYPE  slis_t_listheader      ,
       gt_events            TYPE  slis_t_event           ,
       fieldcat             TYPE  slis_t_fieldcat_alv    .

*&---------------------------------------------------------------------*
*&                      DATA DECLARATIONS
*&---------------------------------------------------------------------*


DATA: date LIKE sy-datum.


DATA : it_ekko TYPE ekko_tty.
DATA : wa_ekko TYPE ekko.
DATA : lv_kalsm TYPE kalsm.
TYPES : BEGIN OF i_ekpo.
INCLUDE TYPE ekpo.
TYPES : penqy TYPE menge_d,
        lifnr TYPE lifnr,
        name1 TYPE lfa1-name1,
        waers TYPE waers,
        wkurs TYPE wkurs.
TYPES : END OF i_ekpo.
DATA : lv_name1 TYPE lfa1-name1.
DATA : it_ekpo TYPE TABLE OF i_ekpo.
DATA : wa_ekpo TYPE i_ekpo.
DATA : lv_exch_rate TYPE bapi1093_0.
DATA : it_t683s TYPE ty_t683s.
DATA : wa_t683s TYPE t683s.
DATA : v_tabix TYPE sy-tabix.
DATA : it_params TYPE TABLE OF z6mma_params,
       wa_params TYPE z6mma_params.
TYPES : BEGIN OF s_kschl,
        kschl TYPE kschl,
        END OF s_kschl.
DATA : i_kschl TYPE TABLE OF s_kschl.
DATA : ia_kschl TYPE TABLE OF s_kschl.
DATA : is_kschl TYPE TABLE OF s_kschl.


DATA : wa_kschl TYPE s_kschl.
DATA : lv_ebeln TYPE bapimepoheader-po_number,
       lv_header TYPE bapimepoheader,
       it_poitem LIKE bapimepoitem OCCURS 0 WITH HEADER LINE,
       it_pocond LIKE bapimepocond OCCURS 0 WITH HEADER LINE,
       it_poconfirmation LIKE bapiekes OCCURS 0  WITH HEADER LINE,
       it_pohistory_totals LIKE  bapiekbes OCCURS 0 WITH HEADER LINE,
       it_pohistory LIKE bapiekbe OCCURS 0 WITH HEADER LINE.
DATA : gv_auth_werks_flg.                                           " added by Naren Karra on 29.10.2015
*&---------------------------------------------------------------------*
*&     AT SELECTION SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

AT SELECTION-SCREEN.
  PERFORM pai_of_selection_screen.

*&---------------------------------------------------------------------*
*&     INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION .
  prg = sy-repid .

*&---------------------------------------------------------------------*
*&                      START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM check_auth.                                                   " added by Naren Karra on 29.10.2015
  PERFORM display_list .
*  PERFORM F_GET_WAGE_TYPES.
  PERFORM wage_grp_get.
  PERFORM f_fill_fieldcat_struct_dyn.
  PERFORM f_field_cat_for_dyn_table.
  PERFORM f_create_dynamic_table.
  PERFORM get_data.
************************Start********************************           " added by NK on 29.10.2015
IF gv_auth_werks_flg = 'X'.
  MESSAGE 'Missing Authorization ' TYPE 'S' DISPLAY LIKE 'W'.
ENDIF.
*************************End*********************************
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&                      END-OF-SELECTION
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .


  FIELD-SYMBOLS : <f1>,<g1>,<h1>,<g2>,<h2>,<g3>,
                 <gs1>,<hs1>,<gs2>,<hs2>,<gs3>,<hs3>.
  DATA : v_text(30),fl_flag,v_text1(30).
  DATA : v_txt(30).
  DATA : v_fpper TYPE hrpy_rgdir-fpper,
         v_flper TYPE hrpy_rgdir-inper.
  DATA : v_lines TYPE i.
  DATA : BEGIN OF stru,
         ebeln LIKE ekko-ebeln,
         ebelp LIKE ekpo-ebelp,
         matnr LIKE ekpo-matnr,
         txz01 LIKE ekpo-txz01,
         menge LIKE ekpo-menge,
         penqy LIKE ekpo-menge,
         lifnr LIKE ekko-lifnr,
         name1 LIKE lfa1-name1,
         werks LIKE ekpo-werks,
         waers LIKE ekko-waers,
         wkurs LIKE ekko-wkurs,
         meins LIKE ekpo-meins,
*{   REPLACE        SBXK900038                                        2
*\         totpr like konv-kwert,
*\         lanCs like konv-kwert,
*\         totlc like konv-kwert,
*\         lanlc like konv-kwert,
*\         totrt like konv-kwert,
*\         lanrt like konv-kwert,
         totpr LIKE prcd_elements-kwert,
         lancs LIKE prcd_elements-kwert,
         totlc LIKE prcd_elements-kwert,
         lanlc LIKE prcd_elements-kwert,
         totrt LIKE prcd_elements-kwert,
         lanrt LIKE prcd_elements-kwert,
*}   REPLACE
         END OF stru.
  DATA : v_total LIKE pa0008-bet01,
*{   REPLACE        SBXK900038                                        3
*\         v_lancs like konv-kwert,
         v_lancs LIKE prcd_elements-kwert,
*}   REPLACE
         esic    TYPE p DECIMALS 2,
         esic1   LIKE pa0008-bet01,
         v_dedc  LIKE pa0008-bet01,
         v_etot LIKE pa0008-bet01,
         v_dtot LIKE pa0008-bet01,
         v_ntot LIKE pa0008-bet01..




  LOOP AT it_ekpo INTO wa_ekpo.
    v_tabix  = sy-tabix.

    CALL FUNCTION 'Z6MM_PO_CAL_TAX'
      EXPORTING
        i_ebeln = wa_ekpo-ebeln
        i_ebelp = wa_ekpo-ebelp
      IMPORTING
        tt_komv = it_komv.

    READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln.
    IF sy-subrc EQ 0.

*{   REPLACE        SBXK900038                                        1
*\      SELECT * FROM KONV INTO CORRESPONDING FIELDS OF TABLE IM_KOMV
*\                                       WHERE KNUMV EQ WA_EKKO-KNUMV
*\                                         AND KPOSN EQ WA_EKPO-EBELP.
      SELECT * FROM prcd_elements INTO CORRESPONDING FIELDS OF TABLE im_komv
                                       WHERE knumv EQ wa_ekko-knumv
                                         AND kposn EQ wa_ekpo-ebelp.
*}   REPLACE
    ENDIF.

    APPEND LINES OF im_komv TO it_komv.
    CLEAR im_komv.

    LOOP AT it_komv INTO wa_komv.
      LOOP AT it_t683s INTO wa_t683s  .
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE  wa_t683s
                                               TO <f1>.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.

          CASE sy-index.
            WHEN '7'.

              IF wa_komv-kschl EQ <f1>  .


*                    ASSIGN COMPONENT RT2-ZWAGE_GRP OF STRUCTURE <STRUC>
*                                            TO <G1>.
                MOVE wa_komv-kschl TO v_txt.
                CONCATENATE '<struc>' '-' v_txt INTO v_text.
*                    CONCATENATE '<struc>' '-' rt2-ZWAGE_GRP INTO V_TEXT.
                ASSIGN (v_text) TO <h1>.

                <h1> = ( wa_ekpo-penqy / wa_ekpo-menge ) * wa_komv-kwert.
                v_total = v_total + <h1>.
                LOOP AT  it_params INTO wa_params WHERE  param2 = 'ADD'.

                    SPLIT wa_params-paramval AT ';' INTO TABLE ia_kschl.
                    READ TABLE ia_kschl INTO wa_kschl WITH KEY kschl = wa_komv-kschl.
                    IF sy-subrc EQ 0.
                      v_lancs = v_lancs + <h1>.
                    ENDIF.
                 ENDLOOP.

                 LOOP AT  it_params INTO wa_params WHERE  param2 = 'SUB'.
                  IF sy-subrc EQ 0.
                    SPLIT wa_params-paramval AT ';' INTO TABLE is_kschl.
                  ENDIF.

                  READ TABLE is_kschl INTO wa_kschl WITH KEY kschl = wa_komv-kschl.
                  IF sy-subrc EQ 0.
                    v_lancs = v_lancs - <h1>.
                    v_total = v_total - <h1>.
                  ENDIF.
                 ENDLOOP.
*                      condense <h1> no-gaps.
*---for total earnings and deductions

                ENDIF.



            ENDCASE.
          ENDDO.
        ENDLOOP.

      ENDLOOP.



*      at end of paydt.
      AT END OF ebelp.
        fl_flag = 1.

      ENDAT.
      IF fl_flag = 1.
        CLEAR fl_flag.

        stru-ebeln = wa_ekpo-ebeln.
        stru-ebelp = wa_ekpo-ebelp.
        stru-totpr = v_total.
        stru-lancs = v_lancs.
        MOVE-CORRESPONDING wa_ekpo TO stru.
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
        stru-wkurs = lv_exch_rate-exch_rate.

        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
         EXPORTING
*           CLIENT                  = SY-MANDT
           date                    =  sy-datum
           foreign_amount          =  stru-totpr
           foreign_currency        =  stru-waers
           local_currency          =  'INR'
*           RATE                    = 0
           type_of_rate            = 'M'
*           READ_TCURR              = 'X'
         IMPORTING
           exchange_rate           = stru-wkurs
*           FOREIGN_FACTOR          =
           local_amount            = stru-totlc
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
        CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
         EXPORTING
*           CLIENT                  = SY-MANDT
           date                    =  sy-datum
           foreign_amount          =  stru-lancs
           foreign_currency        =  stru-waers
           local_currency          =  'INR'
*           RATE                    = 0
           type_of_rate            = 'M'
*           READ_TCURR              = 'X'
         IMPORTING
           exchange_rate           = stru-wkurs
*           FOREIGN_FACTOR          =
           local_amount            = stru-lanlc
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
        CLEAR : v_total,v_lancs.
        stru-totrt = stru-totlc / stru-penqy.
        stru-lanrt = stru-lanlc / stru-penqy.
        MOVE-CORRESPONDING stru TO <struc>.
        APPEND <struc> TO <table>.
        CLEAR <struc>.
      ENDIF.

    ENDLOOP.
  ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
FORM display_list .
  PERFORM  eventtab_build USING gt_events .
  PERFORM  build_layout USING layout .
  PERFORM build_comment USING gt_list_top_of_page[].

ENDFORM.                    " display_list

*&---------------------------------------------------------------------*
*&      Form  f4_for_variant
*&---------------------------------------------------------------------*
FORM f4_for_variant.
  g_save = 'A'.
  g_variant-report = prg.
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
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.                    " f4_for_variant

*&---------------------------------------------------------------------*
*&      Form  pai_of_selection_screen
*&---------------------------------------------------------------------*
FORM pai_of_selection_screen.
  IF NOT p_vari IS INITIAL.
    g_save = 'A'.
    MOVE g_variant TO gx_variant.
    MOVE p_vari TO gx_variant-variant.
    gx_variant-report = sy-repid .
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = g_save
      CHANGING
        cs_variant = gx_variant.
    g_variant = gx_variant.
  ELSE .
    g_save = 'A'.
    CLEAR g_variant.
    g_variant-report = sy-repid .
    gx_variant = g_variant.
  ENDIF.
ENDFORM.                    " pai_of_selection_screen


*&---------------------------------------------------------------------*
*&      Form  initialize_variant
*&---------------------------------------------------------------------*
FORM initialize_variant.
  g_save = 'A'.
  CLEAR g_variant.
  g_variant-report = prg.
  gx_variant = g_variant .
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.

ENDFORM.                    " initialize_variant

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
FORM build_layout USING p_layout TYPE slis_layout_alv.
  p_layout-f2code       = '&IC1'.
  p_layout-zebra        = 'X'.
*  p_layout-detail_popup = ' '.
ENDFORM.                    " build_layout

*&---------------------------------------------------------------------*
*&      Form  disp_alv
*&---------------------------------------------------------------------*
*FORM disp_alv.
**  layout-coltab_fieldname = 'COLOR'.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program = prg
*      it_fieldcat        = fieldcat
*      i_save             = g_save
*      is_variant         = g_variant
*      is_layout          = layout
*      it_events          = gt_events[]
*    TABLES
*      t_outtab           = disptab.
**ENDFORM.                    " disp_alv

FORM display_output .
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
    EXPORTING
      i_callback_program      = sy-repid
      i_callback_user_command = 'USER_COMMAND'.


  DATA fieldcat TYPE slis_fieldcat_alv.
  DATA : v_mcnt TYPE i.
  IF NOT <table1> IS INITIAL.
    LOOP AT lt_alv_cat1 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE1>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.



    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE1>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table1>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  IF NOT lt_alv_cat2[] IS INITIAL.
    LOOP AT lt_alv_cat2 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE2>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.


    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE2>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table2>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  IF NOT lt_alv_cat3[] IS INITIAL.
    LOOP AT lt_alv_cat3 INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE3>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
      EXPORTING
        is_layout                  = layout
        it_fieldcat                = alv_fieldcat
        i_tabname                  = '<TABLE3>'
        it_events                  = gt_events
      TABLES
        t_outtab                   = <table3>
      EXCEPTIONS
        program_error              = 0
        maximum_of_appends_reached = 0
        OTHERS                     = 0.

    CLEAR alv_fieldcat[].
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
    EXPORTING
      i_interface_check             = ' '
*     IS_PRINT                      =
*     I_SCREEN_START_COLUMN         = 0
*     I_SCREEN_START_LINE           = 0
*     I_SCREEN_END_COLUMN           = 0
*     I_SCREEN_END_LINE             = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER       =
*     ES_EXIT_CAUSED_BY_USER        =
    EXCEPTIONS
      program_error                 = 0
      OTHERS                        = 0.


*      fieldcat-fieldname = 'MATNR'.
*      fieldcat-col_pos   = '37'.
*      fieldcat-ref_fieldname = 'MATNR'.
*      fieldcat-ref_tabname = 'MARA'.
*      fieldcat-tabname = 'I_HEADER'.
*      fieldcat-seltext_m   = 'Material'.
**      it_fieldcat-outputlen = 22 .
*      APPEND fieldcat TO alv_fieldcat .
*      CLEAR fieldcat .


*  CALL FUNCTION 'ZCFM_HIDE_INITIALFIELD_ALV'
*    EXPORTING
*      struc      = <struc>
*      fieldcat   = alv_fieldcat
*      hide       = 'X'
*    IMPORTING
*      fieldcat_e = alv_fieldcat
*    TABLES
*      it_tab     = <table>.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*     EXPORTING
*          i_callback_program = sy-repid
*          i_bypassing_buffer = 'X'
**            i_callback_pf_status_set = 'SET_PF_STAT_L'
*          i_callback_user_command  = 'USER_COMMAND'
*          i_callback_top_of_page  = 'TOP_OF_PAGE'
*          it_fieldcat        = alv_fieldcat
*          it_events          = gt_events
**          is_Layout          = layout
**            I_STRUCTURE_NAME   = <struc>
*          TABLES
*          t_outtab           = <table>.

*  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
*            EXPORTING
**         I_INTERFACE_CHECK        = ' '
*                 i_callback_program       = SY-REPID
**         I_CALLBACK_PF_STATUS_SET =
*                i_callback_user_command  = 'USER_COMMAND'
**                 is_layout                =  layout
*                 it_fieldcat             =  alv_fieldcat
**                 is_print                =  alv_print
**         IT_EXCLUDING             =
**         IT_SPECIAL_GROUPS        =
**         IT_SORT                  =
**         IT_FILTER                =
**         IS_SEL_HIDE              =
**         I_SCREEN_START_COLUMN    = 0
**        i_screen_start_line      = 70
**         I_SCREEN_END_COLUMN      = 0
**         I_SCREEN_END_LINE        = 0
**                i_default                = 'A'
**                i_save                   = g_save
**                is_variant               = g_variant
*                it_events                = GT_events
*
**         IT_EVENT_EXIT            =
*
*                 i_tabname_header         = 'I_HEADER'
*
*                 i_tabname_item           = '<TABLE>'
**         I_STRUCTURE_NAME_HEADER  =
**         I_STRUCTURE_NAME_ITEM    =
*                 is_keyinfo               = keyinfo
**         IS_PRINT                 =
**    IMPORTING
**         E_EXIT_CAUSED_BY_CALLER  =
**         ES_EXIT_CAUSED_BY_USER   =
*            TABLES
*                 t_outtab_header          = i_header
*                 t_outtab_item            = <table>
*            EXCEPTIONS
*                 program_error            = 1
*                 OTHERS                   = 2.



ENDFORM.                    " DIsplay_output

*
*---------------------------------------------------------------------*
*       FORM eventtab_build                                           *
*---------------------------------------------------------------------*
FORM eventtab_build USING lt_events TYPE slis_t_event.
  CONSTANTS:
  gc_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
*  gc_formname_user_command TYPE slis_formname VALUE 'USER_COMMAND'.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = lt_events.

  READ TABLE lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE gc_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO lt_events.
  ENDIF.

*  READ TABLE lt_events WITH KEY name =  slis_ev_user_command
*                           INTO ls_event.
*  IF sy-subrc = 0.
*    MOVE gc_formname_user_command TO ls_event-form.
*    APPEND ls_event TO lt_events.
*  ENDIF.
ENDFORM.                    "eventtab_build

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE
*---------------------------------------------------------------------*
FORM top_of_page .
  DATA : v_tcount TYPE i.
*  V_TCOUNT = V_TCOUNT + 1.
*  IF V_TCOUNT EQ 1.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.
*  ENDIF.
ENDFORM.                    "top_of_page

*&--------------------------------------------------------------------*
*&      Form  BUILD_comment
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_HEADING  text
*---------------------------------------------------------------------*
FORM build_comment USING p_heading TYPE slis_t_listheader.
  DATA: line        TYPE  slis_listheader ,
        rs_variant  TYPE  disvariant      ,
        c_date(10)                        .

  CLEAR : gt_list_top_of_page[], gt_list_top_of_page .

  line-info = 'INDOFIL INDUSTRIES LTD' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  line-info = 'Landed Cost for Pending Purchase Orders' .
  line-typ = 'S' .
  APPEND line TO gt_list_top_of_page .

  IMPORT rs_variant FROM MEMORY ID 'VARIANT'.
  IF NOT rs_variant IS INITIAL .
    CONCATENATE 'Layout : ' rs_variant-text INTO line-info
                                             SEPARATED BY space .
    line-typ  = 'S' .
    APPEND line TO gt_list_top_of_page .
    CLEAR line .
  ENDIF .

  line-typ = 'S' .
  WRITE sy-datum TO c_date .
  CONCATENATE 'Date : ' c_date INTO line-info SEPARATED BY space .
  APPEND line TO gt_list_top_of_page .
  CLEAR line .

ENDFORM .                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  F_GET_WAGE_TYPES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_wage_types .


ENDFORM.                    " F_GET_WAGE_TYPES
*&---------------------------------------------------------------------*
*&      Form  F_ADD_WAGE_TYPE
" F_ADD_WAGE_TYPE
*&---------------------------------------------------------------------*
*&      Form  f_fill_fieldcat_struct_dyn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_fill_fieldcat_struct_dyn .
  DATA : v_text(20).
  DATA : i_month_n TYPE t247 OCCURS 0 WITH HEADER LINE.
  it_field_dynamic-fieldname = 'EBELN'.
  it_field_dynamic-desc      = 'Purchase Order'.
  it_field_dynamic-reftab   = 'EKKO'.

  COLLECT it_field_dynamic.

  it_field_dynamic-fieldname = 'EBELP'.
  it_field_dynamic-desc      = 'PO Item'.
  it_field_dynamic-reftab   = 'EKPO'.
  COLLECT it_field_dynamic.

*  it_field_dynamic-fieldname = 'PAYDT'.
*  it_field_dynamic-desc      = 'Pay Date'.
*  it_field_dynamic-reftab   = 'HRPY_RGDIR'.
*  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'MATNR'.
  it_field_dynamic-desc      = 'Material Code'.
  it_field_dynamic-reftab   = 'EKPO'.
  COLLECT it_field_dynamic.

  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'TXZ01'.
  it_field_dynamic-desc      = 'Material Desc'.
  it_field_dynamic-reftab   = 'EKPO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'MENGE'.
  it_field_dynamic-desc      = 'Order Quantity'.
  it_field_dynamic-reftab   = 'EKPO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'PENQY'.
  it_field_dynamic-desc      = 'Open.Ord.Qty'.
  it_field_dynamic-reftab   = 'EKPO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'MEINS'.
  it_field_dynamic-desc      = 'UOM'.
  it_field_dynamic-reftab   = 'EKPO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.


  it_field_dynamic-fieldname = 'LIFNR'.
  it_field_dynamic-desc      = 'Vendor '.
  it_field_dynamic-reftab   = 'EKKO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'NAME1'.
  it_field_dynamic-desc      = 'Vendor Name1 '.
  it_field_dynamic-reftab   = 'LFA1'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'WERKS'.
  it_field_dynamic-desc      = 'Plant '.
  it_field_dynamic-reftab   = 'EKPO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'WAERS'.
  it_field_dynamic-desc      = 'Currency '.
  it_field_dynamic-reftab   = 'EKKO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'WKURS'.
  it_field_dynamic-desc      = 'Curr.Exch.Rate '.
  it_field_dynamic-reftab   = 'EKKO'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.





  DELETE it_t683s WHERE kschl IS INITIAL.

  LOOP AT it_t683s INTO wa_t683s.
*    SELECT SINGLE * FROM t512t CLIENT SPECIFIED
*                        WHERE mandt = sy-mandt
*                          and sprsl = sy-langu
*                          and molga = '40'
*                          AND lgart = it_lgart-lgart.


*    if it_lgart-LGART+0(1) eq '/'.
*      IT_LGART-LGART = IT_LGART-LGART+1(3).
*      MODIFY IT_LGART TRANSPORTING LGART.
*
*    ENDIF.

    SELECT SINGLE vtext FROM t685t INTO v_text
                       WHERE spras = sy-langu
                         AND kvewe = wa_t683s-kvewe
                         AND kappl = wa_t683s-kappl
                         AND kschl = wa_t683s-kschl.


    IF sy-subrc = 0.
      it_field_dynamic-fieldname = wa_t683s-kschl.
      it_field_dynamic-desc      = v_text.
      it_field_dynamic-reftab   = 'PRCD_ELEMENTS'.
      it_field_dynamic-reffield = 'KWERT'.
      COLLECT it_field_dynamic.
      CLEAR   it_field_dynamic.
    ENDIF.

*    it_field_dynamic-fieldname = it_lgart-LGART.
*    it_field_dynamic-desc      = t512t-lgtxt.
*    it_field_dynamic-reftab   = 'T512T'.
*    COLLECT it_field_dynamic.

  ENDLOOP.

  it_field_dynamic-fieldname = 'TOTPR'.
  it_field_dynamic-desc      = 'Total Price (Doc.Curr) '.
  it_field_dynamic-reftab   = 'PRCD_ELEMENTS'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'LANCS'.
  it_field_dynamic-desc      = 'Landed Cost (Doc.Curr)'.
  it_field_dynamic-reftab   = 'PRCD_ELEMENTS'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.


  it_field_dynamic-fieldname = 'TOTLC'.
  it_field_dynamic-desc      = 'Total Price (Loc.Curr) '.
  it_field_dynamic-reftab   = 'PRCD_ELEMENTS'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'LANLC'.
  it_field_dynamic-desc      = 'Landed Cost (Loc.Curr)'.
  it_field_dynamic-reftab   = 'PRCD_ELEMENTS'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'TOTRT'.
  it_field_dynamic-desc      = 'Total Price Per Unit '.
  it_field_dynamic-reftab   = 'PRCD_ELEMENTS'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.

  it_field_dynamic-fieldname = 'LANRT'.
  it_field_dynamic-desc      = 'Landed Cost Per Unit'.
  it_field_dynamic-reftab   = 'PRCD_ELEMENTS'.
  COLLECT it_field_dynamic.
  CLEAR   it_field_dynamic.


ENDFORM.                    " f_fill_fieldcat_struct_dyn
*&---------------------------------------------------------------------*
*&      Form  F_field_cat_for_dyn_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_field_cat_for_dyn_table .
  LOOP AT it_field_dynamic.
    IF it_field_dynamic-fieldname EQ 'EBELN'
           OR it_field_dynamic-fieldname EQ 'EBELP'
           OR it_field_dynamic-fieldname EQ 'MATNR'
           OR it_field_dynamic-fieldname EQ 'TXZ01'
           OR it_field_dynamic-fieldname EQ 'MENGE'
           OR it_field_dynamic-fieldname EQ 'PENQY'
           OR it_field_dynamic-fieldname EQ 'LIFNR'
           OR it_field_dynamic-fieldname EQ 'NAME1'
           OR it_field_dynamic-fieldname EQ 'WAERS'
           OR it_field_dynamic-fieldname EQ 'WKURS'
           OR it_field_dynamic-fieldname EQ 'MEINS'
           OR it_field_dynamic-fieldname EQ 'WERKS'.


      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      IF it_field_dynamic-fieldname EQ 'PENQY'.
        it_fieldcat-ref_field = 'MENGE'.
      ELSE.
        it_fieldcat-ref_field = it_fieldcat-fieldname.
      ENDIF.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
    ELSE.
      it_fieldcat-fieldname = it_field_dynamic-fieldname.
      it_fieldcat-col_pos    = sy-tabix.
      it_fieldcat-ref_field = 'KWERT'.
*      it_fieldcat-ref_field = 'ZWAGE_TXT'.
      it_fieldcat-do_sum = 'X'.
      it_fieldcat-ref_table = it_field_dynamic-reftab.
      it_fieldcat-seltext   = it_field_dynamic-desc.
      COLLECT it_fieldcat INTO lt_alv_cat .
      CLEAR it_fieldcat .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_field_cat_for_dyn_table
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_dynamic_table .

  IF NOT lt_alv_cat[] IS INITIAL .
* Create Dynamic structure -> i_struct
*    CALL METHOD zcl_alv_struct_create=>create_dynammic_stru
*      EXPORTING
*        it_fieldcatalog = lt_alv_cat
*      IMPORTING
*        ep_table        = i_struct.
*    ASSIGN i_struct->* TO <struc> .       " Header Structure for <table>


* Create Dynamic Table -> i_table
    CALL METHOD cl_alv_table_create=>create_dynamic_table
      EXPORTING
        it_fieldcatalog = lt_alv_cat
      IMPORTING
        ep_table        = i_table.
    ASSIGN i_table->* TO <table> .
* Create dynamic work area and assign to FS

    CREATE DATA i_struct LIKE LINE OF <table>.
    ASSIGN i_struct->* TO <struc>.


  ENDIF.

ENDFORM.                    " F_CREATE_DYNAMIC_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .

  DATA : it_sort TYPE slis_sortinfo_alv OCCURS 1 WITH HEADER LINE.

  DATA fieldcat TYPE slis_fieldcat_alv.
  DATA : v_mcnt TYPE i.
  IF NOT <table> IS INITIAL.
    LOOP AT lt_alv_cat INTO it_fieldcat.
      MOVE-CORRESPONDING it_fieldcat TO fieldcat.
      IF sy-tabix LE 2.
        fieldcat-key = 1.
      ENDIF.
      fieldcat-ddictxt = 'M'.
      fieldcat-tabname  = '<TABLE>' .
      fieldcat-seltext_l = it_fieldcat-seltext.
      fieldcat-seltext_m = it_fieldcat-seltext.
      fieldcat-seltext_l = it_fieldcat-seltext.
      APPEND fieldcat TO alv_fieldcat .
      CLEAR fieldcat.
    ENDLOOP.

*    ----hide column
    DATA:
  ld_column      TYPE lvc_fname,
  ld_hide          TYPE c ,
  w_lines TYPE i,
  w_no TYPE i,
  wa_no(4) TYPE c.
    FIELD-SYMBOLS:
      <ls_entry>     TYPE any,
      <ld_fld>         TYPE any.

    DESCRIBE TABLE alv_fieldcat LINES w_lines.

*    w_no = w_lines - 2.
*    wA_no = w_no + 1.
*    do w_no times.
*      wA_no = wa_no - 1.
*      condense wA_no.
*      ld_column = wA_no ."ld_column + '01'.
*      if ld_column ge 1 and ld_column lt 10.
*        concatenate '0' ld_column into ld_column.
*      endif.
    LOOP AT it_field_dynamic WHERE reffield = 'KWERT'.
      CLEAR ld_column.
      ld_column = it_field_dynamic-fieldname.
      CONDENSE ld_column.
      LOOP AT <table> ASSIGNING <ls_entry>.
        CLEAR ld_hide.
        ASSIGN COMPONENT ld_column OF STRUCTURE <ls_entry> TO <ld_fld>.

*        IF ( <ld_fld>   > '0.00' ) .
        IF ( <ld_fld>   <> '0.00' ) .
          ld_hide = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      READ TABLE alv_fieldcat INTO fieldcat
                           WITH KEY fieldname = ld_column.
      IF ( syst-subrc = 0 ).
        IF ld_hide = 'X'.
          fieldcat-no_out = ''.
        ELSE.
          fieldcat-no_out = 'X'.
        ENDIF.

        MODIFY alv_fieldcat FROM fieldcat INDEX syst-tabix.
      ENDIF.
    ENDLOOP.

*    READ TABLE alv_fieldcat INTO fieldcat
*                        WITH KEY fieldname = 'TNET'.
*    IF SY-SUBRC = 0.
*      fieldcat-no_out = 'X'.
*    ENDIF.
*    MODIFY alv_fieldcat FROM fieldcat INDEX syst-tabix.
*    ----

    it_sort-spos = 1.
    it_sort-fieldname = 'EBELN'.
    it_sort-up = 'X'.
    it_sort-subtot = ''.
    APPEND it_sort.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = sy-repid
            i_bypassing_buffer = 'X'
*            i_callback_pf_status_set = 'SET_PF_STAT_L'
*            i_callback_user_command  = 'USER_COMMAND'
            i_callback_top_of_page  = 'TOP_OF_PAGE'
            it_fieldcat        = alv_fieldcat
            it_sort                = it_sort[]
            i_default                         = 'X'
             i_save                            = 'A'
            it_events          = gt_events
*          is_Layout          = layout
*            I_STRUCTURE_NAME   = <struc>
            TABLES
            t_outtab           = <table>.
  ELSE.                                                                             " added by Naren Karra on 29.10.2015
  MESSAGE 'No records found/ Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.      " added by Naren Karra on 29.10.2015
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  WAGE_GRP_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM wage_grp_get .
  DATA : lv_tabix TYPE sy-tabix.
  DATA : t_t549q TYPE t549q OCCURS 0 WITH HEADER LINE.


  SELECT * FROM ekko CLIENT SPECIFIED  INTO CORRESPONDING FIELDS OF TABLE it_ekko
                                   WHERE mandt = sy-mandt
                                    AND  bstyp = 'F'
                                    AND  bedat IN s_bedat
                                    AND  ebeln IN s_ebeln
                                    AND  bsart EQ p_bsart
                                    AND  lifnr IN s_lifnr
                                    AND  loekz EQ space.
  IF NOT it_ekko IS INITIAL.

    SELECT * FROM ekpo INTO CORRESPONDING FIELDS OF TABLE it_ekpo
                      FOR ALL ENTRIES IN it_ekko
                      WHERE ebeln EQ it_ekko-ebeln
                        AND matnr IN s_matnr
                        AND mtart IN s_mtart
                        AND werks IN s_werks
                        AND loekz EQ space.


  ENDIF.

  READ TABLE it_ekpo INTO wa_ekpo INDEX 1.
  IF sy-subrc EQ 0.
    READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln.
    IF sy-subrc EQ 0.
      lv_kalsm = wa_ekko-kalsm.
    ENDIF.
  ENDIF.
  SORT it_ekpo BY ebeln.
  LOOP AT it_ekpo INTO wa_ekpo.
    v_tabix = sy-tabix.
    ON CHANGE OF wa_ekpo-ebeln.

      lv_ebeln = wa_ekko-ebeln.
      REFRESH : it_poitem,it_pohistory_totals,it_poconfirmation.
      CLEAR : it_poitem,it_pohistory_totals,it_poconfirmation,lv_header.
      CLEAR : lv_name1.
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
*       POCOND                   = it_pocond
*          POLIMITS                 =
*          POCONTRACTLIMITS         =
*          POSERVICES               =
*          POSRVACCESSVALUES        =
*          POTEXTHEADER             =
*          POTEXTITEM               =
*          POEXPIMPITEM             =
*          POCOMPONENTS             =
*          POSHIPPINGEXP            =
*        POHISTORY                = IT_POHISTORY
       pohistory_totals         = it_pohistory_totals
*        POCONFIRMATION           = IT_POCONFIRMATION
*          ALLVERSIONS              =
*          POPARTNER                =
*          EXTENSIONOUT             =
*          SERIALNUMBER             =
*          INVPLANHEADER            =
*          INVPLANITEM              =
*          POHISTORY_MA             =
             .
      SELECT SINGLE name1 FROM lfa1 INTO lv_name1 WHERE lifnr EQ lv_header-vendor.

    ENDON.
    READ TABLE it_pohistory_totals WITH KEY po_item = wa_ekpo-ebelp.
    IF sy-subrc EQ 0.
      wa_ekpo-penqy = wa_ekpo-menge -  it_pohistory_totals-deliv_qty.
    ELSE.
      wa_ekpo-penqy = wa_ekpo-menge.
    ENDIF.
    IF wa_ekpo-elikz NE space.
      wa_ekpo-penqy = 0.
    ENDIF.
    wa_ekpo-name1 = lv_name1.
    wa_ekpo-lifnr = lv_header-vendor.
    wa_ekpo-waers = lv_header-currency.
    wa_ekpo-wkurs = lv_header-exch_rate.
    MODIFY it_ekpo FROM wa_ekpo INDEX v_tabix.
    CLEAR wa_ekpo.
  ENDLOOP.
  DELETE it_ekpo WHERE penqy IS INITIAL.

  SELECT * FROM t683s INTO CORRESPONDING FIELDS OF TABLE it_t683s
                               WHERE kvewe EQ 'A'
                                 AND kappl EQ 'M'
                                 AND kalsm EQ lv_kalsm.

  IF p_bsart EQ 'ZDOM' OR p_bsart EQ 'ZSED' OR p_bsart EQ 'ZTOL'.

    SELECT * FROM t683s APPENDING CORRESPONDING FIELDS OF TABLE it_t683s
                                 WHERE kvewe EQ 'A'
                                   AND kappl EQ 'TX'
                                   AND kalsm EQ 'ZTXINN'.
  ENDIF.
  SORT it_t683s BY kschl.
  DELETE ADJACENT DUPLICATES FROM it_t683s COMPARING kschl.

  SELECT * FROM z6mma_params INTO CORRESPONDING FIELDS OF TABLE it_params
                                   WHERE progname = 'LANDEDCOST'
                                    AND   ( param1 EQ lv_kalsm
                                            OR param1 EQ 'ZTXINN' ).

  LOOP AT it_params INTO wa_params WHERE param2 = 'EXCL'.
    IF NOT wa_params-paramval IS INITIAL.
      SPLIT wa_params-paramval AT ';' INTO TABLE i_kschl.
      IF NOT i_kschl IS INITIAL.
        LOOP AT it_t683s INTO wa_t683s.
          v_tabix = sy-tabix.
          READ TABLE i_kschl INTO wa_kschl WITH KEY kschl = wa_t683s-kschl.
          IF sy-subrc EQ 0.
            DELETE it_t683s INDEX v_tabix..
          ENDIF.

        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " WAGE_GRP_GET
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth .
TYPES: BEGIN OF ty_t001w,
        werks TYPE werks_d,
       END OF ty_t001w.
DATA : wa_t001w TYPE ty_t001w, i_t001w TYPE STANDARD TABLE OF ty_t001w.

SELECT werks
  FROM t001w
  INTO TABLE i_t001w
  WHERE werks IN s_werks.

  CLEAR: gv_auth_werks_flg, s_werks.
  REFRESH: s_werks[].
 IF i_t001w[] IS NOT INITIAL.
  LOOP AT i_t001w INTO wa_t001w.
    AUTHORITY-CHECK OBJECT 'M_MSEG_WWA'
                        ID 'WERKS' FIELD wa_t001w-werks
                        ID 'ACTVT' FIELD '03'.
    IF sy-subrc EQ 0.
      s_werks-sign = 'I'.
      s_werks-option = 'EQ'.
      s_werks-low = wa_t001w-werks.
      APPEND s_werks.
      CLEAR s_werks.
    ELSE.
      IF gv_auth_werks_flg IS INITIAL.
        gv_auth_werks_flg = 'X'.
      ENDIF.
    ENDIF.
    CLEAR wa_t001w.
  ENDLOOP.
 ENDIF.

  IF s_werks[] IS INITIAL.
    s_werks-sign = 'I'.
    s_werks-option = 'EQ'.
    s_werks-low = ''.
    APPEND s_werks.
  ENDIF.
ENDFORM.                    " CHECK_AUTH
