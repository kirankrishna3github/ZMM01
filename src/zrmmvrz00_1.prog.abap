*&---------------------------------------------------------------------*
*& Report ZRMMVRZ00_1
*&---------------------------------------------------------------------*
REPORT zrmmvrz00_1 LINE-COUNT 0
                   LINE-SIZE 132 MESSAGE-ID m3.

TYPE-POOLS:slis.

TABLES: marav,
        marc ,
        marcv,
        mbew ,
        t001w,
        t001k,
        tcurm,
        t001 ,
        mmvr1.

*Structures Types
TYPES: BEGIN OF listwa_type.
         INCLUDE TYPE zzplm_alv_230.
         TYPES:
       END OF listwa_type.

*Tables Types
TYPES: listtab_type TYPE STANDARD TABLE OF listwa_type WITH DEFAULT KEY.

*Data
DATA: gt_list TYPE listtab_type,
      listwa LIKE LINE OF gt_list.

*Structures Types
TYPES: BEGIN OF listwa_type1.
  INCLUDE TYPE plm_alv_230.
  TYPES:
END OF listwa_type1.

*Tables Types
TYPES: listtab_type1 TYPE STANDARD TABLE OF listwa_type1 WITH DEFAULT KEY.

*Data
DATA: gt_list1 TYPE listtab_type1,
      listwa1 LIKE LINE OF gt_list1.

"Constants
CONSTANTS: c_marked(1)      TYPE C VALUE 'X',
           c_structure_name TYPE tabname VALUE 'ZZPLM_ALV_230',
           c_save(1)        TYPE C VALUE 'A',
           c_werk(5)        TYPE C VALUE 'WERKS',
           c_matnr(5)       TYPE C VALUE 'MATNR'.

CONSTANTS:
      BEGIN OF cs_callback,
        PROGRAM       TYPE syrepid VALUE sy-repid, "'RMMVRZ00',
        pf_status_set TYPE slis_formname VALUE '',
        user_command  TYPE slis_formname VALUE '',
        top_of_page   TYPE slis_formname VALUE '',
      END   OF cs_callback.

DATA: s_repid   LIKE sy-repid,
      g_save(1) TYPE C,
      g_exit(1) TYPE C,                         " ALV VARIANT
      gx_variant TYPE disvariant.

SELECTION-SCREEN BEGIN OF BLOCK msm WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: ms_matnr FOR marav-matnr MEMORY ID mat MATCHCODE OBJECT mat1,
                ms_werks FOR marcv-werks MEMORY ID wrk,
                mtart    FOR marav-mtart,
                matkl    FOR marav-matkl,
                ernam    FOR marav-ernam.

PARAMETERS: p_vari TYPE disvariant-variant MODIF ID pk,
            bewflg TYPE mit_status_b DEFAULT 'X' AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK msm.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM variant_f4_selection.                    " F4 HELP FOR VARIANT ON SELECTION SCREEN

AT SELECTION-SCREEN.
  s_repid = sy-repid.
  PERFORM check_variant_existance.

INITIALIZATION.
  s_repid = sy-repid.
  PERFORM variant_init.

START-OF-SELECTION.

  PERFORM mm60.

END-OF-SELECTION.

* ALV output
PERFORM alv_list_output.

*&---------------------------------------------------------------------*
*& Form MM60
*&---------------------------------------------------------------------*
FORM mm60.

  cl_salv_bs_runtime_info=>set(
    EXPORTING
      display  = abap_false
      metadata = abap_false
      data     = abap_true ).

  SUBMIT rmmvrz00
         WITH ms_matnr IN ms_matnr
         WITH ms_werks IN ms_werks
         WITH mtart    IN mtart
         WITH matkl    IN matkl
         WITH ernam    IN ernam
         WITH bewflg   EQ bewflg
         AND RETURN.

  TRY.
    REFRESH : gt_list1[], gt_list[].
    cl_salv_bs_runtime_info=>get_data(
    IMPORTING
      t_data = gt_list1 ).

  CATCH cx_salv_bs_sc_runtime_info.
    MESSAGE 'No records were selected' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  CATCH cx_root.
    MESSAGE 'Please Enter Appropriate Entries' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  LOOP AT gt_list1 INTO listwa1.
    MOVE-CORRESPONDING listwa1 to listwa.
    SELECT SINGLE spart FROM mara  INTO listwa-spart WHERE matnr EQ listwa-matnr.
    SELECT SINGLE vtext FROM tspat INTO listwa-vtext WHERE spart EQ listwa-spart AND spras EQ 'EN'.
    SELECT SINGLE wgbez FROM t023t INTO listwa-wgbez WHERE matkl EQ listwa-matkl AND spras EQ 'EN'.
    SELECT SINGLE mtbez FROM t134t INTO listwa-mtbez WHERE mtart EQ listwa-mtart AND spras EQ 'EN'.
    APPEND listwa to gt_list.
    CLEAR : listwa, listwa1.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VARIANT_F4_SELECTION
*&---------------------------------------------------------------------*
FORM variant_f4_selection.

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
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
FORM check_variant_existance.

  IF NOT p_vari IS INITIAL.
    MOVE p_vari TO gx_variant-variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant.
  ELSE.
    PERFORM variant_init.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VARIANT_INIT
*&---------------------------------------------------------------------*
FORM variant_init.

  g_save = 'A'.
  CLEAR gx_variant.
  gx_variant-REPORT = s_repid.
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

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ALV_LIST_OUTPUT
*&---------------------------------------------------------------------*
FORM alv_list_output.

* data declaration
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv.

* create ALV field catalog
  PERFORM alv_list_fieldcat_create
  CHANGING
    lt_fieldcat.

* set layout
  CLEAR: ls_layout.
  ls_layout-zebra                = 'X'.
  ls_layout-colwidth_optimize    = 'X'.
  ls_layout-allow_switch_to_list = 'X'.

  CLEAR listwa.

* display output list
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_interface_check           = ' '
    i_bypassing_buffer          = ' '
    i_buffer_active             = ' '
    i_callback_program          = cs_callback-PROGRAM
    i_callback_pf_status_set    = cs_callback-pf_status_set
    i_callback_user_command     = cs_callback-user_command
    i_callback_top_of_page      = cs_callback-top_of_page
    is_layout                   = ls_layout
    it_fieldcat                 = lt_fieldcat
    i_save                      = 'A' " G_SAVE
    is_variant                  = gx_variant
  TABLES
    t_outtab                    = gt_list
  EXCEPTIONS
    program_error               = 1
    OTHERS                      = 2.

  IF sy-subrc NE 0.
* => bug
    MESSAGE a899(ig) WITH sy-repid 'ALV_LISTTAB_OUTPUT'
    'REUSE_ALV_GRID_DISPLAY' sy-subrc.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ALV_LIST_FIELDCAT_CREATE
*&---------------------------------------------------------------------*
FORM alv_list_fieldcat_create CHANGING p_lt_fieldcat TYPE slis_t_fieldcat_alv.

* data declaration
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

* initialize export parameters
  REFRESH: p_lt_fieldcat.

* create initial ALV field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
*     I_PROGRAM_NAME         =
*     I_INTERNAL_TABNAME     =
    i_structure_name       = c_structure_name
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_INCLNAME             =
*     I_BYPASSING_BUFFER     =
*     I_BUFFER_ACTIVE        =
  CHANGING
    ct_fieldcat            = p_lt_fieldcat
  EXCEPTIONS
    inconsistent_interface = 1
    program_error          = 2
    OTHERS                 = 3.
  IF ( sy-subrc NE 0 ).
* => bug
    MESSAGE a899(ig) WITH sy-repid 'ALV_LISTTAB_FIELDCAT_CREATE'
    'REUSE_ALV_FIELDCATALOG_MERGE' sy-subrc.
  ENDIF.

* set key fields
  ls_fieldcat-KEY = c_marked.
  MODIFY p_lt_fieldcat FROM ls_fieldcat TRANSPORTING KEY
  WHERE ( fieldname EQ c_werk  )
  OR    ( fieldname EQ c_matnr ).
  IF ( sy-subrc NE 0 ).
* => bug
    MESSAGE a899(ig) WITH sy-repid 'ALV_LIST_FIELDCAT_CREATE'
    'MODIFY p_lt_fieldcat' 'TRANSPORTING KEY'.
  ENDIF.
  "note 1307870
  CLEAR ls_fieldcat-ref_tabname.
  MODIFY p_lt_fieldcat FROM ls_fieldcat TRANSPORTING ref_tabname
  WHERE ( fieldname EQ 'PREIS' ).

ENDFORM.
