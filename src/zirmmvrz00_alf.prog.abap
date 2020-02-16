*&---------------------------------------------------------------------*
*&  Include           IRMMVRZ00_ALF
*&---------------------------------------------------------------------*
*  Form  ALV_LIST_OUTPUT
*-----------------------------------------------------------------------
*  Purpose: display output list
************************************************************************
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
* - zebra list
  ls_layout-zebra                = c_marked.
* - optimize column width
  ls_layout-colwidth_optimize    = c_marked.
* - allow switch to classic ALV
  ls_layout-allow_switch_to_list = c_marked.
  CLEAR listwa.

  LOOP AT gt_list INTO listwa.
    SELECT SINGLE spart FROM mara INTO listwa-spart WHERE matnr = listwa-matnr.
    SELECT SINGLE vtext FROM tspat INTO listwa-vtext WHERE spart = listwa-spart AND spras = 'EN'.
    SELECT SINGLE wgbez FROM t023t INTO listwa-wgbez WHERE matkl = listwa-matkl AND spras = 'EN'.
    SELECT SINGLE mtbez FROM t134t INTO listwa-mtbez WHERE mtart = listwa-mtart AND spras = 'EN'.

    MODIFY gt_list FROM listwa.
    CLEAR listwa.
  ENDLOOP.

* display output list
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_interface_check           = ' '
      i_bypassing_buffer          = ' '
      i_buffer_active             = ' '
      i_callback_program          = cs_callback-program
      i_callback_pf_status_set    = cs_callback-pf_status_set
      i_callback_user_command     = cs_callback-user_command
      i_callback_top_of_page      = cs_callback-top_of_page
*     I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*     I_CALLBACK_HTML_END_OF_LIST = ' '
*     I_STRUCTURE_NAME            =
*     I_BACKGROUND_ID             = ' '
*     I_GRID_TITLE                =
*     I_GRID_SETTINGS             =
      is_layout                   = ls_layout
      it_fieldcat                 = lt_fieldcat
              i_save             = 'A' " G_SAVE
              is_variant         = gx_variant
*         I_DEFAULT               = 'A'
*     IT_EXCLUDING                =
*     IT_SPECIAL_GROUPS           =
*     IT_SORT                     =
*     IT_FILTER                   =
*     IS_SEL_HIDE                 =
*     I_DEFAULT                   = 'X'
*      I_SAVE                      = C_SAVE
*     IS_VARIANT                  =
*     IT_EVENTS                   =
*     IT_EVENT_EXIT               =
*     IS_PRINT                    =
*     IS_REPREP_ID                =
*     I_SCREEN_START_COLUMN       = 0
*     I_SCREEN_START_LINE         = 0
*     I_SCREEN_END_COLUMN         = 0
*     I_SCREEN_END_LINE           = 0
*     I_HTML_HEIGHT_TOP           = 0
*     I_HTML_HEIGHT_END           = 0
*     IT_ALV_GRAPHICS             =
*     IT_HYPERLINK                =
*     IT_ADD_FIELDCAT             =
*     IT_EXCEPT_QINFO             =
*     IR_SALV_FULLSCREEN_ADAPTER  =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER     =
*     ES_EXIT_CAUSED_BY_USER      =
    TABLES
      t_outtab                    = gt_list
  EXCEPTIONS
    program_error                 = 1
    OTHERS                        = 2.
  IF ( sy-subrc NE 0 ).
* => bug
    MESSAGE a899(ig) WITH sy-repid 'ALV_LISTTAB_OUTPUT'
      'REUSE_ALV_GRID_DISPLAY' sy-subrc.
  ENDIF.
ENDFORM.                    "alv_list_output


************************************************************************
*  Form  ALV_LIST_FIELDCAT_CREATE
*-----------------------------------------------------------------------
*  Purpose: create ALV field catalog
*-----------------------------------------------------------------------
*  <--  ET_FIELDCAT  size > 0
************************************************************************
FORM alv_list_fieldcat_create
    CHANGING et_fieldcat TYPE slis_t_fieldcat_alv.

* data declaration
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.

* initialize export parameters
  REFRESH: et_fieldcat.

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
      ct_fieldcat            = et_fieldcat
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
  ls_fieldcat-key = c_marked.
  MODIFY et_fieldcat FROM ls_fieldcat TRANSPORTING key
         WHERE ( fieldname EQ c_werk  )
         OR    ( fieldname EQ c_matnr ).
  IF ( sy-subrc NE 0 ).
* => bug
    MESSAGE a899(ig) WITH sy-repid 'ALV_LIST_FIELDCAT_CREATE'
      'MODIFY ET_FIELDCAT' 'TRANSPORTING KEY'.
  ENDIF.
  "note 1307870
  CLEAR ls_fieldcat-ref_tabname.
  MODIFY et_fieldcat FROM ls_fieldcat TRANSPORTING ref_tabname
         WHERE ( fieldname EQ 'PREIS' ).

ENDFORM.                    "alv_list_fieldcat_create

*&--------------------------------------------------------------------*
*&      Form  ALV_LIST_TOP_OF_PAGE
*&--------------------------------------------------------------------*
*       Create TOP_OF_PAGE Area for ALV
*---------------------------------------------------------------------*
*FORM alv_list_top_of_page.                                  "#EC CALLED
*
** data declaration
*  DATA: lt_list_top_of_page TYPE slis_t_listheader,
*        ls_line             TYPE slis_listheader.

* fill top of page
*  CLEAR ls_line.
*  ls_line-typ = 'S'.
*  ls_line-key = text-011.
*  ls_line-info =  werks .
*  APPEND ls_line TO lt_list_top_of_page.
*
*  CLEAR ls_line.
*  ls_line-typ = 'S'.
*  ls_line-key = text-012.
*  ls_line-info =  methode .
*  APPEND ls_line TO lt_list_top_of_page.
*
*  CLEAR ls_line.
*  ls_line-typ = 'S'.
*  ls_line-key = text-013.
*  WRITE gueltigab TO ls_line-info.
*  APPEND ls_line TO lt_list_top_of_page.
*
*  CLEAR ls_line.
*  ls_line-typ = 'S'.
*  ls_line-key =  text-014.
*  CONCATENATE  '(' qmtb_feld ')' INTO ls_line-info.
*  CONCATENATE dfies-fieldtext ls_line-info INTO ls_line-info
*        SEPARATED BY space.
*  APPEND ls_line TO lt_list_top_of_page.
*
*  CLEAR ls_line.
*  ls_line-typ = 'A'.
*  ls_line-key =  ''.
*  ls_line-info = '' .
*  APPEND ls_line TO lt_list_top_of_page.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      it_list_commentary = lt_list_top_of_page.
*
*ENDFORM.                    "alv_list_top_of_page
