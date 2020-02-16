*&---------------------------------------------------------------------*
*& Report  Z6MM009C_MATERIAL_PO_TEXT_UP
*&


REPORT  Z6MM009C_MATERIAL_PO_TEXT_UP.

*----------------------------------------------------------------------*
* OBJECT DESCRIPTION:Purchase Order text in material
* OBJECT TYPE       : BDC - Upload       FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 30.06.2010
*        DEV REQUEST: IRDK900233
*             TCODE : ZMM011
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: t100, mara.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA:   messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
*       error session opened (' ' or 'X')
DATA:   e_group_opened.


*include bdcrecx1.
**********************************************************
*INTERANAL TABLES
***********************************************************
DATA: BEGIN OF it_bdcdata OCCURS 5.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdcdata.

DATA: BEGIN OF it_messtab OCCURS 5.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_messtab.

DATA: BEGIN OF itab OCCURS 0,
        matnr(18) TYPE c,
        TEXT(60)  TYPE c,
      END OF itab.

data : xtab like itab OCCURS 0 with HEADER LINE.
DATA:  gv_xfile TYPE string.      " Filename

DATA: wf_dismode VALUE 'N',
      wf_updmode VALUE 'S'.

**********************************************************
*SELECTION-SCREEN
**********************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001 .
PARAMETERS : P_FILE1 LIKE RLGRAP-FILENAME obligatory.
*             P_MODE LIKE CTU_PARAMS-DISMODE DEFAULT 'N' obligatory.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* At SELECTION screen.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file1.
*Get the file path.
  PERFORM f3000_f4_filename.

*AT SELECTION-SCREEN.
*Get the file data in internal table.
  PERFORM f4000_collect_data .

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
start-of-selection.

  PERFORM f4000_upd_data.


*&---------------------------------------------------------------------*
*&      Form  f3000_f4_filename
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f3000_f4_filename .
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file1
    EXCEPTIONS
      OTHERS    = 1.

  gv_xfile = p_file1.
ENDFORM.                    " f3000_f4_filename

*&---------------------------------------------------------------------*
*&      Form  f4000_collect_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4000_collect_data .
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = gv_xfile
      filetype                = 'ASC'
      has_field_separator     = 'X'
    TABLES
      data_tab                = itab
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " f4000_collect_data

*&---------------------------------------------------------------------*
*&      Form  f4000_upd_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4000_upd_data .

  data : lt_HEADER like THEAD,
         lt_lines  like TLINE OCCURS 0 with header line.

  data : lv_TDNAME like THEAD-TDNAME,
         lv_matnr  like mara-matnr.

*perform open_group.
  refresh xtab. clear xtab.
  xtab[] = itab[].

* Distinct Material
  sort itab by matnr.
  delete ADJACENT DUPLICATES FROM itab COMPARING matnr.

  loop at itab.
    clear : lv_TDNAME, lv_matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT              = itab-matnr
     IMPORTING
       OUTPUT             = lv_matnr
     EXCEPTIONS
       LENGTH_ERROR       = 1
       OTHERS             = 2.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    lv_TDNAME+0(18) = lv_matnr.

    clear lt_header.
    lt_header-TDOBJECT = 'MATERIAL'.
    lt_header-TDNAME   = lv_TDNAME.
    lt_header-TDID   = 'BEST'.
    lt_header-TDSPRAS = sy-langu.

    refresh lt_lines. clear lt_lines.
    loop at xtab where matnr = itab-matnr.
      lt_lines-TDFORMAT = '/'.
      lt_lines-TDLINE   = xtab-text.
      append lt_lines. clear lt_lines.
    endloop.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        CLIENT                = SY-MANDT
        HEADER                = lt_header
        INSERT                = 'X'
        SAVEMODE_DIRECT       = 'X'
*     OWNER_SPECIFIED       = ' '
*     LOCAL_CAT             = ' '
*   IMPORTING
*     FUNCTION              =
*     NEWHEADER             =
      TABLES
        LINES                 = lt_lines
     EXCEPTIONS
       ID                    = 1
       LANGUAGE              = 2
       NAME                  = 3
       OBJECT                = 4
       OTHERS                = 5.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      write : / itab-matnr, ' PO Text not Upload Sucessful'.
    else.
       write : / itab-matnr, ' PO Text Upload Sucessful'.
    ENDIF.

  endloop.

endform.                    " f4000_upd_data
