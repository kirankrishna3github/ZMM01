*&---------------------------------------------------------------------*
*& Report  ZMM_VENDOR_UPLOAD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmm_vendor_upload NO STANDARD PAGE HEADING LINE-SIZE 255.

TYPE-POOLS : slis,truxs.
DATA : v_filename TYPE rlgrap-filename.
DATA: it_raw TYPE truxs_t_text_data,
      col TYPE i,
      str(3) TYPE c,
      str1   TYPE string,
      plant  TYPE zmm_vendor_tag-werks,
      lifnr(10) TYPE c.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

DATA : gt_zmm_vendor_tag  TYPE TABLE OF zmm_vendor_tag,
       gt_zmm_vendor_tag1 TYPE TABLE OF zmm_vendor_tag,
       gt_zmm_vendor_tag2 TYPE TABLE OF zmm_vendor_tag,
       gs_zmm_vendor_tag  LIKE LINE OF gt_zmm_vendor_tag,
       gs_zmm_vendor_tag1 LIKE LINE OF gt_zmm_vendor_tag1,
       gs_zmm_vendor_tag2 LIKE LINE OF gt_zmm_vendor_tag2.

DATA : BEGIN OF record OCCURS 0,
       lifnr           TYPE lifnr,
***       werks           TYPE ZWERKS_D,
       name1           TYPE name1,
       str_suppl1      TYPE ad_strspp1,
       str_suppl2      TYPE ad_strspp2,
       street          TYPE ad_street,
       post_code1      TYPE ad_pstcd1,
       city1           TYPE ad_city1,
       country         TYPE land1,
       region          TYPE regio,
       time_zone       TYPE ad_tzone,
       csttin          TYPE j_1icstno,
       vat             TYPE zvendor_vat,
       pan             TYPE j_1ipanno,
       servicetaxno    TYPE j_1isern,
       centralexciseno TYPE j_1iexcd,
       vtyp            type J_1IVTYP,
  END OF record.

DATA : intern	TYPE TABLE OF alsmex_tabline WITH HEADER LINE,
       record1 LIKE TABLE OF record WITH HEADER LINE .

FIELD-SYMBOLS : <fs>.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FILE'
    IMPORTING
      file_name     = p_file.

  IF NOT p_file IS INITIAL.
    v_filename = p_file.
  ENDIF.

START-OF-SELECTION.
  PERFORM upload_excel TABLES record.
***  PERFORM get_recent_data.
  PERFORM modify_table.
*&---------------------------------------------------------------------*
*&      Form  modify_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modify_table.
***  break ibm_ams.
  record1[] = record[].
  SORT : record  BY lifnr region,
         record1 BY lifnr region.
  DELETE : ADJACENT DUPLICATES FROM record1 COMPARING lifnr region.
  IF record1[] IS NOT INITIAL.
    SELECT * FROM zmm_vendor_tag INTO TABLE gt_zmm_vendor_tag1
    FOR ALL ENTRIES IN record1
    WHERE lifnr = record1-lifnr.

    SORT : gt_zmm_vendor_tag1 BY lifnr ASCENDING werks DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_zmm_vendor_tag1 COMPARING lifnr region.
  ENDIF.

  LOOP AT record1.
    CLEAR : str.
    READ TABLE gt_zmm_vendor_tag1 INTO gs_zmm_vendor_tag1 WITH KEY lifnr = record1-lifnr region = record1-region.
    IF sy-subrc = 0.
      plant = gs_zmm_vendor_tag1-werks.
    ELSE.
      CLEAR : plant.
    ENDIF.
    LOOP AT record WHERE lifnr  = record1-lifnr
                   AND   region = record1-region.

      PERFORM add_zeros USING record-lifnr CHANGING record-lifnr.
      IF plant IS NOT INITIAL.
        str = plant+3(3) + 1.
        CLEAR : plant.
      ELSE.
        str = str + 1.
      ENDIF.
      PERFORM add_zeros USING str CHANGING str.
      CONCATENATE record-region str INTO str1 SEPARATED BY '-'.
      gs_zmm_vendor_tag-lifnr           = record-lifnr.
      gs_zmm_vendor_tag-werks           = str1.
      gs_zmm_vendor_tag-name1           = record-name1.
      gs_zmm_vendor_tag-str_suppl1      = record-str_suppl1.
      gs_zmm_vendor_tag-str_suppl2      = record-str_suppl2.
      gs_zmm_vendor_tag-street          = record-street.
      gs_zmm_vendor_tag-post_code1      = record-post_code1.
      gs_zmm_vendor_tag-city1           = record-city1.
      gs_zmm_vendor_tag-country         = record-country.
      gs_zmm_vendor_tag-region          = record-region.
      gs_zmm_vendor_tag-time_zone       = record-time_zone.
      gs_zmm_vendor_tag-csttin          = record-csttin.
      gs_zmm_vendor_tag-vat             = record-vat.
      gs_zmm_vendor_tag-pan             = record-pan.
      gs_zmm_vendor_tag-servicetaxno    = record-servicetaxno.
      gs_zmm_vendor_tag-centralexciseno = record-centralexciseno.
      gs_zmm_vendor_tag-VENDOR_TYPE     = record-vtyp.
      APPEND gs_zmm_vendor_tag TO gt_zmm_vendor_tag.
      CLEAR : gs_zmm_vendor_tag , str1.
    ENDLOOP.
  ENDLOOP.

  IF gt_zmm_vendor_tag[] IS NOT INITIAL.
    MODIFY zmm_vendor_tag FROM TABLE gt_zmm_vendor_tag.
  ENDIF.
ENDFORM.                    "modify_table
*&---------------------------------------------------------------------*
*&      Form  upload_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_excel TABLES record.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                      = p_file
      i_begin_col                   = '1'
      i_begin_row                   = '2'
      i_end_col                     = '20'
      i_end_row                     = '10000'
    TABLES
      intern                        = intern[]
*   EXCEPTIONS
*     INCONSISTENT_PARAMETERS       = 1
*     UPLOAD_OLE                    = 2
*     OTHERS                        = 3
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
***  break ibm_ams.
  LOOP AT intern.
    IF intern-col = '0001'.
      PERFORM add_zeros USING intern-value CHANGING lifnr.
      intern-value = lifnr.
    ENDIF.
    MOVE: intern-col TO col.
    ASSIGN COMPONENT col OF STRUCTURE record TO <fs>.
    MOVE intern-value TO <fs>.
    AT END OF row.
      APPEND record.
      CLEAR : record.
    ENDAT.
  ENDLOOP.

ENDFORM.                    "upload_excel
*&---------------------------------------------------------------------*
*&      Form  add_zeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->A          text
*      -->B          text
*----------------------------------------------------------------------*
FORM add_zeros USING a TYPE any
               CHANGING b TYPE any.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = a
    IMPORTING
      output = b.

ENDFORM.                    "add_zeros
