*&---------------------------------------------------------------------*
*&  Include           Z6MM010C_SERVICE_MASTER_SUB
*&---------------------------------------------------------------------*
FORM get_data_from_file  TABLES   tdata STRUCTURE tdata
                         USING    fname.

  DATA filename TYPE string. CLEAR filename.

  filename = fname.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = filename
      filetype                = 'ASC'
      has_field_separator     = 'X'
      read_by_line            = 'X'
    TABLES
      data_tab                = tdata
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
    MESSAGE 'Error Occured while Uploading from File' TYPE 'E'.
  ENDIF.


ENDFORM.                    " get_data_from_file
*&---------------------------------------------------------------------*
*&      Form  upload_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TDATA  text
*      -->BDCDATA  text
*----------------------------------------------------------------------*
FORM upload_data  TABLES   tdata   STRUCTURE tdata
                           bdcdata STRUCTURE bdcdata.

  DATA : BEGIN OF xline OCCURS 100,
         line(70) TYPE c,
         END   OF xline.
  DATA   len TYPE i.

  LOOP AT tdata.
    CLEAR: bdcdata, bdcdata[].
    CLEAR: xline, xline[].
    CLEAR  len.

    len = STRLEN( tdata-ltext ).

    DO.
      IF len = 0.
        EXIT.
      ENDIF.

      len = STRLEN( tdata-ltext ).

      IF len > 70.
        xline-line = tdata-ltext+0(70).
      ELSE.
        xline-line = tdata-ltext+0(len).
        len = 0.
      ENDIF.
      APPEND xline. CLEAR xline.

      SHIFT tdata-ltext LEFT BY 70 PLACES.
    ENDDO.

    PERFORM bdc_dynpro      USING 'SAPLBAS0' '0300'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'ASMD-ASNUM'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=NEW'.
    PERFORM bdc_field       USING 'RM63T-SPRAS'
                                  'EN'.

    PERFORM bdc_dynpro      USING 'SAPLBAS0' '0300'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=TXTEDIT'.
    PERFORM bdc_field       USING 'ASMD-ASNUM'
                                  tdata-asnum.
    PERFORM bdc_field       USING 'ASMDT-ASKTX'
                                  tdata-asktx.
    PERFORM bdc_field       USING 'ASMD-MEINS'
                                  tdata-meins.
    PERFORM bdc_field       USING 'ASMD-MATKL'
                                  tdata-matkl.
    PERFORM bdc_field       USING 'ASMD-SPART'
                                  tdata-spart.
    PERFORM bdc_field       USING 'ASMD-BKLAS'
                                  tdata-bklas.
    PERFORM bdc_field       USING 'ASMD-FORMELNR'
                                  tdata-formelnr.
    PERFORM bdc_field       USING 'ASMD-ASTYP'
                                  tdata-astyp.
*    PERFORM bdc_field       USING 'RM63T-SPRAS'
*                                  'EN'.
    PERFORM bdc_dynpro      USING 'SAPLSTXX' '1100'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'RSTXT-TXLINE(01)'.

    DATA xindex TYPE i. CLEAR xindex.

    LOOP AT xline.
      xindex = xindex + 1.

      IF xindex <> 1.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=POSF'.
        PERFORM bdc_dynpro      USING 'SAPLSTXX' '1100'.
        PERFORM bdc_field       USING 'RSTXT-TXPARGRAPH(03)'
                                      '='.
        PERFORM bdc_field       USING 'RSTXT-TXLINE(03)'
                                      xline-line.
        CONTINUE.
      ENDIF.
      PERFORM bdc_field       USING 'RSTXT-TXPARGRAPH(02)'
                                    '*'.
      PERFORM bdc_field       USING 'RSTXT-TXLINE(02)'
                                    xline-line.

    ENDLOOP.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=TXBA'.

    PERFORM bdc_dynpro      USING 'SAPLBAS0' '0300'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=POST'.

    CALL TRANSACTION 'AC02' USING bdcdata MODE 'A' UPDATE 'S'.

  ENDLOOP.

  SUBMIT Z6MM012C_SERVICE_MASTER_UPL VIA SELECTION-SCREEN.

ENDFORM.                    " upload_data

*&--------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*&--------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
