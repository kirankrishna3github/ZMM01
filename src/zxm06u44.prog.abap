*&---------------------------------------------------------------------*
*&  Include           ZXM06U44*
*&---------------------------------------------------------------------*
*IF SY-TCODE = 'ME21N' ."AND SY-UCOMM = 'MESAVE'.
*  DATA: doknr TYPE doknr.
**BREAK-POINT.
*GET PARAMETER ID 'CV1' FIELD doknr.
*
*if doknr is NOT INITIAL.
*** Document data number, version, change number, et cetera.
*DATA: ls_doc    LIKE bapi_doc_draw2,
*
*
*** Indicator for change relevance
*      ls_docx   LIKE bapi_doc_drawx2,
*
*** Bapi return structure
*      ls_return LIKE bapiret2,
*
*** Originals
*      lt_files LIKE bapi_doc_files OCCURS 0 WITH HEADER LINE,
*
*** Short texts
*      lt_drat  LIKE bapi_doc_drat OCCURS 0 WITH HEADER LINE,
*
*** Object links
*      lt_drad  LIKE bapi_doc_drad OCCURS 0 WITH HEADER LINE.
*
*** Assign document data
* ls_doc-documenttype    = 'Z01'.
* ls_doc-documentnumber  = doknr.
* ls_doc-documentversion = '00'.
* ls_doc-documentpart    = '000'.
*
** ls_doc-description    = '''PO DMS Attachment'.
* ls_doc-laboratory     = ''.
*
***  Set indicator for change relevance
* ls_docx-description    = 'X'.
* ls_docX-laboratory     = 'X'.
*
*** Insert object links
* CLEAR lt_drad.
* REFRESH lt_drad.
* lt_drad-objecttype = 'EKKO'.
* lt_drad-objectkey  = I_EKKO-EBELN.
* APPEND lt_drad.
***----------------------------------------------------------------------
*** Change document
***----------------------------------------------------------------------
*  CALL FUNCTION 'BAPI_DOCUMENT_CHANGE2'
*       EXPORTING: documenttype    = ls_doc-documenttype
*                  documentnumber  = ls_doc-documentnumber
*                  documentpart    = ls_doc-documentpart
*                  documentversion = ls_doc-documentversion
*                  documentdata    = ls_doc
*                  documentdatax   = ls_docx
*       IMPORTING: return          = ls_return
*       TABLES:    objectlinks     = lt_drad.
*
*** Did an error occur ??
* IF ls_return-type CA 'EA'.
*   ROLLBACK WORK.
*   MESSAGE ID '26' TYPE 'I' NUMBER '000'
*           WITH ls_return-message.
*
* ELSE.
*   COMMIT WORK.
* ENDIF.
*
* ENDIF.
*
*
**BREAK-POINT.
*
*ENDIF.
