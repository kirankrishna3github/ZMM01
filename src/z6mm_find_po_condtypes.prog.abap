*&---------------------------------------------------------------------*
*& Report  Z6MM_FIND_PO_CONDTYPES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z6mm_find_po_condtypes.

*{   REPLACE        SBXK900019                                        1
*\TABLES: konv.
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************
TABLES: PRCD_ELEMENTS.
*}   REPLACE
TYPES: BEGIN OF ty_konv,
*{   REPLACE        SBXK900019                                        2
*\        kschl TYPE konv-kschl,
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************
        kschl TYPE PRCD_ELEMENTS-kschl,
*}   REPLACE
       END OF ty_konv.

DATA: t_konv TYPE TABLE OF ty_konv,
      w_konv TYPE ty_konv.

START-OF-SELECTION.

  SELECT kschl
*{   REPLACE        SBXK900019                                        1
*\    FROM konv
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************

    FROM PRCD_ELEMENTS
*}   REPLACE
    INTO TABLE t_konv
    WHERE kappl EQ 'M'
      AND kschl LIKE 'Z%'.
  SORT t_konv BY kschl.
  DELETE ADJACENT DUPLICATES FROM t_konv COMPARING ALL FIELDS.

  IF t_konv[] IS NOT INITIAL.
    WRITE: 'Used Condition Type'.
    LOOP AT t_konv INTO w_konv.
      WRITE: / sy-tabix, w_konv-kschl.
      CLEAR: w_konv.
    ENDLOOP.
  ENDIF.

  WRITE: / '--------------------- End Of Program.--------------------------------'.
