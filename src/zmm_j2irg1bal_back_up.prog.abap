*&---------------------------------------------------------------------*
*& Report  ZMM_J2IRG1BAL_BACK_UP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmm_j2irg1bal_back_up.

TABLES : j_2irg1bal , zj2irg1bal.
DATA : lines  TYPE i,
       zlines TYPE i,

       gt_zj2irg1bal TYPE TABLE OF zj2irg1bal,
       gt_j_2irg1bal TYPE TABLE OF j_2irg1bal,

       gs_zj2irg1bal LIKE LINE OF gt_zj2irg1bal,
       gs_j_2irg1bal LIKE LINE OF gt_j_2irg1bal.


SELECT *
  FROM zj2irg1bal
  INTO TABLE gt_zj2irg1bal.

SELECT *
FROM j_2irg1bal
INTO TABLE gt_j_2irg1bal.

***IF sy-subrc = 0.
IF gt_j_2irg1bal[] IS NOT INITIAL.
  DESCRIBE TABLE gt_j_2irg1bal LINES lines.
    PERFORM zupdate_table.
endif.

SELECT *
  FROM zj2irg1bal
  INTO TABLE gt_zj2irg1bal.


  DESCRIBE TABLE gt_zj2irg1bal LINES zlines.
  IF zlines = lines .
    DELETE j_2irg1bal FROM TABLE gt_j_2irg1bal[].
  ENDIF.
***ENDIF.

*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zupdate_table.
  DELETE zj2irg1bal FROM TABLE gt_zj2irg1bal.
  refresh : gt_zj2irg1bal[].
  gt_zj2irg1bal[] = gt_j_2irg1bal[].
  MODIFY zj2irg1bal FROM TABLE gt_zj2irg1bal[].
  refresh : gt_zj2irg1bal[].
ENDFORM.                    "update_table
