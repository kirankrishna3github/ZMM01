*&---------------------------------------------------------------------*
*& Report  Z_CHECK_UPDATE_XBLNR
*&
*&---------------------------------------------------------------------*
*&
*& The report can be used to check and update XBLNR (Reference) in
*& invoices which are created reference to PO/item with WEBRE = 'X'.
*&
*&---------------------------------------------------------------------*

REPORT Z_CHECK_UPDATE_XBLNR.

DATA: S_RBKP TYPE RBKP.
DATA: T_RBKP LIKE TABLE OF S_RBKP.
DATA: S_RSEG TYPE RSEG.
DATA: T_RSEG LIKE TABLE OF S_RSEG.
DATA: S_EKBE TYPE EKBE.
DATA: U_EKBE TYPE EKBE.
DATA: F_NUM  TYPE I.

SELECT-OPTIONS:
  SO_BELNR FOR S_RBKP-BELNR,
  SO_GJAHR FOR S_RBKP-GJAHR.

PARAMETERS:
  P_UPDATE TYPE C.

SELECT * FROM RBKP AS A
INTO TABLE T_RBKP
WHERE BELNR  IN SO_BELNR
AND   GJAHR  IN SO_GJAHR
AND   RBSTAT IN ('A','B','5')
AND   EXISTS
(
  SELECT *
  FROM RSEG AS B
  WHERE B~BELNR  EQ A~BELNR
  AND   B~GJAHR  EQ A~GJAHR
  AND   B~XBLNR  NE ' '
).

IF SY-SUBRC NE 0.
  WRITE:/'No documents are found'.
  EXIT.
ENDIF.

SORT T_RBKP BY GJAHR BELNR.
CLEAR F_NUM.

WRITE: AT  0(10) 'RSEG',
      AT  20(10) 'IV No',
      AT  35(10) 'FI Year',
      AT  45(10) 'IV Item',
      AT  65(10) 'OLD XBLNR',
      AT  85(10) 'NEW XBLNR'.
ULINE.

LOOP AT T_RBKP INTO S_RBKP.

  SELECT * FROM RSEG AS A
  INTO TABLE T_RSEG
  WHERE BELNR EQ S_RBKP-BELNR
  AND   GJAHR EQ S_RBKP-GJAHR
  AND   LFBNR NE ' '
  AND   EXISTS
  (
    SELECT *
    FROM EKBE AS B
    WHERE B~EBELN EQ A~EBELN
    AND   B~EBELP EQ A~EBELP
    AND   B~VGABE IN ('1')
    AND   B~LFGJA EQ A~LFGJA
    AND   B~LFBNR EQ A~LFBNR
    AND   B~LFPOS EQ A~LFPOS
    AND   B~XBLNR NE ' '
    AND   B~XBLNR NE A~XBLNR
  ).

  IF SY-SUBRC = 0.

    LOOP AT T_RSEG INTO S_RSEG.

      SELECT SINGLE *
      FROM EKBE
      INTO S_EKBE
      WHERE EBELN EQ S_RSEG-EBELN
      AND   EBELP EQ S_RSEG-EBELP
      AND   VGABE IN ('1')
      AND   LFGJA EQ S_RSEG-LFGJA
      AND   LFBNR EQ S_RSEG-LFBNR
      AND   LFPOS EQ S_RSEG-LFPOS.

      SELECT SINGLE *
      FROM EKBE
      INTO U_EKBE
      WHERE EBELN EQ S_RSEG-EBELN
      AND   EBELP EQ S_RSEG-EBELP
      AND   VGABE IN ('2','3','P')
      AND   GJAHR EQ S_RSEG-GJAHR
      AND   BELNR EQ S_RSEG-BELNR
      AND   BUZEI EQ S_RSEG-BUZEI.

      F_NUM = F_NUM + 1.
      WRITE :/' ',
      AT  20(10) S_RSEG-BELNR,
      AT  35(4)  S_RSEG-GJAHR,
      AT  45(6)  S_RSEG-BUZEI,
      AT  65(16) S_RSEG-XBLNR,
      AT  85(16) S_EKBE-XBLNR.

      IF P_UPDATE EQ 'X'.

        S_RSEG-XBLNR = S_EKBE-XBLNR.
        UPDATE RSEG FROM S_RSEG.

        U_EKBE-XBLNR = S_EKBE-XBLNR.
        UPDATE EKBE FROM U_EKBE.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDLOOP.

IF P_UPDATE EQ 'X'.
  WRITE : / F_NUM, 'Invoice Item(s) have been updated.' .
ELSE.
  WRITE : / F_NUM, 'Invoice Item(s) must be updated.' .
ENDIF.
