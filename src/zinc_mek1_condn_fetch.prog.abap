*&---------------------------------------------------------------------*
*&  Include           ZINC_MEK1_CONDN_FETCH
*&---------------------------------------------------------------------*

FORM a445_fetch .
*SELECT *
*  FROM a445
*  INTO TABLE it_a445
*  WHERE kappl EQ 'M'
*  AND   kschl IN ( 'ZEX1',
*                   'ZEX2',
*                   'ZEX3' )
*  AND   datbi > '20170630'
*  AND   datab <= sy-datum.
SELECT *
  FROM a445
  INTO TABLE it_a445
  WHERE kappl EQ 'M'
  AND   kschl IN ( 'ZSTR' )
  AND   datbi > sy-datum
  AND   datab < '20170630'.

ENDFORM.

FORM a505_fetch .
SELECT *
    FROM a505
    INTO TABLE it_a505
    WHERE kappl EQ 'M'
    AND   kschl IN ( 'JCV1',
                     'JECV',
                     'J1CV',
                     'JCDB',
                     'JCE1',
                     'JCE2',
                     'ZLBT',
                     'JOCM',
                     'JADC',
                     'ZCH3' )
    AND   datbi > '20170630'
    AND   datab <= sy-datum.
ENDFORM.

FORM a507_fetch .
SELECT *
    FROM a507
    INTO TABLE it_a507
    WHERE kappl EQ 'M'
    AND   kschl IN ( 'JCV1',
                     'JECV',
                     'J1CV',
                     'JCDB',
                     'JCE1',
                     'JCE2',
                     'ZLBT',
                     'JOCM',
                     'JADC',
                     'ZCH3' )
    AND   datbi > '20170630'
    AND   datab <= sy-datum.
ENDFORM.

FORM a516_fetch .
*SELECT *
*  FROM a516
*  INTO TABLE it_a516
*  WHERE kappl EQ 'M'
*  AND   kschl IN ( 'ZCH3', 'ZETX', 'STRD', 'ZEX1', 'ZEX2', 'ZEX3' )
*  AND   datbi > '20170630'
*  AND   datab <= sy-datum.


SELECT *
  FROM a516
  INTO TABLE it_a516
  WHERE kappl EQ 'M'
  AND   kschl IN ( 'ZSTR' )
  AND   datbi > sy-datum
  AND   datab < '20170630'.
ENDFORM.

FORM a518_fetch .
*SELECT *
*  FROM a518
*  INTO TABLE it_a518
*  WHERE kappl EQ 'M'
*  AND   kschl IN ( 'STRD',
*                   'ZEX1',
*                   'ZEX2',
*                   'ZEX3' )
*  AND   datbi > '20170630'
*  AND   datab <= sy-datum.


SELECT *
  FROM a518
  INTO TABLE it_a518
  WHERE kappl EQ 'M'
  AND   kschl IN ( 'ZSTR' )
  AND   datbi > sy-datum
  AND   datab < '20170630'.
ENDFORM.

FORM a540_fetch .
*SELECT *
*  FROM a540
*  INTO TABLE it_a540
*  WHERE kappl EQ 'M'
*  AND   kschl EQ 'ZEX1'
*  AND   datbi > '20170630'
*  AND   datab <= sy-datum.


 SELECT *
  FROM a540
  INTO TABLE it_a540
  WHERE kappl EQ 'M'
  AND   kschl IN ( 'ZSTR' )
  AND   datbi > sy-datum
  AND   datab < '20170630'.

ENDFORM.

FORM a541_fetch .
SELECT *
  FROM a541
  INTO TABLE it_a541
  WHERE kappl EQ 'M'
  AND   kschl EQ 'JOCM'
  AND   datbi > '20170630'
  AND   datab <= sy-datum.
ENDFORM.
