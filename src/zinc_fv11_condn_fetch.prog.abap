*&---------------------------------------------------------------------*
*&  Include           ZINC_FV11_CONDN_FETCH
*&---------------------------------------------------------------------*

FORM a359_fetch .
  SELECT *
    FROM a359
    INTO TABLE it_a359
    WHERE kappl EQ 'TX'
    AND   kschl EQ 'JVRD'
    AND   datbi > '20170630'
    AND   datab <= sy-datum.
ENDFORM.

FORM a504_fetch .
  SELECT *
    FROM a504
    INTO TABLE it_a504
    WHERE kappl EQ 'TX'
    AND   kschl IN ( 'JMOP',
                     'JMX1',
                     'JEC1',
                     'JSEP',
                     'JEX1',
                     'JHX1',
                     'JVRD',
                     'JVCS' )
    AND   datbi > '20170630'
    AND   datab <= sy-datum.
ENDFORM.

FORM a515_fetch .
  SELECT *
    FROM a515
    INTO TABLE it_a515
    WHERE kappl EQ 'TX'
    AND   kschl IN ( 'JMOP',
                     'JMX1',
                     'JEC1',
                     'JSEP',
                     'JEX1',
                     'JHX1',
                     'JVRD',
                     'JVCS' )
    AND   datbi > '20170630'
    AND   datab <= sy-datum.
ENDFORM.

FORM a519_fetch .
  SELECT *
    FROM a519
    INTO TABLE it_a519
    WHERE kappl EQ 'TX'
    AND   kschl IN ( 'JMOP',
                     'JMX1',
                     'JEC1',
                     'JVRD',
                     'JVCS' )
    AND   datbi > '20170630'
    AND   datab <= sy-datum.
ENDFORM.

FORM a536_fetch .
  SELECT *
    FROM a536
    INTO TABLE it_a536
    WHERE kappl EQ 'TX'
    AND   kschl IN ( 'JMOP', 'JVRD' )
    AND   datbi > '20170630'
    AND   datab <= sy-datum.
ENDFORM.
