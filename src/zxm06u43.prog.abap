*&---------------------------------------------------------------------*
*&  Include           ZXM06U43
*&---------------------------------------------------------------------*

 IF SY-TCODE = 'ME28' OR SY-TCODE = 'ME29N'.
   IF I_TRTYP = 'V'      AND I_EKKO-FRGKE = 'G'
                         AND I_EKKO-FRGRL = ''
                         AND I_BSTYP =  'F'.
     C_CI_EKKO-ZZUSRIDREL = SY-UNAME.
   ENDIF.
 ENDIF.
* BREAK 10106.
 DATA: WA_TEKPO LIKE LINE OF TEKPO.

* { Added by Shweta for PO octri
* Change by punam for ZLBT and remove JOCM for 1101 on dt 15.09.2013
 IF SY-TCODE = 'ME21N' OR SY-TCODE = 'ME22N'.
   CHECK I_EKKO-BSART = 'ZDOM'.
   LOOP AT TEKPO INTO WA_TEKPO.


*   READ TABLE TEKPO INTO WA_TEKPO."WITH KEY WERKS = '1101' .
   CASE WA_TEKPO-WERKS.
     WHEN '1101'.
*       READ TABLE TKOMV WITH KEY KSCHL = 'ZLBT'."'JOCM'."
*       IF SY-SUBRC NE 0.
*         MESSAGE E398(00) WITH 'Maintain LBT condition ZLBT'.
*       ENDIF.

*       READ TABLE TKOMV WITH KEY KSCHL = 'JOCM'."
*       IF SY-SUBRC = 0.
*         IF TKOMV-KBETR <> 0.
*           MESSAGE E398(00) WITH 'Remove JOCM Condition.'.
*         ENDIF.
*       ENDIF.

*       READ TABLE TKOMV WITH KEY KSCHL = 'ZETX'."
*       IF SY-SUBRC = 0.
*         MESSAGE E398(00) WITH 'Remove ZETX Condition.'.
*       ENDIF.

*     WHEN '1212'.
*       READ TABLE TKOMV WITH KEY KSCHL = 'ZLBT'."'JOCM'."
*       IF SY-SUBRC NE 0.
*         MESSAGE E398(00) WITH 'Maintain LBT condition ZLBT'.
*       ENDIF.
*
*       READ TABLE TKOMV WITH KEY KSCHL = 'JOCM'."
*       IF SY-SUBRC = 0.
*         IF TKOMV-KBETR <> 0.
*           MESSAGE E398(00) WITH 'Remove JOCM Condition '.
*         ENDIF.
*       ENDIF.
*
*       READ TABLE TKOMV WITH KEY KSCHL = 'ZETX'."
*       IF SY-SUBRC = 0.
*         MESSAGE E398(00) WITH 'Remove ZETX Condition.'.
*       ENDIF.

     WHEN '1204'.
*       READ TABLE TKOMV WITH KEY KSCHL = 'ZETX'."'JOCM'."
*       IF SY-SUBRC NE 0.
*         MESSAGE E398(00) WITH 'Maintain Entry TAX condition ZETX'.
*       ENDIF.

*       READ TABLE TKOMV WITH KEY KSCHL = 'JOCM'."
*       IF SY-SUBRC = 0.
**         IF TKOMV-KBETR <> 0.
*         MESSAGE E398(00) WITH 'Remove JOCM Condition.'.
**         ENDIF.
*       ENDIF.

*       READ TABLE TKOMV WITH KEY KSCHL = 'ZLBT'."
*       IF SY-SUBRC = 0.
*         MESSAGE E398(00) WITH 'Remove ZLBT Condition.'.
*       ENDIF.
     WHEN OTHERS.
       if WA_TEKPO-WERKS is not initial.
       READ TABLE TKOMV WITH KEY KSCHL = 'ZLBT'."
       IF SY-SUBRC = 0.
*         IF TKOMV-KBETR <> 0.
         MESSAGE E398(00) WITH 'Remove ZLBT Condition.'.
*         ENDIF.
       ENDIF.

       READ TABLE TKOMV WITH KEY KSCHL = 'ZETX'."
       IF SY-SUBRC = 0.
         MESSAGE E398(00) WITH 'Remove ZETX Condition.'.
       ENDIF.
       endif.
   ENDCASE.
     ENDLOOP.



*   CHECK I_EKKO-BSART = 'ZDOM'.
*   READ TABLE TEKPO WITH KEY WERKS = '1101' .
*   IF SY-SUBRC = 0.
*     READ TABLE TKOMV WITH KEY KSCHL = 'ZLBT'."'JOCM'."
*     IF SY-SUBRC NE 0.
*       MESSAGE E398(00) WITH 'Maintain LBT condition ZLBT'.
*     ENDIF.
*
*     READ TABLE TKOMV WITH KEY KSCHL = 'JOCM'."
*     IF SY-SUBRC = 0.
*       IF TKOMV-KBETR <> 0.
*         MESSAGE E398(00) WITH 'Make JOCM Condition value 0.00'.
*       ENDIF.
*     ENDIF.
*   ENDIF.
*
*   CHECK I_EKKO-BSART = 'ZDOM'.
*   READ TABLE TEKPO WITH KEY WERKS = '1212' .
*   IF SY-SUBRC = 0.
*     READ TABLE TKOMV WITH KEY KSCHL = 'ZLBT'."'JOCM'."
*     IF SY-SUBRC NE 0.
*       MESSAGE E398(00) WITH 'Maintain LBT condition ZLBT'.
*     ENDIF.
*
*     READ TABLE TKOMV WITH KEY KSCHL = 'JOCM'."
*     IF SY-SUBRC = 0.
*       IF TKOMV-KBETR <> 0.
*         MESSAGE E398(00) WITH 'Make JOCM Condition value 0.00'.
*       ENDIF.
*     ENDIF.
*   ENDIF.
*
*   CHECK I_EKKO-BSART = 'ZDOM'.
*   READ TABLE TEKPO WITH KEY WERKS = '1204' .
*   IF SY-SUBRC = 0.
*     READ TABLE TKOMV WITH KEY KSCHL = 'ZETX'."'JOCM'."
*     IF SY-SUBRC NE 0.
*       MESSAGE E398(00) WITH 'Maintain Entry TAX condition ZETX'.
*     ENDIF.
*
*     READ TABLE TKOMV WITH KEY KSCHL = 'JOCM'."
*     IF SY-SUBRC = 0.
*       IF TKOMV-KBETR <> 0.
*         MESSAGE E398(00) WITH 'Make JOCM Condition value 0.00'.
*       ENDIF.
*     ENDIF.
*   ENDIF.

 ENDIF.
* } End of Addition by Shweta
