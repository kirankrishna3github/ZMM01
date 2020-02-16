*&---------------------------------------------------------------------*
*& Report  Z6MM003R_PURCHASE_ORDER_K
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Purchase Contract Printing
* OBJECT TYPE       : Layout Program  FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna Konda
*      CREATION DATE: 10.06.2010
*        DEV REQUEST: IRDK900
*             TCODE : ME31K,ME32K,ME33K
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

*{   INSERT         SBXK900028                                        2
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Sunday, October 07, 2018 23:39:51
* Changed By - 6010859 - SaurabhK
* Purpose    - Simplification list - 2267308 - konv replaced by prcd_elements
* Solution   - konv replaced by prcd_elements
* TR         - SBXK900028       6010859      S4H: S_K: Simplification List: 03.10.2018
*--------------------------------------------------------------------*
*}   INSERT

REPORT  Z6MM009R_PURCHASE_ORDER_CONT.
TABLES: nast ,
        ekpo ,
        ekko ,
        a016 .

DATA : i_ekko LIKE ekko OCCURS 0 WITH HEADER LINE.
DATA : i_ekpo LIKE ekpo OCCURS 0 WITH HEADER LINE.
DATA : i_ml_esll LIKE ml_esll OCCURS 0 WITH HEADER LINE.
*{   REPLACE        SBXK900028                                        1
*\DATA : i_konv LIKE konv OCCURS 0 WITH HEADER LINE.
DATA : i_konv LIKE prcd_elements OCCURS 0 WITH HEADER LINE.
*}   REPLACE
data : i_komv like komv occurs 0 with header line.
DATA : i_a016 LIKE a016 OCCURS 0 WITH HEADER LINE.
DATA : i_konp LIKE konp OCCURS 0 WITH HEADER LINE.
DATA : tt_komv type komv_itab.
data : wa_komv type komv.
DATA : i_mdsb LIKE mdsb OCCURS 0 WITH HEADER LINE.
DATA : wa_lfm1 LIKE lfm1.

DATA: po_code    TYPE ekpo-ebeln.

*---------------------------------------------------------------------*
*       Form  YMPR_PO_PRINT
*---------------------------------------------------------------------*
FORM entry_neu
  USING ent_retco
        ent_screen.
  PERFORM clear_refresh.
  po_code = nast-objky(10) .
  PERFORM getdata_into_ekko_ekpo.

  PERFORM print_smart_form.
  CLEAR ent_retco .

ENDFORM."Z6MM005R_PURCHASE_ORDER
*&---------------------------------------------------------------------*
*&      Form  CLEAR_REFRESH
*&---------------------------------------------------------------------*
FORM clear_refresh .
  CLEAR :
            i_ekko , i_ekpo , i_konv , i_komv ,
            po_code, wa_lfm1 .
  REFRESH :
            i_ekko , i_ekpo , i_konv , i_komv
            .
ENDFORM.                    " CLEAR_REFRESH
*&---------------------------------------------------------------------*
*&      Form  GETDATA_INTO_EKKO_EKPO
*&---------------------------------------------------------------------*
FORM getdata_into_ekko_ekpo .
  SELECT * INTO TABLE i_ekko
           FROM ekko
           WHERE ebeln = po_code
           AND loekz = ''.
  SELECT * INTO TABLE i_ekpo
         FROM ekpo
         WHERE ebeln = po_code
         AND   loekz = '' .
  READ TABLE i_ekko INDEX 1.
  IF sy-subrc = 0.
    SELECT SINGLE * FROM lfm1 INTO wa_lfm1 WHERE lifnr = i_ekko-lifnr.

*{   REPLACE        SBXK900028                                        1
*\    SELECT * FROM konv
    SELECT * FROM prcd_elements
*}   REPLACE
    into CORRESPONDING fields of table i_konv
        WHERE knumv = i_ekko-knumv.

    SELECT * FROM a016
        INTO CORRESPONDING FIELDS OF TABLE i_a016
        WHERE evrtn = i_ekko-ebeln AND
              kappl = 'M'.

    IF sy-subrc = 0.
      SELECT * FROM konp
          INTO CORRESPONDING FIELDS OF TABLE i_konp
          FOR ALL ENTRIES IN i_a016
          WHERE LOEVM_KO <> 'X' AND  knumh = i_a016-knumh. " ADDED BY GANESH 29.06.2009 ( LOEVM_KO <> 'X')
    ENDIF.

  ENDIF.

      DELETE I_KONP WHERE KSCHL = 'FRA2' and LIFNR <> i_ekko-lifnr and LIFNR NE ''.

      DELETE I_KONP WHERE KSCHL = 'FRB2' and LIFNR <> i_ekko-lifnr and LIFNR NE ''.

      DELETE I_KONP WHERE KSCHL = 'FRC2' and LIFNR <> i_ekko-lifnr and LIFNR NE ''.


  IF NOT i_ekpo[] IS INITIAL.
    SELECT *
         FROM ml_esll
         INTO CORRESPONDING FIELDS OF TABLE i_ml_esll
         FOR ALL ENTRIES IN i_ekpo
         WHERE ebeln = i_ekpo-ebeln AND
         del NE 'X' .
    SELECT *
         FROM mdsb
         INTO CORRESPONDING FIELDS OF TABLE i_mdsb
         FOR ALL ENTRIES IN i_ekpo
         WHERE ebeln = i_ekpo-ebeln AND
         xloek NE 'X' .


  ENDIF.
  clear tt_komv.

 CALL FUNCTION 'Z6MM_PO_CAL_TAX'
   EXPORTING
     I_EBELN       = po_code
*    I_EBELP       =
   IMPORTING
     TT_KOMV       = tt_komv.

 if not tt_komv[] is initial.

    loop at tt_komv into wa_komv.
      move-CORRESPONDING wa_komv to i_komv.
      append i_komv.
      clear  i_komv.
    endloop.

  endif.

      DELETE i_komv WHERE kwert EQ 0
        OR kschl EQ 'JMX1'
        OR kschl EQ 'JMX2'
        OR kschl EQ 'JAX1'
        OR kschl EQ 'JAX2'
        OR kschl EQ 'JSX1'
        OR kschl EQ 'JSX2'
        OR kschl EQ 'JEX1'
        OR kschl EQ 'JEX2'
        OR kschl EQ 'JHX1'
        OR kschl EQ 'JHX2'
        OR kschl EQ 'NAVS'
        OR kschl EQ 'NAVM'.

    delete i_komv where koaid eq 'A'.
    CLEAR  I_KOMV.

    DELETE i_konv WHERE kwert EQ 0
        OR kschl EQ 'JEXS'.


  IF wa_lfm1-kalsk = '01'.
    DELETE i_konv WHERE NOT ( kschl EQ 'PB00'
        OR   kschl EQ 'PBXX'
        OR   kschl EQ 'R000'
        OR   kschl EQ 'R001'
        OR   kschl EQ 'R002'
        OR   kschl EQ 'R003'
        OR   kschl EQ 'KR00'
        OR   kschl EQ 'K000'
        OR   kschl EQ 'ZPK1'
        OR   kschl EQ 'ZPK2'
        OR   kschl EQ 'ZPK3' ).

    DELETE i_konp WHERE NOT ( kschl EQ 'PB00'
        OR   kschl EQ 'PBXX'
        OR   kschl EQ 'R000'
        OR   kschl EQ 'R001'
        OR   kschl EQ 'R002'
        OR   kschl EQ 'R003'
        OR   kschl EQ 'KR00'
        OR   kschl EQ 'K000'
        OR   kschl EQ 'ZPK1'
        OR   kschl EQ 'ZPK2'
        OR   kschl EQ 'ZPK3' ).

  ENDIF.


ENDFORM.                    " GETDATA_INTO_EKKO_EKPO
*&---------------------------------------------------------------------*
*&      Form  PRINT_SMART_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_smart_form .

  DATA: lf_fm_name TYPE rs38l_fnam.
  READ TABLE i_ekpo INDEX 1.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING  formname           = 'Z6MM009S_CONT_PURC_ORDER'
*                 variant            = ' '
*                 direct_call        = ' '
       IMPORTING  fm_name            = lf_fm_name
       EXCEPTIONS no_form            = 1
                  no_function_module = 2
                  OTHERS             = 3.

  CALL FUNCTION lf_fm_name
* EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
*   CONTROL_PARAMETERS         =
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
* IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*   JOB_OUTPUT_INFO            =
*   JOB_OUTPUT_OPTIONS         =
    TABLES
      x_mdsb                     =  i_mdsb
      x_ekko                     =  i_ekko
      x_ekpo                     =  i_ekpo
      x_ml_esll                  =  i_ml_esll
      x_konv                     =  i_konv
      x_komv                     =  i_komv
      x_konp                     =  i_konp
      x_a016                     =  i_a016

* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " PRINT_SMART_FORM
