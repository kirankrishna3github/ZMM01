*&---------------------------------------------------------------------*
*& Report  Z6MM001R_PURCHASE_ORDER
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Purchase Order Printing
* OBJECT TYPE       : Layout Program   FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 10.06.2009
*        DEV REQUEST: IRDK900092
*              TCODE: ME21N,ME22N,ME23N
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

***********************************************************************
*  Program Name : Z6MM001R_PURCHASE_ORDER                             *
*  Program Title: Purchase Order Printing                             *
*  Program type : layout Program                                      *
*  Functional Consultant : Girish                                     *
*  Technical  Consultant : Naveen kumar                               *
*  Create Date  : 26.08.2009                                          *
*  Request No.  : FEDK900324                                          *
*  T.Code       : ME21N,ME22N,ME23N                                   *
*  Description  : Purchase Order Printing                             *
*----------------------------------------------------------------------
REPORT  z6mm006r_purchase_order_new.
TABLES: nast ,
        ekpo ,
        ekko.

DATA : i_ekko LIKE ekko OCCURS 0 WITH HEADER LINE.
data : it_pdfdata like tline OCCURS 0 WITH HEADER LINE.
DATA : i_ekpo LIKE ekpo OCCURS 0 WITH HEADER LINE.
DATA : i_ml_esll LIKE ml_esll OCCURS 0 WITH HEADER LINE.
*{   REPLACE        SBXK900028                                        1
*\DATA : i_konv LIKE konv OCCURS 0 WITH HEADER LINE.
DATA : i_konv LIKE prcd_elements OCCURS 0 WITH HEADER LINE.
*}   REPLACE
data : i_komv like komv occurs 0 with header line.
DATA : tt_komv type komv_itab.
data : wa_komv type komv,
       wa_komv_jvcs type komv.
DATA : i_mdsb LIKE mdsb OCCURS 0 WITH HEADER LINE.
DATA : wa_lfm1 LIKE lfm1.
DATA : wrk_filesiz(10) TYPE c.

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

ENDFORM.                    "entry_neu

*---------------------------------------------------------------------*
*      -->ENT_RETCO  text
*      -->ENT_SCREEN text
*---------------------------------------------------------------------*
FORM adobe_entry_neu USING ent_retco  LIKE sy-subrc
                           ent_screen TYPE c.
  DATA: xdruvo TYPE c,
        L_XFZ type c.

  IF nast-aende EQ space.
    xdruvo = '1'.
  ELSE.
    xdruvo = '2'.
  ENDIF.
  PERFORM clear_refresh.
  po_code = nast-objky(10) .
  PERFORM getdata_into_ekko_ekpo.

  PERFORM adobe_print_output USING    xdruvo
                                      ent_screen
                                      L_XFZ
                             CHANGING ent_retco.
  perform sending_mail_new.
ENDFORM.                    "adobe_entry_neu
*&---------------------------------------------------------------------*
*&      Form  CLEAR_REFRESH
*&---------------------------------------------------------------------*
FORM clear_refresh .
  CLEAR :
            i_ekko , i_ekpo , i_konv , i_komv ,
            po_code, wa_lfm1.
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
  IF sy-subrc = 0 .
    IF i_ekko-frgke = 'B'.
      MESSAGE e398(00) WITH 'Released the PO' '' '' ''..
    ENDIF.

    SELECT SINGLE * FROM lfm1 INTO wa_lfm1 WHERE lifnr = i_ekko-lifnr.
*{   REPLACE        SBXK900028                                        1
*\    SELECT * FROM konv
    SELECT * FROM prcd_elements
*}   REPLACE
    into CORRESPONDING fields of table i_konv
        WHERE knumv = i_ekko-knumv.

  ENDIF.

*  DELETE i_konv WHERE kschl = 'FRA2' AND lifnr <> i_ekko-lifnr." AND lifnr NE ''.
*
*  DELETE i_konv WHERE kschl = 'FRB2' AND lifnr <> i_ekko-lifnr." AND lifnr NE ''.
*
*  DELETE i_konv WHERE kschl = 'FRC2' AND lifnr <> i_ekko-lifnr." AND lifnr NE ''.
*
**    ----added by supriya on 29/7/10
*  DELETE i_konv WHERE kschl = 'ZFBT' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'FRA1' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'FRB1' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'FRC1' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'JOCM' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'ZJO2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'ZUN2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'ZDN2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*  DELETE i_konv WHERE kschl = 'ZOC2' AND lifnr <> i_ekko-lifnr ."AND lifnr NE ''.
*   ----

  IF NOT i_ekpo[] IS INITIAL.
    SELECT *
         FROM ml_esll
         INTO CORRESPONDING FIELDS OF TABLE i_ml_esll
         FOR ALL ENTRIES IN i_ekpo
         WHERE ebeln = i_ekpo-ebeln AND
         del NE 'X'.
    SELECT *
         FROM mdsb
         INTO CORRESPONDING FIELDS OF TABLE i_mdsb
         FOR ALL ENTRIES IN i_ekpo
         WHERE ebeln = i_ekpo-ebeln AND
         xloek NE 'X'.
  ENDIF.

*    CALL FUNCTION 'Z_M_TAXES_PRICING'
*      EXPORTING
*        yebeln = po_code
*      TABLES
*        ytkomv = i_komv.

  clear tt_komv.

  CALL FUNCTION 'Z6MM_PO_CAL_TAX'
    EXPORTING
      I_EBELN       = po_code
*    I_EBELP       =
    IMPORTING
      TT_KOMV       = tt_komv
            .


  if not tt_komv[] is initial.

    loop at tt_komv into wa_komv.
      move-CORRESPONDING wa_komv to i_komv.
      append i_komv.
      clear  i_komv.
    endloop.

  endif.

*  ---read jvcs
read table i_komv into wa_komv with key
kschl = 'JVCS'.
  IF SY-SUBRC = 0.
  wa_komv_jvcs = WA_komv.
  CLEAR WA_KOMV.
  ENDIF.

*    DELETE i_komv WHERE kwert EQ 0
*        OR kschl EQ 'BASB'
*        OR kschl EQ 'JMX1'
*        OR kschl EQ 'JMX2'
*        OR kschl EQ 'JAX1'
*        OR kschl EQ 'JAX2'
*        OR kschl EQ 'JSX1'
*        OR kschl EQ 'JSX2'
*        OR kschl EQ 'JEX1'
*        OR kschl EQ 'JEX2'
*        OR kschl EQ 'JHX1'
*        OR kschl EQ 'JHX2'
*        OR kschl EQ 'NAVS'
*        OR kschl EQ 'NAVM'.

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

  IF wa_lfm1-kalsk = '02'.
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
        OR   kschl EQ 'ZPK3'
        ).
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
      EXPORTING  formname           = 'Z6MM006S_PURCHASE_ORDER_NEW'
*                 variant            = ' '
*                 direct_call        = ' '
       IMPORTING  fm_name            = lf_fm_name
       EXCEPTIONS no_form            = 1
                  no_function_module = 2
                  OTHERS             = 3.
  CALL FUNCTION lf_fm_name
 EXPORTING
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
    wa_komv_jvcs    = wa_komv_jvcs
    TABLES
      x_mdsb                     =  i_mdsb
      x_ekko                     =  i_ekko
      x_ekpo                     =  i_ekpo
      x_ml_esll                  =  i_ml_esll
      x_konv                     =  i_konv
      x_komv                     =  i_komv
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

INCLUDE Z6MM006R_PURCHASE_ORDER_ADOF01.

INCLUDE Z6MM006R_PURCHASE_ORDER_FILF01.
