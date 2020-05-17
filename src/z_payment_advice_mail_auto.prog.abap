*&---------------------------------------------------------------------*
*& Report  Z6FI004R_PAYMENT_ADVICE_MAIL
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_PAYMENT_ADVICE_MAIL_AUTO NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65.
*&---------------------------------------------------------------------*
*& Report  Z6FI004R_PAYMENT_ADVICE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*REPORT ZRPTFI016 NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65.
*&----------------------------------------------------------------&*
*  Dev. Class   : ZFI01                                             *
*  Program Name : Z6FI004R_PAYMENT_ADVICE                                       *
*               :                     *
*  Description  : Covering Letter for Payment Advice               *
*  Author       : Ramakrishna         Created on : 26.10.2010      *
*  Tcode        : ZFIPAD                                                   *
*&----------------------------------------------------------------&*
*  S O U R C E   C O D E   C H A N G E   H I S T O R Y             *
*&----------------------------------------------------------------&*
*  CODE     | AUTHOR     | DATE     |   Description                *
*&---------------------- -----------------------------------------&*
*           |            |          |                              *
*&----------------------------------------------------------------&*
* REVISION HISTORY-------------------------------------------------*
*
*        REVISION NO:
*        DEVELOPER:   Naren Karra             DATE:   12.10.2015
*        DESCRIPTION: FI: New Authorization added for Company Code
*------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:
*        DEVELOPER:   Naren Karra             DATE:   20.10.2015
*        DESCRIPTION: FI: Authorization code snippet modified
*----------------------------------------------------------------------*
*&----------------------------------------------------------------&*
*       TABLE  DECLARATIONS                                        *
*&----------------------------------------------------------------&*

TABLES: BSEG,
        BKPF,
*        LFA1,
*        KNA1,
        SKAT,
        PAYR,
*        T001,
        T880,
        REGUP,
        REGUH,
*        ADRC,
        BSAK,
        BSIK.
*&----------------------------------------------------------------&*
*       DATA  DECLARATIONS                                        *
*&----------------------------------------------------------------&*

*DATA: BEGIN OF it_items OCCURS 0,
*        bukrs  type bukrs,
*        VBLNR   TYPE PAYR-VBLNR,
*        belnr   LIKE bkpf-belnr,
*        sgtxt   LIKE bseg-sgtxt,
*        xblnr   LIKE bkpf-xblnr,
*        wrbtr   LIKE bseg-wrbtr,
*        gjahr   like bkpf-gjahr,
*      END OF it_items.
*
DATA : IT_ITEMS LIKE ZZLT_PAYADVICE OCCURS 0 WITH HEADER LINE .
DATA : TT_ITEMS LIKE ZZLT_PAYADVICE OCCURS 0 WITH HEADER LINE .
DATA: IT_BSAK TYPE STANDARD TABLE OF BSAK,
      IT_BSAK2 TYPE STANDARD TABLE OF BSAK,
      WA_BSAK LIKE LINE OF IT_BSAK2,
      IT_BSIK TYPE STANDARD TABLE OF BSIK,
      WA_BSIK LIKE LINE OF IT_BSIK,
      IT_BSIK2 TYPE STANDARD TABLE OF BSIK,
      WA_BSIK2 LIKE LINE OF IT_BSIK2.


DATA: AMOUNT(100),
      SUM       LIKE BSEG-WRBTR,
      SPELL     LIKE SPELL,
      SGTXT     LIKE BSEG-SGTXT,
      SAKNR     LIKE SKA1-SAKNR ,
      KUNNR     LIKE KNA1-KUNNR,
      LIFNR     LIKE BSEG-LIFNR,
      FLG_START,
      CNT_LINES TYPE I.
DATA : GV_FNAME TYPE RS38L_FNAM.
DATA : IT_BKPF LIKE BKPF OCCURS 0  WITH HEADER LINE.
DATA : I_PAYR LIKE PAYR OCCURS 0 WITH HEADER LINE,
       I_REGUH LIKE REGUH OCCURS 0 WITH HEADER LINE.
DATA : V_LAUFI TYPE REGUH-LAUFI,
         V_LAUFD TYPE REGUH-LAUFD.
DATA : CONTROL TYPE SSFCTRLOP.
DATA: W_RETURN TYPE SSFCRESCL.

DATA: LV_ADRNR TYPE LFA1-ADRNR, LV_SMTP_ADDR TYPE ADR6-SMTP_ADDR.

CONSTANTS: C_KTOPL LIKE SKAT-KTOPL VALUE '1000'.

DATA: BEGIN OF wa_bseg ,
      bukrs TYPE bseg-bukrs,
      belnr TYPE bseg-belnr,
      gjahr TYPE bseg-gjahr,
      ebeln TYPE bseg-ebeln,
      augbl TYPE bseg-augbl,
      auggj type bseg-auggj,
      END OF wa_bseg ,
      it_bseg like TABLE OF wa_bseg ,
      wa_augbl LIKE wa_bseg,
      it_augbl LIKE TABLE OF wa_augbl,
      wa_ebeln LIKE wa_bseg,
      it_ebeln LIKE TABLE OF wa_ebeln .

DATA: BEGIN OF wa_EXGRP ,
      ebeln TYPE ekko-ebeln,
      ekgrp TYPE ekko-ekgrp,
      banfn TYPE ekpo-banfn,
      smtp_addr TYPE T024-smtp_addr,
      END OF wa_exgrp ,
      IT_EXGRP like TABLE OF wa_exgrp .



*&----------------------------------------------------------------&*
*   SELECTION CRITERIA                                             *
*&----------------------------------------------------------------&*

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
PARAMETERS       P_BUKRS LIKE BKPF-BUKRS OBLIGATORY.
SELECT-OPTIONS   S_BELNR FOR BKPF-BELNR OBLIGATORY.
PARAMETERS       P_GJAHR LIKE BKPF-GJAHR OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B1 .

PARAMETERS       P_MAIL AS CHECKBOX modif id m1.  " IHDK901232

" IHDK901232
AT SELECTION-SCREEN OUTPUT.
  IF sy-tcode eq 'ZFIAD'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'M1'.
        screen-invisible = 1.
        screen-active = 0.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*&----------------------------------------------------------------&*
*   START-OF-SELECTION                                             *
*&----------------------------------------------------------------&*

START-OF-SELECTION.
************************Start********************************     " added by NK on 12.10.2015
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
                      ID 'BUKRS' FIELD P_BUKRS
                      ID 'ACTVT' FIELD '03'.
  IF SY-SUBRC NE 0.
    MESSAGE 'User not authorized for Company Code' TYPE 'I' DISPLAY LIKE 'E'.     " modified by Naren Karra on 20.10.2015
    LEAVE LIST-PROCESSING.                                                        " added by Naren Karra on 20.10.2015
  ENDIF.
*************************End*********************************
  PERFORM SELECTION.
*  IF P_MAIL = 'X'.
*    PERFORM SEND_MAIL.
*  ENDIF.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  PRINT_LETTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_LETTER.
  CLEAR CNT_LINES.

ENDFORM.                               " PRINT_LETTER
*&---------------------------------------------------------------------*
*&      Form  WRITE_ADDR
*&---------------------------------------------------------------------*
FORM WRITE_ADDR.

ENDFORM.                               " WRITE_ADDR
*&---------------------------------------------------------------------*
*&      Form  WRITE_REF_NO
*&---------------------------------------------------------------------*
FORM WRITE_REF_NO.
  DATA: LEN LIKE SY-TABIX.
  DATA: LEN1 LIKE SY-TABIX.

  CLEAR: PAYR, SKAT.

  IF NOT BKPF-BKTXT IS INITIAL.
    SPLIT BKPF-BKTXT AT '-' INTO V_LAUFD V_LAUFI.


  ENDIF.

  SELECT  SINGLE * FROM  PAYR   CLIENT SPECIFIED
                         WHERE  MANDT  = SY-MANDT
                           AND  ZBUKR  = P_BUKRS
                           AND  VBLNR  = BKPF-BELNR
                           AND  GJAHR  = P_GJAHR .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING PAYR TO I_PAYR.
    APPEND I_PAYR.
    CLEAR I_PAYR.
  ELSE.
    I_PAYR-ZBUKR = BKPF-BUKRS.

    I_PAYR-VBLNR = BKPF-BELNR.
    I_PAYR-GJAHR = BKPF-GJAHR.
    I_PAYR-RWBTR = BSAK-WRBTR.
    IF I_PAYR-HBKID IS INITIAL.
      SELECT SINGLE HBKID FROM SKB1 INTO I_PAYR-HBKID WHERE SAKNR = SAKNR AND BUKRS = BKPF-BUKRS.
      IF SY-SUBRC <> 0 . CLEAR I_PAYR-HBKID. ENDIF.
    ENDIF.

    IF I_PAYR-CHECT IS INITIAL.

      SELECT SINGLE ZUONR FROM BSEG
        INTO I_PAYR-CHECT WHERE BUKRS = P_BUKRS
        AND BELNR = BKPF-BELNR
        AND GJAHR = P_GJAHR
*        AND HKONT = SAKNR
        and zuonr ne ''. " IHDK901179
      IF SY-SUBRC <> 0 . CLEAR I_PAYR-CHECT. ENDIF.

    ENDIF.
    APPEND I_PAYR.
    CLEAR I_PAYR.
  ENDIF.

  IF V_LAUFD IS INITIAL OR  V_LAUFI IS INITIAL.
    V_LAUFD = PAYR-LAUFD.
    V_LAUFI = PAYR-LAUFI.
  ENDIF.
  SELECT  SINGLE * FROM  REGUH   CLIENT SPECIFIED
                         WHERE  MANDT  = SY-MANDT
                           AND  LAUFD = V_LAUFD
                           AND  ZBUKR  = P_BUKRS
                           AND  VBLNR  = BKPF-BELNR.
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING REGUH TO I_REGUH.
    APPEND I_REGUH.
    CLEAR I_REGUH.
  ELSE.
*    IF NOT BSAK-HBKID IS INITIAL.
    REGUH-ZBUKR = BKPF-BUKRS.
    REGUH-LIFNR = BSAK-LIFNR.
    REGUH-HBKID = BSAK-HBKID.
    APPEND I_REGUH.
    CLEAR I_REGUH.


*    ENDIF.
  ENDIF.
ENDFORM.                               " WRITE_REF_NO

*
*
*I000     ......................................................................                        132
*I001     Choose Company                                                                                 14
*R        Covering Letter                                                                                15
*SC_CH            Kalmeshwar                                                                             18
*SP_BUKRS         Company code                                                                           20
*SP_GJAHR         Year                                                                                   12
*SS_BELNR         Document Number                                                                        23
*
*&---------------------------------------------------------------------*
*&      Form  ITEM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ITEM_DATA .
  DATA: X_QBSHB TYPE WT_WT1.
  SELECT * FROM  BSAK CLIENT SPECIFIED INTO TABLE IT_BSAK
             WHERE  MANDT       = SY-MANDT
             AND    BUKRS       = BKPF-BUKRS
             AND    AUGBL       = BKPF-BELNR
             AND    AUGDT       = BKPF-BUDAT.
*           AND    gjahr       = p_gjahr .
  IF SY-SUBRC = 0.
    "For cleared items select data from bsak
    LOOP AT IT_BSAK INTO BSAK.
      IF BSAK-SHKZG       = 'H'.
        IF BSAK-AUGBL NE BSAK-BELNR.
          IT_ITEMS-VBLNR = BKPF-BELNR.
          IT_ITEMS-BELNR = BSAK-BELNR.
          IT_ITEMS-XBLNR = BSAK-XBLNR.
          IT_ITEMS-SGTXT = BSAK-SGTXT.
          IT_ITEMS-WRBTR = BSAK-WRBTR.
          IT_ITEMS-BLDAT = BSAK-BLDAT.
          IT_ITEMS-BUZEI = BSAK-BUZEI.
          IT_ITEMS-BUKRS = BKPF-BUKRS.
          IT_ITEMS-GJAHR = BSAK-GJAHR.
          IT_ITEMS-BLART = BSAK-BLART.
          IT_ITEMS-BSCHL = BSAK-BSCHL.
          IT_ITEMS-HBKID = BSAK-HBKID.
          IT_ITEMS-AUGDT = BKPF-BUDAT.
          IT_ITEMS-SIGN  = BSAK-SHKZG.

          ADD BSAK-WRBTR TO SUM.
          APPEND IT_ITEMS.
        ELSE.

        ENDIF.
      ELSEIF BSAK-SHKZG       = 'S'.
        IF BSAK-AUGBL NE BSAK-BELNR.
          IT_ITEMS-VBLNR = BKPF-BELNR.
          IT_ITEMS-BELNR = BSAK-BELNR.
          IT_ITEMS-XBLNR = BSAK-XBLNR.
          IT_ITEMS-SGTXT = BSAK-SGTXT.
          IT_ITEMS-WRBTR = BSAK-WRBTR.
          IT_ITEMS-BLDAT = BSAK-BLDAT.
          IT_ITEMS-BUZEI = BSAK-BUZEI.
          IT_ITEMS-BUKRS = BKPF-BUKRS.
          IT_ITEMS-GJAHR = BSAK-GJAHR.
          IT_ITEMS-BLART = BSAK-BLART.
          IT_ITEMS-BSCHL = BSAK-BSCHL.
          IT_ITEMS-HBKID = BSAK-HBKID.
          IT_ITEMS-AUGDT = BKPF-BUDAT.
          IT_ITEMS-SIGN  = BSAK-SHKZG.

          SUBTRACT BSAK-WRBTR FROM SUM.
          APPEND IT_ITEMS.
        ELSE.
          SGTXT = BSAK-SGTXT.
        ENDIF.
      ENDIF.
      IF SGTXT IS INITIAL.
        SGTXT = BSEG-SGTXT.
      ENDIF.
    ENDLOOP.
  ELSE.
    "For Cash Advance (Open Item) select from BSIK
    SELECT * FROM  BSIK INTO TABLE IT_BSIK
             WHERE  BUKRS       = BKPF-BUKRS
             AND    BELNR       = BKPF-BELNR
             AND    BUDAT       = BKPF-BUDAT.
*           AND    gjahr       = p_gjahr .

    SELECT * FROM  BSIK APPENDING TABLE IT_BSIK2
             WHERE  BUKRS       = BKPF-BUKRS
             AND    BELNR       = BKPF-BELNR
             AND    BUDAT       = BKPF-BUDAT.



    LOOP AT IT_BSIK INTO BSIK.
      CLEAR: X_QBSHB.
      SELECT SUM( WT_QBSHB ) FROM WITH_ITEM   "Since payment made already and TDS deducted later, need to subtract TDS amount from bsik-wrbtr
        INTO X_QBSHB
        WHERE BUKRS = BKPF-BUKRS
          AND BELNR = BSIK-BELNR
          AND GJAHR = BSIK-GJAHR
          AND BUZEI = BSIK-BUZEI.

      IT_ITEMS-VBLNR = BKPF-BELNR.
      IT_ITEMS-BELNR = BSIK-BELNR.
      IT_ITEMS-XBLNR = BSIK-XBLNR.
      IT_ITEMS-SGTXT = BSIK-SGTXT.
      IT_ITEMS-WRBTR = BSIK-WRBTR - X_QBSHB.
      IT_ITEMS-BLDAT = BSIK-BLDAT.
      IT_ITEMS-BUZEI = BSIK-BUZEI.
      IT_ITEMS-BUKRS = BKPF-BUKRS.
      IT_ITEMS-GJAHR = BSIK-GJAHR.
      IT_ITEMS-BLART = BSIK-BLART.
      IT_ITEMS-BSCHL = BSIK-BSCHL.
      IT_ITEMS-HBKID = BSIK-HBKID.
      IT_ITEMS-AUGDT = BKPF-BUDAT.
      IT_ITEMS-SIGN  = BSAK-SHKZG.

      ADD BSIK-WRBTR TO SUM.
      APPEND IT_ITEMS.
      IF SGTXT IS INITIAL.
        SGTXT = BSEG-SGTXT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " ITEM_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTION .
  SELECT  * FROM  BKPF       CLIENT SPECIFIED
           INTO CORRESPONDING FIELDS OF TABLE IT_BKPF
           WHERE  MANDT       = SY-MANDT
           AND    BUKRS       = P_BUKRS
           AND    BELNR       IN S_BELNR
           AND    GJAHR       = P_GJAHR   .


  CLEAR: IT_ITEMS, LIFNR,KUNNR,SUM, AMOUNT, SAKNR, SGTXT.
  REFRESH IT_ITEMS.
*    IF NOT flg_start IS INITIAL.
*      NEW-PAGE.
*    ENDIF.

  LOOP AT IT_BKPF.
    CLEAR : BKPF.

    MOVE-CORRESPONDING IT_BKPF TO BKPF.
    SELECT SINGLE * FROM  BSEG       CLIENT SPECIFIED
           WHERE  MANDT       = SY-MANDT
           AND    BUKRS       = BKPF-BUKRS
           AND    BELNR       = BKPF-BELNR
           AND    SHKZG       = 'H'
           and    koart       = 'S' " IHDK901179
           AND    GJAHR       = P_GJAHR .
    SAKNR = BSEG-HKONT.
    CLEAR BSEG.
    SELECT SINGLE * FROM  BSEG       CLIENT SPECIFIED
           WHERE  MANDT       = SY-MANDT
           AND    BUKRS       = BKPF-BUKRS
           AND    BELNR       = BKPF-BELNR
           AND    SHKZG       = 'S'
           AND    GJAHR       = P_GJAHR .
    IF NOT BSEG-LIFNR IS INITIAL.
      LIFNR = BSEG-LIFNR.
    ELSEIF  NOT BSEG-KUNNR IS INITIAL.
      KUNNR = BSEG-KUNNR.
    ENDIF.

    PERFORM ITEM_DATA.


    IF NOT BSEG-LIFNR IS INITIAL.
      LIFNR = BSEG-LIFNR.
    ELSEIF  NOT BSEG-KUNNR IS INITIAL.
      KUNNR = BSEG-KUNNR.
    ENDIF.


*   PERFORM write_address.
    PERFORM PRINT_LETTER.
    PERFORM WRITE_REF_NO.
*    IF cnt_lines LT 3.
*      PERFORM write_v_lines USING 3.
*    ENDIF.
*    PERFORM write_footer.


  ENDLOOP.
  IF IT_BKPF[] IS NOT INITIAL.                                     " added by Naren Karra on 20.10.2015
    IF P_MAIL <> 'X'.
      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          FORMNAME                 = 'Z6FI004S_PAY_ADVICE'
*     VARIANT                  = ' '
*     DIRECT_CALL              = ' '
        IMPORTING
          FM_NAME                  = GV_FNAME
       EXCEPTIONS
         NO_FORM                  = 1
         NO_FUNCTION_MODULE       = 2
         OTHERS                   = 3
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

      ELSE.

        CONTROL-NO_DIALOG = 'X'.
        CONTROL-PREVIEW = 'X'.
        CONTROL-NO_OPEN = 'X'.
        CONTROL-NO_CLOSE = 'X'.

        CALL FUNCTION 'SSF_OPEN'
          EXPORTING
*       ARCHIVE_PARAMETERS       =
*       USER_SETTINGS            = 'X'
*       MAIL_SENDER              =
*       MAIL_RECIPIENT           =
*       MAIL_APPL_OBJ            =
*       OUTPUT_OPTIONS           =
            CONTROL_PARAMETERS       = CONTROL
*     IMPORTING
*       JOB_OUTPUT_OPTIONS       =
*     EXCEPTIONS
*       FORMATTING_ERROR         = 1
*       INTERNAL_ERROR           = 2
*       SEND_ERROR               = 3
*       USER_CANCELED            = 4
*       OTHERS                   = 5
                  .
        IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

      ENDIF.

    ENDIF.


*    if P_MAIL = 'X'.
*
**** send mail based on PO - Purchasing group-  user combination and PR creator .
*    PERFORM get_receipents.
*    endif.

    LOOP AT IT_BKPF.
      CLEAR : REGUH,PAYR.
      REFRESH : TT_ITEMS.
      CLEAR   : TT_ITEMS.
      READ TABLE I_PAYR INTO PAYR WITH KEY ZBUKR = IT_BKPF-BUKRS
                                            VBLNR = IT_BKPF-BELNR
                                            GJAHR = IT_BKPF-GJAHR.
      IF NOT IT_BKPF-BKTXT IS INITIAL.
        SPLIT IT_BKPF-BKTXT AT '-' INTO V_LAUFD V_LAUFI.
      ELSE.
        MOVE : PAYR-LAUFD TO V_LAUFD,
               PAYR-LAUFI TO V_LAUFI.
      ENDIF.

      READ TABLE I_REGUH INTO REGUH WITH KEY  LAUFD = V_LAUFD

                                                 ZBUKR  = P_BUKRS
                                                    VBLNR  = IT_BKPF-BELNR.

      LOOP AT IT_ITEMS WHERE BUKRS EQ IT_BKPF-BUKRS
                         AND VBLNR EQ IT_BKPF-BELNR
                         AND AUGDT EQ IT_BKPF-BUDAT.
*                         AND GJAHR EQ IT_BKPF-GJAHR.
        MOVE-CORRESPONDING IT_ITEMS TO TT_ITEMS.
        APPEND TT_ITEMS.
        CLEAR TT_ITEMS.
      ENDLOOP.
      IF P_MAIL <> 'X'.
        CALL FUNCTION GV_FNAME
    EXPORTING
*   ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
      CONTROL_PARAMETERS         = CONTROL
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*     bkpftab                   = wa_bkpftab
*OUTPUT_OPTIONS             = h_output_options
*   USER_SETTINGS              = 'X'
      PAYR                  = PAYR
      REGUH                 = REGUH
      SUM                   = SUM
*IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
*    job_output_info            =  W_RETURN"h_output_info
*   JOB_OUTPUT_OPTIONS         =
    TABLES
      IT_ITEMS                = TT_ITEMS
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .

      ELSE.
        PERFORM SEND_MAIL.
      ENDIF.

    ENDLOOP.

    IF P_MAIL <> 'X'.
      CALL FUNCTION 'SSF_CLOSE'
*     IMPORTING
*       JOB_OUTPUT_INFO        =
*     EXCEPTIONS
*       FORMATTING_ERROR       = 1
*       INTERNAL_ERROR         = 2
*       SEND_ERROR             = 3
*       OTHERS                 = 4
                .
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDIF.
  ELSE.
    MESSAGE 'No records found / Missing Authorization' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.                    " SELECTION
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_MAIL .
  DATA:P_LF_FORMNAME1 TYPE  TDSFNAME , LF_FM_NAME   TYPE RS38L_FNAM. .

* Internal Table declarations
  DATA: I_OTF TYPE ITCOO OCCURS 0 WITH HEADER LINE,
        I_TLINE TYPE TABLE OF TLINE WITH HEADER LINE,
        I_RECEIVERS TYPE TABLE OF SOMLRECI1 WITH HEADER LINE,
        I_RECORD LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        I_OBJPACK LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
        I_OBJTXT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_OBJBIN LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        I_RECLIST LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        WA_OBJHEAD TYPE SOLI_TAB,
        W_CTRLOP TYPE SSFCTRLOP,
        W_COMPOP TYPE SSFCOMPOP,

        WA_DOC_CHNG TYPE SODOCCHGI1,
*        W_DATA TYPE SODOCCHGI1,
        WA_BUFFER TYPE STRING,
* ” to convert from 132 to 255
* Variables declarations
*        V_FORM_NAME TYPE RS38L_FNAM,
        V_LEN_IN LIKE SOOD-OBJLEN,
*        V_LEN_OUT LIKE SOOD-OBJLEN,
*        V_LEN_OUTN TYPE I,
        V_LINES_TXT TYPE I,
        V_LINES_BIN TYPE I.

  DATA: IT_WYT3 TYPE TABLE OF WYT3 ,
        WA_WYT3 LIKE LINE OF IT_WYT3.

  DATA: ZKTOKK TYPE LFA1-KTOKK.

DATA: ld_sender_address LIKE  soextreci1-receiver,
      ld_sender_address_type LIKE  soextreci1-adr_typ.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING  FORMNAME           = 'Z6FI004S_PAY_ADVICE' "P_LF_FORMNAME1
*                 variant            = ' '
*                 direct_call        = ' '
       IMPORTING  FM_NAME            = LF_FM_NAME
       EXCEPTIONS NO_FORM            = 1
                  NO_FUNCTION_MODULE = 2
                  OTHERS             = 3.
  IF SY-SUBRC <> 0.
  ENDIF.
  IF P_MAIL = 'X'.
    W_CTRLOP-GETOTF    = 'X'.
    W_CTRLOP-NO_DIALOG = 'X'.
    W_COMPOP-TDNOPREV  = 'X'.
  ENDIF.

**  CALL FUNCTION    LF_FM_NAME               "'/1BCDWB/SF00000015'
**    EXPORTING
**      CONTROL_PARAMETERS         = W_CTRLOP
**      OUTPUT_OPTIONS             = W_COMPOP
**      USER_SETTINGS              = 'X'
***      GT_PB0001                  = GT_PB0001
**    IMPORTING
**      JOB_OUTPUT_INFO            = W_RETURN
**  EXCEPTIONS
**      FORMATTING_ERROR           = 1
**      INTERNAL_ERROR             = 2
**      SEND_ERROR                 = 3
**      USER_CANCELED              = 4
**      OTHERS                     = 5
*                  .


*CALL FUNCTION '/1BCDWB/SF00000183'
  CALL FUNCTION    LF_FM_NAME
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

   CONTROL_PARAMETERS = W_CTRLOP
   OUTPUT_OPTIONS     = W_COMPOP
   USER_SETTINGS      = 'X'
     PAYR                       = PAYR
     REGUH                      = REGUH
      SUM                        = SUM
   IMPORTING
*   DOCUMENT_OUTPUT_INFO       =
     JOB_OUTPUT_INFO            = W_RETURN
*   JOB_OUTPUT_OPTIONS         =
*   COUNTER                    =
    TABLES
      IT_ITEMS                   = TT_ITEMS
* EXCEPTIONS
*   FORMATTING_ERROR           = 1
*   INTERNAL_ERROR             = 2
*   SEND_ERROR                 = 3
*   USER_CANCELED              = 4
*   OTHERS                     = 5
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*
*  IF SY-SUBRC <> 0.
*  ENDIF.
  IF P_MAIL = 'X'.
    I_OTF[] = W_RETURN-OTFDATA[].

    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        FORMAT                = 'PDF'
        MAX_LINEWIDTH         = 132
      IMPORTING
        BIN_FILESIZE          = V_LEN_IN
      TABLES
        OTF                   = I_OTF
        LINES                 = I_TLINE
      EXCEPTIONS
        ERR_MAX_LINEWIDTH     = 1
        ERR_FORMAT            = 2
        ERR_CONV_NOT_POSSIBLE = 3
        OTHERS                = 4.

    IF SY-SUBRC <> 0.
    ENDIF.

    LOOP AT I_TLINE.
      TRANSLATE I_TLINE USING '~'.
      CONCATENATE WA_BUFFER I_TLINE INTO WA_BUFFER.
    ENDLOOP.

    TRANSLATE WA_BUFFER USING '~'.
    DO.
      I_RECORD = WA_BUFFER.
      APPEND I_RECORD.
      SHIFT WA_BUFFER LEFT BY 255 PLACES.
      IF WA_BUFFER IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
* Attachment
    REFRESH: I_RECLIST,
             I_OBJTXT,
             I_OBJBIN,
             I_OBJPACK.
    CLEAR :  WA_OBJHEAD.
    I_OBJBIN[] = I_RECORD[].
* create message body  title and description

    I_OBJTXT = 'Sir,'.
    APPEND I_OBJTXT.
    I_OBJTXT = ' '.
    APPEND I_OBJTXT.
    I_OBJTXT = 'Your payment has been processed at our end as attached.'. "'Please Find here with Attached copy of Remittance Letter.'.
    APPEND I_OBJTXT.
    I_OBJTXT = 'With Regards,'.
    APPEND I_OBJTXT.
    I_OBJTXT = ' '.
    APPEND I_OBJTXT.
    I_OBJTXT = 'INDOFIL INDUSTRIES LIMITED'.
    APPEND I_OBJTXT.
*    I_OBJTXT = '(Formerly known as "Indofil Organic Industries Ltd.")'.
*    APPEND I_OBJTXT.
*    I_OBJTXT = ' '.
*    APPEND I_OBJTXT.
*
*    I_OBJTXT = 'AZAD NAGAR, SANDOZ BAUG P.O.,'.
*    APPEND I_OBJTXT.
*
*    I_OBJTXT = 'OFF SWAMI VIVEKANANDA ROAD,'.
*    APPEND I_OBJTXT.
*
*    I_OBJTXT = 'THANE(W), 400607. INDIA'.
*    APPEND I_OBJTXT.


    DESCRIBE TABLE I_OBJTXT LINES V_LINES_TXT.
    READ TABLE I_OBJTXT INDEX V_LINES_TXT.

    WA_DOC_CHNG-OBJ_NAME   = 'INDOFIL'.
    WA_DOC_CHNG-EXPIRY_DAT = SY-DATUM + 10.
    WA_DOC_CHNG-OBJ_DESCR  = 'Payment Advice'.
    WA_DOC_CHNG-SENSITIVTY = 'F'.
    WA_DOC_CHNG-DOC_SIZE   = V_LINES_TXT * 255.
*# main text
* wa_doc_chng-doc_size = ( v_lines_txt - 1 ) * 255 + strlen( i_objtxt )
*.
    CLEAR I_OBJPACK-TRANSF_BIN.
    I_OBJPACK-HEAD_START = 1.
    I_OBJPACK-HEAD_NUM   = 0.
    I_OBJPACK-BODY_START = 1.
    I_OBJPACK-BODY_NUM   = V_LINES_TXT.
    I_OBJPACK-DOC_TYPE   = 'RAW'.
    APPEND I_OBJPACK.

*# attachment
* (pdf-Attachment)
    I_OBJPACK-TRANSF_BIN = 'X'.
    I_OBJPACK-HEAD_START = 1.
    I_OBJPACK-HEAD_NUM   = 0.
    I_OBJPACK-BODY_START = 1.
* Länge des Attachment ermitteln
    DESCRIBE TABLE I_OBJBIN LINES V_LINES_BIN.
    READ TABLE I_OBJBIN INDEX V_LINES_BIN.
    I_OBJPACK-DOC_SIZE  = V_LINES_BIN * 255 .
    I_OBJPACK-BODY_NUM  = V_LINES_BIN.
    I_OBJPACK-DOC_TYPE  = 'PDF'.
    I_OBJPACK-OBJ_NAME  = 'Smart'.
    I_OBJPACK-OBJ_DESCR = 'Payment_Advice'.
    APPEND I_OBJPACK.
    CLEAR I_RECLIST.

    DATA : L_USERID TYPE P0105-USRID_LONG.

    if sy-tcode eq 'ZFIAD_MAIL'.  " IHDK901437
      SELECT SINGLE USRID_LONG INTO L_USERID
                               FROM PA0105
                               WHERE PERNR = SY-UNAME
                               AND ENDDA = '99991231'
                               AND SUBTY = '0010'.

      IF  L_USERID IS NOT INITIAL.
        I_RECLIST-RECEIVER = L_USERID.
        I_RECLIST-REC_TYPE = 'U'.
        I_RECLIST-COM_TYPE = 'INT'.
        APPEND  I_RECLIST.
        CLEAR:  L_USERID.
       ENDIF.
     endif.

    CLEAR: IT_BSAK2 , LV_SMTP_ADDR.
    IF REGUH-LIFNR IS NOT INITIAL.
      SELECT SINGLE ADRNR FROM LFA1
      INTO LV_ADRNR
      WHERE LIFNR =  REGUH-LIFNR .
    ELSE.
      CLEAR: IT_BSAK2.
      SELECT * FROM  BSAK CLIENT SPECIFIED INTO TABLE IT_BSAK2
         WHERE  MANDT       = SY-MANDT
         AND    BUKRS       = IT_BKPF-BUKRS
         AND    BELNR       = IT_BKPF-BELNR
         AND    GJAHR       = IT_BKPF-GJAHR.

      IF SY-SUBRC = 0.
        CLEAR: WA_BSAK.
        READ TABLE IT_BSAK2 INTO WA_BSAK WITH KEY BUKRS = IT_BKPF-BUKRS BELNR = IT_BKPF-BELNR GJAHR = IT_BKPF-GJAHR  .
        IF SY-SUBRC = 0.
          SELECT SINGLE KTOKK FROM LFA1 INTO ZKTOKK WHERE LIFNR = WA_BSAK-LIFNR AND KTOKK <> 'EMPL'.

          IF SY-SUBRC = 0.
            CLEAR: LV_ADRNR , LV_SMTP_ADDR.
            SELECT SINGLE ADRNR FROM LFA1
            INTO LV_ADRNR
            WHERE LIFNR =  WA_BSAK-LIFNR .
            IF SY-SUBRC = 0 .
              SELECT SINGLE SMTP_ADDR
                FROM ADR6
                INTO LV_SMTP_ADDR
                WHERE ADDRNUMBER = LV_ADRNR.

              IF SY-SUBRC <> 0 . CLEAR: LV_SMTP_ADDR. ENDIF.

              CLEAR: IT_WYT3, WA_WYT3.

              SELECT * FROM WYT3 INTO TABLE IT_WYT3
                WHERE LIFNR = WA_BSAK-LIFNR
                AND PARVW = 'ZM'.

              LOOP AT IT_WYT3 INTO WA_WYT3.
                CLEAR: L_USERID.

                SELECT SINGLE USRID_LONG INTO L_USERID
                    FROM PA0105
                    WHERE PERNR = WA_WYT3-PERNR
                    AND ENDDA = '99991231'
                    AND SUBTY = '0010'.

                I_RECLIST-RECEIVER = L_USERID.
                I_RECLIST-REC_TYPE = 'U'.
                I_RECLIST-COM_TYPE = 'INT'.
                APPEND  I_RECLIST.
                CLEAR:  L_USERID.
              ENDLOOP.
            ELSE.
              CLEAR: LV_ADRNR.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR: WA_BSIK2.
        READ TABLE IT_BSIK2 INTO WA_BSIK2 WITH KEY BUKRS = IT_BKPF-BUKRS BELNR = IT_BKPF-BELNR GJAHR = IT_BKPF-GJAHR  .
        IF SY-SUBRC = 0.
          SELECT SINGLE KTOKK FROM LFA1 INTO ZKTOKK WHERE LIFNR = WA_BSIK2-LIFNR AND KTOKK <> 'EMPL'.

          IF SY-SUBRC = 0.
            SELECT SINGLE ADRNR FROM LFA1
               INTO LV_ADRNR
               WHERE LIFNR =  WA_BSIK2-LIFNR .
            IF SY-SUBRC = 0 .

              SELECT SINGLE SMTP_ADDR
                FROM ADR6
                INTO LV_SMTP_ADDR
                WHERE ADDRNUMBER = LV_ADRNR.

              IF SY-SUBRC <> 0 . CLEAR: LV_SMTP_ADDR. ENDIF.

              CLEAR: IT_WYT3, WA_WYT3.

              SELECT * FROM WYT3 INTO TABLE IT_WYT3
                WHERE LIFNR = WA_BSIK2-LIFNR
                AND PARVW = 'ZM'.

              LOOP AT IT_WYT3 INTO WA_WYT3.
                CLEAR: L_USERID.

                SELECT SINGLE USRID_LONG INTO L_USERID
                    FROM PA0105
                    WHERE PERNR = WA_WYT3-PERNR
                    AND ENDDA = '99991231'
                    AND SUBTY = '0010'.

                I_RECLIST-RECEIVER = L_USERID.
                I_RECLIST-REC_TYPE = 'U'.
                I_RECLIST-COM_TYPE = 'INT'.
                APPEND  I_RECLIST.
                CLEAR:  L_USERID.

              ENDLOOP.
            ELSE.
              CLEAR: LV_ADRNR.
            ENDIF.
*            IF SY-SUBRC <> 0.
*              CLEAR: LV_ADRNR.
*            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF .

*      IF SY-SUBRC = 0 .
*        SELECT SINGLE SMTP_ADDR
*          FROM ADR6
*          INTO LV_SMTP_ADDR
*          WHERE ADDRNUMBER = LV_ADRNR.
*
*        IF SY-SUBRC <> 0 . CLEAR: LV_SMTP_ADDR. ENDIF.
*
*      ELSE.
*
*        CLEAR: LV_ADRNR.
*      ENDIF.

    IF LV_SMTP_ADDR IS NOT INITIAL.
      L_USERID = LV_SMTP_ADDR.
      I_RECLIST-RECEIVER = L_USERID.
      I_RECLIST-REC_TYPE = 'U'.
      I_RECLIST-COM_TYPE = 'INT'.
      APPEND  I_RECLIST.
      CLEAR:  L_USERID.
    ENDIF.

    if sy-tcode ne 'ZFIAD_MAIL'.  " IHDK901437
      sy-uname = 'IRPBTC300'. " automail
    endif.

**** send mail based on PO - Purchasing group-  user combination and PR creator .
   PERFORM get_receipents.

    LOOP AT IT_EXGRP INTO wa_exgrp.
        IF wa_exgrp-smtp_addr is NOT INITIAL.

            L_USERID = wa_exgrp-smtp_addr.
            I_RECLIST-RECEIVER = L_USERID.
            I_RECLIST-REC_TYPE = 'U'.
            I_RECLIST-COM_TYPE = 'INT'.

            APPEND  I_RECLIST.
           CLEAR:  L_USERID.

        ENDIF.
    IF wa_exgrp-banfn IS NOT INITIAL .
      SELECT SINGLE ernam
        FROM EBAN INTO @data(zernam) " PR creator
        WHERE banfn = @wa_exgrp-banfn.
        IF sy-subrc = 0.
          CLEAR: L_USERID.
                SELECT SINGLE USRID_LONG INTO L_USERID
                    FROM PA0105
                    WHERE PERNR = zernam
                    AND ENDDA >= sy-datum
                    AND SUBTY = '0010'.
                IF L_USERID <> ''.
                I_RECLIST-RECEIVER = L_USERID.
                I_RECLIST-REC_TYPE = 'U'.
                I_RECLIST-COM_TYPE = 'INT'.
                APPEND  I_RECLIST.
                CLEAR:  L_USERID.
                ELSE.
***** if no user found in HR master then search for Su01 user profile

                 SELECT SINGLE a~smtp_addr
                   FROM adr6 as a
                   JOIN usr21 as b
                   on a~ADDRNUMBER = b~ADDRNUMBER
                   AND b~PERSNUMBER = b~PERSNUMBER
                   INTO L_USERID
                   WHERE b~bname = zernam.


                ENDIF.
        ENDIF.

    ENDIF.

    ENDLOOP.
*
    sort I_RECLIST by RECEIVER.
    delete ADJACENT DUPLICATES FROM I_RECLIST.

    IF I_RECLIST IS NOT INITIAL.

*    ld_sender_address      = 'sapautomail-icc@modi.com'.
*    ld_sender_address_type = 'SMTP'.


      CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
        EXPORTING
          DOCUMENT_DATA              = WA_DOC_CHNG
          PUT_IN_OUTBOX              = 'X'
*          sender_address             = ld_sender_address         " sender's address
*          sender_address_type        = ld_sender_address_type
          COMMIT_WORK                = 'X'
        TABLES
          PACKING_LIST               = I_OBJPACK
          OBJECT_HEADER              = WA_OBJHEAD
          CONTENTS_BIN               = I_OBJBIN
          CONTENTS_TXT               = I_OBJTXT
          RECEIVERS                  = I_RECLIST
        EXCEPTIONS
          TOO_MANY_RECEIVERS         = 1
          DOCUMENT_NOT_SENT          = 2
          DOCUMENT_TYPE_NOT_EXIST    = 3
          OPERATION_NO_AUTHORIZATION = 4
          PARAMETER_ERROR            = 5
          X_ERROR                    = 6
          ENQUEUE_ERROR              = 7
          OTHERS                     = 8.

      IF SY-SUBRC EQ 0.
        MESSAGE 'Mail sent sent Sucessfully' TYPE 'S'.
        IF WA_BSAK-LIFNR IS NOT INITIAL .
          WRITE: / 'Mail send Sucessfully to vendor: '  , WA_BSAK-LIFNR.
        ELSEIF WA_BSIK2-LIFNR IS NOT INITIAL ..
          WRITE: / 'Mail send Sucessfully to vendor: '  , WA_BSIK2-LIFNR.
        ENDIF.
      WRITE: / 'Mail id details for referance :'.
      LOOP AT I_RECLIST.
      WRITE: /  I_RECLIST-receiver.
      ENDLOOP.
      WRITE: / '================================================================================'.
      ELSE.
        MESSAGE 'Error occured in Sending the Mail' TYPE 'E'.
      ENDIF.
*    ELSE.
*      MESSAGE 'Email ID for user not maintained' TYPE 'E'.
*    ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*& Form GET_RECEIPENTS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form get_receipents .
* DATA : L_USERID TYPE P0105-USRID_LONG.
BREAK 10106.
CLEAR: it_bseg , it_augbl , it_ebeln , IT_EXGRP.

** select clearing document of payment voucher from bseg .
SELECT bukrs belnr  gjahr  augbl  auggj
  FROM bseg INTO CORRESPONDING FIELDS OF TABLE it_bseg
*  FOR ALL ENTRIES IN it_bkpf
  WHERE bukrs EQ it_bkpf-BUKRS
  AND belnr EQ it_bkpf-belnr
  AND gjahr EQ it_bkpf-GJAHR
  AND koart EQ 'K'
  AND augbl <> ''.

** select all documents clear in above clearing to fetch PO number
IF sy-subrc = 0.
  SELECT bukrs belnr gjahr augbl auggj
    FROM bseg INTO CORRESPONDING FIELDS OF TABLE it_augbl
    FOR ALL ENTRIES IN it_bseg
    WHERE bukrs EQ it_bseg-bukrs
    AND augbl EQ it_bseg-augbl
    AND auggj EQ it_bseg-auggj.
    IF sy-subrc = 0.

       SELECT bukrs belnr gjahr ebeln  augbl auggj
        FROM bseg INTO CORRESPONDING FIELDS OF TABLE it_ebeln
        FOR ALL ENTRIES IN it_augbl
        WHERE bukrs EQ it_augbl-bukrs
        And   belnr EQ it_augbl-belnr
        AND   gjahr EQ it_augbl-gjahr
        AND   ebeln <> ''.

        IF sy-subrc = 0.
          SELECT a~EBELN a~EKGRP b~banfn c~SMTP_ADDR
            FROM EKKO as a
            JOIN EKPO as b
            on a~ebeln = b~ebeln
            JOIN T024 as c
            on a~ekgrp = c~EKGRP
            INTO TABLE IT_EXGRP
            FOR ALL ENTRIES IN it_ebeln
            WHERE a~EBELN EQ it_ebeln-ebeln.
*            AND c~SMTP_ADDR <> ''.

        ENDIF.

    ENDIF.


ENDIF.


endform.
