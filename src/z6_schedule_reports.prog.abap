*&---------------------------------------------------------------------*
*& Report  Z6_SCHEDULE_REPORTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z6_SCHEDULE_REPORTS.
TABLES: MSEG , MARA , SOMLRECI1.

DATA: START_DT(10) , FYEAR(04).
DATA: P TYPE I.
DATA: P1(02).
DATA: LIST_TAB LIKE STANDARD TABLE OF ABAPLIST,
      WA_LIST_TAB LIKE LINE OF LIST_TAB.

DATA: T_HTML TYPE STANDARD TABLE OF W3HTML,
  T_FIELDS TYPE STANDARD TABLE OF W3FIELDS WITH HEADER LINE.

DATA: BEGIN OF VLIST OCCURS 0,
             LINE(1024) TYPE C,
         END OF VLIST.

DATA: zegroup TYPE ZFI044_EMAILID-EGROUP.

DATA: SEND_REQUEST TYPE REF TO CL_BCS.
DATA: TEXT TYPE BCSY_TEXT.
DATA: DOCUMENT TYPE REF TO CL_DOCUMENT_BCS.
DATA: SENDER_ID TYPE REF TO IF_SENDER_BCS.
DATA: RECIPIENT TYPE REF TO IF_RECIPIENT_BCS.
DATA: BCS_EXCEPTION TYPE REF TO CX_BCS.
DATA: SENT_TO_ALL TYPE OS_BOOLEAN.
DATA: CONLENGTH TYPE I ,
CONLENGTHS TYPE SO_OBJ_LEN ,
RESULT_CONTENT TYPE STRING .
DATA: CONTENT_LENGTH TYPE W3PARAM-CONT_LEN ,
CONTENT_TYPE TYPE W3PARAM-CONT_TYPE,
RETURN_CODE TYPE W3PARAM-RET_CODE .
DATA: LISTOBJECT TYPE TABLE OF ABAPLIST.
DATA: HTML_WA TYPE W3HTML.
DATA: HTML TYPE STANDARD TABLE OF W3HTML .
DATA: WA_REC TYPE AD_SMTPADR .
DATA: BCS_MESSAGE TYPE STRING .
DATA: TMP_STR TYPE STRING .
DATA: PROG TYPE PROGRAMM ,
VAR TYPE RALDB_VARI ,
VARIANT TYPE RALDB_VARI .
DATA: SUBJECT TYPE  SO_OBJ_DES,
      SENDER TYPE  AD_SMTPADR,
      RECEPIENTS TYPE  BCSY_SMTPA,
      RETURN TYPE  TABLE_OF_STRINGS.

DATA: BLANK TYPE CHAR8,
         LINES TYPE I.

DATA: BEGIN OF WA_OUT,
        MBLNR TYPE CHAR30,"MSEG-MBLNR,
        MJAHR TYPE MSEG-MJAHR,
        ZEILE TYPE MSEG-ZEILE,
        WERKS TYPE MSEG-WERKS,
        MTART TYPE MARA-MTART,
        MATNR TYPE MARA-MATNR,
        MAKTX TYPE MAKT-MAKTX,
*  BUDAT TYPE MKPF-BUDAT,
        MENGE_D TYPE CHAR30,"MSEG-MENGE,
*        MEINS_D TYPE MSEG-MEINS,

        DMBTR_D TYPE CHAR30,"MSEG-DMBTR,
        WAERS_D TYPE MSEG-WAERS,
*      Landed Rate for current date
        LR_D TYPE CHAR30,"MSEG-DMBTR,

        MENGE_M TYPE CHAR30,"MSEG-MENGE,
*        MEINS_M TYPE MSEG-MEINS,

        DMBTR_M TYPE CHAR30,"MSEG-DMBTR,
        WAERS_M TYPE MSEG-WAERS,
*      Landed Rate for current month
        LR_M TYPE CHAR30,"MSEG-DMBTR,

        MENGE_P TYPE CHAR30,"MSEG-MENGE,
        MEINS_P TYPE MSEG-MEINS,

        DMBTR_P TYPE CHAR30,"MSEG-DMBTR,
        WAERS_P TYPE MSEG-WAERS,
*      Landed Rate for Current Period
        LR_P TYPE CHAR30,"MSEG-DMBTR,

*      BP Rate
      END OF WA_OUT,
      IT_OUT LIKE TABLE OF WA_OUT.

DATA: BEGIN OF WA_XLS,
        MTART(100)," TYPE MARA-MTART, "Material Type
        WERKS(06)," TYPE MSEG-WERKS,
        plant(30), "plant name
        MATNR TYPE MARA-MATNR,
        MAKTX(40)," TYPE MAKT-MAKTX,

        MENGE_D(20)," TYPE CHAR30,"MSEG-MENGE,
*        MEINS_D(04)," TYPE MSEG-MEINS,
        RATE_D(18),

        MENGE_M(20)," TYPE CHAR30,"MSEG-MENGE,
*        MEINS_M(04)," TYPE MSEG-MEINS,
        RATE_M(18),

        MENGE_Y(20)," TYPE CHAR30,"MSEG-MENGE,
        MEINS_Y(04)," TYPE MSEG-MEINS,
        RATE_Y(18),

        EXTRA_CHAR(2),
      END OF WA_XLS,
      IT_XLS LIKE TABLE OF WA_XLS.

DATA:   IT_ATTACH TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                WITH HEADER LINE.

DATA:   IT_MESSAGE TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                WITH HEADER LINE.


DATA:   T_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
        T_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        T_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
        T_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        T_OBJECT_HEADER LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
        W_CNT TYPE I,
        W_SENT_ALL(1) TYPE C,
        W_DOC_DATA LIKE SODOCCHGI1,
        GD_ERROR    TYPE SY-SUBRC,
        GD_RECIEVER TYPE SY-SUBRC.

DATA: SUCCESS TYPE SY-SUBRC.

DATA: BEGIN OF WA_RECpt ,
        EMAIL TYPE ZFI044_EMAILID-EMAIL,
      END OF WA_RECpt,
      IT_RECpt LIKE TABLE OF WA_RECpt.

SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME.

*PARAMETERS: P_PGM TYPE TRDIR-NAME OBLIGATORY.
SELECT-OPTIONS:  S_WERKS FOR MSEG-WERKS,
                 S_MTART FOR MARA-MTART OBLIGATORY,
                 S_MATNR FOR MARA-MATNR.

SELECT-OPTIONS: S_EMAIL FOR SOMLRECI1-RECEIVER NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK BLK.


START-OF-SELECTION.

  PERFORM CALL_REPORT.
*&---------------------------------------------------------------------*
*&      Form  CALL_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_REPORT .

*  SELECT EMAIL FROM ZFI044_EMAILID INTO TABLE RECEPIENTS WHERE EGROUP = zegroup."'ZPP015'.

  CLEAR: P, P1.
  P = SY-DATUM+04(02).

  IF P >= 4 AND P <= 12.
    CONCATENATE SY-DATUM(04) '0401' INTO START_DT.
  ELSEIF P <= 3 AND P >= 1 .
    FYEAR = SY-DATUM(04) - 1.
    CONCATENATE  FYEAR '0401' INTO START_DT.
  ENDIF.

  SUBMIT Z6MM031R_ALL_RECEIPTS
*  WITH P_VARI = '/ZPLANTWISE'
  WITH S_BUDAT-LOW = START_DT
  WITH S_BUDAT-HIGH = SY-DATUM
  WITH S_WERKS IN S_WERKS
  WITH S_MTART IN S_MTART
  WITH S_MATNR IN S_MATNR
  EXPORTING LIST TO MEMORY
  AND RETURN.

*  * From memory transfer the program output into internal table through below FM :
  IF SY-SUBRC = 0.
    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        LISTOBJECT = LIST_TAB.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    PERFORM CONVERT_XLS.
    perform send_mail.
*    PERFORM CONVERT_HTML.

  ENDIF.
** Convert a (Saved) List Object to HTML by using below FM.
*    IF LIST_TAB[] IS NOT INITIAL.
*
*      WA_LIST_TAB-RFCSIZE = 10.
*      WA_LIST_TAB-RFCRECORD = '||'.
*      APPEND WA_LIST_TAB TO LIST_TAB.
*
*      CALL FUNCTION 'WWW_HTML_FROM_LISTOBJECT'
*        EXPORTING
*          REPORT_NAME = 'Z6MM031R_ALL_RECEIPTS'
*        TABLES
*          HTML        = HTML
*          LISTOBJECT  = LIST_TAB.
*
*      CLEAR TMP_STR .
*      CLEAR : HTML_WA .
*      LOOP AT HTML INTO HTML_WA .
*        CONCATENATE TMP_STR HTML_WA INTO TMP_STR.
*      ENDLOOP .
*      CLEAR: CONLENGTH,CONLENGTHS .
*      CONLENGTH = STRLEN( TMP_STR ) .
*      CONLENGTHS = CONLENGTH .
*      TRY.
*          SUBJECT = 'Daily receipts'.
*          CONCATENATE  S_MTART-LOW ':' SUBJECT INTO SUBJECT.
*
*          SENDER = 'WF-BATCH@indofil.com'.
*          CLEAR SEND_REQUEST .
*          SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).
*          CLEAR DOCUMENT .
*          DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
*          I_TYPE = 'HTM'
*          I_TEXT = HTML
*          I_LENGTH = CONLENGTHS
*          I_SUBJECT = SUBJECT ).
**add document to send request
*          CALL METHOD SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).
*          CLEAR SENDER_ID .
*          SENDER_ID = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( SENDER ).
*          CALL METHOD SEND_REQUEST->SET_SENDER
*            EXPORTING
*              I_SENDER = SENDER_ID.
*          CLEAR WA_REC .
*          LOOP AT RECEPIENTS INTO WA_REC .
*            CLEAR RECIPIENT .
*            RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*            WA_REC ).
** add recipient with its respective attributes to send request
*            CALL METHOD SEND_REQUEST->ADD_RECIPIENT
*              EXPORTING
*                I_RECIPIENT = RECIPIENT
*                I_EXPRESS   = 'X'.
*          ENDLOOP .
*          CALL METHOD SEND_REQUEST->SET_STATUS_ATTRIBUTES
*            EXPORTING
*              I_REQUESTED_STATUS = 'E'
*              I_STATUS_MAIL      = 'E'.
*          CALL METHOD SEND_REQUEST->SET_SEND_IMMEDIATELY( 'X' ).
** --------- send document ---------------------------------------
*          CALL METHOD SEND_REQUEST->SEND(
*          EXPORTING
*          I_WITH_ERROR_SCREEN = 'X'
*          RECEIVING
*          RESULT = SENT_TO_ALL ).
*          IF SENT_TO_ALL = 'X'.
*            APPEND 'Mail sent successfully ' TO RETURN .
*          ENDIF.
*          COMMIT WORK.
*        CATCH CX_BCS INTO BCS_EXCEPTION.
*          BCS_MESSAGE = BCS_EXCEPTION->GET_TEXT( ).
*          APPEND BCS_MESSAGE TO RETURN .
*          EXIT.
*      ENDTRY.
*    ELSE .
*      APPEND 'Specify email address for sending' TO RETURN .
*    ENDIF .



*-----------------------------------------------------------------------------------


*      CALL FUNCTION 'LIST_TO_ASCI'
*        EXPORTING
*          LIST_INDEX = -1
*        TABLES
*          LISTASCI   = VLIST
*          LISTOBJECT = LIST_TAB.
*
*      IF SY-SUBRC <> '0'.
*        WRITE:/ 'LIST_TO_ASCI error !! ', SY-SUBRC.
*      ELSE.
*
*      ENDIF.


*    LOOP AT VLIST WHERE NOT LINE CS '-----' .
*      SPLIT VLIST-LINE AT '|' INTO  BLANK
*                                    WA_OUT-MBLNR
*                                    WA_OUT-MJAHR
*                                    WA_OUT-ZEILE
*                                    WA_OUT-WERKS
*                                    WA_OUT-MTART
*                                    WA_OUT-MATNR
*                                    WA_OUT-MAKTX
*                                    WA_OUT-MENGE_D
*                                    WA_OUT-MEINS_D
*                                    WA_OUT-DMBTR_D
*                                    WA_OUT-WAERS_D
*                                    WA_OUT-LR_D
*                                    WA_OUT-MENGE_M
*                                    WA_OUT-MEINS_M
*                                    WA_OUT-DMBTR_M
*                                    WA_OUT-WAERS_M
*                                    WA_OUT-LR_M
*                                    WA_OUT-MENGE_P
*                                    WA_OUT-MEINS_P
*                                    WA_OUT-DMBTR_P
*                                    WA_OUT-WAERS_P
*                                    WA_OUT-LR_P.
*
*      REPLACE ALL OCCURRENCES OF ',' IN WA_OUT-MENGE_D WITH ''.
*      REPLACE ALL OCCURRENCES OF ',' IN WA_OUT-MENGE_M WITH ''.
*      REPLACE ALL OCCURRENCES OF ',' IN WA_OUT-MENGE_P WITH ''.
*
*
*      CONDENSE : WA_OUT-MBLNR, WA_OUT-MJAHR , WA_OUT-ZEILE , WA_OUT-WERKS ,
*                 WA_OUT-MTART, WA_OUT-MATNR , WA_OUT-MAKTX , WA_OUT-MENGE_D,
*                 WA_OUT-MEINS_D, WA_OUT-DMBTR_D, WA_OUT-WAERS_D,  WA_OUT-LR_D , WA_OUT-MENGE_M ,
*                 WA_OUT-MEINS_M , WA_OUT-DMBTR_M , WA_OUT-WAERS_M , WA_OUT-LR_M ,
*                 WA_OUT-MENGE_P, WA_OUT-MEINS_P , WA_OUT-DMBTR_P ,WA_OUT-WAERS_P , WA_OUT-LR_P.
*
*      APPEND WA_out TO IT_out.
*      CLEAR WA_out.
*    ENDLOOP.
*    DELETE IT_out INDEX 1.
*    DESCRIBE TABLE IT_out LINES LINES.
*
*
*
*  ENDIF.


ENDFORM.                    " CALL_REPORT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_XLS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONVERT_XLS .
  IF LIST_TAB[] IS NOT INITIAL.

    CALL FUNCTION 'LIST_TO_ASCI'
      EXPORTING
        LIST_INDEX = -1
      TABLES
        LISTASCI   = VLIST
        LISTOBJECT = LIST_TAB.

    IF SY-SUBRC <> '0'.
      WRITE:/ 'LIST_TO_ASCI error !! ', SY-SUBRC.
    ELSE.

    ENDIF.
  ENDIF.


  LOOP AT VLIST .
    SPLIT VLIST-LINE AT '|' INTO  BLANK
            WA_XLS-MTART
            WA_XLS-WERKS
            wa_xls-plant
            WA_XLS-MATNR
            WA_XLS-MAKTX
            WA_XLS-MENGE_D
*            WA_XLS-MEINS_D
            WA_XLS-RATE_D
            WA_XLS-MENGE_M
*            WA_XLS-MEINS_M
            WA_XLS-RATE_M
            WA_XLS-MENGE_Y
            WA_XLS-MEINS_Y
            WA_XLS-RATE_Y
            WA_XLS-EXTRA_CHAR.

    REPLACE ALL OCCURRENCES OF ',' IN WA_xls-MENGE_D WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN WA_xls-MENGE_M WITH ''.
    REPLACE ALL OCCURRENCES OF ',' IN WA_xls-MENGE_y WITH ''.


    CONDENSE :  WA_XLS-MTART,
            WA_XLS-WERKS,
            wa_xls-plant,
            WA_XLS-MATNR,
            WA_XLS-MAKTX,
            WA_XLS-MENGE_D,
*            WA_XLS-MEINS_D,
            WA_XLS-RATE_D,
            WA_XLS-MENGE_M,
*            WA_XLS-MEINS_M,
            WA_XLS-RATE_M,
            WA_XLS-MENGE_Y,
            WA_XLS-MEINS_Y,
            WA_XLS-RATE_Y,
            WA_XLS-EXTRA_CHAR.

    APPEND WA_XLS TO IT_XLS.
    CLEAR WA_XLS.
  ENDLOOP.
*  DELETE IT_OUT INDEX 1.
*  DESCRIBE TABLE IT_OUT LINES LINES.


ENDFORM.                    " CONVERT_XLS
*&---------------------------------------------------------------------*
*&      Form  CONVERT_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONVERT_HTML .
** Convert a (Saved) List Object to HTML by using below FM.
*    IF LIST_TAB[] IS NOT INITIAL.
*
*      WA_LIST_TAB-RFCSIZE = 10.
*      WA_LIST_TAB-RFCRECORD = '||'.
*      APPEND WA_LIST_TAB TO LIST_TAB.
*
*      CALL FUNCTION 'WWW_HTML_FROM_LISTOBJECT'
*        EXPORTING
*          REPORT_NAME = 'Z6MM031R_ALL_RECEIPTS'
*        TABLES
*          HTML        = HTML
*          LISTOBJECT  = LIST_TAB.
*
*      CLEAR TMP_STR .
*      CLEAR : HTML_WA .
*      LOOP AT HTML INTO HTML_WA .
*        CONCATENATE TMP_STR HTML_WA INTO TMP_STR.
*      ENDLOOP .
*      CLEAR: CONLENGTH,CONLENGTHS .
*      CONLENGTH = STRLEN( TMP_STR ) .
*      CONLENGTHS = CONLENGTH .
*      TRY.
*          SUBJECT = 'Daily receipts'.
*          CONCATENATE  S_MTART-LOW ':' SUBJECT INTO SUBJECT.
*
*          SENDER = 'WF-BATCH@indofil.com'.
*          CLEAR SEND_REQUEST .
*          SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).
*          CLEAR DOCUMENT .
*          DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
*          I_TYPE = 'HTM'
*          I_TEXT = HTML
*          I_LENGTH = CONLENGTHS
*          I_SUBJECT = SUBJECT ).
**add document to send request
*          CALL METHOD SEND_REQUEST->SET_DOCUMENT( DOCUMENT ).
*          CLEAR SENDER_ID .
*          SENDER_ID = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( SENDER ).
*          CALL METHOD SEND_REQUEST->SET_SENDER
*            EXPORTING
*              I_SENDER = SENDER_ID.
*          CLEAR WA_REC .
*          LOOP AT RECEPIENTS INTO WA_REC .
*            CLEAR RECIPIENT .
*            RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*            WA_REC ).
** add recipient with its respective attributes to send request
*            CALL METHOD SEND_REQUEST->ADD_RECIPIENT
*              EXPORTING
*                I_RECIPIENT = RECIPIENT
*                I_EXPRESS   = 'X'.
*          ENDLOOP .
*          CALL METHOD SEND_REQUEST->SET_STATUS_ATTRIBUTES
*            EXPORTING
*              I_REQUESTED_STATUS = 'E'
*              I_STATUS_MAIL      = 'E'.
*          CALL METHOD SEND_REQUEST->SET_SEND_IMMEDIATELY( 'X' ).
** --------- send document ---------------------------------------
*          CALL METHOD SEND_REQUEST->SEND(
*          EXPORTING
*          I_WITH_ERROR_SCREEN = 'X'
*          RECEIVING
*          RESULT = SENT_TO_ALL ).
*          IF SENT_TO_ALL = 'X'.
*            APPEND 'Mail sent successfully ' TO RETURN .
*          ENDIF.
*          COMMIT WORK.
*        CATCH CX_BCS INTO BCS_EXCEPTION.
*          BCS_MESSAGE = BCS_EXCEPTION->GET_TEXT( ).
*          APPEND BCS_MESSAGE TO RETURN .
*          EXIT.
*      ENDTRY.
*    ELSE .
*      APPEND 'Specify email address for sending' TO RETURN .
*    ENDIF .

ENDFORM.                    " CONVERT_HTML
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_MAIL .
 PERFORM BUILD_XLS_DATA_TABLE.
* Populate message body text
  PERFORM POPULATE_EMAIL_MESSAGE_BODY.

* Send file by email as .xls speadsheet
  PERFORM SEND_FILE_AS_EMAIL_ATTACHMENT
                               TABLES IT_MESSAGE
                                      IT_ATTACH
                                USING S_EMAIL
*                                      'Daily Receips doc Attachment'
                                      'XLS'
                                      'RECEIPTS'
                                      ' '
                                      ' '
                                      ' '
                             CHANGING GD_ERROR
                                      GD_RECIEVER.

  SUCCESS = SY-SUBRC.

ENDFORM.                    " SEND_MAIL
*&---------------------------------------------------------------------*
*&      Form  BUILD_XLS_DATA_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_XLS_DATA_TABLE .

 CONSTANTS:
      CON_TAB  TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB,
      CON_CRET TYPE C VALUE CL_ABAP_CHAR_UTILITIES=>CR_LF.

  LOOP AT IT_xls INTO WA_xls.
    CONCATENATE  WA_XLS-MTART WA_XLS-WERKS wa_xls-plant WA_XLS-MATNR WA_XLS-MAKTX
            WA_XLS-MENGE_D  WA_XLS-RATE_D WA_XLS-MENGE_M "WA_XLS-MEINS_D WA_XLS-MEINS_M
             WA_XLS-RATE_M WA_XLS-MENGE_Y WA_XLS-MEINS_Y WA_XLS-RATE_Y
            INTO IT_ATTACH SEPARATED BY CON_TAB.

    CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
    APPEND  IT_ATTACH.
  ENDLOOP.

ENDFORM.                    " BUILD_XLS_DATA_TABLE
*&---------------------------------------------------------------------*
*&      Form  POPULATE_EMAIL_MESSAGE_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULATE_EMAIL_MESSAGE_BODY .
  DATA: S_ERDATLOW(10), S_ERDATHIGH(10) .
  CLEAR: S_ERDATLOW , S_ERDATHIGH.
  REFRESH IT_MESSAGE.
  CONCATENATE START_DT+06(02) '.' START_DT+04(02) '.' START_DT(04) INTO S_ERDATLOW.
  CONCATENATE sy-datum+06(02) '.' sy-datum+04(02) '.' sy-datum(04) INTO S_ERDATHIGH.
  CONCATENATE 'Please find Receipts Created From:'  S_ERDATLOW  'To  ' S_ERDATHIGH INTO IT_MESSAGE separated by space.
*  IT_MESSAGE = 'Please find attached file of',days STO records for T.STANES.'.
  APPEND IT_MESSAGE.
ENDFORM.                    " POPULATE_EMAIL_MESSAGE_BODY

FORM SEND_FILE_AS_EMAIL_ATTACHMENT TABLES PIT_MESSAGE
                                          PIT_ATTACH
                                    USING S_EMAIL
*                                          P_MTITLE
                                          P_FORMAT
                                          P_FILENAME
                                          P_ATTDESCRIPTION
                                          P_SENDER_ADDRESS
                                          P_SENDER_ADDRES_TYPE
                                 CHANGING P_ERROR
                                          P_RECIEVER.


  DATA: LD_ERROR    TYPE SY-SUBRC,
          LD_RECIEVER TYPE SY-SUBRC,
          LD_MTITLE LIKE SODOCCHGI1-OBJ_DESCR,
          LD_EMAIL LIKE  SOMLRECI1-RECEIVER,
          LD_FORMAT TYPE  SO_OBJ_TP ,
          LD_ATTDESCRIPTION TYPE  SO_OBJ_NAM ,
          LD_ATTFILENAME TYPE  SO_OBJ_DES ,
          LD_SENDER_ADDRESS LIKE  SOEXTRECI1-RECEIVER,
          LD_SENDER_ADDRESS_TYPE LIKE  SOEXTRECI1-ADR_TYP,
          LD_RECEIVER LIKE  SY-SUBRC.


  CONCATENATE s_mtart-low 'Daily Receips' into LD_MTITLE separated by space.

*  LD_EMAIL   = P_EMAIL.
*  LD_MTITLE = P_MTITLE.
  LD_FORMAT              = P_FORMAT.
  LD_ATTDESCRIPTION      = P_ATTDESCRIPTION.
  LD_ATTFILENAME         = P_FILENAME.
  LD_SENDER_ADDRESS      = P_SENDER_ADDRESS.
  LD_SENDER_ADDRESS_TYPE = P_SENDER_ADDRES_TYPE.


* Fill the document data.
  W_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  W_DOC_DATA-OBJ_LANGU = SY-LANGU.
  W_DOC_DATA-OBJ_NAME  = 'RECEIPTS'.
  W_DOC_DATA-OBJ_DESCR = LD_MTITLE .
  W_DOC_DATA-SENSITIVTY = 'F'.

* Fill the document data and get size of attachment
  CLEAR W_DOC_DATA.
  READ TABLE IT_ATTACH INDEX W_CNT.
  W_DOC_DATA-DOC_SIZE =
     ( W_CNT - 1 ) * 255 + STRLEN( IT_ATTACH ).
  W_DOC_DATA-OBJ_LANGU  = SY-LANGU.
  W_DOC_DATA-OBJ_NAME   = 'RECEIPTS'.
  W_DOC_DATA-OBJ_DESCR  = LD_MTITLE.
  W_DOC_DATA-SENSITIVTY = 'F'.
  CLEAR T_ATTACHMENT.
  REFRESH T_ATTACHMENT.
  T_ATTACHMENT[] = PIT_ATTACH[].

* Describe the body of the message
  CLEAR T_PACKING_LIST.
  REFRESH T_PACKING_LIST.
  T_PACKING_LIST-TRANSF_BIN = SPACE.
  T_PACKING_LIST-HEAD_START = 1.
  T_PACKING_LIST-HEAD_NUM = 0.
  T_PACKING_LIST-BODY_START = 1.
  DESCRIBE TABLE IT_MESSAGE LINES T_PACKING_LIST-BODY_NUM.
  T_PACKING_LIST-DOC_TYPE = 'RAW'.
  T_PACKING_LIST-OBJ_NAME = 'RECEIPTS'.
  APPEND T_PACKING_LIST.

* Create attachment notification
  T_PACKING_LIST-TRANSF_BIN = 'X'.
  T_PACKING_LIST-HEAD_START = 1.
  T_PACKING_LIST-HEAD_NUM   = 1.
  T_PACKING_LIST-BODY_START = 1.

  DESCRIBE TABLE T_ATTACHMENT LINES T_PACKING_LIST-BODY_NUM.
  T_PACKING_LIST-DOC_TYPE   =  LD_FORMAT.
  T_PACKING_LIST-OBJ_DESCR  =  LD_ATTDESCRIPTION.
  T_PACKING_LIST-OBJ_NAME   =  LD_ATTFILENAME.
  T_PACKING_LIST-DOC_SIZE   =  T_PACKING_LIST-BODY_NUM * 255.
  APPEND T_PACKING_LIST.

*  LD_EMAIL   = P_EMAIL.
* Add the recipients email address

CLEAR:zegroup .
CONCATENATE 'ZPP015' S_MTART-low INTO zegroup.

  SELECT EMAIL
    FROM ZFI044_EMAILID
    INTO TABLE IT_RECpt
    WHERE EGROUP = zegroup."'ZPP015'.

  IF IT_RECPT IS  NOT INITIAL.
    LOOP AT IT_RECPT INTO WA_RECPT.

*      CLEAR T_RECEIVERS[].
*      REFRESH T_RECEIVERS[].
      T_RECEIVERS-RECEIVER = WA_RECPT-EMAIL."'pshinde-icc@modi.com'."LD_EMAIL.
      T_RECEIVERS-REC_TYPE = 'U'.
      T_RECEIVERS-COM_TYPE = 'INT'.
      T_RECEIVERS-NOTIF_DEL = 'X'.
      T_RECEIVERS-NOTIF_NDEL = 'X'.
      APPEND T_RECEIVERS.

    ENDLOOP.

  ENDIF.


  CONCATENATE 'RECEIPTS' SY-DATUM '.XLS' INTO T_OBJECT_HEADER.
*  T_OBJECT_HEADER = 'T_STANES.xls'.
  APPEND T_OBJECT_HEADER.

* Sending the document
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = W_DOC_DATA
      PUT_IN_OUTBOX              = 'X'
      COMMIT_WORK                = 'X'
    TABLES
      PACKING_LIST               = T_PACKING_LIST
      OBJECT_HEADER              = T_OBJECT_HEADER
      CONTENTS_BIN               = T_ATTACHMENT
      CONTENTS_TXT               = IT_MESSAGE
      RECEIVERS                  = T_RECEIVERS
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      OPERATION_NO_AUTHORIZATION = 4
      OTHERS                     = 99.
  CASE SY-SUBRC.
    WHEN 0.
      WRITE: / 'Result send process:'.
      LOOP AT T_RECEIVERS.
        WRITE: / T_RECEIVERS-RECEIVER(48), ':'.
        IF T_RECEIVERS-RETRN_CODE = 0.
          WRITE 'sent successfully'.
        ELSE.
          WRITE 'not sent'.
        ENDIF.
      ENDLOOP.
    WHEN 1.
      WRITE: / 'no authorization to send to the specified number of recipients!'.
    WHEN 2.
      WRITE: / 'document could not be sent to any of the recipients!'.
    WHEN 4.
      WRITE: / 'no authorization to send !'.
    WHEN OTHERS.
      WRITE: / 'error occurred during sending !'.
  ENDCASE.



*
* Populate zerror return code
  LD_ERROR = SY-SUBRC.

* Populate zreceiver return code
  LOOP AT T_RECEIVERS.
    LD_RECEIVER = T_RECEIVERS-RETRN_CODE.
  ENDLOOP.


ENDFORM.                    " SEND_FILE_AS_EMAIL_ATTACHMENT
