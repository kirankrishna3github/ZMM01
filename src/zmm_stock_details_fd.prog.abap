*&---------------------------------------------------------------------*
*&  Include           ZMM_STOCK_DETAILS_FD
*&---------------------------------------------------------------------*

************************************ logic ***************************
FORM GET_DATA.
  SELECT MATNR WERKS LGORT CHARG ERSDA LAEDA CLABS
    FROM MCHB
    INTO TABLE GT_MCHB
    WHERE LGORT IN ('1501' , '1601')
    AND   CLABS NE 0.

  IF SY-SUBRC = 0.
    SELECT MATNR CHARG LAEDA VFDAT HSDAT
      FROM MCH1
      INTO TABLE GT_MCH1
      FOR ALL ENTRIES IN GT_MCHB
      WHERE MATNR = GT_MCHB-MATNR
      AND   CHARG = GT_MCHB-CHARG.

    SELECT MATNR MTART MATKL MEINS SPART EXTWG
      FROM MARA
      INTO TABLE GT_MARA
      FOR ALL ENTRIES IN GT_MCHB
      WHERE MATNR EQ GT_MCHB-MATNR
      AND MTART IN ('ZFGM' , 'ZTRD')
      AND SPART = '10'.

    IF SY-SUBRC = 0.
      SELECT MATNR SPRAS MAKTX
        FROM MAKT
      INTO TABLE GT_MAKT
      FOR ALL ENTRIES IN GT_MARA
      WHERE MATNR EQ GT_MARA-MATNR
      AND   SPRAS EQ SY-LANGU.
    ENDIF.
  ENDIF.

  SORT : GT_MARD BY MATNR,
         GT_MCHB BY MATNR,
         GT_MARA BY MATNR,
         GT_MAKT BY MATNR,
         GT_MCH1 BY MATNR CHARG.

  PERFORM PROCESS_DATA.
ENDFORM.                    "get_data


*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: ZLAND1 TYPE T001W-LAND1.
  LOOP AT GT_MCHB INTO GS_MCHB.
    READ TABLE GT_MARA INTO GS_MARA WITH KEY MATNR = GS_MCHB-MATNR. " MTART = 'ZFGM'.
    IF SY-SUBRC = 0.

      TAB_BATCH_STOCK-MATNR = GS_MARA-MATNR.
      TAB_BATCH_STOCK-MEINS = GS_MARA-MEINS.


      READ TABLE GT_MAKT INTO GS_MAKT WITH KEY MATNR = GS_MARA-MATNR.
      IF SY-SUBRC = 0.
        TAB_BATCH_STOCK-MAKTX = GS_MAKT-MAKTX.
      ENDIF.

      READ TABLE GT_MCH1 INTO GS_MCH1 WITH KEY MATNR = GS_MCHB-MATNR CHARG = GS_MCHB-CHARG.
      IF SY-SUBRC = 0.
        TAB_BATCH_STOCK-VFDAT = GS_MCH1-VFDAT.
        TAB_BATCH_STOCK-HSDAT = GS_MCH1-HSDAT.
        IF GS_MCHB-LAEDA IS INITIAL.
          TAB_BATCH_STOCK-LAEDA = GS_MCH1-LAEDA.
        ELSE.
          TAB_BATCH_STOCK-LAEDA = GS_MCHB-LAEDA.
        ENDIF.
      ENDIF.
      IF GS_MCHB-LAEDA IS INITIAL AND GS_MCH1-LAEDA IS INITIAL.
        TAB_BATCH_STOCK-LAEDA = GS_MCHB-ERSDA.
      ENDIF.
      TAB_BATCH_STOCK-CHARG = GS_MCHB-CHARG.
      TAB_BATCH_STOCK-CLABS = GS_MCHB-CLABS.
      TAB_BATCH_STOCK-WERKS = GS_MCHB-WERKS.

      SELECT SINGLE NAME1
        FROM T001W
        INTO TAB_BATCH_STOCK-NAME1
        WHERE WERKS = GS_MCHB-WERKS.


      SELECT SINGLE WGBEZ INTO TAB_BATCH_STOCK-WGBEZ FROM T023T
        WHERE SPRAS = 'EN'
        AND   MATKL = GS_MARA-MATKL.

      SELECT SINGLE EWBEZ INTO TAB_BATCH_STOCK-EWBEZ FROM TWEWT
        WHERE SPRAS = 'EN'
        AND   EXTWG = GS_MARA-EXTWG.


      PERFORM REM_ZEROS :
             USING TAB_BATCH_STOCK-MATNR CHANGING TAB_BATCH_STOCK-MATNR,
             USING TAB_BATCH_STOCK-CHARG CHANGING TAB_BATCH_STOCK-CHARG.

      CLEAR: TAB_BATCH_STOCK-CITYC , TAB_BATCH_STOCK-BEZEI , ZLAND1.

      SELECT SINGLE CITYC LAND1
        FROM T001W
        INTO (TAB_BATCH_STOCK-CITYC , ZLAND1)
        WHERE WERKS = TAB_BATCH_STOCK-WERKS
        AND SPRAS = SY-LANGU.

      SELECT SINGLE BEZEI
        FROM T005H
        INTO TAB_BATCH_STOCK-BEZEI
        WHERE SPRAS = SY-LANGU
        AND LAND1 = ZLAND1
        AND CITYC = TAB_BATCH_STOCK-CITYC.

      APPEND TAB_BATCH_STOCK.
    ENDIF.
    CLEAR : GS_MARD , GS_MARA , GS_MAKT , TAB_BATCH_STOCK , GS_MCHB , GS_MCH1.
  ENDLOOP.
ENDFORM.                    "process_data
************************************ logic ***************************
*&---------------------------------------------------------------------*
*&      Form  rem_zeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->A1         text
*      -->A2         text
*----------------------------------------------------------------------*
FORM REM_ZEROS USING A1 TYPE ANY
               CHANGING A2 TYPE ANY.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = A1
    IMPORTING
      OUTPUT = A2.
ENDFORM.                    "rem_zeros
*&---------------------------------------------------------------------*
*&      Form  add_zeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->A1         text
*      -->A2         text
*----------------------------------------------------------------------*
FORM ADD_ZEROS USING A1 TYPE ANY
               CHANGING A2 TYPE ANY.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = A1
    IMPORTING
      OUTPUT = A2.
ENDFORM.                    "add_zeros
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*

FORM BUILD_EVENTTAB USING P_EVENTS TYPE SLIS_T_EVENT.

  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = P_EVENTS.

  READ TABLE P_EVENTS WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO P_EVENTS.
  ENDIF.

ENDFORM.                               " BUILD_EVENTTAB

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LAYOUT   text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT  USING    P_LAYOUT TYPE SLIS_LAYOUT_ALV.
  P_LAYOUT-F2CODE         = F2CODE.
  P_LAYOUT-ZEBRA        = 'X'.
  P_LAYOUT-DETAIL_POPUP = 'X'.
  "added by nitin.
*   p_layout-group_change_edit = 'X'.
  "end by nitin.

ENDFORM.                    "build_layout
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  PERFORM BUILD_FIELDCATALOG.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_BYPASSING_BUFFER     = 'X'
        I_CALLBACK_PROGRAM     = SY-REPID
***                i_callback_top_of_page = 'TOP-OF-PAGE'
        IS_LAYOUT              = LAYOUT
        IT_FIELDCAT            = FIELDCATALOG[]
        I_SAVE                 = 'A'
        IS_VARIANT             = GX_VARIANT
***          it_events              = gt_events
      TABLES
        T_OUTTAB               = TAB_BATCH_STOCK[].
ENDFORM.                    "display_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCATALOG.
  DATA : POS TYPE I.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'WGBEZ'.
  FIELDCATALOG-SELTEXT_M   = 'Material Group'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'EWBEZ'.
  FIELDCATALOG-SELTEXT_M   = 'Ext Material Group'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'WERKS'.
  FIELDCATALOG-SELTEXT_M   = 'Plant Code'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'NAME1'.
  FIELDCATALOG-SELTEXT_M   = 'Plant Name'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'MATNR'.
  FIELDCATALOG-SELTEXT_M   = 'Material'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'MAKTX'.
  FIELDCATALOG-SELTEXT_M   = 'Material Desc'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'CHARG'.
  FIELDCATALOG-SELTEXT_M   = 'Batch'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'CLABS'.
  FIELDCATALOG-SELTEXT_M   = 'Stock'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'MEINS'.
  FIELDCATALOG-SELTEXT_M   = 'UOM'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'VFDAT'.
  FIELDCATALOG-SELTEXT_M   = 'Expiry Date'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'LAEDA'.
  FIELDCATALOG-SELTEXT_M   = 'Last Movement Date'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'HSDAT'.
  FIELDCATALOG-SELTEXT_M   = 'Manufacturing Date'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.

  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'CITYC'.
  FIELDCATALOG-SELTEXT_M   = 'Region Code'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.


  POS = POS + 1.
  FIELDCATALOG-FIELDNAME   = 'BEZEI'.
  FIELDCATALOG-SELTEXT_M   = 'Region'.
  FIELDCATALOG-COL_POS     = POS.
  "fieldcatalog-hotspot     = 'X'.
  APPEND FIELDCATALOG TO FIELDCATALOG.
  CLEAR  FIELDCATALOG.


ENDFORM.                    "build_fieldcatalog

*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SEND_MAIL.
  DATA: BEGIN OF WA_MAIL,
        EMAIL TYPE ZFI044_EMAILID-EMAIL,
        END OF WA_MAIL,
        IT_EMAIL LIKE TABLE OF WA_MAIL.

  DATA : STR1 TYPE STRING,
         LINE_NUMBER TYPE I.
  CLEAR : IT_ATTACH , IT_ATTACH[],EMAIL,P_EMPCODE,ENAME,STR.
  CONCATENATE
  'Material Group'
  'External Material Group'
  'Plant'
  'Name'
  'Material Number'
  'Material Description'
  'Batch Number'
  'Stock'
  'Unit of Measure'
  'Expiry Date'
  'Last Movement Date'
  'Manufacture Date'
  'Region Code'
  'Region'

  INTO IT_ATTACH SEPARATED BY CON_TAB.
  CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
  APPEND  IT_ATTACH.

  LOOP AT TAB_BATCH_STOCK INTO STRUC_BATCH_STOCK.
    GS_STRING-CLABS = STRUC_BATCH_STOCK-CLABS.
    CONCATENATE :
      STRUC_BATCH_STOCK-VFDAT+6(2) STRUC_BATCH_STOCK-VFDAT+4(2) STRUC_BATCH_STOCK-VFDAT+0(4) INTO GS_STRING-DATE1 SEPARATED BY '.',
      STRUC_BATCH_STOCK-LAEDA+6(2) STRUC_BATCH_STOCK-LAEDA+4(2) STRUC_BATCH_STOCK-LAEDA+0(4) INTO GS_STRING-DATE2 SEPARATED BY '.',
      STRUC_BATCH_STOCK-HSDAT+6(2) STRUC_BATCH_STOCK-HSDAT+4(2) STRUC_BATCH_STOCK-HSDAT+0(4) INTO GS_STRING-DATE3 SEPARATED BY '.'.
    CONCATENATE :
      STRUC_BATCH_STOCK-WGBEZ
      STRUC_BATCH_STOCK-EWBEZ
      STRUC_BATCH_STOCK-WERKS
      STRUC_BATCH_STOCK-NAME1
      STRUC_BATCH_STOCK-MATNR
      STRUC_BATCH_STOCK-MAKTX
      STRUC_BATCH_STOCK-CHARG
      GS_STRING-CLABS
      STRUC_BATCH_STOCK-MEINS
      GS_STRING-DATE1
      GS_STRING-DATE2
      GS_STRING-DATE3
      STRUC_BATCH_STOCK-CITYC
      STRUC_BATCH_STOCK-BEZEI
    INTO IT_ATTACH SEPARATED BY CON_TAB.

    CONCATENATE CON_CRET IT_ATTACH  INTO IT_ATTACH.
    APPEND  IT_ATTACH.
    CLEAR : STRUC_BATCH_STOCK , GS_STRING.
  ENDLOOP.

********************** Message ******************
  CLEAR : IT_MESSAGE , IT_MESSAGE[],STR.
  REFRESH IT_MESSAGE.
  STR = 'Dear All'.
  IT_MESSAGE = STR.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  CONCATENATE : SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM+0(4) INTO DATE SEPARATED BY '.',
                SY-UZEIT(2)   SY-UZEIT+2(2) SY-UZEIT+4(2) INTO TIME SEPARATED BY '.'.
  IF SY-UZEIT(2) < '12'.
    CONCATENATE TIME 'AM' INTO TIME SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TIME 'PM' INTO TIME SEPARATED BY SPACE.
  ENDIF.
  CONCATENATE 'Please Find the attached XLS file for Depot Stocks as on .' DATE TIME INTO STR SEPARATED BY SPACE.
  IT_MESSAGE = STR.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  IT_MESSAGE = ' '.
  APPEND IT_MESSAGE.
  IT_MESSAGE = 'PLEASE NOTE :- THIS IS AN AUTO GENERATED MAIL, PLEASE DO NOT REPLY TO THIS MAIL.'.
  APPEND IT_MESSAGE.
********************** Message ******************

****************** send Mail ********************
  CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) INTO DATE SEPARATED BY '.'.
  CONCATENATE SY-UZEIT(2) SY-UZEIT+2(2) SY-UZEIT+4(2) INTO TIME SEPARATED BY '.'.
  IF SY-UZEIT(2) < '12'.
    CONCATENATE TIME 'AM' INTO TIME SEPARATED BY SPACE.
  ELSE.
    CONCATENATE TIME 'PM' INTO TIME SEPARATED BY SPACE.
  ENDIF.

  CONCATENATE 'Depot Stock as on ' DATE TIME INTO STR1 SEPARATED BY SPACE.
  LD_MTITLE = STR1.
  LD_FORMAT              = 'XLS'.
  LD_ATTDESCRIPTION      = ''.
  LD_ATTFILENAME         = STR1.
  LD_SENDER_ADDRESS      = 'sapautomail-icc@modi.com'.
  LD_SENDER_ADDRESS_TYPE = 'SMTP'.

  CLEAR : W_DOC_DATA.
* Fill the document data.
  W_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  W_DOC_DATA-OBJ_LANGU = SY-LANGU.
  W_DOC_DATA-OBJ_NAME  = STR1.
  W_DOC_DATA-OBJ_DESCR = LD_MTITLE .
  W_DOC_DATA-SENSITIVTY = 'F'.

* Fill the document data and get size of attachment
  DESCRIBE TABLE OBJTXT LINES TAB_LINES.
  READ TABLE OBJTXT INDEX TAB_LINES.
  READ TABLE IT_ATTACH INDEX W_CNT.
  W_DOC_DATA-DOC_SIZE = ( TAB_LINES - 1 ) * 255 + STRLEN( OBJTXT ).
***  w_doc_data-doc_size = ( w_cnt - 1 ) * 255 + STRLEN( it_attach ).

  W_DOC_DATA-OBJ_LANGU  = SY-LANGU.
  W_DOC_DATA-OBJ_NAME   = 'Str1'.
  W_DOC_DATA-OBJ_DESCR  = LD_MTITLE.
  W_DOC_DATA-SENSITIVTY = 'F'.
***  w_doc_data-no_change = 'X'.
  CLEAR : T_ATTACHMENT,T_ATTACHMENT[].
  REFRESH : T_ATTACHMENT,T_ATTACHMENT[].
  T_ATTACHMENT[] = IT_ATTACH[].

* Describe the body of the message
  CLEAR: T_PACKING_LIST , T_PACKING_LIST[].
  REFRESH T_PACKING_LIST.
  T_PACKING_LIST-TRANSF_BIN = SPACE.
  T_PACKING_LIST-HEAD_START = 1.
  T_PACKING_LIST-HEAD_NUM = 0.
  T_PACKING_LIST-BODY_START = 1.
  T_PACKING_LIST-BODY_NUM   = TAB_LINES.
  DESCRIBE TABLE IT_MESSAGE LINES T_PACKING_LIST-BODY_NUM.
  T_PACKING_LIST-DOC_TYPE = 'RAW'.
***  t_packing_list-doc_type = 'XLS'.
  T_PACKING_LIST-OBJ_NAME = 'Customer Credit Exposure Report'.
  APPEND T_PACKING_LIST.

* Create attachment notification
  T_PACKING_LIST-TRANSF_BIN = 'X'.
  T_PACKING_LIST-HEAD_START = 1.
  T_PACKING_LIST-HEAD_NUM   = 1.
  T_PACKING_LIST-BODY_START = 1.

  DESCRIBE TABLE IT_ATTACH LINES T_PACKING_LIST-BODY_NUM.
  T_PACKING_LIST-DOC_TYPE   =  LD_FORMAT.
  T_PACKING_LIST-OBJ_DESCR  =  LD_ATTDESCRIPTION.
  T_PACKING_LIST-OBJ_NAME   =  LD_ATTFILENAME.
  T_PACKING_LIST-DOC_SIZE   =  T_PACKING_LIST-BODY_NUM * 255.
  APPEND T_PACKING_LIST.

  CLEAR: IT_EMAIL, WA_MAIL.

  SELECT EMAIL FROM ZFI044_EMAILID
    INTO TABLE IT_EMAIL
    WHERE  EGROUP = 'DEPOTSTOCK'.


  CLEAR : T_RECEIVERS , T_RECEIVERS[].
  LOOP AT IT_EMAIL INTO WA_MAIL.
    T_RECEIVERS-RECEIVER = WA_MAIL-EMAIL.
    T_RECEIVERS-REC_TYPE = 'U'.
    T_RECEIVERS-COM_TYPE = 'INT'.
    T_RECEIVERS-NOTIF_DEL = 'X'.
    T_RECEIVERS-NOTIF_NDEL = 'X'.
    APPEND T_RECEIVERS.
    CLEAR : T_RECEIVERS.

  ENDLOOP.
*  t_receivers-receiver = 'grp-agro-sbu@modi.com'.
***  t_receivers-receiver = 'raulnish09@gmail.com'.
*  t_receivers-rec_type = 'U'.
*  t_receivers-com_type = 'INT'.
*  t_receivers-notif_del = 'X'.
*  t_receivers-notif_ndel = 'X'.
*  APPEND t_receivers.

***  t_object_header = 'Customer Outstanding.XLS'.
  CLEAR : FILENAME,T_OBJECT_HEADER[],T_OBJECT_HEADER.
  PERFORM REM_ZEROS USING P_EMPCODE CHANGING P_EMPCODE.
  "break ibm_ams.

***  CONCATENATE 'E' p_empcode '-Customer Credit Exposure Report As On' date time '.XLS' INTO filename
***    SEPARATED BY space.

  CONCATENATE STR1 '.XLS' INTO FILENAME
    SEPARATED BY SPACE.
  T_OBJECT_HEADER = FILENAME.
***  CONCATENATE  INTO T_OBJECT_HEADER.
*  T_OBJECT_HEADER = 'T_STANES.xls'.
  APPEND T_OBJECT_HEADER.

  DESCRIBE TABLE T_ATTACHMENT LINES LINE_NUMBER.
* Sending the document
  IF T_RECEIVERS[] IS NOT INITIAL AND LINE_NUMBER GT 1.
    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        DOCUMENT_DATA              = W_DOC_DATA
        PUT_IN_OUTBOX              = 'X'
        SENDER_ADDRESS             = LD_SENDER_ADDRESS         " sender's address
        SENDER_ADDRESS_TYPE        = LD_SENDER_ADDRESS_TYPE
        COMMIT_WORK                = 'X'
      TABLES
        PACKING_LIST               = T_PACKING_LIST
        OBJECT_HEADER              = T_OBJECT_HEADER
        CONTENTS_BIN               = T_ATTACHMENT
        CONTENTS_TXT               = IT_MESSAGE
***      contents_hex               = objbin
        RECEIVERS                  = T_RECEIVERS
      EXCEPTIONS
        TOO_MANY_RECEIVERS         = 1
        DOCUMENT_NOT_SENT          = 2
        DOCUMENT_TYPE_NOT_EXIST    = 3
        OPERATION_NO_AUTHORIZATION = 4
        PARAMETER_ERROR            = 5
        X_ERROR                    = 6
        ENQUEUE_ERROR              = 7
        OTHERS                     = 8.
    LD_ERROR = SY-SUBRC.
***  ENDIF.
* Populate zreceiver return code

    LOOP AT T_RECEIVERS.
      LD_RECEIVER = T_RECEIVERS-RETRN_CODE.
    ENDLOOP.
  ENDIF.
****************** send Mail ********************

ENDFORM.                    "send_mail
