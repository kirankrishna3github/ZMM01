*&---------------------------------------------------------------------*
*& Report  Z6R_BIRTHDAY_GREETING
*& developer : punam shinde
*&---------------------------------------------------------------------*
*& requirement :Mr.Sanjays Shah sir
*& developement date: 12.11.2011.
*  this program send email to employee on their birthday , the image in birthday mail is store in
*  gmail id : indofil.images@gmail.com
*  password : indofilsap1
** now we are not using above mail id
** uploaded image in imgur and copied link in below code . -- 30.1.2020


REPORT  Z6R_BIRTHDAY_GREETING MESSAGE-ID ZZ
                       LINE-SIZE 500
                       NO STANDARD PAGE HEADING.

TABLES: SOMLRECI1.

*----------------------------------------------------------------------*
*                     GLOBAL DATA DECLARATION                          *
*----------------------------------------------------------------------*
DATA: T_MAILHEX   TYPE STANDARD TABLE OF SOLIX,
      T_CONTENTS  TYPE STANDARD TABLE OF SOLISTI1,
      WA_CONTENTS TYPE SOLISTI1,
      W_FILE      TYPE DSVASDOCID,
      W_EXTN(5)   TYPE C,
      W_MAIL_SUBJ TYPE STRING,
      W_DOCUMENT  TYPE REF TO CL_DOCUMENT_BCS.

*----------------------------------------------------------------------*
*                     CONSTANTS DECLARATION                            *
*----------------------------------------------------------------------*
CONSTANTS:
*-- Constants used in the body of the Email (HTML)
C_HTM         TYPE CHAR3   VALUE 'HTM',
C_STYLE_START TYPE CHAR255 VALUE '<FONT face=Arial size=2>'.
DATA: C_NEW_LINE    TYPE CHAR255 VALUE 'Dear ',"<br>',
*C_LINK_START  TYPE STRING VALUE '<img src="https://lh6.googleusercontent.com/-LWci_6RjpUs/Ts3LafLI1ZE/AAAAAAAAAAw/UdGszYFGtWs/November242011.jpg">',
*'<img src="\\172.16.1.15\SAP Upload\IMAGE\HAPPY BIRTHDAY.jpg">' ,
*C_LINK_START  TYPE STRING VALUE '<img src="https://lh3.googleusercontent.com/-wpfsB1fSFRM/VrRGVs0UosI/AAAAAAAAAH4/Cl_chqYGut0/s640-Ic42/RKMDBTOALL.png">',
*C_LINK_START  TYPE STRING VALUE '<img src="https://goo.gl/33Mnxs">',
*** new image uploaded as mail from ANITA mhatre on 29.1.2020
*C_LINK_START  TYPE STRING VALUE '<img src="https://photos.app.goo.gl/8ZsyJycY6KBEVtRcA">',
*C_LINK_START  TYPE STRING VALUE '<img src="https://tinyurl.com/s4cjet4">',

C_LINK_START  TYPE STRING VALUE '<img src="https://i.imgur.com/g9pLPoY.jpg">',

*'<img src="https://lh5.googleusercontent.com/-0PQl09HIoww/Ts3LaXAmRcI/AAAAAAAAAAk/PN3ydOse82c/s600/indoimg.jpg">',
"http://cheapbirthdayinvitation.com/wp-content/uploads/2011/09/free-birthday-greetings2.jpg">',"'<img src="\\172.16.1.15\SAP Upload\birthday-greeting.jpg">' ,
*'<a href="http://s1189.photobucket.com/albums/z425/poonam29884/?action=view&amp;current=birthday-greeting.jpg" target="_blank"><img src="http://i1189.photobucket.com/albums/z425/poonam29884/birthday-greeting.jpg"border="0" alt="Photobucket"></a>',
"'<img src="\\172.16.1.15\SAP Upload\birthday-greeting.jpg">' ,"'<A href="www.w3schools.com">',
C_LINK_TEXT   TYPE CHAR32  VALUE '',
C_LINK_END    TYPE CHAR4   VALUE '</A>',
C_SPACE(6)    TYPE C       VALUE '&nbsp;',

*-- Used as an Example for displaying space between texts in Email body
C_EMP1(6)     TYPE C       VALUE 101001,
C_EMP2(6)     TYPE C       VALUE 101002,
C_EMP3(6)     TYPE C       VALUE 101003.

DATA: S_MAILID LIKE STANDARD TABLE OF SOMLRECI1.
DATA: WL_MAILID LIKE LINE OF S_MAILID.

DATA:BEGIN OF WA_PERNR,
      PERNR TYPE PA0002-PERNR,
      GBDAT TYPE PA0002-GBDAT,
*      MIDNM TYPE PA0002-MIDNM,
     USRID_LONG TYPE PA0105-USRID_LONG,
     VORNA TYPE PA0002-VORNA,
     NACHN TYPE PA0002-NACHN,
     ANRED TYPE PA0002-ANRED,
     ATEXT TYPE T522T-ATEXT,
*     BEGDA TYPE PA0000-BEGDA,
     END OF WA_PERNR,
     IT_PERNR LIKE STANDARD TABLE OF WA_PERNR.


*----------------------------------------------------------------------*
*                     SELECTION SCREEN                                 *
*----------------------------------------------------------------------*
*-- Input Details - Block
*SELECTION-SCREEN BEGIN OF BLOCK file WITH FRAME TITLE text-t01.
*PARAMETERS: p_attach   TYPE  rlgrap-filename.
*SELECTION-SCREEN END OF BLOCK file.
*-- Email ID of the Recipient
*SELECTION-SCREEN BEGIN OF BLOCK mail WITH FRAME TITLE text-t02.
*SELECT-OPTIONS: s_mailid  FOR somlreci1-receiver NO INTERVALS.
*SELECTION-SCREEN END OF BLOCK mail.

*----------------------------------------------------------------------*
*                          AT SELECTION SCREEN                         *
*----------------------------------------------------------------------*
*-- Providing F4 Help for the input file
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_attach.
*  PERFORM file_path USING 'P_ATTACH'.

*&---------------------------------------------------------------------*
*                       START-OF-SELECTION                             *
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_RECEIVERS.

*-- Upload data from Presentation Server to SAP
  PERFORM UPLOAD_DATA.

*-- Frame the Body of the Email
  IF S_MAILID IS NOT INITIAL.

*-- Send Mail
    PERFORM SEND_MAIL.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Form Name     : Send_mail                                           *
*&---------------------------------------------------------------------*
*& Description   : To set the recipients and send the mail             *
*&---------------------------------------------------------------------*
*& Parameters    : None                                                *
*&---------------------------------------------------------------------*
FORM SEND_MAIL.

*-- Local data declaration for sending mail
  DATA: L_SEND_REQUEST  TYPE REF TO CL_BCS,
        L_DOCUMENT      TYPE REF TO CL_DOCUMENT_BCS,
        L_SENDER        TYPE REF TO CL_CAM_ADDRESS_BCS,"CL_SAPUSER_BCS,
        L_SUB           TYPE CHAR50,
        L_RECIPIENT     TYPE REF TO IF_RECIPIENT_BCS,
        TL_CONTENTS     TYPE STANDARD TABLE OF SOLI,
        L_DOC_LEN       TYPE SO_OBJ_LEN,
        L_CNT           TYPE SY-TABIX,
        L_RCV_EMAIL     TYPE ADR6-SMTP_ADDR,
        L_RESULT        TYPE SY-BINPT,
        L_BCS_EXCEPTION TYPE REF TO CX_BCS,
        L_SUBJ          TYPE STRING.

  DATA: SENDER TYPE SY-UNAME.

  TRY.

*-- Add the recipients to the Send mail
      LOOP AT S_MAILID INTO WL_MAILID.
        PERFORM MAIL_BODY.
*-- Create persistent send request
        L_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).
        TL_CONTENTS[] = T_CONTENTS[].

*-- Get the length of the Document
        DESCRIBE TABLE TL_CONTENTS LINES L_CNT.
        READ TABLE TL_CONTENTS INTO WA_CONTENTS INDEX L_CNT.
        L_DOC_LEN = ( L_CNT - 1 ) * 255 + STRLEN( WA_CONTENTS ).
*-- Subject of the mail
        L_SUB = W_MAIL_SUBJ.

*-- Create Document
        L_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
                     I_TYPE       = C_HTM
                     I_TEXT       = TL_CONTENTS
                     I_LENGTH     = L_DOC_LEN
                     I_SUBJECT    = L_SUB
                     I_LANGUAGE   = SY-LANGU
                     I_IMPORTANCE = '1' ).
*-- Subject of the mail
        MOVE W_MAIL_SUBJ TO L_SUBJ.
        W_DOCUMENT = L_DOCUMENT.

        TRY.
*-- Set the Message Subject
            CALL METHOD L_SEND_REQUEST->SET_MESSAGE_SUBJECT
              EXPORTING
                IP_SUBJECT = L_SUBJ.
          CATCH CX_SY_DYN_CALL_ILLEGAL_METHOD.
        ENDTRY.

*-- Add document to send request
        CALL METHOD L_SEND_REQUEST->SET_DOCUMENT( L_DOCUMENT ).

*-- Do send delivery info for successful mails
        CALL METHOD L_SEND_REQUEST->SET_STATUS_ATTRIBUTES
          EXPORTING
            I_REQUESTED_STATUS = 'E'
            I_STATUS_MAIL      = 'A'.

*-- Set sender

*        SENDER = '125'.
*         sy-uname = '125'.

        DATA: V_SENDER TYPE AD_SMTPADR.
        DATA L_VISNAME TYPE AD_NAMELAS.
        V_SENDER =   'rkm-icc@modi.com'." 'sshah-icc@modi.com'."
*  L_VISNAME = 'R.K.Malhotra'.
        L_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( V_SENDER ).
*
*        L_SENDER = CL_SAPUSER_BCS=>CREATE( sy-uname ).
        CALL METHOD L_SEND_REQUEST->SET_SENDER
          EXPORTING
            I_SENDER = L_SENDER.

*-- To frame the attachments for the mail
        PERFORM FRAME_ATTACHMENTS.
**-- Add the recipients to the Send mail
*      LOOP AT s_mailid INTO wl_mailid.

        L_RCV_EMAIL = WL_MAILID-RECEIVER.

        CHECK NOT L_RCV_EMAIL IS INITIAL.
        L_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
                                                      L_RCV_EMAIL ).
        CALL METHOD L_SEND_REQUEST->ADD_RECIPIENT
          EXPORTING
            I_RECIPIENT = L_RECIPIENT
            I_EXPRESS   = 'X'.

*      *-- Send Email
        CALL METHOD L_SEND_REQUEST->SEND(
            EXPORTING
              I_WITH_ERROR_SCREEN = 'X'
            RECEIVING
              RESULT              = L_RESULT ).

*      IF l_result = 'X'.
*        MESSAGE s999(zz) WITH
*        'Mail Sent Successfully'(003).
*      ENDIF.

      ENDLOOP.

    CATCH CX_BCS INTO L_BCS_EXCEPTION.
      IF L_RESULT NE 'X'.
        MESSAGE S999(ZZ) WITH
        'Mail Not Successful'(004).
      ENDIF.

**-- Send Email
*      CALL METHOD l_send_request->send(
*          EXPORTING
*            i_with_error_screen = 'X'
*          RECEIVING
*            result              = l_result ).
*
*      IF l_result = 'X'.
*        MESSAGE s999(zz) WITH
*        'Mail Sent Successfully'(003).
*      ENDIF.
*
*    CATCH cx_bcs INTO l_bcs_exception.
*      IF l_result NE 'X'.
*        MESSAGE s999(zz) WITH
*        'Mail Not Successful'(004).
*      ENDIF.
  ENDTRY.
  COMMIT WORK.                                             "Commit Work

ENDFORM.                    " send_mail
*&---------------------------------------------------------------------*
*& Form Name     : frame_attachments                                   *
*&---------------------------------------------------------------------*
*& Description   : To frame the attachments for the mail to be sent    *
*&---------------------------------------------------------------------*
*& Parameters    : None                                                *
*&---------------------------------------------------------------------*
FORM FRAME_ATTACHMENTS.
*-- Local Data declaration
  DATA: L_SUBJECT   TYPE SO_OBJ_DES,
        L_ATT_TYPE  TYPE SOODK-OBJTP.

*-- Subject of the Attachment
  L_SUBJECT  = W_FILE.
*-- Format of the Attachment
  L_ATT_TYPE = W_EXTN.

  IF T_MAILHEX[] IS NOT INITIAL.
    TRY.
*-- Add Attachment to the Document
        CALL METHOD W_DOCUMENT->ADD_ATTACHMENT
          EXPORTING
            I_ATTACHMENT_TYPE    = L_ATT_TYPE
            I_ATTACHMENT_SUBJECT = L_SUBJECT
            I_ATT_CONTENT_HEX    = T_MAILHEX.

      CATCH CX_DOCUMENT_BCS.
    ENDTRY.
  ENDIF.
ENDFORM.                    " frame_attachments
*&---------------------------------------------------------------------*
*&      Form  GET_RECEIVERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_RECEIVERS .

DATA: BEGIN OF WA_PA0000,
      PERNR TYPE PA0000-PERNR,
      END OF WA_PA0000,
      IT_PA0000 LIKE TABLE OF WA_PA0000.

*   SELECT PERNR FROM PA0000 INTO TABLE IT_PA0000 "WHERE STAT2 <> '3'.
*                WHERE MASSN NE 'I7'.
***                  AND BEGDA > SY-DATUM.


  SELECT PERNR FROM PA0000 INTO TABLE IT_PA0000 "WHERE STAT2 <> '3'.
                WHERE pernr in ( SELECT pernr FROM pa0000 WHERE massn =  'I7').


   SELECT A~PERNR A~GBDAT B~USRID_LONG A~VORNA A~NACHN A~ANRED D~ATEXT
     FROM PA0002 AS A
     JOIN PA0105 AS B
     ON A~PERNR = B~PERNR
     JOIN PA0000 AS C
     ON A~PERNR = C~PERNR
     JOIN T522T AS D
     ON A~ANRED = D~ANRED
     INTO TABLE IT_PERNR
     WHERE C~STAT2 = '3' "C~MASSN <> 'I7'
     AND D~SPRSL = 'EN'
     AND B~USRTY = '0010' and A~PERNR <> '4001'
     AND b~endda >= sy-datum.
*     and b~aedtm in ( SELECT max( y~aedtm )
*     FROM PA0002 AS x
*     JOIN PA0105 AS y
*     ON x~PERNR = y~PERNR
*     JOIN PA0000 AS z
*     ON x~PERNR = z~PERNR
*     JOIN T522T AS m
*     ON x~ANRED = m~ANRED
*     WHERE z~MASSN <> 'I7'
*     AND m~SPRSL = 'EN'
*     AND y~USRTY = '0010').


  DELETE IT_PERNR WHERE USRID_LONG IS INITIAL.
  SORT IT_PERNR BY PERNR GBDAT USRID_LONG.
  DELETE ADJACENT DUPLICATES FROM IT_PERNR.

  DELETE ADJACENT DUPLICATES FROM IT_PERNR.

*  DATA: CHKDATE TYPE SY-DATUM VALUE '19950809'.


  LOOP AT IT_PERNR INTO WA_PERNR.

    READ TABLE IT_PA0000 INTO WA_PA0000 WITH KEY PERNR = WA_PERNR-PERNR.
    IF SY-SUBRC = 0.
      CLEAR WA_PERNR.
    ENDIF.

    IF WA_PERNR-GBDAT+4(04) = SY-DATUM+4(04)."CHKDATE+4(04)."

      WL_MAILID-RECEIVER = WA_PERNR-USRID_LONG.
      condense WL_MAILID-RECEIVER.
      APPEND WL_MAILID TO S_MAILID.
      CLEAR WL_MAILID.
*      G_RECEIPIENTS-RECEXTNAM = WA_PERNR-USRID_LONG.
*      G_RECEIPIENTS-RECESC = 'U'.
*      G_RECEIPIENTS-SNDEX = 'X'.
*      APPEND G_RECEIPIENTS.
**        CLEAR WA_PERNR.
**        DELETE IT_PERNR WHERE PERNR = 0000000.
***        MODIFY IT_PERNR FROM WA_PERNR.

    ENDIF.
  ENDLOOP.

*clear S_MAILID.
**
*
*  WL_MAILID-RECEIVER = 'pshinde-icc@modi.com'."'sshah-icc@modi.com'.
*  APPEND WL_MAILID TO S_MAILID.
*  CLEAR WL_MAILID.
*
*  WL_MAILID-RECEIVER = 'vmenon-icc@modi.com'.
*  APPEND WL_MAILID TO S_MAILID.
*  CLEAR WL_MAILID.
*
*  WL_MAILID-RECEIVER = 'pjoshi-icc@modi.com'.
*  APPEND WL_MAILID TO S_MAILID.
*  CLEAR WL_MAILID.

*  WL_MAILID-RECEIVER = 'spalkar-icc@modi.com'.
*  APPEND WL_MAILID TO S_MAILID.
*  CLEAR WL_MAILID.
*
*  WL_MAILID-RECEIVER = 'kvarma-icc@modi.com'.
*  APPEND WL_MAILID TO S_MAILID.
*  CLEAR WL_MAILID.

*  WL_MAILID-RECEIVER = 'hohelpdesk-icc@modi.com'."'sshah-icc@modi.com'.
*  APPEND WL_MAILID TO S_MAILID.
*  CLEAR WL_MAILID.


ENDFORM.                    " GET_RECEIVERS
*&---------------------------------------------------------------------*
*&      Form  MAIL_BODY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAIL_BODY .

*-- Local data declaration to hold the textpool
  DATA: TL_TEXTPOOL TYPE STANDARD TABLE OF TEXTPOOL,
        WL_TEXTPOOL TYPE TEXTPOOL.


  CLEAR: TL_TEXTPOOL, WL_TEXTPOOL , T_CONTENTS , WA_CONTENTS.


*-- Read the Entire Textpool into an Internal table
  READ TEXTPOOL SY-REPID INTO TL_TEXTPOOL LANGUAGE SY-LANGU.
  IF SY-SUBRC IS INITIAL.
    SORT TL_TEXTPOOL BY ID KEY.
  ENDIF.

*-- Font start
  CLEAR WA_CONTENTS.
  WA_CONTENTS-LINE = C_STYLE_START.
  APPEND WA_CONTENTS TO T_CONTENTS.

*-- New line
  CLEAR WA_CONTENTS.

  C_NEW_LINE = 'Dear  '.

  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.

*-- Program name : Email Attachment
  CLEAR: WL_TEXTPOOL, WA_CONTENTS.
  READ TABLE TL_TEXTPOOL INTO WL_TEXTPOOL
                         WITH KEY ID = 'I' KEY = 'T03'
                         BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    WA_CONTENTS-LINE = WL_TEXTPOOL-ENTRY.
*-- "#" Present in the Text Element will be replaced by the below value
    REPLACE: '#' WITH 'Email Attachment' INTO WA_CONTENTS-LINE.
    APPEND WA_CONTENTS TO T_CONTENTS.
  ENDIF.

*-- New line
  CLEAR:  WA_CONTENTS , C_NEW_LINE.

  READ TABLE IT_PERNR INTO WA_PERNR WITH KEY USRID_LONG = WL_MAILID-RECEIVER.
  IF SY-SUBRC <> 0. CLEAR WA_PERNR. ENDIF.
  CONCATENATE C_NEW_LINE WA_PERNR-ATEXT WA_PERNR-VORNA  WA_PERNR-NACHN ',' INTO C_NEW_LINE SEPARATED BY SPACE.
*  ENDIF.

  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.
*-- New line

  CLEAR: WA_CONTENTS , C_NEW_LINE.
  C_NEW_LINE = '<br>'.

  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.

*-- You can also change or add the text here...
  CLEAR: WL_TEXTPOOL, WA_CONTENTS.
  READ TABLE TL_TEXTPOOL INTO WL_TEXTPOOL
                         WITH KEY ID = 'I' KEY = 'T04'
                         BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    WA_CONTENTS-LINE = WL_TEXTPOOL-ENTRY.
    APPEND WA_CONTENTS TO T_CONTENTS.
  ENDIF.

*-- New line
  CLEAR: WA_CONTENTS , C_NEW_LINE.
  C_NEW_LINE = '<br>'.
  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.
*-- New line
  CLEAR WA_CONTENTS.
  WA_CONTENTS-LINE = C_NEW_LINE.
  C_NEW_LINE = '<br>'.

  APPEND WA_CONTENTS TO T_CONTENTS.

*-- For giving spaces between texts, you can use...
  CLEAR: WL_TEXTPOOL, WA_CONTENTS.
  READ TABLE TL_TEXTPOOL INTO WL_TEXTPOOL
                         WITH KEY ID = 'I' KEY = 'T05'
                         BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    WA_CONTENTS-LINE = WL_TEXTPOOL-ENTRY.
    APPEND WA_CONTENTS TO T_CONTENTS.
  ENDIF.

  C_NEW_LINE = '                                        '.

*-- New line
  CLEAR WA_CONTENTS.
  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.

*-- For Ex; Employee Numbers :
  CLEAR: WL_TEXTPOOL, WA_CONTENTS.
  READ TABLE TL_TEXTPOOL INTO WL_TEXTPOOL
                         WITH KEY ID = 'I' KEY = 'T06'
                         BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    WA_CONTENTS-LINE = WL_TEXTPOOL-ENTRY.
*-- How to give Spaces in between texts
    CONCATENATE WA_CONTENTS-LINE C_SPACE C_EMP1 C_SPACE C_EMP2
                                 C_SPACE C_EMP3 INTO WA_CONTENTS-LINE.
    APPEND WA_CONTENTS TO T_CONTENTS.
  ENDIF.
*-- New line
  CLEAR WA_CONTENTS.
  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.
*-- New line
  CLEAR WA_CONTENTS.
  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.

*-- For more Information on HTML..
  CLEAR: WL_TEXTPOOL, WA_CONTENTS.
  READ TABLE TL_TEXTPOOL INTO WL_TEXTPOOL
                         WITH KEY ID = 'I' KEY = 'T07'
                         BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    WA_CONTENTS-LINE = WL_TEXTPOOL-ENTRY.
    APPEND WA_CONTENTS TO T_CONTENTS.
  ENDIF.

*-- New line
  CLEAR WA_CONTENTS.
  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.
*-- New line
  CLEAR WA_CONTENTS.
  WA_CONTENTS-LINE = C_NEW_LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.

*-- Hyperlink
  CLEAR WA_CONTENTS.
  CONCATENATE C_LINK_TEXT C_LINK_START C_LINK_END INTO WA_CONTENTS-LINE.
  APPEND WA_CONTENTS TO T_CONTENTS.


*  C_LINK_START = 'z:\Profile\Poonams\My Documents\My Pictures\birthday-greeting.jpg'."'<A href="www.w3schools.com">',
*   CONCATENATE C_LINK_TEXT C_LINK_START C_LINK_END INTO WA_CONTENTS-LINE.
*  APPEND WA_CONTENTS TO T_CONTENTS.


*-- Subject of the Mail
*  CONCATENATE text-t08 w_mail_subj INTO w_mail_subj.
  W_MAIL_SUBJ = 'HAPPY BIRTHDAY!!' ."'This is TEST MAIL'."

ENDFORM.                    " MAIL_BODY
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOAD_DATA .
*-- Local data declaration
*  DATA: L_FILE  TYPE STRING,
*        L_INDEX TYPE SY-TABIX,
**-- For holding the split file name
*        TL_SPLITFILE TYPE STANDARD TABLE OF RLGRAP-FILENAME,
*        WL_SPLITFILE TYPE RLGRAP-FILENAME.
*
*  L_FILE = '\\172.16.1.15\SAP Upload\IMAGE\HAPPY BIRTHDAY1.jpg'."'z:\Profile\Poonams\My Documents\My Pictures\HAPPY BIRTHDAY.jpg'.
*
***-- Function module to split the Filename and Extension from the Path
*  CALL FUNCTION 'CH_SPLIT_FILENAME'
*    EXPORTING
*      COMPLETE_FILENAME = L_FILE
*    IMPORTING
*      EXTENSION         = W_EXTN
*      NAME              = W_FILE.
*
**-- Split the filename at '.'
*  SPLIT L_FILE AT '.' INTO TABLE TL_SPLITFILE.
*  DESCRIBE TABLE TL_SPLITFILE LINES L_INDEX.
*
**-- In case the filename contains more than one dot
*  IF L_INDEX GT 2.
*    CLEAR: WL_SPLITFILE, W_EXTN.
**-- Get the Extension of the file
*    READ TABLE TL_SPLITFILE INTO WL_SPLITFILE INDEX L_INDEX.
*    W_EXTN = WL_SPLITFILE.
*    DELETE TL_SPLITFILE INDEX L_INDEX.
*    DELETE TL_SPLITFILE INDEX 1.
*    CLEAR WL_SPLITFILE.
**-- Get the Actual filename
*    LOOP AT TL_SPLITFILE INTO WL_SPLITFILE.
*      CONCATENATE '.' WL_SPLITFILE INTO WL_SPLITFILE.
*    ENDLOOP.
*    CONCATENATE W_FILE WL_SPLITFILE INTO W_FILE.
*    W_FILE = 'HAPPY BIRTHDAY'.
*  ENDIF.
*
*  CONDENSE W_EXTN.
*
**-- Upload File
*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      FILENAME                = L_FILE
*      FILETYPE                = 'BIN'
*    TABLES
*      DATA_TAB                = T_MAILHEX
*    EXCEPTIONS
*      FILE_OPEN_ERROR         = 1
*      FILE_READ_ERROR         = 2
*      NO_BATCH                = 3
*      GUI_REFUSE_FILETRANSFER = 4
*      INVALID_TYPE            = 5
*      NO_AUTHORITY            = 6
*      UNKNOWN_ERROR           = 7
*      BAD_DATA_FORMAT         = 8
*      HEADER_NOT_ALLOWED      = 9
*      SEPARATOR_NOT_ALLOWED   = 10
*      HEADER_TOO_LONG         = 11
*      UNKNOWN_DP_ERROR        = 12
*      ACCESS_DENIED           = 13
*      DP_OUT_OF_MEMORY        = 14
*      DISK_FULL               = 15
*      DP_TIMEOUT              = 16
*      OTHERS                  = 17.
*
*  IF SY-SUBRC IS NOT INITIAL.
*    MESSAGE I999(ZZ) WITH 'Error in reading file for upload'(002)
*    W_FILE.
*  ENDIF.

ENDFORM.                    " UPLOAD_DATA
