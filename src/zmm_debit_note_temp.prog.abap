*&---------------------------------------------------------------------*
*& Report  ZMM_DEBIT_NOTE
*&
*&---------------------------------------------------------------------*
*& Created By: Amol Bhagwat
*& Creation Date: 0.07.2017
*&---------------------------------------------------------------------*
REPORT zmm_debit_note_temp.

TABLES: bkpf.
*&---------------------------------------------------------------------*
*& Data Declaration
*&---------------------------------------------------------------------*
DATA: lv_lines TYPE i.
DATA: lf_fm_name TYPE rs38l_fnam,
      wa_para    TYPE ssfctrlop.

TYPES:BEGIN OF ty_bkpf,
        bukrs   TYPE bkpf-bukrs,
        belnr   TYPE bkpf-belnr,
        gjahr   TYPE bkpf-gjahr,
        awkey   TYPE bkpf-awkey,
        i_belnr TYPE rbkp-belnr,
        i_gjahr TYPE rbkp-gjahr,
        flag    TYPE c,
      END OF ty_bkpf.

DATA: it_bkpf TYPE TABLE OF ty_bkpf,
      wa_bkpf TYPE ty_bkpf.

TYPES:BEGIN OF ty_rbkp,
        belnr TYPE rbkp-belnr,
        gjahr TYPE rbkp-gjahr,
        bukrs TYPE rbkp-bukrs,
      END OF ty_rbkp.

DATA: it_rbkp TYPE TABLE OF ty_rbkp,
      wa_rbkp TYPE ty_rbkp.

DATA: v_ebeln TYPE ebeln,
      wa_ekbe TYPE ekbe.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: s_belnr FOR bkpf-belnr OBLIGATORY,
                s_gjahr FOR bkpf-gjahr OBLIGATORY no-EXTENSION no INTERVALS,
                s_bukrs FOR bkpf-bukrs OBLIGATORY no-EXTENSION no INTERVALS.      " Added by Sandeep on 25.06.2019 as said by Saurabh Khare
SELECTION-SCREEN END OF BLOCK b.

*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT bukrs
         belnr
         gjahr
         awkey FROM bkpf
               INTO TABLE it_bkpf
              WHERE belnr IN s_belnr
                AND gjahr IN s_gjahr
                AND BUKRS IN s_bukrs.

  LOOP AT it_bkpf INTO wa_bkpf.

    wa_bkpf-i_belnr = wa_bkpf-awkey(10).
    wa_bkpf-i_gjahr = wa_bkpf-awkey+10(4).
    MODIFY it_bkpf FROM wa_bkpf
                   TRANSPORTING i_belnr i_gjahr.

  ENDLOOP.  "LOOP AT it_bkpf

  SELECT belnr
         gjahr
         bukrs
               FROM rbkp
               INTO TABLE it_rbkp
                FOR ALL ENTRIES IN it_bkpf
              WHERE belnr EQ it_bkpf-i_belnr
                AND gjahr EQ it_bkpf-i_gjahr
                AND bukrs EQ it_bkpf-BUKRS
                AND stblg EQ ''
                AND stjah EQ ''.

  IF it_rbkp[] IS INITIAL.
    MESSAGE text-001 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    LOOP AT it_bkpf INTO wa_bkpf.
      READ TABLE it_rbkp INTO wa_rbkp
                         WITH KEY belnr = wa_bkpf-i_belnr
                                  gjahr = wa_bkpf-i_gjahr
                                  bukrs = wa_bkpf-bukrs.
      IF sy-subrc NE 0.
        wa_bkpf-flag = 'X'.
      ELSE. " Added on Tuesday, November 14, 2017 12:58:21, Exclude credit notes and normal invoices
        CLEAR v_ebeln.
        SELECT SINGLE ebeln FROM rseg INTO v_ebeln WHERE belnr = wa_rbkp-belnr
                                                   AND   gjahr = wa_rbkp-gjahr
                                                   AND   bukrs = wa_rbkp-bukrs.
        IF  v_ebeln IS NOT INITIAL.
          CLEAR wa_ekbe.
          SELECT SINGLE * FROM ekbe INTO wa_ekbe WHERE ebeln = v_ebeln
                                                 AND   belnr = wa_rbkp-belnr
                                                 AND   gjahr = wa_rbkp-gjahr
                                                 AND   shkzg = 'H'. " Debit Note
          IF sy-subrc <> 0.
            wa_bkpf-flag = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
      MODIFY it_bkpf FROM wa_bkpf TRANSPORTING flag.
    ENDLOOP.  "LOOP AT it_bkpf

    DELETE it_bkpf WHERE flag EQ 'X'.

  ENDIF.  "IF it_rbkp[] IS INITIAL.

  DESCRIBE TABLE it_bkpf LINES lv_lines.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZMM_DEBIT_NOTE_GST_SF_TEMP'
*     variant            = ' '
*     direct_call        = ' '
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF it_bkpf IS NOT INITIAL.
    LOOP AT it_bkpf INTO wa_bkpf.

      wa_para-no_open  = 'X'.
      wa_para-no_close = 'X'.

      AT FIRST.
        wa_para-no_open  = ' '. "overwrite values
        wa_para-no_close = 'X'.
      ENDAT.

      AT LAST.
        wa_para-no_open  = 'X'. "overwrite values
        wa_para-no_close = ' '.
      ENDAT.

      IF lv_lines EQ 1.
        wa_para-no_open  = ' '. "overwrite values
        wa_para-no_close = ' '.
      ENDIF.

      CALL FUNCTION lf_fm_name
        EXPORTING
*         ARCHIVE_INDEX      =
*         ARCHIVE_INDEX_TAB  =
*         ARCHIVE_PARAMETERS =
          control_parameters = wa_para
*         MAIL_APPL_OBJ      =
*         MAIL_RECIPIENT     =
*         MAIL_SENDER        =
*         OUTPUT_OPTIONS     =
*         USER_SETTINGS      = 'X'
          gv_belnr           = wa_bkpf-belnr
          gv_gjahr           = wa_bkpf-gjahr
          gv_bukrs           = wa_bkpf-bukrs
*   IMPORTING
*         DOCUMENT_OUTPUT_INFO       =
*         JOB_OUTPUT_INFO    =
*         JOB_OUTPUT_OPTIONS =
*    TABLES
        EXCEPTIONS
          formatting_error   = 1
          internal_error     = 2
          send_error         = 3
          user_canceled      = 4
          OTHERS             = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDLOOP.
  ELSE.
    MESSAGE 'No data found' TYPE 'E'.
  ENDIF.
