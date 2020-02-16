*&---------------------------------------------------------------------*
*&  Include           RM07MLBD_FORM_02                                 *
*&---------------------------------------------------------------------*

* correction Oct. 2008 TW                                  "n1265674
* for active ingredient materials MB5B should not display  "n1265674
* the 141 and 142 movements for the selection valuated     "n1265674
* stock to avoid wrong beginning stock amount.             "n1265674

* correction May  2007 MS                                  "n1049935
* authority check improved, do only execute authority-check"n1049935
* for plants which are selected, not always for all plants "n1049935
* This is a performance improvement as well.               "n1049935

* correction Nov. 2006 TW                                   "n999530
* plant description should appear behind plant number but   "n999530
* nevertheless the plant description should not be vissible "n999530
* for all possible selection combinations the transaction   "n999530
* MB5L could be started for.                                "n999530

* correction Feb. 2006 MM                                   "n921165
* - improve performance processing internal tables          "n921165
*                                                           "n921165
* - improve performance of the access database tables MKPF  "n921165
*   and MSEG using database specific hints for the database "n921165
*   systems :                                               "n921165
*   - DB2 and ORACLE :                                      "n921165
*     - one SELECT command with DBI hints                   "n921165
*   - DB6, Informix, MaxDB, MSSQL :                         "n921165
*     - 3 SELECT commands who could be choosen using 3 new  "n921165
*       related parameters pa_dbstd, pa_dbmat, pa_dbdat     "n921165

* correction Nov. 2005 MM                                   "n890109
* allow the inter active functions 'Specify drill-down'     "n890109
* and 'Choose' from the menu 'Settings -> Summation levels' "n890109

* MB5B improved regarding accessibilty                      "n773673

* Improvements :                       March 2003 MM        "n599218
* - send warning M7 689 when user does not restric the      "n599218
*   database in dialog or print mode                        "n599218
* - error message 'programmfehler' improved                 "n599218

* contains FORM routines without preprocessor commands and  "n547170
* no text elements                                          "n547170

*&---------------------------------------------------------------------*
*---<< S/4HANA >>---*
*&---------------------------------------------------------------------*
* Changed On - Tuesday, October 16, 2018 ::00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - "MANDT" cannot be specified in the ON condition
* Solution   - "MANDT" cannot be specified in the ON condition
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*

FORM STATUS                  USING     EXTAB TYPE SLIS_T_EXTAB.

  DATA : L_S_EXTAB           TYPE      SLIS_EXTAB.

* set the buttons and functions for the interactions        "n890109
* 'Specify drill-down' etc.. depending on the content of    "n890109
* "g_cust_sum_levels"                                       "n890109
                                                            "n890109
  if  g_cust_sum_levels is initial.                         "n890109
*   inactivate these functions in the menue, because these  "n890109
*   functions are not transferred correctly from ALV list   "n890109
*   to the next ALV list                                    "n890109
    MOVE  : '&XPA'           TO  L_S_EXTAB-FCODE. "Expand   "n890109
    APPEND  L_S_EXTAB        TO  EXTAB.                     "n890109
    MOVE  : '&OMP'           TO  L_S_EXTAB-FCODE. "Aggregate"n890109
    APPEND  L_S_EXTAB        TO  EXTAB.                     "n890109
    MOVE  : '&KOM'           TO  L_S_EXTAB-FCODE. "Choose   "n890109
    APPEND  L_S_EXTAB        TO  EXTAB.                     "n890109
*   Define breakdown                                        "n890109
    MOVE  : '&AUF'           TO  L_S_EXTAB-FCODE.           "n890109
    APPEND  L_S_EXTAB        TO  EXTAB.                     "n890109
  endif.                                                    "n890109

  SET PF-STATUS 'STANDARD'   EXCLUDING EXTAB.

ENDFORM.                     "STATUS

*&---------------------------------------------------------------------*
*&      Form  LISTUMFANG
*&---------------------------------------------------------------------*
*       Prüfung gegen Listumfangsparameter auf Selektionsbild
*----------------------------------------------------------------------*

FORM listumfang.

  data : l_category(08)      type c.                        "n599218
  field-symbols : <l_fs>.                                   "n599218
                                                            "n599218
* carry out the check according the list categories in the  "n599218
* case at least one category is not active                  "n599218
  check : not g_cnt_empty_parameter is initial.             "n599218
*                                                           "n599218
* cat. I docs I stock on   I    I stock on I Parameter      "n599218
*      I      I start date I    I end date I                "n599218
* -----+------+------------+----+----------+----------      "n599218
*  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
*  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
*  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
*  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
*  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
*      I      I            I    I          I                "n599218
*  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
*  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
                                                            "n599218
* process table BESTAND                                     "n599218
  loop at bestand.                                          "n599218
    clear                    l_category.                    "n599218
                                                            "n599218
*   determine the category of each entry                    "n599218
    if  bestand-soll      is initial    and                 "n599218
        bestand-haben     is initial    and                 "n599218
        bestand-sollwert  is initial    and                 "n599218
        bestand-habenwert is initial.                       "n599218
*     material without movements                            "n599218
                                                            "n599218
      if  bestand-endmenge  is initial  AND                 "n599218
          bestand-anfmenge  is initial  and                 "n599218
          bestand-anfwert   is initial  and                 "n599218
          bestand-endwert   is initial.                     "n599218
*       material without movements / no stocks              "n599218
        move  'PA_NDZER'     to  l_category.                "n599218
      else.                                                 "n599218
*       material without movements / with stocks            "n599218
        move  'PA_NDSTO'     to  l_category.                "n599218
      endif.                                                "n599218
    else.                                                   "n599218
*     material with movements                               "n599218
*                                                           "n599218
      if      bestand-anfmenge  is initial  and             "n599218
              bestand-anfwert   is initial.                 "n599218
*       stock and value on start date are zero              "n599218
        if   bestand-endmenge  is initial  AND              "n599218
             bestand-endwert   is initial.                  "n599218
*         stock and value on end date are zero, too         "n599218
          move  'PA_WDZER'     to  l_category.              "n599218
        else.                                               "n599218
*         stock and value on end date <> zero               "n599218
          move  'PA_WDZEW'     to  l_category.              "n599218
        endif.                                              "n599218
      else.                                                 "n599218
*       stock and value on start date <> zero               "n599218
        if     bestand-endmenge  is initial  AND            "n599218
               bestand-endwert   is initial.                "n599218
*         stock and value on end date are zero              "n599218
          move  'PA_WDWIZ'   to  l_category.                "n599218
        elseif bestand-endmenge = bestand-anfmenge  and     "n599218
               bestand-anfwert  = bestand-endwert.          "n599218
*         stock and value on end date are equal             "n599218
          move  'PA_WDWEW'   to  l_category.                "n599218
        else.                                               "n599218
*         stock and values on end date are differnt         "n599218
          move  'PA_WDWUW'   to  l_category.                "n599218
        endif.                                              "n599218
      endif.                                                "n599218
    endif.                                                  "n599218
                                                            "n599218
*   evaluate category and corresponding parameter settings  "n599218
    check : not l_category is initial.                      "n599218
    assign (l_category)      to  <l_fs>.                    "n599218
                                                            "n599218
    if  sy-subrc is initial.                                "n599218
      if  <l_fs> is initial.                                "n599218
        delete               bestand.                       "n599218
      endif.                                                "n599218
    endif.                                                  "n599218
  endloop.                                                  "n599218

ENDFORM.                               " LISTUMFANG

*&---------------------------------------------------------------------*
*&      Form  LISTAUSGABE1
*&---------------------------------------------------------------------*

FORM listausgabe1.

  if  g_cust_color = 'X'.              "colorize numeric fields ?
    layout-coltab_fieldname = 'FARBE_PRO_FELD'.
  else.
    layout-info_fieldname   = 'FARBE_PRO_ZEILE'.
  endif.

  layout-f2code = '9PBP'.
  IF NOT bwbst IS INITIAL.
    layout-min_linesize = '92'.
  ENDIF.

  event_exit-ucomm = '&XP1'.
  event_exit-before = 'X'.
  APPEND event_exit.

  if  g_flag_break-b8 = 'X'.                                "n921164
    BREAK-POINT              ID MMIM_REP_MB5B.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  endif.                                                    "n921164

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_INTERFACE_CHECK        = g_flag_i_check       "n599218
            i_callback_program       = repid
            i_callback_pf_status_set = 'STATUS'
            i_callback_user_command  = 'USER_COMMAND'
*           I_STRUCTURE_NAME         =
            is_layout                = layout
            it_fieldcat              = fieldcat[]
*           IT_EXCLUDING             =
            it_special_groups        = gruppen[]
            it_sort                  = sorttab[]
            it_filter                = filttab[]
*           IS_SEL_HIDE              =
            i_default                = 'X'
            i_save                   = 'A'
            is_variant               = variante
            it_events                = events[]
            it_event_exit            = event_exit[]
            IS_PRINT                 = G_S_PRINT
*           I_SCREEN_START_COLUMN    = 0
*           I_SCREEN_START_LINE      = 0
*           I_SCREEN_END_COLUMN      = 0
*           I_SCREEN_END_LINE        = 0
*      IMPORTING
*           e_exit_caused_by_caller  = 'X'
*           es_exit_caused_by_user   = 'X'
       TABLES
            T_OUTTAB                 = G_T_BELEGE1
       EXCEPTIONS
*           program_error            = 1
            OTHERS                   = 2.

* does the ALV return with an error ?
  IF  NOT SY-SUBRC IS INITIAL.         "Fehler vom ALV ?
    MESSAGE ID SY-MSGID TYPE  'S'     NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " LISTAUSGABE1

*----------------------------------------------------------------------*
*    F0000_CREATE_TABLE_G_T_ORGAN
*----------------------------------------------------------------------*

FORM F0000_CREATE_TABLE_G_T_ORGAN
                   USING     L_F_ERROR TYPE STYPE_ORGAN-KEYTYPE.

* at least on of these 3 range tables must be filled for the creation
* of table g_t_organ
* field  description         range table
* -----  -----------         -----------
* werks  plant               g_0000_ra_plant
* bwkey  valuation area      g_0000_ra_bwkey
* bukrs  company code        g_0000_ra_plant

* table g_t_organ must be empty
  CHECK : G_T_ORGAN[] IS INITIAL.                           "n433765

* START OF CHANGE for note 1049935                         "n1049935
* First check if there was a selection for company code    "n1049935
* then do the F9000_AUTH_PLANT_CHECK only for necessary    "n1049935
* selected items..                                         "n1049935

* select all plant from table t001w
  SELECT * FROM T001W        WHERE  WERKS  IN  G_0000_RA_WERKS.

*   check the valuation area
    CHECK : T001W-BWKEY IN G_0000_RA_BWKEY.

*   read the valuation area
    SELECT SINGLE * FROM T001K
                             WHERE  BWKEY  =  T001W-BWKEY.

    CHECK : SY-SUBRC IS INITIAL.       "entry found ?

*   company code is required ?
    CHECK : T001K-BUKRS IN G_0000_RA_BUKRS.

    SELECT SINGLE * FROM T001  WHERE  BUKRS  =  T001K-BUKRS.

    CHECK : SY-SUBRC IS INITIAL.       "entry found ?


    PERFORM  F9000_AUTH_PLANT_CHECK    USING  T001W-WERKS.

*   go on if the user has authority for this plant
    CHECK : G_FLAG_AUTHORITY = 'X'.
* END OF CHANGE for note 1049935                           "n1049935

*   create table g_t_organ_lean
    MOVE : T001W-WERKS       TO  G_S_ORGAN-WERKS,
           T001W-BWKEY       TO  G_S_ORGAN-BWKEY,
           T001-BUKRS        TO  G_S_ORGAN-BUKRS,
           T001-WAERS        TO  G_S_ORGAN-WAERS.

*   write 2 entries for the both search methods _
*   1. with key   valuation area
*   2. with key   plant
    MOVE-CORRESPONDING  G_S_ORGAN
                             TO  G_T_ORGAN.
    MOVE : C_BWKEY           TO  G_T_ORGAN-KEYTYPE,
           G_S_ORGAN-BWKEY   TO  G_T_ORGAN-KEYFIELD.
    APPEND                   G_T_ORGAN.

    MOVE : C_WERKS           TO  G_T_ORGAN-KEYTYPE,
         G_S_ORGAN-WERKS     TO  G_T_ORGAN-KEYFIELD.
    APPEND                   G_T_ORGAN.
    CLEAR                    G_S_ORGAN.

*   create the range tables for plants
    MOVE : T001W-WERKS       TO  G_RA_WERKS-LOW,
           'I'               TO  G_RA_WERKS-SIGN,
           'EQ'              TO  G_RA_WERKS-OPTION.
    APPEND                   G_RA_WERKS.

*   create the range tables for plants and valuation areas
    MOVE : T001K-BWKEY       TO  G_RA_BWKEY-LOW,
           'I'               TO  G_RA_BWKEY-SIGN,
           'EQ'              TO  G_RA_BWKEY-OPTION.
    APPEND                   G_RA_BWKEY.
  ENDSELECT.

* is table g_t_organ empty ?
  IF  G_T_ORGAN[] IS INITIAL.                               "n433765
*   no plants for selection found / process error message ?
    IF  L_F_ERROR = C_ERROR.
      MESSAGE E281.
*     Kein Eintrag zur Selektion Buchungskreis Werk Lagerort vorhanden
    ENDIF.
  ELSE.
    SORT  G_T_ORGAN          BY  KEYTYPE  KEYFIELD  BWKEY  WERKS.
    DELETE ADJACENT DUPLICATES FROM G_T_ORGAN.

    SORT                     G_RA_WERKS.
    DELETE ADJACENT DUPLICATES FROM G_RA_WERKS.

    SORT                     G_RA_BWKEY.
    DELETE ADJACENT DUPLICATES FROM G_RA_BWKEY.
  ENDIF.

ENDFORM.                     "f0000_create_table_g_t_organ

*----------------------------------------------------------------------*
*    F0300_GET_FIELDS
*----------------------------------------------------------------------*

FORM F0300_GET_FIELDS.

  data : l_f_type(01)        type c.

* find out the fields of structure g_s_mseg_lean
  DESCRIBE FIELD G_S_MSEG_LEAN INTO G_T_TD.

  LOOP AT G_T_TD-NAMES       INTO  G_S_NAMEINFO.
*   select all entries who contain 'MSEG-' oder 'MKPF-'
    CASE  G_S_NAMEINFO-NAME(05).
      WHEN  'MKPF-'.
        MOVE    C_TILDE      TO  G_S_NAMEINFO-NAME+4(01).
        APPEND  G_S_NAMEINFO-NAME
                             TO  G_T_MSEG_FIELDS.

      WHEN  'MSEG-'.
        move  : G_S_NAMEINFO-NAME
                             to  g_s_mseg_fields-fieldname,
                C_TILDE      TO  G_S_mseg_fields-fieldname+4(01).
        APPEND  G_S_mseg_fields        TO  G_T_MSEG_FIELDS.

      WHEN  OTHERS.
    ENDCASE.
  ENDLOOP.

* serious error if table g_t_mseg_field does not contain fields
  IF  G_T_MSEG_FIELDS[] IS INITIAL.                         "n599218
    MESSAGE E895                                  "#EC *    "n599218
      WITH 'Error, contact system administrator'. "#EC *    "n599218
  ELSE.
    SORT                     G_T_MSEG_FIELDS.
    DELETE ADJACENT DUPLICATES FROM  G_T_MSEG_FIELDS.
  ENDIF.

ENDFORM.                     "f0300_get_fields.

*----------------------------------------------------------------------*
*    F0500_APPEND_RA_SOBKZ
*----------------------------------------------------------------------*

FORM F0500_APPEND_RA_SOBKZ
                   USING     L_F_SOBKZ LIKE      MSEG-SOBKZ.

* create ranges table with special stock indicator
  CLEAR                      G_RA_SOBKZ.
  MOVE : L_F_SOBKZ           TO  G_RA_SOBKZ-LOW,
         'I'                 TO  G_RA_SOBKZ-SIGN,
         'EQ'                TO  G_RA_SOBKZ-OPTION.
  APPEND                     G_RA_SOBKZ.

ENDFORM.                     "f0500_append_ra_sobkz

*----------------------------------------------------------------------*
*    F0600_CREATE_RANGE_LGORT
*----------------------------------------------------------------------*

FORM F0600_CREATE_RANGE_LGORT.

  REFRESH                    G_RA_LGORT.
  CLEAR                      G_RA_LGORT.

  IF      LGBST = 'X'.       "only Storage loc./batch stock
*   copy the existing select-options
    MOVE LGORT[]               TO  G_RA_LGORT[].

*   add an exclusion for storage location = space
    MOVE : 'E'               TO  G_RA_LGORT-SIGN,
           'EQ'              TO  G_RA_LGORT-OPTION.
    APPEND                   G_RA_LGORT.

  ELSEIF  BWBST = 'X'.       "only valuated stocks
*   copy the existing select-options
    MOVE LGORT[]               TO  G_RA_LGORT[].

  ELSEIF  SBBST = 'X'.       "only special stocks
    IF      SOBKZ  =  'O'  OR
            SOBKZ  =  'V'  OR
            SOBKZ  =  'W'.
*     only records with storage location = space allowed
      MOVE : 'I'             TO  G_RA_LGORT-SIGN,
             'EQ'            TO  G_RA_LGORT-OPTION.
      APPEND                 G_RA_LGORT.
    ELSE.
*     Copy the existing select-options
      MOVE LGORT[]             TO  G_RA_LGORT[].
    ENDIF.
  ENDIF.

ENDFORM.                     "f0600_create_range_lgort

*-----------------------------------------------------------"n547170
*    f0700_prepare_tied_empties.                            "n547170
*-----------------------------------------------------------"n547170
*
* this flag will be set after the found MM doc items
* contain at least one of these values in indicator
* MSEG-XAUTO
*                                                           "n547170
* 2.1 stock type = storage location / batch stock           "n547170
*     use : 'F', 'L', 'M', and 'W'                          "n547170
*                                                           "n547170
* 2.2 stock type = valuated stock                           "n547170
*     use : 'F', 'L', 'M', 'W', 'S', and 'U'                "n547170
*                                                           "n547170
*-----------------------------------------------------------"n547170
                                                            "n547170
form f0700_prepare_tied_empties.                            "n547170
                                                            "n547170
  move : 'F'                 to  g_ra_xauto-low,            "n547170
         space               to  g_ra_xauto-high,           "n547170
         'EQ'                to  g_ra_xauto-option,         "n547170
         'I'                 to  g_ra_xauto-sign.           "n547170
  append                     g_ra_xauto.                    "n547170
                                                            "n547170
  move : 'L'                 to  g_ra_xauto-low.            "n547170
  append                     g_ra_xauto.                    "n547170
                                                            "n547170
  move : 'M'                 to  g_ra_xauto-low.            "n547170
  append                     g_ra_xauto.                    "n547170
                                                            "n547170
  move : 'W'                 to  g_ra_xauto-low.            "n547170
  append                     g_ra_xauto.                    "n547170
                                                            "n547170
  if  not BWBST is initial.                                 "n547170
*   plus these for stock type = valuated stock              "n547170
    move : 'S'               to  g_ra_xauto-low.            "n547170
    append                   g_ra_xauto.                    "n547170
                                                            "n547170
    move : 'U'               to  g_ra_xauto-low.            "n547170
    append                   g_ra_xauto.                    "n547170
  endif.                                                    "n547170
                                                            "n547170
endform.                     "f0700_prepare_tied_empties.   "n547170
                                                            "n547170
*-----------------------------------------------------------"n547170
*    f0800_check_restrictions                               "n547170
*-----------------------------------------------------------"n547170
* check whether FI summarization is active and other        "n547170
* restrictions could deliver wrong results                  "n547170
*-----------------------------------------------------------"n547170
                                                            "n547170
form f0800_check_restrictions.                              "n547170
                                                            "n547170
  data : l_flag_m7390(01)    type c.

* - if FI summarization is active process warning M7 390    "n497992
*   for stock type = valuated stock                         "n497992
  if  not bwbst is initial.                                 "n497992

    BREAK-POINT                ID MMIM_REP_MB5B.           "n921164
*   dynamic break-point : customizing for FI summarization "n921164
                                                            "n497992
*   reference procedures for checking FI summarization :    "n497992
*   MKPF, RMRP, MLHD, PRCHG                                 "n497992
    refresh                  g_ra_awtyp.                    "n547170
                                                            "n547170
    move : 'EQ'              to  g_ra_awtyp-option,         "n497992
           'I'               to  g_ra_awtyp-sign,           "n497992
           'MKPF'            to  g_ra_awtyp-low.            "n497992
    append                   g_ra_awtyp.                    "n497992
                                                            "n497992
    move   'RMRP'            to  g_ra_awtyp-low.            "n497992
    append                   g_ra_awtyp.                    "n497992
                                                            "n497992
    move   'MLHD'            to  g_ra_awtyp-low.            "n497992
    append                   g_ra_awtyp.                    "n497992
                                                            "n497992
    move   'PRCHG'           to  g_ra_awtyp-low.            "n497992
    append                   g_ra_awtyp.                    "n497992
                                                            "n497992
    SELECT * FROM ttypv                                     "n497992
      WHERE awtyp  in g_ra_awtyp.                           "n497992
                                                            "n1278202
      if ttypv-awtyp = 'MKPF'.                              "n1278202
*       any entry from AWTYP = MKPF could lead to wrong     "n1278202
*       results -> send message                             "n1278202
        move  'X'         to  l_flag_m7390.                 "n1278202
        exit.                                               "n1278202
      endif.                                                "n1278202
                                                            "n497992
      IF ttypv-fieldname = '*'      OR                      "n497992
         ttypv-fieldname = 'MATNR'.                         "n497992
*       avoid error reported by the code inspector : to
*       emerge this message during this SELECT - ENDSELECT
*       loop will create a problem for the database cursor
        move  'X'         to  l_flag_m7390.
        exit.                                               "n497992
      ENDIF.                                                "n497992
    ENDSELECT.                                              "n497992

*   emerge message after this SELECT - ENDSELECT loop if
*   an error was detected
    if  l_flag_m7390 = 'X'.
*       emerge warning ?                                    "n497992
        CALL FUNCTION 'ME_CHECK_T160M'                      "n497992
          EXPORTING                                         "n497992
            I_ARBGB          = 'M7'                         "n497992
            I_MSGNR          = '390'                        "n497992
          EXCEPTIONS                                        "n497992
            NOTHING          = 0                            "n497992
            OTHERS           = 1.                           "n497992
                                                            "n497992
        IF SY-SUBRC <> 0.                                   "n497992
*         FI summarization active / results could be wrong  "n497992
          message            w390.                          "n497992
        ENDIF.                                              "n497992
    endif.
  endif.                                                    "n497992
                                                            "n497992
* - the user wants to restrict the movement type : process  "n497992
*   warning M7 391                                          "n497992
  if not bwart[] is initial.                                "n497992
*   emerge warning ?                                        "n497992
    CALL FUNCTION            'ME_CHECK_T160M'               "n497992
          EXPORTING                                         "n497992
            I_ARBGB          = 'M7'                         "n497992
            I_MSGNR          = '391'                        "n497992
          EXCEPTIONS                                        "n497992
            NOTHING          = 0                            "n497992
            OTHERS           = 1.                           "n497992
                                                            "n497992
    IF SY-SUBRC <> 0.                                       "n497992
      set cursor             field  'BWART_LOW'.            "n497992
*     to restric the mov.type could cause wrong results     "n497992
      message                w391.                          "n497992
    endif.                                                  "n497992
  endif.                                                    "n497992

* - send warning M7 689 when user does not restric the      "n599218
*   database in dialog or print mode                        "n599218
  IF  SY-UCOMM  =  'ONLI'     OR                            "n599218
      SY-UCOMM  =  'PRIN'.                                  "n599218
*   only in dialog or online-print mode                     "n599218
    IF  MATNR[] IS INITIAL AND                              "n599218
        BUKRS[] IS INITIAL AND                              "n599218
        WERKS[] IS INITIAL AND                              "n599218
        LGORT[] IS INITIAL AND                              "n599218
        CHARG[] IS INITIAL AND                              "n599218
        BWTAR[] IS INITIAL.                                 "n599218
      MESSAGE  W689.         "Selection was not restricted  "n599218
    ENDIF.                                                  "n599218
  ENDIF.                                                    "n599218

* check the indicators for the scope of list categories     "n599218
  perform                    f0850_empty_parameters.        "n599218
                                                            "n599218
  case  g_cnt_empty_parameter.  "evaluate the result        "n599218
    when  0.                                                "n599218
*    all parameters are set -> take all entries             "n599218
    when  7.                                                "n599218
      case  g_flag_status_liu.                              "n599218
        when  c_hide.                                       "n599218
          move  'S'          to  g_flag_status_liu.         "n599218
          set  cursor        field 'PB_LIU'.                "n599218
        when  c_show.                                       "n599218
          set  cursor        field  'PA_WDZER'.             "n599218
      endcase.                                              "n599218
                                                            "n599218
*     Please choose at least one scope of list              "n599218
      message                 e829.                         "n599218
                                                            "n599218
    when  others.                                           "n599218
*      process selection for scope of list                  "n599218
  endcase.                                                  "n599218

endform.                     "f0800_check_restrictions      "n547170
                                                            "n547170
*-----------------------------------------------------------"n547170
*    f0850_empty_parameters                                 "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
form f0850_empty_parameters.                                "n599218
                                                            "n599218
* check the indicators for the scope of list categories     "n599218
* how many parameters are empty ?                           "n599218
  clear                      g_cnt_empty_parameter.         "n599218
                                                            "n599218
  define macro_check_parameter.                             "n599218
    if  &1 is initial.                                      "n599218
      add  1                 to  g_cnt_empty_parameter.     "n599218
    endif.                                                  "n599218
  end-of-definition.                                        "n599218
                                                            "n599218
  macro_check_parameter      pa_wdzer.                      "n599218
  macro_check_parameter      pa_wdzew.                      "n599218
  macro_check_parameter      pa_wdwiz.                      "n599218
  macro_check_parameter      pa_wdwuw.                      "n599218
  macro_check_parameter      pa_wdwew.                      "n599218
  macro_check_parameter      pa_ndzer.                      "n599218
  macro_check_parameter      pa_ndsto.                      "n599218
                                                            "n599218
endform.                     "f0850_empty_parameters        "n599218
                                                            "n599218
*-----------------------------------------------------------"n547170
*    F1000_SELECT_MSEG_MKPF
*----------------------------------------------------------------------*

FORM F1000_SELECT_MSEG_MKPF.

* selection of material docs in database tables MKPF and MSEG

*--- begin of note 921165 ----------------------------------"n921165
* improve performance of the access database tables MKPF    "n921165
* and MSEG using database specific hints                    "n921165
                                                            "n921165
  BREAK-POINT                ID MMIM_REP_MB5B.              "n921164
* dynamic break-point : before the main SELECT command      "n921164
                                                            "n921165
* what kind of database access does the user choose ?
  if      pa_dbstd = 'X'.
*   standard access, the database optimizer looks for the
*   access path
    SELECT (G_T_MSEG_FIELDS)
         INTO CORRESPONDING FIELDS OF TABLE G_T_MSEG_LEAN
         FROM MKPF AS MKPF  JOIN MSEG AS MSEG
**                  ON MKPF~MANDT  =  MSEG~MANDT  AND
                  ON MKPF~MBLNR  =  MSEG~MBLNR  AND
                     MKPF~MJAHR  =  MSEG~MJAHR
         WHERE MSEG~MATNR  IN  MATNR
           AND MSEG~WERKS  IN  G_RA_WERKS
           AND MSEG~LGORT  IN  G_RA_LGORT
           AND MSEG~CHARG  IN  CHARG
           AND MSEG~BWTAR  IN  BWTAR
           AND MSEG~BWART  IN  BWART
           AND MKPF~BUDAT  GE  DATUM-LOW
           AND MSEG~SOBKZ  IN  G_RA_SOBKZ.
*    %_HINTS
*    DB2    '&SUBSTITUTE VALUES&'
*    ORACLE '&SUBSTITUTE VALUES&'.

  elseif  pa_dbmat = 'X'.
*   database access via material number and MSEG MM doc items
    SELECT (G_T_MSEG_FIELDS)
         INTO CORRESPONDING FIELDS OF TABLE G_T_MSEG_LEAN
         FROM MSEG AS MSEG  JOIN MKPF AS MKPF
**                  ON MKPF~MANDT  =  MSEG~MANDT  AND
                  ON MKPF~MBLNR  =  MSEG~MBLNR  AND
                     MKPF~MJAHR  =  MSEG~MJAHR
         WHERE MSEG~MATNR  IN  MATNR
           AND MSEG~WERKS  IN  G_RA_WERKS
           AND MSEG~LGORT  IN  G_RA_LGORT
           AND MSEG~CHARG  IN  CHARG
           AND MSEG~BWTAR  IN  BWTAR
           AND MSEG~BWART  IN  BWART
           AND MKPF~BUDAT  GE  DATUM-LOW
           AND MSEG~SOBKZ  IN  G_RA_SOBKZ.
*    %_HINTS
*    ADABAS   'ORDERED'
*    INFORMIX 'ORDERED'
*    MSSQLNT  'OPTION FORCE ORDER'                           "921165
*    DB6 '<NLJOIN><IXSCAN TABLE=''MSEG'' SAP_INDEX=''M''/>'
*    DB6 '<IXSCAN TABLE=''MKPF'' SAP_INDEX=''0''/></NLJOIN>'.

  elseif  pa_dbdat = 'X'.
*   database access via posting date and MKPF MM doc headers
    SELECT (G_T_MSEG_FIELDS)
         INTO CORRESPONDING FIELDS OF TABLE G_T_MSEG_LEAN
         FROM MKPF AS MKPF  JOIN MSEG AS MSEG
**                  ON MKPF~MANDT  =  MSEG~MANDT  AND
                  ON MKPF~MBLNR  =  MSEG~MBLNR  AND
                     MKPF~MJAHR  =  MSEG~MJAHR
         WHERE MSEG~MATNR  IN  MATNR
           AND MSEG~WERKS  IN  G_RA_WERKS
           AND MSEG~LGORT  IN  G_RA_LGORT
           AND MSEG~CHARG  IN  CHARG
           AND MSEG~BWTAR  IN  BWTAR
           AND MSEG~BWART  IN  BWART
           AND MKPF~BUDAT  GE  DATUM-LOW
           AND MSEG~SOBKZ  IN  G_RA_SOBKZ.
*    %_HINTS
*    INFORMIX 'ORDERED'
*    ADABAS 'ORDERED'
*    MSSQLNT  'OPTION FORCE ORDER'                           "n921165
*    DB6 '<NLJOIN><IXSCAN TABLE=''MKPF'' SAP_INDEX=''BUD''/>'
*    DB6 '<IXSCAN TABLE=''MSEG'' SAP_INDEX=''0''/></NLJOIN>'.
  else.
*   not allowed
    move  1                  to  sy-subrc.                  "n921165
  endif.                                                    "n921165
*--- end of note 921165 ------------------------------------"n921165

  IF  SY-SUBRC  <>  0.
*   no material documents found
    MESSAGE                  S842.
  ENDIF.

  data: lt_imchb_tmp like imchb occurs 0,                      "838360
        ls_imchb like imchb.                                   "838360

* check whether the found MM doc items contain retail and
* and beverage specific values
* check authority in this loop - endloop
  loop at g_t_mseg_lean      assigning <G_FS_MSEG_LEAN>.
*   a) check authorisation
    PERFORM        F9000_AUTH_PLANT_CHECK
                             USING  <G_FS_MSEG_LEAN>-WERKS.

    IF  G_FLAG_AUTHORITY IS INITIAL.
      DELETE                G_T_MSEG_LEAN.
      CONTINUE.             "take the next entry
    ENDIF.

*   b) look for special indicators
    if  not <G_FS_MSEG_LEAN>-xauto is initial.
      if  <G_FS_MSEG_LEAN>-xauto in g_ra_xauto.
        move  'X'            to  g_cust_tied_empties.
      endif.
    endif.
*   838360_v
*   according to note 62272 MCHB may be archived even if the batches
*   are still in use. Therefore we try to find such batches by
*   collecting the batches used in MSEG.
    IF not XNOMCHB IS INITIAL and
       not <G_FS_MSEG_LEAN>-charg is initial.
      ls_imchb-matnr = <G_FS_MSEG_LEAN>-matnr.
      ls_imchb-werks = <G_FS_MSEG_LEAN>-werks.
      ls_imchb-lgort = <G_FS_MSEG_LEAN>-lgort.
      ls_imchb-charg = <G_FS_MSEG_LEAN>-charg.
      append ls_imchb to lt_imchb_tmp.
    endif.
*   838360_^
* for active ingredient materials MB5B should not display  "n1265674
* the 141 and 142 movements for the selection valuated     "n1265674
* stock to avoid wrong beginning stock amount.             "n1265674
   IF BWBST = 'X' AND <G_FS_MSEG_LEAN>-BUSTW = 'MB08'.     "n1265674
      DELETE                 G_T_MSEG_LEAN.                "n1265674
      CONTINUE.              "take the next entry          "n1265674
   ENDIF.                                                  "n1265674
  endloop.

* 838360_v
  if not lt_imchb_tmp[] is initial.
*   the following lines merge the batches found in MCHB with the ones
*   found in the material documents.
    sort lt_imchb_tmp descending by werks matnr lgort charg.
    delete adjacent duplicates from lt_imchb_tmp
                               comparing werks matnr lgort charg.
    append lines of lt_imchb_tmp to imchb.
*   due to the merge of mchb batches and mseg batches there will be
*   duplicate entries in imchb by now. They will be deleted. The
*   following sort makes sure that only lines without a quantity are
*   deleted by the 'delete adjacent duplicates' command.
    sort imchb descending by WERKS MATNR LGORT CHARG
                             CLABS CUMLM CINSM CEINM CSPEM CRETM.
    delete adjacent duplicates from imchb
                               comparing matnr werks lgort charg.
    free lt_imchb_tmp.
  endif.
* 838360_^

* function for tied empties is active and                   "n547170
* stock type = storage location/batch ?                     "n547170
  if  not g_cust_tied_empties is initial   and              "n547170
      not lgbst               is initial.                   "n547170
*   sort the results by documents numbers und items         "n547170
    sort  g_t_mseg_lean      by  mblnr mjahr zeile.         "n547170
  endif.                                                    "n547170

* process table withe the results form the database selection
  LOOP AT G_T_MSEG_LEAN      INTO  G_S_MSEG_LEAN.
    PERFORM                  F1100_CHECK_LGORT_SOKZG.

    IF  G_FLAG_DELETE = 'X'.
      DELETE                 G_T_MSEG_LEAN.
      CONTINUE.              "take the next entry
    ENDIF.

*   function for tied empties is active and                 "n547170
*   stock type = storage location/batch ?                   "n547170
    if  not g_cust_tied_empties is initial   and            "n547170
        not lgbst               is initial.                 "n547170
                                                            "n547170
*     check whether this line was generated automatically   "n547170
      if  g_s_mseg_lean-xauto = 'X'.                        "n547170
*       look for the origin line                            "n547170
        compute  g_f_zeile = g_s_mseg_lean-zeile - 1.       "n547170
                                                            "n547170
*       check whether the previous line contains the        "n547170
*       original posting                                    "n547170
        if  g_s_mseg_pr-matnr = g_s_mseg_lean-matnr and     "n547170
            g_s_mseg_pr-mblnr = G_s_mseg_lean-mblnr and     "n547170
            g_s_mseg_pr-mjahr = g_s_mseg_lean-mjahr and     "n547170
            g_s_mseg_pr-zeile = g_f_zeile.                  "n547170
*         the previous entry contains the original line     "n547170
                                                            "n547170
          if  g_s_mseg_pr-xauto in g_ra_xauto.              "n547170
*           the previous line contains a matching value     "n547170
*           XAUTO -> save it in working table               "n547170
            append  g_s_mseg_pr     to  g_t_mseg_or.        "n547170
          endif.                                            "n547170
        else.                                               "n547170
*         the previous entry does not contain the original  "n547170
*         posting : save the key                            "n547170
          g_s_mseg_key-matnr      = g_s_mseg_lean-matnr.    "n547170
          g_s_mseg_key-mblnr      = G_s_mseg_lean-mblnr.    "n547170
          g_s_mseg_key-mjahr      = g_s_mseg_lean-mjahr.    "n547170
          g_s_mseg_key-zeile      = g_f_zeile.              "n547170
          append  g_s_mseg_key    to  g_t_mseg_key.         "n547170
        endif.                                              "n547170
      endif.                                                "n547170
                                                            "n547170
*     save the current entry in the buffer previous entry   "n547170
      move-corresponding g_s_mseg_lean to  g_s_mseg_pr.     "n547170
    endif.                                                  "n547170
  ENDLOOP.

* function for tied empties is active and                   "n547170
* stock type = storage location/batch ?                     "n547170
  if  not g_cust_tied_empties is initial   and              "n547170
      not lgbst               is initial.                   "n547170
                                                            "n547170
*   Select the missing items with the origin posting lines  "n547170
*   and append them into the working table                  "n547170
    if  not g_t_mseg_key[] is initial.                      "n547170
      select mblnr mjahr zeile matnr xauto                  "n547170
         from  mseg                                         "n547170
         appending table g_t_mseg_or                        "n547170
         for all entries in g_t_mseg_key                    "n547170
         where  mblnr = g_t_mseg_key-mblnr                  "n547170
           and  mjahr = g_t_mseg_key-mjahr                  "n547170
           and  zeile = g_t_mseg_key-zeile                  "n547170
           and  xauto in g_ra_xauto.   "only F, L, M, W     "n547170
    endif.                                                  "n547170
                                                            "n547170
    sort  g_t_mseg_or      by  mblnr mjahr zeile matnr.     "n547170
                                                            "n547170
*   process the MM docs in any cases :                      "n547170
*   - delete the lines with the special value for XAUTO     "n547170
*   - check lines who were created automatically whether    "n547170
*     the original line has a special value for XAUTO       "n547170
    loop at g_t_mseg_lean into  g_s_mseg_lean.              "n547170
      clear                G_FLAG_DELETE.                   "n547170
                                                            "n547170
*     evaluate field XAUTO                                  "n547170
      if      g_s_mseg_lean-xauto is initial.               "n547170
*       no action                                           "n547170
      elseif  g_s_mseg_lean-xauto in g_ra_xauto.            "n547170
*       delete MM docs with XAUTO = 'F', 'L', 'M', 'W'      "n547170
        move  'X'            to  g_flag_delete.             "n547170
                                                            "n547170
      elseif  g_s_mseg_lean-xauto = 'X'.                    "n547170
        if  not g_t_mseg_or[] is initial.                   "n547170
*         check field XAUTO of the original MM doc item     "n547170
          compute  g_f_zeile = g_s_mseg_lean-zeile - 1.     "n547170
                                                            "n547170
          read table g_t_mseg_or                            "n547170
                    into  g_s_mseg_or                       "n547170
                    WITH KEY mblnr = G_S_mseg_lean-mblnr    "n547170
                             mjahr = g_s_mseg_lean-mjahr    "n547170
                             zeile = g_f_zeile              "n547170
                             matnr = g_s_mseg_lean-matnr    "n547170
                             BINARY SEARCH.                 "n547170
                                                            "n547170
          if  sy-subrc is initial.                          "n547170
*           the original line was a posting for a tied      "n547170
*           empties material -> delete this entry           "n547170
            move  'X'        to  g_flag_delete.             "n547170
          endif.                                            "n547170
        endif.                                              "n547170
      endif.                                                "n547170
                                                            "n547170
      IF  G_FLAG_DELETE = 'X'.                              "n547170
        delete               G_T_MSEG_LEAN.                 "n547170
      ENDIF.                                                "n547170
    endloop.                                                "n547170
                                                            "n547170
*   release the space of the working tables                 "n547170
    free : g_t_mseg_or, g_t_mseg_key.                       "n547170
  endif.                                                    "n547170

* is the table g_t_mseg_lean empty and no authority problems
  DESCRIBE TABLE G_T_MSEG_LEAN         LINES  G_F_CNT_LINES.

  IF  G_F_CNT_LINES   IS INITIAL  AND
      G_F_CNT_BEFORE  =  G_F_CNT_AFTER.
*   Keinen Eintrag zu den Suchbegriffen gefunden/selektiert
    MESSAGE                  S083.
  ENDIF.

ENDFORM.                     "f1000_select_mseg_mkpf

*----------------------------------------------------------------------*
*    F1100_CHECK_LGORT_SOKZG
*----------------------------------------------------------------------*

FORM F1100_CHECK_LGORT_SOKZG.
  CLEAR                      G_FLAG_DELETE.
*   c) additional checks if valuated stock is required
    IF  BWBST = 'X'.
*     check fields sobkz and kzwbs for valuated stocks
      IF      G_S_MSEG_LEAN-SOBKZ  =  C_SPACE  OR
              G_S_MSEG_LEAN-SOBKZ  =  'O'      OR
              G_S_MSEG_LEAN-SOBKZ  =  'V'      OR
              G_S_MSEG_LEAN-SOBKZ  =  'W'.                  "n435403
*       OK : special stock indicator = ' ', 'O', 'W' or 'V'
      ELSEIF  G_S_MSEG_LEAN-KZBWS = 'A'  OR
              G_S_MSEG_LEAN-KZBWS = 'M'.
*       ok : document with valuated special stock
      ELSE.
        MOVE  'X'            TO  G_FLAG_DELETE.
      ENDIF.
    ELSE.
*   b) check the combination of special stock indicator and
*      storage location
    IF       G_S_MSEG_LEAN-SOBKZ = 'O'  OR
             G_S_MSEG_LEAN-SOBKZ = 'V'  OR
             G_S_MSEG_LEAN-SOBKZ = 'W'.
*     these entries must not have a storage location
      IF  NOT G_S_MSEG_LEAN-LGORT IS INITIAL.
        MOVE  'X'            TO  G_FLAG_DELETE.
      ENDIF.
    ELSE.
*     the others entries should have a storage location
      IF  G_S_MSEG_LEAN-LGORT IS INITIAL.
        MOVE  'X'            TO  G_FLAG_DELETE.
      ENDIF.
    ENDIF.
    ENDIF.

ENDFORM.                     "f1100_check_lgort_sokzg.

*----------------------------------------------------------------------*
*    F2100_MAT_TEXT
*----------------------------------------------------------------------*

FORM F2100_MAT_TEXT
                   USING     L_F_MATNR TYPE STYPE_MAT_KEY-MATNR.

  IF  L_F_MATNR  NE  G_S_MAKT-MATNR.                        "n451923
*   read in table imakt                                     "n451923
    READ TABLE G_T_MAKT      INTO  G_S_MAKT                 "n451923
                             WITH KEY MATNR = L_F_MATNR     "n451923
                             BINARY SEARCH.                 "n451923
                                                            "n451923
    IF  SY-SUBRC <> 0.                                      "n451923
*     record not found                                      "n451923
      CLEAR                  G_S_MAKT-MAKTX.                "n451923
    ENDIF.                                                  "n451923
  ENDIF.                                                    "n451923

ENDFORM.                     "f2100_mat_text

*----------------------------------------------------------------------*
*    F2200_READ_T001
*----------------------------------------------------------------------*

FORM F2200_READ_T001
                   USING     L_F_WERKS LIKE T001W-WERKS.

  statics : begin of l_s_t001w,                             "n999530
              werks          type  t001w-werks,             "n999530
              name1          type  t001w-name1,             "n999530
            end of l_s_t001w.                               "n999530

* read name of this plant after the plant has changed       "n999530
  if  l_f_werks <> l_s_t001w-werks.                         "n999530
    select single werks name1                               "n999530
      from t001w                                            "n999530
        into corresponding fields of l_s_t001w              "n999530
          where bwkey = l_f_werks.                          "n999530

    if not sy-subrc is initial.                             "n999530
      clear                  l_s_t001w.                     "n999530
      move  l_f_werks        to  l_s_t001w-werks.           "n999530
    endif.                                                  "n999530
  endif.                                                    "n999530

  move  l_s_t001w-name1      to  t001w-name1.               "n999530

ENDFORM.                     "f2200_read_t001

*----------------------------------------------------------------------*
*    F9000_AUTH_PLANT_CHECK
*----------------------------------------------------------------------*

FORM F9000_AUTH_PLANT_CHECK
                   USING     L_F_WERKS LIKE MARC-WERKS.

  CLEAR                      G_FLAG_AUTHORITY.
  ADD  1                     TO  G_F_CNT_BEFORE.

  READ TABLE G_T_AUTH_PLANT  WITH KEY
                             WERKS = L_F_WERKS BINARY SEARCH.

  IF SY-SUBRC IS INITIAL.
*   plant found in buffer; take the result from the buffer
    MOVE  G_T_AUTH_PLANT-OK            TO  G_FLAG_AUTHORITY.
  ELSE.
*   new plant / do the authority check / save result in buffer table
    AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
                    ID 'ACTVT' FIELD actvt03
                    ID 'WERKS' FIELD  L_F_WERKS.

    IF  SY-SUBRC IS INITIAL.
      MOVE : 'X'             TO  G_T_AUTH_PLANT-OK,
             'X'             TO  G_FLAG_AUTHORITY.
    ELSE.
      CLEAR : G_T_AUTH_PLANT-OK, G_FLAG_AUTHORITY.
    ENDIF.

    MOVE  L_F_WERKS          TO  G_T_AUTH_PLANT-WERKS.
    APPEND                   G_T_AUTH_PLANT.
    SORT                     G_T_AUTH_PLANT.
  ENDIF.

  IF  G_FLAG_AUTHORITY = 'X'.
    ADD  1                   TO  G_F_CNT_AFTER.
  ENDIF.

ENDFORM.                     "f9000_authority_check_plant

*----------------------------------------------------------------------*
*     F9100_AUTH_PLANT_RESULT
*----------------------------------------------------------------------*

FORM  F9100_AUTH_PLANT_RESULT.

  CASE    G_F_CNT_AFTER.     "results ?
    WHEN  G_F_CNT_BEFORE.
*     user has authority for all plants in G_RA_WERKS
    WHEN  0.
*     user has no authority for the plants in G_RA_WERKS
      MESSAGE S124.   "Wegen fehlender Berechtigung ist ...
*     leave report to selection screen
      PERFORM                 ANFORDERUNGSBILD.
    WHEN  OTHERS.
*     user has authority for only a part of the plants
      MESSAGE S124.   "Wegen fehlender Berechtigung ist ...
  ENDCASE.

* clear the counter fields for the next check
  CLEAR : G_F_CNT_AFTER, G_F_CNT_BEFORE.

ENDFORM.                     "f9100_auth_plant_result

*----------------------------------------------------------------------*
*    F9200_COLLECT_PLANT
*----------------------------------------------------------------------*

FORM F9200_COLLECT_PLANT
                   USING     L_F_WERKS LIKE      T001W-WERKS.

  CHECK : G_T_ORGAN[] IS INITIAL.                           "n433765

* build the range table g_0000_ra_werks
  MOVE : L_F_WERKS           TO  G_0000_RA_WERKS-LOW,
         'I'                 TO  G_0000_RA_WERKS-SIGN,
         'EQ'                TO  G_0000_RA_WERKS-OPTION.
  COLLECT                    G_0000_RA_WERKS.

ENDFORM.                     "f9200_collect_plant

*----------------------------------------------------------------------*
*    F9300_READ_ORGAN
*----------------------------------------------------------------------*

FORM F9300_READ_ORGAN
                   USING     L_F_KEYTYPE   LIKE G_S_ORGAN-KEYTYPE
                             L_F_KEYFIELD  LIKE G_S_ORGAN-KEYFIELD.

* buffer
  STATICS : L_S_OLD          TYPE      STYPE_ORGAN,
            L_9300_SUBRC     LIKE      SY-SUBRC.

  IF  L_F_KEYTYPE   =  L_S_OLD-KEYTYPE  AND
      L_F_KEYFIELD  =  L_S_OLD-KEYFIELD.
*   the same key : take the data from the buffer
    MOVE-CORRESPONDING  L_S_OLD        TO  G_S_ORGAN.
    MOVE  L_9300_SUBRC                 TO  SY-SUBRC.
  ELSE.
*   the key has changed : read in table g_t_organ
    READ TABLE G_T_ORGAN     WITH KEY
                             KEYTYPE   =  L_F_KEYTYPE
                             KEYFIELD  =  L_F_KEYFIELD
                             BINARY SEARCH.

    IF  SY-SUBRC IS INITIAL.
*     entry found
      MOVE-CORRESPONDING : G_T_ORGAN   TO  G_S_ORGAN,
                           G_T_ORGAN   TO  L_S_OLD.
      CLEAR                            L_9300_SUBRC.
    ELSE.
*     entry not found / fill the buffer
      CLEAR : L_S_OLD,       G_S_ORGAN.
      MOVE  : SY-SUBRC       TO  L_9300_SUBRC,
              L_F_KEYTYPE    TO  L_S_OLD-KEYTYPE,
              L_F_KEYFIELD   TO  L_S_OLD-KEYFIELD.
    ENDIF.
  ENDIF.

ENDFORM.                     "f9300_read_organ

*----------------------------------------------------------------------*
*    F9400_MATERIAL_KEY
*----------------------------------------------------------------------*

FORM F9400_MATERIAL_KEY
                   USING     L_F_MATNR LIKE      MARA-MATNR.

* create key table with material number
  MOVE : L_F_MATNR           TO G_T_MAT_KEY-MATNR.
  COLLECT                    G_T_MAT_KEY.

ENDFORM.                     "f9400_material_key

*----------------------------------------------------------------------*
*    f9500_set_color_and_sign
*----------------------------------------------------------------------*

form f9500_set_color_and_sign
         using  l_s_BELEGE   type  stype_belege
                l_f_tabname  type  stype_fields-fieldname.

  data : l_f_fieldname       type  stype_fields-fieldname.
  field-symbols : <l_fs_field>.

* clear the table with the color information
  REFRESH color. CLEAR color.

  loop at g_t_color_fields.
    concatenate  l_f_tabname
                 '-'
                 g_t_color_fields-fieldname
                             into l_f_fieldname.
    ASSIGN (l_f_fieldname)   TO  <l_fs_field>.

    check sy-subrc is initial.

    move : g_t_color_fields-fieldname
                             to  color-fieldname,
           0                 to  color-color-int.

*   the color and the sign of this numeric field depend on the
*   debit/credit-indicator
    case    l_s_BELEGE-SHKZG.
      when  'H'.
        color-color-col = '6'.         "red
        APPEND color.

        if  g_t_color_fields-type  <>  'C'.
          compute : <l_fs_field> = <l_fs_field> * -1.
        endif.

      when  'S'.
        color-color-col = '5'.         "green
        APPEND color.
    endcase.
  endloop.

* customizing : set the color information
  if  G_cust_color  = 'X'.
*   default : colorize the numeric fields
    move  color[]            to  l_s_BELEGE-FARBE_pro_feld.
  else.
*   the performant way : colorize the line on when GI
    if  l_s_BELEGE-SHKZG = 'H'.
      move  'C21'  to  l_s_belege-farbe_pro_zeile. "grey
    else.
      move  'C20'  to  l_s_belege-farbe_pro_zeile. "light grey
    endif.
  endif.

endform.                     "f9500_set_color_and_sign

*-----------------------------------------------------------"n547170
*    tpc_check_tax_auditor                                  "n547170
*-----------------------------------------------------------"n547170
                                                            "n547170
form tpc_check_tax_auditor.                                 "n547170
                                                            "n547170
* - the function module FI_CHECK_DATE of note 486477 will   "n547170
*   be processed when it exists                             "n547170
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n547170
    EXPORTING                                               "n547170
      FUNCNAME               = 'FI_CHECK_DATE'              "n547170
    EXCEPTIONS                                              "n547170
      FUNCTION_NOT_EXIST     = 1                            "n547170
      OTHERS                 = 2.                           "n547170
                                                            "n547170
  IF SY-SUBRC is initial.                                   "n547170
*   the function module FI_CHECK_DATE exists -> go on       "n547170
                                                            "n547170
*   separate time depending authorization for tax auditor   "n547170
*   first step : check, whether the user is a tax auditor   "n547170
    move  sy-repid             to  g_f_repid.               "n486477
                                                            "n486477
    call function 'FI_CHECK_DATE'                           "n486477
      exporting                                             "n486477
        i_bukrs           = space                           "n486477
        i_user            = sy-uname                        "n486477
        i_program         = g_f_repid                       "n486477
      importing                                             "n486477
        e_return          = g_flag_tpcuser                  "n486477
      exceptions                                            "n486477
        no_authority_prog = 1                               "n486477
        no_authority_date = 2                               "n486477
        wrong_parameter   = 3                               "n486477
        others            = 4.                              "n486477
                                                            "n486477
    case  sy-subrc.                                         "n486477
      when  0.                                              "n486477
*       what kind of user : g_flag_tpcuser = 1 tax auditor  "n486477
*                           g_flag_tpcuser = 4 other other  "n486477
      when  1.                                              "n486477
*       user is tax auditor, but program is not allowed     "n486477
        message  e001(CA_CHECK_DATE) with g_f_repid.        "n486477
                                                            "n486477
      when  others.                                         "n486477
*       other error                                         "n486477
      message id sy-msgid type sy-msgty number sy-msgno     "n486477
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.     "n486477
    endcase.                                                "n486477
  endif.                                                    "n547170
                                                            "n486477
  if  g_flag_tpcuser = '1'.                                 "n486477
*   check and complete the selection-dates                  "n486477
    IF datum-low IS INITIAL.                                "n486477
      datum-low = '00000101'.                               "n486477
      IF datum-high IS INITIAL.                             "n486477
        datum-high = '99991231'.                            "n486477
      ENDIF.                                                "n486477
    ELSE.                                                   "n486477
      IF datum-high IS INITIAL.                             "n486477
        datum-high = datum-low.                             "n486477
      ENDIF.                                                "n486477
    ENDIF.                                                  "n486477
                                                            "n486477
*   second step : the user is an auditor -> check periods   "n486477
    perform                  tpc_check_date_for_all_CC.     "n486477
  endif.                                                    "n486477
                                                            "n547170
endform.                     "tpc_check_tax_auditor         "n547170
                                                            "n547170
*-----------------------------------------------------------"n486477
*    tpc_check_date_for_all_CC                              "n486477
*-----------------------------------------------------------"n486477
                                                            "n486477
form tpc_check_date_for_all_cc.                             "n486477
                                                            "n486477
* clear working aeras and ranges                            "n486477
  CLEAR    : G_S_BUKRS,  G_S_T001K,  G_S_T001W.             "n486477
  REFRESH  : G_T_BUKRS,  G_T_T001K,  G_T_T001W.             "n486477
                                                            "n486477
  IF  WERKS[] IS INITIAL.                                   "n486477
*   no restriction for plant :                              "n486477
*   get all matching company codes from table T001          "n486477
    SELECT BUKRS             FROM T001                      "n486477
         into corresponding fields of table g_t_bukrs       "n486477
         where  bukrs in bukrs.                             "n486477
                                                            "n486477
    IF  SY-SUBRC <> 0.                                      "n486477
      SET  CURSOR            FIELD  'BUKRS-LOW'.            "n486477
      MESSAGE  E282(M7).     "Company code does not exist   "n486477
    ENDIF.                                                  "n486477
  ELSE.                                                     "n486477
*   look for the corresponding company codes                "n486477
    PERFORM                  TPC_CHECK_GET_ALL_CC.          "n486477
  ENDIF.                                                    "n486477
                                                            "n486477
* check the selected company codes and the dates            "n486477
  loop at g_t_bukrs          into  g_s_bukrs.               "n486477
*   check the authorization for dates and company code      "n486477
    call function 'FI_CHECK_DATE'                           "n486477
      exporting                                             "n486477
        i_bukrs           = g_s_bukrs-bukrs                 "n486477
        i_user            = sy-uname                        "n486477
        I_PROGRAM         = G_F_REPID                       "n486477
        i_from_date       = datum-low                       "n486477
        i_to_date         = datum-high                      "n486477
      exceptions                                            "n486477
        no_authority_prog = 1                               "n486477
        no_authority_date = 2                               "n486477
        wrong_parameter   = 3                               "n486477
        others            = 4.                              "n486477
                                                            "n486477
    case sy-subrc.                                          "n486477
      when 0.                                               "n486477
*       authorization ok --> take this company code         "n486477
                                                            "n486477
      when 2.                                               "n486477
*       send 2 messages to show the company code           "n486477
        set  cursor          field  'BUKRS-LOW'.            "n486477
*       no display authorization for company code &         "n486477
        message  i113(fg)    with  g_s_bukrs-bukrs.         "n486477
                                                            "n486477
        set cursor           field  'BWKEY-LOW'.            "n486477
*       Keine Berechtigung zur Anzeige von Daten aus ...    "n486477
        MESSAGE              E002(CA_CHECK_DATE).           "n486477
                                                            "n486477
      when others.                                          "n486477
*       an error occurred                                   "n486477
        message id sy-msgid type sy-msgty number sy-msgno   "n486477
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.   "n486477
    endcase.                                                "n486477
  endloop.                                                  "n486477
                                                            "n486477
  FREE : G_T_BUKRS,  G_T_T001K,  G_T_T001W.                 "n486477
                                                            "n486477
ENDFORM.                     "tpc_check_date_for_all_CC     "n486477
                                                            "n486477
*-----------------------------------------------------------"n486477
*   tpc_check_get_all_cc                                    "n486477
*   look for the corresponding company codes                "n486477
*-----------------------------------------------------------"n486477
                                                            "n486477
FORM TPC_CHECK_GET_ALL_CC.                                  "n486477
                                                            "n486477
* select the matching plants from T001W                     "n486477
  SELECT WERKS BWKEY         FROM  T001W                    "n486477
         INTO CORRESPONDING FIELDS OF TABLE G_T_T001W       "n486477
         WHERE  WERKS IN WERKS.                             "n486477
                                                            "n486477
  IF  SY-SUBRC IS INITIAL.                                  "n486477
    SORT  G_T_T001W          BY  WERKS BWKEY.               "n486477
  ELSE.                                                     "n486477
    SET CURSOR               FIELD  'WERKS-LOW'.            "n486477
*   Plant & does not exist                                  "n486477
    MESSAGE E892             WITH   WERKS-LOW.              "n486477
  ENDIF.                                                    "n486477
                                                            "n486477
  LOOP AT G_T_T001W          INTO  G_S_T001W.               "n486477
*   select the matching valuation areas and comany codes    "n486477
    SELECT SINGLE BWKEY BUKRS FROM  T001K                   "n486477
         INTO CORRESPONDING FIELDS OF G_S_T001K             "n486477
         WHERE  BWKEY  =  G_S_T001W-BWKEY                   "n486477
           AND  BUKRS  IN BUKRS.                            "n486477
                                                            "n486477
    IF  SY-SUBRC IS INITIAL.                                "n486477
      MOVE  G_S_T001K-BUKRS  TO  G_S_BUKRS-BUKRS.           "n486477
      COLLECT  G_S_BUKRS     INTO  G_T_BUKRS.               "n486477
    ELSE.                                                   "n486477
      SET CURSOR             FIELD  'WERKS-LOW'.            "n486477
      MESSAGE  E283          WITH   G_S_T001W-WERKS.        "n486477
    ENDIF.                                                  "n486477
  ENDLOOP.                                                  "n486477
                                                            "n486477
ENDFORM.                     "tpc_check_get_all_cc          "n486477
                                                            "n486477
*-----------------------------------------------------------"n486477
*    tpc_write_log                                          "n555246
*-----------------------------------------------------------"n555246
                                                            "n555246
form tpc_write_log.                                         "n555246
                                                            "n555246
* check whether the function module is available            "n555246
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n555246
    EXPORTING                                               "n555246
      FUNCNAME           = 'CA_WRITE_LOG'                   "n555246
    EXCEPTIONS                                              "n555246
      FUNCTION_NOT_EXIST = 1                                "n555246
      OTHERS             = 2.                               "n555246
                                                            "n555246
  check : sy-subrc is initial.                              "n555246
                                                            "n555246
* write the entries of the selection screen into log file   "n555246
  CALL FUNCTION         'CA_WRITE_LOG'      "#EC EXISTS     "n555246
        EXPORTING                                           "n555246
          I_PROGRAM     = g_f_repid                         "n555246
        EXCEPTIONS                                          "n555246
          WRITE_ERROR   = 1                                 "n555246
          OTHERS        = 2.                                "n555246
                                                            "n555246
  IF SY-SUBRC is initial.                                   "n555246
    COMMIT WORK.                                            "n555246
  else.                                                     "n555246
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO       "n555246
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.   "n555246
  ENDIF.                                                    "n555246
                                                            "n555246
endform.                     "tpc_write_log                 "n555246
                                                            "n555246
*-----------------------------------------------------------"n555246
