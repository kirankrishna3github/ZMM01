*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 21.05.2020 at 16:51:17
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZFI_V_VEN_DT_VAL................................*
FORM GET_DATA_ZFI_V_VEN_DT_VAL.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM Z6MMA_PARAMS WHERE
    ( PROGNAME EQ 'EXCL_VEND_DT' OR
    PROGNAME EQ 'EXCL_MIGO_DT' OR
    PROGNAME EQ 'EXCL_ML81N_DT' ) AND
(VIM_WHERETAB) .
    CLEAR ZFI_V_VEN_DT_VAL .
ZFI_V_VEN_DT_VAL-MANDT =
Z6MMA_PARAMS-MANDT .
ZFI_V_VEN_DT_VAL-PROGNAME =
Z6MMA_PARAMS-PROGNAME .
ZFI_V_VEN_DT_VAL-PARAM1 =
Z6MMA_PARAMS-PARAM1 .
ZFI_V_VEN_DT_VAL-PARAM2 =
Z6MMA_PARAMS-PARAM2 .
ZFI_V_VEN_DT_VAL-PARAM3 =
Z6MMA_PARAMS-PARAM3 .
ZFI_V_VEN_DT_VAL-PARAM4 =
Z6MMA_PARAMS-PARAM4 .
ZFI_V_VEN_DT_VAL-SERNO =
Z6MMA_PARAMS-SERNO .
ZFI_V_VEN_DT_VAL-FROMDT =
Z6MMA_PARAMS-FROMDT .
ZFI_V_VEN_DT_VAL-TODT =
Z6MMA_PARAMS-TODT .
<VIM_TOTAL_STRUC> = ZFI_V_VEN_DT_VAL.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZFI_V_VEN_DT_VAL .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZFI_V_VEN_DT_VAL.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZFI_V_VEN_DT_VAL-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM Z6MMA_PARAMS WHERE
  PROGNAME = ZFI_V_VEN_DT_VAL-PROGNAME AND
  PARAM1 = ZFI_V_VEN_DT_VAL-PARAM1 AND
  PARAM2 = ZFI_V_VEN_DT_VAL-PARAM2 AND
  PARAM3 = ZFI_V_VEN_DT_VAL-PARAM3 AND
  PARAM4 = ZFI_V_VEN_DT_VAL-PARAM4 AND
  SERNO = ZFI_V_VEN_DT_VAL-SERNO AND
  FROMDT = ZFI_V_VEN_DT_VAL-FROMDT AND
  TODT = ZFI_V_VEN_DT_VAL-TODT .
    IF SY-SUBRC = 0.
    DELETE Z6MMA_PARAMS .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM Z6MMA_PARAMS WHERE
  PROGNAME = ZFI_V_VEN_DT_VAL-PROGNAME AND
  PARAM1 = ZFI_V_VEN_DT_VAL-PARAM1 AND
  PARAM2 = ZFI_V_VEN_DT_VAL-PARAM2 AND
  PARAM3 = ZFI_V_VEN_DT_VAL-PARAM3 AND
  PARAM4 = ZFI_V_VEN_DT_VAL-PARAM4 AND
  SERNO = ZFI_V_VEN_DT_VAL-SERNO AND
  FROMDT = ZFI_V_VEN_DT_VAL-FROMDT AND
  TODT = ZFI_V_VEN_DT_VAL-TODT .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR Z6MMA_PARAMS.
    ENDIF.
Z6MMA_PARAMS-MANDT =
ZFI_V_VEN_DT_VAL-MANDT .
Z6MMA_PARAMS-PROGNAME =
ZFI_V_VEN_DT_VAL-PROGNAME .
Z6MMA_PARAMS-PARAM1 =
ZFI_V_VEN_DT_VAL-PARAM1 .
Z6MMA_PARAMS-PARAM2 =
ZFI_V_VEN_DT_VAL-PARAM2 .
Z6MMA_PARAMS-PARAM3 =
ZFI_V_VEN_DT_VAL-PARAM3 .
Z6MMA_PARAMS-PARAM4 =
ZFI_V_VEN_DT_VAL-PARAM4 .
Z6MMA_PARAMS-SERNO =
ZFI_V_VEN_DT_VAL-SERNO .
Z6MMA_PARAMS-FROMDT =
ZFI_V_VEN_DT_VAL-FROMDT .
Z6MMA_PARAMS-TODT =
ZFI_V_VEN_DT_VAL-TODT .
    IF SY-SUBRC = 0.
    UPDATE Z6MMA_PARAMS ##WARN_OK.
    ELSE.
    INSERT Z6MMA_PARAMS .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZFI_V_VEN_DT_VAL-UPD_FLAG,
STATUS_ZFI_V_VEN_DT_VAL-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZFI_V_VEN_DT_VAL.
  SELECT SINGLE * FROM Z6MMA_PARAMS WHERE
PROGNAME = ZFI_V_VEN_DT_VAL-PROGNAME AND
PARAM1 = ZFI_V_VEN_DT_VAL-PARAM1 AND
PARAM2 = ZFI_V_VEN_DT_VAL-PARAM2 AND
PARAM3 = ZFI_V_VEN_DT_VAL-PARAM3 AND
PARAM4 = ZFI_V_VEN_DT_VAL-PARAM4 AND
SERNO = ZFI_V_VEN_DT_VAL-SERNO AND
FROMDT = ZFI_V_VEN_DT_VAL-FROMDT AND
TODT = ZFI_V_VEN_DT_VAL-TODT .
ZFI_V_VEN_DT_VAL-MANDT =
Z6MMA_PARAMS-MANDT .
ZFI_V_VEN_DT_VAL-PROGNAME =
Z6MMA_PARAMS-PROGNAME .
ZFI_V_VEN_DT_VAL-PARAM1 =
Z6MMA_PARAMS-PARAM1 .
ZFI_V_VEN_DT_VAL-PARAM2 =
Z6MMA_PARAMS-PARAM2 .
ZFI_V_VEN_DT_VAL-PARAM3 =
Z6MMA_PARAMS-PARAM3 .
ZFI_V_VEN_DT_VAL-PARAM4 =
Z6MMA_PARAMS-PARAM4 .
ZFI_V_VEN_DT_VAL-SERNO =
Z6MMA_PARAMS-SERNO .
ZFI_V_VEN_DT_VAL-FROMDT =
Z6MMA_PARAMS-FROMDT .
ZFI_V_VEN_DT_VAL-TODT =
Z6MMA_PARAMS-TODT .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZFI_V_VEN_DT_VAL USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZFI_V_VEN_DT_VAL-PROGNAME TO
Z6MMA_PARAMS-PROGNAME .
MOVE ZFI_V_VEN_DT_VAL-PARAM1 TO
Z6MMA_PARAMS-PARAM1 .
MOVE ZFI_V_VEN_DT_VAL-PARAM2 TO
Z6MMA_PARAMS-PARAM2 .
MOVE ZFI_V_VEN_DT_VAL-PARAM3 TO
Z6MMA_PARAMS-PARAM3 .
MOVE ZFI_V_VEN_DT_VAL-PARAM4 TO
Z6MMA_PARAMS-PARAM4 .
MOVE ZFI_V_VEN_DT_VAL-SERNO TO
Z6MMA_PARAMS-SERNO .
MOVE ZFI_V_VEN_DT_VAL-FROMDT TO
Z6MMA_PARAMS-FROMDT .
MOVE ZFI_V_VEN_DT_VAL-TODT TO
Z6MMA_PARAMS-TODT .
MOVE ZFI_V_VEN_DT_VAL-MANDT TO
Z6MMA_PARAMS-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'Z6MMA_PARAMS'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN Z6MMA_PARAMS TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'Z6MMA_PARAMS'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*

* base table related FORM-routines.............
INCLUDE LSVIMFTX .
