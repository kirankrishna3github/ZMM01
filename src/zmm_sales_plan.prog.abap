*&********************************************************************************************&*
*& OBJECT NAME          : ZMM_SALES_PLAN                                                      &*
*  DEVELOPER            : PRADEEP KODINAGULA                                                  &*
*& MODULE NAME          : SD                                                                  &*
*& PROGRAM TYPE         : REPORT                                                              &*
*& CREATE DATE          : 28/06/2016                                                          &*
*& DESCRIPTION          : THIS REPORT IS USED TO PREDICT FUTURE SALES BASED ON PAST PERIOD.   &*
* REVISION HISTORY----------------------------------------------------------------------------*
*                                                                                             *
*   CHANGED BY:                                                                               *
*   CHANGE ON:                                                                                *
*   REASON FOR CHANGE:                                                                        *
*                                                                                             *
* REVISION HISTORY----------------------------------------------------------------------------*

REPORT  zmm_sales_plan.


*&*******************************************************************************************&
*&                             DATA DECLARATIONS                                             &
*&*******************************************************************************************&

TYPE-POOLS : slis.

DATA : a TYPE zmis_salesdata-werks,
       b TYPE zmis_salesdata-vkorg,
       c TYPE zmis_salesdata-vtweg,
       d TYPE zmis_salesdata-spart,
       e TYPE knvv-bzirk,
       f TYPE knvv-vkgrp,
       g TYPE zmis_salesdata-fkdat,
       h TYPE zmis_salesdata-fkdat,
       i TYPE i.


DATA : BEGIN OF wtab,
         waerk TYPE zmis_salesdata-waerk,
         vkorg TYPE zmis_salesdata-vkorg,
         vtweg TYPE zmis_salesdata-vtweg,
         bzirk TYPE zmis_salesdata-bzirk,
         fkdat TYPE zmis_salesdata-fkdat,
         kunag TYPE zmis_salesdata-kunag,
         spart TYPE zmis_salesdata-spart,
         fkimg TYPE zmis_salesdata-fkimg,
         meins TYPE zmis_salesdata-meins,
         netwr TYPE zmis_salesdata-netwr,
         matnr TYPE zmis_salesdata-matnr,
         matkl TYPE zmis_salesdata-matkl,
         werks TYPE zmis_salesdata-werks,
         vkgrp TYPE zmis_salesdata-vkgrp,
       END OF wtab,

       itab LIKE TABLE OF wtab.

DATA : BEGIN OF wtab1,

         kunag   TYPE zmis_salesdata-kunag,
         name1   TYPE kna1-name1,
         vkorg   TYPE zmis_salesdata-vkorg,
         vtweg   TYPE zmis_salesdata-vtweg,
         spart   TYPE zmis_salesdata-spart,
         bzirk   TYPE zmis_salesdata-bzirk,
         vkgrp   TYPE zmis_salesdata-vkgrp,
         werks   TYPE zmis_salesdata-werks,
         matnr   TYPE zmis_salesdata-matnr,
         maktx   TYPE makt-maktx,
         matkl   TYPE zmis_salesdata-matkl,
         extwg   TYPE mara-extwg,
         waerk   TYPE zmis_salesdata-waerk,
         meins   TYPE zmis_salesdata-meins,
         fkimg   TYPE zmis_salesdata-fkimg,
         netwr   TYPE zmis_salesdata-netwr,
         days    TYPE int4,
         qty     TYPE fkimg,
         days1   TYPE int4,
         qty1    TYPE  fkimg,
         inc     TYPE int4,
         zvkgrp2 TYPE zter,
       END OF wtab1,

       itab1 LIKE TABLE OF wtab1.

DATA : BEGIN OF wtab2,

         vkorg TYPE zmis_salesdata-vkorg,
         vtweg TYPE zmis_salesdata-vtweg,
         spart TYPE zmis_salesdata-spart,
         bzirk TYPE zmis_salesdata-bzirk,
         vkgrp TYPE zmis_salesdata-vkgrp,
         werks TYPE zmis_salesdata-werks,
         matnr TYPE zmis_salesdata-matnr,
         maktx TYPE makt-maktx,
         matkl TYPE zmis_salesdata-matkl,
         extwg TYPE mara-extwg,
         waerk TYPE zmis_salesdata-waerk,
         meins TYPE zmis_salesdata-meins,
         fkimg TYPE zmis_salesdata-fkimg,
         netwr TYPE zmis_salesdata-netwr,
         days  TYPE int4,
         qty   TYPE fkimg,
         days1 TYPE int4,
         qty1  TYPE  fkimg,
         inc   TYPE int4,
       END OF wtab2,

       itab2 LIKE TABLE OF wtab2.

DATA : BEGIN OF wtab3,

         vkorg TYPE zmis_salesdata-vkorg,
         vtweg TYPE zmis_salesdata-vtweg,
         spart TYPE zmis_salesdata-spart,
         bzirk TYPE zmis_salesdata-bzirk,
         werks TYPE zmis_salesdata-werks,
         matnr TYPE zmis_salesdata-matnr,
         maktx TYPE makt-maktx,
         matkl TYPE zmis_salesdata-matkl,
         extwg TYPE mara-extwg,
         waerk TYPE zmis_salesdata-waerk,
         meins TYPE zmis_salesdata-meins,
         fkimg TYPE zmis_salesdata-fkimg,
         netwr TYPE zmis_salesdata-netwr,
         days  TYPE int4,
         qty   TYPE fkimg,
         days1 TYPE int4,
         qty1  TYPE  fkimg,
         inc   TYPE  int4,
       END OF wtab3,

       itab3 LIKE TABLE OF wtab3.

DATA : BEGIN OF wa_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF wa_kna1,

       it_kna1 LIKE TABLE OF wa_kna1.


DATA : BEGIN OF wa_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF wa_makt,

       it_makt LIKE TABLE OF wa_makt.

DATA : BEGIN OF wa_mara,
         matnr TYPE mara-matnr,
         extwg TYPE mara-extwg,
       END OF wa_mara,

       it_mara LIKE TABLE OF wa_mara.

DATA : BEGIN OF wa_tvgrt,
         vkgrp TYPE tvgrt-vkgrp,
         bezei TYPE tvgrt-bezei,
       END OF wa_tvgrt,

       it_tvgrt LIKE TABLE OF wa_tvgrt.

DATA : wa_fcat  TYPE slis_fieldcat_alv,
       it_fcat  LIKE TABLE OF wa_fcat,

       wa_fcat1 TYPE slis_fieldcat_alv,
       it_fcat1 LIKE TABLE OF wa_fcat1,

       wa_fcat2 TYPE slis_fieldcat_alv,
       it_fcat2 LIKE TABLE OF wa_fcat2.


DATA : layout   TYPE slis_layout_alv,
       idx      TYPE sy-tabix,
       rnd      TYPE i,
       add      TYPE i,
       days     TYPE i,
       tot_days TYPE i,
       lv_zter  TYPE zter.


FIELD-SYMBOLS:  <wa>  LIKE LINE OF itab1.
FIELD-SYMBOLS:  <wa1> LIKE LINE OF itab2.
FIELD-SYMBOLS:  <wa2> LIKE LINE OF itab3.

*&*******************************************************************************************&
*&                             SELECTION SCREEN                                              &
*&*******************************************************************************************&

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME.

SELECT-OPTIONS : so_plant FOR a NO INTERVALS NO-EXTENSION OBLIGATORY,                  " PLANT
                 so_vkorg FOR b NO INTERVALS NO-EXTENSION DEFAULT 1000 OBLIGATORY,     " SALES ORG
                 so_vtweg FOR c NO INTERVALS NO-EXTENSION DEFAULT 10 OBLIGATORY,       " DIST CHANNEL
                 so_spart FOR d NO INTERVALS NO-EXTENSION DEFAULT 10 OBLIGATORY,       " DIVISION
                 so_bzirk FOR e NO INTERVALS NO-EXTENSION," OBLIGATORY, IHDK904613     " REGION
                 so_vkgrp FOR f,                                                       " TERRITORY
                 so_date1 FOR g OBLIGATORY,                                           " SALES PERIOD
                 so_date2 FOR h NO-EXTENSION OBLIGATORY,                               " PLANNING PERIOD
                 so_inc   FOR i NO-EXTENSION NO INTERVALS.                             " INCREASE BY %
SELECTION-SCREEN END OF BLOCK a1.


SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.


PARAMETERS : rb1 RADIOBUTTON GROUP g1 USER-COMMAND abc DEFAULT 'X' MODIF ID pr,
             rb2 RADIOBUTTON GROUP g1 MODIF ID pr,
             rb3 RADIOBUTTON GROUP g1 MODIF ID pr.


SELECTION-SCREEN END OF BLOCK a2.

SELECTION-SCREEN END OF BLOCK a.


INITIALIZATION.
*---------------------------------------------------------------------------------------" SELECT OPTION RESTRICTION ON FIELD SO_DATE1
  PERFORM date1_restriction.


AT SELECTION-SCREEN.
*---------------------------------------------------------------------------------------" FETCH DATA AND PROCESS
  PERFORM autho.                                                                          " CHECKING AUTHORIZATIONS

START-OF-SELECTION.
*---------------------------------------------------------------------------------------" FETCH DATA AND PROCESS

  PERFORM days_cal.                                                                       " CALCULTAION FOR TOTAL DAYS IN GIVEN SALES PERIOD

  SELECT waerk vkorg vtweg bzirk fkdat kunag spart fkimg meins netwr matnr matkl werks vkgrp FROM zmis_salesdata INTO TABLE itab
    WHERE werks IN so_plant AND vkorg IN so_vkorg AND vtweg IN so_vtweg AND spart IN so_spart AND bzirk IN so_bzirk
*{   REPLACE        SBXK900101                                        1
*\    AND vkgrp IN so_vkgrp AND fkdat IN so_date1[] %_HINTS ORACLE 'INDEX("ZMIS_SALESDATA" "ZMIS_SALESDATA~Z1" )'.
    AND vkgrp IN so_vkgrp AND fkdat IN so_date1[].  " S4/HANA
*}   REPLACE


  IF itab IS NOT INITIAL.

    SORT itab BY kunag.
    SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1 FOR ALL ENTRIES IN itab WHERE kunnr EQ itab-kunag.

    SORT itab BY matnr.
    SELECT matnr maktx FROM makt INTO TABLE it_makt FOR ALL ENTRIES IN itab WHERE matnr EQ itab-matnr AND spras EQ 'EN' .

    SELECT matnr extwg FROM mara INTO TABLE it_mara FOR ALL ENTRIES IN itab WHERE matnr EQ itab-matnr.

    SORT itab BY vkgrp.
    SELECT vkgrp bezei FROM tvgrt INTO TABLE it_tvgrt FOR ALL ENTRIES IN itab WHERE vkgrp EQ itab-vkgrp AND spras EQ 'EN'.

    SORT it_kna1 BY kunnr.
    SORT it_makt BY matnr.
    SORT it_mara BY matnr.
    SORT it_tvgrt BY vkgrp.

    IF rb1 = 'X'.                                                                        " CUSTOMER WISE CONSOLIDATION OF DATA

      LOOP AT itab INTO wtab.
        MOVE-CORRESPONDING wtab TO wtab1.
        COLLECT wtab1 INTO itab1.
        CLEAR : wtab,wtab1.
      ENDLOOP.


      LOOP AT itab1 ASSIGNING <wa>.
        idx = sy-tabix.
        <wa>-days = ( tot_days ).
        <wa>-qty  = ( <wa>-fkimg / <wa>-days ).
        rnd = <wa>-qty .
        <wa>-qty = rnd.

        <wa>-days1 = ( so_date2-high - so_date2-low ).
        <wa>-qty1  = ( <wa>-days1 ) * ( rnd ).

        add   = ( so_inc-low / 100 ) * ( <wa>-qty1 ).
        <wa>-inc = ( <wa>-qty1 + add ).

        READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = <wa>-kunag BINARY SEARCH.
        IF sy-subrc = 0.
          <wa>-name1 = wa_kna1-name1.
        ENDIF.

        READ TABLE it_makt INTO wa_makt WITH KEY matnr = <wa>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          <wa>-maktx = wa_makt-maktx.
        ENDIF.

        READ TABLE it_mara INTO wa_mara WITH KEY matnr = <wa>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          <wa>-extwg = wa_mara-extwg.
        ENDIF.

        CLEAR:lv_zter.
        READ TABLE it_tvgrt INTO wa_tvgrt WITH KEY vkgrp = <wa>-vkgrp BINARY SEARCH.
        IF sy-subrc EQ 0.
          CONCATENATE wa_tvgrt-vkgrp '-' wa_tvgrt-bezei INTO lv_zter.
          CONDENSE lv_zter.
          <wa>-zvkgrp2 = lv_zter.
        ELSE.
          CLEAR lv_zter.
        ENDIF.

*    MODIFY ITAB1 FROM WTAB1 INDEX IDX TRANSPORTING NAME1 MAKTX EXTWG DAYS QTY DAYS1 QTY1 INC.
        CLEAR: idx, rnd, add.
      ENDLOOP.

      SORT itab1 BY kunag matnr.
      EXPORT itab1 TO MEMORY ID 'ZLU'.

    ELSEIF rb2 = 'X'.                                                                " TERRITORY WISE CONSOLIDATION OF DATA

      LOOP AT itab INTO wtab.
        MOVE-CORRESPONDING wtab TO wtab2.
        COLLECT wtab2 INTO itab2.
        CLEAR : wtab,wtab2.
      ENDLOOP.

      LOOP AT itab2 ASSIGNING <wa1>.

        idx = sy-tabix.
        <wa1>-days = ( tot_days ).
        <wa1>-qty  = ( <wa1>-fkimg / <wa1>-days ).
        rnd = <wa1>-qty .
        <wa1>-qty = rnd.

        <wa1>-days1 = ( so_date2-high - so_date2-low ).
        <wa1>-qty1  = ( <wa1>-days1 ) * ( rnd ).

        add   = ( so_inc-low / 100 ) * ( <wa1>-qty1 ).
        <wa1>-inc = ( <wa1>-qty1 + add ).

        READ TABLE it_makt INTO wa_makt WITH KEY matnr = <wa1>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          <wa1>-maktx = wa_makt-maktx.
        ENDIF.

        READ TABLE it_mara INTO wa_mara WITH KEY matnr = <wa1>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          <wa1>-extwg = wa_mara-extwg.
        ENDIF.

        CLEAR : idx, rnd, add.

      ENDLOOP.

      SORT itab2 BY vkgrp matnr.
      EXPORT itab2 TO MEMORY ID 'ZAN'.

    ELSEIF rb3 = 'X'.                                                        " REGION WISE CONSOLIDATION OF DATA

      LOOP AT itab INTO wtab.
        MOVE-CORRESPONDING wtab TO wtab3.
        COLLECT wtab3 INTO itab3.
        CLEAR : wtab,wtab3.
      ENDLOOP.

      LOOP AT itab3 ASSIGNING <wa2>.
        idx = sy-tabix.
        <wa2>-days = ( tot_days ).
        <wa2>-qty  = ( <wa2>-fkimg / <wa2>-days ).
        rnd = <wa2>-qty .
        <wa2>-qty = rnd.

        <wa2>-days1 = ( so_date2-high - so_date2-low ).
        <wa2>-qty1  = ( <wa2>-days1 ) * ( rnd ).
        add   = ( so_inc-low / 100 ) * ( <wa2>-qty1 ).
        <wa2>-inc = ( <wa2>-qty1 + add ).

        READ TABLE it_makt INTO wa_makt WITH KEY matnr = <wa2>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          <wa2>-maktx = wa_makt-maktx.
        ENDIF.

        READ TABLE it_mara INTO wa_mara WITH KEY matnr = <wa2>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
          <wa2>-extwg = wa_mara-extwg.
        ENDIF.

        CLEAR: idx, rnd, add.
      ENDLOOP.

      SORT itab3 BY bzirk matnr.


    ENDIF.

  ENDIF.


  IF rb1 = 'X'.
    IF itab1 IS NOT INITIAL.
      PERFORM fcat.
      PERFORM display.
    ENDIF.
  ELSEIF rb2 = 'X'.
    IF itab2 IS NOT INITIAL.
      PERFORM fcat1.
      PERFORM display1.
    ENDIF.
  ELSE.
    IF itab3 IS NOT INITIAL.
      PERFORM fcat2.
      PERFORM display2.
    ENDIF.
  ENDIF.

*&*******************************************************************************************&
*&                             SUB ROUTIUNES                                                 &
*&*******************************************************************************************&

FORM fcat.

  wa_fcat-fieldname = 'KUNAG'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Customer'.
  wa_fcat-seltext_m = 'Customer'.
  wa_fcat-seltext_s = 'Cust'.
  wa_fcat-no_zero   = 'X'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'NAME1'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Customer Name'.
  wa_fcat-seltext_m = 'Customer Name'.
  wa_fcat-seltext_s = 'Cust.Name'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'VKORG'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Sales Org'.
  wa_fcat-seltext_m = 'Sales Org'.
  wa_fcat-seltext_s = 'S.Org'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.


  wa_fcat-fieldname = 'VTWEG'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Dist Chanel'.
  wa_fcat-seltext_m = 'DistChan'.
  wa_fcat-seltext_s = 'D.Chan'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'SPART'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Division'.
  wa_fcat-seltext_m = 'Division'.
  wa_fcat-seltext_s = 'Div'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.
*

  wa_fcat-fieldname = 'BZIRK'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Region'.
  wa_fcat-seltext_m = 'Region'.
  wa_fcat-seltext_s = 'Reg'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'VKGRP'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Territory'.
  wa_fcat-seltext_m = 'Territory'.
  wa_fcat-seltext_s = 'Terr'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'ZVKGRP2'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Territory Name'.
  wa_fcat-seltext_m = 'Territory Name'.
  wa_fcat-seltext_s = 'Terr Text'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'WERKS'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Plant'.
  wa_fcat-seltext_m = 'Plant'.
  wa_fcat-seltext_s = 'Plnt'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.


  wa_fcat-fieldname = 'MATNR'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Material'.
  wa_fcat-seltext_m = 'Material'.
  wa_fcat-seltext_s = 'Material'.
  wa_fcat-no_zero   = 'X'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'MAKTX'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Material Name'.
  wa_fcat-seltext_m = 'Material Name'.
  wa_fcat-seltext_s = 'Material Nam'.
  wa_fcat-no_zero   = 'X'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'MATKL'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Material Group'.
  wa_fcat-seltext_m = 'Mat Grp'.
  wa_fcat-seltext_s = 'M.Grp'.
  wa_fcat-no_zero   = 'X'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'EXTWG'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Ext Material Group'.
  wa_fcat-seltext_m = 'Ext Mat Grp'.
  wa_fcat-seltext_s = 'E.M.Grp'.
  wa_fcat-no_zero   = 'X'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'FKIMG'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Quantity'.
  wa_fcat-seltext_m = 'Quantity'.
  wa_fcat-seltext_s = 'Quan'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'MEINS'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Unit'.
  wa_fcat-seltext_m = 'Unit'.
  wa_fcat-seltext_s = 'UOM'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'NETWR'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Gross Value'.
  wa_fcat-seltext_m = 'Gross Val'.
  wa_fcat-seltext_s = 'G.Val'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'WAERK'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Currency'.
  wa_fcat-seltext_m = 'Currency'.
  wa_fcat-seltext_s = 'Curr'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'DAYS'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Days in sales period'.
  wa_fcat-seltext_m = 'Days in S.Period'.
  wa_fcat-seltext_s = 'Days in S.per'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'QTY'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'QTY/DAY in sales period'.
  wa_fcat-seltext_m = 'QTY/DAY in S.Period'.
  wa_fcat-seltext_s = 'QTY/DAY in S.per'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'DAYS1'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'Days in Plan period'.
  wa_fcat-seltext_m = 'Days in P.Period'.
  wa_fcat-seltext_s = 'Days in P.per'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.


  wa_fcat-fieldname = 'QTY1'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = 'QTY in Plan period'.
  wa_fcat-seltext_m = 'QTY in P.Period'.
  wa_fcat-seltext_s = 'QTY in P.per'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

  wa_fcat-fieldname = 'INC'.
  wa_fcat-tabname   = 'ITAB1'.
  wa_fcat-seltext_l = ' Increased to QTY'.
  wa_fcat-seltext_m = 'Increased to QTY'.
  wa_fcat-seltext_s = 'Inc to QTY'.
  APPEND wa_fcat TO it_fcat.
  CLEAR wa_fcat.

***********************Layout design
  layout-colwidth_optimize = 'X'.


ENDFORM.



FORM display.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-cprog
      is_layout          = layout
      it_fieldcat        = it_fcat
*     I_DEFAULT          = 'U'
      i_save             = 'X'
*     I_CALLBACK_PF_STATUS_SET          = 'SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
    TABLES
      t_outtab           = itab1
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.




FORM fcat1.


  wa_fcat1-fieldname = 'VKORG'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Sales Org'.
  wa_fcat1-seltext_m = 'Sales Org'.
  wa_fcat1-seltext_s = 'S.Org'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.


  wa_fcat1-fieldname = 'VTWEG'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Dist Chanel'.
  wa_fcat1-seltext_m = 'DistChan'.
  wa_fcat1-seltext_s = 'D.Chan'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'SPART'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Division'.
  wa_fcat1-seltext_m = 'Division'.
  wa_fcat1-seltext_s = 'Div'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'BZIRK'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Region'.
  wa_fcat1-seltext_m = 'Region'.
  wa_fcat1-seltext_s = 'Reg'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'VKGRP'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Territory'.
  wa_fcat1-seltext_m = 'Territory'.
  wa_fcat1-seltext_s = 'Terr'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'WERKS'.
  wa_fcat1-tabname   = 'ITAB1'.
  wa_fcat1-seltext_l = 'Plant'.
  wa_fcat1-seltext_m = 'Plant'.
  wa_fcat1-seltext_s = 'Plnt'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.


  wa_fcat1-fieldname = 'MATNR'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Material'.
  wa_fcat1-seltext_m = 'Material'.
  wa_fcat1-seltext_s = 'Material'.
  wa_fcat1-no_zero   = 'X'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'MAKTX'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Material Name'.
  wa_fcat1-seltext_m = 'Material Name'.
  wa_fcat1-seltext_s = 'Material Nam'.
  wa_fcat1-no_zero   = 'X'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'MATKL'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Material Group'.
  wa_fcat1-seltext_m = 'Mat Grp'.
  wa_fcat1-seltext_s = 'M.Grp'.
  wa_fcat1-no_zero   = 'X'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'EXTWG'.
  wa_fcat1-tabname   = 'ITAB1'.
  wa_fcat1-seltext_l = 'Ext Material Group'.
  wa_fcat1-seltext_m = 'Ext Mat Grp'.
  wa_fcat1-seltext_s = 'E.M.Grp'.
  wa_fcat1-no_zero   = 'X'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'FKIMG'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Quantity'.
  wa_fcat1-seltext_m = 'Quantity'.
  wa_fcat1-seltext_s = 'Quan'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'MEINS'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Unit'.
  wa_fcat1-seltext_m = 'Unit'.
  wa_fcat1-seltext_s = 'UOM'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'NETWR'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Gross Value'.
  wa_fcat1-seltext_m = 'Gross Val'.
  wa_fcat1-seltext_s = 'G.Val'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'WAERK'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Currency'.
  wa_fcat1-seltext_m = 'Currency'.
  wa_fcat1-seltext_s = 'Curr'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'DAYS'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Days in sales period'.
  wa_fcat1-seltext_m = 'Days in S.Period'.
  wa_fcat1-seltext_s = 'Days in S.per'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'QTY'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'QTY/DAY in sales period'.
  wa_fcat1-seltext_m = 'QTY/DAY in S.Period'.
  wa_fcat1-seltext_s = 'QTY/DAY in S.per'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

  wa_fcat1-fieldname = 'DAYS1'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'Days in Plan period'.
  wa_fcat1-seltext_m = 'Days in P.Period'.
  wa_fcat1-seltext_s = 'Days in P.per'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.


  wa_fcat1-fieldname = 'QTY1'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = 'QTY in Plan period'.
  wa_fcat1-seltext_m = 'QTY in P.Period'.
  wa_fcat1-seltext_s = 'QTY in P.per'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.


  wa_fcat1-fieldname = 'INC'.
  wa_fcat1-tabname   = 'ITAB2'.
  wa_fcat1-seltext_l = ' Increased to QTY'.
  wa_fcat1-seltext_m = 'Increased to QTY'.
  wa_fcat1-seltext_s = 'Inc to QTY'.
  APPEND wa_fcat1 TO it_fcat1.
  CLEAR wa_fcat1.

***********************Layout design
  layout-colwidth_optimize = 'X'.


ENDFORM.


FORM display1.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-cprog
      is_layout          = layout
      it_fieldcat        = it_fcat1
*     I_DEFAULT          = 'U'
      i_save             = 'X'
*     I_CALLBACK_PF_STATUS_SET          = 'SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
    TABLES
      t_outtab           = itab2
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.




FORM fcat2.


  wa_fcat2-fieldname = 'VKORG'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Sales Org'.
  wa_fcat2-seltext_m = 'Sales Org'.
  wa_fcat2-seltext_s = 'S.Org'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.


  wa_fcat2-fieldname = 'VTWEG'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Dist Chanel'.
  wa_fcat2-seltext_m = 'DistChan'.
  wa_fcat2-seltext_s = 'D.Chan'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'SPART'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Division'.
  wa_fcat2-seltext_m = 'Division'.
  wa_fcat2-seltext_s = 'Div'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'BZIRK'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Region'.
  wa_fcat2-seltext_m = 'Region'.
  wa_fcat2-seltext_s = 'Reg'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'WERKS'.
  wa_fcat2-tabname   = 'ITAB1'.
  wa_fcat2-seltext_l = 'Plant'.
  wa_fcat2-seltext_m = 'Plant'.
  wa_fcat2-seltext_s = 'Plnt'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.



  wa_fcat2-fieldname = 'MATNR'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Material'.
  wa_fcat2-seltext_m = 'Material'.
  wa_fcat2-seltext_s = 'Material'.
  wa_fcat2-no_zero   = 'X'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'MAKTX'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Material Name'.
  wa_fcat2-seltext_m = 'Material Name'.
  wa_fcat2-seltext_s = 'Material Nam'.
  wa_fcat2-no_zero   = 'X'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'MATKL'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Material Group'.
  wa_fcat2-seltext_m = 'Mat Grp'.
  wa_fcat2-seltext_s = 'M.Grp'.
  wa_fcat2-no_zero   = 'X'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'EXTWG'.
  wa_fcat2-tabname   = 'ITAB1'.
  wa_fcat2-seltext_l = 'Ext Material Group'.
  wa_fcat2-seltext_m = 'Ext Mat Grp'.
  wa_fcat2-seltext_s = 'E.M.Grp'.
  wa_fcat2-no_zero   = 'X'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'FKIMG'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Quantity'.
  wa_fcat2-seltext_m = 'Quantity'.
  wa_fcat2-seltext_s = 'Quan'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'MEINS'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Unit'.
  wa_fcat2-seltext_m = 'Unit'.
  wa_fcat2-seltext_s = 'UOM'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'NETWR'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Gross Value'.
  wa_fcat2-seltext_m = 'Gross Val'.
  wa_fcat2-seltext_s = 'G.Val'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'WAERK'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Currency'.
  wa_fcat2-seltext_m = 'Currency'.
  wa_fcat2-seltext_s = 'Curr'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'DAYS'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Days in sales period'.
  wa_fcat2-seltext_m = 'Days in S.Period'.
  wa_fcat2-seltext_s = 'Days in S.per'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'QTY'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'QTY/DAY in sales period'.
  wa_fcat2-seltext_m = 'QTY/DAY in S.Period'.
  wa_fcat2-seltext_s = 'QTY/DAY in S.per'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

  wa_fcat2-fieldname = 'DAYS1'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'Days in Plan period'.
  wa_fcat2-seltext_m = 'Days in P.Period'.
  wa_fcat2-seltext_s = 'Days in P.per'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.


  wa_fcat2-fieldname = 'QTY1'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = 'QTY in Plan period'.
  wa_fcat2-seltext_m = 'QTY in P.Period'.
  wa_fcat2-seltext_s = 'QTY in P.per'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.


  wa_fcat2-fieldname = 'INC'.
  wa_fcat2-tabname   = 'ITAB3'.
  wa_fcat2-seltext_l = ' Increased to QTY'.
  wa_fcat2-seltext_m = 'Increased to QTY'.
  wa_fcat2-seltext_s = 'Inc to QTY'.
  APPEND wa_fcat2 TO it_fcat2.
  CLEAR wa_fcat2.

***********************Layout design
  layout-colwidth_optimize = 'X'.


ENDFORM.


FORM display2.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-cprog
      is_layout          = layout
      it_fieldcat        = it_fcat2
*     I_DEFAULT          = 'U'
      i_save             = 'X'
*     I_CALLBACK_PF_STATUS_SET          = 'SET_PF_STATUS'
*     I_CALLBACK_USER_COMMAND           = 'USER_COMMAND'
    TABLES
      t_outtab           = itab3
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM autho.

  AUTHORITY-CHECK OBJECT 'M_MSEG_WWA'
                       ID  'ACTVT' FIELD '03'
                       ID  'WERKS' FIELD so_plant-low.

  IF sy-subrc NE 0.
    MESSAGE 'You are not authorized' TYPE 'E' .
  ENDIF.


  AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
                       ID  'ACTVT' FIELD '03'
                       ID  'VKORG' FIELD so_vkorg-low
                       ID  'VTWEG' FIELD so_vtweg-low
                       ID  'SPART' FIELD so_spart-low.

  IF sy-subrc NE 0.
    MESSAGE 'You are not authorized' TYPE 'E' .
  ENDIF.

ENDFORM.


FORM days_cal.

  LOOP AT so_date1.

    IF so_date1-option = 'BT' AND so_date1-high IS NOT INITIAL AND so_date1-low IS NOT INITIAL.

      days = ( so_date1-high - so_date1-low ) + 1.
      tot_days = tot_days + days.

    ENDIF.

    CLEAR : days, so_date1.
  ENDLOOP.

ENDFORM.

FORM date1_restriction.

* INCLUDE TYPE POOL SSCR
  TYPE-POOLS sscr.

* DEFINE THE OBJECT TO BE PASSED TO THE RESTRICTION PARAMETER
  DATA restrict TYPE sscr_restrict.

* AUXILIARY OBJECTS FOR FILLING RESTRICT
  DATA : optlist TYPE sscr_opt_list,
         sscr    TYPE sscr_ass.

* RESTRICTING THE MATNR SELECTION TO ONLY EQ AND 'BT'.
  optlist-name = 'OBJECTKEY1'.
  optlist-options-bt = 'X'.
  APPEND optlist TO restrict-opt_list_tab.

  sscr-kind = 'S'.
  sscr-name = 'SO_DATE1'.
  sscr-sg_main = 'I'.
  sscr-sg_addy = space.
  sscr-op_main = 'OBJECTKEY1'.
  APPEND sscr TO restrict-ass_tab.

  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
