class ZCL_IM_6MM014B_GATE_ENT_DT definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6MM014B_GATE_ENT_DT
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_MIGO_BADI .
protected section.
*"* protected components of class ZCL_IM_6MM014B_GATE_ENT_DT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6MM014B_GATE_ENT_DT
*"* do not include other source files here!!!

  data GT_EXTDATA type ZTY_T_EXTDATA .
  constants GF_CLASS_ID type MIGO_CLASS_ID value  'MIGO_BADI_IMPLEMENTATION1'. "#EC NOTEXT
  data G_NO_INPUT type XFELD .
  data GS_EXDATA_HEADER type ZST_MIGO_GATE_ENTRY_HEAD .
  data G_CANCEL type XFELD .
  data G_LINE_ID type GOITEM-GLOBAL_COUNTER .
  data GT_ITEM type GOITEM .
  data G_POORD type XFELD .
ENDCLASS.



CLASS ZCL_IM_6MM014B_GATE_ENT_DT IMPLEMENTATION.


method IF_EX_MB_MIGO_BADI~CHECK_HEADER.
  DATA: ls_bapiret TYPE bapiret2.
  data : wa_gt_ent type z6mma_gt_ent_hd.
* Refresh return table
*  REFRESH: et_bapiret2.
* Check header data (W-/E-Messages are useful)
*
*  IF gs_exdata_header-GATEN IS INITIAL.
*    ls_bapiret-type       = 'E'.
*    ls_bapiret-id         = 'M7'.
*    ls_bapiret-number     = '895'.
*    ls_bapiret-message_v1 = 'Enter Gate Entry Number'(001).
*    APPEND ls_bapiret TO et_bapiret2.
*  else.
*
*    select  single * from z6mma_gt_ent_hd into wa_gt_ent where gaten eq gs_exdata_header-gaten.
*    if sy-subrc eq 0.
*      ls_bapiret-type       = 'E'.
*      ls_bapiret-id         = 'M7'.
*      ls_bapiret-number     = '895'.
*      ls_bapiret-message_v1 = 'Enter Gate Entry Number Already attached to ' .
*      ls_bapiret-message_v2 = wa_gt_ent-mblnr.
*      ls_bapiret-message_v3 = wa_gt_ent-mjahr .
*      APPEND ls_bapiret TO et_bapiret2.
*    endif.
*  ENDIF.
DATA:ZVEHNO TYPE  z6mma_gt_ent_hd-VEHNO,
     ZLRNUM TYPE  z6mma_gt_ent_hd-LRNUM,
     ZLRDAT TYPE  z6mma_gt_ent_hd-LRDAT,
     ZREDAT TYPE Z6MMA_GT_ENT_HD-REDAT.

  ZVEHNO = GS_EXDATA_HEADER-VEHNO.
  ZLRNUM = GS_EXDATA_HEADER-LRNUM.
  ZLRDAT = GS_EXDATA_HEADER-LRDAT.
  ZREDAT = GS_EXDATA_HEADER-REDAT.


  EXPORT ZVEHNO FROM ZVEHNO TO MEMORY ID 'ZVEHNO'.
  EXPORT ZLRNUM FROM ZLRNUM TO MEMORY ID 'ZLRNUM'.
  EXPORT ZLRDAT FROM ZLRDAT TO MEMORY ID 'ZLRDAT'.
  EXPORT ZREDAT FROM ZREDAT TO MEMORY ID 'ZREDAT'.

*  if GS_EXDATA_HEADER-VEHNO is INITIAL.
*    MESSAGE 'Please Enter Vehicle Number in Gate Entry Details.' TYPE 'E'.
*  ENDIF.
*
*  if GS_EXDATA_HEADER-LRNUM is INITIAL.
*    MESSAGE 'Please Enter L.R Number in Gate Entry Details.' TYPE 'E'.
*  ENDIF.
*
*  if GS_EXDATA_HEADER-LRDAT is INITIAL.
*    MESSAGE 'Please Enter L.R.Date in Gate Entry Details.' TYPE 'E'.
*  ENDIF.

endmethod.


method IF_EX_MB_MIGO_BADI~CHECK_ITEM.
  DATA: ls_extdata TYPE ZST_MIGO_GATE_ENTRY_ITEM,
        ls_bapiret TYPE bapiret2,
        wa_gt_ent type z6mma_gt_ent_hd,
        wa_mseg type mseg.
  DATA: wa_instance TYPE REF TO ZCL_IM_6MM014B_GATE_ENT_DT.

if g_cancel is INITIAL AND G_POORD NE SPACE.
  IF NOT GT_ITEM IS INITIAL.
    check gt_item-werks eq '1101' or  gt_item-werks eq '2101'.
    if not gs_exdata_header-gaten is INITIAL and gs_exdata_header-gaten+0(4) ne gt_item-werks.
      CONCATENATE GT_ITEM-WERKS gs_exdata_header-gaten INTO gs_exdata_header-gaten.
    endif.
*Refresh return table
    REFRESH: et_bapiret2.
* Read external data


    IF gs_exdata_header-GATEN IS INITIAL.
      ls_bapiret-type       = 'E'.
      ls_bapiret-id         = 'M7'.
      ls_bapiret-number     = '895'.
      ls_bapiret-message_v1 = 'Enter Gate Entry Number'(001).
      APPEND ls_bapiret TO et_bapiret2.
    else.

        select  single * from z6mma_gt_ent_hd into wa_gt_ent where gaten eq gs_exdata_header-gaten.
        if sy-subrc eq 0.
        ls_bapiret-type       = 'E'.
        ls_bapiret-id         = 'M7'.
        ls_bapiret-number     = '895'.
        ls_bapiret-message_v1 = 'Enter Gate Entry Number Already attached to ' .
        ls_bapiret-message_v2 = wa_gt_ent-mblnr.
        ls_bapiret-message_v3 = wa_gt_ent-mjahr .
        APPEND ls_bapiret TO et_bapiret2.
      endif.
    ENDIF.
  ENDIF.
endif.

**Refresh return table
*  REFRESH: et_bapiret2.
** Read external data
*
*
*  IF gs_exdata_header-GATEN IS INITIAL.
*    ls_bapiret-type       = 'E'.
*    ls_bapiret-id         = 'M7'.
*    ls_bapiret-number     = '895'.
*    ls_bapiret-message_v1 = 'Enter Gate Entry Number'(001).
*    APPEND ls_bapiret TO et_bapiret2.
*  else.
*
*    select  single * from z6mma_gt_ent_hd into wa_gt_ent where gaten eq gs_exdata_header-gaten.
*    if sy-subrc eq 0.
*      ls_bapiret-type       = 'E'.
*      ls_bapiret-id         = 'M7'.
*      ls_bapiret-number     = '895'.
*      ls_bapiret-message_v1 = 'Enter Gate Entry Number Already attached to ' .
*      ls_bapiret-message_v2 = wa_gt_ent-mblnr.
*      ls_bapiret-message_v3 = wa_gt_ent-mjahr .
*      APPEND ls_bapiret TO et_bapiret2.
*    endif.
*  ENDIF.
** Refresh return table
*  REFRESH: et_bapiret2.
** Read external data
*  READ TABLE gt_extdata INTO ls_extdata
*       WITH TABLE KEY line_id = i_line_id.
** Check if all entries are done (W-/E-Messages are useful)
*  IF ls_extdata-pksiz IS INITIAL.
*    ls_bapiret-type       = 'E'.
*    ls_bapiret-id         = 'M7'.
*    ls_bapiret-number     = '895'.
*    ls_bapiret-message_v1 = 'Please Enter Packsize '(002).
*    APPEND ls_bapiret TO et_bapiret2.
*  ENDIF.
**  IF ls_extdata-badi_erfme IS INITIAL.
**    ls_bapiret-type       = 'E'.
**    ls_bapiret-id         = 'M7'.
**    ls_bapiret-number     = '895'.
**    ls_bapiret-message_v1 =
**       'Enter unit of measure on external screen'(003).
**    APPEND ls_bapiret TO et_bapiret2.
**  ENDIF.
endmethod.


method IF_EX_MB_MIGO_BADI~HOLD_DATA_DELETE.
* Delete entry from database table
  DELETE FROM migo_badi_hold WHERE guid = i_guid.
endmethod.


method IF_EX_MB_MIGO_BADI~HOLD_DATA_LOAD.
  DATA: ls_migo_badi_hold TYPE migo_badi_hold.
* Select hold data from database
  SELECT SINGLE * FROM migo_badi_hold INTO ls_migo_badi_hold
            WHERE guid = i_guid.
* Get internal structure gs_exdata_header from data buffer
  IMPORT gs_exdata_header TO gs_exdata_header FROM DATA BUFFER
     ls_migo_badi_hold-hold_string_head.
** Get internal table gt_extdata from data buffer
*  IMPORT gt_extdata TO gt_extdata FROM DATA BUFFER
*     ls_migo_badi_hold-hold_string.
endmethod.


method IF_EX_MB_MIGO_BADI~HOLD_DATA_SAVE.
  DATA: ls_migo_badi_hold TYPE migo_badi_hold.

** Store data cluster (detail data) to data buffer
*  EXPORT gt_extdata FROM gt_extdata TO DATA BUFFER
*     ls_migo_badi_hold-hold_string.
* Store data cluster (header data) to data buffer
  EXPORT gs_exdata_header FROM gs_exdata_header TO DATA BUFFER
     ls_migo_badi_hold-hold_string_head.
  ls_migo_badi_hold-guid = i_guid.
* Database update:
  INSERT INTO migo_badi_hold VALUES ls_migo_badi_hold.
  IF sy-subrc <> 0.
    MESSAGE a398(00) WITH 'Error update MIGO_BADI_HOLD'(005).
  ENDIF.
endmethod.


method IF_EX_MB_MIGO_BADI~INIT.
* Regristration of BAdI-Implementation:
* Append class attribute GF_CLASS_ID (='MIGO_BADI_IMPLEMENTATION1') to
* regristration table.
  APPEND gf_class_id TO ct_init.
endmethod.


method IF_EX_MB_MIGO_BADI~LINE_DELETE.
  DELETE TABLE gt_extdata WITH TABLE KEY line_id = i_line_id.
endmethod.


method IF_EX_MB_MIGO_BADI~LINE_MODIFY.
  DATA: ls_extdata_old      TYPE ZST_MIGO_GATE_ENTRY_ITEM,
        ls_extdata_new      TYPE ZST_MIGO_GATE_ENTRY_ITEM,
        ls_migo_badi_exampl TYPE z6mma_gt_ent_dt,
        ls_migo_header      type z6mma_gt_ent_hd,
        l_subrc             TYPE sy-subrc.



* Get external data from internal table:
  READ TABLE gt_extdata INTO ls_extdata_old
    WITH TABLE KEY line_id = i_line_id.
  l_subrc = sy-subrc.
  GT_ITEM = CS_GOITEM.
* Update data in internal table:
  IF l_subrc <> 0.
*   Line is new: If GOITEM has a reference to a material document,
*                the already existing external data can be read.
   IF NOT cs_goitem-mblnr IS INITIAL AND NOT cs_goitem-mjahr IS INITIAL
      AND NOT cs_goitem-zeile IS INITIAL.
      select single * from z6mma_gt_ent_dt into ls_migo_badi_exampl
      where mblnr = cs_goitem-mblnr
               AND mjahr = cs_goitem-mjahr
               AND zeile = cs_goitem-zeile.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_migo_badi_exampl TO ls_extdata_new.
      ENDIF.
    ENDIF.
*    ls_extdata_new-sgtxt  = cs_goitem-sgtxt.
    ls_extdata_new-line_id = i_line_id.
    INSERT ls_extdata_new INTO TABLE gt_extdata.
  ELSE.
*   Line exists: Get external data entered on BAdI-subscreeen, but only
*                if line_modify was called for the item displayed in the
*                'detail tabstrip'.
    check g_line_id = i_line_id.
    CALL FUNCTION 'Z6MM014F_GET_ITEM_DATA'
      IMPORTING
        es_migo_badi_screen_fields = ls_extdata_new.
    ls_extdata_new-line_id = i_line_id.
*    if ls_extdata_new-sgtxt <> ls_extdata_old-sgtxt.
**     Field was changed on external screen
*      cs_goitem-sgtxt = ls_extdata_new-sgtxt.
*    else.
**     Take data from GOITEM
*      ls_extdata_new-sgtxt = cs_goitem-sgtxt.
*    endif.
    MODIFY TABLE gt_extdata FROM ls_extdata_new.
  ENDIF.


*  else.
*
*    select  single * from z6mma_gt_ent_hd into wa_gt_ent where gaten eq gs_exdata_header-gaten.
*    if sy-subrc eq 0.
*      ls_bapiret-type       = 'E'.
*      ls_bapiret-id         = 'M7'.
*      ls_bapiret-number     = '895'.
*      ls_bapiret-message_v1 = 'Enter Gate Entry Number Already attached to ' .
*      ls_bapiret-message_v2 = wa_gt_ent-mblnr.
*      ls_bapiret-message_v3 = wa_gt_ent-mjahr .
*      APPEND ls_bapiret TO et_bapiret2.
*    endif.
*  ENDIF.

endmethod.


  method IF_EX_MB_MIGO_BADI~MAA_LINE_ID_ADJUST.
  endmethod.


method IF_EX_MB_MIGO_BADI~MODE_SET.
* ACTION and REFDOC will discribe the mode of transaction MIGO.
* ----------------------------------------------------------------------
* i_action:
* A01 = Goods receipt
* A02 = Return delivery
* A03 = Cancellation
* A04 = Display
* A05 = Release GR bl.st.
* A06 = Subsequent deliv.
* A07 = Goods issue
*
* i_refdoc:
* R01 = Purchase order
* R02 = Material document
* R03 = Delivery note
* R04 = Inbound delivery
* R05 = Outbound delivery
* R06 = Transport
* R07 = Transport ID code
* R08 = Order
* R09 = Reservation
* R10 = Other GR
*-----------------------------------------------------------------------

* In case of 'DISPLAY' the global field G_NO_INPUT will be set to 'X'.
* The result is that a different external subscreen will be choosen in
* method PBO_DETAIL.
  IF i_action = 'A04' OR i_action = 'A03'.
    g_no_input = 'X'.
  ENDIF.

  IF i_action = 'A01' AND i_REFDOC = 'R01'.
    G_POORD = 'X'.
  ENDIF.
* In case of 'CANCEL' the global field G_CANCEL will be set to 'X'.
* The result is that in method POST_DOCUMENT a different handling is
* used
  IF i_action = 'A03'.
    g_cancel = 'X'.
  ENDIF.
endmethod.


method IF_EX_MB_MIGO_BADI~PAI_DETAIL.
*-----------------------------------------------------------------------
* Changing parameter E_FORCE_CHANGE can be set to 'X'. In this case
* method LINE_MODIFY is called.
* ATTENTION:
* DO NOT SET parameter E_FORCE_CHANGE = ' '. In this case you might
* overwrite parameter E_FORCE_CHANGE of another BAdI implementation.
*-----------------------------------------------------------------------
  DATA: ls_extdata_new TYPE ZST_MIGO_GATE_ENTRY_ITEM,
        ls_extdata_old TYPE ZST_MIGO_GATE_ENTRY_ITEM.

* Only if a line exists
  CHECK i_line_id <> 0.
* Get data from external screen
  CALL FUNCTION 'Z6MM014F_GET_ITEM_DATA'
    IMPORTING
      es_migo_badi_screen_fields = ls_extdata_new.
* Compare new data with old data
  READ TABLE gt_extdata INTO ls_extdata_old
     WITH TABLE KEY line_id = i_line_id.
  ls_extdata_new-line_id = i_line_id.
  IF ls_extdata_old <> ls_extdata_new.
*   If there were any changes, it's obligatory to force MIGO to trigger
*   method LINE_MODIFY.
    e_force_change = 'X'.
  ENDIF.
endmethod.


method IF_EX_MB_MIGO_BADI~PAI_HEADER.
*Get Header data from external screen:
  CALL FUNCTION 'Z6MM014F_GET_HEADER_DATA'
    IMPORTING
      es_migo_badi_header_fields = gs_exdata_header.
endmethod.


method IF_EX_MB_MIGO_BADI~PBO_DETAIL.
*************************************ADDED BY Parmanand on 09.11.2017......
**e_cprog   = 'ZDMS_UPLOAD'.
**  e_dynnr   = '0100'.                     "External fields: Input
**  e_heading = 'DMS Attachment List'.
*** Export data to function group (for display on subscreen)
**  CALL FUNCTION 'MIGO_BADI_EXAMPLE_PUT_HEADER'
**    EXPORTING
**      is_migo_badi_header_fields = gs_exdata_header.






  DATA: ls_extdata TYPE ZST_MIGO_GATE_ENTRY_ITEM.
* This check is obligatory, otherwise the program flow is incorrect
* (If there would be more than one implementation of BAdI MB_MIGO_BADI,
*  only one subscreen would be displayed).
  CHECK i_class_id = gf_class_id.
* Show screen only if there is an item
  CHECK i_line_id <> 0.
* External subscreen:
* The content of global field G_NO_INPUT (set in method MODE_SET) will
* influence the number of external subsreen:
  if g_no_input is initial.
    e_cprog   = 'SAPLZ6MM_FG_MIGO'.
    e_dynnr   = '0001'.                     "External fields: Input
    e_heading = 'Gate Entry Item Data'(004).
  else.
    e_cprog   = 'SAPLZ6MM_FG_MIGO'.
    e_dynnr   = '0002'.                     "External fields: Display
    e_heading = 'Gate Entry Item Data'(004).
  endif.
* Set G_LINE_ID (= line_id of item displayed on detail-tabstrip)
  g_line_id = i_line_id.
* Read data
  READ TABLE gt_extdata INTO ls_extdata
     WITH TABLE KEY line_id = i_line_id.
* Export data to function group (for display on subscreen)
  CALL FUNCTION 'Z6MM014F_PUT_ITEM_DATA'
    EXPORTING
      is_migo_badi_screen_fields = ls_extdata.







endmethod.


method IF_EX_MB_MIGO_BADI~PBO_HEADER.
***************************************ADDED BY Parmanand on 09.11.2017......
**IF g_no_input IS INITIAL.
**  e_cprog   = 'ZDMS_UPLOAD'.
**  e_dynnr   = '0100'.                     "External fields: Input
**  e_heading = 'DMS Attachment List'.
**  ELSE.
**
**  e_cprog   = 'ZDMS_UPLOAD'.
**  e_dynnr   = '0100'.                     "External fields: Input
**  e_heading = 'DMS Attachment List'.
**  endif.
*** Export data to function group (for display on subscreen)
**  CALL FUNCTION 'MIGO_BADI_EXAMPLE_PUT_HEADER'
**    EXPORTING
**      is_migo_badi_header_fields = gs_exdata_header.
*ENDIF.
* This check is obligatory, otherwise the program flow is incorrect
* (If there would be more than one implementation of BAdI MB_MIGO_BADI,
* only one subscreen would be displayed).
  CHECK i_class_id = gf_class_id.

* External subscreen:
* The content of global field G_NO_INPUT (set in method MODE_SET) will
* influence the number of external subsreen:
  IF g_no_input IS INITIAL.
    e_cprog   = 'SAPLZ6MM_FG_MIGO'.
    e_dynnr   = '0003'.                     "External fields: Input
    e_heading = 'Gate Entry Details'(004).
  ELSE.
    e_cprog   = 'SAPLZ6MM_FG_MIGO'.
    e_dynnr   = '0004'.                     "External fields: Display
    e_heading = 'Gate Entry Details'(004).
  ENDIF.

* Export data to function group (for display on subscreen)
  CALL FUNCTION 'Z6MM014F_PUT_HEADER_DATA'
    EXPORTING
      is_migo_badi_header_fields = gs_exdata_header.
endmethod.


method IF_EX_MB_MIGO_BADI~POST_DOCUMENT.
*&********************************************************&*
*& TECHNICAL CONSULTANT : PUNAM S                         &*
*& MODULE NAME          : MM                              &*
*& PROGRAM TYPE         : BADI                            &*
*& CREATE DATE          : 01.10.2015                      &*
*& TRANSPORT NO         : IRDK920978                      &*
*& DESCRIPTION          : FRESH STOCK EMAIL NOTIFICATION  &*
*&********************************************************&*


DATA: LS_MIGO_BADI_EXAMPLE TYPE Z6MMA_GT_ENT_DT,
      LT_MIGO_BADI_EXAMPLE TYPE TABLE OF Z6MMA_GT_ENT_DT,
      LS_EXTDATA           TYPE ZST_MIGO_GATE_ENTRY_ITEM,
      LS_XMSEG             TYPE MSEG.

FIELD-SYMBOLS: <GT_EXTDATA> TYPE ZST_MIGO_GATE_ENTRY_ITEM.

DATA: IT_LIKP TYPE TABLE OF LIKP.

DATA: WA_MSEG LIKE LINE OF IT_MSEG ,
      WA_LIKP LIKE LINE OF IT_LIKP ,

      IT_M  LIKE IT_MSEG,
      WA_M  LIKE LINE OF IT_MSEG,
      IT_M2 LIKE  IT_MSEG.


* Data Declarations
  DATA: LT_MAILSUBJECT     TYPE SODOCCHGI1.
  DATA: LT_MAILRECIPIENTS  TYPE STANDARD TABLE OF SOMLREC90,
        WA_MAILRECIPIENTS  LIKE LINE OF LT_MAILRECIPIENTS  .

  DATA: LT_MAILTXT  TYPE STANDARD TABLE OF SOLI ,
        WA_MAILTXT  LIKE LINE OF LT_MAILTXT   ,
        T_HTML         TYPE BCSY_TEXT.

  DATA: ZMENGE(18), ZSPART TYPE MARA-SPART , ZADRNR TYPE KNA1-ADRNR ,
        ZLABST TYPE MARD-LABST , ZSTOCK TYPE MARD-LABST , ZZSTOCK(18), ZMAKTX TYPE MAKT-MAKTX.

DATA: IT_DEPO  TYPE STANDARD TABLE OF ZATR_USER_DM,
      WA_DEPO  LIKE LINE OF IT_DEPO,
      ZPERNR   TYPE PA0105-PERNR , ZNAME1 TYPE T001W-NAME1,
      IT_EMAIL TYPE STANDARD TABLE OF ZATR_USER_M,
      WA_EMAIL LIKE LINE OF IT_EMAIL.


*****************************************************************
*           Data Declarations for HTML email code               *
*****************************************************************

  DATA:   T_HEADER TYPE STANDARD TABLE OF W3HEAD,
          S_HEADER LIKE LINE OF T_HEADER,                 "HEADER
          T_FIELDS TYPE STANDARD TABLE OF W3FIELDS,
          S_FIELDS LIKE LINE OF T_FIELDS,                 "FIELDS
          T_HTML1 TYPE STANDARD TABLE OF W3HTML,
          S_HTML1 LIKE LINE OF T_HTML1,                   "HTML
          WA_HEADER TYPE W3HEAD,
          W_HEAD TYPE W3HEAD,
          TABLE_ATTR TYPE W3HTML.

 DATA : BEGIN OF WA_FINAL,
        MATNR TYPE MARA-MATNR,
        MAKTX TYPE MAKT-MAKTX,
        MENGE TYPE MSEG-MENGE,
        ZZSTOCK(18),
        END OF WA_FINAL.

DATA     IT_FINAL LIKE TABLE OF WA_FINAL.
DATA :   IT_FCAT TYPE LVC_T_FCAT,
         WA_FCAT LIKE LINE OF IT_FCAT. " FIELDCATALOG

DATA DEPO TYPE CHAR80.

*******Processing data

* Transaction MIGO will now post a material document.
* Any errors here MUST be issued as A-message (better: X-message)

* Copy data from material document into internal table
  LOOP AT gt_extdata INTO ls_extdata.
    IF g_cancel IS INITIAL.
      READ TABLE it_mseg INTO ls_xmseg
         WITH KEY line_id = ls_extdata-line_id.
    ELSE.
      READ TABLE it_mseg INTO ls_xmseg
         WITH KEY smbln = ls_extdata-mblnr
                  smblp = ls_extdata-zeile
                  sjahr = ls_extdata-mjahr.
    ENDIF.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING ls_extdata TO ls_migo_badi_example.
      MOVE-CORRESPONDING ls_xmseg   TO ls_migo_badi_example.
      APPEND ls_migo_badi_example TO lt_migo_badi_example.
    ENDIF.
  ENDLOOP.
* The data from external detail screen can be saved now:
  CHECK gt_extdata IS NOT INITIAL.
  CALL FUNCTION 'Z6MM014F_UPDATE_ITEM_DATA' IN UPDATE TASK
    TABLES
      it_migo_badi_example = lt_migo_badi_example.
* The data from external header screen can be saved now:
  MOVE-CORRESPONDING is_mkpf TO gs_exdata_header.
*  CONCATENATE LS_XMSEG-WERKS gs_exdata_header-GATEN INTO gs_exdata_header-GATEN.
  CALL FUNCTION 'Z6MM014F_UPDATE_HEADER' IN UPDATE TASK
    EXPORTING
      is_migo_badi_header_fields = gs_exdata_header.

IF sy-tcode = 'MIGO_GR' and IS_MKPF-VGART = 'WE' AND IS_MKPF-BLART = 'WE'.
  IF IS_MKPF-mblnr is not initial.
  read table it_mseg into wa_mseg index 1.
  IF sy-subrc = 0.
    CLEAR: ZSPART.
    SELECT SINGLE spart FROM mara INTO zspart WHERE matnr = WA_MSEG-MATNR.


    IF wa_mseg-werks(02) = '14' and wa_mseg-bwart = '101' .
      if wa_mseg-lgort = '1501' or wa_mseg-lgort = '1601'.
      if wa_mseg-werks not between '1421' and '1429'.
*      select single * from likp into wa_likp where vbeln = is_mkpf-XBLNR.
*        IF sy-subrc = 0.
*          if wa_likp-vstel(02) <> '14'.
* send mail to TM
*********************************************************************************************
*           select * from  TVBVK into table it_tvbvk where vkbur =  wa_mseg-werks.
           select * from zatr_user_dm into table it_depo where vkbur =  wa_mseg-werks.

           IF sy-subrc = 0.

*             select * from zatr_user_tmap
*               into corresponding fields of table it_tmap
*               for all entries in it_tvbvk
*               where vkgrp = it_tvbvk-vkgrp.

             select * from ZATR_USER_M
               into corresponding fields of table it_email
               for all entries in it_depo
               where user_id = it_depo-user_id
               and status = ''
               AND SPART = ZSPART.

             LOOP AT it_email into wa_email.
*                   clear: zpernr.
*                   select single user_id from zatr_user_tmap into zpernr where vkgrp = wa_tvbvk-vkgrp.


*                   SELECT SINGLE USRID_LONG FROM PA0105
*                   INTO WA_MAILRECIPIENTS-RECEIVER
*                   WHERE PERNR = wa_tmap-user_id
*                   AND SUBTY = '0010'
*                   AND ENDDA = '99991231'.


                  CLEAR: WA_MAILRECIPIENTS .
                  WA_MAILRECIPIENTS-REC_TYPE  = 'U'.
                  WA_MAILRECIPIENTS-RECEIVER = wa_email-EMAIL_ID."'pshinde-icc@modi.com'.
                  APPEND WA_MAILRECIPIENTS TO LT_MAILRECIPIENTS .
                  CLEAR WA_MAILRECIPIENTS.

             ENDLOOP.

           ENDIF.

           sort LT_MAILRECIPIENTS by  RECEIVER.
           delete adjacent duplicates from LT_MAILRECIPIENTS.
           if LT_MAILRECIPIENTS is not initial.
*            IF WA_SMS_STO-EMAIL IS NOT INITIAL.

*              CONDENSE WA_SMS_STO-EMAIL.

*                  CLEAR: LT_MAILRECIPIENTS, WA_MAILRECIPIENTS , WA_MAILTXT , LT_MAILTXT , LT_MAILSUBJECT.
*                  WA_MAILRECIPIENTS-REC_TYPE  = 'U'.
*                  WA_MAILRECIPIENTS-RECEIVER = WA_SMS_STO-EMAIL."'pshinde-icc@modi.com'.
*                  APPEND WA_MAILRECIPIENTS TO LT_MAILRECIPIENTS .
*                  CLEAR WA_MAILRECIPIENTS.

                  CLEAR: LT_MAILSUBJECT-OBJ_DESCR.
                  LT_MAILSUBJECT-OBJ_NAME = 'GOODSRECPT'.
                  LT_MAILSUBJECT-OBJ_LANGU = SY-LANGU.
                  clear: zname1.
                  select single name1 from t001w into zname1 where werks = wa_mseg-werks and spras = sy-langu.


                  CONCATENATE ' Intimation on Fresh Stock Arrival at Depot:' wa_mseg-werks zname1
                  INTO LT_MAILSUBJECT-OBJ_DESCR SEPARATED BY SPACE.
*
*                  WA_MAILTXT = 'Dear INDOFIL TM,'.
*                  APPEND WA_MAILTXT TO LT_MAILTXT. CLEAR WA_MAILTXT.
*
*
*                  WA_MAILTXT = 'Kindly note fresh stock of following materials has been received at Depot:' .
*                  APPEND WA_MAILTXT TO LT_MAILTXT. CLEAR WA_MAILTXT.
*
*                  CONCATENATE wa_mseg-werks zname1 INTO WA_MAILTXT SEPARATED BY SPACE.
*                  APPEND WA_MAILTXT TO LT_MAILTXT. CLEAR WA_MAILTXT.
*
*                  WA_MAILTXT = '  '.
*                  APPEND WA_MAILTXT TO LT_MAILTXT. CLEAR WA_MAILTXT.
*
*                  WA_MAILTXT = '|-------------------------------------------------------------------------------------|'.
*                  APPEND WA_MAILTXT TO LT_MAILTXT. CLEAR WA_MAILTXT.
*
*                  WA_MAILTXT = '|  Material  |         Description           |     Receipt QTY      |    Total Stock  |'.
*                  APPEND WA_MAILTXT TO LT_MAILTXT. CLEAR WA_MAILTXT.
*
*                  WA_MAILTXT = '|-------------------------------------------------------------------------------------|'.
*                  APPEND WA_MAILTXT TO LT_MAILTXT. CLEAR WA_MAILTXT.


it_m2 = it_mseg.
sort it_m2 by werks matnr .

LOOP AT IT_M2 INTO WA_MSEG.
wa_m-matnr =  wa_mseg-matnr .
move wa_mseg-werks to wa_m-werks.
move wa_mseg-menge to wa_m-menge.
collect wa_m into it_m.
clear: wa_m.
endloop.

clear: wa_m.

 LOOP AT IT_M INTO WA_M.

 CLEAR: ZLABST , ZSTOCK, ZZSTOCK , ZMAKTX.
*{   REPLACE        SBXK900087                                        3
*\ SELECT SINGLE MAKTX FROM MAKT INTO ZMAKTX WHERE MATNR = WA_M-MATNR.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Wednesday, October 11
* Changed By - 6010859 - SaurabhK
* Purpose    - Simplification list - 2215424 - MATNR length change 18 -> 40
* Solution   - Used pseduo comments as usage is ok
* TR         - SBXK900028 - S4H: S_K: Simplification List: 03.10.2018
*--------------------------------------------------------------------*
 SELECT SINGLE MAKTX FROM MAKT INTO ZMAKTX WHERE MATNR = WA_M-MATNR and spras eq 'E'.
*}   REPLACE

                  select single LABST from MARD into ZLABST
                  where MATNR = WA_M-MATNR
                     AND WERKS = WA_M-WERKS
                     AND ( LGORT = '1501' OR  LGORT = '1601' ) .

 CLEAR: ZMENGE.
   ZSTOCK = WA_M-MENGE + ZLABST.
   ZZSTOCK = ZSTOCK.
   ZMENGE =  WA_M-MENGE.

*{   REPLACE        SBXK900087                                        1
*\ CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
 CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT' "#EC CI_FLDEXT_OK[2215424]
*}   REPLACE
                  exporting
        INPUT  = WA_M-MATNR
      IMPORTING
        OUTPUT = WA_M-MATNR.

 WA_FINAL-MATNR   = WA_M-MATNR.
 WA_FINAL-MAKTX   = ZMAKTX.
 WA_FINAL-MENGE   = ZMENGE.
 WA_FINAL-ZZSTOCK = ZZSTOCK.
 APPEND WA_FINAL TO IT_FINAL.
 CLEAR : WA_FINAL, ZMAKTX, ZMENGE, ZZSTOCK.

ENDLOOP.


*********POPULATE FIELDCATALOG*********************************

  WA_FCAT-COLTEXT = 'Material Number'.
  APPEND WA_FCAT TO IT_FCAT.
  WA_FCAT-COLTEXT = 'Material Description'.
  APPEND WA_FCAT TO IT_FCAT.
  WA_FCAT-COLTEXT = 'Receipt Qunatity'.
  APPEND WA_FCAT TO IT_FCAT.
  WA_FCAT-COLTEXT = 'Total Stock'.
  APPEND WA_FCAT TO IT_FCAT.

*******FILL THE COLUMN HEADINGS AND PROPERTIES*******
  LOOP AT IT_FCAT INTO WA_FCAT.

    W_HEAD-TEXT = WA_FCAT-COLTEXT.

    CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'    "POPULATE THE COLUMN HEADINGS
      EXPORTING
        FIELD_NR = SY-TABIX
        TEXT     = W_HEAD-TEXT
        FGCOLOR  = 'BLACK'
        BGCOLOR  = '#69C374'
        SIZE     = '2'
        FONT     = 'LUCIDA GRANDE'
      TABLES
        HEADER   = T_HEADER.

    CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'    "POPULATE COLUMN PROPERTIES
      EXPORTING
        FIELD_NR  = SY-TABIX
        FGCOLOR   = 'BLACK'
        JUSTIFIED = 'CENTER'
        SIZE      = '2'
        FONT      = 'LUCIDA GRANDE'
      TABLES
        FIELDS   = T_FIELDS.

  ENDLOOP.

  TABLE_ATTR = 'WIDTH=530 BORDER=0 CELLSPACING=0  ALIGN=CENTER ALIGN=TOP' .

*{   REPLACE        SBXK900087                                        2
*\   CALL FUNCTION 'WWW_ITAB_TO_HTML'
   CALL FUNCTION 'WWW_ITAB_TO_HTML' "#EC CI_FLDEXT_OK[2215424]
*}   REPLACE
                exporting
      TABLE_HEADER     = WA_HEADER
      TABLE_ATTRIBUTES = TABLE_ATTR
    TABLES
      HTML             = T_HTML1
      FIELDS           = T_FIELDS
      ROW_HEADER       = T_HEADER
      ITABLE           = IT_FINAL.


**************CREATE HTML BODY ************
  append text-401 to t_html.
  append text-402 to t_html.
  append text-403 to t_html.
  append text-404 to t_html.
  append text-405 to t_html.
  append text-406 to t_html.
  append text-407 to t_html.
  append text-408 to t_html.
  append text-409 to t_html.
  append text-410 to t_html.
  append text-411 to t_html.
  append text-412 to t_html.
  append text-413 to t_html.
  append text-414 to t_html.
  append text-415 to t_html.
  append text-416 to t_html.
  append text-417 to t_html.
  append text-418 to t_html.
  append text-419 to t_html.
  append text-420 to t_html.
  append text-421 to t_html.
  append text-422 to t_html.
  append text-423 to t_html.
  append text-424 to t_html.
  CONCATENATE WA_MSEG-WERKS ZNAME1 '.' '<BR><BR>GR Number :' IS_MKPF-MBLNR '<BR> Year:' IS_MKPF-MJAHR '<BR>' INTO
  DEPO separated by space.
  APPEND DEPO TO t_html.
  append text-425 to t_html.
  loop at t_html1 into s_html1.         " HTML1 - SALES DOCUMENT ITEM DETAILS IN HTML FORMAT
  append s_html1 to t_html.
  endloop.
  append text-428 to t_html.
  append text-429 to t_html.
  append text-430 to t_html.
  append text-431 to t_html.
  append text-432 to t_html.
  append text-433 to t_html.
  append text-434 to t_html.
  append text-435 to t_html.
  append text-436 to t_html.
  append text-437 to t_html.
  append text-438 to t_html.
  append text-439 to t_html.
  append text-440 to t_html.

  append text-441 to t_html.
  append text-442 to t_html.
  append text-443 to t_html.
  append text-444 to t_html.
  append text-445 to t_html.
  append text-446 to t_html.
  append text-447 to t_html.
  append text-448 to t_html.
  append text-449 to t_html.
  append text-450 to t_html.
  append text-451 to t_html.
  append text-452 to t_html.
  append text-453 to t_html.
  append text-454 to t_html.
  append text-455 to t_html.
  append text-456 to t_html.
  append text-457 to t_html.
  append text-458 to t_html.
  append text-459 to t_html.
  append text-460 to t_html.

*********** Send Mail

                  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
                    EXPORTING
                      DOCUMENT_DATA              = LT_MAILSUBJECT
                      DOCUMENT_TYPE              = 'HTM'
                    TABLES
                      OBJECT_CONTENT             = t_html[]
                      RECEIVERS                  = LT_MAILRECIPIENTS
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
*                    COMMIT WORK.
*   Push mail out from SAP outbox
*                    SUBMIT RSCONN01 WITH MODE = 'INT' AND RETURN.
                  ENDIF.
*       endif.
*            ENDIF.

******************************************************************************************************
          ENDIF.
*        else.
*
*        ENDIF.
      ENDIF.
     endif.
    endif.
  ENDIF.
ENDIF.
endif.


endmethod.


  method IF_EX_MB_MIGO_BADI~PROPOSE_SERIALNUMBERS.
  endmethod.


method IF_EX_MB_MIGO_BADI~PUBLISH_MATERIAL_ITEM.

endmethod.


method IF_EX_MB_MIGO_BADI~RESET.
* Clear all internal data:
  CLEAR:
         gt_extdata,
         g_no_input,
         gs_exdata_header,
         g_cancel,
         g_line_id.
endmethod.


method IF_EX_MB_MIGO_BADI~STATUS_AND_HEADER.
* In case of 'Display Material document' select external header data
  IF NOT g_no_input IS INITIAL.
    SELECT SINGLE * FROM Z6MMA_GT_ENT_HD INTO gs_exdata_header
       WHERE mblnr = is_gohead-mblnr
       AND   mjahr = is_gohead-mjahr.
  ENDIF.
endmethod.
ENDCLASS.
