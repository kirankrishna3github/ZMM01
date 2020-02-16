***********************************************************************
*  Program Name : Z6MM013R_RGP_NRGP_RPT                               *
*  Program Title: Gate Pass Report                                    *
*  Program type : Report Program                                      *
*  Functional Consultant : Girish Jamdar                              *
*  Technical  Consultant : Arun Pandey                                *
*  Create Date  : 26.10.2009                                          *
*  Request No.  : FEDK901207                                          *
*  T.Code       : zmm013
*  Description  : Gate Pass Rpt                                       *
*----------------------------------------------------------------------
*  Change History                                                     *
*----------------------------------------------------------------------
* Mod.# | Date      | Author         | Desc, Reference & CTS          *
*----------------------------------------------------------------------
*  1    |23/12/2014 | Pradeep Kodinagula | Added 'Overdue by' coloumn *
***********************************************************************

REPORT  z6mm013r_rgp_nrgp_rpt.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Gate Pass Report
* OBJECT TYPE       : Report Program   FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna Konda
*      CREATION DATE: 18.06.2010
*        DEV REQUEST: IRDK900078
*              TCODE: zmm005_6
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 12/10/2015
*   REASON FOR CHANGE: Add Authorization
*   REQUEST #: IRDK920989
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 12/10/2015
*   REASON FOR CHANGE: Add Authorization
*   REQUEST #: IRDK921290
* --------------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*   Table Declaration
*&---------------------------------------------------------------------*
TABLES: z6mma_rgpnrgp_hd, z6mma_rgpnrgp_dt, t001w, lfa1, kna1.

*&---------------------------------------------------------------------*
*   type pool Declaration
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.

*&---------------------------------------------------------------------*
*   Types Declaration
*&---------------------------------------------------------------------*
TYPES : BEGIN OF typ_final,
          gyear	TYPE zzyear,
          gtype	TYPE zzgtype,
          werks	TYPE werks_d,
          gno	TYPE zzgno,
          gino  TYPE zzgino,
          carrier	TYPE zzcarrier,
          vechical_no	TYPE zzvechical_no,
          kunnr	TYPE kunnr,
          lifnr	TYPE lifnr,
          matnr	TYPE matnr,
          maktx	TYPE maktx,
          o_qty	TYPE zzo_qty,
          r_qty	TYPE zzr_qty,
          p_qty TYPE zzr_qty,
          uom	TYPE meins,
          due_date  TYPE sy-datum,
          overdue_by(10) TYPE c,
          reason  TYPE zzreason,
          remark  TYPE zzremark,
          reqs TYPE zzreqs,
          status  TYPE zzstat,
          loekz	TYPE oii_delind,
          erdat TYPE erdat,
          app_stat  TYPE zza_stat,
          ernam  TYPE ernam,
          name1 TYPE lfa1-name1,
*          ind TYPE c,
        END OF typ_final.

DATA :  o_qty1(16)  TYPE c,
        r_qty1(16)  TYPE c,
        p_qty1(16) TYPE c.

TYPES: BEGIN OF ty_lfa1,
      lifnr TYPE lfa1-lifnr,
      name1 TYPE lfa1-name1,
      END OF ty_lfa1.
*&---------------------------------------------------------------------*
*  Internal Table Declaration
*&---------------------------------------------------------------------*
DATA : gt_hd     TYPE STANDARD TABLE OF z6mma_rgpnrgp_hd.
DATA : gt_dtl   TYPE STANDARD TABLE OF z6mma_rgpnrgp_dt.
DATA : gt_final TYPE STANDARD TABLE OF typ_final.
DATA : it_lfa1 TYPE STANDARD TABLE OF ty_lfa1,
       wa_lfa1 TYPE ty_lfa1.
DATA:  bdcdata      LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*&---------------------------------------------------------------------*
*  Structures Declaration
*&---------------------------------------------------------------------*
DATA : gs_hd  TYPE z6mma_rgpnrgp_hd.
DATA : gs_dtl  TYPE z6mma_rgpnrgp_dt.
DATA : gs_final TYPE typ_final.
DATA : it_user TYPE RANGE OF zatr_user_tmap-user_id,
       wa_user LIKE LINE OF it_user.
*&---------------------------------------------------------------------*
*  Data Declaration
*&---------------------------------------------------------------------*
******************  ALV Declaration
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA: gs_fieldcat TYPE slis_fieldcat_alv.

DATA: gt_events TYPE slis_t_event WITH HEADER LINE.
DATA: gs_events TYPE slis_alv_event.                        "#EC NEEDED

DATA: gt_comment TYPE slis_t_listheader.                    "#EC NEEDED
DATA: gs_comment TYPE slis_listheader.                      "#EC NEEDED

DATA: layout   TYPE slis_layout_alv,
      t_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE',
      t_list_top_of_page TYPE slis_t_listheader.

DATA : alv_print        TYPE slis_print_alv,                "#EC NEEDED
       alv_detail_func(30) TYPE c.                          "#EC NEEDED

DATA : counter TYPE i.

DATA : gv_bedat TYPE bedat,
       gv_flag  TYPE c.

******************  VARIANT Declaration
DATA : gv_save(1) TYPE c,
       gv_variant LIKE disvariant,
       gx_variant LIKE disvariant.
DATA : g_tabname_header TYPE slis_tabname,
       g_tabname_item   TYPE slis_tabname,
       gv_exit(1) TYPE c.
DATA: it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
             WITH HEADER LINE,
      it_attach TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
             WITH HEADER LINE,
     t_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
     t_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
     t_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
     t_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
     t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
     w_cnt TYPE i,
     w_sent_all(1) TYPE c,
     w_doc_data LIKE sodocchgi1,
     gd_error    TYPE sy-subrc,
     gd_reciever TYPE sy-subrc,
     line LIKE line,
     success TYPE sy-subrc.
DATA :  result_date TYPE sy-datum.
CONSTANTS:con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
          con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

DATA : it_zfi044_emailid_to TYPE STANDARD TABLE OF zfi044_emailid,
       wa_zfi044_emailid_to TYPE zfi044_emailid,
       it_zfi044_emailid_cc TYPE STANDARD TABLE OF zfi044_emailid,
       wa_zfi044_emailid_cc TYPE zfi044_emailid,
       lv_emailaddr TYPE adr6-smtp_addr,
       str TYPE string ,
       str_drft TYPE string,
       str1 TYPE string ,
       str2 TYPE string,
       reclist   LIKE somlreci1 OCCURS 5 WITH HEADER LINE.
DATA : ld_mtitle LIKE sodocchgi1-obj_descr,
       ld_email LIKE  somlreci1-receiver,
      ld_format TYPE  so_obj_tp ,
      ld_attdescription TYPE  so_obj_nam ,
      ld_attfilename TYPE  so_obj_des ,
      ld_sender_address LIKE  soextreci1-receiver,
      ld_sender_address_type LIKE  soextreci1-adr_typ,
      ld_receiver LIKE  sy-subrc.
DATA: objpack   LIKE sopcklsti1 OCCURS 2 WITH HEADER LINE.
DATA: objhead   LIKE solisti1 OCCURS 1 WITH HEADER LINE.
DATA: objtxt    LIKE solisti1 OCCURS 10 WITH HEADER LINE.
DATA: doc_chng  LIKE sodocchgi1.
DATA: tab_lines LIKE sy-tabix.
DATA : gv_erdat(10) TYPE c,
      gv_due_date(10) TYPE c .

DATA: start_dt(10) , fyear(04).
DATA: p TYPE i.
DATA: p1(02).
DATA: repid TYPE sy-repid.

DATA : filename(10000) TYPE c,
       line_number TYPE i,
       ld_error    TYPE sy-subrc.
TYPES : BEGIN OF str_status,
  zza_stat TYPE zza_stat,
 status(10) TYPE c,
  END OF str_status.
DATA : it_stat TYPE STANDARD TABLE OF str_status WITH HEADER LINE.
DATA: lwa_dfies TYPE dfies.

DATA h_field_wa LIKE dfies.
DATA h_field_tab LIKE dfies OCCURS 0 WITH HEADER LINE.
DATA h_dselc LIKE dselc OCCURS 0 WITH HEADER LINE.

DATA: lv_werks_auth_flg TYPE c VALUE ''.  " Auth. Flag for Receiving Plant

*&---------------------------------------------------------------------*
* Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE text-000.
PARAMETERS : p_gtype TYPE zzgtype OBLIGATORY.
SELECT-OPTIONS: s_werk FOR t001w-werks OBLIGATORY,
                s_lifnr FOR lfa1-lifnr,
                s_kunnr FOR kna1-kunnr,
                s_erdat FOR z6mma_rgpnrgp_hd-erdat,
                s_ernam FOR z6mma_rgpnrgp_hd-ernam,
                p_stat FOR z6mma_rgpnrgp_hd-app_stat.
SELECTION-SCREEN END OF BLOCK b10.

SELECTION-SCREEN BEGIN OF BLOCK b12 WITH FRAME TITLE text-003.
PARAMETERS     : p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b12.
SELECTION-SCREEN BEGIN OF BLOCK b13 WITH FRAME TITLE text-001.
PARAMETERS:  p_all   RADIOBUTTON GROUP rad1 DEFAULT 'X' ,
             p_pend   RADIOBUTTON GROUP rad1 ,
             p_email  RADIOBUTTON GROUP rad1,
             p_pmail  RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b13.

*&---------------------------------------------------------------------*
* Initialization
*&---------------------------------------------------------------------*

INITIALIZATION.
**************code for default date display on selection screen *******

  CLEAR: p, p1.
  p = sy-datum+04(02).
  IF p >= 4 AND p <= 12.
    CONCATENATE sy-datum(04) '0401' INTO start_dt.
  ELSEIF p <= 3 AND p >= 1 .
    fyear = sy-datum(04) - 1.
    CONCATENATE  fyear '0401' INTO start_dt.
  ENDIF.
*  s_erdat-sign = 'I'.
*  s_erdat-option = 'BT'.
*  s_erdat-low = '20140401'."start_dt. "pravin commented 18/08/2015
*  s_erdat-high = sy-datum.
*  APPEND  s_erdat.

**************************end of code *********************************
  PERFORM zf_build_layout .
  PERFORM zf_initialize_variant.

*&---------------------------------------------------------------------*
* AT Selection Screen
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM zf_pai_of_selection_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM zf_f4_for_variant.



*&---------------------------------------------------------------------*
*&      Form  f_fieldinfo_get
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FU_TABNAME     text
*      -->FU_FIELDNAME   text
*      -->FWA_FIELD_TAB  text
*----------------------------------------------------------------------*
FORM f_fieldinfo_get USING fu_tabname
fu_fieldname
CHANGING fwa_field_tab.


  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = fu_tabname
      fieldname      = fu_fieldname
      lfieldname     = fu_fieldname
    IMPORTING
      dfies_wa       = fwa_field_tab
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM. " f_fieldinfo_get

*&---------------------------------------------------------------------*
* Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
*  IF s_erdat[] IS INITIAL."pravin commented 18/08/2015
*    s_erdat-sign = 'I'.
*    s_erdat-option = 'BT'.
*    s_erdat-low = '20140401'."start_dt.
*    s_erdat-high = sy-datum.
*    APPEND  s_erdat.
*  ENDIF.
  PERFORM check_auth_obj.
  PERFORM zf_fetch_data.
  PERFORM zf_manipulate_data.
  IF p_email <> 'X' AND  p_pmail <> 'X'.
***** Start Code: Added by CS on 12.10.2015 for Authorization. *****
    IF gt_final[] IS INITIAL.
      IF lv_werks_auth_flg = 'X'.
        MESSAGE 'Missing Authorization for Plant.' TYPE 'I'.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ELSE.
      IF lv_werks_auth_flg = 'X'.
        MESSAGE 'Missing Authorization for Plant.' TYPE 'S' DISPLAY LIKE 'W'.
*      LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
***** End Code: Added by CS on 12.10.2015 for Authorization. *****
    PERFORM zf_get_events.
    PERFORM zf_sub_comment_build USING t_list_top_of_page.
    PERFORM zf_alv_fieldcat.
    PERFORM zf_alv_display.
  ELSEIF p_email EQ 'X'.
    PERFORM zemail_trigger.
  ENDIF.
  IF p_pmail EQ 'X'.
    PERFORM pending_email.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  ZF_FETCH_DATA
*&---------------------------------------------------------------------*
FORM zf_fetch_data .


  IF NOT s_ernam IS INITIAL.
    SELECT * FROM z6mma_rgpnrgp_hd
             INTO TABLE gt_hd
             WHERE werks IN s_werk
               AND gtype = p_gtype
               AND kunnr IN s_kunnr
               AND lifnr IN s_lifnr
               AND erdat IN s_erdat
               AND ernam IN s_ernam.
  ELSE.
    SELECT * FROM z6mma_rgpnrgp_hd
             INTO TABLE gt_hd
             WHERE werks IN s_werk
               AND gtype = p_gtype
               AND kunnr IN s_kunnr
               AND lifnr IN s_lifnr
               AND erdat IN s_erdat.
  ENDIF.


  IF NOT gt_hd[] IS INITIAL.
    SELECT * FROM z6mma_rgpnrgp_dt
             INTO TABLE gt_dtl
             FOR ALL ENTRIES IN gt_hd
             WHERE werks = gt_hd-werks
               AND gtype = gt_hd-gtype
               AND gyear = gt_hd-gyear
               AND gno   = gt_hd-gno.
    """""""""code added by sachin
    SELECT lifnr name1
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN  gt_hd
    WHERE lifnr = gt_hd-lifnr.
    """"""""""""end of added code
  ENDIF.
ENDFORM.                    " ZF_FETCH_DATA

*&---------------------------------------------------------------------*
*&      Form  ZF_MANIPULATE_DATA
*&---------------------------------------------------------------------*
FORM zf_manipulate_data .


  LOOP AT gt_dtl INTO gs_dtl.
    MOVE-CORRESPONDING gs_dtl TO gs_final.
    gs_final-p_qty = gs_dtl-o_qty - gs_dtl-r_qty.
    READ TABLE gt_hd INTO gs_hd WITH KEY werks = gs_dtl-werks
                                         gtype = gs_dtl-gtype
                                         gyear = gs_dtl-gyear
                                         gno   = gs_dtl-gno.
    IF sy-subrc = '0'.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = gs_hd-lifnr.
      IF sy-subrc = '0'.
        gs_hd-name1 = wa_lfa1-name1.
        CLEAR : wa_lfa1.
      ENDIF.
      MOVE-CORRESPONDING gs_hd TO gs_final.
    ENDIF.
    APPEND gs_final TO gt_final.
    CLEAR : gs_final, gs_dtl, gs_hd.
  ENDLOOP.


  IF NOT p_pend IS INITIAL OR NOT p_pmail IS INITIAL.
    DELETE gt_final WHERE p_qty EQ 0.
  ENDIF.
  SORT gt_final BY gtype gyear werks gno.

  LOOP AT gt_final INTO gs_final.        " Added By Pradeep Kodinagula to display 'Overdue By' column.

    IF gs_final-due_date IS NOT INITIAL.
      gs_final-overdue_by = sy-datum - gs_final-due_date.
      MODIFY gt_final FROM gs_final
             TRANSPORTING overdue_by WHERE gno = gs_final-gno.
    ENDIF.
    CLEAR gs_final.
  ENDLOOP.
*  BREAK IBM_AMS.
ENDFORM.                    " ZF_MANIPULATE_DATA

*&---------------------------------------------------------------------*
*&      Form  zf_alv_fieldcat
*&---------------------------------------------------------------------*
FORM zf_alv_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'GTYPE'.
  gs_fieldcat-seltext_l  = 'Gate Pass Type'.                "#EC NOTEXT
  gs_fieldcat-outputlen  = 5.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'GYEAR'.
  gs_fieldcat-no_zero    = 'X'.
  gs_fieldcat-seltext_l  = 'Gate Pass Year'.
  gs_fieldcat-outputlen  = 5.
  gs_fieldcat-no_zero    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'WERKS'.
  gs_fieldcat-seltext_l  = 'Plant'.                         "#EC NOTEXT
  gs_fieldcat-outputlen  = 5.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'GNO'.
  gs_fieldcat-hotspot    = 'X'.
  gs_fieldcat-seltext_l  = 'Gate Pass No'.                  "#EC NOTEXT
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'GINO'.
  gs_fieldcat-seltext_l  = 'Gate Pass Item no'.
  gs_fieldcat-outputlen  = 5.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'CARRIER'.
  gs_fieldcat-seltext_l  = 'Carrier'.                       "#EC NOTEXT
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'VECHICAL_NO'.
  gs_fieldcat-seltext_l  = 'Vechical No'.
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'KUNNR'.
  gs_fieldcat-seltext_l  = 'Customer'.                      "#EC NOTEXT
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'LIFNR'.
  gs_fieldcat-seltext_l  = 'Vendor'.                        "#EC NOTEXT
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
*********added by sachin
  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'NAME1'.
  gs_fieldcat-seltext_l  = 'Vendor Description'.
  gs_fieldcat-outputlen  = 100.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
**********end of added code
  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'ERDAT'.
  gs_fieldcat-seltext_l  = 'Doc Creation date'.
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'MATNR'.
  gs_fieldcat-seltext_l  = 'Material no'.
  gs_fieldcat-outputlen  = 18.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'MAKTX'.
  gs_fieldcat-seltext_l  = 'Material Desc'.
  gs_fieldcat-outputlen  = 18.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'O_QTY'.
  gs_fieldcat-seltext_l  = 'Gate Pass Qty'.
  gs_fieldcat-outputlen  = 15.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  IF p_gtype = 'RGP'.
    counter = counter + 1.
    gs_fieldcat-col_pos    = counter.
    gs_fieldcat-tabname    = 'GT_FINAL'.
    gs_fieldcat-fieldname  = 'R_QTY'.
    gs_fieldcat-seltext_l  = 'Recieved Qty'.
    gs_fieldcat-outputlen  = 15.
    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR gs_fieldcat.

    counter = counter + 1.
    gs_fieldcat-col_pos    = counter.
    gs_fieldcat-tabname    = 'GT_FINAL'.
    gs_fieldcat-fieldname  = 'P_QTY'.
    gs_fieldcat-seltext_l  = 'Pending Qty'.
    gs_fieldcat-outputlen  = 15.
    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR gs_fieldcat.
  ENDIF.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'UOM'.
  gs_fieldcat-seltext_l  = 'UOM'.
  gs_fieldcat-outputlen  = 5.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'DUE_DATE'.
  gs_fieldcat-seltext_l  = 'Due Date'.
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.                      " Added By Pradeep Kodinagula to display 'Overdue By' column.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'OVERDUE_BY'.
  gs_fieldcat-seltext_l  = 'Overdue By Days'.
  gs_fieldcat-outputlen  = 10.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  IF p_gtype = 'RGP'.
    counter = counter + 1.
    gs_fieldcat-col_pos    = counter.
    gs_fieldcat-tabname    = 'GT_FINAL'.
    gs_fieldcat-fieldname  = 'STATUS'.
    gs_fieldcat-seltext_l  = 'Update Status'.
    gs_fieldcat-outputlen  = 2.
    APPEND gs_fieldcat TO gt_fieldcat.
    CLEAR gs_fieldcat.
  ENDIF.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'LOEKZ'.
  gs_fieldcat-seltext_l  = 'Deletion Ind.'.
  gs_fieldcat-outputlen  = 2.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'APP_STAT'.
  gs_fieldcat-seltext_l  = 'Approval Status'.
  gs_fieldcat-outputlen  = 2.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'REASON'.
  gs_fieldcat-seltext_l  = 'Reason'.                        "#EC NOTEXT
  gs_fieldcat-outputlen  = 100.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'REMARK'.
  gs_fieldcat-seltext_l  = 'Remarks'.
  gs_fieldcat-outputlen  = 100.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
******************code added by sachin ***************
  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'REQS'.
  gs_fieldcat-seltext_l  = 'Requester'.
  gs_fieldcat-outputlen  = 100.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  counter = counter + 1.
  gs_fieldcat-col_pos    = counter.
  gs_fieldcat-tabname    = 'GT_FINAL'.
  gs_fieldcat-fieldname  = 'ERNAM'.
  gs_fieldcat-seltext_l  = 'Created By'.
  gs_fieldcat-outputlen  = 100.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
**************end of added code **********************

ENDFORM.                    " zf_alv_fieldcat

*&---------------------------------------------------------------------*
*&      Form  zf_sub_comment_build
*&---------------------------------------------------------------------*
FORM zf_sub_comment_build  USING  i_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
       lv_dttm(40)  TYPE c,
       lv_date(10)  TYPE c,
       lv_time(10)  TYPE c,
       lv_text(60)  TYPE c,
       lv_dat1(10)  TYPE c,
       lv_dat2(10)  TYPE c.
  CLEAR : lv_date, lv_time, lv_dttm, lv_text, lv_dat1, lv_dat2.

  ls_line-typ  = 'H'.
  ls_line-info = 'Gate Pass Report'.                        "#EC NOTEXT
  APPEND ls_line TO i_top_of_page.
  CLEAR: ls_line.

  IF s_erdat-low IS NOT INITIAL AND s_erdat-high IS  INITIAL.
    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal = s_erdat-low
      IMPORTING
        date_external = lv_dat1.
    CLEAR: ls_line.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Document Date :'.                       "#EC NOTEXT
    ls_line-info = lv_dat1.
    APPEND ls_line TO i_top_of_page.
  ELSEIF s_erdat-low IS NOT INITIAL AND s_erdat-high IS NOT INITIAL.
    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal = s_erdat-low
      IMPORTING
        date_external = lv_dat1.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal = s_erdat-high
      IMPORTING
        date_external = lv_dat2.
    CONCATENATE 'From :' lv_dat1 'To' lv_dat2 INTO lv_text
                      SEPARATED BY space.
    CLEAR: ls_line.
    ls_line-typ  = 'S'.
    ls_line-key  = 'Document Date :'.                       "#EC NOTEXT
    ls_line-info = lv_text.
    APPEND ls_line TO i_top_of_page.
  ENDIF.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal = sy-datum
    IMPORTING
      date_external = lv_date.
  WRITE : sy-uzeit TO lv_time USING EDIT MASK '__:__:__'.
  CONCATENATE lv_date '/' lv_time INTO lv_dttm SEPARATED BY space.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Current Date/Time:'.                      "#EC NOTEXT
  ls_line-info = lv_dttm.
  APPEND ls_line TO i_top_of_page.
  CLEAR: ls_line.
ENDFORM.                    "SUB_COMMENT_BUILD

*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM zf_alv_display .
  DATA : v_text TYPE lvc_title.
  CLEAR v_text.
  IF p_gtype = 'NRGP'.
    v_text = 'NON - RETURNABLE GATE PASS'.
  ELSE.
    v_text = 'RETURNABLE GATE PASS'.
  ENDIF.
  IF p_stat IS NOT INITIAL. "pravin added 18/08/2015
    DELETE gt_final WHERE app_stat NOT IN p_stat.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = gt_fieldcat
      it_events          = gt_events[]
      i_grid_title       = v_text
      is_layout          = layout
      i_save             = gv_save
      is_variant         = gv_variant
    TABLES
      t_outtab           = gt_final
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ZF_ALV_DISPLAY

*&--------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&--------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_list_top_of_page.

ENDFORM.                    "TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  ZF_GET_EVENTS
*&---------------------------------------------------------------------*
FORM zf_get_events .
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = gt_events[]
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE gt_events INTO gs_events
                       WITH KEY name = slis_ev_top_of_page.
  IF sy-subrc = 0.
    MOVE t_formname_top_of_page TO gs_events-form.
    APPEND gs_events TO gt_events.
  ENDIF.
*****************
  CLEAR : gs_events.
  READ TABLE gt_events INTO gs_events WITH KEY name = 'USER_COMMAND'.
  IF sy-subrc = 0.
    gs_events-form = 'ZF_SUBLIST'.
    APPEND gs_events TO gt_events.
  ENDIF.
ENDFORM.                    " ZF_GET_EVENTS

*&---------------------------------------------------------------------*
*&      Form  ZF_BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM zf_build_layout.
*  layout-f2code       = f2code.
  layout-zebra        = 'X'.
  layout-detail_popup = 'X'.
  layout-no_vline = '1'.
  layout-colwidth_optimize = 'X'.
ENDFORM.                    "ZF_BUILD_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ZF_PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_pai_of_selection_screen .
  IF NOT p_vari IS INITIAL.
    MOVE gv_variant TO gx_variant.
    MOVE p_vari TO gx_variant-variant.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = gv_save
      CHANGING
        cs_variant = gx_variant.
    gv_variant = gx_variant.
  ELSE.
    PERFORM zf_initialize_variant.
  ENDIF.

ENDFORM.                    " ZF_PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  ZF_INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_initialize_variant .
  gv_save = 'A'.
  CLEAR gv_variant.
  gv_variant-report = sy-repid.
  gx_variant = gv_variant.
*  gx_variant = gv_variant.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = gv_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.
  layout-get_selinfos = 'X'.
  layout-group_change_edit = 'X'.

  alv_print-no_print_selinfos  = 'X'.
  alv_print-no_coverpage       = 'X'.
  alv_print-no_print_listinfos = 'X'.
  alv_detail_func = 'REUSE_ALV_LIST_DISPLAY'.

ENDFORM.                    " ZF_INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  ZF_F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_f4_for_variant .
  gv_variant-report = sy-repid.
  gx_variant-report = sy-repid.
  gv_save = 'A'.

  gv_variant = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant       = gv_variant
      i_save           = 'A'
      i_tabname_header = g_tabname_header
      i_tabname_item   = g_tabname_item
    IMPORTING
      e_exit           = gv_exit
      es_variant       = gx_variant
    EXCEPTIONS
      not_found        = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF gv_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " ZF_F4_FOR_VARIANT

*&---------------------------------------------------------------------*
*&Form  zf_sublist
*&---------------------------------------------------------------------*
FORM zf_sublist USING ucomm LIKE sy-ucomm sobj TYPE slis_selfield.
  DATA : lv_gno TYPE zzgno.
  DATA : ls_auth TYPE z6mma_rgpnrgp_at.
  CLEAR : ls_auth, lv_gno.
  IF sobj-fieldname = 'GNO'.
    lv_gno = sobj-value.
    CLEAR : gs_final.
    READ TABLE gt_final INTO gs_final INDEX sobj-tabindex.
    IF sy-subrc = '0'.
      SELECT SINGLE * FROM z6mma_rgpnrgp_at
                      INTO ls_auth
                     WHERE usnam = sy-uname
                       AND werks = gs_final-werks.
      IF sy-subrc = '0'.
        PERFORM zf_go_to_screen.
      ELSE.
        MESSAGE e398(00) WITH
                  'You are not authorized to display for this plant'
                  '-' gs_final-werks ''.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                                                    "SUBLIST
*&---------------------------------------------------------------------*
*&      Form  ZF_GO_TO_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_go_to_screen .
  CLEAR : bdcdata. REFRESH bdcdata.
  PERFORM zf_bdc_dynpro      USING 'Z6MM005M_RGP_NRGP' '9003'.
  PERFORM zf_bdc_field       USING 'BDC_OKCODE'
                                   '/00'.
  PERFORM zf_bdc_field       USING 'GS_HD_MOD-GTYPE'
                                   gs_final-gtype.
  PERFORM zf_bdc_field       USING 'GS_HD_MOD-WERKS'
                                   gs_final-werks.
  PERFORM zf_bdc_field       USING 'GS_HD_MOD-GYEAR'
                                   gs_final-gyear.
  PERFORM zf_bdc_field       USING 'GS_HD_MOD-GNO'
                                   gs_final-gno.
  CALL TRANSACTION 'ZMM005_3' USING bdcdata
                          MODE  'E'.
ENDFORM.                    " ZF_GO_TO_SCREEN

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM zf_bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "zf_BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM zf_bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "zf_BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  ZEMAIL_TRIGGER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zemail_trigger .
  IF p_stat  IS NOT INITIAL. "pavin added 18/08/2015
    DELETE gt_final WHERE app_stat NOT IN p_stat.
  ENDIF.
*  DELETE gt_final WHERE app_stat = space.

  IF gt_final[] IS INITIAL.
    WRITE /'Data Does Not Exits'  .
  ELSE.
    LOOP AT gt_final INTO gs_final.
      CALL FUNCTION 'CALCULATE_DATE'
       EXPORTING
         days              = '7'
*   MONTHS            = '0'
         start_date        = sy-datum
       IMPORTING
         result_date       = result_date.

*      IF gs_final-werks = '1101'.    "pravin added
*        IF gs_final-ernam NE '2547'.
*          gs_final-ind = 'X'.
*          MODIFY gt_final FROM gs_final INDEX sy-tabix TRANSPORTING ind.
*        ENDIF.
*        IF gs_final-ernam NE '2457'.
*          gs_final-ind = 'X'.
*          MODIFY gt_final FROM gs_final INDEX sy-tabix TRANSPORTING ind.
*        ENDIF.
*      ENDIF.

      IF gs_final-due_date NOT BETWEEN sy-datum AND result_date.
        DELETE gt_final INDEX sy-tabix."from gs_final .
      ENDIF.

    ENDLOOP.
*    DELETE gt_final WHERE ind = 'X'.


LOOP AT S_WERK.

    IF s_werk-low NE '2101'.
      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_to
      WHERE egroup = 'ZMM005_6TO'.

      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_cc
      WHERE egroup = 'ZMM005_6CC'.

    ELSEIF   s_werk-low EQ '2101'  .

      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_to
      WHERE egroup = 'ZM2101_6TO'.

      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_cc
      WHERE egroup = 'ZM2101_6CC'.
    ENDIF.

ENDLOOP.
*      break ibm_ams.
*LOOP AT gt_final into gs_final.
    CONCATENATE
  'G Pass Typ'
  'Gate Pass yr'
  'plant'
  'G Pass No'
  'G Pass Item'
  'Carrier'
  'Vechical No'
  'Cust.Code'
  'Vend.Code'
  'Vend.Desc'
  'Doc.Crea.date'
  'Mat.No.'
  'Mat.Desc'
  'G Pass Qty'
  'Recieved Qty'
  'Pending Qty'
  'UOM'
  'Due Date'
  'Overdue By'
  'Update Status'
  'Deletion Indi'
  'Approval Status'
  'Reason'
  'Remark'
  'Request'
  'Cre.By'
  INTO it_attach SEPARATED BY con_tab.
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND  it_attach.
*    CLEAR : gs_final.
*ENDLOOP.

    LOOP AT gt_final INTO gs_final.
      o_qty1 = gs_final-o_qty.
      r_qty1 = gs_final-r_qty.
      p_qty1 = gs_final-p_qty.
      WRITE : gs_final-erdat TO gv_erdat,
              gs_final-due_date TO gv_due_date.
      CONDENSE  o_qty1.
      CONDENSE  r_qty1.
      CONDENSE  p_qty1.
      CONCATENATE
      gs_final-gtype
      gs_final-gyear
      gs_final-werks
      gs_final-gno
      gs_final-gino
      gs_final-carrier
      gs_final-vechical_no
      gs_final-kunnr
      gs_final-lifnr
      gs_final-name1
      gv_erdat
      gs_final-matnr
      gs_final-maktx
      o_qty1
      r_qty1
      p_qty1
      gs_final-uom
      gv_due_date
      gs_final-overdue_by
      gs_final-status
      gs_final-loekz
      gs_final-app_stat
      gs_final-reason
      gs_final-remark
      gs_final-reqs
      gs_final-ernam
       INTO it_attach SEPARATED BY con_tab.
      CONCATENATE con_cret it_attach  INTO it_attach.
      APPEND  it_attach.
      CLEAR : gs_final,o_qty1,r_qty1,p_qty1,gv_erdat,gv_due_date.
    ENDLOOP.
    LOOP AT it_zfi044_emailid_to INTO wa_zfi044_emailid_to.
*        wa_zfi044_emailid_to-email = wa_zfi044_emailid_to-email.
      lv_emailaddr = wa_zfi044_emailid_to-email.
      reclist-receiver = lv_emailaddr.
      reclist-rec_type = 'U'.
      APPEND reclist.
      CLEAR :wa_zfi044_emailid_to,lv_emailaddr.
    ENDLOOP.
    LOOP AT it_zfi044_emailid_cc INTO wa_zfi044_emailid_cc.
*        wa_zfi044_emailid_to-email = wa_zfi044_emailid_to-email.
*        lv_emailaddr = wa_zfi044_emailid_cc-email.
      reclist-receiver =  wa_zfi044_emailid_cc-email.
      reclist-rec_type = 'U'.
      reclist-copy = 'X'.
      APPEND reclist.
      CLEAR :wa_zfi044_emailid_cc.
    ENDLOOP.
    REFRESH it_message.
    CLEAR :str.
    IF s_werk-low NE '2101'.
      str = 'Dear Dubey,'.
    ELSEIF s_werk-low EQ '2101'.
      str =  'Dear Champaneria,' .
    ENDIF.
    str_drft = 'Please find attachment for the Returnable Gate Pass whose due is in next 7 Days'.
    it_message = str.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = str_drft.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = 'PLEASE NOTE :- THIS IS AN AUTO GENERATED MAIL, PLEASE DO NOT REPLY TO THIS MAIL.'.
    APPEND it_message.

    str1 = 'Returnable Gate Pass Report'.
    ld_mtitle = str1.
    ld_format              = 'XLS'.
    ld_attdescription      = ''.
    ld_attfilename         = str1.
    ld_sender_address      = 'sapautomail-icc@modi.com'.
    ld_sender_address_type = 'SMTP'.

    CLEAR : w_doc_data.
* Fill the document data.
    w_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
    w_doc_data-obj_langu = sy-langu.
    w_doc_data-obj_name  = str1.
    w_doc_data-obj_descr = ld_mtitle .
    w_doc_data-sensitivty = 'F'.

* Fill the document data and get size of attachment
    DESCRIBE TABLE objtxt LINES tab_lines.
    READ TABLE objtxt INDEX tab_lines.
    READ TABLE it_attach INDEX w_cnt.
    w_doc_data-doc_size = ( tab_lines - 1 ) * 255 + STRLEN( objtxt ).

    w_doc_data-obj_langu  = sy-langu.
    w_doc_data-obj_name   = 'Str1'.
    w_doc_data-obj_descr  = ld_mtitle.
    w_doc_data-sensitivty = 'F'.
***  w_doc_data-no_change = 'X'.
    CLEAR : t_attachment,t_attachment[].
    REFRESH : t_attachment,t_attachment[].
    t_attachment[] = it_attach[].

* Describe the body of the message
    CLEAR: t_packing_list , t_packing_list[].
    REFRESH t_packing_list.
    t_packing_list-transf_bin = space.
    t_packing_list-head_start = 0.
    t_packing_list-head_num = 0.
    t_packing_list-body_start = 0.
    t_packing_list-body_num   = tab_lines.
    DESCRIBE TABLE it_message LINES t_packing_list-body_num.
    t_packing_list-doc_type = 'RAW'.
***  t_packing_list-doc_type = 'XLS'.
*        t_packing_list-obj_name = 'Customer Credit Exposure Report'.
    APPEND t_packing_list.

* Create attachment notification
    t_packing_list-transf_bin = 'X'.
    t_packing_list-head_start = 1.
    t_packing_list-head_num   = 1.
    t_packing_list-body_start = 1.

    DESCRIBE TABLE it_attach LINES t_packing_list-body_num.
    t_packing_list-doc_type   =  ld_format.
    t_packing_list-obj_descr  =  ld_attdescription.
    t_packing_list-obj_name   =  ld_attfilename.
    t_packing_list-doc_size   =  t_packing_list-body_num * 255.
    APPEND t_packing_list.
    CLEAR : t_receivers." ,gs_userm-email_id.
    TRANSLATE wa_zfi044_emailid_to-email TO LOWER CASE.
    TRANSLATE wa_zfi044_emailid_cc-email TO LOWER CASE.

    CLEAR : filename,t_object_header[],t_object_header.
    CONCATENATE str1'.XLS' INTO filename
    SEPARATED BY space.
    t_object_header = filename.
    APPEND t_object_header.

    DESCRIBE TABLE t_attachment LINES line_number.
    IF NOT reclist[] IS INITIAL AND line_number GT 1.
      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          document_data                    = w_doc_data
         put_in_outbox                    = 'X'
         sender_address                   = ld_sender_address
         sender_address_type              = ld_sender_address_type
         commit_work                      = 'X'
*       IP_ENCRYPT                       =
*       IP_SIGN                          =
*       IV_VSI_PROFILE                   =
*     IMPORTING
*       SENT_TO_ALL                      =
*       NEW_OBJECT_ID                    =
*       SENDER_ID                        =
        TABLES
          packing_list                     = t_packing_list
         object_header                    = t_object_header
         contents_bin                     = t_attachment
         contents_txt                     = it_message
*       CONTENTS_HEX                     =
*       OBJECT_PARA                      =
*       OBJECT_PARB                      =
          receivers                        = reclist[]
*       ET_VSI_ERROR                     =
       EXCEPTIONS
         too_many_receivers               = 1
         document_not_sent                = 2
         document_type_not_exist          = 3
         operation_no_authorization       = 4
         parameter_error                  = 5
         x_error                          = 6
         enqueue_error                    = 7
         OTHERS                           = 8
                .
      IF sy-subrc EQ 0.
        WRITE /'Email send successfully'  .
      ELSE.
        WRITE / 'Email not send successfully'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " ZEMAIL_TRIGGER
*&---------------------------------------------------------------------*
*&      Form  PENDING_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pending_email .
  IF p_stat IS NOT INITIAL.  "pravin added 18/08/2015
    DELETE gt_final WHERE app_stat NOT IN p_stat.
  ENDIF.
*  DELETE gt_final WHERE app_stat = space.

*  LOOP AT gt_final INTO gs_final.
*    IF gs_final-werks = '1101'.    "pravin added
*      IF gs_final-ernam NE '2547'.
*        gs_final-ind = 'X'.
*        MODIFY gt_final FROM gs_final INDEX sy-tabix TRANSPORTING ind.
*      ENDIF.
*      IF gs_final-ernam NE '2457'.
*        gs_final-ind = 'X'.
*        MODIFY gt_final FROM gs_final INDEX sy-tabix TRANSPORTING ind.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*  DELETE gt_final WHERE ind = 'X'.

  IF gt_final[] IS INITIAL.
    WRITE /'Data Does Not Exits'  .
  ELSE.

    DELETE gt_final WHERE overdue_by LE 0.

    LOOP AT S_WERK.

    IF s_werk-low EQ '1101'.
      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_to
      WHERE egroup = 'PENDT_6TO'.

      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_cc
      WHERE egroup = 'PENDT_6CC'.

    ELSEIF   s_werk-low EQ '2101'  .

      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_to
      WHERE egroup = 'PENDD_6TO'.

      SELECT *
      FROM zfi044_emailid
      INTO TABLE it_zfi044_emailid_cc
      WHERE egroup = 'PENDD_6CC'.
    ENDIF.

    ENDLOOP.
*      break ibm_ams.
*LOOP AT gt_final into gs_final.
    CONCATENATE
  'G Pass Typ'
  'Gate Pass yr'
  'plant'
  'G Pass No'
  'G Pass Item'
  'Carrier'
  'Vechical No'
  'Cust.Code'
  'Vend.Code'
  'Vend.Desc'
  'Doc.Crea.date'
  'Mat.No.'
  'Mat.Desc'
  'G Pass Qty'
  'Recieved Qty'
  'Pending Qty'
  'UOM'
  'Due Date'
  'Overdue By'
  'Update Status'
  'Deletion Indi'
  'Approval Status'
  'Reason'
  'Remark'
  'Request'
  'Cre.By'
  INTO it_attach SEPARATED BY con_tab.
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND  it_attach.
*    CLEAR : gs_final.
*ENDLOOP.

    LOOP AT gt_final INTO gs_final.
      o_qty1 = gs_final-o_qty.
      r_qty1 = gs_final-r_qty.
      p_qty1 = gs_final-p_qty.
      WRITE : gs_final-erdat TO gv_erdat,
              gs_final-due_date TO gv_due_date.
      CONDENSE  o_qty1.
      CONDENSE  r_qty1.
      CONDENSE  p_qty1.
      CONCATENATE
      gs_final-gtype
      gs_final-gyear
      gs_final-werks
      gs_final-gno
      gs_final-gino
      gs_final-carrier
      gs_final-vechical_no
      gs_final-kunnr
      gs_final-lifnr
      gs_final-name1
      gv_erdat
      gs_final-matnr
      gs_final-maktx
      o_qty1
      r_qty1
      p_qty1
      gs_final-uom
      gv_due_date
      gs_final-overdue_by
      gs_final-status
      gs_final-loekz
      gs_final-app_stat
      gs_final-reason
      gs_final-remark
      gs_final-reqs
      gs_final-ernam
       INTO it_attach SEPARATED BY con_tab.
      CONCATENATE con_cret it_attach  INTO it_attach.
      APPEND  it_attach.
      CLEAR : gs_final,o_qty1,r_qty1,p_qty1,gv_erdat,gv_due_date.
    ENDLOOP.
    LOOP AT it_zfi044_emailid_to INTO wa_zfi044_emailid_to.
*        wa_zfi044_emailid_to-email = wa_zfi044_emailid_to-email.
      lv_emailaddr = wa_zfi044_emailid_to-email.
      reclist-receiver =  lv_emailaddr.
      reclist-rec_type = 'U'.
      APPEND reclist.
      CLEAR :wa_zfi044_emailid_to,lv_emailaddr.
    ENDLOOP.
    LOOP AT it_zfi044_emailid_cc INTO wa_zfi044_emailid_cc.
*        wa_zfi044_emailid_to-email = wa_zfi044_emailid_to-email.
*        lv_emailaddr = wa_zfi044_emailid_cc-email.
      reclist-receiver =  wa_zfi044_emailid_cc-email.
      reclist-rec_type = 'U'.
      reclist-copy = 'X'.
      APPEND reclist.
      CLEAR :wa_zfi044_emailid_cc.
    ENDLOOP.
    REFRESH it_message.
    CLEAR :str.
*      if s_werk-low NE '2101'.
*      str = 'Dear Dubey,'.
*      elseif s_werk-low EQ '2101'.
*      str =  'Dear Champaneria,' .
*      endif.
    str = 'Dear Sir,' .
    str_drft = 'Please find attachment for the Returnable Gate Pass whose due is pending '.
    it_message = str.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = str_drft.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = ' '.
    APPEND it_message.
    it_message = 'PLEASE NOTE :- THIS IS AN AUTO GENERATED MAIL, PLEASE DO NOT REPLY TO THIS MAIL.'.
    APPEND it_message.

*      str1 = 'Returnable Gate Pass Pending Report '.
    str1 = 'Returnable Gate Pass Pending Overdue Report'.

    ld_mtitle = str1.
    ld_format              = 'XLS'.
    ld_attdescription      = ''.
    ld_attfilename         = str1.
    ld_sender_address      = 'sapautomail-icc@modi.com'.
    ld_sender_address_type = 'SMTP'.

    CLEAR : w_doc_data.
* Fill the document data.
    w_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
    w_doc_data-obj_langu = sy-langu.
    w_doc_data-obj_name  = str1.
    w_doc_data-obj_descr = ld_mtitle .
    w_doc_data-sensitivty = 'F'.

* Fill the document data and get size of attachment
    DESCRIBE TABLE objtxt LINES tab_lines.
    READ TABLE objtxt INDEX tab_lines.
    READ TABLE it_attach INDEX w_cnt.
    w_doc_data-doc_size = ( tab_lines - 1 ) * 255 + STRLEN( objtxt ).

    w_doc_data-obj_langu  = sy-langu.
    w_doc_data-obj_name   = 'Str1'.
    w_doc_data-obj_descr  = ld_mtitle.
    w_doc_data-sensitivty = 'F'.
***  w_doc_data-no_change = 'X'.
    CLEAR : t_attachment,t_attachment[].
    REFRESH : t_attachment,t_attachment[].
    t_attachment[] = it_attach[].

* Describe the body of the message
    CLEAR: t_packing_list , t_packing_list[].
    REFRESH t_packing_list.
    t_packing_list-transf_bin = space.
    t_packing_list-head_start = 0.
    t_packing_list-head_num = 0.
    t_packing_list-body_start = 0.
    t_packing_list-body_num   = tab_lines.
    DESCRIBE TABLE it_message LINES t_packing_list-body_num.
    t_packing_list-doc_type = 'RAW'.
***  t_packing_list-doc_type = 'XLS'.
*        t_packing_list-obj_name = 'Customer Credit Exposure Report'.
    APPEND t_packing_list.

* Create attachment notification
    t_packing_list-transf_bin = 'X'.
    t_packing_list-head_start = 1.
    t_packing_list-head_num   = 1.
    t_packing_list-body_start = 1.

    DESCRIBE TABLE it_attach LINES t_packing_list-body_num.
    t_packing_list-doc_type   =  ld_format.
    t_packing_list-obj_descr  =  ld_attdescription.
    t_packing_list-obj_name   =  ld_attfilename.
    t_packing_list-doc_size   =  t_packing_list-body_num * 255.
    APPEND t_packing_list.
    CLEAR : t_receivers." ,gs_userm-email_id.
    TRANSLATE wa_zfi044_emailid_to-email TO LOWER CASE.
    TRANSLATE wa_zfi044_emailid_cc-email TO LOWER CASE.

    CLEAR : filename,t_object_header[],t_object_header.
    CONCATENATE str1'.XLS' INTO filename
    SEPARATED BY space.
    t_object_header = filename.
    APPEND t_object_header.

    DESCRIBE TABLE t_attachment LINES line_number.
    IF NOT reclist[] IS INITIAL AND line_number GT 1.
      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          document_data                    = w_doc_data
         put_in_outbox                    = 'X'
         sender_address                   = ld_sender_address
         sender_address_type              = ld_sender_address_type
         commit_work                      = 'X'
*       IP_ENCRYPT                       =
*       IP_SIGN                          =
*       IV_VSI_PROFILE                   =
*     IMPORTING
*       SENT_TO_ALL                      =
*       NEW_OBJECT_ID                    =
*       SENDER_ID                        =
        TABLES
          packing_list                     = t_packing_list
         object_header                    = t_object_header
         contents_bin                     = t_attachment
         contents_txt                     = it_message
*       CONTENTS_HEX                     =
*       OBJECT_PARA                      =
*       OBJECT_PARB                      =
          receivers                        = reclist[]
*       ET_VSI_ERROR                     =
       EXCEPTIONS
         too_many_receivers               = 1
         document_not_sent                = 2
         document_type_not_exist          = 3
         operation_no_authorization       = 4
         parameter_error                  = 5
         x_error                          = 6
         enqueue_error                    = 7
         OTHERS                           = 8
                .
      IF sy-subrc EQ 0.
        WRITE /'Email send successfully'  .
      ELSE.
        WRITE / 'Email not send successfully'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " PENDING_EMAIL
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 09.10.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth_obj .
  TYPES: BEGIN OF ty_t001w,
          werks TYPE t001w-werks, " Plants
        END OF ty_t001w.
  DATA: t_t001w TYPE TABLE OF ty_t001w, " Plants
        w_t001w TYPE ty_t001w.

  FREE : t_t001w[].
  CLEAR: w_t001w.

  break test1.
  SELECT werks  " Fetch values of Plant
    FROM t001w
    INTO TABLE t_t001w
    WHERE werks IN s_werk.
***** Start Code: Added by CS on 09.10.2015 for Receiving Plant Authorization. *****
  CLEAR: s_werk, lv_werks_auth_flg.
  REFRESH: s_werk[].
  IF t_t001w[] IS NOT INITIAL.
    LOOP AT t_t001w INTO w_t001w.
      AUTHORITY-CHECK OBJECT 'M_MSEG_WWA' " Plant
                     ID 'ACTVT' FIELD '03'
                     ID 'WERKS' FIELD w_t001w-werks.
      IF sy-subrc EQ 0.
        s_werk-sign = 'I'.
        s_werk-option = 'EQ'.
        s_werk-low = w_t001w-werks.
        APPEND s_werk.
        CLEAR: s_werk.
      ELSE.
        IF lv_werks_auth_flg IS INITIAL.  " Authorization Flag
          lv_werks_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t001w.
    ENDLOOP.
  ENDIF.
  IF s_werk[] IS INITIAL.
    s_werk-sign = 'I'.
    s_werk-option = 'EQ'.
    s_werk-low = ''.
    APPEND s_werk.
    CLEAR: s_werk.
  ENDIF.
***** End Code: Added by CS on 09.10.2015 for Receiving Plant Authorization. *****
ENDFORM.                    " CHECK_AUTH_OBJ
