*&***********************************************************************************************&*
*& OBJECT NAME          : ZMM_PROPOSAL_FINAL_CONFI                                               &*
*& TECHNICAL CONSULTANT : RAVINDRA SANAP                                                         &*
*& TEAM LEAD            : POONAM SHINDE                                                          &*
*& FUCTIONAL            : VENU                                                                   &*
*& MODULE NAME          : MM                                                                     &*
*& PROGRAM TYPE         : OOPS INTRACTIVE REPORT                                                 &*
*& CREATE DATE          : DEC 08,2016                                                            &*
*& TCODE                : ZMM015                                                                 &*
*& TRANSPORT NO         : IRDK926351                                                             &*
*& DESCRIPTION          : THIS REPORT IS DEVELOPED FOR PROPOSAL - TO Release and Cancel          &*
*&                          PROPOSAL                                                             &*
* REVISION HISTORY--------------------------------------------------------------------------------*
*                                                                                                 *
*  CHANGED BY:                                                                                   *
*  CHANGE ON:                                                                                    *
*  REASON FOR CHANGE:                                                                            *
*  IRDK926477,IRDK926489,IRDK926495                                                                                              *
*  IRDK926521                                                                                               *
* REVISION HISTORY--------------------------------------------------------------------------------*

REPORT  zmm_approve_proposal.

CLASS event_class DEFINITION DEFERRED.
DATA  event_receiver TYPE REF TO event_class.
DATA: w_variant      TYPE disvariant,
      alv_container  TYPE REF TO cl_gui_custom_container,
      alv_container1 TYPE REF TO cl_gui_custom_container,
      alv_grid       TYPE REF TO cl_gui_alv_grid,
      alv_grid1      TYPE REF TO cl_gui_alv_grid.

DATA: gt_fieldcat    TYPE STANDARD TABLE OF lvc_s_fcat,
      gs_fieldcat    TYPE lvc_s_fcat,
      gs_layout      TYPE lvc_s_layo,
      gs_head        TYPE slis_listheader,
      gt_head        TYPE slis_t_listheader.

TYPES : BEGIN OF ty_zmm_pur_proposal,
        srno             TYPE i,
*        id               TYPE icon-id,
*        style            TYPE     lvc_t_styl,   "for disable
*        approve_proposal TYPE icon-id,
*        cancel_proposal  TYPE c,
        prno             TYPE zmm_pur_proposal-prno,
        gjahr            TYPE zmm_pur_proposal-gjahr,
        pr_itemno        TYPE zmm_pur_proposal-pr_itemno,
        level            TYPE  c,
        pr_date          TYPE zmm_pur_proposal-pr_date,
        ekgrp            TYPE zmm_pur_proposal-ekgrp,
        eknam            TYPE zmm_pur_proposal-eknam,
        bukrs            TYPE zmm_pur_proposal-bukrs,
        werks            TYPE zmm_pur_proposal-werks,
        plantname        TYPE zmm_pur_proposal-plantname,
        matnr            TYPE zmm_pur_proposal-matnr,
        maktx            TYPE zmm_pur_proposal-maktx,
        lifnr            TYPE zmm_pur_proposal-lifnr,
        name1            TYPE zmm_pur_proposal-name1,
        menge            TYPE zmm_pur_proposal-menge,
        netpr            TYPE zmm_pur_proposal-netpr,
        meins            TYPE zmm_pur_proposal-meins,
        waers            TYPE zmm_pur_proposal-waers,
        mwskz            TYPE zmm_pur_proposal-mwskz,
        tax_desc         TYPE zmm_pur_proposal-tax_desc,
        land_pr          TYPE zmm_pur_proposal-land_pr,
        land_value       TYPE zmm_pur_proposal-land_value,
        last_basicpr     TYPE zmm_pur_proposal-last_basicpr,
        last_landpr      TYPE zmm_pur_proposal-last_landpr,
        business_planpr  TYPE zmm_pur_proposal-business_planpr,
        vari_last_pur_in TYPE zmm_pur_proposal-vari_last_pur_in,
        vari_last_pur_va TYPE zmm_pur_proposal-vari_last_pur_va,
        vari_bus_pl_inr  TYPE zmm_pur_proposal-vari_bus_pl_inr,
        vari_bus_pl_val  TYPE zmm_pur_proposal-vari_bus_pl_val,
        opening_m1       TYPE zmm_pur_proposal-opening_m1,
        requirment_m1    TYPE zmm_pur_proposal-requirment_m1,
        app_vend_m1      TYPE zmm_pur_proposal-app_vend_m1,
        other_vend_m1    TYPE zmm_pur_proposal-other_vend_m1,
        closing_m1       TYPE zmm_pur_proposal-closing_m1,
        opening_m2       TYPE zmm_pur_proposal-opening_m2,
        requirment_m2    TYPE zmm_pur_proposal-requirment_m2,
        app_vend_m2      TYPE zmm_pur_proposal-app_vend_m2,
        other_vend_m2    TYPE zmm_pur_proposal-other_vend_m2,
        closing_m2       TYPE zmm_pur_proposal-closing_m2,
        opening_m3       TYPE zmm_pur_proposal-opening_m3,
        requirment_m3    TYPE zmm_pur_proposal-requirment_m3,
        app_vend_m3      TYPE zmm_pur_proposal-app_vend_m3,
        other_vend_m3    TYPE zmm_pur_proposal-other_vend_m3,
        closing_m3       TYPE zmm_pur_proposal-closing_m3,
        comp_data        TYPE zmm_pur_proposal-comp_data,
        comp_data1       TYPE zmm_pur_proposal-comp_data1,
        line_txt         TYPE zmm_pur_proposal-line_txt,
        final_price      TYPE zmm_pur_proposal-final_price,
        remark           TYPE zmm_pur_proposal-remark,
        releaser1        TYPE zmm_pur_proposal-releaser1,
        releaser_name1   TYPE zmm_pur_proposal-releaser_name1,
        releaser1_date   TYPE zmm_pur_proposal-releaser1_date,
        status1(10)      TYPE c,
        releaser2        TYPE zmm_pur_proposal-releaser2,
        releaser_name2   TYPE zmm_pur_proposal-releaser_name2,
        releaser2_date   TYPE zmm_pur_proposal-releaser2_date,
        status2(10)      TYPE c,
        releaser3        TYPE zmm_pur_proposal-releaser3,
        releaser_name3   TYPE zmm_pur_proposal-releaser_name3,
        releaser3_date   TYPE zmm_pur_proposal-releaser3_date,
        status3(10)      TYPE c,
        releaser4        TYPE zmm_pur_proposal-releaser4,
        releaser_name4   TYPE zmm_pur_proposal-releaser_name4,
        releaser4_date   TYPE zmm_pur_proposal-releaser4_date,
        status4(10)      TYPE c,
        erdat            TYPE zmm_pur_proposal-erdat,
        ch_date          TYPE zmm_pur_proposal-ch_date,
        pr_user          TYPE zmm_pur_proposal-pr_user,
        pr_time          TYPE zmm_pur_proposal-pr_time,
        app_vendor       TYPE zmm_pur_proposal-app_vendor,
        cons_proposal    TYPE zmm_pur_proposal-cons_proposal,
        color(4),
        END OF ty_zmm_pur_proposal.


DATA : gt_final             TYPE TABLE OF ty_zmm_pur_proposal,
       gs_final             LIKE LINE OF gt_final," ty_zmm_pur_proposal,
       gt_final2            TYPE TABLE OF ty_zmm_pur_proposal,
       gs_final2            LIKE LINE OF gt_final," ty_zmm_pur_proposal,
       gt_zmm_pur_proposal  TYPE TABLE OF zmm_pur_proposal,
       gs_zmm_pur_proposal  TYPE zmm_pur_proposal,
       gt_zmm_pur_proposal1 TYPE TABLE OF zmm_pur_proposal,
       gs_zmm_pur_proposal1 TYPE zmm_pur_proposal.


DATA : gv_prno(10) TYPE c," VALUE "'SET AND GET PARAMETER'.
       gv_prnr     TYPE persno,
       gv_pernr1   TYPE persno,
       gv_matnr    TYPE matnr,
       gv_lifnr    TYPE lifnr,
       gv_level    TYPE c,
       gv_r1       TYPE c,
       gv_r2       TYPE c,
       gv_user     TYPE zmm_pur_proposal-pr_user,
       lv_ans      TYPE c.

TYPES: BEGIN OF ty_f4_prno,
       prno TYPE zmm_pur_proposal-prno,
       releaser1 TYPE zmm_pur_proposal-releaser1,
       releaser2 TYPE zmm_pur_proposal-releaser2,
       releaser3 TYPE zmm_pur_proposal-releaser3,
       releaser4 TYPE zmm_pur_proposal-releaser4,
       END OF ty_f4_prno.
TYPES: BEGIN OF ty_f4_prno1,
       prno TYPE zmm_pur_proposal-prno,
*       pr_user TYPE zmm_pur_proposal-pr_user,
       END OF ty_f4_prno1.

DATA : lt_f4_prno  TYPE TABLE OF ty_f4_prno,
       lt_f4_prno1 TYPE TABLE OF ty_f4_prno1,
       lt_f4_prno2 TYPE TABLE OF ty_f4_prno,
       lt_f4_prno3 TYPE TABLE OF ty_f4_prno,
       lt_f4_prno4 TYPE TABLE OF ty_f4_prno,
       ls_f4_prno  TYPE ty_f4_prno,
       it_ret      TYPE ddshretval OCCURS 0 WITH HEADER LINE,
       gv_pernr    TYPE persno,
       flag1       TYPE c,
       flag1_1     TYPE c,
       flag1_2     TYPE c,
       flag1_3     TYPE c,
       flag2       TYPE c,
       flag3       TYPE c,
       flag4       TYPE c,
       flag_ok     TYPE c,
       flag_new    TYPE c.

DATA : lc_s_glay   TYPE lvc_s_glay,
       flag        TYPE c.
DATA  p_pernr  TYPE pa0001-pernr.

TABLES : pa0001,zmm_pur_proposal.

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
*PARAMETER : p_pernr TYPE pa0001-pernr OBLIGATORY..
PARAMETERS: r1 RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND a,
            r2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN : END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK rad1 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_prno FOR zmm_pur_proposal-prno MODIF ID b NO INTERVALS NO-EXTENSION .
SELECTION-SCREEN : END OF BLOCK rad1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_prno-low.
  PERFORM f4_perno.

AT SELECTION-SCREEN OUTPUT.
  IF r1 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'A'.
        screen-input = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'B'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF r2 = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'A'.
        screen-input = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'B'.
        screen-input = '1'.
        screen-invisible = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.
  PERFORM check_authorization.
  IF r1 = 'X'.
    PERFORM select_data.
    IF gt_final IS NOT INITIAL.
      CALL SCREEN 901.
    ENDIF.
  ELSEIF r2 = 'X'.
    PERFORM select_data_cancel.
    IF gt_final IS NOT INITIAL.
      CALL SCREEN 902.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
*       CLASS event_class DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS event_class DEFINITION.
  PUBLIC SECTION.
    METHODS:
      double_click
            FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column,
    on_hotspot_click
    FOR EVENT hotspot_click OF cl_gui_alv_grid
          IMPORTING
                e_row_id
                e_column_id.
*                 es_row_no,
*                 sender,
*    handle_toolbar_set
*            FOR EVENT toolbar OF cl_gui_alv_grid
*            IMPORTING
*                  e_object
*                  e_interactive,
*---user command on clicking a button
*        handle_user_command
*          FOR EVENT user_command OF cl_gui_alv_grid
*          IMPORTING
*               e_ucomm.
ENDCLASS.                    "event_class DEFINITION
*----------------------------------------------------------------------*
*       CLASS event_class IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS event_class IMPLEMENTATION.
   METHOD double_click.
   ENDMETHOD.


  METHOD on_hotspot_click.
    CLEAR gs_final.
    READ TABLE gt_final INDEX e_row_id-index INTO gs_final.
    If sy-subrc = 0.
    SHIFT gs_final-prno LEFT DELETING LEADING '0'.
    CHECK ( gs_final-prno IS NOT INITIAL ).
   CASE e_column_id-fieldname.
    WHEN 'PRNO'.
    gv_prno = gs_final-prno.
    gv_matnr = gs_final-matnr.
    gv_lifnr = gs_final-lifnr.
    gv_pernr1 = p_pernr.
    gv_level  = gs_final-level.
    gv_r1 = r1.
    gv_r2 = r2.
    SET PARAMETER ID: 'PRNO'  FIELD gv_prno.
    SET PARAMETER ID: 'PERNR' FIELD gv_pernr1.
    SET PARAMETER ID: 'MATNR' FIELD gv_matnr.
    SET PARAMETER ID: 'LIFNR' FIELD gv_lifnr.
    SET PARAMETER ID: 'LEVEL' FIELD gv_level.
    SET PARAMETER ID: 'R1' FIELD gv_r1.
    SET PARAMETER ID: 'R2' FIELD gv_r2.
    CALL TRANSACTION 'ZMM017' AND SKIP FIRST SCREEN.
    WHEN OTHERS.
   ENDCASE.
    ENDIF.
  ENDMETHOD. "handle_double_click
ENDCLASS.                    "event_class IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_data .
  CLEAR p_pernr.
  p_pernr = sy-uname.
  CLEAR: flag1,
        flag1_1,
        flag1_2,
        flag1_3 ,
        flag2,
        flag3,
        flag4 ,
        flag_ok.
  SHIFT p_pernr LEFT DELETING LEADING '0'.
  IF p_pernr NE ' '.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_pernr
      IMPORTING
        output = p_pernr.
    CLEAR :gv_pernr,gs_zmm_pur_proposal1.
    REFRESH:gt_zmm_pur_proposal.

    SELECT *
        FROM zmm_pur_proposal INTO TABLE gt_zmm_pur_proposal
                        WHERE releaser1 = p_pernr OR
                              releaser2 = p_pernr OR
                              releaser3 = p_pernr OR
                              releaser4 = p_pernr.
    SORT gt_zmm_pur_proposal BY prno.
    DELETE ADJACENT DUPLICATES FROM gt_zmm_pur_proposal COMPARING ALL FIELDS.
    PERFORM final_data_approve.
  ELSE.
    MESSAGE 'No Data Found' TYPE 'E'.
  ENDIF.
ENDFORM.                    " SELECT_DATA


*&---------------------------------------------------------------------*
*&      Form  final_data_approve
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM final_data_approve.
  REFRESH: gt_final.
  CLEAR: gs_final,gs_zmm_pur_proposal,gs_final2.
  LOOP AT  gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
    SHIFT gs_zmm_pur_proposal-releaser1 LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser2 LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser3 LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser4 LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser1_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser2_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser3_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser4_date LEFT DELETING LEADING '0'.
    gs_final-prno            = gs_zmm_pur_proposal-prno.
    gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
    gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno.
    gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
    gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
    gs_final-eknam           = gs_zmm_pur_proposal-eknam.
    gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
    gs_final-werks           = gs_zmm_pur_proposal-werks.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-matnr
      IMPORTING
        output = gs_zmm_pur_proposal-matnr.

    gs_final-matnr           = gs_zmm_pur_proposal-matnr.
    gs_final-maktx           = gs_zmm_pur_proposal-maktx.
    gs_final-menge           = gs_zmm_pur_proposal-menge.
    gs_final-meins           = gs_zmm_pur_proposal-meins.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-lifnr
      IMPORTING
        output = gs_zmm_pur_proposal-lifnr.
    gs_final-lifnr            = gs_zmm_pur_proposal-lifnr.
    gs_final-name1            = gs_zmm_pur_proposal-name1.
    gs_final-netpr            = gs_zmm_pur_proposal-netpr.
    gs_final-waers            = gs_zmm_pur_proposal-waers.
    gs_final-land_pr          = gs_zmm_pur_proposal-land_pr.
    gs_final-mwskz            = gs_zmm_pur_proposal-mwskz.
    gs_final-name1            = gs_zmm_pur_proposal-name1.
    gs_final-land_value       = gs_zmm_pur_proposal-land_value.
    gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr .
    gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr.
    gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.
    gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.
    gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.
    gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
    gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
    gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
    gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
    gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
    gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
    gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
    gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
    gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
    gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
    gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
    gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
    gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
    gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
    gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
    gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
    gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
    gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
    gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
*          gs_final-level            = 1.
*          gs_final-color            = 'C410'.
    gs_final-releaser1        = gs_zmm_pur_proposal-releaser1.
    gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
    gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
    gs_final-releaser2        = gs_zmm_pur_proposal-releaser2.
    gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
    gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
    gs_final-releaser3        = gs_zmm_pur_proposal-releaser3.
    gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
    gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
    gs_final-releaser4        = gs_zmm_pur_proposal-releaser4.
    gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
    gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
    gs_final-app_vendor       = gs_zmm_pur_proposal-app_vendor.
    gs_final2 = gs_final.
    IF gs_zmm_pur_proposal-releaser1 NE ' '.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser1_date NE ' '.
          gs_final-level   = 1.
          gs_final-status1 = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser1_date EQ ' '.
              gs_final2-level            = 1.
              gs_final2-color            = 'C710'.
              gs_final2-status1         = 'Pending'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser2 NE ''.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser2_date NE ' '.
          gs_final-level            = 2.
          gs_final-status2          = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser2_date EQ ' '.
              gs_final2-level           = 2.
              gs_final2-status2         = 'Pending'.
              gs_final2-color            = 'C710'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser3 NE ' '.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser3_date NE ' '.
          gs_final-level            = 3.
          gs_final-status3          = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser3_date EQ ' '.
              gs_final2-level            = 3.
              gs_final2-status3          = 'Pending'.
              gs_final2-color            = 'C710'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser4 NE ' '.
      IF gs_zmm_pur_proposal-app_vendor = 'X'.
        IF gs_zmm_pur_proposal-releaser4_date NE ' '.
          gs_final-level            = 4.
          gs_final-status4          = 'Ok'.
          APPEND gs_final TO gt_final .
        ELSE.
          IF gs_zmm_pur_proposal-app_vendor = 'X'.
            IF gs_zmm_pur_proposal-releaser4_date EQ ' '.
              gs_final2-level            = 4.
              gs_final2-status4          = 'Pending'.
              gs_final2-color            = 'C710'.
              APPEND gs_final2 TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: gs_final,gs_zmm_pur_proposal,gs_final2.
  ENDLOOP.


  SORT gt_final BY prno.

  REFRESH gt_final2..
  gt_final2[] = gt_final[].
  REFRESH gt_final.
  DATA : flag1 TYPE c.
  SORT gt_final2 BY prno.
  LOOP AT gt_final2 INTO gs_final2.
    SHIFT gs_final2-releaser1 LEFT DELETING LEADING '0'.
    SHIFT gs_final2-releaser2 LEFT DELETING LEADING '0'.
    SHIFT gs_final2-releaser3 LEFT DELETING LEADING '0'.
    SHIFT gs_final2-releaser4 LEFT DELETING LEADING '0'.
    gs_final = gs_final2.
    AT END OF prno.
      flag1 = 'X'.
      IF flag1 = 'X'.
        APPEND gs_final TO gt_final.
      ENDIF.
    ENDAT.
    CLEAR :gs_final2,flag1,gs_final.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING prno.
  REFRESH gt_final2.
  gt_final2[] = gt_final[].
  REFRESH gt_final.
  CLEAR: gs_final,gs_final2.

  LOOP AT gt_final2 INTO gs_final2.
    PERFORM ok_pending.
  ENDLOOP.
  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING prno.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_pernr
    IMPORTING
      output = p_pernr.

  LOOP AT gt_final INTO gs_final..
    SHIFT gs_final-releaser1 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser1_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4_date LEFT DELETING LEADING '0'.
    CLEAR :flag1_1,flag1_2,flag1_3.

    IF gs_final-releaser1 NE ' ' AND gs_final-releaser2 NE ' '.
      IF gs_final-releaser1_date NE ' ' AND  gs_final-releaser2_date NE ' '.
        flag1_1 = 'X'.
        IF gs_final-releaser3 NE ' ' AND gs_final-releaser4 NE ' '.
          IF gs_final-releaser3_date NE ' ' AND  gs_final-releaser4_date NE ' '.
            flag1_2 = 'X'.
          ENDIF.
        ELSEIF gs_final-releaser3 NE ' ' AND gs_final-releaser4 EQ ' '.
          IF gs_final-releaser3_date NE ' ' AND  gs_final-releaser4_date EQ ' '.
            flag1_2 = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_final-releaser1 NE ' ' .
      IF gs_final-releaser1_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser2 NE ' ' .
      IF gs_final-releaser2_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser3 NE ' ' .
      IF gs_final-releaser3_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser4 NE ' ' .
      IF gs_final-releaser4_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser1 NE ' ' AND gs_final-releaser2 NE ' ' AND gs_final-releaser3 NE ' '
                                 AND gs_final-releaser4 NE ' ' AND gs_final-releaser4 NE ' '.
      IF gs_final-releaser1_date NE ' ' AND  gs_final-releaser2_date NE ' '
                                        AND gs_final-releaser3_date EQ ' ' AND gs_final-releaser4_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser1 NE ' ' AND gs_final-releaser2 NE ' ' AND gs_final-releaser3 NE ' '
                                  AND gs_final-releaser4 NE ' ' AND gs_final-releaser4 NE ' '.
      IF gs_final-releaser1_date NE ' ' AND  gs_final-releaser2_date NE ' '
                                        AND gs_final-releaser3_date NE ' ' AND gs_final-releaser4_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF flag1_1 EQ 'X' AND flag1_2 EQ 'X'.
      DELETE gt_final.
      flag1_3 = 'X'.
    ENDIF.
    IF flag1_3 NE 'X'.
      IF flag1_1 EQ 'X'.
        DELETE gt_final.
      ENDIF.
    ENDIF.
  ENDLOOP.
*
  LOOP AT gt_final INTO gs_final..
    SHIFT gs_final-releaser1 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser1_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4_date LEFT DELETING LEADING '0'.
    IF gs_final-releaser1_date NE ' '.
      gs_final-status1 = 'Ok'.
    ENDIF.
    IF gs_final-releaser2_date NE ' '.
      gs_final-status2 = 'Ok'.
    ENDIF.
    IF gs_final-releaser1_date EQ ' '.
      gs_final-status1 = 'Pending'.
    ENDIF.
    IF gs_final-releaser2_date EQ ' '.
      gs_final-status2 = 'Pending'.
    ENDIF.
    IF gs_final-releaser3 NE ' '.
      IF gs_final-releaser3_date NE ' '.
        gs_final-status3 = 'Ok'.
      ENDIF.
    ENDIF.
    IF gs_final-releaser3 NE ' '.
      IF gs_final-releaser3_date EQ ' '.
        gs_final-status3 = 'Pending'.
      ENDIF.
    ENDIF.
    IF gs_final-releaser4 NE ' '.
      IF gs_final-releaser4_date NE ' '.
        gs_final-status4 = 'Ok'.
      ENDIF.
    ENDIF.
    IF gs_final-releaser4 NE ' '.
      IF gs_final-releaser4_date EQ ' '.
        gs_final-status4 = 'Pending'.
      ENDIF.
    ENDIF.
    gs_final-pr_itemno  = gs_final-pr_itemno / 10.
    MODIFY gt_final FROM gs_final TRANSPORTING pr_itemno status1 status2 status3 status4.
    CLEAR gs_final.
  ENDLOOP.


""""""""""""""""""""""""""""""""""""""""""""""""""latest added for approve more than one material in proposal but different vendor
  DATA : lv_index TYPE sy-tabix.
  CLEAR flag_new.
  REFRESH:gt_final2.
  LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
    IF gs_zmm_pur_proposal-app_vendor = 'X'.
      lv_index = sy-tabix.
      flag_new = 'X'.
    ENDIF.
    IF lv_index NE ' '.
      LOOP AT gt_final INTO gs_final FROM lv_index..
        IF gs_final-prno = gs_zmm_pur_proposal-prno.
          gs_zmm_pur_proposal-pr_itemno = gs_zmm_pur_proposal-pr_itemno / 10.
          IF gs_final-pr_itemno = gs_zmm_pur_proposal-pr_itemno.
            flag1 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR gs_final.
      ENDLOOP.
    ENDIF.
    IF flag_new = 'X'.
      IF flag1 NE 'X'.
        CLEAR gs_final.
        PERFORM multiple_app_vendor.
        APPEND gs_final TO gt_final2.
      ENDIF.
    ENDIF.
    CLEAR: gs_zmm_pur_proposal,flag1,lv_index,flag_new.
  ENDLOOP.

  SORT gt_final2 BY prno pr_itemno.
  LOOP AT gt_final INTO gs_final.
*    lv_index = sy-tabix.
    LOOP AT gt_final2 INTO gs_final2." FROM lv_index.
      IF gs_final2-prno = gs_final-prno AND gs_final2-pr_itemno = gs_final-pr_itemno.
        DELETE gt_final2.
      ENDIF.
      CLEAR gs_final2.
    ENDLOOP.
    CLEAR :gs_final,lv_index.
  ENDLOOP.
  PERFORM delete_extra.  "from gt_final2
  APPEND LINES OF gt_final2 TO gt_final.
 refresh : gt_final2.
  LOOP AT gt_final INTO gs_final..
    SHIFT gs_final-releaser1 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser1_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4_date LEFT DELETING LEADING '0'.
    IF gs_final-releaser1_date NE ' '.
      gs_final-status1 = 'Ok'.
    ENDIF.
    IF gs_final-releaser2_date NE ' '.
      gs_final-status2 = 'Ok'.
    ENDIF.
    IF gs_final-releaser1_date EQ ' '.
      gs_final-status1 = 'Pending'.
    ENDIF.
    IF gs_final-releaser2_date EQ ' '.
      gs_final-status2 = 'Pending'.
    ENDIF.
    IF gs_final-releaser3 NE ' '.
      IF gs_final-releaser3_date NE ' '.
        gs_final-status3 = 'Ok'.
      ENDIF.
    ENDIF.
    IF gs_final-releaser3 NE ' '.
      IF gs_final-releaser3_date EQ ' '.
        gs_final-status3 = 'Pending'.
      ENDIF.
    ENDIF.
    IF gs_final-releaser4 NE ' '.
      IF gs_final-releaser4_date NE ' '.
        gs_final-status4 = 'Ok'.
      ENDIF.
    ENDIF.
    IF gs_final-releaser4 NE ' '.
      IF gs_final-releaser4_date EQ ' '.
        gs_final-status4 = 'Pending'.
      ENDIF.
    ENDIF.
    MODIFY gt_final FROM gs_final TRANSPORTING status1 status2 status3 status4.
    CLEAR gs_final.
  ENDLOOP.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = p_pernr
   IMPORTING
     OUTPUT        = p_pernr .

    LOOP AT gt_final INTO gs_final.
    IF gs_final-releaser1 EQ p_pernr.
      IF gs_final-app_vendor = 'X'.
        gs_final-level            = 1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser2 EQ p_pernr.
      IF gs_final-app_vendor = 'X'.
        gs_final-level            = 2.
      ENDIF.
    ENDIF.
    IF gs_final-releaser3 EQ p_pernr.
      IF gs_final-app_vendor = 'X'.
        gs_final-level            = 3.
      ENDIF.
    ENDIF.
    IF gs_final-releaser4 EQ p_pernr.
      IF gs_final-app_vendor = 'X'.
        gs_final-level            = 4.
      ENDIF.
    ENDIF.
    MODIFY gt_final FROM gs_final TRANSPORTING level.
    CLEAR gs_final.
  ENDLOOP.
  SORT gt_final by prno pr_itemno.
  IF gt_final IS INITIAL.
    MESSAGE 'No Data Found' TYPE 'E'.
    EXIT.
  ENDIF.
ENDFORM.                    " FINAL_DATA_U1

*&---------------------------------------------------------------------*
*&      Form  ok_pending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ok_pending.
  SHIFT gs_final2-releaser1 LEFT DELETING LEADING '0'.
  SHIFT gs_final2-releaser2 LEFT DELETING LEADING '0'.
  SHIFT gs_final2-releaser3 LEFT DELETING LEADING '0'.
  SHIFT gs_final2-releaser4 LEFT DELETING LEADING '0'.
  SHIFT gs_final2-releaser1_date LEFT DELETING LEADING '0'.
  SHIFT gs_final2-releaser2_date LEFT DELETING LEADING '0'.
  SHIFT gs_final2-releaser3_date LEFT DELETING LEADING '0'.
  SHIFT gs_final2-releaser4_date LEFT DELETING LEADING '0'.
  gs_final = gs_final2.
  IF gs_final2-releaser1 NE ' '.
    IF gs_final2-app_vendor = 'X'.
      IF gs_final2-releaser1_date NE ' '.
        gs_final2-level            = 1.
        gs_final2-status1          = 'Ok'.
        APPEND gs_final2 TO gt_final .
      ELSE.
        IF gs_final2-releaser1 NE ' '.
          IF gs_final2-app_vendor = 'X'.
            IF gs_final2-releaser1_date EQ ' '.
              gs_final-level            = 1.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              APPEND gs_final TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gs_final2-releaser2 NE ' '.
    IF gs_final2-app_vendor = 'X'.
      IF gs_final2-releaser2_date NE ' '.
        gs_final2-level            = 2.
        gs_final2-status1          = 'Ok'.
        APPEND gs_final2 TO gt_final .
      ELSE.
        IF gs_final2-releaser2 NE ' '.
          IF gs_final2-app_vendor = 'X'.
            IF gs_final2-releaser2_date EQ ' '.
              gs_final-level            = 2.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              APPEND gs_final TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gs_final2-releaser3 NE ' '.
    IF gs_final2-app_vendor = 'X'.
      IF gs_final2-releaser3_date NE ' '.
        gs_final2-level            = 3.
        gs_final2-status1          = 'Ok'.
        APPEND gs_final2 TO gt_final .
      ELSE.
        IF gs_final2-releaser3 NE ' '.
          IF gs_final2-app_vendor = 'X'.
            IF gs_final2-releaser3_date EQ ' '.
              gs_final-level            = 3.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              APPEND gs_final TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gs_final2-releaser4 NE ' '.
    IF gs_final2-app_vendor = 'X'.
      IF gs_final2-releaser4_date NE ' '.
        gs_final2-level            = 4.
        gs_final2-status1          = 'Ok'.
        APPEND gs_final2 TO gt_final .
      ELSE.
        IF gs_final2-releaser4 NE ' '.
          IF gs_final2-app_vendor = 'X'.
            IF gs_final2-releaser4_date EQ ' '.
              gs_final-level            = 4.
              gs_final-status1          = 'Pending'.
              gs_final-color            = 'C710'.
              APPEND gs_final TO gt_final .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "ok_pending
*&---------------------------------------------------------------------*
*&      Form  MULTI_APP_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM multiple_app_vendor .
  gs_final-prno            = gs_zmm_pur_proposal-prno.
  gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
  gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno / 10 .
  gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
  gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
  gs_final-eknam           = gs_zmm_pur_proposal-eknam.
  gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
  gs_final-werks           = gs_zmm_pur_proposal-werks.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_zmm_pur_proposal-matnr
    IMPORTING
      output = gs_zmm_pur_proposal-matnr.

  gs_final-matnr           = gs_zmm_pur_proposal-matnr.
  gs_final-maktx           = gs_zmm_pur_proposal-maktx.
  gs_final-menge           = gs_zmm_pur_proposal-menge.
  gs_final-meins           = gs_zmm_pur_proposal-meins.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_zmm_pur_proposal-lifnr
    IMPORTING
      output = gs_zmm_pur_proposal-lifnr.
  gs_final-lifnr            = gs_zmm_pur_proposal-lifnr.
  gs_final-name1            = gs_zmm_pur_proposal-name1.
  gs_final-netpr            = gs_zmm_pur_proposal-netpr.
  gs_final-waers            = gs_zmm_pur_proposal-waers.
  gs_final-land_pr          = gs_zmm_pur_proposal-land_pr.
  gs_final-mwskz            = gs_zmm_pur_proposal-mwskz.
  gs_final-name1            = gs_zmm_pur_proposal-name1.
  gs_final-land_value       = gs_zmm_pur_proposal-land_value.
  gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr .
  gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr.
  gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.
  gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.
  gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.
  gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
  gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
  gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
  gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
  gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
  gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
  gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
  gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
  gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
  gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
  gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
  gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
  gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
  gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
  gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
  gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
  gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
  gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
  gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
*          gs_final-level            = 1.
*          gs_final-color            = 'C410'.
  gs_final-releaser1        = gs_zmm_pur_proposal-releaser1.
  gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
  gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
  gs_final-releaser2        = gs_zmm_pur_proposal-releaser2.
  gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
  gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
  gs_final-releaser3        = gs_zmm_pur_proposal-releaser3.
  gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
  gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
  gs_final-releaser4        = gs_zmm_pur_proposal-releaser4.
  gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
  gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
  gs_final-app_vendor       = gs_zmm_pur_proposal-app_vendor.

ENDFORM.                    " MULTI_APP_VENDOR
*&---------------------------------------------------------------------*
*&      Form  DELETE_EXTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_EXTRA .
 loop at gt_final2 INTO gs_final.
   SHIFT gs_final-releaser1 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4 LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser1_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser2_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser3_date LEFT DELETING LEADING '0'.
    SHIFT gs_final-releaser4_date LEFT DELETING LEADING '0'.
    CLEAR :flag1_1,flag1_2,flag1_3.

    IF gs_final-releaser1 NE ' ' AND gs_final-releaser2 NE ' '.
      IF gs_final-releaser1_date NE ' ' AND  gs_final-releaser2_date NE ' '.
        flag1_1 = 'X'.
        IF gs_final-releaser3 NE ' ' AND gs_final-releaser4 NE ' '.
          IF gs_final-releaser3_date NE ' ' AND  gs_final-releaser4_date NE ' '.
            flag1_2 = 'X'.
          ENDIF.
        ELSEIF gs_final-releaser3 NE ' ' AND gs_final-releaser4 EQ ' '.
          IF gs_final-releaser3_date NE ' ' AND  gs_final-releaser4_date EQ ' '.
            flag1_2 = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_final-releaser1 NE ' ' .
      IF gs_final-releaser1_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser2 NE ' ' .
      IF gs_final-releaser2_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser3 NE ' ' .
      IF gs_final-releaser3_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser4 NE ' ' .
      IF gs_final-releaser4_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser1 NE ' ' AND gs_final-releaser2 NE ' ' AND gs_final-releaser3 NE ' '
                                 AND gs_final-releaser4 NE ' ' AND gs_final-releaser4 NE ' '.
      IF gs_final-releaser1_date NE ' ' AND  gs_final-releaser2_date NE ' '
                                        AND gs_final-releaser3_date EQ ' ' AND gs_final-releaser4_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF gs_final-releaser1 NE ' ' AND gs_final-releaser2 NE ' ' AND gs_final-releaser3 NE ' '
                                  AND gs_final-releaser4 NE ' ' AND gs_final-releaser4 NE ' '.
      IF gs_final-releaser1_date NE ' ' AND  gs_final-releaser2_date NE ' '
                                        AND gs_final-releaser3_date NE ' ' AND gs_final-releaser4_date EQ ' '.
        CLEAR flag1_1.
      ENDIF.
    ENDIF.
    IF flag1_1 EQ 'X' AND flag1_2 EQ 'X'.
      DELETE gt_final2.
      flag1_3 = 'X'.
    ENDIF.
    IF flag1_3 NE 'X'.
      IF flag1_1 EQ 'X'.
        DELETE gt_final2.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DELETE_EXTRA
*&---------------------------------------------------------------------*
*&      Form  fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fieldcat .
  REFRESH: gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname  = 'PRNO'.
  gs_fieldcat-ref_table  = 'GT_FINAL'.
  gs_fieldcat-coltext    = 'Proposal Number'.
  gs_fieldcat-hotspot    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_ITEMNO'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Best Proposal Line'.
  APPEND gs_fieldcat TO gt_fieldcat.
*  CLEAR gs_fieldcat.
*  gs_fieldcat-fieldname = 'LEVEL'.
*  gs_fieldcat-ref_table   = 'GT_FINAL'.
*  gs_fieldcat-coltext = 'Releaser Level'.
*  gs_fieldcat-just = 'C'.
*  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME1'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name1'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS1'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Status 1'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME2'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name2'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS2'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Status 2'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME3'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name3'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS3'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Status 3'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME4'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name4'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STATUS4'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Status 4'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_DATE'.
  gs_fieldcat-inttype   = 'D'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Proposal Date'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'EKGRP'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Purchase Group'.
  gs_fieldcat-Just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'WERKS'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Plant'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MATNR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Material No'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MAKTX'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Material Description'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LIFNR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Vendor No'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NAME1'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Vendor Name'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MENGE'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Quantity'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NETPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'New Basic Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MWSKZ'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Tax Code'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_PR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'New Landed Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_VALUE'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Purchase Value'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_BASICPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Last Purchase Basic PR(PO)'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_LANDPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Last Purchase Landed Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'BUSINESS_PLANPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Business Plan Price(ZBPL)'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext = 'Cost Plus Formula'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA1'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Competitor Data '.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
ENDFORM.                    " FIELDCAT


*&---------------------------------------------------------------------*
*&      Form  free_objects
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM free_objects .
  IF alv_grid IS NOT INITIAL.
    CALL METHOD alv_grid->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
*  if sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  endif.
  ENDIF.
  IF alv_container IS NOT INITIAL.
    CALL METHOD alv_container->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
*  if sy-subrc <> 0.
*    message id sy-msgid type sy-msgty number sy-msgno
*               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  endif.
    CALL METHOD cl_gui_cfw=>flush.
    CALL METHOD cl_gui_cfw=>dispatch.
  ENDIF.
ENDFORM.                    " FREE_OBJECTS

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_cancel .
  CLEAR p_pernr.
  p_pernr = sy-uname.
  SHIFT p_pernr LEFT DELETING LEADING '0'.
  IF p_pernr NE ' '.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_pernr
      IMPORTING
        output = p_pernr.

    CLEAR gv_pernr.
    REFRESH :gt_zmm_pur_proposal,gt_zmm_pur_proposal1,gt_final.
    SELECT * FROM zmm_pur_proposal INTO TABLE gt_zmm_pur_proposal1
                        WHERE releaser1 = p_pernr OR
                              releaser2 = p_pernr OR
                              releaser3 = p_pernr OR
                              releaser4  = p_pernr.
    SORT gt_zmm_pur_proposal BY prno.
  ENDIF.
  IF gt_zmm_pur_proposal1 IS NOT INITIAL.
    LOOP AT gt_zmm_pur_proposal1 INTO gs_zmm_pur_proposal1 WHERE prno = s_prno-low.
      IF gs_zmm_pur_proposal1-releaser1_date IS NOT INITIAL .
        APPEND gs_zmm_pur_proposal1 TO gt_zmm_pur_proposal.
      ENDIF.
      IF gs_zmm_pur_proposal1-releaser2_date IS NOT INITIAL .
        APPEND gs_zmm_pur_proposal1 TO gt_zmm_pur_proposal.
      ENDIF.
      IF gs_zmm_pur_proposal1-releaser3_date IS NOT INITIAL .
        APPEND gs_zmm_pur_proposal1 TO gt_zmm_pur_proposal.
      ENDIF.
      IF gs_zmm_pur_proposal1-releaser4_date IS NOT INITIAL .
        APPEND gs_zmm_pur_proposal1 TO gt_zmm_pur_proposal.
      ENDIF.
      CLEAR gs_zmm_pur_proposal1.
    ENDLOOP.
  ENDIF.

  PERFORM final_data_cancel.


ENDFORM.                    " SELECT_DATA_CANCEL

*&---------------------------------------------------------------------*
*&      Form  FINAL_DATA_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM final_data_cancel .

  LOOP AT  gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
    SHIFT gs_zmm_pur_proposal-releaser1_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser2_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser3_date LEFT DELETING LEADING '0'.
    SHIFT gs_zmm_pur_proposal-releaser4_date LEFT DELETING LEADING '0'.
    IF gs_zmm_pur_proposal-releaser1 = p_pernr.
      IF gs_zmm_pur_proposal-releaser1_date NE ' '.
        IF gs_zmm_pur_proposal-app_vendor = 'X'.
          gs_final-prno            = gs_zmm_pur_proposal-prno.
          gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
          gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno.
          gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
          gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
          gs_final-eknam           = gs_zmm_pur_proposal-eknam.
          gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
          gs_final-werks           = gs_zmm_pur_proposal-werks.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-matnr
            IMPORTING
              output = gs_zmm_pur_proposal-matnr.

          gs_final-matnr           = gs_zmm_pur_proposal-matnr.
          gs_final-maktx           = gs_zmm_pur_proposal-maktx.
          gs_final-menge           = gs_zmm_pur_proposal-menge.
          gs_final-meins           = gs_zmm_pur_proposal-meins.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-lifnr
            IMPORTING
              output = gs_zmm_pur_proposal-lifnr.
          gs_final-lifnr            = gs_zmm_pur_proposal-lifnr.
          gs_final-name1            = gs_zmm_pur_proposal-name1.
          gs_final-netpr            = gs_zmm_pur_proposal-netpr.
          gs_final-waers            = gs_zmm_pur_proposal-waers.
          gs_final-land_pr          = gs_zmm_pur_proposal-land_pr.
          gs_final-mwskz            = gs_zmm_pur_proposal-mwskz.
          gs_final-name1            = gs_zmm_pur_proposal-name1.
          gs_final-land_value       = gs_zmm_pur_proposal-land_value.     " New_Landed_Value
          gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr . "Last_Purchase_Basic_Price(PO)
          gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr. "Last_Purchase_Landed_Price
          gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.       "Business_Plan_Price(ZBPL)
          gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.    "Variance_Last_Purchase_INR_Per_KG
          gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.    "Variance_Last_Purchase_Value
          gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
          gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
          gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
          gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
          gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
          gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
          gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
          gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
          gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
          gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
          gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
          gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
          gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
          gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
          gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
          gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
          gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
          gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
          gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
          gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
          gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
          gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
          gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
          gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
          gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
          gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
          gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
          APPEND gs_final TO gt_final.
        ENDIF.
      ENDIF.
    ENDIF.

    IF gs_zmm_pur_proposal-releaser2 = p_pernr.
      IF gs_zmm_pur_proposal-releaser2_date NE ' '.
        IF gs_zmm_pur_proposal-app_vendor = 'X'.
          gs_final-prno            = gs_zmm_pur_proposal-prno.
          gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
          gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno.
          gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
          gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
          gs_final-eknam           = gs_zmm_pur_proposal-eknam.
          gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
          gs_final-werks           = gs_zmm_pur_proposal-werks.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-matnr
            IMPORTING
              output = gs_zmm_pur_proposal-matnr.

          gs_final-matnr           = gs_zmm_pur_proposal-matnr.
          gs_final-maktx           = gs_zmm_pur_proposal-maktx.
          gs_final-menge           = gs_zmm_pur_proposal-menge.
          gs_final-meins           = gs_zmm_pur_proposal-meins.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-lifnr
            IMPORTING
              output = gs_zmm_pur_proposal-lifnr.
          gs_final-lifnr           = gs_zmm_pur_proposal-lifnr.
          gs_final-name1           = gs_zmm_pur_proposal-name1.
          gs_final-netpr           = gs_zmm_pur_proposal-netpr.
          gs_final-waers           = gs_zmm_pur_proposal-waers.
          gs_final-land_pr         = gs_zmm_pur_proposal-land_pr.
          gs_final-mwskz           = gs_zmm_pur_proposal-mwskz.
          gs_final-name1            = gs_zmm_pur_proposal-name1.
          gs_final-land_value       = gs_zmm_pur_proposal-land_value.     " New_Landed_Value
          gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr . "Last_Purchase_Basic_Price(PO)
          gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr. "Last_Purchase_Landed_Price
          gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.       "Business_Plan_Price(ZBPL)
          gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.    "Variance_Last_Purchase_INR_Per_KG
          gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.    "Variance_Last_Purchase_Value
          gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
          gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
          gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
          gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
          gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
          gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
          gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
          gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
          gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
          gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
          gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
          gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
          gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
          gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
          gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
          gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
          gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
          gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
          gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
          gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
          gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
          gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
          gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
           gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
          gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
          gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
          gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
          APPEND gs_final TO gt_final.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser3 = p_pernr.
      IF gs_zmm_pur_proposal-releaser3_date NE ' '.
        IF gs_zmm_pur_proposal-app_vendor = 'X'.
          gs_final-prno            = gs_zmm_pur_proposal-prno.
          gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
          gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno.
          gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
          gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
          gs_final-eknam           = gs_zmm_pur_proposal-eknam.
          gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
          gs_final-werks           = gs_zmm_pur_proposal-werks.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-matnr
            IMPORTING
              output = gs_zmm_pur_proposal-matnr.

          gs_final-matnr           = gs_zmm_pur_proposal-matnr.
          gs_final-maktx           = gs_zmm_pur_proposal-maktx.
          gs_final-menge           = gs_zmm_pur_proposal-menge.
          gs_final-meins           = gs_zmm_pur_proposal-meins.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-lifnr
            IMPORTING
              output = gs_zmm_pur_proposal-lifnr.
          gs_final-lifnr           = gs_zmm_pur_proposal-lifnr.
          gs_final-name1           = gs_zmm_pur_proposal-name1.
          gs_final-netpr           = gs_zmm_pur_proposal-netpr.
          gs_final-waers           = gs_zmm_pur_proposal-waers.
          gs_final-land_pr         = gs_zmm_pur_proposal-land_pr.
          gs_final-mwskz           = gs_zmm_pur_proposal-mwskz.
          gs_final-name1            = gs_zmm_pur_proposal-name1.
          gs_final-land_value       = gs_zmm_pur_proposal-land_value.     " New_Landed_Value
          gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr . "Last_Purchase_Basic_Price(PO)
          gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr. "Last_Purchase_Landed_Price
          gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.       "Business_Plan_Price(ZBPL)
          gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.    "Variance_Last_Purchase_INR_Per_KG
          gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.    "Variance_Last_Purchase_Value
          gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
          gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
          gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
          gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
          gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
          gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
          gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
          gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
          gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
          gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
          gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
          gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
          gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
          gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
          gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
          gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
          gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
          gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
          gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
          gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
          gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
          gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
          gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
           gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
          gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
          gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
          gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
          APPEND gs_final TO gt_final.
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_zmm_pur_proposal-releaser4 = p_pernr.
      IF gs_zmm_pur_proposal-releaser4_date NE ' '.
        IF gs_zmm_pur_proposal-app_vendor = 'X'.
          gs_final-prno            = gs_zmm_pur_proposal-prno.
          gs_final-gjahr           = gs_zmm_pur_proposal-gjahr.
          gs_final-pr_itemno       = gs_zmm_pur_proposal-pr_itemno.
          gs_final-pr_date         = gs_zmm_pur_proposal-pr_date.
          gs_final-ekgrp           = gs_zmm_pur_proposal-ekgrp.
          gs_final-eknam           = gs_zmm_pur_proposal-eknam.
          gs_final-bukrs           = gs_zmm_pur_proposal-bukrs.
          gs_final-werks           = gs_zmm_pur_proposal-werks.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-matnr
            IMPORTING
              output = gs_zmm_pur_proposal-matnr.

          gs_final-matnr           = gs_zmm_pur_proposal-matnr.
          gs_final-maktx           = gs_zmm_pur_proposal-maktx.
          gs_final-menge           = gs_zmm_pur_proposal-menge.
          gs_final-meins           = gs_zmm_pur_proposal-meins.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = gs_zmm_pur_proposal-lifnr
            IMPORTING
              output = gs_zmm_pur_proposal-lifnr.
          gs_final-lifnr           = gs_zmm_pur_proposal-lifnr.
          gs_final-name1           = gs_zmm_pur_proposal-name1.
          gs_final-netpr           = gs_zmm_pur_proposal-netpr.
          gs_final-waers           = gs_zmm_pur_proposal-waers.
          gs_final-land_pr         = gs_zmm_pur_proposal-land_pr.
          gs_final-mwskz           = gs_zmm_pur_proposal-mwskz.
          gs_final-name1            = gs_zmm_pur_proposal-name1.
          gs_final-land_value       = gs_zmm_pur_proposal-land_value.     " New_Landed_Value
          gs_final-last_basicpr     = gs_zmm_pur_proposal-last_basicpr . "Last_Purchase_Basic_Price(PO)
          gs_final-last_landpr      = gs_zmm_pur_proposal-last_landpr. "Last_Purchase_Landed_Price
          gs_final-business_planpr  = gs_zmm_pur_proposal-business_planpr.       "Business_Plan_Price(ZBPL)
          gs_final-vari_last_pur_in = gs_zmm_pur_proposal-vari_last_pur_in.    "Variance_Last_Purchase_INR_Per_KG
          gs_final-vari_last_pur_va = gs_zmm_pur_proposal-vari_last_pur_va.    "Variance_Last_Purchase_Value
          gs_final-vari_bus_pl_inr  = gs_zmm_pur_proposal-vari_bus_pl_inr.
          gs_final-vari_bus_pl_val  = gs_zmm_pur_proposal-vari_bus_pl_val.
          gs_final-opening_m1       = gs_zmm_pur_proposal-opening_m1.
          gs_final-requirment_m1    = gs_zmm_pur_proposal-requirment_m1.
          gs_final-app_vend_m1      = gs_zmm_pur_proposal-app_vend_m1.
          gs_final-other_vend_m1    = gs_zmm_pur_proposal-other_vend_m1.
          gs_final-closing_m1       = gs_zmm_pur_proposal-closing_m1.
          gs_final-opening_m2       = gs_zmm_pur_proposal-opening_m2.
          gs_final-requirment_m2    = gs_zmm_pur_proposal-requirment_m2.
          gs_final-app_vend_m2      = gs_zmm_pur_proposal-app_vend_m2.
          gs_final-other_vend_m2    = gs_zmm_pur_proposal-other_vend_m2.
          gs_final-closing_m2       = gs_zmm_pur_proposal-closing_m2.
          gs_final-opening_m3       = gs_zmm_pur_proposal-opening_m3.
          gs_final-requirment_m3    = gs_zmm_pur_proposal-requirment_m3.
          gs_final-app_vend_m3      = gs_zmm_pur_proposal-app_vend_m3.
          gs_final-other_vend_m3    = gs_zmm_pur_proposal-other_vend_m3.
          gs_final-comp_data        = gs_zmm_pur_proposal-comp_data.
          gs_final-comp_data1       = gs_zmm_pur_proposal-comp_data1.
          gs_final-line_txt         = gs_zmm_pur_proposal-line_txt.
          gs_final-releaser1_date   = gs_zmm_pur_proposal-releaser1_date.
          gs_final-releaser2_date   = gs_zmm_pur_proposal-releaser2_date.
          gs_final-releaser3_date   = gs_zmm_pur_proposal-releaser3_date.
          gs_final-releaser4_date   = gs_zmm_pur_proposal-releaser4_date.
           gs_final-releaser_name1   = gs_zmm_pur_proposal-releaser_name1.
          gs_final-releaser_name2   = gs_zmm_pur_proposal-releaser_name2.
          gs_final-releaser_name3   = gs_zmm_pur_proposal-releaser_name3.
          gs_final-releaser_name4   = gs_zmm_pur_proposal-releaser_name4.
          APPEND gs_final TO gt_final.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR : gs_zmm_pur_proposal,gs_final.
  ENDLOOP.
  SORT gt_final BY prno.
  DELETE ADJACENT DUPLICATES FROM gt_final COMPARING ALL FIELDS .
  LOOP AT gt_final INTO gs_final.
    gs_final-pr_itemno = gs_final-pr_itemno / 10.
*   GS_FINAL-LINE_COLOR = 'C511'.
    MODIFY gt_final FROM gs_final TRANSPORTING pr_itemno .
    CLEAR gs_final.
  ENDLOOP.

  IF gt_final IS INITIAL.
    MESSAGE 'No Data Found' TYPE 'E'.
  ENDIF.
ENDFORM.                    " SELECT_DATA_CANCEL
*&---------------------------------------------------------------------*
*&      Module  STATUS_0901  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0901 OUTPUT.
  IF alv_container IS INITIAL.
    SET PF-STATUS 'ZPF901'.
    SET TITLEBAR 'ZTIT901'.
    CREATE OBJECT alv_container
      EXPORTING
        container_name = 'ALV_CONTAINER'.
    IF sy-subrc = 0.
      CREATE OBJECT alv_grid
        EXPORTING
          i_parent = alv_container.
    ENDIF.

    w_variant-report = sy-repid.
    gs_layout-zebra = 'X'.
*   gs_layout-info_fname = 'COLOR'.
    gs_layout-grid_title = 'List'.
    gs_layout-cwidth_opt  = 'X'.
    PERFORM fieldcat.
    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
        is_variant      = w_variant
        i_save          = 'A'
        is_layout       = gs_layout
      CHANGING
        it_outtab       = gt_final[]
        it_fieldcatalog = gt_fieldcat[]. "it_fieldcatalog
*       PERFORM fieldcat.
    CALL METHOD alv_grid->refresh_table_display.

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->on_hotspot_click FOR alv_grid.

  ENDIF.

  IF flag = 'X'.
    CLEAR flag.
    SET PF-STATUS 'ZPF901'.
    SET TITLEBAR 'ZTIT901'.
    CREATE OBJECT alv_container1
      EXPORTING
        container_name = 'ALV_CONTAINER'.
    IF sy-subrc = 0.
      CREATE OBJECT alv_grid1
        EXPORTING
          i_parent = alv_container1.
      w_variant-report = sy-repid.
      gs_layout-zebra = 'X'.
*   gs_layout-info_fname = 'COLOR'.
      gs_layout-grid_title = 'List'.
      gs_layout-cwidth_opt  = 'X'.
      PERFORM fieldcat.

      CALL METHOD alv_grid1->set_table_for_first_display
        EXPORTING
          is_variant      = w_variant
          i_save          = 'A'
          is_layout       = gs_layout
        CHANGING
          it_outtab       = gt_final[]
          it_fieldcatalog = gt_fieldcat[]. "it_fieldcatalog
*       PERFORM fieldcat.
      CALL METHOD alv_grid1->refresh_table_display.

      CREATE OBJECT event_receiver.
      SET HANDLER event_receiver->on_hotspot_click FOR alv_grid1.
    ENDIF.


  ENDIF.

ENDMODULE.                 " STATUS_0901  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0901  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0901 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
      PERFORM free_objects.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      IF r1 = 'X'.
        flag = 'X'.
        PERFORM free_objects.
        PERFORM free_objects1.
        PERFORM select_data.
        IF gt_final IS NOT INITIAL.
          CALL SCREEN 901.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0901  INPUT
*&---------------------------------------------------------------------*
*&      Form  FREE_OBJECTS1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_objects1 .
  IF alv_grid1 IS NOT INITIAL.
    CALL METHOD alv_grid1->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
  IF alv_container1 IS NOT INITIAL.
    CALL METHOD alv_container1->free
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>dispatch.
ENDFORM.                    " FREE_OBJECTS1

*&---------------------------------------------------------------------*
*&      Module  STATUS_0902  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0902 OUTPUT.

  IF alv_container IS INITIAL.
    SET PF-STATUS 'ZPF902'.
    SET TITLEBAR 'ZTIT902'.
    CREATE OBJECT alv_container
      EXPORTING
        container_name = 'ALV_CONTAINER1'.
    IF sy-subrc = 0.
      CREATE OBJECT alv_grid
        EXPORTING
          i_parent = alv_container.
    ENDIF.
    w_variant-report = sy-repid.
    gs_layout-zebra = 'X'.
*   gs_layout-info_fname = 'COLOR'.
    gs_layout-grid_title = 'List'.
    gs_layout-cwidth_opt  = 'X'.
    PERFORM fieldcat1.
    CALL METHOD alv_grid->set_table_for_first_display
      EXPORTING
        is_variant      = w_variant
        i_save          = 'A'
        is_layout       = gs_layout
      CHANGING
        it_outtab       = gt_final[]
        it_fieldcatalog = gt_fieldcat[]. "it_fieldcatalog
*       PERFORM fieldcat.
    CALL METHOD alv_grid->refresh_table_display.

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->on_hotspot_click FOR alv_grid.

  ENDIF.

  IF flag = 'X'.
    CLEAR flag.
    SET PF-STATUS 'ZPF902'.
    SET TITLEBAR 'ZTIT902'.
    CREATE OBJECT alv_container1
      EXPORTING
        container_name = 'ALV_CONTAINER1'.
    IF sy-subrc = 0.
      CREATE OBJECT alv_grid1
        EXPORTING
          i_parent = alv_container1.
      w_variant-report = sy-repid.
      gs_layout-zebra = 'X'.
*   gs_layout-info_fname = 'COLOR'.
      gs_layout-grid_title = 'List'.
      gs_layout-cwidth_opt  = 'X'.
      PERFORM fieldcat1.

      CALL METHOD alv_grid1->set_table_for_first_display
        EXPORTING
          is_variant      = w_variant
          i_save          = 'A'
          is_layout       = gs_layout
        CHANGING
          it_outtab       = gt_final[]
          it_fieldcatalog = gt_fieldcat[]. "it_fieldcatalog
*       PERFORM fieldcat.
      CALL METHOD alv_grid1->refresh_table_display.

      CREATE OBJECT event_receiver.
      SET HANDLER event_receiver->on_hotspot_click FOR alv_grid1.
    ENDIF.
  ENDIF.
ENDMODULE.                 " STATUS_0902  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0902  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0902 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
      PERFORM free_objects.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANREF'.
      IF r2 = 'X'.
        PERFORM free_objects.
        PERFORM free_objects1.
        PERFORM select_data_cancel.
        IF gt_final IS NOT INITIAL.
          CALL SCREEN 901.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0902  INPUT

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat1 .
  REFRESH: gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname  = 'PRNO'.
  gs_fieldcat-ref_table  = 'GT_FINAL'.
  gs_fieldcat-coltext    = 'Proposal Number'.
  gs_fieldcat-hotspot    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_ITEMNO'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Proposal Line Number'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PR_DATE'.
  gs_fieldcat-inttype   = 'D'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Proposal Date'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME1'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name1'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER1_DATE'.
  gs_fieldcat-inttype   = 'D'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser1 Date'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME2'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name2'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER2_DATE'.
   gs_fieldcat-inttype  = 'D'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser2 Date'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

  gs_fieldcat-fieldname = 'RELEASER_NAME3'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name3'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER3_DATE'.
   gs_fieldcat-inttype  = 'D'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser3 Date'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER_NAME4'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser Name4'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RELEASER4_DATE'.
   gs_fieldcat-inttype  = 'D'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Releaser4 Date'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'EKGRP'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Purchase Group'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'WERKS'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Plant'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MATNR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Material No'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MAKTX'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Material Description'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LIFNR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Vendor No'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NAME1'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Vendor Name'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MENGE'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Quantity'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NETPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'New Basic Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MWSKZ'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Tax Code'.
  gs_fieldcat-just      = 'C'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_PR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'New Landed Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAND_VALUE'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Purchase Value'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_BASICPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Last Purchase Basic PR(PO)'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LAST_LANDPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Last Purchase Landed Price'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'BUSINESS_PLANPR'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Business Plan Price(ZBPL)'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Cost Plus Formula'.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'COMP_DATA1'.
  gs_fieldcat-ref_table = 'GT_FINAL'.
  gs_fieldcat-coltext   = 'Competitor Data '.
  APPEND gs_fieldcat TO gt_fieldcat.
  CLEAR gs_fieldcat.

ENDFORM.                    " FIELDCAT1
*&---------------------------------------------------------------------*
*&      Form  F4_PERNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_perno .
  REFRESH: lt_f4_prno,lt_f4_prno1.
  SELECT prno releaser1 releaser2 releaser3 releaser4 FROM zmm_pur_proposal INTO TABLE lt_f4_prno
                        WHERE releaser1 = sy-uname OR
                              releaser2 = sy-uname OR
                              releaser3 = sy-uname OR
                              releaser4  = sy-uname.

  SORT lt_f4_prno BY prno.
  DELETE ADJACENT DUPLICATES FROM lt_f4_prno COMPARING prno.
*  DELETE lt_f4_prno WHERE pr_user <> sy-uname.
  LOOP AT lt_f4_prno INTO ls_f4_prno.
    APPEND ls_f4_prno-prno TO lt_f4_prno1.
    CLEAR ls_f4_prno.
  ENDLOOP.
  REFRESH it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'S_PRNO-LOW'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = sy-dynnr
     dynprofield = 'S_PRNO-LOW'
     TABLES
     value_tab   = lt_f4_prno1
     return_tab     = it_ret
  EXCEPTIONS
   parameter_error = 1
   no_values_found = 2
   OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    s_prno-low = it_ret-fieldval.
  ENDIF.
ENDFORM.                    " F4_PERNO
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTHORIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_authorization .
  DATA lv_pernr TYPE pernr.
  CLEAR lv_pernr.

  SELECT SINGLE pernr FROM pa0105 INTO lv_pernr WHERE uname = sy-uname AND subty = '0010'
                                   AND aedtm LE sy-datum AND endda GE sy-datum. .

  AUTHORITY-CHECK OBJECT 'Y_PROPOSAL'
                  ID 'ACTVT' FIELD '01'
                  ID 'ACTVT' FIELD '02'
                  ID 'ACTVT' FIELD '03'
                  ID 'ACTVT' FIELD '16'
                  ID 'PERNR' FIELD lv_pernr.
*                  .
  IF sy-subrc NE 0.
    MESSAGE 'You are not Authorized Person' TYPE 'E'.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " CHECK_AUTHORIZATION
