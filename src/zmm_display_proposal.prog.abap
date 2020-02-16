*&***********************************************************************************************&*
*& OBJECT NAME          : ZMM_DISPLAY_PROPOSAL                                                   &*
*& TECHNICAL CONSULTANT : RAVINDRA SANAP                                                         &*
*& TEAM LEAD            : POONAM SHINDE                                                          &*
*& FUCTIONAL            : VENU                                                                   &*
*& MODULE NAME          : MM                                                                     &*
*& PROGRAM TYPE         : REPORT                                                                 &*
*& CREATE DATE          : DEC 08,2016                                                            &*
*& TCODE                : ZMM017                                                                 &*
*& TRANSPORT NO         : IRDK926351                                                             &*
*& DESCRIPTION          : THIS REPORT IS DEVELOPED TO DISPLAY PROPOSAL DETAILS                   &*
*&                                                                                               &*
* REVISION HISTORY--------------------------------------------------------------------------------*
*                                                                                                 *
*   CHANGED BY:                                                                                   *
*   CHANGE ON:                                                                                    *
*   REASON FOR CHANGE:                                                                            *
*   IRDK926485                                                                                              *
*                                                                                                 *
* REVISION HISTORY--------------------------------------------------------------------------------*

REPORT  zmm_display_proposal.

INCLUDE zmm_include_proposal.
GET PARAMETER ID : 'PRNO' FIELD gv_prno.
GET PARAMETER ID : 'PERNR' FIELD gv_pernr1.
GET PARAMETER ID : 'MATNR' FIELD gv_matnr.
GET PARAMETER ID : 'LEVEL' FIELD gv_level.
GET PARAMETER ID : 'R1' FIELD gv_r1.
GET PARAMETER ID : 'R2' FIELD gv_r2.

START-OF-SELECTION.
  PERFORM refresh.
  PERFORM get_data.
  CALL SCREEN 050.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_data .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = gv_prno
   IMPORTING
     OUTPUT        = gv_prno.

  SELECT * FROM zmm_pur_proposal INTO CORRESPONDING FIELDS OF TABLE gt_zmm_pur_proposal WHERE prno = gv_prno.
  IF sy-subrc = 0.
    REFRESH: gt_tab1.
    CLEAR : gs_tab1.
*  SET PARAMETER ID 'PRNO' FIELD SPACE.
    LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal .
      gv_ekgrp          = gs_zmm_pur_proposal-ekgrp.
      gv_eknam          = gs_zmm_pur_proposal-eknam.
      gv_werks          = gs_zmm_pur_proposal-werks.
      gv_plantname      = gs_zmm_pur_proposal-plantname.
      gv_date           = gs_zmm_pur_proposal-pr_date.
      gv_chdt           = gs_zmm_pur_proposal-ch_date.
      gv_user           = gs_zmm_pur_proposal-pr_user.
      gv_time           = gs_zmm_pur_proposal-pr_time .
      gs_tab1-chk                     =  gs_zmm_pur_proposal-app_vendor .
      gv_prno                         =  gs_zmm_pur_proposal-prno.
      gs_tab1-gjahr                   =  gs_zmm_pur_proposal-gjahr.
      gs_tab1-pr_date                 =  gs_zmm_pur_proposal-pr_date.
      gs_tab1-ekgrp                   =  gs_zmm_pur_proposal-ekgrp.
      gs_tab1-eknam                   =  gs_zmm_pur_proposal-eknam.
      gs_tab1-bukrs                   =  gs_zmm_pur_proposal-bukrs .
      gs_tab1-werks                   =  gs_zmm_pur_proposal-werks.
      gs_tab1-plantname               =  gs_zmm_pur_proposal-plantname.
      gs_tab1-erdat                   =  gs_zmm_pur_proposal-erdat.
      gs_zmm_pur_proposal-pr_itemno   = gs_zmm_pur_proposal-pr_itemno / 10.
      gs_tab1-ebelp                   =  gs_zmm_pur_proposal-pr_itemno .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gs_zmm_pur_proposal-matnr
        IMPORTING
          output = gs_zmm_pur_proposal-matnr.
      gs_tab1-matnr                   =  gs_zmm_pur_proposal-matnr.
      gs_tab1-maktx                   =  gs_zmm_pur_proposal-maktx .
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = gs_zmm_pur_proposal-lifnr
        IMPORTING
          output = gs_zmm_pur_proposal-lifnr.
      gs_tab1-lifnr                   =  gs_zmm_pur_proposal-lifnr.
      gs_tab1-name1                   =  gs_zmm_pur_proposal-name1.
      gs_tab1-qty                     =  gs_zmm_pur_proposal-menge .
      gs_tab1-meins                   =  gs_zmm_pur_proposal-meins.
      gs_tab1-netpr                   =  gs_zmm_pur_proposal-netpr.
      gs_tab1-waers                   =  gs_zmm_pur_proposal-waers .
      gs_tab1-mwskz                   =  gs_zmm_pur_proposal-mwskz.
      gs_tab1-tax_desc                =  gs_zmm_pur_proposal-tax_desc.
      gs_tab1-netpr                   =  gs_zmm_pur_proposal-netpr .
      gs_tab1-landed_price            =  gs_zmm_pur_proposal-land_pr    .
      gs_tab1-landed_value            =  gs_zmm_pur_proposal-land_value.
      gs_tab1-last_pur_basic_price    =  gs_zmm_pur_proposal-last_basicpr.
      gs_tab1-last_pur_land_price     =  gs_zmm_pur_proposal-last_landpr .
      gs_tab1-business_price          =  gs_zmm_pur_proposal-business_planpr.
      gs_tab1-vari_last_pur_inr       =  gs_zmm_pur_proposal-vari_last_pur_in.
      gs_tab1-vari_last_pur_val       =  gs_zmm_pur_proposal-vari_last_pur_va.
      gs_tab1-vari_business_plan_inr  =  gs_zmm_pur_proposal-vari_bus_pl_inr.
      gs_tab1-vari_business_plan_val  =  gs_zmm_pur_proposal-vari_bus_pl_val.
      gs_tab1-op_m1                   =  gs_zmm_pur_proposal-opening_m1.
      gs_tab1-req_m1                  =  gs_zmm_pur_proposal-requirment_m1.
      gs_tab1-app_vend_m1             =  gs_zmm_pur_proposal-app_vend_m1.
      gs_tab1-otr_vend_m1             =  gs_zmm_pur_proposal-other_vend_m1.
      gs_tab1-clos_m1                 =  gs_zmm_pur_proposal-closing_m1.
      gs_tab1-op_m2                   =  gs_zmm_pur_proposal-opening_m2.
      gs_tab1-req_m2                  =  gs_zmm_pur_proposal-requirment_m2.
      gs_tab1-app_vend_m2             =  gs_zmm_pur_proposal-app_vend_m2.
      gs_tab1-otr_vend_m2             =  gs_zmm_pur_proposal-other_vend_m2.
      gs_tab1-clos_m2                 =  gs_zmm_pur_proposal-closing_m2.
      gs_tab1-op_m3                   =  gs_zmm_pur_proposal-opening_m3.
      gs_tab1-req_m3                  =  gs_zmm_pur_proposal-requirment_m3.
      gs_tab1-app_vend_m3             =  gs_zmm_pur_proposal-app_vend_m3.
      gs_tab1-otr_vend_m3             =  gs_zmm_pur_proposal-other_vend_m3.
      gs_tab1-clos_m3                 =  gs_zmm_pur_proposal-closing_m3.
      gs_tab1-costformula             =  gs_zmm_pur_proposal-comp_data.
      gs_tab1-comp_data_avl           =  gs_zmm_pur_proposal-comp_data1.
      gs_tab1-text                    =  gs_zmm_pur_proposal-line_txt.
      gs_tab1-remarks                 =  gs_zmm_pur_proposal-remark.
      old_remarks                     =  gs_zmm_pur_proposal-remark.
      gs_tab1-releaser1               =  gs_zmm_pur_proposal-releaser1.
      gs_tab1-releaser_name1          =  gs_zmm_pur_proposal-releaser_name1.
      lv_releaser1                    = gs_tab1-releaser1.
      CONCATENATE gs_tab1-releaser1 gs_tab1-releaser_name1 INTO releaser_name1 SEPARATED BY space.
      SHIFT releaser_name1 LEFT DELETING LEADING '0'.
*    releaser_name1                  =  gs_zmm_pur_proposal-releaser_name1.
      gs_tab1-releaser2               =  gs_zmm_pur_proposal-releaser2.
      lv_releaser2                    = gs_tab1-releaser2.
      gs_tab1-releaser_name2          =  gs_zmm_pur_proposal-releaser_name2.
      CONCATENATE gs_tab1-releaser2 gs_tab1-releaser_name2 INTO releaser_name2 SEPARATED BY space.
      SHIFT releaser_name2 LEFT DELETING LEADING '0'.
*    releaser_name2                  =  gs_zmm_pur_proposal-releaser_name2.
      gs_tab1-releaser3               =  gs_zmm_pur_proposal-releaser3.
      lv_releaser3                    = gs_tab1-releaser3.
      gs_tab1-releaser_name3          =  gs_zmm_pur_proposal-releaser_name3.
      CONCATENATE gs_tab1-releaser3 gs_tab1-releaser_name3 INTO releaser_name3 SEPARATED BY space.
      SHIFT releaser_name3 LEFT DELETING LEADING '0'.
*    releaser_name3                  =  gs_zmm_pur_proposal-releaser_name3.
      gs_tab1-releaser4               =  gs_zmm_pur_proposal-releaser4 .
      lv_releaser4                    = gs_tab1-releaser4.
      gs_tab1-releaser_name4          =  gs_zmm_pur_proposal-releaser_name4.
      CONCATENATE gs_tab1-releaser4 gs_tab1-releaser_name4 INTO releaser_name4 SEPARATED BY space.
      SHIFT releaser_name4 LEFT DELETING LEADING '0'.
*    releaser_name4                  =  gs_zmm_pur_proposal-releaser_name4.
      gs_tab1-pr_date                 =  gs_zmm_pur_proposal-erdat.
      gs_tab1-ch_date                 =  sy-datum.
      gs_tab1-pr_user                 =  gs_zmm_pur_proposal-pr_user.
      gs_tab1-pr_time                 =         gs_zmm_pur_proposal-pr_time .
      APPEND gs_tab1 TO gt_tab1.
      CLEAR :gs_tab1,gs_zmm_pur_proposal,remarks.

    ENDLOOP.
*  remarks = old_remarks.
*  CALL METHOD remarks_editor->set_textstream
*    EXPORTING
*      text = remarks.
  ELSE.
    MESSAGE 'No Data Found' TYPE 'E'.
  ENDIF.
ENDFORM.                    "GET_DATA

*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh .
  REFRESH: gt_zmm_pur_proposal,gt_tab1.
  CLEAR: gs_tab1,gs_zmm_pur_proposal,gv_ekgrp,gv_eknam,gv_werks,
                                              gv_plantname,
                                              gv_date,
                                              gv_chdt ,
                                              gv_user  ,
                                              gv_time.

ENDFORM.                    " REFRESH                   " GET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0050 OUTPUT.
  SET PF-STATUS 'ZPF050'.
  SET TITLEBAR 'ZTIT050'.
  CASE sy-ucomm.
    WHEN 'BACK'.

      REFRESH:gt_tab1,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
          list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal.
      CLEAR: gs_tab1,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab3,gs_stxh,gs_a516, gs_pa0001,
             gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
             flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
             no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
             gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
             gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs,gv_plantname,gv_eknam,gv_date,g_mytable,gv_user,gv_time,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,remarks,old_remarks.
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      CALL METHOD remarks_container->free.
      CALL METHOD remarks_editor->free.

      SET SCREEN 0.
*      CALL TRANSACTION 'ZMM015' AND SKIP FIRST SCREEN.
      LEAVE SCREEN.
*      LEAVE TO SCREEN 101.
  ENDCASE.

  IF flag1 EQ ' '.
    CREATE OBJECT remarks_container
      EXPORTING
        container_name              = 'OTH_REMARKS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    CREATE OBJECT remarks_editor
      EXPORTING
        parent                     = remarks_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = line_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    CALL METHOD remarks_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.

    CALL METHOD remarks_editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.

    CREATE OBJECT editor_container
      EXPORTING
        container_name              = 'TEXTEDITOR'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    CREATE OBJECT text_editor
      EXPORTING
        parent                     = editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = line_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
    CALL METHOD text_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.
    CALL METHOD text_editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.

    v_mode = '1'.
    CALL METHOD text_editor->set_readonly_mode "Sets the text field on the screen to display mode
      EXPORTING
      readonly_mode = v_mode
      EXCEPTIONS
          error_cntl_call_method = 1
          invalid_parameter = 2
          OTHERS = 3.
    IF sy-subrc <> 0.                                       "#EC NEEDED
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    flag1 = 'X'.

  ENDIF.
  remarks = old_remarks.
  CALL METHOD remarks_editor->set_textstream
    EXPORTING
      text = remarks.

  v_mode = '1'.
  CALL METHOD remarks_editor->set_readonly_mode "Sets the text field on the screen to display mode
    EXPORTING
    readonly_mode = v_mode
    EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter = 2
        OTHERS = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  IF c1 = 'X'.
    IF sy-ucomm = 'RELEASE' OR sy-ucomm = 'CANCRL'.
      LOOP AT SCREEN.
        IF screen-group1 = 'CK1'.
          screen-name = 'C1'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  IF c2 = 'X'..
    IF sy-ucomm = 'RELEASE' OR sy-ucomm = 'CANCRL'.
      LOOP AT SCREEN.
        IF screen-group1 = 'CK2'.
          screen-name = 'C2'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF c3 = 'X'.
    IF sy-ucomm = 'RELEASE' OR sy-ucomm = 'CANCRL'.
      LOOP AT SCREEN.
        IF screen-group1 = 'CK3'.
          screen-name = 'C3'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF c4 = 'X'.
    IF sy-ucomm = 'RELEASE' OR sy-ucomm = 'CANCRL'.
      LOOP AT SCREEN.
        IF screen-group1 = 'CK4'.
          screen-name = 'C4'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  DATA flag_chk TYPE c.

  SHIFT lv_releaser1 LEFT DELETING LEADING '0'.
  SHIFT lv_releaser2 LEFT DELETING LEADING '0'.
  SHIFT lv_releaser3 LEFT DELETING LEADING '0'.
  SHIFT lv_releaser3 LEFT DELETING LEADING '0'.
  SHIFT gv_pernr1 LEFT DELETING LEADING '0'.
  IF gv_pernr1 NE ' '.
    IF lv_releaser1 = gv_pernr1.
      LOOP AT SCREEN.
*     IF  flag_chk EQ ''.
        IF screen-group1 = 'CK1'.
          screen-name = 'C1'.
          screen-input = 1.
*          screen-active = 0.
          flag_chk = 'X'.
          MODIFY SCREEN.
        ELSEIF screen-group1 = 'CK2'.
          IF screen-group1 = 'CK2'.
            screen-name = 'C2'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK3'.
          IF screen-group1 = 'CK3'.
            screen-name = 'C3'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK4'.
          IF screen-group1 = 'CK4'.
            screen-name = 'C4'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
*     ENDIF.
      ENDLOOP.

    ELSEIF lv_releaser2 = gv_pernr1.
      LOOP AT SCREEN.
        IF screen-group1 = 'CK2'.
          screen-name = 'C2'.
          screen-active = 1.
          flag_chk = 'X'.
          MODIFY SCREEN.
        ELSEIF screen-group1 = 'CK1'.
          IF screen-group1 = 'CK1'.
            screen-name = 'C1'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK3'.
          IF screen-group1 = 'CK3'.
            screen-name = 'C3'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK4'.
          IF screen-group1 = 'CK4'.
            screen-name = 'C4'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSEIF lv_releaser3 = gv_pernr1.
      LOOP AT SCREEN.
        IF screen-group1 = 'CK3'.
          screen-name = 'C3'.
          screen-active = 1.
          flag_chk = 'X'.
          MODIFY SCREEN.
        ELSEIF screen-group1 = 'CK2'.
          IF screen-group1 = 'CK2'.
            screen-name = 'C2'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK1'.
          IF screen-group1 = 'CK1'.
            screen-name = 'C1'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK4'.
          IF screen-group1 = 'CK4'.
            screen-name = 'C4'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSEIF lv_releaser4 = gv_pernr1.
      LOOP AT SCREEN.
        IF screen-group1 = 'CK4'.
          screen-name = 'C4'.
          screen-active = 1.
          flag_chk = 'X'.
          MODIFY SCREEN.
        ELSEIF screen-group1 = 'CK2'.
          IF screen-group1 = 'CK2'.
            screen-name = 'C2'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK3'.
          IF screen-group1 = 'CK3'.
            screen-name = 'C3'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ELSEIF screen-group1 = 'CK1'.
          IF screen-group1 = 'CK1'.
            screen-name = 'C1'.
            screen-active = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF gv_r1 = 'X'.
      LOOP AT SCREEN.
        IF screen-name = 'RELEASE'.
          screen-input = 1.
        ENDIF.
        IF screen-name = 'CANCRL'.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
    IF gv_r2 = 'X'.
      LOOP AT SCREEN.
        IF screen-name = 'RELEASE'.
          screen-input = 0.
        ENDIF.
        IF screen-name = 'CANCRL'.
          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.

*    IF flag = 'X'.
*     LOOP AT SCREEN.
*      IF screen-name = 'CLOSETXT'.
*      screen-input = 1.
*      ENDIF.
*      MODIFY SCREEN.
*     ENDLOOP.
*     ELSE.
*      LOOP AT SCREEN.
*       IF screen-name = 'CLOSETXT'.
*       screen-input = 0.
*       ENDIF.
*       MODIFY SCREEN.
*      ENDLOOP.
*    ENDIF.

  ENDIF.
ENDMODULE.                 " STATUS_0050  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0050 INPUT.

  CASE sy-ucomm.
    WHEN 'PTERM'.
      GET CURSOR LINE g_lineno.
      lv_curline  = g_lineno.
      READ TABLE gt_tab1 INTO gs_tab1 INDEX lv_curline.
      IF sy-subrc = 0.
        REFRESH g_mytable.
        text = gs_tab1-text.
        APPEND text TO g_mytable.
        DELETE g_mytable WHERE table_line IS INITIAL.
      ENDIF.
      IF g_mytable IS NOT INITIAL.
        flag = 'X'.
*      CREATE OBJECT editor_container
*        EXPORTING
*          container_name              = 'TEXTEDITOR'
*        EXCEPTIONS
*          cntl_error                  = 1
*          cntl_system_error           = 2
*          create_error                = 3
*          lifetime_error              = 4
*          lifetime_dynpro_dynpro_link = 5
*          OTHERS                      = 6.
*      CREATE OBJECT text_editor
*        EXPORTING
*          parent                     = editor_container
*          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
*          wordwrap_position          = line_length
*          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
*      CALL METHOD text_editor->set_toolbar_mode
*        EXPORTING
*          toolbar_mode = cl_gui_textedit=>false.
*      CALL METHOD text_editor->set_statusbar_mode
*        EXPORTING
*          statusbar_mode = cl_gui_textedit=>false.
*        get_text_as_r3table
        CALL METHOD text_editor->set_text_as_r3table
          EXPORTING
            table = g_mytable.
*       v_mode = '1'.
*      CALL METHOD text_editor->set_readonly_mode "Sets the text field on the screen to display mode
*        EXPORTING
*        readonly_mode = v_mode
*        EXCEPTIONS
*            error_cntl_call_method = 1
*            invalid_parameter = 2
*            OTHERS = 3.
*      IF sy-subrc <> 0. "#EC NEEDED
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
      ELSE.
        CALL METHOD text_editor->set_text_as_r3table
          EXPORTING
            table = g_mytable.
        MESSAGE 'No Data found for selected Line' TYPE 'I'.
      ENDIF.

*    WHEN 'CLOSE_TEXT'.
*
*      IF flag = 'X'.
*        CLEAR flag.
*        CALL METHOD text_editor->get_textstream
**          EXPORTING
**              ONLY_WHEN_MODIFIED     = CL_GUI_TEXTEDIT=>TRUE
*           IMPORTING
*               text                   = text
**              IS_MODIFIED            =
*           EXCEPTIONS
*               error_cntl_call_method = 1
*               not_supported_by_gui   = 2
*               OTHERS                 = 3.
*        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*
*        CALL METHOD cl_gui_cfw=>flush
*          EXCEPTIONS
*            cntl_system_error = 1
*            cntl_error        = 2
*            OTHERS            = 3.
*        CALL METHOD editor_container->free.
*        CALL METHOD text_editor->free.
*      ENDIF.

    WHEN 'RELEASE'.
      PERFORM approve_proposal.
    WHEN 'CANCRL'.
      PERFORM cancel_release.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT


*&---------------------------------------------------------------------*
*&      Form  APPROVE_PROPOSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM approve_proposal .
  IF gv_r1 = 'X'.
    CLEAR: flag_final,flag3,flag4.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gv_matnr
*\      IMPORTING
*\        output = gv_matnr.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Monday, November 13, 2018
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - CONVERSION_EXIT_ALPHA_INPUT Changes in Material
* Solution   - commenting below code as no need to convert material
*              number after converting it is checking for 40 digits
*              material code which which returns sy-subrc 4
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = gv_matnr
      IMPORTING
        output = gv_matnr.

*}   REPLACE
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gv_pernr1
      IMPORTING
        output = gv_pernr1.
    REFRESH gt_zmm_pur_proposal.
    SELECT  * FROM zmm_pur_proposal INTO CORRESPONDING FIELDS OF TABLE gt_zmm_pur_proposal WHERE prno = gv_prno"gs_final-prno
                                                                                          AND matnr = gv_matnr."GS_final-matnr.
    IF c1 EQ ' ' AND c2 EQ '' AND c3 EQ ' ' AND c4 EQ ''.
      MESSAGE 'Select check box' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    IF c1 = 'X'.
      CLEAR gs_zmm_pur_proposal1.
      READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser1 = gv_pernr1.
      IF sy-subrc = 0.
        SHIFT gs_zmm_pur_proposal1-releaser1_date LEFT DELETING LEADING '0'.
        IF gs_zmm_pur_proposal1-releaser1_date EQ ' '.
          LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
            gv_prno = gs_zmm_pur_proposal-prno.
            gv_user = gs_zmm_pur_proposal-pr_user.
            gs_zmm_pur_proposal-releaser1_date = sy-datum.
            gv_pernr = gs_zmm_pur_proposal-releaser2.
            gv_level1 = 'L2'.
            MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser1_date.
            CLEAR gs_zmm_pur_proposal.
            flag4 = 'X'.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
    IF c2 = 'X'.
      CLEAR gs_zmm_pur_proposal1.
      READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser2 = gv_pernr1.
      IF sy-subrc = 0.
        SHIFT gs_zmm_pur_proposal1-releaser1_date LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser2_date LEFT DELETING LEADING '0'.
        IF gs_zmm_pur_proposal1-releaser1_date NE ' '.
          IF gs_zmm_pur_proposal1-releaser2_date EQ ' '.
            LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
              gv_prno = gs_zmm_pur_proposal-prno.
              gv_user = gs_zmm_pur_proposal-pr_user.
              gs_zmm_pur_proposal-releaser2_date = sy-datum.
              gv_pernr = gs_zmm_pur_proposal-releaser3.
              gv_level1 = 'L3'.
              MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser2_date.
              CLEAR gs_zmm_pur_proposal.
              flag4 = 'X'.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF c3 = 'X'.
      CLEAR gs_zmm_pur_proposal1.
      READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser3 = gv_pernr1.
      IF sy-subrc = 0.
        SHIFT gs_zmm_pur_proposal1-releaser2_date LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser3_date LEFT DELETING LEADING '0'.
        IF gs_zmm_pur_proposal1-releaser2_date NE ' '.
          IF gs_zmm_pur_proposal1-releaser3_date EQ ' '.
            LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
              gv_prno = gs_zmm_pur_proposal-prno.
              gv_user = gs_zmm_pur_proposal-pr_user.
              gs_zmm_pur_proposal-releaser3_date = sy-datum.
              gv_pernr = gs_zmm_pur_proposal-releaser4.
              gv_level1 = 'L4'.
              MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser3_date.
              CLEAR gs_zmm_pur_proposal.
              flag4 = 'X'.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    IF c4 = 'X'.
      CLEAR gs_zmm_pur_proposal1.
      READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser4 = gv_pernr1.
      IF sy-subrc = 0.
        SHIFT gs_zmm_pur_proposal1-releaser3_date LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser4_date LEFT DELETING LEADING '0'.
        IF gs_zmm_pur_proposal1-releaser3_date NE ' '.
          IF gs_zmm_pur_proposal1-releaser4_date EQ ' '.
            LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
              gv_prno = gs_zmm_pur_proposal-prno.
              gv_user = gs_zmm_pur_proposal-pr_user.
              gs_zmm_pur_proposal-releaser4_date = sy-datum.
*              gv_pernr = gs_zmm_pur_proposal-releaser4.
              MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser4_date.
              CLEAR gs_zmm_pur_proposal.
              flag4 = 'X'.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR :gs_zmm_pur_proposal1,flag_final.
    LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1.
      SHIFT gs_zmm_pur_proposal1-releaser1_date LEFT DELETING LEADING '0'.
      SHIFT gs_zmm_pur_proposal1-releaser2_date LEFT DELETING LEADING '0'.
      SHIFT gs_zmm_pur_proposal1-releaser3_date LEFT DELETING LEADING '0'.
      SHIFT gs_zmm_pur_proposal1-releaser4_date LEFT DELETING LEADING '0'.
      IF gs_zmm_pur_proposal1-releaser1 = gv_pernr1.
        IF gs_zmm_pur_proposal1-releaser1_date NE ' '.
          flag_final = 'X'.
        ENDIF.
      ENDIF.
      IF gs_zmm_pur_proposal1-releaser2 = gv_pernr1.
        IF gs_zmm_pur_proposal1-releaser2_date NE ' '.
          flag_final = 'X'.
        ENDIF.
      ENDIF.
      IF gs_zmm_pur_proposal1-releaser3 = gv_pernr1.
        IF gs_zmm_pur_proposal1-releaser3_date NE ' '.
          flag_final = 'X'.
        ENDIF.
      ENDIF.
      IF gs_zmm_pur_proposal1-releaser4 = gv_pernr1.
        IF gs_zmm_pur_proposal1-releaser4_date NE ' '.
          flag_final = 'X'.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF flag4 NE 'X'.
      IF  flag_final = 'X'.
        MESSAGE 'Relase allready done' TYPE 'I' DISPLAY LIKE 'E'.
        flag3 = 'X'.
      ELSE.
      ENDIF.
    ENDIF.
    IF flag3 EQ ' '.
      IF flag4 = 'X'.
        CLEAR lv_ans.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Approve Proposal'
            text_question         = 'Do you want to Approve Proposal ?'
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            default_button        = '1'
            display_cancel_button = ' '
            start_column          = 55
            start_row             = 3
          IMPORTING
            answer                = lv_ans.
        IF lv_ans = 1.
          MODIFY zmm_pur_proposal FROM TABLE gt_zmm_pur_proposal.
          COMMIT WORK.

          MESSAGE 'Proposal has been successfully approved' TYPE 'S'.
          IF gv_pernr IS NOT INITIAL.
            PERFORM email_send.
          ENDIF.

        ENDIF.
      ELSE.
        MESSAGE 'Approval is Pending from other Releaser' TYPE 'E'.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE 'You have not selected Release Option' TYPE 'E'.
  ENDIF.
ENDFORM.                    " APPROVE_PROPOSAL


*----------------------------------------------------------------------*
FORM email_send .
  DATA :lv_userid TYPE pa0105-usrid_long.
  DATA: tab_lines LIKE sy-tabix,
        lv_desc   TYPE string,
        lv_p1     TYPE string,
        dd(2)     TYPE c,
        mm(2)     TYPE c,
        yy(4)     TYPE c,
        lv_date(10) TYPE c,
        pr_date1(10) TYPE c,
        lv_ename    TYPE string.

  CLEAR:lv_userid,w_document_data,doc_chng,objpack,objhead,zreceivers,i_body_msg,pr_date1,
           lv_desc,lv_p1,lv_date,lv_ename,dd,mm,yy.

  SELECT SINGLE ename FROM pa0001 INTO lv_ename WHERE pernr = sy-uname.

  SELECT SINGLE usrid_long INTO lv_userid
                               FROM pa0105
                               WHERE pernr = gv_pernr "SY-UNAME
                               AND endda GE sy-datum "  CHANGED TO INCASE OF SEPARATION HAPPENS IN ADVANCE. '99991231'
*                               AND aedtm le sy-datum
                               AND subty = '0010'.
  IF sy-subrc = 0.
    TRANSLATE lv_userid TO LOWER CASE.
    zreceivers-receiver = lv_userid.
    zreceivers-rec_type = 'U'.
    APPEND zreceivers.
    CONCATENATE 'Proposal No' gv_prno 'Waiting for your Release.' INTO lv_desc SEPARATED BY space.
    doc_chng-obj_name = 'SENDMAIL'.
    doc_chng-obj_descr = lv_desc.
    doc_chng-obj_langu = sy-langu.
    doc_chng-sensitivty = 'F'.
    CLEAR gs_zmm_pur_proposal.
    READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal INDEX 1.
    IF sy-subrc = 0.
      dd = gs_zmm_pur_proposal-pr_date+6(2).
      mm = gs_zmm_pur_proposal-pr_date+4(2).
      yy = gs_zmm_pur_proposal-pr_date+0(4).
    ENDIF.
*    CLEAR gv_level1.
*    IF gv_level = 1.
*      gv_level1 = 'L1'.
*    ELSEIF gv_level = 2.
*      gv_level1 = 'L2'.
*    ELSEIF gv_level = 3.
*      gv_level1 = 'L3'.
*    ELSEIF gv_level = 4.
*      gv_level1 = 'L4'.
*    ENDIF.
    CONCATENATE dd '.' mm '.' yy INTO lv_date.
    CONCATENATE 'Proposal Number' gv_prno ', dated:' lv_date
                                          '- is Waiting for your' gv_level1 'Release.'
                                            INTO lv_p1 SEPARATED BY space.
    PERFORM build_body_of_mail
    USING:
     lv_p1,
    '     ',
    'Thanks & Regards.',
     lv_ename.

    LOOP AT i_body_msg INTO w_body_msg.
      APPEND w_body_msg TO objtxt.
      CLEAR w_body_msg.
    ENDLOOP.
    objpack-transf_bin = space.
    objpack-head_start = 1.
    objpack-head_num   = 0.
    objpack-body_start = 1.
    DESCRIBE TABLE objtxt LINES objpack-body_num.
    objpack-doc_type = 'RAW'.
    APPEND objpack.

    DESCRIBE TABLE objtxt LINES tab_lines.
    READ TABLE objtxt INDEX tab_lines.
    doc_chng-doc_size = ( tab_lines - 1 ) * 255 + STRLEN( objtxt ).

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
       document_data                    = doc_chng
       put_in_outbox                    = 'X'
       commit_work                      = 'X'
      TABLES
       packing_list                     = objpack
*   OBJECT_HEADER                    = OBJHEAD
*   CONTENTS_BIN                     = OBJBIN
       contents_txt                     =  objtxt
*   CONTENTS_HEX                     =
*   OBJECT_PARA                      =
*   OBJECT_PARB                      =
        receivers                        = zreceivers
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
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    " EMAIL_SEND


*----------------------------------------------------------------------*
FORM build_body_of_mail  USING   l_message..
  w_body_msg = l_message.
  APPEND w_body_msg TO i_body_msg.
  CLEAR  w_body_msg.
ENDFORM.                    " BUILD_BODY_OF_MAIL
*&---------------------------------------------------------------------*
*&      Form  CANCEL_RELEASE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cancel_release .
  IF gv_r2 EQ 'X'.
    CLEAR :flag4.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gv_matnr
*\      IMPORTING
*\        output = gv_matnr.
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Monday, November 13, 2018
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - CONVERSION_EXIT_ALPHA_INPUT Changes in Material
* Solution   - commenting below code as no need to convert material
*              number after converting it is checking for 40 digits
*              material code which which returns sy-subrc 4
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = gv_matnr
      IMPORTING
        output = gv_matnr.

*}   REPLACE
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gv_pernr1
      IMPORTING
        output = gv_pernr1.
    REFRESH gt_zmm_pur_proposal.
    SELECT  * FROM zmm_pur_proposal INTO CORRESPONDING FIELDS OF TABLE gt_zmm_pur_proposal WHERE prno = gv_prno"gs_final-prno
                                                                                          AND matnr = gv_matnr."GS_final-matnr.
    IF c1 EQ ' ' AND c2 EQ '' AND c3 EQ ' ' AND c4 EQ ''.
      MESSAGE 'Select check box' TYPE 'I' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    IF c1 = 'X'.
      CLEAR gs_zmm_pur_proposal1.
      READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser1 = gv_pernr1.
      IF sy-subrc = 0.
        SHIFT gs_zmm_pur_proposal1-releaser1 LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser2 LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser3 LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser4 LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser1_date LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser2_date LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser3_date LEFT DELETING LEADING '0'.
        SHIFT gs_zmm_pur_proposal1-releaser4_date LEFT DELETING LEADING '0'.
        IF gs_zmm_pur_proposal1-releaser1_date NE ' '.
          IF gs_zmm_pur_proposal1-releaser2_date EQ ' '. .
            IF gs_zmm_pur_proposal1-releaser3_date EQ ' '. .
              IF gs_zmm_pur_proposal1-releaser4_date EQ ' '.
                LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
                  gv_prno = gs_zmm_pur_proposal-prno.
                  gv_user = gs_zmm_pur_proposal-pr_user.
                  gs_zmm_pur_proposal-releaser1_date = ' '.
                  gs_zmm_pur_proposal-pr_cancel1 = 'X'.
                  gv_pernr = gs_zmm_pur_proposal-releaser2.
                  MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser1_date pr_cancel1.
                  CLEAR gs_zmm_pur_proposal.
                  flag4 = 'X'.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
     ENDIF.
      IF c2 = 'X'.
        CLEAR gs_zmm_pur_proposal1.
        READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser2 = gv_pernr1.
        IF sy-subrc = 0.
          SHIFT gs_zmm_pur_proposal1-releaser1_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser2_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser3_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser4_date LEFT DELETING LEADING '0'.
          IF gs_zmm_pur_proposal1-releaser1_date NE ' '.
            IF gs_zmm_pur_proposal1-releaser2_date NE ' '.
              IF gs_zmm_pur_proposal1-releaser3_date EQ ' ' AND gs_zmm_pur_proposal1-releaser4_date EQ ' '.
                LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
                  gv_prno = gs_zmm_pur_proposal-prno.
                  gv_user = gs_zmm_pur_proposal-pr_user.
                  gs_zmm_pur_proposal-releaser2_date = ' '.
                  gs_zmm_pur_proposal-pr_cancel2 = 'X'.
                  gv_pernr = gs_zmm_pur_proposal-releaser3.
                  MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser2_date pr_cancel2.
                  CLEAR gs_zmm_pur_proposal.
                  flag4 = 'X'.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF c3 = 'X'.
        CLEAR gs_zmm_pur_proposal1.
        READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser3 = gv_pernr1.
        IF sy-subrc = 0.
          SHIFT gs_zmm_pur_proposal1-releaser1_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser2_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser3_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser4_date LEFT DELETING LEADING '0'.
          IF gs_zmm_pur_proposal1-releaser1_date NE ' '.
            IF gs_zmm_pur_proposal1-releaser2_date NE ' '.
              IF gs_zmm_pur_proposal1-releaser3_date NE ' '.
                IF gs_zmm_pur_proposal1-releaser4_date EQ ' '.
                  LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
                    gv_prno = gs_zmm_pur_proposal-prno.
                    gv_user = gs_zmm_pur_proposal-pr_user.
                    gs_zmm_pur_proposal-releaser3_date = ' '.
                    gs_zmm_pur_proposal-pr_cancel3 = 'X'.
                    gv_pernr = gs_zmm_pur_proposal-releaser4.
                    MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser3_date pr_cancel3.
                    CLEAR gs_zmm_pur_proposal.
                    flag4 = 'X'.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF c4 = 'X'.
        CLEAR gs_zmm_pur_proposal1.
        READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal1 WITH KEY prno = gv_prno releaser4 = gv_pernr1.
        IF sy-subrc = 0.
          SHIFT gs_zmm_pur_proposal1-releaser1_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser2_date LEFT DELETING LEADING '0'.
          SHIFT gs_zmm_pur_proposal1-releaser3_date LEFT DELETING LEADING '0'.
          IF gs_zmm_pur_proposal1-releaser1_date NE ' '.
            IF gs_zmm_pur_proposal1-releaser2_date NE ' '.
              IF gs_zmm_pur_proposal1-releaser3_date NE ' '.
                IF gs_zmm_pur_proposal1-releaser4_date NE ' '.
                  LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal.
                    gv_prno = gs_zmm_pur_proposal-prno.
                    gv_user = gs_zmm_pur_proposal-pr_user.
                    gs_zmm_pur_proposal-releaser4_date = ' '.
                    gs_zmm_pur_proposal-pr_cancel4 = 'X'.
*              gv_pernr = gs_zmm_pur_proposal-releaser4.
                    MODIFY gt_zmm_pur_proposal FROM gs_zmm_pur_proposal TRANSPORTING releaser4_date pr_cancel4.
                    CLEAR gs_zmm_pur_proposal.
                    flag4 = 'X'.
                  ENDLOOP.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*    ENDIF.
    CLEAR gs_zmm_pur_proposal1.

    IF flag4 = 'X'.
      CLEAR lv_ans.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Cancel Proposal'
          text_question         = 'Do you want to Cancel Proposal ?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 55
          start_row             = 3
        IMPORTING
          answer                = lv_ans.
      IF lv_ans = 1.
        MODIFY zmm_pur_proposal FROM TABLE gt_zmm_pur_proposal.
        COMMIT WORK.
        PERFORM email_cancel.
        MESSAGE 'Release has been cancelled successfully' TYPE 'S'.
      ENDIF.

    ELSE.
      MESSAGE 'You are not Authorized to Cancel this Level,Next Level release is allready done' TYPE 'E'.
    ENDIF.

  ELSE.
    MESSAGE 'You have not selected Cancel Release Option ' TYPE 'E'.
  ENDIF.
ENDFORM.                    " CANCEL_RELEASE

*&---------------------------------------------------------------------*
*&      Form  EMAIL_CANCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM email_cancel .
  DATA :  lv_userid TYPE pa0105-usrid_long.
  DATA: tab_lines LIKE sy-tabix,
        lv_desc   TYPE string,
        lv_p1     TYPE string,
        dd(2)     TYPE c,
        mm(2)     TYPE c,
        yy(4)     TYPE c,
        lv_date(10) TYPE c,
        lv_ename    TYPE pa0001-ename,
        lv_vend_name TYPE lfa1-name1.
  CLEAR : lv_userid,w_document_data,doc_chng,objpack,objhead,zreceivers,i_body_msg,
          lv_desc,lv_p1,lv_date,lv_ename,lv_vend_name.

  SELECT SINGLE ename FROM pa0001 INTO lv_ename WHERE pernr = sy-uname.
  READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal INDEX 1.
  SELECT SINGLE usrid_long INTO lv_userid
                              FROM pa0105
                              WHERE uname = gs_zmm_pur_proposal-pr_user "SY-UNAME
                              AND endda GE sy-datum "  CHANGED TO INCASE OF SEPARATION HAPPENS IN ADVANCE. '99991231'
                              AND aedtm LE sy-datum
                              AND subty = '0010'.
  IF sy-subrc = 0.
    TRANSLATE lv_userid TO LOWER CASE.
    zreceivers-receiver = lv_userid.
    zreceivers-rec_type = 'U'.
    APPEND zreceivers.
    CONCATENATE 'Proposal No' gv_prno 'has been cancelled.' INTO lv_desc SEPARATED BY space.
    doc_chng-obj_name = 'SENDMAIL'.
    doc_chng-obj_descr = lv_desc.
    doc_chng-obj_langu = sy-langu.
    doc_chng-sensitivty = 'F'.

    dd = sy-datum+6(2).
    mm = sy-datum+4(2).
    yy = sy-datum+0(4).
    CONCATENATE dd '.' mm '.' yy INTO lv_date.
    CONCATENATE 'Proposal Number' gv_prno ', dated:' lv_date
                                          '- has been cancelled.'
                                            INTO lv_p1 SEPARATED BY space.
    PERFORM build_body_of_mail
    USING:space,
    lv_p1,
    '     ',
    'Thanks & Regards.',
     lv_ename.

    LOOP AT i_body_msg INTO w_body_msg.
      APPEND w_body_msg TO objtxt.
      CLEAR w_body_msg.
    ENDLOOP.
    objpack-transf_bin = space.
    objpack-head_start = 1.
    objpack-head_num   = 0.
    objpack-body_start = 1.
    DESCRIBE TABLE objtxt LINES objpack-body_num.
    objpack-doc_type = 'RAW'.
    APPEND objpack.

    DESCRIBE TABLE objtxt LINES tab_lines.
    READ TABLE objtxt INDEX tab_lines.
    doc_chng-doc_size = ( tab_lines - 1 ) * 255 + STRLEN( objtxt ).

    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
       document_data                    = doc_chng
       put_in_outbox                    = 'X'
       commit_work                      = 'X'
      TABLES
       packing_list                     = objpack
*   OBJECT_HEADER                    = OBJHEAD
*   CONTENTS_BIN                     = OBJBIN
       contents_txt                     =  objtxt
*   CONTENTS_HEX                     =
*   OBJECT_PARA                      =
*   OBJECT_PARB                      =
        receivers                        = zreceivers
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
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " EMAIL_CANCEL

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC1' ITSELF
CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 0050.

*&SPWIZARD: LINES OF TABLECONTROL 'TC1'
DATA:     g_tc1_lines  LIKE sy-loopc.

DATA:     ok_code LIKE sy-ucomm.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc1_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_tab1 LINES tc1-lines.
ENDMODULE.                    "TC1_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc1_get_lines OUTPUT.
  g_tc1_lines = sy-loopc.
ENDMODULE.                    "TC1_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc1_modify INPUT.
  MODIFY gt_tab1
    FROM gs_tab1
    INDEX tc1-current_line.
ENDMODULE.                    "TC1_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc1_mark INPUT.
  DATA: g_tc1_wa2 LIKE LINE OF gt_tab1.
  IF tc1-line_sel_mode = 1
  AND gs_tab1-flag = 'X'.
    LOOP AT gt_tab1 INTO g_tc1_wa2
      WHERE flag = 'X'.
      g_tc1_wa2-flag = ''.
      MODIFY gt_tab1
        FROM g_tc1_wa2
        TRANSPORTING flag.
    ENDLOOP.
  ENDIF.
  MODIFY gt_tab1
    FROM gs_tab1
    INDEX tc1-current_line
    TRANSPORTING flag.
ENDMODULE.                    "TC1_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc1_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC1'
                              'GT_TAB1'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC1_USER_COMMAND INPUT

*----------------------------------------------------------------------*
*   INCLUDE TABLECONTROL_FORMS                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  USER_OK_TC                                               *
*&---------------------------------------------------------------------*
FORM user_ok_tc USING    p_tc_name TYPE dynfnam
                         p_table_name
                         p_mark_name
                CHANGING p_ok      LIKE sy-ucomm.

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA: l_ok              TYPE sy-ucomm,
        l_offset          TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

*&SPWIZARD: Table control specific operations                          *
*&SPWIZARD: evaluate TC name and operations                            *
  SEARCH p_ok FOR p_tc_name.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  l_offset = STRLEN( p_tc_name ) + 1.
  l_ok = p_ok+l_offset.
*&SPWIZARD: execute general and TC specific operations                 *
  CASE l_ok.
    WHEN 'INSR'.                      "insert row
      PERFORM fcode_insert_row USING    p_tc_name
                                        p_table_name.
      CLEAR p_ok.

    WHEN 'DELE'.                      "delete row
      PERFORM fcode_delete_row USING    p_tc_name
                                        p_table_name
                                        p_mark_name.
      CLEAR p_ok.

    WHEN 'P--' OR                     "top of list
         'P-'  OR                     "previous page
         'P+'  OR                     "next page
         'P++'.                       "bottom of list
      PERFORM compute_scrolling_in_tc USING p_tc_name
                                            l_ok.
      CLEAR p_ok.
*     WHEN 'L--'.                       "total left
*       PERFORM FCODE_TOTAL_LEFT USING P_TC_NAME.
*
*     WHEN 'L-'.                        "column left
*       PERFORM FCODE_COLUMN_LEFT USING P_TC_NAME.
*
*     WHEN 'R+'.                        "column right
*       PERFORM FCODE_COLUMN_RIGHT USING P_TC_NAME.
*
*     WHEN 'R++'.                       "total right
*       PERFORM FCODE_TOTAL_RIGHT USING P_TC_NAME.
*
    WHEN 'MARK'.                      "mark all filled lines
      PERFORM fcode_tc_mark_lines USING p_tc_name
                                        p_table_name
                                        p_mark_name   .
      CLEAR p_ok.

    WHEN 'DMRK'.                      "demark all filled lines
      PERFORM fcode_tc_demark_lines USING p_tc_name
                                          p_table_name
                                          p_mark_name .
      CLEAR p_ok.

*     WHEN 'SASCEND'   OR
*          'SDESCEND'.                  "sort column
*       PERFORM FCODE_SORT_TC USING P_TC_NAME
*                                   l_ok.

  ENDCASE.

ENDFORM.                              " USER_OK_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_INSERT_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_insert_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name             .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_lines_name       LIKE feld-name.
  DATA l_selline          LIKE sy-stepl.
  DATA l_lastline         TYPE i.
  DATA l_line             TYPE i.
  DATA l_table_name       LIKE feld-name.
  FIELD-SYMBOLS <tc>                 TYPE cxtab_control.
  FIELD-SYMBOLS <table>              TYPE STANDARD TABLE.
  FIELD-SYMBOLS <lines>              TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_lines_name.
  ASSIGN (l_lines_name) TO <lines>.

*&SPWIZARD: get current line                                           *
  GET CURSOR LINE l_selline.
  IF sy-subrc <> 0.                   " append line to table
    l_selline = <tc>-lines + 1.
*&SPWIZARD: set top line                                               *
    IF l_selline > <lines>.
      <tc>-top_line = l_selline - <lines> + 1 .
    ELSE.
      <tc>-top_line = 1.
    ENDIF.
  ELSE.                               " insert line into table
    l_selline = <tc>-top_line + l_selline - 1.
    l_lastline = <tc>-top_line + <lines> - 1.
  ENDIF.
*&SPWIZARD: set new cursor line                                        *
  l_line = l_selline - <tc>-top_line + 1.

*&SPWIZARD: insert initial line                                        *
  INSERT INITIAL LINE INTO <table> INDEX l_selline.
  <tc>-lines = <tc>-lines + 1.
*&SPWIZARD: set cursor                                                 *
  SET CURSOR LINE l_line.

ENDFORM.                              " FCODE_INSERT_ROW

*&---------------------------------------------------------------------*
*&      Form  FCODE_DELETE_ROW                                         *
*&---------------------------------------------------------------------*
FORM fcode_delete_row
              USING    p_tc_name           TYPE dynfnam
                       p_table_name
                       p_mark_name   .

*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: delete marked lines                                        *
  DESCRIBE TABLE <table> LINES <tc>-lines.

  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    IF <mark_field> = 'X'.
      DELETE <table> INDEX syst-tabix.
      IF sy-subrc = 0.
        <tc>-lines = <tc>-lines - 1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                              " FCODE_DELETE_ROW

*&---------------------------------------------------------------------*
*&      Form  COMPUTE_SCROLLING_IN_TC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*      -->P_OK       ok code
*----------------------------------------------------------------------*
FORM compute_scrolling_in_tc USING    p_tc_name
                                      p_ok.
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_tc_new_top_line     TYPE i.
  DATA l_tc_name             LIKE feld-name.
  DATA l_tc_lines_name       LIKE feld-name.
  DATA l_tc_field_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <lines>      TYPE i.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.
*&SPWIZARD: get looplines of TableControl                              *
  CONCATENATE 'G_' p_tc_name '_LINES' INTO l_tc_lines_name.
  ASSIGN (l_tc_lines_name) TO <lines>.


*&SPWIZARD: is no line filled?                                         *
  IF <tc>-lines = 0.
*&SPWIZARD: yes, ...                                                   *
    l_tc_new_top_line = 1.
  ELSE.
*&SPWIZARD: no, ...                                                    *
    CALL FUNCTION 'SCROLLING_IN_TABLE'
         EXPORTING
              entry_act             = <tc>-top_line
              entry_from            = 1
              entry_to              = <tc>-lines
              last_page_full        = 'X'
              loops                 = <lines>
              ok_code               = p_ok
              overlapping           = 'X'
         IMPORTING
              entry_new             = l_tc_new_top_line
         EXCEPTIONS
*              NO_ENTRY_OR_PAGE_ACT  = 01
*              NO_ENTRY_TO           = 02
*              NO_OK_CODE_OR_PAGE_GO = 03
              OTHERS                = 0.
  ENDIF.

*&SPWIZARD: get actual tc and column                                   *
  GET CURSOR FIELD l_tc_field_name
             AREA  l_tc_name.

  IF syst-subrc = 0.
    IF l_tc_name = p_tc_name.
*&SPWIZARD: et actual column                                           *
      SET CURSOR FIELD l_tc_field_name LINE 1.
    ENDIF.
  ENDIF.

*&SPWIZARD: set the new top line                                       *
  <tc>-top_line = l_tc_new_top_line.


ENDFORM.                              " COMPUTE_SCROLLING_IN_TC

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_MARK_LINES
*&---------------------------------------------------------------------*
*       marks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_mark_lines USING p_tc_name
                               p_table_name
                               p_mark_name.
*&SPWIZARD: EGIN OF LOCAL DATA-----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: mark all filled lines                                      *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = 'X'.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines

*&---------------------------------------------------------------------*
*&      Form  FCODE_TC_DEMARK_LINES
*&---------------------------------------------------------------------*
*       demarks all TableControl lines
*----------------------------------------------------------------------*
*      -->P_TC_NAME  name of tablecontrol
*----------------------------------------------------------------------*
FORM fcode_tc_demark_lines USING p_tc_name
                                 p_table_name
                                 p_mark_name .
*&SPWIZARD: BEGIN OF LOCAL DATA----------------------------------------*
  DATA l_table_name       LIKE feld-name.

  FIELD-SYMBOLS <tc>         TYPE cxtab_control.
  FIELD-SYMBOLS <table>      TYPE STANDARD TABLE.
  FIELD-SYMBOLS <wa>.
  FIELD-SYMBOLS <mark_field>.
*&SPWIZARD: END OF LOCAL DATA------------------------------------------*

  ASSIGN (p_tc_name) TO <tc>.

*&SPWIZARD: get the table, which belongs to the tc                     *
  CONCATENATE p_table_name '[]' INTO l_table_name. "table body
  ASSIGN (l_table_name) TO <table>.                "not headerline

*&SPWIZARD: demark all filled lines                                    *
  LOOP AT <table> ASSIGNING <wa>.

*&SPWIZARD: access to the component 'FLAG' of the table header         *
    ASSIGN COMPONENT p_mark_name OF STRUCTURE <wa> TO <mark_field>.

    <mark_field> = space.
  ENDLOOP.
ENDFORM.                                          "fcode_tc_mark_lines
