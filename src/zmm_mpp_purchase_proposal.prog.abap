*&***********************************************************************************************&*
*& OBJECT NAME          : ZMM_MPP_PURCHASE_PROPOSAL                                              &*
*& TECHNICAL CONSULTANT : RAVINDRA SANAP                                                         &*
*& TEAM LEAD            : POONAM SHINDE                                                          &*
*& FUCTIONAL            : VENU                                                                   &*
*& MODULE NAME          : MM                                                                     &*
*& PROGRAM TYPE         : MODULE POOL                                                            &*
*& TCODE                : ZMM014                                                                 &*
*& CREATE DATE          : OCT 11,2016                                                            &*
*& TRANSPORT NO         : IRDK925982                                                             &*
*& DESCRIPTION          : THIS IS DEVELOPED FOR PURCHASE PROPOSAL - Process Document             &*
*&                        FEATURES OF THE PROPOSAL: CREATE,CREATE WITH REFRENCE,CHANGE,DISPLAY,  &*
*&                                    ATTACHMENT : CREATE,DELETE,DISPLAY                         &*
* REVISION HISTORY--------------------------------------------------------------------------------*
*                                                                                                 *
*   CHANGED BY:                                                                                   *
*   CHANGE ON:                                                                                    *
*   REASON FOR CHANGE:                                                                            *
*                                                                                                 *
*                                                                                                 *
* REVISION HISTORY--------------------------------------------------------------------------------*


REPORT  zmm_mpp_purchase_proposal.

INCLUDE zmm_include_mpp_pur_proposal.

START-OF-SELECTION.
 PERFORM check_authorization.
  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZSTAT100'.
  SET TITLEBAR 'ZTIT100'.
  CLEAR: gv_prno,gv_ekgrp,gv_werks,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,text,remarks,flag,flag1,gv_okcode,
         gs_tab1,lv_maktx,lv_name1,gs_tab2,gs_tab22,g_lineno,lv_curline.
  REFRESH: gt_tab1,gt_tab2,gt_tab3,g_mytable,list.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE user_command_0100 INPUT.

  gv_okcode = sy-ucomm.
  CASE gv_okcode.

    WHEN 'EXECUTE'.
      IF r1 = 'X'.         "create
        gv_date = sy-datum.
        gv_user = sy-uname.
        gv_time = sy-timlo.
        CALL SCREEN 0101.
      ELSEIF r2 = 'X'.     "create with refrence
        CALL SCREEN 0104.
      ELSEIF r3 = 'X'.    " change
        CALL SCREEN 0102.
      ELSEIF r4 = 'X'.     "display proposal
        CALL SCREEN 0103.
      ELSEIF r5 = 'X'.     "add/delete/view attchment
        CALL SCREEN 0502.
*      ELSEIF r6 = 'X'.     "only view
*        CALL SCREEN 0501.
      ELSEIF r7 = 'X'.     "report
        CALL TRANSACTION 'ZMM016' AND SKIP FIRST SCREEN .
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'ZPF101'.
  SET TITLEBAR 'ZTIT101'.

  CLEAR: gv_okcode.
  gv_okcode = sy-ucomm.
  CASE gv_okcode.
    WHEN 'BACK'.
      CLEAR:sy-ucomm,gv_ekgrp,gv_werks,list_box,list_box2,list_box3,list_box4,remarks,
            gv_plantname,gv_user.
      REFRESH:gt_tab1,gt_tab2,gt_tab3,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
         list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal.
      CLEAR: gs_tab1,gs_tab22,gs_tab2,gs_tab3,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab3,gs_stxh,gs_a516, gs_pa0001,
             gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
             flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
             no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
             gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
             gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs,gv_plantname,gv_eknam,gv_date,g_mytable,gv_user,gv_time,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,remarks,old_remarks,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4.
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      CALL METHOD remarks_container->free.
      CALL METHOD remarks_editor->free.
      LEAVE TO SCREEN 100.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      CLEAR sy-ucomm.
      LEAVE PROGRAM.
  ENDCASE.

  CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN 0101.

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

    flag1 = 'X'.

  ENDIF.

*  CLEAR:gv_ekgrp,gv_werks,list_box,list_box2,list_box3,list_box4,remarks,
*            gv_plantname,gv_user.
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*  IF manager IS INITIAL.
*    obj-objtype = objtype.
*    SELECT SINGLE name
*    FROM trdir
*    INTO obj-objkey
*    WHERE name = sy-repid.
*    CLEAR: exclude_tab, exclude_wa.
*    MOVE 'ATTACH' TO exclude_wa-fcode.
*    APPEND exclude_wa TO exclude_tab.
*    MOVE 'LIST' TO exclude_wa-fcode.
*    APPEND exclude_wa TO exclude_tab.
**    SET PF-STATUS 'MAIN' EXCLUDING exclude_tab.
*    CREATE OBJECT manager
*      EXPORTING
*        is_object    = obj
*        ip_no_commit = 'R'
*      EXCEPTIONS
*        OTHERS       = 1.
*
*    CREATE OBJECT manager
*      EXPORTING
*        ip_no_commit = 'R'
*      EXCEPTIONS
*        OTHERS       = 1.
*  ENDIF.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  DATA: cols TYPE cxtab_column.
*      CONTROLS: tc1 TYPE TABLEVIEW USING SCREEN '0101'.
  LOOP AT tc1-cols INTO cols WHERE index GT 0.
    IF  cols-screen-group1 = 'MT1'
         AND cols-screen-input = '0'.
      cols-screen-input = '1'."'1'.
    ELSEIF cols-screen-group1 = 'MT1'
       AND cols-screen-input = '1'.
      cols-screen-input = '0'.
    ENDIF.
    MODIFY tc1-cols FROM cols INDEX sy-tabix.
  ENDLOOP.


ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor OUTPUT.
  DATA: field1(20) TYPE c.
  CLEAR: field1.
  GET CURSOR FIELD field1.
ENDMODULE.                    "GET_CURSOR OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_0100 INPUT.
  CLEAR: gv_okcode.
  gv_okcode = sy-ucomm.
  CASE gv_okcode.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " EXIT_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  IF sy-ucomm = 'EXIT'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_EKGRP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_ekgrp INPUT.

  SELECT ekgrp eknam FROM t024 INTO TABLE lt_f4_pgrp  WHERE ekgrp IN ('B01','B02','B03','B04','B05','B06').

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'EKGRP'
      value_org       = 'S'
*     dynpprog        = ''
      dynpnr          = '0101'
      dynprofield     = 'GV_EKGRP'
     TABLES
      value_tab       = lt_f4_pgrp
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gv_ekgrp = it_ret-fieldval.
    SELECT SINGLE eknam FROM t024 INTO gv_eknam WHERE ekgrp = gv_ekgrp.
    CLEAR: it_ret,lt_f4_pgrp.
  ENDIF.

ENDMODULE.                 " F4_EKGRP  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_WERKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_werks INPUT.

  CLEAR: it_ret,lt_f4_werks.
  SELECT werks name1 FROM t001w INTO TABLE lt_f4_werks .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'WERKS'
      value_org       = 'S'
*     dynpprog        = ''
      dynpnr          = '0101'
      dynprofield     = 'GV_WERKS'
     TABLES
      value_tab       = lt_f4_werks
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gv_werks = it_ret-fieldval.
    SELECT SINGLE name1 FROM t001w INTO gv_plantname WHERE werks = gv_werks.
    CLEAR: it_ret,lt_f4_werks.
  ENDIF.
ENDMODULE.                 " F4_WERKS  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_101 INPUT.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      IF gv_ekgrp IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
                 titlebar                     = 'Proposal Data'
                  text_question               = 'Do you want to Save Proposal data ?'
                 text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
                 text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
                 default_button               = '1'
                 display_cancel_button        = ' '
                 start_column                 = 55
                 start_row                    = 3
               IMPORTING
                 answer                       = lv_ans                 .
        IF lv_ans = 2.
          LEAVE PROGRAM.
        ENDIF.
      ELSE.
        LEAVE PROGRAM.
      ENDIF.
  ENDCASE.
  CLEAR:sy-ucomm,gv_ekgrp,gv_werks.
ENDMODULE.                 " EXIT_101  INPUT


*&--------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_matnr INPUT.

  CLEAR: it_ret,lt_mara,ls_mara.
  REFRESH: lt_f4_matnr,lt_f4_matnr1.
  IF gv_ekgrp IS NOT INITIAL.
    SELECT matnr FROM marc INTO TABLE lt_f4_matnr WHERE ekgrp = gv_ekgrp." AND WERKS = GV_WERKS.
    IF lt_f4_matnr IS NOT INITIAL.
      SELECT matnr maktx FROM makt INTO TABLE lt_f4_matnr1 FOR ALL ENTRIES IN lt_f4_matnr WHERE matnr = lt_f4_matnr-matnr AND spras = 'E'..
    ENDIF.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'MATNR'
        value_org       = 'S'
*     dynpprog        = ''
        dynpnr          = '0101'
        dynprofield     = 'GS_TAB1-MATNR'
       TABLES
        value_tab       = lt_f4_matnr1
        return_tab      = it_ret
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3.
    ELSE.
      READ TABLE it_ret INDEX 1.
      gs_tab1-matnr = it_ret-fieldval.
      CLEAR: it_ret,lt_f4_matnr.
      REFRESH: lt_f4_matnr,lt_f4_matnr1.
    ENDIF.
  ELSE.
    MESSAGE text-011 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
  ENDIF.

ENDMODULE.                 " F4_MATNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TC1_LIFNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tc1_lifnr INPUT.

  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.

  CLEAR : selline ,selindex.
  GET CURSOR FIELD gs_tab1-lifnr LINE selline.
*  selindex = tc1-top_line + selline - 1.   "
  READ TABLE gt_tab1 INTO gs_tab1
                            INDEX selindex.
*  IF sy-subrc = 0.
  REFRESH:lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                            WHERE lifnr = lt_lfb1-lifnr.
  ENDIF.
  REFRESH it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'LIFNR'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0101'
     dynprofield = 'GS_TAB1-LIFNR'
     TABLES
     value_tab   = lt_f4_lifnr
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
    gs_tab1-lifnr = it_ret-fieldval.
    CLEAR: it_ret,lt_f4_lifnr.
    MODIFY gt_tab1 FROM gs_tab1 INDEX selline TRANSPORTING lifnr.
  ENDIF.

ENDMODULE.                 " F4_TC1_LIFNR  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TC1_WAERS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tc1_waers INPUT.

  CLEAR : selline ,selindex.
  GET CURSOR FIELD gs_tab1-waers LINE selline.
*  selindex = tc1-top_line + selline - 1.   "
  READ TABLE gt_tab1 INTO gs_tab1
                            INDEX selindex.
*  IF sy-subrc = 0.
  SELECT waers ltext FROM tcurt INTO TABLE lt_f4_waers WHERE spras = 'EN'.
  REFRESH:it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'WAERS'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0101'
     dynprofield = 'GS_TAB1-WAERS'
     TABLES
     value_tab   = lt_f4_waers
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
    gs_tab1-waers = it_ret-fieldval.
    CLEAR: it_ret,lt_f4_lifnr.
*    MODIFY gt_tab1 FROM gs_tab1 INDEX selline TRANSPORTING lifnr.
  ENDIF.

ENDMODULE.                 " F4_TC1_WAERS  INPUT
*&---------------------------------------------------------------------*
*&      Module  TEXT_EDITOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE text_editor INPUT.
*  DATA: g_lineno TYPE sy-index,
*        gs_tab2 TYPE ty_tab1.

ENDMODULE.                    "text_editor INPUT


*&---------------------------------------------------------------------*
*&      Module  COPY_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE copy_data INPUT.

  gv_okcode = sy-ucomm.
  CLEAR: g_lineno.
*  selindex = tc1-top_line + selline - 1.
  IF gv_okcode = 'COPY'.
    GET CURSOR LINE g_lineno.
    IF tc1-lines GE 10 AND tc1-top_line NE 1.
      CLEAR: g_lineno,gs_tab1.
      g_lineno = tc1-lines.
    ENDIF.

    READ TABLE gt_tab1 INTO gs_tab1 INDEX g_lineno.
    IF sy-subrc = 0.
      gs_tab1-ebelp =  gs_tab1-ebelp + 1.
      gs_tab1-netpr = ' '.
      gs_tab1-landed_price = ' '.
* *  APPEND GS_TAB1 TO GT_TAB1 WHERE INDEX = GS_TAB1-EBELP .
      INSERT gs_tab1 INTO gt_tab1 INDEX gs_tab1-ebelp.
      CLEAR gs_tab1.
      LOOP AT gt_tab1 INTO gs_tab1.
        gs_tab1-ebelp = sy-tabix .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_tab1-ebelp
          IMPORTING
            output = gs_tab1-ebelp.
        MODIFY gt_tab1 FROM gs_tab1.
        CLEAR: gs_tab1.
      ENDLOOP.
      CLEAR : gv_okcode,sy-ucomm.
    ENDIF.
  ENDIF.

ENDMODULE.                 " COPY_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  gv_okcode = sy-ucomm.
  CASE gv_okcode.

    WHEN ' '.
      CLEAR : flag_final,flag3_1,flag3_3,flag7.
      SELECT SINGLE eknam FROM t024 INTO gv_eknam WHERE ekgrp = gv_ekgrp.
      SELECT SINGLE name1 FROM t001w INTO gv_plantname WHERE werks = gv_werks.
      GET CURSOR LINE g_lineno .
      IF tc1-lines GE 10 AND tc1-top_line NE 1.
        CLEAR: g_lineno,gs_tab1.
        g_lineno = tc1-lines.
      ENDIF.
      READ TABLE gt_tab1 INTO gs_tab1 INDEX g_lineno.
      IF sy-subrc = 0.
*{   REPLACE        SBXK900030                                        1
*\        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\          EXPORTING
*\            input  = gs_tab1-matnr
*\          IMPORTING
*\            output = gs_tab1-matnr.
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
          input  = gs_tab1-matnr
        IMPORTING
          output = gs_tab1-matnr.

*}   REPLACE
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
          PERFORM check_material.
        ENDIF.
        PERFORM check_vendor.
        PERFORM check_newprice_landprice.  " compare
        PERFORM final_data.
      ENDIF.
      PERFORM get_auth_list.

    WHEN 'PTERM'.
      GET CURSOR LINE g_lineno.
      READ TABLE gt_tab1 INTO gs_tab1 INDEX g_lineno.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
        PERFORM line_text_create.
      ENDIF.
      IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
        PERFORM line_text_create_1.
      ENDIF.

    WHEN 'SAVE_TEXT'.
      IF flag = 'X'.
        PERFORM save_line_text.
      ELSE.
        MESSAGE text-013 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

*    WHEN 'ATTACH'.
*
*      CALL METHOD manager->start_service_direct
*        EXPORTING
*          ip_service       = 'CREATE_ATTA'
*          is_object        = obj
*        EXCEPTIONS
*          no_object        = 1
*          object_invalid   = 2
*          execution_failed = 3
*          OTHERS           = 4.

*    WHEN 'LIST'.
*      CALL METHOD manager->start_service_direct
*        EXPORTING
*          ip_service       = 'VIEW_ATTA'
*          is_object        = obj
*        EXCEPTIONS
*          no_object        = 1
*          object_invalid   = 2
*          execution_failed = 3
*          OTHERS           = 4.

    WHEN 'CHECK'.
     CLEAR: flag_final,flag3_1,flag3_3,flag7,flag_final.
      IF gt_tab1 IS INITIAL.
        MESSAGE text-014 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 100.
      ENDIF.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'..
        PERFORM check_material.
        PERFORM check_final_101.
      ENDIF.
      IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
        PERFORM check_final_101_1.
      ENDIF.
      PERFORM check_releaser_and_remarks.

    WHEN 'SAVE'.                                     "ravi101

      IF flag6 = 'X'.
        IF gt_tab1 IS NOT INITIAL.
          CLEAR: flag3,flag3_1,flag3_2.
          IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'..
            PERFORM check_save_final_101.
          ENDIF.
          IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
            PERFORM check__save_final_101_1.
          ENDIF.
          CLEAR remarks.
          CALL METHOD remarks_editor->get_textstream
            IMPORTING
              text = remarks.
          CALL METHOD cl_gui_cfw=>flush
            EXCEPTIONS
              cntl_system_error = 1
              cntl_error        = 2
              OTHERS            = 3.
          PERFORM check_releaser_101.

          IF flag3 NE 'X'.
            MESSAGE text-015 TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF flag7 = 'X'.
            PERFORM screen_edit101.
            CLEAR flag7.
            MESSAGE 'Check Material,Vendor,Qty,New Basic Price,Landed Price' TYPE 'S' DISPLAY LIKE 'E'.
          ELSEIF flag3_1 EQ 'X'.
            PERFORM screen_edit101.
            MESSAGE 'Check Material,Last Purchase and Business Plan Price' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR flag3_1.
          ELSEIF flag3_2 EQ 'X'.
            PERFORM screen_edit101.
            CLEAR flag3_2.
          ELSEIF flag3_3 EQ 'X'.
            PERFORM screen_edit101.
            CLEAR flag3_3.
          ELSEIF flag3_4 EQ 'X'.
            PERFORM screen_edit101.
            CLEAR flag3_4.
          ELSEIF remarks IS INITIAL.
            MESSAGE text-018 TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSE.
            IF gv_ekgrp NOT BETWEEN 'B06' AND 'B07'.
              PERFORM compare_sel_new_basic_price.      "selected line check with other
              CLEAR flag3_4.
            ENDIF.
            lv_msg = 'Save Proposal'.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
               titlebar                     = lv_msg
                text_question               = 'Do you want to Save Proposal ?'
               text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
               text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
               default_button               = '1'
               display_cancel_button        = ' '
               start_column                 = 55
               start_row                    = 3
             IMPORTING
               answer                       = gv_ans                 .
            IF gv_ans = 2.
              LOOP AT SCREEN.
                screen-input = 0.
                screen-active = 0.
                MODIFY SCREEN.
              ENDLOOP.
            ELSE.
              PERFORM save_proposal.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC1' ITSELF
*CONTROLS: TC1 TYPE TABLEVIEW USING SCREEN 0101.

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

  DATA: lv_line TYPE i.
  IF gs_tab1 IS NOT INITIAL.
    lv_line  = tc1-current_line.
  ENDIF.
*  LOOP AT SCREEN.                                  "ravi
**     IF tc1-current_line = lv_line.
*    IF flag3_1 NE 'X'.
*      IF gs_tab1-chk = 'X'.
*        screen-input = 0.
*        screen-active = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
ENDMODULE.                    "TC1_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc1_modify INPUT.
  gs_tab1-ebelp = tc1-current_line.  "RAVI
  MODIFY gt_tab1
    FROM gs_tab1
    INDEX tc1-current_line.
  IF sy-subrc <> 0.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab1-matnr
*\      IMPORTING
*\        output = gs_tab1-matnr.
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
        input  = gs_tab1-matnr
      IMPORTING
        output = gs_tab1-matnr.

*}   REPLACE
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_tab1-lifnr
      IMPORTING
        output = gs_tab1-lifnr.
    IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
      SELECT SINGLE maktx FROM makt INTO lv_maktx WHERE matnr = gs_tab1-matnr AND spras = 'E'.
      gs_tab1-maktx = lv_maktx.
    ENDIF.
    SELECT SINGLE name1 FROM lfa1 INTO lv_name1 WHERE lifnr = gs_tab1-lifnr.
    gs_tab1-name1 = lv_name1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_tab1-matnr
      IMPORTING
        output = gs_tab1-matnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_tab1-lifnr
      IMPORTING
        output = gs_tab1-lifnr.
*    condense gs_tab1-landed_price No-GAPS.
    APPEND gs_tab1 TO gt_tab1.
    CLEAR: lv_maktx,lv_name1,lv_kbetr,gs_a516.
  ENDIF.
  DELETE ADJACENT DUPLICATES FROM gt_tab1 COMPARING ALL FIELDS.

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
  l_selline = l_selline + 1.          """""""""RAVI
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
ENDFORM.                    "fcode_tc_demark_lines
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_cursor INPUT.
  CLEAR field1.
  GET CURSOR FIELD field1.
  CASE field1.
    WHEN 'GV_EKGRP'.
      SET CURSOR FIELD 'GV_EKGRP' OFFSET gv_ekgrp.
    WHEN 'GV_WERKS'.
      SET CURSOR FIELD 'GV_WERKS' OFFSET gv_werks.
    WHEN 'PTERM'.
      SET CURSOR FIELD 'GS_TAB1-COMP_DATA_AVL' OFFSET gs_tab1-comp_data_avl.
  ENDCASE.

ENDMODULE.                 " SET_CURSOR  INPUT
"fcode_tc_mark_lines
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-COSTFORMULA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-costformula INPUT.

  TYPES: BEGIN OF ty_compdata,
         field1 TYPE lfa1-name1,
         END OF ty_compdata.

  DATA: gt_compdata  TYPE TABLE OF ty_compdata,
        gs_compdata  TYPE ty_compdata.

  IF flag4 = ' '.
    gs_compdata-field1 = 'Yes'.
    APPEND gs_compdata TO gt_compdata.
    CLEAR gs_compdata.
    gs_compdata-field1 = 'No'.
    APPEND gs_compdata TO gt_compdata.
    flag4 = 'X'.
  ENDIF.

  CLEAR: it_ret.
  GET CURSOR FIELD gs_tab1-costformula LINE selline.
  READ TABLE gt_tab1 INTO gs_tab1
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COSTFORMULA'
      dynpnr          = '101'
      dynprofield     = 'GS_TAB1-COSTFORMULA'
      value_org       = 'S'
    TABLES
      value_tab       = gt_compdata
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab1-costformula = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
  IF selline NE 0.
    MODIFY gt_tab1 FROM gs_tab1 INDEX selline TRANSPORTING costformula .
  ENDIF.

ENDMODULE.                 " F4_TCL-COSTFORMULA  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-COMP_DATA_AVL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-comp_data_avl INPUT.

  TYPES: BEGIN OF ty_compdata1,
         field1 TYPE lfa1-name1,
       END OF ty_compdata1.

  DATA: gt_compdata1  TYPE TABLE OF ty_compdata1,
        gs_compdata1  TYPE ty_compdata1.

  IF flag5 = ' '.
    gs_compdata1-field1 = 'Yes'.
    APPEND gs_compdata1 TO gt_compdata1.
    CLEAR gs_compdata1.
    gs_compdata1-field1 = 'No'.
    APPEND gs_compdata1 TO gt_compdata1.
    flag5 = 'X'.
  ENDIF.

  CLEAR: it_ret.
  GET CURSOR FIELD gs_tab1-comp_data_avl LINE selline.
  READ TABLE gt_tab1 INTO gs_tab1
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COMP_DATA_AVL'
      dynpnr          = '101'
      dynprofield     = 'GS_TAB1-COMP_DATA_AVL'
      value_org       = 'S'
    TABLES
      value_tab       = gt_compdata1
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab1-comp_data_avl = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
  IF selline NE 0.
    MODIFY gt_tab1 FROM gs_tab1 INDEX selline TRANSPORTING comp_data_avl.
  ENDIF.

ENDMODULE.                 " F4_TCL-COMP_DATA_AVL  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-MWSKZ  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-mwskz INPUT.

  TYPES: BEGIN OF ty_mwskz,
         mwskz TYPE t007s-mwskz,
         text1 TYPE t007s-text1,
         END OF ty_mwskz.

  DATA: lt_f4_mwskz TYPE TABLE OF ty_mwskz,
        ls_f4_mwskz TYPE ty_mwskz.

  SELECT mwskz text1 FROM t007s INTO TABLE lt_f4_mwskz WHERE spras = 'EN'.

  GET CURSOR FIELD gs_tab1-mwskz LINE selline.
  READ TABLE gt_tab1 INTO gs_tab1
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MWSKZ'
      dynpnr          = '101'
      dynprofield     = 'GS_TAB1-MWSKZ'
      value_org       = 'S'
    TABLES
      value_tab       = lt_f4_mwskz
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab1-mwskz = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
*     if selline NE 0.
*     MODIFY gt_tab1 FROM gs_tab1 INDEX selline TRANSPORTING comp_data_avl.
*     endif.
*
ENDMODULE.                 " F4_TCL-MWSKZ  INPUT

*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_material .

  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  REFRESH: lt_matnr.
*{   REPLACE        SBXK900030                                        1
*\  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\    EXPORTING
*\      input  = gs_tab1-matnr
*\    IMPORTING
*\      output = gs_tab1-matnr.
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
      input  = gs_tab1-matnr
    IMPORTING
      output = gs_tab1-matnr.

*}   REPLACE

  IF gs_tab1-matnr IS NOT INITIAL.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
    CLEAR ls_matnr.
    READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = gs_tab1-matnr.
    IF sy-subrc NE 0.
      MESSAGE i004(zpp) WITH gs_tab1-matnr gv_ekgrp DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
    IF lt_matnr IS INITIAL.
      MESSAGE i004(zpp) WITH gs_tab1-matnr gv_ekgrp DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  CHECK_FINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_final_101 .
  CLEAR flag3.
  LOOP AT gt_tab1 INTO gs_tab1.

    lv_no = sy-tabix - 1.
    READ TABLE gt_tab1 INTO gs_tab2 INDEX lv_no.
    IF sy-subrc = 0.
      CLEAR flag7.
      IF gs_tab1-lifnr = gs_tab2-lifnr AND gs_tab1-matnr = gs_tab2-matnr.
        MESSAGE i003(zpp) WITH gs_tab1-lifnr gs_tab1-matnr DISPLAY LIKE 'E'.
        flag7 = 'X'.
        flag_final = 'X'.
      ENDIF.
    ENDIF.

    PERFORM check_newprice_landprice.
*          PERFORM check_best_line.

    CLEAR: lv_matnr,lv_index.
    IF gs_tab1-chk = 'X'.
      flag3 = 'X'.
      lv_matnr = gs_tab1-matnr.
      lv_index = gs_tab1-ebelp + 1.
    ENDIF.
    REFRESH lt_matnr.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab1-matnr
*\      IMPORTING
*\        output = gs_tab1-matnr.
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
        input  = gs_tab1-matnr
      IMPORTING
        output = gs_tab1-matnr.

*}   REPLACE
    PERFORM check_mt_vn_bline.  "chk material,ven,zbpl,best line
    PERFORM check_vendor_101.
    CLEAR: gs_tab1,gs_tab2.
  ENDLOOP.
ENDFORM.                    " CHECK_FINAL

*&---------------------------------------------------------------------*
*&      Form  CHECK_FINAL_101_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_final_101_1 .
  REFRESH : gt_tab2.
  CLEAR flag3.
  gt_tab2[] = gt_tab1[].
  LOOP AT gt_tab1 INTO gs_tab1.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab1-chk = 'X'.
      flag3 = 'X'.
      lv_maktx = gs_tab1-maktx.
      lv_index = gs_tab1-ebelp + 1.
    ENDIF.
    LOOP AT gt_tab2 INTO gs_tab2." WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab2-ebelp = lv_index.
        IF gs_tab2-chk = 'X'.
          IF gs_tab2-maktx = lv_maktx.
            MESSAGE i010(zpp) WITH gs_tab2-maktx DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            flag_final = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab2.
    ENDLOOP.
    PERFORM check_vendor_101.
    IF gs_tab1-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      flag_final = 'X'.
    ENDIF.
    IF gs_tab1-netpr > gs_tab1-landed_price.
      MESSAGE i013(zpp) WITH gs_tab1-netpr gs_tab1-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      flag_final = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_FINAL_101_1
*&---------------------------------------------------------------------*
*&      Form  CHECK_RELEASER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_releaser_and_remarks.
  CLEAR remarks.
  CALL METHOD remarks_editor->get_textstream
    IMPORTING
      text = remarks.
  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  CLEAR :gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4.
  READ TABLE list WITH KEY list_box.
  IF sy-subrc = 0.
    gv_releaser1 = list-key.
    gv_releaser_name1 = list-text.
  ENDIF.
  READ TABLE list2 WITH KEY list_box2.
  IF sy-subrc = 0.
    gv_releaser2 = list2-key.
    gv_releaser_name2 = list2-text.
  ENDIF.
  READ TABLE list3 WITH KEY list_box3.
  IF sy-subrc = 0.
    gv_releaser3 = list3-key.
    gv_releaser_name3 = list3-text.
  ENDIF.
  READ TABLE list4 WITH KEY list_box4.
  IF sy-subrc = 0.
    gv_releaser4 = list4-key.
    gv_releaser_name4 = list4-text.
  ENDIF.
  IF gv_releaser1 IS INITIAL AND gv_releaser2 IS INITIAL.
    MESSAGE text-016 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  IF gv_releaser1 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser4 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser4 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF flag3 NE 'X'.
    MESSAGE text-015 TYPE 'S' DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag3_2 = 'X'.
    PERFORM screen_edit101.
    CLEAR flag3_2.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag3_3 = 'X'.
    PERFORM screen_edit101.
    CLEAR flag3_3.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag7 = 'X'.
    PERFORM screen_edit101.
    CLEAR flag7.
  ELSEIF remarks IS INITIAL.
    MESSAGE text-018 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF flag_final = 'X'.
    PERFORM screen_edit101.
    CLEAR flag_final.
  ELSEIF flag_final = ' '.
    MESSAGE 'All entered line items are correct,You can Save your Proposal' TYPE 'I'.
    CLEAR flag_final.
  ENDIF.
  flag6 ='X'.
ENDFORM.                    " CHECK_RELEASER

*&---------------------------------------------------------------------*
*&      Form  CHECK_SAVE_FINAL_101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_save_final_101 .
  REFRESH gt_tab2.
  gt_tab2[] = gt_tab1[].
  LOOP AT gt_tab1 INTO gs_tab1.
    IF gs_tab1-business_price = 0.
      MESSAGE i001(zpp) WITH gs_tab1-matnr gs_tab1-business_price DISPLAY LIKE 'E'.
      flag3_1 = 'X'.
    ENDIF.
    IF gs_tab1-mwskz IS INITIAL.
      MESSAGE i019(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
      flag3_3 = 'X'.
    ENDIF.
    lv_no = sy-tabix - 1.
    READ TABLE gt_tab1 INTO gs_tab2 INDEX lv_no.
    IF sy-subrc = 0.
      CLEAR flag7.
      IF gs_tab1-lifnr = gs_tab2-lifnr AND gs_tab1-matnr = gs_tab2-matnr.
        MESSAGE i003(zpp) WITH gs_tab1-lifnr gs_tab1-matnr DISPLAY LIKE 'E'.
        flag7 = 'X'.
      ENDIF.
    ENDIF.
    PERFORM check_newprice_landprice.
*          PERFORM check_best_line.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab1-chk = 'X'.
      flag3 = 'X'.
      lv_matnr = gs_tab1-matnr.
      lv_index = gs_tab1-ebelp + 1.
    ENDIF.
    REFRESH lt_matnr.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab1-matnr
*\      IMPORTING
*\        output = gs_tab1-matnr.
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
        input  = gs_tab1-matnr
      IMPORTING
        output = gs_tab1-matnr.

*}   REPLACE
    PERFORM check_mt_vn_bline.  "chk material,ven,zbpl,best line
    PERFORM check_vendor_101.

    IF gs_tab1-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
    IF gs_tab1-business_price = 0.
      MESSAGE i001(zpp) WITH gs_tab1-matnr gs_tab1-business_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
    IF gs_tab1-netpr > gs_tab1-landed_price.
      MESSAGE i013(zpp) WITH gs_tab1-netpr gs_tab1-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab1-netpr IS INITIAL.
      MESSAGE i017(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab1-landed_price IS INITIAL.
      MESSAGE i018(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab1-mwskz IS INITIAL.
      MESSAGE i019(zpp) WITH gs_tab1-matnr  DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_SAVE_FINAL_101

*&---------------------------------------------------------------------*
*&      Form  CHECK__SAVE_FINAL_101_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check__save_final_101_1 .
  REFRESH : gt_tab2.
  gt_tab2[] = gt_tab1[].
  LOOP AT gt_tab1 INTO gs_tab1.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab1-chk = 'X'.
      flag3 = 'X'.
      lv_maktx = gs_tab1-maktx.
      lv_index = gs_tab1-ebelp + 1.
    ENDIF.
    LOOP AT gt_tab2 INTO gs_tab2." WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab2-ebelp = lv_index.
        IF gs_tab2-chk = 'X'.
          IF gs_tab2-maktx = lv_maktx.
            MESSAGE i010(zpp) WITH gs_tab2-maktx DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab2.
    ENDLOOP.
    PERFORM check_vendor_101.
    IF gs_tab1-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
     if gs_tab1-netpr is initial.
      message i017(zpp) with gs_tab1-matnr display like 'E'.
      flag7 = 'X'.
      exit.
    endif.
    if gs_tab1-landed_price is initial.
      message i018(zpp) with gs_tab1-matnr display like 'E'.
      flag7 = 'X'.
      exit.
    endif.
    IF gs_tab1-netpr > gs_tab1-landed_price.
      MESSAGE i013(zpp) WITH gs_tab1-netpr gs_tab1-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK__SAVE_FINAL_101_1

*&---------------------------------------------------------------------*
*&      Form  CHECK_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vendor .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_tab1-lifnr
    IMPORTING
      output = gs_tab1-lifnr.
  IF gs_tab1-lifnr IS NOT INITIAL.
    REFRESH:lt_lfb1,lt_f4_lifnr.
    DATA : ls_f4_lifnr TYPE ty_f4_lifnr.
    CLEAR:ls_f4_lifnr.
    SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
    IF lt_lfb1 IS NOT INITIAL.
      SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                             WHERE lifnr = lt_lfb1-lifnr.
      READ TABLE lt_f4_lifnr INTO ls_f4_lifnr WITH KEY lifnr = gs_tab1-lifnr.
      IF sy-subrc NE 0.
        MESSAGE i008(zpp) WITH gs_tab1-lifnr gv_werks DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

    IF lt_f4_lifnr IS INITIAL.
      MESSAGE i008(zpp) WITH gs_tab1-lifnr gv_werks DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_VENDOR

*&---------------------------------------------------------------------*
*&      Form  CHECK_NEWPRICE_LANDPRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_newprice_landprice .

  IF gs_tab1-qty IS NOT INITIAL.
    flag_final = ' '.
  ENDIF.
  IF gs_tab1-qty IS INITIAL.
    MESSAGE i016(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ENDIF.
  IF gs_tab1-netpr IS INITIAL.
    MESSAGE i017(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ENDIF.
  IF gs_tab1-landed_price IS INITIAL.
    MESSAGE i018(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  endif.
  IF gs_tab1-netpr > gs_tab1-landed_price.
    MESSAGE i013(zpp) WITH gs_tab1-netpr gs_tab1-landed_price DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ENDIF.

if gv_ekgrp not between 'B05' and 'B06'.
  IF gs_tab1-business_price = 0.
    MESSAGE i001(zpp) WITH gs_tab1-matnr gs_tab1-business_price DISPLAY LIKE 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
  ENDIF.

  IF gs_tab1-mwskz IS INITIAL.
    MESSAGE i019(zpp) WITH gs_tab1-matnr DISPLAY LIKE 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
  ENDIF.
 ENDIF.
ENDFORM.                    " CHECK_NEWPRICE_LANDPRICE

*&---------------------------------------------------------------------*
*&      Form  CHECK_BEST_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_best_line .
  CLEAR flag3.
  LOOP AT gt_tab1 INTO gs_tab1 .
    IF gs_tab1-chk = 'X'.
      flag3 = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_BEST_LINE
*&---------------------------------------------------------------------*
*&      Form  FINAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_data .
  TRANSLATE gs_tab1-waers TO UPPER CASE.
  TRANSLATE gs_tab1-mwskz TO UPPER CASE.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_tab1-lifnr
    IMPORTING
      output = gs_tab1-lifnr.

  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    GET CURSOR LINE g_lineno.
    CLEAR gs_tab1.
    READ TABLE gt_tab1 INTO gs_tab1 INDEX g_lineno.
    gs_tab1-matnr = ' '.
    gs_tab1-maktx = gs_tab1-maktx.
  ENDIF.

  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab1-matnr
*\      IMPORTING
*\        output = gs_tab1-matnr.
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
        input  = gs_tab1-matnr
      IMPORTING
        output = gs_tab1-matnr.

*}   REPLACE
    SELECT SINGLE maktx FROM makt INTO lv_maktx WHERE matnr = gs_tab1-matnr.
    gs_tab1-maktx = lv_maktx.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_tab1-lifnr
    IMPORTING
      output = gs_tab1-lifnr.

  SELECT SINGLE name1 FROM lfa1 INTO lv_name1 WHERE lifnr = gs_tab1-lifnr.
*    IF sy-subrc = 0.
  gs_tab1-name1 = lv_name1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_tab1-lifnr
    IMPORTING
      output = gs_tab1-lifnr.
*          gs_tab1-landed_price = gs_tab1-qty + lv_cons.
  gs_tab1-landed_value = ( gs_tab1-landed_price * gs_tab1-qty )." / 100.     landed peice  = purchase value
  REFRESH gt_a516.
  SELECT kschl
         werks
         matnr
         datbi
         datab
         knumh FROM a516 INTO TABLE gt_a516
          WHERE kschl = 'ZBPL' AND werks = gv_werks AND matnr = gs_tab1-matnr
                AND datbi GT gv_date AND datab LE gv_date.
  IF sy-subrc = 0.
    READ TABLE gt_a516 INTO gs_a516 INDEX 1."datab le gv_date.
    IF sy-subrc = 0.
      SELECT SINGLE kbetr FROM konp INTO lv_kbetr WHERE knumh = gs_a516-knumh.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
        gs_tab1-business_price = lv_kbetr.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    gs_tab1-business_price = ' '.
  ENDIF.
  SELECT SINGLE text1 FROM t007s INTO gs_tab1-tax_desc WHERE spras = 'EN' AND mwskz = gs_tab1-mwskz.
  REFRESH gt_ekpo.

  DATA: zlifnr TYPE lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT         = gs_tab1-lifnr
   IMPORTING
     OUTPUT        = zlifnr
            .



  SELECT ebeln
         EBELP_I
         aedat
         matnr_i
         werks_i
         netpr_i
    FROM WB2_V_EKKO_EKPO2
    INTO TABLE gt_ekpo WHERE MATNR_I = gs_tab1-matnr
    AND lifnr = zlifnr
    AND WERKS_I = gv_werks.
  IF gt_ekpo IS NOT INITIAL.
    IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
      SORT gt_ekpo DESCENDING BY aedat.
      READ TABLE gt_ekpo INTO gs_ekpo INDEX 1.
      IF sy-subrc = 0.
        IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
          gs_tab1-last_pur_basic_price = gs_ekpo-netpr_i.
          CLEAR gs_ekpo.
        ELSE.
          gs_tab1-last_pur_basic_price = ' '.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    gs_tab1-last_pur_basic_price = ' '.
  ENDIF.

  gs_tab1-vari_last_pur_inr = ( gs_tab1-last_pur_land_price - gs_tab1-landed_price )."( gs_tab1-landed_price - gs_tab1-last_pur_land_price )."Variance Last Purchase INR per KG
  gs_tab1-vari_last_pur_val = ( gs_tab1-vari_last_pur_inr * gs_tab1-qty ) / 100000.    "variance last purchase value
  gs_tab1-vari_business_plan_inr = ( gs_tab1-business_price - gs_tab1-landed_price ). "variance Business plan per KG
  gs_tab1-vari_business_plan_val = ( gs_tab1-vari_business_plan_inr * gs_tab1-qty ) / 100000. "variance Business plan value
*  gs_tab1-otr_vend_m1  = ( gs_tab1-req_m1 - gs_tab1-app_vend_m1 ).
*  IF gs_tab1-vari_last_pur_inr LT 0.
*    gs_tab1-vari_last_pur_inr = gs_tab1-vari_last_pur_inr * -1.
*  ENDIF.
*  gs_tab1-otr_vend_m2  = ( gs_tab1-req_m2 - gs_tab1-app_vend_m2 ).
*  IF gs_tab1-otr_vend_m2 LT 0.
*    gs_tab1-otr_vend_m2 = gs_tab1-otr_vend_m2 * -1.
*  ENDIF.
*  gs_tab1-otr_vend_m3  = gs_tab1-req_m3 - gs_tab1-app_vend_m3.
*  IF gs_tab1-otr_vend_m3 LT 0.
*    gs_tab1-otr_vend_m3 = gs_tab1-otr_vend_m3 * -1.
*  ENDIF.
  gs_tab1-clos_m1 = ( ( gs_tab1-op_m1 + gs_tab1-app_vend_m1 + gs_tab1-otr_vend_m1 ) - gs_tab1-req_m1 ) .
  gs_tab1-op_m2 = gs_tab1-clos_m1.
  gs_tab1-clos_m2 = ( ( gs_tab1-op_m2 + gs_tab1-app_vend_m2 + gs_tab1-otr_vend_m2 ) - gs_tab1-req_m2 ) .
  gs_tab1-op_m3 = gs_tab1-clos_m2.
  gs_tab1-clos_m3 = ( ( gs_tab1-op_m3 + gs_tab1-app_vend_m3 + gs_tab1-otr_vend_m3 ) - gs_tab1-req_m3 ) .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_tab1-matnr
    IMPORTING
      output = gs_tab1-matnr.
  LOOP AT gt_tab1 INTO gs_tab2 .
    IF sy-tabix = g_lineno.
      MODIFY gt_tab1 FROM gs_tab1 TRANSPORTING matnr maktx name1 landed_price landed_value
                         last_pur_basic_price last_pur_land_price business_price vari_last_pur_inr vari_last_pur_val
                         vari_business_plan_inr vari_business_plan_val tax_desc
                         otr_vend_m1 otr_vend_m2 otr_vend_m3 clos_m1 clos_m2 clos_m3 waers mwskz
                         op_m2 op_m3.
    ENDIF.
    CLEAR: gs_tab2,lv_kbetr.
  ENDLOOP.
  CLEAR: lv_maktx,lv_name1.

ENDFORM.                    " FINAL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_AUTH_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_auth_list.
  REFRESH: gt_user_list,gt_pa0001,gt_pa0000.
  CLEAR flag2.
  SELECT ekgrp
         pernr
         FROM zmm_pp_user_auth INTO TABLE gt_user_list WHERE ekgrp = gv_ekgrp.

  IF gt_user_list IS NOT INITIAL.
    SELECT pernr
           massn
           FROM pa0000 INTO TABLE gt_pa0000 FOR ALL ENTRIES IN gt_user_list
                                            WHERE pernr = gt_user_list-pernr.
  ENDIF.
  SORT gt_pa0000 BY pernr massn.
  LOOP AT gt_pa0000 INTO gs_pa0000.
    IF gs_pa0000-massn = 'I7'.
      DELETE gt_pa0000 WHERE pernr EQ gs_pa0000-pernr.
    ENDIF.
    CLEAR gs_pa0000.
  ENDLOOP.
  SORT gt_pa0000 BY pernr.
  DELETE ADJACENT DUPLICATES FROM gt_pa0000 COMPARING pernr.

  IF gt_pa0000 IS NOT INITIAL.
    SELECT pernr
           ename
           FROM pa0001 INTO TABLE gt_pa0001 FOR ALL ENTRIES IN gt_pa0000
                     WHERE pernr = gt_pa0000-pernr.
    SORT gt_pa0001 BY pernr.
    DELETE ADJACENT DUPLICATES FROM gt_pa0001 COMPARING pernr.
  ELSE.
    REFRESH:list,list2,list3,list4,gt_pa0001.
    CLEAR flag2.
    name = 'LIST_BOX'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list[].
    name = 'LIST_BOX2'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list2[].
    name = 'LIST_BOX3'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list3[].
    name = 'LIST_BOX4'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list4[].

  ENDIF.

  IF gt_pa0001 IS NOT INITIAL.
    IF flag2 = ' '.
      REFRESH:list,list2,list3,list4.
      name = 'LIST_BOX'.
      LOOP AT gt_pa0001 INTO gs_pa0001.
        no = gs_pa0001-pernr."no + 1.
        list-key = no.
        list-text = gs_pa0001-ename.
        APPEND list.
        CLEAR:gs_pa0001,no.
      ENDLOOP.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list[].
      name = 'LIST_BOX2'.
      list2[] = list[].
      CLEAR no.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list2[].
      name = 'LIST_BOX3'.
      CLEAR no.
      list3[] = list[].
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list3[].
      name = 'LIST_BOX4'.
      CLEAR no.
      list4[] = list[].
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list4[].
      flag2 = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_AUTH_LIST

*&---------------------------------------------------------------------*
*&      Form  LINE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_text_create.
  IF gs_tab1-matnr IS NOT INITIAL.
    CLEAR gs_tab1.
    lv_curline  = g_lineno.
    flag = 'X'.
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
    READ TABLE gt_tab1 INTO gs_tab1 INDEX lv_curline.
    IF sy-subrc = 0.
      REFRESH g_mytable.
      text = gs_tab1-text.
      APPEND text TO g_mytable.
      DELETE g_mytable WHERE table_line IS INITIAL.
*        get_text_as_r3table
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
      SET CURSOR LINE g_lineno.
      GET CURSOR FIELD field1.

    ENDIF.
  ELSE.
    MESSAGE text-012 TYPE 'S' DISPLAY LIKE 'E'." 'Please Enter Line Item Details!!!' TYPE 'I'.
  ENDIF.
ENDFORM.                    " LINE_TEXT
*&---------------------------------------------------------------------*
*&      Form  LINE_TEXT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_text_create_1 .
  IF gs_tab1-maktx IS NOT INITIAL.
    CLEAR gs_tab1.
    lv_curline  = g_lineno.
    flag = 'X'.
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
    READ TABLE gt_tab1 INTO gs_tab1 INDEX lv_curline.
    IF sy-subrc = 0.
      REFRESH g_mytable.
      text = gs_tab1-text.
      APPEND text TO g_mytable.
      DELETE g_mytable WHERE table_line IS INITIAL.
*        get_text_as_r3table
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
      SET CURSOR LINE g_lineno.
      GET CURSOR FIELD field1.
    ENDIF.
  ELSE.
    MESSAGE text-012 TYPE 'S' DISPLAY LIKE 'E'." 'Please Enter Line Item Details!!!' TYPE 'I'.
  ENDIF.
ENDFORM.                    " LINE_TEXT1
*&---------------------------------------------------------------------*
*&      Form  SAVE_LINE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_line_text .
  CALL METHOD text_editor->get_textstream
    IMPORTING
      text                   = text
    EXCEPTIONS
      error_cntl_call_method = 1
      not_supported_by_gui   = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.
  CALL METHOD editor_container->free.
  CALL METHOD text_editor->free.

  CLEAR: flag,gs_tab1,g_lineno.
*        GET CURSOR LINE g_lineno.
  READ TABLE gt_tab1 INTO gs_tab1 INDEX lv_curline.
  IF sy-subrc = 0.
    LOOP AT gt_tab1 INTO gs_tab2 .
      gs_tab2-text = text.
      IF sy-tabix = lv_curline.
        MODIFY gt_tab1 FROM gs_tab2 TRANSPORTING text.
      ENDIF.
      CLEAR gs_tab2.
    ENDLOOP.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 = 'PB1'.
      IF screen-name = 'SAVE_TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR : text.
  SET CURSOR LINE lv_curline.
*         SET CURSOR FIELD gs_tab1-comp_data_avl." LINE lv_curline ."OFFSET offset .

ENDFORM.                    " SAVE_LINE_TEXT

*&---------------------------------------------------------------------*
*&      Form  CHECK_MT_VN_BLINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mt_vn_bline .
  REFRESH gt_tab2.
  gt_tab2[] = gt_tab1[].
  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
    CLEAR ls_matnr.
    READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = gs_tab1-matnr.
    IF sy-subrc NE 0.
      MESSAGE i004(zpp) WITH gs_tab1-matnr gv_ekgrp.
      flag3_1 = 'X'.
      flag_final = 'X'.
*      CLEAR gs_tab1.
    ENDIF.

    IF gs_tab1-business_price = 0.
      MESSAGE i001(zpp) WITH gs_tab1-matnr gs_tab1-business_price.
      flag3_1 = 'X'.
      flag_final = 'X'.
*      CLEAR gs_tab1.
    ENDIF.

    LOOP AT gt_tab2 INTO gs_tab2." WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab2-ebelp = lv_index.
        IF gs_tab2-chk = 'X'.
          IF gs_tab2-matnr = lv_matnr.
            MESSAGE i010(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            flag_final = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab2.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHECK_MT_VN_BLINE

*&---------------------------------------------------------------------*
*&      Form  CHECK_VENDOR_101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vendor_101 .

  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  CLEAR: lv_matnr,flag3_3.
  lv_lifnr = gs_tab1-lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_lifnr
    IMPORTING
      output = lv_lifnr.
  REFRESH: lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                            WHERE lifnr = lt_lfb1-lifnr.
    READ TABLE lt_f4_lifnr INTO ls_f4_lifnr WITH KEY lifnr = lv_lifnr.
    IF sy-subrc NE 0.
      MESSAGE i008(zpp) WITH gs_tab1-lifnr gv_werks.
      flag3_3 = 'X'.
      flag_final = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_VENDOR_101

*&---------------------------------------------------------------------*
*&      Form  CHECK_RELEASER_101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_releaser_101 .
  CLEAR :gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4.
  READ TABLE list WITH KEY list_box.
  IF sy-subrc = 0.
    gv_releaser1 = list-key.
    gv_releaser_name1 = list-text.
  ENDIF.
  READ TABLE list2 WITH KEY list_box2.
  IF sy-subrc = 0.
    gv_releaser2 = list2-key.
    gv_releaser_name2 = list2-text.
  ENDIF.
  READ TABLE list3 WITH KEY list_box3.
  IF sy-subrc = 0.
    gv_releaser3 = list3-key.
    gv_releaser_name3 = list3-text.
  ENDIF.
  READ TABLE list4 WITH KEY list_box4.
  IF sy-subrc = 0.
    gv_releaser4 = list4-key.
    gv_releaser_name4 = list4-text.
  ENDIF.


  IF gv_releaser1 IS INITIAL OR  gv_releaser2 IS INITIAL.
    MESSAGE text-016 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ENDIF.
  IF gv_releaser1 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.

  ELSEIF gv_releaser2 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
    .
  ELSEIF gv_releaser4 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ELSEIF gv_releaser4 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    flag3_4 = 'X'.
    EXIT.
  ENDIF.
ENDFORM.                    " CHECK_RELEASER_101

*&---------------------------------------------------------------------*
*&      Form  COMPARE_SEL_NEW_BASIC_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compare_sel_new_basic_price .
  REFRESH : gt_tab2.
  CLEAR flag3_4.
  gt_tab2[] = gt_tab1[].
  LOOP AT gt_tab1 INTO gs_tab1.
    IF gs_tab1-chk = 'X'.
      LOOP AT gt_tab2 INTO gs_tab2 WHERE matnr = gs_tab1-matnr.
        IF gs_tab2-chk NE 'X'.
          IF gs_tab2-netpr LE gs_tab1-netpr.
            IF gs_tab1-text IS INITIAL.
              MESSAGE w015(zpp) WITH gs_tab1-matnr .
              flag3_4 = 'X'.
*              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR gs_tab2.
      ENDLOOP.
    ENDIF.
    CLEAR gs_tab1.
  ENDLOOP.
  IF flag3_4 = 'X'.
    PERFORM screen_edit101.
  ENDIF.
ENDFORM.                    " COMPARE_SEL_NEW_BASIC_PRICE

*&---------------------------------------------------------------------*
*&      Form  SCREEN_EDIT101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_edit101 .
  LOOP AT SCREEN.                                "ravi
    IF gs_tab1-chk = 'X'.
      screen-input = 0.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SCREEN_EDIT101

*&---------------------------------------------------------------------*
*&      Form  SCREEN_EDITTT101
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_edittt101 .
*  LOOP AT SCREEN.
**    screen-active = .
*    MODIFY SCREEN.
*  ENDLOOP.
ENDFORM.                    " SCREEN_EDITTT101


*&---------------------------------------------------------------------*
*&      Form  SAVE_PROPOSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_proposal .

  call function 'DATE_TO_PERIOD_CONVERT'    "fiscial year
  exporting
    i_date               = sy-datum
    i_periv              = 'V3'
 importing
   e_gjahr               = gv_fiscal.

  loop at gt_tab1 into gs_tab1.
    gs_tab1-chk              =   gs_tab1-chk .
    gs_tab1-pr_date          =   gv_date .
    gs_tab1-erdat            =   gv_date .
    gs_tab1-ekgrp            =   gv_ekgrp .
    gs_tab1-eknam            =   gv_eknam.
    gs_tab1-werks            =   gv_werks .
    gs_tab1-bukrs            =   gv_bukrs .
    gs_tab1-plantname        =   gv_plantname .
    gs_tab1-pr_user          =   gv_user.
    gs_tab1-pr_time          =   gv_time.
    gs_tab1-remarks          =   remarks.
    gs_tab1-releaser1        =   gv_releaser1.
    gs_tab1-releaser_name1   =   gv_releaser_name1.
    gs_tab1-releaser2        =   gv_releaser2.
    gs_tab1-releaser_name2   =   gv_releaser_name2.
    gs_tab1-releaser3        =   gv_releaser3.
    gs_tab1-releaser_name3   =   gv_releaser_name3.
    gs_tab1-releaser4        =   gv_releaser4.
    gs_tab1-releaser_name4   =   gv_releaser_name4.
    modify gt_tab1 from gs_tab1 transporting chk pr_date ekgrp bukrs werks pr_time pr_user remarks releaser1
                                             releaser2 releaser3 releaser4 releaser_name1 releaser_name2
                                             releaser_name3 releaser_name4 eknam plantname                                          .
    clear gs_tab1.
  endloop.
  lv_year = sy-datum(04).
  call function 'NUMBER_GET_NEXT'
    exporting
      nr_range_nr             = '01'
      object                  = 'YPPINTNO'
      toyear                  = lv_year
    importing
      number                  = gv_prno
    exceptions
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      others                  = 8.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
     with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  if gv_prno is not initial.
    loop at gt_tab1 into gs_tab1.
      gs_zmm_pur_proposal-app_vendor         =   gs_tab1-chk.
      gs_zmm_pur_proposal-prno               =   gv_prno.
      gs_zmm_pur_proposal-gjahr              =   gv_fiscal.
      gs_zmm_pur_proposal-pr_date            =   gs_tab1-pr_date .
      gs_zmm_pur_proposal-ekgrp              =   gs_tab1-ekgrp.
      gs_zmm_pur_proposal-eknam              =   gs_tab1-eknam.
      gs_zmm_pur_proposal-bukrs              =   gs_tab1-bukrs.
      gs_zmm_pur_proposal-werks              =   gs_tab1-werks.
      gs_zmm_pur_proposal-plantname          =   gs_tab1-plantname.
      gs_zmm_pur_proposal-erdat              =   gs_tab1-erdat.
      gs_zmm_pur_proposal-pr_itemno          =   gs_tab1-ebelp * 10.
*{   REPLACE        SBXK900030                                        1
*\      call function 'CONVERSION_EXIT_ALPHA_INPUT'
*\        exporting
*\          input  = gs_tab1-matnr
*\        importing
*\          output = gs_tab1-matnr.
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
          input  = gs_tab1-matnr
        IMPORTING
          output = gs_tab1-matnr.

*}   REPLACE
      IF gs_zmm_pur_proposal-ekgrp NOT BETWEEN 'B05' AND 'B06'.
        gs_zmm_pur_proposal-matnr              =   gs_tab1-matnr.
      endif.
      gs_zmm_pur_proposal-maktx              =   gs_tab1-maktx.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = gs_tab1-lifnr
        importing
          output = gs_tab1-lifnr.
      gs_zmm_pur_proposal-lifnr              =   gs_tab1-lifnr.
      gs_zmm_pur_proposal-name1              =   gs_tab1-name1.
      gs_zmm_pur_proposal-menge              =   gs_tab1-qty.
      gs_zmm_pur_proposal-meins              =   gs_tab1-meins.
      gs_zmm_pur_proposal-netpr              =   gs_tab1-netpr.
      gs_zmm_pur_proposal-waers              =   gs_tab1-waers.
      gs_zmm_pur_proposal-mwskz              =   gs_tab1-mwskz.
      gs_zmm_pur_proposal-tax_desc           =   gs_tab1-tax_desc.
      gs_zmm_pur_proposal-land_pr            =   gs_tab1-landed_price.
      gs_zmm_pur_proposal-land_value         =   gs_tab1-landed_value.
      gs_zmm_pur_proposal-last_basicpr       =   gs_tab1-last_pur_basic_price.
      gs_zmm_pur_proposal-last_landpr        =   gs_tab1-last_pur_land_price.
      gs_zmm_pur_proposal-business_planpr    =   gs_tab1-business_price.
      gs_zmm_pur_proposal-vari_last_pur_in   =   gs_tab1-vari_last_pur_inr.
      gs_zmm_pur_proposal-vari_last_pur_va   =   gs_tab1-vari_last_pur_val.
      gs_zmm_pur_proposal-vari_bus_pl_inr    =   gs_tab1-vari_business_plan_inr.
      gs_zmm_pur_proposal-vari_bus_pl_val    =   gs_tab1-vari_business_plan_val.
      gs_zmm_pur_proposal-opening_m1         =   gs_tab1-op_m1.
      gs_zmm_pur_proposal-requirment_m1      =   gs_tab1-req_m1.
      gs_zmm_pur_proposal-app_vend_m1        =   gs_tab1-app_vend_m1.
      gs_zmm_pur_proposal-other_vend_m1      =   gs_tab1-otr_vend_m1.
      gs_zmm_pur_proposal-closing_m1         =   gs_tab1-clos_m1.
      gs_zmm_pur_proposal-opening_m2         =   gs_tab1-op_m2.
      gs_zmm_pur_proposal-requirment_m2      =   gs_tab1-req_m2.
      gs_zmm_pur_proposal-app_vend_m2        =   gs_tab1-app_vend_m2.
      gs_zmm_pur_proposal-other_vend_m2      =   gs_tab1-otr_vend_m2.
      gs_zmm_pur_proposal-closing_m2         =   gs_tab1-clos_m2.
      gs_zmm_pur_proposal-opening_m3         =   gs_tab1-op_m3.
      gs_zmm_pur_proposal-requirment_m3      =   gs_tab1-req_m3.
      gs_zmm_pur_proposal-app_vend_m3        =   gs_tab1-app_vend_m3.
      gs_zmm_pur_proposal-other_vend_m3      =   gs_tab1-otr_vend_m3.
      gs_zmm_pur_proposal-closing_m3         =   gs_tab1-clos_m3.
      gs_zmm_pur_proposal-comp_data          =   gs_tab1-costformula.
      gs_zmm_pur_proposal-comp_data1         =   gs_tab1-comp_data_avl.
      gs_zmm_pur_proposal-line_txt           =   gs_tab1-text.
      gs_zmm_pur_proposal-remark             =   gs_tab1-remarks.
      gs_zmm_pur_proposal-releaser1          =   gs_tab1-releaser1.
      gv_pernr                               =   gs_tab1-releaser1.
      gs_zmm_pur_proposal-releaser_name1     =   gs_tab1-releaser_name1.
      gs_zmm_pur_proposal-releaser2          =   gs_tab1-releaser2.
      gs_zmm_pur_proposal-releaser_name2     =   gs_tab1-releaser_name2.
      gs_zmm_pur_proposal-releaser3          =   gs_tab1-releaser3.
      gs_zmm_pur_proposal-releaser_name3     =   gs_tab1-releaser_name3.
      gs_zmm_pur_proposal-releaser4          =   gs_tab1-releaser4.
      gs_zmm_pur_proposal-releaser_name4     =   gs_tab1-releaser_name4.
      gs_zmm_pur_proposal-erdat              =   gs_tab1-pr_date .
      gs_zmm_pur_proposal-pr_user            =   gs_tab1-pr_user.
      gv_user                                =   gs_tab1-pr_user.
      gs_zmm_pur_proposal-pr_time            =                gs_tab1-pr_time.
*                                           = gs_tab1-flag.
      append gs_zmm_pur_proposal to gt_zmm_pur_proposal.
      clear: gs_zmm_pur_proposal,gs_tab1.
    endloop.

    modify zmm_pur_proposal from table gt_zmm_pur_proposal.
    commit work.

    perform email_send_101.
*    wait UP TO 3 SECONDS.
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = gv_prno
      importing
        output = gv_prno.
    message s002(zpp) with gv_prno." DISPLAY LIKE 'S'.
    wait up to 3 seconds.
    clear gv_ans.
    lv_msg = 'Create Attachment'.
    call function 'POPUP_TO_CONFIRM'
       exporting
         titlebar                     = lv_msg
         text_question               = 'Do you want to Create an attachment?'
         text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
         text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
         default_button               = '1'
         display_cancel_button        = ' '
         start_column                 = 75
         start_row                    = 5
         importing
           answer                       = gv_ans                 .
    if gv_ans = 1.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = gv_prno
        importing
          output = gv_prno.
      perform refresh.
      call screen 500.
      leave program.
    else.
      leave program.
    endif.
  endif.
ENDFORM.                    " SAVE_PROPOSAL
*&---------------------------------------------------------------------*
*&      Form  EMAIL_SEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EMAIL_SEND_101.
 DATA :  lv_userid TYPE pa0105-usrid_long.
  DATA: tab_lines LIKE sy-tabix,
        lv_desc   TYPE string,
        lv_p1     TYPE string,
        dd(2)     TYPE c,
        mm(2)     TYPE c,
        yy(4)     TYPE c,
        lv_date(10) type c,
        lv_ename    TYPE pa0001-ename,
        lv_vend_name TYPE lfa1-name1.
  CLEAR : lv_userid,w_document_data,doc_chng,objpack,objhead,zreceivers,i_body_msg,
          lv_desc,lv_p1,lv_date,lv_ename,lv_lifnr,lv_vend_name.

  SELECT SINGLE ename FROM pa0001 INTO lv_ename WHERE pernr = sy-uname.

  READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal with key app_vendor = 'X'.
  IF sy-subrc = 0.
  lv_lifnr = gs_zmm_pur_proposal-lifnr.
  lv_vend_name = gs_zmm_pur_proposal-name1.
  ENDIF.
  SELECT SINGLE usrid_long INTO lv_userid
                               FROM pa0105
                               WHERE pernr = gv_pernr "SY-UNAME
                               AND endda GE sy-datum "
                               AND subty = '0010'.
  IF sy-subrc = 0.
    TRANSLATE lv_userid TO LOWER CASE.
    zreceivers-receiver = lv_userid.
    zreceivers-rec_type = 'U'.
    APPEND zreceivers.
    CONCATENATE 'Proposal No' gv_prno 'Waiting for your Release.' INTO lv_desc SEPARATED BY SPACE.
    doc_chng-obj_name = 'SENDMAIL'.
    doc_chng-obj_descr = lv_desc.
    doc_chng-obj_langu = sy-langu.
    doc_chng-sensitivty = 'F'.
    dd = sy-datum+6(2).
    mm = sy-datum+4(2).
    yy = sy-datum+0(4).
    CONCATENATE dd '.' mm '.' yy INTO lv_date.
     CONCATENATE 'Proposal Number' gv_prno ', dated:' lv_date
                                           '- is Waiting for your L1 Release.'
                                             INTO lv_p1 SEPARATED BY SPACE.
*    CONCATENATE 'Proposal Number' gv_prno ', dated:' lv_date 'on Vendor:' lv_vend_name '(' lv_lifnr ')'
*                                           '- is Waiting for your Release.'
*                                             INTO lv_p1 SEPARATED BY SPACE.
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
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh .

  REFRESH:gt_tab1,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
          list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal.

  CLEAR: gs_tab1,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab2,gs_stxh,gs_a516, gs_pa0001,
         gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
         flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
         no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
         gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
         gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs.
ENDFORM.                    " REFRESH

*&---------------------------------------------------------------------*
*&      Form  EMAIL_SEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM email_send_104 .
  DATA :  lv_userid TYPE pa0105-usrid_long.
  DATA: tab_lines LIKE sy-tabix,
        lv_desc   TYPE string,
        lv_p1     TYPE string,
        dd(2)     TYPE c,
        mm(2)     TYPE c,
        yy(4)     TYPE c,
        lv_date(10) type c,
        lv_ename    TYPE pa0001-ename,
        lv_vend_name TYPE lfa1-name1.
  CLEAR : lv_userid,w_document_data,doc_chng,objpack,objhead,zreceivers,i_body_msg,
          lv_desc,lv_p1,lv_date,lv_ename,lv_lifnr,lv_vend_name.

  select single ename from pa0001 into lv_ename where pernr = sy-uname.
  select single usrid_long into lv_userid
                               from pa0105
                               where pernr = gv_pernr "SY-UNAME
                               and endda ge sy-datum "
                               and subty = '0010'.
  if sy-subrc = 0.
    translate lv_userid to lower case.
    zreceivers-receiver = lv_userid.
    zreceivers-rec_type = 'U'.
    append zreceivers.
    CONCATENATE 'Proposal No' gv_newprno 'Waiting for your Release.' INTO lv_desc SEPARATED BY SPACE.
    doc_chng-obj_name = 'SENDMAIL'.
    doc_chng-obj_descr = lv_desc.
    doc_chng-obj_langu = sy-langu.
    doc_chng-sensitivty = 'F'.
    dd = sy-datum+6(2).
    mm = sy-datum+4(2).
    yy = sy-datum+0(4).
    CONCATENATE dd '.' mm '.' yy INTO lv_date.
     CONCATENATE 'Proposal Number' gv_newprno ',has been created with Reference to old Proposal No' gv_prno
                                       'on dated:' lv_date
                                           '- is Waiting for your L1 Release.'
                                             INTO lv_p1 SEPARATED BY SPACE.
*    CONCATENATE 'Proposal Number' gv_prno ', dated:' lv_date 'on Vendor:' lv_vend_name '(' lv_lifnr ')'
*                                           '- is Waiting for your Release.'
*                                             INTO lv_p1 SEPARATED BY SPACE.
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


*&---------------------------------------------------------------------*
*&      Form  build_body_of_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_MESSAGE  text
*----------------------------------------------------------------------*
FORM build_body_of_mail  USING l_message.
  w_body_msg = l_message.
  APPEND w_body_msg TO i_body_msg.
  CLEAR  w_body_msg.

ENDFORM.                    "BUILD_BODY_OF_MAIL

*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0104 OUTPUT.
  SET PF-STATUS 'ZPF104'.
  SET TITLEBAR 'ZTIT104'.

  CASE sy-ucomm.
  when 'BACK'.
    refresh:gt_tab4,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
         list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal,g_mytable.
      clear: gs_tab44,gs_tab2,gs_tab4,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab3,gs_stxh,gs_a516, gs_pa0001,
             gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
             flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
             no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
             gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
             gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs,gv_plantname,gv_eknam,gv_date,g_mytable,gv_user,gv_time,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,remarks,old_remarks,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,
             list,list2,list3,list4,list_box,list_box2,list_box3,list_box4.

      call method cl_gui_cfw=>flush
        exceptions
          cntl_system_error = 1
          cntl_error        = 2
          others            = 3.
      call method remarks_container->free.
      call method remarks_editor->free.
      if editor_container is not initial ."AND text_editor IS NOT INITIAL.
       call method cl_gui_cfw=>flush
        exceptions
          cntl_system_error = 1
          cntl_error        = 2
          others            = 3.
*      CALL METHOD editor_container->free.
      call method text_editor->free.
      endif.
*      CALL METHOD editor_container->free.
*      CALL METHOD text_editor->free.

      name = 'LIST_BOX'.
      call function 'VRM_DELETE_VALUES'
        exporting
          id           = name
        exceptions
          id_not_found = 1
          others       = 2.
      call function 'VRM_SET_VALUES'
        exporting
          id     = name
          values = list[].
      name = 'LIST_BOX2'.
      call function 'VRM_DELETE_VALUES'
        exporting
          id           = name
        exceptions
          id_not_found = 1
          others       = 2.
      call function 'VRM_SET_VALUES'
        exporting
          id     = name
          values = list2[].
      name = 'LIST_BOX3'.
      call function 'VRM_DELETE_VALUES'
        exporting
          id           = name
        exceptions
          id_not_found = 1
          others       = 2.
      call function 'VRM_SET_VALUES'
        exporting
          id     = name
          values = list3[].
      name = 'LIST_BOX4'.
      call function 'VRM_DELETE_VALUES'
        exporting
          id           = name
        exceptions
          id_not_found = 1
          others       = 2.
      call function 'VRM_SET_VALUES'
        exporting
          id     = name
          values = list4[].
      leave to screen 100.
    when 'EXIT'.
      leave program.
    when 'CANCEL'.
      leave program.

  ENDCASE.

  IF flag1 EQ ' '.
    CREATE OBJECT remarks_container
      EXPORTING
        container_name              = 'OTH_REMARKS'     "custom container name
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
    flag1 = 'X'.
  ENDIF.
  CONTROLS: tc4 TYPE TABLEVIEW USING SCREEN 0104.

  LOOP AT tc4-cols INTO cols WHERE index GT 0.
    IF  cols-screen-group1 = 'MT1'
         AND cols-screen-input = '0'.
      cols-screen-input = '1'."'1'.
    ELSEIF cols-screen-group1 = 'MT1'
       AND cols-screen-input = '1'.
      cols-screen-input = '0'.
    ENDIF.
    MODIFY tc4-cols FROM cols INDEX sy-tabix.
  ENDLOOP.

  IF flag9 = 'X'.
    REFRESH:list,list2,list3,list4.
    CLEAR :list_box,list_box2,list_box3,list_box4,gs_tab22,lv_key3,lv_key4,flag9.
    READ TABLE gt_tab4 INTO gs_tab44 INDEX 1.
    IF sy-subrc = 0.
      list-key   = gs_tab44-releaser1.
      list-text  = gs_tab44-releaser_name1.
      list2-key   = gs_tab44-releaser2.
      list2-text  = gs_tab44-releaser_name2.
      list3-key   = gs_tab44-releaser3.
      list3-text  = gs_tab44-releaser_name3.
      list4-key   = gs_tab44-releaser4.
      list4-text  = gs_tab44-releaser_name4.
      APPEND list.
      APPEND list2.
      APPEND list3.
      APPEND list4.
      name = 'LIST_BOX'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list[].
      list_box = list-key.
      name = 'LIST_BOX2'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list2[].
      list_box2 = list2-key.
      name = 'LIST_BOX3'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list3[].
      lv_key3 = list3-key.
      SHIFT list3-key LEFT DELETING LEADING '0'.
      IF list3-key IS NOT INITIAL.
        list_box3 = lv_key3.
      ENDIF.
      name = 'LIST_BOX4'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list4[].
      lv_key4 = list4-key.
      SHIFT list4-key LEFT DELETING LEADING '0'.
      IF list4-key IS NOT INITIAL.
        list_box4 = lv_key4.
      ENDIF.
    ENDIF.
    CLEAR gs_tab44.
  ENDIF.

ENDMODULE.                 " STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL_GVPRNO104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl_gvprno104 INPUT.
  TYPES: BEGIN OF ty_f4_prno,
          prno TYPE zmm_pur_proposal-prno,
         END OF ty_f4_prno.
  DATA: lt_f4_prno TYPE TABLE OF ty_f4_prno.

  SELECT prno FROM zmm_pur_proposal INTO TABLE lt_f4_prno.
  SORT lt_f4_prno BY prno.
  DELETE ADJACENT DUPLICATES FROM lt_f4_prno COMPARING prno.
  REFRESH it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'GV_PRNO'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0104'
     dynprofield = 'GV_PRNO'
     TABLES
     value_tab   = lt_f4_prno
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
    gv_prno = it_ret-fieldval.
  ENDIF.
ENDMODULE.                 " F4_TCL_GVPRNO104  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_EKGRP104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_ekgrp104 INPUT.
  CLEAR: it_ret,lt_f4_pgrp.

  SELECT ekgrp eknam FROM t024 INTO TABLE lt_f4_pgrp  WHERE ekgrp IN ('B01','B02','B03','B04','B05','B06').

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'EKGRP'
      value_org       = 'S'
*     dynpprog        = ''
      dynpnr          = '0104'
      dynprofield     = 'GV_EKGRP'
     TABLES
      value_tab       = lt_f4_pgrp
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gv_ekgrp = it_ret-fieldval.
    SELECT SINGLE eknam FROM t024 INTO gv_eknam WHERE ekgrp = gv_ekgrp.
    CLEAR: it_ret,lt_f4_pgrp.
  ENDIF.

ENDMODULE.                 " F4_EKGRP104  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_WERKS104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_werks104 INPUT.
  CLEAR: it_ret,lt_f4_werks.
  SELECT werks name1 FROM t001w INTO TABLE lt_f4_werks .

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'WERKS'
      value_org       = 'S'
*     dynpprog        = ''
      dynpnr          = '0104'
      dynprofield     = 'GV_WERKS'
     TABLES
      value_tab       = lt_f4_werks
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gv_werks = it_ret-fieldval.
    SELECT SINGLE name1 FROM t001w INTO gv_plantname WHERE werks = gv_werks.
    CLEAR: it_ret,lt_f4_werks.
  ENDIF.

ENDMODULE.                 " F4_WERKS104  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_MATNR104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_matnr104 INPUT.
  CLEAR: it_ret,lt_mara,ls_mara.
  REFRESH: lt_f4_matnr,lt_f4_matnr1.

  IF gv_ekgrp IS NOT INITIAL.
    SELECT matnr FROM marc INTO TABLE lt_f4_matnr WHERE ekgrp = gv_ekgrp." AND WERKS = GV_WERKS.
    IF lt_f4_matnr IS NOT INITIAL.
      SELECT matnr maktx FROM makt INTO TABLE lt_f4_matnr1 FOR ALL ENTRIES IN lt_f4_matnr WHERE matnr = lt_f4_matnr-matnr AND spras = 'E'..
    ENDIF.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'MATNR'
        value_org       = 'S'
*     dynpprog        = ''
        dynpnr          = '0104'
        dynprofield     = 'GS_TAB4-MATNR'
       TABLES
        value_tab       = lt_f4_matnr1
        return_tab      = it_ret
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3.
    ELSE.
      READ TABLE it_ret INDEX 1.
      gs_tab4-matnr = it_ret-fieldval.
      CLEAR: it_ret,lt_f4_matnr.
      REFRESH: lt_f4_matnr,lt_f4_matnr1.
    ENDIF.

  ELSE.
    MESSAGE text-011 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
  ENDIF.
ENDMODULE.                 " F4_MATNR104  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'ZPF500'.
  SET TITLEBAR 'ZTIT500'.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR :gv_prno,gv_itemno,list_box6.
      REFRESH:list6,list_final.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      CLEAR :gv_prno,gv_itemno,list_box6.
      REFRESH:list6,list_final.
      LEAVE PROGRAM.
  ENDCASE.

  REFRESH: lt_f4_itemno,it_ret,list5,list_final.
  SELECT *
         FROM zmm_proposal_doc INTO TABLE lt_f4_itemno WHERE prno = gv_prno.
  IF sy-subrc = 0..
    LOOP AT lt_f4_itemno INTO ls_f4_itemno.
      list5-key = ls_f4_itemno-pr_itemno / 10.
      list_final-key = ls_f4_itemno-pr_itemno .
      APPEND list5.
      APPEND list_final.
      CLEAR ls_f4_itemno.
    ENDLOOP.
    name = 'LIST_BOX5'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list5[].
*      EXIT.
  ENDIF.
ENDMODULE.                 " STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  CLEAR gv_okcode.
  gv_okcode = sy-ucomm.
  CASE gv_okcode.
    WHEN ' '.
      REFRESH: lt_f4_itemno,it_ret,list5,list_final.
      SELECT *
             FROM zmm_proposal_doc INTO TABLE lt_f4_itemno WHERE prno = gv_prno.
      IF sy-subrc = 0..
        LOOP AT lt_f4_itemno INTO ls_f4_itemno.
          list5-key = ls_f4_itemno-pr_itemno / 10.
          list_final-key = ls_f4_itemno-pr_itemno .
          APPEND list5.
          APPEND list_final.
          CLEAR ls_f4_itemno.
        ENDLOOP.
        name = 'LIST_BOX5'.
        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id     = name
            values = list5[].
*      EXIT.
      ENDIF.
    WHEN 'ATCH'.
      PERFORM create_attachment.
      CLEAR sy-ucomm.
    WHEN 'DELETE'.
      PERFORM delete_attachment.
    WHEN 'VIEW'.
      PERFORM view_attachment.
      CLEAR sy-ucomm.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0500  INPUT

*&---------------------------------------------------------------------*
*&      Form  CREATE_ATTACHMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_attachment .

  l_objkey = ls_srgbtbrel-instid_b. "i.e. the Note's ID

  DATA : lt_zmm_proposal_doc TYPE TABLE OF zmm_proposal_doc,
         ls_zmm_proposal_doc TYPE zmm_proposal_doc,
         n_lines TYPE i.
  CREATE OBJECT lo_gos_service.
  CALL METHOD lo_gos_service->create_attachment
    EXPORTING
      is_object     = is_object
    IMPORTING
      ep_attachment = l_objkey.

  REFRESH gt_zmm_pur_proposal.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gv_prno
    IMPORTING
      output = gv_prno.

  REFRESH lt_zmm_proposal_doc.
  SELECT * FROM zmm_proposal_doc INTO TABLE lt_zmm_proposal_doc
                                WHERE prno = gv_prno." AND pr_itemno = gv_itemno.
  IF lt_zmm_proposal_doc IS NOT INITIAL.
    SORT lt_zmm_proposal_doc BY pr_itemno DESCENDING.
    READ TABLE lt_zmm_proposal_doc INTO ls_zmm_proposal_doc INDEX 1.
    IF ls_zmm_proposal_doc-prno IS INITIAL.
      ls_zmm_proposal_doc-pr_itemno = ls_zmm_proposal_doc-pr_itemno + 10."gv_itemno.
    ELSE.
      ls_zmm_proposal_doc-pr_itemno = ls_zmm_proposal_doc-pr_itemno + 10.
    ENDIF.

    ls_zmm_proposal_doc-prno = gv_prno.
    ls_zmm_proposal_doc-objkey = l_objkey.
    CLEAR n_lines.
    DESCRIBE TABLE lt_zmm_proposal_doc LINES n_lines.
    IF n_lines = 5.
      MESSAGE text-020 TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE. "IF ls_zmm_proposal_doc-objkey IS NOT INITIAL.
      APPEND ls_zmm_proposal_doc TO lt_zmm_proposal_doc.
*         ENDIF.
    ENDIF.
    CLEAR ls_zmm_proposal_doc.
  ELSE.
    CLEAR ls_zmm_proposal_doc.
    ls_zmm_proposal_doc-prno = gv_prno.
    ls_zmm_proposal_doc-pr_itemno = 10."gv_itemno.
    ls_zmm_proposal_doc-objkey = l_objkey.
    IF ls_zmm_proposal_doc-objkey IS NOT INITIAL.
      APPEND ls_zmm_proposal_doc TO lt_zmm_proposal_doc.
    ENDIF.
  ENDIF.
*    move l_objkey to ZMM_PUR_PROPOSAL-objkey.
  IF lt_zmm_proposal_doc IS NOT INITIAL.
    MODIFY zmm_proposal_doc FROM TABLE lt_zmm_proposal_doc."TRANSPORTING prno objkey .
    COMMIT WORK.
    IF sy-subrc = 0.
      MESSAGE s005(zpp) WITH gv_prno.
    ENDIF.
  ELSE.
    MESSAGE i009(zpp) WITH gv_prno DISPLAY LIKE 'E'.
  ENDIF.
*  ENDIF.
  CLEAR list_box5.
ENDFORM.                    " CREATE_ATTACHMENT

*&---------------------------------------------------------------------*
*&      Form  DELETE_ATTACHMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM delete_attachment . "500

  DATA: lt_zmm_proposal_doc TYPE TABLE OF zmm_proposal_doc,
        ls_zmm_proposal_doc TYPE zmm_proposal_doc.
  CLEAR: gv_itemno,ls_zmm_proposal_doc,gv_ans,lv_msg,lv_list_boxx.
  REFRESH: lt_f4_itemno,it_ret.
  gv_itemno = list_box5 * 10.
  MOVE list_box5 TO lv_list_boxx.
  lv_list_boxx = lv_list_boxx * 10.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_list_boxx
    IMPORTING
      output = lv_list_boxx.
*  READ TABLE list5 WITH KEY list_box5.
  READ TABLE list_final WITH KEY lv_list_boxx.
  IF sy-subrc = 0.
    CREATE OBJECT lo_gos_service.
*    gv_itemno = list5-key * 10.
    SELECT SINGLE * FROM zmm_proposal_doc INTO ls_zmm_proposal_doc
                    WHERE prno = gv_prno AND pr_itemno = gv_itemno.
    IF ls_zmm_proposal_doc-prno IS NOT INITIAL.
      lv_msg = 'Delete Attachment'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
         titlebar                     = lv_msg
          text_question               = 'Do you want to Delete Attachment for Selected Line?'
         text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
         text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
         default_button               = '1'
         display_cancel_button        = ' '
         start_column                 = 55
         start_row                    = 3
       IMPORTING
         answer                       = gv_ans                 .
      IF gv_ans = 1.
        CALL METHOD lo_gos_service->delete_attachment
         EXPORTING
*              is_object     = is_object
        ip_attachment = ls_zmm_proposal_doc-objkey.
        DELETE FROM zmm_proposal_doc WHERE prno = ls_zmm_proposal_doc-prno
                                   AND pr_itemno = ls_zmm_proposal_doc-pr_itemno.
        COMMIT WORK.
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE i007(zpp) WITH gv_prno DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    MESSAGE i007(zpp) WITH gv_prno DISPLAY LIKE 'E'.
  ENDIF.
  CLEAR: sy-ucomm,list_box5,gv_itemno.
ENDFORM.                    " DELETE_ATTACHMENT
*&---------------------------------------------------------------------*
*&      Form  view_attachment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM view_attachment .

  TABLES :zmm_proposal_doc.
  DATA:lv_objkey TYPE zmm_proposal_doc-objkey.
  DATA: lt_zmm_proposal_doc TYPE TABLE OF zmm_proposal_doc,
        ls_zmm_proposal_doc TYPE zmm_proposal_doc.

  CLEAR : ls_zmm_proposal_doc,gv_itemno,lv_list_boxx.
  REFRESH : lt_zmm_proposal_doc.
*   lv_list_boxx = list_box5. "* 10.
  MOVE list_box5 TO lv_list_boxx.
  lv_list_boxx = lv_list_boxx * 10.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_list_boxx
    IMPORTING
      output = lv_list_boxx.

  CREATE OBJECT lo_gos_service.
*  READ TABLE list5 WITH KEY list_box5.
  READ TABLE list_final WITH KEY lv_list_boxx."list_box5.
  IF sy-subrc = 0.
    gv_itemno = list_box5 * 10.
  ENDIF.

  IF gv_itemno IS INITIAL.
    MESSAGE i012(zpp) WITH list_box5 DISPLAY LIKE 'E'.
  ELSE.
    SELECT * FROM zmm_proposal_doc INTO TABLE lt_zmm_proposal_doc WHERE prno = gv_prno AND pr_itemno = gv_itemno..
    SORT lt_zmm_proposal_doc BY objkey.
    DELETE lt_zmm_proposal_doc WHERE objkey EQ ' '.
    SORT lt_zmm_proposal_doc BY objkey DESCENDING.
    READ TABLE lt_zmm_proposal_doc INTO ls_zmm_proposal_doc WITH KEY prno = gv_prno pr_itemno = gv_itemno.
    IF sy-subrc = 0.
      CALL METHOD lo_gos_service->display_attachment
        EXPORTING
          is_object     = is_object
          ip_attachment = ls_zmm_proposal_doc-objkey.
    ELSE.
      MESSAGE i007(zpp) WITH gv_prno DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
  CLEAR list_box5.
ENDFORM.                    " VIEW_ATTACHMENT


*&---------------------------------------------------------------------*
*&      Module  STATUS_0501  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0501 OUTPUT.
  SET PF-STATUS 'ZPF501'.
  SET TITLEBAR 'ZTIT501'.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR :gv_prno,gv_itemno,list_box6.
      REFRESH list6.
      LEAVE TO SCREEN 100.
    WHEN 'EXIT'.
      CLEAR :gv_prno,gv_itemno,list_box6.
      REFRESH list6.
      LEAVE PROGRAM.
  ENDCASE.
  CLEAR gv_itemno.
ENDMODULE.                 " STATUS_0501  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0501  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0501 INPUT.            """" view attachment
  CASE sy-ucomm.
    WHEN ' '.
      REFRESH: lt_f4_itemno,list6,list_final.
      SELECT *
           FROM zmm_proposal_doc INTO TABLE lt_f4_itemno WHERE prno = gv_prno.
      IF lt_f4_itemno IS NOT INITIAL.
        LOOP AT lt_f4_itemno INTO ls_f4_itemno.
          list6-key = ls_f4_itemno-pr_itemno / 10.
          list_final-key = ls_f4_itemno-pr_itemno.
          APPEND list6.
          APPEND list_final.
          CLEAR ls_f4_itemno.
        ENDLOOP.
        name = 'LIST_BOX6'.
        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id     = name
            values = list6[].
      ELSE.
        MESSAGE 'No Data Found ' TYPE 'I' DISPLAY LIKE 'E'..
        EXIT.
      ENDIF.
    WHEN 'VIEW'.
      PERFORM view_attachment_501.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0501  INPUT


*&---------------------------------------------------------------------*
*&      Form  VIEW_ATTACHMENT_501
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM view_attachment_501 .

  DATA:lv_objkey TYPE zmm_proposal_doc-objkey.
  DATA: lt_zmm_proposal_doc TYPE TABLE OF zmm_proposal_doc,
        ls_zmm_proposal_doc TYPE zmm_proposal_doc.

  CREATE OBJECT lo_gos_service.
  REFRESH: lt_zmm_proposal_doc.
  CLEAR : ls_zmm_proposal_doc,gv_itemno,lv_list_boxx.
  lv_list_boxx = list_box6 * 10.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_list_boxx
    IMPORTING
      output = lv_list_boxx.

  READ TABLE list_final WITH KEY lv_list_boxx."list_box6.
  IF sy-subrc = 0.
    gv_itemno = list6-key * 10.
  ENDIF.

  IF gv_itemno IS INITIAL.
    MESSAGE i012(zpp) WITH list6-key DISPLAY LIKE 'E'.
  ELSE.
    SELECT * FROM zmm_proposal_doc INTO TABLE lt_zmm_proposal_doc WHERE prno = gv_prno AND pr_itemno = gv_itemno..
    SORT lt_zmm_proposal_doc BY objkey.
    DELETE lt_zmm_proposal_doc WHERE objkey EQ ' '.
    SORT lt_zmm_proposal_doc BY objkey DESCENDING.
    READ TABLE lt_zmm_proposal_doc INTO ls_zmm_proposal_doc WITH KEY prno = gv_prno pr_itemno = gv_itemno.
    IF sy-subrc = 0.
      CALL METHOD lo_gos_service->display_attachment
        EXPORTING
          is_object     = is_object
          ip_attachment = ls_zmm_proposal_doc-objkey.
    ELSE.
      MESSAGE i007(zpp) WITH gv_prno DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
  CLEAR list_box6.
ENDFORM.                    " VIEW_ATTACHMENT_501

*&---------------------------------------------------------------------*
*&      Module  STATUS_0502  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0502 OUTPUT.
  SET PF-STATUS 'ZPF502'.
  SET TITLEBAR 'ZTIT502'.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR :gv_prno,gv_itemno,list_box6.
      REFRESH:list6.
      LEAVE TO SCREEN 0100.
    WHEN 'EXIT'.
      CLEAR :gv_prno,gv_itemno,list_box6.
      REFRESH:list6.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      CLEAR :gv_prno,gv_itemno,list_box6.
      REFRESH:list6.
      LEAVE PROGRAM.
  ENDCASE.

  REFRESH: lt_f4_itemno,it_ret,list5.
  SELECT *
         FROM zmm_proposal_doc INTO TABLE lt_f4_itemno WHERE prno = gv_prno.
  IF sy-subrc = 0..
    LOOP AT lt_f4_itemno INTO ls_f4_itemno.
      list5-key = ls_f4_itemno-pr_itemno / 10.
      APPEND list5.
      CLEAR ls_f4_itemno.
    ENDLOOP.
    name = 'LIST_BOX5'.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = name
        values = list5[].
*      EXIT.
  ENDIF.
ENDMODULE.                 " STATUS_0502  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL_GVPRNO502  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl_gvprno502 INPUT.

  SELECT prno FROM zmm_pur_proposal INTO TABLE lt_f4_prno WHERE pr_user = sy-uname.
  SORT lt_f4_prno BY prno.
  DELETE ADJACENT DUPLICATES FROM lt_f4_prno COMPARING prno.
  REFRESH it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'GV_PRNO'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0502'
     dynprofield = 'GV_PRNO'
     TABLES
     value_tab   = lt_f4_prno
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
    gv_prno = it_ret-fieldval.
  ENDIF.
ENDMODULE.                 " F4_TCL_GVPRNO502  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0502  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0502 INPUT.
  CLEAR gv_okcode.
  gv_okcode = sy-ucomm.

  CASE gv_okcode.

    WHEN ' '.
      REFRESH: lt_f4_itemno,it_ret,list5,list_final.
      CLEAR: name,list_box5,list5,list_final.
      SELECT *
             FROM zmm_proposal_doc INTO TABLE lt_f4_itemno WHERE prno = gv_prno.
      .
      IF sy-subrc = 0.
        LOOP AT lt_f4_itemno INTO ls_f4_itemno.
          list5-key = ls_f4_itemno-pr_itemno / 10.
          list_final-key = ls_f4_itemno-pr_itemno.
          APPEND list5.
          APPEND list_final.
          CLEAR ls_f4_itemno.
        ENDLOOP.
        name = 'LIST_BOX5'.
        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id     = name
            values = list5[].
      ELSE.
        name = 'LIST_BOX5'.
        CALL FUNCTION 'VRM_DELETE_VALUES'
          EXPORTING
            id           = name
          EXCEPTIONS
            id_not_found = 1
            OTHERS       = 2.
        name = 'LIST_BOX5'.
        CALL FUNCTION 'VRM_SET_VALUES'
          EXPORTING
            id     = name
            values = list5[].
      ENDIF.

    WHEN 'ATCH'.
      PERFORM create_attachment_502.
      CLEAR sy-ucomm.
    WHEN 'DELETE'.
      PERFORM delete_attachment_502.
    WHEN 'VIEW'.
      PERFORM view_attachment.
      CLEAR sy-ucomm.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0502  INPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_ATTACHMENT_502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_attachment_502 .
  l_objkey = ls_srgbtbrel-instid_b. "i.e. the Note's ID

  DATA :lv_pruser  TYPE zmm_pur_proposal-pr_user,
        n_lines    TYPE i.
  DATA : lt_zmm_proposal_doc TYPE TABLE OF zmm_proposal_doc,
         ls_zmm_proposal_doc TYPE zmm_proposal_doc.

  SELECT SINGLE pr_user FROM zmm_pur_proposal INTO lv_pruser WHERE prno = gv_prno.
  IF lv_pruser NE sy-uname.
    MESSAGE i014(zpp) WITH lv_pruser DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    CREATE OBJECT lo_gos_service.
    CALL METHOD lo_gos_service->create_attachment
      EXPORTING
        is_object     = is_object
      IMPORTING
        ep_attachment = l_objkey.
    CLEAR gv_itemno.
    REFRESH gt_zmm_pur_proposal.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gv_prno
      IMPORTING
        output = gv_prno.
    REFRESH lt_zmm_proposal_doc.
    SELECT * FROM zmm_proposal_doc INTO TABLE lt_zmm_proposal_doc
                                  WHERE prno = gv_prno." AND pr_itemno = gv_itemno.
    IF lt_zmm_proposal_doc IS NOT INITIAL.
      SORT lt_zmm_proposal_doc BY pr_itemno DESCENDING.
      READ TABLE lt_zmm_proposal_doc INTO ls_zmm_proposal_doc INDEX 1.
      IF ls_zmm_proposal_doc-prno IS INITIAL.
        ls_zmm_proposal_doc-pr_itemno = ls_zmm_proposal_doc-pr_itemno + 10."gv_itemno.
      ELSE.
        ls_zmm_proposal_doc-pr_itemno = ls_zmm_proposal_doc-pr_itemno + 10.
      ENDIF.
      ls_zmm_proposal_doc-prno = gv_prno.
      ls_zmm_proposal_doc-objkey = l_objkey.
      CLEAR n_lines.
      DESCRIBE TABLE lt_zmm_proposal_doc LINES n_lines.
      IF n_lines = 5.
        MESSAGE text-020 TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        APPEND ls_zmm_proposal_doc TO lt_zmm_proposal_doc.
      ENDIF.
      CLEAR ls_zmm_proposal_doc.
    ELSE.
      CLEAR ls_zmm_proposal_doc.
      ls_zmm_proposal_doc-prno = gv_prno.
      ls_zmm_proposal_doc-pr_itemno = 10."gv_itemno.
      ls_zmm_proposal_doc-objkey = l_objkey.
      IF ls_zmm_proposal_doc-objkey IS NOT INITIAL.
        APPEND ls_zmm_proposal_doc TO lt_zmm_proposal_doc.
      ENDIF.
    ENDIF.
*    move l_objkey to ZMM_PUR_PROPOSAL-objkey.
    IF lt_zmm_proposal_doc IS NOT INITIAL.
      MODIFY zmm_proposal_doc FROM TABLE lt_zmm_proposal_doc."TRANSPORTING prno objkey .
      COMMIT WORK.
      IF sy-subrc = 0.
        MESSAGE s005(zpp) WITH gv_prno.
      ENDIF.
    ELSE.
      MESSAGE i009(zpp) WITH gv_prno DISPLAY LIKE 'E'.
    ENDIF.
    CLEAR list_box5.
  ENDIF.
ENDFORM.                    " CREATE_ATTACHMENT_502
*&---------------------------------------------------------------------*
*&      Form  DELETE_ATTACHMENT_502
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_attachment_502 .
  DATA: lt_zmm_proposal_doc TYPE TABLE OF zmm_proposal_doc,
        ls_zmm_proposal_doc TYPE zmm_proposal_doc,
        lv_pruser  TYPE zmm_pur_proposal-pr_user.
  CLEAR: gv_itemno,ls_zmm_proposal_doc,gv_ans,lv_msg,lv_pruser,lv_list_boxx.
  gv_itemno = list_box5 * 10.
  lv_list_boxx = list_box5 * 10.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_list_boxx
    IMPORTING
      output = lv_list_boxx.
  SELECT SINGLE pr_user FROM zmm_pur_proposal INTO lv_pruser WHERE prno = gv_prno.
  IF lv_pruser NE sy-uname.
    MESSAGE i014(zpp) WITH lv_pruser DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    READ TABLE list_final WITH KEY lv_list_boxx."list_box5.
    IF sy-subrc = 0.
      CREATE OBJECT lo_gos_service.
*      gv_itemno = list5-key * 10.
      SELECT SINGLE * FROM zmm_proposal_doc INTO ls_zmm_proposal_doc
                      WHERE prno = gv_prno AND pr_itemno = gv_itemno.
      IF ls_zmm_proposal_doc-prno IS NOT INITIAL.
        lv_msg = 'Delete Attachment'.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
           titlebar                     = lv_msg
            text_question               = 'Do you want to Delete Attachment for Selected Line?'
           text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
           text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
           default_button               = '1'
           display_cancel_button        = ' '
           start_column                 = 55
           start_row                    = 3
         IMPORTING
           answer                       = gv_ans                 .
        IF gv_ans = 1.
          CALL METHOD lo_gos_service->delete_attachment
           EXPORTING
*              is_object     = is_object
             ip_attachment = ls_zmm_proposal_doc-objkey.

          DELETE FROM zmm_proposal_doc WHERE prno = ls_zmm_proposal_doc-prno
                                     AND pr_itemno = ls_zmm_proposal_doc-pr_itemno.
          COMMIT WORK.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE i007(zpp) WITH gv_prno DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE i007(zpp) WITH gv_prno DISPLAY LIKE 'E'.
    ENDIF.
    CLEAR: sy-ucomm,list_box5.
  ENDIF.
ENDFORM.                    " DELETE_ATTACHMENT_502
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  BACK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE back INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 100.
  ENDCASE.
ENDMODULE.                 " BACK  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  SET PF-STATUS 'ZPF103'.
  SET TITLEBAR 'ZTIT103'.
  CASE sy-ucomm.
    WHEN 'BACK'.

      REFRESH:gt_tab3,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
         list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal,g_mytable.
      CLEAR: gs_tab3,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab3,gs_stxh,gs_a516, gs_pa0001,
             gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
             flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
             no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
             gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
             gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs,gv_plantname,gv_eknam,gv_date,g_mytable,gv_user,gv_time,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,remarks,old_remarks,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4.
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      CALL METHOD remarks_container->free.
      CALL METHOD remarks_editor->free.

      LEAVE TO SCREEN 100.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

  IF flag1 EQ ' '.
    CLEAR:  releaser_name1,releaser_name2,releaser_name3,releaser_name4.
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
      v_mode = '1'.
    call method remarks_editor->set_readonly_mode
      exporting
      readonly_mode = v_mode
      exceptions
          error_cntl_call_method = 1
          invalid_parameter = 2
          others = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

     create object editor_container
      exporting
        container_name              = 'TEXTEDITOR'
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        others                      = 6.
    create object text_editor
      exporting
        parent                     = editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position          = line_length
        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
    call method text_editor->set_toolbar_mode
      exporting
        toolbar_mode = cl_gui_textedit=>false.
    call method text_editor->set_statusbar_mode
      exporting
        statusbar_mode = cl_gui_textedit=>false.

    v_mode = '1'.
    call method text_editor->set_readonly_mode
      exporting
      readonly_mode = v_mode
      exceptions
          error_cntl_call_method = 1
          invalid_parameter = 2
          others = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    flag1 = 'X'.

  ENDIF.
ENDMODULE.                 " STATUS_0103  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.
  SET PF-STATUS 'ZPF102'.
  SET TITLEBAR 'ZTIT102'.

  CASE sy-ucomm.
    WHEN 'BACK'.
      REFRESH:gt_tab2,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
         list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal,g_mytable.
      CLEAR: gs_tab22,gs_tab2,gs_tab3,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab3,gs_stxh,gs_a516, gs_pa0001,
             gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
             flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
             no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
             gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
             gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs,gv_plantname,gv_eknam,gv_date,g_mytable,gv_user,gv_time,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,remarks,old_remarks,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,
             list,list2,list3,list4,list_box,list_box2,list_box3,list_box4.

      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      CALL METHOD remarks_container->free.
      CALL METHOD remarks_editor->free.
      IF editor_container IS NOT INITIAL ."AND text_editor IS NOT INITIAL.
       CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
*      CALL METHOD editor_container->free.
      CALL METHOD text_editor->free.
      ENDIF.
*      CALL METHOD editor_container->free.
*      CALL METHOD text_editor->free.

      name = 'LIST_BOX'.
      CALL FUNCTION 'VRM_DELETE_VALUES'
        EXPORTING
          id           = name
        EXCEPTIONS
          id_not_found = 1
          OTHERS       = 2.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list[].
      name = 'LIST_BOX2'.
      CALL FUNCTION 'VRM_DELETE_VALUES'
        EXPORTING
          id           = name
        EXCEPTIONS
          id_not_found = 1
          OTHERS       = 2.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list2[].
      name = 'LIST_BOX3'.
      CALL FUNCTION 'VRM_DELETE_VALUES'
        EXPORTING
          id           = name
        EXCEPTIONS
          id_not_found = 1
          OTHERS       = 2.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list3[].
      name = 'LIST_BOX4'.
      CALL FUNCTION 'VRM_DELETE_VALUES'
        EXPORTING
          id           = name
        EXCEPTIONS
          id_not_found = 1
          OTHERS       = 2.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list4[].
      LEAVE TO SCREEN 100.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

  CASE gv_okcode.
    WHEN 'BACK'.

      REFRESH:gt_tab2,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
         list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal.
      CLEAR: gs_tab22,gs_tab2,gs_tab3,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab3,gs_stxh,gs_a516, gs_pa0001,
             gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
             flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
             no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
             gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
             gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs,gv_plantname,gv_eknam,gv_date,g_mytable,gv_user,gv_time,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,remarks,old_remarks,
             releaser_name1,releaser_name2,releaser_name3,releaser_name4,
             list,list2,list3,list4.

      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2
          OTHERS            = 3.
      CALL METHOD remarks_container->free.
      CALL METHOD remarks_editor->free.
      LEAVE TO SCREEN 100.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
  IF flag1 EQ ' '.
    CREATE OBJECT remarks_container
      EXPORTING
        container_name              = 'OTH_REMARKS'     "custom container name
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
    flag1 = 'X'.
  ENDIF.
  CONTROLS: tc2 TYPE TABLEVIEW USING SCREEN 0102.

  LOOP AT tc2-cols INTO cols WHERE index GT 0.
    IF  cols-screen-group1 = 'MT1'
         AND cols-screen-input = '0'.
      cols-screen-input = '1'."'1'.
    ELSEIF cols-screen-group1 = 'MT1'
       AND cols-screen-input = '1'.
      cols-screen-input = '0'.
    ENDIF.
    MODIFY tc2-cols FROM cols INDEX sy-tabix.
  ENDLOOP.

  IF flag9 = 'X'.
    REFRESH:list,list2,list3,list4.
    CLEAR :list_box,list_box2,list_box3,list_box4,gs_tab22,lv_key3,lv_key4,flag9.
    READ TABLE gt_tab2 INTO gs_tab22 INDEX 1.
    IF sy-subrc = 0.
      list-key   = gs_tab22-releaser1.
      list-text  = gs_tab22-releaser_name1.
      list2-key   = gs_tab22-releaser2.
      list2-text  = gs_tab22-releaser_name2.
      list3-key   = gs_tab22-releaser3.
      list3-text  = gs_tab22-releaser_name3.
      list4-key   = gs_tab22-releaser4.
      list4-text  = gs_tab22-releaser_name4.
      APPEND list.
      APPEND list2.
      APPEND list3.
      APPEND list4.
      name = 'LIST_BOX'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list[].
      list_box = list-key.
      name = 'LIST_BOX2'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list2[].
      list_box2 = list2-key.
      name = 'LIST_BOX3'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list3[].
      lv_key3 = list3-key.
      SHIFT list3-key LEFT DELETING LEADING '0'.
      IF list3-key IS NOT INITIAL.
        list_box3 = lv_key3.
      ENDIF.
      name = 'LIST_BOX4'.
      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id     = name
          values = list4[].
      lv_key4 = list4-key.
      SHIFT list4-key LEFT DELETING LEADING '0'.
      IF list4-key IS NOT INITIAL.
        list_box4 = lv_key4.
      ENDIF.
    ENDIF.

    CLEAR gs_tab22.
  ENDIF.

ENDMODULE.                 " STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL_GVPRNO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl_gvprno INPUT.
 TYPES: BEGIN OF ty_f4_prno_102,
          prno      TYPE zmm_pur_proposal-prno,
          pr_date   TYPE zpr_dt,
          ekgrp     TYPE ekgrp,
          eknam     TYPE eknam,
          werks     TYPE WERKS_D,
          plantname TYPE name1,
          matnr     TYPE matnr,
          maktx     TYPE maktx,
          lifnr     TYPE lifnr,
          name1     TYPE name1,
         END OF ty_f4_prno_102.
  DATA : lt_f4_prno1 TYPE TABLE OF ty_f4_prno_102,
         ls_f4_prno1 TYPE ty_f4_prno_102.
  DATA : fmap        TYPE TABLE OF dselc WITH HEADER LINE,
         dynfields   type table of dynpread with header line.

  refresh:lt_f4_prno1.
  SELECT prno pr_date ekgrp eknam werks plantname matnr maktx lifnr name1 FROM zmm_pur_proposal INTO TABLE lt_f4_prno1 WHERE pr_user = sy-uname.
                                                               " AND app_vendor = 'X' ."AND prno = gv_prno.
  SORT lt_f4_prno1 BY prno matnr lifnr .
  DELETE ADJACENT DUPLICATES FROM lt_f4_prno1 COMPARING prno matnr lifnr .
  refresh fmap.
  fmap-fldname = 'PRNO'.
  fmap-dyfldname = 'GV_PRNO'.
  APPEND fmap.
  fmap-fldname = 'PR_DATE'.
  fmap-dyfldname = 'GV_DATE'.
  APPEND fmap.
  fmap-fldname = 'EKGRP'.
  fmap-dyfldname = 'GV_EKGRP'.
  APPEND fmap.
  fmap-fldname = 'EKNAM'.
  fmap-dyfldname = 'GV_EKNAM'.
  APPEND fmap.
  fmap-fldname = 'WERKS'.
  fmap-dyfldname = 'GV_WERKS'.
  APPEND fmap.
  fmap-fldname = 'PLANTNAME'.
  fmap-dyfldname = 'GV_PLANTNAME'.
  APPEND fmap.
  fmap-fldname = 'MATNR'.
  fmap-dyfldname = 'GS_TAB2-MATNR'.
  APPEND fmap.
  fmap-fldname = 'MAKTX'.
  fmap-dyfldname = 'GS_TAB2-MAKTX'.
  APPEND fmap.
  fmap-fldname = 'LIFNR'.
  fmap-dyfldname = 'GS_TAB2-LIFNR'.
  APPEND fmap.
  fmap-fldname = 'NAME1'.
  fmap-dyfldname = 'GS_TAB2-NAME1'.
  APPEND fmap.

  REFRESH it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
*     DDIC_STRUCTURE = 'LS_F4_PRNO1'
     retfield    = 'PRNO'
     dynpprog    = sy-cprog"repid
     dynpnr      = sy-dynnr "'0102'
     dynprofield = 'GV_PRNO'
     window_title = 'Select Proposal With Approved Material'
     value_org   = 'S'
     TABLES
     value_tab       = lt_f4_prno1
*     return_tab      = it_ret
     dynpfld_mapping = fmap
  EXCEPTIONS
   parameter_error = 1
   no_values_found = 2
   OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
*    READ TABLE it_ret INDEX 1.
*    gv_prno = it_ret-fieldval.
  ENDIF.

ENDMODULE.                 " F4_TCL_GVPRNO  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  CLEAR gv_okcode.
  gv_okcode = sy-ucomm.
*  CLEAR sy-ucomm.
  CASE gv_okcode .

    WHEN 'DISPLAY'.

      REFRESH: gt_zmm_pur_proposal.
      SELECT * FROM zmm_pur_proposal INTO TABLE gt_zmm_pur_proposal WHERE prno = gv_prno.
      IF sy-subrc = 0.
        CLEAR : gs_zmm_pur_proposal,gv_ekgrp,gv_eknam,gv_werks,gv_plantname.
        READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal WITH KEY prno = gv_prno.
        IF gs_zmm_pur_proposal-pr_user = sy-uname.
          PERFORM display_102.
        ELSE.
          MESSAGE 'You are not authorised Person to change this Proposal' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Proposal Number not found' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    WHEN ' '.
      CLEAR : flag_final,flag3_1,flag3_3,flag7.
      SELECT SINGLE eknam FROM t024 INTO gv_eknam WHERE ekgrp = gv_ekgrp.
      SELECT SINGLE name1 FROM t001w INTO gv_plantname WHERE werks = gv_werks.
      GET CURSOR LINE g_lineno.
      IF tc2-lines GE 10 AND tc2-top_line NE 1.
        CLEAR: g_lineno,gs_tab2.
        g_lineno = tc2-lines.
      ENDIF.
      READ TABLE gt_tab2 INTO gs_tab2 INDEX g_lineno.
      IF sy-subrc = 0.
*{   REPLACE        SBXK900030                                        1
*\        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\          EXPORTING
*\            input  = gs_tab2-matnr
*\          IMPORTING
*\            output = gs_tab2-matnr.
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
          input  = gs_tab2-matnr
        IMPORTING
          output = gs_tab2-matnr.

*}   REPLACE
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
          PERFORM check_material_102.
        ENDIF.
        PERFORM check_vendor_102.
        PERFORM check_newprice_landprice_102.
        PERFORM final_data_102.
      ENDIF.
      PERFORM get_auth_list.

    WHEN 'PTERM'.
      GET CURSOR LINE g_lineno.
      READ TABLE gt_tab2 INTO gs_tab2 INDEX g_lineno.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
        PERFORM create_line_text_102.
      ENDIF.
      IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
        PERFORM create_line_text_102_1.
      ENDIF.

    WHEN 'SAVE_TEXT'.
      IF flag = 'X'.
        PERFORM save_line_text_102.
      ELSE.
        MESSAGE text-013 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'CHECK'.

      CLEAR : flag_final,flag3_1,flag3_3,flag7,flag_final.
      IF gt_tab2 IS INITIAL.
        MESSAGE text-014 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 100.
      ENDIF.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'..
        PERFORM check_material_102.
        PERFORM check_final_102.
      ENDIF.

      IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
        PERFORM check_final_102_1.
      ENDIF.
      PERFORM check_releaser_remarks_102.


    WHEN 'SAVE'. "change proposal                                    "ravi102
      CLEAR : flag_final,flag3_1,flag3_3,flag7.
      IF flag6 = 'X'.
        IF gt_tab2 IS NOT INITIAL.
          CLEAR: flag3,flag3_1,flag3_2.
          REFRESH gt_tab3.
          gt_tab3[] = gt_tab2[].
          IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'..
            PERFORM save_check_102.
          ENDIF.
          IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
            REFRESH : gt_tab3.
            gt_tab3[] = gt_tab2[].
            PERFORM save_check_102_1.
          ENDIF.
          CLEAR remarks.
          CALL METHOD remarks_editor->get_textstream
            IMPORTING
              text = remarks.
          CALL METHOD cl_gui_cfw=>flush
            EXCEPTIONS
              cntl_system_error = 1
              cntl_error        = 2
              OTHERS            = 3.
          PERFORM check_releaser_101.
          IF flag3 NE 'X'.
            MESSAGE text-015 TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF flag7 = 'X'.
            PERFORM screen_edit101.
            CLEAR flag7.
            MESSAGE 'Check Material,Vendor,Qty,New Basic Price,Landed Price' TYPE 'S' DISPLAY LIKE 'E'.
          ELSEIF flag3_1 EQ 'X'.
            PERFORM screen_edit102.
            MESSAGE 'Check Material,Last Purchase and Business Plan Price' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR flag3_1.
          ELSEIF flag3_2 EQ 'X'.
            PERFORM screen_edit102.
            CLEAR flag3_2.
          ELSEIF flag3_3 EQ 'X'.
            PERFORM screen_edit102.
            CLEAR flag3_3.
          ELSEIF flag3_4 EQ 'X'.
            PERFORM screen_edit102.
            CLEAR flag3_4.
          ELSEIF remarks IS INITIAL.
            MESSAGE text-018 TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSE.
            IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
              PERFORM compare_sel_new_basic_price102.      "selected line check with other
              CLEAR flag3_4.
            ENDIF.
            lv_msg = 'Change Proposal'.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
               titlebar                     = lv_msg
                text_question               = 'Do you want to Change this Proposal ?'
               text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
               text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
               default_button               = '1'
               display_cancel_button        = ' '
               start_column                 = 55
               start_row                    = 3
             IMPORTING
               answer                       = gv_ans                 .
            IF gv_ans = 2.
              LOOP AT SCREEN.
                screen-input = 0.
                screen-active = 0.
                MODIFY SCREEN.
              ENDLOOP.
            ELSE.
              PERFORM change_proposal.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0102  INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC2' ITSELF
*CONTROLS: tc2 TYPE TABLEVIEW USING SCREEN 0102.

*&SPWIZARD: LINES OF TABLECONTROL 'TC2'
DATA:     g_tc2_lines  LIKE sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC2'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc2_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_tab2 LINES tc2-lines.
ENDMODULE.                    "TC2_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC2'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc2_get_lines OUTPUT.
  g_tc2_lines = sy-loopc.
ENDMODULE.                    "TC2_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC2'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc2_modify INPUT.
  MODIFY gt_tab2
    FROM gs_tab2
    INDEX tc2-current_line.
ENDMODULE.                    "TC2_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC2'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc2_mark INPUT.
  DATA: g_tc2_wa2 LIKE LINE OF gt_tab2.
  IF tc2-line_sel_mode = 1
  AND gs_tab2-flag = 'X'.
    LOOP AT gt_tab2 INTO g_tc2_wa2
      WHERE flag = 'X'.
      g_tc2_wa2-flag = ''.
      MODIFY gt_tab2
        FROM g_tc2_wa2
        TRANSPORTING flag.
    ENDLOOP.
  ENDIF.
  MODIFY gt_tab2
    FROM gs_tab2
    INDEX tc2-current_line
    TRANSPORTING flag.
ENDMODULE.                    "TC2_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC2'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc2_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC2'
                              'GT_TAB2'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC2_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_MATNR102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_matnr102 INPUT.

  CLEAR: it_ret,lt_mara,ls_mara.
  REFRESH: lt_f4_matnr,lt_f4_matnr1.

  IF gv_ekgrp IS NOT INITIAL.
    SELECT matnr FROM marc INTO TABLE lt_f4_matnr WHERE ekgrp = gv_ekgrp." AND WERKS = GV_WERKS.
    IF lt_f4_matnr IS NOT INITIAL.
      SELECT matnr maktx FROM makt INTO TABLE lt_f4_matnr1 FOR ALL ENTRIES IN lt_f4_matnr WHERE matnr = lt_f4_matnr-matnr AND spras = 'E'..
    ENDIF.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'MATNR'
        value_org       = 'S'
*     dynpprog        = ''
        dynpnr          = '0102'
        dynprofield     = 'GS_TAB2-MATNR'
       TABLES
        value_tab       = lt_f4_matnr1
        return_tab      = it_ret
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3.
    ELSE.
      READ TABLE it_ret INDEX 1.
      gs_tab2-matnr = it_ret-fieldval.
      CLEAR: it_ret,lt_f4_matnr.
      REFRESH: lt_f4_matnr,lt_f4_matnr1.
    ENDIF.

  ELSE.
    MESSAGE text-011 TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO SCREEN 0100.
  ENDIF.
ENDMODULE.                 " F4_MATNR102  INPUT

*&---------------------------------------------------------------------*
*&      Module  F4_TC1_LIFNR102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tc1_lifnr102 INPUT.
  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.

  CLEAR : selline ,selindex.
  GET CURSOR FIELD gs_tab2-lifnr LINE selline.
*  selindex = tc1-top_line + selline - 1.   "
  READ TABLE gt_tab2 INTO gs_tab2
                            INDEX selindex.
*  IF sy-subrc = 0.
  REFRESH:lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                           WHERE lifnr = lt_lfb1-lifnr.
  ENDIF.
  REFRESH:it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'LIFNR'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0102'
     dynprofield = 'GS_TAB2-LIFNR'
     TABLES
     value_tab   = lt_f4_lifnr
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
    gs_tab2-lifnr = it_ret-fieldval.
    CLEAR: it_ret,lt_f4_lifnr.
    MODIFY gt_tab2 FROM gs_tab2 INDEX selline TRANSPORTING lifnr.
  ENDIF.

ENDMODULE.                 " F4_TC1_LIFNR102  INPUT

*&---------------------------------------------------------------------*
*&      Module  F4_TC1_WAERS102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tc1_waers102 INPUT.
  CLEAR : selline ,selindex.
  GET CURSOR FIELD gs_tab2-waers LINE selline.
*  selindex = tc1-top_line + selline - 1.   "
  READ TABLE gt_tab2 INTO gs_tab2
                            INDEX selindex.
*  IF sy-subrc = 0.
  SELECT waers ltext FROM tcurt INTO TABLE lt_f4_waers WHERE spras = 'EN'.
  REFRESH:it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'WAERS'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0102'
     dynprofield = 'GS_TAB2-WAERS'
     TABLES
     value_tab   = lt_f4_waers
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
    gs_tab2-waers = it_ret-fieldval.
    CLEAR: it_ret,lt_f4_lifnr.
  ENDIF.
ENDMODULE.                 " F4_TC1_WAERS102  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-MWSKZ102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-mwskz102 INPUT.
  SELECT mwskz text1 FROM t007s INTO TABLE lt_f4_mwskz WHERE spras = 'EN'.

  GET CURSOR FIELD gs_tab2-mwskz LINE selline.
  READ TABLE gt_tab2 INTO gs_tab2
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MWSKZ'
      dynpnr          = '102'
      dynprofield     = 'GS_TAB2-MWSKZ'
      value_org       = 'S'
    TABLES
      value_tab       = lt_f4_mwskz
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab2-mwskz = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
ENDMODULE.                 " F4_TCL-MWSKZ102  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-COSTFORMULA102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-costformula102 INPUT.

  IF flag4 = ' '.
    gs_compdata-field1 = 'Yes'.
    APPEND gs_compdata TO gt_compdata.
    CLEAR gs_compdata.
    gs_compdata-field1 = 'No'.
    APPEND gs_compdata TO gt_compdata.
    flag4 = 'X'.
  ENDIF.

  CLEAR: it_ret.
  GET CURSOR FIELD gs_tab2-costformula LINE selline.
  READ TABLE gt_tab2 INTO gs_tab2
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COSTFORMULA'
      dynpnr          = '102'
      dynprofield     = 'GS_TAB2-COSTFORMULA'
      value_org       = 'S'
    TABLES
      value_tab       = gt_compdata
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab2-costformula = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
  IF selline NE 0.
    MODIFY gt_tab2 FROM gs_tab2 INDEX selline TRANSPORTING costformula .
  ENDIF.

ENDMODULE.                 " F4_TCL-COSTFORMULA102  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-COMP_DATA_AVL102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-comp_data_avl102 INPUT.
  IF flag5 = ' '.
    gs_compdata1-field1 = 'Yes'.
    APPEND gs_compdata1 TO gt_compdata1.
    CLEAR gs_compdata1.
    gs_compdata1-field1 = 'No'.
    APPEND gs_compdata1 TO gt_compdata1.
    flag5 = 'X'.
  ENDIF.

  CLEAR: it_ret.
  GET CURSOR FIELD gs_tab2-comp_data_avl LINE selline.
  READ TABLE gt_tab2 INTO gs_tab2
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COMP_DATA_AVL'
      dynpnr          = '102'
      dynprofield     = 'GS_TAB2-COMP_DATA_AVL'
      value_org       = 'S'
    TABLES
      value_tab       = gt_compdata1
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab2-comp_data_avl = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
  IF selline NE 0.
    MODIFY gt_tab2 FROM gs_tab2 INDEX selline TRANSPORTING comp_data_avl.
  ENDIF.
ENDMODULE.                 " F4_TCL-COMP_DATA_AVL102  INPUT

*&---------------------------------------------------------------------*
*&      Module  COPY_DATA102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE copy_data102 INPUT.
*gv_okcode = sy-ucomm.
  CLEAR: g_lineno.
*  selindex = tc1-top_line + selline - 1.

  IF gv_okcode = 'COPY'.
    GET CURSOR LINE g_lineno.
    READ TABLE gt_tab2 INTO gs_tab2 INDEX g_lineno.
    IF sy-subrc = 0.
      gs_tab2-ebelp =   gs_tab2-ebelp + 1.
      gs_tab2-netpr = ' '.
      gs_tab2-landed_price = ' '.
*  APPEND GS_TAB1 TO GT_TAB1 WHERE INDEX = GS_TAB1-EBELP .
      INSERT gs_tab2 INTO gt_tab2 INDEX gs_tab2-ebelp.
      CLEAR gs_tab2.
      LOOP AT gt_tab2 INTO gs_tab2.
        gs_tab2-ebelp = sy-tabix .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_tab2-ebelp
          IMPORTING
            output = gs_tab2-ebelp.
        MODIFY gt_tab2 FROM gs_tab2.
        CLEAR: gs_tab2.
      ENDLOOP.
      CLEAR : gv_okcode,sy-ucomm.
    ENDIF.
  ENDIF.
ENDMODULE.                 " COPY_DATA102  INPUT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_102 .
  gv_ekgrp = gs_zmm_pur_proposal-ekgrp.
  gv_eknam = gs_zmm_pur_proposal-eknam.
  gv_werks = gs_zmm_pur_proposal-werks.
  gv_plantname = gs_zmm_pur_proposal-plantname.
  gv_date = sy-datum.
  gv_user = sy-uname.
  gv_time = sy-timlo.
  REFRESH: gt_tab2.
  CLEAR : gs_tab2.
  LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal .
*    gs_tab2-chk                     =  gs_zmm_pur_proposal-app_vendor .
*       gv_prno                         =  gs_zmm_pur_proposal-prno.
    gs_tab2-gjahr                   =  gs_zmm_pur_proposal-gjahr.
    gs_tab2-pr_date                 =  gs_zmm_pur_proposal-pr_date.
    gs_tab2-ekgrp                   =  gs_zmm_pur_proposal-ekgrp.
    gs_tab2-eknam                   =  gs_zmm_pur_proposal-eknam.
    gs_tab2-bukrs                   =  gs_zmm_pur_proposal-bukrs .
    gs_tab2-werks                   =  gs_zmm_pur_proposal-werks.
    gs_tab2-plantname               =  gs_zmm_pur_proposal-plantname.
    gs_tab2-erdat                   =  gs_zmm_pur_proposal-erdat.
    gs_zmm_pur_proposal-pr_itemno   = gs_zmm_pur_proposal-pr_itemno / 10.
    gs_tab2-ebelp                   =  gs_zmm_pur_proposal-pr_itemno .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-matnr
      IMPORTING
        output = gs_zmm_pur_proposal-matnr.
    gs_tab2-matnr                   =  gs_zmm_pur_proposal-matnr.
    gs_tab2-maktx                   =  gs_zmm_pur_proposal-maktx .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-lifnr
      IMPORTING
        output = gs_zmm_pur_proposal-lifnr.
    gs_tab2-lifnr                   =  gs_zmm_pur_proposal-lifnr.
    gs_tab2-name1                   =  gs_zmm_pur_proposal-name1.
    gs_tab2-qty                     =  gs_zmm_pur_proposal-menge .
    gs_tab2-meins                   =  gs_zmm_pur_proposal-meins.
    gs_tab2-netpr                   =  gs_zmm_pur_proposal-netpr.
    gs_tab2-waers                   =  gs_zmm_pur_proposal-waers .
    gs_tab2-mwskz                   =  gs_zmm_pur_proposal-mwskz.
    gs_tab2-tax_desc                =  gs_zmm_pur_proposal-tax_desc.
    gs_tab2-landed_price            =  gs_zmm_pur_proposal-land_pr    .
    gs_tab2-landed_value            =  gs_zmm_pur_proposal-land_value.
    gs_tab2-last_pur_basic_price    =  gs_zmm_pur_proposal-last_basicpr.
    gs_tab2-last_pur_land_price     =  gs_zmm_pur_proposal-last_landpr .
    gs_tab2-business_price          =  gs_zmm_pur_proposal-business_planpr.
    gs_tab2-vari_last_pur_inr       =  gs_zmm_pur_proposal-vari_last_pur_in.
    gs_tab2-vari_last_pur_val       =  gs_zmm_pur_proposal-vari_last_pur_va.
    gs_tab2-vari_business_plan_inr  =  gs_zmm_pur_proposal-vari_bus_pl_inr.
    gs_tab2-vari_business_plan_val  =  gs_zmm_pur_proposal-vari_bus_pl_val.
    gs_tab2-op_m1                   =  gs_zmm_pur_proposal-opening_m1.
    gs_tab2-req_m1                  =  gs_zmm_pur_proposal-requirment_m1.
    gs_tab2-app_vend_m1             =  gs_zmm_pur_proposal-app_vend_m1.
    gs_tab2-otr_vend_m1             =  gs_zmm_pur_proposal-other_vend_m1.
    gs_tab2-clos_m1                 =  gs_zmm_pur_proposal-closing_m1.
    gs_tab2-op_m2                   =  gs_zmm_pur_proposal-opening_m2.
    gs_tab2-req_m2                  =  gs_zmm_pur_proposal-requirment_m2.
    gs_tab2-app_vend_m2             =  gs_zmm_pur_proposal-app_vend_m2.
    gs_tab2-otr_vend_m2             =  gs_zmm_pur_proposal-other_vend_m2.
    gs_tab2-clos_m2                 =  gs_zmm_pur_proposal-closing_m2.
    gs_tab2-op_m3                   =  gs_zmm_pur_proposal-opening_m3.
    gs_tab2-req_m3                  =  gs_zmm_pur_proposal-requirment_m3.
    gs_tab2-app_vend_m3             =  gs_zmm_pur_proposal-app_vend_m3.
    gs_tab2-otr_vend_m3             =  gs_zmm_pur_proposal-other_vend_m3.
    gs_tab2-clos_m3                 =  gs_zmm_pur_proposal-closing_m3.
    gs_tab2-costformula             =  gs_zmm_pur_proposal-comp_data.
    gs_tab2-comp_data_avl           =  gs_zmm_pur_proposal-comp_data1.
    gs_tab2-text                    =  gs_zmm_pur_proposal-line_txt.
    gs_tab2-remarks                 =  gs_zmm_pur_proposal-remark.
    old_remarks                     =  gs_zmm_pur_proposal-remark.
    gs_tab2-releaser1               =  gs_zmm_pur_proposal-releaser1.
    gs_tab2-releaser_name1          =  gs_zmm_pur_proposal-releaser_name1.
    releaser_name1              =  gs_zmm_pur_proposal-releaser_name1.
    gs_tab2-releaser2               =  gs_zmm_pur_proposal-releaser2.
    gs_tab2-releaser_name2          =  gs_zmm_pur_proposal-releaser_name2.
    releaser_name2              =  gs_zmm_pur_proposal-releaser_name2.
    gs_tab2-releaser3               =  gs_zmm_pur_proposal-releaser3.
    gs_tab2-releaser_name3          =  gs_zmm_pur_proposal-releaser_name3.
    releaser_name3              =  gs_zmm_pur_proposal-releaser_name3.
    gs_tab2-releaser4               =  gs_zmm_pur_proposal-releaser4 .
    gs_tab2-releaser_name4          =  gs_zmm_pur_proposal-releaser_name4.
    releaser_name4              =  gs_zmm_pur_proposal-releaser_name4.
    gs_tab2-pr_date                 =  gs_zmm_pur_proposal-erdat.
    gs_tab2-ch_date                 =  sy-datum.
    gs_tab2-pr_user                 =  gs_zmm_pur_proposal-pr_user.
    gs_tab2-pr_time                 =         gs_zmm_pur_proposal-pr_time .
    APPEND gs_tab2 TO gt_tab2.
    CLEAR :gs_tab2,gs_zmm_pur_proposal,remarks.

  ENDLOOP.
  remarks = old_remarks.
  CALL METHOD remarks_editor->set_textstream
    EXPORTING
      text = remarks.
  flag9 = 'X'.

ENDFORM.                    " DISPLAY_102
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_material_102 .

  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  REFRESH: lt_matnr.
*{   REPLACE        SBXK900030                                        1
*\  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\    EXPORTING
*\      input  = gs_tab2-matnr
*\    IMPORTING
*\      output = gs_tab2-matnr.
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
      input  = gs_tab2-matnr
    IMPORTING
      output = gs_tab2-matnr.

*}   REPLACE

  IF gs_tab2-matnr IS NOT INITIAL.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
    CLEAR ls_matnr.
    READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = gs_tab2-matnr.
    IF sy-subrc NE 0.
      MESSAGE i004(zpp) WITH gs_tab2-matnr gv_ekgrp DISPLAY LIKE 'E'.
      flag_final = 'X'.
      EXIT.
    ENDIF.
    IF lt_matnr IS INITIAL.
      MESSAGE i004(zpp) WITH gs_tab2-matnr gv_ekgrp DISPLAY LIKE 'E'.
      flag_final = 'X'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL_102

*&---------------------------------------------------------------------*
*&      Form  CHECK_VENDOR_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vendor_102 .

  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  CLEAR: lv_matnr,flag3_3.
  lv_lifnr = gs_tab2-lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_tab2-lifnr
    IMPORTING
      output = gs_tab2-lifnr.
  REFRESH: lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                           WHERE lifnr = lt_lfb1-lifnr.
    SORT lt_f4_lifnr BY lifnr.
    READ TABLE lt_f4_lifnr INTO ls_f4_lifnr WITH KEY lifnr = gs_tab2-lifnr.
    IF sy-subrc NE 0.
      MESSAGE i008(zpp) WITH gs_tab2-lifnr gv_werks DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

  IF lt_f4_lifnr IS INITIAL.
    MESSAGE i008(zpp) WITH gs_tab2-lifnr gv_werks DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_VENDOR_102

*&---------------------------------------------------------------------*
*&      Form  CHECK_NEWPRICE_LANDPRICE_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_newprice_landprice_102 .
  IF gs_tab2-qty IS INITIAL.
    MESSAGE i016(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ENDIF.
  IF gs_tab2-netpr IS INITIAL.
    MESSAGE i017(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ENDIF.
  IF gs_tab2-landed_price IS INITIAL.
    MESSAGE i018(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
    EXIT.
  ENDIF.
  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
  IF gs_tab2-business_price = 0.
    MESSAGE i001(zpp) WITH gs_tab2-matnr gs_tab2-business_price DISPLAY LIKE 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
  ENDIF.
  ENDIF.

  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
  IF gs_tab2-mwskz IS INITIAL.
    MESSAGE i019(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
  ENDIF.
  ENDIF.
  IF gs_tab2-netpr > gs_tab2-landed_price.
    MESSAGE i013(zpp) WITH gs_tab2-netpr gs_tab2-landed_price DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECK_NEWPRICE_LANDPRICE_102
*&---------------------------------------------------------------------*
*&      Form  FINAL_DATA_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_data_102 .
  TRANSLATE gs_tab2-waers TO UPPER CASE.
  TRANSLATE gs_tab2-mwskz TO UPPER CASE.
  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    GET CURSOR LINE g_lineno.
    CLEAR gs_tab1.
    READ TABLE gt_tab2 INTO gs_tab2 INDEX g_lineno.
    gs_tab2-matnr = ' '.
    gs_tab2-maktx = gs_tab2-maktx.
  ENDIF.

  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab2-matnr
*\      IMPORTING
*\        output = gs_tab2-matnr.
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
        input  = gs_tab2-matnr
      IMPORTING
        output = gs_tab2-matnr.

*}   REPLACE
    SELECT SINGLE maktx FROM makt INTO lv_maktx WHERE matnr = gs_tab2-matnr.
    gs_tab2-maktx = lv_maktx.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_tab2-lifnr
    IMPORTING
      output = gs_tab2-lifnr.
  SELECT SINGLE name1 FROM lfa1 INTO lv_name1 WHERE lifnr = gs_tab2-lifnr.
  gs_tab2-name1 = lv_name1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_tab2-lifnr
    IMPORTING
      output = gs_tab2-lifnr.
  gs_tab2-landed_value = ( gs_tab2-landed_price * gs_tab2-qty )." / 100.
  REFRESH: gt_a516.
  SELECT kschl
         werks
         matnr
         datbi
         datab
         knumh FROM a516 INTO TABLE gt_a516
          WHERE kschl = 'ZBPL' AND werks = gv_werks AND matnr = gs_tab2-matnr
                AND datbi GT gv_date AND datab LE gv_date.
  IF sy-subrc = 0.
    READ TABLE gt_a516 INTO gs_a516 INDEX 1."datab le gv_date.
    IF sy-subrc = 0.
      SELECT SINGLE kbetr FROM konp INTO lv_kbetr WHERE knumh = gs_a516-knumh.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
        gs_tab2-business_price = lv_kbetr.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    gs_tab2-business_price = ' '.
  ENDIF.

  SELECT SINGLE text1 FROM t007s INTO gs_tab2-tax_desc WHERE spras = 'EN' AND mwskz = gs_tab2-mwskz.
  REFRESH:gt_ekpo.
  SELECT ebeln
         ebelp
         aedat
         matnr
         werks
         netpr
    FROM ekpo INTO TABLE gt_ekpo WHERE matnr = gs_tab2-matnr AND werks = gv_werks.
  IF gt_ekpo IS NOT INITIAL.
    IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
      SORT gt_ekpo DESCENDING BY aedat.
      READ TABLE gt_ekpo INTO gs_ekpo INDEX 1.
      IF sy-subrc = 0.
        gs_tab2-last_pur_basic_price = gs_ekpo-netpr_i.
        CLEAR gs_ekpo.
      ELSE.
        gs_tab2-last_pur_basic_price = ' '.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    gs_tab2-last_pur_basic_price = ' '.
  ENDIF.
  gs_tab2-vari_last_pur_inr = ( gs_tab2-last_pur_land_price - gs_tab2-landed_price )."( gs_tab2-landed_price - gs_tab2-last_pur_land_price ).
  gs_tab2-vari_last_pur_val = ( gs_tab2-vari_last_pur_inr * gs_tab2-qty ) / 100000."variance last purchase value
  gs_tab2-vari_business_plan_inr = ( gs_tab2-business_price - gs_tab2-landed_price ). "variance Business plan per KG
  gs_tab2-vari_business_plan_val = ( gs_tab2-vari_business_plan_inr * gs_tab2-qty ) / 100000. "variance Business plan value
*  gs_tab2-otr_vend_m1  = ( gs_tab2-req_m1 - gs_tab2-app_vend_m1 ).
*  IF gs_tab2-otr_vend_m1 LT 0.
*    gs_tab2-otr_vend_m1 = gs_tab2-otr_vend_m1 * -1.
*  ENDIF.
*  gs_tab2-otr_vend_m2  = ( gs_tab2-req_m2 - gs_tab2-app_vend_m2 ).
*  IF gs_tab2-otr_vend_m2 LT 0.
*    gs_tab2-otr_vend_m2 = gs_tab2-otr_vend_m2 * -1.
*  ENDIF.
*  gs_tab2-otr_vend_m3  = gs_tab2-req_m3 - gs_tab2-app_vend_m3.
*  IF gs_tab2-otr_vend_m3 LT 0.
*    gs_tab2-otr_vend_m3 = gs_tab2-otr_vend_m3 * -1.
*  ENDIF.
  gs_tab2-clos_m1 = ( ( gs_tab2-op_m1 + gs_tab2-app_vend_m1 + gs_tab2-otr_vend_m1 ) - gs_tab2-req_m1 ) .
  gs_tab2-op_m2 = gs_tab2-clos_m1.
  gs_tab2-clos_m2 = ( ( gs_tab2-op_m2 + gs_tab2-app_vend_m2 + gs_tab2-otr_vend_m2 ) - gs_tab2-req_m2 ) .
  gs_tab2-op_m3 = gs_tab2-clos_m2.
  gs_tab2-clos_m3 = ( ( gs_tab2-op_m3 + gs_tab2-app_vend_m3 + gs_tab2-otr_vend_m3 ) - gs_tab2-req_m3 ) .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_tab2-matnr
    IMPORTING
      output = gs_tab2-matnr.
  LOOP AT gt_tab2 INTO gs_tab22 .
    IF sy-tabix = g_lineno.
      MODIFY gt_tab2 FROM gs_tab2 TRANSPORTING matnr maktx name1 landed_price landed_value
                         last_pur_basic_price last_pur_land_price business_price vari_last_pur_inr vari_last_pur_val
                         vari_business_plan_inr vari_business_plan_val tax_desc
                         otr_vend_m1 otr_vend_m2 otr_vend_m3 clos_m1 clos_m2 clos_m3 waers mwskz
                         op_m2 op_m3.
    ENDIF.
    CLEAR: gs_tab22,lv_kbetr.
  ENDLOOP.
*  ENDIF.
  CLEAR: lv_maktx,lv_name1.


ENDFORM.                    " FINAL_DATA_102
*&---------------------------------------------------------------------*
*&      Form  CHECK_MT_VN_BLINE_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mt_vn_bline_102 .
  CLEAR ls_matnr.
  REFRESH gt_tab3.
  gt_tab3[] = gt_tab2[].
  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
    READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = gs_tab2-matnr.
    IF sy-subrc NE 0.
      MESSAGE i004(zpp) WITH gs_tab2-matnr gv_ekgrp.
      flag3_1 = 'X'.
      flag_final = 'X'.
*      CLEAR gs_tab1.
    ENDIF.
    IF gs_tab2-business_price = 0.
      MESSAGE i001(zpp) WITH gs_tab2-matnr gs_tab2-business_price.
      flag3_1 = 'X'.
      flag_final = 'X'.
*      CLEAR gs_tab1.
    ENDIF.

    LOOP AT gt_tab3 INTO gs_tab22 ."WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab22-ebelp = lv_index.
        IF gs_tab22-chk = 'X'.
          IF gs_tab22-matnr = lv_matnr.
            MESSAGE i010(zpp) WITH gs_tab22-matnr DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab22.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CHECK_MT_VN_BLINE_102

"&---------------------------------------------------------------------*
*&      Form  CHECK_VEND_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vend_102 .
  CLEAR flag3_3.
  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  CLEAR lv_lifnr.
  lv_lifnr = gs_tab2-lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_lifnr
    IMPORTING
      output = lv_lifnr.

  REFRESH: lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                            WHERE lifnr = lt_lfb1-lifnr.
    READ TABLE lt_f4_lifnr INTO ls_f4_lifnr WITH KEY lifnr = lv_lifnr.
    IF sy-subrc NE 0.
      MESSAGE i008(zpp) WITH gs_tab2-lifnr gv_werks.
      flag3_3 = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_VEND_102

*&---------------------------------------------------------------------*
*&      Form  SCREEN_EDIT102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_edit102 .
  LOOP AT SCREEN.                                "ravi
    IF gs_tab2-chk = 'X'.
      screen-input = 0.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SCREEN_EDIT102

*&---------------------------------------------------------------------*
*&      Form  COMPARE_SEL_NEW_BASIC_PRICE102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compare_sel_new_basic_price102 .
  REFRESH : gt_tab1.
  CLEAR flag3_4.
  gt_tab1[] = gt_tab2[].
  LOOP AT gt_tab2 INTO gs_tab2.
    IF gs_tab2-chk = 'X'.
      LOOP AT gt_tab1 INTO gs_tab1 WHERE matnr = gs_tab2-matnr.
        IF gs_tab1-chk NE 'X'.
          IF gs_tab1-netpr LE gs_tab2-netpr.
            IF gs_tab2-text IS INITIAL.
              MESSAGE w015(zpp) WITH gs_tab2-matnr .
              flag3_4 = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR gs_tab1.
      ENDLOOP.
    ENDIF.
    CLEAR gs_tab1.
  ENDLOOP.
  IF flag3_4 = 'X'.
*    PERFORM screen_edittt102.
  ENDIF.
ENDFORM.                    " COMPARE_SEL_NEW_BASIC_PRICE102
*&---------------------------------------------------------------------*
*&      Form  SCREEN_EDITTT102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_edittt102 .
  LOOP AT SCREEN.                                "ravi
    IF gs_tab2-chk = 'X'.
      screen-input = 0.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SCREEN_EDITTT102

*&---------------------------------------------------------------------*
*&      Form  CREATE_LINE_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_line_text_102.

  IF gs_tab2-matnr IS NOT INITIAL.
    CLEAR gs_tab2.
    lv_curline  = g_lineno.
    flag = 'X'.
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
     REFRESH g_mytable.
    READ TABLE gt_tab2 INTO gs_tab2 INDEX lv_curline.
    IF gs_tab2-text IS NOT INITIAL.
      REFRESH g_mytable.
      text = gs_tab2-text.
      APPEND text TO g_mytable.
      DELETE g_mytable WHERE table_line IS INITIAL.
*        get_text_as_r3table
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
      SET CURSOR LINE g_lineno.
    ENDIF.
   IF gs_tab2-text IS INITIAL.
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
*       CALL METHOD cl_gui_cfw=>flush
*        EXCEPTIONS
*          cntl_system_error = 1
*          cntl_error        = 2
*          OTHERS            = 3.
*
*      CALL METHOD editor_container->free.
*      CALL METHOD text_editor->free.
    MESSAGE 'No Data found for selected Line,Please maintain and click on SAVE_TEXT' TYPE 'I'.

    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_LINE_TEXT
*&---------------------------------------------------------------------*
*&      Form  CREATE_LINE_TEXT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_line_text_102_1 .
  IF gs_tab2-maktx IS NOT INITIAL.
    CLEAR gs_tab2.
    lv_curline  = g_lineno.
    flag = 'X'.
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
    refresh g_mytable.
    READ TABLE gt_tab2 INTO gs_tab2 INDEX lv_curline.
    IF gs_tab2-text IS NOT INITIAL.
      REFRESH g_mytable.
      text = gs_tab2-text.
      APPEND text TO g_mytable.
      DELETE g_mytable WHERE table_line IS INITIAL.
*        get_text_as_r3table
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
      SET CURSOR LINE g_lineno.
      GET CURSOR FIELD field1.
    ENDIF.
  IF gs_tab2-text IS INITIAL.
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
*       CALL METHOD cl_gui_cfw=>flush
*        EXCEPTIONS
*          cntl_system_error = 1
*          cntl_error        = 2
*          OTHERS            = 3.
*
*      CALL METHOD editor_container->free.
*      CALL METHOD text_editor->free.
    MESSAGE 'No Data found for selected Line,Please maintain and click on SAVE_TEXT' TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_LINE_TEXT1
*&---------------------------------------------------------------------*
*&      Form  SAVE_LINE_TEXT_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_line_text_102 .
  CALL METHOD text_editor->get_textstream
*          EXPORTING
*              ONLY_WHEN_MODIFIED     = CL_GUI_TEXTEDIT=>TRUE
            IMPORTING
                text                   = text
*              IS_MODIFIED            =
            EXCEPTIONS
                error_cntl_call_method = 1
                not_supported_by_gui   = 2
                OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.
  CALL METHOD editor_container->free.
  CALL METHOD text_editor->free.

  CLEAR: flag,gs_tab2,g_lineno.
*        GET CURSOR LINE g_lineno.
  READ TABLE gt_tab2 INTO gs_tab2 INDEX lv_curline.
  IF sy-subrc = 0.
    LOOP AT gt_tab2 INTO gs_tab22 .
      gs_tab22-text = text.
      IF sy-tabix = lv_curline.
        MODIFY gt_tab2 FROM gs_tab22 TRANSPORTING text.
      ENDIF.
      CLEAR gs_tab22.
    ENDLOOP.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 = 'PB1'.
      IF screen-name = 'SAVE_TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR : text.
  SET CURSOR LINE lv_curline.
ENDFORM.                    " SAVE_LINE_TEXT_102
*&---------------------------------------------------------------------*
*&      Form  CHECK_FINAL_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_final_102 .
  CLEAR flag3.
  LOOP AT gt_tab2 INTO gs_tab2.
    lv_no = sy-tabix - 1.
    READ TABLE gt_tab2 INTO gs_tab22 INDEX lv_no.
    IF sy-subrc = 0.
      CLEAR flag7.
      IF gs_tab2-lifnr = gs_tab22-lifnr AND gs_tab2-matnr = gs_tab22-matnr.
        MESSAGE i003(zpp) WITH gs_tab2-lifnr gs_tab2-matnr DISPLAY LIKE 'E'.
        flag7 = 'X'.
      ENDIF.
    ENDIF.
    PERFORM check_newprice_landprice_102.

    CLEAR: lv_matnr,lv_index.
    IF gs_tab2-chk = 'X'.
      flag3 = 'X'.
      lv_matnr = gs_tab2-matnr.
      lv_index = gs_tab2-ebelp + 1.
    ENDIF.
    REFRESH lt_matnr.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab2-matnr
*\      IMPORTING
*\        output = gs_tab2-matnr.
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
        input  = gs_tab2-matnr
      IMPORTING
        output = gs_tab2-matnr.

*}   REPLACE
    PERFORM check_mt_vn_bline_102.  "chk material,ven,zbpl,best line
    PERFORM check_vendor_102.

    CLEAR: gs_tab2,gs_tab22.
  ENDLOOP.
ENDFORM.                    " CHECK_FINAL_102
*&---------------------------------------------------------------------*
*&      Form  CHECK_FINAL_102_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_final_102_1 .
  REFRESH : gt_tab3.
  gt_tab3[] = gt_tab2[].
  clear flag3.
  LOOP AT gt_tab2 INTO gs_tab2.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab2-chk = 'X'.
      flag3 = 'X'.
      lv_maktx = gs_tab2-maktx.
      lv_index = gs_tab2-ebelp + 1.
    ENDIF.
    LOOP AT gt_tab3 INTO gs_tab22." WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab22-ebelp = lv_index.
        IF gs_tab22-chk = 'X'.
          IF gs_tab22-maktx = lv_maktx.
            MESSAGE i010(zpp) WITH gs_tab22-maktx DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab22.
    ENDLOOP.
    PERFORM check_vendor_102.
    IF gs_tab2-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      flag_final = 'X'.
    ENDIF.
    IF gs_tab2-netpr > gs_tab2-landed_price.
      MESSAGE i013(zpp) WITH gs_tab2-netpr gs_tab2-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      flag_final = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_FINAL_102_1
*&---------------------------------------------------------------------*
*&      Form  CHECK_RELEASER_REMARKS_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_releaser_remarks_102 .
  CLEAR remarks.
  CALL METHOD remarks_editor->get_textstream
*    EXPORTING
*      only_when_modified     = FALSE
        IMPORTING
          text                   = remarks.
  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  CLEAR :gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4.
  READ TABLE list WITH KEY list_box.
  IF sy-subrc = 0.
    gv_releaser1 = list-key.
    gv_releaser_name1 = list-text.
  ENDIF.
  READ TABLE list2 WITH KEY list_box2.
  IF sy-subrc = 0.
    gv_releaser2 = list2-key.
    gv_releaser_name2 = list2-text.
  ENDIF.
  READ TABLE list3 WITH KEY list_box3.
  IF sy-subrc = 0.
    gv_releaser3 = list3-key.
    gv_releaser_name3 = list3-text.
  ENDIF.
  READ TABLE list4 WITH KEY list_box4.
  IF sy-subrc = 0.
    gv_releaser4 = list4-key.
    gv_releaser_name4 = list4-text.
  ENDIF.
  IF gv_releaser1 IS INITIAL AND gv_releaser2 IS INITIAL.
    MESSAGE text-016 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF gv_releaser1 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
*          ELSEIF gv_releaser3 EQ gv_releaser4.
*            MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
*            EXIT.
  ELSEIF gv_releaser4 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser4 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
*          ELSEIF gv_releaser4 EQ gv_releaser3.
*            MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
*            EXIT.
  ELSEIF flag3 NE 'X'.
    MESSAGE text-015 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF flag3_2 = 'X'.
    PERFORM screen_edit102.
    CLEAR flag3_2.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag3_3 = 'X'.
    PERFORM screen_edit102.
    CLEAR flag3_3.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag7 = 'X'.
    PERFORM screen_edit102.
    CLEAR flag7.
    flag_final = 'X'.
  ELSEIF remarks IS INITIAL.
    MESSAGE text-018 TYPE 'S' DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag_final = 'X'.
    PERFORM screen_edit102.
    CLEAR flag_final.
  ELSEIF flag_final = ' '.
    MESSAGE 'All entered line items are correct,You can Save your Proposal' TYPE 'I'.
    CLEAR flag_final.
  ENDIF.
  flag6 ='X'.
ENDFORM.                    " CHECK_RELEASER_REMARKS_102
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHECK_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_check_102 .
  LOOP AT gt_tab2 INTO gs_tab2.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab2-chk = 'X'.
      flag3 = 'X'.
      lv_matnr = gs_tab2-matnr.
      lv_index = gs_tab2-ebelp + 1.
    ENDIF.
    REFRESH lt_matnr.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab2-matnr
*\      IMPORTING
*\        output = gs_tab2-matnr.
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
        input  = gs_tab2-matnr
      IMPORTING
        output = gs_tab2-matnr.

*}   REPLACE
    PERFORM check_mt_vn_bline_102.  "chk material,zbpl,best line
    PERFORM check_vend_102.
    IF gs_tab2-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
    IF gs_tab2-business_price = 0.
      MESSAGE i001(zpp) WITH gs_tab2-matnr gs_tab2-business_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
    IF gs_tab2-netpr > gs_tab2-landed_price.
      MESSAGE i013(zpp) WITH gs_tab2-netpr gs_tab2-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab2-netpr IS INITIAL.
      MESSAGE i017(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab2-landed_price IS INITIAL.
      MESSAGE i018(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab2-mwskz IS INITIAL.
      MESSAGE i019(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SAVE_CHECK_102
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHECK_102_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_check_102_1 .
  LOOP AT gt_tab2 INTO gs_tab2.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab2-chk = 'X'.
      flag3 = 'X'.
      lv_maktx = gs_tab2-maktx.
      lv_index = gs_tab2-ebelp + 1.
    ENDIF.
    LOOP AT gt_tab3 INTO gs_tab22." WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab22-ebelp = lv_index.
        IF gs_tab22-chk = 'X'.
          IF gs_tab22-maktx = lv_maktx.
            MESSAGE i010(zpp) WITH gs_tab22-maktx DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab22.
    ENDLOOP.
    PERFORM check_vendor_102.
    IF gs_tab2-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab2-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
    if gs_tab2-netpr is initial.
      message i017(zpp) with gs_tab2-matnr display like 'E'.
      flag7 = 'X'.
      exit.
    endif.
    if gs_tab2-landed_price is initial.
      message i018(zpp) with gs_tab2-matnr display like 'E'.
      flag7 = 'X'.
      exit.
    endif.
    IF gs_tab2-netpr > gs_tab2-landed_price.
      MESSAGE i013(zpp) WITH gs_tab2-netpr gs_tab2-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SAVE_CHECK_102_1
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PROPOSAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_proposal .
  LOOP AT gt_tab2 INTO gs_tab2.
    gs_tab2-chk              =   gs_tab2-chk .
*    gs_tab2-pr_date          =   gv_date .
*    gs_tab2-erdat            =   gv_date .
    gs_tab2-ekgrp            =   gv_ekgrp .
    gs_tab2-eknam            =   gv_eknam.
    gs_tab2-werks            =   gv_werks .
    gs_tab2-bukrs            =   gv_bukrs .
    gs_tab2-plantname        =   gv_plantname .
    gs_tab2-pr_user          =   gv_user.
    gs_tab2-pr_time          =   gv_time.
    gs_tab2-remarks          =   remarks.
    gs_tab2-releaser1        =   gv_releaser1.
    gs_tab2-releaser_name1   =   gv_releaser_name1.
    gs_tab2-releaser2        =   gv_releaser2.
    gs_tab2-releaser_name2   =   gv_releaser_name2.
    gs_tab2-releaser3        =   gv_releaser3.
    gs_tab2-releaser_name3   =   gv_releaser_name3.
    gs_tab2-releaser4        =   gv_releaser4.
    gs_tab2-releaser_name4   =   gv_releaser_name4.
    MODIFY gt_tab2 FROM gs_tab2 TRANSPORTING chk ekgrp bukrs werks pr_time pr_user remarks releaser1
                                             releaser2 releaser3 releaser4 releaser_name1 releaser_name2
                                             releaser_name3 releaser_name4 eknam plantname                                          .
    CLEAR gs_tab2.
  ENDLOOP.

  REFRESH:gt_zmm_pur_proposal.
  LOOP AT gt_tab2 INTO gs_tab2.
    gs_zmm_pur_proposal-app_vendor         =   gs_tab2-chk.
    gs_zmm_pur_proposal-prno               =   gv_prno.
    gs_zmm_pur_proposal-gjahr              =   gs_tab2-gjahr..
    gs_zmm_pur_proposal-pr_date            =   gs_tab2-pr_date .
    gs_zmm_pur_proposal-ekgrp              =   gs_tab2-ekgrp.
    gs_zmm_pur_proposal-eknam              =   gs_tab2-eknam.
    gs_zmm_pur_proposal-bukrs              =   gs_tab2-bukrs.
    gs_zmm_pur_proposal-werks              =   gs_tab2-werks.
    gs_zmm_pur_proposal-plantname          =   gs_tab2-plantname.
    gs_zmm_pur_proposal-erdat              =   gs_tab2-erdat.
    gs_zmm_pur_proposal-pr_itemno          =   gs_tab2-ebelp * 10.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_tab2-matnr
      IMPORTING
        output = gs_tab2-matnr.
    IF gs_zmm_pur_proposal-ekgrp NOT BETWEEN 'B05' AND 'B06'.
      gs_zmm_pur_proposal-matnr            =   gs_tab2-matnr.
    ENDIF.
    gs_zmm_pur_proposal-maktx              =   gs_tab2-maktx.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_tab2-lifnr
      IMPORTING
        output = gs_tab2-lifnr.
    gs_zmm_pur_proposal-lifnr              =   gs_tab2-lifnr.
    gs_zmm_pur_proposal-name1              =   gs_tab2-name1.
    gs_zmm_pur_proposal-menge              =   gs_tab2-qty.
    gs_zmm_pur_proposal-meins              =   gs_tab2-meins.
    gs_zmm_pur_proposal-netpr              =   gs_tab2-netpr.
    gs_zmm_pur_proposal-waers              =   gs_tab2-waers.
    gs_zmm_pur_proposal-mwskz              =   gs_tab2-mwskz.
    gs_zmm_pur_proposal-tax_desc           =   gs_tab2-tax_desc.
    gs_zmm_pur_proposal-land_pr            =   gs_tab2-landed_price.
    gs_zmm_pur_proposal-land_value         =   gs_tab2-landed_value.
    gs_zmm_pur_proposal-last_basicpr       =   gs_tab2-last_pur_basic_price.
    gs_zmm_pur_proposal-last_landpr        =   gs_tab2-last_pur_land_price.
    gs_zmm_pur_proposal-business_planpr    =   gs_tab2-business_price.
    gs_zmm_pur_proposal-vari_last_pur_in   =   gs_tab2-vari_last_pur_inr.
    gs_zmm_pur_proposal-vari_last_pur_va   =   gs_tab2-vari_last_pur_val.
    gs_zmm_pur_proposal-vari_bus_pl_inr    =   gs_tab2-vari_business_plan_inr.
    gs_zmm_pur_proposal-vari_bus_pl_val    =   gs_tab2-vari_business_plan_val.
    gs_zmm_pur_proposal-opening_m1         =   gs_tab2-op_m1.
    gs_zmm_pur_proposal-requirment_m1      =   gs_tab2-req_m1.
    gs_zmm_pur_proposal-app_vend_m1        =   gs_tab2-app_vend_m1.
    gs_zmm_pur_proposal-other_vend_m1      =   gs_tab2-otr_vend_m1.
    gs_zmm_pur_proposal-closing_m1         =   gs_tab2-clos_m1.
    gs_zmm_pur_proposal-opening_m2         =   gs_tab2-op_m2.
    gs_zmm_pur_proposal-requirment_m2      =   gs_tab2-req_m2.
    gs_zmm_pur_proposal-app_vend_m2        =   gs_tab2-app_vend_m2.
    gs_zmm_pur_proposal-other_vend_m2      =   gs_tab2-otr_vend_m2.
    gs_zmm_pur_proposal-closing_m2         =   gs_tab2-clos_m2.
    gs_zmm_pur_proposal-opening_m3         =   gs_tab2-op_m3.
    gs_zmm_pur_proposal-requirment_m3      =   gs_tab2-req_m3.
    gs_zmm_pur_proposal-app_vend_m3        =   gs_tab2-app_vend_m3.
    gs_zmm_pur_proposal-other_vend_m3      =   gs_tab2-otr_vend_m3.
    gs_zmm_pur_proposal-closing_m3         =   gs_tab2-clos_m3.
    gs_zmm_pur_proposal-comp_data          =   gs_tab2-costformula.
    gs_zmm_pur_proposal-comp_data1         =   gs_tab2-comp_data_avl.
    gs_zmm_pur_proposal-line_txt           =   gs_tab2-text.
    gs_zmm_pur_proposal-remark             =   gs_tab2-remarks.
    gs_zmm_pur_proposal-releaser1          =   gs_tab2-releaser1.
    gv_pernr                               =   gs_tab2-releaser1.
    gs_zmm_pur_proposal-releaser_name1     =   gs_tab2-releaser_name1.
    gs_zmm_pur_proposal-releaser2          =   gs_tab2-releaser2.
    gs_zmm_pur_proposal-releaser_name2     =   gs_tab2-releaser_name2.
    gs_zmm_pur_proposal-releaser3          =   gs_tab2-releaser3.
    gs_zmm_pur_proposal-releaser_name3     =   gs_tab2-releaser_name3.
    gs_zmm_pur_proposal-releaser4          =   gs_tab2-releaser4.
    gs_zmm_pur_proposal-releaser_name4     =   gs_tab2-releaser_name4.
    gs_zmm_pur_proposal-ch_date            =   sy-datum .
    gs_zmm_pur_proposal-pr_user            =   gs_tab2-pr_user.
    gs_zmm_pur_proposal-pr_time            =                gs_tab2-pr_time.
    APPEND gs_zmm_pur_proposal TO gt_zmm_pur_proposal.
    CLEAR: gs_zmm_pur_proposal,gs_tab2.
  ENDLOOP.

  MODIFY zmm_pur_proposal FROM TABLE gt_zmm_pur_proposal.
  COMMIT WORK.
  PERFORM email_send_102.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gv_prno
    IMPORTING
      output = gv_prno.
  MESSAGE s011(zpp) WITH gv_prno." DISPLAY LIKE 'S'.
  WAIT UP TO 2 SECONDS.
  CLEAR gv_ans.
  lv_msg = 'Create Attachment'.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
     EXPORTING
       titlebar                     = lv_msg
       text_question               = 'Do you want to Create an attachment?'
       text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
       text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
       default_button               = '1'
       display_cancel_button        = ' '
       start_column                 = 75
       start_row                    = 5
       IMPORTING
         answer                       = gv_ans                 .
  IF gv_ans = 1.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gv_prno
      IMPORTING
        output = gv_prno.
    PERFORM refresh_102.
    CALL SCREEN 500.
  ELSE.
    LEAVE PROGRAM.
  ENDIF.
ENDFORM.                    " CHANGE_PROPOSAL

*&---------------------------------------------------------------------*
*&      Form  EMAIL_SEND_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EMAIL_SEND_102 .
  data :  lv_userid type pa0105-usrid_long.
   DATA: tab_lines LIKE sy-tabix,
        lv_desc   TYPE string,
        lv_p1     TYPE string,
        dd(2)     TYPE c,
        mm(2)     TYPE c,
        yy(4)     TYPE c,
        lv_date(10) type c,
        lv_ename    TYPE pa0001-ename,
        lv_vend_name TYPE lfa1-name1.
  CLEAR : lv_userid,w_document_data,doc_chng,objpack,objhead,zreceivers,i_body_msg,
          lv_desc,lv_p1,lv_date,lv_ename,lv_lifnr,lv_vend_name.
  SELECT SINGLE ename FROM pa0001 INTO lv_ename WHERE pernr = sy-uname.
  select single usrid_long into lv_userid
                               from pa0105
                               where pernr = gv_pernr "SY-UNAME
                               and endda ge sy-datum "
                               and subty = '0010'.
  if sy-subrc = 0.
    translate lv_userid to lower case.
    zreceivers-receiver = lv_userid.
    zreceivers-rec_type = 'U'.
    append zreceivers.
    CONCATENATE 'Proposal No' gv_prno 'Waiting for your Release.' INTO lv_desc SEPARATED BY SPACE.
    doc_chng-obj_name = 'SENDMAIL'.
    doc_chng-obj_descr = lv_desc.
    doc_chng-obj_langu = sy-langu.
    doc_chng-sensitivty = 'F'.
    dd = sy-datum+6(2).
    mm = sy-datum+4(2).
    yy = sy-datum+0(4).
    CONCATENATE dd '.' mm '.' yy INTO lv_date.
     CONCATENATE 'Proposal Number' gv_prno ',has been changed on dated:' lv_date
                                           '- is Waiting for your L1 Release.'
                                             INTO lv_p1 SEPARATED BY SPACE.
    perform build_body_of_mail_102
    using:
    lv_p1,
    '     ',
    'Thanks & Regards.',
     lv_ename.

    loop at i_body_msg into w_body_msg.
      append w_body_msg to objtxt.
      clear w_body_msg.
    endloop.
    objpack-transf_bin = space.
    objpack-head_start = 1.
    objpack-head_num   = 0.
    objpack-body_start = 1.
    describe table objtxt lines objpack-body_num.
    objpack-doc_type = 'RAW'.
    append objpack.

    describe table objtxt lines tab_lines.
    read table objtxt index tab_lines.
    doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

    call function 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      exporting
       document_data                    = doc_chng
       put_in_outbox                    = 'X'
       commit_work                      = 'X'
      tables
       packing_list                     = objpack
*   OBJECT_HEADER                    = OBJHEAD
*   CONTENTS_BIN                     = OBJBIN
       contents_txt                     =  objtxt
*   CONTENTS_HEX                     =
*   OBJECT_PARA                      =
*   OBJECT_PARB                      =
        receivers                        = zreceivers
     exceptions
       too_many_receivers               = 1
       document_not_sent                = 2
       document_type_not_exist          = 3
       operation_no_authorization       = 4
       parameter_error                  = 5
       x_error                          = 6
       enqueue_error                    = 7
       others                           = 8
              .
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
  endif.

endform.                    " EMAIL_SEND


*&---------------------------------------------------------------------*
*&      Form  build_body_of_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_MESSAGE  text
*----------------------------------------------------------------------*
form build_body_of_mail_102  using l_message.
  w_body_msg = l_message.
  append w_body_msg to i_body_msg.
  clear  w_body_msg.

ENDFORM.                    " EMAIL_SEND_102
FORM refresh_102 .
  REFRESH:gt_tab1,gt_tab2,gt_tab3,gt_ekpo,gt_pp,gt_user_list,gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
          list,list2,list3,list4,gt_a516,gt_zmm_pur_proposal.

  CLEAR: gs_tab1,gs_tab2,gs_tab22,gv_date,gv_ekgrp,gv_werks,gv_time,remarks,gs_tab2,gs_stxh,gs_a516, gs_pa0001,
         gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,g_mytable,gv_plantname,gv_eknam,
         flag,flag1,flag2,flag3,flag3_1,flag3_2,flag4,flag5,flag6,flag7,flag8,
         no,g_lineno,lv_curline,text,gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4,gv_releaser_name1,
         gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
         gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs,lv_index,list_box,list_box2,list_box3,list_box4,
         list_box5,list_box6.
ENDFORM.                    " REFRESH_102_

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC3' ITSELF
*CONTROLS: TC3 TYPE TABLEVIEW USING SCREEN 0103.

*&SPWIZARD: LINES OF TABLECONTROL 'TC3'
DATA:     g_tc3_lines  LIKE sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC3'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc3_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_tab3 LINES tc3-lines.
ENDMODULE.                    "TC3_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC3'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc3_get_lines OUTPUT.
  g_tc3_lines = sy-loopc.
ENDMODULE.                    "TC3_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC3'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc3_modify INPUT.
  MODIFY gt_tab3
    FROM gs_tab3
    INDEX tc3-current_line.
ENDMODULE.                    "TC3_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC3'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc3_mark INPUT.
  DATA: g_tc3_wa2 LIKE LINE OF gt_tab3.
  IF tc3-line_sel_mode = 1
  AND gs_tab3-flag = 'X'.
    LOOP AT gt_tab3 INTO g_tc3_wa2
      WHERE flag = 'X'.
      g_tc3_wa2-flag = ''.
      MODIFY gt_tab3
        FROM g_tc3_wa2
        TRANSPORTING flag.
    ENDLOOP.
  ENDIF.
  MODIFY gt_tab3
    FROM gs_tab3
    INDEX tc3-current_line
    TRANSPORTING flag.
ENDMODULE.                    "TC3_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC3'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc3_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC3'
                              'GT_TAB3'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC3_USER_COMMAND INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  CASE sy-ucomm .

    WHEN 'DISPLAY'.
      REFRESH: gt_zmm_pur_proposal.
      SELECT * FROM zmm_pur_proposal INTO TABLE gt_zmm_pur_proposal WHERE prno = gv_prno.
      IF  sy-subrc = 0.
        PERFORM display_103.

      ELSE.
        MESSAGE 'Proposal Number not found' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    WHEN 'PTERM'.
      GET CURSOR LINE g_lineno.
      lv_curline  = g_lineno.
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
      refresh g_mytable.
      READ TABLE gt_tab3 INTO gs_tab3 INDEX lv_curline.
      IF gs_tab3-text IS NOT INITIAL.
        REFRESH g_mytable.
        text = gs_tab3-text.
        APPEND text TO g_mytable.
        DELETE g_mytable WHERE table_line IS INITIAL.
*        get_text_as_r3table
        CALL METHOD text_editor->set_text_as_r3table
          EXPORTING
            table = g_mytable.
      ENDIF.
      IF gs_tab3-text IS INITIAL.
         CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
*       CALL METHOD cl_gui_cfw=>flush
*        EXCEPTIONS
*          cntl_system_error = 1
*          cntl_error        = 2
*          OTHERS            = 3.
*
*      CALL METHOD editor_container->free.
*      CALL METHOD text_editor->free.
       MESSAGE 'No Data found for selected Line' TYPE 'I'.
*
    ENDIF.

*    WHEN 'CLOSE_TEXT'.
*
*      IF flag = 'X'.
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
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_103 .

  REFRESH: gt_tab3.
  CLEAR : gs_tab3.
  LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal .
    gv_ekgrp          = gs_zmm_pur_proposal-ekgrp.
    gv_eknam          = gs_zmm_pur_proposal-eknam.
    gv_werks          = gs_zmm_pur_proposal-werks.
    gv_plantname      = gs_zmm_pur_proposal-plantname.
    gv_date           = gs_zmm_pur_proposal-pr_date.
    gv_chdate           = gs_zmm_pur_proposal-ch_date.
    gv_user           = gs_zmm_pur_proposal-pr_user.
    gv_time           = gs_zmm_pur_proposal-pr_time .
    gs_tab3-chk                     =  gs_zmm_pur_proposal-app_vendor .
*       gv_prno                         =  gs_zmm_pur_proposal-prno.
    gs_tab3-gjahr                   =  gs_zmm_pur_proposal-gjahr.
    gs_tab3-pr_date                 =  gs_zmm_pur_proposal-pr_date.
    gs_tab3-ekgrp                   =  gs_zmm_pur_proposal-ekgrp.
    gs_tab3-eknam                   =  gs_zmm_pur_proposal-eknam.
    gs_tab3-bukrs                   =  gs_zmm_pur_proposal-bukrs .
    gs_tab3-werks                   =  gs_zmm_pur_proposal-werks.
    gs_tab3-plantname               =  gs_zmm_pur_proposal-plantname.
    gs_tab3-erdat                   =  gs_zmm_pur_proposal-erdat.
    gs_zmm_pur_proposal-pr_itemno   = gs_zmm_pur_proposal-pr_itemno / 10.
    gs_tab3-ebelp                   =  gs_zmm_pur_proposal-pr_itemno .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-matnr
      IMPORTING
        output = gs_zmm_pur_proposal-matnr.
    gs_tab3-matnr                   =  gs_zmm_pur_proposal-matnr.
    gs_tab3-maktx                   =  gs_zmm_pur_proposal-maktx .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-lifnr
      IMPORTING
        output = gs_zmm_pur_proposal-lifnr.
    gs_tab3-lifnr                   =  gs_zmm_pur_proposal-lifnr.
    gs_tab3-name1                   =  gs_zmm_pur_proposal-name1.
    gs_tab3-qty                     =  gs_zmm_pur_proposal-menge .
    gs_tab3-meins                   =  gs_zmm_pur_proposal-meins.
    gs_tab3-netpr                   =  gs_zmm_pur_proposal-netpr.
    gs_tab3-waers                   =  gs_zmm_pur_proposal-waers .
    gs_tab3-mwskz                   =  gs_zmm_pur_proposal-mwskz.
    gs_tab3-tax_desc                =  gs_zmm_pur_proposal-tax_desc.
    gs_tab3-landed_price            =  gs_zmm_pur_proposal-land_pr    .
    gs_tab3-landed_value            =  gs_zmm_pur_proposal-land_value.
    gs_tab3-last_pur_basic_price    =  gs_zmm_pur_proposal-last_basicpr.
    gs_tab3-last_pur_land_price     =  gs_zmm_pur_proposal-last_landpr .
    gs_tab3-business_price          =  gs_zmm_pur_proposal-business_planpr.
    gs_tab3-vari_last_pur_inr       =  gs_zmm_pur_proposal-vari_last_pur_in.
    gs_tab3-vari_last_pur_val       =  gs_zmm_pur_proposal-vari_last_pur_va.
    gs_tab3-vari_business_plan_inr  =  gs_zmm_pur_proposal-vari_bus_pl_inr.
    gs_tab3-vari_business_plan_val  =  gs_zmm_pur_proposal-vari_bus_pl_val.
    gs_tab3-op_m1                   =  gs_zmm_pur_proposal-opening_m1.
    gs_tab3-req_m1                  =  gs_zmm_pur_proposal-requirment_m1.
    gs_tab3-app_vend_m1             =  gs_zmm_pur_proposal-app_vend_m1.
    gs_tab3-otr_vend_m1             =  gs_zmm_pur_proposal-other_vend_m1.
    gs_tab3-clos_m1                 =  gs_zmm_pur_proposal-closing_m1.
    gs_tab3-op_m2                   =  gs_zmm_pur_proposal-opening_m2.
    gs_tab3-req_m2                  =  gs_zmm_pur_proposal-requirment_m2.
    gs_tab3-app_vend_m2             =  gs_zmm_pur_proposal-app_vend_m2.
    gs_tab3-otr_vend_m2             =  gs_zmm_pur_proposal-other_vend_m2.
    gs_tab3-clos_m2                 =  gs_zmm_pur_proposal-closing_m2.
    gs_tab3-op_m3                   =  gs_zmm_pur_proposal-opening_m3.
    gs_tab3-req_m3                  =  gs_zmm_pur_proposal-requirment_m3.
    gs_tab3-app_vend_m3             =  gs_zmm_pur_proposal-app_vend_m3.
    gs_tab3-otr_vend_m3             =  gs_zmm_pur_proposal-other_vend_m3.
    gs_tab3-clos_m3                 =  gs_zmm_pur_proposal-closing_m3.
    gs_tab3-costformula             =  gs_zmm_pur_proposal-comp_data.
    gs_tab3-comp_data_avl           =  gs_zmm_pur_proposal-comp_data1.
    gs_tab3-text                    =  gs_zmm_pur_proposal-line_txt.
    gs_tab3-remarks                 =  gs_zmm_pur_proposal-remark.
    old_remarks                     =  gs_zmm_pur_proposal-remark.

*    releaser_name1                  =  gs_zmm_pur_proposal-releaser_name1.

    gs_tab3-pr_date                 =  gs_zmm_pur_proposal-erdat.
    gs_tab3-ch_date                 =  sy-datum.
    gs_tab3-pr_user                 =  gs_zmm_pur_proposal-pr_user.
    gs_tab3-pr_time                 =         gs_zmm_pur_proposal-pr_time .
    APPEND gs_tab3 TO gt_tab3.
    CLEAR :gs_tab3,gs_zmm_pur_proposal,remarks.
  ENDLOOP.
  CLEAR :gs_zmm_pur_proposal,gs_tab3,releaser_name1,releaser_name2,releaser_name3,releaser_name4.
  READ TABLE gt_zmm_pur_proposal INTO gs_zmm_pur_proposal INDEX 1.
  IF sy-subrc = 0.
    gs_tab3-releaser1               =  gs_zmm_pur_proposal-releaser1.
    gs_tab3-releaser_name1          =  gs_zmm_pur_proposal-releaser_name1.
    CONCATENATE gs_tab3-releaser1 gs_tab3-releaser_name1 INTO releaser_name1 SEPARATED BY space.
    SHIFT releaser_name1 LEFT DELETING LEADING '0'.
    gs_tab3-releaser2               =  gs_zmm_pur_proposal-releaser2.
    gs_tab3-releaser_name2          =  gs_zmm_pur_proposal-releaser_name2.
    CONCATENATE gs_tab3-releaser2 gs_tab3-releaser_name2 INTO releaser_name2 SEPARATED BY space.
    SHIFT releaser_name2 LEFT DELETING LEADING '0'.
    gs_tab3-releaser3               =  gs_zmm_pur_proposal-releaser3.
    gs_tab3-releaser_name3          =  gs_zmm_pur_proposal-releaser_name3.
    CONCATENATE gs_tab3-releaser3 gs_tab3-releaser_name3 INTO releaser_name3 SEPARATED BY space.
    SHIFT releaser_name3 LEFT DELETING LEADING '0'.
    gs_tab3-releaser4               =  gs_zmm_pur_proposal-releaser4 .
    gs_tab3-releaser_name4          =  gs_zmm_pur_proposal-releaser_name4.
    CONCATENATE gs_tab3-releaser4 gs_tab3-releaser_name4 INTO releaser_name4 SEPARATED BY space.
    SHIFT releaser_name4 LEFT DELETING LEADING '0'.
  ENDIF.
  remarks = old_remarks.
  CALL METHOD remarks_editor->set_textstream
    EXPORTING
      text = remarks.


ENDFORM.                    " DISPLAY_103
*&---------------------------------------------------------------------*
*&      Module  F4_TCL_GVPRNO103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl_gvprno103 INPUT.
  SELECT prno FROM zmm_pur_proposal INTO TABLE lt_f4_prno.
  SORT lt_f4_prno BY prno.
  DELETE ADJACENT DUPLICATES FROM lt_f4_prno COMPARING prno.
  REFRESH it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'GV_PRNO'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0103'
     dynprofield = 'GV_PRNO'
     TABLES
     value_tab   = lt_f4_prno
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
    gv_prno = it_ret-fieldval.
  ENDIF.
ENDMODULE.                 " F4_TCL_GVPRNO103  INPUT

*&---------------------------------------------------------------------*
*&      Module  COPY_DATA103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE copy_data104 INPUT.
  CLEAR: g_lineno.
  IF gv_okcode = 'COPY'.
    GET CURSOR LINE g_lineno.
    READ TABLE gt_tab4 INTO gs_tab4 INDEX g_lineno.
    IF sy-subrc = 0.
      gs_tab4-ebelp =   gs_tab4-ebelp + 1.
      gs_tab4-netpr = ' '.
      gs_tab4-landed_price = ' '.
*  APPEND GS_TAB1 TO GT_TAB1 WHERE INDEX = GS_TAB1-EBELP .
      INSERT gs_tab4 INTO gt_tab4 INDEX gs_tab4-ebelp.
      CLEAR gs_tab4.
      LOOP AT gt_tab4 INTO gs_tab4.
        gs_tab4-ebelp = sy-tabix .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_tab4-ebelp
          IMPORTING
            output = gs_tab4-ebelp.
        MODIFY gt_tab4 FROM gs_tab4.
        CLEAR: gs_tab4.
      ENDLOOP.
      CLEAR : gv_okcode,sy-ucomm.
    ENDIF.
  ENDIF.
ENDMODULE.                 " COPY_DATA103  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TC1_LIFNR104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tc1_lifnr104 INPUT.

  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.

  CLEAR : selline ,selindex.
  GET CURSOR FIELD gs_tab4-lifnr LINE selline.
*  selindex = tc1-top_line + selline - 1.   "
  READ TABLE gt_tab4 INTO gs_tab4
                            INDEX selindex.
*  IF sy-subrc = 0.
  REFRESH:lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                           WHERE lifnr = lt_lfb1-lifnr.
  ENDIF.
  REFRESH:it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'LIFNR'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0104'
     dynprofield = 'GS_TAB4-LIFNR'
     TABLES
     value_tab   = lt_f4_lifnr
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
    gs_tab4-lifnr = it_ret-fieldval.
    CLEAR: it_ret,lt_f4_lifnr.
    MODIFY gt_tab4 FROM gs_tab4 INDEX selline TRANSPORTING lifnr.
  ENDIF.

ENDMODULE.                 " F4_TC1_LIFNR104  INPUT

*&---------------------------------------------------------------------*
*&      Module  F4_TCL-MWSKZ104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-mwskz104 INPUT.
  SELECT mwskz text1 FROM t007s INTO TABLE lt_f4_mwskz WHERE spras = 'EN'.

  GET CURSOR FIELD gs_tab4-mwskz LINE selline.
  READ TABLE gt_tab4 INTO gs_tab4
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MWSKZ'
      dynpnr          = '104'
      dynprofield     = 'GS_TAB4-MWSKZ'
      value_org       = 'S'
    TABLES
      value_tab       = lt_f4_mwskz
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab4-mwskz = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
ENDMODULE.                 " F4_TCL-MWSKZ104  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-COSTFORMULA104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-costformula104 INPUT.
  IF flag4 = ' '.
    gs_compdata-field1 = 'Yes'.
    APPEND gs_compdata TO gt_compdata.
    CLEAR gs_compdata.
    gs_compdata-field1 = 'No'.
    APPEND gs_compdata TO gt_compdata.
    flag4 = 'X'.
  ENDIF.

  CLEAR: it_ret.
  GET CURSOR FIELD gs_tab4-costformula LINE selline.
  READ TABLE gt_tab4 INTO gs_tab4
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COSTFORMULA'
      dynpnr          = '104'
      dynprofield     = 'GS_TAB4-COSTFORMULA'
      value_org       = 'S'
    TABLES
      value_tab       = gt_compdata
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab4-costformula = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
  IF selline NE 0.
    MODIFY gt_tab4 FROM gs_tab4 INDEX selline TRANSPORTING costformula .
  ENDIF.
ENDMODULE.                 " F4_TCL-COSTFORMULA104  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TCL-COMP_DATA_AVL104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcl-comp_data_avl104 INPUT.
  IF flag5 = ' '.
    gs_compdata1-field1 = 'Yes'.
    APPEND gs_compdata1 TO gt_compdata1.
    CLEAR gs_compdata1.
    gs_compdata1-field1 = 'No'.
    APPEND gs_compdata1 TO gt_compdata1.
    flag5 = 'X'.
  ENDIF.

  CLEAR: it_ret.
  GET CURSOR FIELD gs_tab3-comp_data_avl LINE selline.
  READ TABLE gt_tab3 INTO gs_tab3
                          INDEX selindex.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COMP_DATA_AVL'
      dynpnr          = '104'
      dynprofield     = 'GS_TAB3-COMP_DATA_AVL'
      value_org       = 'S'
    TABLES
      value_tab       = gt_compdata1
      return_tab      = it_ret
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3.
  ELSE.
    READ TABLE it_ret INDEX 1.
    gs_tab3-comp_data_avl = it_ret-fieldval.
    CLEAR: it_ret.
  ENDIF.
  IF selline NE 0.
    MODIFY gt_tab3 FROM gs_tab3 INDEX selline TRANSPORTING comp_data_avl.
  ENDIF.
ENDMODULE.                 " F4_TCL-COMP_DATA_AVL104  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_TC1_WAERS104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tc1_waers104 INPUT.

  CLEAR : selline ,selindex.
  GET CURSOR FIELD gs_tab4-waers LINE selline.
*  selindex = tc1-top_line + selline - 1.   "
  READ TABLE gt_tab4 INTO gs_tab4
                            INDEX selindex.
*  IF sy-subrc = 0.
  SELECT waers ltext FROM tcurt INTO TABLE lt_f4_waers WHERE spras = 'EN'.
  REFRESH:it_ret.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
   EXPORTING
     retfield    = 'WAERS'
     value_org   = 'S'
*       dynpprog    = sy-repid
     dynpnr      = '0104'
     dynprofield = 'GS_TAB4-WAERS'
     TABLES
     value_tab   = lt_f4_waers
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
    gs_tab4-waers = it_ret-fieldval.
    CLEAR: it_ret,lt_f4_lifnr.
  ENDIF.
ENDMODULE.                 " F4_TC1_WAERS104  INPUT

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TC4' ITSELF
*CONTROLS: tc4 TYPE TABLEVIEW USING SCREEN 0104.

*&SPWIZARD: LINES OF TABLECONTROL 'TC4'
DATA:     g_tc4_lines  LIKE sy-loopc.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC4'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tc4_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_tab4 LINES tc4-lines.
ENDMODULE.                    "TC4_CHANGE_TC_ATTR OUTPUT

*&SPWIZARD: OUTPUT MODULE FOR TC 'TC4'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GET LINES OF TABLECONTROL
MODULE tc4_get_lines OUTPUT.
  g_tc4_lines = sy-loopc.
ENDMODULE.                    "TC4_GET_LINES OUTPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC4'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MODIFY TABLE
MODULE tc4_modify INPUT.
  MODIFY gt_tab4
    FROM gs_tab4
    INDEX tc4-current_line.
ENDMODULE.                    "TC4_MODIFY INPUT

*&SPWIZARD: INPUT MODUL FOR TC 'TC4'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: MARK TABLE
MODULE tc4_mark INPUT.
  DATA: g_tc4_wa2 LIKE LINE OF gt_tab4.
  IF tc4-line_sel_mode = 1
  AND gs_tab4-flag = 'X'.
    LOOP AT gt_tab4 INTO g_tc4_wa2
      WHERE flag = 'X'.
      g_tc4_wa2-flag = ''.
      MODIFY gt_tab4
        FROM g_tc4_wa2
        TRANSPORTING flag.
    ENDLOOP.
  ENDIF.
  MODIFY gt_tab4
    FROM gs_tab4
    INDEX tc4-current_line
    TRANSPORTING flag.
ENDMODULE.                    "TC4_MARK INPUT

*&SPWIZARD: INPUT MODULE FOR TC 'TC4'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: PROCESS USER COMMAND
MODULE tc4_user_command INPUT.
  ok_code = sy-ucomm.
  PERFORM user_ok_tc USING    'TC4'
                              'GT_TAB4'
                              'FLAG'
                     CHANGING ok_code.
  sy-ucomm = ok_code.
ENDMODULE.                    "TC4_USER_COMMAND INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0104 INPUT.
  CLEAR gv_okcode.
  gv_okcode = sy-ucomm.
*  CLEAR sy-ucomm.

  CASE gv_okcode .
    WHEN 'DISPLAY'.
      REFRESH: gt_zmm_pur_proposal.
      SELECT * FROM zmm_pur_proposal INTO TABLE gt_zmm_pur_proposal WHERE prno = gv_prno.
      IF sy-subrc = 0.
        PERFORM display_104.
      ELSE.
        MESSAGE 'Proposal Number not found' TYPE 'I' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    WHEN ' '.
      CLEAR : flag_final,flag3_1,flag3_3,flag7.
      SELECT SINGLE eknam FROM t024 INTO gv_eknam WHERE ekgrp = gv_ekgrp.
      SELECT SINGLE name1 FROM t001w INTO gv_plantname WHERE werks = gv_werks.
      GET CURSOR LINE g_lineno.
      IF tc4-lines GE 10 AND tc4-top_line NE 1.
        CLEAR: g_lineno,gs_tab2.
        g_lineno = tc4-lines.
      ENDIF.
      READ TABLE gt_tab4 INTO gs_tab4 INDEX g_lineno.
      IF sy-subrc = 0.
*{   REPLACE        SBXK900030                                        1
*\        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\          EXPORTING
*\            input  = gs_tab4-matnr
*\          IMPORTING
*\            output = gs_tab4-matnr.
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
          input  = gs_tab4-matnr
        IMPORTING
          output = gs_tab4-matnr.
*}   REPLACE
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
          PERFORM check_material_104.
        ENDIF.
        PERFORM check_vendor_104.
        PERFORM check_newprice_landprice_104.
        PERFORM final_data_104.
      ENDIF.
      PERFORM get_auth_list.

    WHEN 'PTERM'.
      GET CURSOR LINE g_lineno.
      READ TABLE gt_tab4 INTO gs_tab4 INDEX g_lineno.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
        PERFORM create_text_104.
      ENDIF.

      IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
        PERFORM create_text_104_1.
      ENDIF.

    WHEN 'SAVE_TEXT'.

      IF flag = 'X'.
        PERFORM save_line_text_104.
      ELSE.
        MESSAGE text-013 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN 'CHECK'.
       CLEAR : flag_final,flag3_1,flag3_3,flag7.
       IF gt_tab4 IS INITIAL.
        MESSAGE text-014 TYPE 'S' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 100.
      ENDIF.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'..
        PERFORM check_material_104.
        PERFORM check_final_104.
      ENDIF.
      IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
        PERFORM check_final_104_1.
      ENDIF.
      PERFORM check_rel_remks_104.

    WHEN 'SAVE'. "create proposal                                    "ravi104

      IF flag6 = 'X'.
        IF gt_tab4 IS NOT INITIAL.
          CLEAR: flag3,flag3_1,flag3_2.
          REFRESH gt_tab5.
          gt_tab5[] = gt_tab4[].
          IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'..
            PERFORM save_check_104.
          ENDIF.
          IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
            refresh : gt_tab5.
            gt_tab5[] = gt_tab4[].
            PERFORM save_check_104_1.
          ENDIF.
          CLEAR remarks.
          CALL METHOD remarks_editor->get_textstream
            IMPORTING
              text = remarks.
          CALL METHOD cl_gui_cfw=>flush
            EXCEPTIONS
              cntl_system_error = 1
              cntl_error        = 2
              OTHERS            = 3.
          PERFORM check_releaser_101.
          IF flag3 NE 'X'.
            MESSAGE text-015 TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSEIF flag7 = 'X'.
            PERFORM screen_edit104.
            CLEAR flag7.
            MESSAGE 'Check Material,Vendor,Qty,New Basic Price,Landed Price' TYPE 'S' DISPLAY LIKE 'E'.
          ELSEIF flag3_1 EQ 'X'.
            PERFORM screen_edit104.
            MESSAGE 'Check Material,Last Purchase and Business Plan Price' TYPE 'S' DISPLAY LIKE 'E'.
            CLEAR flag3_1.
          ELSEIF flag3_2 EQ 'X'.
            PERFORM screen_edit104.
            CLEAR flag3_2.
          ELSEIF flag3_3 EQ 'X'.
            PERFORM screen_edit104.
            CLEAR flag3_3.
          ELSEIF flag3_4 EQ 'X'.
            PERFORM screen_edit104.
            CLEAR flag3_4.
          ELSEIF remarks IS INITIAL.
            MESSAGE text-018 TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ELSE.
            IF gv_ekgrp NOT BETWEEN 'B06' AND 'B07'.
              PERFORM compare_sel_new_basic_price104.      "selected line check with other
              CLEAR flag3_4.
            ENDIF.
            lv_msg = 'Create With Reference'.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
               titlebar                     = lv_msg
                text_question               = 'Do you want to Create Proposal With Reference?'
               text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
               text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
               default_button               = '1'
               display_cancel_button        = ' '
               start_column                 = 55
               start_row                    = 3
             IMPORTING
               answer                       = gv_ans                 .
            IF gv_ans = 2.
              LOOP AT SCREEN.
                screen-input = 0.
                screen-active = 0.
                MODIFY SCREEN.
              ENDLOOP.
            ELSE.
              PERFORM create_from_existing_104.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE text-019 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_104 .

  REFRESH: gt_tab4.
  CLEAR : gs_tab4.
  LOOP AT gt_zmm_pur_proposal INTO gs_zmm_pur_proposal .
    gv_ekgrp                     = gs_zmm_pur_proposal-ekgrp.
    gv_eknam                     = gs_zmm_pur_proposal-eknam.
    gv_werks                     = gs_zmm_pur_proposal-werks.
    gv_plantname                 = gs_zmm_pur_proposal-plantname.
    gv_date                      = sy-datum.
*    gs_tab4-chk                     =  gs_zmm_pur_proposal-app_vendor .
*       gv_prno                         =  gs_zmm_pur_proposal-prno.
    gs_tab4-gjahr                   =  gs_zmm_pur_proposal-gjahr.
    gs_tab4-pr_date                 =  gs_zmm_pur_proposal-pr_date.
    gs_tab4-ekgrp                   =  gs_zmm_pur_proposal-ekgrp.
    gs_tab4-eknam                   =  gs_zmm_pur_proposal-eknam.
    gs_tab4-bukrs                   =  gs_zmm_pur_proposal-bukrs .
    gs_tab4-werks                   =  gs_zmm_pur_proposal-werks.
    gs_tab4-plantname               =  gs_zmm_pur_proposal-plantname.
    gs_tab4-erdat                   =  gs_zmm_pur_proposal-erdat.
    gs_zmm_pur_proposal-pr_itemno   = gs_zmm_pur_proposal-pr_itemno / 10.
    gs_tab4-ebelp                   =  gs_zmm_pur_proposal-pr_itemno .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-matnr
      IMPORTING
        output = gs_zmm_pur_proposal-matnr.
    gs_tab4-matnr                   =  gs_zmm_pur_proposal-matnr.
    gs_tab4-maktx                   =  gs_zmm_pur_proposal-maktx .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gs_zmm_pur_proposal-lifnr
      IMPORTING
        output = gs_zmm_pur_proposal-lifnr.
    gs_tab4-lifnr                   =  gs_zmm_pur_proposal-lifnr.
    gs_tab4-name1                   =  gs_zmm_pur_proposal-name1.
    gs_tab4-qty                     =  gs_zmm_pur_proposal-menge .
    gs_tab4-meins                   =  gs_zmm_pur_proposal-meins.
    gs_tab4-netpr                   =  gs_zmm_pur_proposal-netpr.
    gs_tab4-waers                   =  gs_zmm_pur_proposal-waers .
    gs_tab4-mwskz                   =  gs_zmm_pur_proposal-mwskz.
    gs_tab4-tax_desc                =  gs_zmm_pur_proposal-tax_desc.
    gs_tab4-landed_price            =  gs_zmm_pur_proposal-land_pr    .
    gs_tab4-landed_value            =  gs_zmm_pur_proposal-land_value.
    gs_tab4-last_pur_basic_price    =  gs_zmm_pur_proposal-last_basicpr.
    gs_tab4-last_pur_land_price     =  gs_zmm_pur_proposal-last_landpr .
    gs_tab4-business_price          =  gs_zmm_pur_proposal-business_planpr.
    gs_tab4-vari_last_pur_inr       =  gs_zmm_pur_proposal-vari_last_pur_in.
    gs_tab4-vari_last_pur_val       =  gs_zmm_pur_proposal-vari_last_pur_va.
    gs_tab4-vari_business_plan_inr  =  gs_zmm_pur_proposal-vari_bus_pl_inr.
    gs_tab4-vari_business_plan_val  =  gs_zmm_pur_proposal-vari_bus_pl_val.
    gs_tab4-op_m1                   =  gs_zmm_pur_proposal-opening_m1.
    gs_tab4-req_m1                  =  gs_zmm_pur_proposal-requirment_m1.
    gs_tab4-app_vend_m1             =  gs_zmm_pur_proposal-app_vend_m1.
    gs_tab4-otr_vend_m1             =  gs_zmm_pur_proposal-other_vend_m1.
    gs_tab4-clos_m1                 =  gs_zmm_pur_proposal-closing_m1.
    gs_tab4-op_m2                   =  gs_zmm_pur_proposal-opening_m2.
    gs_tab4-req_m2                  =  gs_zmm_pur_proposal-requirment_m2.
    gs_tab4-app_vend_m2             =  gs_zmm_pur_proposal-app_vend_m2.
    gs_tab4-otr_vend_m2             =  gs_zmm_pur_proposal-other_vend_m2.
    gs_tab4-clos_m2                 =  gs_zmm_pur_proposal-closing_m2.
    gs_tab4-op_m3                   =  gs_zmm_pur_proposal-opening_m3.
    gs_tab4-req_m3                  =  gs_zmm_pur_proposal-requirment_m3.
    gs_tab4-app_vend_m3             =  gs_zmm_pur_proposal-app_vend_m3.
    gs_tab4-otr_vend_m3             =  gs_zmm_pur_proposal-other_vend_m3.
    gs_tab4-clos_m3                 =  gs_zmm_pur_proposal-closing_m3.
    gs_tab4-costformula             =  gs_zmm_pur_proposal-comp_data.
    gs_tab4-comp_data_avl           =  gs_zmm_pur_proposal-comp_data1.
    gs_tab4-text                    =  gs_zmm_pur_proposal-line_txt.
    gs_tab4-remarks                 =  gs_zmm_pur_proposal-remark.
    old_remarks                     =  gs_zmm_pur_proposal-remark.
    gs_tab4-releaser1               =  gs_zmm_pur_proposal-releaser1.
    gs_tab4-releaser_name1          =  gs_zmm_pur_proposal-releaser_name1.
    releaser_name1              =  gs_zmm_pur_proposal-releaser_name1.
    gs_tab4-releaser2               =  gs_zmm_pur_proposal-releaser2.
    gs_tab4-releaser_name2          =  gs_zmm_pur_proposal-releaser_name2.
    releaser_name2              =  gs_zmm_pur_proposal-releaser_name2.
    gs_tab4-releaser3               =  gs_zmm_pur_proposal-releaser3.
    gs_tab4-releaser_name3          =  gs_zmm_pur_proposal-releaser_name3.
    releaser_name3              =  gs_zmm_pur_proposal-releaser_name3.
    gs_tab4-releaser4               =  gs_zmm_pur_proposal-releaser4 .
    gs_tab4-releaser_name4          =  gs_zmm_pur_proposal-releaser_name4.
    releaser_name4              =  gs_zmm_pur_proposal-releaser_name4.

    APPEND gs_tab4 TO gt_tab4.
    CLEAR :gs_tab4,gs_zmm_pur_proposal,remarks.
  ENDLOOP.
  remarks = old_remarks.
  CALL METHOD remarks_editor->set_textstream
    EXPORTING
      text = remarks.
  gv_user = sy-uname.
  gv_time = sy-timlo.
  flag9 = 'X'.

ENDFORM.                    " DISPLAY_104
*&---------------------------------------------------------------------*
*&      Form  CHECK_MATERIAL_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_material_104 .
  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  REFRESH: lt_matnr.
*{   REPLACE        SBXK900030                                        1
*\  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\    EXPORTING
*\      input  = gs_tab4-matnr
*\    IMPORTING
*\      output = gs_tab4-matnr.
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
      input  = gs_tab4-matnr
    IMPORTING
      output = gs_tab4-matnr.

*}   REPLACE

  IF gs_tab4-matnr IS NOT INITIAL.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
    CLEAR ls_matnr.
    READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = gs_tab4-matnr.
    IF sy-subrc NE 0.
      MESSAGE i004(zpp) WITH gs_tab4-matnr gv_ekgrp DISPLAY LIKE 'E'.
      flag_final = 'X'.
      EXIT.
    ENDIF.
    IF lt_matnr IS INITIAL.
      MESSAGE i004(zpp) WITH gs_tab4-matnr gv_ekgrp DISPLAY LIKE 'E'.
      flag_final = 'X'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_MATERIAL_104
*&---------------------------------------------------------------------*
*&      Form  CHECK_VENDOR_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vendor_104 .
  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  CLEAR: lv_matnr,flag3_3.
  lv_lifnr = gs_tab4-lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_tab4-lifnr
    IMPORTING
      output = gs_tab4-lifnr.
  REFRESH: lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                           WHERE lifnr = lt_lfb1-lifnr.
    SORT lt_f4_lifnr BY lifnr.
    READ TABLE lt_f4_lifnr INTO ls_f4_lifnr WITH KEY lifnr = gs_tab4-lifnr.
    IF sy-subrc NE 0.
      MESSAGE i008(zpp) WITH gs_tab4-lifnr gv_werks DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

  IF lt_f4_lifnr IS INITIAL.
    MESSAGE i008(zpp) WITH gs_tab4-lifnr gv_werks DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDFORM.                    " CHECK_VENDOR_104
*&---------------------------------------------------------------------*
*&      Form  CHECK_NEWPRICE_LANDPRICE_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_newprice_landprice_104 .

  IF gs_tab4-qty IS INITIAL.
    MESSAGE i016(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  ENDIF.
  IF gs_tab4-netpr IS INITIAL.
    MESSAGE i017(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
     flag_final = 'X'.
    EXIT.
  ENDIF.
  IF gs_tab4-landed_price IS INITIAL.
    MESSAGE i018(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
    EXIT.
  ENDIF.
   If gv_ekgrp not between 'B05' and 'B06'.
  if gs_tab4-business_price = 0.
    message i001(zpp) with gs_tab4-matnr gs_tab4-business_price display like 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
  endif.
  endif.

  if gv_ekgrp not between 'B05' and 'B06'.
  if gs_tab4-mwskz IS INITIAL.
    message i019(zpp) with gs_tab4-matnr display like 'E'.
    flag3_3 = 'X'.
    flag_final = 'X'.
  endif.
  endif.
*  IF gv_ekgrp not between 'B05' and 'B06'.
   IF gs_tab4-netpr > gs_tab4-landed_price.
    MESSAGE i013(zpp) WITH gs_tab4-netpr gs_tab4-landed_price DISPLAY LIKE 'E'.
    flag_final = 'X'.
     flag3_3 = 'X'.
    EXIT.
  ENDIF.
ENDFORM.                    " CHECK_NEWPRICE_LANDPRICE_104
*&---------------------------------------------------------------------*
*&      Form  FINAL_DATA_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_data_104 .
  translate gs_tab4-waers to upper case.
  translate gs_tab4-mwskz to upper case.
  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    GET CURSOR LINE g_lineno.
    CLEAR gs_tab1.
    READ TABLE gt_tab4 INTO gs_tab4 INDEX g_lineno.
    gs_tab4-matnr = ' '.
    gs_tab4-maktx = gs_tab4-maktx.
  ENDIF.

  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab4-matnr
*\      IMPORTING
*\        output = gs_tab4-matnr.
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
        input  = gs_tab4-matnr
      IMPORTING
        output = gs_tab4-matnr.

*}   REPLACE
    SELECT SINGLE maktx FROM makt INTO lv_maktx WHERE matnr = gs_tab4-matnr.
    gs_tab4-maktx = lv_maktx.
  ENDIF.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gs_tab4-lifnr
    IMPORTING
      output = gs_tab4-lifnr.
  SELECT SINGLE name1 FROM lfa1 INTO lv_name1 WHERE lifnr = gs_tab4-lifnr.
  gs_tab4-name1 = lv_name1.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_tab4-lifnr
    IMPORTING
      output = gs_tab4-lifnr.
  gs_tab4-landed_value = ( gs_tab4-landed_price * gs_tab4-qty )." / 100.
  REFRESH: gt_a516.
  SELECT kschl
         werks
         matnr
         datbi
         datab
         knumh FROM a516 INTO TABLE gt_a516
          WHERE kschl = 'ZBPL' AND werks = gv_werks AND matnr = gs_tab4-matnr
                AND datbi GT gv_date AND datab LE gv_date.
  IF sy-subrc = 0.
    READ TABLE gt_a516 INTO gs_a516 INDEX 1."datab le gv_date.
    IF sy-subrc = 0.
      CLEAR lv_kbetr.
      SELECT SINGLE kbetr FROM konp INTO lv_kbetr WHERE knumh = gs_a516-knumh.
      IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
        gs_tab4-business_price = lv_kbetr.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    gs_tab4-business_price = ' '.
  ENDIF.

  SELECT SINGLE text1 FROM t007s INTO gs_tab4-tax_desc WHERE spras = 'EN' AND mwskz = gs_tab4-mwskz.
  REFRESH:gt_ekpo.
  SELECT ebeln
         ebelp
         aedat
         matnr
         werks
         netpr
    FROM ekpo INTO TABLE gt_ekpo WHERE matnr = gs_tab4-matnr AND werks = gv_werks.
  IF gt_ekpo IS NOT INITIAL.
    IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
      SORT gt_ekpo DESCENDING BY aedat.
      READ TABLE gt_ekpo INTO gs_ekpo INDEX 1.
      IF sy-subrc = 0.
        gs_tab4-last_pur_basic_price = gs_ekpo-netpr_i.
        CLEAR gs_ekpo.
      ELSE.
        gs_tab4-last_pur_basic_price = ' '.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gv_ekgrp BETWEEN 'B05' AND 'B06'.
    gs_tab4-last_pur_basic_price = ' '.
  ENDIF.
  gs_tab4-vari_last_pur_inr = ( gs_tab4-last_pur_land_price - gs_tab4-landed_price )."( gs_tab4-landed_price - gs_tab4-last_pur_land_price ).
  gs_tab4-vari_last_pur_val = ( gs_tab4-vari_last_pur_inr * gs_tab4-qty ) / 100000.
  gs_tab4-vari_business_plan_inr = ( gs_tab4-business_price - gs_tab4-landed_price ).
  gs_tab4-vari_business_plan_val = ( gs_tab4-vari_business_plan_inr * gs_tab4-qty ) / 100000.

  gs_tab4-clos_m1 = ( ( gs_tab4-op_m1 + gs_tab4-app_vend_m1 + gs_tab4-otr_vend_m1 ) - gs_tab4-req_m1 ) .
  gs_tab4-op_m2 = gs_tab4-clos_m1.
  gs_tab4-clos_m2 = ( ( gs_tab4-op_m2 + gs_tab4-app_vend_m2 + gs_tab4-otr_vend_m2 ) - gs_tab4-req_m2 ) .
  gs_tab4-op_m3 = gs_tab4-clos_m2.
  gs_tab4-clos_m3 = ( ( gs_tab4-op_m3 + gs_tab4-app_vend_m3 + gs_tab4-otr_vend_m3 ) - gs_tab4-req_m3 ) .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gs_tab4-matnr
    IMPORTING
      output = gs_tab4-matnr.
  LOOP AT gt_tab4 INTO gs_tab44 .
    IF sy-tabix = g_lineno.
      MODIFY gt_tab4 FROM gs_tab4 TRANSPORTING matnr maktx name1 landed_price landed_value
                         last_pur_basic_price last_pur_land_price business_price vari_last_pur_inr vari_last_pur_val
                         vari_business_plan_inr vari_business_plan_val tax_desc
                         otr_vend_m1 otr_vend_m2 otr_vend_m3 clos_m1 clos_m2 clos_m3 waers mwskz
                         op_m2 op_m3.
    ENDIF.
    CLEAR: gs_tab44,lv_kbetr.
  ENDLOOP.
  CLEAR: lv_maktx,lv_name1.
ENDFORM.                    " FINAL_DATA_104
*&---------------------------------------------------------------------*
*&      Form  CHECK_MT_VN_BLINE_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mt_vn_bline_104 .
  CLEAR ls_matnr.
  REFRESH gt_tab5.
  gt_tab5[] = gt_tab5[].
  IF gv_ekgrp NOT BETWEEN 'B05' AND 'B06'.
    READ TABLE lt_matnr INTO ls_matnr WITH KEY matnr = gs_tab4-matnr.
    IF sy-subrc NE 0.
      MESSAGE i004(zpp) WITH gs_tab4-matnr gv_ekgrp.
      flag3_1 = 'X'.
      flag_final = 'X'.
*      CLEAR gs_tab1.
    ENDIF.
    IF gs_tab4-business_price = 0.
      MESSAGE i001(zpp) WITH gs_tab4-matnr gs_tab4-business_price.
      flag3_1 = 'X'.
      flag_final = 'X'.
*      CLEAR gs_tab1.
    ENDIF.

    LOOP AT gt_tab5 INTO gs_tab44 ."WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab44-ebelp = lv_index.
        IF gs_tab44-chk = 'X'.
          IF gs_tab44-matnr = lv_matnr.
            MESSAGE i010(zpp) WITH gs_tab44-matnr DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab44.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " CHECK_MT_VN_BLINE_104
*&---------------------------------------------------------------------*
*&      Form  SCREEN_EDIT104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_edit104 .
  LOOP AT SCREEN.                                "ravi
    IF gs_tab4-chk = 'X'.
      screen-input = 0.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SCREEN_EDIT104
*&---------------------------------------------------------------------*
*&      Form  COMPARE_SEL_NEW_BASIC_PRICE104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compare_sel_new_basic_price104 .
  REFRESH : gt_tab5.
  CLEAR flag3_4.
  gt_tab5[] = gt_tab4[].
  LOOP AT gt_tab4 INTO gs_tab4.
    IF gs_tab4-chk = 'X'.
      LOOP AT gt_tab5 INTO gs_tab44 WHERE matnr = gs_tab4-matnr.
        IF gs_tab44-chk NE 'X'.
          IF gs_tab44-netpr LE gs_tab4-netpr.
            IF gs_tab4-text IS INITIAL.
              MESSAGE w015(zpp) WITH gs_tab4-matnr .
              flag3_4 = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR gs_tab44.
      ENDLOOP.
    ENDIF.
    CLEAR gs_tab44.
  ENDLOOP.
  IF flag3_4 = 'X'.
*    PERFORM screen_edit104.
  ENDIF.
ENDFORM.                    " COMPARE_SEL_NEW_BASIC_PRICE104
*&---------------------------------------------------------------------*
*&      Form  CHECK_VEND_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vend_104 .
  CLEAR flag3_3.
  CALL FUNCTION 'SD_GET_KOKRS_BUKRS_FROM_WERKS'
    EXPORTING
      i_werks                  = gv_werks
      i_gsber                  = ' '
    IMPORTING
      e_bukrs                  = gv_bukrs
    EXCEPTIONS
      plant_not_valid          = 1
      valuation_area_not_valid = 2
      no_kokrs_found           = 3
      OTHERS                   = 4.
  CLEAR lv_lifnr.
  lv_lifnr = gs_tab4-lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_lifnr
    IMPORTING
      output = lv_lifnr.

  REFRESH: lt_lfb1,lt_f4_lifnr.
  SELECT lifnr FROM lfb1 INTO TABLE lt_lfb1 WHERE bukrs = gv_bukrs.
  IF lt_lfb1 IS NOT INITIAL.
    SELECT lifnr name1 FROM lfa1 INTO TABLE lt_f4_lifnr FOR ALL ENTRIES IN lt_lfb1
                                            WHERE lifnr = lt_lfb1-lifnr.
    READ TABLE lt_f4_lifnr INTO ls_f4_lifnr WITH KEY lifnr = lv_lifnr.
    IF sy-subrc NE 0.
      MESSAGE i008(zpp) WITH gs_tab4-lifnr gv_werks.
      flag3_3 = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_VEND_104
*&---------------------------------------------------------------------*
*&      Form  CREATE_TEXT_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_text_104 .
  IF gs_tab4-matnr IS NOT INITIAL.
    CLEAR gs_tab4.
    lv_curline  = g_lineno.
    flag = 'X'.
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

    READ TABLE gt_tab4 INTO gs_tab4 INDEX lv_curline.
    IF sy-subrc = 0.
      REFRESH g_mytable.
      text = gs_tab4-text.
      APPEND text TO g_mytable.
      DELETE g_mytable WHERE table_line IS INITIAL.
*        get_text_as_r3table
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
      SET CURSOR LINE g_lineno.
    ENDIF.
  if gs_tab4-text is initial.
      call method text_editor->set_text_as_r3table
        exporting
          table = g_mytable.
*       CALL METHOD cl_gui_cfw=>flush
*        EXCEPTIONS
*          cntl_system_error = 1
*          cntl_error        = 2
*          OTHERS            = 3.
*
*      CALL METHOD editor_container->free.
*      CALL METHOD text_editor->free.
    message 'No Data found for selected Line,Please maintain and click on SAVE_TEXT' type 'I'.

    endif.
  endif.
ENDFORM.                    " CREATE_TEXT_104
*&---------------------------------------------------------------------*
*&      Form  CREATE_TEXT_104_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_text_104_1 .
  IF gs_tab4-maktx IS NOT INITIAL.
    CLEAR gs_tab4.
    lv_curline  = g_lineno.
    flag = 'X'.
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
    READ TABLE gt_tab4 INTO gs_tab4 INDEX lv_curline.
    IF sy-subrc = 0.
      REFRESH g_mytable.
      text = gs_tab4-text.
      APPEND text TO g_mytable.
      DELETE g_mytable WHERE table_line IS INITIAL.
*        get_text_as_r3table
      CALL METHOD text_editor->set_text_as_r3table
        EXPORTING
          table = g_mytable.
      SET CURSOR LINE g_lineno.
      GET CURSOR FIELD field1.
    ENDIF.
 if gs_tab4-text is initial.
      call method text_editor->set_text_as_r3table
        exporting
          table = g_mytable.
*       CALL METHOD cl_gui_cfw=>flush
*        EXCEPTIONS
*          cntl_system_error = 1
*          cntl_error        = 2
*          OTHERS            = 3.
*
*      CALL METHOD editor_container->free.
*      CALL METHOD text_editor->free.
    message 'No Data found for selected Line,Please maintain and click on SAVE_TEXT' type 'I'.

    endif.
  endif.
ENDFORM.                    " CREATE_TEXT_104_1
*&---------------------------------------------------------------------*
*&      Form  SAVE_LINE_TEXT_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_line_text_104 .
  CALL METHOD text_editor->get_textstream
*          EXPORTING
*              ONLY_WHEN_MODIFIED     = CL_GUI_TEXTEDIT=>TRUE
          IMPORTING
              text                   = text
*              IS_MODIFIED            =
          EXCEPTIONS
              error_cntl_call_method = 1
              not_supported_by_gui   = 2
              OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.
  CALL METHOD editor_container->free.
  CALL METHOD text_editor->free.

  CLEAR: flag,gs_tab4,g_lineno.
*        GET CURSOR LINE g_lineno.
  READ TABLE gt_tab4 INTO gs_tab4 INDEX lv_curline.
  IF sy-subrc = 0.
    LOOP AT gt_tab4 INTO gs_tab44 .
      gs_tab44-text = text.
      IF sy-tabix = lv_curline.
        MODIFY gt_tab4 FROM gs_tab44 TRANSPORTING text.
      ENDIF.
      CLEAR gs_tab44.
    ENDLOOP.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 = 'PB1'.
      IF screen-name = 'SAVE_TEXT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
  CLEAR : text.
  SET CURSOR LINE lv_curline.
ENDFORM.                    " SAVE_LINE_TEXT_104
*&---------------------------------------------------------------------*
*&      Form  CHECK_FINAL_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_final_104 .
  LOOP AT gt_tab4 INTO gs_tab4.
    lv_no = sy-tabix - 1.
    READ TABLE gt_tab4 INTO gs_tab44 INDEX lv_no.
    IF sy-subrc = 0.
      CLEAR flag7.
      IF gs_tab4-lifnr = gs_tab44-lifnr AND gs_tab4-matnr = gs_tab44-matnr.
        MESSAGE i003(zpp) WITH gs_tab4-lifnr gs_tab4-matnr DISPLAY LIKE 'E'.
        flag7 = 'X'.
      ENDIF.
    ENDIF.

    PERFORM check_newprice_landprice_104.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab4-chk = 'X'.
      flag3 = 'X'.
      lv_matnr = gs_tab4-matnr.
      lv_index = gs_tab4-ebelp + 1.
    ENDIF.
    REFRESH lt_matnr.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab4-matnr
*\      IMPORTING
*\        output = gs_tab4-matnr.
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
        input  = gs_tab4-matnr
      IMPORTING
        output = gs_tab4-matnr.
*}   REPLACE
    PERFORM check_mt_vn_bline_104.  "chk material,ven,zbpl,best line
    PERFORM check_vendor_104.

    CLEAR: gs_tab4,gs_tab44.
  ENDLOOP.
ENDFORM.                    " CHECK_FINAL_104
*&---------------------------------------------------------------------*
*&      Form  CHECK_FINAL_104_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_final_104_1 .
  REFRESH : gt_tab5.
  gt_tab5[] = gt_tab4[].
  LOOP AT gt_tab4 INTO gs_tab4.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab4-chk = 'X'.
      flag3 = 'X'.
      lv_maktx = gs_tab4-maktx.
      lv_index = gs_tab4-ebelp + 1.
    ENDIF.
    LOOP AT gt_tab5 INTO gs_tab44." .
      IF gs_tab44-ebelp = lv_index.
        IF gs_tab44-chk = 'X'.
          IF gs_tab44-maktx = lv_maktx.
            MESSAGE i010(zpp) WITH gs_tab44-maktx DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab44.
    ENDLOOP.
    PERFORM check_vendor_104.
    IF gs_tab4-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      flag_final = 'X'.
    ENDIF.
    IF gs_tab4-netpr > gs_tab4-landed_price.
      MESSAGE i013(zpp) WITH gs_tab4-netpr gs_tab4-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      flag_final = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_FINAL_104_1
*&---------------------------------------------------------------------*
*&      Form  CHECK_REL_REMKS_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rel_remks_104 .
  CLEAR remarks.
  CALL METHOD remarks_editor->get_textstream
    IMPORTING
      text = remarks.
  CALL METHOD cl_gui_cfw=>flush
    EXCEPTIONS
      cntl_system_error = 1
      cntl_error        = 2
      OTHERS            = 3.

  CLEAR :gv_releaser1,gv_releaser2,gv_releaser3,gv_releaser4.
  READ TABLE list WITH KEY list_box.
  IF sy-subrc = 0.
    gv_releaser1 = list-key.
    gv_releaser_name1 = list-text.
  ENDIF.
  READ TABLE list2 WITH KEY list_box2.
  IF sy-subrc = 0.
    gv_releaser2 = list2-key.
    gv_releaser_name2 = list2-text.
  ENDIF.
  READ TABLE list3 WITH KEY list_box3.
  IF sy-subrc = 0.
    gv_releaser3 = list3-key.
    gv_releaser_name3 = list3-text.
  ENDIF.
  READ TABLE list4 WITH KEY list_box4.
  IF sy-subrc = 0.
    gv_releaser4 = list4-key.
    gv_releaser_name4 = list4-text.
  ENDIF.
  IF gv_releaser1 IS INITIAL AND gv_releaser2 IS INITIAL.
    MESSAGE text-016 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  IF gv_releaser1 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser1 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser3.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser2 EQ gv_releaser4.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser3 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
*          ELSEIF gv_releaser3 EQ gv_releaser4.
*            MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
*            EXIT.
  ELSEIF gv_releaser4 EQ gv_releaser1.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF gv_releaser4 EQ gv_releaser2.
    MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
*          ELSEIF gv_releaser4 EQ gv_releaser3.
*            MESSAGE text-017 TYPE 'S' DISPLAY LIKE 'E'.
*            EXIT.
  ELSEIF flag3 NE 'X'.
    MESSAGE text-015 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF flag3_2 = 'X'.
    PERFORM screen_edit104.
    CLEAR flag3_2.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag3_3 = 'X'.
    PERFORM screen_edit104.
    CLEAR flag3_3.
    flag_final = 'X'.
    EXIT.
  ELSEIF flag7 = 'X'.
   PERFORM screen_edit104.
   CLEAR flag7.
   flag_final = 'X'.
  ELSEIF remarks IS INITIAL.
    MESSAGE text-018 TYPE 'S' DISPLAY LIKE 'E'.
    flag_final = 'X'.
    EXIT.
  elseif flag_final = 'X'.
    perform screen_edit104.
    clear flag_final.
  elseif flag_final = ' '.
    message 'All entered line items are correct,You can Save your Proposal' type 'I'.
    clear flag_final.
  endif.
  flag6 ='X'.
ENDFORM.                    " CHECK_REL_REMKS_104
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHECK_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_check_104 .
  LOOP AT gt_tab4 INTO gs_tab4.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab4-chk = 'X'.
      flag3 = 'X'.
      lv_matnr = gs_tab4-matnr.
      lv_index = gs_tab4-ebelp + 1.
    ENDIF.
    REFRESH lt_matnr.
    SELECT matnr FROM marc INTO TABLE lt_matnr WHERE ekgrp = gv_ekgrp.
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = gs_tab4-matnr
*\      IMPORTING
*\        output = gs_tab4-matnr.
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
        input  = gs_tab4-matnr
      IMPORTING
        output = gs_tab4-matnr.

*}   REPLACE
    PERFORM check_mt_vn_bline_104.  "chk material,zbpl,best line
    PERFORM check_vend_104.
    IF gs_tab4-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
    IF gs_tab4-business_price = 0.
      MESSAGE i001(zpp) WITH gs_tab4-matnr gs_tab4-business_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
    IF gs_tab4-netpr > gs_tab4-landed_price.
      MESSAGE i013(zpp) WITH gs_tab4-netpr gs_tab4-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab4-netpr IS INITIAL.
      MESSAGE i017(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab4-landed_price IS INITIAL.
      MESSAGE i018(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
    IF gs_tab4-mwskz IS INITIAL.
      MESSAGE i019(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SAVE_CHECK_104
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHECK_104_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_check_104_1 .

  LOOP AT gt_tab4 INTO gs_tab4.
    CLEAR: lv_matnr,lv_index.
    IF gs_tab4-chk = 'X'.
      flag3 = 'X'.
      lv_maktx = gs_tab4-maktx.
      lv_index = gs_tab4-ebelp + 1.
    ENDIF.
    LOOP AT gt_tab5 INTO gs_tab44." WHERE ebelp = lv_index."FROM lv_index .
      IF gs_tab44-ebelp = lv_index.
        IF gs_tab44-chk = 'X'.
          IF gs_tab44-maktx = lv_maktx.
            MESSAGE i010(zpp) WITH gs_tab44-maktx DISPLAY LIKE 'E'.
            flag3_2 = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        lv_index = lv_index + 1.
      ENDIF.
      CLEAR gs_tab44.
    ENDLOOP.
    PERFORM check_vendor_104.
    IF gs_tab4-qty IS INITIAL.
      MESSAGE i016(zpp) WITH gs_tab4-matnr DISPLAY LIKE 'E'.
      flag7 = 'X'.
    ENDIF.
     if gs_tab4-netpr is initial.
      message i017(zpp) with gs_tab4-matnr display like 'E'.
      flag7 = 'X'.
      exit.
    endif.
    if gs_tab4-landed_price is initial.
      message i018(zpp) with gs_tab4-matnr display like 'E'.
      flag7 = 'X'.
      exit.
    endif.
    IF gs_tab4-netpr > gs_tab4-landed_price.
      MESSAGE i013(zpp) WITH gs_tab4-netpr gs_tab4-landed_price DISPLAY LIKE 'E'.
      flag7 = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SAVE_CHECK_104_1
*&---------------------------------------------------------------------*
*&      Form  CREATE_FROM_EXISTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_from_existing_104.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'    "fiscial year
  EXPORTING
    i_date               = sy-datum
    i_periv              = 'V3'
 IMPORTING
   e_gjahr               = gv_fiscal.

  LOOP AT gt_tab4 INTO gs_tab4.
    gs_tab4-chk              =   gs_tab4-chk .
    gs_tab4-pr_date          =   gv_date .
    gs_tab4-erdat            =   gv_date .
    gs_tab4-ekgrp            =   gv_ekgrp .
    gs_tab4-eknam            =   gv_eknam.
    gs_tab4-werks            =   gv_werks .
    gs_tab4-bukrs            =   gv_bukrs .
    gs_tab4-plantname        =   gv_plantname .
    gs_tab4-pr_user          =   gv_user.
    gs_tab4-pr_time          =   gv_time.
    gs_tab4-remarks          =   remarks.
    gs_tab4-releaser1        =   gv_releaser1.
    gs_tab4-releaser_name1   =   gv_releaser_name1.
    gs_tab4-releaser2        =   gv_releaser2.
    gs_tab4-releaser_name2   =   gv_releaser_name2.
    gs_tab4-releaser3        =   gv_releaser3.
    gs_tab4-releaser_name3   =   gv_releaser_name3.
    gs_tab4-releaser4        =   gv_releaser4.
    gs_tab4-releaser_name4   =   gv_releaser_name4.
    MODIFY gt_tab4 FROM gs_tab4 TRANSPORTING chk pr_date ekgrp bukrs werks pr_time pr_user remarks releaser1
                                             releaser2 releaser3 releaser4 releaser_name1 releaser_name2
                                             releaser_name3 releaser_name4 eknam plantname                                          .
    CLEAR gs_tab4.
  ENDLOOP.
  lv_year = sy-datum(04).
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'YPPINTNO'
      toyear                  = lv_year
    IMPORTING
      number                  = gv_newprno
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
     WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  IF gv_newprno IS NOT INITIAL.
    LOOP AT gt_tab4 INTO gs_tab4.
      gs_zmm_pur_proposal-app_vendor         =   gs_tab4-chk.
      gs_zmm_pur_proposal-prno               =   gv_newprno.
      gs_zmm_pur_proposal-gjahr              =   gv_fiscal.
      gs_zmm_pur_proposal-pr_date            =   gs_tab4-pr_date .
      gs_zmm_pur_proposal-ekgrp              =   gs_tab4-ekgrp.
      gs_zmm_pur_proposal-eknam              =   gs_tab4-eknam.
      gs_zmm_pur_proposal-bukrs              =   gs_tab4-bukrs.
      gs_zmm_pur_proposal-werks              =   gs_tab4-werks.
      gs_zmm_pur_proposal-plantname          =   gs_tab4-plantname.
      gs_zmm_pur_proposal-erdat              =   gs_tab4-erdat.
      gs_zmm_pur_proposal-pr_itemno          =   gs_tab4-ebelp * 10.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_tab4-matnr
        IMPORTING
          output = gs_tab4-matnr.
      IF gs_zmm_pur_proposal-ekgrp NOT BETWEEN 'B05' AND 'B06'.
        gs_zmm_pur_proposal-matnr              =   gs_tab4-matnr.
      ENDIF.
      gs_zmm_pur_proposal-maktx              =   gs_tab4-maktx.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_tab4-lifnr
        IMPORTING
          output = gs_tab4-lifnr.
      gs_zmm_pur_proposal-lifnr              =   gs_tab4-lifnr.
      gs_zmm_pur_proposal-name1              =   gs_tab4-name1.
      gs_zmm_pur_proposal-menge              =   gs_tab4-qty.
      gs_zmm_pur_proposal-meins              =   gs_tab4-meins.
      gs_zmm_pur_proposal-netpr              =   gs_tab4-netpr.
      gs_zmm_pur_proposal-waers              =   gs_tab4-waers.
      gs_zmm_pur_proposal-mwskz              =   gs_tab4-mwskz.
      gs_zmm_pur_proposal-tax_desc           =   gs_tab4-tax_desc.
      gs_zmm_pur_proposal-land_pr            =   gs_tab4-landed_price.
      gs_zmm_pur_proposal-land_value         =   gs_tab4-landed_value.
      gs_zmm_pur_proposal-last_basicpr       =   gs_tab4-last_pur_basic_price.
      gs_zmm_pur_proposal-last_landpr        =   gs_tab4-last_pur_land_price.
      gs_zmm_pur_proposal-business_planpr    =   gs_tab4-business_price.
      gs_zmm_pur_proposal-vari_last_pur_in   =   gs_tab4-vari_last_pur_inr.
      gs_zmm_pur_proposal-vari_last_pur_va   =   gs_tab4-vari_last_pur_val.
      gs_zmm_pur_proposal-vari_bus_pl_inr    =   gs_tab4-vari_business_plan_inr.
      gs_zmm_pur_proposal-vari_bus_pl_val    =   gs_tab4-vari_business_plan_val.
      gs_zmm_pur_proposal-opening_m1         =   gs_tab4-op_m1.
      gs_zmm_pur_proposal-requirment_m1      =   gs_tab4-req_m1.
      gs_zmm_pur_proposal-app_vend_m1        =   gs_tab4-app_vend_m1.
      gs_zmm_pur_proposal-other_vend_m1      =   gs_tab4-otr_vend_m1.
      gs_zmm_pur_proposal-closing_m1         =   gs_tab4-clos_m1.
      gs_zmm_pur_proposal-opening_m2         =   gs_tab4-op_m2.
      gs_zmm_pur_proposal-requirment_m2      =   gs_tab4-req_m2.
      gs_zmm_pur_proposal-app_vend_m2        =   gs_tab4-app_vend_m2.
      gs_zmm_pur_proposal-other_vend_m2      =   gs_tab4-otr_vend_m2.
      gs_zmm_pur_proposal-closing_m2         =   gs_tab4-clos_m2.
      gs_zmm_pur_proposal-opening_m3         =   gs_tab4-op_m3.
      gs_zmm_pur_proposal-requirment_m3      =   gs_tab4-req_m3.
      gs_zmm_pur_proposal-app_vend_m3        =   gs_tab4-app_vend_m3.
      gs_zmm_pur_proposal-other_vend_m3      =   gs_tab4-otr_vend_m3.
      gs_zmm_pur_proposal-closing_m3         =   gs_tab4-clos_m3.
      gs_zmm_pur_proposal-comp_data          =   gs_tab4-costformula.
      gs_zmm_pur_proposal-comp_data1         =   gs_tab4-comp_data_avl.
      gs_zmm_pur_proposal-line_txt           =   gs_tab4-text.
      gs_zmm_pur_proposal-remark             =   gs_tab4-remarks.
      gs_zmm_pur_proposal-releaser1          =   gs_tab4-releaser1.
      gv_pernr                               =   gs_tab4-releaser1.
      gs_zmm_pur_proposal-releaser_name1     =   gs_tab4-releaser_name1.
      gs_zmm_pur_proposal-releaser2          =   gs_tab4-releaser2.
      gs_zmm_pur_proposal-releaser_name2     =   gs_tab4-releaser_name2.
      gs_zmm_pur_proposal-releaser3          =   gs_tab4-releaser3.
      gs_zmm_pur_proposal-releaser_name3     =   gs_tab4-releaser_name3.
      gs_zmm_pur_proposal-releaser4          =   gs_tab4-releaser4.
      gs_zmm_pur_proposal-releaser_name4     =   gs_tab4-releaser_name4.
      gs_zmm_pur_proposal-erdat              =   gs_tab4-pr_date .
      gs_zmm_pur_proposal-pr_user            =   gs_tab4-pr_user.
      gv_user                                =   gs_tab4-pr_user.
      gs_zmm_pur_proposal-pr_time            =                gs_tab4-pr_time.
*                                           = gs_tab1-flag.
      APPEND gs_zmm_pur_proposal TO gt_zmm_pur_proposal.
      CLEAR: gs_zmm_pur_proposal,gs_tab4.
    ENDLOOP.

    MODIFY zmm_pur_proposal FROM TABLE gt_zmm_pur_proposal.
    COMMIT WORK.

    PERFORM email_send_104.
*    wait UP TO 3 SECONDS.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = gv_newprno
      IMPORTING
        output = gv_newprno.
    MESSAGE s002(zpp) WITH gv_newprno." DISPLAY LIKE 'S'.
    WAIT UP TO 2 SECONDS.
    CLEAR gv_ans.
    lv_msg = 'Create Attachment'.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
         titlebar                     = lv_msg
         text_question               = 'Do you want to Create an attachment?'
         text_button_1                = 'Yes'
*        ICON_BUTTON_1               = ' '
         text_button_2                = 'No'
*        ICON_BUTTON_2               = ' '
         default_button               = '1'
         display_cancel_button        = ' '
         start_column                 = 75
         start_row                    = 5
         IMPORTING
           answer                       = gv_ans                 .
    IF gv_ans = 1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gv_newprno
        IMPORTING
          output = gv_newprno.
      CLEAR gv_prno.
      gv_prno = gv_newprno.
      PERFORM refresh_104.
      CALL SCREEN 500.
      LEAVE PROGRAM.
    ELSE.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

ENDFORM.                    " CREATE_FROM_EXISTING
*&---------------------------------------------------------------------*
*&      Form  REFRESH_104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_104 .
 REFRESH:  gt_tab4,gt_ekpo,gt_pp,gt_user_list,
           gt_ekpo,gt_pa0001,gt_a516,gt_stxh,
           list,list2,list3,list4,gt_a516,
           gt_zmm_pur_proposal.

  CLEAR: gs_tab4,gv_date,gv_ekgrp,gv_werks,gv_time,
         remarks,gs_tab2,
         gs_stxh,gs_a516, gs_pa0001,
         gs_ekpo,gs_user_list,gs_zmm_pur_proposal,gs_pp,gs_tab1,
         g_mytable,gv_plantname,gv_eknam,
         flag,flag1,flag2,flag3,flag3_1,flag4,flag5,flag6,flag7,
         no,g_lineno,lv_curline,text,gv_releaser1,
         gv_releaser2,gv_releaser3,
         gv_releaser4,gv_releaser_name1,
         gv_releaser_name2,gv_releaser_name3,gv_releaser_name4,lv_msg,
         gv_ans,gv_msg,lv_name1,lv_maktx,gv_bukrs.
ENDFORM.                    " REFRESH_104
*&---------------------------------------------------------------------*
*&      Form  AUTHORIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AUTHORIZATION .

  DATA lv_pernr TYPE pernr.
  CLEAR lv_pernr.

  SELECT SINGLE pernr FROM pa0105 INTO lv_pernr WHERE uname = sy-uname and subty = '0010'
                                   AND AEDTM LE SY-DATUM AND ENDDA GE SY-DATUM. .

   AUTHORITY-CHECK OBJECT 'Y_PROPOSAL'
                   ID 'ACTVT' FIELD '01'
                   ID 'ACTVT' FIELD '02'
                   ID 'ACTVT' FIELD '03'
                   ID 'ACTVT' FIELD '16'
                   ID 'PERNR' FIELD LV_PERNR.
*                  .
    IF SY-SUBRC NE 0.
      MESSAGE 'You are not Authorized Person' TYPE 'E'.
      LEAVE PROGRAM.
    ENDIF.

ENDFORM.                    " AUTHORIZATION
