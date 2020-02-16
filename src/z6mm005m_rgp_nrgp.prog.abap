*&---------------------------------------------------------------------*
*& Module Pool       Z6MM005M_RGP_NRGP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

PROGRAM  Z6MM005M_RGP_NRGP.
*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Returnable / Not Returnable Gate Pass Process
* OBJECT TYPE       : Module Pool    FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 09.06.2010
*        DEV REQUEST: IRDK900078
*  TCODE            :zmm005_1,  zmm005_2, zmm005_3, zmm005_4, zmm005_5,
*                    zmm005_6
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*


TABLES : Z6MMA_RGPNRGP_HD, Z6MMA_RGPNRGP_DT,Z6MMA_RGPNRGP_HT.

TYPES : BEGIN OF typ_item,
        mark     TYPE c,
        gino     TYPE Zzgino,
        matnr	   TYPE matnr,
        maktx	   TYPE maktx,
        o_qty	   TYPE kawrt,
        r_qty	   TYPE Zzr_qty,
        p_qty    TYPE Zzr_qty,
        uom	     TYPE meins,
        due_date TYPE DZFBDT,
        status   TYPE Zzstat,
        END OF typ_item.
data : v_girno type numc3.
DATA : gt_item   TYPE STANDARD TABLE OF typ_item.
DATA : gs_item   LIKE LINE OF gt_item.
DATA : gs_hd     TYPE Z6MMA_RGPNRGP_HD.
CONTROLS tc_01 TYPE TABLEVIEW USING SCREEN 9001.

DATA : gt_modify   TYPE STANDARD TABLE OF typ_item.
DATA : gs_modify   LIKE LINE OF gt_item.
DATA : gs_hd_mod   TYPE Z6MMA_RGPNRGP_HD.
CONTROLS tc_02 TYPE TABLEVIEW USING SCREEN 9002.

DATA : gt_display   TYPE STANDARD TABLE OF typ_item.
DATA : gs_display   LIKE LINE OF gt_item.
CONTROLS tc_03 TYPE TABLEVIEW USING SCREEN 9003.
CONTROLS tc_05 TYPE TABLEVIEW USING SCREEN 9005.

DATA : gt_update   TYPE STANDARD TABLE OF typ_item.
DATA : gs_update   LIKE LINE OF gt_item.
data : st_Z6MMA_RGPNRGP_HT type Z6MMA_RGPNRGP_HT.
CONTROLS tc_04 TYPE TABLEVIEW USING SCREEN 9004.

DATA: save_ok TYPE sy-ucomm.
DATA: vend(1) TYPE c, cust(1) TYPE c,
      p_chk TYPE c, p_del TYPE c,
      p_app TYPE c, p_delete,
      gv_gno(10)  TYPE c.

DATA: gv_lines TYPE i,
      gv_limit TYPE i,
      gv_fill  TYPE i,
      gv_cnt   TYPE i,
      cols LIKE LINE OF tc_01-cols,
      gv_clear TYPE c,
      gv_flag  TYPE c,
      gv_flag1 TYPE c,
      lv_flag2 TYPE c,
      lv_flag3 TYPE c,
      lv_flag4 TYPE c.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'ZGATE'.
  SET TITLEBAR 'T9001'  with 'Create'.
  DATA : lv_fname(30)  TYPE c.
  LOOP AT SCREEN.
    IF cust = 'X'.
      IF screen-group1 = 'CU'.
        screen-input = '1'.
      ENDIF.
      IF screen-group1 = 'VE'.
        screen-input = '0'.
      ENDIF.
      MODIFY SCREEN.
    ELSE.
      IF screen-group1 = 'CU'.
        screen-input = '0'.
      ENDIF.
      IF screen-group1 = 'VE'.
        screen-input = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  IF gv_flag1 = 'X'.
    CLEAR : gs_hd, gt_item, gs_item.
    REFRESH gt_item.
  ENDIF.
ENDMODULE.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  DATA : lv_ans TYPE c.
  CLEAR : gv_cnt, gv_flag1.
  save_ok = sy-ucomm.
  CLEAR sy-ucomm.
  CASE save_ok.
    WHEN 'DEL'.
      READ TABLE tc_01-cols INTO cols WITH KEY screen-input = '1'.
      IF sy-subrc = 0.
        LOOP AT gt_item INTO gs_item WHERE mark = 'X'.
          DELETE gt_item.
        ENDLOOP.
      ENDIF.
    WHEN 'SAVE'.
      PERFORM zf_check_cust_vend.
      IF gv_flag = ' '.
        IF NOT gt_item[] IS INITIAL.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar      = 'Create'
              text_question = 'Are you sure, want to create record'
              start_column  = 25
              start_row     = 6
            IMPORTING
              answer        = lv_ans.
          IF lv_ans = '1'.
            PERFORM zf_insert_into_table.
            MESSAGE i398(00) WITH 'Gate Pass No :' gv_gno 'Created' ''.
            gv_flag1 = 'X'.
            CLEAR : gs_item, gt_item.
            REFRESH gt_item.
          ENDIF.
        ELSE.
          MESSAGE i398(00) WITH 'Enter line item details' '' '' ''.
        ENDIF.
      ENDIF.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9001  INPUT

*&---------------------------------------------------------------------*
*&      Module  read_table_control  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_table_control INPUT.
  DATA : ls_item TYPE typ_item,
         lv_gino(5) TYPE n,
         lv_lno(5)  TYPE n.
  CLEAR : ls_item, lv_lno.
  gv_lines = sy-loopc.
  DESCRIBE TABLE gt_item LINES lv_lno.
  IF lv_lno IS INITIAL.
    PERFORM zf_get_mat_desc USING gs_item-matnr
                            CHANGING gs_item-maktx gs_item-uom.
    lv_gino = lv_gino + 1.
    gs_item-gino = lv_gino.
    APPEND gs_item TO gt_item.
    PERFORM zf_modification_check USING gs_item-matnr gs_item-maktx
                         gs_item-o_qty gs_item-uom gs_item-due_date.
  ELSE.
    READ TABLE gt_item INTO ls_item WITH KEY gino = gs_item-gino.
    IF sy-subrc = '0'.
      PERFORM zf_get_mat_desc USING gs_item-matnr
                            CHANGING gs_item-maktx gs_item-uom.
      MODIFY gt_item FROM gs_item INDEX tc_01-current_line.
      PERFORM zf_modification_check USING gs_item-matnr gs_item-maktx
                         gs_item-o_qty gs_item-uom gs_item-due_date.
    ELSE.
      PERFORM zf_get_mat_desc USING gs_item-matnr
                            CHANGING gs_item-maktx gs_item-uom.
      lv_gino = lv_gino + 1.
      gs_item-gino = lv_gino.
      APPEND gs_item TO gt_item.
      PERFORM zf_modification_check USING gs_item-matnr gs_item-maktx
                         gs_item-o_qty gs_item-uom gs_item-due_date.
    ENDIF.
  ENDIF.
ENDMODULE.                 " read_table_control  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.
  DATA : lsmod TYPE Z6MMA_RGPNRGP_HD.
  CLEAR : lsmod.
  SET PF-STATUS 'ZGATE'.
  SET TITLEBAR 'T9001' with 'Change'.
  IF gv_clear = 'X'.
    CLEAR : gs_modify, gs_hd_mod, gt_modify.
    REFRESH gt_modify.

    LOOP AT SCREEN.
      IF screen-group1 = 'GR'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF NOT gs_hd_mod IS INITIAL AND gt_modify IS INITIAL.
      SELECT SINGLE * FROM Z6MMA_RGPNRGP_HD
                      INTO lsmod
                      WHERE werks = gs_hd_mod-werks
                        AND gtype = gs_hd_mod-gtype
                        AND gyear = gs_hd_mod-gyear
                        AND gno = gs_hd_mod-gno.
      IF NOT lsmod IS INITIAL.
        MOVE-CORRESPONDING lsmod TO gs_hd_mod.
        IF gs_hd_mod-loekz = 'X'.
          MESSAGE i398(00) WITH 'Gate Pass deleted.' '' '' ''.
          CLEAR : gs_hd_mod.
        ELSE.
          IF gs_hd_mod-app_stat = 'A'.
            MESSAGE i398(00) WITH 'Gate Pass Approved.' '' '' ''.
            CLEAR : gs_hd_mod.
          ELSE.
            SELECT * FROM Z6MMA_RGPNRGP_DT
                 INTO CORRESPONDING FIELDS OF TABLE gt_modify
                 WHERE werks = gs_hd_mod-werks
                          AND gtype = gs_hd_mod-gtype
                          AND gyear = gs_hd_mod-gyear
                          AND gno = gs_hd_mod-gno.
            LOOP AT SCREEN.
              IF screen-group1 = 'GR'.
                screen-input = '0'.
                MODIFY SCREEN.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE i398(00) WITH 'Record not found.' '' '' ''.
        CLEAR : gs_hd_mod.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gv_flag1 = 'X'.
    CLEAR : gs_hd_mod, gs_modify, gt_modify.
    REFRESH gt_modify.
  ENDIF.
  DESCRIBE TABLE gt_modify LINES gv_fill.
*  tc_02-lines = gv_fill.
ENDMODULE.                 " STATUS_9002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.
  DATA : lv_an TYPE c.
  save_ok = sy-ucomm.
  CLEAR : sy-ucomm, gv_clear, lv_an, gv_flag1.
  CASE save_ok.
    WHEN 'NEW'.
      gv_clear = 'X'.
    WHEN 'DEL'.
      READ TABLE tc_01-cols INTO cols WITH KEY screen-input = '1'.
      IF sy-subrc = 0.
        LOOP AT gt_modify INTO gs_modify WHERE mark = 'X'.
          DELETE gt_modify.
          clear gs_modify.
        ENDLOOP.
      ENDIF.
    WHEN 'MOD'.
      PERFORM zf_check_cust_vend.
      IF gv_flag = ' '.
        IF NOT gt_modify[] IS INITIAL.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar      = 'Modify'
              text_question = 'Are you sure, want to Modify record'
              start_column  = 25
              start_row     = 6
            IMPORTING
              answer        = lv_an.
          IF lv_an = '1'.
            PERFORM zf_modfiy_table.
            MESSAGE i398(00) WITH 'Gate Pass Modified' '' '' ''.
            gv_flag1 = 'X'.
          ENDIF.
        ELSE.
          MESSAGE i398(00) WITH 'Enter line item details' '' '' ''.
        ENDIF.
      ENDIF.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9002  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_02  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_table_control_02 INPUT.
  DATA : lt_dtl TYPE STANDARD TABLE OF Z6MMA_RGPNRGP_DT,
         ls_dtl TYPE Z6MMA_RGPNRGP_DT,
         lv_lno2(5)  TYPE n.
  IF lt_dtl IS INITIAL.
    CLEAR : lt_dtl, ls_dtl, lv_lno2. REFRESH lt_dtl.
    SELECT * FROM Z6MMA_RGPNRGP_DT INTO TABLE lt_dtl
           WHERE werks = gs_hd_mod-werks
             AND gtype = gs_hd_mod-gtype
             AND gyear = gs_hd_mod-gyear
             AND gno = gs_hd_mod-gno.
    SORT lt_dtl BY gino DESCENDING.
    READ TABLE lt_dtl INTO ls_dtl INDEX 1.
    lv_lno2 = ls_dtl-gino.
  ENDIF.

  IF gv_clear = ' '.
    DATA : ls_modify TYPE typ_item,
           lv_lno1(5)  TYPE n.
    CLEAR : ls_modify, lv_lno1.
    gv_lines = sy-loopc.
    DESCRIBE TABLE gt_modify LINES lv_lno1.
    IF NOT lv_lno1 IS INITIAL.
      READ TABLE gt_modify INTO ls_modify WITH KEY gino =
gs_modify-gino.
      IF sy-subrc = '0'.
        PERFORM zf_get_mat_desc USING ls_modify-matnr
                              CHANGING gs_modify-maktx gs_modify-uom.
        MODIFY gt_modify FROM gs_modify INDEX tc_02-current_line.
        PERFORM zf_modification_check USING ls_modify-matnr
                           ls_modify-maktx  ls_modify-o_qty
                           ls_modify-uom ls_modify-due_date.
      ELSE.
        PERFORM zf_get_mat_desc USING gs_modify-matnr
                              CHANGING gs_modify-maktx gs_modify-uom.
        lv_lno2 = lv_lno2 + 1.
        gs_modify-gino = lv_lno2.
        APPEND gs_modify TO gt_modify.
        PERFORM zf_modification_check USING gs_modify-matnr
                           gs_modify-maktx  gs_modify-o_qty
                           gs_modify-uom gs_modify-due_date.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " READ_TABLE_CONTROL_02  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9003 OUTPUT.
  SET PF-STATUS 'ZGATE'.
  SET TITLEBAR 'T9001' with 'Display'.
  PERFORM zf_get_details.
  DESCRIBE TABLE gt_display LINES gv_fill.
  tc_03-lines = gv_fill.
ENDMODULE.                 " STATUS_9003  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9003 INPUT.
  DATA lt_item TYPE STANDARD TABLE OF typ_item.
  CLEAR lt_item.
  REFRESH lt_item.
  save_ok = sy-ucomm.
  CLEAR : sy-ucomm, gv_clear.
  CASE save_ok.
    WHEN 'NEW'.
      gv_clear = 'X'.
    WHEN 'PRINT'.
      IF gs_hd_mod-app_stat = 'A'.
        lt_item[] = gt_display[].
        DELETE lt_item WHERE status = 'U'.
        IF NOT lt_item[] IS INITIAL.
          PERFORM zf_print_gate_pass.
          gv_clear = 'X'.
        ELSE.
          MESSAGE i398(00) WITH
           'Gate Pass Quantity recieved, you can not print'.
        ENDIF.
      ELSE.
        MESSAGE i398(00) WITH 'Gate Pass Not Approved'.
      ENDIF.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9003  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_03  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_table_control_03 INPUT.
  gv_lines = sy-loopc.
  MODIFY gt_display FROM gs_display INDEX tc_03-current_line.
ENDMODULE.                 " READ_TABLE_CONTROL_03  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9004 INPUT.
  DATA : lv_an1 TYPE c.
  CLEAR : lv_an1.
  save_ok = sy-ucomm.
  CLEAR : sy-ucomm, gv_clear.
  CASE save_ok.
    WHEN 'NEW'.
      gv_clear = 'X'.
    WHEN 'UP'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar      = 'Update'
          text_question = 'Are you sure, want to Update record'
          start_column  = 25
          start_row     = 6
        IMPORTING
          answer        = lv_an1.
      IF lv_an1 = '1'.
        PERFORM zf_update_table.
        IF lv_flag2 = 'X'.
          MESSAGE i398(00) WITH 'Gate Pass updated'.
        ENDIF.
      ENDIF.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9004  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9004 OUTPUT.
  SET PF-STATUS 'ZGATE'.
  SET TITLEBAR 'T9001' with 'Update Received Quantity'.
  IF gv_clear = 'X'.
    CLEAR : gs_update, gs_hd_mod, gt_update.
    REFRESH gt_update.

    LOOP AT SCREEN.
      IF screen-group1 = 'GR'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF NOT gs_hd_mod IS INITIAL AND gt_update IS INITIAL.
      SELECT SINGLE * FROM Z6MMA_RGPNRGP_HD
                      INTO gs_hd_mod
                      WHERE werks = gs_hd_mod-werks
                        AND gtype = gs_hd_mod-gtype
                        AND gyear = gs_hd_mod-gyear
                        AND gno = gs_hd_mod-gno.
      IF gs_hd_mod-gtype = 'RGP'.
        IF gs_hd_mod-loekz = 'X'.
          MESSAGE i398(00) WITH 'Gate Pass deleted.' '' '' ''.
          CLEAR : gs_hd_mod.
        ELSE.
          IF gs_hd_mod-app_stat = 'A'.
            SELECT * FROM Z6MMA_RGPNRGP_DT
                 INTO CORRESPONDING FIELDS OF TABLE gt_update
                 WHERE werks = gs_hd_mod-werks
                          AND gtype = gs_hd_mod-gtype
                          AND gyear = gs_hd_mod-gyear
                          AND gno = gs_hd_mod-gno
                          AND status = ''.
            IF NOT gt_update[] IS INITIAL.
              LOOP AT gt_update INTO gs_update.
                IF  gs_update-p_qty IS INITIAL.
                  gs_update-p_qty = gs_update-o_qty - gs_update-r_qty.
                ENDIF.
                gs_update-r_qty = ''.
                MODIFY gt_update FROM gs_update
                TRANSPORTING p_qty r_qty.
                CLEAR : gs_update.
              ENDLOOP.
              LOOP AT SCREEN.
                IF screen-group1 = 'GR'.
                  screen-input = '0'.
                  MODIFY SCREEN.
                ENDIF.
              ENDLOOP.
            ELSE.
              MESSAGE i398(00) WITH 'Gate Pass already updated'.
              CLEAR gs_hd_mod.
            ENDIF.
          ELSE.
            MESSAGE i398(00) WITH 'Gate Pass Not Approved.' '' '' ''.
            CLEAR gs_hd_mod.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE i398(00) WITH 'Enter only RGP only' '' '' ''.
        CLEAR : gs_hd_mod.
      ENDIF.
    ENDIF.
  ENDIF.
  IF lv_flag2 = 'X'.
    CLEAR : gs_hd_mod, gt_update, gs_update.
    REFRESH gt_update.
  ENDIF.
  DESCRIBE TABLE gt_update LINES gv_fill.
  tc_04-lines = gv_fill.

ENDMODULE.                 " STATUS_9004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_04  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_table_control_04 INPUT.
  gv_lines = sy-loopc.
  IF gs_update-p_qty IS INITIAL.
    gs_update-p_qty = gs_update-o_qty - gs_update-r_qty.
  ENDIF.
*  gs_update-R_QTY = ''.
  MODIFY gt_update FROM gs_update INDEX tc_04-current_line.
ENDMODULE.                 " READ_TABLE_CONTROL_04  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9005 OUTPUT.
  SET PF-STATUS 'ZGATE'.
  SET TITLEBAR 'T9001' with 'Approve / Reject '.
  PERFORM zf_get_details.
  IF lv_flag3 = 'X'.
    CLEAR : gs_hd_mod, gs_display, gt_display.
    REFRESH gt_display.
  ENDIF.
  DESCRIBE TABLE gt_display LINES gv_fill.
  tc_05-lines = gv_fill.
ENDMODULE.                 " STATUS_9005  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  READ_TABLE_CONTROL_05  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_table_control_05 INPUT.
  gv_lines = sy-loopc.
  MODIFY gt_display FROM gs_display INDEX tc_05-current_line.
ENDMODULE.                 " READ_TABLE_CONTROL_05  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9005 INPUT.
  DATA : lv_del TYPE c, lv_check TYPE c, lv_an2 TYPE c.
  CLEAR : lv_check, lv_flag3, lv_an2.
  save_ok = sy-ucomm.
  CLEAR : sy-ucomm, gv_clear.
  CASE save_ok.
    WHEN 'NEW'.
      gv_clear = 'X'.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'APP'.
      IF p_chk = 'X'.
        SELECT SINGLE loekz app_stat FROM Z6MMA_RGPNRGP_HD
               INTO (lv_del, lv_check)
                WHERE werks = gs_hd_mod-werks
                  AND gtype = gs_hd_mod-gtype
                  AND gyear = gs_hd_mod-gyear
                  AND gno = gs_hd_mod-gno.
        IF lv_check = 'A'.
          MESSAGE i398(00) WITH 'Gate Pass Already Approved.' '' '' ''.
          CLEAR : gs_hd_mod.
        ELSE.
          IF lv_del = 'X'.
            MESSAGE i398(00) WITH
                    'Gate Pass Deleted, you can not Approve' '' '' ''.
            CLEAR : gs_hd_mod.
          ELSE.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar      = 'Approve'
                text_question = 'Are you sure, want to Approve record'
                start_column  = 25
                start_row     = 6
              IMPORTING
                answer        = lv_an2.
            IF lv_an2 = '1'.
              UPDATE Z6MMA_RGPNRGP_HD SET app_stat = 'A'
                              app_date = sy-datum
                              app_by = sy-uname
                        WHERE werks = gs_hd_mod-werks
                          AND gtype = gs_hd_mod-gtype
                          AND gyear = gs_hd_mod-gyear
                          AND gno = gs_hd_mod-gno.
              IF sy-subrc = '0'.
                MESSAGE w398(00) WITH 'Gate Pass Approved' '' '' ''.
                lv_flag3 = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE i398(00) WITH 'Please Click the Approvel check box'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9005  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9006 OUTPUT.
  SET PF-STATUS 'ZGATE'.
  SET TITLEBAR 'T9001' with 'Delete'.
  IF gv_clear = 'X'.
    CLEAR : gs_hd_mod, p_del.
  ELSE.
    IF NOT gs_hd_mod IS INITIAL .
      SELECT SINGLE * FROM Z6MMA_RGPNRGP_HD
                      INTO gs_hd_mod
                      WHERE werks = gs_hd_mod-werks
                        AND gtype = gs_hd_mod-gtype
                        AND gyear = gs_hd_mod-gyear
                        AND gno = gs_hd_mod-gno.
    ENDIF.
  ENDIF.
  IF lv_flag4 = 'X'.
    CLEAR : gs_hd_mod.
  ENDIF.
ENDMODULE.                 " STATUS_9006  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9006 INPUT.
  DATA : lv_del1 TYPE c, lv_app, lv_an3 TYPE c.
  CLEAR : lv_del1, lv_app, lv_flag4, lv_an3.
  save_ok = sy-ucomm.
  CLEAR : sy-ucomm, gv_clear.
  CASE save_ok.
    WHEN 'NEW'.
      gv_clear = 'X'.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'DEL'.
      IF p_del = 'X'.
        SELECT SINGLE loekz app_stat FROM Z6MMA_RGPNRGP_HD
               INTO (lv_del1, lv_app)
                WHERE werks = gs_hd_mod-werks
                  AND gtype = gs_hd_mod-gtype
                  AND gyear = gs_hd_mod-gyear
                  AND gno = gs_hd_mod-gno.
        IF lv_del1 = 'X'.
          MESSAGE i398(00) WITH 'Gate Pass Already Deleted.' '' '' ''.
          CLEAR: gs_hd_mod.
        ELSE.
          IF lv_app = 'A'.
            MESSAGE i398(00) WITH
                    'Gate Pass Approved, you can not Delete' '' '' ''.
            CLEAR: gs_hd_mod.
          ELSE.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar      = 'Delete'
                text_question = 'Are you sure, want to Delete record'
                start_column  = 25
                start_row     = 6
              IMPORTING
                answer        = lv_an3.
            IF lv_an3 = '1'.
              UPDATE Z6MMA_RGPNRGP_HD SET loekz = 'X'
                        WHERE werks = gs_hd_mod-werks
                          AND gtype = gs_hd_mod-gtype
                          AND gyear = gs_hd_mod-gyear
                          AND gno = gs_hd_mod-gno.
              IF sy-subrc = '0'.
                MESSAGE w398(00) WITH 'Gate Pass Deleted' '' '' ''.
                CLEAR: gs_hd_mod.
                lv_flag4 = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE i398(00) WITH 'Please Click the Delete check box'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9006  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_GET_WROKS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_get_wroks INPUT.
  TYPES : BEGIN OF typ_list,
              gtype TYPE Zzgtype,
              werks TYPE werks_d,
            END OF typ_list.
  DATA : lt_list TYPE STANDARD TABLE OF typ_list,
         ls_list TYPE typ_list.
  DATA: lt_dynflds         TYPE TABLE OF dynpread,
        ls_dynflds         TYPE dynpread,
        lt_return_tab      TYPE STANDARD TABLE OF ddshretval,
        ls_ret_tab         TYPE ddshretval.
  DATA : lt_hd TYPE STANDARD TABLE OF Z6MMA_RGPNRGP_HD,
         ls_hd LIKE LINE OF lt_hd.
  DATA : lv_stepl           TYPE systepl,
         lv_gtype           TYPE Zzgtype.

  CLEAR : lv_stepl, ls_dynflds, lt_dynflds, ls_hd,
          lt_hd, lt_return_tab, ls_ret_tab, lv_gtype, lt_list, ls_list.
  REFRESH : lt_dynflds, lt_hd, lt_return_tab, lt_list.

  CALL FUNCTION 'DYNP_GET_STEPL'
    IMPORTING
      povstepl        = lv_stepl
    EXCEPTIONS
      stepl_not_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    REFRESH: lt_dynflds.
    CLEAR:   ls_dynflds.
    ls_dynflds-fieldname = 'GS_HD_MOD-GTYPE'.
    ls_dynflds-stepl     = lv_stepl.
    APPEND ls_dynflds TO lt_dynflds.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynflds
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc = '0'.
      READ TABLE lt_dynflds INTO ls_dynflds INDEX 1.
      IF sy-subrc = '0'.
        lv_gtype = ls_dynflds-fieldvalue.
      ENDIF.
      SELECT * FROM Z6MMA_RGPNRGP_HD INTO TABLE lt_hd
                    WHERE gtype = lv_gtype.
      LOOP AT lt_hd INTO ls_hd.
        MOVE ls_hd-gtype TO ls_list-gtype.
        MOVE ls_hd-werks TO ls_list-werks.
        APPEND ls_list TO lt_list.
        CLEAR : ls_list, ls_hd.
      ENDLOOP.
      DELETE ADJACENT DUPLICATES FROM lt_list COMPARING ALL FIELDS.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield    = 'WERKS'
          dynpprog    = sy-repid
          dynpnr      = sy-dynnr
          dynprofield = 'GS_HD_MOD-WERKS'
          value_org   = 'S'
        TABLES
          value_tab   = lt_list
          return_tab  = lt_return_tab
        EXCEPTIONS
          OTHERS      = 0.
      READ TABLE lt_return_tab INTO ls_ret_tab
           INDEX 1.
      IF sy-subrc = '0'.
        READ TABLE lt_list INTO ls_list
                         WITH KEY werks = ls_ret_tab-fieldval.
        IF sy-subrc = 0.
          REFRESH: lt_dynflds.
          CLEAR:   ls_dynflds.
          ls_dynflds-fieldname  = 'GS_HD_MOD-GTYPE'.
          ls_dynflds-fieldvalue = ls_list-gtype.
          ls_dynflds-stepl      = lv_stepl.
          APPEND ls_dynflds TO lt_dynflds.
          CLEAR:   ls_dynflds.
          ls_dynflds-fieldname  = 'GS_HD_MOD-WERKS'.
          ls_dynflds-fieldvalue = ls_ret_tab-fieldval.
          ls_dynflds-stepl      = lv_stepl.
          APPEND ls_dynflds TO lt_dynflds.

          CALL FUNCTION 'DYNP_VALUES_UPDATE'
            EXPORTING
              dyname               = sy-repid
              dynumb               = sy-dynnr
            TABLES
              dynpfields           = lt_dynflds
            EXCEPTIONS
              invalid_abapworkarea = 1
              invalid_dynprofield  = 2
              invalid_dynproname   = 3
              invalid_dynpronummer = 4
              invalid_request      = 5
              no_fielddescription  = 6
              undefind_error       = 7
              OTHERS               = 8.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " ZM_GET_WROKS  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_GET_GNO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_get_gno INPUT.
  TYPES : BEGIN OF typ_list1,
              gtype TYPE Zzgtype,
              werks TYPE werks_d,
              year  TYPE Zzyear,
              gno   TYPE Zzgno,
            END OF typ_list1.
  DATA : lt_list1 TYPE STANDARD TABLE OF typ_list1,
        ls_list1 TYPE typ_list1.
  DATA: lt_dynflds1         TYPE TABLE OF dynpread,
        ls_dynflds1         TYPE dynpread,
        lt_return_tab1      TYPE STANDARD TABLE OF ddshretval,
        ls_ret_tab1         TYPE ddshretval.
  DATA : lt_hd1 TYPE STANDARD TABLE OF Z6MMA_RGPNRGP_HD,
         ls_hd1 LIKE LINE OF lt_hd.
  DATA : lv_stepl1           TYPE systepl,
         lv_gtype1           TYPE Zzgtype,
         lv_werks           TYPE werks_d,
         lv_gyear           TYPE Zzyear.

  CLEAR : lv_stepl1, ls_dynflds1, lt_dynflds1, ls_hd1,
            lt_hd1, lt_return_tab1, ls_ret_tab1, lv_gtype1, lt_list1,
            ls_list1, lv_werks, lv_gyear.
  REFRESH : lt_dynflds1, lt_hd1, lt_return_tab1, lt_list1.

  CALL FUNCTION 'DYNP_GET_STEPL'
    IMPORTING
      povstepl        = lv_stepl1
    EXCEPTIONS
      stepl_not_found = 1
      OTHERS          = 2.
  IF sy-subrc = 0.
    REFRESH: lt_dynflds1.
    CLEAR:   ls_dynflds1.
    ls_dynflds1-fieldname = 'GS_HD_MOD-GTYPE'.
    ls_dynflds1-stepl     = lv_stepl1.
    APPEND ls_dynflds1 TO lt_dynflds1.
    ls_dynflds1-fieldname = 'GS_HD_MOD-WERKS'.
    ls_dynflds1-stepl     = lv_stepl1.
    APPEND ls_dynflds1 TO lt_dynflds1.
    ls_dynflds1-fieldname = 'GS_HD_MOD-GYEAR'.
    ls_dynflds1-stepl     = lv_stepl1.
    APPEND ls_dynflds1 TO lt_dynflds1.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = lt_dynflds1
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc = '0'.
      CLEAR : ls_dynflds1.
      READ TABLE lt_dynflds1 INTO ls_dynflds1 INDEX 1.
      IF sy-subrc = '0'.
        lv_gtype1 = ls_dynflds1-fieldvalue.
      ENDIF.
      CLEAR : ls_dynflds1.
      READ TABLE lt_dynflds1 INTO ls_dynflds1 INDEX 2.
      IF sy-subrc = '0'.
        lv_werks = ls_dynflds1-fieldvalue.
      ENDIF.
      CLEAR : ls_dynflds1.
      READ TABLE lt_dynflds1 INTO ls_dynflds1 INDEX 3.
      IF sy-subrc = '0'.
        lv_gyear = ls_dynflds1-fieldvalue.
      ENDIF.

      SELECT * FROM Z6MMA_RGPNRGP_HD INTO TABLE lt_hd1
                    WHERE werks = lv_werks
                      AND gtype = lv_gtype1
                      AND gyear = lv_gyear.
      LOOP AT lt_hd1 INTO ls_hd1.
        MOVE ls_hd1-gtype TO ls_list1-gtype.
        MOVE ls_hd1-werks TO ls_list1-werks.
        MOVE ls_hd1-gyear TO ls_list1-year.
        MOVE ls_hd1-gno TO ls_list1-gno.
        APPEND ls_list1 TO lt_list1.
        CLEAR : ls_list1, ls_hd1.
      ENDLOOP.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield    = 'GNO'
          dynpprog    = sy-repid
          dynpnr      = sy-dynnr
          dynprofield = 'GS_HD_MOD-GNO'
          value_org   = 'S'
        TABLES
          value_tab   = lt_list1
          return_tab  = lt_return_tab1
        EXCEPTIONS
          OTHERS      = 0.
      READ TABLE lt_return_tab1 INTO ls_ret_tab1
           INDEX 1.
      IF sy-subrc = '0'.
        READ TABLE lt_list1 INTO ls_list1
                         WITH KEY gno = ls_ret_tab1-fieldval.
        IF sy-subrc = 0.
          REFRESH: lt_dynflds1.
          CLEAR:   ls_dynflds1.
          ls_dynflds1-fieldname  = 'GS_HD_MOD-GTYPE'.
          ls_dynflds1-fieldvalue = ls_list1-gtype.
          ls_dynflds1-stepl      = lv_stepl1.
          APPEND ls_dynflds1 TO lt_dynflds1.
          CLEAR:   ls_dynflds1.

          ls_dynflds1-fieldname  = 'GS_HD_MOD-WERKS'.
          ls_dynflds1-fieldvalue = ls_list1-werks.
          ls_dynflds1-stepl      = lv_stepl1.
          APPEND ls_dynflds1 TO lt_dynflds1.
          CLEAR:   ls_dynflds1.

          ls_dynflds1-fieldname  = 'GS_HD_MOD-GYEAR'.
          ls_dynflds1-fieldvalue = ls_list1-year.
          ls_dynflds1-stepl      = lv_stepl1.
          APPEND ls_dynflds1 TO lt_dynflds1.
          CLEAR:   ls_dynflds1.

          ls_dynflds1-fieldname  = 'GS_HD_MOD-GNO'.
          ls_dynflds1-fieldvalue = ls_ret_tab1-fieldval.
          ls_dynflds1-stepl      = lv_stepl1.
          APPEND ls_dynflds1 TO lt_dynflds1.
        ENDIF.

        CALL FUNCTION 'DYNP_VALUES_UPDATE'
          EXPORTING
            dyname               = sy-repid
            dynumb               = sy-dynnr
          TABLES
            dynpfields           = lt_dynflds1
          EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " ZM_GET_GNO  INPUT

*&---------------------------------------------------------------------*
*&      Form  ZF_INSERT_INTO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_insert_into_table .
  DATA : lv_count(5) TYPE n.
  CLEAR : lv_count.
  DATA : lt_header TYPE STANDARD TABLE OF Z6MMA_RGPNRGP_HD.
  DATA : lt_detail TYPE STANDARD TABLE OF Z6MMA_RGPNRGP_DT.
  DATA : ls_detail TYPE Z6MMA_RGPNRGP_DT,
         lv_nr     TYPE nrnr,
         lv_toyear TYPE nryear,
         lv_suobj  TYPE nrsobj,
         lv_no(10) TYPE c.

*get the number ranges
  CLEAR : lv_nr, lv_toyear, lv_suobj, lv_no.
  lv_toyear = sy-datum+0(4).
  lv_suobj = gs_hd-werks.
  SELECT SINGLE nr FROM Z6MMA_RGPNRGP_nr INTO lv_nr
            WHERE werks = gs_hd-werks
              AND gjahr = sy-datum+0(4)
              AND gtype = gs_hd-gtype.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = lv_nr
      object      = 'ZRGPNRGP'
      subobject   = lv_suobj
      toyear      = lv_toyear
    IMPORTING
      number      = lv_no.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
*Insert date into the table Z6MMA_RGPNRGP_HD.
  gv_gno = lv_no.
  gs_hd-ernam = sy-uname.
  gs_hd-erdat = sy-datum.
  gs_hd-gyear = sy-datum+0(4).
  gs_hd-gno   = gv_gno.
  APPEND gs_hd TO lt_header.
  INSERT Z6MMA_RGPNRGP_HD FROM TABLE lt_header.

*Insert date into the table Z6MMA_RGPNRGP_DT.
  LOOP AT gt_item INTO gs_item.
    MOVE-CORRESPONDING gs_item TO ls_detail.
    lv_count = lv_count + 1.
    ls_detail-werks = gs_hd-werks.
    ls_detail-gtype = gs_hd-gtype.
    ls_detail-gyear = gs_hd-gyear.
    ls_detail-gno   = gs_hd-gno.
    ls_detail-gino  = lv_count.
    APPEND ls_detail TO lt_detail.
    CLEAR : ls_detail, gs_item.
  ENDLOOP.
  INSERT Z6MMA_RGPNRGP_DT FROM TABLE lt_detail.
ENDFORM.                    " ZF_INSERT_INTO_TABLE

*&---------------------------------------------------------------------*
*&      Form  ZF_GET_MAT_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ITEM_MATNR  text
*      <--P_LS_ITEM_MAKTX  text
*      <--P_LS_ITEM_UOM  text
*----------------------------------------------------------------------*
FORM zf_get_mat_desc  USING    p_ls_item_matnr TYPE matnr
                      CHANGING p_ls_item_maktx TYPE maktx
                               p_ls_item_uom   TYPE meins.
  DATA : lv_matnr TYPE matnr.
  CLEAR : lv_matnr.
  IF NOT p_ls_item_matnr IS INITIAL.
    lv_matnr = p_ls_item_matnr.
*** {chngd by snd
*{   REPLACE        SBXK900030                                        1
*\    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*\      EXPORTING
*\        input  = lv_matnr
*\      IMPORTING
*\        output = lv_matnr.
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
        input  = lv_matnr
      IMPORTING
        output = lv_matnr.

*}   REPLACE

*    UNPACK LV_MATNR to LV_MATNR.
data : gv_matnr type matnr.
select single matnr from mara
  into gv_matnr
  where matnr = lv_matnr.
  if sy-subrc ne 0.
    message e003(ZMM01).
  endif.
*** }chngd by snd
    SELECT SINGLE maktx FROM makt INTO p_ls_item_maktx
                  WHERE matnr = lv_matnr
                    AND spras = sy-langu.
    SELECT SINGLE meins FROM mara INTO p_ls_item_uom
                  WHERE matnr = lv_matnr.
  ENDIF.
ENDFORM.                    " ZF_GET_MAT_DESC

*&---------------------------------------------------------------------*
*&      Form  ZF_MODIFICATION_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ITEM_MATNR  text
*      -->P_LS_ITEM_MAKTX  text
*      -->P_LS_ITEM_O_QTY  text
*      -->P_LS_ITEM_UOM  text
*      -->P_LS_ITEM_DUE_DATE  text
*----------------------------------------------------------------------*
FORM zf_modification_check  USING    p_ls_item_matnr TYPE matnr
                                     p_ls_item_maktx TYPE maktx
                                     p_ls_item_o_qty TYPE kawrt
                                     p_ls_item_uom   TYPE meins
                                     p_ls_item_due_date TYPE datum.
  IF p_ls_item_uom IS INITIAL.
    MESSAGE i398(00) WITH 'Enter UOM'.
  ENDIF.
  IF p_ls_item_o_qty IS INITIAL.
    MESSAGE i398(00) WITH 'Enter Quantity'.
  ENDIF.
  IF gs_hd-gtype = 'NRGP'.
  ELSE.
    IF p_ls_item_due_date IS INITIAL.
      MESSAGE i398(00) WITH 'Enter Due Date'.
    ENDIF.
  ENDIF.

  IF ( p_ls_item_matnr IS INITIAL OR
    p_ls_item_matnr = '000000000000000000' )
                AND p_ls_item_maktx IS INITIAL.
    MESSAGE i398(00) WITH
    'Please enter material or material description' '' '' ''.
  ENDIF.
ENDFORM.                    " ZF_MODIFICATION_CHECK

*&---------------------------------------------------------------------*
*&      Form  ZF_MODFIY_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_modfiy_table .
  DATA : lt_detail  TYPE STANDARD TABLE OF Z6MMA_RGPNRGP_DT,
         lt_detail1 TYPE STANDARD TABLE OF Z6MMA_RGPNRGP_DT.
  DATA : ls_detail  TYPE Z6MMA_RGPNRGP_DT,
         ls_detail1 TYPE Z6MMA_RGPNRGP_DT.
  CLEAR : ls_detail1, ls_detail, lt_detail1, lt_detail.
  REFRESH : lt_detail1, lt_detail.
*MOdify data into the table Z6MMA_RGPNRGP_HD.
  UPDATE Z6MMA_RGPNRGP_HD SET reqs = gs_hd_mod-reqs
                          reason = gs_hd_mod-reason
                          remark = gs_hd_mod-remark
                          carrier = gs_hd_mod-carrier
                          vechical_no = gs_hd_mod-vechical_no
                          aedat = sy-datum
                          usnam = sy-uname
                    WHERE werks = gs_hd_mod-werks
                      AND gtype = gs_hd_mod-gtype
                      AND gyear = gs_hd_mod-gyear
                      AND gno = gs_hd_mod-gno.

*Modify data into the table Z6MMA_RGPNRGP_DT.
  SELECT * FROM Z6MMA_RGPNRGP_DT INTO TABLE lt_detail
           WHERE werks = gs_hd_mod-werks
             AND gtype = gs_hd_mod-gtype
             AND gyear = gs_hd_mod-gyear
             AND gno = gs_hd_mod-gno.

  LOOP AT gt_modify INTO gs_modify.
    READ TABLE lt_detail INTO ls_detail
                         WITH KEY werks = gs_hd_mod-werks
                                  gtype = gs_hd_mod-gtype
                                  gyear = gs_hd_mod-gyear
                                  gno = gs_hd_mod-gno
                                  gino = gs_modify-gino.
    IF sy-subrc = '0'.
      UPDATE Z6MMA_RGPNRGP_DT SET matnr = gs_modify-matnr
                               maktx = gs_modify-maktx
                               o_qty = gs_modify-o_qty
                               uom   = gs_modify-uom
                               due_date = gs_modify-due_date
                    WHERE werks = gs_hd_mod-werks
                      AND gtype = gs_hd_mod-gtype
                      AND gyear = gs_hd_mod-gyear
                      AND gno = gs_hd_mod-gno
                      AND gino = gs_modify-gino.
    ELSE.
      MOVE-CORRESPONDING gs_modify TO ls_detail1.
      ls_detail1-werks = gs_hd_mod-werks.
      ls_detail1-gtype = gs_hd_mod-gtype.
      ls_detail1-gyear = gs_hd_mod-gyear.
      ls_detail1-gno = gs_hd_mod-gno.
      APPEND ls_detail1 TO lt_detail1.
    ENDIF.
    CLEAR : ls_modify, ls_detail, ls_detail1.
  ENDLOOP.
  IF NOT lt_detail1[] IS INITIAL.
    INSERT Z6MMA_RGPNRGP_DT FROM TABLE lt_detail1.
  ENDIF.

  LOOP AT lt_detail INTO ls_detail.
    READ TABLE gt_modify INTO gs_modify
                         WITH KEY gino = ls_detail-gino.
    IF sy-subrc = '0'.
      DELETE TABLE lt_detail FROM ls_detail.
    ENDIF.
    CLEAR : ls_detail1, gs_modify.
  ENDLOOP.
  IF NOT lt_detail[] IS INITIAL.
    DELETE Z6MMA_RGPNRGP_DT FROM TABLE lt_detail.
  ENDIF.
ENDFORM.                    " ZF_MODFIY_TABLE

*&---------------------------------------------------------------------*
*&      Form  ZF_GET_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_get_details .
  CLEAR : p_delete, p_app.
  IF gv_clear = 'X'.
    CLEAR : gs_hd_mod, gs_display, gt_display, p_chk.
    REFRESH gt_display.

    LOOP AT SCREEN.
      IF screen-group1 = 'GR'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF NOT gs_hd_mod IS INITIAL AND gt_display IS INITIAL.
      SELECT SINGLE * FROM Z6MMA_RGPNRGP_HD
                      INTO gs_hd_mod
                      WHERE werks = gs_hd_mod-werks
                        AND gtype = gs_hd_mod-gtype
                        AND gyear = gs_hd_mod-gyear
                        AND gno = gs_hd_mod-gno.
      IF gs_hd_mod-loekz = 'X'.
        p_delete = 'X'.
      ELSEIF gs_hd_mod-app_stat = 'A'.
        p_app = 'X'.
      ENDIF.
      SELECT * FROM Z6MMA_RGPNRGP_DT
               INTO CORRESPONDING FIELDS OF TABLE gt_display
               WHERE werks = gs_hd_mod-werks
                        AND gtype = gs_hd_mod-gtype
                        AND gyear = gs_hd_mod-gyear
                        AND gno = gs_hd_mod-gno.
      IF NOT gt_display[] IS INITIAL.
        LOOP AT SCREEN.
          IF screen-group1 = 'GR'.
            screen-input = '0'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE i398(00) WITH 'Record not found' '' '' ''.
        CLEAR : gs_hd_mod, gt_display.
      ENDIF.

    ENDIF.
  ENDIF.
ENDFORM.                    " ZF_GET_DETAILS

*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_AUTHORIZATION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_check_authorization INPUT.
  DATA : ls_auth TYPE Z6MMA_RGPNRGP_AT.
  CLEAR : ls_auth.
  SELECT SINGLE * FROM Z6MMA_RGPNRGP_AT
                  INTO ls_auth
                 WHERE usnam = sy-uname
                   AND werks = gs_hd_mod-werks.
  IF sy-subrc <> '0'.
    MESSAGE e398(00) WITH 'You are not authorized for this plant' ''
'' ''.
  ENDIF.
ENDMODULE.                 " ZM_CHECK_AUTHORIZATION  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit INPUT.
  save_ok = sy-ucomm.
  CLEAR : sy-ucomm.
  CASE save_ok.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_AUTHORIZATION_C  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_check_authorization_c INPUT.
  DATA : ls_authc TYPE Z6MMA_RGPNRGP_AT.
  CLEAR : ls_authc.
  SELECT SINGLE * FROM Z6MMA_RGPNRGP_AT
                  INTO ls_authc
                 WHERE usnam = sy-uname
                   AND werks = gs_hd-werks.
  IF sy-subrc <> '0'.
    MESSAGE e398(00) WITH 'You are not authorized for this plant' ''
'' ''.
  ENDIF.
ENDMODULE.                 " ZM_CHECK_AUTHORIZATION_C  INPUT

*&---------------------------------------------------------------------*
*&      Form  ZF_CHECK_CUST_VEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_check_cust_vend .
  CLEAR : gv_flag.
  IF vend = 'X'.
    IF gs_hd-lifnr = ''.
      MESSAGE i398(00) WITH 'Please Enter the Vendor code' '' '' ''.
      gv_flag = 'X'.
*** {chngd by snd
    ELSE.
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_hd-lifnr
      IMPORTING
        output = gs_hd-lifnr.
      DATA : lv_lifnr TYPE lifnr.
      SELECT SINGLE lifnr FROM lfa1
        INTO lv_lifnr
        WHERE lifnr = gs_hd-lifnr.
      IF sy-subrc NE 0.
*        MESSAGE 'Enter correct Vendor' TYPE 'E'.
        message e002(ZMM01).
      ENDIF.
*** }chngd by snd
    ENDIF.
  ELSEIF cust = 'X'.
    IF gs_hd-kunnr = ''.
      MESSAGE i398(00) WITH 'Please Enter the Customer code' '' '' ''.
      gv_flag = 'X'.
*** {chngd by snd
    ELSE.
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_hd-kunnr
      IMPORTING
        output = gs_hd-kunnr.

      DATA : lv_kunnr TYPE kunnr.
      SELECT SINGLE kunnr FROM kna1
        INTO lv_kunnr
        WHERE kunnr = gs_hd-kunnr.
      IF sy-subrc NE 0.
*        MESSAGE 'Enter correct Customer' TYPE 'E'.
        message e001(ZMM01).
      ENDIF.
*** }chngd by snd
    ENDIF.
  ENDIF.
ENDFORM.                    " ZF_CHECK_CUST_VEND
*&---------------------------------------------------------------------*
*&      Form  ZF_UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_update_table .
  DATA : lv_qty_o TYPE kawrt,
         lv_qty_r TYPE kawrt,
         lv_qty   TYPE kawrt, lv_upqty TYPE kawrt.
  CLEAR : lv_qty_o, lv_qty_r, lv_qty, lv_upqty, lv_flag2.

  LOOP AT gt_update INTO gs_update.
    SELECT SINGLE o_qty r_qty FROM Z6MMA_RGPNRGP_DT
            INTO (lv_qty_o, lv_qty_r)
           WHERE werks = gs_hd_mod-werks
             AND gtype = gs_hd_mod-gtype
             AND gyear = gs_hd_mod-gyear
             AND gno = gs_hd_mod-gno
             AND gino = gs_update-gino.
    lv_qty = lv_qty_o - lv_qty_r.
    IF gs_update-r_qty LE lv_qty.
      lv_upqty = gs_update-r_qty + lv_qty_r.
      IF lv_qty_o = lv_upqty.
        UPDATE Z6MMA_RGPNRGP_DT SET r_qty = lv_upqty
                                 status = 'U'
                          WHERE werks = gs_hd_mod-werks
                            AND gtype = gs_hd_mod-gtype
                            AND gyear = gs_hd_mod-gyear
                            AND gno = gs_hd_mod-gno
                            AND gino = gs_update-gino.
        lv_flag2 = 'X'.
      ELSE.
        UPDATE Z6MMA_RGPNRGP_DT SET r_qty = lv_upqty
                          WHERE werks = gs_hd_mod-werks
                            AND gtype = gs_hd_mod-gtype
                            AND gyear = gs_hd_mod-gyear
                            AND gno = gs_hd_mod-gno
                            AND gino = gs_update-gino.
        lv_flag2 = 'X'.
      ENDIF.
      clear st_Z6MMA_RGPNRGP_HT.
      select single max( girno ) from Z6MMA_RGPNRGP_HT into v_girno
                        WHERE werks = gs_hd_mod-werks
                            AND gtype = gs_hd_mod-gtype
                            AND gyear = gs_hd_mod-gyear
                            AND gno = gs_hd_mod-gno
                            AND gino = gs_update-gino.

      move-corresponding gs_update to st_Z6MMA_RGPNRGP_HT.
      move-corresponding gs_hd_mod to st_z6mma_rgpnrgp_ht.
      st_z6mma_rgpnrgp_ht-rdate = sy-datum.
      st_z6mma_rgpnrgp_ht-uname = sy-uname.
      st_z6mma_rgpnrgp_ht-uzeit = sy-uzeit.
      st_z6mma_rgpnrgp_ht-r_Qty = gs_update-r_qty.

      st_z6mma_rgpnrgp_ht-girno = v_girno + 1.
      move-CORRESPONDING st_Z6MMA_RGPNRGP_HT to Z6MMA_RGPNRGP_HT.
      modify Z6MMA_RGPNRGP_HT.
      clear Z6MMA_RGPNRGP_HT.
      clear st_Z6MMA_RGPNRGP_HT.

    ELSE.
      MESSAGE i398(00) WITH 'Recieved qty is greter than Order qty'.
      EXIT.
    ENDIF.
    CLEAR : lv_qty_o, lv_qty_r, lv_qty, lv_upqty, gs_update.
  ENDLOOP.
ENDFORM.                    " ZF_UPDATE_TABLE

*&---------------------------------------------------------------------*
*&      Form  ZF_PRINT_GATE_PASS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_print_gate_pass .
  DATA : gv_fsname TYPE rs38l_fnam.
  CLEAR : gv_fsname.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'Z6MM005S_RGP_NRGP'
    IMPORTING
      fm_name            = gv_fsname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION gv_fsname
    EXPORTING
      werks            = gs_hd_mod-werks
      gtype            = gs_hd_mod-gtype
      gyear            = gs_hd_mod-gyear
      gno              = gs_hd_mod-gno
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ZF_PRINT_GATE_PASS
*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_GTYPE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZM_CHECK_GTYPE INPUT.

ENDMODULE.                 " ZM_CHECK_GTYPE  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_CHECK_GTYPE_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZM_CHECK_GTYPE_9002 INPUT.
IF GS_HD_mod-GTYPE EQ 'RGP' AND GS_HD_mod-EBELN IS INITIAL.

  IF sy-subrc <> '0'.
    MESSAGE e398(00) WITH 'Purchase Order Number Required for RGP Gate Pass'.
  endif.

  ENDIF.
ENDMODULE.                 " ZM_CHECK_GTYPE_9002  INPUT
