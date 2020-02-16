*----------------------------------------------------------------------*
***INCLUDE Z6MM005R_RGP_NRGP_I01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
v_ok_code = sy-ucomm.
CLEAR SY-UCOMM.
case  v_ok_CODE.
  WHEN 'TRAN01'.

    CALL TRANSACTION 'ZMM005_1'.

  WHEN 'TRAN02'.

    CALL TRANSACTION 'ZMM005_2'.
  WHEN 'TRAN03'.

    CALL TRANSACTION 'ZMM005_3'.

  WHEN 'TRAN04'.

    CALL TRANSACTION 'ZMM005_4'.

  WHEN 'TRAN05'.

    CALL TRANSACTION 'ZMM005_5'.

   WHEN 'TRAN06'.

    CALL TRANSACTION 'ZMM005_7'.

    WHEN 'REP01'.

    CALL TRANSACTION 'ZMM005_6'.

    WHEN 'CUST01'.

    CALL TRANSACTION 'ZMM005_C2'.

    WHEN 'CUST02'.

    CALL TRANSACTION 'ZMM005_C1'.

ENDCASE.

 CLEAR V_OK_CODE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZM_EXIT INPUT.
v_ok_Code = sy-ucomm.
  CLEAR : sy-ucomm.
  CASE v_ok_code.
    WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " ZM_EXIT  INPUT
