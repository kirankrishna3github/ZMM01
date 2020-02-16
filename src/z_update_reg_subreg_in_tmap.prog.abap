*&---------------------------------------------------------------------*
*& Report  Z_UPDATE_REG_SUBREG_IN_TMAP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_UPDATE_REG_SUBREG_IN_TMAP.



*&*********************************************************************&*
*&                    Data Declarations                                &*
*&*********************************************************************&*
data : it_final like alsmex_tabline occurs 0 with header line,
       flag(1) type c.

DATA : BEGIN OF WA,
      VKGRP TYPE ZATR_USER_TMAP-VKGRP,
      BZIRK TYPE ZATR_USER_TMAP-BZIRK,
      KVGR1 TYPE ZATR_USER_TMAP-KVGR1,
      END OF WA,

       it like table of  WA,
       wa_final like line  of  it.


data a type char01.


*&*********************************************************************&*
*&                    SELECTION SCREEN                                 &*
*&*********************************************************************&*
selection-screen begin of block a with frame title text-001.

parameters:  i_file    type rlgrap-filename modif id kp.         " INPUT PARAMETER TO READ PATH FOR INPUT FILE

selection-screen end of block a.



at selection-screen on value-request for i_file.
*------------------------------------------------" READ INPUT FILE PATH

  call function 'F4_FILENAME'
    exporting
    program_name  = syst-cprog
    dynpro_number = syst-dynnr
    field_name    = ' '
    importing
    file_name     = i_file.


  start-of-selection.
*----------------------------------------------"  START OF SELECTION - EXTRACT DATA TO INTERNAL TABLE
call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
  exporting
    filename                          = i_file
    i_begin_col                       = '1'
    i_begin_row                       = '1'
    i_end_col                         = '3'
    i_end_row                         = '65536'
  tables
    intern                        = it_final
 exceptions
   inconsistent_parameters       = 1
   upload_ole                    = 2
   others                        = 3.
if sy-subrc <> 0.
 message id sy-msgid type sy-msgty number sy-msgno
         with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
endif.


 loop at it_final.

   case it_final-col.
      when '0001'.

        wa_final-VKGRP            = it_final-value.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input         = wa_final-VKGRP
         IMPORTING
           OUTPUT        = wa_final-VKGRP .

        condense wa_final-VKGRP.
      when '0002'.
        wa_final-BZIRK             = it_final-value.
        condense wa_final-BZIRK.
      when '0003'.
        wa_final-KVGR1             = it_final-value.
        flag = 'X'.
      endcase.

    if flag eq 'X'.
      append  wa_final to it.
      clear:  wa_final, flag.
    endif.

    clear : it_final.
endloop.

DATA : WA_TMAP TYPE ZATR_USER_TMAP,
       IT_TMAP LIKE TABLE OF WA_TMAP.

SELECT * FROM ZATR_USER_TMAP INTO TABLE IT_TMAP.

  LOOP AT IT_TMAP INTO WA_TMAP.

  READ TABLE IT INTO WA WITH KEY VKGRP = WA_TMAP-VKGRP.
  IF SY-SUBRC = 0.
    WA_TMAP-BZIRK = WA-BZIRK.
    WA_TMAP-KVGR1 = WA-KVGR1.
    UPDATE ZATR_USER_TMAP FROM WA_TMAP.
  ENDIF.

   CLEAR : WA, WA_TMAP.
  ENDLOOP.

  MESSAGE 'UPDATED' TYPE 'S'.
