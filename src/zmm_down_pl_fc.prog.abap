*&---------------------------------------------------------------------*
*& Report  ZMM_DOWN_PL_FC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZMM_DOWN_PL_FC.


data : a type zmis_sales_data-vkorg,
       b type zmis_sales_data-vtweg,
       c type zmis_sales_data-spart,
       d type zmis_sales_data-werks,
       e type zmis_sales_data-zmonth,
       f type zmis_sales_data-zyear.

data :  wa_final TYPE zsd_ho_sale_fc,

        it_final like table of wa_final.

*&*********************************************************************&*
*&                    SELECTION SCREEN                                 &*
*&*********************************************************************&*
SELECTION-SCREEN BEGIN OF BLOCK A WITH FRAME TITLE TEXT-001.


SELECTION-SCREEN BEGIN OF BLOCK B  WITH FRAME.

 select-options : so_vkorg  for a modif id  pk  default 1000,                           " SALES ORGAN
                  so_vtweg  for b modif id  pk  default 10,                             " DIST CHANNEL
                  so_spart  for c modif id  pk  default 10,                             " DIVISION
                  so_werks  for d modif id  pk,                                         " PLANT
                  so_zmont  for e modif id  pk  default sy-datum+4(2)
                                                no intervals no-extension obligatory,  " MONTH
                  so_zyear  for f modif id  pk  default sy-datum+0(4) obligatory
                                                no intervals no-extension.             " YEAR


SELECTION-SCREEN END OF BLOCK B.



PARAMETERS:  I_FILE    TYPE RLGRAP-FILENAME DEFAULT 'C:\PLAN.XLS'.      " INPUT PARAMETER TO READ PATH FOR DOWNLOAD FILE

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS RB17  RADIOBUTTON GROUP G6    MODIF ID KP DEFAULT 'X'.
SELECTION-SCREEN COMMENT 8(50) TEXT-211  MODIF ID KP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS RB18  RADIOBUTTON GROUP G6    MODIF ID KP.
SELECTION-SCREEN COMMENT 8(50) TEXT-212  MODIF ID KP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS RB19  RADIOBUTTON GROUP G6   MODIF ID KP.
SELECTION-SCREEN COMMENT 8(50) TEXT-213 MODIF ID KP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK A.

AT SELECTION-SCREEN OUTPUT.
*-------------------------------------------------" MODIFY SCREEN
  LOOP AT SCREEN.
    IF screen-GROUP1 = 'KP'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR I_FILE.
*------------------------------------------------" READ INPUT FILE PATH

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
    PROGRAM_NAME  = SYST-CPROG
    DYNPRO_NUMBER = SYST-DYNNR
    FIELD_NAME    = ' '
    IMPORTING
    FILE_NAME     = I_FILE.


 START-OF-SELECTION.

select * from zsd_pl_sale_fc into table it_final where  vkorg in so_vkorg
                                                  and   vtweg in so_vtweg
                                                  and   spart in so_spart
                                                  and   werks  in so_werks
                                                  and   zmonth in so_zmont
                                                  and   zyear  in so_zyear.

  IF RB17 = 'X'.

    SORT IT_FINAL BY WERKS MATNR.

  ELSEIF RB18 = 'X'.

    SORT IT_FINAL BY WERKS EXTWG.

  ELSE.
    SORT IT_FINAL BY MATNR.

  ENDIF.

  DATA FILE TYPE STRING.

FILE = I_FILE.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
*     BIN_FILESIZE                    =
      filename                        = FILE
      FILETYPE                        = 'DAT'
*     APPEND                          = ' '
*     WRITE_FIELD_SEPARATOR           = ' '
      HEADER                          = '00'
*     TRUNC_TRAILING_BLANKS           = ' '
*     WRITE_LF                        = 'X'
*     COL_SELECT                      = ' '
*     COL_SELECT_MASK                 = ' '
*     DAT_MODE                        = ' '
*     CONFIRM_OVERWRITE               = ' '
*     NO_AUTH_CHECK                   = ' '
*     CODEPAGE                        = ' '
*     IGNORE_CERR                     = ABAP_TRUE
*     REPLACEMENT                     = '#'
*     WRITE_BOM                       = ' '
*     TRUNC_TRAILING_BLANKS_EOL       = 'X'
*     WK1_N_FORMAT                    = ' '
*     WK1_N_SIZE                      = ' '
*     WK1_T_FORMAT                    = ' '
*     WK1_T_SIZE                      = ' '
*     WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*     SHOW_TRANSFER_STATUS            = ABAP_TRUE
*     VIRUS_SCAN_PROFILE              = '/SCET/GUI_DOWNLOAD'
*   IMPORTING
*     FILELENGTH                      =
    tables
      data_tab                        = IT_FINAL
*     FIELDNAMES                      =
   EXCEPTIONS
     FILE_WRITE_ERROR                = 1
     NO_BATCH                        = 2
     GUI_REFUSE_FILETRANSFER         = 3
     INVALID_TYPE                    = 4
     NO_AUTHORITY                    = 5
     UNKNOWN_ERROR                   = 6
     HEADER_NOT_ALLOWED              = 7
     SEPARATOR_NOT_ALLOWED           = 8
     FILESIZE_NOT_ALLOWED            = 9
     HEADER_TOO_LONG                 = 10
     DP_ERROR_CREATE                 = 11
     DP_ERROR_SEND                   = 12
     DP_ERROR_WRITE                  = 13
     UNKNOWN_DP_ERROR                = 14
     ACCESS_DENIED                   = 15
     DP_OUT_OF_MEMORY                = 16
     DISK_FULL                       = 17
     DP_TIMEOUT                      = 18
     FILE_NOT_FOUND                  = 19
     DATAPROVIDER_EXCEPTION          = 20
     CONTROL_FLUSH_ERROR             = 21
     OTHERS                          = 22
            .
  IF sy-subrc <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
