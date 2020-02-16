*&---------------------------------------------------------------------*
*&  Include           ZMM_PO_SEND_MAIL_DD
*&---------------------------------------------------------------------*

TYPES :
      BEGIN OF ty_ekko,
        ebeln TYPE ekko-ebeln,
        bukrs TYPE ekko-bukrs,
        bstyp TYPE ekko-bstyp,
        bsart TYPE ekko-bsart,
        aedat TYPE ekko-aedat,
        lifnr TYPE ekko-lifnr,
        spras TYPE ekko-spras,
        FRGKE TYPE ekko-FRGKE,
*        RLWRT TYPE ekko-RLWRT,
      END OF ty_ekko,

      BEGIN OF ty_lfa1,
        lifnr TYPE lfa1-lifnr,
        land1 TYPE lfa1-land1,
        name1 TYPE lfa1-name1,
        adrnr TYPE lfa1-adrnr,
      END OF ty_lfa1,

      BEGIN OF ty_adr6,
        addrnumber TYPE adr6-addrnumber,
        persnumber TYPE adr6-persnumber,
        date_from TYPE adr6-date_from,
        consnumber TYPE adr6-consnumber,
        smtp_addr TYPE adr6-smtp_addr,
      END OF ty_adr6,

      BEGIN OF ty_adr2,
        addrnumber TYPE adr2-addrnumber,
        persnumber TYPE adr2-persnumber,
        date_from  TYPE adr2-date_from,
        consnumber TYPE adr2-consnumber,
        tel_number TYPE adr2-tel_number,
        dft_receiv TYPE adr2-dft_receiv,
      END OF ty_adr2,


      BEGIN OF ty_eket,
        ebeln TYPE eket-ebeln,
        ebelp TYPE eket-ebelp,
        etenr TYPE eket-etenr,
        eindt TYPE eket-eindt,
        menge TYPE eket-menge,
      END OF ty_eket,

      BEGIN OF ty_final,
        chk          TYPE C,
        ebeln        TYPE ekko-ebeln,
        aedat        TYPE ekko-aedat,
        bsart        TYPE ekko-bsart,
        lifnr        TYPE ekko-lifnr,
        name1        TYPE lfa1-name1,
        SMTP_ADDR    TYPE adr6-SMTP_ADDR,
        TEL_NUMBER   TYPE adr2-TEL_NUMBER,
        RLWRT        TYPE ekko-RLWRT,
        field_style  TYPE lvc_t_styl,
      END OF ty_final.


data : gt_ekko   TYPE TABLE OF ty_ekko,
       gt_lfa1   TYPE TABLE OF ty_lfa1,
       gt_adr6   TYPE TABLE OF ty_adr6,
       gt_adr2   TYPE TABLE OF ty_adr2,
       gt_eket   TYPE TABLE OF ty_eket,
       gt_final  TYPE TABLE OF ty_final,

       gs_ekko   TYPE ty_ekko,
       gs_lfa1   TYPE ty_lfa1,
       gs_adr6   TYPE ty_adr6,
       gs_adr2   TYPE ty_adr2,
       gs_eket   TYPE ty_eket,
       gs_final  TYPE ty_final,
       gs_final1 TYPE ty_final
       .



********************************** ALV Fields *****************************
DATA:
***      fieldcatalog  TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      fieldcatalog1 TYPE slis_t_fieldcat_alv ,"WITH HEADER LINE,
fieldcatalog  TYPE lvc_t_fcat with header line.

***lvc_s_fcat
***DATA: list_layout   TYPE slis_layout_alv."lvc_s_layo
DATA: list_layout   TYPE lvc_s_layo.

DATA : g_save(1) TYPE c,
       g_variant LIKE disvariant,
       gx_variant LIKE disvariant,
       f2code   LIKE sy-ucomm VALUE  '&IC1',
       gt_list_top_of_page TYPE slis_t_listheader,
       gt_events           TYPE slis_t_event,
       gt_sort             TYPE TABLE OF slis_t_sortinfo_alv,
       gs_sort             TYPE slis_sortinfo_alv,
       gs_layout           TYPE slis_layout_alv,
       g_exit(1) TYPE c,
       heading  TYPE slis_t_listheader,
***       layout   TYPE slis_layout_alv,"lvc_s_layo
       layout   TYPE lvc_s_layo,"
       msg TYPE string,
       empno TYPE zatr_user_m-user_id,
       e_grid TYPE REF TO cl_gui_alv_grid..
CONSTANTS: formname_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE'.
********************************** ALV Fields *****************************

****************** for calling Smartforms ****************
DATA : I_EKKO LIKE EKKO OCCURS 0 WITH HEADER LINE,
       WA_EKKO LIKE LINE OF I_EKKO,
       IT_PDFDATA LIKE TLINE OCCURS 0 WITH HEADER LINE,
       I_EKPO LIKE EKPO OCCURS 0 WITH HEADER LINE,
       I_ML_ESLL LIKE ML_ESLL OCCURS 0 WITH HEADER LINE,
*{   REPLACE        SBXK900019                                        1
*\       I_KONV LIKE KONV OCCURS 0 WITH HEADER LINE,
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************

       I_KONV LIKE PRCD_ELEMENTS OCCURS 0 WITH HEADER LINE,
*}   REPLACE
       I_KOMV LIKE KOMV OCCURS 0 WITH HEADER LINE,
       TT_KOMV TYPE KOMV_ITAB,
       WA_KOMV TYPE KOMV,
       WA_KOMV_JVCS TYPE KOMV,
       I_MDSB LIKE MDSB OCCURS 0 WITH HEADER LINE,
       WA_LFM1 LIKE LFM1,
       WRK_FILESIZ(10) TYPE C,
       LF_FM_NAME TYPE RS38L_FNAM,
       ls_stylerow TYPE lvc_s_styl ,
       lt_styletab TYPE lvc_t_styl .
data : fp_controlparams type SSFCTRLOP,
         fp_outputparams type SSFCOMPOP.

  data : it_job_output_info type SSFCRESCL.
  data : it_otfdata type TABLE OF SSFCRESCL-otfdata.
  data : wa_otfdata type ITCOO.
****************** for calling Smartforms ****************


**************** Mail Details *************************
DATA :it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
             WITH HEADER LINE,
     it_attach TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
             WITH HEADER LINE,
     t_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
     t_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
     objbin    LIKE solix OCCURS 10 WITH HEADER LINE,
     t_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
     t_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
     t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
     w_cnt TYPE i,
     w_sent_all(1) TYPE c,
     w_doc_data LIKE sodocchgi1,
     gd_error    TYPE sy-subrc,
     gd_reciever TYPE sy-subrc,
     success TYPE sy-subrc,

     ld_error    TYPE sy-subrc,
     ld_reciever TYPE sy-subrc,
     ld_mtitle LIKE sodocchgi1-obj_descr,
     ld_email LIKE  somlreci1-receiver,
     ld_format TYPE  so_obj_tp ,
     ld_attdescription TYPE  so_obj_nam ,
     ld_attfilename TYPE  so_obj_des ,
     ld_sender_address LIKE  soextreci1-receiver,
     ld_sender_address_type LIKE  soextreci1-adr_typ,
     ld_receiver LIKE  sy-subrc,
     cnt  type i,
     cstr TYPE string,
     smsg TYPE string.
**************** Mail Details *************************
