*&---------------------------------------------------------------------*
*&  Include           ZMM_STOCK_DETAILS_M_DD_EXP90
*&---------------------------------------------------------------------*

TYPES :BEGIN OF ty_mara,
  matnr TYPE mara-matnr,
  mtart TYPE mara-mtart,
  matkl TYPE mara-matkl,
  meins TYPE mara-meins,
  spart TYPE mara-spart,
  extwg TYPE mara-extwg,
END OF ty_mara,

BEGIN OF ty_makt,
  matnr TYPE makt-matnr,
  spras TYPE makt-spras,
  maktx TYPE makt-maktx,
END OF ty_makt,

BEGIN OF ty_mard,
  matnr TYPE mard-matnr,
  werks TYPE mard-werks,
  lgort TYPE mard-lgort,
  labst TYPE mard-labst,
END OF ty_mard,

BEGIN OF ty_mchb,
  matnr TYPE mchb-matnr,
  werks TYPE mchb-werks,
  lgort TYPE mchb-lgort,
  charg TYPE mchb-charg,
  ERSDA TYPE mchb-ERSDA,
  laeda TYPE mchb-laeda,
  clabs TYPE mchb-clabs,
END OF ty_mchb,

BEGIN OF ty_mch1,
  matnr TYPE mch1-matnr,
  charg TYPE mch1-charg,
  laeda TYPE mch1-laeda,
  vfdat TYPE mch1-vfdat,
  hsdat TYPE mch1-hsdat,
END OF ty_mch1.


DATA :
      gt_user  TYPE TABLE OF zatr_user_dm,
      gt_userm TYPE TABLE OF zatr_user_m,
      gt_mara  TYPE TABLE OF ty_mara,
      gt_makt  TYPE TABLE OF ty_makt,
      gt_mard  TYPE TABLE OF ty_mard,
      gt_mchb  TYPE TABLE OF ty_mchb,
      gt_mchb1 TYPE TABLE OF ty_mchb,
      gt_mch1  TYPE TABLE OF ty_mch1,


      gs_user  LIKE LINE OF gt_user,
      gs_userm LIKE LINE OF gt_userm,
      gs_mara  LIKE LINE OF gt_mara,
      gs_makt  LIKE LINE OF gt_makt,
      gs_mard  LIKE LINE OF gt_mard,
      gs_mchb  LIKE LINE OF gt_mchb,
      gs_mchb1 LIKE LINE OF gt_mchb,
      gs_mch1  LIKE LINE OF gt_mch1,

BEGIN OF gs_string,
  clabs TYPE string,
  date1 TYPE string,
  date2 TYPE string,
  date3 TYPE string,
END OF gs_string.

DATA: fieldcatalog  TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      fieldcatalog1 TYPE slis_t_fieldcat_alv ."WITH HEADER LINE.
DATA: list_layout   TYPE slis_layout_alv.

DATA : g_save(1) TYPE C,
      g_variant LIKE disvariant,
      gx_variant LIKE disvariant,
      f2code   LIKE sy-ucomm VALUE  '&IC1',
      gt_list_top_of_page TYPE slis_t_listheader,
      gt_events           TYPE slis_t_event,
      gt_sort             TYPE TABLE OF slis_t_sortinfo_alv,
      gs_sort             TYPE slis_sortinfo_alv,
      gs_layout           TYPE slis_layout_alv,
      g_exit(1) TYPE C,
      heading  TYPE slis_t_listheader,
      layout   TYPE slis_layout_alv,
      msg TYPE string,
      empno TYPE zatr_user_m-user_id.
CONSTANTS: formname_top_of_page  TYPE slis_formname VALUE 'TOP_OF_PAGE',
con_tab  TYPE C VALUE cl_abap_char_utilities=>horizontal_tab,
con_cret TYPE C VALUE cl_abap_char_utilities=>cr_lf.

**************** Mail Details *************************
DATA:it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
      WITH HEADER LINE,
      it_attach TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
      WITH HEADER LINE,
      t_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
      t_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      objbin    LIKE solix OCCURS 10 WITH HEADER LINE,
      t_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
      t_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
      w_cnt TYPE I,
      w_sent_all(1) TYPE C,
      w_doc_data LIKE sodocchgi1,
      gd_error    TYPE sy-subrc,
      gd_reciever TYPE sy-subrc,
      success TYPE sy-subrc,
      filename(10000) TYPE C,
      DATE TYPE string,
      TIME TYPE string,


      p_empcode TYPE zatr_user_m-user_id,
      ename     TYPE zatr_user_m-user_name,
      email     TYPE zatr_user_m-email_id,

      ID     TYPE zatr_user_m-user_id,
      ername TYPE zatr_user_m-user_name,
      ermail TYPE zatr_user_m-email_id.



DATA : str  TYPE string.
DATA: ld_error    TYPE sy-subrc,
      ld_reciever TYPE sy-subrc,
      ld_mtitle LIKE sodocchgi1-obj_descr,
      ld_email LIKE  somlreci1-receiver,
      ld_format TYPE  so_obj_tp ,
      ld_attdescription TYPE  so_obj_nam ,
      ld_attfilename TYPE  so_obj_des ,
      ld_sender_address LIKE  soextreci1-receiver,
      ld_sender_address_type LIKE  soextreci1-adr_typ,
      ld_receiver LIKE  sy-subrc.

*  LD_EMAIL   = P_EMAIL.

DATA: wa_input  TYPE p DECIMALS 8,
      wa_output TYPE p DECIMALS 2.

DATA: objpack   LIKE sopcklsti1 OCCURS 2 WITH HEADER LINE.
DATA: objhead   LIKE solisti1 OCCURS 1 WITH HEADER LINE.
DATA: objtxt    LIKE solisti1 OCCURS 10 WITH HEADER LINE.
DATA: reclist   LIKE somlreci1 OCCURS 5 WITH HEADER LINE.
DATA: doc_chng  LIKE sodocchgi1.
DATA: tab_lines LIKE sy-tabix.
DATA: l_num(3).
DATA: subj_date(10) TYPE C,
      gv_date TYPE d,"sy-datum,
      gv_day TYPE I,
      gv_date1 TYPE d,
      gv_exp_day type i.


**************** Mail Details *************************
