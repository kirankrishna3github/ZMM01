*&---------------------------------------------------------------------*
*&  Include           ZMM_STOCK_DETAILS_M_FD_EXP90
*&---------------------------------------------------------------------*

************************************ logic ***************************
FORM get_data.
  SELECT matnr werks lgort charg ersda laeda clabs
  FROM mchb
  INTO TABLE gt_mchb
  WHERE lgort IN ('1501' , '1601')
  AND   clabs NE 0.

  IF sy-subrc = 0.
    SELECT matnr charg laeda vfdat hsdat
    FROM mch1
    INTO TABLE gt_mch1
    FOR ALL ENTRIES IN gt_mchb
    WHERE matnr = gt_mchb-matnr
    AND   charg = gt_mchb-charg.

    SELECT matnr mtart matkl meins spart extwg
    FROM mara
    INTO TABLE gt_mara
    FOR ALL ENTRIES IN gt_mchb
    WHERE matnr EQ gt_mchb-matnr
    AND mtart IN ('ZFGM', 'ZTRD')
    AND spart = '10'.

    IF sy-subrc = 0.
      SELECT matnr spras maktx
      FROM makt
      INTO TABLE gt_makt
      FOR ALL ENTRIES IN gt_mara
      WHERE matnr EQ gt_mara-matnr
      AND   spras EQ sy-langu.
    ENDIF.

    gt_mchb1[] = gt_mchb[].
    SORT gt_mchb1 BY werks.
    DELETE ADJACENT DUPLICATES FROM gt_mchb1 COMPARING werks.
  ENDIF.

  SORT : gt_mard  BY matnr,
  gt_mchb  BY matnr,
  gt_mchb1 BY werks,
  gt_mara  BY matnr,
  gt_makt  BY matnr,
  gt_mch1  BY matnr charg.

  IF gt_mchb1[] IS NOT INITIAL.
    SELECT *
    FROM zatr_user_dm
    INTO TABLE gt_user
    FOR ALL ENTRIES IN gt_mchb1
    WHERE vkbur EQ gt_mchb1-werks.
    DELETE gt_user WHERE status EQ 'X'.
    IF gt_user[] IS NOT INITIAL.
      SELECT *
      FROM zatr_user_m
      INTO TABLE gt_userm
      FOR ALL ENTRIES IN gt_user
      WHERE user_id = gt_user-user_id.

      DELETE gt_userm WHERE status EQ 'X'.
    ENDIF.

  ENDIF.
  PERFORM process_data.
ENDFORM.                    "get_data


*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_data.

  gv_date = sy-datum + 30 .
  gv_date1 = gv_date + 90.
  LOOP AT gt_mchb INTO gs_mchb.
    READ TABLE gt_mara INTO gs_mara WITH KEY matnr = gs_mchb-matnr. " mtart = 'ZFGM'.
    IF sy-subrc = 0.
      tab_batch_stock-matnr = gs_mara-matnr.
      tab_batch_stock-meins = gs_mara-meins.

      READ TABLE gt_makt INTO gs_makt WITH KEY matnr = gs_mara-matnr.
      IF sy-subrc = 0.
        tab_batch_stock-maktx = gs_makt-maktx.
      ENDIF.

      READ TABLE gt_mch1 INTO gs_mch1 WITH KEY matnr = gs_mchb-matnr charg = gs_mchb-charg.
      IF sy-subrc = 0.
        tab_batch_stock-vfdat = gs_mch1-vfdat.
        tab_batch_stock-hsdat = gs_mch1-hsdat.
        IF gs_mchb-laeda IS INITIAL.
          tab_batch_stock-laeda = gs_mch1-laeda.
        ELSE.
          tab_batch_stock-laeda = gs_mchb-laeda.
        ENDIF.
      ENDIF.
      IF gs_mchb-laeda IS INITIAL AND gs_mch1-laeda IS INITIAL.
        tab_batch_stock-laeda = gs_mchb-ersda.
      ENDIF.
      tab_batch_stock-charg = gs_mchb-charg.
      tab_batch_stock-clabs = gs_mchb-clabs.
      tab_batch_stock-werks = gs_mchb-werks.

      SELECT SINGLE name1
      FROM t001w
      INTO tab_batch_stock-name1
      WHERE werks = gs_mchb-werks.


      SELECT SINGLE wgbez INTO tab_batch_stock-wgbez FROM t023t
      WHERE spras = 'EN'
      AND   matkl = gs_mara-matkl.

      SELECT SINGLE ewbez INTO tab_batch_stock-ewbez FROM twewt
      WHERE spras = 'EN'
      AND   extwg = gs_mara-extwg.


      PERFORM rem_zeros :
      USING tab_batch_stock-matnr CHANGING tab_batch_stock-matnr,
      USING tab_batch_stock-charg CHANGING tab_batch_stock-charg.

*      IF tab_batch_stock-vfdat NE 00000000 OR tab_batch_stock-vfdat NE ' '.
*if not tab_batch_stock-vfdat is INITIAL.
*        CALL FUNCTION 'HR_SGPBS_YRS_MTHS_DAYS'
*          EXPORTING
*            beg_da              = gv_date
*            end_da              = tab_batch_stock-vfdat
*         IMPORTING
*           no_day              = gv_day
**   NO_MONTH            =
**   NO_YEAR             =
**   NO_CAL_DAY          =
*         EXCEPTIONS
*           dateint_error       = 1
*           OTHERS              = 2
*                  .
*        IF sy-subrc <> 0.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*      ENDIF.
      CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
        EXPORTING
          i_datum_bis                   = tab_batch_stock-vfdat
          i_datum_von                   = sy-datum
*   I_KZ_EXCL_VON                 = '0'
*   I_KZ_INCL_BIS                 = '0'
*   I_KZ_ULT_BIS                  = ' '
*   I_KZ_ULT_VON                  = ' '
*   I_STGMETH                     = '0'
*   I_SZBMETH                     = '1'
       IMPORTING
         e_tage                        = gv_exp_day
       EXCEPTIONS
         days_method_not_defined       = 1
         OTHERS                        = 2
                .
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      tab_batch_stock-zday = gv_exp_day.

      IF ( ( NOT tab_batch_stock-vfdat IS INITIAL ) AND ( tab_batch_stock-vfdat GE gv_date
      AND  tab_batch_stock-vfdat LE gv_date1 ) ).
        APPEND tab_batch_stock.
      ENDIF.
    ENDIF.
    CLEAR : gs_mard , gs_mara , gs_makt , tab_batch_stock , gs_mchb , gs_mch1,
    gv_day,gv_exp_day.
  ENDLOOP.

  SORT tab_batch_stock BY werks.
ENDFORM.                    "process_data
************************************ logic ***************************
*&---------------------------------------------------------------------*
*&      Form  rem_zeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->A1         text
*      -->A2         text
*----------------------------------------------------------------------*
FORM rem_zeros USING a1 TYPE any
CHANGING a2 TYPE any.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = a1
    IMPORTING
      output = a2.
ENDFORM.                    "rem_zeros
*&---------------------------------------------------------------------*
*&      Form  add_zeros
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->A1         text
*      -->A2         text
*----------------------------------------------------------------------*
FORM add_zeros USING a1 TYPE any
CHANGING a2 TYPE any.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = a1
    IMPORTING
      output = a2.
ENDFORM.                    "add_zeros
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*

FORM build_eventtab USING p_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = p_events.

  READ TABLE p_events WITH KEY name = slis_ev_top_of_page
  INTO ls_event.
  IF sy-subrc = 0.
    MOVE formname_top_of_page TO ls_event-form.
    APPEND ls_event TO p_events.
  ENDIF.

ENDFORM.                               " BUILD_EVENTTAB

*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LAYOUT   text
*----------------------------------------------------------------------*
FORM build_layout  USING    p_layout TYPE slis_layout_alv.
  p_layout-f2code         = f2code.
  p_layout-zebra        = 'X'.
  p_layout-detail_popup = 'X'.
  "added by nitin.
*   p_layout-group_change_edit = 'X'.
  "end by nitin.

ENDFORM.                    "build_layout
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM build_fieldcatalog.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_bypassing_buffer     = 'X'
    i_callback_program     = sy-repid
***                i_callback_top_of_page = 'TOP-OF-PAGE'
    is_layout              = layout
    it_fieldcat            = fieldcatalog[]
    i_save                 = 'A'
    is_variant             = gx_variant
***          it_events              = gt_events
  TABLES
    t_outtab               = tab_batch_stock[].
ENDFORM.                    "display_data
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcatalog.
  DATA : pos TYPE i.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'WGBEZ'.
  fieldcatalog-seltext_m   = 'Material Group'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'EWBEZ'.
  fieldcatalog-seltext_m   = 'Ext Material Group'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'WERKS'.
  fieldcatalog-seltext_m   = 'Plant Code'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'NAME1'.
  fieldcatalog-seltext_m   = 'Plant Name'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'MATNR'.
  fieldcatalog-seltext_m   = 'Material'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'MAKTX'.
  fieldcatalog-seltext_m   = 'Material Desc'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'CHARG'.
  fieldcatalog-seltext_m   = 'Batch'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'CLABS'.
  fieldcatalog-seltext_m   = 'Stock'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'MEINS'.
  fieldcatalog-seltext_m   = 'UOM'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'VFDAT'.
  fieldcatalog-seltext_m   = 'Expiry Date'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'LAEDA'.
  fieldcatalog-seltext_m   = 'Last Movement Date'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'HSDAT'.
  fieldcatalog-seltext_m   = 'Manufacturing Date'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.

  pos = pos + 1.
  fieldcatalog-fieldname   = 'ZDAY'.
  fieldcatalog-seltext_m   = 'Expiry Days'.
  fieldcatalog-col_pos     = pos.
  "fieldcatalog-hotspot     = 'X'.
  APPEND fieldcatalog TO fieldcatalog.
  CLEAR  fieldcatalog.
ENDFORM.                    "build_fieldcatalog

*&---------------------------------------------------------------------*
*&      Form  send_mail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM send_mail.
  DATA : str1  TYPE string,
        name1 TYPE t001w-name1.
  DATA : line_number TYPE i .

  LOOP AT gt_mchb1 INTO gs_mchb1.
    CLEAR : it_attach , it_attach[],email,p_empcode,ename,str,t_receivers[].
    CONCATENATE
    'Material Group'
    'External Material Group'
    'Plant'
    'Name'
    'Material Number'
    'Material Description'
    'Batch Number'
***    'Valuated Unrestricted-Use Stock'
    'Stock'
    'Unit Of Measure'
    'Expiry Date'
    'Last Movement Date'
    'Manufacture Date'
    'Expiry Days'

    INTO it_attach SEPARATED BY con_tab.
    CONCATENATE con_cret it_attach  INTO it_attach.
    APPEND  it_attach.
    LOOP AT tab_batch_stock INTO struc_batch_stock WHERE werks = gs_mchb1-werks.
      gs_string-clabs = struc_batch_stock-clabs.
      CONCATENATE :
      struc_batch_stock-vfdat+6(2) struc_batch_stock-vfdat+4(2) struc_batch_stock-vfdat+0(4) INTO gs_string-date1 SEPARATED BY '.',
      struc_batch_stock-laeda+6(2) struc_batch_stock-laeda+4(2) struc_batch_stock-laeda+0(4) INTO gs_string-date2 SEPARATED BY '.',
      struc_batch_stock-hsdat+6(2) struc_batch_stock-hsdat+4(2) struc_batch_stock-hsdat+0(4) INTO gs_string-date3 SEPARATED BY '.'.
      CONCATENATE :
      struc_batch_stock-wgbez
      struc_batch_stock-ewbez
      struc_batch_stock-werks
      struc_batch_stock-name1
      struc_batch_stock-matnr
      struc_batch_stock-maktx
      struc_batch_stock-charg
      gs_string-clabs
      struc_batch_stock-meins
      gs_string-date1
      gs_string-date2
      gs_string-date3
      struc_batch_stock-zday

      INTO it_attach SEPARATED BY con_tab.

      CONCATENATE con_cret it_attach  INTO it_attach.
      APPEND  it_attach.
      CLEAR : struc_batch_stock , gs_string.
    ENDLOOP.
********************** Message ******************

************** user mail ID's **************************
    break ibm_ams.
    LOOP AT gt_user INTO gs_user WHERE vkbur = gs_mchb1-werks.
      CLEAR : it_message , it_message[],str.
      REFRESH it_message.
      READ TABLE gt_userm INTO gs_userm WITH KEY user_id = gs_user-user_id
                                                 spart   = '10'.
      IF sy-subrc = 0.
        CONCATENATE 'Dear' gs_userm-user_name INTO str SEPARATED BY space.
***        str = 'Dear All'.
        it_message = str.
        APPEND it_message.
        it_message = ' '.
        APPEND it_message.
        it_message = ' '.
        APPEND it_message.
        CONCATENATE : sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO date SEPARATED BY '.',
        sy-uzeit(2)   sy-uzeit+2(2) sy-uzeit+4(2) INTO time SEPARATED BY '.'.
        IF sy-uzeit(2) < '12'.
          CONCATENATE time 'AM' INTO time SEPARATED BY space.
        ELSE.
          CONCATENATE time 'PM' INTO time SEPARATED BY space.
        ENDIF.

        SELECT SINGLE name1
        FROM t001w
        INTO name1
        WHERE werks = gs_mchb1-werks.

        CONCATENATE 'Please Find the attached XLS file for Near expiry Stocks of Depot' gs_mchb1-werks '-' name1 'as on .' date time INTO str SEPARATED BY space.
        it_message = str.
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
********************** Message ******************

****************** send Mail ********************
        CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO date SEPARATED BY '.'.
        CONCATENATE sy-uzeit(2) sy-uzeit+2(2) sy-uzeit+4(2) INTO time SEPARATED BY '.'.
        IF sy-uzeit(2) < '12'.
          CONCATENATE time 'AM' INTO time SEPARATED BY space.
        ELSE.
          CONCATENATE time 'PM' INTO time SEPARATED BY space.
        ENDIF.

*        CONCATENATE 'Stock of Depot' gs_mchb1-werks 'as on ' date time INTO str1 SEPARATED BY space.
        str1 = 'stocks expire in next 90 days'.
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
***  w_doc_data-doc_size = ( w_cnt - 1 ) * 255 + STRLEN( it_attach ).

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

****************** user mail ID's **************************
*******    break ibm_ams.
****    LOOP AT gt_user INTO gs_user WHERE vkbur = gs_mchb1-werks.
***      READ TABLE gt_userm INTO gs_userm WITH KEY user_id = gs_user-user_id.
***      IF sy-subrc = 0.
        CLEAR : t_receivers." ,gs_userm-email_id.
***  t_receivers-receiver = 'grp-agro-sbu@modi.com'.
*        gs_userm-email_id = 'sachin.khedkar17@gmail.com'.
        TRANSLATE gs_userm-email_id TO LOWER CASE.
        t_receivers-receiver = gs_userm-email_id.
        t_receivers-rec_type = 'U'.
        t_receivers-com_type = 'INT'.
        t_receivers-notif_del = 'X'.
        t_receivers-notif_ndel = 'X'.
        APPEND t_receivers.
        CLEAR : gs_userm.
      ENDIF.
      CLEAR : filename,t_object_header[],t_object_header.
      PERFORM rem_zeros USING p_empcode CHANGING p_empcode.
      CONCATENATE str1 '.XLS' INTO filename
      SEPARATED BY space.
      t_object_header = filename.
      APPEND t_object_header.

      DESCRIBE TABLE t_attachment LINES line_number.

* Sending the document
      IF NOT t_receivers[] IS INITIAL AND line_number GT 1.
        CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
        EXPORTING
          document_data              = w_doc_data
          put_in_outbox              = 'X'
          sender_address             = ld_sender_address         " sender's address
          sender_address_type        = ld_sender_address_type
          commit_work                = 'X'
        TABLES
          packing_list               = t_packing_list
          object_header              = t_object_header
          contents_bin               = t_attachment
          contents_txt               = it_message
***      contents_hex               = objbin
          receivers                  = t_receivers
        EXCEPTIONS
          too_many_receivers         = 1
          document_not_sent          = 2
          document_type_not_exist    = 3
          operation_no_authorization = 4
          parameter_error            = 5
          x_error                    = 6
          enqueue_error              = 7
          OTHERS                     = 8.
        ld_error = sy-subrc.
      ENDIF.
      CLEAR : gs_user , t_receivers[].
    ENDLOOP.
************** user mail ID's **************************
***    CLEAR : filename,t_object_header[],t_object_header.
***    PERFORM rem_zeros USING p_empcode CHANGING p_empcode.
***    CONCATENATE str1 '.XLS' INTO filename
***      SEPARATED BY space.
***    t_object_header = filename.
***    APPEND t_object_header.
***
**** Sending the document
***    IF t_receivers[] IS NOT INITIAL.
***      CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
***        EXPORTING
***          document_data              = w_doc_data
***          put_in_outbox              = 'X'
***          sender_address             = ld_sender_address         " sender's address
***          sender_address_type        = ld_sender_address_type
***          commit_work                = 'X'
***        TABLES
***          packing_list               = t_packing_list
***          object_header              = t_object_header
***          contents_bin               = t_attachment
***          contents_txt               = it_message
******      contents_hex               = objbin
***          receivers                  = t_receivers
***        EXCEPTIONS
***          too_many_receivers         = 1
***          document_not_sent          = 2
***          document_type_not_exist    = 3
***          operation_no_authorization = 4
***          parameter_error            = 5
***          x_error                    = 6
***          enqueue_error              = 7
***          OTHERS                     = 8.
***      ld_error = sy-subrc.
***    ENDIF.
* Populate zreceiver return code

***    LOOP AT t_receivers.
***      ld_receiver = t_receivers-retrn_code.
***    ENDLOOP.

****************** send Mail ********************
  ENDLOOP.
ENDFORM.                    "send_mail
