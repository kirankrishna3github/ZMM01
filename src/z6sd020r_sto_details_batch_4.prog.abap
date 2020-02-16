*&--------------------------------------------------------------------------------------*
*& Report  Z6PP007C_TG_STK_UPL
*&
*&--------------------------------------------------------------------------------------*
*&
*&
*&--------------------------------------------------------------------------------------*

report  z6sd020r_sto_details_batch_3.
*---------------------------------------------------------------------------------------------*
* OBJECT DESCRIPTION: Stock Transfer Order Details                                            *
* OBJECT TYPE       : Report            FUNC. CONSULTANT  : Karnesh                           *
*          DEVELOPER: Ramakrishna                                                             *
*      CREATION DATE: 24.11.2010                                                              *
*        DEV REQUEST: IRDK903365                                                              *
*             TCODE : ZSTKTRF                                                                 *
* REVISION HISTORY----------------------------------------------------------------------------*
*                                                                                             *
*        CHANGED BY: Mrs.Punam Shinde                                                         *
*        CHANGE ON: 26.07.2011                                                                *
*        Reason for Change: 1.Remobe short close / Deleted Documents from output              *
*                           2.Remove Incomplete PO no from Output                             *
*                           3.Selection screen Material selection wasn't working, correct ir  *
*                           4.On double clicking on PO no call transaction ME23n.             *
* REVISION HISTORY----------------------------------------------------------------------------*
*                                                                                             *
*        CHANGED BY: Mrs.Punam Shinde                                                         *
*        CHANGE ON: 04.08.2011                                                                *
*        Reason for Change: 1.Add new columns to report Excise Amount , GRN no's.             *
*                                                                                             *
*                                                                                             *
*REVISION HISTORY-----------------------------------------------------------------------------*
*                                                                                             *
*        CHANGED BY: Mrs.Punam Shinde                                                         *
*        CHANGE ON: 06.09.2011                                                                *
*        Reason for Change: 1.Goods recept qty correction.                                    *
*                                                                                             *
*REVISION HISTORY-----------------------------------------------------------------------------*
*                                                                                             *
*        CHANGED BY: Mr.Pradeep Kodinagula                                                    *
*        CHANGE ON: 15.10.2014                                                                *
*        Reason for Change: 1. Added new field 'GRR Batch'  .                                 *
*                           2. Added new field 'Date of Manufacture'                          *
*                           3. Added new field 'Discount on Base(STRD)'                       *
*                           4. Added new fiels 'Base Total'.                                  *
*                           5. Corrected 'Goods Receipt QTY'.                                 *
*                           6. Corrected 'Goods Receipt Date'                                 *
*                           7. Added new field 'Excise Invoice No'                            *
*                           8. Added new field 'Excise Document date'.                        *
*---------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Chirag Shah
*   CHANGE ON: 04/11/2015
*   REASON FOR CHANGE: Add Authorization & Layout
*   REQUEST #: IRDK921489
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 31/07/2016
*   REASON FOR CHANGE: Stock Transfer logic modification for different Plants & batches
*   REQUEST #: IRDK924699
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 01/08/2016
*   REASON FOR CHANGE: Stock Transfer logic modification for different Plants & batches
*   REQUEST #: IRDK924767
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 05/08/2016
*   REASON FOR CHANGE: No. of days to transfer
*   REQUEST #: IRDK924917
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 11/08/2016
*   REASON FOR CHANGE: Summation logic for Despatched & Received Qty
*   REQUEST #: IRDK925022
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 06/09/2016
*   REASON FOR CHANGE: Need the Stk in Transit from MB5T
*   REQUEST #: IRDK925502
* --------------------------------------------------------------------------------------------*
* REVISION HISTORY----------------------------------------------------------------------------*
*
*   CHANGED BY: Naren Karra
*   CHANGE ON: 07/09/2016
*   REASON FOR CHANGE: Addition in logic for Process_id = '1'
*   REQUEST #: IRDK925530
* --------------------------------------------------------------------------------------------*
*----------------------------------------------------------------------*
*     TABLES6
*----------------------------------------------------------------------*
tables : ekko,ekpo,lips, mara , likp.
type-pools : slis,truxs.

*----------------------------------------------------------------------*
*     SELECTION SCREEN
*----------------------------------------------------------------------*
selection-screen begin of block s1 with frame title text-003 .
parameters: p1   radiobutton group g1 default 'X' ,
            p2   radiobutton group g1 .
selection-screen end   of block s1 .
selection-screen begin of block s2 with frame title text-002 .
parameters    : p_ekorg type ekko-ekorg default '1000' obligatory.
select-options: s_bukrs for ekko-bukrs,
                s_lifnr for ekko-lifnr  ,
                s_ekgrp for ekko-ekgrp,
                s_matnr for mara-matnr,
                s_reswk for ekko-reswk obligatory,
                s_werks for ekpo-werks obligatory,
                s_bedat for ekko-bedat,
                s_dldat for likp-lfdat,
                s_lgort for lips-lgort,
                s_ebeln for ekko-ebeln.
parameters : p_var type disvariant-variant modif id pk. " Added by CS on 04.11.2015 for Layout
selection-screen end of block s2 .

selection-screen : begin of block textnote  with frame title text-i01.
selection-screen begin of line.
selection-screen comment 1(79) text-i02 .
selection-screen end of line.
selection-screen : end of block textnote.


data: it_sort type  slis_t_sortinfo_alv." WITH HEADER LINE.
data: wa_sort like line of it_sort.
data: ex_matnr type mara-matnr,
      ex_reswk type ekko-reswk.
*----------------------------------------------------------------------*
*     INTERNAL TABLES
*----------------------------------------------------------------------*
types: begin of ty_detail,
         ebeln type ebeln,
         ebelp type ebelp,
         matnr type matnr,
         quant type bapimepoitem-quantity,
       end of ty_detail,

       begin of type_asci,
           line(200),
       end of type_asci.
data: wa_detail type ty_detail,
      it_detail type table of ty_detail.

types: begin of ty_zmb5t,
        matnr type mara-matnr,
        reswk type ekko-reswk,
        werks type ekpo-werks,
       end of ty_zmb5t.
data: wa_zmb5t type ty_zmb5t, it_zmb5t type table of ty_zmb5t.
types: begin of type_falv1,
        ebeln type ebeln,
        ebelp type ebelp,
        matnr type matnr,
        menge type menge_d,
        meins type meins,
       end of type_falv1.

data: ls_falv1 type type_falv1,
      lt_falv1 type standard table of type_falv1.

data :  v_message type lvc_title.
data : it_ekko type ekko_tty.
data : wa_ekko type ekko.
data : it_itab type zztt_sto_details1.
data : wa_itab type zzst_sto_details1.
data : wa_ntab type zzst_sto_details1.

data : it_it type zztt_sto_details1.
data : wa_it type zzst_sto_details1.

data : lv_refdoc type mkpf-xblnr.
data : lv_tdname type thead-tdname,
       it_lines like tline occurs 0 with header line.
data : it_mard type  mard_tt.
data : wa_mard type mard.
data : v_tabix type sy-tabix.
data : v_tabix1 type sy-tabix.
data :
       v_total   type mseg-menge,
       v_total_d type mseg-menge,
       v_total_r type mseg-menge,
       v_total_diff type mseg-menge,
       fl_flag,
       flg_mb5t,
       flg_mb5t2,
       flg_process_1,
       flg_tot_diff.

data: lv_goods_r type bapiekbes-deliv_qty,
      lv_dl_qty_trf type bapiekbes-dl_qty_trsp .

data : lv_stock type labst,
       lv_exch_rate type bapi1093_0,
       lv_kzwi2 type kzwi2.
*       lv_matnr TYPE mara-matnr.

data : lv_ebeln type bapimepoheader-po_number,
       lv_header type bapimepoheader,
       it_poitem like bapimepoitem occurs 0 with header line,
       it_pocond like bapimepocond occurs 0 with header line,
       it_poconfirmation like bapiekes occurs 0  with header line,
       it_pohistory_totals like  bapiekbes occurs 0 with header line,
       it_pohistory like bapiekbe occurs 0 with header line,
       tt_pohistory like bapiekbe occurs 0 with header line.

*{   REPLACE        SBXK900019                                        1
*\data: cond_value type konv-kbetr.
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************
data: cond_value type PRCD_ELEMENTS-kbetr.
*}   REPLACE

data: i_event type slis_t_event.

data: begin of wa_konv,
*{   REPLACE        SBXK900019                                        2
*\        kwert type konv-kwert,
*\        knumv type konv-knumv,
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************
        kwert type PRCD_ELEMENTS-kwert,
        knumv type PRCD_ELEMENTS-knumv,
*}   REPLACE
      end of wa_konv,
      it_konv like table of wa_konv.

***** Start Code: Added by CS on 04.11.2015 for layout. *****
data: s_repid   like sy-repid,
      g_save(1) type c,
      g_exit(1) type c,                         " ALV VARIANT
      gx_variant type disvariant.
***** End Code: Added by CS on 04.11.2015 for layout. *****
***** Start Code: Added by CS on 04.11.2015 for Authorization. *****
data: lv_ekorg_auth_flg type c value '',  " Auth. Flag for Purchase Org.
      lv_bukrs_auth_flg type c value '',  " Auth. Flag for Company Code
      lv_ekgrp_auth_flg type c value '',  " Auth. Flag for Purchasing Group
      lv_reswk_auth_flg type c value '',  " Auth. Flag for Supplying Plant, Added on 04.11.2015
      lv_werks_auth_flg type c value ''  " Auth. Flag for Receiving Plant
.
***** End Code: Added by CS on 04.11.2015 for Authorization. *****
***** Start Code: Added by CS on 04.11.2015 for layout. *****
at selection-screen on value-request for p_var.
  perform variant_f4_selection.                    " F4 HELP FOR VARIANT ON SELECTION SCREEN

at selection-screen.
  s_repid = sy-repid.
  perform check_variant_existance.

initialization.
  s_repid = sy-repid.
  perform variant_init.
***** End Code: Added by CS on 04.11.2015 for layout. *****

*----------------------------------------------------------------------*
*     START OF SELECTION
*----------------------------------------------------------------------*
start-of-selection .
  perform check_auth_obj.  " Added by CS on 04.11.2015 for Authorization.
  if p2 = 'X'.
    perform disp_zmb5t.
  endif.
*  IF p_fore EQ 'X' .
*    v_mode = 'A' .
*  ELSE .
*    v_mode = 'E' .
*  ENDIF .
  perform  f_get_data  .
  sort it_itab by ebeln ebelp.
***** Start Code: Added by CS on 04.11.2015 for Authorization. *****
*  IF NOT it_itab IS INITIAL.
*    PERFORM  disp_records.
*  ENDIF.
  if not it_itab is initial.
    if lv_reswk_auth_flg = 'X' or lv_werks_auth_flg = 'X' or lv_bukrs_auth_flg = 'X' or lv_ekgrp_auth_flg = 'X'.
      message 'Missing Authorization for Company Code/Pur Group/Receiving Plant/Supplying Plant.' type 'S' display like 'W'.
*      LEAVE LIST-PROCESSING.
    endif.
    if p1 = 'X'.
      perform disp_records.
*    ELSEIF p2 = 'X'.
*      PERFORM disp_zmb5t.
    endif.
  else.
    if lv_reswk_auth_flg = 'X' or lv_werks_auth_flg = 'X' or lv_bukrs_auth_flg = 'X' or lv_ekgrp_auth_flg = 'X'.
      message 'No data found/ Missing Authorization for Company Code/Pur Group/Receiving Plant/Supplying Plant.' type 'I'.
      leave list-processing.
    endif.
  endif.
***** End Code: Added by CS on 04.11.2015 for Authorization. *****
*&---------------------------------------------------------------------*
*&      Form  I_UPLOAD
*&---------------------------------------------------------------------*
form f_get_data.

  if not s_bedat[] is initial.

    select ebeln bsart memory knumv from ekko  client specified
      into corresponding fields of table it_ekko
                                where mandt = sy-mandt
                                  and bstyp = 'F'
                                  and bedat in s_bedat
                                  and bukrs in s_bukrs
                                  and lifnr in s_lifnr
                                  and ebeln in s_ebeln
                                  and reswk in s_reswk
                                  and ekorg eq p_ekorg.

  elseif not s_lifnr[] is initial.

    select ebeln  bsart memory knumv from ekko client specified
                        into corresponding fields of table it_ekko
                             where mandt = sy-mandt
                               and lifnr in s_lifnr
                               and ekorg eq p_ekorg
                               and ekgrp in s_ekgrp
                               and bedat in s_bedat
                               and bukrs in s_bukrs
                               and ebeln in s_ebeln
                               and reswk in s_reswk.

  elseif not s_ebeln[] is initial.

    select ebeln bsart memory knumv from ekko into corresponding fields of table it_ekko
                         where ebeln in s_ebeln
                           and bukrs in s_bukrs
                           and lifnr in s_lifnr
                           and ekgrp in s_ekgrp
                           and bedat in s_bedat
                           and ekorg eq p_ekorg
                           and reswk in s_reswk.


  elseif not s_reswk[] is initial.

    select ebeln  bsart memory knumv from ekko client specified into corresponding fields of table it_ekko
                                where mandt = sy-mandt
                                  and reswk in s_reswk
                                  and ekorg eq p_ekorg
                                  and bstyp eq 'F'
                                  and lifnr in s_lifnr
                                  and ekgrp in s_ekgrp
                                  and bedat in s_bedat
                                  and ebeln in s_ebeln.
  else.
    select ebeln bsart memory knumv from ekko into corresponding fields of table it_ekko
                         where ebeln in s_ebeln
                           and bukrs in s_bukrs
                           and lifnr in s_lifnr
                           and ekgrp in s_ekgrp
                           and bedat in s_bedat
                           and ekorg eq p_ekorg
                           and reswk in s_reswk.


  endif.
  delete it_ekko where bsart ne 'ZSTO' and memory = 'X'. " Purchase order not yet complete

  sort it_ekko by ebeln .

  loop at it_ekko into wa_ekko.

    lv_ebeln = wa_ekko-ebeln.
    refresh : it_poitem,it_pohistory_totals,it_poconfirmation,it_pohistory.
    clear : it_poitem,it_pohistory_totals,it_poconfirmation,it_pohistory.
    call function 'BAPI_PO_GETDETAIL1'
      exporting
        purchaseorder            = lv_ebeln
*          ACCOUNT_ASSIGNMENT       = ' '
*          ITEM_TEXT                = ' '
*          HEADER_TEXT              = ' '
*          DELIVERY_ADDRESS         = ' '
*          VERSION                  = ' '
*          SERVICES                 = ' '
*          SERIALNUMBERS            = ' '
*          INVOICEPLAN              = ' '
      importing
        poheader                 = lv_header
*          POEXPIMPHEADER           =
      tables
*          RETURN                   =
        poitem                   = it_poitem
*          POADDRDELIVERY           =
*          POSCHEDULE               =
*          POACCOUNT                =
*          POCONDHEADER             =
       pocond                   = it_pocond
*          POLIMITS                 =
*          POCONTRACTLIMITS         =
*          POSERVICES               =
*          POSRVACCESSVALUES        =
*          POTEXTHEADER             =
*          POTEXTITEM               =
*          POEXPIMPITEM             =
*          POCOMPONENTS             =
*          POSHIPPINGEXP            =
        pohistory                = it_pohistory
        pohistory_totals         = it_pohistory_totals
        poconfirmation           = it_poconfirmation
*          ALLVERSIONS              =
*          POPARTNER                =
*          EXTENSIONOUT             =
*          SERIALNUMBER             =
*          INVPLANHEADER            =
*          INVPLANITEM              =
*          POHISTORY_MA             =
              .

    loop at it_poitem where plant in s_werks
                        and material in s_matnr and delete_ind <> 'L'.
      move : lv_ebeln to wa_itab-ebeln,
             it_poitem-po_item to wa_itab-ebelp,
             it_poitem-material to wa_itab-matnr,
             it_poitem-short_text to wa_itab-maktx,
             it_poitem-po_unit to wa_itab-meins,              "it_poitem-quantity TO wa_itab-menge,
             it_poitem-plant to wa_itab-werks,
             it_poitem-matl_group to wa_itab-matl_group,
             lv_header-currency to wa_itab-waers,
             lv_header-suppl_plnt to wa_itab-reswk,
             lv_header-vendor to wa_itab-lifnr.

********************************************************************** ~ NK
      move: lv_ebeln to wa_detail-ebeln,
            it_poitem-po_item to wa_detail-ebelp,
            it_poitem-material to wa_detail-matnr.
      read table it_pohistory_totals with key po_item = wa_detail-ebelp.
      if sy-subrc eq 0.
        move it_pohistory_totals-deliv_qty to wa_detail-quant.
      endif.
      append wa_detail to it_detail.  " line 950
      clear wa_detail.
**********************************************************************
      select single name1 from t001w into wa_itab-sname
                              where werks = wa_itab-reswk.

      select single name1 from t001w into wa_itab-rname
                              where werks = wa_itab-werks.

      select single name1 from lfa1 into wa_itab-lname
                               where lifnr = wa_itab-lifnr.

      select single mtart from ekpo into wa_itab-mtart
                               where ebeln = lv_ebeln
                                and ebelp =  it_poitem-po_item
                                and matnr =  it_poitem-material.


      select single wgbez from t023t into wa_itab-wgbez
      where matkl = wa_itab-matl_group and spras = sy-langu.

      tt_pohistory[] = it_pohistory[].
      clear : wa_itab-menge, wa_itab-delqy ,wa_itab-grqty,wa_itab-ststk,wa_itab-borqy,wa_itab-dldat,wa_itab-prdat,wa_itab-grdat,
              wa_itab-mblnr,wa_itab-vbeln, wa_itab-dposn,wa_itab-lrnum,wa_itab-invno,wa_itab-fkdat,wa_itab-trans,wa_itab-ndays.
      loop at it_pohistory where po_item = wa_itab-ebelp
                             and process_id = '8'
                             and quantity ne 0.
        clear : wa_itab-vbeln,wa_itab-dposn,wa_itab-grdat,wa_itab-fkdat,wa_itab-prdat,wa_itab-dldat,wa_itab-mblnr
                ,wa_itab-delqy,wa_itab-grqty,wa_itab-ststk,wa_itab-invno,wa_itab-fkdat.

        wa_itab-vbeln = it_pohistory-mat_doc.
        wa_itab-menge = it_pohistory-quantity.

        select single wadat lfdat xabln bldat from likp into (wa_itab-prdat,wa_itab-dldat,wa_itab-lrnum, wa_itab-bldat)
                                  where vbeln = wa_itab-vbeln
                                  and lfdat in s_dldat.

        clear : lv_tdname,it_lines.
        refresh : it_lines.
        lv_tdname = wa_itab-vbeln.



        call function 'READ_TEXT'
          exporting
*           CLIENT                        = SY-MANDT
            id                            = 'ZTRA'
            language                      = sy-langu
            name                          = lv_tdname
            object                        = 'VBBK'
*           ARCHIVE_HANDLE                = 0
*           LOCAL_CAT                     = ' '
*         IMPORTING
*           HEADER                        =
          tables
            lines                         = it_lines
         exceptions
           id                            = 1
           language                      = 2
           name                          = 3
           not_found                     = 4
           object                        = 5
           reference_check               = 6
           wrong_access_to_archive       = 7
           others                        = 8
                  .
        if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        else.
          read table it_lines index 1.
          if sy-subrc eq 0.
            wa_itab-trans = it_lines-tdline.
          endif.
        endif.


        clear : it_lines.
        refresh : it_lines.

        call function 'READ_TEXT'
          exporting
*           CLIENT                        = SY-MANDT
            id                            = 'ZCNT'
            language                      = sy-langu
            name                          = lv_tdname
            object                        = 'VBBK'
*           ARCHIVE_HANDLE                = 0
*           LOCAL_CAT                     = ' '
*         IMPORTING
*           HEADER                        =
          tables
            lines                         = it_lines
         exceptions
           id                            = 1
           language                      = 2
           name                          = 3
           not_found                     = 4
           object                        = 5
           reference_check               = 6
           wrong_access_to_archive       = 7
           others                        = 8
                  .
        if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        else.
          read table it_lines index 1.
          if sy-subrc eq 0.
            wa_itab-lr_type = it_lines-tdline.
          endif.
        endif.

        select single lifnr
          from vbpa
          into wa_itab-fr_agt
          where vbeln = wa_itab-vbeln
          and parvw = 'TF'.

        select single name1
          from lfa1
          into wa_itab-forw_name
          where lifnr = wa_itab-fr_agt.



        concatenate it_pohistory-doc_year+2(2) it_pohistory-matdoc_itm into wa_itab-dposn.
        select single charg lgort from lips  into (wa_itab-batch,wa_itab-lgort)
                                where vbeln eq wa_itab-vbeln
                                  and posnr eq wa_itab-dposn.

*        if sy-subrc ne 0.
*          continue.
*        endif.

        select single vbeln from vbfa into wa_itab-invno where vbelv eq wa_itab-vbeln
                                                           and posnv eq wa_itab-dposn
                                                           and vbtyp_v eq 'J'
                                                           and vbtyp_n eq 'U'.

        if not wa_itab-invno is initial.
          select single fkdat from vbrk into wa_itab-fkdat
                                  where vbeln eq wa_itab-invno.

        endif.
        move wa_itab-vbeln to lv_refdoc.
        condense lv_refdoc no-gaps.
        loop at tt_pohistory where  material = wa_itab-matnr
                               and  plant    = wa_itab-reswk
                               and  batch    = wa_itab-batch
                               and  ref_doc_no = lv_refdoc
                               and  process_id = '6'
                               and pstng_date in s_dldat.

          if tt_pohistory-db_cr_ind eq 'H'.
            tt_pohistory-quantity = tt_pohistory-quantity * -1.
          endif.

          wa_itab-dldat = tt_pohistory-pstng_date.
          wa_itab-prdat = wa_itab-dldat + it_poitem-plan_del.

* Issue in the where conditions - modified 1 is commented
          select single lgort from mseg into wa_itab-lgort
                                where  mblnr eq tt_pohistory-mat_doc
                                  and  mjahr eq tt_pohistory-matdoc_itm
                                  and  zeile eq tt_pohistory-doc_year.

*          SELECT SINGLE lgort FROM mseg INTO wa_itab-lgort
*                                WHERE  mblnr EQ tt_pohistory-mat_doc
*                                  AND  mjahr EQ tt_pohistory-doc_year
*                                  AND  zeile EQ tt_pohistory-matdoc_itm.

          wa_itab-delqy = wa_itab-delqy + tt_pohistory-quantity.

        endloop.

        clear tt_pohistory.

        read table it_pocond with key itm_number = wa_itab-ebelp cond_type = 'ZSTR'.
        if sy-subrc = 0.
          wa_itab-zstr_rate = it_pocond-cond_value.
        else.
          clear : it_pocond-cond_value.
        endif.
        read table it_pocond with key itm_number = wa_itab-ebelp cond_type = 'P101'.
        if sy-subrc = 0.
          wa_itab-p101_rate = it_pocond-cond_value.
        else.
          clear : it_pocond-cond_value.
        endif.

        read table it_pocond with key itm_number = wa_itab-ebelp cond_type = 'FRC1'.
        if sy-subrc = 0.
          wa_itab-frc1_rate = it_pocond-cond_value.
        else.
          clear : it_pocond-cond_value.
        endif.

        read table it_pocond with key itm_number = wa_itab-ebelp cond_type = 'STRD'.       " Added By - Pradeep Kodinagula
        if sy-subrc = 0.
          wa_itab-kbetr = it_pocond-cond_value.
        else.
          wa_itab-kbetr = '0.00'.
          clear : it_pocond-cond_value.
        endif.                                                                              " End of discount on Base(STRD) Logic - Pradeep Kodinagula

        data a type i.                                                                      " Added Base total - Pradeep Kodinagula
        if wa_itab-kbetr is not initial.
          a = wa_itab-zstr_rate * wa_itab-kbetr / 100 .
          wa_itab-base_total = wa_itab-zstr_rate + a .
        endif.                                                                              " End of Base total Logic - Pradeep Kodinagula


        concatenate wa_itab-ebeln wa_itab-ebelp into lv_tdname.
        clear : it_lines.
        refresh : it_lines.

        call function 'READ_TEXT'
          exporting
*           CLIENT                        = SY-MANDT
            id                            = 'F01'
            language                      = sy-langu
            name                          = lv_tdname
            object                        = 'EKPO'
*           ARCHIVE_HANDLE                = 0
*           LOCAL_CAT                     = ' '
*         IMPORTING
*           HEADER                        =
          tables
            lines                         = it_lines
         exceptions
           id                            = 1
           language                      = 2
           name                          = 3
           not_found                     = 4
           object                        = 5
           reference_check               = 6
           wrong_access_to_archive       = 7
           others                        = 8
                  .
        if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        else.
          read table it_lines index 1.
          if sy-subrc eq 0.
            wa_itab-item_txt = it_lines-tdline.
          endif.
        endif.

        data: f(1).
        data: flg_mat_doc.
        clear f.
        if wa_itab-batch is not initial.
          read table  tt_pohistory with key batch =  wa_itab-batch process_id = '1'.
          if sy-subrc = 0.
            loop  at tt_pohistory where    material   = wa_itab-matnr
                                    and    plant      = wa_itab-werks
                                    and    batch      = wa_itab-batch
                                    and    ref_doc_no = lv_refdoc
                                    and    process_id = '1'.


              if tt_pohistory-db_cr_ind eq 'H'.
                tt_pohistory-quantity = tt_pohistory-quantity * -1.
              endif.

              wa_itab-charg = tt_pohistory-batch.                         " Added GRR Batch - Pradeep Kodinagula

              wa_itab-grqty = wa_itab-grqty + tt_pohistory-quantity.
              wa_itab-grdat = tt_pohistory-pstng_date.
              wa_itab-mblnr = tt_pohistory-mat_doc.

              select single hsdat from mch1 into wa_itab-hsdat            " Added Date of Manufacture - Pradeep Kodinagula
                               where matnr = wa_itab-matnr
                               and charg = wa_itab-charg.

              select single exnum exdat from j_1iexcdtl                   " Added Excise Number and Excise doc date - Pradeep Kodinagula
                     into (wa_itab-exnum, wa_itab-exdat)
                     where rdoc2 = wa_itab-invno
                     and ritem1 = wa_itab-dposn.

              f = 'X'.
            endloop.
          endif.
        endif.

********************************************************************** ~ by NarenKarra  on 31/07/2016 - start
* Since in the before case loop has failed due to Reference no.

        if sy-subrc ne 0.
          flg_process_1 = 'X'.
          sy-subrc = 0.   " i know sy-subrc is automatically set/reset  but still for the below LOOP
        else.
          clear flg_process_1.
        endif.
* This logic is for the Received Qty
        if flg_process_1 = 'X'.
          loop  at tt_pohistory where    material = wa_itab-matnr
                                and    plant    = wa_itab-werks
                                and    batch    = wa_itab-batch
*                                    AND    ref_doc_no = lv_refdoc
                                and    process_id = '1'.


            if tt_pohistory-db_cr_ind eq 'H'.
              tt_pohistory-quantity = tt_pohistory-quantity * -1.
            endif.


            wa_itab-charg = tt_pohistory-batch.                         " Added GRR Batch - Pradeep Kodinagula
            wa_itab-grqty = wa_itab-grqty + tt_pohistory-quantity.
            wa_itab-grdat = tt_pohistory-pstng_date.
            wa_itab-mblnr = tt_pohistory-mat_doc.

            select single hsdat from mch1 into wa_itab-hsdat            " Added Date of Manufacture - Pradeep Kodinagula
                             where matnr = wa_itab-matnr
                             and charg = wa_itab-charg.

            select single exnum exdat from j_1iexcdtl                   " Added Excise Number and Excise doc date - Pradeep Kodinagula
                   into (wa_itab-exnum, wa_itab-exdat)
                   where rdoc2 = wa_itab-invno
                   and ritem1 = wa_itab-dposn.

            f = 'X'.
          endloop.
********************************************************************** ~ nk 07.09.2016 - start
*       this code snippet is for 'if Good Receipt is not issued' i.e., no process_id = '1' is found
          if sy-subrc = 4.
            loop  at tt_pohistory where    material = wa_itab-matnr
                                  and    plant    = wa_itab-werks
                                  and    batch    = wa_itab-batch
*                                    AND    ref_doc_no = lv_refdoc
                                  and    process_id = '1'.


              if tt_pohistory-db_cr_ind eq 'H'.
                tt_pohistory-quantity = tt_pohistory-quantity * -1.
              endif.


              wa_itab-charg = tt_pohistory-batch.                         " Added GRR Batch - Pradeep Kodinagula
              wa_itab-grqty = wa_itab-grqty + tt_pohistory-quantity.
              wa_itab-grdat = tt_pohistory-pstng_date.
              wa_itab-mblnr = tt_pohistory-mat_doc.

              select single hsdat from mch1 into wa_itab-hsdat            " Added Date of Manufacture - Pradeep Kodinagula
                               where matnr = wa_itab-matnr
                               and charg = wa_itab-charg.

              select single exnum exdat from j_1iexcdtl                   " Added Excise Number and Excise doc date - Pradeep Kodinagula
                     into (wa_itab-exnum, wa_itab-exdat)
                     where rdoc2 = wa_itab-invno
                     and ritem1 = wa_itab-dposn.

              f = 'X'.
            endloop.
          endif.
********************************************************************** ~ nk 07.09.2016 - end


        endif.
********************************************************************** ~ by NarenKarra  on 31/07/2016 - end

        read table  tt_pohistory with key ref_doc_no = lv_refdoc process_id = '1'.
*        IF sy-subrc = 0.
**          IF f <> 'X'.
**            LOOP  AT tt_pohistory WHERE    material = wa_itab-matnr
**                                    AND    plant    = wa_itab-werks
***                                and    batch    = wa_itab-batch
**                                    AND    ref_doc_no = lv_refdoc
**                                    AND    process_id = '1'.
**
**              IF tt_pohistory-db_cr_ind EQ 'H'.
**                tt_pohistory-quantity = tt_pohistory-quantity * -1.
**              ENDIF.
***              wa_itab-charg = tt_pohistory-batch.
***              wa_itab-grqty = wa_itab-grqty + tt_pohistory-quantity.
***              wa_itab-grdat = tt_pohistory-pstng_date.
**              wa_itab-mblnr = tt_pohistory-mat_doc.
**            ENDLOOP.
*          ENDIF.
*        ENDIF.

        clear f.
*end code by punam

        if wa_itab-delqy le 0.
          wa_itab-delqy = wa_itab-delqy * -1.
        endif.
        if wa_itab-grqty le 0.
          wa_itab-grqty = wa_itab-grqty * -1.
        endif.
        if wa_itab-menge le 0.
          wa_itab-menge = wa_itab-menge * -1.
        endif.
        wa_itab-ststk = wa_itab-delqy - wa_itab-grqty .
********************************************************************** ~ nk added on 06.10.2016
* For clearing STSTK field if no stock in transit ( SIT ) present or adding a new line in final table with SIT
* qty. in negative so that the final qty. can be zero
*  Pass MATNR no. to this Sub-Routine  for Tcode:- ZMB5T
        perform disp_zmb5t_2." using wa_itab-matnr.

        import lt_falv1 to lt_falv1 from memory id 'ZMB5T_NK1'.

        sort lt_falv1 by ebeln ebelp matnr.
        read table lt_falv1 into ls_falv1 with key ebeln = wa_itab-ebeln ebelp = wa_itab-ebelp matnr = wa_itab-matnr binary search.
*  SY-SUBRC ne 0  - Then no Stock in Transit found clear WA_ITAB-STSTK for the current MATNR
        if sy-subrc ne 0.
          wa_itab-grqty = wa_itab-ststk + wa_itab-grqty.
          clear wa_itab-ststk.
        endif.
********************************************************************** ~ nk added on 06.10.2016
* The logic for 'No. of days in transit' should work theortical - but seems to me in this case it's not ...
* So let's keep it this way - [ if no stock is available in inTransit then obviously NDAYS = 0 ] !!
        if wa_itab-ststk is initial.
          clear wa_itab-ndays.
        else.
* Y is the GR date being used here(below code snippet- GRDAT) ?? Commenting it for now ~ Naren Karra - 05.08.2016
* Basic understanding is ' date on which depatched - the current date = No. of days in transit '
          wa_itab-ndays = sy-datum - wa_itab-dldat.

*          IF NOT wa_itab-grdat IS INITIAL.
*            wa_itab-ndays = wa_itab-grdat - wa_itab-dldat.
*          ELSE.
*            wa_itab-ndays = sy-datum - wa_itab-dldat.
*          ENDIF.
        endif.
        append wa_itab to it_itab.
        clear  : wa_itab-charg , wa_itab-hsdat.

      endloop.

      if sy-subrc ne 0.

        append wa_itab to it_itab.
        clear wa_itab.
      endif.

    endloop.



    clear cond_value.

    clear: it_konv , it_konv[].

    select kwert knumv
*{   REPLACE        SBXK900019                                        1
*\      from konv
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/05/2018 (MM/DD/YYYY)
* Changed By - 10106
* Purpose - KONV table no longer used hence replaced by PRCD_ELEMENTS
****************************************************************************************************
      from PRCD_ELEMENTS
*}   REPLACE
      into table it_konv
      where knumv = wa_ekko-knumv
      and kschl in ('ZEX1', 'ZEX2' , 'ZEX3').

    if it_konv is not initial.
      loop at it_konv into wa_konv where knumv = wa_ekko-knumv .
        cond_value = cond_value + wa_konv-kwert.
      endloop.
*      LOOP AT IT_ITAB INTO WA_ITAB WHERE EBELN = WA_EKKO-EBELN .
      read table it_itab into wa_itab with key ebeln = wa_ekko-ebeln .
      if sy-subrc = 0.
        wa_itab-cond_value = cond_value.
        modify table it_itab from wa_itab transporting cond_value.
      endif.
*      ENDLOOP.
      clear wa_itab.
    endif.


  endloop.



  if s_dldat is not initial.
    delete it_itab where dldat eq '00000000'.
  endif.

  if not s_lgort[] is initial.
    loop at it_itab into wa_itab.
      if not  wa_itab-lgort in s_lgort.
        delete it_itab index sy-tabix.
      endif.
    endloop.
  endif.

*  LOOP AT it_itab into wa_itab.
*
*    MOVE wa_itab-ebeln to wa_it-ebeln.
*    MOVE wa_itab-ebelp to wa_it-ebelp.
*    MOVE wa_itab-grqty to wa_it-grqty.
*
*    COLLECT  wa_it INTO it_it.
*
*  ENDLOOP.


*  CLEAR wa_it.
*
*  LOOP AT it_it INTO wa_it.
*    IF wa_it-GRQTY = 0.
*       SELECT SINGLE WEMNG FROM EKET INTO  WA_IT-GRQTY
*       WHERE EBELN = WA_IT-EBELN
*       AND EBELP = WA_IT-EBELP.
*    ENDIF.
*  ENDLOOP.
  clear: flg_mb5t,flg_mb5t2.
  sort it_itab by ebeln ebelp.
  loop at it_itab into wa_itab.


*    IF WA_ITAB-GRQTY = 0.
*       SELECT SINGLE WEMNG FROM EKET INTO  WA_ITAB-GRQTY
*       WHERE EBELN = WA_ITAB-EBELN
*       AND EBELP = WA_ITAB-EBELP.
*    ENDIF.

    if wa_itab-delqy le 0.
      wa_itab-delqy = wa_itab-delqy * -1.
    endif.
    if wa_itab-grqty le 0.
      wa_itab-grqty = wa_itab-grqty * -1.
    endif.
    if wa_itab-menge le 0.
      wa_itab-menge = wa_itab-menge * -1.
    endif.

*    AT END OF ebeln.
    at end of ebelp.                  "  ~ Naren Karra - 11.08.2016 " PO - 1500000024 in IRP
      sum.
      v_total_d = wa_itab-delqy.
      v_total_r = wa_itab-grqty.
      v_total_diff = v_total_d - v_total_r.
      fl_flag = 1.
*    ENDAT.


      read table it_pohistory_totals with key po_item = wa_itab-ebelp.
      if sy-subrc eq 0.
        lv_goods_r = it_pohistory_totals-deliv_qty.
        lv_dl_qty_trf = it_pohistory_totals-dl_qty_trsp.
      endif.

      if v_total_r = lv_goods_r.
*   This flag is if - ' No Stock is in transit '  !!
***    But this issue will persist if we are going with multiple MATNR stock and verifying them    ***
        flg_mb5t = 'X'.
      else.
        flg_mb5t2 = 'X'.
      endif.
    endat.
*    ENDAT.
* If stock in Transit is -ve then Days in transit is also req. in -ve since the ' user wants to check the those STO's which has 0 days
    if wa_itab-ststk le 0.                      " added by NK 22.08.2016
      wa_itab-ndays = wa_itab-ndays * -1.
    endif.

    modify it_itab from wa_itab transporting delqy grqty menge ndays.
    clear wa_itab.
  endloop.

**********************************************************************  ~ Naren Karra - 11.08.2016
  sort it_itab by ebeln ebelp.
  loop at it_itab into wa_itab.

*************
*    AT END OF ebeln.
    at end of ebelp.                  "  ~ Naren Karra - 11.08.2016 " PO - 1500000024 in IRP
      sum.
      v_total_d = wa_itab-delqy.
      v_total_r = wa_itab-grqty.
      v_total_diff = v_total_d - v_total_r.

      read table it_pohistory_totals with key po_item = wa_itab-ebelp.
      if sy-subrc eq 0.
        lv_goods_r = it_pohistory_totals-deliv_qty.
        lv_dl_qty_trf = it_pohistory_totals-dl_qty_trsp.
      endif.

      if v_total_r = lv_goods_r.
*   This flag is if - ' No Stock is in transit '  !!
***    But this issue will persist if we are going with multiple MATNR stock and verifying them    ***
        flg_mb5t = 'X'.
      else.
        flg_mb5t2 = 'X'.
      endif.

      if v_total_d = v_total_r.         " PO - 1500000024 in IRP
* This code snippet is written in case if the ' Qty from Source plant is going into some other batch @ destination plant
*    and in those cases the summation of Despatched Qty & Received Qty r same for that MATNR then logically Stock in transit & no. of days in transit
*    should be nil( zero )!!      -  which is actually NOT
        clear flg_tot_diff.
        if lv_dl_qty_trf = v_total_d.
          clear: wa_itab-ststk, wa_itab-ndays.
        endif.
      else.
        if lv_dl_qty_trf = v_total_d.
* this difference is due to the not availability of the same batch
          wa_itab-grqty = v_total_diff .
          flg_tot_diff = 'X'.
          " append wa_itab to it_itab.
        endif.
      endif.
    endat.
*    ENDAT.
*************

*  No Stock is in transit
    if flg_mb5t2 = 'X'.

    endif.

    modify it_itab from wa_itab transporting menge borqy ststk ndays.
    clear wa_itab.
  endloop.

  data flg_new.

  sort it_itab by ebeln ebelp matnr.
  loop at it_itab into wa_itab.
    at new matnr.
      sum.
      wa_itab-menge = wa_itab-menge.  " Order Qty
      wa_itab-borqy = wa_itab-borqy.  " Pending Order Qty
      modify it_itab from wa_itab transporting menge borqy.
*      CLEAR wa_itab.
      flg_new = 'X'.
    endat.
    if flg_new is initial. "NE 'X'.
      clear: wa_itab-menge, wa_itab-borqy.
      modify it_itab from wa_itab transporting menge borqy.
      clear wa_itab.
    endif.
    clear flg_new.
  endloop.

*  DATA: lt_po_itm_hist     TYPE STANDARD TABLE OF bapiekbe,
*        lt_po_itm_hist_tot TYPE STANDARD TABLE OF bapiekbes,
*        lt_return          TYPE STANDARD TABLE OF bapireturn,
*        ls_po_itm_hist     TYPE bapiekbe,
*        ls_po_itm_hist_tot TYPE bapiekbes.
*
*  SORT it_itab   BY ebeln ebelp.
*  LOOP AT it_itab INTO wa_itab.
*    CALL FUNCTION 'BAPI_PO_GETDETAIL'
*      EXPORTING
*        purchaseorder          = wa_itab-ebeln
*        items                  = 'X'
*        history                = 'X'
*        po_item_history        = lt_po_itm_hist
*        po_item_history_totals = lt_po_itm_hist_tot
*        return                 = lt_return.
*    READ TABLE lt_po_itm_hist INTO ls_po_itm_hist WITH KEY po_item = wa_itab-ebelp.
*    IF sy-subrc EQ 0.
*      IF ls_po_itm_hist-quantity IS NOT INITIAL.
*        IF ls_po_itm_hist-process_id = '1'.
*
*        ELSEIF ls_po_itm_hist-process_id = '6'.
*
*        ELSEIF ls_po_itm_hist-process_id = '8'.

*        ENDIF.
*        wa_itab-grqty = ls_po_itm_hist-quantity.
*        CLEAR: wa_itab-ststk, wa_itab-ndays.
*      ENDIF.
*    ENDIF.
*    MODIFY it_itab FROM wa_itab TRANSPORTING menge delqy grqty borqy ststk ndays.
*  ENDLOOP.

********************************************************************** ~ by NK  on 09.09.2016 - start
*  in a particular case of 1500038432 - the Reference no. is not failing but the batch no. is different for
*    different line items of SAME Material doc. no (GRR no)       Eg:- 5000212342 - 0001 - TMZL-9001
*                                                                      5000212342 - 0002 - TMZL-9000
*       So now the quantity in Received column of this 2nd line item was suppose to get knocked-off
*         with that of the Despatched column  ( of same line )
*   1500031742 ~ same issue
* On 21.09.2016
*          Even the above case/scenario is working fine for other STO's were, in GRR item
* level the batch is different or empty

**********************************************************************  " commented by NK - 21.09.2016
*  DATA: lv_index TYPE sy-tabix,
*        lv_mat_doc TYPE mkpf-mblnr.
*
*  LOOP AT it_itab INTO wa_itab.
*
*    READ TABLE it_pohistory_totals WITH KEY po_item = wa_itab-ebelp.
*    IF sy-subrc EQ 0.
*      lv_goods_r = it_pohistory_totals-deliv_qty.
*      lv_dl_qty_trf = it_pohistory_totals-dl_qty_trsp.
*    ENDIF.
*
*    lv_mat_doc = tt_pohistory-mat_doc.
*
*    READ TABLE tt_pohistory WITH KEY mat_doc = tt_pohistory-mat_doc.
*    IF sy-subrc EQ 0.
*      lv_index = sy-tabix.
*      LOOP AT tt_pohistory FROM lv_index." mat_doc = tt_pohistory-mat_doc.
*        IF lv_mat_doc EQ tt_pohistory-mat_doc.
*          wa_itab-grqty = wa_itab-grqty + tt_pohistory-quantity.
*        ELSE.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*  ENDLOOP.                                                             " commented by NK - 21.09.2016
********************************************************************** ~ by NK  on 09.09.2016 - end




* This is the trick for 1500000014
*   now consolidate all - entire 400 lines of ur code snippet into a max. of 25-35 lines ---> use 'GROUP BY' concept ! ! !
*** Commenting this for now - this is not serving the purpose with a range of STO's
*  IF flg_tot_diff = 'X'.
*    LOOP AT it_itab INTO wa_itab.
*      IF v_total_diff IS NOT INITIAL.
*        IF wa_itab-delqy = wa_itab-grqty.
**    OK
*        ELSE.
*
*          wa_ntab = wa_itab.
*          wa_ntab-grqty = v_total_diff.
*          CLEAR: wa_ntab-menge,wa_ntab-delqy, wa_ntab-borqy, wa_ntab-ststk, wa_ntab-ndays,
*                 v_total_diff, wa_ntab-cond_value.  " Excise Amt.
*          CLEAR: wa_itab-ststk, wa_itab-ndays.
*        ENDIF.
*      ENDIF.
*      MODIFY it_itab FROM wa_itab TRANSPORTING ststk ndays.
*    ENDLOOP.
*
*    APPEND wa_ntab TO it_itab.
*    CLEAR wa_ntab.
*  ENDIF.




* From the below code snippet I understand that this code is used for summation looking @ the CONTROL BREAK stat.
* This piece of code snippet is not working as it should for couple of cases  Eg:- PO 1500034711 (in PRD)
* ~ by NarenKarra  on 02/08/2016 - start

*  LOOP AT  it_itab  INTO wa_itab.
*    v_tabix = sy-tabix.
*    AT NEW ebeln.
*
*    ENDAT.
*    AT NEW ebelp.
*
*    ENDAT.
*
*    AT END OF ebelp.
*      SUM.
*      v_total = wa_itab-delqy.
*      fl_flag = 1.
*
*    ENDAT.
*    IF fl_flag = 1.
*      wa_itab-borqy = wa_itab-menge - v_total.
*      MODIFY it_itab FROM  wa_itab INDEX v_tabix.
*      CLEAR wa_itab.
*      CLEAR fl_flag.
*    ENDIF.
*  ENDLOOP.
*
********************************************************************** ~ by NarenKarra  on 02/08/2016 - end

* Why is their a CLEAR statment on MENGE when ' tabix NE 1 ' ---> need to discuss this wit Poonam !!
* Commenting this LOOP for now, .... unless otherwise ??        ~ by NarenKarra  on 31/07/2016 - start

*  SORT it_itab BY ebeln ebelp.
*  LOOP AT  it_itab  INTO wa_itab.
*    v_tabix1 = sy-tabix.
*    AT NEW ebeln.
*      CLEAR : v_tabix.
*    ENDAT.
*    AT NEW ebelp.
*      CLEAR : v_tabix.
*    ENDAT.
*    v_tabix = v_tabix + 1.
*    IF v_tabix NE 1.
*      CLEAR wa_itab-menge.
*      MODIFY it_itab FROM  wa_itab INDEX v_tabix1.
*      CLEAR wa_itab.
*    ENDIF.
*    AT END OF ebelp.
*
*    ENDAT.
*  ENDLOOP.
********************************************************************** ~ by NarenKarra  on 31/07/2016 - end
  "Start of Anees
  loop at  it_itab  into wa_itab.
    select single aedat from ekko into wa_itab-aedat
      where ebeln = wa_itab-ebeln.
    if wa_itab-ststk <> 0.
      wa_itab-intransit = sy-datum - wa_itab-dldat.
* If stock in Transit is -ve then Days in transit is also req. in -ve since the ' user wants to check the those STO's which has 0 days
      if wa_itab-ststk le 0.                      " added by NK 22.08.2016
        wa_itab-intransit = wa_itab-intransit * -1.
      endif.
    endif.
    modify it_itab from wa_itab transporting intransit.
    clear wa_itab.
  endloop.
  "End of Anees

endform.                    " I_UPLOAD



*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
form check_data.





endform.                    " check_data

" run_bdc

*&---------------------------------------------------------------------*
*&      Form  disp_err
*&---------------------------------------------------------------------*
form disp_records.


  data: it_fieldcat type slis_t_fieldcat_alv,

  wa_fieldcat type line of slis_t_fieldcat_alv,

  wa_layout type slis_layout_alv,
  it_keyinfo type table of slis_keyinfo_alv with header line,
  wa_sort type slis_sortinfo_alv,
   i_sort type  slis_t_sortinfo_alv.



  v_message = 'Stock Transfer Order Details'.

*wa_LAYOUT-BOX_FIELDNAME        = 'SELECTED'.  "field for checkbox

*  wa_LAYOUT-LIGHTS_FIELDNAME     = 'LIGHTS'.  "field for lights
* wa_LAYOUT-LIGHTS_ROLLNAME      = 'QVLOTSLIGHTS'.  "F1 help for lights

  wa_layout-get_selinfos         = 'X'. "show selection screen criteria
  wa_layout-detail_popup         = 'X'. "show detail via popup
  wa_layout-group_change_edit    = 'X'. "allows data to be grouped
  wa_layout-zebra                = 'X'. "striped pattern


  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
   exporting
    i_program_name               = sy-repid
*    I_INTERNAL_TABNAME           = 'IT_ITAB'
       i_structure_name             = 'ZZST_STO_DETAILS1'
*   I_CLIENT_NEVER_DISPLAY       = 'X'
*   I_INCLNAME                   =
     i_bypassing_buffer           = 'X'
      i_buffer_active              ='X'
    changing
      ct_fieldcat                  = it_fieldcat[]
   exceptions
     inconsistent_interface       = 1
     program_error                = 2
     others                       = 3
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = sy-repid
*      i_internal_tabname     = 'ID_T438R'
**      i_inclname             = sy-repid
*    CHANGING
*      ct_fieldcat            = it_fieldcat[]
*    EXCEPTIONS
*      inconsistent_interface = 1
*      program_error          = 2
*      OTHERS                 = 3.
*
*  IF sy-subrc <> 0.
*
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  else.

    loop at it_fieldcat into wa_fieldcat.
      if wa_fieldcat-fieldname = 'EBELN'.
        wa_fieldcat-seltext_l = 'Order Number  '.
        wa_fieldcat-seltext_m = 'Order Number '.
        wa_fieldcat-seltext_s = 'Order Number '.
        wa_fieldcat-key         = 'X'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'BATCH'.                          " Added By - Pradeep Kodinagula
        wa_fieldcat-seltext_l = 'OB BATCH'.
        wa_fieldcat-seltext_m = 'OB BATCH'.
        wa_fieldcat-seltext_s = 'OB BATCH'.
        wa_fieldcat-ddictxt = 'M'.
      endif.


      if wa_fieldcat-fieldname = 'HSDAT'.                           " Added By - Pradeep Kodinagula
        wa_fieldcat-seltext_l = 'Date of Manufacture'.
        wa_fieldcat-seltext_m = 'Date of Manufacture'.
        wa_fieldcat-seltext_s = 'Date of Manufacture'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'CHARG'.
        wa_fieldcat-seltext_l = 'GRR BATCH'.
        wa_fieldcat-seltext_m = 'GRR BATCH'.
        wa_fieldcat-seltext_s = 'GRR BATCH'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'MENGE'.
        wa_fieldcat-seltext_l = 'Order Qty  '.
        wa_fieldcat-seltext_m = 'Order Qty '.
        wa_fieldcat-seltext_s = 'Order Qty '.
        wa_fieldcat-ddictxt = 'M'.
        wa_fieldcat-do_sum  = 'X'.

      endif.

      if wa_fieldcat-fieldname = 'DELQY'.
        wa_fieldcat-seltext_l = 'Despatch Qty'.
        wa_fieldcat-seltext_m = 'Despatch Qty'.
        wa_fieldcat-seltext_s = 'Despatch Qty'.
        wa_fieldcat-ddictxt = 'M'.
        wa_fieldcat-do_sum  = 'X'.

      endif.

      if wa_fieldcat-fieldname = 'GRQTY'.
        wa_fieldcat-seltext_l = 'Received Qty'.
        wa_fieldcat-seltext_m = 'Received Qty'.
        wa_fieldcat-seltext_s = 'Received Qty'.
        wa_fieldcat-ddictxt = 'M'.
        wa_fieldcat-do_sum  = 'X'.

      endif.
      if wa_fieldcat-fieldname = 'BORQY'.
        wa_fieldcat-seltext_l = 'Pend.Ord.Qty'.
        wa_fieldcat-seltext_m = 'Pend.Ord.Qty'.
        wa_fieldcat-seltext_s = 'Pend.Ord.Qty'.
        wa_fieldcat-ddictxt = 'M'.
        wa_fieldcat-do_sum  = 'X'.

      endif.

      if wa_fieldcat-fieldname = 'STSTK'.
        wa_fieldcat-seltext_l = 'Stock in Transit'.
        wa_fieldcat-seltext_m = 'Stock in Transit'.
        wa_fieldcat-seltext_s = 'Stock in Transit'.
        wa_fieldcat-ddictxt = 'M'.
        wa_fieldcat-do_sum  = 'X'.

      endif.
      if wa_fieldcat-fieldname = 'SNAME'.
        wa_fieldcat-seltext_l = 'Supp.Plant.Name'.
        wa_fieldcat-seltext_m = 'Supp.Plant.Name'.
        wa_fieldcat-seltext_s = 'Supp.Plant.Name'.
        wa_fieldcat-ddictxt = 'M'.


      endif.


      if wa_fieldcat-fieldname = 'RNAME'.
        wa_fieldcat-seltext_l = 'Dest.Plant.Name'.
        wa_fieldcat-seltext_m = 'Dest.Plant.Name'.
        wa_fieldcat-seltext_s = 'Dest.Plant.Name'.
        wa_fieldcat-ddictxt = 'M'.


      endif.
      if wa_fieldcat-fieldname = 'DLDAT'.
        wa_fieldcat-seltext_l = 'Despatch .Date'.
        wa_fieldcat-seltext_m = 'Despatch .Date'.
        wa_fieldcat-seltext_s = 'DDespatch .Date'.
        wa_fieldcat-ddictxt = 'M'.


      endif.
      if wa_fieldcat-fieldname = 'PRDAT'.
        wa_fieldcat-seltext_l = 'Plan.Rcpt.Date'.
        wa_fieldcat-seltext_m = 'Plan.Rcpt.Datee'.
        wa_fieldcat-seltext_s = 'Plan.Rcpt.Date'.
        wa_fieldcat-ddictxt = 'M'.


      endif.
      if wa_fieldcat-fieldname = 'GRDAT'.
        wa_fieldcat-seltext_l = 'Goods.Rcpt.Date'.
        wa_fieldcat-seltext_m = 'Goods.Rcpt.Date'.
        wa_fieldcat-seltext_s = 'Goods.Rcpt.Date'.
        wa_fieldcat-ddictxt = 'M'.


      endif.

      if wa_fieldcat-fieldname = 'NDAYS'.
        wa_fieldcat-seltext_l = 'Days in Transit'.
        wa_fieldcat-seltext_m = 'Days in Transit'.
        wa_fieldcat-seltext_s = 'Days in Transit'.
        wa_fieldcat-ddictxt = 'M'.


      endif.

      if wa_fieldcat-fieldname = 'TRANS'.
        wa_fieldcat-seltext_l = 'Vehicle No.'.
        wa_fieldcat-seltext_m = 'Vehicle No.'.
        wa_fieldcat-seltext_s = 'Vehicle No.'.
        wa_fieldcat-ddictxt = 'M'.


      endif.

      if wa_fieldcat-fieldname = 'LRNUM'.
        wa_fieldcat-seltext_l = 'LR Number'.
        wa_fieldcat-seltext_m = 'LR Number'.
        wa_fieldcat-seltext_s = 'LR Number'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'EXNUM'.                                " Added By - Pradeep Kodinagula
        wa_fieldcat-seltext_l = 'Excise Invoice No.'.
        wa_fieldcat-seltext_m = 'Excise Invoice No.'.
        wa_fieldcat-seltext_s = 'Excise Invoice No.'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'EXDAT'.                             " Added By - Pradeep Kodinagula
        wa_fieldcat-seltext_l = 'Excise Doc Date'.
        wa_fieldcat-seltext_m = 'Excise Doc Date'.
        wa_fieldcat-seltext_s = 'Excise Doc Date'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'COND_VALUE'.
        wa_fieldcat-seltext_l = 'Excise Amt.'.
        wa_fieldcat-seltext_m = 'Excise Amt.'.
        wa_fieldcat-seltext_s = 'Excise Amt.'.
        wa_fieldcat-ddictxt = 'M'.
        wa_fieldcat-do_sum  = 'X'.
      endif.



      if wa_fieldcat-fieldname = 'MBLNR'.
        wa_fieldcat-seltext_l = 'GRN No.'.
        wa_fieldcat-seltext_m = 'GRN No.'.
        wa_fieldcat-seltext_s = 'GRN No.'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'FR_AGT'.
        wa_fieldcat-seltext_l = 'Freight Service agent'.
        wa_fieldcat-seltext_m = 'Frght.Serv.agnt.'.
        wa_fieldcat-seltext_s = 'Frght.Serv.agnt.'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'FORW_NAME'.
        wa_fieldcat-seltext_l = 'Freight Service agent Name'.
        wa_fieldcat-seltext_m = 'Frght.Serv.agnt.'.
        wa_fieldcat-seltext_s = 'Frght.Serv.agnt.'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'LR_TYPE'.
        wa_fieldcat-seltext_l = 'Vehicle Type'.
        wa_fieldcat-seltext_m = 'Vehicle Type'.
        wa_fieldcat-seltext_s = 'Vehicle Type'.
        wa_fieldcat-ddictxt = 'M'.
      endif.


      if wa_fieldcat-fieldname = 'ZSTR_RATE'.
        wa_fieldcat-seltext_l = 'MRP Rate(ZSTR)'.
        wa_fieldcat-seltext_m = 'MRP Rate(ZSTR)'.
        wa_fieldcat-seltext_s = 'MRP RATE'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'KBETR'.                                   " Added By - Pradeep Kodinagula
        wa_fieldcat-seltext_l = 'DISC On Base(STRD)'.
        wa_fieldcat-seltext_m = 'DISC On Base(STRD)'.
        wa_fieldcat-seltext_s = 'DISC On Base(STRD)'.
        wa_fieldcat-ddictxt = 'M'.
      endif.

      if wa_fieldcat-fieldname = 'BASE_TOTAL'.                              " Added By - Pradeep Kodinagula
        wa_fieldcat-seltext_l = 'Base Total'.
        wa_fieldcat-seltext_m = 'Base Total'.
        wa_fieldcat-seltext_s = 'Base Total'.
        wa_fieldcat-ddictxt = 'M'.
      endif.


      if wa_fieldcat-fieldname = 'ITEM_TXT'.
        wa_fieldcat-seltext_l = 'PO ITEM TEXT'.
        wa_fieldcat-seltext_m = 'PO ITEM TEXT'.
        wa_fieldcat-seltext_s = 'PO ITEM TEXT'.
        wa_fieldcat-ddictxt = 'M'.
      endif.
      if wa_fieldcat-fieldname = 'WERKS'.
        wa_fieldcat-seltext_l = 'Dest.Plant'.
        wa_fieldcat-seltext_m = 'Dest.Plant'.
        wa_fieldcat-seltext_s = 'Dest.Plant'.
        wa_fieldcat-ddictxt = 'M'.
      endif.
      if wa_fieldcat-fieldname = 'P101_RATE'.
        wa_fieldcat-seltext_l = 'Distributor Price(P101)'.
        wa_fieldcat-seltext_m = 'Dist.Price(P101)'.
        wa_fieldcat-seltext_s = 'Dist.Price(P101)'.
        wa_fieldcat-ddictxt = 'M'.
      endif.
      if wa_fieldcat-fieldname = 'FRC1_RATE'.
        wa_fieldcat-seltext_l = 'Freight(FRC1)'.
        wa_fieldcat-seltext_m = 'Freight(FRC1)'.
        wa_fieldcat-seltext_s = 'Freight(FRC1)'.
        wa_fieldcat-ddictxt = 'M'.
      endif.
      if wa_fieldcat-fieldname = 'BLDAT'.
        wa_fieldcat-seltext_l = 'Delivery Doc.DT'.
        wa_fieldcat-seltext_m = 'Delivery Doc.DT'.
        wa_fieldcat-seltext_s = 'Delivery Doc.DT'.
        wa_fieldcat-ddictxt = 'M'.
      endif.



*       IF  WA_FIELDCAT-FIELDNAME = ''.
*       wa_lfl_fcat-do_sum  = 'X'.
*       ENDIF.

*      if p_qty is initial.
*        if wa_fieldcat-fieldname eq 'QUANTITY' OR wa_fieldcat-fieldname eq 'BLOCKED_QY'
*                           OR wa_fieldcat-fieldname eq 'DELIV_QTY'.
*
*          WA_FIELDCAT-NO_OUT = 'X'.
*
*
*        endif.
*      ELSE.
*        if wa_fieldcat-fieldname eq 'CIFVL' OR wa_fieldcat-fieldname eq 'CIFRL'
*                          OR wa_fieldcat-fieldname eq 'WKURS'.
*
*          WA_FIELDCAT-NO_OUT = 'X'.
*
*
*        endif.
*
*      ENDIF.

      modify it_fieldcat from wa_fieldcat.
      clear wa_fieldcat.
    endloop.

*** Sort on order no
*  wa_sort-spos = '01' .
*  wa_sort-fieldname = 'EBELN'.
*  wa_sort-tabname = 'IT_Itab'.
*  wa_sort-up = 'X'.
*  wa_sort-subtot = 'X'.
*  APPEND wa_sort TO i_sort .
*  CLEAR wa_sort.
*** Sort on plant
**  wa_sort-spos = '02'.
**  wa_sort-fieldname = 'WERKS'.
**  wa_sort-tabname = 'I_EKPO'.
**  wa_sort-up = 'X'.
**  wa_sort-subtot = 'X'.
**  APPEND wa_sort TO i_sort .
**  CLEAR wa_sort.

* PERFORM sub_get_event.

    wa_sort-spos = '01' .
    wa_sort-fieldname = 'EBELN'.
    wa_sort-tabname = 'IT_Itab'.
    wa_sort-up = 'X' .
    wa_sort-subtot = 'X'.
    append wa_sort to it_sort.


    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
        i_bypassing_buffer      = 'X'
        i_callback_program      = sy-repid
        i_callback_user_command = 'USER_COMMAND'
        i_grid_title            = v_message
        is_layout               = wa_layout
        it_fieldcat             = it_fieldcat[]
        it_sort                 = it_sort
        i_default               = 'X'
        i_save                  = 'A'
      tables
        t_outtab                = it_itab
      exceptions
        program_error           = 1
        others                  = 2.

  endif.

endform.                    " disp_err


*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
form user_command using r_ucomm like sy-ucomm
                  selfield type slis_selfield.

  read table it_itab into wa_itab index selfield-tabindex.
  case selfield-sel_tab_field.
    when '1-EBELN'.
      set parameter id 'BES' field wa_itab-ebeln.
      if not wa_itab-ebeln is initial.
        call transaction 'ME23N' and skip first screen.
      endif.
*   WHEN '2-AGR_NAME'.
**      SET PARAMETER ID 'BES' FIELD WA_OUT-AGR_NAME.
*      IF NOT WA_OUT-AGR_NAME IS INITIAL.
*        CALL TRANSACTION 'PFCG' AND SKIP FIRST SCREEN.
*      ENDIF.
  endcase.


*  CASE R_UCOMM.
*
*    WHEN '&IC1'.
**   Check field clicked on within ALVgrid report
*      IF RS_SELFIELD-FIELDNAME = 'EBELN'.
*        CALL TRANSACTION 'ME23N' USING .
*
**     Read data table, using index of row user clicked on
**        CLEAR: IT_OUT , FIELDCATALOG2[].
**        READ TABLE IT INTO WA_FINAL WITH KEY MATNR = RS_SELFIELD-VALUE.
**     Set parameter ID for transaction screen field
*
**        LOOP AT IT INTO WA_FINAL WHERE MATNR = RS_SELFIELD-VALUE.
**          IF WA_FINAL IS NOT INITIAL .
**            APPEND WA_FINAL TO IT_OUT.
**            CLEAR WA_FINAL.
**          ENDIF.
**        ENDLOOP.
**
**        PERFORM NEXT_DISP.
*      ENDIF.
*  ENDCASE.
endform.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  SUB_GET_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SUB_GET_EVENT .
* CONSTANTS : c_formname_subtotal_text TYPE slis_formname VALUE
*'SUBTOTAL_TEXT'.
*  DATA: l_s_event TYPE slis_alv_event.
*  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
*    EXPORTING
*      i_list_type     = 4
*    IMPORTING
*      et_events       = i_event
*    EXCEPTIONS
*      list_type_wrong = 0
*      OTHERS          = 0.
** Subtotal
*  READ TABLE i_event  INTO l_s_event
*                    WITH KEY name = slis_ev_subtotal_text.
*  IF sy-subrc = 0.
*    MOVE c_formname_subtotal_text TO l_s_event-form.
*    MODIFY i_event FROM l_s_event INDEX sy-tabix.
*  ENDIF.
*ENDFORM.                    " SUB_GET_EVENT
*
*FORM subtotal_text CHANGING
*               p_total TYPE any
*               p_subtot_text TYPE slis_subtot_text.
** Material level sub total
*  IF p_subtot_text-criteria = 'EBELN'.
*    p_subtot_text-display_text_for_subtotal
*    = 'ORDER LEVEL Total'(009).
*  ENDIF.
*** Plant level sub total
**  IF p_subtot_text-criteria = 'WERKS'.
**    p_subtot_text-display_text_for_subtotal = 'Plant level total'(010).
**  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VARIANT_INIT
*&---------------------------------------------------------------------*
*       Added by CS on 04.11.2015 for layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form variant_init .
*  BREAK-POINT.
  g_save = 'A'.
  clear gx_variant.
  gx_variant-report = s_repid.
  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save     = g_save
    changing
      cs_variant = gx_variant
    exceptions
      not_found  = 2.
  if sy-subrc = 0.
    p_var = gx_variant-variant.
  endif.

endform.                    " VARIANT_INIT
*&---------------------------------------------------------------------*
*&      Form  VARIANT_F4_SELECTION
*&---------------------------------------------------------------------*
* Addd by CS on 04.11.2015 for F4 Help Layout.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form variant_f4_selection .
*  BREAK-POINT.
  gx_variant-report = sy-repid.
  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = gx_variant
      i_save     = 'A'
    importing
      e_exit     = g_exit
      es_variant = gx_variant
    exceptions
      not_found  = 2.

  if sy-subrc = 2.
    message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.

    if g_exit = space.
      p_var = gx_variant-variant.
    endif.

  endif.
endform.                    " VARIANT_F4_SELECTION
*&---------------------------------------------------------------------*
*&      Form  CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
* Added by CS on 04.11.2015 for validate existance of layout
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_variant_existance .
  if not p_var is initial.
    move p_var to gx_variant-variant.
    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = g_save
      changing
        cs_variant = gx_variant.
  else.
    perform variant_init.
  endif.

endform.                    " CHECK_VARIANT_EXISTANCE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ_EKORG
*&---------------------------------------------------------------------*
* Added by CS on 04.11.2015 for Authorization Pur. Organization.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_auth_obj_ekorg .
  data: lv_err_msg type string.
  if p_ekorg is not initial.
    authority-check object 'M_BEST_EKO'
                   id 'ACTVT' field '03'
                   id 'EKORG' field p_ekorg.
    if not sy-subrc is initial.
      set  cursor          field  p_ekorg.
      concatenate 'You have no authorization for this transaction in Purch. Organization ' p_ekorg into lv_err_msg separated by space.
      message lv_err_msg type 'I' display like 'E'.
      leave list-processing.
    endif.
  endif.
endform.                    " CHECK_AUTH_OBJ_EKORG
*&---------------------------------------------------------------------*
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 04.11.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_auth_obj .
  types: "BEGIN OF type_t024e,
*          ekorg TYPE t024e-ekorg,
*        END OF type_t024e,
        begin of ty_t001w,
          werks type t001w-werks, " Plants
        end of ty_t001w,
        begin of ty_t024,
          ekgrp type t024-ekgrp,  " Purchasing Group
        end of ty_t024,
        begin of ty_t001,
          bukrs type t001-bukrs,  " Company Code
        end of ty_t001
          .
  data: "t_t024e TYPE TABLE OF type_t024e,
        "w_t024e TYPE type_t024e,
        t_t024 type table of ty_t024, " Purchasing Group
        w_t024 type ty_t024,
        t_t001w type table of ty_t001w, " Receiving Plants
        w_t001w type ty_t001w,
        ts_t001w type table of ty_t001w, " Supplying Plants, Added on 04.11.2015
        ws_t001w type ty_t001w,
        t_t001 type table of ty_t001, " Company Code
        w_t001 type ty_t001.

  free : t_t024[], t_t001w[], t_t001[].
  clear: w_t024, w_t001w, w_t001.

  break test1.

  perform  check_auth_obj_ekorg. " Added by CS on 04.11.2015 for Authorization Pur. Organization
***** Start Code: Added by CS on 04.11.2015 for Receiving Plant Authorization. *****
  select werks  " Fetch values of Plant
    from t001w
    into table t_t001w
    where werks in s_werks.

  clear: s_werks, lv_werks_auth_flg.
  refresh: s_werks[].
  if t_t001w[] is not initial.
    loop at t_t001w into w_t001w.
*      AUTHORITY-CHECK OBJECT 'M_MSEG_WWA' " Plant
*                     ID 'ACTVT' FIELD '03'
*                     ID 'WERKS' FIELD w_t001w-werks.
      authority-check object 'ZRPLANT' " Receiving Plant  " Added on 04.11.2015
                     id 'ACTVT' field '03'
                     id 'WERKS' field w_t001w-werks.

      if sy-subrc eq 0.
        s_werks-sign = 'I'.
        s_werks-option = 'EQ'.
        s_werks-low = w_t001w-werks.
        append s_werks.
        clear: s_werks.
      else.
        if lv_werks_auth_flg is initial.  " Authorization Flag
          lv_werks_auth_flg = 'X'.
        endif.
      endif.
      clear: w_t001w.
    endloop.
  endif.
  if s_werks[] is initial.
    s_werks-sign = 'I'.
    s_werks-option = 'EQ'.
    s_werks-low = ''.
    append s_werks.
    clear: s_werks.
  endif.
***** End Code: Added by CS on 04.11.2015 for Receiving Plant Authorization. *****

***** Start Code: Added by CS on 04.11.2015 for Supplying Plant Authorization. *****
  select werks  " Fetch values of Supplying Plant
    from t001w
    into table ts_t001w
    where werks in s_reswk.

  clear: s_reswk, lv_reswk_auth_flg.
  refresh: s_reswk[].
  if ts_t001w[] is not initial.
    loop at ts_t001w into ws_t001w.
      authority-check object 'ZSPLANT' " Supplying Plant
                     id 'ACTVT' field '03'
                     id 'RESWK' field ws_t001w-werks.

      if sy-subrc eq 0.
        s_reswk-sign = 'I'.
        s_reswk-option = 'EQ'.
        s_reswk-low = ws_t001w-werks.
        append s_reswk.
        clear: s_reswk.
      else.
        if lv_reswk_auth_flg is initial.  " Authorization Flag
          lv_reswk_auth_flg = 'X'.
        endif.
      endif.
      clear: ws_t001w.
    endloop.
  endif.
  if s_reswk[] is initial.
    s_reswk-sign = 'I'.
    s_reswk-option = 'EQ'.
    s_reswk-low = ''.
    append s_reswk.
    clear: s_reswk.
  endif.
***** End Code: Added by CS on 04.11.2015 for Supplying Plant Authorization. *****

***** Start Code: Added by CS on 04.11.2015 for Receiving Company Code Authorization. *****
  select bukrs  " Fetch values of Company Code
    from t001
    into table t_t001
    where bukrs in s_bukrs.

  clear: s_bukrs, lv_bukrs_auth_flg.
  refresh: s_bukrs[].
  if t_t001[] is not initial.
    loop at t_t001 into w_t001.
*      CLEAR: lv_werks_auth_flg.
      authority-check object 'F_BKPF_BUK' " Company Code
                     id 'ACTVT' field '03'
                     id 'BUKRS' field w_t001-bukrs.
      if sy-subrc eq 0.
        s_bukrs-sign = 'I'.
        s_bukrs-option = 'EQ'.
        s_bukrs-low = w_t001-bukrs.
        append s_bukrs.
        clear: s_bukrs.
      else.
        if lv_bukrs_auth_flg is initial.  " Authorization Flag
          lv_bukrs_auth_flg = 'X'.
        endif.
      endif.
      clear: w_t001.
    endloop.
  endif.
  if s_bukrs[] is initial.
    s_bukrs-sign = 'I'.
    s_bukrs-option = 'EQ'.
    s_bukrs-low = ''.
    append s_bukrs.
    clear: s_bukrs.
  endif.
***** End Code: Added by CS on 04.11.2015 for Receiving Company Code Authorization. *****

***** Start Code: Added by CS on 04.11.2015 for Receiving Purchasing Group Authorization. *****
  select ekgrp  " Fetch values of Purchasing Group
    from t024
    into table t_t024
    where ekgrp in s_ekgrp.

  clear: s_ekgrp, lv_ekgrp_auth_flg.
  refresh: s_ekgrp[].
  if t_t024[] is not initial.
    loop at t_t024 into w_t024.
*      CLEAR: lv_werks_auth_flg.
      authority-check object 'M_BEST_EKG' " Purchasing Group
                     id 'ACTVT' field '03'
                     id 'EKGRP' field w_t024-ekgrp.
      if sy-subrc eq 0.
        s_ekgrp-sign = 'I'.
        s_ekgrp-option = 'EQ'.
        s_ekgrp-low = w_t024-ekgrp.
        append s_ekgrp.
        clear: s_ekgrp.
      else.
        if lv_ekgrp_auth_flg is initial.  " Authorization Flag
          lv_ekgrp_auth_flg = 'X'.
        endif.
      endif.
      clear: w_t024.
    endloop.
  endif.
  if s_ekgrp[] is initial.
    s_ekgrp-sign = 'I'.
    s_ekgrp-option = 'EQ'.
    s_ekgrp-low = ''.
    append s_ekgrp.
    clear: s_ekgrp.
  endif.
***** End Code: Added by CS on 04.11.2015 for Receiving Purchasing Group Authorization. *****
endform.                    " CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*&      Form  DISP_ZMB5T
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form disp_zmb5t .
  data: lt_seltab  type table of rsparams,
        ls_seltab  like line of lt_seltab.

*  LOOP AT it_itab INTO wa_itab.
*    MOVE : wa_itab-matnr TO wa_zmb5t-matnr,
*           wa_itab-reswk TO wa_zmb5t-reswk,
*           wa_itab-werks to wa_zmb5t-werks.
*    APPEND wa_zmb5t TO it_zmb5t.
*    CLEAR wa_zmb5t.
*  ENDLOOP.
  if s_matnr is not initial.
    loop at s_matnr.
      ls_seltab-selname = 'MATNR'.  " Material
      ls_seltab-kind    = 'S'.
      ls_seltab-sign    = 'I'.
      ls_seltab-option  = 'EQ'.
      ls_seltab-low     = s_matnr-low.
      append ls_seltab to lt_seltab.
    endloop.
  endif.

  loop at s_werks.
    ls_seltab-selname = 'WERKS'.  " Receiving Plant
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = s_werks-low.
    append ls_seltab to lt_seltab.
  endloop.

  loop at s_reswk.
    ls_seltab-selname = 'RESWK'.  " Issuing Plant
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = s_reswk-low.
    append ls_seltab to lt_seltab.
  endloop.


*  SORT it_zmb5t BY matnr reswk.
*  DELETE ADJACENT DUPLICATES FROM it_zmb5t COMPARING matnr reswk.
*  IF it_zmb5t IS NOT INITIAL.
*    LOOP AT it_zmb5t INTO wa_zmb5t.
*      ls_seltab-selname = 'MATNR'.  " Material
*      ls_seltab-kind    = 'S'.
*      ls_seltab-sign    = 'I'.
*      ls_seltab-option  = 'EQ'.
*      ls_seltab-low     = wa_zmb5t-matnr.
*      APPEND ls_seltab TO lt_seltab.
*
*      ls_seltab-selname = 'RESWK'.  " Issuing Plant
*      ls_seltab-kind    = 'S'.
*      ls_seltab-sign    = 'I'.
*      ls_seltab-option  = 'EQ'.
*      ls_seltab-low     = wa_zmb5t-reswk.
*      APPEND ls_seltab TO lt_seltab.
*
*      ls_seltab-selname = 'WERKS'.  " Receiving Plant
*      ls_seltab-kind    = 'S'.
*      ls_seltab-sign    = 'I'.
*      ls_seltab-option  = 'EQ'.
*      ls_seltab-low     = wa_zmb5t-werks.
*      APPEND ls_seltab TO lt_seltab.
*    ENDLOOP.
*  ENDIF.

  submit zrm07mtrb2 with selection-table lt_seltab and return.

endform.                    " DISP_ZMB5T
*&---------------------------------------------------------------------*
*&      Form  DISP_ZMB5T_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form disp_zmb5t_2." using pw_wa_itab-matnr ."type lv_matnr.
  data: lt_seltab  type table of rsparams,
        ls_seltab  like line of lt_seltab,
        lt_listtab    type table of abaplist,
        lt_asci       type standard table of type_asci,
        ls_listtab    type abaplist,                        "#EC NEEDED
        ls_asci       type type_asci.                       "#EC NEEDED

  if wa_itab-matnr is not initial.
*    LOOP AT s_matnr.
    ls_seltab-selname = 'MATNR'.  " Material
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = wa_itab-matnr.
    append ls_seltab to lt_seltab.
*    ENDLOOP.
  endif.

  loop at s_werks.
    ls_seltab-selname = 'WERKS'.  " Receiving Plant
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = s_werks-low.
    append ls_seltab to lt_seltab.
  endloop.

  loop at s_reswk.
    ls_seltab-selname = 'RESWK'.  " Issuing Plant
    ls_seltab-kind    = 'S'.
    ls_seltab-sign    = 'I'.
    ls_seltab-option  = 'EQ'.
    ls_seltab-low     = s_reswk-low.
    append ls_seltab to lt_seltab.
  endloop.

  submit zrm07mtrb3 with selection-table lt_seltab exporting list to memory and return.

*  CALL FUNCTION 'LIST_FROM_MEMORY'
*    TABLES
*      listobject = lt_listtab
*    EXCEPTIONS
*      not_found  = 1
*      OTHERS     = 2.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  CALL FUNCTION 'LIST_TO_ASCI'
*    TABLES
*      listasci           = lt_asci
*      listobject         = lt_listtab
*    EXCEPTIONS
*      empty_list         = 1
*      list_index_invalid = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

endform.                    " DISP_ZMB5T_2
