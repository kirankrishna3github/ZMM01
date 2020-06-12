*&---------------------------------------------------------------------*
*& Report  ZVENDOR_MASTER_DETAILS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*   CHANGED BY: Naren karra
*   CHANGE ON: 25/07/2017
*   REASON FOR CHANGE: Added new column and Acct. Grp - PLNT validation
*   REQUEST #:IRDK928560
* ---------------------------------------------------------------------*
report  zvendor_master_details.

type-pools:slis.

tables : lfa1,lfb1,lfm1.

*ALV DATA DECLARATIONS
data: it_fieldcatalog type  slis_t_fieldcat_alv,
      wa_fieldcatalog type  slis_fieldcat_alv,
      gd_layout       type slis_layout_alv,
      g_variant       like disvariant,
      gt_events       type slis_t_event,
      gd_prntparams   type slis_print_alv.
data : g_save(1)  type c,
       g_exit(1)  type c,
       gx_variant like disvariant,
       gv_sr      type i.

types : begin of ty_adrc,
          addrnumber type adrc-addrnumber,
          name1      type adrc-name1,
          name2      type adrc-name2,
          name3      type adrc-name3,
          name4      type adrc-name4,
          name_co    type adrc-name_co, " added by NK
          city1      type adrc-city1,
          city2      type adrc-city2,
          city_code  type adrc-city_code,
          cityp_code type adrc-cityp_code,
          home_city  type adrc-home_city,
          post_code1 type adrc-post_code1,
          post_code2 type adrc-post_code2,
          po_box     type adrc-po_box,
          street     type adrc-street,
          streetcode type adrc-streetcode,
          str_suppl1 type adrc-str_suppl1,
          str_suppl2 type adrc-str_suppl2,
          str_suppl3 type adrc-str_suppl3,
          location   type adrc-location,
          building   type adrc-building,
          floor      type adrc-floor,
          country    type adrc-country,
          region     type adrc-region,
        end of ty_adrc.

types : begin of ty_lfb1,
          lifnr type lfb1-lifnr,
          bukrs type lfb1-bukrs,
          pernr type lfb1-pernr,
          akont type lfb1-akont,
          zterm type lfb1-zterm,
          frgrp type lfb1-frgrp,
        end of ty_lfb1.

types : begin of ty_lfm1,
          lifnr type lfm1-lifnr,
          ekorg type lfm1-ekorg,
          telf1 type lfm1-telf1,
          zterm type lfm1-zterm,
          inco1 type lfm1-inco1,
          inco2 type lfm1-inco2,
          kalsk type lfm1-kalsk,
          webre type lfm1-webre,
          lebre type lfm1-lebre,
        end of ty_lfm1.
"---------mod1----------------"
types : begin of ts_cvi_vend_link,
          partner_guid type cvi_vend_link-partner_guid,
          vendor       type cvi_vend_link-vendor,
        end of ts_cvi_vend_link.

types: begin of ts_but000,
         partner      type but000-partner,
         partner_guid type but000-partner_guid,
         bpkind       type but000-bpkind,
       end of ts_but000.


"----------end-----------------"
types : begin of ty_final,
          partner        type but000-partner, "mod1
          lifnr          type lfa1-lifnr,
          anred          type lfa1-anred,
          name1          type lfa1-name1,
          name2          type lfa1-name2,
          name_co        type adrc-name_co,
          adrnr          type lfa1-adrnr,
          bukrs          type lfb1-bukrs,
          ekorg          type lfm1-ekorg,
          str_suppl1     type adrc-str_suppl1,
          str_suppl2     type adrc-str_suppl2,
          street         type adrc-street,
          str_suppl3     type adrc-str_suppl3,
          location       type adrc-location,
          ort02          type  kna1-ort02,  """district
          city1          type  adrc-city1,
          post_code1     type adrc-post_code1,
          regio          type lfa1-regio,
          bezei          type t005u-bezei,   """region desc.
          country        type adrc-country,   """country
          telf1          type lfa1-telf1,
          telf2          type lfa1-telf2,
          tel_number     type adr2-tel_number, ""tel number
          telnr_call     type adr2-telnr_call , """mob no
          telfx          type lfa1-telfx,    """fax number
          smtp_addr      type adr6-smtp_addr, """E-Mail Address
          ktokk          type lfa1-ktokk, ""account group
          kunnr          type lfa1-kunnr, "customer - added by NK
          werks          type lfa1-werks, "plant - added by NK
          """""table BNKA (bank master record table)
          bankl          type lfbk-bankl, ""bank key
          banka          type bnka-banka, "" bank name
          stras          type bnka-stras, ""bank address
          ort01          type bnka-ort01, ""bank city
          brnch          type bnka-brnch, ""bank branch name
          swift          type bnka-swift, ""swift code
          bankn          type lfbk-bankn, "" bank name
          koinh          type lfbk-koinh, ""Account holder
          ebpp_accname   type lfbk-ebpp_accname, "acc holder name
*{   REPLACE        SBXK900270                                        1
*\          j_1iexcd       TYPE j_1imovend-j_1iexcd,  """ECC No.
*\          j_1iexrn       TYPE j_1imovend-j_1iexrn,  """Excise Reg. No.
*\          j_1iexrg       TYPE j_1imovend-j_1iexrg,  """Excise Range
*\          j_1iexdi       TYPE j_1imovend-j_1iexdi,   """Excise Division
*\          j_1iexco       TYPE j_1imovend-j_1iexco,   """Commissionerate
*\          j_1icstno      TYPE j_1imovend-j_1icstno,  """CST no.
*\          j_1ilstno      TYPE j_1imovend-j_1ilstno,   """LST no.
*\          j_1ipanno      TYPE j_1imovend-j_1ipanno,   """PAN
*\          j_1iexcive     TYPE j_1imovend-j_1iexcive,  """Exc.Ind.Vendor
*---------------------------------------------------------------------*
*--------------------------- << S/4HANA >> ---------------------------*
*---------------------------------------------------------------------*
* Changed On - Wensday, November 14, 2018
* Changed By - 10106 - Bhushan Mehta
* Purpose    - Table Replacement
* Solution   - Replace Table LFA1 from J_1IMOVEND
* TR         - SBXK900270 - BM:Replace Table Excise to General Master Data:14.11.2018
*--------------------------------------------------------------------*
          j_1iexcd       type lfa1-j_1iexcd,  """ECC No.
          j_1iexrn       type lfa1-j_1iexrn,  """Excise Reg. No.
          j_1iexrg       type lfa1-j_1iexrg,  """Excise Range
          j_1iexdi       type lfa1-j_1iexdi,   """Excise Division
          j_1iexco       type lfa1-j_1iexco,   """Commissionerate
          j_1icstno      type lfa1-j_1icstno,  """CST no.
          j_1ilstno      type lfa1-j_1ilstno,   """LST no.
          j_1ipanno      type lfa1-j_1ipanno,   """PAN
          j_1iexcive     type lfa1-j_1iexcive,  """Exc.Ind.Vendor
*}   REPLACE
          indi_desc(30)  type c,     """"" Exc.Ind.Vendor description
*{   REPLACE        SBXK900270                                        2
*\          j_1ivtyp       TYPE j_1imovend-j_1ivtyp,   """Type of Vendor
          j_1ivtyp       type lfa1-j_1ivtyp,   """Type of Vendor
*}   REPLACE
          typ_ven_desc   type dd07d-ddtext, """"type of vendor description
*{   REPLACE        SBXK900270                                        3
*\          j_1isern       TYPE j_1imovend-j_1isern, """"Service Tax Regn.No.
          j_1isern       type lfa1-j_1isern, """"Service Tax Regn.No.
*}   REPLACE
          konzs          type lfa1-konzs,
          city_ort01     type lfa1-ort01 ,  """city
*        ORT02  type lfa1-ort02,   """district
          namev          type knvk-namev,
          name_second    type knvk-name1,
          zterm          type lfm1-zterm,
          inco1          type lfm1-inco1,
          inco2          type lfm1-inco2,
          kalsk          type lfm1-kalsk,
          webre          type lfm1-webre,
          lebre          type lfm1-lebre,
          zterm_act      type lfb1-zterm,
          text1_desc_act type t052u-text1,
          text1_desc_pur type t052u-text1,
          indi(3)        type c,
          indi1(3)       type c,
          sperq          type lfa1-sperq,
          sdesc          type tq04s-kurztext,
          frgrp          type lfb1-frgrp,
          frgrt          type vbwf07-frgrt,
          akont          type lfb1-akont,
          txt50          type skat-txt50,
***        added GSTN & Tax Indicator by NK on 05.07.2017
          stcd3          type lfa1-stcd3,
          erdat          type lfa1-erdat,
*{   REPLACE        SBXK900270                                        4
*\          ven_class      TYPE j_1imovend-ven_class,
          ven_class      type lfa1-ven_class,
*}   REPLACE
          bptype         type but000-bpkind,
          "Added by varun on 12.11.19
          gst_regio      type z6mma_params-param2,
          "Added by varun on 07.01.2020 for gst no and postal code validation
          check_gst_no   type char30,
          check_postal   type char30,
          check_pan_no   type char100,
          color          type slis_t_specialcol_alv, "for row color
          "added by varun on 22.05.2020
          esic_no        type but0id-idnumber,
          msme_no        type but0id-idnumber,
        end of ty_final.

types : begin of ty_knvk,
          parnr type knvk-parnr,
          kunnr type knvk-kunnr,
          namev type knvk-namev,
          name1 type knvk-name1,
        end of ty_knvk,

        begin of ty_tq04s,
          sperrfkt type tq04s-sperrfkt,
          kurztext type tq04s-kurztext,
        end of ty_tq04s,

        begin of ty_vbwf07,
*        spras TYPE vbwf07-spras,
          frgrp type vbwf07-frgrp,
          frgrt type vbwf07-frgrt,
        end of ty_vbwf07,

*        begin of ty_J_1IMOVEND,
*        lifnr type lifnr,
*        ven_class type J_1IGTAKLD,"J_1IMOVEND-ven_class
*        end of ty_J_1IMOVEND,

        begin of ty_skat,
*        spras TYPE skat-spras,
          saknr type skat-saknr,
          txt50 type skat-txt50,
        end of ty_skat.


data : it_final  type standard table of ty_final,
       wa_final  type ty_final,
       it_lfa1   type standard table of lfa1,
       wa_lfa1   type lfa1,
*       it_J_1IMOVEND type standard table of ty_J_1IMOVEND,
*       wa_J_1IMOVEND type ty_J_1IMOVEND,
       it_lfb1   type standard table of ty_lfb1,
       wa_lfb1   type ty_lfb1,
       it_vbwf07 type standard table of ty_vbwf07,
       wa_vbwf07 type ty_vbwf07,
       it_skat   type standard table of ty_skat,
       wa_skat   type ty_skat,
       it_lfm1   type standard table of ty_lfm1,
       wa_lfm1   type ty_lfm1,
       it_adrc   type standard table of ty_adrc,
       wa_adrc   type ty_adrc,
       it_adr2   type standard table of adr2,
       it_knvk   type standard table of ty_knvk,
       wa_knvk   type ty_knvk,
       wa_adr2   type adr2,
*{   DELETE         SBXK900270                                        5
*\       it_j_1imovend TYPE STANDARD TABLE OF j_1imovend,
*\       wa_j_1imovend TYPE j_1imovend,
*}   DELETE
       it_adr6   type standard table of adr6,
       wa_adr6   type adr6,
       it_lfbk   type standard table of lfbk,
       wa_lfbk   type lfbk,
       it_bnka   type standard table of bnka,
       wa_bnka   type bnka,
       it_tq04s  type table of ty_tq04s,
       wa_tq04s  type ty_tq04s.

types : begin of ty_ddfixvalue,
          low        type ddfixvalue-low,
          high       type ddfixvalue-high,
          option     type ddfixvalue-option,
          ddlanguage type ddfixvalue-ddlanguage,
          ddtext     type ddfixvalue-ddtext,
        end of ty_ddfixvalue.


data : wa_t005u        type t005u-bezei,
       wa_j_1iinddes   type j_1itaxind-j_1iinddes,
       it_fixed_values type standard table of  ty_ddfixvalue,
       gv_text1_pur    type t052u-text1,
       gv_text1_act    type t052u-text1,
       wa_fixed_values type ty_ddfixvalue.

types : begin of ty_purorg,
          sign(1)   type c,
          option(2) type c,
          low(4)    type c,
          high(4)   type c,
        end of ty_purorg.

types : begin of ty_bukrs,
          sign(1)   type c,
          option(2) type c,
          low(4)    type c,
          high(4)   type c,
        end of ty_bukrs.

data : select_purorg    type standard table of ty_purorg,
       wa_select_purorg type ty_purorg,
       select_bukrs     type standard table of ty_bukrs,
       wa_select_bukrs  type ty_bukrs.

data : flag_a(1) type c,
       flag_b(1) type c,
       flag_c(1) type c,
       flag_d(1) type c.

"-------------------Mod1--------------"
data : it_link    type table of ts_cvi_vend_link,
       wa_link    type ts_cvi_vend_link,
       it_partner type table of ts_but000,
       wa_partner type ts_but000.
"----------------end-----------------"
"added by varun on 22.05.2020
types: begin of ty_but0id,
         partner  type but0id-partner,
         type     type but0id-type,
         idnumber type but0id-idnumber,
       end of ty_but0id.
data: lt_but0id type table of ty_but0id.

data: lv_incrt_gst    type flag,  "added by varun on 07.01.2020 for gst and postal code validation
      lv_incrt_postal type flag,
      lv_incrt_pan    type flag,
      ls_color        type slis_specialcol_alv.

selection-screen begin of block blk1 with frame title text-001.
select-options: s_lifnr for lfa1-lifnr , """vendor
                s_bukrs for lfb1-bukrs obligatory,   """company code
                s_accgr for lfa1-ktokk , """account group
                s_purorg for lfm1-ekorg,"""Purchasing Org.
                s_pan for lfa1-j_1ipanno ,
                s_gst for lfa1-stcd3 ,
                s_erdat for lfa1-erdat.
selection-screen end of block blk1   .

selection-screen begin of block layout with frame title text-002.
parameters: p_vari like disvariant-variant. " ALV Variant
selection-screen end of block layout.

initialization.
  perform initialize_variant  .

at selection-screen on value-request for p_vari.
  perform f4_for_variant.

*----------------------------------------------------------------------*
*         :   Start of Selection                                       *
*----------------------------------------------------------------------*
start-of-selection.
  perform fetch_data.

  perform display_data.

end-of-selection.
  perform alv_fieldcatlog.
  perform display_alv_output.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initialize_variant .
  g_save = 'A'.
  clear g_variant.
  g_variant-report = sy-repid.
*  g_variant-variant = p_vari.
  gx_variant = g_variant.

  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save        = g_save
    changing
      cs_variant    = gx_variant
    exceptions
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      others        = 4.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  if sy-subrc = 0.
    p_vari = gx_variant-variant.
  endif.
endform.                    " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_for_variant .
  g_save = 'A'.
  g_variant-report = sy-repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = g_variant
      i_save     = g_save
    importing
      e_exit     = g_exit
      es_variant = gx_variant
    exceptions
      not_found  = 2.
  if sy-subrc = 2.
    message id sy-msgid type 'S'      number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    if g_exit = space.
      p_vari = gx_variant-variant.
    endif.
  endif.
endform.                    " F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fetch_data .
*  break ibm_ams.

*  AUTHORITY-CHECK OBJECT 'M_BEST_EKO'
*  ID 'EKORG' FIELD '3000'.
*  IF sy-subrc = 0 .
**    MESSAGE 'No Authorization' TYPE 'E'.
**  else.
*    if s_purorg-low eq '3000'.
*    append s_purorg to select_purorg.
*    endif.
*  ENDIF.
*
*  AUTHORITY-CHECK OBJECT 'F_LFA1_BUK'
*  id 'BUKRS' field '3000'.
*    IF sy-subrc = 0 .
**    MESSAGE 'No Authorization' TYPE 'E'.
**    else.
*   if s_bukrs-low eq '3000' .
*   append s_bukrs to select_bukrs.
*   endif.
*  ENDIF.
*
*if select_purorg is initial or select_bukrs is initial.
*MESSAGE 'No Authorization' TYPE 'E'.
*endif.

  select *
  from lfa1
  into table it_lfa1
  where lifnr in s_lifnr
  and   ktokk in s_accgr
  and   j_1ipanno in s_pan
  and   stcd3 in s_gst
  and   erdat in s_erdat       .


  if not it_lfa1[] is initial ."and not s_bukrs is initial.


    "----------------mod1------------------------"
    select partner_guid vendor from cvi_vend_link
       into table it_link
       for all entries in it_lfa1 where vendor = it_lfa1-lifnr.
    if it_link[] is not initial.
      select partner partner_guid bpkind from but000
        into table it_partner
        for all entries in  it_link where partner_guid = it_link-partner_guid.
      "added by varun on 22.05.2020
      if sy-subrc eq 0.
        select partner,type,idnumber from but0id into table @lt_but0id
                                     for all entries in @it_partner
                                     where partner eq @it_partner-partner
                                     and   type in ( 'ZESIC', 'ZMSME' ).
      endif.
    endif.
    "----------------end------------------------"




    select sperrfkt kurztext from tq04s into table
       it_tq04s for all entries in it_lfa1 where sperrfkt eq it_lfa1-sperq
                                           and sprache = 'E'.

    select lifnr
           bukrs
           pernr
           akont
           zterm
           frgrp
    from lfb1
    into table it_lfb1
    for all entries in it_lfa1
    where lifnr = it_lfa1-lifnr
    and   bukrs in s_bukrs.

    if it_lfa1[] is initial or it_lfb1[] is initial.
      message 'Data does not exits' type 'I'.
      stop.
    endif.

    if it_lfb1 is not initial.

      select frgrp frgrt from vbwf07 into table it_vbwf07 for all entries in it_lfb1
                                                          where frgrp = it_lfb1-frgrp
                                                          and spras = 'E'.

      select saknr txt50 from skat into table it_skat for all entries in it_lfb1 "IT_SKAT
                                                      where saknr = it_lfb1-akont
                                                      and   spras = 'E'.
    endif.


    select addrnumber
           name1
           name2
           name3
           name4
           name_co
           city1
           city2
           city_code
           cityp_code
           home_city
           post_code1
           post_code2
           po_box
           street
           streetcode
           str_suppl1
           str_suppl2
           str_suppl3
           location
           building
           floor
           country
           region

    from adrc
    into table it_adrc
    for all entries in it_lfa1
    where addrnumber = it_lfa1-adrnr .

  endif.

  if not it_lfa1[] is initial."and not s_purorg is initial.
    select lifnr
           ekorg
           telf1
           zterm
           inco1
           inco2
           kalsk
           webre
           lebre

    from lfm1
    into table it_lfm1
    for all entries in it_lfa1
    where lifnr = it_lfa1-lifnr
    and   ekorg in s_purorg.

*select  PARNR
*        KUNNR
*        NAMEV
*        NAME1
*from  KNVK
*into table it_KNVK
*for all entries in it_lfb1
*where PARNR = it_lfb1-PeRNR.

  endif.

  if not it_lfa1[] is initial.
    select *
    from lfbk into table it_lfbk
    for all entries in it_lfa1
    where lifnr = it_lfa1-lifnr .

*{   DELETE         SBXK900270                                        1
*\    SELECT *
*\    FROM j_1imovend
*\    INTO TABLE it_j_1imovend
*\    FOR ALL ENTRIES IN it_lfa1
*\    WHERE lifnr = it_lfa1-lifnr .
*}   DELETE

    select *
    from adr6
    into table it_adr6
    for all entries in it_lfa1
    where addrnumber = it_lfa1-adrnr.

    select *
    from adr2
    into table it_adr2
    for all entries in it_lfa1
    where addrnumber = it_lfa1-adrnr
    and   dft_receiv = 'X'.

    select *
    from lfbk
    into table it_lfbk
    for all entries in it_lfa1
    where lifnr = it_lfa1-lifnr.

  endif.

  if not it_lfbk[] is initial.
    select *
    from bnka
    into table it_bnka
    for all entries in it_lfbk
    where banks = it_lfbk-banks
    and   bankl = it_lfbk-bankl.
  endif.
endform.                    " FETCH_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data .

  loop at it_lfa1 into wa_lfa1.
    wa_final-lifnr = wa_lfa1-lifnr.

    wa_final-anred = wa_lfa1-anred.

******start of addition made by varun on 07.01.2020 as said by punam on mail
    clear: lv_incrt_postal, lv_incrt_gst, lv_incrt_pan.
    try.
        zcl_bupa_utilities=>validate_postal_code(
          exporting
            iv_entity      = wa_final-lifnr " Customer/Vendor
          receiving
            rv_valid       = lv_incrt_postal   " 'X' = Valid/'' = Invalid
        ).
        wa_final-check_postal = 'Valid Postal Code'.
      catch zcx_generic. " Generic Exception Class
        wa_final-check_postal = 'Incorrect Postal Code'.
    endtry.
    clear ls_color.
    ls_color-fieldname = 'CHECK_POSTAL'.
    ls_color-color-col = cond #( when lv_incrt_postal is initial
                                 then 6 else 5 ).
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    append ls_color to wa_final-color.

    try.
        zcl_bupa_utilities=>validate_gst_number(
          exporting
            iv_entity     = wa_final-lifnr " Customer/Vendor
          receiving
            rv_valid      = lv_incrt_gst " 'X' = Valid/'' = Invalid
        ).
        wa_final-check_gst_no = 'Valid GST Number'.
      catch zcx_generic. " Generic Exception Class
        wa_final-check_gst_no = 'Incorrect GST Number'.
    endtry.

    clear ls_color.
    ls_color-fieldname = 'CHECK_GST_NO'.
    ls_color-color-col = cond #( when lv_incrt_gst is initial
                                 then 6 else 5 ).
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    append ls_color to wa_final-color.

    " PAN check - IHDK905932 - Friday, June 05, 2020 23:23:10
    " Subject: RE: PAN validation | From: Prajay Bhansali [mailto:pbhansali@indofil.com] | Sent: Monday, June 1, 2020 17:09
    try.
        zcl_bupa_utilities=>validate_pan_number(
          exporting
            iv_entity     = wa_final-lifnr " Customer/Vendor
          receiving
            rv_valid      = lv_incrt_pan " 'X' = Valid/'' = Invalid
        ).
        wa_final-check_pan_no = 'Valid PAN Number'.
      catch zcx_generic into data(lox_generic). " Generic Exception Class
        wa_final-check_pan_no = |Incorrect PAN Number - { lox_generic->get_text( ) }|.
    endtry.

    clear ls_color.
    ls_color-fieldname = 'CHECK_PAN_NO'.
    ls_color-color-col = cond #( when lv_incrt_pan is initial
                                 then 6 else 5 ).
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    append ls_color to wa_final-color.

******End of addition made by varun on 07.01.2020 as said by punam on mail

    wa_final-name1 = wa_lfa1-name1.
    wa_final-name2 = wa_lfa1-name2.
    wa_final-adrnr = wa_lfa1-adrnr.
    wa_final-konzs = wa_lfa1-konzs. """"Corporate Group
    wa_final-city_ort01  = wa_lfa1-ort01.
    wa_final-ort02 = wa_lfa1-ort02.
    wa_final-regio = wa_lfa1-regio.
    "start of addition by varun on 12.11.19 as said by punam on mail
    if wa_final-regio is not initial.
      select single param2 from z6mma_params into wa_final-gst_regio
                                             where progname eq 'STATE_MAP'
                                             and   param1   eq wa_final-regio.
    endif.
    "end of addition by varun
    wa_final-telf1 = wa_lfa1-telf1. """phone
    wa_final-telf2 = wa_lfa1-telf2. """mobile
    wa_final-telfx  = wa_lfa1-telfx . """FAX
    wa_final-ktokk = wa_lfa1-ktokk. """"acc. grp.
    wa_final-sperq = wa_lfa1-sperq.
    wa_final-stcd3 = wa_lfa1-stcd3.  " added by NK on 05.07.2017
    wa_final-erdat = wa_lfa1-erdat.

    wa_final-kunnr = wa_lfa1-kunnr.  " added by NK on 25.07.2017
    wa_final-werks = wa_lfa1-werks.  " added by NK on 25.07.2017


    "------------------mod1------------------------------"
    read table it_link into wa_link with key vendor = wa_lfa1-lifnr.
    if sy-subrc = 0.
      read table it_partner into wa_partner with key partner_guid = wa_link-partner_guid.
      if sy-subrc = 0.
        wa_final-partner = wa_partner-partner.
        wa_final-bptype = wa_partner-bpkind.

        loop at lt_but0id into data(ls_but0id) where partner = wa_final-partner.
          if ls_but0id-type eq 'ZMSME'.
            wa_final-msme_no = ls_but0id-idnumber.
          elseif ls_but0id-type eq 'ZESIC'.
            wa_final-esic_no = ls_but0id-idnumber.
          endif.
        endloop.
      endif.
    endif.
    "------------------end------------------------------"

    read table it_tq04s into wa_tq04s with key sperrfkt = wa_lfa1-sperq.

    if sy-subrc = 0.
      wa_final-sdesc = wa_tq04s-kurztext.
    endif.

    select single bezei from t005u
    into wa_t005u
    where spras = 'EN'
    and land1 = 'IN'
    and bland = wa_final-regio.

    wa_final-bezei  =   wa_t005u.

    read table it_lfb1 into wa_lfb1 with key lifnr = wa_lfa1-lifnr.
*                                         bukrs in s_bukrs.
    if sy-subrc = 0 .
      wa_final-bukrs = wa_lfb1-bukrs.
      wa_final-zterm_act = wa_lfb1-zterm.
      select single text1 from t052u
      into gv_text1_act
      where spras = 'EN'
      and   zterm = wa_final-zterm_act.
      wa_final-text1_desc_act = gv_text1_act .


      wa_final-frgrp = wa_lfb1-frgrp.
      wa_final-akont = wa_lfb1-akont.

      read table it_vbwf07 into wa_vbwf07 with key frgrp = wa_lfb1-frgrp.
      if sy-subrc = 0.
        wa_final-frgrt = wa_vbwf07-frgrt.
      endif.

      read table it_skat into wa_skat with key saknr = wa_lfb1-akont.
      if sy-subrc = 0.
        wa_final-txt50 = wa_skat-txt50.
      endif.

    endif.





    read table it_lfm1 into wa_lfm1 with key lifnr = wa_lfa1-lifnr.
    if sy-subrc = 0.
      wa_final-ekorg = wa_lfm1-ekorg.
      wa_final-zterm = wa_lfm1-zterm.
      wa_final-inco1 = wa_lfm1-inco1.
      wa_final-inco2 = wa_lfm1-inco2.
      wa_final-kalsk = wa_lfm1-kalsk.
      wa_final-webre = wa_lfm1-webre.

      if wa_final-webre eq 'X'.
        wa_final-indi = 'YES'.
      else.
        wa_final-indi = 'NO'.
      endif.

      wa_final-lebre = wa_lfm1-lebre.

      if wa_final-webre eq 'X'.
        wa_final-indi1 = 'YES'.
      else.
        wa_final-indi1 = 'NO'.
      endif.
      select single text1 from t052u
      into gv_text1_pur
      where spras = 'EN'
     and   zterm = wa_final-zterm.
      wa_final-text1_desc_pur = gv_text1_pur .
    endif.
    read table it_adrc into wa_adrc with key addrnumber = wa_lfa1-adrnr.
    if sy-subrc = 0.
      wa_final-name_co    = wa_adrc-name_co.
      wa_final-str_suppl1 = wa_adrc-str_suppl1.
      wa_final-str_suppl2 = wa_adrc-str_suppl2.
      wa_final-street     = wa_adrc-street.
      wa_final-str_suppl3 = wa_adrc-str_suppl3.
      wa_final-location   = wa_adrc-location.
      wa_final-post_code1 = wa_adrc-post_code1.
      wa_final-country = wa_adrc-country.
    endif.
    read table it_adr6 into wa_adr6 with key addrnumber = wa_lfa1-adrnr.
    if sy-subrc = 0 .
      wa_final-smtp_addr  = wa_adr6-smtp_addr.
    endif.
    read table it_adr2 into wa_adr2 with key addrnumber = wa_lfa1-adrnr.
    if sy-subrc = 0 .
      wa_final-tel_number = wa_adr2-tel_number.
      wa_final-telnr_call = wa_adr2-telnr_call.
    endif.
    read table it_lfbk into wa_lfbk with key lifnr = wa_lfa1-lifnr.
    if sy-subrc = 0 .
      wa_final-bankl        = wa_lfbk-bankl.
      wa_final-bankn        = wa_lfbk-bankn.
      wa_final-koinh        = wa_lfbk-koinh .
      wa_final-ebpp_accname = wa_lfbk-ebpp_accname.


      read table it_bnka into wa_bnka with key banks = wa_lfbk-banks
                                               bankl = wa_lfbk-bankl.
      if sy-subrc = 0 .

        wa_final-banka = wa_bnka-banka.
        wa_final-stras = wa_bnka-stras.
        wa_final-ort01 = wa_bnka-ort01.
        wa_final-brnch = wa_bnka-brnch.
        wa_final-swift = wa_bnka-swift.
      endif.
    endif.

*{   DELETE         SBXK900270                                        1
*\    READ TABLE it_j_1imovend INTO wa_j_1imovend WITH KEY lifnr = wa_lfa1-lifnr.
*\    IF sy-subrc = 0 .
*\      wa_final-j_1iexcd     =  wa_j_1imovend-j_1iexcd  .
*\      wa_final-j_1iexrn     =  wa_j_1imovend-j_1iexrn  .
*\      wa_final-j_1iexrg     =  wa_j_1imovend-j_1iexrg  .
*\      wa_final-j_1iexdi     =  wa_j_1imovend-j_1iexdi  .
*\      wa_final-j_1iexco     =  wa_j_1imovend-j_1iexco  .
*\      wa_final-j_1icstno    =  wa_j_1imovend-j_1icstno .
*\      wa_final-j_1ilstno    =  wa_j_1imovend-j_1ilstno .
*\      wa_final-j_1ipanno    =  wa_j_1imovend-j_1ipanno .
*\      wa_final-j_1iexcive   =  wa_j_1imovend-j_1iexcive.
*\      wa_final-ven_class    =  wa_j_1imovend-ven_class.
*\
*\      SELECT SINGLE j_1iinddes FROM j_1itaxind
*\      INTO wa_j_1iinddes WHERE j_1iindtax = wa_final-j_1iexcive.
*\
*\      wa_final-indi_desc = wa_j_1iinddes.
*\
*\      wa_final-j_1ivtyp     =  wa_j_1imovend-j_1ivtyp.
*\      wa_final-j_1isern     =  wa_j_1imovend-j_1isern.
*\
*\      CALL FUNCTION 'DDIF_FIELDINFO_GET'
*\        EXPORTING
*\          tabname        = 'J_1IMOVEND'
*\          fieldname      = 'J_1IVTYP'
*\          langu          = sy-langu
*\          lfieldname     = 'J_1IVTYP'
*\*         ALL_TYPES      = ' '
*\*         GROUP_NAMES    = ' '
*\*         UCLEN          =
*\* IMPORTING
*\*         X030L_WA       =
*\*         DDOBJTYPE      =
*\*         DFIES_WA       =
*\*         LINES_DESCR    =
*\        TABLES
*\*         DFIES_TAB      =
*\          fixed_values   = it_fixed_values
*\        EXCEPTIONS
*\          not_found      = 1
*\          internal_error = 2
*\          OTHERS         = 3.
*\      IF sy-subrc <> 0.
*\        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*\                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*\      ENDIF.
*\
*\      READ TABLE it_fixed_values INTO wa_fixed_values WITH KEY low = wa_final-j_1ivtyp.
*\      IF sy-subrc = 0 .
*\        wa_final-typ_ven_desc = wa_fixed_values-ddtext.
*\      ENDIF.
*\    ENDIF.
*}   DELETE
*{   INSERT         SBXK900270                                        2

    wa_final-j_1iexcd   =  wa_lfa1-j_1iexcd.
    wa_final-j_1iexrn   =  wa_lfa1-j_1iexrn.
    wa_final-j_1iexrg   =  wa_lfa1-j_1iexrg.
    wa_final-j_1iexdi   =  wa_lfa1-j_1iexdi.
    wa_final-j_1iexco   =  wa_lfa1-j_1iexco.
    wa_final-j_1icstno  =  wa_lfa1-j_1icstno.
    wa_final-j_1ilstno  =  wa_lfa1-j_1ilstno.
    wa_final-j_1ipanno  =  wa_lfa1-j_1ipanno.
    wa_final-j_1iexcive =  wa_lfa1-j_1iexcive.
    wa_final-ven_class  =  wa_lfa1-ven_class.

    select single j_1iinddes
             from j_1itaxind
             into wa_j_1iinddes
            where j_1iindtax eq wa_final-j_1iexcive.

    wa_final-indi_desc = wa_j_1iinddes.

    call function 'DDIF_FIELDINFO_GET'
      exporting
        tabname        = 'LFA1'
        fieldname      = 'J_1IVTYP'
        langu          = sy-langu
        lfieldname     = 'J_1IVTYP'
      tables
        fixed_values   = it_fixed_values
      exceptions
        not_found      = 1
        internal_error = 2
        others         = 3.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    read table it_fixed_values into wa_fixed_values with key low = wa_final-j_1ivtyp.
    if sy-subrc = 0 .
      wa_final-typ_ven_desc = wa_fixed_values-ddtext.
    endif.

*}   INSERT
    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_final-lifnr
      importing
        output = wa_final-lifnr.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = wa_final-adrnr
      importing
        output = wa_final-adrnr.

    authority-check object 'M_BEST_EKO'
    id 'EKORG' field wa_final-ekorg.
    if sy-subrc = 0.
      authority-check object 'F_LFA1_BUK'
      id 'BUKRS' field wa_final-bukrs.
      if sy-subrc = 0 .
        append wa_final to it_final.
      else.
        data(lv_auth_failed) = abap_true.
      endif.
    else.
      lv_auth_failed = abap_true.
    endif.
*{   REPLACE        SBXK900270                                        3
*\    CLEAR : wa_final,wa_lfa1,wa_lfb1, wa_skat, wa_vbwf07,lfm1,wa_adrc,wa_adr2,wa_adr6,wa_lfbk,wa_bnka,wa_t005u,wa_j_1imovend,wa_fixed_values,wa_j_1iinddes,gv_text1_pur,gv_text1_act.
    clear : wa_final,wa_lfa1,wa_lfb1, wa_skat, wa_vbwf07,lfm1,wa_adrc,wa_adr2,wa_adr6,wa_lfbk,wa_bnka,wa_t005u,
            wa_fixed_values,wa_j_1iinddes,gv_text1_pur,gv_text1_act.
*}   REPLACE

  endloop.

*  if  it_final[] is initial  .
*    message 'No Authorization' type 'E'.
*  endif.

  if not s_bukrs is initial.
    delete it_final where bukrs not in s_bukrs .
  endif.

*  IF NOT s_purorg IS INITIAL.                              " Finance vendors for which purchase org is not maintained is not displaying in output
*    DELETE it_final WHERE ekorg NOT IN s_purorg .          " as suggested by Mr.Venu this validation is removed - Pradeep K
*  ENDIF.

  if it_final[] is initial.
    message 'No data found' type 'I'.
    if lv_auth_failed = abap_true.
      message 'Missing authorization. Check SU53.' type 'W'.
    endif.
    stop.
  endif.

**  AUTHORITY-CHECK OBJECT 'M_BEST_EKO'
**  ID 'EKORG' FIELD s_purorg-low.
**  IF sy-subrc <> 0 .
**    flag_a = 'X'.
**    delete it_final where ekorg eq s_purorg-low.
**  ENDIF.
**
**  AUTHORITY-CHECK OBJECT 'M_BEST_EKO'
**  ID 'EKORG' FIELD s_purorg-high.
**  IF sy-subrc <> 0 .
**    flag_b = 'X'.
**    delete it_final where ekorg eq s_purorg-high.
**  ENDIF.
**
**  AUTHORITY-CHECK OBJECT 'F_LFA1_BUK'
**  id 'BUKRS' field s_bukrs-low.
**    IF sy-subrc <> 0 .
**    flag_c = 'X'.
**   delete it_final where bukrs eq s_bukrs-low.
**  ENDIF.
**
**  AUTHORITY-CHECK OBJECT 'F_LFA1_BUK'
**  id 'BUKRS' field s_bukrs-high.
**    IF sy-subrc <> 0 .
**    flag_d = 'X'.
**   delete it_final where bukrs eq s_bukrs-high.
**  ENDIF.
**
**if not flag_a is initial and not flag_b is initial and not flag_c is initial and not flag_d is initial.
**delete it_final[] where bukrs not in s_bukrs.
**delete it_final[] where ekorg not in s_purorg.
**endif.

*  IF  it_final[] IS INITIAL  ."AND ( NOT flag_a IS INITIAL OR NOT flag_b IS INITIAL OR NOT flag_c IS INITIAL OR NOT flag_d IS INITIAL ) ).
*    MESSAGE 'No Authorization' TYPE 'E'.
*  ENDIF.

endform.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form alv_fieldcatlog .
  data : cnt type sy-index.
  clear : wa_fieldcatalog,
          it_fieldcatalog[].


  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'PARTNER'.
  wa_fieldcatalog-seltext_m   = 'Partner'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'LIFNR'.
  wa_fieldcatalog-seltext_m   = 'Vendor'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ANRED'.
  wa_fieldcatalog-seltext_m   = 'TITLE'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'NAME1'.
  wa_fieldcatalog-seltext_m   = 'NAME1'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'NAME2'.
  wa_fieldcatalog-seltext_m   = 'NAME2'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'NAME_CO'.
  wa_fieldcatalog-seltext_m   = 'C/o Name'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ADRNR'.
  wa_fieldcatalog-seltext_m   = 'Address Key'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'BUKRS'.
  wa_fieldcatalog-seltext_m   = 'Company Code'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'EKORG'.
  wa_fieldcatalog-seltext_m   = 'Purchse Organization'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'STR_SUPPL1'.
  wa_fieldcatalog-seltext_m   = 'Street 1'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'STR_SUPPL2'.
  wa_fieldcatalog-seltext_m   = 'Street 2'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'STREET'.
  wa_fieldcatalog-seltext_m   = 'Street'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'STR_SUPPL3'.
  wa_fieldcatalog-seltext_m   = 'Street4'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'LOCATION'.
  wa_fieldcatalog-seltext_m   = 'Street 5'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ORT02'.
  wa_fieldcatalog-seltext_m   = 'District'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'CITY_ORT01'.
  wa_fieldcatalog-seltext_m   = 'City'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'POST_CODE1'.
  wa_fieldcatalog-seltext_m   = 'Postl Code'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'CHECK_POSTAL'.
  wa_fieldcatalog-seltext_m   = 'Postal Code Log'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'REGIO'.
  wa_fieldcatalog-seltext_m   = 'Region'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'BEZEI'.
  wa_fieldcatalog-seltext_m   = 'Region'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'GST_REGIO'.
  wa_fieldcatalog-seltext_m   = 'GST Region'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'COUNTRY'.
  wa_fieldcatalog-seltext_m   = 'country'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

*  add 1 to cnt.
*  wa_fieldcatalog-fieldname   = 'TEL_NUMBER'.
*  wa_fieldcatalog-seltext_m   = 'Caller number'.
*  wa_fieldcatalog-col_pos     = cnt.
*  append wa_fieldcatalog to it_fieldcatalog.
*  clear  wa_fieldcatalog.
*
*  add 1 to cnt.
*  wa_fieldcatalog-fieldname   = 'TELNR_CALL'.
*  wa_fieldcatalog-seltext_m   = 'Caller number'.
*  wa_fieldcatalog-col_pos     = cnt.
*  append wa_fieldcatalog to it_fieldcatalog.
*  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'TELF1'.
  wa_fieldcatalog-seltext_m   = 'Telephone'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'TELF2'.
  wa_fieldcatalog-seltext_m   = 'Mobile Phone'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.


  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'SMTP_ADDR'.
  wa_fieldcatalog-seltext_m   = 'E-Mail Address'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'TELFX'.
  wa_fieldcatalog-seltext_m   = 'FAX Number'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'KTOKK'.
  wa_fieldcatalog-seltext_m   = 'Group'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'KUNNR'.
  wa_fieldcatalog-seltext_m   = 'Customer'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'WERKS'.
  wa_fieldcatalog-seltext_m   = 'Plant'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'BANKL'.
  wa_fieldcatalog-seltext_m   = 'Bank Key'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'BANKA'.
  wa_fieldcatalog-seltext_m   = 'Bank name'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'STRAS'.
  wa_fieldcatalog-seltext_m   = 'Bank address'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ORT01'.
  wa_fieldcatalog-seltext_m   = 'Bank city'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'BRNCH'.
  wa_fieldcatalog-seltext_m   = 'Bank Banch name'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'SWIFT'.
  wa_fieldcatalog-seltext_m   = 'Swift code'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'BANKN'.
  wa_fieldcatalog-seltext_m   = 'Bank name'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'KOINH'.
  wa_fieldcatalog-seltext_m   = 'Account holder'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'EBPP_ACCNAME'.
  wa_fieldcatalog-seltext_m   = 'Account holder name'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXCD'.
  wa_fieldcatalog-seltext_m   = 'ECC No.'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXRN'.
  wa_fieldcatalog-seltext_m   = 'Excise Reg. No.'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXRG' .
  wa_fieldcatalog-seltext_m   = 'Excise Range'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXDI'.
  wa_fieldcatalog-seltext_m   = 'Excise Division'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXCO'.
  wa_fieldcatalog-seltext_m   = 'Excise Commissionerate'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1ICSTNO' .
  wa_fieldcatalog-seltext_m   = 'CST No'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1ILSTNO' .
  wa_fieldcatalog-seltext_m   = 'LST No'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IPANNO'.
  wa_fieldcatalog-seltext_m   = 'PAN No.'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXCIVE' .
  wa_fieldcatalog-seltext_m   = 'Excise Indicator'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'INDI_DESC' .
  wa_fieldcatalog-seltext_m   = 'Exc.Ind.desc.'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1IVTYP'.
  wa_fieldcatalog-seltext_m   = 'Type of Vendor'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'TYP_VEN_DESC' .
  wa_fieldcatalog-seltext_m   = 'Type Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'J_1ISERN' .
  wa_fieldcatalog-seltext_m   = 'Service Tax Reg.No.'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'KONZS' .
  wa_fieldcatalog-seltext_m   = 'Corporate Group'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'INCO1' .
  wa_fieldcatalog-seltext_m   = 'Incoterms'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'INCO2' .
  wa_fieldcatalog-seltext_m   = 'Incoterms Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'KALSK' .
  wa_fieldcatalog-seltext_m   = 'Vendor schema'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'INDI' .
  wa_fieldcatalog-seltext_m   = 'GR-Base'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'INDI1' .
  wa_fieldcatalog-seltext_m   = 'Service-Base'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ZTERM' .
  wa_fieldcatalog-seltext_m   = 'Terms of Pay.(Purchase)'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'TEXT1_DESC_PUR' .
  wa_fieldcatalog-seltext_m   = 'Terms of Pay.Pur.desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ZTERM_ACT' .
  wa_fieldcatalog-seltext_m   = 'Payt Terms(ACT)'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'TEXT1_DESC_ACT' .
  wa_fieldcatalog-seltext_m   = 'Payt Terms(ACT) Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.


  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'SPERQ' .
  wa_fieldcatalog-seltext_m   = 'Block Function'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'SDESC' .
  wa_fieldcatalog-seltext_m   = 'Block Function Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

*--
  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'AKONT' .
  wa_fieldcatalog-seltext_m   = 'Recon Account.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'TXT50' .
  wa_fieldcatalog-seltext_m   = 'Recon A/c Descrition.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  wa_fieldcatalog-outputlen = 50.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'FRGRP' .
  wa_fieldcatalog-seltext_m   = 'Release group'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'FRGRT' .
  wa_fieldcatalog-seltext_m   = 'Release group Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

** added by NK on 05.07.2017
  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'STCD3' .
  wa_fieldcatalog-seltext_s   = 'GST No.'.
  wa_fieldcatalog-seltext_m   = 'Tax Number 3 '.
  wa_fieldcatalog-seltext_l   = 'GST No. Tax Number 3'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'CHECK_GST_NO'.
  wa_fieldcatalog-seltext_m   = 'GST No. Log'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'CHECK_PAN_NO'.
  wa_fieldcatalog-seltext_m   = 'PAN No. Log'.
  wa_fieldcatalog-col_pos     = cnt.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'VEN_CLASS' .
  wa_fieldcatalog-seltext_m   = 'GST Ven Class.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'BPTYPE' .
  wa_fieldcatalog-seltext_m   = 'BP Type'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.


  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ERDAT' .
  wa_fieldcatalog-seltext_m   = 'Creation Date'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'ESIC_NO' .
  wa_fieldcatalog-seltext_m   = 'ESIC Number'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

  add 1 to cnt.
  wa_fieldcatalog-fieldname   = 'MSME_NO' .
  wa_fieldcatalog-seltext_m   = 'MSME Number'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  append wa_fieldcatalog to it_fieldcatalog.
  clear  wa_fieldcatalog.

endform.                    " ALV_FIELDCATLOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_alv_output .

  if not it_final[] is initial.

    gd_layout-zebra = 'X'.
    gd_layout-colwidth_optimize = 'X'.
    gd_layout-coltab_fieldname = 'COLOR'.

    call function 'REUSE_ALV_GRID_DISPLAY'
      exporting
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER = ' '
*       I_BUFFER_ACTIVE    = ' '
        i_callback_program = sy-repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
*       I_GRID_TITLE       =
*       I_GRID_SETTINGS    =
        is_layout          = gd_layout
        it_fieldcat        = it_fieldcatalog[]
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
*       IT_SORT            =
*       IT_FILTER          =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        i_save             = 'A'
        is_variant         = gx_variant
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       I_HTML_HEIGHT_TOP  = 0
*       I_HTML_HEIGHT_END  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       IR_SALV_FULLSCREEN_ADAPTER        =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      tables
        t_outtab           = it_final[]
      exceptions
        program_error      = 1
        others             = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.
  endif.
endform.                    " DISPLAY_ALV_OUTPUT
