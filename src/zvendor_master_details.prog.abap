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
REPORT  zvendor_master_details.

TYPE-POOLS:slis.

TABLES : lfa1,lfb1,lfm1.

*ALV DATA DECLARATIONS
DATA: it_fieldcatalog TYPE  slis_t_fieldcat_alv,
      wa_fieldcatalog TYPE  slis_fieldcat_alv,
      gd_layout       TYPE slis_layout_alv,
      g_variant       LIKE disvariant,
      gt_events       TYPE slis_t_event,
      gd_prntparams   TYPE slis_print_alv.
DATA : g_save(1)  TYPE c,
       g_exit(1)  TYPE c,
       gx_variant LIKE disvariant,
       gv_sr      TYPE i.

TYPES : BEGIN OF ty_adrc,
          addrnumber TYPE adrc-addrnumber,
          name1      TYPE adrc-name1,
          name2      TYPE adrc-name2,
          name3      TYPE adrc-name3,
          name4      TYPE adrc-name4,
          name_co    TYPE adrc-name_co, " added by NK
          city1      TYPE adrc-city1,
          city2      TYPE adrc-city2,
          city_code  TYPE adrc-city_code,
          cityp_code TYPE adrc-cityp_code,
          home_city  TYPE adrc-home_city,
          post_code1 TYPE adrc-post_code1,
          post_code2 TYPE adrc-post_code2,
          po_box     TYPE adrc-po_box,
          street     TYPE adrc-street,
          streetcode TYPE adrc-streetcode,
          str_suppl1 TYPE adrc-str_suppl1,
          str_suppl2 TYPE adrc-str_suppl2,
          str_suppl3 TYPE adrc-str_suppl3,
          location   TYPE adrc-location,
          building   TYPE adrc-building,
          floor      TYPE adrc-floor,
          country    TYPE adrc-country,
          region     TYPE adrc-region,
        END OF ty_adrc.

TYPES : BEGIN OF ty_lfb1,
          lifnr TYPE lfb1-lifnr,
          bukrs TYPE lfb1-bukrs,
          pernr TYPE lfb1-pernr,
          akont TYPE lfb1-akont,
          zterm TYPE lfb1-zterm,
          frgrp TYPE lfb1-frgrp,
        END OF ty_lfb1.

TYPES : BEGIN OF ty_lfm1,
          lifnr TYPE lfm1-lifnr,
          ekorg TYPE lfm1-ekorg,
          telf1 TYPE lfm1-telf1,
          zterm TYPE lfm1-zterm,
          inco1 TYPE lfm1-inco1,
          inco2 TYPE lfm1-inco2,
          kalsk TYPE lfm1-kalsk,
          webre TYPE lfm1-webre,
          lebre TYPE lfm1-lebre,
        END OF ty_lfm1.
"---------mod1----------------"
TYPES : BEGIN OF ts_cvi_vend_link,
          partner_guid TYPE cvi_vend_link-partner_guid,
          vendor       TYPE cvi_vend_link-vendor,
        END OF ts_cvi_vend_link.

TYPES: BEGIN OF ts_but000,
         partner      TYPE but000-partner,
         partner_guid TYPE but000-partner_guid,
         bpkind       TYPE but000-bpkind,
       END OF ts_but000.


"----------end-----------------"
TYPES : BEGIN OF ty_final,
          partner        TYPE but000-partner, "mod1
          lifnr          TYPE lfa1-lifnr,
          name1          TYPE lfa1-name1,
          name2          TYPE lfa1-name2,
          name_co        TYPE adrc-name_co,
          adrnr          TYPE lfa1-adrnr,
          bukrs          TYPE lfb1-bukrs,
          ekorg          TYPE lfm1-ekorg,
          str_suppl1     TYPE adrc-str_suppl1,
          str_suppl2     TYPE adrc-str_suppl2,
          street         TYPE adrc-street,
          str_suppl3     TYPE adrc-str_suppl3,
          location       TYPE adrc-location,
          ort02          TYPE  kna1-ort02,  """district
          city1          TYPE  adrc-city1,
          post_code1     TYPE adrc-post_code1,
          regio          TYPE lfa1-regio,
          bezei          TYPE t005u-bezei,   """region desc.
          country        TYPE adrc-country,   """country
          telf1          TYPE lfa1-telf1,
          telf2          TYPE lfa1-telf2,
          tel_number     TYPE adr2-tel_number, ""tel number
          telnr_call     TYPE adr2-telnr_call , """mob no
          telfx          TYPE lfa1-telfx,    """fax number
          smtp_addr      TYPE adr6-smtp_addr, """E-Mail Address
          ktokk          TYPE lfa1-ktokk, ""account group
          kunnr          TYPE lfa1-kunnr, "customer - added by NK
          werks          TYPE lfa1-werks, "plant - added by NK
          """""table BNKA (bank master record table)
          bankl          TYPE lfbk-bankl, ""bank key
          banka          TYPE bnka-banka, "" bank name
          stras          TYPE bnka-stras, ""bank address
          ort01          TYPE bnka-ort01, ""bank city
          brnch          TYPE bnka-brnch, ""bank branch name
          swift          TYPE bnka-swift, ""swift code
          bankn          TYPE lfbk-bankn, "" bank name
          koinh          TYPE lfbk-koinh, ""Account holder
          ebpp_accname   TYPE lfbk-ebpp_accname, "acc holder name
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
          j_1iexcd       TYPE lfa1-j_1iexcd,  """ECC No.
          j_1iexrn       TYPE lfa1-j_1iexrn,  """Excise Reg. No.
          j_1iexrg       TYPE lfa1-j_1iexrg,  """Excise Range
          j_1iexdi       TYPE lfa1-j_1iexdi,   """Excise Division
          j_1iexco       TYPE lfa1-j_1iexco,   """Commissionerate
          j_1icstno      TYPE lfa1-j_1icstno,  """CST no.
          j_1ilstno      TYPE lfa1-j_1ilstno,   """LST no.
          j_1ipanno      TYPE lfa1-j_1ipanno,   """PAN
          j_1iexcive     TYPE lfa1-j_1iexcive,  """Exc.Ind.Vendor
*}   REPLACE
          indi_desc(30)  TYPE c,     """"" Exc.Ind.Vendor description
*{   REPLACE        SBXK900270                                        2
*\          j_1ivtyp       TYPE j_1imovend-j_1ivtyp,   """Type of Vendor
          j_1ivtyp       TYPE lfa1-j_1ivtyp,   """Type of Vendor
*}   REPLACE
          typ_ven_desc   TYPE dd07d-ddtext, """"type of vendor description
*{   REPLACE        SBXK900270                                        3
*\          j_1isern       TYPE j_1imovend-j_1isern, """"Service Tax Regn.No.
          j_1isern       TYPE lfa1-j_1isern, """"Service Tax Regn.No.
*}   REPLACE
          konzs          TYPE lfa1-konzs,
          city_ort01     TYPE lfa1-ort01 ,  """city
*        ORT02  type lfa1-ort02,   """district
          namev          TYPE knvk-namev,
          name_second    TYPE knvk-name1,
          zterm          TYPE lfm1-zterm,
          inco1          TYPE lfm1-inco1,
          inco2          TYPE lfm1-inco2,
          kalsk          TYPE lfm1-kalsk,
          webre          TYPE lfm1-webre,
          lebre          TYPE lfm1-lebre,
          zterm_act      TYPE lfb1-zterm,
          text1_desc_act TYPE t052u-text1,
          text1_desc_pur TYPE t052u-text1,
          indi(3)        TYPE c,
          indi1(3)       TYPE c,
          sperq          TYPE lfa1-sperq,
          sdesc          TYPE tq04s-kurztext,
          frgrp          TYPE lfb1-frgrp,
          frgrt          TYPE vbwf07-frgrt,
          akont          TYPE lfb1-akont,
          txt50          TYPE skat-txt50,
***        added GSTN & Tax Indicator by NK on 05.07.2017
          stcd3          TYPE lfa1-stcd3,
          erdat          TYPE lfa1-erdat,
*{   REPLACE        SBXK900270                                        4
*\          ven_class      TYPE j_1imovend-ven_class,
          ven_class      TYPE lfa1-ven_class,
*}   REPLACE
          bptype         TYPE but000-bpkind,
          "Added by varun on 12.11.19
          gst_regio      TYPE z6mma_params-param2,
          "Added by varun on 07.01.2020 for gst no and postal code validation
          check_gst_no   TYPE char30,
          check_postal   TYPE char30,
          color          TYPE slis_t_specialcol_alv, "for row color
          "added by varun on 22.05.2020
          esic_no        TYPE but0id-idnumber,
          msme_no        TYPE but0id-idnumber,
        END OF ty_final.

TYPES : BEGIN OF ty_knvk,
          parnr TYPE knvk-parnr,
          kunnr TYPE knvk-kunnr,
          namev TYPE knvk-namev,
          name1 TYPE knvk-name1,
        END OF ty_knvk,

        BEGIN OF ty_tq04s,
          sperrfkt TYPE tq04s-sperrfkt,
          kurztext TYPE tq04s-kurztext,
        END OF ty_tq04s,

        BEGIN OF ty_vbwf07,
*        spras TYPE vbwf07-spras,
          frgrp TYPE vbwf07-frgrp,
          frgrt TYPE vbwf07-frgrt,
        END OF ty_vbwf07,

*        begin of ty_J_1IMOVEND,
*        lifnr type lifnr,
*        ven_class type J_1IGTAKLD,"J_1IMOVEND-ven_class
*        end of ty_J_1IMOVEND,

        BEGIN OF ty_skat,
*        spras TYPE skat-spras,
          saknr TYPE skat-saknr,
          txt50 TYPE skat-txt50,
        END OF ty_skat.


DATA : it_final  TYPE STANDARD TABLE OF ty_final,
       wa_final  TYPE ty_final,
       it_lfa1   TYPE STANDARD TABLE OF lfa1,
       wa_lfa1   TYPE lfa1,
*       it_J_1IMOVEND type standard table of ty_J_1IMOVEND,
*       wa_J_1IMOVEND type ty_J_1IMOVEND,
       it_lfb1   TYPE STANDARD TABLE OF ty_lfb1,
       wa_lfb1   TYPE ty_lfb1,
       it_vbwf07 TYPE STANDARD TABLE OF ty_vbwf07,
       wa_vbwf07 TYPE ty_vbwf07,
       it_skat   TYPE STANDARD TABLE OF ty_skat,
       wa_skat   TYPE ty_skat,
       it_lfm1   TYPE STANDARD TABLE OF ty_lfm1,
       wa_lfm1   TYPE ty_lfm1,
       it_adrc   TYPE STANDARD TABLE OF ty_adrc,
       wa_adrc   TYPE ty_adrc,
       it_adr2   TYPE STANDARD TABLE OF adr2,
       it_knvk   TYPE STANDARD TABLE OF ty_knvk,
       wa_knvk   TYPE ty_knvk,
       wa_adr2   TYPE adr2,
*{   DELETE         SBXK900270                                        5
*\       it_j_1imovend TYPE STANDARD TABLE OF j_1imovend,
*\       wa_j_1imovend TYPE j_1imovend,
*}   DELETE
       it_adr6   TYPE STANDARD TABLE OF adr6,
       wa_adr6   TYPE adr6,
       it_lfbk   TYPE STANDARD TABLE OF lfbk,
       wa_lfbk   TYPE lfbk,
       it_bnka   TYPE STANDARD TABLE OF bnka,
       wa_bnka   TYPE bnka,
       it_tq04s  TYPE TABLE OF ty_tq04s,
       wa_tq04s  TYPE ty_tq04s.

TYPES : BEGIN OF ty_ddfixvalue,
          low        TYPE ddfixvalue-low,
          high       TYPE ddfixvalue-high,
          option     TYPE ddfixvalue-option,
          ddlanguage TYPE ddfixvalue-ddlanguage,
          ddtext     TYPE ddfixvalue-ddtext,
        END OF ty_ddfixvalue.


DATA : wa_t005u        TYPE t005u-bezei,
       wa_j_1iinddes   TYPE j_1itaxind-j_1iinddes,
       it_fixed_values TYPE STANDARD TABLE OF  ty_ddfixvalue,
       gv_text1_pur    TYPE t052u-text1,
       gv_text1_act    TYPE t052u-text1,
       wa_fixed_values TYPE ty_ddfixvalue.

TYPES : BEGIN OF ty_purorg,
          sign(1)   TYPE c,
          option(2) TYPE c,
          low(4)    TYPE c,
          high(4)   TYPE c,
        END OF ty_purorg.

TYPES : BEGIN OF ty_bukrs,
          sign(1)   TYPE c,
          option(2) TYPE c,
          low(4)    TYPE c,
          high(4)   TYPE c,
        END OF ty_bukrs.

DATA : select_purorg    TYPE STANDARD TABLE OF ty_purorg,
       wa_select_purorg TYPE ty_purorg,
       select_bukrs     TYPE STANDARD TABLE OF ty_bukrs,
       wa_select_bukrs  TYPE ty_bukrs.

DATA : flag_a(1) TYPE c,
       flag_b(1) TYPE c,
       flag_c(1) TYPE c,
       flag_d(1) TYPE c.

"-------------------Mod1--------------"
DATA : it_link    TYPE TABLE OF ts_cvi_vend_link,
       wa_link    TYPE ts_cvi_vend_link,
       it_partner TYPE TABLE OF ts_but000,
       wa_partner TYPE ts_but000.
"----------------end-----------------"
"added by varun on 22.05.2020
TYPES: BEGIN OF ty_but0id,
         partner  TYPE but0id-partner,
         type     TYPE but0id-type,
         idnumber TYPE but0id-idnumber,
       END OF ty_but0id.
DATA: lt_but0id TYPE TABLE OF ty_but0id.

DATA: lv_incrt_gst    TYPE flag,  "added by varun on 07.01.2020 for gst and postal code validation
      lv_incrt_postal TYPE flag,
      ls_color        TYPE slis_specialcol_alv.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr , """vendor
                s_bukrs FOR lfb1-bukrs OBLIGATORY,   """company code
                s_accgr FOR lfa1-ktokk , """account group
                s_purorg FOR lfm1-ekorg,"""Purchasing Org.
                s_pan FOR lfa1-j_1ipanno ,
                s_gst FOR lfa1-stcd3 ,
                s_erdat FOR lfa1-erdat.
SELECTION-SCREEN END OF BLOCK blk1   .

SELECTION-SCREEN BEGIN OF BLOCK layout WITH FRAME TITLE TEXT-002.
PARAMETERS: p_vari LIKE disvariant-variant. " ALV Variant
SELECTION-SCREEN END OF BLOCK layout.

INITIALIZATION.
  PERFORM initialize_variant  .

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

*----------------------------------------------------------------------*
*         :   Start of Selection                                       *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fetch_data.

  PERFORM display_data.

END-OF-SELECTION.
  PERFORM alv_fieldcatlog.
  PERFORM display_alv_output.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_variant .
  g_save = 'A'.
  CLEAR g_variant.
  g_variant-report = sy-repid.
*  g_variant-variant = p_vari.
  gx_variant = g_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = g_save
    CHANGING
      cs_variant    = gx_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
  ENDIF.
ENDFORM.                    " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_for_variant .
  g_save = 'A'.
  g_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = g_variant
      i_save     = g_save
    IMPORTING
      e_exit     = g_exit
      es_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      p_vari = gx_variant-variant.
    ENDIF.
  ENDIF.
ENDFORM.                    " F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_data .
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

  SELECT *
  FROM lfa1
  INTO TABLE it_lfa1
  WHERE lifnr IN s_lifnr
  AND   ktokk IN s_accgr
  AND   j_1ipanno IN s_pan
  AND   stcd3 IN s_gst
  AND   erdat IN s_erdat       .


  IF NOT it_lfa1[] IS INITIAL ."and not s_bukrs is initial.


    "----------------mod1------------------------"
    SELECT partner_guid vendor FROM cvi_vend_link
       INTO TABLE it_link
       FOR ALL ENTRIES IN it_lfa1 WHERE vendor = it_lfa1-lifnr.
    IF it_link[] IS NOT INITIAL.
      SELECT partner partner_guid bpkind FROM but000
        INTO TABLE it_partner
        FOR ALL ENTRIES IN  it_link WHERE partner_guid = it_link-partner_guid.
      "added by varun on 22.05.2020
      IF sy-subrc EQ 0.
        SELECT partner,type,idnumber FROM but0id INTO TABLE @lt_but0id
                                     FOR ALL ENTRIES IN @it_partner
                                     WHERE partner EQ @it_partner-partner
                                     AND   type IN ( 'ZESIC', 'ZMSME' ).
      ENDIF.
    ENDIF.
    "----------------end------------------------"




    SELECT sperrfkt kurztext FROM tq04s INTO TABLE
       it_tq04s FOR ALL ENTRIES IN it_lfa1 WHERE sperrfkt EQ it_lfa1-sperq
                                           AND sprache = 'E'.

    SELECT lifnr
           bukrs
           pernr
           akont
           zterm
           frgrp
    FROM lfb1
    INTO TABLE it_lfb1
    FOR ALL ENTRIES IN it_lfa1
    WHERE lifnr = it_lfa1-lifnr
    AND   bukrs IN s_bukrs.

    IF it_lfa1[] IS INITIAL OR it_lfb1[] IS INITIAL.
      MESSAGE 'Data does not exits' TYPE 'I'.
      STOP.
    ENDIF.

    IF it_lfb1 IS NOT INITIAL.

      SELECT frgrp frgrt FROM vbwf07 INTO TABLE it_vbwf07 FOR ALL ENTRIES IN it_lfb1
                                                          WHERE frgrp = it_lfb1-frgrp
                                                          AND spras = 'E'.

      SELECT saknr txt50 FROM skat INTO TABLE it_skat FOR ALL ENTRIES IN it_lfb1 "IT_SKAT
                                                      WHERE saknr = it_lfb1-akont
                                                      AND   spras = 'E'.
    ENDIF.


    SELECT addrnumber
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

    FROM adrc
    INTO TABLE it_adrc
    FOR ALL ENTRIES IN it_lfa1
    WHERE addrnumber = it_lfa1-adrnr .

  ENDIF.

  IF NOT it_lfa1[] IS INITIAL."and not s_purorg is initial.
    SELECT lifnr
           ekorg
           telf1
           zterm
           inco1
           inco2
           kalsk
           webre
           lebre

    FROM lfm1
    INTO TABLE it_lfm1
    FOR ALL ENTRIES IN it_lfa1
    WHERE lifnr = it_lfa1-lifnr
    AND   ekorg IN s_purorg.

*select  PARNR
*        KUNNR
*        NAMEV
*        NAME1
*from  KNVK
*into table it_KNVK
*for all entries in it_lfb1
*where PARNR = it_lfb1-PeRNR.

  ENDIF.

  IF NOT it_lfa1[] IS INITIAL.
    SELECT *
    FROM lfbk INTO TABLE it_lfbk
    FOR ALL ENTRIES IN it_lfa1
    WHERE lifnr = it_lfa1-lifnr .

*{   DELETE         SBXK900270                                        1
*\    SELECT *
*\    FROM j_1imovend
*\    INTO TABLE it_j_1imovend
*\    FOR ALL ENTRIES IN it_lfa1
*\    WHERE lifnr = it_lfa1-lifnr .
*}   DELETE

    SELECT *
    FROM adr6
    INTO TABLE it_adr6
    FOR ALL ENTRIES IN it_lfa1
    WHERE addrnumber = it_lfa1-adrnr.

    SELECT *
    FROM adr2
    INTO TABLE it_adr2
    FOR ALL ENTRIES IN it_lfa1
    WHERE addrnumber = it_lfa1-adrnr
    AND   dft_receiv = 'X'.

    SELECT *
    FROM lfbk
    INTO TABLE it_lfbk
    FOR ALL ENTRIES IN it_lfa1
    WHERE lifnr = it_lfa1-lifnr.

  ENDIF.

  IF NOT it_lfbk[] IS INITIAL.
    SELECT *
    FROM bnka
    INTO TABLE it_bnka
    FOR ALL ENTRIES IN it_lfbk
    WHERE banks = it_lfbk-banks
    AND   bankl = it_lfbk-bankl.
  ENDIF.
ENDFORM.                    " FETCH_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .

  LOOP AT it_lfa1 INTO wa_lfa1.
    wa_final-lifnr = wa_lfa1-lifnr.

******start of addition made by varun on 07.01.2020 as said by punam on mail
    CLEAR: lv_incrt_postal,lv_incrt_gst.
    TRY.
        zcl_bupa_utilities=>validate_postal_code(
          EXPORTING
            iv_entity      = wa_final-lifnr " Customer/Vendor
          RECEIVING
            rv_valid       = lv_incrt_postal   " 'X' = Valid/'' = Invalid
        ).
        wa_final-check_postal = 'Valid Postal Code'.
      CATCH zcx_generic. " Generic Exception Class
        wa_final-check_postal = 'Incorrect Postal Code'.
    ENDTRY.
    CLEAR ls_color.
    ls_color-fieldname = 'CHECK_POSTAL'.
    ls_color-color-col = COND #( WHEN lv_incrt_postal IS INITIAL
                                 THEN 6 ELSE 5 ).
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    APPEND ls_color TO wa_final-color.

    TRY.
        zcl_bupa_utilities=>validate_gst_number(
          EXPORTING
            iv_entity     = wa_final-lifnr " Customer/Vendor
          RECEIVING
            rv_valid      = lv_incrt_gst " 'X' = Valid/'' = Invalid
        ).
        wa_final-check_gst_no = 'Valid GST Number'.
      CATCH zcx_generic. " Generic Exception Class
        wa_final-check_gst_no = 'Incorrect GST Number'.
    ENDTRY.

    CLEAR ls_color.
    ls_color-fieldname = 'CHECK_GST_NO'.
    ls_color-color-col = COND #( WHEN lv_incrt_gst IS INITIAL
                                 THEN 6 ELSE 5 ).
    ls_color-color-int = 1.
    ls_color-color-inv = 0.
    APPEND ls_color TO wa_final-color.

******End of addition made by varun on 07.01.2020 as said by punam on mail

    wa_final-name1 = wa_lfa1-name1.
    wa_final-name2 = wa_lfa1-name2.
    wa_final-adrnr = wa_lfa1-adrnr.
    wa_final-konzs = wa_lfa1-konzs. """"Corporate Group
    wa_final-city_ort01  = wa_lfa1-ort01.
    wa_final-ort02 = wa_lfa1-ort02.
    wa_final-regio = wa_lfa1-regio.
    "start of addition by varun on 12.11.19 as said by punam on mail
    IF wa_final-regio IS NOT INITIAL.
      SELECT SINGLE param2 FROM z6mma_params INTO wa_final-gst_regio
                                             WHERE progname EQ 'STATE_MAP'
                                             AND   param1   EQ wa_final-regio.
    ENDIF.
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
    READ TABLE it_link INTO wa_link WITH KEY vendor = wa_lfa1-lifnr.
    IF sy-subrc = 0.
      READ TABLE it_partner INTO wa_partner WITH KEY partner_guid = wa_link-partner_guid.
      IF sy-subrc = 0.
        wa_final-partner = wa_partner-partner.
        wa_final-bptype = wa_partner-bpkind.

        LOOP AT lt_but0id INTO DATA(ls_but0id) WHERE partner = wa_final-partner.
          IF ls_but0id-type EQ 'ZMSME'.
            wa_final-msme_no = ls_but0id-idnumber.
          ELSEIF ls_but0id-type EQ 'ZESIC'.
            wa_final-esic_no = ls_but0id-idnumber.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    "------------------end------------------------------"

    READ TABLE it_tq04s INTO wa_tq04s WITH KEY sperrfkt = wa_lfa1-sperq.

    IF sy-subrc = 0.
      wa_final-sdesc = wa_tq04s-kurztext.
    ENDIF.

    SELECT SINGLE bezei FROM t005u
    INTO wa_t005u
    WHERE spras = 'EN'
    AND land1 = 'IN'
    AND bland = wa_final-regio.

    wa_final-bezei  =   wa_t005u.

    READ TABLE it_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_lfa1-lifnr.
*                                         bukrs in s_bukrs.
    IF sy-subrc = 0 .
      wa_final-bukrs = wa_lfb1-bukrs.
      wa_final-zterm_act = wa_lfb1-zterm.
      SELECT SINGLE text1 FROM t052u
      INTO gv_text1_act
      WHERE spras = 'EN'
      AND   zterm = wa_final-zterm_act.
      wa_final-text1_desc_act = gv_text1_act .


      wa_final-frgrp = wa_lfb1-frgrp.
      wa_final-akont = wa_lfb1-akont.

      READ TABLE it_vbwf07 INTO wa_vbwf07 WITH KEY frgrp = wa_lfb1-frgrp.
      IF sy-subrc = 0.
        wa_final-frgrt = wa_vbwf07-frgrt.
      ENDIF.

      READ TABLE it_skat INTO wa_skat WITH KEY saknr = wa_lfb1-akont.
      IF sy-subrc = 0.
        wa_final-txt50 = wa_skat-txt50.
      ENDIF.

    ENDIF.





    READ TABLE it_lfm1 INTO wa_lfm1 WITH KEY lifnr = wa_lfa1-lifnr.
    IF sy-subrc = 0.
      wa_final-ekorg = wa_lfm1-ekorg.
      wa_final-zterm = wa_lfm1-zterm.
      wa_final-inco1 = wa_lfm1-inco1.
      wa_final-inco2 = wa_lfm1-inco2.
      wa_final-kalsk = wa_lfm1-kalsk.
      wa_final-webre = wa_lfm1-webre.

      IF wa_final-webre EQ 'X'.
        wa_final-indi = 'YES'.
      ELSE.
        wa_final-indi = 'NO'.
      ENDIF.

      wa_final-lebre = wa_lfm1-lebre.

      IF wa_final-webre EQ 'X'.
        wa_final-indi1 = 'YES'.
      ELSE.
        wa_final-indi1 = 'NO'.
      ENDIF.
      SELECT SINGLE text1 FROM t052u
      INTO gv_text1_pur
      WHERE spras = 'EN'
     AND   zterm = wa_final-zterm.
      wa_final-text1_desc_pur = gv_text1_pur .
    ENDIF.
    READ TABLE it_adrc INTO wa_adrc WITH KEY addrnumber = wa_lfa1-adrnr.
    IF sy-subrc = 0.
      wa_final-name_co    = wa_adrc-name_co.
      wa_final-str_suppl1 = wa_adrc-str_suppl1.
      wa_final-str_suppl2 = wa_adrc-str_suppl2.
      wa_final-street     = wa_adrc-street.
      wa_final-str_suppl3 = wa_adrc-str_suppl3.
      wa_final-location   = wa_adrc-location.
      wa_final-post_code1 = wa_adrc-post_code1.
      wa_final-country = wa_adrc-country.
    ENDIF.
    READ TABLE it_adr6 INTO wa_adr6 WITH KEY addrnumber = wa_lfa1-adrnr.
    IF sy-subrc = 0 .
      wa_final-smtp_addr  = wa_adr6-smtp_addr.
    ENDIF.
    READ TABLE it_adr2 INTO wa_adr2 WITH KEY addrnumber = wa_lfa1-adrnr.
    IF sy-subrc = 0 .
      wa_final-tel_number = wa_adr2-tel_number.
      wa_final-telnr_call = wa_adr2-telnr_call.
    ENDIF.
    READ TABLE it_lfbk INTO wa_lfbk WITH KEY lifnr = wa_lfa1-lifnr.
    IF sy-subrc = 0 .
      wa_final-bankl        = wa_lfbk-bankl.
      wa_final-bankn        = wa_lfbk-bankn.
      wa_final-koinh        = wa_lfbk-koinh .
      wa_final-ebpp_accname = wa_lfbk-ebpp_accname.


      READ TABLE it_bnka INTO wa_bnka WITH KEY banks = wa_lfbk-banks
                                               bankl = wa_lfbk-bankl.
      IF sy-subrc = 0 .

        wa_final-banka = wa_bnka-banka.
        wa_final-stras = wa_bnka-stras.
        wa_final-ort01 = wa_bnka-ort01.
        wa_final-brnch = wa_bnka-brnch.
        wa_final-swift = wa_bnka-swift.
      ENDIF.
    ENDIF.

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

    SELECT SINGLE j_1iinddes
             FROM j_1itaxind
             INTO wa_j_1iinddes
            WHERE j_1iindtax EQ wa_final-j_1iexcive.

    wa_final-indi_desc = wa_j_1iinddes.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = 'LFA1'
        fieldname      = 'J_1IVTYP'
        langu          = sy-langu
        lfieldname     = 'J_1IVTYP'
      TABLES
        fixed_values   = it_fixed_values
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE it_fixed_values INTO wa_fixed_values WITH KEY low = wa_final-j_1ivtyp.
    IF sy-subrc = 0 .
      wa_final-typ_ven_desc = wa_fixed_values-ddtext.
    ENDIF.

*}   INSERT
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_final-lifnr
      IMPORTING
        output = wa_final-lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_final-adrnr
      IMPORTING
        output = wa_final-adrnr.

    AUTHORITY-CHECK OBJECT 'M_BEST_EKO'
    ID 'EKORG' FIELD wa_final-ekorg.
    IF sy-subrc = 0.
      AUTHORITY-CHECK OBJECT 'F_LFA1_BUK'
      ID 'BUKRS' FIELD wa_final-bukrs.
      IF sy-subrc = 0 .
        APPEND wa_final TO it_final.
      ENDIF.
    ENDIF.
*{   REPLACE        SBXK900270                                        3
*\    CLEAR : wa_final,wa_lfa1,wa_lfb1, wa_skat, wa_vbwf07,lfm1,wa_adrc,wa_adr2,wa_adr6,wa_lfbk,wa_bnka,wa_t005u,wa_j_1imovend,wa_fixed_values,wa_j_1iinddes,gv_text1_pur,gv_text1_act.
    CLEAR : wa_final,wa_lfa1,wa_lfb1, wa_skat, wa_vbwf07,lfm1,wa_adrc,wa_adr2,wa_adr6,wa_lfbk,wa_bnka,wa_t005u,
            wa_fixed_values,wa_j_1iinddes,gv_text1_pur,gv_text1_act.
*}   REPLACE

  ENDLOOP.

  IF  it_final[] IS INITIAL  .
    MESSAGE 'No Authorization' TYPE 'E'.
  ENDIF.

  IF NOT s_bukrs IS INITIAL.
    DELETE it_final WHERE bukrs NOT IN s_bukrs .
  ENDIF.

*  IF NOT s_purorg IS INITIAL.                              " Finance vendors for which purchase org is not maintained is not displaying in output
*    DELETE it_final WHERE ekorg NOT IN s_purorg .          " as suggested by Mr.Venu this validation is removed - Pradeep K
*  ENDIF.

  IF it_final[] IS INITIAL.
    MESSAGE 'Data Does not Exits' TYPE 'I'.
    STOP.
  ENDIF.

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

ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDCATLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_fieldcatlog .
  DATA : cnt TYPE sy-index.
  CLEAR : wa_fieldcatalog,
          it_fieldcatalog[].


  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'PARTNER'.
  wa_fieldcatalog-seltext_m   = 'Partner'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'LIFNR'.
  wa_fieldcatalog-seltext_m   = 'Vendor'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'NAME1'.
  wa_fieldcatalog-seltext_m   = 'NAME1'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'NAME2'.
  wa_fieldcatalog-seltext_m   = 'NAME2'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'NAME_CO'.
  wa_fieldcatalog-seltext_m   = 'C/o Name'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'ADRNR'.
  wa_fieldcatalog-seltext_m   = 'Address Key'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'BUKRS'.
  wa_fieldcatalog-seltext_m   = 'Company Code'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'EKORG'.
  wa_fieldcatalog-seltext_m   = 'Purchse Organization'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'STR_SUPPL1'.
  wa_fieldcatalog-seltext_m   = 'Street 1'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'STR_SUPPL2'.
  wa_fieldcatalog-seltext_m   = 'Street 2'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'STREET'.
  wa_fieldcatalog-seltext_m   = 'Street'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'STR_SUPPL3'.
  wa_fieldcatalog-seltext_m   = 'Street4'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'LOCATION'.
  wa_fieldcatalog-seltext_m   = 'Street 5'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'ORT02'.
  wa_fieldcatalog-seltext_m   = 'District'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'CITY_ORT01'.
  wa_fieldcatalog-seltext_m   = 'City'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'POST_CODE1'.
  wa_fieldcatalog-seltext_m   = 'Postl Code'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'CHECK_POSTAL'.
  wa_fieldcatalog-seltext_m   = 'Postal Code Log'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'REGIO'.
  wa_fieldcatalog-seltext_m   = 'Region'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'BEZEI'.
  wa_fieldcatalog-seltext_m   = 'Region'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'GST_REGIO'.
  wa_fieldcatalog-seltext_m   = 'GST Region'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'COUNTRY'.
  wa_fieldcatalog-seltext_m   = 'country'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

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

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'TELF1'.
  wa_fieldcatalog-seltext_m   = 'Telephone'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'TELF2'.
  wa_fieldcatalog-seltext_m   = 'Mobile Phone'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.


  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'SMTP_ADDR'.
  wa_fieldcatalog-seltext_m   = 'E-Mail Address'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'TELFX'.
  wa_fieldcatalog-seltext_m   = 'FAX Number'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'KTOKK'.
  wa_fieldcatalog-seltext_m   = 'Group'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'KUNNR'.
  wa_fieldcatalog-seltext_m   = 'Customer'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'WERKS'.
  wa_fieldcatalog-seltext_m   = 'Plant'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'BANKL'.
  wa_fieldcatalog-seltext_m   = 'Bank Key'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'BANKA'.
  wa_fieldcatalog-seltext_m   = 'Bank name'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'STRAS'.
  wa_fieldcatalog-seltext_m   = 'Bank address'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'ORT01'.
  wa_fieldcatalog-seltext_m   = 'Bank city'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'BRNCH'.
  wa_fieldcatalog-seltext_m   = 'Bank Banch name'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'SWIFT'.
  wa_fieldcatalog-seltext_m   = 'Swift code'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'BANKN'.
  wa_fieldcatalog-seltext_m   = 'Bank name'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'KOINH'.
  wa_fieldcatalog-seltext_m   = 'Account holder'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'EBPP_ACCNAME'.
  wa_fieldcatalog-seltext_m   = 'Account holder name'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXCD'.
  wa_fieldcatalog-seltext_m   = 'ECC No.'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXRN'.
  wa_fieldcatalog-seltext_m   = 'Excise Reg. No.'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXRG' .
  wa_fieldcatalog-seltext_m   = 'Excise Range'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXDI'.
  wa_fieldcatalog-seltext_m   = 'Excise Division'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXCO'.
  wa_fieldcatalog-seltext_m   = 'Excise Commissionerate'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1ICSTNO' .
  wa_fieldcatalog-seltext_m   = 'CST No'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1ILSTNO' .
  wa_fieldcatalog-seltext_m   = 'LST No'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IPANNO'.
  wa_fieldcatalog-seltext_m   = 'PAN No.'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IEXCIVE' .
  wa_fieldcatalog-seltext_m   = 'Excise Indicator'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'INDI_DESC' .
  wa_fieldcatalog-seltext_m   = 'Exc.Ind.desc.'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1IVTYP'.
  wa_fieldcatalog-seltext_m   = 'Type of Vendor'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'TYP_VEN_DESC' .
  wa_fieldcatalog-seltext_m   = 'Type Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'J_1ISERN' .
  wa_fieldcatalog-seltext_m   = 'Service Tax Reg.No.'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'KONZS' .
  wa_fieldcatalog-seltext_m   = 'Corporate Group'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'INCO1' .
  wa_fieldcatalog-seltext_m   = 'Incoterms'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'INCO2' .
  wa_fieldcatalog-seltext_m   = 'Incoterms Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'KALSK' .
  wa_fieldcatalog-seltext_m   = 'Vendor schema'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'INDI' .
  wa_fieldcatalog-seltext_m   = 'GR-Base'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'INDI1' .
  wa_fieldcatalog-seltext_m   = 'Service-Base'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'ZTERM' .
  wa_fieldcatalog-seltext_m   = 'Terms of Pay.(Purchase)'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'TEXT1_DESC_PUR' .
  wa_fieldcatalog-seltext_m   = 'Terms of Pay.Pur.desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'ZTERM_ACT' .
  wa_fieldcatalog-seltext_m   = 'Payt Terms(ACT)'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'TEXT1_DESC_ACT' .
  wa_fieldcatalog-seltext_m   = 'Payt Terms(ACT) Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.


  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'SPERQ' .
  wa_fieldcatalog-seltext_m   = 'Block Function'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'SDESC' .
  wa_fieldcatalog-seltext_m   = 'Block Function Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

*--
  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'AKONT' .
  wa_fieldcatalog-seltext_m   = 'Recon Account.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'TXT50' .
  wa_fieldcatalog-seltext_m   = 'Recon A/c Descrition.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  wa_fieldcatalog-outputlen = 50.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'FRGRP' .
  wa_fieldcatalog-seltext_m   = 'Release group'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'FRGRT' .
  wa_fieldcatalog-seltext_m   = 'Release group Desc.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

** added by NK on 05.07.2017
  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'STCD3' .
  wa_fieldcatalog-seltext_s   = 'GST No.'.
  wa_fieldcatalog-seltext_m   = 'Tax Number 3 '.
  wa_fieldcatalog-seltext_l   = 'GST No. Tax Number 3'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'CHECK_GST_NO'.
  wa_fieldcatalog-seltext_m   = 'GST No. Log'.
  wa_fieldcatalog-col_pos     = cnt.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'VEN_CLASS' .
  wa_fieldcatalog-seltext_m   = 'GST Ven Class.'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'BPTYPE' .
  wa_fieldcatalog-seltext_m   = 'BP Type'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.


  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'ERDAT' .
  wa_fieldcatalog-seltext_m   = 'Creation Date'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'ESIC_NO' .
  wa_fieldcatalog-seltext_m   = 'ESIC Number'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

  ADD 1 TO cnt.
  wa_fieldcatalog-fieldname   = 'MSME_NO' .
  wa_fieldcatalog-seltext_m   = 'MSME Number'.
  wa_fieldcatalog-col_pos     = cnt.
*  wa_fieldcatalog-no_out      = 'X'.
  APPEND wa_fieldcatalog TO it_fieldcatalog.
  CLEAR  wa_fieldcatalog.

ENDFORM.                    " ALV_FIELDCATLOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_output .

  IF NOT it_final[] IS INITIAL.

    gd_layout-zebra = 'X'.
    gd_layout-colwidth_optimize = 'X'.
    gd_layout-coltab_fieldname = 'COLOR'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
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
      TABLES
        t_outtab           = it_final[]
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " DISPLAY_ALV_OUTPUT
