*&---------------------------------------------------------------------*
*& Report  ZMM_PURCH_REGIS_REPORT
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Transaction            : ZMM084
*& Creation Date          : Monday, June 18, 2018 14:13:53
*& Author                 : 6010859 - SaurabhK
*& Functional             : VG/PS
*& Requested/Approved By  : Mr Bhupesh Adoni
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK932460
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&---------------------------------------------------------------------*
*& 1. Purchase tax register
*&---------------------------------------------------------------------*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Monday, June 18, 2018 16:12:48
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Self
*& Rev. Request#              : IRDK932529, IRDK932531, IRDK932533, IRDK932535, IRDK932537, IRDK932539, IRDK932547
*& Rev. Description           : Pre-Production Fixes
*&---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Thursday, June 28, 2018 11:49:09
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Bhupesh Adoni
*& Rev. Request#              : IRDK932665
*& Rev. Description           : MM: S_K: ZMM084: Add payment reference: 28.06.2018
*&---------------------------------------------------------------------*
*& Revision #                 : 03
*& Rev. Date                  : Thursday, August 02, 2018 17:12:00
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Bhupesh Adoni
*& Rev. Request#              : IRDK933010
*& Rev. Description           : MM: S_K: ZMM084: Add few columns, move to PRD: 2.8.18
*&---------------------------------------------------------------------*
*& Revision #                 : 04
*& Rev. Date                  : Wednesday, September 12, 2018 14:37:58
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Bhupesh Adoni
*& Rev. Request#              : IRDK933390, IRDK933392, IRDK933385
*& Rev. Description           : MM: S_K: ZMM084: Add new fields/fixes: 12.09.2018
*&---------------------------------------------------------------------*
*& Revision #                 : 05
*& Rev. Date                  : Wednesday, January 16, 2019 19:15:08
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Bhupesh Adoni/Sujay Gokhale
*& Rev. Request#              : IHDK900167, IHDK900257
*& Rev. Description           : 1. Performance improvents
*&                              2. State code for unregeistered vendors and impoprt vendors(IM)
*&                              3. Option to include/exclude empl vendor transaction
*&---------------------------------------------------------------------*
report zmm_purch_regis_report.

* ---- global data and selection screen ---- *
tables: bkpf, bseg, bset.

selection-screen begin of block sel with frame title text-sel.
select-options :  s_bukrs for bkpf-bukrs obligatory,
                  s_belnr for bkpf-belnr,
                  s_budat for bkpf-budat obligatory,
                  s_gjahr for bkpf-gjahr obligatory,
                  s_bupla for bseg-bupla.
selection-screen skip.
parameters: c_empl as checkbox.
selection-screen end of block sel.

selection-screen begin of block lay with frame title text-lay.
parameters: p_var type disvariant-variant.
selection-screen end of block lay.

* ---- Local class definitions ---- *
class lcl_sel_screen_events definition.
  public section.
    methods: variant_f4_selection, check_variant_existence, variant_init.
endclass.

class lcl_sel_screen_events implementation.
  method variant_f4_selection.
    data: ls_layout type salv_s_layout_info,
          ls_key    type salv_s_layout_key.

    clear: ls_key, ls_layout.
    ls_key-report = sy-repid.

    ls_layout = cl_salv_layout_service=>f4_layouts(
                  s_key    = ls_key
                  restrict = if_salv_c_layout=>restrict_none  ).

    p_var = ls_layout-layout.
  endmethod.

  method check_variant_existence.
    check sy-ucomm eq 'ONLI'.
    data: lt_layout type salv_t_layout_info,
          ls_layout type salv_s_layout_info,
          ls_key    type salv_s_layout_key.

    clear ls_key.
    ls_key-report = sy-repid.
    if not p_var is initial.
      refresh lt_layout.
      lt_layout = cl_salv_layout_service=>get_layouts( s_key = ls_key ).
      if lt_layout is not initial.
        read table lt_layout into ls_layout with key layout = p_var.
        if sy-subrc <> 0.
          message 'No such variant' type 'S' display like 'E'.
          leave screen.
        endif.
      else.
        message 'No such variant' type 'S' display like 'E'.
        leave screen.
      endif.
    else.
      variant_init( ).
    endif.
  endmethod.

  method variant_init.
    data: value  type salv_s_layout_info,
          ls_key type salv_s_layout_key.

    clear: value, ls_key.
    ls_key-report = sy-repid.
    value = cl_salv_layout_service=>get_default_layout( s_key = ls_key ).

    if value is not initial.
      p_var = value-layout.
    endif.
  endmethod.
endclass.

class lcl_application definition.
  public section.
    methods: process.

  protected section.

  private section.
    data: begin of out,
            ccd type bseg-bukrs,    " company code
            doc type bseg-belnr,    " document no
            fyr type bseg-gjahr,    " fiscal year
            pdt type bkpf-budat,    " posting date
            ddt type bkpf-bldat,    " document date IRDK933390
            dty type bkpf-blart,    " document type IRDK933390
            ref type bkpf-xblnr,    " reference
            txc type bset-mwskz,    " tax code
            tal type bset-hwbas,    " tax base amount in local currency
            dcr type bkpf-waers,    " document currency
            tad type bset-fwbas,    " tax base amount in document currency
            cgc type bset-ktosl,    " central gst tax condition type
            cgl type bset-hwste,    " central gst in local currency
            cgd type bset-fwste,    " central gst in document currency
            cgr type bset-kbetr,    " central gst rate
            cgg type bset-hkont,    " central gst gl account
            cra type flag,          " central gst RCM applicable  IRDK933390
            sgc type bset-ktosl,    " state gst tax condition type
            sgl type bset-hwste,    " state gst in local currency
            sgd type bset-fwste,    " state gst in document currency
            sgr type bset-kbetr,    " state gst rate
            sgg type bset-hkont,    " state gst gl account
            sra type flag,          " state gst RCM applicable  IRDK933390
            igc type bset-ktosl,    " inter-state gst tax condition type
            igl type bset-hwste,    " inter-state gst in local currency
            igd type bset-fwste,    " inter-state gst in document currency
            igr type bset-kbetr,    " inter-state gst rate
            igg type bset-hkont,    " inter-state gst gl account
            ira type flag,          " inter-state gst RCM applicable  IRDK933390
            ttx type bset-hwste,    " total tax in local currency
            mat type bseg-matnr,    " material code
            mds type makt-maktx,    " material description
            bpl type bseg-bupla,    " business place/place of supply
            bpn type kna1-name1,    " place of supply(name) " IRDK933010
            bgs type kna1-stcd3,    " gst no of place of supply(indofil gst no) " IRDK933010
            rst type kna1-regio,    " receiving state code  IRDK933390
            hsn type bseg-hsn_sac,  " hsn/sac code
            hds type text100,       " hsn/sac description,
            qty type bseg-menge,    " invoice quantity
            uom type bseg-meins,    " unit of measure
            tam type bseg-dmbtr,    " total amount in local currency
            vdr type bseg-lifnr,    " vendor
            vds type lfa1-name1,    " vendor description
            vgs type lfa1-stcd3,    " vendor gst no " IRDK933010
            vcl type lfa1-ven_class, " vendor classification for GST
            sst type kna1-regio,    " supplying state code  IRDK933390
            cld type bseg-augdt,    " clearing date
            dif type verzn,         " diff in no. of days = invoice date - payment date
            ite type char2,         " eligibility of ITC  IRDK933390
            itc type flag,          " ITC claimed?  IRDK933390
            prf type bseg-kidno,    " payment reference " IRDK932665
            asn type bseg-zuonr,    " assignment
            txt type bseg-sgtxt,    " text
          end of out,
          outtab like standard table of out,

          begin of acc_hdr,
            bukrs type bkpf-bukrs,
            belnr type bkpf-belnr,
            gjahr type bkpf-gjahr,
            blart type bkpf-blart,
            bldat type bkpf-bldat,
            budat type bkpf-budat,
            xblnr type bkpf-xblnr,
            bktxt type bkpf-bktxt,
            waers type bkpf-waers,
          end of acc_hdr,
          acc_hdrs like standard table of acc_hdr,

          begin of acc_itm,
            bukrs   type bseg-bukrs,
            belnr   type bseg-belnr,
            gjahr   type bseg-gjahr,
            buzei   type bseg-buzei,
            buzid   type bseg-buzid,
            augbl   type bseg-augbl,
            augdt   type bseg-augdt,
            auggj   type bseg-auggj,
            bschl   type bseg-bschl,
            koart   type bseg-koart,
            shkzg   type bseg-shkzg,
            mwskz   type bseg-mwskz,
            ktosl   type bseg-ktosl,
            dmbtr   type bseg-dmbtr,
            wrbtr   type bseg-wrbtr,
            hwbas   type bseg-hwbas,
            fwbas   type bseg-fwbas,
            txgrp   type bseg-txgrp,
            mwart   type bseg-mwart,
            zuonr   type bseg-zuonr,
            sgtxt   type bseg-sgtxt,
            hkont   type bseg-hkont,
            lifnr   type bseg-lifnr,
            matnr   type bseg-matnr,
            werks   type bseg-werks,
            menge   type bseg-menge,
            meins   type bseg-meins,
            ebeln   type bseg-ebeln,
            ebelp   type bseg-ebelp,
            bupla   type bseg-bupla,
            secco   type bseg-secco,
            taxps   type bseg-taxps,
            hsn_sac type bseg-hsn_sac,
          end of acc_itm,
          acc_itms like standard table of acc_itm,

          begin of tax_itm,
            bukrs type bset-bukrs,
            belnr type bset-belnr,
            gjahr type bset-gjahr,
            buzei type bset-buzei,
            mwskz type bset-mwskz,
            hkont type bset-hkont,
            txgrp type bset-txgrp,
            shkzg type bset-shkzg,
            hwbas type bset-hwbas,
            fwbas type bset-fwbas,
            hwste type bset-hwste,
            fwste type bset-fwste,
            ktosl type bset-ktosl,
            kbetr type bset-kbetr,
            kschl type bset-kschl,
            bupla type bset-bupla,
            taxps type bset-taxps,
          end of tax_itm,
          tax_itms     like standard table of tax_itm,

          ip_tax_code  type t007a,
          ip_tax_codes like standard table of ip_tax_code,

          tax_gl       type j_1it030k,
          tax_gls      like standard table of tax_gl,

          tax_gl_desc  type skat,
          tax_gl_descs like standard table of tax_gl_desc.

    methods: fetch_acc_hdr, fetch_acc_itm, fetch_tax_dtls, fetch_othr_dtls, construct_outtab, display_alv.

endclass.

class lcl_application implementation.
  method process.
    fetch_acc_hdr( ).
    if acc_hdrs is not initial.
      fetch_acc_itm( ).
      if acc_itms is not initial.
        fetch_othr_dtls( ).
        fetch_tax_dtls( ).
        if tax_itms is not initial.
          construct_outtab( ).
          if outtab is not initial.
            display_alv( ).
          else.
            write: / 'No data found'.
            return.
          endif.
        else.
          write: / 'No data found'.
          return.
        endif.
      else.
        write: / 'No data found'.
        return.
      endif.
    else.
      write: / 'No data found'.
      return.
    endif.
  endmethod.

  method fetch_acc_hdr.
    refresh acc_hdrs.
    select bukrs
           belnr
           gjahr
           blart
           bldat
           budat
           xblnr
           bktxt
           waers
      from bkpf
      into table acc_hdrs
      where bukrs in s_bukrs
      and   belnr in s_belnr
      and   gjahr in s_gjahr
      and   budat in s_budat.
  endmethod.

  method fetch_acc_itm.
    check acc_hdrs is not initial.
    refresh acc_itms.
    select bukrs
           belnr
           gjahr
           buzei
           buzid
           augbl
           augdt
           auggj
           bschl
           koart
           shkzg
           mwskz
           ktosl
           dmbtr
           wrbtr
           hwbas
           fwbas
           txgrp
           mwart
           zuonr
           sgtxt
           hkont
           lifnr
           matnr
           werks
           menge
           meins
           ebeln
           ebelp
           bupla
           secco
           taxps
           hsn_sac
      from bseg
      into table acc_itms
      for all entries in acc_hdrs
      where bukrs = acc_hdrs-bukrs
      and belnr = acc_hdrs-belnr
      and gjahr = acc_hdrs-gjahr
      and bupla in s_bupla.
  endmethod.

  method fetch_tax_dtls.
    check acc_itms is not initial.
* -- performance optimisation -- *
    data: r_tax_gl type range of skat-saknr,
          r_tax_cd type range of t007a-mwskz.
*--------------------------------------------------------------------*
    " filter GST specific GL codes based desc(already filtered during selection)
    refresh: r_tax_gl.
    clear tax_gl_desc.
    loop at tax_gl_descs into tax_gl_desc.
      append initial line to r_tax_gl assigning field-symbol(<tax_gl>).
      if <tax_gl> is assigned.
        <tax_gl>-sign = 'I'.
        <tax_gl>-option = 'EQ'.
        <tax_gl>-low = tax_gl_desc-saknr.
      endif.
      clear tax_gl_desc.
    endloop.

    delete tax_gls where konth not in r_tax_gl.

    sort tax_gls by mwskz.
    delete adjacent duplicates from tax_gls comparing mwskz.
*--------------------------------------------------------------------*
    " filter GST specific input tax codes based on GL's(filtered above)
    refresh r_tax_cd.
    clear tax_gl.
    loop at tax_gls into tax_gl.
      append initial line to r_tax_cd assigning field-symbol(<tax_cd>).
      if <tax_cd> is assigned.
        <tax_cd>-sign = 'I'.
        <tax_cd>-option = 'EQ'.
        <tax_cd>-low = tax_gl-mwskz.
      endif.
      clear tax_gl.
    endloop.

    delete ip_tax_codes where mwskz not in r_tax_cd.
*--------------------------------------------------------------------*
    refresh tax_itms.
    select bukrs
           belnr
           gjahr
           buzei
           mwskz
           hkont
           txgrp
           shkzg
           hwbas
           fwbas
           hwste
           fwste
           ktosl
           kbetr
           kschl
           bupla
           taxps
      from bset
      into table tax_itms
      for all entries in acc_itms
      where bukrs = acc_itms-bukrs
      and   belnr = acc_itms-belnr
      and   gjahr = acc_itms-gjahr
      and   taxps = acc_itms-taxps
      and   txgrp = acc_itms-txgrp
      and   bupla in s_bupla
      and   mwskz in r_tax_cd
      and   hkont in r_tax_gl.
  endmethod.

  method fetch_othr_dtls.
    " fetch input tax codes
    refresh ip_tax_codes.
    select *
      from t007a
      into table ip_tax_codes
      where kalsm = 'ZTXINN'
      and   mwart = 'V'.      " V - Input tax, A - Output tax
*--------------------------------------------------------------------*
    " fetch gl's of input tax codes
    check ip_tax_codes is not initial.
    refresh tax_gls.
    select *
      from j_1it030k
      into table tax_gls
      for all entries in ip_tax_codes
      where ktopl = '1000'
      and   mwskz = ip_tax_codes-mwskz.
*--------------------------------------------------------------------*
    " fetch gl desc
    check tax_gls is not initial.
    refresh tax_gl_descs.
    select *
      from skat
      into table tax_gl_descs
      for all entries in tax_gls
      where spras = 'E'
      and   ktopl = '1000'
      and   saknr = tax_gls-konth
      and   txt20 like '%GST%'.   " fetch only GST specific GL's
  endmethod.

  method construct_outtab.
    " IHDK900841
    clear acc_itm.
    loop at acc_itms into acc_itm where lifnr is not initial.
      data(lv_proceed) = abap_true.
      clear acc_itm.
      exit.
    endloop.
    check tax_itms is not initial and lv_proceed eq abap_true.  " further processing makes sense if there are atleast a few vendor transactions
    " End IHDK900841
    refresh outtab.
    " sorted for binary search
    sort: acc_hdrs by bukrs belnr gjahr,
          acc_itms by bukrs belnr gjahr txgrp taxps buzei,
          tax_itms by bukrs belnr gjahr txgrp taxps buzei,
          tax_gl_descs by saknr.

* -- performance optimisation -- *
    " place of supply GST no.
    select kunnr, name1, stcd3
      from kna1
      into table @data(lt_kna1)
      where kunnr like 'C%'
      order by kunnr.
*--------------------------------------------------------------------*
    " material description
    data: begin of ls_mat_desc,
            matnr type makt-matnr,
            maktx type makt-maktx,
          end of ls_mat_desc,
          lt_mat_desc like standard table of ls_mat_desc with key primary_key components matnr.

    refresh: lt_mat_desc.
    move-corresponding acc_itms to lt_mat_desc.
    sort lt_mat_desc.
    delete adjacent duplicates from lt_mat_desc comparing matnr.
    delete lt_mat_desc where matnr is initial.

    if lt_mat_desc is not initial.
      select matnr maktx
        from makt
        into table lt_mat_desc
        for all entries in lt_mat_desc
        where matnr eq lt_mat_desc-matnr
        and   spras eq 'E'.

      sort lt_mat_desc by matnr.
    endif.
*--------------------------------------------------------------------*
    " hsn/sac code description
    data: begin of ls_hsn_desc,
            hsn_sac type t604n-steuc,
            text1   type t604n-text1,
          end of ls_hsn_desc,
          lt_hsn_desc like standard table of ls_hsn_desc with key primary_key components hsn_sac,

          begin of ls_sac_desc,
            hsn_sac  type j_1ichidtx-j_1ichid,
            j_1icht1 type j_1ichidtx-j_1icht1,
          end of ls_sac_desc,
          lt_sac_desc like standard table of ls_sac_desc with key primary_key components hsn_sac.

    refresh: lt_hsn_desc, lt_sac_desc.
    move-corresponding: acc_itms to lt_hsn_desc, acc_itms to lt_sac_desc.
    sort: lt_hsn_desc, lt_sac_desc.
    delete adjacent duplicates from lt_hsn_desc comparing hsn_sac.
    delete adjacent duplicates from lt_sac_desc comparing hsn_sac.

    delete: lt_hsn_desc where hsn_sac is initial,
            lt_sac_desc where hsn_sac is initial.

    if lt_hsn_desc is not initial.
      select steuc text1
        from t604n
        into table lt_hsn_desc
        for all entries in lt_hsn_desc
        where steuc = lt_hsn_desc-hsn_sac
        and   spras = 'E'
        and   land1 = 'IN'.

      select j_1ichid j_1icht1
        from j_1ichidtx
        into table lt_sac_desc
        for all entries in lt_sac_desc
        where j_1ichid = lt_sac_desc-hsn_sac
        and   langu = 'E'.

      sort: lt_hsn_desc by hsn_sac, lt_sac_desc by hsn_sac.
    endif.
*--------------------------------------------------------------------*
    " payment reference no/clearing documents
    data(acc_itms_cld) = acc_itms[].
    delete acc_itms_cld where augbl is initial or koart ne 'S'.
    sort acc_itms_cld by bukrs augbl auggj.
    delete adjacent duplicates from acc_itms_cld comparing bukrs augbl auggj.

    if acc_itms_cld is not initial. " IHDK900841
      select bukrs, belnr, gjahr, kidno
        from bseg
        into table @data(lt_cld)
        for all entries in @acc_itms_cld
        where bukrs = @acc_itms_cld-bukrs
        and   belnr = @acc_itms_cld-augbl
        and   gjahr = @acc_itms_cld-auggj.

      sort lt_cld by bukrs belnr gjahr.
    endif.
*--------------------------------------------------------------------*
    " vendor GST details
    data: begin of ls_lfa1,
            lifnr     type lfa1-lifnr,
            name1     type lfa1-name1,
            stcd3     type lfa1-stcd3,
            regio     type lfa1-regio,
            ktokk     type lfa1-ktokk,  " IHDK900257: to detect foreign vendors
            ven_class type lfa1-ven_class,  " IHDK900841
          end of ls_lfa1,
          lt_lfa1 like standard table of ls_lfa1 with key primary_key components lifnr.

    refresh lt_lfa1.
    move-corresponding acc_itms to lt_lfa1.
    sort lt_lfa1 by lifnr.
    delete adjacent duplicates from lt_lfa1 comparing lifnr.
    delete lt_lfa1 where lifnr is initial.

    if lt_lfa1 is not initial.  " IHDK900841
      select lifnr name1 stcd3 regio ktokk ven_class  " IHDK900257; IHDK900841 - ven_class
        from lfa1
        into table lt_lfa1
        for all entries in lt_lfa1
        where lifnr = lt_lfa1-lifnr.

      sort lt_lfa1 by lifnr.
    endif.
*--------------------------------------------------------------------*
    " sap state code to GST state code mapping
    select param1, param2
      from z6mma_params
      into table @data(lt_state_code)
      where progname = 'STATE_MAP'.

    sort lt_state_code by param1.
*--------------------------------------------------------------------*
    data(tax_itms_copy) = tax_itms[].
    delete adjacent duplicates from tax_itms comparing bukrs belnr gjahr txgrp taxps. "( already sorted by these fields )
    clear: tax_itm, out.
    loop at tax_itms into tax_itm.  " at this point tax items table only contains GST specific input tax code lines
      clear: out.
      loop at tax_itms_copy into data(tax_itm_copy) where bukrs = tax_itm-bukrs
                                                    and   belnr = tax_itm-belnr
                                                    and   gjahr = tax_itm-gjahr
                                                    and   taxps = tax_itm-taxps
                                                    and   txgrp = tax_itm-txgrp.

        " IHDK902089
        if tax_itm_copy-shkzg = 'H'.  " Credit
          tax_itm_copy-hwste = tax_itm_copy-hwste * -1.
          tax_itm_copy-fwste = tax_itm_copy-fwste * -1.
          tax_itm_copy-hwbas = tax_itm_copy-hwbas * -1.
          tax_itm_copy-fwbas = tax_itm_copy-fwbas * -1.
        endif.

        clear tax_gl_desc.
        read table tax_gl_descs into tax_gl_desc with key saknr = tax_itm_copy-hkont binary search. " get description of the gl
        if sy-subrc = 0.
          " determine what kind of tax it is, based on gl description
          if to_upper( tax_gl_desc-txt20 ) cs 'CGST'. " central gst
            if to_upper( tax_gl_desc-txt20 ) cs 'RCM'.
              out-cra = abap_true.
              data(rcm_flg) = abap_true.
            else.
              out-cgc = tax_itm_copy-ktosl.
              out-cgl = tax_itm_copy-hwste.
              out-cgd = tax_itm_copy-fwste.
              out-cgr = tax_itm_copy-kbetr / 10.
              out-cgg = tax_itm_copy-hkont.
              data(append_flg) = abap_true.
            endif.
          endif.

          if to_upper( tax_gl_desc-txt20 ) cs 'SGST'. " state gst
            if to_upper( tax_gl_desc-txt20 ) cs 'RCM'.
              out-sra = abap_true.
              rcm_flg = abap_true.
            else.
              out-sgc = tax_itm_copy-ktosl.
              out-sgl = tax_itm_copy-hwste.
              out-sgd = tax_itm_copy-fwste.
              out-sgr = tax_itm_copy-kbetr / 10.
              out-sgg = tax_itm_copy-hkont.
              append_flg = abap_true.
            endif.
          endif.

          if to_upper( tax_gl_desc-txt20 ) cs 'IGST'. " interstate gst
            if to_upper( tax_gl_desc-txt20 ) cs 'RCM'.
              out-ira = abap_true.
              rcm_flg = abap_true.
            else.
              out-igc = tax_itm_copy-ktosl.
              out-igl = tax_itm_copy-hwste.
              out-igd = tax_itm_copy-fwste.
              out-igr = tax_itm_copy-kbetr / 10.
              out-igg = tax_itm_copy-hkont.
              append_flg = abap_true.
            endif.
          endif.

          " in case of rcm, only indicator is set; amount is not updated
          if rcm_flg = abap_false and append_flg = abap_true.
            out-txc = tax_itm_copy-mwskz.
            out-tal = tax_itm_copy-hwbas.
            out-tad = tax_itm_copy-fwbas.
          endif.
        endif.
        clear: tax_itm_copy, rcm_flg.
      endloop.

      " executed only once per document tax grp
      if append_flg eq abap_true.
        out-ccd = tax_itm-bukrs.
        out-doc = tax_itm-belnr.
        out-fyr = tax_itm-gjahr.

        clear acc_hdr.
        read table acc_hdrs into acc_hdr with key bukrs = tax_itm-bukrs
                                                  belnr = tax_itm-belnr
                                                  gjahr = tax_itm-gjahr binary search.
        if sy-subrc = 0.
          out-dty = acc_hdr-blart.
          out-pdt = acc_hdr-budat.
          out-ddt = acc_hdr-bldat.
          out-ref = acc_hdr-xblnr.
          out-dcr = acc_hdr-waers.
        endif.

        if tax_itm-shkzg = 'H'.  " Credit
          tax_itm-hwste = tax_itm-hwste * -1.
          tax_itm-fwste = tax_itm-fwste * -1.
          tax_itm-hwbas = tax_itm-hwbas * -1.
          tax_itm-fwbas = tax_itm-fwbas * -1.
        endif.

        out-ttx = out-cgl + out-sgl + out-igl.

        if tax_itm-bupla is not initial.
          out-bpl = tax_itm-bupla.
          data(c_bupla) = 'C' && out-bpl.
          try.
              data(ls_kna1) = lt_kna1[ kunnr = c_bupla ].
              out-bpn = ls_kna1-name1.
              out-bgs = ls_kna1-stcd3.
              out-rst = out-bgs+0(2).
            catch cx_sy_itab_line_not_found.
          endtry.
          clear c_bupla.
        endif.

        out-tam = out-ttx + out-tal.

        clear acc_itm.
        read table acc_itms into acc_itm with key bukrs = tax_itm-bukrs
                                                  belnr = tax_itm-belnr
                                                  gjahr = tax_itm-gjahr
                                                  txgrp = tax_itm-txgrp
                                                  taxps = tax_itm-taxps binary search.
        if sy-subrc = 0.
          if acc_itm-matnr is not initial.
            out-mat = acc_itm-matnr.
            try .
                out-mds = lt_mat_desc[ matnr = out-mat ]-maktx.
              catch cx_sy_itab_line_not_found.
            endtry.
          endif.

          out-hsn = acc_itm-hsn_sac.

          if out-hsn is not initial.
            case out-hsn+0(2).
              when '99'.
                out-ite = 'IS'. " service
              when '84' or '88'.
                out-ite = 'CS'. " capital goods
              when others.
                out-ite = 'IP'. " input
            endcase.

            try .
                out-hds = lt_hsn_desc[ hsn_sac = out-hsn ]-text1.
              catch cx_sy_itab_line_not_found.
                try.
                    out-hds = lt_sac_desc[ hsn_sac = out-hsn ]-j_1icht1.
                  catch cx_sy_itab_line_not_found.  " IHDK901259
                endtry.
            endtry.
          else.
            out-ite = 'NO'. " not applicable
          endif.

          out-qty = acc_itm-menge.
          out-uom = acc_itm-meins.

        endif.

        clear acc_itm.
        read table acc_itms into acc_itm with key bukrs = tax_itm-bukrs
                                                  belnr = tax_itm-belnr
                                                  gjahr = tax_itm-gjahr
                                                  koart = 'K'.  " vendor line item
        if sy-subrc = 0.
          out-txt = acc_itm-sgtxt.  " IRDK932665
          out-cld = acc_itm-augdt.
          try.  " IHDK900145
              if out-cld is initial.
                out-dif = sy-datum - acc_hdr-bldat.
              else.
                out-dif = out-cld - acc_hdr-bldat.

                " IRDK932665
                try.
                    out-prf = lt_cld[ bukrs = acc_itm-bukrs belnr = acc_itm-augbl gjahr = acc_itm-auggj ]-kidno.
                  catch cx_sy_itab_line_not_found.
                endtry.
              endif.
            catch cx_sy_arithmetic_overflow.
            catch cx_sy_arithmetic_error.
          endtry.

          if acc_itm-lifnr is not initial.
            out-vdr = acc_itm-lifnr.
            try .
                clear ls_lfa1.
                ls_lfa1 = lt_lfa1[ lifnr = out-vdr ].
                out-vds = ls_lfa1-name1.
                out-vgs = ls_lfa1-stcd3.
                out-vcl = ls_lfa1-ven_class.  " IHDK900841
                " IHDK900841 - re-org for import vendor
                if ls_lfa1-ktokk eq '2000'. " Import vendor
                  out-sst = cond #(
                              when out-vcl eq '2'  " registered/SEZ
                              then out-vgs+0(2)
                              else 'IM' ).
                else.
                  out-sst = cond #(
                              when out-vgs+0(2) co '0123456789' and out-vcl eq ''
                              then out-vgs+0(2)
                              else lt_state_code[ param1 = ls_lfa1-regio ]-param2 ).
                  " for cases where some random text is maintained in GST no
                endif.
                " End IHDK900841

              catch cx_sy_itab_line_not_found.

            endtry.
          endif.
        endif.

        " IRDK932665
        clear acc_itm.
        read table acc_itms into acc_itm with key bukrs = tax_itm-bukrs
                                                  belnr = tax_itm-belnr
                                                  gjahr = tax_itm-gjahr
                                                  buzid = 'W'.  " GR/IR Item
        if sy-subrc = 0.
          out-asn = acc_itm-zuonr.
        endif.

        append out to outtab.
      endif.

      clear tax_itm.
      clear: append_flg, rcm_flg. " append flag cleared here; only once per document tax grp
    endloop.

    " exclude empl vendors by default
    if c_empl eq abap_false.
      data: r_empl type range of lfa1-lifnr.
      select lifnr
        from lfa1
        into table @data(lt_empl)
        where ktokk eq 'EMPL'.

      if lt_empl is not initial.
        loop at lt_empl into data(ls_empl).
          append initial line to r_empl assigning field-symbol(<r_empl>).
          if <r_empl> is assigned.
            <r_empl>-sign = 'I'.
            <r_empl>-option = 'EQ'.
            <r_empl>-low = ls_empl-lifnr.
          endif.
          clear ls_empl.
        endloop.

        delete outtab where ( vdr in r_empl ).
      endif.
    endif.

    " IHDK900841 - delete non-vendor items
    delete outtab where ( vdr is initial ).
  endmethod.

  method display_alv.
    check outtab is not initial.
    data: o_table     type ref to cl_salv_table,
          o_container type ref to cl_gui_container,
          o_functions type ref to cl_salv_functions_list,
          o_columns   type ref to cl_salv_columns_table,
          o_column    type ref to cl_salv_column_table,
          o_col_list  type ref to cl_salv_column_list,
          o_layout    type ref to cl_salv_layout,
          o_layo      type ref to cl_salv_layout_service,
          o_key       type salv_s_layout_key,
          o_info      type salv_s_layout_info,
          o_display   type ref to cl_salv_display_settings,
          o_head_grid type ref to cl_salv_form_layout_grid,
          o_label     type ref to cl_salv_form_label,
          o_flow      type ref to cl_salv_form_layout_flow,
          o_sorts     type ref to cl_salv_sorts,
          o_aggrs     type ref to cl_salv_aggregations.

    " Display alv
    free: o_table    ,
          o_container,
          o_functions,
          o_columns  ,
          o_column   ,
          o_col_list ,
          o_layout   ,
          o_layo     ,
          o_key      ,
          o_info     ,
          o_display  ,
          o_head_grid,
          o_label    ,
          o_flow     ,
          o_sorts    ,
          o_aggrs    .

    try.
        cl_salv_table=>factory(
        importing
          r_salv_table   = o_table
        changing
          t_table        = outtab ).
      catch cx_salv_msg.
    endtry.

    if o_table is bound.
      o_columns = o_table->get_columns( ).

      if o_columns is bound.
        try.
            " Column procesing
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CCD' ).
            o_column->set_long_text( value = 'Company Code' ).
            o_column->set_medium_text( value = 'Company Code' ).
            o_column->set_short_text( value = 'Cmp Cd' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'DOC' ).
            o_column->set_long_text( value = 'Invoice Number' ).
            o_column->set_medium_text( value = 'Invoice No.' ).
            o_column->set_short_text( value = 'Inv No' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'FYR' ).
            o_column->set_long_text( value = 'Fiscal Year' ).
            o_column->set_medium_text( value = 'Fiscal Year' ).
            o_column->set_short_text( value = 'Fis Yr' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PDT' ).
            o_column->set_long_text( value = 'Posting Date' ).
            o_column->set_medium_text( value = 'Posting Date' ).
            o_column->set_short_text( value = 'Post Dt' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'DDT' ).
            o_column->set_long_text( value = 'Document Date' ).
            o_column->set_medium_text( value = 'Document Date' ).
            o_column->set_short_text( value = 'Doc Dt' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'DTY' ).
            o_column->set_long_text( value = 'Document Type' ).
            o_column->set_medium_text( value = 'Document Type' ).
            o_column->set_short_text( value = 'Doc Typ' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'REF' ).
            o_column->set_long_text( value = 'Reference' ).
            o_column->set_medium_text( value = 'Reference' ).
            o_column->set_short_text( value = 'Ref' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TXC' ).
            o_column->set_long_text( value = 'Tax Code' ).
            o_column->set_medium_text( value = 'Tax Code' ).
            o_column->set_short_text( value = 'Tax Cd' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TAL' ).
            o_column->set_long_text( value = 'Tax Base Amount in Local Currency' ).
            o_column->set_medium_text( value = 'Tax Base Amt in LC' ).
            o_column->set_short_text( value = 'T BAmt LC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'DCR' ).
            o_column->set_long_text( value = 'Document Currency' ).
            o_column->set_medium_text( value = 'Document Curr.' ).
            o_column->set_short_text( value = 'Doc Curr' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TAD' ).
            o_column->set_long_text( value = 'Tax Base Amount in Document Currency' ).
            o_column->set_medium_text( value = 'Tax Base Amt in DC' ).
            o_column->set_short_text( value = 'T BAmt DC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CGC' ).
            o_column->set_long_text( value = 'CGST Condition' ).
            o_column->set_medium_text( value = 'CGST Cond.' ).
            o_column->set_short_text( value = 'CGST Cond' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CGL' ).
            o_column->set_long_text( value = 'CGST in Local Currency' ).
            o_column->set_medium_text( value = 'CGST in LC' ).
            o_column->set_short_text( value = 'CGST LC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CGD' ).
            o_column->set_long_text( value = 'CGST in Document Currency' ).
            o_column->set_medium_text( value = 'CGST in DC' ).
            o_column->set_short_text( value = 'CGST in DC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CGR' ).
            o_column->set_long_text( value = 'CGST Rate' ).
            o_column->set_medium_text( value = 'CGST Rate' ).
            o_column->set_short_text( value = 'CGST Rate' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CGG' ).
            o_column->set_long_text( value = 'CGST GL Account' ).
            o_column->set_medium_text( value = 'CGST GL Acc.' ).
            o_column->set_short_text( value = 'CGST GL' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CRA' ).
            o_column->set_long_text( value = 'CGST RCM Applicable?' ).
            o_column->set_medium_text( value = 'CGST RCM Appl.?' ).
            o_column->set_short_text( value = 'CGST RCM?' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SGC' ).
            o_column->set_long_text( value = 'SGST Condition' ).
            o_column->set_medium_text( value = 'SGST Cond.' ).
            o_column->set_short_text( value = 'SGST Cond' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SGL' ).
            o_column->set_long_text( value = 'SGST in Local Currency' ).
            o_column->set_medium_text( value = 'SGST in LC' ).
            o_column->set_short_text( value = 'SGST LC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SGD' ).
            o_column->set_long_text( value = 'SGST in Document Currency' ).
            o_column->set_medium_text( value = 'SGST in DC' ).
            o_column->set_short_text( value = 'SGST in DC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SGR' ).
            o_column->set_long_text( value = 'SGST Rate' ).
            o_column->set_medium_text( value = 'SGST Rate' ).
            o_column->set_short_text( value = 'SGST Rate' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SGG' ).
            o_column->set_long_text( value = 'SGST GL Account' ).
            o_column->set_medium_text( value = 'SGST GL Acc.' ).
            o_column->set_short_text( value = 'SGST GL' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SRA' ).
            o_column->set_long_text( value = 'SGST RCM Applicable?' ).
            o_column->set_medium_text( value = 'SGST RCM Appl.?' ).
            o_column->set_short_text( value = 'SGST RCM?' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'IGC' ).
            o_column->set_long_text( value = 'IGST Condition' ).
            o_column->set_medium_text( value = 'IGST Cond.' ).
            o_column->set_short_text( value = 'IGST Cond' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'IGL' ).
            o_column->set_long_text( value = 'IGST in Local Currency' ).
            o_column->set_medium_text( value = 'IGST in LC' ).
            o_column->set_short_text( value = 'IGST LC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'IGD' ).
            o_column->set_long_text( value = 'IGST in Document Currency' ).
            o_column->set_medium_text( value = 'IGST in DC' ).
            o_column->set_short_text( value = 'IGST in DC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'IGR' ).
            o_column->set_long_text( value = 'IGST Rate' ).
            o_column->set_medium_text( value = 'IGST Rate' ).
            o_column->set_short_text( value = 'IGST Rate' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'IGG' ).
            o_column->set_long_text( value = 'IGST GL Account' ).
            o_column->set_medium_text( value = 'IGST GL Acc.' ).
            o_column->set_short_text( value = 'IGST GL' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'IRA' ).
            o_column->set_long_text( value = 'IGST RCM Applicable?' ).
            o_column->set_medium_text( value = 'IGST RCM Appl.?' ).
            o_column->set_short_text( value = 'IGST RCM?' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TTX' ).
            o_column->set_long_text( value = 'Total Tax in Local Currency' ).
            o_column->set_medium_text( value = 'Tot. Tax in LC' ).
            o_column->set_short_text( value = 'Tot Tax LC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MAT' ).
            o_column->set_long_text( value = 'Material Code' ).
            o_column->set_medium_text( value = 'Material Code' ).
            o_column->set_short_text( value = 'Mat Cd' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MDS' ).
            o_column->set_long_text( value = 'Material Description' ).
            o_column->set_medium_text( value = 'Material Desc.' ).
            o_column->set_short_text( value = 'Mat Desc' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BPL' ).
            o_column->set_long_text( value = 'Business Place/Place of Supply' ).
            o_column->set_medium_text( value = 'Business Place' ).
            o_column->set_short_text( value = 'B Place' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BPN' ).
            o_column->set_long_text( value = 'Place of Supply Description' ).
            o_column->set_medium_text( value = 'Pl. of Supply Desc.' ).
            o_column->set_short_text( value = 'PoS Desc' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BGS' ).
            o_column->set_long_text( value = 'Place of Supply GST Number' ).
            o_column->set_medium_text( value = 'Pl. of Supply GST' ).
            o_column->set_short_text( value = 'PoS GST' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'RST' ).
            o_column->set_long_text( value = 'Receiving State Code' ).
            o_column->set_medium_text( value = 'Recv. State' ).
            o_column->set_short_text( value = 'Rec State' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'HSN' ).
            o_column->set_long_text( value = 'HSN/SAC Code' ).
            o_column->set_medium_text( value = 'HSN/SAC Code' ).
            o_column->set_short_text( value = 'HSN/SAC' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'HDS' ).
            o_column->set_long_text( value = 'HSN/SAC Description' ).
            o_column->set_medium_text( value = 'HSN/SAC Desc' ).
            o_column->set_short_text( value = 'HSN/SAC Ds' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'QTY' ).
            o_column->set_long_text( value = 'Quantity' ).
            o_column->set_medium_text( value = 'Quantity' ).
            o_column->set_short_text( value = 'Qty' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'UOM' ).
            o_column->set_long_text( value = 'Unit Of Meassure' ).
            o_column->set_medium_text( value = 'Unit Of Measure' ).
            o_column->set_short_text( value = 'UoM' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TAM' ).
            o_column->set_long_text( value = 'Total Amount in Local Currency' ).
            o_column->set_medium_text( value = 'Tot. Amt. in LC' ).
            o_column->set_short_text( value = 'T Amt LC' ).
            o_column->set_sign( if_salv_c_bool_sap=>true ). " preserve -ve/+ve sign in exported file and bg output

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VDR' ).
            o_column->set_long_text( value = 'Vendor Code' ).
            o_column->set_medium_text( value = 'Vendor Cd' ).
            o_column->set_short_text( value = 'Vendor' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VDS' ).
            o_column->set_long_text( value = 'Vendor Name' ).
            o_column->set_medium_text( value = 'Vendor Name' ).
            o_column->set_short_text( value = 'Ven Name' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VGS' ).
            o_column->set_long_text( value = 'Vendor GST Number' ).
            o_column->set_medium_text( value = 'Vendor GST' ).
            o_column->set_short_text( value = 'Ven GST' ).

            " IHDK900841
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VCL' ).
            o_column->set_long_text( value = 'Vendor Classification for GST' ).
            o_column->set_medium_text( value = 'Vendor GST Class' ).
            o_column->set_short_text( value = 'Ven Class' ).
            " End IHDK900841

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SST' ).
            o_column->set_long_text( value = 'Supplying State Code' ).
            o_column->set_medium_text( value = 'Suppl. State' ).
            o_column->set_short_text( value = 'Sup State' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'CLD' ).
            o_column->set_long_text( value = 'Clearing/Payment Date' ).
            o_column->set_medium_text( value = 'Clearing Date' ).
            o_column->set_short_text( value = 'Cl Date' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'DIF' ).
            o_column->set_long_text( value = 'Diff. btwn invoice & and payment date' ).
            o_column->set_medium_text( value = 'Diff. Days' ).
            o_column->set_short_text( value = 'Diff Days' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ITE' ).
            o_column->set_long_text( value = 'ITC Eligibility' ).
            o_column->set_medium_text( value = 'ITC Eligibility' ).
            o_column->set_short_text( value = 'ITC Elig.' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ITC' ).
            o_column->set_long_text( value = 'ITC Claimed?' ).
            o_column->set_medium_text( value = 'ITC Claimed?' ).
            o_column->set_short_text( value = 'ITC Claim?' ).

            " IRDK932665
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PRF' ).
            o_column->set_long_text( value = 'Payment Reference/Check/UTR No' ).
            o_column->set_medium_text( value = 'Check/UTR' ).
            o_column->set_short_text( value = 'Chk/UTR' ).

            " IRDK932665
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ASN' ).
            o_column->set_long_text( value = 'Assignment' ).
            o_column->set_medium_text( value = 'Assignment' ).
            o_column->set_short_text( value = 'Assign' ).

            " IRDK932665
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'TXT' ).
            o_column->set_long_text( value = 'Text' ).
            o_column->set_medium_text( value = 'Text' ).
            o_column->set_short_text( value = 'Text' ).

          catch: cx_salv_not_found.                     "#EC NO_HANDLER
        endtry.
        o_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
      endif.

      o_functions = o_table->get_functions( ).

      if o_functions is bound.
        o_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
      endif.

      o_layout = o_table->get_layout( ).

      o_key-report = sy-repid.

      if o_layout is bound.
        o_layout->set_key( exporting value = o_key ).

        o_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

        o_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
      endif.

      o_display = o_table->get_display_settings( ).

      if o_display is bound.
        o_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
        o_display->set_list_header( exporting value = 'Purchase Tax Register' ).
      endif.
      " IRDK933010 - commented
*      o_sorts = o_table->get_sorts( ).
*      if o_sorts is bound.
*        o_sorts->add_sort( columnname = 'CCD' position = 1 subtotal = if_salv_c_bool_sap=>false ).
*        o_sorts->add_sort( columnname = 'DOC' position = 2 subtotal = if_salv_c_bool_sap=>true ).
*        o_sorts->add_sort( columnname = 'FYR' position = 3 subtotal = if_salv_c_bool_sap=>false ).
*      endif.
*
*      o_aggrs = o_table->get_aggregations( ).
*      o_aggrs->add_aggregation( columnname = 'TAL' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'TAD' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'CGL' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'CGD' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'CGR' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'SGL' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'SGD' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'SGR' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'IGL' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'IGD' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'IGR' aggregation = if_salv_c_aggregation=>total ).
*      o_aggrs->add_aggregation( columnname = 'TTX' aggregation = if_salv_c_aggregation=>total ).
      " end IRDK933010
      create object o_head_grid.
      if o_head_grid is bound.
*        data: begin of selection,
*                flag,
*                olenght type x,
*                line    type raldb-infoline,
*              end of selection,
*              selections like standard table of selection,
*
*              index      type i.
*
*        refresh selections.
*        call function 'PRINT_SELECTIONS'
*          exporting
*            mode      = 0
*            rname     = sy-cprog
*            rvariante = sy-slset
*          tables
*            infotab   = selections.
*
*        clear: selection, index.
*        loop at selections into selection.
*          index = index + 1.
*          free o_flow.
*          o_flow = o_head_grid->create_flow( exporting row = index column = 1 ).
*          if o_flow is bound.
*            o_flow->create_text( exporting text = selection-line ).
*          endif.
*          clear selection.
*        endloop.
        o_label = o_head_grid->create_label( exporting row = 1 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'Purchase Tax Register' ).
        endif.

        o_label = o_head_grid->create_label( exporting row = 2 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'No. of rows:' ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 2 column = 2 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = | { lines( outtab ) }| ).
        endif.

        o_label = o_head_grid->create_label( exporting row = 3 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'Time-stamp:' ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 3 column = 2 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = |{ sy-datum date = user } { sy-uzeit time = user }| ).
        endif.

        o_table->set_top_of_list( exporting value = o_head_grid ).
        o_table->set_top_of_list_print( exporting value = o_head_grid ).
      endif.

      " no page break in spool
      zcl_helper=>print_no_pg_brk( ). " IRDK933385

      o_table->display( ).
    endif.
  endmethod.
endclass.

class main definition.
  public section.
    class-methods: start.
endclass.

class main implementation.
  method start.
    data: lo_app type ref to lcl_application.

    free lo_app.
    lo_app = new lcl_application( ).

    if lo_app is bound.
      lo_app->process( ).
    endif.
  endmethod.
endclass.

* ---- Selection screen events ---- *
* ---- initialisation ---- *
initialization.
  data: lo_sel type ref to lcl_sel_screen_events.

  free lo_sel.
  lo_sel = new lcl_sel_screen_events( ).

  if lo_sel is bound.
    lo_sel->variant_init( ).
  endif.

* ---- value request ---- *
at selection-screen on value-request for p_var.
  if lo_sel is bound.
    lo_sel->variant_f4_selection( ).
  endif.

* ---- at sel screen ---- *
at selection-screen.
  if lo_sel is bound.
    lo_sel->check_variant_existence( ).
  endif.

* ---- Begin main method ---- *
start-of-selection.
  main=>start( ).

end-of-selection.
