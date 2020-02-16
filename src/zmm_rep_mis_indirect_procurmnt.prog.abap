*&---------------------------------------------------------------------*
*& Report  ZMM_REP_MIS_INDIRECT_PROCURMNT
*&---------------------------------------------------------------------*
*& Transaction            : ZMM083
*& Creation Date          : Monday, July 09, 2018 11:40:03
*& Author                 : 6010859 - SaurabhK
*& Functional             : Kamalakar Varma
*& Requested/Approved By  : Mr Prashant Nair
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK930859; IRDK932730
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&---------------------------------------------------------------------*
*& 1. Purchase Register
*&---------------------------------------------------------------------*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Monday, July 16, 2018 10:35:11
*& Rev. Author                : 6010859; SaurabhK
*& Rev. Requested/Approved By : Prashant Nair/Pre-production
*& Rev. Request#              : IRDK932830, IRDK932871
*& Rev. Description           : MM: S_K: ZMM083: Pre-production changes: 16.07.2018
*&                              MM: S_K: ZMM083: Fixes for net price: 18.7.18
*&---------------------------------------------------------------------*
REPORT zmm_rep_mis_indirect_procurmnt.

* ---- Selection Screen and Global Data ---- *
" For Select Options

TYPES: BEGIN OF ty_text,
         text TYPE tdline,
       END OF ty_text,

       BEGIN OF ty_his,
         vgabe   TYPE vgabe,
         bewtl   TYPE bewtl,
         cc      TYPE bukrs,
         st      TYPE t163c-bewtk,
         mov_typ TYPE bwart,
         mat_doc TYPE mblnr,
         mat_itm TYPE mblpo,
         e_date  TYPE cpudt,
         p_date  TYPE budat,
         batch   TYPE charg_d,
         qty     TYPE menge_d,
         amt     TYPE val_loccurr,
         ckey    TYPE waers,
         gr_ir   TYPE clear_val,
       END OF ty_his.

DATA: it_lines TYPE TABLE OF tline,
      lv_name  TYPE thead-tdname,
      it_text  TYPE TABLE OF ty_text,
      wa_text  TYPE ty_text.

DATA: lv_po   TYPE bapimepoheader-po_number,
      it_his  TYPE TABLE OF bapiekbe,
      wa_his  TYPE bapiekbe,
      it_his1 TYPE TABLE OF ty_his,
      wa_his1 TYPE ty_his.

DATA: lv_table   TYPE REF TO cl_salv_table,
      lv_columns TYPE REF TO cl_salv_columns_table,
      lv_column  TYPE REF TO cl_salv_column_table,
      lv_display TYPE REF TO cl_salv_display_settings,
      lv_sort    TYPE REF TO cl_salv_sorts.

DATA : lr_sort_column TYPE REF TO cl_salv_sort,
       lv_agg         TYPE REF TO cl_salv_aggregations,
       lv_functions   TYPE REF TO cl_salv_functions_list.

DATA: pr_no   TYPE eban-banfn,
      pr_dt   TYPE eban-erdat,
      plant   TYPE eban-werks,
      pur_grp TYPE eban-ekgrp.  " IRDK932830

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-sel.
SELECT-OPTIONS: s_banfn FOR pr_no,
                s_erdat FOR pr_dt OBLIGATORY, " IRDK932871
                s_werks FOR plant,
                s_ekgrp FOR pur_grp.  " IRDK932830
SELECTION-SCREEN END OF BLOCK sel.

********Start of changes made by varun on 15.05.2019 ********
SELECTION-SCREEN BEGIN OF BLOCK del WITH FRAME TITLE TEXT-del.
PARAMETERS p_del AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK del.
*******End of changes made by varun on 15.05.2019 ********

* ---- Local class definition ---- *
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS: process.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: pr            TYPE eban,
          pr_tab        LIKE STANDARD TABLE OF pr,

          po            TYPE wb2_v_ekko_ekpo2,
          po_tab        LIKE STANDARD TABLE OF po,

          gr            TYPE ekbe,
          gr_tab        LIKE STANDARD TABLE OF gr,

          mgrp_desc     TYPE t023t,
          mgrp_desc_tab LIKE STANDARD TABLE OF mgrp_desc,

          pgrp_desc     TYPE t024,
          pgrp_desc_tab LIKE STANDARD TABLE OF pgrp_desc,

          it_eket       TYPE TABLE OF eket, "varun
          wa_eket       TYPE eket,

          it_mseg       TYPE TABLE OF mseg,  "varun
          it_ekbe       TYPE TABLE OF ekbe,
*          it_ekbe1      TYPE TABLE OF ekbe,
          it_ekkn       TYPE TABLE OF ekkn,
          it_ekbz       TYPE TABLE OF ekbz,
*          it_ekbz1      TYPE TABLE OF ekbz,
          it_lfa1       TYPE TABLE OF lfa1,

          BEGIN OF out,
            pr_no        TYPE eban-banfn,
            pr_itm       TYPE eban-bnfpo,
            pr_del       TYPE eban-loekz,
            pr_dt        TYPE eban-erdat,
            pr_cdt       TYPE eban-badat,
            po_no        TYPE ekko-ebeln,
            po_itm       TYPE ekpo-ebelp,
            po_del       TYPE ekko-loekz,
            po_dt        TYPE ekko-bedat,
            mat_cd       TYPE eban-matnr,
            mat_ds       TYPE eban-txz01,
            pr_rel       TYPE cdhdr-udate,
            po_rel       TYPE cdhdr-udate,
            qty          TYPE eban-menge,
            uom          TYPE eban-meins,
            price        TYPE eban-preis,
            crncy        TYPE eban-waers,
            tot_amt      TYPE netwr,
            plant        TYPE eban-werks,
            agng         TYPE i,           " no. of days from pr rel/creation to po rel/creation
            mat_grp      TYPE eban-matkl,
            mgrp_ds      TYPE t023t-wgbez,
            pur_grp      TYPE eban-ekgrp,
            pgrp_ds      TYPE t024-eknam,
            gr_no        TYPE ekbe-belnr,
            gr_yr        TYPE ekbe-gjahr,
            gr_itm       TYPE ekbe-buzei,
            gr_dt        TYPE ekbe-budat,
            due_dt       TYPE d,           " gr post date + payment terms from po
*********start of addition made by varun on 06.05.19**********
            po_del_date  TYPE eket-eindt,
            pr_vendor    TYPE lfa1-lifnr,
            pr_vname     TYPE lfa1-name1,
            po_vendor    TYPE lfa1-lifnr,
            po_vname     TYPE lfa1-name1,
            pr_value     TYPE ekko-rlwrt,
            po_value     TYPE ekko-rlwrt,
            acc_ass      TYPE wb2_v_ekko_ekpo2-knttp_i,
            item_c       TYPE t163y-ptext,
            req          TYPE ekpo-afnam,
            to_qty       TYPE i, "ekpo-menge,
            to_value     TYPE i, "ekpo-netwr,
            to_inv       TYPE i, "ekpo-netwr,
            grn          TYPE char04,
            ass_no       TYPE ekkn-anln1,
            ass_name     TYPE anlhtxt,
            cc           TYPE steuc,
            tax_code     TYPE ekpo-mwskz,
            gl           TYPE mseg-sakto,
            gl_name      TYPE skat-txt50,
            ccenter      TYPE mseg-kostl,
            ccenter_name TYPE cskt-ltext,
            text         TYPE char04,
            comp_code    TYPE bukrs,
*********End of addition made by varun on 06.05.19**********
          END OF out,
          outtab LIKE STANDARD TABLE OF out.

    METHODS: select, consolidate, auth_check, display,
      hotspot_click FOR EVENT link_click OF cl_salv_events_table    " Rev 02
        IMPORTING
            row
            column.
ENDCLASS.

* ---- Local class implementation ---- *
CLASS lcl_application IMPLEMENTATION.
  METHOD process.
    select( ).

    IF pr_tab IS NOT INITIAL.
      consolidate( ).

      auth_check( ).
    ELSE.
      MESSAGE 'No data found!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF outtab IS NOT INITIAL.
      display( ).
    ELSE.
      MESSAGE 'No data found!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD select.
    REFRESH pr_tab.
    REFRESH po_tab.
    IF p_del EQ 'X'.

      SELECT *
        FROM eban
        INTO TABLE pr_tab
        WHERE banfn IN s_banfn
        AND   badat IN s_erdat
        AND   bsart IN ( 'YNB', 'ZNB' )
        AND   werks IN s_werks
        AND   ekgrp IN s_ekgrp  " IRDK932830
        AND   bstyp EQ 'B'
*      AND   loekz EQ ''   " not deleted
        AND   blckd EQ ''   " not blocked
        AND   ebakz EQ ''.  " not closed

      CHECK pr_tab IS NOT INITIAL.
************Start of addition made by varun on 23.05.2019***************
*      SELECT *
*        FROM wb2_v_ekko_ekpo2
*        INTO TABLE po_tab
*        FOR ALL ENTRIES IN pr_tab
*        WHERE ebeln = pr_tab-ebeln
*        AND   ebelp_i = pr_tab-ebelp.
**      AND   loekz   EQ ''
**      AND   loekz_i EQ ''.  " l is deleted, s is blocked
      SELECT *
      FROM wb2_v_ekko_ekpo2
      INTO TABLE po_tab
      FOR ALL ENTRIES IN pr_tab
      WHERE banfn_i = pr_tab-banfn
      AND   bnfpo_i = pr_tab-bnfpo.
***********End of addition made by varun on 23.05.2019********************
    ELSE.

      SELECT *
        FROM eban
        INTO TABLE pr_tab
        WHERE banfn IN s_banfn
        AND   badat IN s_erdat
        AND   bsart IN ( 'YNB', 'ZNB' )
        AND   werks IN s_werks
        AND   ekgrp IN s_ekgrp  " IRDK932830
        AND   bstyp EQ 'B'
        AND   loekz EQ ''   " not deleted
        AND   blckd EQ ''   " not blocked
        AND   ebakz EQ ''.  " not closed

      CHECK pr_tab IS NOT INITIAL.
************Start of addition made by varun on 23.05.2019***************
*      SELECT *
*        FROM wb2_v_ekko_ekpo2
*        INTO TABLE po_tab
*        FOR ALL ENTRIES IN pr_tab
*        WHERE ebeln = pr_tab-ebeln
*        AND   ebelp_i = pr_tab-ebelp
*        AND   loekz   EQ ''
*        AND   loekz_i EQ ''.  " l is deleted, s is blocked
      SELECT *
      FROM wb2_v_ekko_ekpo2
      INTO TABLE po_tab
      FOR ALL ENTRIES IN pr_tab
      WHERE banfn_i = pr_tab-banfn
      AND   bnfpo_i = pr_tab-bnfpo
      AND   loekz   EQ ''
      AND   loekz_i EQ ''.  " l is deleted, s is blocked
************End of addition made by varun on 23.05.2019***************
    ENDIF.

    "varun
    CHECK pr_tab IS NOT INITIAL.
    SELECT * INTO TABLE it_lfa1 FROM lfa1 FOR ALL ENTRIES IN pr_tab
                                          WHERE lifnr EQ pr_tab-lifnr.

    CHECK po_tab IS NOT INITIAL.
    REFRESH gr_tab.
    SELECT *
      FROM ekbe
      INTO TABLE gr_tab
      FOR ALL ENTRIES IN po_tab
      WHERE ebeln = po_tab-ebeln
      AND   ebelp = po_tab-ebelp_i
      AND   vgabe IN ( '1', '9' )
      AND   bewtp IN ( 'D', 'E' ).

*********Start of addition made by varun on 06.05.19**********
    CHECK po_tab IS NOT INITIAL.
    SELECT * FROM lfa1 APPENDING TABLE it_lfa1 FOR ALL ENTRIES IN po_tab
                                               WHERE lifnr EQ po_tab-lifnr.

    REFRESH it_eket.
    SELECT * FROM eket INTO TABLE it_eket FOR ALL ENTRIES IN po_tab
                       WHERE ebeln = po_tab-ebeln
                       AND   ebelp = po_tab-ebelp_i.

    SELECT * FROM ekbe INTO TABLE it_ekbe FOR ALL ENTRIES IN po_tab
                       WHERE ebeln = po_tab-ebeln
                       AND   ebelp = po_tab-ebelp_i.

    SELECT * FROM ekkn INTO TABLE it_ekkn FOR ALL ENTRIES IN po_tab
                       WHERE ebeln = po_tab-ebeln
                       AND   ebelp = po_tab-ebelp_i.

    SELECT * FROM ekbz INTO TABLE it_ekbz FOR ALL ENTRIES IN po_tab
                       WHERE ebeln = po_tab-ebeln
                       AND   ebelp = po_tab-ebelp_i.

    REFRESH it_mseg.
    SELECT * FROM mseg INTO TABLE it_mseg FOR ALL ENTRIES IN po_tab
                       WHERE ebeln = po_tab-ebeln
                       AND   ebelp = po_tab-ebelp_i.
*********End of addition made by varun on 06.05.19**********

    " get description master
    SELECT *
      FROM t023t
      INTO TABLE mgrp_desc_tab
      WHERE spras = 'E'.

    SELECT *
      FROM t024
      INTO TABLE pgrp_desc_tab.
  ENDMETHOD.

  METHOD consolidate.
    DATA: pr_no LIKE out-pr_no.
    DATA: po_no LIKE out-po_no.
    CHECK pr_tab IS NOT INITIAL.
    SORT gr_tab ASCENDING BY belnr gjahr budat. " IRDK932881
*****Start of addition by varun on 06.05.2019*******************
    SORT it_eket[] BY ebeln ebelp.
    SORT it_ekbe[] BY ebeln ebelp.
    SORT it_ekkn[] BY ebeln ebelp.
    SORT it_ekbz[] BY ebeln ebelp.
    SORT it_mseg[] BY ebeln ebelp.
********End of addition made by varun on06.05.2019**************
    CLEAR pr.
    LOOP AT pr_tab INTO pr.
      CLEAR out.
      out-pr_no   = pr-banfn.
      out-pr_itm  = pr-bnfpo.
      out-pr_dt   = pr-erdat. " IRDK932871
      out-pr_cdt  = pr-badat. " IRDK932871
      out-po_dt   = pr-bedat.
      out-mat_cd  = pr-matnr.
      out-mat_ds  = pr-txz01.
      out-qty     = pr-menge.
      out-uom     = pr-meins.
      out-price   = pr-preis. " IRDK932871
      out-crncy   = pr-waers.
      out-tot_amt = out-qty * out-price.
      out-plant   = pr-werks.
      out-mat_grp = pr-matkl.

*********Start of addition made by varun on 06.05.19**********
      out-pr_vendor = pr-lifnr.
      out-text   = '@0P@'.
      out-pr_del = pr-loekz.
      IF out-pr_vendor IS NOT INITIAL.
        TRY.
            out-pr_vname = it_lfa1[ lifnr = out-pr_vendor ]-name1.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.
      out-pr_value  = pr-menge * ( pr-preis / pr-peinh ).

*********End of addition made by varun on 06.05.19**********

      CLEAR mgrp_desc.
      READ TABLE mgrp_desc_tab INTO mgrp_desc WITH KEY matkl = out-mat_grp.
      IF sy-subrc = 0.
        out-mgrp_ds = mgrp_desc-wgbez.
      ENDIF.
      out-pur_grp = pr-ekgrp.
      CLEAR pgrp_desc.
      READ TABLE pgrp_desc_tab INTO pgrp_desc WITH KEY ekgrp = out-pur_grp.
      IF sy-subrc = 0.
        out-pgrp_ds = pgrp_desc-eknam.
      ENDIF.

      IF pr-banpr EQ '05'.  " completely released
        CLEAR pr_no.
        pr_no = |{ out-pr_no ALPHA = IN }|.
        SELECT changenr FROM cdpos INTO TABLE @DATA(changenr_tab)
          WHERE objectclas EQ 'BANF' AND objectid EQ @pr_no
          AND   tabname EQ 'EBAN' AND fname EQ 'BANPR' AND value_new EQ '05'.

        IF changenr_tab IS NOT INITIAL.
          SORT changenr_tab DESCENDING BY changenr.
          READ TABLE changenr_tab INTO DATA(changenr) INDEX 1.
          SELECT SINGLE udate FROM cdhdr INTO out-pr_rel
            WHERE changenr EQ changenr-changenr
            AND objectclas EQ 'BANF'
            AND objectid   EQ pr_no.
        ENDIF.

        IF out-pr_rel IS INITIAL.
          out-pr_rel = pr-frgdt.
        ENDIF.
      ENDIF.
      REFRESH changenr_tab.
      CLEAR changenr.

      CLEAR po.
*      IF out-po_no IS NOT INITIAL AND out-po_itm IS NOT INITIAL. "varun
*        READ TABLE po_tab INTO po WITH KEY ebeln = out-po_no ebelp_i = out-po_itm.
*        IF sy-subrc = 0.
      LOOP AT po_tab INTO po WHERE banfn_i = out-pr_no AND bnfpo_i = out-pr_itm.

        SELECT SINGLE procstat FROM ekko INTO @DATA(procstat) WHERE ebeln = @po-ebeln.
        IF procstat = '05'. " completely released
          CLEAR po_no.
          po_no = |{ po-ebeln ALPHA = IN }|.
          SELECT changenr FROM cdpos INTO TABLE changenr_tab
            WHERE objectclas EQ 'EINKBELEG' AND objectid EQ po_no
            AND   tabname EQ 'EKKO' AND fname EQ 'PROCSTAT' AND value_new EQ '05'.

          IF changenr_tab IS NOT INITIAL.
            SORT changenr_tab DESCENDING BY changenr.
            READ TABLE changenr_tab INTO changenr INDEX 1.
            SELECT SINGLE udate FROM cdhdr INTO out-po_rel
              WHERE changenr EQ changenr-changenr
              AND objectclas EQ 'EINKBELEG'
              AND objectid   EQ po_no.
          ENDIF.
        ENDIF.

        " Pick following details from PO if PR has been converted into PO, else keep displaying PR details; IRDK932830
        out-qty     = po-menge_i.
        out-uom     = po-meins_i.
        out-price   = po-netpr_i. " IRDK932871
        out-crncy   = po-waers.
        out-tot_amt = out-qty * out-price.

        SELECT SINGLE ztag1 FROM t052 INTO @DATA(pterm_days) WHERE zterm = @po-zterm.

*********Start of addition made by varun on 06.05.19**********
        out-po_no   = po-ebeln.
        out-po_itm  = po-ebelp_i.

        DATA(lv_count) = REDUCE i( INIT x = 0 FOR wa_ekbe1 IN it_ekbe
                            WHERE ( ebeln = out-po_no AND ebelp = out-po_itm )
                            NEXT x = x + 1 ).
        lv_count = REDUCE i( INIT x = lv_count FOR wa_ekbz IN it_ekbz
                             WHERE ( ebeln = out-po_no AND ebelp = out-po_itm )
                             NEXT x = x + 1 ).
        IF lv_count GT 0.
          out-grn    = '@0N@'.
        ENDIF.

        out-po_vendor = po-lifnr.
        IF out-po_vendor IS NOT INITIAL.
          TRY.
              out-po_vname = it_lfa1[ lifnr = out-po_vendor ]-name1.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDIF.
        out-po_value  = po-netwr_i.
        out-acc_ass   = po-knttp_i.
        out-item_c    = po-pstyp_i.
        SELECT SINGLE ptext INTO out-item_c FROM t163y WHERE spras EQ 'E' AND pstyp EQ out-item_c.
        out-req       = po-afnam_i.
        out-tax_code  = po-mwskz_i.
        out-comp_code = po-bukrs.
        out-po_del    = po-loekz_i.

        out-to_value  = po-netwr_i.
        out-to_qty    = po-menge_i.

        out-to_value = REDUCE i( INIT x = out-to_value FOR wa_mseg1 IN it_mseg
                                 WHERE ( ebeln EQ out-po_no AND ebelp EQ out-po_itm )
                                 NEXT x = x - wa_mseg1-dmbtr ).
        out-to_qty = REDUCE i( INIT x = out-to_qty FOR wa_mseg IN it_mseg
                               WHERE ( ebeln EQ out-po_no AND ebelp EQ out-po_itm )
                               NEXT x = x - wa_mseg-menge ).

        READ TABLE it_ekkn INTO DATA(wa_ekkn) WITH KEY ebeln = out-po_no
                                                       ebelp = out-po_itm
                                                       BINARY SEARCH.
        IF sy-subrc EQ 0.
          out-ass_no  = wa_ekkn-anln1.
          out-gl      = wa_ekkn-sakto.
          out-ccenter = wa_ekkn-kostl.
        ENDIF.
        IF out-gl IS NOT INITIAL.
          SELECT SINGLE txt50 FROM skat INTO out-gl_name WHERE spras = 'E'
                                                         AND   ktopl = '1000'
                                                         AND   saknr = out-gl.
        ENDIF.
        IF out-ass_no IS NOT INITIAL.
          SELECT SINGLE anlhtxt FROM anlh INTO out-ass_name WHERE bukrs = po-bukrs
                                                            AND   anln1 = out-ass_no.
        ENDIF.

        IF out-ccenter IS NOT INITIAL.
          SELECT SINGLE ltext INTO out-ccenter_name FROM cskt WHERE spras EQ 'E'
                                                              AND   kokrs EQ '1000'
                                                              AND   kostl EQ out-ccenter
                                                              AND   datbi GT sy-datum.
        ENDIF.

****///// To be Invoiced value = to be Delivered value - Total Invoiced value till now in EKBE table
****//// Built the logic using PO number 7500001462 and PR 3000015886 which is excatly matches with ME2L
*          LOOP AT it_ekbe INTO DATA(wa_ekbe) WHERE ebeln = out-po_no
*                                             AND   ebelp = out-po_itm
*                                             AND   vgabe = 2.
*            out-to_inv = out-to_inv + wa_ekbe-dmbtr."arewr.
*          ENDLOOP.
*          out-to_inv = po-netwr_i - out-to_value - out-to_inv.
        out-to_inv = po-netwr_i - out-to_value - REDUCE i( INIT x = out-to_inv
                                                           FOR wa_ekbe IN it_ekbe
                                                           WHERE ( ebeln EQ out-po_no  AND
                                                                   ebelp EQ out-po_itm AND
                                                                   vgabe EQ 2 )
                                                           NEXT x = COND #( WHEN wa_ekbe-shkzg EQ 'S'
                                                                            THEN x + wa_ekbe-dmbtr
                                                                            WHEN wa_ekbe-shkzg EQ 'H'
                                                                            THEN x - wa_ekbe-dmbtr ) ).

        SELECT SINGLE steuc INTO out-cc FROM marc WHERE matnr EQ po-matnr_i AND werks EQ po-werks_i.

        READ TABLE it_eket INTO wa_eket WITH KEY ebeln = out-po_no
                                                 ebelp = out-po_itm
                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          out-po_del_date = wa_eket-eindt.
        ENDIF.
*        ENDIF.
*********End of addition made by varun on 06.05.19**********
        REFRESH changenr_tab.
        CLEAR: changenr, procstat.

        CLEAR gr.
        READ TABLE gr_tab INTO gr WITH KEY ebeln = out-po_no ebelp = out-po_itm.
        IF sy-subrc = 0.
          out-gr_no   = gr-belnr.
          out-gr_yr   = gr-gjahr.
          out-gr_itm  = gr-buzei.
          out-gr_dt   = gr-budat.

          out-due_dt  = gr-budat + pterm_days.
        ENDIF.
*        ENDIF. "varun
        CLEAR pterm_days. " IRDK932830

        IF out-pr_rel IS NOT INITIAL.
          IF out-po_rel IS NOT INITIAL.
            out-agng = out-po_rel - out-pr_rel.
          ELSE.
            out-agng = sy-datum - out-pr_rel.
          ENDIF.
        ENDIF.

        APPEND out TO outtab.
      ENDLOOP.
      IF sy-subrc NE 0.
        APPEND out TO outtab.
      ENDIF.
      CLEAR out.
    ENDLOOP.
  ENDMETHOD.

  METHOD auth_check.
    CHECK outtab IS NOT INITIAL.
    LOOP AT outtab INTO out.
      AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
               ID 'ACTVT' FIELD '03'
               ID 'WERKS' FIELD out-plant.
      IF sy-subrc <> 0.
        DATA(noauth_flag) = abap_true.
        DELETE outtab.
      ENDIF.

      " IRDK932830
      AUTHORITY-CHECK OBJECT 'M_BEST_EKG'
               ID 'ACTVT' FIELD '03'
               ID 'EKGRP' FIELD out-pur_grp.
      IF sy-subrc <> 0.
        noauth_flag = abap_true.
        DELETE outtab.
      ENDIF.

      CLEAR: out, noauth_flag.
    ENDLOOP.

    IF noauth_flag = abap_true.
      MESSAGE 'Authorisation missing for some data. Check SU53.' TYPE 'I' DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

  METHOD hotspot_click.
    CLEAR out.
    CASE column.
      WHEN 'PR_NO'.
        READ TABLE outtab INTO out INDEX row.
        IF sy-subrc IS INITIAL AND out-pr_no IS NOT INITIAL.
          SET PARAMETER ID 'BAN' FIELD out-pr_no.
          CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'PO_NO'.
        READ TABLE outtab INTO out INDEX row.
        IF sy-subrc IS INITIAL AND out-po_no IS NOT INITIAL.
          SET PARAMETER ID 'BES' FIELD out-po_no.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'GR_NO'.
        READ TABLE outtab INTO out INDEX row.
        IF sy-subrc IS INITIAL AND out-gr_no IS NOT INITIAL.
          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_mblnr             = out-gr_no
              i_mjahr             = out-gr_yr
            EXCEPTIONS
              illegal_combination = 1
              OTHERS              = 2.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.
        ENDIF.
*********start of addition made by varun on 09.05.2019**********
      WHEN 'TEXT'.
        REFRESH it_text.
        READ TABLE outtab INTO out INDEX row.
        IF sy-subrc EQ 0 AND out-pr_no IS NOT INITIAL.
          lv_name = out-pr_no .

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = 'B01'
              language                = 'E'
              name                    = lv_name
              object                  = 'EBANH'
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc EQ 0.
            wa_text-text = 'HEADER TEXT'.
            APPEND wa_text TO it_text.
            CLEAR wa_text.
            LOOP AT it_lines INTO DATA(wa_lines) WHERE tdline NE ' '.
              wa_text-text = wa_lines-tdline.
              APPEND wa_text TO it_text.
              CLEAR wa_text.
            ENDLOOP.
          ENDIF.


          lv_name = |{ out-pr_no }{ out-pr_itm }|.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
*             CLIENT                  = SY-MANDT
              id                      = 'B01'
              language                = 'E'
              name                    = lv_name
              object                  = 'EBAN'
*             ARCHIVE_HANDLE          = 0
*             LOCAL_CAT               = ' '
*         IMPORTING
*             HEADER                  =
*             OLD_LINE_COUNTER        =
            TABLES
              lines                   = it_lines
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
          IF sy-subrc EQ 0.
            wa_text-text = 'ITEM TEXT'.
            APPEND wa_text TO it_text.
            CLEAR wa_text.
            LOOP AT it_lines INTO wa_lines WHERE tdline NE ' '.
              wa_text-text = wa_lines-tdline.
              APPEND wa_text TO it_text.
              CLEAR wa_text.
            ENDLOOP.
          ENDIF.
          IF it_text IS INITIAL.
            wa_text-text = 'No Texts Maintained'.
            APPEND wa_text TO it_text.
            CLEAR wa_text.
          ENDIF.
          cl_demo_output=>display_data( it_text ).
        ENDIF.
      WHEN 'GRN'.
        REFRESH:it_his1,it_his.
        READ TABLE outtab INTO out INDEX row.
        IF sy-subrc EQ 0 AND out-po_no IS NOT INITIAL.
          lv_po = out-po_no.

          CALL FUNCTION 'BAPI_PO_GETDETAIL1'
            EXPORTING
              purchaseorder = lv_po
            TABLES
              pohistory     = it_his.
          IF sy-subrc EQ 0.

            SELECT * FROM ekbz INTO TABLE @DATA(it_ekbz) WHERE ebeln = @out-po_no
                                                         AND   ebelp = @out-po_itm.
            IF sy-subrc EQ 0.
              LOOP AT it_ekbz INTO DATA(wa_ekbz).
                wa_his1-cc      = out-comp_code.
                wa_his1-mat_doc = wa_ekbz-belnr.
                wa_his1-mat_itm = wa_ekbz-buzei.
                wa_his1-e_date  = wa_ekbz-cpudt.
                wa_his1-p_date  = wa_ekbz-budat.
                wa_his1-amt     = wa_ekbz-dmbtr.
                wa_his1-ckey    = wa_ekbz-waers.
                wa_his1-gr_ir   = wa_ekbz-arewr.
                wa_his1-vgabe   = wa_ekbz-vgabe.
                SELECT SINGLE bewtk bewtl INTO (wa_his1-st,wa_his1-bewtl) FROM t163c
                                                             WHERE spras EQ 'E'
                                                             AND   bewtp EQ wa_ekbz-bewtp.
                APPEND wa_his1 TO it_his1.
                CLEAR wa_his1.
              ENDLOOP.
            ENDIF.

            LOOP AT it_his INTO wa_his WHERE po_item EQ out-po_itm.
              wa_his1-cc      = out-comp_code.
              wa_his1-mov_typ = wa_his-move_type.
              wa_his1-mat_doc = wa_his-mat_doc.
              wa_his1-mat_itm = wa_his-matdoc_itm.
              wa_his1-e_date  = wa_his-entry_date.
              wa_his1-p_date  = wa_his-pstng_date.
              wa_his1-batch   = wa_his-batch.
              wa_his1-qty     = wa_his-quantity.
              wa_his1-amt     = wa_his-val_loccur.
              wa_his1-ckey    = wa_his-currency.
              wa_his1-gr_ir   = wa_his-cl_val_loc.
              wa_his1-vgabe   = wa_his-process_id.

              SELECT SINGLE bewtk bewtl INTO (wa_his1-st,wa_his1-bewtl) FROM t163c
                                                             WHERE spras EQ 'E'
                                                             AND   bewtp EQ wa_his-hist_type.
              APPEND wa_his1 TO it_his1.
              CLEAR wa_his1.
            ENDLOOP.

            CHECK it_his1 IS NOT INITIAL.

            FREE: lv_table,lv_columns,lv_display,lv_sort.

            TRY.
                CALL METHOD cl_salv_table=>factory
                  EXPORTING
                    list_display = if_salv_c_bool_sap=>false
*                   r_container  =
*                   container_name =
                  IMPORTING
                    r_salv_table = lv_table
                  CHANGING
                    t_table      = it_his1.
              CATCH cx_salv_msg .
            ENDTRY.

            IF lv_table IS BOUND.

              lv_columns = lv_table->get_columns( ).
              IF lv_columns IS BOUND.
                FREE lv_column.
                lv_column ?= lv_columns->get_column( columnname = 'ST' ).
                lv_column->set_key( value = if_salv_c_bool_sap=>true ).
                FREE lv_column.
                lv_column ?= lv_columns->get_column( columnname = 'VGABE' ).
                lv_column->set_visible( value = if_salv_c_bool_sap=>false ).
                FREE lv_column.
                lv_column ?= lv_columns->get_column( columnname = 'BEWTL' ).
                lv_column->set_visible( value = if_salv_c_bool_sap=>false ).
              ENDIF.

              lv_display = lv_table->get_display_settings( ).
              IF lv_display IS BOUND.
                lv_display->set_striped_pattern( value = cl_salv_display_settings=>true ).
                lv_display->set_list_header( value = |PO History for Purchase Order { out-po_no } Item { out-po_itm }| ).
              ENDIF.

              lv_functions = lv_table->get_functions( ).
              IF lv_functions IS BOUND.
                lv_functions->set_all( value = if_salv_c_bool_sap=>true ).
              ENDIF.

              lv_sort = lv_table->get_sorts( ).
              IF lv_sort IS BOUND.
                TRY.
                    lv_sort->add_sort(
                      EXPORTING
                        columnname = 'VGABE'
                        sequence   = if_salv_c_sort=>sort_up
                        subtotal   = if_salv_c_bool_sap=>false  ).
                    lv_sort->add_sort(
                      EXPORTING
                        columnname = 'ST'
                        sequence   = if_salv_c_sort=>sort_up
                        subtotal   = if_salv_c_bool_sap=>false  ).
                    lv_sort->add_sort(
                      EXPORTING
                        columnname = 'BEWTL'
                        sequence   = if_salv_c_sort=>sort_up
*                        subtotal   = if_salv_c_bool_sap=>true
                        RECEIVING
                          value      = lr_sort_column  ).
*                    lr_sort_column->set_subtotal( value = if_salv_c_bool_sap=>true ).
*                    CATCH cx_salv_data_error.
                    lv_sort->add_sort(
                      EXPORTING
                        columnname = 'E_DATE'
                        sequence   = if_salv_c_sort=>sort_down
                        subtotal   = if_salv_c_bool_sap=>false  ).

                  CATCH cx_salv_not_found.
                  CATCH cx_salv_existing.
                  CATCH cx_salv_data_error.
                ENDTRY.

*                LV_AGG = lv_table->get_aggregations( ).
*                lv_agg->add_aggregation(
*                  EXPORTING
*                    columnname  = 'AMT'                             " ALV Control: Field Name of Internal Table Field
*                    aggregation = if_salv_c_aggregation=>total " Aggregation
**                  RECEIVING
**                    value       =                              " ALV: Aggregations
*                ).
*                CATCH cx_salv_data_error. " ALV: General Error Class (Checked During Syntax Check)
*                CATCH cx_salv_not_found.  " ALV: General Error Class (Checked During Syntax Check)
*                CATCH cx_salv_existing.   " ALV: General Error Class (Checked During Syntax Check)
              ENDIF.

              lv_table->set_screen_popup(
                EXPORTING
                  start_column = 25
                  end_column   = 150
                  start_line   = 5
                  end_line     = 25
              ).
              lv_table->display( ).
            ENDIF.
          ENDIF.

        ENDIF.
*********End of addition made by varun on 09.05.2019**********
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.

  METHOD display.
    CHECK outtab IS NOT INITIAL.
    DATA: o_table     TYPE REF TO cl_salv_table,
          o_container TYPE REF TO cl_gui_container,
          o_functions TYPE REF TO cl_salv_functions_list,
          o_columns   TYPE REF TO cl_salv_columns_table,
          o_column    TYPE REF TO cl_salv_column_table,
          o_col_list  TYPE REF TO cl_salv_column_list,
          o_layout    TYPE REF TO cl_salv_layout,
          o_layo      TYPE REF TO cl_salv_layout_service,
          o_key       TYPE salv_s_layout_key,
          o_info      TYPE salv_s_layout_info,
          o_display   TYPE REF TO cl_salv_display_settings,
          o_head_grid TYPE REF TO cl_salv_form_layout_grid,
          o_label     TYPE REF TO cl_salv_form_label,
          o_flow      TYPE REF TO cl_salv_form_layout_flow,
          o_events    TYPE REF TO cl_salv_events_table,
          o_sorts     TYPE REF TO cl_salv_sorts.

    " Display alv
    FREE: o_table    ,
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
          o_events   ,
          o_sorts    ,
          o_layout   .

    TRY.
        cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = o_table
        CHANGING
          t_table        = outtab ).
      CATCH cx_salv_msg.
    ENDTRY.

    IF o_table IS BOUND.
      o_columns = o_table->get_columns( ).

      IF o_columns IS BOUND.
        TRY.
            " Column procesing
            " set hotspot, set key
            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_NO' ).
            o_column->set_key( EXPORTING value = if_salv_c_bool_sap=>true ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_NO' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'GR_NO' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_ITM' ).
            o_column->set_key( value = if_salv_c_bool_sap=>true ).

            " IRDK932871
            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_DT' ).
            o_column->set_long_text( value = 'Purhase Requisition Date' ).
            o_column->set_medium_text( value = 'Pur. Req. Date' ).
            o_column->set_short_text( value = 'PR Dt' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_CDT' ).
            o_column->set_long_text( value = 'Purhase Requisition Date(Changed)' ).
            o_column->set_medium_text( value = 'Pur. Req. Date(Chng)' ).
            o_column->set_short_text( value = 'PR Dt(C)' ).
            o_column->set_visible( EXPORTING value = if_salv_c_bool_sap=>false ). " IRDK932881
            " End IRDK932871

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_REL' ).
            o_column->set_long_text( value = 'Purhase Requisition Release Date' ).
            o_column->set_medium_text( value = 'Pur. Req. Rel. Date' ).
            o_column->set_short_text( value = 'PR Rel Dt' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_REL' ).
            o_column->set_long_text( value = 'Purhase Order Release Date' ).
            o_column->set_medium_text( value = 'Pur. Ord. Rel. Date' ).
            o_column->set_short_text( value = 'PO Rel Dt' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'TOT_AMT' ).
            o_column->set_long_text( value = 'Total Amount' ).
            o_column->set_medium_text( value = 'Total Amount' ).
            o_column->set_short_text( value = 'Tot Amt' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'AGNG' ).
            o_column->set_long_text( value = 'Ageing' ).
            o_column->set_medium_text( value = 'Ageing' ).
            o_column->set_short_text( value = 'Ageing' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'DUE_DT' ).
            o_column->set_long_text( value = 'Payment Due Date' ).
            o_column->set_medium_text( value = 'Pay. Due Date' ).
            o_column->set_short_text( value = 'Due Dt' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'GL_NAME' ).
            o_column->set_medium_text( value = 'G/L Account Name' ).
            o_column->set_long_text( value = 'G/L Account Name' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'ASS_NAME' ).
            o_column->set_medium_text( value = 'Asset Name' ).
            o_column->set_long_text( value = 'Asset Name' ).

*********start of addition made by varun on 06.05.19**********
            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_VENDOR' ).
            o_column->set_long_text( value = 'PO Vendor Number' ).
            o_column->set_medium_text( value = 'PO Vendor No.' ).
            o_column->set_short_text( value = 'PO Vendor' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_VENDOR' ).
            o_column->set_long_text( value = 'PR Vendor Number' ).
            o_column->set_medium_text( value = 'PR Vendor No.' ).
            o_column->set_short_text( value = 'PR Vendor' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_VNAME' ).
            o_column->set_long_text( value = 'PO Vendor Name' ).
            o_column->set_medium_text( value = 'PO Vendor Name' ).
            o_column->set_short_text( value = 'PO Vendor' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_VNAME' ).
            o_column->set_long_text( value = 'PR Vendor Name' ).
            o_column->set_medium_text( value = 'PR Vendor Name' ).
            o_column->set_short_text( value = 'PR Vendor' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_VALUE' ).
            o_column->set_long_text( value = 'PO Value' ).
            o_column->set_medium_text( value = 'PO Value' ).
            o_column->set_short_text( value = 'PO Value' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_VALUE' ).
            o_column->set_long_text( value = 'PR Value' ).
            o_column->set_medium_text( value = 'PR Value' ).
            o_column->set_short_text( value = 'PR Value' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PO_DEL' ).
            o_column->set_long_text( value = 'PO Deleted' ).
            o_column->set_medium_text( value = 'PO Deleted' ).
            o_column->set_short_text( value = 'PO Deleted' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'PR_DEL' ).
            o_column->set_long_text( value = 'PR Deleted' ).
            o_column->set_medium_text( value = 'PR Deleted' ).
            o_column->set_short_text( value = 'PR Deleted' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'TO_QTY' ).
            o_column->set_long_text( value = 'To Be Delivered(QTY)' ).
            o_column->set_medium_text( value = 'To Be Delivered(QTY)' ).
            o_column->set_short_text( value = 'Del QTY' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'TO_VALUE' ).
            o_column->set_long_text( value = 'To Be Delivered(Value)' ).
            o_column->set_medium_text( value = 'To Be Delivered(Val)' ).
            o_column->set_short_text( value = 'Del Value' ).

            FREE o_column.
            o_column ?= o_columns->get_column( 'GRN' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).
            o_column->set_icon( value = if_salv_c_bool_sap=>true ).
            o_column->set_short_text( value = 'POH' ).
            o_column->set_medium_text( value = 'POH' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'TEXT' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).
            o_column->set_icon( value = if_salv_c_bool_sap=>true ).
            o_column->set_medium_text( value = 'Long Text' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'TO_INV' ).
            o_column->set_short_text( value = 'To Inv Val' ).
            o_column->set_medium_text( value = 'To Be Invoiced(Val)' ).
            o_column->set_long_text( value = 'To Be Invoiced(Value)' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'COMP_CODE' ).
            o_column->set_visible( value = if_salv_c_bool_sap=>false ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'CCENTER_NAME' ).
            o_column->set_medium_text( value = 'Cost Center Name' ).
            o_column->set_long_text( value = 'Cost Center Name' ).

            FREE o_column.
            o_column ?= o_columns->get_column( columnname = 'TEXT' ).
            o_column->set_short_text( value = 'Long Text' ).
            o_column->set_medium_text( value = 'Long Text' ).
*********End of addition made by varun on 06.05.19**********
          CATCH: cx_salv_not_found.                     "#EC NO_HANDLER
        ENDTRY.
        o_columns->set_optimize( EXPORTING value = if_salv_c_bool_sap=>true ). " Default input bool true
        o_columns->set_key_fixation( EXPORTING value = if_salv_c_bool_sap=>true ).  " fix key columns
      ENDIF.

      " enable all functions
      o_functions = o_table->get_functions( ).

      IF o_functions IS BOUND.
        o_functions->set_all( EXPORTING value = if_salv_c_bool_sap=>true ). " Default input bool true
      ENDIF.

      " IRDK932830
      o_layout = o_table->get_layout( ).

      o_key-report = sy-repid.

      IF o_layout IS BOUND.
        o_layout->set_key( EXPORTING value = o_key ).

        o_layout->set_save_restriction( EXPORTING value = cl_salv_layout=>restrict_none ).

        o_layout->set_default( EXPORTING value = if_salv_c_bool_sap=>true ).
      ENDIF.
      " end IRDK932830

      " set stripped pattern
      o_display = o_table->get_display_settings( ).

      IF o_display IS BOUND.
        o_display->set_striped_pattern( EXPORTING value = cl_salv_display_settings=>true ).
        o_display->set_list_header( EXPORTING value = 'MIS: Indirect Procurement' ).
      ENDIF.

      " Report header
      CREATE OBJECT o_head_grid.
      IF o_head_grid IS BOUND.
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

        o_label = o_head_grid->create_label( EXPORTING row = 1 column = 1 ).
        IF o_label IS BOUND.
          o_label->set_text( EXPORTING value = 'MIS: Indirect Procurement' ).
        ENDIF.

        o_label = o_head_grid->create_label( EXPORTING row = 2 column = 1 ).
        IF o_label IS BOUND.
          o_label->set_text( EXPORTING value = 'No. of rows:' ).
        ENDIF.

        o_flow = o_head_grid->create_flow( EXPORTING row = 2 column = 2 ).
        IF o_flow IS BOUND.
          o_flow->create_text( EXPORTING text = | { lines( outtab ) }| ).
        ENDIF.

        o_label = o_head_grid->create_label( EXPORTING row = 3 column = 1 ).
        IF o_label IS BOUND.
          o_label->set_text( EXPORTING value = 'Time-stamp:' ).
        ENDIF.

        o_flow = o_head_grid->create_flow( EXPORTING row = 3 column = 2 ).
        IF o_flow IS BOUND.
          o_flow->create_text( EXPORTING text = |{ sy-datum DATE = USER } { sy-uzeit TIME = USER }| ).
        ENDIF.

        o_table->set_top_of_list( EXPORTING value = o_head_grid ).
        o_table->set_top_of_list_print( EXPORTING value = o_head_grid ).
      ENDIF.

      " Event handling - hotpost click
      o_events = o_table->get_event( ).
      IF o_events IS BOUND.
        SET HANDLER hotspot_click FOR o_events.
      ENDIF.

      " Sort
      o_sorts = o_table->get_sorts( ).
      IF o_sorts IS BOUND.
        TRY.
            o_sorts->add_sort( columnname = 'PR_NO' position = 1 subtotal = if_salv_c_bool_sap=>false ).

            o_sorts->add_sort( columnname = 'PR_ITM' sequence = if_salv_c_sort=>sort_up ).

            o_sorts->add_sort( columnname = 'PO_NO' sequence = if_salv_c_sort=>sort_up ).

            o_sorts->add_sort( columnname = 'PO_ITM' sequence = if_salv_c_sort=>sort_up ).

          CATCH cx_salv_not_found.
          CATCH cx_salv_existing.
          CATCH cx_salv_data_error.
        ENDTRY.
      ENDIF.

      o_table->display( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

* ---- main class definition ---- *
CLASS main DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: start.
ENDCLASS.

* ---- main class implementation ---- *
CLASS main IMPLEMENTATION.
  METHOD start.
    DATA: lo_app TYPE REF TO lcl_application.

    FREE lo_app.
    lo_app = NEW lcl_application( ).

    IF lo_app IS BOUND.
      lo_app->process( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

* ---- Initiialisation ---- *

* ---- Selection Screen Events ---- *

* ---- Begin main method ---- *
START-OF-SELECTION.
  main=>start( ).
