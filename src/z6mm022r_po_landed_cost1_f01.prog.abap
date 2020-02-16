*&---------------------------------------------------------------------*
*&  Include           Z6MM022R_PO_LANDED_COST_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Dev. Class    : ZPP                                                *
*   Report Name   :  Z6MM022R_PO_LANDED_COST_F01                       *
*   Program Type  : Include                                            *
*   Created by    : Kiruthika                                          *
*   Created on    : 26/06/2010                                         *
*   TCode         :                                                    *
*   Module Name   :                                                    *
*   Description   : PO landed cost report                              *
*----------------------------------------------------------------------*
*   S O U R C E   C O D E   C H A N G E   H I S T O R Y
* ---------------------------------------------------------------------*
*   CODE    | AUTHOR    | DATE     |  DESC
* ---------------------------------------------------------------------*
*
* ---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  VALIDATION
*&---------------------------------------------------------------------*
* Checks the user input and throws error message if no field in the
* selection screen is filled
*----------------------------------------------------------------------*
*{   INSERT         SBXK900038                                        1
****************************************************************************************************
*<< S/4HANA >>*
* Changed On - 10/07/2018
* Changed By - Rahul Shukla
* Purpose  - Simplification List: HANA Migration
* Solution - Table konv Conditions (Obsolete - replaced by PRCD_ELEMENTS)
* TR - SBXK900038
*****************************************************************************************************
*}   INSERT

FORM validation .

  DATA: l_ekorg TYPE ekorg,
        l_werks TYPE ekko-eq_werks,
        l_ebeln TYPE ekko-ebeln,
        l_matnr TYPE ekpo-matnr,
        l_matkl TYPE ekpo-matkl,
        l_mtart TYPE ekpo-mtart,
        l_bsart TYPE ekko-bsart,
        l_llief TYPE ekko-llief,
        l_reswk TYPE ekko-reswk.

  IF  so_lifnr IS INITIAL AND
      so_ekorg IS INITIAL AND
      so_werks IS INITIAL AND
      so_ebeln IS INITIAL AND
      so_matnr IS INITIAL AND
      so_matkl IS INITIAL AND
      so_mtart IS INITIAL AND
      so_bsart IS INITIAL AND
      so_llief IS INITIAL AND
      so_reswk IS INITIAL AND
      so_matkl IS INITIAL AND
      ( sy-ucomm = 'ONLI' OR sy-ucomm = 'SPOS' ).
    MESSAGE e000.
*   Please enter values for any one of the option!!!

  ENDIF.
*  Validation for vendor
  IF NOT so_lifnr IS INITIAL.
    CLEAR wa_lifnr.
    SELECT SINGLE lifnr name1 FROM lfa1 INTO wa_lifnr       "#EC *
      WHERE lifnr IN so_lifnr .
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-010 .
    ENDIF.
  ENDIF.
* Pur. Org.
  IF NOT so_ekorg IS INITIAL.
    SELECT SINGLE ekorg INTO l_ekorg FROM t024e             "#EC *
      WHERE ekorg IN so_ekorg.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-041 .
    ENDIF.
  ENDIF.
*  Plant
  IF NOT so_werks IS INITIAL.
    SELECT SINGLE werks FROM t001w INTO l_werks             "#EC *
      WHERE werks IN so_werks.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-012 .
    ENDIF.
  ENDIF.
* Purchasing document
  IF NOT so_ebeln IS INITIAL.
    SELECT SINGLE ebeln FROM ekko INTO l_ebeln              "#EC *
      WHERE ebeln IN so_ebeln.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-042 .
    ENDIF.
  ENDIF.
* Material Number
  IF NOT so_matnr IS INITIAL.
    SELECT SINGLE matnr FROM mara INTO l_matnr              "#EC *
      WHERE matnr IN so_matnr.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-043.
    ENDIF.
  ENDIF.
* Material group
  IF NOT so_matkl IS INITIAL.
    SELECT SINGLE matkl FROM t023 INTO l_matkl              "#EC *
      WHERE matkl IN so_matkl.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-044.
    ENDIF.
  ENDIF.
* Material type
  IF NOT so_mtart IS INITIAL.
    SELECT SINGLE mtart FROM t134 INTO l_mtart              "#EC *
      WHERE mtart IN so_mtart.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-045.
    ENDIF.
  ENDIF.
* PO type
  IF NOT so_bsart IS INITIAL.
    SELECT SINGLE bsart FROM t161 INTO l_bsart              "#EC *
      WHERE bsart IN so_bsart.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-046.
    ENDIF.
  ENDIF.
* Supply Vendor
  IF NOT so_llief IS INITIAL.
    SELECT SINGLE lifnr FROM lfa1 INTO l_llief              "#EC *
      WHERE lifnr IN so_llief.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-047.
    ENDIF.
  ENDIF.
* Supply plant
  IF NOT so_reswk IS INITIAL.
    SELECT SINGLE werks FROM t001w INTO l_reswk             "#EC *
      WHERE werks IN so_reswk.
    IF sy-subrc <> 0.
      MESSAGE e001 WITH text-048.
    ENDIF.
  ENDIF.
ENDFORM.                    " VALIDATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
* Fetch data from database
*----------------------------------------------------------------------*
FORM select_data .
*  Refresh all the internal tables and work area
  PERFORM refresh_int_tables.
* Fills the header details like PO number, PO type, Vendor, Currency
*  key and currency exchange rate from table EKKO based on user input
  IF r_po = 'X'.

    SELECT ebeln bedat bsart lifnr waers wkurs knumv zterm rlwrt ekgrp FROM ekko
      INTO TABLE it_ekko
      WHERE ebeln IN so_ebeln AND     " pur doc
            lifnr IN so_lifnr AND     " Vendor
            ekorg IN so_ekorg AND     " pur org
            bsart IN so_bsart AND     " Pur doc type
            llief IN so_llief AND     " Supply vendor
            reswk IN so_reswk AND   " Supply plant
            bedat IN so_bedat AND
            frgke IN s_frgke.
*    If value does not exists an error message will appear
    IF it_ekko IS INITIAL.
***** Start Code: Added by CS on 16.10.2015 for Authorization message. *****
      IF lv_werks_auth_flg = 'X' OR lv_mtart_auth_flg = 'X' OR lv_ekorg_auth_flg = 'X'.
        MESSAGE 'Missing Authorization for Pur. Org./Plant/Material Type.' TYPE 'I' DISPLAY LIKE 'W'.
        LEAVE LIST-PROCESSING.
      ELSE.
        MESSAGE e002 DISPLAY LIKE 'I'.
      ENDIF.
*      MESSAGE e002 DISPLAY LIKE 'I'.
***** End Code: Added by CS on 16.10.2015 for Authorization message. *****
*   No data found!!!!
    ENDIF.
* Sort EKKO based on PO number
    SORT  it_ekko BY ebeln ASCENDING.

    IF NOT it_ekko[] IS INITIAL.
*    Fills item details like PO number, PO item no, Material No, Mat
* desc, Unit, plant, netprice from EKPO based on the user input
* and header details.
      SELECT ebeln ebelp txz01 matnr menge meins werks netpr FROM ekpo
          INTO TABLE it_ekpo
          FOR ALL ENTRIES IN it_ekko
          WHERE
            ebeln EQ it_ekko-ebeln AND
            werks IN so_werks AND
            matnr IN so_matnr AND" material
            matkl IN so_matkl AND " material group
            mtart IN so_mtart AND   " Mat type
            loekz = ''.

*  Sort EKPO based on PO number and PO item no
      SORT it_ekpo BY ebeln ASCENDING ebelp ASCENDING.

*   Get Material info i.e. Division
      IF it_ekpo IS NOT INITIAL.
        SELECT matnr spart
          FROM mara
          INTO TABLE it_mara
          FOR ALL ENTRIES IN it_ekpo
          WHERE matnr EQ it_ekpo-matnr.
        SORT it_mara BY matnr.
      ENDIF.


* Gets vendor details LIFNR
      SELECT lifnr name1 FROM lfa1
        INTO TABLE it_lifnr
        FOR ALL ENTRIES IN it_ekko
        WHERE lifnr EQ it_ekko-lifnr.
*  Sort vendor based on vendor number
      SORT it_lifnr BY lifnr ASCENDING.
      DELETE ADJACENT DUPLICATES FROM it_lifnr.
* Gets condition type
*{   REPLACE        SBXK900038                                        1
*\      SELECT knumv kposn kschl kwert kbetr FROM konv
*\        INTO TABLE it_konv
*\        FOR ALL ENTRIES IN it_ekko
*\        WHERE knumv = it_ekko-knumv.
      SELECT knumv kposn kschl kwert kbetr FROM PRCD_ELEMENTS
        INTO TABLE it_konv
        FOR ALL ENTRIES IN it_ekko
        WHERE knumv = it_ekko-knumv.
*}   REPLACE

      SORT it_konv BY knumv ASCENDING.
    ENDIF.
  ENDIF.
* If GR based output, get the Material doc number, year, item, quantity
*  from EKBE ( PO History )
  IF r_gr = 'X' ."AND IT_EKPO[] IS NOT INITIAL.
    SELECT ebeln ebelp vgabe gjahr belnr budat dmbtr buzei menge shkzg bewtp
      FROM ekbe
      INTO TABLE it_ekbe
*      FOR ALL ENTRIES IN IT_EKPO
      WHERE ebeln IN so_ebeln "= IT_EKPO-EBELN AND
*            EBELP = IT_EKPO-EBELP AND
        AND werks IN so_werks     " Added by CS on 21.12.2015
        AND budat IN so_budat
        AND bewtp IN ('E').
    SORT it_ekbe BY ebeln ebelp.  " Added by CS on 21.12.2015

    SELECT a~ebeln a~ebelp a~vgabe a~gjahr a~belnr a~budat a~dmbtr a~buzei a~menge a~shkzg a~bewtp
      FROM ekbe AS a
      JOIN ekko AS b
      ON a~ebeln = b~ebeln
      INTO TABLE it_ekbe_sto
*      FOR ALL ENTRIES IN IT_EKPO
      WHERE a~ebeln IN so_ebeln "= IT_EKPO-EBELN AND
*            EBELP = IT_EKPO-EBELP AND
            AND a~budat IN so_budat
            AND a~werks IN so_werks     " Added by CS on 21.12.2015
            AND a~bewtp IN ('U')
            AND b~bsart = 'ZSTO'.
    SORT it_ekbe_sto BY ebeln ebelp.  " Added by CS on 21.12.2015

    IF it_ekbe_sto IS NOT INITIAL.
      SELECT *
        FROM ekbz
        INTO TABLE it_ekbz_sto
        FOR ALL ENTRIES IN it_ekbe_sto
        WHERE ebeln EQ it_ekbe_sto-ebeln  " Added by CS on 18.12.2015
          AND ebelp EQ it_ekbe_sto-ebelp  " Added by CS on 21.12.2015
          AND vgabe EQ it_ekbe_sto-vgabe  " Added by CS on 21.12.2015
          AND gjahr EQ it_ekbe_sto-gjahr  " Added by CS on 18.12.2015
          AND belnr EQ it_ekbe_sto-belnr
          AND buzei EQ it_ekbe_sto-buzei.  " Added by CS on 18.12.2015
      IF sy-subrc = 0.
        SORT it_ekbz_sto BY belnr buzei.
      ENDIF.

    ENDIF.
    IF it_ekbe IS NOT INITIAL.

      SELECT *
        FROM ekbz
        INTO TABLE it_ekbz
        FOR ALL ENTRIES IN it_ekbe
        WHERE ebeln EQ it_ekbe-ebeln  " Added by CS on 18.12.2015
          AND ebelp EQ it_ekbe-ebelp  " Added by CS on 21.12.2015
          AND vgabe EQ it_ekbe-vgabe  " Added by CS on 21.12.2015
          AND gjahr EQ it_ekbe-gjahr  " Added by CS on 18.12.2015
          AND belnr EQ it_ekbe-belnr
          AND buzei EQ it_ekbe-buzei.  " Added by CS on 18.12.2015
      IF sy-subrc = 0.
        SORT it_ekbz BY belnr buzei.
      ENDIF.
      SELECT *
        FROM j_1igrxref
        INTO TABLE it_j_1igrxref
        FOR ALL ENTRIES IN it_ekbe
        WHERE mblnr = it_ekbe-belnr.
      IF sy-subrc = 0.
        SORT it_j_1igrxref BY mblnr.
      ENDIF.

      SELECT ebeln bedat bsart lifnr waers wkurs knumv zterm rlwrt ekgrp
        FROM ekko
        INTO TABLE it_ekko
        FOR ALL ENTRIES IN it_ekbe
        WHERE ebeln = it_ekbe-ebeln AND     " pur doc
              lifnr IN so_lifnr AND     " Vendor
              ekorg IN so_ekorg AND     " pur org
              bsart IN so_bsart AND     " Pur doc type
              llief IN so_llief AND     " Supply vendor
              reswk IN so_reswk AND   " Supply plant
              bedat IN so_bedat AND
              frgke IN s_frgke.
*    If value does not exists an error message will appear
      IF it_ekko IS INITIAL.
***** Start Code: Added by CS on 16.10.2015 for Authorization message. *****
        IF lv_werks_auth_flg = 'X' OR lv_mtart_auth_flg = 'X' OR lv_ekorg_auth_flg = 'X'.
          MESSAGE 'Missing Authorization for Pur. Org./Plant/Material Type.' TYPE 'I' DISPLAY LIKE 'W'.
          LEAVE LIST-PROCESSING.
        ELSE.
          MESSAGE e002 DISPLAY LIKE 'I'.
        ENDIF.
*      MESSAGE e002 DISPLAY LIKE 'I'.
***** End Code: Added by CS on 16.10.2015 for Authorization message. *****
*   No data found!!!!
      ENDIF.
* Sort EKKO based on PO number
      SORT  it_ekko BY ebeln ASCENDING.

      IF NOT it_ekko[] IS INITIAL.
*    Fills item details like PO number, PO item no, Material No, Mat
* desc, Unit, plant, netprice from EKPO based on the user input
* and header details.
        SELECT ebeln ebelp txz01 matnr menge meins werks netpr
          FROM ekpo
          INTO TABLE it_ekpo
            FOR ALL ENTRIES IN it_ekko
            WHERE ebeln EQ it_ekko-ebeln AND
                  werks IN so_werks AND
                  matnr IN so_matnr AND" material
                  matkl IN so_matkl AND " material group
                  mtart IN so_mtart AND   " Mat type
                  loekz = ''.

*  Sort EKPO based on PO number and PO item no
        SORT it_ekpo BY ebeln ASCENDING ebelp ASCENDING.

*   Get Material info i.e. Division
        IF it_ekpo IS NOT INITIAL.
          SELECT matnr spart
            FROM mara
            INTO TABLE it_mara
            FOR ALL ENTRIES IN it_ekpo
            WHERE matnr EQ it_ekpo-matnr.
          SORT it_mara BY matnr.
        ENDIF.

* Gets vendor details LIFNR
        SELECT lifnr name1 FROM lfa1
          INTO TABLE it_lifnr
          FOR ALL ENTRIES IN it_ekko
          WHERE lifnr EQ it_ekko-lifnr.
*  Sort vendor based on vendor number
        SORT it_lifnr BY lifnr ASCENDING.
        DELETE ADJACENT DUPLICATES FROM it_lifnr.
* Gets condition type
*{   REPLACE        SBXK900038                                        2
*\        SELECT knumv kposn kschl kwert kbetr FROM konv
*\          INTO TABLE it_konv
*\          FOR ALL ENTRIES IN it_ekko
*\          WHERE knumv = it_ekko-knumv.
        SELECT knumv kposn kschl kwert kbetr FROM PRCD_ELEMENTS
          INTO TABLE it_konv
          FOR ALL ENTRIES IN it_ekko
          WHERE knumv = it_ekko-knumv.
*}   REPLACE

        SORT it_konv BY knumv ASCENDING.
      ENDIF.
    ENDIF.

  ENDIF.


ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
* Fills the output table
*----------------------------------------------------------------------*
FORM fill_data .
*  For each PO, get the item details and fill the condition value for
*  each item.
  LOOP AT it_ekko INTO wa_ekko.
    LOOP AT it_ekpo INTO wa_ekpo WHERE ebeln = wa_ekko-ebeln.
      wa_output-ebeln = wa_ekko-ebeln.
      wa_output-bedat = wa_ekko-bedat.
      wa_output-ebelp = wa_ekpo-ebelp.
      wa_output-matnr = wa_ekpo-matnr.
      wa_output-txz01 = wa_ekpo-txz01.
      wa_output-menge = wa_ekpo-menge.
      wa_output-meins = wa_ekpo-meins.
      wa_output-lifnr = wa_ekko-lifnr.

      CLEAR wa_lifnr.
      READ TABLE it_lifnr INTO wa_lifnr WITH KEY lifnr = wa_ekko-lifnr.
      IF sy-subrc = 0.
        wa_output-name1 = wa_lifnr-name1.
      ENDIF.
      wa_output-werks  = wa_ekpo-werks.
      wa_output-waers = wa_ekko-waers.
      wa_output-wkurs = wa_ekko-wkurs.

      wa_output-lcurr = wa_ekpo-menge * wa_ekpo-netpr * wa_ekko-wkurs.
      wa_output-base_mat = wa_ekpo-netpr * wa_ekko-wkurs.
      wa_output-per_unit = wa_ekpo-netpr.

* if PO type other than service PO get the general pricing details
      IF ( wa_ekko-bsart CS 'SE' ).
* Only few pricing details are required for service PO
        PERFORM get_pricing_for_servicepo.
      ELSE.
        PERFORM fill_pricing_details.
        PERFORM change_price_imp.
      ENDIF.


      wa_output-tot_price =  wa_output-lcurr + wa_output-bedamt + wa_output-ecsamt + wa_output-secs + wa_output-incvd + wa_output-incvd_prim  + wa_output-incvd_sec +
                           wa_output-bcustom + wa_output-educess_prim + wa_output-educess_sec  + wa_output-custduty + wa_output-cha + wa_output-landing +
                           wa_output-fri_amt + wa_output-octori + wa_output-cst + wa_output-vat.

      IF wa_output-menge IS  NOT INITIAL.
        wa_output-tot_price1 = wa_output-tot_price / wa_output-menge.
      ELSE.
        CLEAR: wa_output-tot_price1.
      ENDIF.

*   If the report is GR based, then the pricing should be converted for
*   GR quantity
      IF r_gr = 'X'.
        DATA : w_sum TYPE ekbe-menge.
        CLEAR w_sum.
        LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln = wa_ekpo-ebeln AND
                                           ebelp = wa_ekpo-ebelp.
          wa_output-gjahr    = wa_ekbe-gjahr.
          wa_output-belnr    = wa_ekbe-belnr.
          wa_output-buzei    = wa_ekbe-buzei.
          wa_output-gr_menge = wa_ekbe-menge .
*          w_sum = w_sum + wa_ekbe-menge.
*          PERFORM change_price_gr.
*          CLEAR wa_ekbe.
*        ENDLOOP.
*        wa_output-gr_menge = w_sum.
          wa_output-lcurr = wa_ekbe-menge * wa_ekpo-netpr * wa_ekko-wkurs.
          CLEAR w_sum.
*          PERFORM change_price_gr.
*****
*          calculation of landed cost is different for diffent types of po
*          CASE wa_ekko-bsart.
**        Domestic PO
*            WHEN  'ZDOM' OR 'YDOM'.
*              wa_output-land_cost = wa_output-lcurr + wa_output-cha +
*                                    wa_output-fri_amt + wa_output-landing +
*                                    wa_output-cst + wa_output-vat +
*                                    wa_output-octori.
*
*              IF wa_ekpo-menge IS NOT INITIAL.
*                wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*              ENDIF.
**      Import PO
*            WHEN 'ZIMP' OR 'YIMP'.
**   Change the pricing into local currency
*              PERFORM change_price_imp.
*              wa_output-land_cost = wa_output-lcurr + wa_output-landing +
*                                    wa_output-bcustom + wa_output-cha +
*                                    wa_output-educess_prim +
*                                    wa_output-educess_sec +
*                                    wa_output-custduty + "wa_output-cha +
*                                    wa_output-fri_amt +
*                                    wa_output-octori.
*              IF wa_ekpo-menge IS NOT INITIAL.
*                wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*              ENDIF.
**      Service PO
*            WHEN 'ZSEI' OR 'ZSED' OR 'YSEI' OR 'YSED'.
*
*              wa_output-land_cost = wa_output-lcurr + wa_output-bstax +
*                                   wa_output-ecstax + wa_output-shetax.
*              IF wa_ekpo-menge IS NOT INITIAL.
*                wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*              ENDIF.
**      Other types of PO
*            WHEN OTHERS.
*              wa_output-land_cost = wa_output-lcurr + wa_output-bcustom +
*                                    wa_output-educess_prim +
*                                    wa_output-educess_sec +
*                                    wa_output-custduty + wa_output-cha +
*                                    wa_output-landing + wa_output-fri_amt +
*                                    wa_output-octori + wa_output-cst +
*                                    wa_output-vat.
*              IF wa_ekpo-menge IS NOT INITIAL.
*                wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*              ENDIF.
*
*          ENDCASE.

          wa_output-tot_exc = wa_output-bedamt + wa_output-ecsamt + wa_output-secs.
          TRY.
              wa_output-cenvare = wa_output-tot_exc / wa_ekpo-menge.
            CATCH cx_sy_arithmetic_error .
              wa_output-cenvare = 0.
          ENDTRY.
*          wa_output-tot_cd = wa_output-bcustom + wa_output-incvd +
*                             wa_output-incvd_prim + wa_output-incvd_sec
*                             + wa_output-educess_prim +
*                               wa_output-educess_sec .
          wa_output-tot_cd = wa_output-incvd +
                             wa_output-incvd_prim +
                             wa_output-incvd_sec +
                             wa_output-custduty.


          IF wa_ekpo-menge IS NOT INITIAL.
            wa_output-cust_pu = wa_output-tot_cd / wa_ekpo-menge.
          ENDIF.

          wa_output-land_cost = wa_output-tot_price - wa_output-tot_exc - wa_output-tot_cd.
          IF wa_ekpo-menge IS NOT INITIAL.
            wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
          ENDIF.


          IF r_gr = 'X'.
            IF wa_output-gr_menge IS NOT INITIAL.
              wa_output-land_costpu = wa_output-land_cost / wa_output-gr_menge.
            ENDIF.
          ELSE.
            IF wa_output-menge IS  NOT INITIAL.
              wa_output-land_costpu = wa_output-land_cost / wa_output-menge.
            ENDIF.
          ENDIF.

          APPEND wa_output TO it_output.
        ENDLOOP.

      ELSE.



*      TRY.
*          wa_output-land_cost = wa_output-tot_price / wa_ekpo-menge.
*        CATCH cx_sy_arithmetic_error .
*          wa_output-land_cost = 0.
*      ENDTRY.

* Calculation of landed cost is different for diffent types of PO
*        CASE wa_ekko-bsart.
**        Domestic PO
*          WHEN  'ZDOM' OR 'YDOM'.
*            wa_output-land_cost = wa_output-lcurr + wa_output-cha +
*                                  wa_output-fri_amt + wa_output-landing +
*                                  wa_output-cst + wa_output-vat +
*                                  wa_output-octori.
*
*            IF wa_ekpo-menge IS NOT INITIAL.
*              wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*            ENDIF.
**      Import PO
*          WHEN 'ZIMP' OR 'YIMP'.
**   Change the pricing into local currency
**            PERFORM change_price_imp.
*            wa_output-land_cost = wa_output-lcurr + wa_output-landing +
*                                  wa_output-bcustom + wa_output-cha +
*                                  wa_output-educess_prim +
*                                  wa_output-educess_sec +
*                                  wa_output-custduty +" wa_output-cha +
*                                  wa_output-fri_amt +
*                                  wa_output-octori.
*            IF wa_ekpo-menge IS NOT INITIAL.
*              wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*            ENDIF.
**      Service PO
*          WHEN 'ZSEI' OR 'ZSED' OR 'YSEI' OR 'YSED'.
*
*            wa_output-land_cost = wa_output-lcurr + wa_output-bstax +
*                                 wa_output-ecstax + wa_output-shetax.
*            IF wa_ekpo-menge IS NOT INITIAL.
*              wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*            ENDIF.
**      Other types of PO
*          WHEN OTHERS.
*            wa_output-land_cost = wa_output-lcurr + wa_output-bcustom +
*                                  wa_output-educess_prim +
*                                  wa_output-educess_sec +
*                                  wa_output-custduty + wa_output-cha +
*                                  wa_output-landing + wa_output-fri_amt +
*                                  wa_output-octori + wa_output-cst +
*                                  wa_output-vat.
*            IF wa_ekpo-menge IS NOT INITIAL.
*              wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
*            ENDIF.
*
*        ENDCASE.

        wa_output-tot_exc = wa_output-bedamt + wa_output-ecsamt + wa_output-secs.
        TRY.
            wa_output-cenvare = wa_output-tot_exc / wa_ekpo-menge.
          CATCH cx_sy_arithmetic_error .
            wa_output-cenvare = 0.
        ENDTRY.
*        wa_output-tot_cd = wa_output-bcustom + wa_output-incvd +
*                           wa_output-incvd_prim + wa_output-incvd_sec
*                           + wa_output-educess_prim +
*                             wa_output-educess_sec .
        wa_output-tot_cd = wa_output-incvd +
                             wa_output-incvd_prim +
                             wa_output-incvd_sec +
                             wa_output-custduty.
        IF wa_ekpo-menge IS NOT INITIAL.
          wa_output-cust_pu = wa_output-tot_cd / wa_ekpo-menge.
        ENDIF.

        wa_output-land_cost = wa_output-tot_price - wa_output-tot_exc - wa_output-tot_cd.
        IF wa_ekpo-menge IS NOT INITIAL.
          wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
        ENDIF.

        IF r_gr = 'X'.
          IF wa_output-gr_menge IS NOT INITIAL.
            wa_output-land_costpu = wa_output-land_cost / wa_output-gr_menge.
          ENDIF.
        ELSE.
          IF wa_output-menge IS  NOT INITIAL.
            wa_output-land_costpu = wa_output-land_cost / wa_output-menge.
          ENDIF.
        ENDIF.

        APPEND wa_output TO it_output.
        CLEAR : wa_output, wa_ekpo.
      ENDIF.
    ENDLOOP.
    CLEAR : wa_output, wa_ekko.
  ENDLOOP.

  LOOP AT it_output INTO wa_output."where spart in i_tspat.   " modify by nk on 16.01.2017
    IF r_gr = 'X'.
      v_menge = wa_output-gr_menge.
    ELSE.
      v_menge = wa_output-menge.
    ENDIF.

    IF wa_output-waers <> 'INR' AND v_menge <> 0.
      wa_output-bedamt1 = wa_output-bedamt / v_menge.
      wa_output-ecsamt1 = wa_output-ecsamt / v_menge.
      wa_output-secs1 = wa_output-secs / v_menge.
      wa_output-incvd1 = wa_output-incvd / v_menge.

*      wa_output-incvd_prim = wa_output-incvd_prim * wa_output-wkurs.
      wa_output-incvd_prim1 = wa_output-incvd_prim / v_menge.

*      wa_output-incvd_sec = wa_output-incvd_sec * wa_output-wkurs.
      wa_output-incvd_sec1 = wa_output-incvd_sec / v_menge.

      wa_output-bcustom1 = wa_output-bcustom / v_menge.
      wa_output-educess_prim1 = wa_output-educess_prim / v_menge.
      wa_output-educess_sec1 = wa_output-educess_sec / v_menge.
      wa_output-custduty1 = wa_output-custduty / v_menge.
      wa_output-cha1 = wa_output-cha / v_menge.
      wa_output-landing1 = wa_output-landing / v_menge.
      wa_output-fri_amt1 = wa_output-fri_amt / v_menge.
      wa_output-octori1 = wa_output-octori / v_menge.
      wa_output-cst1 = wa_output-cst / v_menge.
      wa_output-vat1 = wa_output-vat / v_menge.
      wa_output-tot_price1 = wa_output-tot_price / v_menge.


      MODIFY it_output FROM wa_output.
    ENDIF.
    CLEAR: wa_output, v_menge.
  ENDLOOP.
ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDCAT
*&---------------------------------------------------------------------*
* Fills the fieldcatalog of the output table
*----------------------------------------------------------------------*
FORM fill_fieldcat .

  PERFORM get_fieldcat USING :
    'EBELN'     'IT_OUTPUT' 'EBELN' text-004,"12,
    'BEDAT'     'IT_OUTPUT' 'BEDAT' text-075,"10,
    'EBELP'     'IT_OUTPUT' 'EBELP' text-005,"03,
    'MATNR'     'IT_OUTPUT' 'MATNR' text-006,"12,
    'TXZ01'     'IT_OUTPUT' 'TXZ01' text-007,"20,
    'EKGRP'     'IT_OUTPUT' 'EKGRP' text-094," 16,
    'SPART'     'IT_OUTPUT' 'SPART' text-095," 08,
    'ZTERM'     'IT_OUTPUT' 'ZTERM' text-079,"04,
    'ZTAG1'     'IT_OUTPUT' 'ZTAG1' text-085,"04,
    'TEXT1'     'IT_OUTPUT' 'TEXT1' text-079."10.
  IF r_gr = 'X'.
    PERFORM get_fieldcat USING :
      'BELNR'  'IT_OUTPUT' 'BELNR' text-049,"10,
      'BUDAT'  'IT_OUTPUT' 'BUDAT' text-076,"10,
      'GJAHR'  'IT_OUTPUT' 'GJAHR' text-050,"04,
      'BUZEI'  'IT_OUTPUT' 'BUZEI' text-051,"03,
      'GR_MENGE' 'IT_OUTPUT' 'MENGE' text-052."10.

  ENDIF.
  PERFORM get_fieldcat USING :
      'LIFNR'     'IT_OUTPUT' 'LIFNR' text-010,"12,
      'NAME1'     'IT_OUTPUT' 'NAME1' text-011,"20,
      'WERKS'     'IT_OUTPUT' 'WERKS' text-012,"04,
      'MENGE'     'IT_OUTPUT' 'MENGE' text-008,"10,
      'MEINS'     'IT_OUTPUT' 'MEINS' text-009,"03,
      'PER_UNIT'  'IT_OUTPUT' 'WAERS' text-074,"05,
      'WAERS'     'IT_OUTPUT' 'WAERS' text-013."03.
*      'LCURR'     'EKKO' 'WAERS' text-015,
  IF r_gr = 'X'.
    PERFORM get_fieldcat USING :
      'WKURS'     'IT_OUTPUT' 'WKURS' text-077,"05,
      'KURSF'     'IT_OUTPUT' 'KURSF' text-078."05.
  ELSE.
    PERFORM get_fieldcat USING :
      'WKURS'     'IT_OUTPUT' 'WKURS' text-014."05.
  ENDIF.
  PERFORM get_fieldcat USING :
    'LCURR'     'IT_OUTPUT' 'LCURR' text-054,"10,
    'BASE_MAT'     'IT_OUTPUT' 'NETWR' text-055,"10,
*      'MPRICE'    'KONV' 'KWERT' text-016,
    'BEDAMT'    'IT_OUTPUT' 'KWERT' text-017,"10,
    'BEDAMT1'    'IT_OUTPUT' 'KWERT' text-056,"10,
    'ECSAMT'    'IT_OUTPUT' 'KWERT' text-018,"10,
    'ECSAMT1'    'IT_OUTPUT' 'KWERT' text-057,"10,
    'SECS'      'IT_OUTPUT' 'KWERT' text-019,"10,
    'SECS1'      'IT_OUTPUT' 'KWERT' text-058,"10,
    'INCVD'     'IT_OUTPUT' 'KWERT' text-021,"10,
    'INCVD1'     'IT_OUTPUT' 'KWERT' text-059,"10,
    'INCVD_PRIM'    'IT_OUTPUT' 'KWERT' text-022,"10,
    'INCVD_PRIM1'    'IT_OUTPUT' 'KWERT' text-060,"10,
    'INCVD_SEC'     'IT_OUTPUT' 'KWERT' text-023,"10,
    'INCVD_SEC1'     'IT_OUTPUT' 'KWERT' text-070,"10,
    'BCUSTOM'       'IT_OUTPUT' 'KWERT' text-020,"10,
    'BCUSTOM1'       'IT_OUTPUT' 'KWERT' text-061,"10,
    'EDUCESS_PRIM'  'IT_OUTPUT' 'KWERT' text-024,"10,
    'EDUCESS_PRIM1'  'IT_OUTPUT' 'KWERT' text-062,"10,
    'EDUCESS_SEC'   'IT_OUTPUT' 'KWERT' text-025,"10,
    'EDUCESS_SEC1'   'IT_OUTPUT' 'KWERT' text-063,"10,
    'CUSTDUTY'      'IT_OUTPUT' 'KWERT' text-026,"10,
    'CUSTDUTY1'      'IT_OUTPUT' 'KWERT' text-064,"10,
    'CHA'           'IT_OUTPUT' 'KWERT' text-027,"10,
    'CHA1'           'IT_OUTPUT' 'KWERT' text-065,"10,
    'LANDING'       'IT_OUTPUT' 'KWERT' text-028,"10,
    'LANDING1'       'IT_OUTPUT' 'KWERT' text-066,"10,
    'FRI_AMT'       'IT_OUTPUT' 'KWERT' text-029,"10,
    'FRI_AMT1'       'IT_OUTPUT' 'KWERT' text-067,"10,
    'OCTORI'        'IT_OUTPUT' 'KWERT' text-030,"10,
    'OCTORI1'        'IT_OUTPUT' 'KWERT' text-068,"10,
    'CST'           'IT_OUTPUT' 'KWERT' text-031,"10,
    'CST1'           'IT_OUTPUT' 'KWERT' text-069,"10,
    'VAT'           'IT_OUTPUT' 'KWERT' text-032,"10,
    'VAT1'           'IT_OUTPUT' 'KWERT' text-071,"10,
    'TOT_PRICE'     'IT_OUTPUT' 'KWERT' text-033,"10,
    'TOT_PRICE1'     'IT_OUTPUT' 'KWERT' text-072."10.

*  Import PO
  CLEAR wa_ekko.
  LOOP AT it_ekko INTO wa_ekko WHERE bsart CS 'IMP'.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    PERFORM get_fieldcat USING  'LCOSTPU' 'IT_OUTPUT' 'KWERT' text-053."10.
*    PERFORM get_fieldcat USING  'LCOSTPU' 'KONV' 'KWERT' text-053."10.
  ENDIF.
*  Service PO
  CLEAR wa_ekko.
  LOOP AT it_ekko INTO wa_ekko WHERE bsart CS 'SE' .
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    PERFORM get_fieldcat USING :
          'BSTAX' 'IT_OUTPUT' 'KWERT' text-082,"10,
          'ECSTAX' 'IT_OUTPUT' 'KWERT' text-083,"10,
          'SHETAX' 'IT_OUTPUT' 'KWERT' text-084."10.
  ENDIF.
  PERFORM get_fieldcat USING :
  'TOT_EXC'       'IT_OUTPUT' 'KWERT' text-035,"10,
  'CENVARE'       'IT_OUTPUT' 'KWERT' text-036,"10,
  'TOT_CD'        'IT_OUTPUT' 'KWERT' text-037,"10,
  'CUST_PU'       'IT_OUTPUT' 'KWERT' text-038,"10,
  'LAND_COST'     'IT_OUTPUT' 'KWERT' text-034,"10,
  'LAND_COSTPU'   'IT_OUTPUT' 'KWERT' text-073,"10,
  'RLWRT' 'IT_OUTPUT' 'RLWRT' text-080,"10.
  'ZBPL' 'IT_OUTPUT' 'ZBPL' text-086,"10.
  'ZBPP' 'IT_OUTPUT' 'ZBPP' text-087,"10.
  'ZBPC' 'IT_OUTPUT' 'ZBPC' text-088,"10.
  'ZBPL_VARIANCE' 'IT_OUTPUT' 'ZBPL_VARIANCE' text-089,"10.
  'ZBPP_VARIANCE' 'IT_OUTPUT' 'ZBPP_VARIANCE' text-090,"10.
  'ZBPC_VARIANCE' 'IT_OUTPUT' 'ZBPC_VARIANCE' text-091,"10.
  'AVG_CRVAL' 'IT_OUTPUT' 'AVG_CRVAL' text-092,"10.
  'CR_DAYS' 'IT_OUTPUT' 'CR_DAYS' text-093."10.
  IF r_gr = 'X'.
    PERFORM get_fieldcat USING :
          'GRR_BAS' 'IT_OUTPUT' 'KWERT' text-081."10.
  ENDIF.
ENDFORM.                    " FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
* Display output
*----------------------------------------------------------------------*
FORM display_data .
  layout-zebra        = 'X'. "striped pattern
  PERFORM headder.
****** Start Code: Added by CS on 07.01.2016 for Footer. *****
*  wa_events-name = 'END_OF_LIST'.
*  wa_events-form = 'FOOTER'.
*  APPEND wa_events TO it_events.
*  CLEAR wa_events.
****** End Code: Added by CS on 07.01.2016 for Footer. *****
**********  added by nk on 16.01.2017
  CLEAR: wa_tspat.
  LOOP AT i_tspat INTO wa_tspat.
    LOOP AT it_output INTO wa_output WHERE spart EQ wa_tspat-spart.
      MOVE-CORRESPONDING wa_output TO wa_out_10.
      APPEND wa_out_10 TO it_out_10.
      CLEAR: wa_out_10.
    ENDLOOP.
  ENDLOOP.
**********  added by nk on 16.01.2017
*  layout-colwidth_optimize = 'X'.
  IF it_out_10 IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
        i_callback_program                = sy-repid
        is_layout                         = layout
        it_fieldcat                       = it_fieldcat
*      i_callback_user_command           = 'USER_COMMAND'
*      i_callback_top_of_page            = 'TOP_OF_PAGE'
*      i_background_id                   = 'ALV_BACKGROUND'
*      is_layout                         = st_layout
*      IT_EXCLUDING                      =
*      IT_SPECIAL_GROUPS                 =
*      IT_SORT                           =
*      IT_FILTER                         =
*      IS_SEL_HIDE                       =
        i_default                         = 'A'
        i_save                            = g_save
        is_variant                        = g_variant
        it_events                         = it_events
      TABLES
        t_outtab                          = it_out_10"it_output
     EXCEPTIONS
        program_error                     = 1
       OTHERS                             = 2
              .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.                   "layo
  ELSE.
    MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.
*  CALL SCREEN 100.
ENDFORM.                    " DISPLAY_DATA
**&---------------------------------------------------------------------*
**&      Form  footer
**&---------------------------------------------------------------------*
**       Added by CS on 07.01.2016
**----------------------------------------------------------------------*
*FORM footer." USING document TYPE REF TO cl_dd_document.
*
*  DATA : wa_footer TYPE slis_listheader,
*         i_footer TYPE slis_t_listheader.
*  DATA: lv_crdays_txt TYPE string.
*
*CONCATENATE 'Credit Days: ' lv_txt_crdays INTO lv_crdays_txt SEPARATED BY space.
*
*  wa_footer-typ = 'S'.
*  wa_footer-info = lv_crdays_txt.
*  APPEND wa_footer TO i_footer.
*  CLEAR wa_footer.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      it_list_commentary = i_footer.
*
*ENDFORM.                    "FOOTER
*&---------------------------------------------------------------------*
*&      Form  GET_FIELDCAT
*&---------------------------------------------------------------------*
* Fills the field catalog
*----------------------------------------------------------------------*
*      -->p_dataname output field name
*      -->p_tabname reference DB table name
*      -->P_fieldname refer DB field name
*      -->P_coltext column heading
*----------------------------------------------------------------------*
FORM get_fieldcat  USING    p_dataname  "TYPE LVC_S_FCAT-FIELDNAME
                            p_tabname   "TYPE LVC_S_FCAT-REF_TABLE
                            p_fieldname "TYPE LVC_S_FCAT-REF_FIELD
                            p_heading.   "TYPE LVC_S_FCAT-COLTEXT.
*                            a_outputlen.


  IF p_fieldname = 'LCURR'.
    CLEAR wa_fieldcat.
    wa_fieldcat-fieldname   = 'LCURR'.
    wa_fieldcat-tabname = p_tabname.
    wa_fieldcat-seltext_l     = p_heading.
*    WA_FIELDCAT-TOOLTIP     = P_COLTEXT.
*    WA_FIELDCAT-OUTPUTLEN = A_OUTPUTLEN.
*    WA_FIELDCAT-DO_SUM = 'X'.

    wa_fieldcat-qfieldname = 'LCURR'.
    wa_fieldcat-cfieldname    = 'WAERS'.
    APPEND wa_fieldcat TO it_fieldcat.
  ELSEIF p_fieldname = 'RLWRT'.
    CLEAR wa_fieldcat.
    wa_fieldcat-fieldname   = p_dataname.
    wa_fieldcat-tabname = p_tabname.
    wa_fieldcat-seltext_l     = p_heading.
*    WA_FIELDCAT-COLTEXT     = P_COLTEXT.
*    WA_FIELDCAT-TOOLTIP     = P_COLTEXT.
*    WA_FIELDCAT-REF_TABLE   = P_TABNAME.
*    WA_FIELDCAT-REF_FIELD   = P_FIELDNAME.

*    WA_FIELDCAT-OUTPUTLEN = A_OUTPUTLEN.
    wa_fieldcat-no_out = 'X'.

    APPEND wa_fieldcat TO it_fieldcat.
  ELSE.
    CLEAR wa_fieldcat.
    wa_fieldcat-fieldname   = p_dataname.
    wa_fieldcat-tabname = p_tabname.
    wa_fieldcat-seltext_l     = p_heading.
*    WA_FIELDCAT-REF_TABLE   = P_TABNAME.
*    WA_FIELDCAT-REF_FIELD   = P_FIELDNAME.

*    WA_FIELDCAT-OUTPUTLEN = A_OUTPUTLEN.

*    WA_FIELDCAT-DO_SUM = 'X'.
    APPEND wa_fieldcat TO it_fieldcat.
  ENDIF.


ENDFORM.                    " GET_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
* details in the Top of page
*----------------------------------------------------------------------*
FORM top_of_page .
  CONSTANTS: lc_ind(100) TYPE c VALUE 'INDOFIL INDUSTRIES LTD.,'.
  DATA: l_date TYPE sy-datum,
        l_datum TYPE sdydo_text_element.
  l_date = sy-datum.
  CONCATENATE l_date+6(2) l_date+4(2) l_date+0(4) INTO l_datum
    SEPARATED BY '/'.

  PERFORM add_text USING lc_ind.
  CALL METHOD w_document->new_line .
  PERFORM add_text USING text-040.
  CALL METHOD w_document->add_gap
    EXPORTING
      width = 10.
  PERFORM add_text USING l_datum.

  CALL METHOD w_document->new_line .
* display the data
  CALL METHOD w_document->display_document
    EXPORTING
      parent = w_top_container.
ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_vale  text that needs to be printed
*----------------------------------------------------------------------*
FORM add_text  USING    p_value .                           "#EC *

  DATA: l_cid   TYPE sdydo_text_element.
* Passing the value
  l_cid = p_value.
*calling the methods for dynamic text
  CALL METHOD w_document->add_text
    EXPORTING
      text = l_cid.

ENDFORM.                    " ADD_TEXT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
* Creates custom container and grid for display
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*  SET PF-STATUS 'PF_STATUS'.
*  SET TITLEBAR 'TITLE'.
**  Checks the container is already created
*  IF W_CONTAINER IS INITIAL.
** Creates container
*    CREATE OBJECT W_CONTAINER
*      EXPORTING
*        CONTAINER_NAME = 'CONTAINER'.
** Splits the container
*    IF CL_GUI_ALV_GRID=>OFFLINE( ) IS INITIAL.
*      CREATE OBJECT W_SPLIT_CON
*        EXPORTING
*          PARENT        = W_CONTAINER
*          SASH_POSITION = 15
*          WITH_BORDER   = 2.
*    ELSE.
*      CREATE OBJECT W_DOC
*        EXPORTING
*          REPID     = SY-REPID
*          DYNNR     = SY-DYNNR
*          SIDE      = W_DOC->DOCK_AT_LEFT
*          EXTENSION = 500.
*
*      CREATE OBJECT W_SPLIT_CON
*        EXPORTING
*          PARENT        = W_DOC
*          SASH_POSITION = 15
*          WITH_BORDER   = 2.
*    ENDIF.
*
**   Placing the containers in the splitter
*    W_TOP_CONTAINER = W_SPLIT_CON->TOP_LEFT_CONTAINER .
*    W_BOTTOM_CONTAINER = W_SPLIT_CON->BOTTOM_RIGHT_CONTAINER .
**   Creating Grid
*    CREATE OBJECT W_GRID
*      EXPORTING
*        I_PARENT = W_BOTTOM_CONTAINER.
*
**   Creating the document
*    CREATE OBJECT W_DOCUMENT
*      EXPORTING
*        STYLE = 'CONTAINER'.
** top of page
*    PERFORM TOP_OF_PAGE.
*
** Calling the method of ALV to process top of page
*    CALL METHOD W_GRID->LIST_PROCESSING_EVENTS
*      EXPORTING
*        I_EVENT_NAME = 'TOP_OF_PAGE'
*        I_DYNDOC_ID  = W_DOCUMENT.
*
*    CALL METHOD W_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*      CHANGING
*        IT_OUTTAB                     = IT_OUTPUT
*        IT_FIELDCATALOG               = IT_FIELDCAT
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*  ELSE.
*    CALL METHOD W_GRID->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = IT_FIELDCAT.
*
*    CALL METHOD W_GRID->REFRESH_TABLE_DISPLAY
*      EXCEPTIONS
*        FINISHED = 1
*        OTHERS   = 2.
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*
*  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code_100.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_INT_TABLES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_int_tables .
  REFRESH: it_ekko, it_ekpo, it_lifnr, it_konv, it_fieldcat, it_ekbe.

  CLEAR: wa_ekko, wa_ekpo, wa_lifnr, wa_konv, wa_fieldcat,wa_ekbe.
ENDFORM.                    " REFRESH_INT_TABLES
*&---------------------------------------------------------------------*
*&      Form  GET_CONDITION_VALUE
*&---------------------------------------------------------------------*
* Changes the condition value using the FM Z6MM_PO_CAL_TAX
*----------------------------------------------------------------------*
FORM get_condition_value .
  DATA: lit_komv TYPE komv_itab,
        lwa_komv TYPE komv.
  DATA: lv_ekbe_flg TYPE c. " Flg for EKBE - Added by CS on 28.12.2015

*  Condition value for taxes cannot be obtained directly from KONV
*  it will be obtained using FM Z6MM_PO_CAL_TAX
  IF it_ekpo[] IS NOT INITIAL.
    SORT it_ekpo BY ebeln ebelp.
*    LOOP AT it_ekko INTO wa_ekko.
    LOOP AT it_ekpo INTO wa_ekpo." WHERE ebeln = wa_ekko-ebeln.
      CLEAR: lv_ekbe_flg.
***** Start Code : Added by CS on 28.12.2015 for Code Optimization. *****
      IF r_gr = 'X'.
        SORT it_ekbe BY ebeln ebelp.
        READ TABLE it_ekbe INTO wa_ekbe WITH KEY ebeln = wa_ekpo-ebeln
                                                 ebelp = wa_ekpo-ebelp
                                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          lv_ekbe_flg = 'X'.
        ENDIF.
      ELSE.
        lv_ekbe_flg = 'X'.
      ENDIF.
***** End Code : Added by CS on 28.12.2015 for Code Optimization. *****

      REFRESH lit_komv.
      IF lv_ekbe_flg EQ 'X'.   " Added by CS on 28.12.2015 for Code Optimization

        CALL FUNCTION 'Z6MM_PO_CAL_TAX'
          EXPORTING
            i_ebeln = wa_ekpo-ebeln
            i_ebelp = wa_ekpo-ebelp
          IMPORTING
            tt_komv = lit_komv.

        READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln.
        IF sy-subrc = 0.
          lwa_komv-knumv = wa_ekko-knumv.
        ENDIF.

        MODIFY lit_komv FROM lwa_komv TRANSPORTING knumv
                                      WHERE kposn = wa_ekpo-ebelp.
        APPEND LINES OF lit_komv TO it_komv.

        CLEAR: lwa_komv, wa_ekpo, wa_ekko.
      ENDIF.  " Added by CS on 28.12.2015 for Code Optimization

    ENDLOOP.
*      CLEAR wa_ekko.
*    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_CONDITION_VALUE
*&---------------------------------------------------------------------*
*&      Form  FILL_PRICING_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_pricing_details .
  LOOP AT it_konv INTO wa_konv WHERE knumv = wa_ekko-knumv AND
                                           kposn = wa_ekpo-ebelp.
*        Get condition value
    CASE wa_konv-kschl.
*      WHEN 'PBXX' OR 'PB00' OR 'P001' OR 'P000'.
*        wa_output-mprice = wa_konv-kwert.
      WHEN 'JCDB'.
        wa_output-bcustom  = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-bcustom1  = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-bcustom1.
        ENDIF.
      WHEN 'JCV1'.
        wa_output-incvd = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-incvd1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-incvd1.
        ENDIF.
      WHEN 'JECV'.
        wa_output-incvd_prim = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-incvd_prim1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-incvd_prim1.
        ENDIF.
      WHEN 'J1CV'.
        wa_output-incvd_sec = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-incvd_sec1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-incvd_sec1.
        ENDIF.
      WHEN 'JCE1'.
        wa_output-educess_prim = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-educess_prim1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR: wa_output-educess_prim1.
        ENDIF.
      WHEN 'JCE2'.
        wa_output-educess_sec = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-educess_sec1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-educess_sec1.

        ENDIF.
      WHEN 'JADC'.
        IF wa_ekko-bsart = 'ZIMP'.

          wa_output-custduty = wa_konv-kwert.
          IF wa_output-menge IS NOT INITIAL.
            wa_output-custduty1 = wa_konv-kwert / wa_output-menge.
          ELSE.
            CLEAR : wa_output-custduty1 .
          ENDIF.
        ENDIF.
      WHEN 'ZCH2' OR 'ZCH3'.
        wa_output-cha = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-cha1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-cha1 .
        ENDIF.
      WHEN 'JAVC'.
        wa_output-landing = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-landing1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR: wa_output-landing1.
        ENDIF.

      WHEN 'FRB1' OR 'FRC1'.
        wa_output-fri_amt = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-fri_amt1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR  wa_output-fri_amt1 .
        ENDIF.
      WHEN 'JOCM' OR 'ZJO2'.
        wa_output-octori = wa_konv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-octori1 = wa_konv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-octori1.
        ENDIF.
***** Start Code: Added by CS on 07.01.2016 *****
      WHEN 'ZBPL'.
        wa_output-zbpl = wa_konv-kbetr.
      WHEN 'ZBPP'.
        wa_output-zbpp = wa_konv-kbetr.
        IF wa_output-zbpp IS NOT INITIAL.
          wa_output-zbpp_variance = wa_output-zbpp - wa_output-per_unit.
        ENDIF.
      WHEN 'ZBPC'.
        wa_output-zbpc = wa_konv-kbetr.
        IF wa_output-zbpc IS NOT INITIAL.
          wa_output-zbpc_variance = wa_output-zbpc - wa_output-per_unit.
        ENDIF.

***** End Code: Added by CS on 07.01.2016 *****
      WHEN OTHERS.
    ENDCASE.
    CLEAR wa_konv.
  ENDLOOP.
  LOOP AT it_komv INTO wa_komv WHERE knumv = wa_ekko-knumv AND
                                     kposn = wa_ekpo-ebelp.
*        Get tax code
    CASE wa_komv-kschl.
      WHEN 'JMOP'.
        wa_output-bedamt = wa_komv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-bedamt1 = wa_komv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-bedamt1.
        ENDIF.
      WHEN 'JEC1'.
        wa_output-ecsamt = wa_komv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-ecsamt1 = wa_komv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-ecsamt1.
        ENDIF.
      WHEN 'JSEP'.
        wa_output-secs = wa_komv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-secs1 = wa_komv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-secs1.
        ENDIF.
      WHEN 'JVCS'.
        wa_output-cst = wa_komv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-cst1 = wa_komv-kwert / wa_output-menge.
        ELSE.
          CLEAR wa_output-cst1.
        ENDIF.
      WHEN 'JVRD'.
        wa_output-vat = wa_komv-kwert.
        IF wa_output-menge IS NOT INITIAL.
          wa_output-vat1 = wa_komv-kwert / wa_output-menge.
        ELSE.
          CLEAR  wa_output-vat1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    CLEAR wa_komv.
  ENDLOOP.

ENDFORM.                    " FILL_PRICING_DETAILS
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PRICE_GR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_price_gr.
  PERFORM change_price :
*                   CHANGING wa_output-mprice ,
                   CHANGING wa_output-bcustom ,
                   CHANGING wa_output-incvd ,
                   CHANGING wa_output-incvd_prim ,
                   CHANGING wa_output-incvd_sec ,
                   CHANGING wa_output-educess_prim ,
                   CHANGING wa_output-educess_sec ,
                   CHANGING wa_output-custduty ,
                   CHANGING wa_output-cha ,
                   CHANGING wa_output-landing ,
                   CHANGING wa_output-fri_amt ,
                   CHANGING wa_output-octori ,
                   CHANGING wa_output-bedamt ,
                   CHANGING wa_output-ecsamt ,
                   CHANGING wa_output-secs ,
                   CHANGING wa_output-cst ,
                   CHANGING wa_output-vat .
ENDFORM.                    " CHANGE_PRICE_GR
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PRICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_OUTPUT_MPRICE  text
*----------------------------------------------------------------------*
FORM change_price CHANGING p_price.
  IF wa_output-menge IS  NOT INITIAL.
    p_price = ( p_price * wa_output-gr_menge ) / wa_output-menge.
  ENDIF.
ENDFORM.                    " CHANGE_PRICE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PRICE_IMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_price_imp .
  PERFORM change_cost :
*                     CHANGING wa_output-mprice ,
                     CHANGING wa_output-bcustom ,
                     CHANGING wa_output-incvd ,
                     CHANGING wa_output-incvd_prim ,
                     CHANGING wa_output-incvd_sec ,
                     CHANGING wa_output-educess_prim ,
                     CHANGING wa_output-educess_sec ,
                     CHANGING wa_output-custduty ,
                     CHANGING wa_output-cha ,
                     CHANGING wa_output-landing ,
                     CHANGING wa_output-fri_amt ,
                     CHANGING wa_output-octori ,
                     CHANGING wa_output-bedamt ,
                     CHANGING wa_output-ecsamt ,
                     CHANGING wa_output-secs ,
                     CHANGING wa_output-cst ,
                     CHANGING wa_output-vat .
ENDFORM.                    " CHANGE_PRICE_IMP
*&---------------------------------------------------------------------*
*&      Form  CHANGE_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_OUTPUT_MPRICE  text
*----------------------------------------------------------------------*
FORM change_cost  CHANGING p_price.
  p_price = p_price * wa_ekko-wkurs.
ENDFORM.                    " CHANGE_COST
*&---------------------------------------------------------------------*
*&      Form  GET_PRICING_FOR_SERVICEPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pricing_for_servicepo .
  LOOP AT it_komv INTO wa_komv WHERE knumv = wa_ekko-knumv AND
                                       kposn = wa_ekpo-ebelp.
*        Get tax code
    CASE wa_komv-kschl.
      WHEN 'JSER'.
        wa_output-bstax = wa_komv-kwert.
      WHEN 'JSE3'.
        wa_output-ecstax = wa_komv-kwert.
      WHEN 'JSE4'.
        wa_output-shetax = wa_komv-kwert.
      WHEN OTHERS.
    ENDCASE.
    CLEAR wa_komv.
  ENDLOOP.
ENDFORM.                    " GET_PRICING_FOR_SERVICEPO
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA_GL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_data_gl .

  DATA: lv_ekbe_ind TYPE i, " Index for EKBE  - Added by CS on 18.12.2015
        lv_ekbz_ind TYPE i, " Index for EKBZ  - Added by CS on 18.12.2015
        lv_ekbesto_ind TYPE i,  " Index for EKBE STO  - Added by CS on 18.12.2015
        lv_ekbzsto_ind TYPE i.  " Index for EKBZ STO  - Added by CS on 18.12.2015
  CLEAR: lv_tot_lcost, lv_tot_avgcr.
  LOOP AT it_ekpo INTO wa_ekpo.

***** Start Code : Added by CS on 21.12.2015 for Code Optimization. *****
    CLEAR: lv_ekbe_ind, lv_ekbz_ind, lv_ekbesto_ind, lv_ekbzsto_ind.

    SORT it_ekbe BY ebeln ebelp.
    READ TABLE it_ekbe INTO wa_ekbe WITH KEY ebeln = wa_ekpo-ebeln
                                             ebelp = wa_ekpo-ebelp
                                             BINARY SEARCH.
    IF sy-subrc EQ 0.
      lv_ekbe_ind = sy-tabix.
    ENDIF.
***** End Code : Added by CS on 21.12.2015 for Code Optimization. *****

    IF lv_ekbe_ind IS NOT INITIAL.
      READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_output-ebeln = wa_ekko-ebeln.
        wa_output-bedat = wa_ekko-bedat.
        wa_output-lifnr = wa_ekko-lifnr.
        wa_output-waers = wa_ekko-waers.
        wa_output-wkurs = wa_ekko-wkurs.
        wa_output-rlwrt = wa_ekko-rlwrt.
        wa_output-ekgrp = wa_ekko-ekgrp.  " Purchasing Group Added by CS on 30.03.2016

        wa_output-zterm = wa_ekko-zterm.
***** Start Code: Added by CS on 07.01.2016 for Payterm Days. *****
        SELECT SINGLE ztag1
          FROM t052
          INTO wa_output-ztag1  " Payterm Days
          WHERE zterm EQ wa_output-zterm.
***** End Code: Added by CS on 07.01.2016 for Payterm Days. *****
        SELECT SINGLE text1
          FROM t052u
          INTO wa_output-text1
          WHERE spras = sy-langu
          AND zterm = wa_output-zterm.
      ENDIF.

      wa_output-ebelp = wa_ekpo-ebelp.
      wa_output-matnr = wa_ekpo-matnr.
      wa_output-txz01 = wa_ekpo-txz01.
      wa_output-menge = wa_ekpo-menge.
      wa_output-meins = wa_ekpo-meins.

      CLEAR wa_lifnr.
      READ TABLE it_lifnr INTO wa_lifnr WITH KEY lifnr = wa_ekko-lifnr.
      IF sy-subrc = 0.
        wa_output-name1 = wa_lifnr-name1.
      ENDIF.
      wa_output-werks  = wa_ekpo-werks.
      wa_output-per_unit = wa_ekpo-netpr.

***** Start Code: For Division - Added by CS on 30.03.2016. *****
      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_ekpo-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_output-spart = wa_mara-spart.  " Division
      ENDIF.
***** End Code: For Division - Added by CS on 30.03.2016. *****

* if PO type other than service PO get the general pricing details
      IF ( wa_ekko-bsart CS 'SE' ).
* Only few pricing details are required for service PO
        PERFORM get_pricing_for_servicepo.
      ELSE.
        PERFORM fill_pricing_details.
        PERFORM change_price_imp.
      ENDIF.

*   If the report is GR based, then the pricing should be converted for
*   GR quantity
      DATA : w_sum TYPE ekbe-menge.
      CLEAR w_sum.

      LOOP AT it_ekbe INTO wa_ekbe FROM lv_ekbe_ind. "ebeln = wa_ekpo-ebeln AND
        "ebelp = wa_ekpo-ebelp.
        IF wa_ekbe-ebeln NE wa_ekpo-ebeln OR wa_ekbe-ebelp NE wa_ekpo-ebelp .
          EXIT.
        ENDIF.
        wa_output-gjahr    = wa_ekbe-gjahr.
        wa_output-belnr    = wa_ekbe-belnr.
        wa_output-budat    = wa_ekbe-budat.
        wa_output-buzei    = wa_ekbe-buzei.
        wa_output-gr_menge = wa_ekbe-menge .
        wa_output-lcurr    = wa_ekbe-dmbtr.        "Base Net Unit
        wa_output-shkzg = wa_ekbe-shkzg.
        CONCATENATE wa_output-belnr wa_output-gjahr INTO wa_output-awkey.

        READ TABLE it_j_1igrxref INTO wa_j_1igrxref WITH KEY mblnr = wa_ekbe-belnr zeile = wa_ekbe-buzei.
        IF sy-subrc = 0.
          IF wa_ekko-bsart <> 'ZIMP'.
            wa_output-bedamt = wa_j_1igrxref-exbed.
            wa_output-ecsamt = wa_j_1igrxref-ecs.
            wa_output-secs   = wa_j_1igrxref-exaddtax1.
          ELSEIF wa_ekko-bsart = 'ZIMP'.
            wa_output-incvd      = wa_j_1igrxref-exbed.
            wa_output-incvd_prim = wa_j_1igrxref-ecs.
            wa_output-incvd_sec  = wa_j_1igrxref-exaddtax1.
            wa_output-custduty = wa_j_1igrxref-exaed.
*          wa_output-
          ENDIF.
        ENDIF.
*        IF WA_EKKO-BSART <> 'ZIMP'.
*          CLEAR: WA_OUTPUT-CUSTDUTY.
*        ENDIF.
*
*        IF WA_EKKO-BSART <> 'ZSTO'.
*          CLEAR : WA_OUTPUT-FRI_AMT, WA_OUTPUT-OCTORI.
*        ENDIF.

        CLEAR: wa_output-bcustom , wa_output-educess_prim, wa_output-educess_sec,
               wa_output-cha, wa_output-landing , wa_output-custduty , wa_output-fri_amt, wa_output-octori.

        IF wa_ekko-bsart = 'ZSTO'.
          LOOP AT it_ekbe_sto INTO wa_ekbe_sto WHERE  ebeln = wa_ekpo-ebeln AND
                                                      ebelp = wa_ekpo-ebelp AND
                                                      menge = wa_ekbe-menge.
***** Start Code: Added by CS on 21.12.2015 for Code Optimization *****
            CLEAR: lv_ekbzsto_ind.
            CLEAR: wa_ekbz_sto.
            SORT it_ekbz_sto BY belnr buzei.

            READ TABLE it_ekbz_sto INTO wa_ekbz_sto WITH KEY "ebeln = wa_ekbe_sto-ebeln
                                                             belnr = wa_ekbe_sto-belnr
                                                             buzei = wa_ekbe_sto-buzei.
            IF sy-subrc EQ 0.
              lv_ekbzsto_ind = sy-tabix.
            ENDIF.

            CLEAR: wa_ekbz_sto.
            IF lv_ekbzsto_ind IS NOT INITIAL.
              LOOP AT it_ekbz_sto INTO wa_ekbz_sto FROM lv_ekbzsto_ind.
                IF wa_ekbz_sto-belnr NE wa_ekbe_sto-belnr OR wa_ekbz_sto-buzei NE wa_ekbe_sto-buzei..
                  EXIT.
                ENDIF.
***** End Code: Added by CS on 21.12.2015 for Code Optimization *****

*          LOOP AT it_ekbz_sto INTO wa_ekbz_sto WHERE  belnr = wa_ekbe_sto-belnr
*                                                  AND buzei = wa_ekbe_sto-buzei .

                CASE wa_ekbz_sto-kschl.
                  WHEN 'JCDB'.
                    wa_output-bcustom         = wa_ekbz_sto-dmbtr.
                  WHEN 'JCE1'.
                    wa_output-educess_prim    = wa_ekbz_sto-dmbtr.
                  WHEN 'JCE2'.
                    wa_output-educess_sec     = wa_ekbz_sto-dmbtr.
                  WHEN 'JADC'.
                    wa_output-custduty        = wa_ekbz_sto-dmbtr.
                  WHEN 'ZCH2' OR 'ZCH3'.
                    wa_output-cha             = wa_ekbz_sto-dmbtr.
                  WHEN 'JAVC'.
                    wa_output-landing         = wa_ekbz_sto-dmbtr.
                  WHEN 'FRB1' OR 'FRC1'.
                    wa_output-fri_amt         = wa_ekbz_sto-dmbtr.
                  WHEN 'JOCM' OR 'ZJO2'.
                    wa_output-octori          = wa_ekbz_sto-dmbtr.
                  WHEN OTHERS.
                ENDCASE.
                CLEAR: wa_ekbz_sto.
              ENDLOOP.
            ENDIF.
            CLEAR: wa_ekbe_sto.
          ENDLOOP.
        ELSE.
***** Start Code: Added by CS on 21.12.2015 for Code Optimization *****
          CLEAR: lv_ekbz_ind.
          CLEAR: wa_ekbz.

          SORT it_ekbz BY belnr buzei.
          READ TABLE it_ekbz INTO wa_ekbz WITH KEY "ebeln = wa_ekbe-ebeln
                                                   belnr = wa_ekbe-belnr
                                                   buzei = wa_ekbe-buzei.
          IF sy-subrc EQ 0.
            lv_ekbz_ind = sy-tabix.
          ENDIF.
          CLEAR: wa_ekbz.
          IF lv_ekbz_ind IS NOT INITIAL.
            LOOP AT it_ekbz INTO wa_ekbz FROM lv_ekbz_ind.
              IF wa_ekbz-belnr NE wa_ekbe-belnr OR wa_ekbz-buzei NE wa_ekbe-buzei..
                EXIT.
              ENDIF.
***** End Code: Added by CS on 21.12.2015 for Code Optimization *****

*        LOOP AT it_ekbz INTO wa_ekbz WHERE belnr = wa_ekbe-belnr AND buzei = wa_ekbe-buzei.
              CASE wa_ekbz-kschl.
                WHEN 'JCDB'.
                  wa_output-bcustom         = wa_ekbz-dmbtr.
                WHEN 'JCE1'.
                  wa_output-educess_prim    = wa_ekbz-dmbtr.
                WHEN 'JCE2'.
                  wa_output-educess_sec     = wa_ekbz-dmbtr.
                WHEN 'JADC'.
                  wa_output-custduty        = wa_ekbz-dmbtr.
                WHEN 'ZCH2' OR 'ZCH3'.
                  wa_output-cha             = wa_ekbz-dmbtr.
                WHEN 'JAVC'.
                  wa_output-landing         = wa_ekbz-dmbtr.
                WHEN 'FRB1' OR 'FRC1'.
                  wa_output-fri_amt         = wa_ekbz-dmbtr.
                WHEN 'JOCM' OR 'ZJO2'.
                  wa_output-octori          = wa_ekbz-dmbtr.
                WHEN OTHERS.
              ENDCASE.
              CLEAR: wa_ekbz.
            ENDLOOP.
          ENDIF.
        ENDIF.
*          wa_output-lcurr = wa_ekbe-menge * wa_ekpo-netpr * wa_ekko-wkurs.
        CLEAR w_sum.

        wa_output-tot_exc = wa_output-bedamt + wa_output-ecsamt + wa_output-secs.
        TRY.
            wa_output-cenvare = wa_output-tot_exc / wa_output-gr_menge."WA_EKPO-MENGE.
          CATCH cx_sy_arithmetic_error .
            wa_output-cenvare = 0.
        ENDTRY.

        wa_output-tot_cd = wa_output-incvd +
                           wa_output-incvd_prim +
                           wa_output-incvd_sec +
                           wa_output-custduty.

*        CLEAR: WA_OUTPUT-VAT, WA_OUTPUT-CST.  "VAT & CST already included in RM, so clear the amount and display zero in resp columns

        wa_output-tot_price =  wa_output-lcurr + wa_output-bedamt + wa_output-ecsamt + wa_output-secs + wa_output-incvd + wa_output-incvd_prim  + wa_output-incvd_sec +
                             wa_output-bcustom + wa_output-educess_prim + wa_output-educess_sec  + wa_output-custduty + wa_output-cha + wa_output-landing +
                             wa_output-fri_amt + wa_output-octori." + WA_OUTPUT-CST + WA_OUTPUT-VAT.

        IF wa_ekko-bsart = 'ZSTO'.

          SELECT SINGLE dmbtr FROM ekbe INTO wa_output-lcurr
            WHERE ebeln = wa_output-ebeln
            AND ebelp = wa_output-ebelp
            AND menge = wa_output-gr_menge
            AND bewtp = 'U'.
          wa_output-land_cost = wa_output-lcurr + wa_output-fri_amt + wa_output-octori.
*        WA_OUTPUT-LAND_COST = WA_OUTPUT-LAND_COSTPU * WA_OUTPUT-GR_MENGE.
        ELSE.
          wa_output-land_cost = wa_output-tot_price - wa_output-tot_exc - wa_output-tot_cd.
        ENDIF.
        wa_output-grr_bas = wa_output-per_unit * wa_output-gr_menge.

        APPEND wa_output TO it_output.
      ENDLOOP.
    ENDIF.

    CLEAR: wa_ekpo, wa_ekko, wa_ekbe, wa_output, wa_mara.
  ENDLOOP.

*****  LOOP AT it_ekko INTO wa_ekko.
*****    LOOP AT it_ekpo INTO wa_ekpo WHERE ebeln = wa_ekko-ebeln.
*****      wa_output-ebeln = wa_ekko-ebeln.
*****      wa_output-bedat = wa_ekko-bedat.
*****      wa_output-ebelp = wa_ekpo-ebelp.
*****      wa_output-matnr = wa_ekpo-matnr.
*****      wa_output-txz01 = wa_ekpo-txz01.
*****      wa_output-menge = wa_ekpo-menge.
*****      wa_output-meins = wa_ekpo-meins.
*****      wa_output-lifnr = wa_ekko-lifnr.
*****      wa_output-zterm = wa_ekko-zterm.
*****      wa_output-rlwrt = wa_ekko-rlwrt.
*****
*****      SELECT SINGLE text1
*****        FROM t052u
*****        INTO wa_output-text1
*****        WHERE spras = sy-langu
*****        AND zterm = wa_output-zterm.
*****
*****      CLEAR wa_lifnr.
*****      READ TABLE it_lifnr INTO wa_lifnr WITH KEY lifnr = wa_ekko-lifnr.
*****      IF sy-subrc = 0.
*****        wa_output-name1 = wa_lifnr-name1.
*****      ENDIF.
*****      wa_output-werks  = wa_ekpo-werks.
*****      wa_output-waers = wa_ekko-waers.
*****      wa_output-wkurs = wa_ekko-wkurs.
*****
******      IF WA_EKKO-BSART = 'ZSTO'.
******        WA_OUTPUT-LCURR = WA_EKPO-MENGE * WA_EKPO-NETPR * WA_EKKO-WKURS.
******      ENDIF.
******      wa_output-lcurr = wa_ekpo-menge * wa_ekpo-netpr * wa_ekko-wkurs.
******      wa_output-base_mat = wa_ekpo-netpr * wa_ekko-wkurs.
*****      wa_output-per_unit = wa_ekpo-netpr.
*****
****** if PO type other than service PO get the general pricing details
*****      IF ( wa_ekko-bsart CS 'SE' ).
****** Only few pricing details are required for service PO
*****        PERFORM get_pricing_for_servicepo.
*****      ELSE.
*****        PERFORM fill_pricing_details.
*****        PERFORM change_price_imp.
*****      ENDIF.
*****
*****
******      wa_output-tot_price =  wa_output-lcurr + wa_output-bedamt + wa_output-ecsamt + wa_output-secs + wa_output-incvd + wa_output-incvd_prim  + wa_output-incvd_sec +
******                           wa_output-bcustom + wa_output-educess_prim + wa_output-educess_sec  + wa_output-custduty + wa_output-cha + wa_output-landing +
******                           wa_output-fri_amt + wa_output-octori + wa_output-cst + wa_output-vat.
******
******      wa_output-tot_price1 = wa_output-tot_price / wa_output-menge.
*****
*****
******   If the report is GR based, then the pricing should be converted for
******   GR quantity
*****      DATA : w_sum TYPE ekbe-menge.
*****      CLEAR w_sum.
*****      LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln = wa_ekpo-ebeln AND
*****                                         ebelp = wa_ekpo-ebelp.
*****        wa_output-gjahr    = wa_ekbe-gjahr.
*****        wa_output-belnr    = wa_ekbe-belnr.
*****        wa_output-budat    = wa_ekbe-budat.
*****        wa_output-buzei    = wa_ekbe-buzei.
*****        wa_output-gr_menge = wa_ekbe-menge .
*****        wa_output-lcurr    = wa_ekbe-dmbtr.        "Base Net Unit
*****        wa_output-shkzg = wa_ekbe-shkzg.
*****        CONCATENATE wa_output-belnr wa_output-gjahr INTO wa_output-awkey.
*****        READ TABLE it_j_1igrxref INTO wa_j_1igrxref WITH KEY mblnr = wa_ekbe-belnr zeile = wa_ekbe-buzei.
*****        IF sy-subrc = 0.
*****          IF wa_ekko-bsart <> 'ZIMP'.
*****            wa_output-bedamt = wa_j_1igrxref-exbed.
*****            wa_output-ecsamt = wa_j_1igrxref-ecs.
*****            wa_output-secs   = wa_j_1igrxref-exaddtax1.
*****          ELSEIF wa_ekko-bsart = 'ZIMP'.
*****            wa_output-incvd      = wa_j_1igrxref-exbed.
*****            wa_output-incvd_prim = wa_j_1igrxref-ecs.
*****            wa_output-incvd_sec  = wa_j_1igrxref-exaddtax1.
*****            wa_output-custduty = wa_j_1igrxref-exaed.
******          wa_output-
*****          ENDIF.
*****        ENDIF.
******        IF WA_EKKO-BSART <> 'ZIMP'.
******          CLEAR: WA_OUTPUT-CUSTDUTY.
******        ENDIF.
******
******        IF WA_EKKO-BSART <> 'ZSTO'.
******          CLEAR : WA_OUTPUT-FRI_AMT, WA_OUTPUT-OCTORI.
******        ENDIF.
*****
*****        CLEAR: wa_output-bcustom , wa_output-educess_prim, wa_output-educess_sec,
*****               wa_output-cha, wa_output-landing , wa_output-custduty , wa_output-fri_amt, wa_output-octori.
*****
*****        IF wa_ekko-bsart = 'ZSTO'.
*****          LOOP AT it_ekbe_sto INTO wa_ekbe_sto WHERE ebeln = wa_ekpo-ebeln AND
*****                                          ebelp = wa_ekpo-ebelp AND menge = wa_ekbe-menge.
*****            LOOP AT it_ekbz_sto INTO wa_ekbz_sto WHERE belnr = wa_ekbe_sto-belnr
*****              AND buzei = wa_ekbe_sto-buzei .
*****
*****              CASE wa_ekbz_sto-kschl.
*****                WHEN 'JCDB'.
*****                  wa_output-bcustom         = wa_ekbz_sto-dmbtr.
*****                WHEN 'JCE1'.
*****                  wa_output-educess_prim    = wa_ekbz_sto-dmbtr.
*****                WHEN 'JCE2'.
*****                  wa_output-educess_sec     = wa_ekbz_sto-dmbtr.
*****                WHEN 'JADC'.
*****                  wa_output-custduty        = wa_ekbz_sto-dmbtr.
*****                WHEN 'ZCH2' OR 'ZCH3'.
*****                  wa_output-cha             = wa_ekbz_sto-dmbtr.
*****                WHEN 'JAVC'.
*****                  wa_output-landing         = wa_ekbz_sto-dmbtr.
*****                WHEN 'FRB1' OR 'FRC1'.
*****                  wa_output-fri_amt         = wa_ekbz_sto-dmbtr.
*****                WHEN 'JOCM' OR 'ZJO2'.
*****                  wa_output-octori          = wa_ekbz_sto-dmbtr.
*****                WHEN OTHERS.
*****              ENDCASE.
*****            ENDLOOP.
*****          ENDLOOP.
*****        ELSE.
*****          LOOP AT it_ekbz INTO wa_ekbz WHERE belnr = wa_ekbe-belnr AND buzei = wa_ekbe-buzei.
*****            CASE wa_ekbz-kschl.
*****              WHEN 'JCDB'.
*****                wa_output-bcustom         = wa_ekbz-dmbtr.
*****              WHEN 'JCE1'.
*****                wa_output-educess_prim    = wa_ekbz-dmbtr.
*****              WHEN 'JCE2'.
*****                wa_output-educess_sec     = wa_ekbz-dmbtr.
*****              WHEN 'JADC'.
*****                wa_output-custduty        = wa_ekbz-dmbtr.
*****              WHEN 'ZCH2' OR 'ZCH3'.
*****                wa_output-cha             = wa_ekbz-dmbtr.
*****              WHEN 'JAVC'.
*****                wa_output-landing         = wa_ekbz-dmbtr.
*****              WHEN 'FRB1' OR 'FRC1'.
*****                wa_output-fri_amt         = wa_ekbz-dmbtr.
*****              WHEN 'JOCM' OR 'ZJO2'.
*****                wa_output-octori          = wa_ekbz-dmbtr.
*****              WHEN OTHERS.
*****            ENDCASE.
*****          ENDLOOP.
*****        ENDIF.
******          wa_output-lcurr = wa_ekbe-menge * wa_ekpo-netpr * wa_ekko-wkurs.
*****        CLEAR w_sum.
*****
*****        wa_output-tot_exc = wa_output-bedamt + wa_output-ecsamt + wa_output-secs.
*****        TRY.
*****            wa_output-cenvare = wa_output-tot_exc / wa_output-gr_menge."WA_EKPO-MENGE.
*****          CATCH cx_sy_arithmetic_error .
*****            wa_output-cenvare = 0.
*****        ENDTRY.
*****
*****        wa_output-tot_cd = wa_output-incvd +
*****                           wa_output-incvd_prim +
*****                           wa_output-incvd_sec +
*****                           wa_output-custduty.
*****
******        CLEAR: WA_OUTPUT-VAT, WA_OUTPUT-CST.  "VAT & CST already included in RM, so clear the amount and display zero in resp columns
*****
*****        wa_output-tot_price =  wa_output-lcurr + wa_output-bedamt + wa_output-ecsamt + wa_output-secs + wa_output-incvd + wa_output-incvd_prim  + wa_output-incvd_sec +
*****                             wa_output-bcustom + wa_output-educess_prim + wa_output-educess_sec  + wa_output-custduty + wa_output-cha + wa_output-landing +
*****                             wa_output-fri_amt + wa_output-octori." + WA_OUTPUT-CST + WA_OUTPUT-VAT.
*****
*****        IF wa_ekko-bsart = 'ZSTO'.
*****
*****          SELECT SINGLE dmbtr FROM ekbe INTO wa_output-lcurr
*****            WHERE ebeln = wa_output-ebeln
*****            AND ebelp = wa_output-ebelp
*****            AND menge = wa_output-gr_menge
*****            AND bewtp = 'U'.
*****          wa_output-land_cost = wa_output-lcurr + wa_output-fri_amt + wa_output-octori.
******        WA_OUTPUT-LAND_COST = WA_OUTPUT-LAND_COSTPU * WA_OUTPUT-GR_MENGE.
*****        ELSE.
*****          wa_output-land_cost = wa_output-tot_price - wa_output-tot_exc - wa_output-tot_cd.
*****        ENDIF.
*****        wa_output-grr_bas = wa_output-per_unit * wa_output-gr_menge.
*****
*****        APPEND wa_output TO it_output.
*****      ENDLOOP.
*****
*****    ENDLOOP.
*****    CLEAR : wa_output, wa_ekko.
*****  ENDLOOP.

  IF it_output IS NOT INITIAL.
    SELECT awkey kursf FROM bkpf
      INTO TABLE it_bkpf FOR ALL ENTRIES IN it_output
      WHERE awkey = it_output-awkey.
    IF sy-subrc = 0.
      SORT it_bkpf BY awkey.
    ENDIF.
  ENDIF.

  LOOP AT it_output INTO wa_output.

    IF wa_output-waers = 'INR'.
      wa_output-kursf = wa_output-wkurs.
    ELSE.
      READ TABLE it_bkpf INTO wa_bkpf WITH KEY awkey = wa_output-awkey.
      IF sy-subrc = 0.
        wa_output-kursf = wa_bkpf-kursf.
      ENDIF.
    ENDIF.
    v_menge = wa_output-gr_menge.


    IF v_menge <> 0.
      wa_output-base_mat = wa_output-lcurr / wa_output-gr_menge.
      wa_output-bedamt1 = wa_output-bedamt / v_menge.
      wa_output-ecsamt1 = wa_output-ecsamt / v_menge.
      wa_output-secs1 = wa_output-secs / v_menge.
      wa_output-incvd1 = wa_output-incvd / v_menge.
      wa_output-incvd_prim1 = wa_output-incvd_prim / v_menge.
      wa_output-incvd_sec1 = wa_output-incvd_sec / v_menge.

      wa_output-bcustom1 = wa_output-bcustom / v_menge.
      wa_output-educess_prim1 = wa_output-educess_prim / v_menge.
      wa_output-educess_sec1 = wa_output-educess_sec / v_menge.
      wa_output-custduty1 = wa_output-custduty / v_menge.
      wa_output-cha1 = wa_output-cha / v_menge.
      wa_output-landing1 = wa_output-landing / v_menge.
      wa_output-fri_amt1 = wa_output-fri_amt / v_menge.
      wa_output-octori1 = wa_output-octori / v_menge.
      wa_output-cst1 = wa_output-cst / v_menge.
      wa_output-vat1 = wa_output-vat / v_menge.
      wa_output-tot_price1 = wa_output-tot_price / v_menge.

      wa_output-cust_pu = wa_output-tot_cd / v_menge.
      wa_output-lcostpu = wa_output-land_cost / v_menge.
      wa_output-land_costpu = wa_output-land_cost / v_menge.

    ENDIF.
    IF wa_output-shkzg = 'H'.

      wa_output-gr_menge = wa_output-gr_menge * -1. "GR Quantity
      wa_output-lcurr = wa_output-lcurr * -1. " Base Net Value Material
      wa_output-bedamt = wa_output-bedamt * -1."Excise BED amount
      wa_output-ecsamt = wa_output-ecsamt * -1."Excise ECS Amount
      wa_output-secs = wa_output-secs * -1. "Excise SHES Amount
      wa_output-fri_amt = wa_output-fri_amt * -1." Freight amount
      wa_output-cst = wa_output-cst * -1.       "CST
      wa_output-tot_price = wa_output-tot_price * -1.
      wa_output-tot_exc = wa_output-tot_exc * -1." *Total Excise duty  35  TOT_EXC
      wa_output-land_cost = wa_output-land_cost * -1. " *Landed Cost  34  LAND_COST
      wa_output-rlwrt = wa_output-rlwrt * -1.
      wa_output-custduty = wa_output-custduty * -1."Add Duty of Custom
      wa_output-cha = wa_output-cha * -1."CHA (Value)
      wa_output-bcustom = wa_output-bcustom * -1."IN Basic customs
      wa_output-educess_prim = wa_output-educess_prim * -1." *IN Cust EduCess Prim  24  EDUCESS_PRIM
      wa_output-educess_sec = wa_output-educess_sec * -1." *IN Cust EduCess Sec 25  EDUCESS_SEC
      wa_output-incvd = wa_output-incvd * -1."*IN CVD 21  INCVD
      wa_output-incvd_sec = wa_output-incvd_sec * -1." *IN CVD Sec Edu Cess 23  INCVD_SEC
      wa_output-incvd_prim = wa_output-incvd_prim * -1."*IN CVDPrim Edu Cess  22  INCVD_PRIM
      wa_output-landing = wa_output-landing * -1." *Landing Ch % STS 28  LANDING
      wa_output-octori = wa_output-octori * -1. " *Octroi 30  OCTORI
      wa_output-tot_cd = wa_output-tot_cd * -1. "*Total of custom duty 37  TOT_CD
      wa_output-vat = wa_output-vat * -1."*VAT  32  VAT


    ENDIF.

***** Start Code: Added by CS on 07.01.2016. *****
    IF wa_output-zbpl IS NOT INITIAL.
      wa_output-zbpl_variance = wa_output-zbpl - wa_output-land_costpu. " Variance of ZBPL
    ENDIF.

    wa_output-avg_crval = wa_output-land_cost * wa_output-ztag1. " Landed cost * Payterms Days
    lv_tot_lcost = lv_tot_lcost + wa_output-land_cost.  " Total of Landed Cost
    lv_tot_avgcr = lv_tot_avgcr + wa_output-avg_crval.  " Total of Avg. Cr. Value
***** End Code: Added by CS on 07.01.2016. *****

    MODIFY it_output FROM wa_output.
    CLEAR: wa_output, v_menge.
  ENDLOOP.

ENDFORM.                    " FILL_DATA_GL
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_data_po .
*  For each PO, get the item details and fill the condition value for
*  each item.
  LOOP AT it_ekko INTO wa_ekko.
    LOOP AT it_ekpo INTO wa_ekpo WHERE ebeln = wa_ekko-ebeln.
      wa_output-ebeln = wa_ekko-ebeln.
      wa_output-bedat = wa_ekko-bedat.
      wa_output-ebelp = wa_ekpo-ebelp.
      wa_output-matnr = wa_ekpo-matnr.
      wa_output-txz01 = wa_ekpo-txz01.
      wa_output-menge = wa_ekpo-menge.
      wa_output-meins = wa_ekpo-meins.
      wa_output-lifnr = wa_ekko-lifnr.
      wa_output-zterm = wa_ekko-zterm.
      wa_output-rlwrt = wa_ekko-rlwrt.
      wa_output-ekgrp = wa_ekko-ekgrp.  " Purchasing Group Added by CS on 30.03.2016
***** Start Code: Added by CS on 07.01.2016 for Payterm Days. *****
      SELECT SINGLE ztag1
        FROM t052
        INTO wa_output-ztag1  " Payterm Days
        WHERE zterm EQ wa_output-zterm.
***** End Code: Added by CS on 07.01.2016 for Payterm Days. *****

      SELECT SINGLE text1
        FROM t052u
        INTO wa_output-text1
        WHERE spras = sy-langu
        AND zterm = wa_output-zterm.

***** Start Code: For Division - Added by CS on 30.03.2016. *****
      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_ekpo-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_output-spart = wa_mara-spart.  " Division
      ENDIF.
***** End Code: For Division - Added by CS on 30.03.2016. *****

      CLEAR wa_lifnr.
      READ TABLE it_lifnr INTO wa_lifnr WITH KEY lifnr = wa_ekko-lifnr.
      IF sy-subrc = 0.
        wa_output-name1 = wa_lifnr-name1.
      ENDIF.
      wa_output-werks  = wa_ekpo-werks.
      wa_output-waers = wa_ekko-waers.
      wa_output-wkurs = wa_ekko-wkurs.

      ON CHANGE OF wa_ekpo-ebeln.
        CLEAR: tot_net ,  wa_output-tot_net_val .
      ENDON.
      wa_output-lcurr = wa_ekpo-menge * wa_ekpo-netpr * wa_ekko-wkurs.
      wa_output-base_mat = wa_ekpo-netpr * wa_ekko-wkurs.
      wa_output-per_unit = wa_ekpo-netpr.
      tot_net = wa_output-lcurr + tot_net.


*      wa_output-tot_net_val = TOT_NET.

* if PO type other than service PO get the general pricing details
      IF ( wa_ekko-bsart CS 'SE' ).
* Only few pricing details are required for service PO
        PERFORM get_pricing_for_servicepo.
      ELSE.
        PERFORM fill_pricing_details.
        PERFORM change_price_imp.
      ENDIF.


      wa_output-tot_price =  wa_output-lcurr + wa_output-bedamt + wa_output-ecsamt + wa_output-secs + wa_output-incvd + wa_output-incvd_prim  + wa_output-incvd_sec +
                           wa_output-bcustom + wa_output-educess_prim + wa_output-educess_sec  + wa_output-custduty + wa_output-cha + wa_output-landing +
                           wa_output-fri_amt + wa_output-octori + wa_output-cst + wa_output-vat.

      IF wa_output-menge IS NOT INITIAL.
        wa_output-tot_price1 = wa_output-tot_price / wa_output-menge.
      ENDIF.

      wa_output-tot_exc = wa_output-bedamt + wa_output-ecsamt + wa_output-secs.
      TRY.
          wa_output-cenvare = wa_output-tot_exc / wa_ekpo-menge.
        CATCH cx_sy_arithmetic_error .
          wa_output-cenvare = 0.
      ENDTRY.
*        wa_output-tot_cd = wa_output-bcustom + wa_output-incvd +
*                           wa_output-incvd_prim + wa_output-incvd_sec
*                           + wa_output-educess_prim +
*                             wa_output-educess_sec .
      wa_output-tot_cd = wa_output-incvd +
                           wa_output-incvd_prim +
                           wa_output-incvd_sec +
                           wa_output-custduty.
      IF wa_ekpo-menge IS NOT INITIAL.
        wa_output-cust_pu = wa_output-tot_cd / wa_ekpo-menge.
      ENDIF.

      wa_output-land_cost = wa_output-tot_price - wa_output-tot_exc - wa_output-tot_cd.
***** Start Code: Added by CS on 07.01.2016. *****
      wa_output-avg_crval = wa_output-land_cost * wa_output-ztag1. " Landed cost * Payterms Days
      lv_tot_lcost = lv_tot_lcost + wa_output-land_cost.  " Total of Landed Cost
      lv_tot_avgcr = lv_tot_avgcr + wa_output-avg_crval.  " Total of Avg. Cr. Value
***** End Code: Added by CS on 07.01.2016. *****

      IF wa_ekpo-menge IS NOT INITIAL.
        wa_output-lcostpu = wa_output-land_cost / wa_ekpo-menge.
      ENDIF.


      IF wa_output-menge IS  NOT INITIAL.
        wa_output-land_costpu = wa_output-land_cost / wa_output-menge.
      ENDIF.

      IF s_rlwrt IS NOT INITIAL.

        wa_output-rlwrt = wa_output-rlwrt * wa_output-wkurs.

        IF wa_output-rlwrt IN s_rlwrt.
          APPEND wa_output TO it_output.
        ENDIF.
        CLEAR : wa_ekpo, wa_mara.
      ELSE.
        APPEND wa_output TO it_output.
        CLEAR : wa_ekpo, wa_mara.
      ENDIF.
      vat_total = vat_total + wa_output-vat.
    ENDLOOP.
    wa_output-rlwrt = wa_output-rlwrt + vat_total.

    LOOP AT it_output INTO wa_temp WHERE ebeln = wa_output-ebeln.
      wa_temp-rlwrt = wa_output-rlwrt.
      MODIFY it_output FROM wa_temp TRANSPORTING rlwrt.
    ENDLOOP.

    CLEAR : wa_output, wa_ekko , vat_total ,wa_temp.
  ENDLOOP.

  LOOP AT it_output INTO wa_output.
    v_menge = wa_output-menge.

    IF v_menge <> 0.
      wa_output-bedamt1 = wa_output-bedamt / v_menge.
      wa_output-ecsamt1 = wa_output-ecsamt / v_menge.
      wa_output-secs1 = wa_output-secs / v_menge.
      wa_output-incvd1 = wa_output-incvd / v_menge.

*      wa_output-incvd_prim = wa_output-incvd_prim * wa_output-wkurs.
      wa_output-incvd_prim1 = wa_output-incvd_prim / v_menge.

*      wa_output-incvd_sec = wa_output-incvd_sec * wa_output-wkurs.
      wa_output-incvd_sec1 = wa_output-incvd_sec / v_menge.

      wa_output-bcustom1 = wa_output-bcustom / v_menge.
      wa_output-educess_prim1 = wa_output-educess_prim / v_menge.
      wa_output-educess_sec1 = wa_output-educess_sec / v_menge.
      wa_output-custduty1 = wa_output-custduty / v_menge.
      wa_output-cha1 = wa_output-cha / v_menge.
      wa_output-landing1 = wa_output-landing / v_menge.
      wa_output-fri_amt1 = wa_output-fri_amt / v_menge.
      wa_output-octori1 = wa_output-octori / v_menge.
      wa_output-cst1 = wa_output-cst / v_menge.
      wa_output-vat1 = wa_output-vat / v_menge.
      wa_output-tot_price1 = wa_output-tot_price / v_menge.

      wa_output-land_costpu = wa_output-land_cost / v_menge.
***** Start Code: Added by CS on 07.01.2016. *****
      IF wa_output-zbpl IS NOT INITIAL.
        wa_output-zbpl_variance = wa_output-zbpl - wa_output-land_costpu. "
      ENDIF.
***** End Code: Added by CS on 07.01.2016. *****
      MODIFY it_output FROM wa_output.
    ENDIF.
    CLEAR: wa_output, v_menge.
  ENDLOOP.

ENDFORM.                    " FILL_DATA_PO
*&---------------------------------------------------------------------*
*&      Form  HEADDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM headder .

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = it_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

  SORT it_events BY name.
*  READ TABLE  IT_EVENTS INTO WA_EVENTS  WITH KEY NAME = 'TOP_OF_PAGE' BINARY SEARCH.
*  IF SY-SUBRC = 0.
*    WA_EVENTS-FORM = 'TOP_OF_PAGE'.
*    MODIFY IT_EVENTS FROM WA_EVENTS INDEX SY-TABIX.
*  ENDIF.

  READ TABLE it_events INTO wa_events WITH KEY name = 'PF_STATUS_SET' BINARY SEARCH .
  IF sy-subrc = 0.
    wa_events-form = 'PF_STATUS'.
    MODIFY it_events FROM wa_events INDEX sy-tabix.
  ENDIF.
  CLEAR wa_events.

*  READ TABLE IT_EVENTS INTO WA_EVENTS WITH KEY NAME = 'USER_COMMAND' BINARY SEARCH .
*  IF SY-SUBRC = 0.
*    WA_EVENTS-FORM = 'USER_COMMAND'.
*    MODIFY IT_EVENTS FROM WA_EVENTS INDEX SY-TABIX.
*  ENDIF.
*  CLEAR WA_EVENTS.

ENDFORM.                    " HEADDER


*&---------------------------------------------------------------------*
*&      Form  PF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RT_EXTAB text
*----------------------------------------------------------------------*
FORM pf_status USING p_rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'PF_STATUS'.
ENDFORM.                    "pf_status
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
  g_variant-report = repname.
  g_variant-variant = p_vari.
  gx_variant = g_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = g_save
    CHANGING
      cs_variant = gx_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
    p_vari = gx_variant-variant.
    g_variant = gx_variant.

  ENDIF.

ENDFORM.                    " INITIALIZE_VARIANT
*&---------------------------------------------------------------------*
*&      Form  PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pai_of_selection_screen .
  PERFORM initialize_variant.
ENDFORM.                    " PAI_OF_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_for_variant .
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
*&      Form  CHECK_AUTH_OBJ
*&---------------------------------------------------------------------*
*   Added by CS on 16.10.2015 for Authorization
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_auth_obj .
  TYPES: BEGIN OF ty_t024e,
          ekorg TYPE t024e-ekorg, " Pur. Org.
        END OF ty_t024e,
        BEGIN OF ty_t001w,
          werks TYPE t001w-werks, " Plants
        END OF ty_t001w,
        BEGIN OF ty_t134t,
          mtart TYPE t134t-mtart, " Material Type
        END OF ty_t134t.
  DATA:
        t_t024e TYPE TABLE OF ty_t024e, " Pur. Org.
        w_t024e TYPE ty_t024e,
        t_t001w TYPE TABLE OF ty_t001w, " Plants
        w_t001w TYPE ty_t001w,
        t_t134t TYPE TABLE OF ty_t134t, " Material Type
        w_t134t TYPE ty_t134t.

  FREE : t_t024e[], t_t001w[], t_t134t[].
  CLEAR: w_t024e, w_t001w, w_t134t.

  break test1.

**********************************************************************
* added by nk on 16.01.2017

  SELECT spart
    FROM tspat
    INTO TABLE i_tspat
    WHERE spart IN s_spart
    AND   spras EQ sy-langu.

*IF S_SPART[] IS NOT INITIAL.
  CLEAR: s_spart, lv_auth_spart_flg.
  REFRESH: s_spart[].
  IF i_tspat[] IS NOT INITIAL.
    LOOP AT i_tspat INTO wa_tspat.
      AUTHORITY-CHECK OBJECT 'V_VBAK_VKO'
                       ID 'SPART' FIELD wa_tspat-spart
                       ID 'ACTVT' FIELD '03'.
      IF sy-subrc EQ 0.
        s_spart-sign = 'I'.
        s_spart-option = 'EQ'.
        s_spart-low = wa_tspat-spart.
        APPEND s_spart.
        CLEAR s_spart.
      ELSE.
        IF lv_auth_spart_flg IS INITIAL.
          lv_auth_spart_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR wa_tspat.
    ENDLOOP.
  ENDIF.
*ENDIF.

  IF s_spart[] IS INITIAL.
    s_spart-sign = 'I'.
    s_spart-option = 'EQ'.
    s_spart-low = ''.
    APPEND s_spart.
*  ELSE.
*    wa_tspat-spart = ''.
*    APPEND wa_tspat TO i_tspat.
  ENDIF.

**********************************************************************
***** Start Code: Added by CS on 16.10.2015 for Pur. Org. Authorization. *****
  SELECT ekorg  " Fetch values of Pur. Org.
    FROM t024e
    INTO TABLE t_t024e
    WHERE ekorg IN so_ekorg.

  CLEAR: so_ekorg, lv_ekorg_auth_flg.
  REFRESH: so_ekorg[].
  IF t_t024e[] IS NOT INITIAL.
    LOOP AT t_t024e INTO w_t024e.
      AUTHORITY-CHECK OBJECT 'M_BEST_EKO'
                     ID 'ACTVT' FIELD '03'
                     ID 'EKORG' FIELD w_t024e-ekorg.
      IF sy-subrc EQ 0.
        so_ekorg-sign = 'I'.
        so_ekorg-option = 'EQ'.
        so_ekorg-low = w_t024e-ekorg.
        APPEND so_ekorg.
        CLEAR: so_ekorg.
      ELSE.
        IF lv_ekorg_auth_flg IS INITIAL.  " Authorization Flag
          lv_ekorg_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t024e.
    ENDLOOP.
  ENDIF.
  IF so_ekorg[] IS INITIAL.
    so_ekorg-sign = 'I'.
    so_ekorg-option = 'EQ'.
    so_ekorg-low = ''.
    APPEND so_ekorg.
    CLEAR: so_ekorg.
  ENDIF.
***** End Code: Added by CS on 16.10.2015 for Pur. Org. Authorization. *****

***** Start Code: Added by CS on 16.10.2015 for Plant Authorization. *****
  SELECT werks  " Fetch values of Plant
    FROM t001w
    INTO TABLE t_t001w
    WHERE werks IN so_werks.

  CLEAR: so_werks, lv_werks_auth_flg.
  REFRESH: so_werks[].
  IF t_t001w[] IS NOT INITIAL.
    LOOP AT t_t001w INTO w_t001w.
      AUTHORITY-CHECK OBJECT 'M_MSEG_WWA' " Plant
                     ID 'ACTVT' FIELD '03'
                     ID 'WERKS' FIELD w_t001w-werks.
      IF sy-subrc EQ 0.
        so_werks-sign = 'I'.
        so_werks-option = 'EQ'.
        so_werks-low = w_t001w-werks.
        APPEND so_werks.
        CLEAR: so_werks.
      ELSE.
        IF lv_werks_auth_flg IS INITIAL.  " Authorization Flag
          lv_werks_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t001w.
    ENDLOOP.
  ENDIF.
  IF so_werks[] IS INITIAL.
    so_werks-sign = 'I'.
    so_werks-option = 'EQ'.
    so_werks-low = ''.
    APPEND so_werks.
    CLEAR: so_werks.
  ENDIF.
***** End Code: Added by CS on 16.10.2015 for Plant Authorization. *****

***** Start Code: Added by CS on 16.10.2015 for Material Type Authorization. *****
  SELECT mtart  " Fetch values of Material Type
    FROM t134t
    INTO TABLE t_t134t
    WHERE mtart IN so_mtart.

  CLEAR: so_mtart, lv_mtart_auth_flg.
  REFRESH: so_mtart[].
  IF t_t134t[] IS NOT INITIAL.
    LOOP AT t_t134t INTO w_t134t.
      AUTHORITY-CHECK OBJECT 'K_ML_MTART' " Material Type
                     ID 'ACTVT' FIELD '03'
                     ID 'MTART' FIELD w_t134t-mtart.
      IF sy-subrc EQ 0.
        so_mtart-sign = 'I'.
        so_mtart-option = 'EQ'.
        so_mtart-low = w_t134t-mtart.
        APPEND so_mtart.
        CLEAR: so_mtart.
      ELSE.
        IF lv_mtart_auth_flg IS INITIAL.  " Authorization Flag
          lv_mtart_auth_flg = 'X'.
        ENDIF.
      ENDIF.
      CLEAR: w_t134t.
    ENDLOOP.
  ENDIF.
  IF so_mtart[] IS INITIAL.
    so_mtart-sign = 'I'.
    so_mtart-option = 'EQ'.
    so_mtart-low = ''.
    APPEND so_mtart.
    CLEAR: so_mtart.
  ENDIF.
***** End Code: Added by CS on 16.10.2015 for Material Type Authorization. *****

ENDFORM.                    " CHECK_AUTH_OBJ
