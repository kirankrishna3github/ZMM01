FUNCTION Z6MM_PO_CAL_TAX.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_EBELN) TYPE  EBELN
*"     REFERENCE(I_EBELP) TYPE  EBELP OPTIONAL
*"  EXPORTING
*"     REFERENCE(TT_KOMV) TYPE  KOMV_ITAB
*"----------------------------------------------------------------------
*{   REPLACE        SBXK900272                                        1
*\  TYPES : WT_EKPO TYPE EKPO.
*\  DATA : WA_EKKO TYPE EKKO.
*\
*\  DATA : it_komv TYPE STANDARD TABLE OF komv  .
*\
*\
*\  DATA : I_EKPO TYPE STANDARD TABLE OF WT_EKPO .
*\  DATA : WA_EKPO TYPE EKPO.
*\  DATA  : WA_T001 TYPE T001.
*\  DATA :  WA_T005 TYPE T005.
*\  DATA : TAXCOM TYPE TAXCOM.
*\  data : komk type komk.
*\  data : komp type komp.
*\
*\  SELECT SINGLE * FROM ekko INTO WA_EKKO WHERE ebeln = I_ebeln.
*\  IF NOT I_EBELP IS INITIAL.
*\    SELECT SINGLE * FROM ekpo  INTO  WA_EKPO
*\                WHERE ebeln = I_EBELN AND ebelp = I_EBELP.
*\    APPEND WA_EKPO TO I_EKPO.
*\    CLEAR  WA_EKPO.
*\
*\  ELSE.
*\    SELECT * FROM ekpo  INTO CORRESPONDING FIELDS OF TABLE I_EKPO
*\                WHERE ebeln = I_EBELN .
*\
*\
*\  ENDIF.
*\*{   INSERT         DEVK901502                                        1
*\*
*\  LOOP AT I_EKPO INTO WA_EKPO.
*\    CHECK NOT WA_ekpo-mwskz IS INITIAL.
*\*}   INSERT
*\    SELECT SINGLE * FROM t001 INTO WA_T001 WHERE bukrs = WA_ekko-bukrs.
*\    SELECT SINGLE * FROM t005 INTO WA_T005 WHERE land1 = WA_t001-land1.
*\
*\    IF WA_ekpo-mwskz NE space.
*\      CLEAR taxcom.
*\      taxcom-bukrs = WA_ekpo-bukrs.
*\      taxcom-budat = WA_ekko-bedat.
*\      taxcom-waers = WA_ekko-waers.
*\      taxcom-kposn = WA_ekpo-ebelp.
*\      taxcom-mwskz = WA_ekpo-mwskz.
*\      taxcom-txjcd = WA_ekpo-txjcd.
*\      taxcom-shkzg = 'H'.
*\      taxcom-xmwst = 'X'.
*\      IF WA_ekko-bstyp EQ 'F'.
*\        taxcom-wrbtr = WA_ekpo-netwr.
*\      ELSE.
*\        taxcom-wrbtr = wA_ekpo-zwert.
*\      ENDIF.
*\*- Beginn neue Felder zu 3.0C für internationale Steuermodule
*\      taxcom-lifnr = WA_ekko-lifnr.
*\*   taxcom-land1 = t001-land1.
*\      taxcom-land1 = WA_ekko-lands.                              "WIA
*\      taxcom-ekorg = WA_ekko-ekorg.
*\*? TAXCOM-GSBER = EKPO-KO_GSBER.      "noch nicht versorgt bei uns
*\      taxcom-hwaer = wa_t001-waers.
*\      taxcom-llief = WA_ekko-llief.
*\      taxcom-bldat = WA_ekko-bedat.
*\*   taxcom-matnr = ekpo-ematn.
*\      taxcom-matnr = WA_ekpo-matnr.         "HTN-Abwicklung
*\      taxcom-werks = WA_ekpo-werks.
*\      taxcom-bwtar = WA_ekpo-bwtar.
*\      taxcom-matkl = WA_ekpo-matkl.
*\      taxcom-meins = WA_ekpo-meins.
*\*- Mengen richtig fuellen ---------------------------------------------*
*\      IF wa_ekko-bstyp EQ 'F'.
*\        taxcom-mglme = WA_ekpo-menge.
*\      ELSE.
*\        IF wa_ekko-bstyp EQ 'K' AND WA_ekpo-abmng GT 0.
*\          taxcom-mglme = WA_ekpo-abmng.
*\        ELSE.
*\          taxcom-mglme = WA_ekpo-ktmng.
*\        ENDIF.
*\      ENDIF.
*\      IF taxcom-mglme EQ 0.  "falls keine Menge gesetzt --> auf 1 setzen
*\        taxcom-mglme = 1000.  "z.B. bestellte Banf nochmal bestellt
*\      ENDIF.
*\      taxcom-mtart = WA_ekpo-mtart.
*\*- Ende   neue Felder zu 3.0C für internationale Steuermodule
*\
*\*--- SHIP FROM always from supplier -----
*\      IF NOT WA_ekko-llief IS INITIAL.
*\        taxcom-lifnr = WA_ekko-llief.
*\      ENDIF.
*\    ENDIF.
*\***
*\    CALL FUNCTION 'J_1BSA_COMPONENT_CHECK'
*\      EXPORTING
*\        component               = 'IN'
*\      EXCEPTIONS
*\        component_not_installed = 1
*\        OTHERS                  = 2.
*\    IF sy-subrc = 0.
*\
*\      CALL FUNCTION 'SAP_TO_ISO_COUNTRY_CODE'
*\        EXPORTING
*\          sap_code    = WA_t001-land1
*\        IMPORTING
*\          iso_code    = WA_t005-intca
*\        EXCEPTIONS
*\          not_found   = 1
*\          no_iso_code = 2
*\          OTHERS      = 3.
*\      IF sy-subrc <> 0.
*\        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*\                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*\      ENDIF.
*\
*\*      CALL METHOD CL_EXITHANDLER=>GET_INSTANCE
*\*          CHANGING INSTANCE =  ME_COPYPO.
*\*
*\*      CALL METHOD  ME_COPYPO->ME_CIN_COPY_PO_DATA
*\*           EXPORTING
*\*              FLT_VAL = T005-INTCA
*\*              Y_EKPO = EKPO.
*\* *   endif.
*\***
*\*BadI for India India
*\************************************************************************
*\
*\*  IF SY-SUBRC NE 0.     "keine Reaktion, Exception eingebaut wegen
*\*  ENDIF.                "Problemen in Calculate_tax_item
*\    ENDIF.
*\
*\    CLEAR komk.
*\    komk-mandt = sy-mandt.
*\    komk-kappl = 'TX'.
*\    komk-waerk = taxcom-waers.
*\    komk-aland = taxcom-land1.
*\    komk-mwskz = taxcom-mwskz.
*\    komk-kunnr = taxcom-kunnr.
*\    komk-lifnr = taxcom-lifnr.
*\    komk-llief = taxcom-llief.
*\    komk-ekorg = taxcom-ekorg.
*\    komk-gsber = taxcom-gsber.
*\    komk-bukrs = WA_t001-bukrs.
*\    komk-hwaer = WA_t001-waers.
*\    komk-kalsm = WA_t005-kalsm.
*\    IF komk-aland IS INITIAL.
*\      komk-aland = WA_t001-land1.
*\    ENDIF.
*\    IF WA_t001-xstdt = 'X' AND NOT taxcom-bldat IS INITIAL.
*\      komk-prsdt = taxcom-bldat.
*\    ELSE.
*\      komk-prsdt = taxcom-budat.
*\    ENDIF.
*\
*\    komp-kposn = taxcom-kposn.
*\    komp-mwskz = taxcom-mwskz.
*\    komp-kzinc = space.
*\    komp-matnr = taxcom-matnr.
*\    komp-werks = taxcom-werks.
*\    komp-bwtar = taxcom-bwtar.
*\    komp-matkl = taxcom-matkl.
*\    komp-meins = taxcom-meins.
*\    komp-mglme = taxcom-mglme.
*\    komp-mtart = taxcom-mtart.
*\    komp-evrtn = taxcom-ebeln.
*\    komp-evrtp = taxcom-ebelp.
*\    komk-projk = taxcom-projk.
*\    komp-prctr = taxcom-prctr.
*\    komk-aufnr = taxcom-aufnr.
*\    komk-kostl = taxcom-kostl.
*\    komk-kokrs = taxcom-kokrs.
*\    komk-blart = taxcom-blart.
*\
*\*------- MWSKZ prüfen ------------------------------------------------*
*\*    CALL FUNCTION 'TAX_INDICATOR_CHECK'
*\*      EXPORTING
*\*        steuerschema      = WA_t005-kalsm
*\*        steuerkennzeichen = taxcom-mwskz
*\*      IMPORTING
*\*        t007a             = t007a.
*\
*\*  mwstab-egrkz = t007a-egrkz.
*\
*\*------- US-Taxes ? --------------------------------------------------*
*\    IF WA_t005-kalsm NE space.
*\*   IF taxcom-TXJCD = SPACE
*\*   AND taxcom-KOART NE 'K'.
*\*     CALL FUNCTION 'SHOW_TXJCD'
*\*       EXPORTING  I_KPOSN = taxcom-KPOSN
*\*                  I_TXJCD = taxcom-TXJCD
*\*                  I_WAERS = I_TAXCOM-WAERS
*\*                  I_WRBTR = I_TAXCOM-WRBTR
*\*       IMPORTING  E_TXJCD = taxcom-TXJCD.
*\*   ENDIF.
*\      komk-txjcd = taxcom-txjcd.
*\    ELSE.
*\      taxcom-txjcd = space.
*\    ENDIF.
*\
*\*------- Steuerbasis netto ? -----------------------------------------*
*\*  IF txctl-xmwsn NE space.
*\*    IF NOT taxcom-wskto IS INITIAL.
*\*      skonto = taxcom-wskto.
*\*    ELSEIF NOT taxcom-skfbt IS INITIAL.
*\*      skonto = taxcom-skfbt  * taxcom-zbd1p / 100000.
*\*    ELSE.
*\*      skonto = taxcom-wrbtr  * taxcom-zbd1p / 100000.
*\*      skonto_auto = 'X'.
*\*    ENDIF.
*\*  ENDIF.
*\*  IF taxcom-mwart NE space.
*\    komp-wrbtr = taxcom-wrbtr.
*\*  ELSE.
*\*    PERFORM komp-wrbtr_setzen USING skonto taxcom anzahlung
*\*                              CHANGING komp-wrbtr.
*\*  ENDIF.
*\
*\   clear it_komv.
*\
*\
*\    CALL FUNCTION 'PRICING'
*\      EXPORTING
*\        comm_head_i      = komk
*\        comm_item_i      = komp
*\        calculation_type = 'B'
*\      IMPORTING
*\        comm_head_e      = komk
*\        comm_item_e      = komp
*\      TABLES
*\        tkomv            = IT_komv.
*\
*\   append lines of it_komv to tt_komv.
*\   clear it_komv.
*\ endloop.
* << S/4HANA >> / 6010859 / SBXK900272 / Tuesday, November 20, 2018 11:25:00
  select single * from ekko into @data(wa_ekko) where ebeln = @i_ebeln.
  if i_ebelp is initial.
    select * from ekpo into table @data(i_ekpo)
                where ebeln = @i_ebeln.
  else.
    select single * from ekpo into @data(wa_ekpo)
          where ebeln = @i_ebeln and ebelp = @i_ebelp.
    append wa_ekpo to i_ekpo.
    clear  wa_ekpo.
  endif.

  refresh tt_komv.
  loop at i_ekpo into wa_ekpo.
    " IHDK900056
*    data iv_ebeln type ekpo-ebeln.
*    data iv_ebelp type ekpo-ebelp.
    data et_taxes type komv_t.

*    clear: iv_ebeln, iv_ebelp.
    refresh et_taxes.
*    iv_ebeln = wa_ekpo-ebeln.
*    iv_ebelp = wa_ekpo-ebelp.
    call method zcl_helper=>calc_po_item_tax(
      exporting
        is_ekko = wa_ekko
        is_ekpo = wa_ekpo
      importing
        et_taxes = et_taxes ).

    if et_taxes is not initial.
      append lines of et_taxes to tt_komv.
    endif.
  endloop.
*}   REPLACE



  ENDFUNCTION.
