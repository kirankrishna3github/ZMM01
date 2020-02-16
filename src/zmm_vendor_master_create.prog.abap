REPORT zmm_vendor_master_create
       NO STANDARD PAGE HEADING LINE-SIZE 255.

" Re-enable DMS field when DMS is activated for vendor; IRDK932938
*------------------------------------------------------------------------*
*                       Modification Log:
*------------------------------------------------------------------------*
*&Date       Request No. Technical  Functional  Description
*22.08.2018 IRDK933193   Bhushan    Venugopal   Add ZTERM in Vendor Creation
*04.10.2018 IRDK933553   Bhushan    Venugopal   Add Indicator for GR & Service Based
*------------------------------------------------------------------------*

INCLUDE zbdcrecx1_xk01.
TYPE-POOLS slis.
TYPES: BEGIN OF ty_file,
         lifnr(10),
         bukrs(4),
         ekorg(4),
         ktokk(4),
         name1(40),
         name2(40),
*         sort1(20),  " not required
         str_suppl1(40),
         str_suppl2(40),
         street(60),
         str_suppl3(40),
         location(40),
         post_code1(10),
         city1(40),
         country(4),
         region(3),
*         langu(2),
         tel_number(30),
         mob_number(30),
         fax_number(30),
         smtp_addr(241),
         ven_class(1), " GST indicator
         stcd3(18),"tax number 3
         gv_j_1ipanno(40),  " added new
*         stceg(20),
         namev_01(35),
         name1_01(35),
         akont(35),
*         altkn(4),
*         ven_class(1),
         waers(4),
         zterm(4),
         inco1(3),
         inco2(28),
         kalsk(2),
         webre(1),
         lebre(1),
**  new fields added by NK on 01.08.2017
         banks(3),
         bankl(15), " IFSC code
         banka(60),
         provz(3),
         stras(35),
         ort01(35),
         brnch(40),
         swift(11),
         bankn(18),
         koinh(60),
         zwels(10), " IRDK932921
         hbkid(5),  " IRDK932921
         frgrp(4), " Vendor Category - Release grp
         blank1(20), " TDS applicable Yes/No
         blank2(20), " Type of Service
         qland(3),
         witht1(2),
*         witht2(2),
         wt_withcd(2),
         wt_subjct1(1),
*         wt_subjct2(1),
         qsrec1(2),
*         qsrec2(2),
         " Re-enable DMS field when DMS is activated for vendor; IRDK932938
*         dms_doc          type doknr,  " DMS Doc, char 25, IRDK930677
       END OF ty_file,

       BEGIN OF ty_vendor,
         lifnr  TYPE lifnr,
         name1  TYPE name1_gp,
         stcd3  TYPE stcd3,
         remark TYPE zremarks,
       END OF ty_vendor.

DATA: wa         TYPE ty_file,
      it         TYPE TABLE OF ty_file,
      it_type    TYPE truxs_t_text_data,
      it_bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE,
      wa_vendor  TYPE ty_vendor,
      it_vendor  TYPE TABLE OF ty_vendor.

DATA : lv_country    TYPE char4,
       lv_mob_number TYPE char30.

*{   INSERT         IRDK928355                                        1
**data: ls_j_1ipanno  type j_1imovend-j_1ipanno,
**      ls_j_1ipanref type j_1imovend-j_1ipanref.
*---------------------------------------------------------------------*
*--------------------------- << S/4HANA >> ---------------------------*
*---------------------------------------------------------------------*
* Changed On - Wensday, November 14, 2018
* Changed By - 10106 - Bhushan Mehta
* Purpose    - Table Replacement
* Solution   - Replace Table LFA1 from J_1IMOVEND
* TR         - SBXK900270 - BM:Replace Table Excise to General Master Data:14.11.2018
*--------------------------------------------------------------------*
DATA: ls_j_1ipanno  TYPE lfa1-j_1ipanno,
      ls_j_1ipanref TYPE lfa1-j_1ipanref.
*}   INSERT

DATA: lv_file TYPE string.
DATA: lv_file1 TYPE rlgrap-filename.
DATA: gv_stcd3 TYPE lfa1-stcd3.
DATA: gv_name1 TYPE lfa1-name1.
DATA: gv_sort1(20),
      gv_witht2(2),
      gv_wt_subjct2(1),
      gv_qsrec2(2),
      gv_j_1ipanno(40).
DATA: ls_bnka   TYPE bnka,
      gv_lifnr3 TYPE lfa1-lifnr.
DATA: gt_t059u TYPE TABLE OF t059u,
      gs_t059u TYPE t059u.
DATA: gv_flg1,
      gv_flg2,
      gv_flg3,
      gv_flg4,
      gv_flg5,
      gv_repid TYPE sy-repid.

DATA: title  TYPE string,
      text   TYPE string,
      answer.
DATA:it_fieldcat TYPE slis_t_fieldcat_alv,
     wa_fieldcat TYPE slis_fieldcat_alv.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR file.
  CLEAR file.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = file.

  lv_file  = file.
  lv_file1 = file.    " For Excel upload file type.

START-OF-SELECTION.
  gv_repid = sy-repid.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_line_header        = 'X'
      i_tab_raw_data       = it_type
      i_filename           = lv_file1
    TABLES
      i_tab_converted_data = it
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

***** Begin of Code IRDK933193
  IF NOT it[] IS INITIAL.
    PERFORM validate_file.
  ENDIF.
***** End of Code IRDK933193

* Validation for With holding Tax
  SELECT * FROM t059u INTO TABLE gt_t059u
    WHERE spras = sy-langu
    AND   land1 = 'IN'.

  LOOP AT it INTO wa.
    IF wa-stcd3 IS NOT INITIAL.
      CLEAR:gv_lifnr3, gv_stcd3, gv_name1.
      SELECT SINGLE lifnr name1 stcd3 FROM lfa1 INTO (gv_lifnr3, gv_name1 , gv_stcd3)
              WHERE stcd3 = wa-stcd3.
      IF sy-subrc EQ 0.
        wa_vendor-lifnr = gv_lifnr3.
        wa_vendor-name1 = gv_name1.
        wa_vendor-stcd3 = gv_stcd3.
        wa_vendor-remark = 'GSTN already maintained for this Vendor. Kindly check'.
        APPEND wa_vendor TO it_vendor.
        CLEAR:wa_vendor.
      ELSE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE it_vendor WHERE stcd3 IS INITIAL.

  IF it_vendor IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = gv_repid
        i_structure_name       = 'ZVEN_MAST'
        i_client_never_display = 'X'
      CHANGING
        ct_fieldcat            = it_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = gv_repid
*       i_callback_pf_status_set = 'SET_STATUS' "'ZVENDORSTATU'
*       I_CALLBACK_USER_COMMAND  = ' '
        it_fieldcat        = it_fieldcat
      TABLES
        t_outtab           = it_vendor
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc EQ 0.

      title = 'Information Dialog'.
      text = 'Do you still want to run this BDC'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = title
*         DIAGNOSE_OBJECT             = ' '
          text_question  = text
          text_button_1  = 'Yes' "(001)
*         ICON_BUTTON_1  = ' '
          text_button_2  = 'No' "(002)
*         ICON_BUTTON_2  = ' '
          default_button = '1'
*         DISPLAY_CANCEL_BUTTON       = 'X'
*         USERDEFINED_F1_HELP         = ' '
          start_column   = 25
          start_row      = 6
*         POPUP_TYPE     =
*         IV_QUICKINFO_BUTTON_1       = ' '
*         IV_QUICKINFO_BUTTON_2       = ' '
        IMPORTING
          answer         = answer
*       TABLES
*         PARAMETER      =
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*    CLEAR: answer.
  ELSE.
    answer = '1'.
  ENDIF.

*START-OF-SELECTION.
  IF answer = '1'.  " Yes
    PERFORM open_group.
    LOOP AT it INTO wa.
********************************************************************** - new bdc start 2
      IF wa-ven_class EQ '0'.
        CLEAR:ls_j_1ipanno,ls_j_1ipanref.
        ls_j_1ipanno = wa-gv_j_1ipanno.
        ls_j_1ipanref = wa-gv_j_1ipanno.
        wa-stcd3 = 'UN-REGISTERED'.
      ELSE.
        CLEAR:ls_j_1ipanno,ls_j_1ipanref.
        ls_j_1ipanno = wa-gv_j_1ipanno.
        ls_j_1ipanref = wa-gv_j_1ipanno.
      ENDIF.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0100'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'USE_ZAV'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'RF02K-BUKRS'
                                    wa-bukrs."'1000'.
      PERFORM bdc_field       USING 'RF02K-EKORG'
                                    wa-ekorg."'1000'.
      IF wa-ktokk = '1000' OR wa-ktokk = '2000' OR wa-ktokk = '3000' OR wa-ktokk = '5000'.
        PERFORM bdc_field       USING 'RF02K-KTOKK'
                                      wa-ktokk."'1000'.
      ELSE.
        MESSAGE 'Kindly maintained Account Group 1000 , 2000 , 3000 or 5000 for creation'
        TYPE 'E'." DISPLAY LIKE 'E'.
      ENDIF.
      PERFORM bdc_field       USING 'USE_ZAV'
                                    'X'.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=OPFI'.

      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SZA1_D0100-SMTP_ADDR'.
      PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'
                                    wa-name1."'EVEREST INDUSTRIES LIMITED'.
      PERFORM bdc_field       USING 'ADDR1_DATA-NAME2'
                                     wa-name2."'DEXO FINE CHEM PVT LTD'.
      gv_sort1 = wa-name1.
      PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'
                                    gv_sort1."wa-sort1."'DEXO FINE CHEM PVT L'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL1'
                                    wa-str_suppl1."'street2'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL2'
                                    wa-str_suppl2."'street3'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STREET'
                                    wa-street."'streethouseno'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL3'
                                    wa-str_suppl3."'street4'.
      PERFORM bdc_field       USING 'ADDR1_DATA-LOCATION'
                                    wa-location."'street5'.
      PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1'
                                    wa-post_code1.          "'392130'.
      PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'
                                    wa-city1."'BHARUCH'.
      PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'
                                    wa-country."'IN'.
      PERFORM bdc_field       USING 'ADDR1_DATA-REGION'
                                    wa-region."'06'.
      PERFORM bdc_field       USING 'ADDR1_DATA-LANGU'
                                    'EN'.
      PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'
                                    wa-tel_number."'9920038227'.
      PERFORM bdc_field       USING 'SZA1_D0100-MOB_NUMBER'
                                    wa-mob_number."'9920038227'.
      PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'
                                    wa-fax_number."'123'.
      PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'
                                    wa-smtp_addr."'test@gmail.com'.

      PERFORM bdc_dynpro      USING 'SAPLJ1I_MASTER' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=CIN_VENDOR_FC3'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
*                                    'J_1IMOVEND-J_1IEXCD'.
                                    'GS_LFA1-J_1IEXCD'.
      PERFORM bdc_dynpro      USING 'SAPLJ1I_MASTER' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=CIN_VENDOR_FC5'.
*      perform bdc_field       using 'BDC_CURSOR'
*                                    'J_1IMOVEND-J_1IPANREF'.
*      perform bdc_field       using 'J_1IMOVEND-J_1IPANNO'
*                                    ls_j_1ipanno."'testnkpan'.
*      perform bdc_field       using 'J_1IMOVEND-J_1IPANREF'
*                                    ls_j_1ipanref."'testnkpan'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'GS_LFA1-J_1IPANREF'.
      PERFORM bdc_field       USING 'GS_LFA1-J_1IPANNO'
                                    ls_j_1ipanno."'testnkpan'.
      PERFORM bdc_field       USING 'GS_LFA1-J_1IPANREF'
                                    ls_j_1ipanref."'testnkpan'.
      PERFORM bdc_dynpro      USING 'SAPLJ1I_MASTER' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'J_1IMOVEND-VEN_CLASS'.
      PERFORM bdc_field       USING 'J_1IMOVEND-VEN_CLASS'
*      perform bdc_field       using 'BDC_CURSOR'
*                                    'GS_LFA1-VEN_CLASS'.
*      perform bdc_field       using 'GS_LFA1-VEN_CLASS'
                                    wa-ven_class."'0'.
      PERFORM bdc_dynpro      USING 'SAPLJ1I_MASTER' '0100'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=BACK'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'J_1IMOVEND-VEN_CLASS'.
      PERFORM bdc_field       USING 'J_1IMOVEND-VEN_CLASS'
                                    wa-ven_class."'0'.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'SZA1_D0100-SMTP_ADDR'.
      PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'
                                        wa-name1."'EVEREST INDUSTRIES LIMITED'.
      PERFORM bdc_field       USING 'ADDR1_DATA-NAME2'
                                     wa-name2."'DEXO FINE CHEM PVT LTD'.
      gv_sort1 = wa-name1.
      PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'
                                    gv_sort1."wa-sort1."'DEXO FINE CHEM PVT L'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL1'
                                    wa-str_suppl1."'street2'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL2'
                                    wa-str_suppl2."'street3'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STREET'
                                    wa-street."'streethouseno'.
      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL3'
                                    wa-str_suppl3."'street4'.
      PERFORM bdc_field       USING 'ADDR1_DATA-LOCATION'
                                    wa-location."'street5'.
      PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1'
                                    wa-post_code1.          "'392130'.
      PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'
                                    wa-city1."'BHARUCH'.
      PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'
                                    wa-country."'IN'.
      PERFORM bdc_field       USING 'ADDR1_DATA-REGION'
                                    wa-region."'06'.
      PERFORM bdc_field       USING 'ADDR1_DATA-LANGU'
                                    'EN'.
      PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'
                                    wa-tel_number."'9920038227'.
      PERFORM bdc_field       USING 'SZA1_D0100-MOB_NUMBER'
                                    wa-mob_number."'9920038227'.
      PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'
                                    wa-fax_number."'123'.
      PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'
                                    wa-smtp_addr."'test@gmail.com'.
      IF wa-ven_class NE '0'.
*      PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
*      PERFORM bdc_field       USING 'BDC_CURSOR'
*                                    'LFA1-STCD3'.
*      PERFORM bdc_field       USING 'BDC_OKCODE'
*                                    '/00'.
        SELECT SINGLE lifnr stcd3 FROM lfa1 INTO (gv_lifnr3, gv_stcd3)
                WHERE stcd3 = wa-stcd3.
        IF sy-subrc NE 0.

          PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'LFA1-STCD3'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM bdc_field       USING 'LFA1-STCD3'
                                        wa-stcd3.
*        PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
*        PERFORM bdc_field       USING 'BDC_CURSOR'
*                                      'LFA1-STCD3'.
*        PERFORM bdc_field       USING 'BDC_OKCODE'
*                                      '/00'.
**perform bdc_field       using 'LFA1-STCD3'
**                              'UN-REGISTERED'.
*        PERFORM bdc_field       USING 'LFA1-STCD3'
*                                      wa-stcd3."'UN-REGISTERED'.
        ELSEIF sy-subrc EQ 0.
*** commented by NK as discussed by Varmaji
*        MESSAGE 'Kindly maintained unique GSTN number for Vendor'
*        TYPE 'W' DISPLAY LIKE 'E'.

          PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'LFA1-STCD3'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '/00'.
          PERFORM bdc_field       USING 'LFA1-STCD3'
                                        wa-stcd3.

        ENDIF.
      ELSE.
*      gv_J_1IPANNO.
        ls_j_1ipanno = wa-gv_j_1ipanno.
        ls_j_1ipanref = wa-gv_j_1ipanno.
        wa-stcd3 = 'UN-REGISTERED'.
        PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'LFA1-STCD3'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '/00'.
        PERFORM bdc_field       USING 'LFA1-STCD3'
                                      wa-stcd3."'UN-REGISTERED'.
      ENDIF.

      PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LFBK-BANKS(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      IF wa-waers = 'INR' AND wa-bankl is NOT INITIAL.
        PERFORM bdc_field       USING 'LFBK-BANKS(01)'
                                      wa-banks."'IN'.
        PERFORM bdc_field       USING 'LFBK-BANKL(01)'
                                      wa-bankl."'TESTBANK02'.
        PERFORM bdc_field       USING 'LFBK-BANKN(01)'        " Reposition: IHDK900161
                                      wa-bankn."'TEST BANK A'.
        SELECT SINGLE * FROM bnka INTO ls_bnka WHERE bankl = wa-bankl.
        IF sy-subrc NE 0.
          PERFORM bdc_dynpro      USING 'SAPLBANK' '0100'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'BNKA-SWIFT'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTR'.
          PERFORM bdc_field       USING 'BNKA-BANKA'
                                        wa-banka."'TEST BANK A'.
          PERFORM bdc_field       USING 'BNKA-PROVZ'
                                        wa-provz."'13'.
          PERFORM bdc_field       USING 'BNKA-STRAS'
                                        wa-stras."'TEST-1'.
          PERFORM bdc_field       USING 'BNKA-ORT01'
                                        wa-ort01."'MUMBAI'.
          PERFORM bdc_field       USING 'BNKA-BRNCH'
                                        wa-brnch."'GHATKOPAR'.
          PERFORM bdc_field       USING 'BNKA-SWIFT'
                                        wa-swift.           "'12345'.
          PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
          PERFORM bdc_field       USING 'BDC_CURSOR'
                                        'LFBK-KOINH(01)'.
          PERFORM bdc_field       USING 'BDC_OKCODE'
                                        '=ENTR'.
        ENDIF.
        PERFORM bdc_field       USING 'LFBK-KOINH(01)'            " IHDK900163
                                      wa-koinh."'TESTACCTHOLDER'.
        PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'LFBK-KOINH(01)'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=ENTR'.
      ENDIF.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0380'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'KNVK-NAME1(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      PERFORM bdc_field       USING 'KNVK-NAMEV(01)'
                                    wa-namev_01."'SUDHIR'.
      PERFORM bdc_field       USING 'KNVK-NAME1(01)'
                                    wa-name1_01."'SHARMA'.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0380'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'KNVK-NAMEV(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LFB1-AKONT'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      IF wa-akont IS NOT INITIAL.
        PERFORM bdc_field       USING 'LFB1-AKONT'
                                     wa-akont." '15105001'.
      ELSE.
        MESSAGE 'Kindly maintianed Reconciliation Account'
          TYPE 'W' DISPLAY LIKE 'E'.
      ENDIF.
      IF wa-frgrp IS NOT INITIAL.
        SELECT SINGLE frgrp FROM vbwf08 INTO wa-frgrp
          WHERE frgrp = wa-frgrp.
        IF sy-subrc EQ 0.
          PERFORM bdc_field       USING 'LFB1-FRGRP'
                                        wa-frgrp."'0007'.
        ELSE.
          MESSAGE 'Maintain valid Vendor Category' TYPE 'E'.
        ENDIF.
      ENDIF.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LFB1-ZTERM'.
**      perform bdc_field       using 'BDC_OKCODE'                                             "--IRDK933193
**                                    '/00'.                                                   "--IRDK933193
      PERFORM bdc_field       USING 'LFB1-ZTERM'                                               "++IRDK933193
                                     wa-zterm.                                        "'Z006'. "++IRDK933193
      PERFORM bdc_field       USING 'LFB1-ZWELS'      " IRDK932921
                                      wa-zwels. "'EFI'.
      PERFORM bdc_field       USING 'LFB1-HBKID'      " IRDK932921
                                    wa-hbkid. "'IND06'.

      PERFORM bdc_dynpro      USING 'SAPMF02K' '0220'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LFB5-MAHNA'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LFBW-QSREC(02)'.
      PERFORM bdc_field       USING 'LFB1-QLAND'
                                    wa-qland."'IN'.
      PERFORM bdc_field       USING 'LFBW-WITHT(01)'
                                    wa-witht1."'C1'.
      READ TABLE gt_t059u INTO gs_t059u WITH KEY witht = wa-witht1.
      IF sy-subrc EQ 0.
        IF wa-witht1 = 'C1'.
          gv_witht2 = 'C2'.
        ELSEIF wa-witht1 = 'A1'.
          gv_witht2 = 'A2'.
        ELSEIF wa-witht1 = 'F1'.
          gv_witht2 = 'F2'.
        ELSEIF wa-witht1 = 'H1'.
          gv_witht2 = 'H2'.
        ELSEIF wa-witht1 = 'I1'.
          gv_witht2 = 'I2'.
        ELSEIF wa-witht1 = 'I3'.  " added by NK on 05.10.2017
          gv_witht2 = 'I4'.
        ELSEIF wa-witht1 = 'I5'.
          gv_witht2 = 'I6'.
        ELSEIF wa-witht1 = 'J1'.
          gv_witht2 = 'J2'.
        ENDIF.
        PERFORM bdc_field       USING 'LFBW-WITHT(02)'
                                     gv_witht2." 'C2'.
      ENDIF.
      PERFORM bdc_field       USING 'LFBW-WT_WITHCD(01)'
                                    wa-wt_withcd."'10'.
      PERFORM bdc_field       USING 'LFBW-WT_SUBJCT(01)'
                                    wa-wt_subjct1."'X'.
      IF wa-wt_subjct1 IS NOT INITIAL.
        gv_wt_subjct2 = wa-wt_subjct1.
        PERFORM bdc_field       USING 'LFBW-WT_SUBJCT(02)'
                                      gv_wt_subjct2."'X'.
      ENDIF.
      PERFORM bdc_field       USING 'LFBW-QSREC(01)'
                                    wa-qsrec1."'OT'.
      IF wa-qsrec1 IS NOT INITIAL.
        gv_qsrec2 = wa-qsrec1.
        PERFORM bdc_field       USING 'LFBW-QSREC(02)'
                                      gv_qsrec2."'OT'.
      ENDIF.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENTR'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LFBW-QSREC(02)'.
      PERFORM bdc_field       USING 'LFB1-QLAND'
                                    wa-qland."'IN'.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0310'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'LFM1-LEBRE'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'LFM1-WAERS'
                                    wa-waers."'INR'.
      PERFORM bdc_field       USING 'LFM1-ZTERM'
                                    wa-zterm."'V090'.
      PERFORM bdc_field       USING 'LFM1-INCO1'
                                    wa-inco1."'ALL'.
      PERFORM bdc_field       USING 'LFM1-INCO2'
                                    wa-inco2."'DELIVERED'.
      PERFORM bdc_field       USING 'LFM1-KALSK'
                                    wa-kalsk."'01'.
      PERFORM bdc_field       USING 'LFM1-WEBRE'
                                    wa-webre."'X'.
      PERFORM bdc_field       USING 'LFM1-LEBRE'
                                    wa-lebre."'X'.
      PERFORM bdc_dynpro      USING 'SAPMF02K' '0320'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'WYT3-PARVW(01)'.
*                              'WRF02K-GPARN(04)'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=ENTR'.
*perform bdc_field       using 'WYT3-PARVW(04)'
*                              'ER'.
*perform bdc_field       using 'WRF02K-GPARN(04)'
*                              '10106'.
*perform bdc_dynpro      using 'SAPMF02K' '0320'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'WRF02K-GPARN(04)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=UPDA'.

      " " Re-enable DMS field when DMS is activated for vendor; IRDK932938
*      data lv_flag type flag.
*      lv_flag = abap_true.  " flag to skip dms doc mandatory check in customer master during creation via ZSD010
*      export lv_flag from lv_flag to memory id 'ZNEW_VENDOR'.
*
*      " IRDK931098 changed code position to before call transaction so that vendor creation can be halter for invalid dms doc
*      " Begin code to update dms doc in master - IRDK930677
*      data lifnr type bdcdata-fval.
*      data doknr type bdcdata-fval.
*      data lv_dokvr type draw-dokvr.
*      data dokvr type bdcdata-fval.
*
*      clear: lifnr, doknr, dokvr.
*
*      condense messtab-msgv1.
*
*      lifnr = messtab-msgv1.  " Created vendor code
*
*      clear lv_dokvr.
*      select single max( dokvr )      " IRDK930973
*        from draw
*        into lv_dokvr
*        where doknr = wa-dms_doc
*        and   dokst = 'FR'. " Released only
*
*      " IRDK931098: MM: S_K: ZNEW_VENDOR: DMS: dms doc mandat., check released?
*      if wa-dms_doc is initial.
*        message 'DMS Document is mandatory' type 'E'. " to-do, log the error and continue without creating this vendor line
*      endif.
*
*      if lv_dokvr is initial.
*        message 'Latest version of DMS document is not yet released' type 'E'.
*      endif.
*      " End IRDK931098
      " End IRDK932938
      lv_country   = wa-country.
      lv_mob_number = wa-mob_number.


      FREE MEMORY ID 'PAN'.


      EXPORT ls_j_1ipanno lv_country lv_mob_number TO MEMORY ID 'PAN'.

      PERFORM bdc_transaction USING 'XK01'. " no error handling?

      " " Re-enable DMS field when DMS is activated for vendor; IRDK932938
*      doknr = wa-dms_doc.
*      dokvr = lv_dokvr.
*
*      free memory id 'ZNEW_VENDOR'.
*      call function 'ZFM_XK02_DMS_DOC'
*        exporting
*          lifnr_001    = lifnr
*          doknr_01_013 = doknr
*          dokvr_01_014 = dokvr.
*      " End code - IRDK930677
      " End IRDK932938


      CLEAR: wa.
    ENDLOOP.
********************************************************************** - new bdc end 2
    PERFORM close_group.
  ELSEIF answer = '2'.  " No
**Do nth
    CALL TRANSACTION 'ZNEW_VENDOR'.
  ELSEIF answer = 'A'.  " Cancel
**Do nth
    CALL TRANSACTION 'ZNEW_VENDOR'.
*    EXIT.
*    LEAVE LIST-PROCESSING.
* elseif answer = 'A'.  " Window closed
**Do nth
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FILE
*&---------------------------------------------------------------------*
FORM validate_file.

  TYPES: BEGIN OF lty_t052u,
           zterm TYPE t052u-zterm,
         END OF lty_t052u.

  DATA : lt_bdc   TYPE TABLE OF ty_file,
         lt_t052u TYPE TABLE OF lty_t052u,
         ls_t052u TYPE lty_t052u.

  lt_bdc[] = it[].
  REFRESH : it[].

  SELECT zterm
         FROM t052u
         INTO TABLE lt_t052u
         WHERE spras EQ 'EN'.

  SORT : lt_t052u BY zterm.

  LOOP AT lt_bdc INTO wa.
    IF wa-zterm IS INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE lt_t052u INTO ls_t052u
                        WITH KEY zterm = wa-zterm
                        BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ELSE.
      wa-webre = 'X'.
      wa-lebre = 'X'.
    ENDIF.

    APPEND : wa TO it.
    CLEAR : wa.
  ENDLOOP.

  IF it[] IS INITIAL.
    MESSAGE 'Upload file with Valid Payment Terms' TYPE 'I' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
