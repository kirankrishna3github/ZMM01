*&---------------------------------------------------------------------*
*& Report  ZMM_EXTEND_MAT_SALORG
*&
*&---------------------------------------------------------------------*
*& Author: Saurabh Khare (Adroit Infotech Ltd)
*& Date: Wednesday, July 05, 2017 19:04:46
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*---<< S/4HANA >>---*
*&---------------------------------------------------------------------*
* Changed On - Tuesday, October 16, 2018 16:10:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - Syntactically incompatible change
*              of existing functionality 'BAPI_MATERIAL_SAVEDATA'
* Solution   - Add Material Long Field
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*&---------------------------------------------------------------------*

REPORT zmm_extend_mat_salorg.

* Data Declaration *
* Tables *
TABLES: mara, marc, mvke, mast, mlan.

* Types *
TYPES: BEGIN OF ty_mlan,        " Structure for tax classification data
         matnr TYPE mlan-matnr,
       END OF ty_mlan,

       BEGIN OF ty_data,        " Structure for consolidated data
         matnr TYPE mara-matnr,
       END OF ty_data,

       BEGIN OF ty_log,         " Structure for log file
         matnr TYPE matnr,
         msg   TYPE string,
       END OF ty_log.

* Internal Tables *
DATA: it_view TYPE TABLE OF mvke,     " Sales Data
      wa_view LIKE LINE OF it_view,

      it_mara TYPE TABLE OF mara,     " Material Data
      wa_mara TYPE mara,

      it_sfg  TYPE TABLE OF mara,     " Table for separate processing of Semifinished Goods(ZSFG)
      wa_sfg  TYPE mara,

      it_mast TYPE TABLE OF mast,     " Material BOM data (for ZSFG type materials)
      wa_mast TYPE mast,

      wa_mvke TYPE mvke,              " Sales data

      it_marc TYPE TABLE OF marc,     " Plant Data
      wa_marc TYPE marc,

      it_mlan TYPE TABLE OF ty_mlan,  " Tax classification data
      wa_mlan TYPE ty_mlan,

      it_data TYPE TABLE OF ty_data,  " Consolidated data for material extension
      wa_data TYPE ty_data,

      it_log  TYPE TABLE OF ty_log,   " Log file
      wa_log  TYPE ty_log.

* Variables *
* BAPI_MATERIAL_SAVEDATA *
DATA: material TYPE matnr,      " Material Number
      subrc    TYPE sy-subrc,   " Stores BOM where used FM's result
      lines(5) TYPE c,          " Table line count
      msg      TYPE string,     " Message
      answer   TYPE c,          " Answer from confirmation pop-up
      v_stlan  TYPE mast-stlan, " BOM Usage
      min_grg  LIKE sy-datum   VALUE '19000101',  " 01.01.1900 - Valid From for BOM Usage FM
      max_grg  LIKE sy-datum   VALUE '99991231'.  " 31.12.9999 - Valid To for BOM Usage FM

* BAPI Related *
DATA: headdata    TYPE bapimathead,
      clientdata  TYPE bapi_mara,   " MARA
      clientdatax TYPE bapi_marax,  " Checkbox
      plantdata   TYPE bapi_marc,   " MARC
      plantdatax  TYPE bapi_marcx,  " Checkbox
      salesdata   TYPE bapi_mvke,   " MVKE
      salesdatax  TYPE bapi_mvkex,  " Checkbox
      return      TYPE bapiret2.    " return structure

DATA: taxclassifications TYPE TABLE OF bapi_mlan WITH HEADER LINE,  " MLAN
      returnmessages     TYPE TABLE OF bapiret2 WITH HEADER LINE,   " Return table with messages
      errormessages      TYPE TABLE OF bapiret2,
      wa_errormessages   TYPE bapiret2.

* STEUERTAB_IDENTIFY *
DATA: steuertab TYPE TABLE OF mg03steuer WITH HEADER LINE.  " Tax types for sal. org. / distr. channel

* CS_WHERE_USED_MAT *
DATA: topmat TYPE mc29s.  " Data on the initial material

DATA: wultb   TYPE TABLE OF stpov   WITH HEADER LINE,   " WHERE-USED list table (items)
      equicat TYPE TABLE OF cscequi WITH HEADER LINE,   " Mandatory
      kndcat  TYPE TABLE OF cscknd  WITH HEADER LINE,   " Mandatory
      matcat  TYPE TABLE OF cscmat  WITH HEADER LINE,   " Mandatory
      stdcat  TYPE TABLE OF cscstd  WITH HEADER LINE,   " Mandatory
      tplcat  TYPE TABLE OF csctpl  WITH HEADER LINE.   " Mandatory

* File output related *
* cl_gui_frontend_services=>file_save_dialog *
* cl_gui_frontend_services=>gui_download *
DATA: it_errfname TYPE TABLE OF fieldnames,
      wa_errfname TYPE fieldnames,
      it_logfname TYPE TABLE OF fieldnames,
      wa_logfname TYPE fieldnames.

DATA: file       TYPE string,
      path       TYPE string,
      file_path  TYPE string,
      title      TYPE string,
      defname    TYPE string,
      defext     TYPE string,
      useraction TYPE i.

* Selection Screen *
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_matnr FOR mara-matnr.
PARAMETERS:     p_werks TYPE marc-werks OBLIGATORY,
                p_vkorg TYPE mvke-vkorg OBLIGATORY,
                p_vtweg TYPE mvke-vtweg OBLIGATORY,
                p_ktgrm type mvke-ktgrm.  " IHDK905589
SELECTION-SCREEN END OF BLOCK b1.

* Start of Main *
START-OF-SELECTION.

  PERFORM validation.
  PERFORM data_retreival.
  PERFORM sfg_specific_processing.

END-OF-SELECTION.
  PERFORM check_extensibility.
  PERFORM material_extension.
  IF  answer EQ '1'.
    PERFORM log_output.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  DATA_RETREIVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_retreival .
  " Get valid materials
  SELECT *
    FROM mara
    INTO TABLE it_mara
  WHERE matnr IN s_matnr.   " Add filter for ZRAW, ZPKG, ZSFG ?

  IF it_mara[] IS NOT INITIAL.
    " Get materials that belong to the given plant from the list of valid ones
    SELECT *
      FROM marc
      INTO TABLE it_marc
      FOR ALL ENTRIES IN it_mara
      WHERE matnr = it_mara-matnr
    AND   werks = p_werks.

    IF it_marc[] IS NOT INITIAL.
      " Get materials that have already been extended to the given combination
      SELECT *
        FROM mvke
        INTO TABLE it_view
        FOR ALL ENTRIES IN it_marc
        WHERE matnr = it_marc-matnr
        AND   vkorg = p_vkorg
      AND   vtweg = p_vtweg.

      IF it_view[] IS NOT INITIAL.
        " Get materials that have been extended but contain wrong tax classification
        SELECT matnr
          FROM mlan
          INTO TABLE it_mlan
          FOR ALL ENTRIES IN it_view
          WHERE matnr = it_view-matnr
          AND  ( ( aland EQ 'IN'        " IN with 9 taxes (ZRAW, ZPKG, ZSFG), BE with 1 tax(ZPKG, ZSFG)
          AND   ( taxm1 NE '0' OR
                  taxm2 NE '0' OR
                  taxm3 NE '0' OR
                  taxm4 NE '0' OR
                  taxm5 NE '0' OR
                  taxm6 NE '0' OR
                  taxm7 NE '0' OR
                  taxm8 NE '0' OR
                  taxm9 NE '0' ) )
          OR ( aland EQ 'BE'
        AND taxm1 NE '0' ) ).
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&----------------------------------------------------------------------------------------------------------------*
*&      Form  CHECK_EXTENSIBILITY
*&----------------------------------------------------------------------------------------------------------------*
* 1. it_data[] contains all the materials in the specified range whether valid or invalid. We check mara and filter
* out only the valid ones. Discarded materials will be logged as invalid.

* 2. it_data[] contains all valid materials. We check marc and filter out materials that belong to the entered plant.
* Material will not be extended to the plant if it is not maintained for it in the first place. Discarded materials
* are logged.

* 3. it_data[] contains all valid materials maintained in the plant entered by the user. We check mvke/view to
* discard materials that have already been extended to the given combination of sal.org/distr. channel.
* Correctness of plant has already been checked. If it is extended it will be discarded and logged irrespective of
* whether the sales view data is incomplete/incorrect (Not part of the requirement/functionality).

* Note: This step could have ben skipped and all valid materials entered by the user maintained in the plant given by
* the user could have been extended blindly overwriting the already extended ones. But this step increases the
* performance many-fold by eliminating unnecessary processing cycles.

* 4. Now as per an additional requirement we check the correctness of tax classification(All tax class value
* should be 0) in the already extended materials (though all other data  is not checked). mlan will contain
* materials that have been extended to the given comb. but their tax class. is inconsistent. Such materials are not
* deleted from it_data[] in the third step, thus keeping them alive for extension/modification.
*-----------------------------------------------------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*-----------------------------------------------------------------------------------------------------------------*
FORM check_extensibility .
* Remove invalid materials *
*  IF it_mara[] IS NOT INITIAL.
  IF it_data[] IS NOT INITIAL.
    LOOP AT it_data INTO wa_data.
      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_data-matnr.
      IF sy-subrc <> 0.
        wa_log-matnr = wa_data-matnr.
        wa_log-msg   = 'Invalid material'.
        APPEND wa_log TO it_log.
        DELETE it_data WHERE matnr = wa_data-matnr.
      ENDIF.
      CLEAR: wa_data, wa_mara.
    ENDLOOP.
  ENDIF.
*  ENDIF.

* Remove materials not maintained in given plant *
*  IF it_marc[] IS NOT INITIAL.
  IF it_data[] IS NOT INITIAL.
    LOOP AT it_data INTO wa_data.
      READ TABLE it_marc INTO wa_marc WITH KEY matnr = wa_data-matnr.
      IF sy-subrc <> 0.
        wa_log-matnr = wa_data-matnr.
        wa_log-msg   = 'Material not maintained for given plant'.
        APPEND wa_log TO it_log.
        DELETE it_data WHERE matnr = wa_data-matnr.
      ENDIF.
      CLEAR: wa_data, wa_marc.
    ENDLOOP.
  ENDIF.
*  ENDIF.

* Remove materials already extended to given combination *
*  IF it_view[] IS NOT INITIAL.
  IF it_data[] IS NOT INITIAL.
    LOOP AT it_data INTO wa_data.
      READ TABLE it_view INTO wa_view WITH KEY matnr = wa_data-matnr.
      IF sy-subrc = 0.
        " materials that have been extended but tax classification is not consistent
        READ TABLE it_mlan INTO wa_mlan WITH KEY matnr = wa_view-matnr.
        IF sy-subrc <> 0. " Do not delete such materials <-- Kept alive
          wa_log-matnr = wa_data-matnr.
          wa_log-msg   = 'Material already extended to given plant/sales org/distr. channel'.
          APPEND wa_log TO it_log.
          DELETE it_data WHERE matnr = wa_data-matnr.
        ENDIF.
      ENDIF.
      CLEAR: wa_data, wa_marc.
    ENDLOOP.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validation .
* Make material mandatory *
  IF s_matnr IS INITIAL.
    MESSAGE 'Please enter material' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ELSE.
* Get all materials in specified range *
    REFRESH it_data[].
    IF s_matnr-option EQ 'BT'.
      WHILE NOT s_matnr-low GT s_matnr-high.
        wa_data-matnr = s_matnr-low.
        APPEND wa_data TO it_data.
        ADD 1 TO s_matnr-low.
        UNPACK s_matnr-low TO s_matnr-low.
        CLEAR: wa_data.
      ENDWHILE.
    ENDIF.
    IF s_matnr-option EQ 'EQ'.
      LOOP AT s_matnr.
        wa_data-matnr = s_matnr-low.
        APPEND wa_data TO it_data.
        CLEAR: wa_data.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MATERIAL_EXTENSION
*&---------------------------------------------------------------------*
* 1. For SFG type material, head material is first passed to the where-
* -used FM. It gives all the top materials of the BOM's in which the
* head material is a component.
* 2. One of the top material is selected and recursively passed to the
* FM until no further top material can be found. The previous material
* is thus the top-most material (FG).
* 3. Sales data for this topmost material is fetched from mvke and used
* in the extension of the head material.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM material_extension .
  IF it_data[] IS NOT INITIAL.
* Count extendable materials *
    DESCRIBE TABLE it_data[] LINES lines.
    CLEAR: msg, answer.
    SHIFT lines LEFT DELETING LEADING space.
    CONCATENATE lines 'materials will be extended/updated to given plant/sales.org/distr. chan. combination. Continue?'
    INTO msg SEPARATED BY space.
* Confirm extension action *
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Extend Confirmation'
        text_question  = msg
        text_button_1  = 'Yes'
        text_button_2  = 'No'
*       START_COLUMN   = 25
*       START_ROW      = 6
      IMPORTING
        answer         = answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF answer = '1'.  " answer 1 - Yes, 2 - No, A - Cancel
      LOOP AT it_data INTO wa_data.
        CLEAR headdata.
        headdata-material = wa_data-matnr.
        headdata-material_long = wa_data-matnr.
        headdata-ind_sector = 'C'.
        READ TABLE it_mara INTO wa_mara WITH KEY matnr = headdata-material.
        IF sy-subrc = 0.
          headdata-matl_type = wa_mara-mtart.
        ENDIF.
        headdata-basic_view = 'X'.
        headdata-sales_view = 'X'.

        CLEAR clientdata.
        clientdata-trans_grp = '0001'.

        CLEAR clientdatax.
        clientdatax-trans_grp = 'X'.

        CLEAR plantdata.
        plantdata-plant = p_werks.
        plantdata-loadinggrp = 'Z001'.

        CLEAR plantdatax.
        plantdatax-plant = p_werks.
        plantdatax-loadinggrp = 'X'.

        CLEAR salesdata.
        salesdata-sales_org = p_vkorg.
        salesdata-distr_chan = p_vtweg.
        salesdata-item_cat = 'NORM'.

        IF headdata-matl_type EQ 'ZSFG'.
          " Get BOM data recursively until the top material is reached
          " Then get the sales data of that material for entered comb. from mvke
          " Use that info to fill the matl groups and acct cat for this material.
          CLEAR: wa_mast, v_stlan.
          " mast is bom -> material link table. If no BOM exists for the current head material
          " means no top-FG material is possible. So, continue without extending this material
          " and log it.
          READ TABLE it_mast INTO wa_mast WITH KEY matnr = headdata-material.
          IF sy-subrc = 0.
            v_stlan = wa_mast-stlan.
          ELSE.
            wa_log-matnr = headdata-material.
            wa_log-msg   = 'No BOM finished product found. Skipping!'.
            APPEND wa_log TO it_log.
            CLEAR wa_log.
            CONTINUE.
          ENDIF.
          material = headdata-material.
          WHILE subrc = 0.  " subrc 0 -> Further BOM implosion might be possible
            REFRESH: wultb[], equicat[], kndcat[], matcat[], stdcat[], tplcat[].
            CLEAR: subrc, topmat.
            CALL FUNCTION 'CS_WHERE_USED_MAT' " Find top materials of BOM's in which the current material is a component
              EXPORTING
                datub                      = max_grg
                datuv                      = min_grg
                matnr                      = material
*               POSTP                      = ' '
*               RETCODE_ONLY               = ' '
                stlan                      = v_stlan
                werks                      = p_werks
              IMPORTING
                topmat                     = topmat
              TABLES
                wultb                      = wultb    " BOM usage table with top materials for each BOM
                equicat                    = equicat
                kndcat                     = kndcat
                matcat                     = matcat
                stdcat                     = stdcat
                tplcat                     = tplcat
              EXCEPTIONS
                call_invalid               = 1
                material_not_found         = 2
                no_where_used_rec_found    = 3
                no_where_used_rec_selected = 4
                no_where_used_rec_valid    = 5
                OTHERS                     = 6.
            IF sy-subrc <> 0. " No further BOM's/Top materials, No further BOM implosion possible, Reached Root Node
              subrc = sy-subrc.
              " Implement suitable error handling here
              EXIT.
            ELSE. " Root not yet confirmed, further BOM implosion might be possible
              CLEAR: material, wultb.
              READ TABLE wultb INDEX 1 TRANSPORTING matnr.
              IF sy-subrc = 0.
                material = wultb-matnr. " Top material obtained for next iteration
              ENDIF.
            ENDIF.
          ENDWHILE.

          " If subrc <> 0, either a topmost material has been found or the head material
          " was not used in any bom at all. In which case, material will be to EQ head-
          " -material as we pass the head-material as the first material in the first
          " recursion of the FM. So we check that this condition is not true else we log
          " the material and do not extend it.
          IF subrc <> 0 AND material IS NOT INITIAL AND material NE headdata-material.
            " Get sales data for top material
            SELECT SINGLE *
              FROM mvke
              INTO wa_mvke
            WHERE matnr = material.
*              AND   vkorg = p_vkorg.   " May not be maintained for the given sal. org

            IF wa_mvke IS NOT INITIAL.
              salesdata-acct_assgt  = cond #( when p_ktgrm is not initial
                                              then p_ktgrm
                                              else wa_mvke-ktgrm ). " IHDK905589
              salesdata-matl_grp_1  = wa_mvke-mvgr1.
              salesdata-matl_grp_2  = wa_mvke-mvgr2.
              salesdata-matl_grp_3  = wa_mvke-mvgr3.
              salesdata-matl_grp_4  = wa_mvke-mvgr4.
              salesdata-matl_grp_5  = wa_mvke-mvgr5.
            ENDIF.
          ELSE. " Head material was not part of any BOM or an error occured during BOM implosion - > Do not extend
            wa_log-matnr = headdata-material.
            wa_log-msg   = 'No BOM finished product found. Skipping!'.
            APPEND wa_log TO it_log.
            CLEAR wa_log.
            CONTINUE.
          ENDIF.
        ENDIF.  " IF headdata-matl_type EQ 'ZSFG'.

        CLEAR salesdatax.
        salesdatax-sales_org = p_vkorg.
        salesdatax-distr_chan = p_vtweg.
        salesdatax-item_cat = 'X'.
        " Checks necessary again for checkbox structure to be consistent with value structure
        IF headdata-matl_type = 'ZSFG'
          AND subrc <> 0
          AND material IS NOT INITIAL
          AND material NE headdata-material
          AND wa_mvke IS NOT INITIAL.
          salesdatax-acct_assgt = 'X'.
          salesdatax-matl_grp_1 = 'X'.
          salesdatax-matl_grp_2 = 'X'.
          salesdatax-matl_grp_3 = 'X'.
          salesdatax-matl_grp_4 = 'X'.
          salesdatax-matl_grp_5 = 'X'.
        ENDIF.

        REFRESH: taxclassifications[], steuertab[].
        CLEAR: taxclassifications.
        " Get tax classification data for sal. org/distr. channel combination given by user.
        CALL FUNCTION 'STEUERTAB_IDENTIFY'
          EXPORTING
*           KZRFB                 = ' '
            vkorg                 = salesdata-sales_org
            bukrs_vkorg           = salesdata-sales_org
            vtweg                 = salesdata-distr_chan
          TABLES
            steuertab             = steuertab
          EXCEPTIONS
            wrong_call            = 1
            vkorg_bukrs_not_found = 2
            steuertab_empty       = 3
            OTHERS                = 4.
        IF sy-subrc <> 0.
*Implement suitable error handling here
        ELSE.
          LOOP AT steuertab.
            taxclassifications-depcountry = steuertab-aland.
            taxclassifications-tax_type_1 = steuertab-tatyp.
            taxclassifications-taxclass_1 = '0'.
            APPEND taxclassifications.
            CLEAR: steuertab, taxclassifications.
          ENDLOOP.
        ENDIF.

        CLEAR: return.
        REFRESH: returnmessages[].
        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
          EXPORTING
            headdata           = headdata
            clientdata         = clientdata
            clientdatax        = clientdatax
            plantdata          = plantdata
            plantdatax         = plantdatax
            salesdata          = salesdata
            salesdatax         = salesdatax
          IMPORTING
            return             = return
          TABLES
            taxclassifications = taxclassifications
            returnmessages     = returnmessages.

        " Begin read returnmessages
        READ TABLE returnmessages WITH KEY type = 'E' TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.  " Error exists, log it in error log
          LOOP AT returnmessages WHERE type = 'E'.
            MOVE-CORRESPONDING returnmessages TO wa_errormessages.
            wa_errormessages-message_v4 = headdata-material.
            APPEND wa_errormessages TO errormessages.
            CLEAR: wa_errormessages, returnmessages.
          ENDLOOP.
        ELSE. " No error
          IF return-number = '356' AND return-type = 'S'. " Check if extension succeeded (MSG 356) and log it in status log
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
            wa_log-matnr = headdata-material.
            " Begin read it_mlan
            READ TABLE it_mlan INTO wa_mlan WITH KEY matnr = headdata-material. " Check if material extended or just tax updated
            IF sy-subrc = 0.
              wa_log-msg = 'Tax classification updated'.
            ELSE.
              IF headdata-matl_type EQ 'ZSFG'.  " Add parent FG for ref in log
                CONCATENATE return-message 'as per' material INTO wa_log-msg SEPARATED BY space.
              ELSE.
                wa_log-msg   = return-message.
              ENDIF.  " IF headdata-matl_type
            ENDIF.
            " End read it_mlan
            APPEND wa_log TO it_log.
            CLEAR: wa_log, wa_mlan.
          ENDIF.  " IF return-number = '356'
        ENDIF.
        " End read returnmessages
        CLEAR: wa_data, wa_mara, wa_mvke, material, subrc.
      ENDLOOP.
    ELSE.   " IF answer =
* If user cancels extension or answers with NO *
      MESSAGE 'Action cancelled by user. No material extended.' TYPE 'S'.
    ENDIF.  " IF answer =
  ELSE. " IF it_data[] IS NOT INITIAL
* No extendable materials found, display log and exit *
    MESSAGE 'No data found. Please check the log.' TYPE 'I' DISPLAY LIKE 'E'.
    PERFORM log_output.
  ENDIF.  " IF it_data[] IS NOT INITIAL
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOG_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM log_output .
* create errormessages header *
  IF errormessages[] IS NOT INITIAL.
    PERFORM err_head USING 'TYPE'.
    PERFORM err_head USING 'ID'.
    PERFORM err_head USING 'NUMBER'.
    PERFORM err_head USING 'MESSAGE'.
    PERFORM err_head USING 'LOG_NO'.
    PERFORM err_head USING 'LOG_MSG_NO'.
    PERFORM err_head USING 'MESSAGE_V1'.
    PERFORM err_head USING 'MESSAGE_V2'.
    PERFORM err_head USING 'MESSAGE_V3'.
    PERFORM err_head USING 'MESSAGE_V4'.
    PERFORM err_head USING 'PARAMETER'.
    PERFORM err_head USING 'ROW'.
    PERFORM err_head USING 'FIELD'.
    PERFORM err_head USING 'SYSTEM'.

* Download return file *
    PERFORM down_err.
  ENDIF.

  IF it_log[] IS NOT INITIAL.
* create log header *
    PERFORM log_head USING 'MATERIAL'.
    PERFORM log_head USING 'MESSAGE'.

* Download return file *
    PERFORM down_log.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  RET_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0424   text
*----------------------------------------------------------------------*
FORM err_head  USING    VALUE(p_name).
  CLEAR wa_errfname.
  wa_errfname-fieldname = p_name.
  APPEND wa_errfname TO it_errfname.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LOG_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0480   text
*----------------------------------------------------------------------*
FORM log_head  USING    VALUE(p_name).
  CLEAR wa_logfname.
  wa_logfname-fieldname = p_name.
  APPEND wa_logfname TO it_logfname.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWN_RET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_err .
  CLEAR: file, path, file_path, title, defname, defext, useraction.
  title = 'Save error log'.
  defname = 'material_extend_error_log.xls'.
  defext = '*.xls'.
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = title
      default_file_name = defname
      default_extension = defext
    CHANGING
      filename          = file
      path              = path
      fullpath          = file_path
      user_action       = useraction.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF useraction EQ '9'. " Cancelled
      MESSAGE 'No file specified. Error log download cancelled.' TYPE 'S'.
    ELSE.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename              = file_path
          write_field_separator = 'X'
          fieldnames            = it_errfname
        CHANGING
          data_tab              = errormessages.
    ENDIF.  " IF useraction EQ '9'.
  ENDIF.  " IF sy-subrc <> 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DOWN_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM down_log .
  CLEAR: file, path, file_path, title, defname, defext, useraction.
  title = 'Save status log'.
  defname = 'material_extend_log.xls'.
  defext = '*.xls'.
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = title
      default_file_name = defname
      default_extension = defext
    CHANGING
      filename          = file
      path              = path
      fullpath          = file_path
      user_action       = useraction.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF useraction EQ '9'. " Cancelled
      MESSAGE 'No file specified. Log download cancelled.' TYPE 'S'.
    ELSE.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename              = file_path
          write_field_separator = 'X'
          fieldnames            = it_logfname
        CHANGING
          data_tab              = it_log.
    ENDIF.  " IF useraction EQ '9'.
  ENDIF.  " IF sy-subrc <> 0.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SFG_SPECIFIC_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sfg_specific_processing .
* Get BOM data for SFG type materials *
  SELECT *
    FROM mara
    INTO TABLE it_sfg
    FOR ALL ENTRIES IN it_data
    WHERE matnr = it_data-matnr
  AND   mtart = 'ZSFG'.

  IF it_sfg[] IS NOT INITIAL.
    SELECT *
      FROM mast
      INTO TABLE it_mast
      FOR ALL ENTRIES IN it_sfg
      WHERE matnr = it_sfg-matnr
    AND   werks = p_werks.
  ENDIF.
ENDFORM.
