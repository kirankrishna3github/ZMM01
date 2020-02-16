***INCLUDE BDCRECX1.
*  for programs doing a data transfer by creating a batch-input session
*  and
*  for programs doing a data transfer by CALL TRANSACTION USING
selection-screen begin of block b1 with frame title text-999.
selection-screen skip 1.
parameters : file like rlgrap-filename obligatory.
selection-screen end of block b1.
selection-screen begin of block b2 with frame title text-998.
selection-screen begin of line.
parameters session no-display . "RADIOBUTTON GROUP CTU.  "create session
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S07 FOR FIELD SESSION.
*  selection-screen position 45.

parameters ctu no-display . "RADIOBUTTON GROUP  CTU default 'X'.
"call transaction
selection-screen comment 1(20) text-s08 for field ctu.
selection-screen end of line.

selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S01 FOR FIELD GROUP.
*  selection-screen position 25.
parameters group(12) no-display.  "group name of session
selection-screen comment 1(20) text-s05 for field ctumode.
*  selection-screen position 70.
parameters ctumode like ctu_params-dismode default 'E'.
"A: show all dynpros
"E: show dynpro on error only
"N: do not display dynpro
selection-screen end of line.

selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S02 FOR FIELD USER.
*  selection-screen position 25.
parameters: user(12) default sy-uname no-display.     "user for
*session in batch
selection-screen comment 1(20) text-s06 for field cupdate.
*  selection-screen position 70.
parameters cupdate like ctu_params-updmode default 'S'.
"S: synchronously
"A: asynchronously
"L: local
selection-screen end of line.
selection-screen end of block b2.
selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S03 FOR FIELD KEEP.
*  selection-screen position 25.
parameters: keep type xfeld no-display."AS CHECKBOX.       "' ' =
*delete session if finished
"'X' = keep   session if finished
*  SELECTION-SCREEN COMMENT 48(20) TEXT-S09 FOR FIELD E_GROUP.
*  selection-screen position 70.
parameters e_group(12) no-display.             "group name of
*error-session
selection-screen end of line.

selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 3(20) TEXT-S04 FOR FIELD HOLDDATE.
*  selection-screen position 25.
parameters: holddate like sy-datum no-display.
*  SELECTION-SCREEN COMMENT 51(17) TEXT-S02 FOR FIELD E_USER.
*  selection-screen position 70.
parameters: e_user(12) default sy-uname no-display.    "user for
*error-session
selection-screen end of line.

selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 51(17) TEXT-S03 FOR FIELD E_KEEP.
*  selection-screen position 70.
parameters: e_keep type xfeld no-display." AS CHECKBOX.     "' ' =
*delete session if finished
"'X' = keep   session if finished
selection-screen end of line.

selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 51(17) TEXT-S04 FOR FIELD E_HDATE.
*  selection-screen position 70.
parameters: e_hdate like sy-datum no-display.
selection-screen end of line.

selection-screen skip.

selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 1(33) TEXT-S10 FOR FIELD NODATA.
parameters: nodata no-display. " DEFAULT '/' LOWER CASE.
"nodata
selection-screen end of line.

selection-screen begin of line.
*  SELECTION-SCREEN COMMENT 1(33) TEXT-S11 FOR FIELD SMALLLOG.
parameters: smalllog type xfeld no-display." as checkbox.  "' ' = log
*all transactions
"'X' = no transaction logging
selection-screen end of line.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
data:   bdcdata like bdcdata    occurs 0 with header line.
*       messages of call transaction
data:   messtab like bdcmsgcoll occurs 0 with header line.
*       error session opened (' ' or 'X')
data:   e_group_opened.
*       message texts
tables: t100.


*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
at selection-screen.
* group and user must be filled for create session
  if session = 'X'
     and group = space or user = space.
    message e613(ms).
  endif.

*----------------------------------------------------------------------*
*   open dataset                                                       *
*----------------------------------------------------------------------*
form open_dataset using p_dataset.
  open dataset p_dataset
               for input in text mode
               encoding default.
  if sy-subrc <> 0.
    write: / text-e00, sy-subrc.
    stop.
  endif.
endform.

*----------------------------------------------------------------------*
*   close dataset                                                      *
*----------------------------------------------------------------------*
form close_dataset using p_dataset.
  close dataset p_dataset.
endform.

*----------------------------------------------------------------------*
*   create batchinput session                                          *
*   (not for call transaction using...)                                *
*----------------------------------------------------------------------*
form open_group.
  if session = 'X'.
    skip.
    write: /(20) 'Create group'(I01), group.
    skip.
*   open batchinput group
    call function 'BDC_OPEN_GROUP'
      exporting
        client   = sy-mandt
        group    = group
        user     = user
        keep     = keep
        holddate = holddate.
    write: /(30) 'BDC_OPEN_GROUP'(I02),
            (12) 'returncode:'(I05),
                 sy-subrc.
  endif.
endform.

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
form close_group.
  if session = 'X'.
*   close batchinput group
    call function 'BDC_CLOSE_GROUP'.
    write: /(30) 'BDC_CLOSE_GROUP'(I04),
            (12) 'returncode:'(I05),
                 sy-subrc.
  else.
    if e_group_opened = 'X'.
      call function 'BDC_CLOSE_GROUP'.
      write: /.
      write: /(30) 'Fehlermappe wurde erzeugt'(I06).
      e_group_opened = ' '.
    endif.
  endif.
endform.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
form bdc_transaction using tcode.
  data: l_mstring(480).
  data: l_subrc like sy-subrc.
* batch input session
  if session = 'X'.
    call function 'BDC_INSERT'
      exporting
        tcode     = tcode
      tables
        dynprotab = bdcdata.
    if smalllog <> 'X'.
      write: / 'BDC_INSERT'(I03),
               tcode,
               'returncode:'(I05),
               sy-subrc,
               'RECORD:',
               sy-index.
    endif.
* call transaction using
  else.
    refresh messtab.
    call transaction tcode using bdcdata
                     mode   ctumode
                     update cupdate
                     messages into messtab.
    l_subrc = sy-subrc.
    if smalllog <> 'X'.
      write: / 'CALL_TRANSACTION',
               tcode,
               'returncode:'(I05),
               l_subrc,
               'RECORD:',
               sy-index.
      loop at messtab.
        message id     messtab-msgid
                type   messtab-msgtyp
                number messtab-msgnr
                into l_mstring
                with messtab-msgv1
                     messtab-msgv2
                     messtab-msgv3
                     messtab-msgv4.
        write: / messtab-msgtyp, l_mstring(250).
      endloop.
      "-----------------------------------------------------------"
      data : lv_partner type bapibus1006_head-bpartner.
      data : lt_return     type table of bapiret2 with header line.
      data : lt_telef  type table of  bapiadtel,
             lt_telefx type table of  bapiadtelx,
             wa_telefx type	bapiadtel,
             wa_telef  type  bapiadtel.
      data: ls_j_1ipanno  type lfa1-j_1ipanno.
      data : lv_country    type char4,
             lv_mob_number type char30.

      data : lv_gui type cvi_vend_link-partner_guid.


      if sy-repid = 'ZMM_VENDOR_MASTER_CREATE'.
        read table messtab with key msgtyp = 'S'
                                    msgid = 'F2'
                                    msgnr = '175'.
        if sy-subrc = 0.


          import ls_j_1ipanno to ls_j_1ipanno
                 lv_country to lv_country
                 lv_mob_number to lv_mob_number  from memory id 'PAN'.

          wa_telef-country   = lv_country.
          wa_telef-telephone = lv_mob_number.
          wa_telef-r_3_user = '3'.
          append wa_telef to lt_telef.
          clear : wa_telef.

          wa_telefx-country   = 'X'.
          wa_telefx-telephone = 'I'.
          wa_telefx-r_3_user = 'X'.
          append wa_telefx to lt_telefx.
          clear : wa_telefx.

          select single partner_guid from cvi_vend_link into lv_gui
            where vendor = messtab-msgv1+0(10).
          select single partner from but000 into lv_partner
            where partner_guid = lv_gui.

          refresh lt_return.
          call function 'BAPI_BUPA_CENTRAL_CHANGE'
            exporting
              businesspartner        = lv_partner
              valid_date             = sy-datum
            tables
              telefondatanonaddress  = lt_telef
              telefondatanonaddressx = lt_telefx
              return                 = lt_return.

          if lt_return is initial.
            call function 'BAPI_TRANSACTION_COMMIT'
              exporting
                wait = 'X'.
          endif.



          update lfa1 set j_1ipanno = ls_j_1ipanno
                          where lifnr = messtab-msgv1.

       WRITE : 'Partner', lv_partner.
       CLEAR : lv_partner,lv_gui,lv_country,lv_mob_number,ls_j_1ipanno.
       FREE MEMORY ID 'PAN'.
        endif.
      endif.
      "-----------------------------------------------------------"
      skip.
    endif.
** Erzeugen fehlermappe ************************************************
    if l_subrc <> 0 and e_group <> space.
      if e_group_opened = ' '.
        call function 'BDC_OPEN_GROUP'
          exporting
            client   = sy-mandt
            group    = e_group
            user     = e_user
            keep     = e_keep
            holddate = e_hdate.
        e_group_opened = 'X'.
      endif.
      call function 'BDC_INSERT'
        exporting
          tcode     = tcode
        tables
          dynprotab = bdcdata.
    endif.
  endif.
  refresh bdcdata.
endform.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
endform.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
*  IF fval <> nodata.
  clear bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  append bdcdata.
*  ENDIF.
endform.
