***INCLUDE ZZBDCRECX1.
*----------------------------------------------------------------------*
*   Developer: SaurabhK (Adroit Infotech)
*   Date: Tuesday, August 08, 2017 11:18:59
*   Description: for programs doing a data transfer by CALL TRANSACTION USING
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   selection screen
*----------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-b01.
parameters: p_file  type ibipparms-path.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-b02.
selection-screen begin of line.
parameters foreg radiobutton group mod default 'X'.  " Foreground Mode
selection-screen comment (15)  text-s01 for field foreg.

parameters backg radiobutton group mod.  " Background Mode
selection-screen comment (15) text-s02 for field backg.

parameters error radiobutton group mod.  " Errors only
selection-screen comment (15) text-s03 for field error.
selection-screen end of line.
selection-screen end of block b2.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
data:   bdcdata like bdcdata    occurs 0 with header line.
*       messages of call transaction
data:   messtab like bdcmsgcoll occurs 0 with header line.
*       Mode for call transaction
data:   ctumode like ctu_params-dismode.
"A: show all dynpros
"E: show dynpro on error only
"N: do not display dynpro
*       Update Mode
data:   cupdate like ctu_params-updmode value 'L'.
"S: synchronously
"A: asynchronously
"L: local
data:   nodata(1) type c value '/'.
data: wa     type ty_file,
      it     type standard table of ty_file,
      it_raw type truxs_t_text_data,
      v_file type rlgrap-filename.

*----------------------------------------------------------------------*
*        Start new transaction according to parameters                 *
*----------------------------------------------------------------------*
form bdc_transaction using tcode.
* call transaction using
  refresh messtab.
  call transaction tcode using bdcdata
                   mode   ctumode
                   update cupdate
                   messages into messtab.
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
  if fval <> nodata.
    clear bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    append bdcdata.
  endif.
endform.
