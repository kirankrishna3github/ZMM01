class ZCL_ME_CHANGE_OUTTAB_CUS_ME5A definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_CHANGE_OUTTAB_CUS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ME_CHANGE_OUTTAB_CUS_ME5A IMPLEMENTATION.


  METHOD if_ex_me_change_outtab_cus~fill_outtab.

    IF sy-tcode EQ 'ME5A' AND im_struct_name EQ 'MEREP_OUTTAB_EBAN'.

      DATA: s_areldt TYPE RANGE OF merep_outtab_eban-zzrelease_date.
      SELECT * FROM cdpos INTO TABLE @DATA(it_cdpos)
                          WHERE objectclas EQ 'BANF'
                          AND   tabname    EQ 'EBAN'
                          AND   fname      EQ 'BANPR'
                          AND   value_new  NE '02'
                          ORDER BY objectid.
      IF sy-subrc EQ 0.
        SELECT * FROM cdhdr INTO TABLE @DATA(it_cdhdr)
                            FOR ALL ENTRIES IN @it_cdpos
                            WHERE objectclas EQ 'BANF'
                            AND   objectid EQ @it_cdpos-objectid
                            AND   changenr EQ @it_cdpos-changenr.
      ENDIF.
*******************************************************************
*Import the Actual Release date field from ME5A selection screen
* Imported from RM06BA00 report in fm06bcs1 include
* if the selection screen actual release date is not the range of release date in selection screen
* the record is deleted from the final output table.
      IMPORT s_areldt TO s_areldt FROM MEMORY ID 'ZACTREL'.
      FREE MEMORY ID 'ZACTREL'.
      LOOP AT ch_outtab ASSIGNING FIELD-SYMBOL(<wa_out>).
        ASSIGN COMPONENT 'BANFN' OF STRUCTURE <wa_out> TO FIELD-SYMBOL(<banfn>).
        CHECK sy-subrc EQ 0.
        ASSIGN COMPONENT 'ZZRELEASE_DATE' OF STRUCTURE <wa_out> TO FIELD-SYMBOL(<zzrelease_date>).
        CHECK sy-subrc EQ 0.
        TRY.
            DATA(lv_changenr) = it_cdpos[ objectid = <banfn> ]-changenr.
            <zzrelease_date>  = it_cdhdr[ objectid = <banfn>
                                          changenr = lv_changenr ]-udate.
            IF <zzrelease_date> IN s_areldt.
            ELSE.
              DELETE TABLE ch_outtab FROM <wa_out>.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
*            DELETE TABLE ch_outtab FROM <wa_out>.
        ENDTRY.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
