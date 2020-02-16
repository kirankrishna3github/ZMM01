*--------------------------------------------------------------------*
* Author: 6010859; SaurabhK
* Wednesday, January 15, 2020 17:31:11
* TR: IHDK904627: MM: S_K: ZMM098: MM01 BDC for SFG N2 plant: 15.1.20
*--------------------------------------------------------------------*
program zmm_bdc_mm01_nbw_n2_plant.

*--------------------------------------------------------------------*
* Global data
*--------------------------------------------------------------------*
tables: sscrfields.
*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
selection-screen begin of block fil with frame title text-fil.
parameters: p_file type rlgrap-filename.
selection-screen end of block fil.

selection-screen: function key 1.
*--------------------------------------------------------------------*
* local class definitions
*--------------------------------------------------------------------*
class lcl_app definition.
  public section.
    class-methods:
      select_file,
      set_func_btn,
      dwn_file_format.

    methods:
      read_file,
      process.

  protected section.
    " placeholder

  private section.
*--------------------------------------------------------------------*
* Declarations
*--------------------------------------------------------------------*
    types:
      mty_excelcell type bdcdata-fval,

      begin of mty_refernce,
        plant     type marc-werks,
        stor_loc  type mard-lgort,
        sales_org type mvke-vkorg,
        dist_chnl type mvke-vtweg,
      end of mty_refernce.

    data:
      begin of ms_excel,
        descr_mat  type mty_excelcell,  " material - extend / description - create
        plant      type mty_excelcell,
        stor_loc   type mty_excelcell,
        sales_org  type mty_excelcell,
        division   type mty_excelcell,
        dist_chnl  type mty_excelcell,
        base_unit  type mty_excelcell,
        mat_grp    type mty_excelcell,
        accassngrp type mty_excelcell,
        pur_grp    type mty_excelcell,
        hsn_code   type mty_excelcell,
      end of ms_excel,
      mt_excel like standard table of ms_excel with empty key,

      begin of ms_output,
        procstat type icon_d,
        material type mara-matnr.   " IHDK904655
        include structure ms_excel.
    data:
      message type bapi_msg,
      end of ms_output,
      mt_output like standard table of ms_output with empty key.

    methods:
      call_bdc
        importing
          is_data   like ms_excel
          iv_mode   type char1
        changing
          cs_output like ms_output,

      get_reference_data
        importing
          iv_material         type mara-matnr
        returning
          value(rs_reference) type mty_refernce,

      display_log.
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.
    " placeholder

  private section.
    " placeholder
endclass.
*--------------------------------------------------------------------*
* local class implementation
*--------------------------------------------------------------------*
class lcl_app implementation.
  method set_func_btn.
    data : functxt type smp_dyntxt.
    clear functxt.
    functxt-icon_id = '@49@'.
    functxt-quickinfo = 'Download File Format'.
    functxt-icon_text = 'Download File Format'.

    sscrfields-functxt_01 = functxt.  " 01, 02, 03, 04, 05
  endmethod.

  method dwn_file_format.
    call function 'CALL_BROWSER'
      exporting
        url                    = 'https://tinyurl.com/way9cmo'
        window_name            = 'ZMM098_File_Format'
        new_window             = abap_true
      exceptions
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        others                 = 6.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.
  endmethod.

  method select_file.
    call function 'F4_FILENAME'
      exporting
        field_name = 'P_FILE'
      importing
        file_name  = p_file.
  endmethod.

  method process.
    read_file( ).

    if mt_excel is not initial.
      loop at mt_excel into data(ls_excel).
        clear ms_output.
        ms_output = corresponding #( ls_excel ).

        if condense( ls_excel-descr_mat ) co '0123456789'.
          data(lv_material) = conv matnr( condense( ls_excel-descr_mat ) ).

          call function 'CONVERSION_EXIT_MATN1_INPUT'
            exporting
              input        = lv_material
            importing
              output       = lv_material
            exceptions
              length_error = 1
              others       = 2.
          if sy-subrc <> 0.
* Implement suitable error handling here
          endif.

          select single @abap_true
            from mara
            where matnr = @lv_material
            into @data(lv_extend).
          if lv_extend <> abap_true.
            ms_output-procstat = icon_red_light.
            ms_output-message  = ms_output-message && ` ` && 'Material not yet created. Cannot extend.'.
          else.
            select single @abap_true
              from marc
              where matnr = @lv_material
              and   werks = @ls_excel-plant
              into @data(lv_exists_plant).

            if lv_exists_plant = abap_true.
              ms_output-procstat = icon_red_light.
              ms_output-message  = ms_output-message && ` ` && 'Material already exists in plant'.
              clear lv_extend.
            endif.

*            select single @abap_true
*              from mvke
*              where matnr = @lv_material
*              and   vkorg = @ls_excel-sales_org
*              into @data(lv_exists_sales).
*
*            if lv_exists_sales = abap_true.
*              ms_output-procstat = icon_red_light.
*              ms_output-message  = ms_output-message && ` ` && 'Material already exists in sales area'.
*              clear lv_extend.
*            endif.
          endif.
        else.
          select single @abap_true
            from makt
            where maktx = @ls_excel-descr_mat
            and   spras = 'E'
            into @data(lv_exists).

          if lv_exists = abap_true.
            ms_output-procstat = icon_red_light.
            ms_output-message  = ms_output-message && ` ` && 'Material already created with the same description'.
          else.
            data(lv_create) = abap_true.
          endif.
        endif.

        if lv_create = abap_true or lv_extend = abap_true.
          call_bdc(
            exporting
              is_data = ls_excel
              iv_mode = cond #( when lv_create = abap_true then 'C'
                                when lv_extend = abap_true then 'E' )
            changing
              cs_output = ms_output ).
        endif.

        append ms_output to mt_output.

        clear:
         ls_excel,
         lv_material,
         lv_extend,
         lv_create,
         lv_exists,
         lv_exists_plant.
*         lv_exists_sales.
      endloop.
    else.
      message 'No data read' type 'S' display like 'E'.
    endif.

    display_log( ).
  endmethod.

  method read_file.
    if p_file is not initial.
      data: lt_raw type truxs_t_text_data.

      clear: mt_excel.

      call function 'TEXT_CONVERT_XLS_TO_SAP'
        exporting
          i_field_seperator    = abap_true
          i_line_header        = abap_false
          i_tab_raw_data       = lt_raw
          i_filename           = p_file
        tables
          i_tab_converted_data = mt_excel
        exceptions
          conversion_failed    = 1
          others               = 2.
      if sy-subrc <> 0.
        message 'Error reading file' type 'S' display like 'E'.
      endif.

      if mt_excel is not initial.
        zcl_helper=>check_file_format(
          changing
            ctab                = mt_excel " Internal Table with Excel Data
          exceptions
            file_format_altered = 1
            others              = 2 ).
        if sy-subrc <> 0.
          clear mt_excel.
        endif.
      endif.
    else.
      message 'No file selected' type 'S' display like 'E'.
    endif.
  endmethod.

  method call_bdc.
    data:
      lt_bdcdata type standard table of bdcdata with empty key,
      lt_message type standard table of bdcmsgcoll with empty key.

    constants:
      lc_create  type c length 1 value 'C',
      lc_extend  type c length 1 value 'E',
      lc_no_data type c length 1 value '/',
      lc_mm01    type sy-tcode value 'MM01'.
*--------------------------------------------------------------------*
* Macros
*--------------------------------------------------------------------*
    define bdc_dynpro.
      append value #( program  = condense( conv bdc_prog( &1 ) )
                      dynpro   = condense( conv bdc_dynr( &2 ) )
                      dynbegin = abap_true ) to lt_bdcdata.
    end-of-definition.

    define bdc_field.
      if condense( conv bdc_fval( &2 ) ) <> lc_no_data.
        append value #( fnam = condense( conv fnam_____4( &1 ) )
                        fval = condense( conv bdc_fval( &2 ) ) ) to lt_bdcdata.
      endif.
    end-of-definition.
*--------------------------------------------------------------------*

    clear:
      lt_bdcdata,
      lt_message.

    select single prctr
      from zmat01_pctr_mast
      where werks = @is_data-plant
      and   spart = @is_data-division
      into @data(lv_prof_ctr).

    if iv_mode = lc_create.
      bdc_dynpro 'SAPLMGMM' '0060'.
      bdc_field  'BDC_CURSOR' 'RMMG1-MATNR'.
      bdc_field  'BDC_OKCODE' '=AUSW'.
      bdc_field  'RMMG1-MBRSH' 'C'.
      bdc_field  'RMMG1-MTART' 'ZNBW'.
      bdc_field  'RMMG1_REF-MATNR' ''.

      bdc_dynpro 'SAPLMGMM' '0070'.
      bdc_field  'BDC_CURSOR' 'MSICHTAUSW-DYTXT(01)'.
      bdc_field  'BDC_OKCODE' '=SELA'.

      bdc_dynpro 'SAPLMGMM' '0070'.
      bdc_field  'BDC_CURSOR' 'MSICHTAUSW-DYTXT(01)'.
      bdc_field  'BDC_OKCODE' '=ENTR'.

      bdc_dynpro 'SAPLMGMM' '0080'.
      bdc_field  'BDC_CURSOR' 'RMMG1-VTWEG'.
      bdc_field  'BDC_OKCODE' '=ENTR'.
      bdc_field  'RMMG1-WERKS' is_data-plant.               " '1101'.
      bdc_field  'RMMG1-LGORT' is_data-stor_loc.            " '1501'.
      bdc_field  'RMMG1-VKORG' is_data-sales_org.           " '1000'.
      bdc_field  'RMMG1-VTWEG' is_data-dist_chnl.  " '10'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP02'.
      bdc_field  'MAKT-MAKTX' is_data-descr_mat.
      bdc_field  'MARA-MEINS' is_data-base_unit.   " 'EA'.
      bdc_field  'MARA-MATKL' is_data-mat_grp.              " '54003'.
      bdc_field  'MARA-SPART' is_data-division.    " '10'.
      bdc_field  'BDC_CURSOR' 'MARA-VOLEH'.
      bdc_field  'MARA-BRGEW' '1'.
      bdc_field  'MARA-GEWEI' 'KG'.
      bdc_field  'MARA-NTGEW' '1'.
      bdc_field  'MARA-VOLUM' '0'.
      bdc_field  'MARA-VOLEH' 'FT3'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP04'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP05'.

      bdc_dynpro 'SAPLMGMM' '4200'.
      bdc_field  'BDC_OKCODE' '=MAIN'.
      bdc_field  'BDC_CURSOR' 'MG03STEUER-TAXKM(09)'.
      bdc_field  'MG03STEUER-TAXKM(01)' '0'.
      bdc_field  'MG03STEUER-TAXKM(02)' '0'.
      bdc_field  'MG03STEUER-TAXKM(03)' '0'.
      bdc_field  'MG03STEUER-TAXKM(04)' '0'.
      bdc_field  'MG03STEUER-TAXKM(05)' '0'.
      bdc_field  'MG03STEUER-TAXKM(06)' '0'.
      bdc_field  'MG03STEUER-TAXKM(07)' '0'.
      bdc_field  'MG03STEUER-TAXKM(08)' '0'.
      bdc_field  'MG03STEUER-TAXKM(09)' '0'.
      if is_data-sales_org = '2000'.
        bdc_field  'MG03STEUER-TAXKM(10)' '0'.
      endif.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP05'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP06'.
      bdc_field  'BDC_CURSOR' 'MVKE-MTPOS'.
      bdc_field  'MARA-MTPOS_MARA' 'NORM'.
      bdc_field  'MVKE-MTPOS' 'NORM'.
      bdc_field  'MVKE-KTGRM' is_data-accassngrp.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP07'.
      bdc_field  'MARC-MTVFP' 'KP'.
      bdc_field  'MARA-TRAGR' '0001'.
      bdc_field  'MARC-LADGR' 'Z001'.
      bdc_field  'BDC_CURSOR' 'MARC-PRCTR'.
      bdc_field  'MARC-PRCTR' lv_prof_ctr.        " '0000100101'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP08'.
      bdc_field  'BDC_CURSOR' 'MARC-STEUC'.
      bdc_field  'MARC-STEUC' is_data-hsn_code.             " '3808'.

      bdc_dynpro 'SAPLMGMM' '4040'.
      bdc_field  'BDC_OKCODE' '=SP09'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP10'.
      bdc_field  'BDC_CURSOR' 'MARC-EKGRP'.
      bdc_field  'MARC-EKGRP' is_data-pur_grp.    " '304'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP11'.

      bdc_dynpro 'SAPLMGMM' '4040'.
      bdc_field  'BDC_OKCODE' '=SP12'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP13'.
      bdc_field  'BDC_CURSOR' 'MARC-DISMM'.
      bdc_field  'MARC-DISMM' 'ND'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP14'.
      bdc_field  'MARC-BESKZ' 'X'.
      bdc_field  'BDC_CURSOR' 'MARC-PLIFZ'.
      bdc_field  'MARC-PLIFZ' '5'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP15'.
      bdc_field  'MARC-PERKZ' 'M'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP16'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP19'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' 'SP20'.
      bdc_field  'BDC_CURSOR' 'MARA-MHDHB'.
      bdc_field  'MARA-MHDRZ' '30'.
      bdc_field  'MARA-MHDHB' '730'.
      bdc_field  'MARA-IPRKZ' 'D'.
      bdc_field  'MARA-SLED_BBD' 'B'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP24'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP31'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP32'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=BU'.
    endif.

    if iv_mode = lc_extend.
      bdc_dynpro 'SAPLMGMM' '0060'.
      bdc_field  'BDC_CURSOR' 'RMMG1-MATNR'.
      bdc_field  'BDC_OKCODE' '=AUSW'.
      bdc_field  'RMMG1-MATNR' is_data-descr_mat.
      bdc_field  'RMMG1-MBRSH' 'C'.
      bdc_field  'RMMG1-MTART' 'ZNBW'.
      bdc_field  'RMMG1_REF-MATNR' is_data-descr_mat.

      bdc_dynpro 'SAPLMGMM' '0070'.
      bdc_field  'BDC_CURSOR' 'MSICHTAUSW-DYTXT(01)'.
      bdc_field  'BDC_OKCODE' '=SELA'.

      bdc_dynpro 'SAPLMGMM' '0070'.
      bdc_field  'BDC_CURSOR' 'MSICHTAUSW-DYTXT(01)'.
      bdc_field  'BDC_OKCODE' '=SCHL'.

      data(lv_material) = conv matnr( condense( is_data-descr_mat ) ).

      call function 'CONVERSION_EXIT_MATN1_INPUT'
        exporting
          input        = lv_material
        importing
          output       = lv_material
        exceptions
          length_error = 1
          others       = 2.
      if sy-subrc <> 0.
*Implement suitable error handling here
      endif.

      data(ls_reference) = get_reference_data(
                             exporting
                               iv_material = lv_material ).

      bdc_dynpro 'SAPLMGMM' '0080'.
      bdc_field  'BDC_CURSOR' 'RMMG1_REF-VTWEG'.
      bdc_field  'BDC_OKCODE' '=ENTR'.
      bdc_field  'RMMG1-WERKS' is_data-plant.
      bdc_field  'RMMG1_REF-WERKS' ls_reference-plant.
      bdc_field  'RMMG1-LGORT' is_data-stor_loc.
      bdc_field  'RMMG1_REF-LGORT' ls_reference-stor_loc.
      bdc_field  'RMMG1-VKORG' is_data-sales_org.
      bdc_field  'RMMG1_REF-VKORG' ls_reference-sales_org.
      bdc_field  'RMMG1-VTWEG' is_data-dist_chnl.
      bdc_field  'RMMG1_REF-VTWEG' ls_reference-dist_chnl.

      if is_data-sales_org = '2000'.
        bdc_dynpro 'SAPLMGMM' '4000'.
        bdc_field  'BDC_OKCODE' '=PB08'.

        bdc_dynpro 'SAPLMGMM' '4200'.
        bdc_field  'BDC_OKCODE' '=MAIN'.
        bdc_field  'BDC_CURSOR' 'MG03STEUER-TAXKM(01)'.
        bdc_field  'MG03STEUER-TAXKM(01)' '0'.
      else.
        bdc_dynpro 'SAPLMGMM' '4000'.
        bdc_field  'BDC_OKCODE' '=PB08'.

        bdc_dynpro 'SAPLMGMM' '4200'.
        bdc_field  'BDC_OKCODE' '=MAIN'.
      endif.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP06'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP07'.
      bdc_field  'BDC_CURSOR' 'MARC-PRCTR'.
      bdc_field  'MARC-PRCTR' lv_prof_ctr.        " '0000100101'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP08'.

      bdc_dynpro 'SAPLMGMM' '4040'.
      bdc_field  'BDC_OKCODE' '=SP09'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP10'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP11'.

      bdc_dynpro 'SAPLMGMM' '4040'.
      bdc_field  'BDC_OKCODE' '=SP12'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP13'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP14'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP15'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP16'.

      bdc_dynpro 'SAPLMGMM' '4004'.
      bdc_field  'BDC_OKCODE' '=SP19'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' 'SP20'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=SP24'.

      bdc_dynpro 'SAPLMGMM' '4000'.
      bdc_field  'BDC_OKCODE' '=BU'.
    endif.

    data(ls_options) = value ctu_params( dismode  = 'N'
                                         updmode  = 'L'
                                         racommit = 'X' ).

    call transaction lc_mm01
      without authority-check
      using lt_bdcdata
      options from ls_options
      messages into lt_message.

    try.
        data(ls_message) = lt_message[  msgid = 'M3' msgnr = '800' ].
        cs_output-procstat = icon_green_light.
        cs_output-material = condense( ls_message-msgv1 ).  " IHDK904655
        cs_output-message  = |Material { condense( ls_message-msgv1 ) } { cond #( when iv_mode = lc_extend then 'extended' else 'created' ) }|.
      catch cx_sy_itab_line_not_found ##no_handler.
        try.
            ls_message = lt_message[  msgtyp = 'E' ].
            message id ls_message-msgid type ls_message-msgtyp number ls_message-msgnr
              with ls_message-msgv1 ls_message-msgv1 ls_message-msgv1 ls_message-msgv1
              into data(lv_dummy).
            cs_output-procstat = icon_red_light.
            cs_output-message  = lv_dummy.
          catch cx_sy_itab_line_not_found ##no_handler.
            try.
                ls_message = lt_message[ 1 ].
                message id ls_message-msgid type ls_message-msgtyp number ls_message-msgnr
                  with ls_message-msgv1 ls_message-msgv1 ls_message-msgv1 ls_message-msgv1
                  into lv_dummy.
                cs_output-procstat = icon_red_light.
                cs_output-message  = lv_dummy.
              catch cx_sy_itab_line_not_found ##no_handler.
            endtry.
        endtry.
    endtry.
  endmethod.

  method get_reference_data.
    clear rs_reference.
    if iv_material is not initial.
      select single werks
        from marc
        where matnr = @iv_material
        into @rs_reference-plant.

      if rs_reference-plant is not initial.
        select single lgort
          from mard
          where matnr = @iv_material
          and   werks = @rs_reference-plant
          into @rs_reference-stor_loc.
      endif.

      select single vkorg, vtweg
        from mvke
        where matnr = @iv_material
        into ( @rs_reference-sales_org, @rs_reference-dist_chnl ).
    endif.
  endmethod.

  method display_log.
    if mt_output is not initial.
      try.
          cl_salv_table=>factory(
            importing
              r_salv_table   = data(lo_alv)              " Basis Class Simple ALV Tables
            changing
              t_table        = mt_output ).

          if lo_alv is bound.
            data(lo_columns) = lo_alv->get_columns( ).
            if lo_columns is bound.
              data(lt_col) = lo_columns->get( ).

              if lt_col is not initial.
                loop at lt_col into data(ls_col).
                  translate ls_col-columnname using '_ '.
                  ls_col-r_column->set_long_text( exporting value = conv #( ls_col-columnname ) ).
                  ls_col-r_column->set_medium_text( exporting value = conv #( ls_col-columnname ) ).
                  ls_col-r_column->set_short_text( exporting value = conv #( ls_col-columnname ) ).
                  ls_col-r_column->set_output_length( exporting value = '15' ).
                  clear ls_col.
                endloop.
              endif.
            endif.

            data(lo_functions) = lo_alv->get_functions( ).

            if lo_functions is bound.
              lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
            endif.

            data(lo_layout) = lo_alv->get_layout( ).

            data(ls_key) = value salv_s_layout_key( report = sy-repid ).

            if lo_layout is bound.
              lo_layout->set_key( exporting value = ls_key ).

              lo_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

              lo_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
            endif.

            data(lo_display) = lo_alv->get_display_settings( ).

            if lo_display is bound.
              lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
            endif.

            lo_alv->display( ).
          endif.
        catch cx_salv_msg ##no_handler. " ALV: General Error Class with Message
      endtry.
    else.
      message 'No log generated' type 'S' display like 'E'.
    endif.
  endmethod.
endclass.

class lcl_main implementation.
  method start.
    try.
        new lcl_app( )->process( ).
      catch cx_root into data(lox_root).
        message lox_root->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
endclass.
*--------------------------------------------------------------------*
* pre-selection screen events
*--------------------------------------------------------------------*
load-of-program.
  " placeholder

initialization.
  lcl_app=>set_func_btn( ).
*--------------------------------------------------------------------*
* selection screen events
*--------------------------------------------------------------------*
at selection-screen on value-request for p_file.
  lcl_app=>select_file( ).

at selection-screen output.
  " placeholder

at selection-screen.
  if sscrfields-ucomm = 'FC01'.
    lcl_app=>dwn_file_format( ).
  endif.
*--------------------------------------------------------------------*
* start-of-selection
*--------------------------------------------------------------------*
start-of-selection.
  lcl_main=>start( ).
*--------------------------------------------------------------------*
* end-of-selection
*--------------------------------------------------------------------*
end-of-selection.
