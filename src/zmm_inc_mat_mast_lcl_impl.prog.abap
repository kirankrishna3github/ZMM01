*&---------------------------------------------------------------------*
*&  Include           ZMM_INC_MAT_MAST_LCL_IMPL
*&---------------------------------------------------------------------*
* ---- begin class implementations ---- *
* ---- begin base class definition ---- *
class lcl_mm implementation.
  method constructor.
    " Do something
    " Could call the empty invoke( ) if need be by writing some meaningful initilisation code there
    try.
        invoke( ).  " empty invoke of base called for 'call pattern consistency'
      catch lcx_generic_error
        cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.

  method set_func_buttons.
    data: functxt type smp_dyntxt.
    clear functxt.
    functxt-icon_id = icon_composite_activitygroup.
    functxt-quickinfo = 'Maintain Classification Characteristics'.
    functxt-icon_text = 'Maintain Classification Characteristics'.

    sscrfields-functxt_01 = functxt.  " 01, 02, 03, 04, 05
  endmethod.

  method set_frame_title.
    clear: dwnld, upld.
    move: text-003 to dwnld,
          text-002 to upld.

    refresh tpool.
    clear objectname.

    objectname = sy-cprog.
    call function 'RS_TEXTPOOL_READ'
      exporting
        objectname           = objectname
        action               = space
      tables
        tpool                = tpool
      exceptions
        object_not_found     = 1
        permission_failure   = 2
        invalid_program_type = 3
        error_occured        = 4
        action_cancelled     = 5
        others               = 6.
    if sy-subrc <> 0.
* Implement suitable error handling here
    endif.

    case abap_true.
      when p_rad1.
        read table tpool into data(wpool) with key key = 'R01'.
      when p_rad2.
        read table tpool into wpool with key key = 'R02'.
      when p_rad3.
        read table tpool into wpool with key key = 'R03'.
      when p_rad4.
        read table tpool into wpool with key key = 'R04'.
      when p_rad5.
        read table tpool into wpool with key key = 'R05'.
      when p_rad6.
        read table tpool into wpool with key key = 'R06'.
      when p_rad7.
        read table tpool into wpool with key key = 'R07'.
      when p_rad8.
        read table tpool into wpool with key key = 'R08'.
      when p_rad9.
        read table tpool into wpool with key key = 'R09'.
      when p_rad10.
        read table tpool into wpool with key key = 'R10'.
      when p_rad11.
        read table tpool into wpool with key key = 'R11'.
      when p_rad12.
        read table tpool into wpool with key key = 'R12'.
      when p_rad13.
        read table tpool into wpool with key key = 'R13'.
      when p_rad14.
        read table tpool into wpool with key key = 'R14'.
      when others.
    endcase.
    condense wpool-entry.
    concatenate: dwnld 'for' wpool-entry into dwnld separated by space,
                 upld  'for' wpool-entry into upld  separated by space.
    condense: dwnld, upld.
  endmethod.

  " IHDK904062
  method modify_screen.
    select single parva from usr05 into @data(lv_no_create) where bname eq @sy-uname and parid eq 'ZMAT01_NO_CREATE'.
    if lv_no_create eq 'X'.
      loop at screen.
        if screen-group1 eq 'CRT'.
          screen-active = 0.
          modify screen.
        endif.
      endloop.
    endif.

    select single parva from usr05 into @data(lv_no_extend) where bname eq @sy-uname and parid eq 'ZMAT01_NO_EXTEND'.
    if lv_no_extend eq 'X'.
      loop at screen.
        if screen-group1 eq 'EXT'.
          screen-active = 0.
          modify screen.
        endif.
      endloop.
    endif.

    if lv_no_create eq 'X' and lv_no_extend eq 'X'.
      raise exception type lcx_generic_error message id '00' type 'E' number '001' with 'No authority to create, extend.'.
    endif.
  endmethod.

  method file_open.
    clear: title, file_filter, rc, useraction.
    refresh: it_filein.

    move 'Select material upload file' to title.
    move '(*.xls,*.xlsx)|*.xls*' to file_filter.          " IRDK930435
    call method cl_gui_frontend_services=>file_open_dialog
      exporting
        window_title            = title
        file_filter             = file_filter
        multiselection          = space
      changing
        file_table              = it_filein
        rc                      = rc
        user_action             = useraction
      exceptions
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        others                  = 5.
    if sy-subrc <> 0.
      disp_message(
      exporting
        type  = 'E'
        id    = 'ZMM'
        no    = '002' ).
      message id sy-msgid
      type sy-msgty
      number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      raise exception type lcx_generic_error.
    endif.

    if useraction eq '9'.
      disp_message(
      exporting
        type  = 'S'
        id    = 'ZMM'
        no    = '001' ).
      raise exception type lcx_generic_error.
    else.
      clear: wa_filein, p_file.
      check it_filein is not initial and rc eq 1.
      read table it_filein into wa_filein index 1.
      if sy-subrc = 0.
        move wa_filein-filename to p_file.
      endif.
    endif.

  endmethod.

  " IHDK901148
  method handle_sel_screen_ucomm.
    if sscrfields-ucomm eq 'DWN'.
      " Write a class to handle file format download
      " Rev 02
      call function 'CALL_BROWSER'
        exporting
          url                    = 'https://goo.gl/Tk5nWu'
          window_name            = 'Download ZMAT01 File Formats'
          new_window             = 'X'
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
    endif.
    " End Rev 02

    if sscrfields-ucomm eq 'FC01'.
      call transaction 'ZMM094'.
    endif.
  endmethod.

  method invoke.
    " actual implementation here in subclasses
    if gt_pctr_mast is initial. " IHDK904473
      " IHDK900305
      refresh gt_pctr_mast. " static variable available globally
      select *
        from zmat01_pctr_mast
        into table gt_pctr_mast.
    endif.
  endmethod.

  method file_to_tab.
    data: lo_structdescr type ref to cl_abap_structdescr,
          components     type abap_component_tab,
          component      type abap_componentdescr.
    refresh it_raw.
    clear v_file.

    unassign: <fs_tab>.
    assign it_file to <fs_tab>.

    if p_file is not initial.
      move p_file to v_file.
      call function 'TEXT_CONVERT_XLS_TO_SAP'
        exporting
          i_field_seperator    = 'X'
*         i_line_header        = 'X'
          i_tab_raw_data       = it_raw
          i_filename           = v_file
        tables
          i_tab_converted_data = <fs_tab>
        exceptions
          conversion_failed    = 1
          others               = 2.
      if sy-subrc <> 0.
        " show system message
        message id sy-msgid
        type 'S'  "sy-msgty
        number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        display like 'E'.

        " show generic message in popup
        disp_message(
        exporting
          type  = 'E'
          id    = 'ZMM'
          no    = '008' ).

        raise exception type lcx_generic_error.
      endif.
    else.
      disp_message(
      exporting
        type  = 'E'
        id    = 'ZMM'
        no    = '004' ).
      raise exception type lcx_generic_error.
    endif.

    if <fs_tab> is assigned.
      clear: lo_structdescr, component.
      refresh: components.
      unassign <fs_wa>.
      read table <fs_tab> assigning <fs_wa> index 1.  " Header row
      if sy-subrc = 0 and <fs_wa> is assigned.
        lo_structdescr ?= cl_abap_structdescr=>describe_by_data( p_data = <fs_wa> ).
        components = lo_structdescr->get_components( ).

        " IHDK902348: MM: S_K: ZMAT01: Fix file format chk zif_mm_mat01: 9.7.19
        " structures for file and data are now include structure rather than flat
        try.
            free lo_structdescr.
            if line_exists( components[ as_include = abap_true ] ).
              lo_structdescr ?= cast cl_abap_structdescr( components[ 1 ]-type ).
              refresh components.
              components = lo_structdescr->get_components( ).
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.

        if components is not initial.
          move 0 to sy-subrc.
          do.
            unassign <fs>.
            assign component sy-index of structure <fs_wa> to <fs>.
            if sy-subrc <> 0.
              exit.
            endif.
            if <fs> is assigned.
              translate <fs> to upper case.
              clear component.
              read table components into component index sy-index.
              if sy-subrc <> 0 or ( component-name ne <fs> ).
                disp_message(
                exporting
                  type  = 'E'
                  id    = 'ZMM'
                  no    = '010' ).
                raise exception type lcx_generic_error.
              endif.
              translate <fs> to lower case.
            endif.
          enddo.
        endif.
      endif.
    endif.
    unassign: <fs_tab>.
  endmethod.

  method check_if_tab_empty.

    clear: lv_tabix.
    unassign: <fs_tab>, <fs_wa>, <fs_head>, <fs>.

    check it_file is not initial.
    assign: it_file to <fs_tab>.

    if <fs_tab> is assigned.
      loop at <fs_tab> assigning <fs_wa>.
        add 1 to lv_tabix.
        if lv_tabix eq 1 or lv_tabix eq 2.
          case lv_tabix.
            when 1.
              assign log_header1 to <fs_head>.
              if <fs_head> is assigned.
                move-corresponding <fs_wa> to <fs_head>.
                sy-subrc = 0.
                do.
                  assign component sy-index of structure <fs_head> to <fs>.
                  if sy-subrc <> 0.
                    exit.
                  endif.
                  if <fs> is assigned.
                    shift <fs> left deleting leading space.
                    move <fs> to wa_head-fieldname.
                    append wa_head to it_head.
                  endif.
                  unassign: <fs>.
                  clear: wa_head.
                enddo.
              endif.
            when 2.
              assign log_header2 to <fs_head>.
              if <fs_head> is assigned.
                move-corresponding <fs_wa> to <fs_head>.
                assign component 'INDEX' of structure <fs_head> to <fs>.
                if <fs> is assigned.
                  move 'Index/Status' to <fs>.
                endif.
                unassign <fs>.
                assign component 'LOG' of structure <fs_head> to <fs>.
                if <fs> is assigned.
                  move 'Message Log' to <fs>.
                endif.
                unassign <fs>.
              endif.
            when others.
          endcase.
          delete <fs_tab>.
        else.
          exit.
        endif.
        unassign: <fs_head>, <fs>.
      endloop.
      unassign: <fs_tab>, <fs_wa>, <fs>.

      if it_file is initial.
        disp_message(
        exporting
          type  = 'E'
          id    = 'ZMM'
          no    = '005' ).
        raise exception type lcx_generic_error.
      endif.
    else.
      disp_message(
      exporting
        type  = 'E'
        id    = 'ZMM'
        no    = '005' ).
      raise exception type lcx_generic_error.
    endif.
  endmethod.

  method file_format_adjust.

    unassign: <fs_tab>, <fs_wa>, <fs>.
    check it_file is not initial.
    assign it_file to <fs_tab>.
    loop at <fs_tab> assigning <fs_wa>.
      sy-subrc = 0.
      do.
        assign component sy-index of structure <fs_wa> to <fs>.
        if sy-subrc <> 0.
          exit.
        endif.
        if <fs> is assigned.  " All components of ty_file are character type
          shift <fs> left deleting leading space.
          condense <fs>.  " IRDK931038
          translate <fs> to upper case.
        endif.
        unassign: <fs>.
      enddo.
    endloop.
    unassign: <fs_tab>, <fs_wa>, <fs>.
  endmethod.

  method convert_data_to_bapi_format.
    " Local field symbols for data table
    field-symbols: <fs_data_tab> type standard table,
                   <fs_data_wa>  type any,
                   <fs_data>     type any.

    clear: lo_descr, lv_relative_name, lv_data_element, lv_function, wa_dd04l, lv_index.

    unassign: <fs_tab>, <fs_wa>, <fs>,
    <fs_data_tab>, <fs_wa>, <fs>.

    check it_file is not initial.
    assign it_file to <fs_tab>.
    assign it_data to <fs_data_tab>.
    if <fs_tab> is assigned.
      loop at <fs_tab> assigning <fs_wa>.
        append initial line to <fs_data_tab> assigning <fs_data_wa>.
        try.
            move-corresponding <fs_wa> to <fs_data_wa>.             " convert character(file) to respective type(data)
          catch cx_sy_conversion_no_number into data(lo_cx_con).
            disp_message(
            type = 'E'
            id   = 'ZMM'
            no   = '007' ).
            message lo_cx_con->get_text( ) type 'S' display like 'E'.
            raise exception type cx_sy_conversion_no_number.
          catch cx_sy_conversion_overflow into data(lo_cx_ovflw).       " IHDK900679
            message lo_cx_ovflw->get_text( ) type 'S' display like 'E'.
            raise exception type cx_sy_conversion_overflow.
          catch cx_root into data(lo_cx_root).                          " IHDK900679
            message lo_cx_root->get_text( ) type 'S' display like 'E'.
            raise exception type cx_sy_no_handler.
        endtry.
        add 1 to lv_index.
        assign component 'INDEX' of structure <fs_data_wa> to <fs_data>.
        if <fs_data> is assigned.
          move lv_index to <fs_data>.
        endif.
        unassign <fs_data>.
        assign component 'SEL' of structure <fs_data_wa> to <fs_data>.
        if <fs_data> is assigned.
          move abap_true to <fs_data>.
        endif.
        unassign: <fs_data>.
        move 0 to sy-subrc.
        do.
          assign component sy-index of structure <fs_data_wa> to <fs_data>.
          if sy-subrc <> 0.
            exit.
          endif.
          if <fs_data> is assigned and <fs_data> is not initial.
            lo_descr = cl_abap_typedescr=>describe_by_data( p_data = <fs_data> ).

            if lo_descr is bound.
              lv_relative_name = lo_descr->get_relative_name( ).

              if lv_relative_name is not initial.
                move lv_relative_name to lv_data_element.

                select single *
                from dd04l
                into wa_dd04l
                where rollname = lv_data_element.

                if wa_dd04l-convexit is not initial.
                  concatenate 'CONVERSION_EXIT_' wa_dd04l-convexit '_INPUT' into lv_function.

                  call function 'FUNCTION_EXISTS'
                    exporting
                      funcname           = lv_function
                    exceptions
                      function_not_exist = 1
                      others             = 2.
                  if sy-subrc = 0.
                    try.
                        call function lv_function
                          exporting
                            input  = <fs_data>
                          importing
                            output = <fs_data>
                          exceptions
                            others = 1. " any exception; IHDK900502
                      catch cx_sy_dyn_call_illegal_func.
                        " catch-block
                    endtry.
                  endif.
                endif.
              endif.
            endif.
          endif.
          unassign: <fs_data>.
          clear: lo_descr, lv_relative_name, lv_data_element, wa_dd04l, lv_function.
        enddo.
      endloop.
    endif.
    unassign: <fs_tab>, <fs_wa>, <fs>,
    <fs_data_tab>, <fs_wa>, <fs>.
  endmethod.

  method material_validation.
    " actual implementation here in subclasses
    " IHDK900894
    " common validation functions
    data: l_str type string.
    field-symbols: <fs_log_tab> type standard table,
                   <fs_log_wa>  type any,
                   <fs_log>     type any.

    unassign: <fs_tab>, <fs_wa>, <fs>,
              <fs_log_tab>, <fs_log_wa>, <fs_log>.

    check it_data is not initial.
    assign it_data to <fs_tab>.
    assign it_log  to <fs_log_tab>.
    " authority check - IHDK900305
    check <fs_tab> is not initial.
    " IHDK900896--->
    clear l_str.
    l_str = 'MATNR EQ @<FS_TAB>-MATNR'. " IHDK900903
    select matnr, mtart
      from mara
      into table @data(lt_material)
      for all entries in @<fs_tab>
      where (l_str).
    loop at <fs_tab> assigning <fs_wa>.
      " material type
      unassign <fs>.
      assign component 'MTART' of structure <fs_wa> to <fs>.
      if <fs> is assigned.
      else. " in case of extension, material type is not part of the file format
        assign component 'MATNR' of structure <fs_wa> to <fs>.
        if <fs> is assigned.
          try.
              assign lt_material[ matnr = <fs> ]-mtart to <fs>.
            catch cx_sy_itab_line_not_found.            "#EC NO_HANDLER
          endtry.
        endif.
      endif.
      " ---->End IHDK900896
      if <fs> is not initial. " at this point <fs> contains material type; either from file(creation) or from mara(extension)
        assign <fs> to field-symbol(<fv_mtart>).
        authority-check object 'M_MATE_MAR'
          id 'ACTVT' dummy
          id 'BEGRU' field <fs>.

        if sy-subrc <> 0.
          data(lv_auth_check_failed) = abap_true.
          unassign <fs_log_wa>.
          append initial line to <fs_log_tab> assigning <fs_log_wa>.
          move-corresponding <fs_wa> to <fs_log_wa>.
          if <fs_log_wa> is assigned.
            unassign <fs_log>.
            assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
            if <fs_log> is assigned.
              <fs_log> = <fs> && 'No authorisation for material type' && ` ` && <fs>.
            endif.
          endif.
        endif.
      endif.

      if lv_auth_check_failed eq abap_false.
        " plant
        unassign <fs>.
        assign component 'WERKS' of structure <fs_wa> to <fs>.
        if <fs> is assigned.
          assign <fs> to field-symbol(<fv_werks>).
          if <fs> is not initial.
            authority-check object 'M_MATE_WRK'
              id 'ACTVT' dummy
              id 'WERKS' field <fs>.

            if sy-subrc <> 0.
              lv_auth_check_failed = abap_true.
              unassign <fs_log_wa>.
              append initial line to <fs_log_tab> assigning <fs_log_wa>.
              move-corresponding <fs_wa> to <fs_log_wa>.
              if <fs_log_wa> is assigned.
                unassign <fs_log>.
                assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
                if <fs_log> is assigned.
                  <fs_log> = <fs> && 'No authorisation for plant' && ` ` && <fs>.
                endif.
              endif.
            endif.
          endif.
        endif.
      endif.

*****added by varun on 16.10.19 as said by punam on mail
      if lv_auth_check_failed eq abap_false.
        if <fv_werks> is assigned and <fv_mtart> is assigned.
          if <fv_werks> is not initial and <fv_mtart> is not initial .
            authority-check object 'ZMAT01_CHK'
            id 'ACTVT' dummy
            id 'WERKS' field <fv_werks>
            id 'BEGRU' field <fv_mtart>.
            if sy-subrc ne 0.
              lv_auth_check_failed = abap_true.
              unassign <fs_log_wa>.
              append initial line to <fs_log_tab> assigning <fs_log_wa>.
              move-corresponding <fs_wa> to <fs_log_wa>.
              if <fs_log_wa> is assigned.
                unassign <fs_log>.
                assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
                if <fs_log> is assigned.
                  <fs_log> = 'No authorisation for Plant or Material Type' && ` ` && <fv_werks> && ',' && <fv_mtart>.
                endif.
              endif.
            endif.
          endif.
        endif.
      endif.
****End of addition by varun

      if lv_auth_check_failed eq abap_true.
        unassign <fs>.
        assign component 'SEL' of structure <fs_wa> to <fs>.
        if <fs> is assigned.
          clear <fs>.  " Unselect such materials
        endif.
      endif.
      clear: lv_auth_check_failed.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials

    " plant existence check
    check <fs_tab> is not initial.
    select werks
      from t001w
      into table @data(lt_plant).

    loop at <fs_tab> assigning <fs_wa>.
      unassign <fs>.
      assign component 'WERKS' of structure <fs_wa> to <fs>.
      if <fs> is assigned.
        if <fs> is not initial.
          try.
              data(lv_not_a_plant) = cond abap_bool(
                                      when line_exists( lt_plant[ werks = <fs> ] )
                                        then abap_false
                                      else abap_true ).

              " IHDK906991
              if <fs> = '1000'.
                lv_not_a_plant = abap_true.
              endif.
            catch cx_sy_itab_line_not_found.            "#EC NO_HANDLER
          endtry.
        else.
          data(lv_plant_empty) = abap_true.
        endif.
      endif.

      if lv_not_a_plant eq abap_true or lv_plant_empty eq abap_true.
        unassign <fs_log_wa>.
        append initial line to <fs_log_tab> assigning <fs_log_wa>.
        if <fs_log_wa> is assigned.
          move-corresponding <fs_wa> to <fs_log_wa>.
          unassign <fs_log>.
          assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
        endif.
        case abap_true.
          when lv_not_a_plant.
            if <fs_log> is assigned.
              <fs_log> = |{ <fs> } plant does not exist/invalid plant|.
            endif.
          when lv_plant_empty.
            if <fs_log> is assigned.
              <fs_log> = |{ <fs> } plant is a mandatory input|.
            endif.
          when others.
        endcase.
        assign component 'SEL' of structure <fs_wa> to <fs>.
        if <fs> is assigned.
          clear <fs>. " Unselect such materials
        endif.
      endif.

      clear: lv_not_a_plant, lv_plant_empty.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials
    " End IHDK900894
  endmethod.

  method confirm_action.
    clear: msg, lines_file, lines_data, answer.
    describe table it_file lines lines_file.
    describe table it_data lines lines_data.

    condense: lines_file, lines_data.

    if lines_data gt '0'.
      concatenate lines_file 'materials data supplied for creation/extension.'
      lines_data 'materials data can be processed/created/extended.'
      'Do you want to proceed?' into msg separated by space.

* Confirm creation action *
      clear answer.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar       = 'Material Create/Extend Confirmation'
          text_question  = msg
          text_button_1  = 'Yes'
          text_button_2  = 'No'
*         START_COLUMN   = 25
*         START_ROW      = 6
        importing
          answer         = answer
        exceptions
          text_not_found = 1
          others         = 2.
      if sy-subrc <> 0.
        " Display message
      endif.
    else.
      concatenate lines_file space 'materials data supplied for creation/extension.'
      'No materials data can be processed/created/extended. See log for details.' into msg separated by space.
* Inform no creation/extension possible *
      message msg type 'I'.
    endif.

    " Add popup for blocked materials in case of extension
    if answer eq '1' and blk_cnt gt 0.  " User agrees to extend materials but some materials are found to be blocked
      clear: msg, answer_blk.
      msg = blk_cnt && ` ` && 'materials are blocked. Continue extending blocked materials?'.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar       = 'Blocked Material Extend Confirmation'
          text_question  = msg
          text_button_1  = 'Yes'
          text_button_2  = 'No'
*         START_COLUMN   = 25
*         START_ROW      = 6
        importing
          answer         = answer_blk
        exceptions
          text_not_found = 1
          others         = 2.
      if sy-subrc <> 0.
        " Display message
      endif.
    endif.
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " actual implementation here in subclasses
  endmethod.

  " IHDK904473
  method material_number_get.
    clear: material_number, t_material_number.
    call function 'BAPI_MATERIAL_GETINTNUMBER'
      exporting
        material_type    = material_type
        industry_sector  = 'C'
        required_numbers = 1                " get a single material number
      importing
        return           = return
      tables
        material_number  = t_material_number.

    try.
        material_number = t_material_number[ 1 ]-material.
        if material_number is not initial.
          call function 'CONVERSION_EXIT_MATN1_INPUT'
            exporting
              input  = material_number
            importing
              output = material_number.
        endif.
      catch cx_sy_itab_line_not_found ##no_handler.
    endtry.
  endmethod.

  method head_views_from_pstat.
    clear: lv_pstatlen, lv_index.
    lv_pstatlen = strlen( pstat ).  " IHDK900652
    do lv_pstatlen times.
      clear lv_index.
      lv_index = sy-index - 1.
      case pstat+lv_index(1). " IHDK900652
        when 'A'. move abap_true to headdata-work_sched_view.
        when 'B'. move abap_true to headdata-account_view.
*              WHEN 'C'. MOVE abap_true TO headdata-classification_view?
        when 'D'. move abap_true to headdata-mrp_view.
        when 'E'. move abap_true to headdata-purchase_view.
        when 'F'. move abap_true to headdata-prt_view.
        when 'G'. move abap_true to headdata-cost_view.
        when 'K'. move abap_true to headdata-basic_view.
        when 'L'. move abap_true to headdata-storage_view.
        when 'P'. move abap_true to headdata-forecast_view.
        when 'Q'. move abap_true to headdata-quality_view.
        when 'S'. move abap_true to headdata-warehouse_view.
        when 'V'. move abap_true to headdata-sales_view.
        when others.
      endcase.
    enddo.
  endmethod.

  method fill_bapix.
    " Local field symbols
    field-symbols: <fs_bapi>  type any,
                   <fs_bapix> type any,
                   <fs_x>     type any.

    clear: lo_descr, lv_relative_name.

    unassign: <fs_bapi>, <fs_bapix>, <fs_x>.

    assign: bapi  to <fs_bapi>,
    bapix to <fs_bapix>.

    if <fs_bapi> is assigned and <fs_bapix> is assigned.
      clear: <fs_bapix>.
      move 0 to sy-subrc.
      do.
        unassign <fs_x>.
        assign component sy-index of structure <fs_bapix> to <fs_x>.
        if sy-subrc <> 0.
          exit.
        endif.
        if <fs_x> is assigned.
          unassign <fs>.
          assign component sy-index of structure <fs_bapi> to <fs>.
          if <fs> is assigned and <fs> is not initial.
            lo_descr = cl_abap_typedescr=>describe_by_data( p_data = <fs_x> ).

            if lo_descr is bound.
              lv_relative_name = lo_descr->get_relative_name( ).
              if lv_relative_name eq c_bapix_type.
                move abap_true to <fs_x>.
              else.
                move <fs> to <fs_x>.
              endif.
            endif.
          endif.
        endif.
      enddo.
    endif.
  endmethod.

  " IRDK930708
  method get_tax_classification.
    refresh: taxclassifications, steuertab.
    clear: wa_taxclass, wa_steuer.
    " Get tax classification data for sal. org/distr. channel combination
    call function 'STEUERTAB_IDENTIFY'
      exporting
*       KZRFB                 = ' '
        vkorg                 = salesdata-sales_org
        bukrs_vkorg           = salesdata-sales_org
        vtweg                 = salesdata-distr_chan
      tables
        steuertab             = steuertab
      exceptions
        wrong_call            = 1
        vkorg_bukrs_not_found = 2
        steuertab_empty       = 3
        others                = 4.
    if sy-subrc <> 0.
*Implement suitable error handling here
    else.
      loop at steuertab into wa_steuer.
        move wa_steuer-aland to wa_taxclass-depcountry.
        move wa_steuer-tatyp to wa_taxclass-tax_type_1.
        move '0' to wa_taxclass-taxclass_1.
        move '0' to wa_taxclass-tax_ind.  " Not to be maintained for *FG type materials, cleared in respective classes
        append wa_taxclass to taxclassifications.
        clear: wa_steuer, wa_taxclass.
      endloop.
    endif.
  endmethod.
  " End IRDK930708

  method maint_material_classif.
    " Do Something
    clear: objectkey, objecttable, classnum, classtype, classif_status.
    refresh: allocvaluesnumnew, allocvaluescharnew, allocvaluescurrnew, mat_class_ret.
    move: material  to objectkey,
    'MARA'     to objecttable,
    class_num  to classnum,  "'ZSFG_FG' TO classnum,
    class_type to classtype.

    call function 'BAPI_OBJCL_CHANGE'
      exporting
        objectkey          = objectkey
        objecttable        = objecttable
        classnum           = classnum
        classtype          = classtype
      importing
        classif_status     = classif_status
      tables
        allocvaluesnumnew  = allocvaluesnumnew
        allocvaluescharnew = allocvaluescharnew
        allocvaluescurrnew = allocvaluescurrnew
        return             = mat_class_ret.

    if line_exists( mat_class_ret[ type = 'E' ] ) or classif_status ne 1.
      call function 'BAPI_TRANSACTION_ROLLBACK'. " IHDK903847
      " IHDK904473
      try.
          ms_err_log_db-classif_status = mat_class_ret[ type = 'E' ]-message.
        catch cx_sy_itab_line_not_found ##no_handler.
      endtry.
    else.
      call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847
      ms_err_log_db-classif_status = 'OK'.  " IHDK904473
    endif.
  endmethod.

  method maint_material_insp_setup.
    refresh: tq34, inspectionctrl.
    clear: wa_tq34, qmat, bapi_qmat, wa_inspctrl, wa_insp_ret, insp_status.

    select *
    from tq34
    into table tq34
    where art in s_art. "('01', '02', '04', '05', '06', '08', '09', '10', '89').

    if tq34 is not initial.
      loop at tq34 into wa_tq34.
        move material to qmat-matnr.
        move plant   to qmat-werks.
        move-corresponding wa_tq34 to qmat.
        if qmat is not initial.
          call function 'MAP2E_QMAT_TO_BAPI1001004_QMAT'
            exporting
              qmat             = qmat
            changing
              bapi1001004_qmat = bapi_qmat.
          if bapi_qmat is not initial.
            move '009' to wa_inspctrl-function.
            move-corresponding bapi_qmat to wa_inspctrl.
            move abap_true to wa_inspctrl-ind_insptype_mat_active.
            append wa_inspctrl to inspectionctrl.
          endif.
        endif.
        clear: wa_tq34, qmat, bapi_qmat, wa_inspctrl.
      endloop.

      if inspectionctrl is not initial.
        call function 'BAPI_MATINSPCTRL_SAVEREPLICA'
          tables
            return         = insp_return
            inspectionctrl = inspectionctrl.

        read table insp_return into wa_insp_ret with key type = 'E'.
        if sy-subrc = 0.
          clear: insp_status.
          call function 'BAPI_TRANSACTION_ROLLBACK'. " IHDK903847
          " IHDK904473
          try.
              ms_err_log_db-insp_status = insp_return[ type = 'E' ]-message.
            catch cx_sy_itab_line_not_found ##no_handler.
          endtry.
        else.
          move abap_true to insp_status.
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847
          ms_err_log_db-insp_status = 'OK'. " IHDK904473
        endif.
      endif.
    endif.
  endmethod.

  method extend_to_val_class.
    clear: headdata-basic_view,
           headdata-sales_view,
           headdata-purchase_view,
           headdata-mrp_view,
*          headdata-work_sched_view. " Excluded for ZTRD
           headdata-storage_view,
           headdata-quality_view,
           headdata-cost_view.

    move bklas to valuationdata-val_class.
    move bwtar to valuationdata-val_type.
    clear: valuationdata-val_cat.

    fill_bapix(
    exporting
      bapi = valuationdata
    changing
      bapix = valuationdatax ).

    clear return.
    refresh: returnmessages.
    call function 'BAPI_MATERIAL_SAVEDATA'
      exporting
        headdata       = headdata
        valuationdata  = valuationdata
        valuationdatax = valuationdatax
      importing
        return         = return
      tables
        returnmessages = returnmessages.

    read table returnmessages into wa_retmessage with key type = 'E' transporting message.
    if sy-subrc <> 0.
      if return-number = '356' and return-type = 'S'.
        call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " Rev 05
        move abap_true to ext_status.
        " IHDK904473
        ms_err_log_db-val_class_ext_status = 'OK'.
      endif.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.
      clear ext_status.
      " IHDK904473
      try.
          ms_err_log_db-val_class_ext_status = returnmessages[ type = 'E' ]-message.
        catch cx_sy_itab_line_not_found ##no_handler.
      endtry.
    endif.
  endmethod.

  method maintain_mrp_area. " Rev 14
    data: i_matnr        type mdma-matnr,
          i_werk         type mdma-werks,
          i_mrp_area     type mdma-berid,
          i_selfields    type sdibe_massfields,
          i_mdma         type mdma,
          i_dpop         type dpop,
          e_error_return type bapireturn1.

    clear mrp_status.
    if material is initial or plant is initial or mrp_ctrler is initial or mrp_ls_key is initial.
      mrp_status = abap_false.
      return.
    endif.

    clear: i_matnr, i_werk.
    i_matnr = material.
    i_werk  = plant.
    select * from mdlg into table @data(lt_mdlg) where werks = @i_werk.

    if lt_mdlg is not initial.
      loop at lt_mdlg into data(ls_mdlg).
        clear: i_mrp_area, i_selfields, i_mdma, i_dpop, e_error_return.
        i_mrp_area = ls_mdlg-berid.
        i_selfields = value #( xdismm = abap_true xdispo = abap_true xdisls = abap_true ).
        i_mdma = value #( mandt = sy-mandt matnr = i_matnr berid = i_mrp_area werks = i_werk
                          dismm = 'ND' dispo = mrp_ctrler disls = mrp_ls_key ).

        call function 'MD_MRP_LEVEL_CREATE_DATA'
          exporting
            i_matnr        = i_matnr
            i_werk         = i_werk
            i_mrp_area     = i_mrp_area
            i_selfields    = i_selfields
            i_mdma         = i_mdma
            i_dpop         = i_dpop
          importing
            e_error_return = e_error_return.

        if e_error_return is not initial.
          if not e_error_return-id = 'MD06' and not e_error_return-number = '416'.  " already exists
            mrp_status = abap_false.
            data(lv_error) = abap_true.
            ms_err_log_db-mrp_area_status = e_error_return-message. " IHDK904473
          endif.
        else.
          mrp_status = abap_true.
          ms_err_log_db-mrp_area_status = 'OK'. " IHDK904473
        endif.

        clear ls_mdlg.
      endloop.
    else.
      mrp_status = abap_false.
    endif.

    if lv_error eq abap_true.
      mrp_status = abap_false.
    endif.
  endmethod.

  method extend_to_stor_loc.  " IHDK900652
    " assume success; since logging is easier
    clear ext_status.
    ext_status = abap_true.

    if material is initial or plant is initial or matl_type is initial.
      ext_status = abap_false.
      return.
    endif.

    select * from zmat01_sloc_mast
      into table @data(lt_sloc_mast)
      where mtart     = @matl_type
      and   werks     = @plant
      and   division  = @division " IHDK900685
      and   lgort not in ( select lgort from mard where matnr = @material and werks = @plant ).
    if sy-subrc = 0.
      loop at lt_sloc_mast into data(ls_sloc_mast).
        clear headdata.
        headdata-material     = material.
        headdata-mrp_view     = abap_true.
        headdata-storage_view = abap_true.

        clear storagelocationdata.
        storagelocationdata-plant = plant.
        storagelocationdata-stge_loc = ls_sloc_mast-lgort.

        clear storagelocationdatax.
        fill_bapix(
          exporting
            bapi = storagelocationdata
          changing
            bapix = storagelocationdatax ).

        " Execute BAPI
        clear: return, wa_retmessage.
        refresh: returnmessages.
        call function 'BAPI_MATERIAL_SAVEDATA'
          exporting
            headdata             = headdata
            storagelocationdata  = storagelocationdata
            storagelocationdatax = storagelocationdatax
          importing
            return               = return
          tables
            returnmessages       = returnmessages.

        try.
            if line_exists( returnmessages[ type = 'E' ] ).

              call function 'BAPI_TRANSACTION_ROLLBACK'.
              ext_status = abap_false.
              " IHDK904473
              try.
                  ms_err_log_db-stor_loc_ext_status = returnmessages[ type = 'E' ]-message.
                catch cx_sy_itab_line_not_found ##no_handler.
              endtry.
            elseif return-number = '356' and return-id = 'MM' and return-type = 'S'.
              if ext_status ne abap_false.  " set false in a previous loop, => error overall

                call function 'BAPI_TRANSACTION_COMMIT'
                  exporting
                    wait = abap_true.

                ext_status = abap_true.
                ms_err_log_db-stor_loc_ext_status = 'OK'. " IHDK904473
              endif.
            endif.
          catch cx_sy_itab_line_not_found.
        endtry.

        clear ls_sloc_mast.
      endloop.
    else.
      " not maintained in master or already extended; in any case => not an error
      return.
    endif.
  endmethod.

  " IHDK904473
  method update_db_log.
    ms_err_log_db-act_date = sy-datum.
    ms_err_log_db-act_time = sy-uzeit.
    ms_err_log_db-act_user = sy-uname.
    ms_err_log_db-action   = mv_action.
    insert zmm_t_mat01_log from ms_err_log_db.
    if sy-dbcnt = 1.
      commit work.
    endif.
  endmethod.

  method log_output.
    " Do something
    data: sort_field type fieldname.
    data: lv_date(10) type c,
          lv_time(8)  type c,
          lv_filename type string.

    field-symbols: <fs_index> type any.

    clear: lv_date, lv_time, lv_filename.
    write: sy-datum to lv_date using edit mask '__-__-____',
    sy-uzeit to lv_time using edit mask '__.__.__'.
    concatenate filename lv_date '_' lv_time into lv_filename.

    clear: sort_field, lv_tabix.
    unassign: <fs_tab>, <fs_wa>, <fs>, <fs_index>.

    check it_log is not initial.
    move 'INDEX' to sort_field.
    sort it_log ascending by (sort_field).
    insert log_header2 into it_log index 1.
*    INSERT log_header1 INTO it_log INDEX 1.  " Replaced by it_head for DBF file

    assign it_log to <fs_tab>.
    check <fs_tab> is assigned.
    loop at <fs_tab> assigning <fs_wa>.
      add  1 to lv_tabix.
      sy-subrc = 0.
      do.
        assign component sy-index of structure <fs_wa> to <fs>.
        if sy-subrc <> 0.
          exit.
        endif.
        if <fs> is assigned.
          shift <fs> left deleting leading space.
        endif.
        unassign <fs>.
      enddo.
      if lv_tabix gt 1. " Leave out header line
        unassign <fs_index>.
        assign component 'INDEX' of structure <fs_wa> to <fs_index>.
        if <fs_index> is assigned.
          data(index) = conv i( <fs_index> ).
          unassign <fs>.
          assign component 'LOG' of structure <fs_wa> to <fs>.  " First check log for success message
          if <fs> is assigned.
            if <fs> cs 'has been created or extended'.  " Material creation/extension was successful
              " IRDK931550 - Indicate whether extension to valuation class was successful for successfully created/extended materials
              unassign <fs>.
              assign component 'BWTAR_DOM' of structure <fs_wa> to <fs>.
              if <fs> is assigned.  " not assigned if file format does not have this field
                if <fs> is not initial.  " <fs> not initial => bwtar_dom field in file is filled, extension is required
                  if <fs> cs 'Yes'. " Extension was successful
                    clear <fs_index>.
                    <fs_index> = index && '/S'.
                  else. " Extension failed, indicate with a *
                    clear <fs_index>.
                    <fs_index> = index && '/S*'.
                    continue. " Rev 12
                  endif.
                elseif <fs> is initial. " Extension was not required, so success
                  clear <fs_index>.
                  <fs_index> = index && '/S'.
                endif.
              else. " For file formats without valuation class extension
                clear <fs_index>.
                <fs_index> = index && '/S'.
              endif.
              unassign <fs>.
              assign component 'BWTAR_IMP' of structure <fs_wa> to <fs>.
              if <fs> is assigned.  " not assigned if file format does not have this field
                if <fs> is not initial.  " <fs> not initial => bwtar_dom field in file is filled, extension is required
                  if <fs> cs 'Yes'. " Extendion was successful
                    clear <fs_index>.
                    <fs_index> = index && '/S'.
                  else. " Extension failed, indicate with a *
                    clear <fs_index>.
                    <fs_index> = index && '/S*'.
                    continue. " Rev 12
                  endif.
                elseif <fs> is initial. " Extension was not required, so success
                  clear <fs_index>.
                  <fs_index> = index && '/S'.
                endif.
              else. " For file formats without valuation class extension
                clear <fs_index>.
                <fs_index> = index && '/S'.
              endif.
            else. " Material creation/extension failed
              unassign <fs>.
              assign component 'INDEX' of structure <fs_wa> to <fs>.
              if <fs> is assigned.
                <fs> = <fs> && '/E'.
                assign component 'MATNR' of structure <fs_wa> to field-symbol(<matnr>).
                if sy-subrc = 0 and  <matnr> is assigned and <matnr> is not initial.  " true only in case of extension
                  assign component 'LOG' of structure <fs_wa> to field-symbol(<log>).
                  if sy-subrc = 0 and <log> is assigned.
                    <log> = condense( |{ <matnr> alpha = out }| ) && ':' && ` ` && <log>.
                  endif.
                endif.
                unassign: <log>, <matnr>.
              endif.
            endif.
          endif.
        endif.
      endif.
    endloop.

    " save o/p log to desktop->MM_log by default => IHDK900227
    check it_log is not initial.

    data: desktop_directory type string.
    clear desktop_directory.
    cl_gui_frontend_services=>get_desktop_directory(
        changing
          desktop_directory     = desktop_directory
        exceptions
          cntl_error            = 1
          error_no_gui          = 2
          not_supported_by_gui  = 3
          others                = 4 ).
    cl_gui_cfw=>flush( exceptions cntl_error = 1 cntl_system_error = 2 others = 3 ).

    data(directory) = |{ desktop_directory }\\MM_Log|.
    data: rc type i.
    clear rc.
    call method cl_gui_frontend_services=>directory_create
      exporting
        directory                = directory
      changing
        rc                       = rc
      exceptions
        directory_create_failed  = 1
        cntl_error               = 2
        error_no_gui             = 3
        directory_access_denied  = 4
        directory_already_exists = 5
        path_not_found           = 6
        unknown_error            = 7
        not_supported_by_gui     = 8
        wrong_parameter          = 9
        others                   = 10.

    if sy-subrc = 0 or sy-subrc = 5.
* Implement suitable error handling here
      rc = sy-subrc.  " rc = 183 = subrc = 5 mean the same thing => directory_already_exists
      data(file_path) = |{ directory }\\{ lv_filename }.xls|.
      call method cl_gui_frontend_services=>gui_download
        exporting
          filename                = file_path
          filetype                = 'DBF'
          write_field_separator   = 'X'
          fieldnames              = it_head
        changing
          data_tab                = it_log
        exceptions
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          others                  = 24.
      if sy-subrc <> 0.
        disp_message(
        exporting
          type  = sy-msgty
          id    = sy-msgid
          no    = sy-msgno
          var1  = sy-msgv1
          var2  = sy-msgv2
          var3  = sy-msgv3
          var4  = sy-msgv4 ).
        raise exception type lcx_generic_error.
      else.
        message |Log saved to: { file_path }| type 'S'.
      endif.
    endif.

    log_alv_display( exporting it_log = it_log iv_filename = lv_filename ). " IHDK900690

*    cl_demo_output=>display( data = it_log name = 'Process Log' ).

*    clear: file, path, file_path, file_filter, title, defname, defext, useraction.
*    title = 'Save process log'.
*    defname = lv_filename.
*    defext = 'xls'.
*    file_filter = '*.xls*'.
*    call method cl_gui_frontend_services=>file_save_dialog
*      exporting
*        window_title              = title
*        default_file_name         = defname
*        default_extension         = defext
*        file_filter               = file_filter
*      changing
*        filename                  = file
*        path                      = path
*        fullpath                  = file_path
*        user_action               = useraction
*      exceptions
*        cntl_error                = 1
*        error_no_gui              = 2
*        not_supported_by_gui      = 3
*        invalid_default_file_name = 4
*        others                    = 5.
*    if sy-subrc <> 0.
*      disp_message(
*      exporting
*        type  = sy-msgty
*        id    = sy-msgid
*        no    = sy-msgno
*        var1  = sy-msgv1
*        var2  = sy-msgv2
*        var3  = sy-msgv3
*        var4  = sy-msgv4 ).
*      raise exception type lcx_generic_error.
*    else.
*      if useraction eq '9'. " Cancelled
*        disp_message(
*        exporting
*          type  = 'E'
*          id    = 'ZMM'
*          no    = '006' ).
*        raise exception type lcx_generic_error..
*      else.
*        call method cl_gui_frontend_services=>gui_download
*          exporting
*            filename                = file_path
*            filetype                = 'DBF'
*            write_field_separator   = 'X'
*            fieldnames              = it_head
*          changing
*            data_tab                = it_log
*          exceptions
*            file_write_error        = 1
*            no_batch                = 2
*            gui_refuse_filetransfer = 3
*            invalid_type            = 4
*            no_authority            = 5
*            unknown_error           = 6
*            header_not_allowed      = 7
*            separator_not_allowed   = 8
*            filesize_not_allowed    = 9
*            header_too_long         = 10
*            dp_error_create         = 11
*            dp_error_send           = 12
*            dp_error_write          = 13
*            unknown_dp_error        = 14
*            access_denied           = 15
*            dp_out_of_memory        = 16
*            disk_full               = 17
*            dp_timeout              = 18
*            file_not_found          = 19
*            dataprovider_exception  = 20
*            control_flush_error     = 21
*            not_supported_by_gui    = 22
*            error_no_gui            = 23
*            others                  = 24.
*        if sy-subrc <> 0.
*          disp_message(
*          exporting
*            type  = sy-msgty
*            id    = sy-msgid
*            no    = sy-msgno
*            var1  = sy-msgv1
*            var2  = sy-msgv2
*            var3  = sy-msgv3
*            var4  = sy-msgv4 ).
*          raise exception type lcx_generic_error.
*        endif.
*      endif.  " IF useraction EQ '9'.
*    endif.  " IF sy-subrc <> 0.
  endmethod.

  method log_alv_display. " IHDK900690
    include icons.
    check it_log is not initial.
    data(lo_table_descr) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( it_log ) ).
    data(lo_struct_descr) = cast cl_abap_structdescr( lo_table_descr->get_table_line_type( ) ).

    data(lt_component) = lo_struct_descr->get_components( ).

    data: proc_stat type icon_d.
    insert value #( name = 'PROC_STAT'
                    type = cast cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( proc_stat ) ) )
                    into lt_component index 1.

    data(lo_struct) = cl_abap_structdescr=>create(
                           exporting
                             p_components = lt_component ).

    data: ls_table type ref to data.
    create data ls_table type handle lo_struct.

    data(lo_tabletype) = cl_abap_tabledescr=>create(
                          exporting
                            p_line_type = cast cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( ls_table ) )
                            p_table_kind = cl_abap_tabledescr=>tablekind_std ).

    data: lt_table type ref to data.
    create data lt_table type handle lo_tabletype.

    field-symbols: <lt_table> type standard table.
    assign lt_table->* to <lt_table>.

    refresh <lt_table>.
    loop at it_log assigning field-symbol(<ls>).
      data(lv_index) = sy-tabix.
      append initial line to <lt_table> assigning field-symbol(<ls_table>).
      if <ls_table> is assigned.
        move-corresponding <ls> to <ls_table>.
        assign component 'PROC_STAT' of structure <ls_table> to field-symbol(<fs_proc_stat>).
        assign component 'INDEX' of structure <ls_table> to field-symbol(<fs_index>).
        if <fs_index> is assigned and <fs_proc_stat> is assigned and <fs_index> is not initial.
          if lv_index = 1.  " first line is column headers
            <fs_proc_stat> = 'Icon'.
          else.
            if <fs_index> ca 'S'.
              <fs_proc_stat> = icon_green_light.
            endif.
            if <fs_index> ca 'E'.
              <fs_proc_stat> = icon_red_light.
            endif.
            if <fs_index> na 'ES'.
              <fs_proc_stat> = icon_yellow_light.
            endif.
          endif.
        endif.
      endif.
    endloop.
    check <lt_table> is assigned and <lt_table> is not initial.

    " display log in alv
    try.
        cl_salv_table=>factory(
          importing
            r_salv_table = data(lo_table)
          changing
            t_table      = <lt_table> ).

        check lo_table is bound.

        data(lo_columns) = lo_table->get_columns( ).

        if lo_columns is bound.
          try.
              data(lt_col) = lo_columns->get( ).
              if lt_col is not initial.
                loop at lt_col into data(ls_col).
                  translate ls_col-columnname using '_ '.
                  ls_col-r_column->set_long_text( exporting value = conv scrtext_l( ls_col-columnname ) ).
                  ls_col-r_column->set_medium_text( exporting value = conv scrtext_m( ls_col-columnname ) ).
                  ls_col-r_column->set_short_text( exporting value = conv scrtext_s( ls_col-columnname ) ).

                  clear ls_col.
                endloop.
              endif.
            catch cx_salv_not_found.
          endtry.
          lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_functions) = lo_table->get_functions( ).
        if lo_functions is bound.
          lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_layout) = lo_table->get_layout( ).

        data(lo_key) = value salv_s_layout_key( report = sy-repid ).

        if lo_layout is bound.
          lo_layout->set_key( exporting value = lo_key ).

          lo_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

          lo_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
        endif.

        data(lo_display) = lo_table->get_display_settings( ).

        if lo_display is bound.
          lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
          translate iv_filename using '_ '.
          lo_display->set_list_header( exporting value = conv lvc_title( iv_filename ) ).
        endif.

        lo_table->display( ).
      catch cx_salv_msg.
    endtry.
  endmethod.

  method disp_message.
    refresh: it_messages.
    clear wa_message.
    call function 'BALW_BAPIRETURN_GET2'  " Generate the message string
      exporting
        type   = type
        cl     = id
        number = no
        par1   = var1
        par2   = var2
        par3   = var3
        par4   = var4
*       log_no = ' '
*       log_msg_no = ' '
*       parameter  = ' '
        row    = 1
*       field  = ' '
      importing
        return = wa_message.  " struct. bapiret2

    append wa_message to it_messages. " Displays only 1 message at a time,
    " it_messages is refreshed at the beginning of the method

    check it_messages is not initial.

    " FM to display the message table in a pop-up tabular format with additional functions
    call function 'FINB_BAPIRET2_DISPLAY'
      exporting
        it_message = it_messages.
  endmethod.
endclass.

class lcl_create_helper implementation.
  " IHDK904473
  method constructor.
    super->constructor( ).
    clear mv_action.
    mv_action = 'C'.  " creation
  endmethod.

  method material_validate.
    types: begin of ty_mara,
             matnr type matnr,
             mtart type mtart,
           end of ty_mara.

    data: it_mara type standard table of ty_mara,
          wa_mara type ty_mara.

    data: l_str type string.

    field-symbols: <fs_log_tab> type standard table,
                   <fs_log_wa>  type any,
                   <fs_log>     type any.

    material_validation( changing it_data = it_data it_log = it_log ). " IHDK900894

    unassign: <fs_tab>, <fs_wa>, <fs>,
              <fs_log_tab>, <fs_log_wa>, <fs_log>.

    clear: wa_makt.
    refresh: it_makt.

    " only internal number range assignment
    check it_data is not initial.
    assign it_data to <fs_tab>.
    assign it_log  to <fs_log_tab>.
    check <fs_tab> is assigned.
    l_str = 'MATNR IS NOT INITIAL'. " Dynamic conditions
    loop at <fs_tab> assigning <fs_wa> where (l_str).
      unassign <fs>.
      assign component 'SEL' of structure <fs_wa> to <fs>.
      if <fs> is assigned.
        clear <fs>.  " Unselect such materials
      endif.
      unassign <fs_log_wa>.
      append initial line to <fs_log_tab> assigning <fs_log_wa>.
      move-corresponding <fs_wa> to <fs_log_wa>.
      if <fs_log_wa> is assigned.
        unassign <fs_log>.
        assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
        if <fs_log> is assigned.
          unassign <fs>.
          assign component 'MATNR' of structure <fs_wa> to <fs>.
          if <fs> is assigned.
            shift <fs> left deleting leading '0'.
            <fs_log> = <fs> && ' you cannot supply an external material number for creation'.
          endif.
        endif.
      endif.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials

    " Do not allow same material description within same material type
    check <fs_tab> is not initial.
    clear l_str.
    l_str = 'MAKTX EQ <FS_TAB>-MAKTX'.
    select *
      from makt
      into table it_makt
      for all entries in <fs_tab>
      where (l_str)
      and   spras eq sy-langu.

    check it_makt is not initial.
    select matnr mtart
      from mara
      into table it_mara
      for all entries in it_makt
      where matnr eq it_makt-matnr.

    loop at <fs_tab> assigning <fs_wa>.
      clear: wa_makt.
      unassign <fs>.
      assign component 'MAKTX' of structure <fs_wa> to <fs>.
      if <fs> is assigned.
        read table it_makt into wa_makt with key maktx = <fs>.
        if sy-subrc = 0.
          unassign <fs>.
          assign component 'MTART' of structure <fs_wa> to <fs>.
          if <fs> is assigned.
            read table it_mara into wa_mara with key matnr = wa_makt-matnr  " Do not allow same material description within same material type
                                                     mtart = <fs>.
            if sy-subrc = 0.
              unassign <fs>.
              assign component 'SEL' of structure <fs_wa> to <fs>.
              if <fs> is assigned.
                clear <fs>.  " Unselect such materials
              endif.
              unassign <fs_log_wa>.
              append initial line to <fs_log_tab> assigning <fs_log_wa>.
              move-corresponding <fs_wa> to <fs_log_wa>.
              if <fs_log_wa> is assigned.
                unassign <fs_log>.
                assign component 'MATNR' of structure <fs_log_wa> to <fs_log>.
                if <fs_log> is assigned.
                  clear <fs_log>.
                  <fs_log> = wa_makt-matnr.
                  unassign <fs_log>.
                  assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
                  if <fs_log> is assigned.
                    shift wa_makt-matnr left deleting leading '0'.
                    <fs_log> = 'Material' && ` ` && wa_makt-matnr && ` ` && 'already exists with description' && ` ` && wa_makt-maktx.
                  endif.
                endif.
              endif.
            endif.
          endif.
        endif.
      endif.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials

    " IHDK902928 --->
    check <fs_tab> is not initial.
    select *
      from tvkbz
      into table @data(lt_plant_sales_area).

    loop at <fs_tab> assigning <fs_wa>.
      assign component 'VKORG' of structure <fs_wa> to field-symbol(<lv_vkorg>).
      assign component 'VTWEG' of structure <fs_wa> to field-symbol(<lv_vtweg>).
      assign component 'SPART' of structure <fs_wa> to field-symbol(<lv_spart>).
      assign component 'WERKS' of structure <fs_wa> to field-symbol(<lv_werks>).
      if <lv_vkorg> is assigned and <lv_vtweg> is assigned and <lv_spart> is assigned and <lv_werks> is assigned.
        if <lv_vkorg> is not initial and <lv_vtweg> is not initial and <lv_spart> is not initial and <lv_werks> is not initial.
          if not line_exists( lt_plant_sales_area[ vkorg = <lv_vkorg> vtweg = <lv_vtweg> spart = <lv_spart> vkbur = <lv_werks> ] ).
            assign component 'SEL' of structure <fs_wa> to <fs>.
            if <fs> is assigned.
              clear <fs>.  " Unselect such materials
            endif.
            unassign <fs_log_wa>.
            append initial line to <fs_log_tab> assigning <fs_log_wa>.
            move-corresponding <fs_wa> to <fs_log_wa>.
            if <fs_log_wa> is assigned.
              unassign <fs_log>.
              assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
              if <fs_log> is assigned.
                <fs_log> = |Plant { <lv_werks> } is not defined in sales area { <lv_vkorg> } { <lv_vtweg> } { <lv_spart> }|.
              endif.
            endif.
          endif.
        endif.
      endif.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials
    " ---> End IHDK902928
  endmethod.
endclass.

class lcl_extend_helper implementation.
  " IHDK904473
  method constructor.
    super->constructor( ).
    clear mv_action.
    mv_action = 'E'.  " extension
  endmethod.

  method material_validate.
    " Method implementation here
    types: begin of lty_plant,
             bwkey type t001k-bwkey,
           end of lty_plant,

           begin of lty_comp,
             bukrs type t001-bukrs,
           end of lty_comp.

    data: lt_comp  type table of lty_comp,  " List of company codes
          lw_comp  type lty_comp,
          lt_t001k type table of t001k,     " Get comp code for current plant 1:1
          lw_t001k type t001k,
          lt_plant type table of lty_plant, " Plant(Valuation Area) <-> CompanyCode, Get all plants for above comp code N:1
          lw_plant type lty_plant,
          l_str    type string.

    data: matnr type mara-matnr,
          werks type marc-werks.

    field-symbols: <fs_log_tab> type standard table,
                   <fs_log_wa>  type any,
                   <fs_log>     type any,
                   <fs_werks>   type any. " IRDK930708

    material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    clear: wa_mara, wa_marc, wa_mat, lw_comp, lw_t001k, lw_plant, l_str, matnr, werks.
    refresh: it_mara, it_marc, it_mat, lt_comp, lt_t001k, lt_plant.

    unassign: <fs_tab>, <fs_wa>, <fs>, <fs_werks>,  " IRDK930708
              <fs_log_tab>, <fs_log_wa>, <fs_log>.

    check it_data is not initial.
    assign it_data to <fs_tab>.
    assign it_log  to <fs_log_tab>.
    check <fs_tab> is assigned.
    l_str = 'MATNR IS INITIAL OR WERKS IS INITIAL'. " Dynamic conditions
    loop at <fs_tab> assigning <fs_wa> where (l_str).
      unassign <fs>.
      assign component 'SEL' of structure <fs_wa> to <fs>.
      if <fs> is assigned.
        clear <fs>.  " Unselect such materials
      endif.
      unassign <fs_log_wa>.
      append initial line to <fs_log_tab> assigning <fs_log_wa>.
      move-corresponding <fs_wa> to <fs_log_wa>.
      if <fs_log_wa> is assigned.
        unassign <fs_log>.
        assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
        if <fs_log> is assigned.
          unassign <fs>.
          assign component 'MATNR' of structure <fs_wa> to <fs>.
          if <fs> is assigned.
            if <fs> is initial.
              <fs_log> = <fs_log> && 'Material to be extended is mandatory'.
            endif.
          endif.
          unassign <fs>.
          assign component 'WERKS' of structure <fs_wa> to <fs>.
          if <fs> is assigned.
            if <fs> is initial.
              <fs_log> = <fs_log> && 'Target/Extend to plant is mandatory'.
            endif.
          endif.
        endif.
      endif.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials

    check <fs_tab> is not initial.
    clear l_str.
    l_str = 'MATNR EQ <FS_TAB>-MATNR'.
    select *
      from mara
      into table it_mara
      for all entries in <fs_tab>
      where (l_str).

    " Check for it_mara initial skipped on purpose as we need status for all materials,
    " if it_mara is indeed initial, all materials should be updated with 'does ot exist' status in log
    loop at <fs_tab> assigning <fs_wa>.
      unassign <fs>.
      assign component 'MATNR' of structure <fs_wa> to <fs>.
      if <fs> is assigned.  " IRDK930708
        clear wa_mara.
        read table it_mara into wa_mara with key matnr = <fs>.
        if sy-subrc <> 0.
          wa_mara-matnr = <fs>. " IRDK931038
          unassign <fs>.        " IRDK931038
          assign component 'SEL' of structure <fs_wa> to <fs>.
          if <fs> is assigned.
            clear <fs>.  " Unselect such materials
          endif.
          unassign <fs_log_wa>.
          append initial line to <fs_log_tab> assigning <fs_log_wa>.
          move-corresponding <fs_wa> to <fs_log_wa>.
          shift: wa_mara-matnr left deleting leading '0'.
          if <fs_log_wa> is assigned.
            assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
            if <fs_log> is assigned.
              concatenate 'Material' wa_mara-matnr 'does not exist.' into <fs_log> separated by space.  " IRDK930708
            endif.
          endif.
        endif.
      endif.  " IRDK930708
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials

    " IRDK930708
    " Do not allow extension, if material is already extended to target plant
    check <fs_tab> is not initial.
    clear l_str.
    l_str = 'MATNR EQ <FS_TAB>-MATNR'.
    select *
      from marc
      into table it_marc
      for all entries in <fs_tab>
      where (l_str)
      and   werks <> '1000'.  " IHDK906991

    loop at <fs_tab> assigning <fs_wa>.
      unassign: <fs>, <fs_werks>.
      assign component 'MATNR' of structure <fs_wa> to <fs>.
      assign component 'WERKS' of structure <fs_wa> to <fs_werks>.
      if <fs> is assigned and <fs_werks> is assigned.
        clear wa_marc.
        read table it_marc into wa_marc with key matnr = <fs> werks = <fs_werks>.
        if sy-subrc = 0.  " Material already exists in the target plant
          assign component 'SEL' of structure <fs_wa> to <fs>.
          if <fs> is assigned.
            clear <fs>.  " Unselect such materials, do not allow extension(will overwrite if allowed)
          endif.
          unassign <fs_log_wa>.
          append initial line to <fs_log_tab> assigning <fs_log_wa>.
          move-corresponding <fs_wa> to <fs_log_wa>.
          shift: wa_marc-matnr left deleting leading '0'.
          shift: wa_marc-werks left deleting leading '0'.
          if <fs_log_wa> is assigned.
            assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
            if <fs_log> is assigned.
              concatenate 'Material' wa_marc-matnr 'already exists/extended in/to plant' wa_marc-werks
              into <fs_log> separated by space.
            endif.
          endif.
        endif.
      endif.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>, <fs_werks>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials
    " End IRDK930708

* ---- Get reference plant, company code ---- *
    check <fs_tab> is not initial.
    select bukrs
      from t001
      into table lt_comp
      where land1 eq 'IN'     " Country
      and   spras eq 'E'      " Language
      and   ktopl eq '1000'   " Chart of Acc
      order by bukrs ascending.  " ATC: IHDK900896

    clear l_str.
    l_str = 'BWKEY EQ <FS_TAB>-WERKS'.
    select *
      from t001k
      into table lt_t001k
      for all entries in <fs_tab>
      where (l_str).

    if lt_t001k is not initial.
      sort lt_t001k ascending by bwkey bukrs.
      loop at <fs_tab> assigning <fs_wa>.
        clear lw_t001k.
        unassign <fs>.
        assign component 'WERKS' of structure <fs_wa> to <fs>.
        if <fs> is assigned.
          read table lt_t001k into lw_t001k with key bwkey = <fs>.
          if sy-subrc = 0.
            refresh lt_plant.
            select bwkey
              from t001k
              into corresponding fields of table lt_plant
              where bukrs eq lw_t001k-bukrs.  " Target company code

            if lt_plant is not initial.
              sort lt_plant ascending by bwkey.
              unassign <fs>.
              assign component 'MATNR' of structure <fs_wa> to <fs>.
              if <fs> is assigned.
                refresh it_marc.
                select *
                  from marc
                  into table it_marc
                  for all entries in lt_plant
                  where matnr eq <fs>
                  and   werks eq lt_plant-bwkey.
              endif.

              if it_marc is not initial.
                sort it_marc ascending by werks.
                clear: wa_marc, wa_mat.
                read table it_marc into wa_marc index 1.
                if sy-subrc = 0.
                  move: wa_marc-matnr   to wa_mat-matnr,
                        lw_t001k-bukrs  to wa_mat-source_bukrs, " Since the comp code of the source and target plant is same
                        lw_t001k-bukrs  to wa_mat-target_bukrs, " source comp code = target comp code
                        wa_marc-werks   to wa_mat-source_werks.
                  unassign <fs>.
                  assign component 'WERKS' of structure <fs_wa> to <fs>.
                  if <fs> is assigned.
                    move <fs> to wa_mat-target_werks.
                  endif.
                  append wa_mat to it_mat.
                endif.
              else. " If material is not present in the company code of the target plant
                " Get data from a plant of a company code in which it exists
                loop at lt_comp into lw_comp where bukrs ne lw_t001k-bukrs.
                  refresh lt_plant.
                  select bwkey
                    from t001k
                    into table lt_plant
                    where bukrs = lw_comp-bukrs.  " probable source/ref company code

                  if lt_plant is not initial.
                    sort lt_plant ascending by bwkey.
                    unassign <fs>.
                    assign component 'MATNR' of structure <fs_wa> to <fs>.
                    if <fs> is assigned.
                      refresh it_marc.
                      select *
                       from marc
                       into table it_marc
                       for all entries in lt_plant
                       where matnr eq <fs>
                       and   werks eq lt_plant-bwkey.
                    endif.
                  endif.

                  if it_marc is not initial.
                    sort it_marc ascending by werks.
                    clear: wa_marc, wa_mat.
                    read table it_marc into wa_marc index 1.
                    if sy-subrc = 0.
                      move: wa_marc-matnr   to wa_mat-matnr,
                            lw_comp-bukrs   to wa_mat-source_bukrs, " When comp code of source plant <> comp code of target plant
                            lw_t001k-bukrs  to wa_mat-target_bukrs,
                            wa_marc-werks   to wa_mat-source_werks.
                      unassign <fs>.
                      assign component 'WERKS' of structure <fs_wa> to <fs>.
                      if <fs> is assigned.
                        move <fs> to wa_mat-target_werks.
                      endif.
                      append wa_mat to it_mat.
                      exit.
                    endif.
                  endif.
                endloop.

                if it_marc is initial.  " If reference plant still not found
                  assign component 'SEL' of structure <fs_wa> to <fs>.
                  if <fs> is assigned.
                    clear <fs>.  " Unselect such materials
                  endif.
                  unassign <fs_log_wa>.
                  append initial line to <fs_log_tab> assigning <fs_log_wa>.
                  move-corresponding <fs_wa> to <fs_log_wa>.
                  if <fs_log_wa> is assigned.
                    assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
                    if <fs_log> is assigned.
                      unassign <fs>.
                      assign component 'MATNR' of structure <fs_wa> to <fs>.
                      if <fs> is assigned.
                        move <fs> to matnr.
                        shift: matnr left deleting leading '0'.
                      endif.
                      concatenate 'Material' matnr 'does not exist in any company code/plant'
                      into <fs_log> separated by space.
                    endif.
                  endif.
                endif.
              endif.
            endif.
          endif.
        endif.
      endloop.
    endif.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials

    " Begin validation for blocked matereials - IRDK930708
    check <fs_tab> is not initial.
    refresh blocked_materials.
    clear l_str.
    l_str = 'MATNR EQ <FS_TAB>-MATNR AND ( MSTAV LIKE ''Z%'' OR MSTAE LIKE ''Z%'' )'.
    select distinct matnr
    from mara
    into table blocked_materials
    for all entries in <fs_tab>
    where (l_str).

    clear l_str.
    l_str = 'MATNR EQ <FS_TAB>-MATNR AND VMSTA LIKE ''Z%'''.
    select distinct matnr
    from mvke
    appending table blocked_materials
    for all entries in <fs_tab>
    where (l_str).

    clear l_str.
    l_str = 'MATNR EQ <FS_TAB>-MATNR AND MMSTA LIKE ''Z%'''.
    select distinct matnr
    from marc
    appending table blocked_materials
    for all entries in <fs_tab>
    where (l_str).

    sort blocked_materials ascending by matnr.
    delete adjacent duplicates from blocked_materials comparing matnr.

    describe table blocked_materials lines v_block_material_count.
    " End IRDK930708
  endmethod.

  " IRDK930708
  method filter_out_blocked_materials.
    data: l_str type string.

    field-symbols: <fs_log_tab> type standard table,
                   <fs_log_wa>  type any,
                   <fs_log>     type any.

    unassign: <fs_tab>, <fs_wa>, <fs>,
              <fs_log_tab>, <fs_log_wa>, <fs_log>.

    check it_data is not initial.
    assign: it_data to <fs_tab>.
    assign it_log  to <fs_log_tab>.
    check <fs_tab> is assigned.
    unassign <fs_wa>.
    loop at <fs_tab> assigning <fs_wa>.
      unassign <fs>.
      assign component 'MATNR' of structure <fs_wa> to <fs>.
      if <fs> is assigned.
        clear blocked_material.
        read table blocked_materials into blocked_material with key matnr = <fs>.
        if sy-subrc = 0.  " this material is blocked for any reason
          assign component 'SEL' of structure <fs_wa> to <fs>.
          if <fs> is assigned.
            clear <fs>.  " Unselect such materials
          endif.
          unassign <fs_log_wa>.
          append initial line to <fs_log_tab> assigning <fs_log_wa>.
          move-corresponding <fs_wa> to <fs_log_wa>.
          if <fs_log_wa> is assigned.
            assign component 'LOG' of structure <fs_log_wa> to <fs_log>.
            if <fs_log> is assigned.
              shift blocked_material-matnr left deleting leading '0'.
              concatenate 'Material' blocked_material-matnr 'is blocked. Processing cancelled.' into <fs_log> separated by space.
            endif.
          endif.
        endif.
      endif.
    endloop.
    unassign: <fs_wa>, <fs_log_wa>, <fs>, <fs_log>.
    clear l_str.
    l_str = 'SEL NE ABAP_TRUE'.
    delete <fs_tab> where (l_str).  " Delete unselected materials
  endmethod.
  " End IRDK930708
endclass.

class lcl_zfgm_zlfg_create implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_crt_helper = new lcl_create_helper( ).
    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
    changing
      answer = me->answer ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        me->material_create_extend( ).  " Rename - IHDK900652
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZFGM_ZLFG_Create_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_crt_helper is bound.
    lo_crt_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear headdata.

      move 'C'            to headdata-ind_sector.    " MBRSH
      move wa_data-mtart  to headdata-matl_type.
      move abap_true      to headdata-basic_view.
      move abap_true      to headdata-sales_view.
      move abap_true      to headdata-purchase_view.
      move abap_true      to headdata-mrp_view.
      move abap_true      to headdata-work_sched_view.
      move abap_true      to headdata-storage_view.
      move abap_true      to headdata-quality_view.
      move abap_true      to headdata-account_view.
      move abap_true      to headdata-cost_view.
      move 'I'            to headdata-inp_fld_check.

      if headdata-matl_type ne 'ZFGM' and headdata-matl_type ne 'ZLFG'. " IHDK900652
        wa_log-log = |Incorrect material type { headdata-matl_type }|.
        append wa_log to it_log.
        continue.
      endif.

      clear clientdata.
      move wa_data-meins to clientdata-base_uom.
      move wa_data-matkl to clientdata-matl_group.
      move wa_data-bismt to clientdata-old_mat_no.
      move wa_data-spart to clientdata-division.
      move 'NORM'        to clientdata-item_cat.    " MTPOS_MARA
      move wa_data-ntgew to clientdata-net_weight.
      move wa_data-gewei to clientdata-unit_of_wt.
      move '0001'        to clientdata-trans_grp.   " TRAGR
      move wa_data-bstme to clientdata-po_unit.
      move wa_data-vabme to clientdata-var_ord_un.
      move wa_data-ekwsl to clientdata-pur_valkey.
      move wa_data-mhdrz to clientdata-minremlife.
      move wa_data-mhdhb to clientdata-shelf_life.
      move wa_data-prodh to clientdata-prod_hier.   " PRODH_H
      move abap_true     to clientdata-batch_mgmt.  " XCHPF

      fill_bapix(
      exporting
        bapi = clientdata
      changing
        bapix = clientdatax ).

      clear plantdata.
      move wa_data-werks to plantdata-plant.
      move 'Z001'        to plantdata-loadinggrp.   " LADGR
      move 'IN'          to plantdata-countryori.   " HERKL
      move wa_data-herkr to plantdata-regionorig.
      move wa_data-steuc to plantdata-ctrl_code.
      move wa_data-ekgrp to plantdata-pur_group.
      move wa_data-kordb to plantdata-sourcelist.
      move wa_data-dismm to plantdata-mrp_type.
      move wa_data-dispo to plantdata-mrp_ctrler.
      move wa_data-minbe to plantdata-reorder_pt.
      move wa_data-disls to plantdata-lotsizekey.
      move wa_data-bstmi to plantdata-minlotsize.
      move wa_data-bstma to plantdata-maxlotsize.
      move wa_data-bstfe to plantdata-fixed_lot.
      move wa_data-eisbe to plantdata-safety_stk.
      move wa_data-ausss to plantdata-assy_scrap.
      move wa_data-beskz to plantdata-proc_type.
      move wa_data-sobsl to plantdata-spproctype.
      move '3'           to plantdata-batchentry.   " KZECH
      move wa_data-lgpro to plantdata-iss_st_loc.
      move wa_data-lgfsb to plantdata-sloc_exprc.
      move '1'           to plantdata-backflush.    " MARC-RGEKZ, PLANTDATA-BACKFLUSH with DType RGEKM
      move '1'           to plantdata-inhseprodt.   " DZEIT
      move wa_data-plifz to plantdata-plnd_delry.
      move wa_data-webaz to plantdata-gr_pr_time.
      move wa_data-fhori to plantdata-sm_key.
      move wa_data-rwpro to plantdata-covprofile.
      move wa_data-strgr to plantdata-plan_strgp.
      move wa_data-mtvfp to plantdata-availcheck.
      move wa_data-sbdkz to plantdata-dep_req_id.
      if clientdata-division eq '10' or clientdata-division eq '15'.
        move 'AGR'       to plantdata-production_scheduler.   " FEVOR
        move 'AGRO'      to plantdata-prodprof.               " MARC-SFCPF, PLANTDATA-PRODPROF with Dtype CO_PRODPRF
      elseif clientdata-division eq '20' or clientdata-division eq '28'  .
        move 'SPC'       to plantdata-production_scheduler.   " FEVOR
        move 'SPCD'      to plantdata-prodprof.               " MARC-SFCPF, PLANTDATA-PRODPROF with Dtype CO_PRODPRF
      endif.
      move wa_data-uneto to plantdata-under_tol.
      move wa_data-ueeto to plantdata-over_tol.
      " IHDK900305
      read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
      if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
        wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
        append wa_log to it_log.
        continue.
      endif.
      move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
      plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
      move wa_data-prfrq to plantdata-insp_int.
      move wa_data-ncost to plantdata-no_costing.     " MARC-NCOST, PLANTDATA-NO_COSTING with Dtype CK_NO_COSTING
      move wa_data-awsls to plantdata-variance_key.
      move wa_data-sobsk to plantdata-specprocty.     " MARC-SOBSK, PLANTDATA-SPECPROCTY with Dtype CK_SOBSL
      move '1000'        to plantdata-lot_size.       " LOSGR

      fill_bapix(
      exporting
        bapi = plantdata
      changing
        bapix = plantdatax ).

      clear storagelocationdata.
      move plantdata-plant to storagelocationdata-plant.
      move wa_data-lgort   to storagelocationdata-stge_loc.

      fill_bapix(
      exporting
        bapi = storagelocationdata
      changing
        bapix = storagelocationdatax ).

      clear valuationdata.
      move plantdata-plant  to valuationdata-val_area.  " ? include as it is mandatory
      move wa_data-bklas    to valuationdata-val_class.
      move 'S'              to valuationdata-price_ctrl.  " IHDK900305
      move '1'              to valuationdata-price_unit.  " IHDK900305
*     move wa_data-verpr    to valuationdata-moving_pr.   " IHDK900305
      move '1'              to valuationdata-std_price.   " IHDK900305
      move abap_true        to valuationdata-qty_struct.  " MBEW-EKALR, VALUATIONDATA-QTY_STRUCT with Dtype CK_EKALREL
      move abap_true        to valuationdata-orig_mat.    " HKMAT
      move wa_data-hrkft    to valuationdata-orig_group.
      move ls_pctr_mast-kosgr to valuationdata-overhead_grp.  " IHDK900305
      clear ls_pctr_mast. "IHDK900305.

      fill_bapix(
      exporting
        bapi = valuationdata
      changing
        bapix = valuationdatax ).

      clear salesdata.
      move wa_data-vkorg to salesdata-sales_org.
      move wa_data-vtweg to salesdata-distr_chan.
      move wa_data-vrkme to salesdata-sales_unit.
      move wa_data-dwerk to salesdata-delyg_plnt.
      move wa_data-sktof to salesdata-cash_disc.
      move wa_data-versg to salesdata-matl_stats.   " MVKE-VERSG, SALESDATA-MATL_STATS with Dtype STGMA
      move wa_data-bonus to salesdata-rebate_grp.
      move wa_data-kondm to salesdata-mat_pr_grp.
      " IHDK901148
      if wa_data-ktgrm is initial.
        wa_log-log = |Account assignment group is mandatory|.
        append wa_log to it_log.
        continue.
      else.
        move wa_data-ktgrm to salesdata-acct_assgt.
      endif.
      move 'NORM'        to salesdata-item_cat.     " MTPOS
      move wa_data-mvgr1 to salesdata-matl_grp_1.
      move wa_data-mvgr2 to salesdata-matl_grp_2.
      move wa_data-mvgr3 to salesdata-matl_grp_3.
      move wa_data-mvgr4 to salesdata-matl_grp_4.
      move wa_data-mvgr5 to salesdata-matl_grp_5.
      move wa_data-aumng to salesdata-min_order.    " MVKE-AUMNG, SALESDATA-MIN_ORDER with Dtype MINAU

      fill_bapix(
      exporting
        bapi = salesdata
      changing
        bapix = salesdatax ).

      refresh materialdescription.
      clear wa_matdesc.
      move sy-langu       to wa_matdesc-langu.
      move wa_data-maktx  to wa_matdesc-matl_desc.
      append wa_matdesc to materialdescription.

      refresh: unitsofmeasure, unitsofmeasurex.
      clear: wa_uom, wa_uomx.
      move clientdata-base_uom    to wa_uom-alt_unit.
      move wa_data-brgew          to wa_uom-gross_wt.
      fill_bapix(
      exporting
        bapi = wa_uom
      changing
        bapix = wa_uomx ).
      append: wa_uom  to unitsofmeasure,
      wa_uomx to unitsofmeasurex.

      " IRDK930708
      get_tax_classification(
        exporting
          salesdata = salesdata
        changing
          taxclassifications = taxclassifications ).

      if taxclassifications is not initial.
        loop at taxclassifications into wa_taxclass.
          clear wa_taxclass-tax_ind.  " Tax indicator not to be maintained for *FG
          modify taxclassifications from wa_taxclass transporting tax_ind.
          clear wa_taxclass.
        endloop.
      endif.
      " End IRDK930708

      " IHDK904473
      clear: mat_no_return, ms_err_log_db.

      if wa_data-matnr is initial.  " To do else => log as error, material to be created cannot be supplied externally
        headdata-material = material_number_get(
                              exporting
                                material_type = wa_data-mtart
                              importing
                                return = mat_no_return ).

        if headdata-material is initial or mat_no_return-type <> 'S'.
          wa_log-log = mat_no_return-message.
          append wa_log to it_log.
          continue.
        endif.
      else.
        continue.
      endif.

      " Note: For Extensionin refer https://gist.github.com/saurabhk-nbssap/0ca3873d400e1581f7a91ffa3c219b72

      " Execute BAPI
      clear: return, wa_retmessage.
      refresh: returnmessages.
      call function 'BAPI_MATERIAL_SAVEDATA'
        exporting
          headdata             = headdata
          clientdata           = clientdata
          clientdatax          = clientdatax
          plantdata            = plantdata
          plantdatax           = plantdatax
          storagelocationdata  = storagelocationdata
          storagelocationdatax = storagelocationdatax
          valuationdata        = valuationdata
          valuationdatax       = valuationdatax
          salesdata            = salesdata
          salesdatax           = salesdatax
        importing
          return               = return
        tables
          materialdescription  = materialdescription
          unitsofmeasure       = unitsofmeasure
          unitsofmeasurex      = unitsofmeasurex
          taxclassifications   = taxclassifications
          returnmessages       = returnmessages.

      read table returnmessages into wa_retmessage with key type = 'E' transporting message.
      if sy-subrc = 0.  " Error exists, log it in error log
        wa_log-log = wa_retmessage-message.
        append wa_log to it_log.
        clear: wa_retmessage. " IHDK904473

        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else. " No error
        if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

          maint_material_classif(
            exporting
              material = headdata-material
              class_num = 'ZSFG_FG'
              class_type = '023'   " IHDK901148
            changing
              classif_status = classif_status ).

          " Build insp-type select options
          refresh: s_art.
          clear s_art_wa.
          s_art_wa-sign   = 'I'.
          s_art_wa-option = 'EQ'.

          s_art_wa-low    = '01'.
          append s_art_wa to s_art.
*          s_art_wa-low    = '02'.    " IRDK932590
*          append s_art_wa to s_art.
          s_art_wa-low    = '04'.
          append s_art_wa to s_art.
          s_art_wa-low    = '05'.
          append s_art_wa to s_art.
          s_art_wa-low    = '06'.
          append s_art_wa to s_art.
          s_art_wa-low    = '08'.
          append s_art_wa to s_art.
          s_art_wa-low    = '09'.
          append s_art_wa to s_art.
          s_art_wa-low    = '10'.
          append s_art_wa to s_art.
          s_art_wa-low    = '89'.
          append s_art_wa to s_art.

          maint_material_insp_setup(
           exporting
             material = headdata-material
             plant    = plantdata-plant
             s_art    = s_art
           changing
             insp_status = insp_status ).

          " Rev 14
          maintain_mrp_area(
            exporting
              material    = headdata-material
              plant       = plantdata-plant
              mrp_ctrler  = plantdata-mrp_ctrler
              mrp_ls_key  = plantdata-lotsizekey
            importing
              mrp_status  = data(mrp_status) ).

          wa_log-matnr = headdata-material.
          wa_log-log = return-message.
          " Check status of classification
          if classif_status ne 1.
            wa_log-log = wa_log-log && '. Classification data: Failed.'.
          endif.

          " Check status of inspection setup
          if insp_status ne 'X'.
            wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
          endif.

          " Rev 14
          " check status of mrp area maintainance
          if mrp_status eq abap_false.
            wa_log-log = wa_log-log && '. MRP Area: Failed'.
          endif.

          extend_to_stor_loc( " IHDK900652
            exporting
              material   = headdata-material
              plant      = plantdata-plant
              matl_type  = headdata-matl_type
              division   = clientdata-division  " IHDK900685
            importing
              ext_status = data(lv_ext_status) ).

          if lv_ext_status eq abap_false.
            wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
          endif.

          append wa_log to it_log.
          clear: lv_ext_status. " IHDK904473
        endif.  " IF return-number = '356'
      endif.
      " End read returnmessages
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zfgm_zlfg_ext_plant implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
                          changing it_data = it_data
                                   it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " End IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZFGM_ZLFG_Plant_Extend_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
                                                                   target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, salesdata, _return_wa.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        " For sales org data, pass the source company code as the ref sales org
        " Change the sales org to the target comp code, copy rest of the sales data as it is
        " if the source cc = target cc, it will be just like copying the sales data without any change
        " if the source cc <> target cc, the material will be extended to the target sales org using the data of the source sales org

        " Get distribution channel
        clear lo_ext_helper->v_dist_ch.
        select min( vtweg )
          from mvke
          into lo_ext_helper->v_dist_ch
          where matnr eq lo_ext_helper->wa_mat-matnr
          and   vkorg eq lo_ext_helper->wa_mat-source_bukrs.

        if lo_ext_helper->v_dist_ch is initial.
          shift: wa_data-matnr left deleting leading '0',
                 lo_ext_helper->wa_mat-source_bukrs left deleting leading '0',
                 lo_ext_helper->wa_mat-source_werks left deleting leading '0'.
          wa_log-log = 'No reference sales data found: Ref Comp\Sales Org:' && ` ` && lo_ext_helper->wa_mat-source_bukrs
                && ` ` && 'Ref. plant:' && ` ` && lo_ext_helper->wa_mat-source_werks.
          append wa_log to it_log.
          clear: wa_log.
          continue.
        endif.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
            salesorganisation   = lo_ext_helper->wa_mat-source_bukrs  " sales org is maintained same as comp code for that comp code in masters
            distributionchannel = lo_ext_helper->v_dist_ch
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
            salesdata           = _salesdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial or _valuationdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, storagelocationdata, valuationdata,
                 plantdatax, storagelocationdatax, valuationdatax.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
            exporting
              pstat = _clientdata-maint_stat
            changing
              headdata = headdata ).

          if headdata-matl_type ne 'ZFGM' and headdata-matl_type ne 'ZLFG'. " IHDK900652
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-bstmi to plantdata-minlotsize.
          move wa_data-beskz to plantdata-proc_type.
          move wa_data-sobsl to plantdata-spproctype.
          move wa_data-lgfsb to plantdata-sloc_exprc.
          move wa_data-mtvfp to plantdata-availcheck.
          " IHDK900305
          read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
          if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
            wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
            append wa_log to it_log.
            continue.
          endif.
          move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
          plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
          move '1'           to plantdata-inhseprodt. " Rev 03
          move '2'           to plantdata-plnd_delry. " Rev 03
*** ---- Additional Requirement ----- ***
          " Specific fields not to be copied are cleared here
          clear: " Fields in MRP3 view 'Planning' tab
                 plantdata-consummode,
                 plantdata-bwd_cons,
                 plantdata-fwd_cons,
                 plantdata-mixed_mrp,
                 " Fields from MVGD/MPGD - Planning Data/View, currently not included
*                 planningdata-plng_matl, " Planning material
*                 planningdata-plng_plant, " Planning plant
*                 planningdata-convfactor. " Conv. factor f. plng material
                 " Planning matl BUnit : not found

                 " Fields in WorkScheduling View 'In-house production time in days' tab
                 plantdata-setuptime,
                 plantdata-proc_time,
                 plantdata-interop,
                 plantdata-base_qty,
                 plantdata-specprocty,  " Rev 03

                 " other fields
                 plantdata-covprofile.  " IHDK900305, do not copy dss profile(rwpro) from source plant
*** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          " Fill storagelocationdata
          " Note storage location data is not fetched using getall bapi, only the fields plant, stge_loc need to be passed as provided in file
          move wa_data-werks to storagelocationdata-plant.
          move wa_data-lgort to storagelocationdata-stge_loc.
          " Fill storagelocationdatax
          fill_bapix(
          exporting
            bapi = storagelocationdata
          changing
            bapix = storagelocationdatax ).

          " Fill valuationdata
          move-corresponding _valuationdata to valuationdata.
          move: wa_data-werks to valuationdata-val_area,
                'S'           to valuationdata-price_ctrl,  " IHDK900305
                '1'           to valuationdata-price_unit,  " IHDK900305
*                wa_data-verpr to valuationdata-moving_pr,  " IHDK900305
                '1'           to valuationdata-std_price,   " IHDK900305
                ls_pctr_mast-kosgr to valuationdata-overhead_grp.  " IHDK900305
          clear ls_pctr_mast. " IHDK900305
          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill salesdata
          move-corresponding _salesdata to salesdata.
          move: lo_ext_helper->wa_mat-target_bukrs to salesdata-sales_org.

          " IHDK901148
          if salesdata-acct_assgt is initial.
            wa_log-log = |Mandatory field account assignment group is missing|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill salesdatax
          fill_bapix(
          exporting
            bapi = salesdata
          changing
            bapix = salesdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Commented - IRDK930708
          " Fill taxclassifications
*          MOVE-CORRESPONDING _taxclassifications TO taxclassifications.
*          LOOP AT taxclassifications INTO wa_taxclass. " WHERE taxclass_1 IS INITIAL.  " Rev 01
*            MOVE '0' TO wa_taxclass-taxclass_1.
*            MOVE '0' TO wa_taxclass-tax_ind.   " Rev 01
*            MODIFY taxclassifications FROM wa_taxclass TRANSPORTING taxclass_1 taxind.
*            CLEAR: wa_taxclass.
*          ENDLOOP.

          " Re-maintain tax classification for extension from scratch(as in case of creation) rather than depending on tax classification ...
          " ...of source plant since it could have certain data missing(due to post GST requirements)
          " Hence, above code commented and below code added
          get_tax_classification(
            exporting
              salesdata = salesdata
            changing
              taxclassifications = taxclassifications ).

          if taxclassifications is not initial.
            loop at taxclassifications into wa_taxclass.
              clear wa_taxclass-tax_ind.  " Tax indicator not to be maintained for *FG
              modify taxclassifications from wa_taxclass transporting tax_ind.
              clear wa_taxclass.
            endloop.
          endif.
          " End IRDK930708

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = headdata
              clientdata           = clientdata
              clientdatax          = clientdatax
              plantdata            = plantdata
              plantdatax           = plantdatax
              storagelocationdata  = storagelocationdata
              storagelocationdatax = storagelocationdatax
              valuationdata        = valuationdata
              valuationdatax       = valuationdatax
              salesdata            = salesdata
              salesdatax           = salesdatax
            importing
              return               = return
            tables
              materialdescription  = materialdescription
              unitsofmeasure       = unitsofmeasure
              unitsofmeasurex      = unitsofmeasurex
              taxclassifications   = taxclassifications
              returnmessages       = returnmessages.
          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_retmessage.  " IHDK904473

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

              " Build insp-type select options
              refresh: s_art.
              clear s_art_wa.
              s_art_wa-sign   = 'I'.
              s_art_wa-option = 'EQ'.

              s_art_wa-low    = '01'.
              append s_art_wa to s_art.
*              s_art_wa-low    = '02'.    " IRDK932590
*              append s_art_wa to s_art.
              s_art_wa-low    = '04'.
              append s_art_wa to s_art.
              s_art_wa-low    = '05'.
              append s_art_wa to s_art.
              s_art_wa-low    = '06'.
              append s_art_wa to s_art.
              s_art_wa-low    = '08'.
              append s_art_wa to s_art.
              s_art_wa-low    = '09'.
              append s_art_wa to s_art.
              s_art_wa-low    = '10'.
              append s_art_wa to s_art.
              s_art_wa-low    = '89'.
              append s_art_wa to s_art.

              maint_material_insp_setup(
                exporting
                  material = headdata-material
                  plant    = plantdata-plant
                  s_art    = s_art
                changing
                  insp_status = insp_status ).

              " Rev 14
              " dispo and disls is not part of input file, this logic prevents empty input to below method in case it is not copied from source plant
              if plantdata-mrp_ctrler is initial.
                case clientdata-division.
                  when '10'.
                    plantdata-mrp_ctrler = '010'.
                  when '15'.
                    plantdata-mrp_ctrler = '011'.
                  when '20'.
                    plantdata-mrp_ctrler = 'IL5'.
                  when '28'.
                    plantdata-mrp_ctrler = 'IL5'.
                  when others.
                endcase.
              endif.

              if plantdata-lotsizekey is initial.
                plantdata-lotsizekey = 'EX'.
              endif.

              maintain_mrp_area(
                exporting
                  material    = headdata-material
                  plant       = plantdata-plant
                  mrp_ctrler  = plantdata-mrp_ctrler
                  mrp_ls_key  = plantdata-lotsizekey
                importing
                  mrp_status  = data(mrp_status) ).

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant/depot' && ` ` && wa_data-werks.
              " Check status of inspection setup
              if insp_status ne 'X'.
                wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
              endif.

              " Rev 14
              " check status of mrp area maintainance
              if mrp_status eq abap_false.
                wa_log-log = wa_log-log && '. MRP Area: Failed'.
              endif.

              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division  " IHDK900685
                importing
                  ext_status = data(lv_ext_status) ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zfgm_zlfg_ext_depot implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
                          changing it_data = it_data
                                   it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " End IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZFGM_ZLFG_Depot_Extend_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log. " Prepare for log creation

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
                                                                   target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, salesdata, _return_wa.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        " For sales org data, pass the source company code as the ref sales org
        " Change the sales org to the target comp code, copy rest of the sales data as it is
        " if the source cc = target cc, it will be just like copying the sales data without any change
        " if the source cc <> target cc, the material will be extended to the target sales org using the data of the source sales org

        " Get distribution channel
        clear lo_ext_helper->v_dist_ch.
        select min( vtweg )
          from mvke
          into lo_ext_helper->v_dist_ch
          where matnr eq lo_ext_helper->wa_mat-matnr
          and   vkorg eq lo_ext_helper->wa_mat-source_bukrs.

        if lo_ext_helper->v_dist_ch is initial.
          shift: wa_data-matnr left deleting leading '0',
                 lo_ext_helper->wa_mat-source_bukrs left deleting leading '0',
                 lo_ext_helper->wa_mat-source_werks left deleting leading '0'.
          wa_log-log = 'No reference sales data found: Ref Comp\Sales Org:' && ` ` && lo_ext_helper->wa_mat-source_bukrs
                && ` ` && 'Ref. plant:' && ` ` && lo_ext_helper->wa_mat-source_werks.
          append wa_log to it_log.
          clear: wa_log.
          continue.
        endif.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
            salesorganisation   = lo_ext_helper->wa_mat-source_bukrs  " sales org is maintained same as comp code for that comp code in masters
            distributionchannel = lo_ext_helper->v_dist_ch
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
            salesdata           = _salesdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial or _valuationdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, storagelocationdata, valuationdata, salesdata,
                 plantdatax, storagelocationdatax, valuationdatax, salesdatax.
          refresh: materialdescription, unitsofmeasure, taxclassifications.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
            exporting
              pstat = _clientdata-maint_stat
            changing
              headdata = headdata ).

          if headdata-matl_type ne 'ZFGM' and headdata-matl_type ne 'ZLFG'. " IHDK900652
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-ekgrp to plantdata-pur_group.
          move wa_data-plifz to plantdata-plnd_delry.
          move wa_data-bstmi to plantdata-minlotsize.
          move wa_data-beskz to plantdata-proc_type.
          move wa_data-sobsl to plantdata-spproctype.
          move wa_data-lgfsb to plantdata-sloc_exprc.
          move wa_data-mtvfp to plantdata-availcheck.
          " IHDK900305
          read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
          if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
            wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
            append wa_log to it_log.
            continue.
          endif.
          move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
          plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
          move wa_data-rwpro to plantdata-covprofile.
*** ---- Additional Requirement ----- ***
          " Specific fields not to be copied are cleared here(As per requirement)
          clear: " Fields in MRP2/3 view 'Planning'tab
                 plantdata-consummode,
                 plantdata-bwd_cons,
                 plantdata-fwd_cons,
                 plantdata-mixed_mrp,
                 " Additional fields to be cleared in case of depot extension
                 plantdata-iss_st_loc,
                 plantdata-inhseprodt,
                 plantdata-insp_int,

                 " Fields from MVGD/MPGD - Planning Data/View, currently not included
*                 planningdata-plng_matl, " Planning material
*                 planningdata-plng_plant, " Planning plant
*                 planningdata-convfactor. " Conv. factor f. plng material
                 " Planning matl BUnit : not found

                 " Fields in WorkScheduling View 'In-house production time in days' tab
                 plantdata-setuptime,
                 plantdata-proc_time,
                 plantdata-interop,
                 plantdata-base_qty,
                 " Additional fields to be cleared in case of depot extension
                 plantdata-production_scheduler,
                 plantdata-prodprof,
                 plantdata-under_tol,
                 plantdata-over_tol,

                 " other fields
                 plantdata-specprocty.  " IRDK931344
*** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          " Fill storagelocationdata
          " Note storage location data is not fetched using getall bapi, only the fields plant, stge_loc need to be passed as provided in file
          move wa_data-werks to storagelocationdata-plant.
          move wa_data-lgort to storagelocationdata-stge_loc.
          " Fill storagelocationdatax
          fill_bapix(
          exporting
            bapi = storagelocationdata
          changing
            bapix = storagelocationdatax ).

          " Fill valuationdata
          move-corresponding _valuationdata to valuationdata.
          move: wa_data-werks to valuationdata-val_area,
                'S'           to valuationdata-price_ctrl,  " IHDK900305
                '1'           to valuationdata-price_unit,  " IHDK900305
*                wa_data-verpr to valuationdata-moving_pr,  " IHDK900305
                '1'           to valuationdata-std_price,   " IHDK900305
                ls_pctr_mast-kosgr to valuationdata-overhead_grp. " IHDK900305
          clear ls_pctr_mast. " IHDK900305
          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill salesdata
          move-corresponding _salesdata to salesdata.
          move: lo_ext_helper->wa_mat-target_bukrs to salesdata-sales_org.

          " IHDK901148
          if salesdata-acct_assgt is initial.
            wa_log-log = |Mandatory field account assignment group is missing|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill salesdatax
          fill_bapix(
          exporting
            bapi = salesdata
          changing
            bapix = salesdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Commented - IRDK930708
          " Fill taxclassifications
*          MOVE-CORRESPONDING _taxclassifications TO taxclassifications.
*          LOOP AT taxclassifications INTO wa_taxclass. " WHERE taxclass_1 IS INITIAL.  " Rev 01
*            MOVE '0' TO wa_taxclass-taxclass_1.
*            MOVE '0' TO wa_taxclass-tax_ind.   " Rev 01
*            MODIFY taxclassifications FROM wa_taxclass TRANSPORTING taxclass_1 taxind.
*            CLEAR: wa_taxclass.
*          ENDLOOP.

          " Re-maintain tax classification for extension from scratch(as in case of creation) rather than depending on tax classification ...
          " ...of source plant since it could have certain data missing(due to post GST requirements)
          " Hence, above code commented and below code added
          get_tax_classification(
            exporting
              salesdata = salesdata
            changing
              taxclassifications = taxclassifications ).

          if taxclassifications is not initial.
            loop at taxclassifications into wa_taxclass.
              clear wa_taxclass-tax_ind.  " Tax indicator not to be maintained for *FG
              modify taxclassifications from wa_taxclass transporting tax_ind.
              clear wa_taxclass.
            endloop.
          endif.
          " End IRDK930708

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = headdata
              clientdata           = clientdata
              clientdatax          = clientdatax
              plantdata            = plantdata
              plantdatax           = plantdatax
              storagelocationdata  = storagelocationdata
              storagelocationdatax = storagelocationdatax
              valuationdata        = valuationdata
              valuationdatax       = valuationdatax
              salesdata            = salesdata
              salesdatax           = salesdatax
            importing
              return               = return
            tables
              materialdescription  = materialdescription
              unitsofmeasure       = unitsofmeasure
              unitsofmeasurex      = unitsofmeasurex
              taxclassifications   = taxclassifications
              returnmessages       = returnmessages.
          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_retmessage.  " IHDK904473

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              " Inspection Setup not to be maintained for depot

              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " Rev 05

              " Rev 14
              " dispo and disls is not part of input file, this logic prevents empty input to below method in case it is not copied from source plant
              if plantdata-mrp_ctrler is initial.
                case clientdata-division.
                  when '10'.
                    plantdata-mrp_ctrler = '010'.
                  when '15'.
                    plantdata-mrp_ctrler = '011'.
                  when '20'.
                    plantdata-mrp_ctrler = 'IL5'.
                  when '28'.
                    plantdata-mrp_ctrler = 'IL5'.
                  when others.
                endcase.
              endif.

              if plantdata-lotsizekey is initial.
                plantdata-lotsizekey = 'EX'.
              endif.

              maintain_mrp_area(
                exporting
                  material    = headdata-material
                  plant       = plantdata-plant
                  mrp_ctrler  = plantdata-mrp_ctrler
                  mrp_ls_key  = plantdata-lotsizekey
                importing
                  mrp_status  = data(mrp_status) ).

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant/depot' && ` ` && wa_data-werks.
              " Inspection Setup not to be maintained for depot

              " Rev 14
              " check status of mrp area maintainance
              if mrp_status eq abap_false.
                wa_log-log = wa_log-log && '. MRP Area: Failed'.
              endif.

              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division  " IHDK900685
                importing
                  ext_status = data(lv_ext_status) ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_ztrd_create implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_crt_helper = new lcl_create_helper( ).
    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
    changing
      answer = me->answer ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        me->material_create_extend( ).  " Rename - IHDK900652
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZTRD_Create_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_crt_helper is bound.
    lo_crt_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    data: lv_insp_perform type flag.  " flg indicating whether to maintain insp setup
    clear lv_insp_perform.

    check it_data is not initial.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear headdata.

      move 'C'            to headdata-ind_sector.    " MBRSH
      move wa_data-mtart  to headdata-matl_type.
      move abap_true      to headdata-basic_view.
      move abap_true      to headdata-sales_view.
      move abap_true      to headdata-purchase_view.
      move abap_true      to headdata-mrp_view.
*     MOVE abap_true      TO headdata-work_sched_view. " Excluded for ZTRD
      move abap_true      to headdata-storage_view.
      move abap_true      to headdata-quality_view.
      move abap_true      to headdata-account_view.
      move abap_true      to headdata-cost_view.
      move 'I'            to headdata-inp_fld_check.

      if headdata-matl_type ne 'ZTRD'.  " IHDK900652
        wa_log-log = |Incorrect material type { headdata-matl_type }|.
        append wa_log to it_log.
        continue.
      endif.

      clear clientdata.
      move wa_data-meins to clientdata-base_uom.
      move wa_data-matkl to clientdata-matl_group.
      move wa_data-bismt to clientdata-old_mat_no.
      move wa_data-spart to clientdata-division.
      move 'NORM'        to clientdata-item_cat.    " MTPOS_MARA
      move wa_data-ntgew to clientdata-net_weight.
      move wa_data-gewei to clientdata-unit_of_wt.
      move '0001'        to clientdata-trans_grp.   " TRAGR
      move wa_data-bstme to clientdata-po_unit.
      move wa_data-vabme to clientdata-var_ord_un.
      move wa_data-ekwsl to clientdata-pur_valkey.
      move wa_data-mhdrz to clientdata-minremlife.
      move wa_data-mhdhb to clientdata-shelf_life.
      move wa_data-prodh to clientdata-prod_hier.   " PRODH_H
      move abap_true     to clientdata-batch_mgmt.  " XCHPF
      move abap_true     to clientdata-qm_procmnt.

      fill_bapix(
      exporting
        bapi = clientdata
      changing
        bapix = clientdatax ).

      clear plantdata.
      move wa_data-werks to plantdata-plant.
      move 'Z001'        to plantdata-loadinggrp.   " LADGR
      move 'IN'          to plantdata-countryori.   " HERKL
      move wa_data-herkr to plantdata-regionorig.
      move wa_data-steuc to plantdata-ctrl_code.
      move wa_data-ekgrp to plantdata-pur_group.
      move wa_data-kordb to plantdata-sourcelist.
      move wa_data-dismm to plantdata-mrp_type.
      move wa_data-dispo to plantdata-mrp_ctrler.
      move wa_data-minbe to plantdata-reorder_pt.
      move wa_data-disls to plantdata-lotsizekey.
      move wa_data-bstmi to plantdata-minlotsize.
      move wa_data-bstma to plantdata-maxlotsize.
      move wa_data-bstfe to plantdata-fixed_lot.
      move wa_data-eisbe to plantdata-safety_stk.
      move wa_data-beskz to plantdata-proc_type.
      if wa_data-werks+0(2) eq '11' or wa_data-werks+0(2) eq '12'.
        move '3'           to plantdata-batchentry.   " KZECH
      endif.
      move wa_data-lgort to plantdata-sloc_exprc.   " Default value in ZTRD for lgfsb = lgort
      if wa_data-werks+0(2) eq '11' or wa_data-werks+0(2) eq '12'.
        move '1'           to plantdata-backflush.    " MARC-RGEKZ, PLANTDATA-BACKFLUSH with DType RGEKM
      endif.
      move wa_data-plifz to plantdata-plnd_delry.
      move wa_data-webaz to plantdata-gr_pr_time.
      move wa_data-fhori to plantdata-sm_key.
      move wa_data-strgr to plantdata-plan_strgp.
      move wa_data-mtvfp to plantdata-availcheck.
      move wa_data-sbdkz to plantdata-dep_req_id.
      " IHDK900305
      read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
      if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
        wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
        append wa_log to it_log.
        continue.
      endif.
      move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
      plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
      move wa_data-awsls to plantdata-variance_key.
      move wa_data-sobsk to plantdata-specprocty.     " MARC-SOBSK, PLANTDATA-SPECPROCTY with Dtype CK_SOBSL
      move '1000'        to plantdata-lot_size.       " LOSGR
      move wa_data-ssqss to plantdata-ctrl_key.       " MARC-SSQSS, PLANTDATA-CTRL_KEY with Dtype QSSPUR
      move wa_data-qzgtp to plantdata-cert_type.      " MARC-QZGTP, PLANTDATA-CERT_TYPEY with Dtype QZGTYP

      fill_bapix(
      exporting
        bapi = plantdata
      changing
        bapix = plantdatax ).

      clear storagelocationdata.
      move plantdata-plant to storagelocationdata-plant.
      move wa_data-lgort   to storagelocationdata-stge_loc.

      fill_bapix(
      exporting
        bapi = storagelocationdata
      changing
        bapix = storagelocationdatax ).

      clear valuationdata.
      move plantdata-plant  to valuationdata-val_area.  " ? include as it is mandatory
      move wa_data-bklas    to valuationdata-val_class.
      move wa_data-vprsv    to valuationdata-price_ctrl.
      move wa_data-peinh    to valuationdata-price_unit.
      move wa_data-verpr    to valuationdata-moving_pr.
      move wa_data-hrkft    to valuationdata-orig_group.

      fill_bapix(
      exporting
        bapi = valuationdata
      changing
        bapix = valuationdatax ).

      clear salesdata.
      move wa_data-vkorg to salesdata-sales_org.
      move wa_data-vtweg to salesdata-distr_chan.
      move wa_data-vrkme to salesdata-sales_unit.
      move wa_data-dwerk to salesdata-delyg_plnt.
      move wa_data-sktof to salesdata-cash_disc.
      move wa_data-versg to salesdata-matl_stats.   " MVKE-VERSG, SALESDATA-MATL_STATS with Dtype STGMA
      move wa_data-bonus to salesdata-rebate_grp.
      move wa_data-kondm to salesdata-mat_pr_grp.

      " IHDK901148
      if wa_data-ktgrm is initial.
        wa_log-log = |Account assignment group is mandatory|.
        append wa_log to it_log.
        continue.
      else.
        move wa_data-ktgrm to salesdata-acct_assgt.
      endif.
      move 'NORM'        to salesdata-item_cat.     " MTPOS
      move wa_data-mvgr1 to salesdata-matl_grp_1.
      move wa_data-mvgr2 to salesdata-matl_grp_2.
      move wa_data-mvgr3 to salesdata-matl_grp_3.
      move wa_data-mvgr4 to salesdata-matl_grp_4.
      move wa_data-mvgr5 to salesdata-matl_grp_5.
      move wa_data-aumng to salesdata-min_order.    " MVKE-AUMNG, SALESDATA-MIN_ORDER with Dtype MINAU

      fill_bapix(
      exporting
        bapi = salesdata
      changing
        bapix = salesdatax ).

      refresh materialdescription.
      clear wa_matdesc.
      move sy-langu       to wa_matdesc-langu.
      move wa_data-maktx  to wa_matdesc-matl_desc.
      append wa_matdesc to materialdescription.

      refresh: unitsofmeasure, unitsofmeasurex.
      clear: wa_uom, wa_uomx.
      move clientdata-base_uom    to wa_uom-alt_unit.
      move wa_data-brgew          to wa_uom-gross_wt.
      fill_bapix(
      exporting
        bapi = wa_uom
      changing
        bapix = wa_uomx ).
      append: wa_uom  to unitsofmeasure,
      wa_uomx to unitsofmeasurex.

      " IRDK930708
      get_tax_classification(
        exporting
          salesdata = salesdata
        changing
          taxclassifications = taxclassifications ).
      " End IRDK930708

      " IHDK904473
      clear: mat_no_return, ms_err_log_db.

      if wa_data-matnr is initial.  " To do else => log as error, material to be created cannot be supplied externally
        headdata-material = material_number_get(
                              exporting
                                material_type = wa_data-mtart
                              importing
                                return = mat_no_return ).

        if headdata-material is initial or mat_no_return-type <> 'S'.
          wa_log-log = mat_no_return-message.
          append wa_log to it_log.
          continue.
        endif.
      else.
        continue.
      endif.

      " Note: For Extensionin refer https://gist.github.com/saurabhk-nbssap/0ca3873d400e1581f7a91ffa3c219b72

      " Execute BAPI
      clear: return, wa_retmessage.
      refresh: returnmessages.
      call function 'BAPI_MATERIAL_SAVEDATA'
        exporting
          headdata             = headdata
          clientdata           = clientdata
          clientdatax          = clientdatax
          plantdata            = plantdata
          plantdatax           = plantdatax
          storagelocationdata  = storagelocationdata
          storagelocationdatax = storagelocationdatax
          valuationdata        = valuationdata
          valuationdatax       = valuationdatax
          salesdata            = salesdata
          salesdatax           = salesdatax
        importing
          return               = return
        tables
          materialdescription  = materialdescription
          unitsofmeasure       = unitsofmeasure
          unitsofmeasurex      = unitsofmeasurex
          taxclassifications   = taxclassifications
          returnmessages       = returnmessages.

      read table returnmessages into wa_retmessage with key type = 'E' transporting message.
      if sy-subrc = 0.  " Error exists, log it in error log
        wa_log-log = wa_retmessage-message.
        append wa_log to it_log.
        clear: wa_retmessage.  " IHDK904473

        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else. " No error
        if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

          maint_material_classif(
            exporting
              material = headdata-material
              class_num = 'ZSFG_FG'
              class_type = '023'  " IHDK901148
            changing
              classif_status = classif_status ).

          " For ZTRD, Only perform inspection setup for plant of series 13..
          clear: lv_insp_perform.
          if plantdata-plant+0(2) eq '13' or plantdata-plant+0(2) eq '28'.
            move abap_true to lv_insp_perform.
          endif.

          if lv_insp_perform eq abap_true.
            " Build insp-type select options
            refresh: s_art.
            clear s_art_wa.
            s_art_wa-sign   = 'I'.
            s_art_wa-option = 'EQ'.
            s_art_wa-low    = '01'.
            append s_art_wa to s_art.

            maint_material_insp_setup(
            exporting
              material = headdata-material
              plant    = plantdata-plant
              s_art    = s_art
            changing
              insp_status = insp_status ).
          endif.

          " Rev 14
          maintain_mrp_area(
            exporting
              material    = headdata-material
              plant       = plantdata-plant
              mrp_ctrler  = plantdata-mrp_ctrler
              mrp_ls_key  = plantdata-lotsizekey
            importing
              mrp_status  = data(mrp_status) ).

          wa_log-matnr = headdata-material.
          wa_log-log = return-message.
          " Check status of classification
          if classif_status ne 1.
            wa_log-log = wa_log-log && '. Classification data: Failed.'.
          endif.

          " Check status of inspection setup
          if insp_status ne 'X' and lv_insp_perform eq abap_true.
            wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
          endif.

          " Rev 14
          " check status of mrp area maintainance
          if mrp_status eq abap_false.
            wa_log-log = wa_log-log && '. MRP Area: Failed'.
          endif.

          extend_to_stor_loc( " IHDK900652
            exporting
              material   = headdata-material
              plant      = plantdata-plant
              matl_type  = headdata-matl_type
              division   = clientdata-division  " IHDK900685
            importing
              ext_status = data(lv_ext_status) ).

          if lv_ext_status eq abap_false.
            wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
          endif.

          append wa_log to it_log.
          clear: lv_ext_status. " IHDK904473
        endif.  " IF return-number = '356'
      endif.
      " End read returnmessages
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_ztrd_ext_plant implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
                          changing it_data = it_data
                                   it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZTRD_Plant_Extend_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    data: lv_insp_perform type flag.  " flg indicating whether to maintain insp setup
    clear lv_insp_perform.

    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
                                                                   target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, salesdata, _return_wa.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        " For sales org data, pass the source company code as the ref sales org
        " Change the sales org to the target comp code, copy rest of the sales data as it is
        " if the source cc = target cc, it will be just like copying the sales data without any change
        " if the source cc <> target cc, the material will be extended to the target sales org using the data of the source sales org

        " Get distribution channel
        clear lo_ext_helper->v_dist_ch.
        select min( vtweg )
          from mvke
          into lo_ext_helper->v_dist_ch
          where matnr eq lo_ext_helper->wa_mat-matnr
          and   vkorg eq lo_ext_helper->wa_mat-source_bukrs.

        if lo_ext_helper->v_dist_ch is initial.
          shift: wa_data-matnr left deleting leading '0',
                 lo_ext_helper->wa_mat-source_bukrs left deleting leading '0',
                 lo_ext_helper->wa_mat-source_werks left deleting leading '0'.
          wa_log-log = 'No reference sales data found: Ref Comp\Sales Org:' && ` ` && lo_ext_helper->wa_mat-source_bukrs
                && ` ` && 'Ref. plant:' && ` ` && lo_ext_helper->wa_mat-source_werks.
          append wa_log to it_log.
          clear: wa_log.
          continue.
        endif.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
            salesorganisation   = lo_ext_helper->wa_mat-source_bukrs  " sales org is maintained same as comp code for that comp code in masters
            distributionchannel = lo_ext_helper->v_dist_ch
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
            salesdata           = _salesdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial or _valuationdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, storagelocationdata, valuationdata,
                 plantdatax, storagelocationdatax, valuationdatax.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
            exporting
              pstat = _clientdata-maint_stat
            changing
              headdata = headdata ).

          if headdata-matl_type ne 'ZTRD'.  " IHDK900652
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          move abap_true to clientdata-qm_procmnt.  " Rev 03
          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-dispo to plantdata-mrp_ctrler.
          move wa_data-minbe to plantdata-reorder_pt.
          move wa_data-disls to plantdata-lotsizekey.
          move wa_data-bstmi to plantdata-minlotsize.
          move wa_data-bstma to plantdata-maxlotsize.
          move wa_data-bstfe to plantdata-fixed_lot.
          move wa_data-eisbe to plantdata-safety_stk.
          move wa_data-plifz to plantdata-plnd_delry.
          move wa_data-lgort to plantdata-sloc_exprc. " Fill lgfsb with default value = lgort from file
          move wa_data-mtvfp to plantdata-availcheck.
          " IHDK900305
          read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
          if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
            wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
            append wa_log to it_log.
            continue.
          endif.
          move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
          plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
          move wa_data-webaz to plantdata-gr_pr_time.
          move wa_data-lgpro to plantdata-iss_st_loc. " Only diff between depot and plant ext for ZTRD
**** ---- Additional Requirement ----- ***
*          " Specific fields not to be copied are cleared here
**** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          " Fill storagelocationdata
          " Note storage location data is not fetched using getall bapi, only the fields plant, stge_loc need to be passed as provided in file
          move wa_data-werks to storagelocationdata-plant.
          move wa_data-lgort to storagelocationdata-stge_loc.
          " Fill storagelocationdatax
          fill_bapix(
          exporting
            bapi = storagelocationdata
          changing
            bapix = storagelocationdatax ).

          " Fill valuationdata
          move-corresponding _valuationdata to valuationdata.
          move: wa_data-werks to valuationdata-val_area,
                wa_data-verpr to valuationdata-moving_pr.
**** ---- Additional Requirement ----- ***
*          " Specific fields not to be copied are cleared here
          clear valuationdata-overhead_grp.
**** ---- End Additional Requirement ---- ***
          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill salesdata
          move-corresponding _salesdata to salesdata.
          move: lo_ext_helper->wa_mat-target_bukrs to salesdata-sales_org.

          " IHDK901148
          if salesdata-acct_assgt is initial.
            wa_log-log = |Mandatory field account assignment group is missing|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill salesdatax
          fill_bapix(
          exporting
            bapi = salesdata
          changing
            bapix = salesdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Commented - IRDK930708
          " Fill taxclassifications
*          MOVE-CORRESPONDING _taxclassifications TO taxclassifications.
*          LOOP AT taxclassifications INTO wa_taxclass. " WHERE taxclass_1 IS INITIAL.  " Rev 01
*            MOVE '0' TO wa_taxclass-taxclass_1.
*            MOVE '0' TO wa_taxclass-tax_ind.   " Rev 01
*            MODIFY taxclassifications FROM wa_taxclass TRANSPORTING taxclass_1 taxind.
*            CLEAR: wa_taxclass.
*          ENDLOOP.

          " Re-maintain tax classification for extension from scratch(as in case of creation) rather than depending on tax classification ...
          " ...of source plant since it could have certain data missing(due to post GST requirements)
          " Hence, above code commented and below code added
          get_tax_classification(
            exporting
              salesdata = salesdata
            changing
              taxclassifications = taxclassifications ).
          " End IRDK930708

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = headdata
              clientdata           = clientdata
              clientdatax          = clientdatax
              plantdata            = plantdata
              plantdatax           = plantdatax
              storagelocationdata  = storagelocationdata
              storagelocationdatax = storagelocationdatax
              valuationdata        = valuationdata
              valuationdatax       = valuationdatax
              salesdata            = salesdata
              salesdatax           = salesdatax
            importing
              return               = return
            tables
              materialdescription  = materialdescription
              unitsofmeasure       = unitsofmeasure
              unitsofmeasurex      = unitsofmeasurex
              taxclassifications   = taxclassifications
              returnmessages       = returnmessages.

          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_retmessage.  " IHDK904473

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

              " For ZTRD, Only perform inspection setup for plant of series 13..
              clear: lv_insp_perform.
              if plantdata-plant+0(2) eq '13' or plantdata-plant+0(2) eq '28'.
                move abap_true to lv_insp_perform.
              endif.

              if lv_insp_perform eq abap_true.
                " Build insp-type select options
                refresh: s_art.
                clear s_art_wa.
                s_art_wa-sign   = 'I'.
                s_art_wa-option = 'EQ'.
                s_art_wa-low    = '01'.
                append s_art_wa to s_art.

                maint_material_insp_setup(
                  exporting
                    material = headdata-material
                    plant    = plantdata-plant
                    s_art    = s_art
                  changing
                    insp_status = insp_status ).
              endif.

              " Rev 14
              maintain_mrp_area(
                exporting
                  material    = headdata-material
                  plant       = plantdata-plant
                  mrp_ctrler  = plantdata-mrp_ctrler
                  mrp_ls_key  = plantdata-lotsizekey
                importing
                  mrp_status  = data(mrp_status) ).

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant/depot' && ` ` && wa_data-werks.
              " Check status of inspection setup
              if insp_status ne 'X' and lv_insp_perform eq abap_true.
                wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
              endif.

              " Rev 14
              " check status of mrp area maintainance
              if mrp_status eq abap_false.
                wa_log-log = wa_log-log && '. MRP Area: Failed'.
              endif.

              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division  " IHDK900685
                importing
                  ext_status = data(lv_ext_status) ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_ztrd_ext_depot implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
                          changing it_data = it_data
                                   it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " End IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZTRD_Depot_Extend_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    data: lv_insp_perform type flag.  " flg indicating whether to maintain insp setup
    clear lv_insp_perform.

    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
                                                                   target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, salesdata, _return_wa.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        " For sales org data, pass the source company code as the ref sales org
        " Change the sales org to the target comp code, copy rest of the sales data as it is
        " if the source cc = target cc, it will be just like copying the sales data without any change
        " if the source cc <> target cc, the material will be extended to the target sales org using the data of the source sales org

        " Get distribution channel
        clear lo_ext_helper->v_dist_ch.
        select min( vtweg )
          from mvke
          into lo_ext_helper->v_dist_ch
          where matnr eq lo_ext_helper->wa_mat-matnr
          and   vkorg eq lo_ext_helper->wa_mat-source_bukrs.

        if lo_ext_helper->v_dist_ch is initial.
          shift: wa_data-matnr left deleting leading '0',
                 lo_ext_helper->wa_mat-source_bukrs left deleting leading '0',
                 lo_ext_helper->wa_mat-source_werks left deleting leading '0'.
          wa_log-log = 'No reference sales data found: Ref Comp\Sales Org:' && ` ` && lo_ext_helper->wa_mat-source_bukrs
                && ` ` && 'Ref. plant:' && ` ` && lo_ext_helper->wa_mat-source_werks.
          append wa_log to it_log.
          clear: wa_log.
          continue.
        endif.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
            salesorganisation   = lo_ext_helper->wa_mat-source_bukrs  " sales org is maintained same as comp code for that comp code in masters
            distributionchannel = lo_ext_helper->v_dist_ch
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
            salesdata           = _salesdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial or _valuationdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, storagelocationdata, valuationdata,
                 plantdatax, storagelocationdatax, valuationdatax.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
            exporting
              pstat = _clientdata-maint_stat
            changing
              headdata = headdata ).

          if headdata-matl_type ne 'ZTRD'.  " IHDK900652
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-dispo to plantdata-mrp_ctrler.
          move wa_data-minbe to plantdata-reorder_pt.
          move wa_data-disls to plantdata-lotsizekey.
          move wa_data-bstmi to plantdata-minlotsize.
          move wa_data-bstma to plantdata-maxlotsize.
          move wa_data-bstfe to plantdata-fixed_lot.
          move wa_data-eisbe to plantdata-safety_stk.
          move wa_data-plifz to plantdata-plnd_delry.
          move wa_data-lgort to plantdata-sloc_exprc. " Fill lgfsb with default value = lgort from file
          move wa_data-mtvfp to plantdata-availcheck.
          " IHDK900305
          read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
          if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
            wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
            append wa_log to it_log.
            continue.
          endif.
          move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
          plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
          move wa_data-webaz to plantdata-gr_pr_time.
**** ---- Additional Requirement ----- ***
*          " Specific fields not to be copied are cleared here
          clear: plantdata-iss_st_loc.
**** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          " Fill storagelocationdata
          " Note storage location data is not fetched using getall bapi, only the fields plant, stge_loc need to be passed as provided in file
          move wa_data-werks to storagelocationdata-plant.
          move wa_data-lgort to storagelocationdata-stge_loc.
          " Fill storagelocationdatax
          fill_bapix(
          exporting
            bapi = storagelocationdata
          changing
            bapix = storagelocationdatax ).

          " Fill valuationdata
          move-corresponding _valuationdata to valuationdata.
          move: wa_data-werks to valuationdata-val_area,
                wa_data-verpr to valuationdata-moving_pr.
**** ---- Additional Requirement ----- ***
*          " Specific fields not to be copied are cleared here
          clear valuationdata-overhead_grp.
**** ---- End Additional Requirement ---- ***
          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill salesdata
          move-corresponding _salesdata to salesdata.
          move: lo_ext_helper->wa_mat-target_bukrs to salesdata-sales_org.

          " IHDK901148
          if salesdata-acct_assgt is initial.
            wa_log-log = |Mandatory field account assignment group is missing|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill salesdatax
          fill_bapix(
          exporting
            bapi = salesdata
          changing
            bapix = salesdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Commented - IRDK930708
          " Fill taxclassifications
*          MOVE-CORRESPONDING _taxclassifications TO taxclassifications.
*          LOOP AT taxclassifications INTO wa_taxclass. " WHERE taxclass_1 IS INITIAL.  " Rev 01
*            MOVE '0' TO wa_taxclass-taxclass_1.
*            MOVE '0' TO wa_taxclass-tax_ind.     " Rev 01
*            MODIFY taxclassifications FROM wa_taxclass TRANSPORTING taxclass_1 taxind.
*            CLEAR: wa_taxclass.
*          ENDLOOP.

          " Re-maintain tax classification for extension from scratch(as in case of creation) rather than depending on tax classification ...
          " ...of source plant since it could have certain data missing(due to post GST requirements)
          " Hence, above code commented and below code added
          get_tax_classification(
            exporting
              salesdata = salesdata
            changing
              taxclassifications = taxclassifications ).
          " End IRDK930708

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = headdata
              clientdata           = clientdata
              clientdatax          = clientdatax
              plantdata            = plantdata
              plantdatax           = plantdatax
              storagelocationdata  = storagelocationdata
              storagelocationdatax = storagelocationdatax
              valuationdata        = valuationdata
              valuationdatax       = valuationdatax
              salesdata            = salesdata
              salesdatax           = salesdatax
            importing
              return               = return
            tables
              materialdescription  = materialdescription
              unitsofmeasure       = unitsofmeasure
              unitsofmeasurex      = unitsofmeasurex
              taxclassifications   = taxclassifications
              returnmessages       = returnmessages.
          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_retmessage.  " IHDK904473

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

              " For ZTRD, Only perform inspection setup for plant of series 13..
              clear: lv_insp_perform.
              if plantdata-plant+0(2) eq '13' or plantdata-plant+0(2) eq '28'.
                move abap_true to lv_insp_perform.
              endif.

              if lv_insp_perform eq abap_true.
                " Build insp-type select options
                refresh: s_art.
                clear s_art_wa.
                s_art_wa-sign   = 'I'.
                s_art_wa-option = 'EQ'.
                s_art_wa-low    = '01'.
                append s_art_wa to s_art.

                maint_material_insp_setup(
                  exporting
                    material = headdata-material
                    plant    = plantdata-plant
                    s_art    = s_art
                  changing
                    insp_status = insp_status ).
              endif.

              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " Rev 05

              " Rev 14
              maintain_mrp_area(
                exporting
                  material    = headdata-material
                  plant       = plantdata-plant
                  mrp_ctrler  = plantdata-mrp_ctrler
                  mrp_ls_key  = plantdata-lotsizekey
                importing
                  mrp_status  = data(mrp_status) ).

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant/depot' && ` ` && wa_data-werks.
              " Check status of inspection setup
              if insp_status ne 'X' and lv_insp_perform eq abap_true.
                wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
              endif.

              " Rev 14
              " check status of mrp area maintainance
              if mrp_status eq abap_false.
                wa_log-log = wa_log-log && '. MRP Area: Failed'.
              endif.

              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division  " IHDK900685
                importing
                  ext_status = data(lv_ext_status) ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zraw_zpkg_zpku_create implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_crt_helper = new lcl_create_helper( ).
    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
    changing
      answer = me->answer ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        me->material_create_extend( ).  " Rename - IHDK900652
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZRAW_ZPKG_ZPKU_Create_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_crt_helper is bound.
    lo_crt_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    data: lv_insp_perform type flag,  " flg indicating whether to maintain insp setup
          lv_ext_status.              " flg indicating whether extension to valuation class was successful
    clear lv_insp_perform.

    check it_data is not initial.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear headdata.

      move 'C'            to headdata-ind_sector.    " MBRSH
      move wa_data-mtart  to headdata-matl_type.
      move abap_true      to headdata-basic_view.
      move abap_true      to headdata-sales_view.
      move abap_true      to headdata-purchase_view.
      move abap_true      to headdata-mrp_view.
*     MOVE abap_true      TO headdata-work_sched_view. " Excluded for ZTRD
      move abap_true      to headdata-storage_view.
      move abap_true      to headdata-quality_view.
      move abap_true      to headdata-account_view.
      move abap_true      to headdata-cost_view.
      move 'I'            to headdata-inp_fld_check.

      if headdata-matl_type ne 'ZRAW' and headdata-matl_type ne 'ZPKG' and headdata-matl_type ne 'ZPKU'.  " IHDK900652
        wa_log-log = |Incorrect material type { headdata-matl_type }|.
        append wa_log to it_log.
        continue.
      endif.

      clear clientdata.
      move wa_data-meins to clientdata-base_uom.
      move wa_data-matkl to clientdata-matl_group.
      move wa_data-spart to clientdata-division.
      move 'NORM'        to clientdata-item_cat.    " MTPOS_MARA
      move '0001'        to clientdata-trans_grp.   " TRAGR
      move wa_data-bstme to clientdata-po_unit.
      move wa_data-vabme to clientdata-var_ord_un.
      move wa_data-ekwsl to clientdata-pur_valkey.
      move wa_data-mhdrz to clientdata-minremlife.
      move wa_data-mhdhb to clientdata-shelf_life.
      move wa_data-xchpf to clientdata-batch_mgmt.  " XCHPF
      move wa_data-qmpur to clientdata-qm_procmnt.
      move 'KG'          to clientdata-unit_of_wt.

      fill_bapix(
      exporting
        bapi = clientdata
      changing
        bapix = clientdatax ).

      clear plantdata.
      move wa_data-werks to plantdata-plant.
      move 'Z001'        to plantdata-loadinggrp.   " LADGR
      move 'IN'          to plantdata-countryori.   " HERKL
      move wa_data-steuc to plantdata-ctrl_code.
      move wa_data-ekgrp to plantdata-pur_group.
      move wa_data-dismm to plantdata-mrp_type.
      move wa_data-dispo to plantdata-mrp_ctrler.
      move wa_data-minbe to plantdata-reorder_pt.
      move wa_data-disls to plantdata-lotsizekey.
      move wa_data-bstmi to plantdata-minlotsize.
      move wa_data-bstma to plantdata-maxlotsize.
      move wa_data-bstfe to plantdata-fixed_lot.
      move wa_data-eisbe to plantdata-safety_stk.
      move wa_data-beskz to plantdata-proc_type.
      if wa_data-xchpf eq abap_true.
        move '3'           to plantdata-batchentry.   " KZECH
      else.
        move 'IC01'   to plantdata-determ_grp.  " Stock determination group
      endif.
      move wa_data-lgpro to plantdata-iss_st_loc.
      move wa_data-lgort to plantdata-sloc_exprc.   " Default value in ZTRD for lgfsb = lgort
      if wa_data-werks+0(2) ne '13' and wa_data-werks+0(2) ne '14'.
        move '1'           to plantdata-backflush.    " MARC-RGEKZ, PLANTDATA-BACKFLUSH with DType RGEKM
      endif.
      move wa_data-plifz to plantdata-plnd_delry.
      move wa_data-webaz to plantdata-gr_pr_time.
      move wa_data-fhori to plantdata-sm_key.
      move wa_data-mtvfp to plantdata-availcheck.
      " IHDK900305
      read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
      if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
        wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
        append wa_log to it_log.
        continue.
      endif.
      move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
      plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
      move wa_data-sobsk to plantdata-specprocty.     " MARC-SOBSK, PLANTDATA-SPECPROCTY with Dtype CK_SOBSL
      move wa_data-losgr to plantdata-lot_size.       " LOSGR
      move wa_data-ssqss to plantdata-ctrl_key.       " MARC-SSQSS, PLANTDATA-CTRL_KEY with Dtype QSSPUR
      move wa_data-qzgtp to plantdata-cert_type.      " MARC-QZGTP, PLANTDATA-CERT_TYPEY with Dtype QZGTYP
      move wa_data-prfrq to plantdata-insp_int.

      fill_bapix(
      exporting
        bapi = plantdata
      changing
        bapix = plantdatax ).

      clear storagelocationdata.
      move plantdata-plant to storagelocationdata-plant.
      move wa_data-lgort   to storagelocationdata-stge_loc.

      fill_bapix(
      exporting
        bapi = storagelocationdata
      changing
        bapix = storagelocationdatax ).

      clear valuationdata.
      move plantdata-plant  to valuationdata-val_area.  " ? include as it is mandatory
      if wa_data-mtart eq 'ZRAW'.
        move '1000' to valuationdata-val_class.
        move 'H' to valuationdata-val_cat.
      elseif wa_data-mtart eq 'ZPKG' or wa_data-mtart eq 'ZPKU'. " IHDK902087
        move '1500' to valuationdata-val_class.
        move 'P' to valuationdata-val_cat.
      endif.
      move wa_data-vprsv    to valuationdata-price_ctrl.
      move wa_data-peinh    to valuationdata-price_unit.
      if wa_data-verpr is initial.  " IHDK903652
        wa_log-log = |Moving average price is mandatory|.
        append wa_log to it_log.
        continue.
      endif.
      move wa_data-verpr    to valuationdata-moving_pr.
      move wa_data-hrkft    to valuationdata-orig_group.
      " IRDK931550
      move abap_true        to valuationdata-qty_struct.     " MBEW-EKALR, VALUATIONDATA-QTY_STRUCT with Dtype CK_EKALREL
      move abap_true        to valuationdata-orig_mat.       " HKMAT

      " IHDK902087
      if wa_data-bwtar_dom is initial and wa_data-bwtar_imp is initial.
        wa_log-log = |Atleast one valuation type must be supplied - DOM/IMP|.
        append wa_log to it_log.
        continue.
      else.
        if wa_data-bwtar_dom is not initial and wa_data-bwtar_dom ns 'DOM'.
          wa_log-log = |Domestic valuation type: Invalid value|.
          append wa_log to it_log.
          continue.
        endif.
        if wa_data-bwtar_imp is not initial and wa_data-bwtar_imp ns 'IMP'.
          wa_log-log = |Import valuation type: Invalid value|.
          append wa_log to it_log.
          continue.
        endif.
      endif.
      " End IHDK902087

      fill_bapix(
      exporting
        bapi = valuationdata
      changing
        bapix = valuationdatax ).

      clear salesdata.
      select single bukrs from t001k into salesdata-sales_org where bwkey eq wa_data-werks.
      move '10' to salesdata-distr_chan.
      move 'NORM' to salesdata-item_cat.
      " IHDK906383
      if wa_data-ktgrm is initial.
        wa_log-log = |Account assignment group is mandatory|.
        append wa_log to it_log.
        continue.
      else.
        move wa_data-ktgrm to salesdata-acct_assgt.
      endif.

      fill_bapix(
      exporting
        bapi = salesdata
      changing
        bapix = salesdatax ).

      refresh materialdescription.
      clear wa_matdesc.
      move sy-langu       to wa_matdesc-langu.
      move wa_data-maktx  to wa_matdesc-matl_desc.
      append wa_matdesc to materialdescription.

      if ( wa_data-meins ne wa_data-bstme ) and wa_data-umrez is not initial.
        refresh: unitsofmeasure, unitsofmeasurex.
        clear: wa_uom, wa_uomx.
        move clientdata-po_unit to wa_uom-alt_unit.
        move wa_data-umrez      to wa_uom-numerator.

        fill_bapix(
        exporting
          bapi = wa_uom
        changing
          bapix = wa_uomx ).
        append: wa_uom  to unitsofmeasure,
        wa_uomx to unitsofmeasurex.
      endif.

      " IRDK930708
      get_tax_classification(
        exporting
          salesdata = salesdata
        changing
          taxclassifications = taxclassifications ).
      " End IRDK930708

      " IHDK904473
      clear: mat_no_return, ms_err_log_db.

      if wa_data-matnr is initial.  " To do else => log as error, material to be created cannot be supplied externally
        headdata-material = material_number_get(
                              exporting
                                material_type = wa_data-mtart
                              importing
                                return = mat_no_return ).

        if headdata-material is initial or mat_no_return-type <> 'S'.
          wa_log-log = mat_no_return-message.
          append wa_log to it_log.
          continue.
        endif.
      else.
        continue.
      endif.

      " Note: For Extensionin refer https://gist.github.com/saurabhk-nbssap/0ca3873d400e1581f7a91ffa3c219b72

      " Execute BAPI
      clear: return, wa_retmessage.
      refresh: returnmessages.
      call function 'BAPI_MATERIAL_SAVEDATA'
        exporting
          headdata             = headdata
          clientdata           = clientdata
          clientdatax          = clientdatax
          plantdata            = plantdata
          plantdatax           = plantdatax
          storagelocationdata  = storagelocationdata
          storagelocationdatax = storagelocationdatax
          valuationdata        = valuationdata
          valuationdatax       = valuationdatax
          salesdata            = salesdata
          salesdatax           = salesdatax
        importing
          return               = return
        tables
          materialdescription  = materialdescription
          unitsofmeasure       = unitsofmeasure
          unitsofmeasurex      = unitsofmeasurex
          taxclassifications   = taxclassifications
          returnmessages       = returnmessages.

      read table returnmessages into wa_retmessage with key type = 'E' transporting message.
      if sy-subrc = 0.  " Error exists, log it in error log
        wa_log-log = wa_retmessage-message.
        append wa_log to it_log.
        clear: wa_retmessage.  " IHDK904473

        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else. " No error
        if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

          " commented: IHDK905737
*          if wa_data-xchpf eq abap_true.
*            maint_material_classif(
*              exporting
*                material = headdata-material
*                class_num = cond #( when wa_data-mtart eq 'ZRAW' then 'ZRM' else 'ZPM' )  " IHDK900652
*                                                                                          " As per requirement from Varma Sir(Mail dated 18.9.18)
*                class_type = '023'    " IHDK901148
*          changing
*            classif_status = classif_status ).
*          endif.

          " For ZRAW, ZPKG, ZPKU, Exclude materials of plant 14XX from inspection setup
          clear: lv_insp_perform.
          if plantdata-plant+0(2) ne '14'.
            move abap_true to lv_insp_perform.
          endif.

          if lv_insp_perform eq abap_true.
            " Build insp-type select options
            if plantdata-plant+0(2) eq '13'.
              refresh: s_art.
              clear s_art_wa.
              s_art_wa-sign   = 'I'.
              s_art_wa-option = 'EQ'.
              s_art_wa-low    = '01'.
              append s_art_wa to s_art.
            else.
              refresh: s_art.
              clear s_art_wa.
              s_art_wa-sign   = 'I'.
              s_art_wa-option = 'EQ'.
              s_art_wa-low    = '01'.
              append s_art_wa to s_art.

              clear s_art_wa.
              s_art_wa-sign   = 'I'.
              s_art_wa-option = 'EQ'.
              s_art_wa-low    = '08'.
              append s_art_wa to s_art.

              clear s_art_wa.
              s_art_wa-sign   = 'I'.
              s_art_wa-option = 'EQ'.
              s_art_wa-low    = '09'.
              append s_art_wa to s_art.

              clear s_art_wa.
              s_art_wa-sign   = 'I'.
              s_art_wa-option = 'EQ'.
              s_art_wa-low    = '89'.
              append s_art_wa to s_art.
            endif.

            maint_material_insp_setup(
            exporting
              material = headdata-material
              plant    = plantdata-plant
              s_art    = s_art
            changing
              insp_status = insp_status ).
          endif.

          " Rev 14
          maintain_mrp_area(
            exporting
              material    = headdata-material
              plant       = plantdata-plant
              mrp_ctrler  = plantdata-mrp_ctrler
              mrp_ls_key  = plantdata-lotsizekey
            importing
              mrp_status  = data(mrp_status) ).

          wa_log-matnr = headdata-material.
          wa_log-log = return-message.
*          " Check status of classification; commented: IHDK905737
*          if classif_status ne 1 and wa_data-xchpf eq abap_true.
*            wa_log-log = wa_log-log && '. Classification data: Failed.'.
*          endif.
          " Check status of inspection setup
          if insp_status ne 'X' and lv_insp_perform eq abap_true.
            wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
          endif.

          clear lv_ext_status.
          translate wa_data-bwtar_dom to upper case.
          translate wa_data-bwtar_imp to upper case.
          if wa_data-bwtar_dom is not initial.
            if wa_data-bwtar_dom cp 'R*D*'.
              wait up to 2 seconds.
              extend_to_val_class(
                exporting
                  headdata      = headdata
                  valuationdata = valuationdata
                  bwtar = wa_data-bwtar_dom
                  bklas = '1000'
                importing
                  ext_status =  lv_ext_status ).
            endif.

            if wa_data-bwtar_dom cp 'P*D*'.
              wait up to 2 seconds.
              extend_to_val_class(
                exporting
                  headdata      = headdata
                  valuationdata = valuationdata
                  bwtar = wa_data-bwtar_dom
                  bklas = '1500'
                importing
                  ext_status =  lv_ext_status ).
            endif.
            if lv_ext_status is not initial.
              wa_log-bwtar_dom = wa_log-bwtar_dom && '-Yes'.
            endif.
          endif.

          if wa_data-bwtar_imp is not initial.
            if wa_data-bwtar_imp cp 'R*I*'.
              wait up to 2 seconds.
              extend_to_val_class(
                exporting
                  headdata      = headdata
                  valuationdata = valuationdata
                  bwtar = wa_data-bwtar_imp
                  bklas = '1001'
                importing
                  ext_status =  lv_ext_status ).
            endif.

            if wa_data-bwtar_imp cp 'P*I*'.
              wait up to 2 seconds.
              extend_to_val_class(
                exporting
                  headdata      = headdata
                  valuationdata = valuationdata
                  bwtar = wa_data-bwtar_imp
                  bklas = '1501'
                importing
                  ext_status =  lv_ext_status ).
            endif.
            if lv_ext_status is not initial.
              wa_log-bwtar_imp = wa_log-bwtar_imp && '-Yes'.
            endif.
          endif.

          " Rev 14
          " check status of mrp area maintainance
          if mrp_status eq abap_false.
            wa_log-log = wa_log-log && '. MRP Area: Failed'.
          endif.

          clear lv_ext_status.
          extend_to_stor_loc( " IHDK900652
            exporting
              material   = headdata-material
              plant      = plantdata-plant
              matl_type  = headdata-matl_type
              division   = clientdata-division  " IHDK900685
            importing
              ext_status = lv_ext_status ).

          if lv_ext_status eq abap_false.
            wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
          endif.

          append wa_log to it_log.
          clear: lv_ext_status. " IHDK904473
        endif.  " IF return-number = '356'
      endif.
      " End read returnmessages
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zraw_zpkg_zpku_ext_plt_dpt implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
                          changing it_data = it_data
                                   it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZRAW_ZPKG_ZPKU_Plant_Depot_Extend_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    data: lv_insp_perform type flag,  " flg indicating whether to maintain insp setup
          lv_ext_status.              " flg indicating whether extension to valuation class was successful
    clear lv_insp_perform.

    data: lv_salesorg type mvke-vkorg,
          lv_distchnl type mvke-vtweg.

    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
                                                                   target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, salesdata, _return_wa.
        clear: lv_salesorg, lv_distchnl.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        " For sales org data, pass the source company code as the ref sales org
        " Change the sales org to the target comp code, copy rest of the sales data as it is
        " if the source cc = target cc, it will be just like copying the sales data without any change
        " if the source cc <> target cc, the material will be extended to the target sales org using the data of the source sales org

        " Get distribution channel
        clear lo_ext_helper->v_dist_ch.
        select min( vtweg )
          from mvke
          into lo_ext_helper->v_dist_ch
          where matnr eq lo_ext_helper->wa_mat-matnr
          and   vkorg eq lo_ext_helper->wa_mat-source_bukrs.

        " If sales data is not maintained in initial creation
        " we still need to create the sales data at extension time
        " Set default sales org = comp code and dist. ch. = '10'
        if lo_ext_helper->v_dist_ch is initial.
          clear lv_salesorg.
          clear lv_distchnl.
        else.
          " sales org is maintained same as comp code for that comp code in masters
          move lo_ext_helper->wa_mat-source_bukrs to lv_salesorg.
          move lo_ext_helper->v_dist_ch to lv_distchnl.
        endif.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
            salesorganisation   = lv_salesorg
            distributionchannel = lv_distchnl
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
            salesdata           = _salesdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial or _valuationdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, storagelocationdata, valuationdata,
                 plantdatax, storagelocationdatax, valuationdatax.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
            exporting
              pstat = _clientdata-maint_stat
            changing
              headdata = headdata ).

          if headdata-matl_type ne 'ZRAW' and headdata-matl_type ne 'ZPKG' and headdata-matl_type ne 'ZPKU'.  " IHDK900652
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          " The following fields will be set to predefined default values if the reference plant does not have valid values for these
          if clientdata-trans_grp ne '0001'.
            move '0001'        to clientdata-trans_grp.   " TRAGR
          endif.
          if clientdata-qm_procmnt ne abap_true.
            move abap_true to clientdata-qm_procmnt.
          endif.
          if clientdata-unit_of_wt is initial.
            move 'KG' to clientdata-unit_of_wt.
          endif.

          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-dispo to plantdata-mrp_ctrler.
          move wa_data-minbe to plantdata-reorder_pt.
          move wa_data-disls to plantdata-lotsizekey.
          move wa_data-bstmi to plantdata-minlotsize.
          move wa_data-bstma to plantdata-maxlotsize.
          move wa_data-bstfe to plantdata-fixed_lot.
          move wa_data-eisbe to plantdata-safety_stk.
          move wa_data-plifz to plantdata-plnd_delry.
          move wa_data-lgort to plantdata-sloc_exprc. " Fill lgfsb with default value = lgort from file
          move wa_data-mtvfp to plantdata-availcheck.
          " IHDK900305
          read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
          if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
            wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
            append wa_log to it_log.
            continue.
          endif.
          move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
          plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
          move wa_data-webaz to plantdata-gr_pr_time.
          move wa_data-lgpro to plantdata-iss_st_loc. " Only diff between depot and plant ext for ZTRD
          " The following fields will be set to predefined default values if the reference plant does not have valid values for these
          if plantdata-loadinggrp ne 'Z001'.
            move 'Z001'        to plantdata-loadinggrp.   " LADGR
          endif.
          if headdata-matl_type eq 'ZRAW'.
            if plantdata-ctrl_key ne '0005'.
              move '0005' to plantdata-ctrl_key.       " MARC-SSQSS, PLANTDATA-CTRL_KEY with Dtype QSSPUR
            endif.
            if plantdata-cert_type ne 'E31C'.
              move 'E31C' to plantdata-cert_type.      " MARC-QZGTP, PLANTDATA-CERT_TYPEY with Dtype QZGTYP
            endif.
          endif.

          if headdata-matl_type eq 'ZPKG' or headdata-matl_type eq 'ZPKU'.
            if plantdata-ctrl_key ne '0001'.
              move '0001' to plantdata-ctrl_key.       " MARC-SSQSS, PLANTDATA-CTRL_KEY with Dtype QSSPUR
            endif.
            if plantdata-cert_type is not initial.
              clear plantdata-cert_type.               " MARC-QZGTP, PLANTDATA-CERT_TYPEY with Dtype QZGTYP
            endif.

            if plantdata-plant between 1401 and 1499.
              plantdata-mrp_type = 'ND'.  " Rev 07
            endif.
          endif.

          if clientdata-batch_mgmt eq abap_true.
            move '3' to plantdata-batchentry.
            clear plantdata-determ_grp.
          else.
            clear plantdata-batchentry.
            move 'IC01' to plantdata-determ_grp.
          endif.

          " IRDK930700
          if plantdata-plant+0(2) eq '13' or plantdata-plant+0(2) eq '14'.  " Depot
            clear plantdata-batchentry.
            clear plantdata-determ_grp.
          endif.
          " End IRDK930700

**** ---- Additional Requirement ----- ***
*          " Specific fields not to be copied are cleared here
**** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          " Fill storagelocationdata
          " Note storage location data is not fetched using getall bapi, only the fields plant, stge_loc need to be passed as provided in file
          move wa_data-werks to storagelocationdata-plant.
          move wa_data-lgort to storagelocationdata-stge_loc.
          " Fill storagelocationdatax
          fill_bapix(
          exporting
            bapi = storagelocationdata
          changing
            bapix = storagelocationdatax ).

          " Fill valuationdata
          move-corresponding _valuationdata to valuationdata.
          if wa_data-verpr is initial.  " IHDK903652
            wa_log-log = |Moving average price is mandatory|.
            append wa_log to it_log.
            continue.
          endif.
          move: wa_data-werks to valuationdata-val_area,
                wa_data-verpr to valuationdata-moving_pr.
          " IRDK931550
          move abap_true      to valuationdata-qty_struct.     " MBEW-EKALR, VALUATIONDATA-QTY_STRUCT with Dtype CK_EKALREL
          move abap_true      to valuationdata-orig_mat.       " HKMAT
**** ---- Additional Requirement ----- ***
*          " Specific fields not to be copied are cleared here
          clear valuationdata-overhead_grp.
**** ---- End Additional Requirement ---- ***
          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill salesdata
          move-corresponding _salesdata to salesdata.
          " If sales data is not maintained in initial creation
          " we still need to create the sales data at extension time
          " Set default sales org = comp code and dist. ch. = '10'
          if lo_ext_helper->v_dist_ch is initial.
            move abap_true to headdata-sales_view.  " sales view needs to be selected as well
            move '10' to lv_distchnl.
            move lv_distchnl to lo_ext_helper->v_dist_ch.  " Domestic only
            move lo_ext_helper->v_dist_ch to salesdata-distr_chan.
          endif.
          if salesdata-item_cat ne 'NORM'.
            move 'NORM' to salesdata-item_cat.
          endif.
          move: lo_ext_helper->wa_mat-target_bukrs to salesdata-sales_org.
          " IHDK906383
          if salesdata-acct_assgt is initial.
            wa_log-log = |Mandatory field account assignment group is missing|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill salesdatax
          fill_bapix(
          exporting
            bapi = salesdata
          changing
            bapix = salesdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Commented - IRDK930708
          " Fill taxclassifications
*          MOVE-CORRESPONDING _taxclassifications TO taxclassifications.
*          LOOP AT taxclassifications INTO wa_taxclass. " WHERE taxclass_1 IS INITIAL.  " Rev 01
*            MOVE '0' TO wa_taxclass-taxclass_1.
*            MOVE '0' TO wa_taxclass-tax_ind.   " Rev 01
*            MODIFY taxclassifications FROM wa_taxclass TRANSPORTING taxclass_1 taxind.
*            CLEAR: wa_taxclass.
*          ENDLOOP.

          " Re-maintain tax classification for extension from scratch(as in case of creation) rather than depending on tax classification ...
          " ...of source plant since it could have certain data missing(due to post GST requirements)
          " Hence, above code commented and below code added
          get_tax_classification(
            exporting
              salesdata = salesdata
            changing
              taxclassifications = taxclassifications ).
          " End IRDK930708

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = headdata
              clientdata           = clientdata
              clientdatax          = clientdatax
              plantdata            = plantdata
              plantdatax           = plantdatax
              storagelocationdata  = storagelocationdata
              storagelocationdatax = storagelocationdatax
              valuationdata        = valuationdata
              valuationdatax       = valuationdatax
              salesdata            = salesdata
              salesdatax           = salesdatax
            importing
              return               = return
            tables
              materialdescription  = materialdescription
              unitsofmeasure       = unitsofmeasure
              unitsofmeasurex      = unitsofmeasurex
              taxclassifications   = taxclassifications
              returnmessages       = returnmessages.
          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_retmessage.  " IHDK904473

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

              " For ZRAW, ZPKG, ZPKU, Exclude materials of plant 14XX from inspection setup
              clear: lv_insp_perform.
              if plantdata-plant+0(2) ne '14'.
                move abap_true to lv_insp_perform.
              endif.

              if lv_insp_perform eq abap_true.
                " Build insp-type select options
                if plantdata-plant+0(2) eq '13'.
                  refresh: s_art.
                  clear s_art_wa.
                  s_art_wa-sign   = 'I'.
                  s_art_wa-option = 'EQ'.
                  s_art_wa-low    = '01'.
                  append s_art_wa to s_art.
                else.
                  refresh: s_art.
                  clear s_art_wa.
                  s_art_wa-sign   = 'I'.
                  s_art_wa-option = 'EQ'.
                  s_art_wa-low    = '01'.
                  append s_art_wa to s_art.

                  clear s_art_wa.
                  s_art_wa-sign   = 'I'.
                  s_art_wa-option = 'EQ'.
                  s_art_wa-low    = '08'.
                  append s_art_wa to s_art.

                  clear s_art_wa.
                  s_art_wa-sign   = 'I'.
                  s_art_wa-option = 'EQ'.
                  s_art_wa-low    = '09'.
                  append s_art_wa to s_art.

                  clear s_art_wa.
                  s_art_wa-sign   = 'I'.
                  s_art_wa-option = 'EQ'.
                  s_art_wa-low    = '89'.
                  append s_art_wa to s_art.
                endif.

                maint_material_insp_setup(
                  exporting
                    material = headdata-material
                    plant    = plantdata-plant
                    s_art    = s_art
                  changing
                    insp_status = insp_status ).
              endif.

              " Rev 14
              maintain_mrp_area(
                exporting
                  material    = headdata-material
                  plant       = plantdata-plant
                  mrp_ctrler  = plantdata-mrp_ctrler
                  mrp_ls_key  = plantdata-lotsizekey
                importing
                  mrp_status  = data(mrp_status) ).

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant/depot' && ` ` && wa_data-werks.
              " Check status of inspection setup
              if insp_status ne 'X' and lv_insp_perform eq abap_true.
                wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
              endif.

              clear lv_ext_status.
              refresh it_mbew.
              select matnr bwkey bwtar bklas
                from mbew
                into table it_mbew
                where matnr = headdata-material
                and   bwkey = lo_ext_helper->wa_mat-source_werks.

              if it_mbew is not initial.
                loop at it_mbew into wa_mbew where bwtar is not initial.
                  wait up to 2 seconds.
                  extend_to_val_class(
                    exporting
                      headdata      = headdata
                      valuationdata = valuationdata
                      bwtar = wa_mbew-bwtar
                      bklas = wa_mbew-bklas
                    importing
                      ext_status =  lv_ext_status ).

                  if lv_ext_status is not initial.
                    wa_log-log = wa_log-log && ` ` && wa_mbew-bwtar && '-Yes'.
                  else.
                    wa_log-log = wa_log-log && ` ` && wa_mbew-bwtar && '-No'.
                  endif.
                  clear wa_mbew.
                endloop.
              endif.

              " Rev 14
              " check status of mrp area maintainance
              if mrp_status eq abap_false.
                wa_log-log = wa_log-log && '. MRP Area: Failed'.
              endif.

              clear lv_ext_status.
              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division  " IHDK900685
                importing
                  ext_status = lv_ext_status ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zsfg_create implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_crt_helper = new lcl_create_helper( ).
    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
    changing
      answer = me->answer ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        me->material_create_extend( ).  " Rename - IHDK900652
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZSFG_Create_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_crt_helper is bound.
    lo_crt_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear headdata.

      move 'C'            to headdata-ind_sector.    " MBRSH
      move wa_data-mtart  to headdata-matl_type.
      move abap_true      to headdata-basic_view.
      move abap_true      to headdata-sales_view.
      move abap_true      to headdata-purchase_view.
      move abap_true      to headdata-mrp_view.
      move abap_true      to headdata-work_sched_view.
      move abap_true      to headdata-storage_view.
      move abap_true      to headdata-quality_view.
      move abap_true      to headdata-account_view.
      move abap_true      to headdata-cost_view.
      move 'I'            to headdata-inp_fld_check.

      if headdata-matl_type ne 'ZSFG'.  " IHDK900652
        wa_log-log = |Incorrect material type { headdata-matl_type }|.
        append wa_log to it_log.
        continue.
      endif.

      clear clientdata.
      move wa_data-meins to clientdata-base_uom.
      move wa_data-matkl to clientdata-matl_group.
      move wa_data-spart to clientdata-division.
      move 'NORM'        to clientdata-item_cat.    " MTPOS_MARA
      move wa_data-gewei to clientdata-unit_of_wt.
      move '0001'        to clientdata-trans_grp.   " TRAGR
      move wa_data-mhdrz to clientdata-minremlife.
      move wa_data-mhdhb to clientdata-shelf_life.
      move abap_true     to clientdata-batch_mgmt.  " XCHPF

      fill_bapix(
      exporting
        bapi = clientdata
      changing
        bapix = clientdatax ).

      clear plantdata.
      move wa_data-werks to plantdata-plant.
      move 'Z001'        to plantdata-loadinggrp.   " LADGR
      move 'IN'          to plantdata-countryori.   " HERKL
      move wa_data-steuc to plantdata-ctrl_code.
      move wa_data-dismm to plantdata-mrp_type.
      move wa_data-dispo to plantdata-mrp_ctrler.
      move wa_data-minbe to plantdata-reorder_pt.
      move wa_data-disls to plantdata-lotsizekey.
      move wa_data-bstmi to plantdata-minlotsize.
      move wa_data-bstma to plantdata-maxlotsize.
      move wa_data-bstfe to plantdata-fixed_lot.
      move wa_data-eisbe to plantdata-safety_stk.
      move wa_data-ausss to plantdata-assy_scrap.
      move wa_data-beskz to plantdata-proc_type.
      move wa_data-sobsl to plantdata-spproctype.
      move '3'           to plantdata-batchentry.   " KZECH
      move wa_data-lgpro to plantdata-iss_st_loc.
      move wa_data-losgr to plantdata-lot_size.
      move wa_data-lgfsb to plantdata-sloc_exprc.
      move '1'           to plantdata-backflush.    " MARC-RGEKZ, PLANTDATA-BACKFLUSH with DType RGEKM
      move '1'           to plantdata-inhseprodt.   " DZEIT
      move wa_data-plifz to plantdata-plnd_delry.
      move wa_data-fhori to plantdata-sm_key.
      move wa_data-strgr to plantdata-plan_strgp.
      move wa_data-mtvfp to plantdata-availcheck.
      move wa_data-sbdkz to plantdata-dep_req_id.
      if clientdata-division eq '10' or clientdata-division eq '15'.
        move 'AGR'       to plantdata-production_scheduler.   " FEVOR
        move 'AGRO'      to plantdata-prodprof.               " MARC-SFCPF, PLANTDATA-PRODPROF with Dtype CO_PRODPRF
      elseif clientdata-division eq '20' or clientdata-division eq '28'.
        move 'SPC'       to plantdata-production_scheduler.   " FEVOR
        move 'SPCD'      to plantdata-prodprof.               " MARC-SFCPF, PLANTDATA-PRODPROF with Dtype CO_PRODPRF
      endif.
      move wa_data-uneto to plantdata-under_tol.
      move wa_data-ueeto to plantdata-over_tol.
      " IHDK900305
      read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
      if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
        wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
        append wa_log to it_log.
        continue.
      endif.
      move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
      plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
      move wa_data-prfrq to plantdata-insp_int.
      move wa_data-awsls to plantdata-variance_key.

      fill_bapix(
      exporting
        bapi = plantdata
      changing
        bapix = plantdatax ).

      clear storagelocationdata.
      move plantdata-plant to storagelocationdata-plant.
      move wa_data-lgort   to storagelocationdata-stge_loc.

      fill_bapix(
      exporting
        bapi = storagelocationdata
      changing
        bapix = storagelocationdatax ).

      clear valuationdata.
      move plantdata-plant  to valuationdata-val_area.  " ? include as it is mandatory
      move wa_data-bklas    to valuationdata-val_class.
      move wa_data-vprsv    to valuationdata-price_ctrl.
      move wa_data-peinh    to valuationdata-price_unit.
      move wa_data-stprs    to valuationdata-std_price.
      move abap_true        to valuationdata-qty_struct.     " MBEW-EKALR, VALUATIONDATA-QTY_STRUCT with Dtype CK_EKALREL
      move abap_true        to valuationdata-orig_mat.       " HKMAT
      move ls_pctr_mast-kosgr to valuationdata-overhead_grp.  " IHDK900305
      clear ls_pctr_mast. "IHDK900305.

      fill_bapix(
      exporting
        bapi = valuationdata
      changing
        bapix = valuationdatax ).

      clear salesdata.
      move wa_data-vkorg to salesdata-sales_org.
      move wa_data-vtweg to salesdata-distr_chan.
      move wa_data-vrkme to salesdata-sales_unit.
      move wa_data-dwerk to salesdata-delyg_plnt.
      move wa_data-sktof to salesdata-cash_disc.
      move wa_data-versg to salesdata-matl_stats.   " MVKE-VERSG, SALESDATA-MATL_STATS with Dtype STGMA

      " IHDK901148
      if wa_data-ktgrm is initial.
        wa_log-log = |Account assignment group is mandatory|.
        append wa_log to it_log.
        continue.
      else.
        move wa_data-ktgrm to salesdata-acct_assgt.
      endif.
      move 'NORM'        to salesdata-item_cat.     " MTPOS
      move '010'         to salesdata-matl_grp_1.
      move wa_data-mvgr2 to salesdata-matl_grp_2.
      move wa_data-mvgr3 to salesdata-matl_grp_3.
      move wa_data-mvgr4 to salesdata-matl_grp_4.
      move '001'         to salesdata-matl_grp_5.
      move wa_data-aumng to salesdata-min_order.    " MVKE-AUMNG, SALESDATA-MIN_ORDER with Dtype MINAU

      fill_bapix(
      exporting
        bapi = salesdata
      changing
        bapix = salesdatax ).

      refresh materialdescription.
      clear wa_matdesc.
      move sy-langu       to wa_matdesc-langu.
      move wa_data-maktx  to wa_matdesc-matl_desc.
      append wa_matdesc to materialdescription.

      " IRDK930708
      get_tax_classification(
        exporting
          salesdata = salesdata
        changing
          taxclassifications = taxclassifications ).

      if taxclassifications is not initial.
        loop at taxclassifications into wa_taxclass.
          clear wa_taxclass-tax_ind.  " Tax indicator not to be maintained for *FG
          modify taxclassifications from wa_taxclass transporting tax_ind.
          clear wa_taxclass.
        endloop.
      endif.
      " End IRDK930708

      " IHDK904473
      clear: mat_no_return, ms_err_log_db.

      if wa_data-matnr is initial.  " To do else => log as error, material to be created cannot be supplied externally
        headdata-material = material_number_get(
                              exporting
                                material_type = wa_data-mtart
                              importing
                                return = mat_no_return ).

        if headdata-material is initial or mat_no_return-type <> 'S'.
          wa_log-log = mat_no_return-message.
          append wa_log to it_log.
          continue.
        endif.
      else.
        continue.
      endif.

      " Note: For Extensionin refer https://gist.github.com/saurabhk-nbssap/0ca3873d400e1581f7a91ffa3c219b72

      " Execute BAPI
      clear: return, wa_retmessage.
      refresh: returnmessages.
      call function 'BAPI_MATERIAL_SAVEDATA'
        exporting
          headdata             = headdata
          clientdata           = clientdata
          clientdatax          = clientdatax
          plantdata            = plantdata
          plantdatax           = plantdatax
          storagelocationdata  = storagelocationdata
          storagelocationdatax = storagelocationdatax
          valuationdata        = valuationdata
          valuationdatax       = valuationdatax
          salesdata            = salesdata
          salesdatax           = salesdatax
        importing
          return               = return
        tables
          materialdescription  = materialdescription
          taxclassifications   = taxclassifications
          returnmessages       = returnmessages.

      read table returnmessages into wa_retmessage with key type = 'E' transporting message.
      if sy-subrc = 0.  " Error exists, log it in error log
        wa_log-log = wa_retmessage-message.
        append wa_log to it_log.
        clear: wa_retmessage.  " IHDK904473

        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else. " No error
        if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

          maint_material_classif(
            exporting
              material = headdata-material
              class_num = 'ZSFG_FG'
              class_type = '023'  " IHDK901148
            changing
              classif_status = classif_status ).

          " Build insp-type select options
          refresh: s_art.
          clear s_art_wa.
          s_art_wa-sign   = 'I'.
          s_art_wa-option = 'EQ'.

          s_art_wa-low    = '03'.
          append s_art_wa to s_art.
          s_art_wa-low    = '08'.
          append s_art_wa to s_art.
          s_art_wa-low    = '89'.
          append s_art_wa to s_art.

          maint_material_insp_setup(
            exporting
              material = headdata-material
              plant    = plantdata-plant
              s_art    = s_art
            changing
              insp_status = insp_status ).

          " Rev 14
          maintain_mrp_area(
            exporting
              material    = headdata-material
              plant       = plantdata-plant
              mrp_ctrler  = plantdata-mrp_ctrler
              mrp_ls_key  = plantdata-lotsizekey
            importing
              mrp_status  = data(mrp_status) ).

          wa_log-matnr = headdata-material.
          wa_log-log = return-message.
          " Check status of classification
          if classif_status ne 1.
            wa_log-log = wa_log-log && '. Classification data: Failed.'.
          endif.
          " Check status of inspection setup
          if insp_status ne 'X'.
            wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
          endif.

          " Rev 14
          " check status of mrp area maintainance
          if mrp_status eq abap_false.
            wa_log-log = wa_log-log && '. MRP Area: Failed'.
          endif.

          extend_to_stor_loc( " IHDK900652
            exporting
              material   = headdata-material
              plant      = plantdata-plant
              matl_type  = headdata-matl_type
              division   = clientdata-division  " IHDK900685
            importing
              ext_status = data(lv_ext_status) ).

          if lv_ext_status eq abap_false.
            wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
          endif.

          append wa_log to it_log.
          clear: lv_ext_status. " IHDK904473
        endif.  " IF return-number = '356'
      endif.
      " End read returnmessages
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zsfg_ext_plant implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
                          changing it_data = it_data
                                   it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " End IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZSFG_Plant_Extend_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
                changing it_data = it_data
                         it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
                                                                   target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, salesdata, _return_wa.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        " For sales org data, pass the source company code as the ref sales org
        " Change the sales org to the target comp code, copy rest of the sales data as it is
        " if the source cc = target cc, it will be just like copying the sales data without any change
        " if the source cc <> target cc, the material will be extended to the target sales org using the data of the source sales org

        " Get distribution channel
        clear lo_ext_helper->v_dist_ch.
        select min( vtweg )
          from mvke
          into lo_ext_helper->v_dist_ch
          where matnr eq lo_ext_helper->wa_mat-matnr
          and   vkorg eq lo_ext_helper->wa_mat-source_bukrs.

        if lo_ext_helper->v_dist_ch is initial.
          shift: wa_data-matnr left deleting leading '0',
                 lo_ext_helper->wa_mat-source_bukrs left deleting leading '0',
                 lo_ext_helper->wa_mat-source_werks left deleting leading '0'.
          wa_log-log = 'No reference sales data found: Ref Comp\Sales Org:' && ` ` && lo_ext_helper->wa_mat-source_bukrs
                && ` ` && 'Ref. plant:' && ` ` && lo_ext_helper->wa_mat-source_werks.
          append wa_log to it_log.
          clear: wa_log.
          continue.
        endif.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
            salesorganisation   = lo_ext_helper->wa_mat-source_bukrs  " sales org is maintained same as comp code for that comp code in masters
            distributionchannel = lo_ext_helper->v_dist_ch
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
            salesdata           = _salesdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial or _valuationdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, storagelocationdata, valuationdata,
                 plantdatax, storagelocationdatax, valuationdatax.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
            exporting
              pstat = _clientdata-maint_stat
            changing
              headdata = headdata ).

          if headdata-matl_type ne 'ZSFG'.  " IHDK900652
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-bstmi to plantdata-minlotsize.
          move wa_data-beskz to plantdata-proc_type.
          move wa_data-sobsl to plantdata-spproctype.
          move wa_data-lgfsb to plantdata-sloc_exprc.
          move wa_data-mtvfp to plantdata-availcheck.
          " IHDK900305
          read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
          if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
            wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
            append wa_log to it_log.
            continue.
          endif.
          move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305
          plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.
          move '1'           to plantdata-inhseprodt. " Rev 03
          move '2'           to plantdata-plnd_delry. " Rev 03
*** ---- Additional Requirement ----- ***
          " Specific fields not to be copied are cleared here
          clear: " Fields in MRP3 view 'Planning' tab
                 plantdata-consummode,
                 plantdata-bwd_cons,
                 plantdata-fwd_cons,
                 plantdata-mixed_mrp,
                 " Fields from MVGD/MPGD - Planning Data/View, currently not included
*                 planningdata-plng_matl, " Planning material
*                 planningdata-plng_plant, " Planning plant
*                 planningdata-convfactor. " Conv. factor f. plng material
                 " Planning matl BUnit : not found

                 " Fields in WorkScheduling View 'In-house production time in days' tab
                 plantdata-setuptime,
                 plantdata-proc_time,
                 plantdata-interop,
                 plantdata-base_qty,
                 plantdata-specprocty.  " Rev 03
*** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          " Fill storagelocationdata
          " Note storage location data is not fetched using getall bapi, only the fields plant, stge_loc need to be passed as provided in file
          move wa_data-werks to storagelocationdata-plant.
          move wa_data-lgort to storagelocationdata-stge_loc.
          " Fill storagelocationdatax
          fill_bapix(
          exporting
            bapi = storagelocationdata
          changing
            bapix = storagelocationdatax ).

          " Fill valuationdata
          move-corresponding _valuationdata to valuationdata.
          move: wa_data-werks to valuationdata-val_area,
                wa_data-vprsv to valuationdata-price_ctrl,
                wa_data-verpr to valuationdata-moving_pr,
                wa_data-stprs to valuationdata-std_price,
                ls_pctr_mast-kosgr to valuationdata-overhead_grp.  " IHDK900305
          clear ls_pctr_mast. "IHDK900305.
          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill salesdata
          move-corresponding _salesdata to salesdata.
          move: lo_ext_helper->wa_mat-target_bukrs to salesdata-sales_org.

          " IHDK901148
          if salesdata-acct_assgt is initial.
            wa_log-log = |Mandatory field account assignment group is missing|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill salesdatax
          fill_bapix(
          exporting
            bapi = salesdata
          changing
            bapix = salesdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Commented - IRDK930708
          " Fill taxclassifications
*          MOVE-CORRESPONDING _taxclassifications TO taxclassifications.
*          LOOP AT taxclassifications INTO wa_taxclass. " WHERE taxclass_1 IS INITIAL.  " Rev 01
*            MOVE '0' TO wa_taxclass-taxclass_1.
*            MOVE '0' TO wa_taxclass-tax_ind.   " Rev 01
*            MODIFY taxclassifications FROM wa_taxclass TRANSPORTING taxclass_1 taxind.
*            CLEAR: wa_taxclass.
*          ENDLOOP.

          " Re-maintain tax classification for extension from scratch(as in case of creation) rather than depending on tax classification ...
          " ...of source plant since it could have certain data missing(due to post GST requirements)
          " Hence, above code commented and below code added
          get_tax_classification(
            exporting
              salesdata = salesdata
            changing
              taxclassifications = taxclassifications ).

          if taxclassifications is not initial.
            loop at taxclassifications into wa_taxclass.
              clear wa_taxclass-tax_ind.  " Tax indicator not to be maintained for *FG
              modify taxclassifications from wa_taxclass transporting tax_ind.
              clear wa_taxclass.
            endloop.
          endif.
          " End IRDK930708

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = headdata
              clientdata           = clientdata
              clientdatax          = clientdatax
              plantdata            = plantdata
              plantdatax           = plantdatax
              storagelocationdata  = storagelocationdata
              storagelocationdatax = storagelocationdatax
              valuationdata        = valuationdata
              valuationdatax       = valuationdatax
              salesdata            = salesdata
              salesdatax           = salesdatax
            importing
              return               = return
            tables
              materialdescription  = materialdescription
              unitsofmeasure       = unitsofmeasure
              unitsofmeasurex      = unitsofmeasurex
              taxclassifications   = taxclassifications
              returnmessages       = returnmessages.
          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_log, wa_retmessage.

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " IHDK903847

              " Build insp-type select options
              refresh: s_art.
              clear s_art_wa.
              s_art_wa-sign   = 'I'.
              s_art_wa-option = 'EQ'.

              s_art_wa-low    = '03'.
              append s_art_wa to s_art.
              s_art_wa-low    = '08'.
              append s_art_wa to s_art.
              s_art_wa-low    = '89'.
              append s_art_wa to s_art.

              maint_material_insp_setup(
                exporting
                  material = headdata-material
                  plant    = plantdata-plant
                  s_art    = s_art
                changing
                  insp_status = insp_status ).

              " Rev 14
              " dispo and disls is not part of input file, this logic prevents empty input to below method in case it is not copied from source plant
              if plantdata-mrp_ctrler is initial.
                plantdata-mrp_ctrler = '008'.
              endif.

              if plantdata-lotsizekey is initial.
                plantdata-lotsizekey = 'EX'.
              endif.

              maintain_mrp_area(
                exporting
                  material    = headdata-material
                  plant       = plantdata-plant
                  mrp_ctrler  = plantdata-mrp_ctrler
                  mrp_ls_key  = plantdata-lotsizekey
                importing
                  mrp_status  = data(mrp_status) ).

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant/depot' && ` ` && wa_data-werks.
              " Check status of inspection setup
              if insp_status ne 'X'.
                wa_log-log = wa_log-log && '. Inspection setup: Failed.'.
              endif.

              " Rev 14
              " check status of mrp area maintainance
              if mrp_status eq abap_false.
                wa_log-log = wa_log-log && '. MRP Area: Failed'.
              endif.

              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division  " IHDK900685
                importing
                  ext_status = data(lv_ext_status) ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data, mrp_status.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zmco_create implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_crt_helper = new lcl_create_helper( ).
    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
    changing
      answer = me->answer ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        me->material_create_extend( ).  " Rename - IHDK900652
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZMCO_Create_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_crt_helper is bound.
    lo_crt_helper->material_validate(
    changing it_data = it_data
      it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.
      if wa_data-steuc is initial.
        wa_log-log = 'Valid control code is mandatory'.
        append wa_log to it_log.
        continue.
      endif.

      if wa_data-ekgrp is initial.
        wa_log-log = 'Purchasing group is mandatory'.
        append wa_log to it_log.
        continue.
      endif.

      clear headdata.

      move 'C'            to headdata-ind_sector.    " MBRSH
      move wa_data-mtart  to headdata-matl_type.
      move abap_true      to headdata-basic_view.
      move abap_true      to headdata-purchase_view.
      move abap_true      to headdata-account_view.
      move 'I'            to headdata-inp_fld_check.

      if headdata-matl_type ne 'ZMCO'.  " IHDK900652
        wa_log-log = |Incorrect material type { headdata-matl_type }|.
        append wa_log to it_log.
        continue.
      endif.

      clear clientdata.
      move wa_data-meins to clientdata-base_uom.
      move wa_data-matkl to clientdata-matl_group.
      move wa_data-spart to clientdata-division.
      move 'NLAG'        to clientdata-item_cat.    " MTPOS_MARA
      move wa_data-bstme to clientdata-po_unit.
      move wa_data-vabme to clientdata-var_ord_un.

      fill_bapix(
      exporting
        bapi = clientdata
      changing
        bapix = clientdatax ).

      clear plantdata.
      move wa_data-werks to plantdata-plant.
      move wa_data-ekgrp to plantdata-pur_group.
      move wa_data-steuc to plantdata-ctrl_code.

      fill_bapix(
      exporting
        bapi = plantdata
      changing
        bapix = plantdatax ).

      if ( wa_data-meins ne wa_data-bstme ) and wa_data-umrez is not initial.
        refresh: unitsofmeasure, unitsofmeasurex.
        clear: wa_uom, wa_uomx.
        move clientdata-po_unit to wa_uom-alt_unit.
        move wa_data-umrez      to wa_uom-numerator.

        fill_bapix(
        exporting
          bapi = wa_uom
        changing
          bapix = wa_uomx ).
        append: wa_uom  to unitsofmeasure,
        wa_uomx to unitsofmeasurex.
      endif.

      clear valuationdata.
      move plantdata-plant  to valuationdata-val_area.  " ? include as it is mandatory
      move wa_data-vprsv    to valuationdata-price_ctrl.
      move wa_data-peinh    to valuationdata-price_unit.
      move wa_data-verpr    to valuationdata-moving_pr.

      fill_bapix(
      exporting
        bapi = valuationdata
      changing
        bapix = valuationdatax ).

      refresh materialdescription.
      clear wa_matdesc.
      move sy-langu       to wa_matdesc-langu.
      move wa_data-maktx  to wa_matdesc-matl_desc.
      append wa_matdesc to materialdescription.

      clear wa_taxclass.
      refresh taxclassifications.
      wa_taxclass-depcountry = 'IN'.
      wa_taxclass-tax_ind    = wa_data-taxim.
      append wa_taxclass to taxclassifications.

      " IHDK904473
      clear: mat_no_return, ms_err_log_db.

      if wa_data-matnr is initial.  " To do else => log as error, material to be created cannot be supplied externally
        headdata-material = material_number_get(
                              exporting
                                material_type = wa_data-mtart
                              importing
                                return = mat_no_return ).

        if headdata-material is initial or mat_no_return-type <> 'S'.
          wa_log-log = mat_no_return-message.
          append wa_log to it_log.
          continue.
        endif.
      else.
        continue.
      endif.

      " Note: For Extensionin refer https://gist.github.com/saurabhk-nbssap/0ca3873d400e1581f7a91ffa3c219b72

      " Execute BAPI
      clear: return, wa_retmessage.
      refresh: returnmessages.
      call function 'BAPI_MATERIAL_SAVEDATA'
        exporting
          headdata            = headdata
          clientdata          = clientdata
          clientdatax         = clientdatax
          plantdata           = plantdata
          plantdatax          = plantdatax
          valuationdata       = valuationdata
          valuationdatax      = valuationdatax
        importing
          return              = return
        tables
          materialdescription = materialdescription
          taxclassifications  = taxclassifications
          unitsofmeasure      = unitsofmeasure
          unitsofmeasurex     = unitsofmeasurex
          returnmessages      = returnmessages.

      read table returnmessages into wa_retmessage with key type = 'E' transporting message.
      if sy-subrc = 0.  " Error exists, log it in error log
        wa_log-log = wa_retmessage-message.
        append wa_log to it_log.
        clear: wa_retmessage.  " IHDK904473

        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else. " No error
        if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " Rev 05

          wa_log-matnr = headdata-material.
          wa_log-log = return-message.

          extend_to_stor_loc( " IHDK900652
            exporting
              material   = headdata-material
              plant      = plantdata-plant
              matl_type  = headdata-matl_type
              division   = clientdata-division  " IHDK900685
            importing
              ext_status = data(lv_ext_status) ).

          if lv_ext_status eq abap_false.
            wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
          endif.

          append wa_log to it_log.
          clear: lv_ext_status. " IHDK904473
        endif.  " IF return-number = '356'
      endif.
      " End read returnmessages
      clear wa_data.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_zmco_ext_plant implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
          changing it_data = it_data
            it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " End IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'ZMCO_Plant_Extend_Log_' to msg.
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
    changing it_data = it_data
      it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
      target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, _return_wa.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, plantdatax, valuationdata, valuationdatax.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
          exporting
            pstat = _clientdata-maint_stat
          changing
            headdata = headdata ).

          if headdata-matl_type ne 'ZMCO'.  " IHDK900652
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-ekgrp to plantdata-pur_group.

*** ---- Additional Requirement ----- ***
          " Specific fields not to be copied are cleared here

*** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          move-corresponding _valuationdata to valuationdata.
          move plantdata-plant to valuationdata-val_area.     " IRDK931855
          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Fill taxclassifications
          if _taxclassifications is not initial.
            move-corresponding _taxclassifications to taxclassifications.
          else.
            clear wa_taxclass.
            refresh taxclassifications.
            wa_taxclass-depcountry = 'IN'.
            wa_taxclass-tax_ind    = '0'.
            append wa_taxclass to taxclassifications.
          endif.

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata            = headdata
              clientdata          = clientdata
              clientdatax         = clientdatax
              plantdata           = plantdata
              plantdatax          = plantdatax
              valuationdata       = valuationdata
              valuationdatax      = valuationdatax
            importing
              return              = return
            tables
              materialdescription = materialdescription
              unitsofmeasure      = unitsofmeasure
              unitsofmeasurex     = unitsofmeasurex
              taxclassifications  = taxclassifications
              returnmessages      = returnmessages.

          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_retmessage.  " IHDK904473

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " Rev 05

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant/depot' && ` ` && wa_data-werks.

              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division   " IHDK900685
                importing
                  ext_status = data(lv_ext_status) ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_yzspr_yspi_zeu2_create implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_crt_helper = new lcl_create_helper( ).
    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
    changing
      answer = me->answer ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        me->material_create_extend( ).  " Rename - IHDK900652
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'YSPR_ZSPR_YSPI_ZEU2_ZCON_Create_Log_' to msg. " IHDK902087
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_crt_helper is bound.
    lo_crt_helper->material_validate(
    changing it_data = it_data
      it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear headdata.

      move 'C'            to headdata-ind_sector.    " MBRSH
      move wa_data-mtart  to headdata-matl_type.
      move abap_true      to headdata-basic_view.
      move abap_true      to headdata-purchase_view.
      move abap_true      to headdata-mrp_view.
      move abap_true      to headdata-storage_view.
      move abap_true      to headdata-account_view.
      move 'I'            to headdata-inp_fld_check.

      if headdata-matl_type ne 'YSPR' and " IHDK900652
         headdata-matl_type ne 'ZSPR' and
         headdata-matl_type ne 'YSPI' and
         headdata-matl_type ne 'ZEU2' and
         headdata-matl_type ne 'ZCON'.  " IHDK902087
        wa_log-log = |Incorrect material type { headdata-matl_type }|.
        append wa_log to it_log.
        continue.
      endif.

      clear clientdata.
      move wa_data-meins to clientdata-base_uom.
      move wa_data-matkl to clientdata-matl_group.
      move wa_data-spart to clientdata-division.
      move 'NORM'        to clientdata-item_cat.    " MTPOS_MARA
      move '0001'        to clientdata-trans_grp.   " TRAGR
      move wa_data-bstme to clientdata-po_unit.
      move wa_data-vabme to clientdata-var_ord_un.
      move wa_data-ekwsl to clientdata-pur_valkey.  " IRDK932541

      fill_bapix(
      exporting
        bapi = clientdata
      changing
        bapix = clientdatax ).

      clear plantdata.
      move wa_data-werks to plantdata-plant.
      move 'Z001'        to plantdata-loadinggrp.   " LADGR
      move wa_data-steuc to plantdata-ctrl_code.
      move wa_data-ekgrp to plantdata-pur_group.
      move wa_data-dismm to plantdata-mrp_type.
      move wa_data-dispo to plantdata-mrp_ctrler.
      move wa_data-minbe to plantdata-reorder_pt.
      move wa_data-disls to plantdata-lotsizekey.
      move wa_data-bstmi to plantdata-minlotsize.
      move wa_data-bstma to plantdata-maxlotsize.
      move wa_data-bstfe to plantdata-fixed_lot.
      move wa_data-eisbe to plantdata-safety_stk.
      move wa_data-beskz to plantdata-proc_type.
      if headdata-matl_type eq 'ZCON'.  " IHDK902087
        move wa_data-lgort to plantdata-sloc_exprc.
      else.
        move '1701'        to plantdata-sloc_exprc.   " Default value '1701'
      endif.
      move wa_data-plifz to plantdata-plnd_delry.
      move wa_data-webaz to plantdata-gr_pr_time.
      move wa_data-mtvfp to plantdata-availcheck.
*      case plantdata-plant.
*        when '1101'.
*          move '100101' to plantdata-profit_ctr.
*        when '2101'.
*          move '200101' to plantdata-profit_ctr.
*        when '2510'.
*          move '205001' to plantdata-profit_ctr.
*        when '1105'.
*          move '101202' to plantdata-profit_ctr.
*        when '1108'.
*          move '101102' to plantdata-profit_ctr.
*        when '3101'.
*          move '300301PR' to plantdata-profit_ctr.
*        when others.
*          move wa_data-prctr to plantdata-profit_ctr.
*      endcase.
      " IHDK900305
      read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
      if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
        wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
        append wa_log to it_log.
        continue.
      endif.
      move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305

      plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.

      fill_bapix(
      exporting
        bapi = plantdata
      changing
        bapix = plantdatax ).

      clear storagelocationdata.
      move plantdata-plant to storagelocationdata-plant.
      move wa_data-lgort   to storagelocationdata-stge_loc.

      fill_bapix(
      exporting
        bapi = storagelocationdata
      changing
        bapix = storagelocationdatax ).

      clear valuationdata.
      move plantdata-plant  to valuationdata-val_area.  " ? include as it is mandatory
      if wa_data-mtart eq 'YSPR'.
        move '7950' to valuationdata-val_class.
      elseif wa_data-mtart eq 'YSPI'.
        move '7960' to valuationdata-val_class.
      elseif wa_data-mtart eq 'ZCON'. " IHDK902087
        move '5500' to valuationdata-val_class.
      else. " other material type
        move wa_data-bklas to valuationdata-val_class.
      endif.
      move wa_data-vprsv    to valuationdata-price_ctrl.
      move wa_data-peinh    to valuationdata-price_unit.
      move wa_data-verpr    to valuationdata-moving_pr.

      fill_bapix(
      exporting
        bapi = valuationdata
      changing
        bapix = valuationdatax ).

      refresh materialdescription.
      clear wa_matdesc.
      move sy-langu       to wa_matdesc-langu.
      move wa_data-maktx  to wa_matdesc-matl_desc.
      append wa_matdesc to materialdescription.

      if ( wa_data-meins ne wa_data-bstme ) and wa_data-umrez is not initial.
        refresh: unitsofmeasure, unitsofmeasurex.
        clear: wa_uom, wa_uomx.
        move clientdata-po_unit to wa_uom-alt_unit.
        move wa_data-umrez      to wa_uom-numerator.

        fill_bapix(
        exporting
          bapi = wa_uom
        changing
          bapix = wa_uomx ).
        append: wa_uom  to unitsofmeasure,
        wa_uomx to unitsofmeasurex.
      endif.

      clear wa_taxclass.
      refresh taxclassifications.
      wa_taxclass-depcountry = 'IN'.
      wa_taxclass-tax_ind    = wa_data-taxim.
      append wa_taxclass to taxclassifications.

      " IHDK904473
      clear: mat_no_return, ms_err_log_db.

      if wa_data-matnr is initial.  " To do else => log as error, material to be created cannot be supplied externally
        headdata-material = material_number_get(
                              exporting
                                material_type = wa_data-mtart
                              importing
                                return = mat_no_return ).

        if headdata-material is initial or mat_no_return-type <> 'S'.
          wa_log-log = mat_no_return-message.
          append wa_log to it_log.
          continue.
        endif.
      else.
        continue.
      endif.

      " Note: For Extensionin refer https://gist.github.com/saurabhk-nbssap/0ca3873d400e1581f7a91ffa3c219b72

      " Execute BAPI
      clear: return, wa_retmessage.
      refresh: returnmessages.
      call function 'BAPI_MATERIAL_SAVEDATA'
        exporting
          headdata             = headdata
          clientdata           = clientdata
          clientdatax          = clientdatax
          plantdata            = plantdata
          plantdatax           = plantdatax
          storagelocationdata  = storagelocationdata
          storagelocationdatax = storagelocationdatax
          valuationdata        = valuationdata
          valuationdatax       = valuationdatax
        importing
          return               = return
        tables
          materialdescription  = materialdescription
          unitsofmeasure       = unitsofmeasure
          unitsofmeasurex      = unitsofmeasurex
          taxclassifications   = taxclassifications
          returnmessages       = returnmessages.

      read table returnmessages into wa_retmessage with key type = 'E' transporting message.
      if sy-subrc = 0.  " Error exists, log it in error log
        wa_log-log = wa_retmessage-message.
        append wa_log to it_log.
        clear: wa_retmessage.  " IHDK904473

        call function 'BAPI_TRANSACTION_ROLLBACK'.
      else. " No error
        if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
          call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " Rev 05

          wa_log-matnr = headdata-material.
          wa_log-log = return-message.

          extend_to_stor_loc( " IHDK900652
            exporting
              material   = headdata-material
              plant      = plantdata-plant
              matl_type  = headdata-matl_type
              division   = clientdata-division  " IHDK900685
            importing
              ext_status = data(lv_ext_status) ).

          if lv_ext_status eq abap_false.
            wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
          endif.

          append wa_log to it_log.
          clear: lv_ext_status. " IHDK904473
        endif.  " IF return-number = '356'
      endif.
      " End read returnmessages
      clear wa_data.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_yzspr_yspi_zeu2_ext_plt implementation.
  method constructor.
    super->constructor( ).
    try.
        me->invoke( ).
      catch cx_root into lo_cx.
        message lo_cx->get_text( ) type 'S' display like 'E'.
        return.
    endtry.
  endmethod.
  method invoke.
    lo_ext_helper = new lcl_extend_helper( ).

    try.
        file_to_tab(
        changing
          it_file = it_file ).
      catch lcx_generic_error.
        return.
    endtry.
    try.
        check_if_tab_empty(
        changing
          it_file = it_file
          log_header1 = log_header1
          log_header2 = log_header2 ).
      catch lcx_generic_error.
        return.
    endtry.

    file_format_adjust(
    changing
      it_file = it_file ).

    try.
        convert_data_to_bapi_format(
        exporting
          it_file = it_file
        changing
          it_data = it_data ).
      catch cx_root.
        return.
    endtry.

    me->material_validation( changing it_data = it_data it_log = it_log ).  " IHDK900894

    " IRDK930708
    confirm_action(
    exporting
      it_file = it_file
      it_data = it_data
      blk_cnt = lo_ext_helper->v_block_material_count
    changing
      answer = me->answer
      answer_blk = me->answer_blk ).

    if lines_data gt '0'.
      if me->answer eq '1'.
        if lo_ext_helper->v_block_material_count gt 0 and answer_blk ne '1'.
          lo_ext_helper->filter_out_blocked_materials(
          changing it_data = it_data
            it_log  = it_log ).
        endif.
        me->material_create_extend( ).  " Rename - IHDK900652
        " IRDK930708
      else.   " IF answer EQ
* If user cancels extension or answers with NO *
        disp_message(
        type = 'S'
        id   = 'ZMM'
        no   = '009' ).
      endif.  " IF answer EQ
    endif.

    try.
        clear msg.
        move 'YSPR_ZSPR_YSPI_ZEU2_ZCON_Plant_Extend_Log_' to msg. " IHDK902087
        log_output(
        exporting
          log_header1 = log_header1
          log_header2 = log_header2
          filename    = msg
        changing
          it_log = it_log ).
      catch lcx_generic_error.
        return.
    endtry.
  endmethod.

  method material_validation.
    check lo_ext_helper is bound.
    lo_ext_helper->material_validate(
    changing it_data = it_data
      it_log  = it_log ).
  endmethod.

  method material_create_extend.  " Rename - IHDK900652
    " Method implementation here
    check it_data is not initial and lo_ext_helper is bound.
    loop at it_data into wa_data where sel eq abap_true.
      clear wa_log.
      move-corresponding wa_data to wa_log.

      clear lo_ext_helper->wa_mat.
      read table lo_ext_helper->it_mat into lo_ext_helper->wa_mat with key matnr = wa_data-matnr
      target_werks = wa_data-werks.
      if sy-subrc eq 0.
        clear: _clientdata, _plantdata, _valuationdata, _return_wa.
        refresh: _materialdescription, _unitsofmeasure, _taxclassifications, _return_tab.

        call function 'BAPI_MATERIAL_GETALL'
          exporting
            material            = lo_ext_helper->wa_mat-matnr
            companycode         = lo_ext_helper->wa_mat-source_bukrs
            valuationarea       = lo_ext_helper->wa_mat-source_werks
            plant               = lo_ext_helper->wa_mat-source_werks
          importing
            clientdata          = _clientdata
            plantdata           = _plantdata
            valuationdata       = _valuationdata
            salesdata           = _salesdata
          tables
            materialdescription = _materialdescription
            unitsofmeasure      = _unitsofmeasure
            taxclassifications  = _taxclassifications
            return              = _return_tab.

        read table _return_tab into _return_wa with key type = 'E'.
        if sy-subrc eq 0 or ( _clientdata is initial or _plantdata is initial or _valuationdata is initial ).
          shift: wa_data-matnr left deleting leading '0', wa_data-werks left deleting leading '0'.
          wa_log-log = 'Reference data could not be fetched for' &&  ` ` && wa_data-matnr &&  ` ` && '-' && ` ` && wa_data-werks.
          append wa_log to it_log.
          clear: wa_log, _return_wa.
        else.
          clear: headdata, clientdata, clientdatax, plantdata, storagelocationdata, valuationdata,
          plantdatax, storagelocationdatax, valuationdatax.

          " Fill headdata
          move-corresponding _clientdata to headdata.
          head_views_from_pstat(
          exporting
            pstat = _clientdata-maint_stat
          changing
            headdata = headdata ).

          if headdata-matl_type ne 'YSPR' and " IHDK900652
             headdata-matl_type ne 'ZSPR' and
             headdata-matl_type ne 'YSPI' and
             headdata-matl_type ne 'ZEU2' and
             headdata-matl_type ne 'ZCON'.  " IHDK902087
            wa_log-log = |Incorrect material type { headdata-matl_type }|.
            append wa_log to it_log.
            continue.
          endif.

          " Fill clientdata
          move-corresponding _clientdata to clientdata.
          " The following fields will be set to predefined default values if the reference plant does not have valid values for these
          if clientdata-trans_grp ne '0001'.
            move '0001'        to clientdata-trans_grp.   " TRAGR
          endif.

          " Fill clientdatax
          fill_bapix(
          exporting
            bapi = clientdata
          changing
            bapix = clientdatax ).

          " Fill plantdata
          move-corresponding _plantdata to plantdata.
          move wa_data-werks to plantdata-plant.
          move wa_data-ekgrp to plantdata-pur_group.
          move wa_data-dismm to plantdata-mrp_type.   " IHDK900227
          move wa_data-dispo to plantdata-mrp_ctrler.
          move wa_data-minbe to plantdata-reorder_pt.
          move wa_data-disls to plantdata-lotsizekey.
          move wa_data-bstmi to plantdata-minlotsize.
          move wa_data-bstma to plantdata-maxlotsize.
          move wa_data-bstfe to plantdata-fixed_lot.
          move wa_data-eisbe to plantdata-safety_stk.
          move wa_data-plifz to plantdata-plnd_delry.
          move '1701'        to plantdata-sloc_exprc. " Fill lgfsb with default value = '1701'
          move wa_data-mtvfp to plantdata-availcheck.

          " IHDK900227
          if plantdata-ctrl_code is not initial.
            if strlen( plantdata-ctrl_code ) ne 4 or plantdata-ctrl_code+0(2) eq '99'.
              move wa_data-steuc to plantdata-ctrl_code.
            endif.
          else.
            move wa_data-steuc to plantdata-ctrl_code.
          endif.

*          case plantdata-plant.
*            when '1101'.
*              move '100101' to plantdata-profit_ctr.
*            when '2101'.
*              move '200101' to plantdata-profit_ctr.
*            when '2510'.
*              move '205001' to plantdata-profit_ctr.
*            when '1105'.
*              move '101202' to plantdata-profit_ctr.
*            when '1108'.
*              move '101102' to plantdata-profit_ctr.
*            when '3101'.
*              move '300301PR' to plantdata-profit_ctr.
*            when others.
*              move wa_data-prctr to plantdata-profit_ctr.
*          endcase.

          " IHDK900305
          read table gt_pctr_mast into data(ls_pctr_mast) with key werks = plantdata-plant spart = clientdata-division.
          if sy-subrc <> 0 or ls_pctr_mast-prctr is initial.
            wa_log-log = |Profit center master data missing for plant { plantdata-plant }, division { clientdata-division }|.
            append wa_log to it_log.
            continue.
          endif.
          move ls_pctr_mast-prctr to plantdata-profit_ctr.  " IHDK900305

          plantdata-profit_ctr = |{ plantdata-profit_ctr alpha = in }|.

          move wa_data-webaz to plantdata-gr_pr_time.
          " The following fields will be set to predefined default values if the reference plant does not have valid values for these
          if plantdata-loadinggrp ne 'Z001'.
            move 'Z001'        to plantdata-loadinggrp.   " LADGR
          endif.

**** ---- Additional Requirement ----- ***
*          " Specific fields not to be copied are cleared here
**** ---- End Additional Requirement ---- ***
          " Fill plantdatax
          fill_bapix(
          exporting
            bapi = plantdata
          changing
            bapix = plantdatax ).

          " Fill storagelocationdata
          " Note storage location data is not fetched using getall bapi, only the fields plant, stge_loc need to be passed as provided in file
          if headdata-matl_type ne 'ZCON'.  " IHDK902087
            move wa_data-werks to storagelocationdata-plant.
            move '1701' to storagelocationdata-stge_loc.    " ?? For ZCON ??
            " Fill storagelocationdatax
            fill_bapix(
            exporting
              bapi = storagelocationdata
            changing
              bapix = storagelocationdatax ).
          else.
            clear: storagelocationdata, storagelocationdatax. " no extension to 1701 SLOC in case of ZCON
          endif.

          " Fill valuationdata
          move-corresponding _valuationdata to valuationdata.
          move: wa_data-werks to valuationdata-val_area,
          wa_data-verpr to valuationdata-moving_pr.

          " Fill valuationdatax
          fill_bapix(
          exporting
            bapi = valuationdata
          changing
            bapix = valuationdatax ).

          " Fill materialdescription
          move-corresponding _materialdescription to materialdescription.

          " Fill unitsofmeasure
          move-corresponding _unitsofmeasure to unitsofmeasure.
          " Fill unitsofmeasurex
          loop at unitsofmeasure into wa_uom.
            fill_bapix(
            exporting
              bapi = wa_uom
            changing
              bapix = wa_uomx ).
            append: wa_uomx to unitsofmeasurex.
            clear: wa_uom, wa_uomx.
          endloop.

          " Fill taxclassifications
          if _taxclassifications is not initial.
            move-corresponding _taxclassifications to taxclassifications.
          else.
            clear wa_taxclass.
            refresh taxclassifications.
            wa_taxclass-depcountry = 'IN'.
            wa_taxclass-tax_ind    = '0'.
            append wa_taxclass to taxclassifications.
          endif.

          " Execute BAPI
          clear: return, wa_retmessage, ms_err_log_db.  " IHDK904473
          refresh: returnmessages.
          call function 'BAPI_MATERIAL_SAVEDATA'
            exporting
              headdata             = headdata
              clientdata           = clientdata
              clientdatax          = clientdatax
              plantdata            = plantdata
              plantdatax           = plantdatax
              storagelocationdata  = storagelocationdata
              storagelocationdatax = storagelocationdatax
              valuationdata        = valuationdata
              valuationdatax       = valuationdatax
            importing
              return               = return
            tables
              materialdescription  = materialdescription
              unitsofmeasure       = unitsofmeasure
              unitsofmeasurex      = unitsofmeasurex
              taxclassifications   = taxclassifications
              returnmessages       = returnmessages.
          read table returnmessages into wa_retmessage with key type = 'E' transporting message.
          if sy-subrc = 0.  " Error exists, log it in error log
            wa_log-log = wa_retmessage-message.
            append wa_log to it_log.
            clear: wa_retmessage.  " IHDK904473

            call function 'BAPI_TRANSACTION_ROLLBACK'.
          else. " No error
            if return-number = '356' and return-type = 'S'. " Check if creation/extension succeeded (MSG 356) and log it in status log
              call function 'BAPI_TRANSACTION_COMMIT' exporting wait = abap_true. " Rev 05

              wa_log-matnr = headdata-material.
              shift wa_data-werks left deleting leading '0'.
              wa_log-log = return-message && ` ` && 'to plant' && ` ` && wa_data-werks.

              extend_to_stor_loc( " IHDK900652
                exporting
                  material   = headdata-material
                  plant      = plantdata-plant
                  matl_type  = headdata-matl_type
                  division   = clientdata-division   " IHDK900685
                importing
                  ext_status = data(lv_ext_status) ).

              if lv_ext_status eq abap_false.
                wa_log-log = wa_log-log && '. SLoc Ext: Failed'.
              endif.

              append wa_log to it_log.
              clear: lv_ext_status. " IHDK904473
            endif.  " IF return-number = '356'
          endif.
          " End read returnmessages
        endif.
      endif.
      clear: wa_data.

      " IHDK904473
      ms_err_log_db-material = headdata-material.
      ms_err_log_db-main_act_status = wa_log-log.
      update_db_log( ).
      clear: wa_log.
    endloop.
  endmethod.
endclass.

class lcl_main implementation.
  method main.
* 1. class name = name of class based on selected radio button
    clear: cl_name. " type seoclsname
    free: lo.       " type ref to object(generic object ref)
    case abap_true.
      when p_rad1.
        move 'LCL_ZFGM_ZLFG_CREATE'    to cl_name.
      when p_rad2.
        move 'LCL_ZFGM_ZLFG_EXT_PLANT' to cl_name.
      when p_rad3.
        move 'LCL_ZFGM_ZLFG_EXT_DEPOT' to cl_name.
      when p_rad4.
        move 'LCL_ZTRD_CREATE'    to cl_name.
      when p_rad5.
        move 'LCL_ZTRD_EXT_PLANT' to cl_name.
      when p_rad6.
        move 'LCL_ZTRD_EXT_DEPOT' to cl_name.
      when p_rad7.
        move 'LCL_ZRAW_ZPKG_ZPKU_CREATE' to cl_name.
      when p_rad8.
        move 'LCL_ZRAW_ZPKG_ZPKU_EXT_PLT_DPT' to cl_name.
      when p_rad9.
        move 'LCL_ZSFG_CREATE' to cl_name.
      when p_rad10.
        move 'LCL_ZSFG_EXT_PLANT' to cl_name.
      when p_rad11.
        move 'LCL_ZMCO_CREATE' to cl_name.
      when p_rad12.
        move 'LCL_ZMCO_EXT_PLANT' to cl_name.
      when p_rad13.
        move 'LCL_YZSPR_YSPI_ZEU2_CREATE' to cl_name.
      when p_rad14.
        move 'LCL_YZSPR_YSPI_ZEU2_EXT_PLT' to cl_name.
      when others.
    endcase.
    if cl_name is not initial.
      try.
          create object lo type (cl_name). " Class name resolved @ runtime,
          " constructor of respective class is called implicitly
*        CATCH cx_sy_create_object_error.
*          RETURN.
        catch cx_root into lo_cx.
          message lo_cx->get_text( ) type 'S' display like 'E'.
          return.
      endtry.
    endif.
  endmethod.
endclass.
* ---- end class implementations ---- *
