*&---------------------------------------------------------------------*
*& Report ZMM_PRG_MATERIAL_MASTER_ES
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zmm_prg_material_master_usr.

tables: sscrfields.

* ---- selection  screen ---- *
selection-screen begin of block b1 with frame title text-001.
parameters: p_rad1 radiobutton group rad user-command abc default 'X',
            p_rad2 radiobutton group rad,
            p_rad3 radiobutton group rad,
            p_rad4 radiobutton group rad.
selection-screen end of block b1.

* Button for downloading file format for respective material type *
selection-screen:
begin of block b2 with frame title dwnld, "text-003,
  begin of line,
    pushbutton 2(15) text-004 user-command dwn,
  end of line,
end of block b2.

* File Selection *
selection-screen:
begin of block b3 with frame title upld, "text-002,
  begin of line,
    comment 1(79) text-005,
  end of line.
parameters:     p_file type string.
selection-screen:
end of block b3.

* ---- begin local exception class definition ---- *
class lcx_generic_error definition inheriting from cx_static_check.
  " Used in throwing a generic exception, Error displayed is based on context of the exception
endclass.
* ---- end exception class definition ---- *

* ---- begin local class definitions ---- *
class lcl_application definition.
  public section.
    class-methods:
      set_frame_title       " Set block-frame titles based on radio button selected(On selection screen)
        changing
          dwnld type any    " frame title of file format download block
          upld  type any,   " file upload block

      file_open             " Load file from user front-end
        raising lcx_generic_error,

      disp_message          " Generate message string and structure, fill message table(only 1 at a time), and display as a pop-up
        importing
          value(type) type bapiret2-type
          value(id)   type bapiret2-id
          value(no)   type bapiret2-number
          value(var1) type bapiret2-message_v1 optional
          value(var2) type bapiret2-message_v2 optional
          value(var3) type bapiret2-message_v3 optional
          value(var4) type bapiret2-message_v4 optional,

      dwn_file_format.

    methods: process.

  protected section.

  private section.
* Static data, accessed directly using class name, value is common and constant for all intances of the class
    class-data: it_seltext type fc00_t_sel_screen_textpool, " Selection texts for the program
                wa_seltext type textpool.
endclass.

class lcl_main definition.
  public section.
    class-methods: start.

  protected section.

  private section.
endclass.
* ---- end local class definitions ---- *

* ---- begin local class implementations ---- *
class lcl_application implementation.
  method file_open.
* file table containing selected files, used in file_open *
    data: it_filein type filetable,
          wa_filein type file_table,

          " No of files selected
          rc        type i.

* Parameters used in :
* cl_gui_frontend_services=>file_open_dialog *
* cl_gui_frontend_services=>file_save_dialog *
    data: file        type string,  " filename
          path        type string,  " directory path
          file_path   type string,  " full path: directory + filename with ext
          file_filter type string,  " save as
          title       type string,  " dialog title
          defname     type string,  " default file name
          defext      type string,  " default extension
          useraction  type i.       " button clicked by user

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

  method set_frame_title.

    clear: dwnld, upld, wa_seltext.
    move: text-003 to dwnld,
          text-002 to upld.
    if it_seltext is initial.
      call function 'FC_SEL_SCREEN_SELTEXTS_READ'
        exporting
          e_repid    = sy-repid
          e_tabname  = space
        importing
          it_seltext = it_seltext.
    endif.

    case abap_true.
      when p_rad1.
        read table it_seltext into wa_seltext with key key = 'P_RAD1'.
      when p_rad2.
        read table it_seltext into wa_seltext with key key = 'P_RAD2'.
      when p_rad3.
        read table it_seltext into wa_seltext with key key = 'P_RAD3'.
      when p_rad4.
        read table it_seltext into wa_seltext with key key = 'P_RAD4'.
      when others.
    endcase.
    condense wa_seltext-entry.
    concatenate: dwnld 'for' wa_seltext-entry into dwnld separated by space,
                 upld  'for' wa_seltext-entry into upld  separated by space.
    condense: dwnld, upld.
  endmethod.

  method disp_message.
* balw_bapireturn_get2 *
* finb_bapiret2_display *
    " Used in disp_message method
    data: it_messages type /eacc/t_bapiret2,
          wa_message  type bapiret2.

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

  method dwn_file_format.
    call function 'CALL_BROWSER'
      exporting
        url                    = 'https://goo.gl/dztGxE'
        window_name            = 'Download ZMAT01_USR File Formats'
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
  endmethod.

  method process.
    submit zmm_prg_material_master
      with p_rad1  = abap_false
      with p_rad2  = abap_false
      with p_rad3  = abap_false
      with p_rad4  = abap_false
      with p_rad5  = abap_false
      with p_rad6  = abap_false
      with p_rad7  = abap_false
      with p_rad8  = abap_false
      with p_rad9  = abap_false
      with p_rad10 = abap_false
      with p_rad11 = p_rad3
      with p_rad12 = p_rad4
      with p_rad13 = p_rad1
      with p_rad14 = p_rad2
      with p_file  = p_file
      and return.
  endmethod.

endclass.

class lcl_main implementation.
  method start.
    new lcl_application( )->process( ).
  endmethod.
endclass.
* ---- end local class implementations ---- *

* ---- selection screen events ---- *
at selection-screen output.
* Change frame titles on selection screen based on selected radio button *
  try.
      lcl_application=>set_frame_title(
        changing
          dwnld = dwnld
          upld  = upld ).
    catch cx_root into data(lo_cx).
      message lo_cx->get_text( ) type 'E'.
  endtry.

at selection-screen on value-request for p_file.
* Load file from user front-end/F4 for file *
  try.
      lcl_application=>file_open( ).
    catch lcx_generic_error.
      return.
  endtry.

at selection-screen.
* File is mandatory for execution *
  if sscrfields-ucomm eq 'ONLI' and p_file is initial.  " ONLI = Execute
    lcl_application=>disp_message(
      exporting
        type = 'E'
        id   = 'ZMM'
        no   = '003' ).
    try.
        raise exception type lcx_generic_error.
      catch lcx_generic_error.
        stop.
    endtry.
  endif.

* File format download *
  if sscrfields-ucomm eq 'DWN'.
    lcl_application=>dwn_file_format( ).
  endif.

* ---- start of selection ---- *
start-of-selection.
  lcl_main=>start( ).
