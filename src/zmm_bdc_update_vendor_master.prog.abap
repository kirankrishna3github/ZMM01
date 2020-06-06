*&---------------------------------------------------------------------*
*& Program  ZMM_BDC_UPDATE_VENDOR_MASTER
*&---------------------------------------------------------------------*
*& Transaction            : ZMM086
*& Creation Date          : Thursday, July 12, 2018 17:44:37
*& Author                 : 6010859 - SaurabhK
*& Functional             : Venugopal Menon
*& Requested/Approved By  : Treasury/AP team for VP module
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK932804
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*&---------------------------------------------------------------------*
*& 1. Goal is to create an all purpose vendor master update module
*& 2. Structure of the program: - Assumptions: T-Code used is FK02
*&    i. Prior to recording a bdc check whether the fields that you are going to record already exist in the file format
*&    ii. Record the bdc if all or some of the required fields are missing - record only the missing fields
*&    iii. Add the new fields to the excel structure
*&    iv. Create a structure m(x) containing newly recorded fields except lifnr and bukrs which are global fields extracted from excel itself
*&    v. Type of the fields in the excel structure should be 'excelcell' and in your 'm' structure should be actual datatype of those fields
*&    vi. Declare a new method meth(x) with the same signature as that of meth1, meth2 etc. This method will contain your actual bdc steps
*&    vii. Inside the update method, write code same as that of m1-meth1, m2-meth2 etc for your stucture(m(x)) and method(meth(x))
*&    viii. Implement your method meth(x) and write your bdc steps in it similar to the way done in meth1, meth2 etc. Remember to refresh...
*&    ... bdc_tab and conv your data to bdc_fval type as done in meth1, meth2. Trigger a call to the method call_bdc at the end of your meth(x)
*&    ix. Add your fields to the fieldcatalog
*&    x. If possible download the file format from the sel-screen; add your own fields; upload it to g-drive and update the link in the..
*&    ... download action(call browser)
*&---------------------------------------------------------------------*
*& Goal is to make this a universal vendor master update program. The structure of the program is such that new fields can be easily added.
*& Also existing fields can be supplied optionally, meaning fields that are not supplied will not be cleared or overwritten.
*& Currently, i have made provision for updating fields related to Vendor Payment module including address details,
*& pan, gst no and bank details. Instructions for adding new fields have been enumerated in the program. You may to add new fields
*& in the same program in the future rather than creating a new one. This will help reduce multiple bdc’s for the same purpose
*& and reduce duplication of efforts. if time permits, we will try to bring all the existing vendor master update programs
*& under this single program. File format can be downloaded from the selection screen.
*&---------------------------------------------------------------------*
*& * ---- Revision History ---- *
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Tuesday, July 17, 2018 23:04:48
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IRDK932865
*& Rev. Description           : MM: S_K: ZMM086: Remove koinh/mobile, email mandate: 17.7.18
*&---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Tuesday, January 29, 2019 15:04:37
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG/Bhupesh Adoni
*& Rev. Request#              : IHDK900384
*& Rev. Description           : MM: S_K: ZMM086: Add vendor GST class: 29.1.19
*&---------------------------------------------------------------------*
*& Revision #                 : 03
*& Rev. Date                  : Thursday, February 07, 2019 14:28:25
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG/Bhupesh Adoni
*& Rev. Request#              : IHDK900511, IHDK900517
*& Rev. Description           : MM: S_K: ZMM086: Separate address data and tax data: 7.2.19; Re-structuring
*&---------------------------------------------------------------------*
*& Revision #                 : 04
*& Rev. Date                  : Wedensday, May 15, 2019 12:03:25
*& Rev. Author                : ABAP01 - Sandeep Pore
*& Rev. Requested/Approved By : SK
*& Rev. Request#              : IHDK901704
*& Rev. Description           : ABAP::MM::Changes in ZMM086 added Method 4::15.05.2019::SP
*& Change Description         : Method 4 and 5 added to Update the Payment Terms and Recon. acc. of Vendor
*&---------------------------------------------------------------------*
program zmm_bdc_update_vendor_master.

* Tables
tables: sscrfields.

* Type-pools used
type-pools: icon.

constants:
  " task
  gc_object_task_insert  type bus_ei_object_task value 'I', " I  Insert
  gc_object_task_update  type bus_ei_object_task value 'U', " U  Update
  gc_object_task_modify  type bus_ei_object_task value 'M', " M  Modify
  gc_object_task_delete  type bus_ei_object_task value 'D', " D  Delete
  gc_object_task_current type bus_ei_object_task value 'C', " C  Current State
  gc_object_bp           type bu_object value 'BusinessPartner'.  " Object

* ---- Selection Screen and Global Data ---- *
selection-screen begin of block fil with frame title text-fil.
parameters: p_file type string default 'Drive:\Path_of_your_upload_file.xls'.  " Input File
selection-screen end of block fil.

selection-screen begin of block opt with frame title text-opt.
parameters: p_bg radiobutton group mod default 'X' modif id rad,
            p_fg radiobutton group mod modif id rad,
            p_er radiobutton group mod modif id rad.
selection-screen end of block opt.

" IRDK931592
selection-screen begin of block dwn with frame title text-dwn.
selection-screen: begin of line,
                    pushbutton 1(20) dwn_btn user-command dwn_format,
                    comment 25(70) text-wrn,
                  end of line.
selection-screen end of block dwn.

" local class definition
* ---- begin local exception class definition ---- *
class lcx_generic_error definition inheriting from cx_static_check.
  " Used in throwing a generic exception, Error displayed is based on context of the exception
endclass.
* ---- end exception class definition ---- *

class lcl_application definition.

  public section.
    class-methods: file_open.
    methods: process.

  private section.
    types: excelcell(255) type c.
    data: begin of excel_line,      " Add to this structure when new fields are required
            lifnr     type excelcell,   " vendor
            bukrs     type excelcell,   " company code
            ekorg     type excelcell,   " Purchase Org.
            regio     type excelcell,   " state code
            pstlz     type excelcell,   " postal code; IHDK905062
            mobil     type excelcell,   " mobile
            email     type excelcell,   " email
            panno     type excelcell,   " pan no
            ven_class type excelcell,   " vendor gst registration status
            gstno     type excelcell,   " gst no
            banks     type excelcell,   " bank country key
            bankl     type excelcell,   " bank ifsc code
            bankn     type excelcell,   " acc no
*            koinh type excelcell,   " acc name
            banka     type excelcell,   " bank name
            provz     type excelcell,   " bank state code
            stras     type excelcell,   " bank street/address
            ort01     type excelcell,   " bank city
            brnch     type excelcell,   " bank branch
            swift     type excelcell,   " bank swift code
            bnklz     type excelcell,   " bank code
            zwels     type excelcell,   " payment method
            hbkid     type excelcell,   " house bank
            c_zterm   type excelcell,   " Company Payment Terms
            akont     type excelcell,   " Recon acc no
            p_zterm   type excelcell,   " Purchasing Payment Terms
            bptype    type excelcell,   " BP Type
            qland     type excelcell,   " W/H Tax Country
            " Additional fields here...
          end of excel_line,
          excel like standard table of excel_line.

    " structure corresponding to meth1 bdc
    data: begin of m1, " add separate data structures for new fields added to file and create a corresponding bdc method
            regio type regio,
            pstlz type lfa1-pstlz,  " IHDK905062
            mobil type ad_tlnmbr,
            email type ad_smtpadr,
            " all address related fields can be and should be added here - get screen field names from F1 of those fields
          end of m1,
          m1_tab like standard table of m1,

          begin of m2,
            panno     type j_1ipanno,
            ven_class type lfa1-ven_class,
            gstno     type stcd3,
          end of m2,
          m2_tab like standard table of m2,

          " structure corresponding to meth2 bdc
          begin of m3,
            banks type banks,
            bankl type bankl,
            bankn type bankn,
            koinh type koinh_fi,
            banka type banka,
            provz type provz,
            stras type stras,
            ort01 type ort01,
            brnch type brnch,
            swift type swift,
            bnklz type bankl,
            zwels type dzwels,
            hbkid type hbkid,
          end of m3,
          m3_tab like standard table of m3,

          " structure corresponding to meth4 bdc
          begin of m4,
            p_zterm type lfb1-zterm,     " Purchasing Payment Terms
          end of m4,
          m4_tab like standard table of m4,

          " structure corresponding to meth5 bdc
          begin of m5,
            akont   type lfb1-akont,     " Recon Acc. no.
            c_zterm type lfb1-zterm,     " Company Payment Terms
          end of m5,
          m5_tab like standard table of m5,

          " structure corresponding to meth6 bdc
          begin of m6,
            bptype type tb004-bpkind,    " BP Type
          end of m6,
          m6_tab like standard table of m6,

          begin of m7,
            qland type lfb1-qland,
          end of m7.

    " Your structure here...

    data: begin of output_line.           " ALV output/Log
    data:   icon type icon_d.
        include structure excel_line.
    data: name1    type name1_gp,
          msg(255) type c,    " result
          end of output_line,
          output like standard table of output_line.

    data: begin of msg_log,
            index     type syindex,
            m_name(5) type c,
            msg_typ   type bdcmsgcoll-msgtyp,
            msg       type bapi_msg,
          end of msg_log,
          msg_logs like standard table of msg_log with non-unique sorted key index components index.

*       Batchinputdata of single transaction
    data: bdc_tab type standard table of bdcdata,
          bdc_wa  like line of bdc_tab.
*       messages of call transaction
    data: mess_tab type standard table of bdcmsgcoll,
          mess_wa  like line of mess_tab.

    data: mode(1) type c.

    data: msg(255) type c.
    data: gv_vendor type lfa1-lifnr.
    data: gv_comp  type lfb1-bukrs.
    data: gv_ekorg type lfm1-ekorg.

    constants: no_data(1) type c value '/'. " This helps in avoiding overwriting of non-supplied values

    methods:
      set_mode,
      file_to_tab,
      check_file_format raising lcx_generic_error,
      update,
      meth1
        importing
          index type i, " update data in m1
      meth2
        importing
          index type i, " update data in m2
      meth3
        importing
          index type i, " update data in m3
      meth4
        importing
          index type i, " update data in m4
      meth5
        importing
          index type i, " update data in m5
      meth6
        importing
          index type i, " update data in m6
      meth7
        importing
          index type i, " update data in m7

      " Your method definition here
      conversion_exit
        changing
          m type any,

      call_bdc
        importing
          iv_tcode type sy-tcode
          m_name   type any
          index    type i,

      bdc_dynpro
        importing
          program type bdcdata-program
          dynpro  type bdcdata-dynpro,

      bdc_field
        importing
          fnam type bdcdata-fnam
          fval type any,  "bdcdata-fval,

      call_bapi
        importing
          it_data type cvis_ei_extern_t
          m_name  type any
          index   type i,

      display_log,

      hotspot_click for event link_click of cl_salv_events_table  " event handler
        importing
            row
            column.
endclass.

class main definition.
  public section.
    class-methods: start.
endclass.

class lcl_application implementation.
  method process.
    set_mode( ).

    file_to_tab( ).

    if excel is not initial.
      try.
          check_file_format( ).
        catch lcx_generic_error.
          return.
      endtry.
      update( ).
    else.
      message 'No data could be read' type 'I' display like 'E'.
      return.
    endif.

    if output is not initial.
      display_log( ).
    else.
      message 'No log generated' type 'S' display like 'E'.
      return.
    endif.
  endmethod.

  method set_mode.
    clear mode.
    case abap_true.
      when p_bg.
        mode = 'N'.
      when p_fg.
        mode = 'A'.
      when p_er.
        mode = 'E'.
      when others.
    endcase.
  endmethod.

  method file_to_tab.
    data i_tab_raw_data type truxs_t_text_data.
    data: lv_msg type string.

    refresh excel.
    call function 'TEXT_CONVERT_XLS_TO_SAP' " IHDK900159
      exporting
        i_field_seperator    = abap_true
        i_line_header        = abap_true
        i_tab_raw_data       = i_tab_raw_data
        i_filename           = conv rlgrap-filename( p_file )
      tables
        i_tab_converted_data = excel
      exceptions
        conversion_failed    = 1
        others               = 2.
    if sy-subrc <> 0.
* Implement suitable error handling here
      if sy-msgid eq 'UX' and sy-msgno eq '893'.  " Replace file cannot be processed with a meaningful error
        clear lv_msg.
        concatenate 'Is file' p_file 'still open? Please close the file.' into lv_msg separated by space.
        message lv_msg type 'S' display like 'E'.
      else.
        message id sy-msgid
        type 'S'
        number sy-msgno
        with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like 'E'.
      endif.
    endif.

  endmethod.

  method check_file_format.
    data: lo_struct type ref to cl_abap_structdescr,
          lo_table  type ref to cl_abap_tabledescr,
          lt_comp   type cl_abap_structdescr=>component_table,
          ls_comp   like line of lt_comp.

    free: lo_table, lo_struct.
    refresh lt_comp.
    lo_table  ?=  cl_abap_structdescr=>describe_by_data( excel[] ).
    lo_struct ?=  lo_table->get_table_line_type( ).
    lt_comp   =   lo_struct->get_components( ).

    clear excel_line.
    read table excel into excel_line index 1.
    if sy-subrc = 0.
      do.
        assign component sy-index of structure excel_line to field-symbol(<fs>).
        if sy-subrc <> 0.
          exit.
        endif.
        if <fs> is assigned.
          condense <fs>.
          <fs> = shift_left( val = <fs> sub = '' ).

          clear ls_comp.
          read table lt_comp into ls_comp index sy-index.
          if sy-subrc = 0 and to_upper( ls_comp-name ) <> to_upper( <fs> ).
            message ls_comp-name && ': Field sequence altered. Please check your file format' type 'S' display like 'E'.
            raise exception type lcx_generic_error.
          endif.
        endif.
        unassign <fs>.
      enddo.
    endif.
    delete excel index 1.
  endmethod.

  method update.
    data: lv_index  type i.

    " IHDK905062
    select single @abap_true
      from usr05
      where bname = @sy-uname
      and parid = 'ZMM086_GST_ONLY'
      and parva = @abap_true
      into @data(lv_gst_only).

    if lv_gst_only = abap_true.
      select param1 as name
        from z6mma_params
        where progname = 'ZMM086_GST_FIELDS'
        into table @data(lt_gst_fields).
    endif.

    data(lo_table) = cast cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( exporting p_data = excel ) ).
    if lo_table is bound.
      data(lo_struct) = cast cl_abap_structdescr( lo_table->get_table_line_type( ) ).
      if lo_struct is bound.
        data(lt_comp) = lo_struct->get_components( ).
      endif.
    endif.

    clear msg_logs.
    check excel is not initial.

    clear: excel_line, lv_index.
    loop at excel into excel_line.
      lv_index = lv_index + 1.
      clear: output_line, gv_vendor, gv_comp, gv_ekorg, m1, m2, m3.
      move-corresponding excel_line to output_line. " prepare log

      loop at lt_comp into data(ls_comp).
        assign component ls_comp-name of structure excel_line to field-symbol(<fs>).
        if sy-subrc <> 0.
          exit.
        endif.
        if <fs> is assigned.
          condense <fs>.
          <fs> = shift_left( val = <fs> sub = ' ' ).

          if lt_gst_fields is not initial.
            if not line_exists( lt_gst_fields[ name = ls_comp-name ] ).
              <fs> = no_data.
            endif.
          endif.
        endif.
        unassign <fs>.
        clear ls_comp.
      endloop.
      " End IHDK905062

      gv_vendor = excel_line-lifnr.
      gv_vendor = |{ gv_vendor alpha = in }|.
      gv_comp  = excel_line-bukrs.
      gv_ekorg = excel_line-ekorg.
      if gv_vendor is initial.
        gv_vendor = no_data.
      endif.
      if gv_comp is initial.
        gv_comp = no_data.
      endif.

*--------------------------------------------------------------------*
  " Vendor level validations - No update if any of these fails
*--------------------------------------------------------------------*
      select single * from lfa1 where lifnr = @gv_vendor into @data(ls_lfa1).

      output_line-name1 = ls_lfa1-name1.

      if gv_vendor is initial or gv_vendor = no_data or sy-subrc <> 0.
        output_line-icon = icon_red_light.
        output_line-msg = 'Err - Invalid vendor code supplied'.
        data(lv_lifnr_err) = abap_true.
      endif.

      select single smtp_addr
        from adr6
        into @data(lv_email)
        where addrnumber = @ls_lfa1-adrnr.

      select single tel_number
        from adr2
        into @data(lv_mobile)
        where addrnumber = @ls_lfa1-adrnr
        and   r3_user = '3'.

      " mobile no and email are mandatory; IRDK932865
      if ( lv_email is initial and excel_line-email is initial ).
        output_line-icon = icon_red_light.
        output_line-msg = output_line-msg && ` ` && 'Err - Email ID not supplied/not maintained'.
        lv_lifnr_err = abap_true.
        data(lv_m1_err) = abap_true.
      endif.

      if ( lv_mobile is initial and excel_line-mobil is initial ).
        output_line-icon = icon_red_light.
        output_line-msg = output_line-msg && ` ` && 'Err - Mobile Number not supplied/not maintained'.
        lv_lifnr_err = abap_true.
        lv_m1_err = abap_true.
      endif.

      if ( ls_lfa1-stcd3 is initial and excel_line-gstno is initial ).
        output_line-icon = icon_red_light.
        output_line-msg = output_line-msg && ` ` && 'Err - GST No. not supplied/not mainained'.
        lv_lifnr_err = abap_true.
        lv_m1_err = abap_true.
      endif.

      " validate gst number
      try.
          zcl_bupa_utilities=>validate_gst_number(
            exporting
              iv_entity      = gv_vendor
              iv_state       = cond #( when m1-regio is not initial
                                       then m1-regio
                                       else ls_lfa1-regio ) " Region (State, Province, County)
              iv_gst_number  = cond #( when m2-gstno is not initial
                                       then m2-gstno
                                       else ls_lfa1-stcd3 ) ). " GST Number
        catch zcx_generic into data(lox_generic). " Generic Exception Class
          output_line-icon = icon_red_light.
          output_line-msg = output_line-msg && ` ` && |Err - { lox_generic->get_text( ) }|.
          lv_lifnr_err = abap_true.
          lv_m1_err = cond #( when m1-regio is not initial then abap_true else lv_m1_err ).
          data(lv_m2_err) = cond #( when m2-gstno is not initial then abap_true ).
      endtry.

      " validate postal code
      clear lox_generic.
      try.
          zcl_bupa_utilities=>validate_postal_code(
            exporting
              iv_entity      = gv_vendor
              iv_state       = cond #( when m1-regio is not initial
                                       then m1-regio
                                       else ls_lfa1-regio ) " Region (State, Province, County)
              iv_country     = ls_lfa1-land1
              iv_postal_code = cond #( when m1-pstlz is not initial
                                       then m1-pstlz
                                       else ls_lfa1-pstlz ) ). " Postal Code
        catch zcx_generic into lox_generic. " Generic Exception Class
          output_line-icon = icon_red_light.
          output_line-msg = output_line-msg && ` ` && |Err - { lox_generic->get_text( ) }|.
          lv_lifnr_err = abap_true.
          lv_m1_err = cond #( when m1-regio is not initial or m1-pstlz is not initial then abap_true else lv_m1_err ).
      endtry.

      " validate pan number - IHDK905932 - Friday, June 05, 2020 22:53:22
      " Subject: RE: PAN validation | From: Prajay Bhansali [mailto:pbhansali@indofil.com] | Sent: Monday, June 1, 2020 17:09
      clear lox_generic.
      try.
          zcl_bupa_utilities=>validate_pan_number(
            exporting
              iv_entity      = gv_vendor
              iv_pan_number  = cond #( when m2-panno is not initial
                                       then m2-panno
                                       else ls_lfa1-j_1ipanno ) " PAN Number
              iv_first_name  = ls_lfa1-name1
              iv_last_name   = ls_lfa1-name2
              iv_title       = ls_lfa1-anred ).
        catch zcx_generic into lox_generic. " Generic Exception Class
          output_line-icon = icon_red_light.
          output_line-msg = output_line-msg && ` ` && |Err - { lox_generic->get_text( ) }|.
          lv_lifnr_err = abap_true.
          lv_m2_err = cond #( when m2-panno is not initial then abap_true else lv_m2_err ).
      endtry.
*--------------------------------------------------------------------*

      if lv_lifnr_err eq abap_false.  " error in vendor, no further processing
        " M1
        if lv_m1_err eq abap_false. " m1 validation error
          move-corresponding excel_line to m1.
          if m1 is not initial.
            conversion_exit( changing m = m1 ).
            meth1( exporting index = lv_index ).
          endif.
        endif.

        " M2
        move-corresponding excel_line to m2.
        if m2 is not initial.
          if gv_comp is initial or gv_comp eq no_data.
            output_line-icon = icon_red_light.
            output_line-msg = output_line-msg && ` ` && 'M2:Err - Company code is mandatory for tax details'.
            lv_m2_err = abap_true.
          endif.
          select single * from usrm0 into @data(lv_cin_check) where uname eq @sy-uname.
          if lv_cin_check is initial.
            output_line-icon = icon_red_light.
            output_line-msg = output_line-msg && ` ` && 'M2:Err - OMT3E - No authority for maintaining CIN details'.
            lv_m2_err = abap_true.
          endif.

          if lv_m2_err eq abap_false. " m2 validation error
            conversion_exit( changing m = m2 ).
            meth2( exporting index = lv_index ).

            call function 'ENQUEUE_EXLFA1'
              exporting
                lifnr          = gv_vendor
              exceptions
                foreign_lock   = 1
                system_failure = 2
                others         = 3.
            if sy-subrc = 0.
              if m2-panno is not initial and m2-panno ne no_data. " IHDK902779
* Implement suitable error handling here
                update lfa1 set j_1ipanno = m2-panno
                                j_1ipanref = m2-panno
                            where lifnr = gv_vendor.

                if sy-dbcnt = 1.
                  commit work.
                endif.
              endif.

              " IHDK900384
              if m2-ven_class is not initial and m2-ven_class ne no_data. " IHDK902779
                if to_upper( m2-ven_class ) eq 'X'.
                  clear m2-ven_class.
                endif.
                update lfa1 set ven_class = m2-ven_class
                            where lifnr = gv_vendor.

                if sy-dbcnt = 1.
                  commit work.
                endif.

              endif.
              " end IHDK900384
              call function 'DEQUEUE_EXLFA1'
                exporting
                  lifnr = gv_vendor.
            endif.
          endif.
        endif.

        " M3
        move-corresponding excel_line to m3.
        if m3 is not initial.
          if gv_comp is initial or gv_comp eq no_data.
            output_line-icon = icon_red_light.
            output_line-msg = output_line-msg && ` ` && 'M3:Err - Company code is mandatory for bank details'.
            data(lv_m3_err) = abap_true.
          endif.
          if lv_m3_err eq abap_false. " m3 validation error
            conversion_exit( changing m = m3 ).
            meth3( exporting index = lv_index ).
          endif.
        endif.

        " M4
        move-corresponding excel_line to m4.
        if m4 is not initial.
          if gv_ekorg is initial.
            output_line-icon = icon_red_light.
            output_line-msg = output_line-msg && ` ` && 'M4:Err - Purchasing organisation is mandatory for purchasing data'.  " IHDK902056
            data(lv_m4_err) = abap_true.
          endif.

          if lv_m4_err eq abap_false. " m4 validation error
            conversion_exit( changing m = m4 ).
            meth4( exporting index = lv_index ).
          endif.
        endif.

        " M5
        move-corresponding excel_line to m5.
        if m5 is not initial.
          if gv_comp is initial or gv_comp eq no_data.  " IHDK902058
            output_line-icon = icon_red_light.
            output_line-msg = output_line-msg && ` ` && 'M5:Err - Company code is mandatory for company code data'. " IHDK902056
            data(lv_m5_err) = abap_true.
          endif.

          if lv_m5_err eq abap_false. " m5 validation error
            conversion_exit( changing m = m5 ).
            meth5( exporting index = lv_index ).
          endif.
        endif.

        " M6
        move-corresponding excel_line to m6.
        if m6 is not initial.
          conversion_exit( changing m = m6 ).
          meth6( exporting index = lv_index ).
        endif.

        " M7
        move-corresponding excel_line to m7.
        if m7 is not initial.
          if gv_comp is initial or gv_comp eq no_data.  " IHDK902058
            output_line-icon = icon_red_light.
            output_line-msg = output_line-msg && ` ` && 'M5:Err - Company code is mandatory for company code data'. " IHDK902056
            data(lv_m7_err) = abap_true.
          endif.

          if lv_m7_err = abap_false.
            conversion_exit( changing m = m7 ).
            meth7( exporting index = lv_index ).
          endif.
        endif.

        " Similarly for M8 and so on
      endif.
      append output_line to output.
      clear:
        excel_line, lv_email, lv_mobile, lv_lifnr_err, lv_m1_err,
        lv_m2_err, lv_m3_err, lv_m4_err, lv_m5_err, lv_m7_err.
    endloop.
  endmethod.

  method conversion_exit.
* local variables for method: convert to internal format *  " RTTS: ABAP Run Time Type Services
    data: lo_descr          type ref to cl_abap_typedescr.  " type info of supplied data
    data: lv_relative_name  type string.    " data element of supplied variable
    data: lv_data_element   type rollname.
    data: lv_function       type funcname.  " CONV. EXIT Name
    data: wa_dd04l          type dd04l.     " Data elements - get converion exit for data element

    do.
      assign component sy-index of structure m to field-symbol(<fs>).
      if sy-subrc <> 0.
        exit.
      endif.
      if <fs> is assigned and <fs> is not initial and <fs> <> no_data.  " IHDK905062
        lo_descr = cl_abap_typedescr=>describe_by_data( p_data = <fs> ).

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
                        input  = <fs>
                      importing
                        output = <fs>
                      exceptions
                        others = 1.
                  catch cx_sy_dyn_call_illegal_func.
                    " catch-block
                endtry.
              endif.
            endif.
          endif.
        endif.
      elseif <fs> is assigned and <fs> is initial.
        <fs> = no_data. " This avoids unintentional clearing of existing non-supplied values
      endif.
      unassign: <fs>.
      clear: lo_descr, lv_relative_name, lv_data_element, wa_dd04l, lv_function.
    enddo.
  endmethod.

  method meth1. " all address related fields can be and should be added here - get screen field names from F1 of those fields
    refresh bdc_tab.

    bdc_dynpro( exporting program = 'SAPMF02K'    dynpro  = '0106' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'  fval    = 'USE_ZAV' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'  fval    = '/00' ).
    bdc_field(  exporting fnam    = 'RF02K-LIFNR' fval    = gv_vendor ).
    bdc_field(  exporting fnam    = 'RF02K-D0110' fval    = 'X' ).
    bdc_field(  exporting fnam    = 'USE_ZAV'     fval    = 'X' ).

    bdc_dynpro( exporting program = 'SAPMF02K'              dynpro  = '0111' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'            fval    = '=UPDA' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'            fval    = 'SZA1_D0100-SMTP_ADDR' ).
    bdc_field(  exporting fnam    = 'ADDR1_DATA-REGION'     fval    = m1-regio ).
    bdc_field(  exporting fnam    = 'ADDR1_DATA-POST_CODE1' fval    = m1-pstlz ). " IHDK905062
    bdc_field(  exporting fnam    = 'SZA1_D0100-MOB_NUMBER' fval    = m1-mobil ).
    bdc_field(  exporting fnam    = 'SZA1_D0100-SMTP_ADDR'  fval    = m1-email ).
    " all address related fields can be and should be added here - get screen field names from F1 of those fields

    call_bdc( exporting iv_tcode = 'FK02' m_name = 'M1' index = index ).

* --- Update Modile no in BP
    data: lt_data type cvis_ei_extern_t.  " complex BP interface

    " get business partner assigned to vendor
    call method zcl_bupa_utilities=>get_bp_assigned_to_vendor
      exporting
        iv_vendor       = gv_vendor
      importing
        ev_partner      = data(lv_partner)
        ev_partner_guid = data(lv_partner_guid).

    if lv_partner is not initial. " Do not proceed if B-partner does not exist.
      append initial line to lt_data assigning field-symbol(<ls_data>).
      if <ls_data> is assigned.

        " Inform the system that the BP is being updated
        <ls_data>-partner-header-object_task = gc_object_task_update.
        <ls_data>-partner-header-object = gc_object_bp.
        <ls_data>-partner-header-object_instance-bpartner = lv_partner.
        <ls_data>-partner-header-object_instance-bpartnerguid = lv_partner_guid.

        " Inform the system that the customer is being updated
        <ls_data>-vendor-header-object_instance-lifnr = gv_vendor.
        <ls_data>-vendor-header-object_task = gc_object_task_update.

        append initial line to <ls_data>-partner-central_data-communication-phone-phone assigning field-symbol(<ls_phone>).
        if <ls_phone> is assigned.
          <ls_phone>-contact-task     = gc_object_task_insert.
          <ls_phone>-contact-data-telephone   = m1-mobil.
          <ls_phone>-contact-data-r_3_user    = '3'.
          <ls_phone>-contact-data-home_flag   = abap_true.
          <ls_phone>-contact-data-std_no      = abap_true.

          <ls_phone>-contact-datax-telephone  = abap_true.
          <ls_phone>-contact-datax-r_3_user   = abap_true.
          <ls_phone>-contact-datax-home_flag  = abap_true.
          <ls_phone>-contact-datax-std_no     = abap_true.
        endif.
      endif.

*--------------------------------------------------------------------* End Data Fill
      call_bapi( exporting it_data = lt_data m_name = 'M1' index = index ).
    endif.

  endmethod.

  method meth2.
    refresh bdc_tab.

    bdc_dynpro( exporting program = 'SAPMF02K'    dynpro  = '0106' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'  fval    = 'USE_ZAV' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'  fval    = '/00' ).
    bdc_field(  exporting fnam    = 'RF02K-LIFNR' fval    = gv_vendor ).
    bdc_field(  exporting fnam    = 'RF02K-BUKRS' fval    = gv_comp ).
    bdc_field(  exporting fnam    = 'RF02K-D0120' fval    = 'X' ).
    bdc_field(  exporting fnam    = 'USE_ZAV'     fval    = 'X' ).

    bdc_dynpro( exporting program = 'SAPMF02K'   dynpro = '0120' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE' fval   = '=OPFI' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR' fval   = 'LFA1-KUNNR' ).
    bdc_field(  exporting fnam = 'LFA1-STCD3' fval = m2-gstno ).
    " all address related fields can be and should be added here - get screen field names from F1 of those fields

    bdc_dynpro( exporting program = 'SAPLJ1I_MASTER' dynpro = '0100' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'     fval   = '=CIN_VENDOR_FC3' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'     fval   = 'GS_LFA1-J_1IEXCD' ). " IHDK900075

    bdc_dynpro( exporting program = 'SAPLJ1I_MASTER'      dynpro  = '0100' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'          fval    = '=CIN_VENDOR_FC5' ).  " IHDK900384
    bdc_field(  exporting fnam    = 'BDC_CURSOR'          fval    = 'GS_LFA1-J_1IPANNO' ). " IHDK900075, IHDK900077
    bdc_field(  exporting fnam    = 'GS_LFA1-J_1IPANNO'   fval    = m2-panno ). " IHDK900073
    bdc_field(  exporting fnam    = 'GS_LFA1-J_1IPANREF'  fval    = m2-panno ). " IHDK900073

    " IHDK900384
    bdc_dynpro( exporting program = 'SAPLJ1I_MASTER' dynpro = '0100' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'     fval   = '=BACK' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'     fval   = 'J_1IMOVEND-VEN_CLASS' ).
    if to_upper( m2-ven_class ) eq 'X'.
      clear m2-ven_class.
    endif.
    bdc_field(  exporting fnam    = 'J_1IMOVEND-VEN_CLASS'  fval = m2-ven_class ).
    " end IHDK900384

    bdc_dynpro( exporting program = 'SAPMF02K'   dynpro = '0120' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR' fval   = 'LFA1-STCD3' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE' fval   = '=UPDA' ).
    bdc_field(  exporting fnam    = 'LFA1-STCD3' fval   = m2-gstno ).

    call_bdc( exporting iv_tcode = 'FK02' m_name = 'M2' index = index ).
  endmethod.

  method meth3.
    refresh bdc_tab.

    bdc_dynpro( exporting program = 'SAPMF02K'    dynpro  = '0106' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'  fval    = 'RF02K-D0215' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'  fval    = '/00' ).
    bdc_field(  exporting fnam    = 'RF02K-LIFNR' fval    = gv_vendor ).
    bdc_field(  exporting fnam    = 'RF02K-BUKRS' fval    = gv_comp  ). " IHDK902054
    bdc_field(  exporting fnam    = 'RF02K-D0130' fval    = 'X' ).
    bdc_field(  exporting fnam    = 'RF02K-D0215' fval    = 'X' ).

    bdc_dynpro( exporting program = 'SAPMF02K'        dynpro  = '0130' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'      fval    = 'LFBK-BANKL(01)' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'      fval    = '=BANK' ).
    bdc_field(  exporting fnam    = 'LFBK-BANKS(01)'  fval    = m3-banks ).
    bdc_field(  exporting fnam    = 'LFBK-BANKL(01)'  fval    = m3-bankl ).
    bdc_field(  exporting fnam    = 'LFBK-BANKN(01)'  fval    = m3-bankn ).
    if m3-koinh = no_data.
      m3-koinh = output_line-name1.
    endif.
    " Banks do not accept these charaters in acc name for security purposes, so remove these characters
    bdc_field(  exporting fnam = 'LFBK-KOINH(01)' fval = conv bdc_fval( translate( val = m3-koinh from = '$":,&^+~`!|{}?\;<>@#%*' to = '' ) ) ).

    bdc_dynpro( exporting program = 'SAPLBANK'   dynpro = '0100' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR' fval   = 'BNKA-BNKLZ' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE' fval   = '=ENTR' ).
    bdc_field(  exporting fnam    = 'BNKA-BANKA' fval   = m3-banka ).
    bdc_field(  exporting fnam    = 'BNKA-PROVZ' fval   = m3-provz ).
    bdc_field(  exporting fnam    = 'BNKA-STRAS' fval   = m3-stras ).
    bdc_field(  exporting fnam    = 'BNKA-ORT01' fval   = m3-ort01 ).
    bdc_field(  exporting fnam    = 'BNKA-BRNCH' fval   = m3-brnch ).
    bdc_field(  exporting fnam    = 'BNKA-SWIFT' fval   = m3-swift ).
    bdc_field(  exporting fnam    = 'BNKA-BNKLZ' fval   = m3-bnklz ).


    bdc_dynpro( exporting program = 'SAPMF02K'   dynpro = '0130' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR' fval   = 'LFBK-BANKL(01)' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE' fval   = '=ENTR' ).

    bdc_dynpro( exporting program = 'SAPMF02K'   dynpro = '0215' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR' fval   = 'LFB1-HBKID' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE' fval   = '=UPDA' ).
    bdc_field(  exporting fnam    = 'LFB1-ZWELS' fval   = m3-zwels ).
    bdc_field(  exporting fnam    = 'LFB1-HBKID' fval   = m3-hbkid ).

    call_bdc( exporting iv_tcode = 'FK02' m_name = 'M3' index = index ).
  endmethod.

  method meth4.     " Update Payment Terms
    refresh bdc_tab.

    bdc_dynpro( exporting program = 'SAPMF02K'    dynpro = '0101' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR'  fval   = 'RF02K-D0310' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE'  fval   = '/00' ).
    bdc_field(  exporting fnam    = 'RF02K-LIFNR' fval   =  gv_vendor  ).
    bdc_field(  exporting fnam    = 'RF02K-EKORG' fval   =  gv_ekorg ).

    bdc_field(  exporting fnam    = 'RF02K-D0310' fval   = 'X' ).

    bdc_dynpro( exporting program =  'SAPMF02K'  dynpro = '0310' ).
    bdc_field(  exporting fnam    = 'BDC_CURSOR' fval   = 'LFM1-WAERS' ).
    bdc_field(  exporting fnam    = 'BDC_OKCODE' fval   = '=UPDA' ).

    bdc_field(  exporting fnam    = 'LFM1-ZTERM' fval   =  m4-p_zterm ).

    call_bdc( exporting iv_tcode = 'XK02' m_name = 'M4' index = index ).

  endmethod.

  method meth5.     " Update Recon. Acc. No. and Payment Terms
    refresh bdc_tab.

    bdc_dynpro( exporting program     = 'SAPMF02K'    dynpro = '0101' ).
    bdc_field(  exporting fnam        = 'BDC_CURSOR'  fval   = 'RF02K-D0215' ).
    bdc_field(  exporting fnam        = 'BDC_OKCODE'  fval   = '/00' ).
    bdc_field(  exporting fnam        = 'RF02K-LIFNR' fval   = gv_vendor ).
    bdc_field(  exporting fnam        = 'RF02K-BUKRS' fval   = gv_comp ).
    bdc_field(  exporting fnam        = 'RF02K-D0210' fval   = 'X' ).
    bdc_field(  exporting fnam        = 'RF02K-D0215' fval   = 'X' ).

    bdc_dynpro(  exporting program    = 'SAPMF02K'    dynpro = '0210' ).
    bdc_field(  exporting fnam        = 'BDC_CURSOR'  fval   = 'LFB1-AKONT' ).
    bdc_field(  exporting fnam        = 'BDC_OKCODE'  fval   = '/00' ).
    bdc_field(  exporting fnam        = 'LFB1-AKONT'  fval   = m5-akont ).

    bdc_dynpro(  exporting program    = 'SAPMF02K'    dynpro = '0215' ).
    bdc_field(  exporting fnam        = 'BDC_CURSOR'  fval   = 'LFB1-ZTERM' ).
    bdc_field(  exporting fnam        = 'BDC_OKCODE'  fval   = '=UPDA' ).
    bdc_field(  exporting fnam        = 'LFB1-ZTERM'  fval   = m5-c_zterm ).

    call_bdc( exporting iv_tcode = 'XK02' m_name = 'M5' index = index ).

  endmethod.

  method meth6. " Update BP Type

    data: lt_data type cvis_ei_extern_t.  " complex BP interface

    " get business partner assigned to vendor
    call method zcl_bupa_utilities=>get_bp_assigned_to_vendor
      exporting
        iv_vendor       = gv_vendor
      importing
        ev_partner      = data(lv_partner)
        ev_partner_guid = data(lv_partner_guid).

    if lv_partner is not initial. " Do not proceed if B-partner does not exist.
      append initial line to lt_data assigning field-symbol(<ls_data>).
      if <ls_data> is assigned.

        " Inform the system that the BP is being updated
        <ls_data>-partner-header-object_task = gc_object_task_update.
        <ls_data>-partner-header-object = gc_object_bp.
        <ls_data>-partner-header-object_instance-bpartner = lv_partner.
        <ls_data>-partner-header-object_instance-bpartnerguid = lv_partner_guid.

        " Inform the system that the customer is being updated
        <ls_data>-vendor-header-object_instance-lifnr = gv_vendor.
        <ls_data>-vendor-header-object_task = gc_object_task_update.

        if m6-bptype is not initial.
          <ls_data>-partner-central_data-common-data-bp_centraldata-bapi-partnertype = m6-bptype.
          <ls_data>-partner-central_data-common-datax-bp_centraldata-bapix-partnertype = abap_true.
        endif.

      endif.

*--------------------------------------------------------------------* End Data Fill
      call_bapi( exporting it_data = lt_data m_name = 'M6' index = index ).
    endif.

  endmethod.

  method meth7. " Update BP Type

    data: lt_data type cvis_ei_extern_t.  " complex BP interface

    " get business partner assigned to vendor
    call method zcl_bupa_utilities=>get_bp_assigned_to_vendor
      exporting
        iv_vendor       = gv_vendor
      importing
        ev_partner      = data(lv_partner)
        ev_partner_guid = data(lv_partner_guid).

    if lv_partner is not initial. " Do not proceed if B-partner does not exist.
      append initial line to lt_data assigning field-symbol(<ls_data>).
      if <ls_data> is assigned.

        " Inform the system that the BP is being updated
        <ls_data>-partner-header-object_task = gc_object_task_update.
        <ls_data>-partner-header-object = gc_object_bp.
        <ls_data>-partner-header-object_instance-bpartner = lv_partner.
        <ls_data>-partner-header-object_instance-bpartnerguid = lv_partner_guid.

        " Inform the system that the customer is being updated
        <ls_data>-vendor-header-object_instance-lifnr = gv_vendor.
        <ls_data>-vendor-header-object_task = gc_object_task_update.

        append initial line to <ls_data>-vendor-company_data-company assigning field-symbol(<ls_comp>).
        if <ls_comp> is assigned.
          <ls_comp>-task = gc_object_task_update.
          <ls_comp>-data_key-bukrs = gv_comp.
          if m7-qland is not initial.
            <ls_comp>-data-qland = m7-qland.
            <ls_comp>-datax-qland = abap_true.
          endif.
        endif.
      endif.
*--------------------------------------------------------------------* End Data Fill
      call_bapi( exporting it_data = lt_data m_name = 'M7' index = index ).
    endif.

  endmethod.

  " Your method implementation here...

  method call_bapi.
    check it_data is not initial.
    " dummy check for future use; we need the update to happen irrespective of validation status
    " since customer block/unblock is a business crtitcal event
    cl_md_bp_maintain=>validate_single(
      exporting
        i_data        = it_data[ 1 ]
      importing
        et_return_map = data(lt_return_validation) ).

    try.
        " requires explicit commit
        cl_md_bp_maintain=>maintain(
          exporting
            i_data    = it_data
          importing
            e_return  = data(lt_return) ).

      catch cx_sy_dyn_call_illegal_func into data(err).
        data(message) = err->get_text( ).
    endtry.
*--------------------------------------------------------------------* End BAPI Call
    read table lt_return into data(ls_return) index 1.  " since we send only 1 bp
    if sy-subrc = 0.
      delete ls_return-object_msg where type eq 'S' or type eq 'W' or type eq 'I'.
    endif.

    if ls_return-object_msg is initial.
      call function 'BAPI_TRANSACTION_COMMIT'
        exporting
          wait = abap_true.
* Success
      if output_line-icon ne icon_red_light and output_line-icon ne icon_yellow_light.  " all ok only previous methods haven't reported errors
        output_line-icon = icon_green_light.
      endif.
      output_line-msg = output_line-msg && ` ` && m_name && ':' && 'OK'.  " Update status in log

      clear: msg_log.
      msg_log-index = index.
      msg_log-m_name = m_name.
      msg_log-msg_typ = 'S'.
      msg_log-msg = 'Changes have been made'.

      append msg_log to msg_logs.
    else.
      call function 'BAPI_TRANSACTION_ROLLBACK'.

      if ls_return-object_msg[ 1 ]-type eq 'E' or ls_return-object_msg[ 1 ]-type eq 'A'.
        output_line-icon = icon_red_light.
      else.
        if output_line-icon ne icon_red_light.
          output_line-icon = icon_yellow_light.
        endif.
      endif.
      output_line-msg = output_line-msg && ` ` && m_name && ':' && |Err - { ls_return-object_msg[ 1 ]-message }|.  " Update status in log

      " never overwrite errors with success or warning
      clear: msg_log.
      loop at ls_return-object_msg into data(ls_msg).
        msg_log-index = index.
        msg_log-m_name = m_name.
        msg_log-msg_typ = ls_msg-type.
        msg_log-msg = ls_msg-message.

        append msg_log to msg_logs.
        clear: msg_log, ls_msg.
      endloop.
    endif.
  endmethod.

  method call_bdc.
    check iv_tcode is not initial.
    refresh mess_tab.
    call transaction iv_tcode using bdc_tab
          mode   mode
          update 'L'
          messages into mess_tab.

    clear: mess_wa, msg.
    read table mess_tab into mess_wa with key msgid = 'F2' msgnr = '056'. " Success message
    if sy-subrc <> 0.
      read table mess_tab into mess_wa index 1.
      if sy-subrc = 0.
        call function 'FORMAT_MESSAGE'
          exporting
            id        = conv sy-msgid( mess_wa-msgid )
            lang      = '-D'
            no        = conv sy-msgno( mess_wa-msgnr )
            v1        = conv sy-msgv1( mess_wa-msgv1 )
            v2        = conv sy-msgv2( mess_wa-msgv2 )
            v3        = conv sy-msgv3( mess_wa-msgv3 )
            v4        = conv sy-msgv4( mess_wa-msgv4 )
          importing
            msg       = msg
          exceptions
            not_found = 1
            others    = 2.
        if sy-subrc <> 0.
* Implement suitable error handling here
        endif.
      endif.
      " never overwrite errors with success or warning
      if mess_wa-msgtyp eq 'E' or mess_wa-msgtyp eq 'A'.
        output_line-icon = icon_red_light.
      else.
        if output_line-icon ne icon_red_light and output_line-icon ne icon_yellow_light.
          if mess_wa-msgid = 'F2' and mess_wa-msgnr = '035'.  " No changes made
            output_line-icon = icon_green_light.
          else.
            output_line-icon = icon_yellow_light.
          endif.
        endif.
      endif.
      output_line-msg = output_line-msg && ` ` && m_name && ':' && |Err - { msg }|.  " Update status in log
    else.
*   Success
      if output_line-icon ne icon_red_light and output_line-icon ne icon_yellow_light.  " all ok only previous methods haven't reported errors
        output_line-icon = icon_green_light.
      endif.
      output_line-msg = output_line-msg && ` ` && m_name && ':' && 'OK'.  " Update status in log
    endif.

    output_line-msg = output_line-msg && ';'. " message separator

    " All messages from the bdc call are logged in a separate table; later used to display entire log of one row in a popup alv dialog
    clear msg_log.
    clear: mess_wa, msg.
    loop at mess_tab into mess_wa.
      call function 'FORMAT_MESSAGE'
        exporting
          id        = conv sy-msgid( mess_wa-msgid )
          lang      = '-D'
          no        = conv sy-msgno( mess_wa-msgnr )
          v1        = conv sy-msgv1( mess_wa-msgv1 )
          v2        = conv sy-msgv2( mess_wa-msgv2 )
          v3        = conv sy-msgv3( mess_wa-msgv3 )
          v4        = conv sy-msgv4( mess_wa-msgv4 )
        importing
          msg       = msg
        exceptions
          not_found = 1
          others    = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      msg_log-index = index.
      msg_log-m_name = m_name.
      msg_log-msg_typ = mess_wa-msgtyp.
      msg_log-msg = msg.

      append msg_log to msg_logs.
      clear: msg_log, mess_wa.
    endloop.
  endmethod.

  method bdc_dynpro.
    clear bdc_wa.
    bdc_wa-program  = program.
    bdc_wa-dynpro   = dynpro.
    bdc_wa-dynbegin = 'X'.
    append bdc_wa to bdc_tab.
  endmethod.

  method bdc_field.
    if conv bdc_fval( fval ) <> no_data. " No data character
      clear bdc_wa.
      bdc_wa-fnam = fnam.
      bdc_wa-fval = conv bdc_fval( fval ).
      append bdc_wa to bdc_tab.
    endif.
  endmethod.

  method display_log.
    check output is not initial.
    data: o_table     type ref to cl_salv_table,
          o_container type ref to cl_gui_container,
          o_functions type ref to cl_salv_functions_list,
          o_columns   type ref to cl_salv_columns_table,
          o_column    type ref to cl_salv_column_table,
          o_col_list  type ref to cl_salv_column_list,
          o_layout    type ref to cl_salv_layout,
          o_layo      type ref to cl_salv_layout_service,
          o_key       type salv_s_layout_key,
          o_info      type salv_s_layout_info,
          o_display   type ref to cl_salv_display_settings,
          o_head_grid type ref to cl_salv_form_layout_grid,
          o_label     type ref to cl_salv_form_label,
          o_flow      type ref to cl_salv_form_layout_flow,
          o_events    type ref to cl_salv_events_table.


    free: o_table    ,
          o_container,
          o_functions,
          o_columns  ,
          o_column   ,
          o_col_list ,
          o_layout   ,
          o_layo     ,
          o_key      ,
          o_info     ,
          o_display  ,
          o_head_grid,
          o_label    ,
          o_flow     ,
          o_events   .

    if output is not initial.

      try.
          cl_salv_table=>factory(
          exporting
            list_display = if_salv_c_bool_sap=>false
          importing
            r_salv_table = o_table
          changing
            t_table      = output ).
        catch cx_salv_msg .
      endtry.

      check o_table is bound.

      o_columns = o_table->get_columns( ).

      if o_columns is bound.
        try.
            " Column procesing
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ICON' ).
            o_column->set_long_text( value = 'Processing Status' ).
            o_column->set_medium_text( value = 'Proc. Status' ).
            o_column->set_short_text( value = 'Proc Stat' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'LIFNR' ).
            o_column->set_long_text( value = 'Vendor Code' ).
            o_column->set_medium_text( value = 'Vendor' ).
            o_column->set_short_text( value = 'Vendor' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BUKRS' ).
            o_column->set_long_text( value = 'Company Code' ).
            o_column->set_medium_text( value = 'Comp. Code' ).
            o_column->set_short_text( value = 'CoCd' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EKORG' ).
            o_column->set_long_text( value = 'Purchasing Organisation' ).
            o_column->set_medium_text( value = 'Purch. Org.' ).
            o_column->set_short_text( value = 'POrg' ).
            o_column->set_key( exporting value = if_salv_c_bool_sap=>true ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'REGIO' ).
            o_column->set_long_text( value = 'Region/State Code' ).
            o_column->set_medium_text( value = 'Reg/State Cd.' ).
            o_column->set_short_text( value = 'State Cd' ).

            " IHDK905062
            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PSTLZ' ).
            o_column->set_long_text( value = 'Postal Code' ).
            o_column->set_medium_text( value = 'Postal Code.' ).
            o_column->set_short_text( value = 'Post Code' ).
            " End IHDK905062

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MOBIL' ).
            o_column->set_long_text( value = 'Mobile Number' ).
            o_column->set_medium_text( value = 'Mobile No.' ).
            o_column->set_short_text( value = 'Mobile' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'EMAIL' ).
            o_column->set_long_text( value = 'Email ID' ).
            o_column->set_medium_text( value = 'Email ID' ).
            o_column->set_short_text( value = 'Email' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PANNO' ).
            o_column->set_long_text( value = 'PAN Number' ).
            o_column->set_medium_text( value = 'PAN No.' ).
            o_column->set_short_text( value = 'PAN' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'VEN_CLASS' ).
            o_column->set_long_text( value = 'Vendor Classification for GST' ).
            o_column->set_medium_text( value = 'Vendor Class for GST' ).
            o_column->set_short_text( value = 'GST Class' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'GSTNO' ).
            o_column->set_long_text( value = 'GST Number' ).
            o_column->set_medium_text( value = 'GST No.' ).
            o_column->set_short_text( value = 'GST' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BANKS' ).
            o_column->set_long_text( value = 'Bank Country Key' ).
            o_column->set_medium_text( value = 'Bank Country' ).
            o_column->set_short_text( value = 'Bnk Cntry' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BANKL' ).
            o_column->set_long_text( value = 'Bank Key/IFSC Code' ).
            o_column->set_medium_text( value = 'Bank Key' ).
            o_column->set_short_text( value = 'IFSC' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BANKN' ).
            o_column->set_long_text( value = 'Bank Account Number' ).
            o_column->set_medium_text( value = 'Account No.' ).
            o_column->set_short_text( value = 'Acc No' ).

*            free o_column.
*            o_column ?= o_columns->get_column( columnname = 'KOINH' ). " IRDK932940
*            o_column->set_long_text( value = 'Account Holders Name' ).
*            o_column->set_medium_text( value = 'Acc Holder Name' ).
*            o_column->set_short_text( value = 'Acc Name' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BANKA' ).
            o_column->set_long_text( value = 'Bank Name' ).
            o_column->set_medium_text( value = 'Bank Name' ).
            o_column->set_short_text( value = 'Bnk Name' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'PROVZ' ).
            o_column->set_long_text( value = 'Bank Region/State Code' ).
            o_column->set_medium_text( value = 'Bank State Code' ).
            o_column->set_short_text( value = 'Bnk State' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'STRAS' ).
            o_column->set_long_text( value = 'Bank Steet Address' ).
            o_column->set_medium_text( value = 'Bank Street' ).
            o_column->set_short_text( value = 'Bnk Street' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ORT01' ).
            o_column->set_long_text( value = 'Bank City' ).
            o_column->set_medium_text( value = 'Bank City' ).
            o_column->set_short_text( value = 'Bnk City' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BRNCH' ).
            o_column->set_long_text( value = 'Bank Branch' ).
            o_column->set_medium_text( value = 'Bank Branch' ).
            o_column->set_short_text( value = 'Bnk Branch' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'SWIFT' ).
            o_column->set_long_text( value = 'Bank SWIFT Code' ).
            o_column->set_medium_text( value = 'Bank SWIFT Code' ).
            o_column->set_short_text( value = 'Bnk SWIFT' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BNKLZ' ).  " IRDK932942
            o_column->set_long_text( value = 'Bank Number/Code' ).
            o_column->set_medium_text( value = 'Bank No/Code' ).
            o_column->set_short_text( value = 'Bnk Code' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'ZWELS' ).
            o_column->set_long_text( value = 'Payment Method' ).
            o_column->set_medium_text( value = 'Payment Method' ).
            o_column->set_short_text( value = 'Pay Meth' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'HBKID' ).
            o_column->set_long_text( value = 'House Bank' ).
            o_column->set_medium_text( value = 'House Bank' ).
            o_column->set_short_text( value = 'Hs Bnk' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'C_ZTERM' ).
            o_column->set_long_text( value = 'FI Payment Terms' ).
            o_column->set_medium_text( value = 'FI Pay Terms' ).
            o_column->set_short_text( value = 'FI PTrms' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'AKONT' ).
            o_column->set_long_text( value = 'Reconciliation Account' ).
            o_column->set_medium_text( value = 'Recon. Account' ).
            o_column->set_short_text( value = 'Recon Act' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'P_ZTERM' ).
            o_column->set_long_text( value = 'Purch Payment Terms' ).
            o_column->set_medium_text( value = 'Purch Pay Terms' ).
            o_column->set_short_text( value = 'Pur PTrms' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BPTYPE' ).
            o_column->set_long_text( value = 'Partner Type' ).
            o_column->set_medium_text( value = 'Partner Type' ).
            o_column->set_short_text( value = 'BP Type' ).

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'BPTYPE' ).
            o_column->set_long_text( value = 'WTax Country' ).
            o_column->set_medium_text( value = 'WTax Country' ).
            o_column->set_short_text( value = 'WTax Cty' ).

            " Add fcat for your fields here

            free o_column.
            o_column ?= o_columns->get_column( columnname = 'MSG' ).
            o_column->set_long_text( value = 'Message' ).
            o_column->set_medium_text( value = 'Message' ).
            o_column->set_short_text( value = 'Message' ).
            o_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

          catch: cx_salv_not_found.
        endtry.
        o_columns->set_column_position( exporting columnname = 'NAME1' position = 5 ).  " IHDK901832
        o_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
        o_columns->set_key_fixation( exporting value = if_salv_c_bool_sap=>true ).
      endif.

      o_functions = o_table->get_functions( ).

      if o_functions is bound.
        o_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
      endif.

      o_layout = o_table->get_layout( ).

      o_key-report = sy-repid.

      if o_layout is bound.
        o_layout->set_key( exporting value = o_key ).

        o_layout->set_save_restriction( exporting value = cl_salv_layout=>restrict_none ).

        o_layout->set_default( exporting value = if_salv_c_bool_sap=>true ).
      endif.

      o_display = o_table->get_display_settings( ).

      if o_display is bound.
        o_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
      endif.

      " Build report header
      create object o_head_grid.
      if o_head_grid is bound.
        o_label = o_head_grid->create_label( exporting row = 1 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'Vendor Master Mass Update: Processing Log' ).
        endif.

        o_label = o_head_grid->create_label( exporting row = 2 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'No. of rows:' ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 2 column = 2 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = | { lines( output ) }| ).
        endif.

        o_label = o_head_grid->create_label( exporting row = 3 column = 1 ).
        if o_label is bound.
          o_label->set_text( exporting value = 'Time-stamp:' ).
        endif.

        o_flow = o_head_grid->create_flow( exporting row = 3 column = 2 ).
        if o_flow is bound.
          o_flow->create_text( exporting text = |{ sy-datum date = user } { sy-uzeit time = user }| ).
        endif.

        o_table->set_top_of_list( exporting value = o_head_grid ).
        o_table->set_top_of_list_print( exporting value = o_head_grid ).
      endif.

      " Event handling - hotpost click
      o_events = o_table->get_event( ).
      if o_events is bound.
        set handler hotspot_click for o_events.
      endif.

      o_table->display( ).
    else.
      message 'No log generated' type 'S' display like 'E'.
    endif.
  endmethod.

  method hotspot_click.
    data: lo_table     type ref to cl_salv_table,
          lo_columns   type ref to cl_salv_columns_table,
          lo_functions type ref to cl_salv_functions_list,
          lo_column    type ref to cl_salv_column_table,
          lo_display   type ref to cl_salv_display_settings.

    case column.
      when 'ICON' or 'MSG'.
        data(index_log) = filter #( msg_logs using key index where index = row ).
        sort index_log by m_name.
        try.
            cl_salv_table=>factory(
            exporting
              list_display = abap_false
            importing
              r_salv_table = lo_table
            changing
              t_table      = index_log ).
          catch cx_salv_msg.
        endtry.

        if lo_table is bound.
          lo_columns = lo_table->get_columns( ).
          if lo_columns is bound.
            try.
                free lo_column.
                lo_column ?= lo_columns->get_column( columnname = 'M_NAME' ).
                lo_column->set_long_text( value = 'Method' ).
                lo_column->set_medium_text( value = 'Method' ).
                lo_column->set_short_text( value = 'Meth' ).

              catch: cx_salv_not_found.
            endtry.
            lo_columns->set_optimize( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
          endif.

          lo_functions = lo_table->get_functions( ).

          if lo_functions is bound.
            lo_functions->set_all( exporting value = if_salv_c_bool_sap=>true ). " Default input bool true
          endif.

          lo_display = lo_table->get_display_settings( ).

          if lo_display is bound.
            lo_display->set_striped_pattern( exporting value = cl_salv_display_settings=>true ).
            lo_display->set_list_header( exporting value = |Processing log for index { row }| ).
          endif.

* ALV as Popup
          lo_table->set_screen_popup(
          start_column = 5
          end_column   = 80
          start_line   = 1
          end_line     = 10 ).

* Display
          lo_table->display( ).
        endif.
      when 'LIFNR'.
        clear output_line.
        read table output into output_line index row.
        if sy-subrc is initial and output_line-lifnr is not initial.
          set parameter id 'LIF' field output_line-lifnr.
          set parameter id 'BUK' field output_line-bukrs.
          set parameter id 'EKO' field output_line-ekorg.
          call transaction 'XK03'.
        endif.
      when others.
    endcase.
  endmethod.

  method file_open.
    data field_name    type dynpread-fieldname.
    data file_name     type ibipparms-path.

    clear: field_name, file_name.
    field_name = 'P_FILE'.
    call function 'F4_FILENAME'
      exporting
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = field_name
      importing
        file_name     = file_name.

    if file_name is not initial.
      p_file = file_name.
    endif.
  endmethod.
endclass.

class main implementation.
  method start.
    data: lo_app type ref to lcl_application.

    free lo_app.
    lo_app = new lcl_application( ).

    if lo_app is bound.
      lo_app->process( ).
    endif.
  endmethod.
endclass.

* ---- Selection Screen Events ---- *
at selection-screen output.
  dwn_btn = '@49@ Download'.
  loop at screen.
    if screen-group1 = 'RAD'.
      screen-active = 0.
      modify screen.
    endif.
  endloop.

at selection-screen on value-request for p_file.
  lcl_application=>file_open( ).

at selection-screen.
  if sscrfields-ucomm eq 'DWN_FORMAT'.
    call function 'CALL_BROWSER'
      exporting
        url                    = 'https://goo.gl/jzwnnE'      " File download url, google drive link
        window_name            = 'ZMM086_File_format'
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
    else.
      message 'Opening your default web browser...'  type 'S'.
    endif.
  endif.

  if sscrfields-ucomm eq 'ONLI'.
    if p_file is initial.
      set cursor field 'P_FILE'.
      message id '00' type 'S' number '055' display like 'E'.
      stop.
    endif.
  endif.

* ---- Begin main program ---- *
start-of-selection.
  main=>start( ).
