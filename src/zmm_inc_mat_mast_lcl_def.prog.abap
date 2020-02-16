*&---------------------------------------------------------------------*
*&  Include           ZMM_INC_MAT_MAST_LCL_DEF
*&---------------------------------------------------------------------*
* ---- begin local exception class definition ---- *
class lcx_generic_error definition inheriting from cx_static_check.
  public section.
    interfaces: if_t100_dyn_msg, if_t100_message.
    " Used in throwing a generic exception, Error displayed is based on context of the exception
endclass.
* ---- end exception class definition ---- *

* ---- begin interface ---- *
* changing parameters have been passed as references since we want them to be altered globally *
interface lif_mm.
  methods:
* Called from within constructor, acts as a bootstrap/ignition *
    invoke,

* Convert Excel to Internal Table *
    file_to_tab
      changing
        it_file type standard table
      raising
        lcx_generic_error,

* Extract header/s and see if there's any data present after removing the headers *
    check_if_tab_empty
      changing
        it_file     type standard table
        log_header1 type any
        log_header2 type any
      raising
        lcx_generic_error,

* Remove leading spaces if any from excel cells(character format) *
    file_format_adjust
      changing
        it_file type standard table,

* Convert character(file) to bapi format(data)/conversion exits *
    convert_data_to_bapi_format
      importing
        value(it_file) type standard table
      changing
        it_data        type standard table,

* Redefined method -> Uses class private data members => Check if material creation/extension is possible, already created/extended? *
    material_validation
      changing            " IHDK900894
        it_data type standard table
        it_log  type standard table,  " Could be made private in the subclasses, in which case must be removed from mm and interface
    " handled by the common helper class since the same validations/actions need to be performed for all material types

* Displays the number of materials supplied in file, and can be created/extended, asks for user confirmation before proceeding *
    confirm_action
      importing
        value(it_file) type standard table
        value(it_data) type standard table
        value(blk_cnt) type i optional
      changing
        answer         type c
        answer_blk     type c optional,

* Redefined method -> Uses class private data members, material creation is specific to each material type *
    " Rename - IHDK900652
    material_create_extend, " Fields supplied via file are different for each material type, bapi structures are filled differently
    " Could be made private in the subclasses, in which case must be removed from mm and interface

    " IHDK904473
    " internal material number generation based on material type
    material_number_get
      importing
        value(material_type)    type mara-mtart
      exporting
        value(return)           type bapireturn1
      returning
        value(material_number)  type bapimathead-material,

* Dynamically fill bapi x (checkbox - bapiupdate) structure based on bapi structure values *
    fill_bapix
      importing
        value(bapi) type any
      changing
        bapix       type any,

* Called from within material creation, but implementation is common for all, hence in base class *
    " IRDK930708 - Method added for re-usability
    get_tax_classification  " get tax classification for the respective sales area
      importing
        value(salesdata)   type bapi_mvke
      changing
        taxclassifications type t_bapi_mlan,  " Table Type for BAPI_MLAN

* Called from within material creation, but implementation is common for all, hence in base class *
    maint_material_classif  " Class Num, Class type may be imported additionaly if it varies for material types
      importing
        value(material)   type bapimathead-material
        value(class_num)  type bapi1003_key-classnum
        value(class_type) type bapi1003_key-classtype " IHDK901148
      changing
        classif_status    type bapi1003_key-status,

* Called from within material creation, but implementation is common for all, hence in base class *
    maint_material_insp_setup
      importing
        value(material) type bapimathead-material
        value(plant)    type bapi_marc-plant
        value(s_art)    type rseloption           " Insp-types to be maintained vary for different material types,
        " filled and passed from material creation of method of respective material type class
      changing
        insp_status     type flag,

* Called from material creation, to extend raw, pkg type materials
* for extending the material to specified valuation type
    extend_to_val_class
      importing
        value(headdata)      type bapimathead
        value(valuationdata) type bapi_mbew
        value(bwtar)         type mbew-bwtar
        value(bklas)         type mbew-bklas
      exporting
        ext_status           type flag,

* Called from material creation, to maintain MRP area for certain material types as per requirement in S4H
    maintain_mrp_area " Rev 14
      importing
        value(material)   type bapimathead-material
        value(plant)      type bapi_marc-plant
        value(mrp_ctrler) type bapi_marc-mrp_ctrler
        value(mrp_ls_key) type bapi_marc-lotsizekey
      exporting
        mrp_status        type flag,

* Called from material creation/extension, to extend material to additional storage locations based on zmat01_sloc_mast
    extend_to_stor_loc   " IHDK900652
      importing
        value(material)  type bapimathead-material
        value(plant)     type bapi_marc-plant
        value(matl_type) type bapimathead-matl_type
        value(division)  type bapi_mara-division     " IHDK900685
      exporting
        ext_status       type flag,

    " IHDK904473
    update_db_log,

* Display log using log table, insert previously extracted header => display log *
    log_output
      importing
        value(log_header1) type any
        value(log_header2) type any
        value(filename)    type string
      changing
        it_log             type standard table
      raising
        lcx_generic_error,

* Display log in alv format after adding a proc stat column
    log_alv_display       " IHDK900690
      importing
        value(it_log)      type standard table
        value(iv_filename) type string.
endinterface.
* ---- end interface ---- *

* ---- begin class definitions ---- *
* ---- begin base mm class definition, contains re-usable data ---- *
class lcl_mm definition.
  public section.
* Public data: re-used in redefined methods in subclasses *

* Type-Pools *
    type-pools: truxs, abap, esp1.
    " truxs -> it_raw in excel to internal table conversion -> TEXT_CONVERT_XLS_TO_SAP
    " abap  -> abap_true/false
    " esp1  ->

* Types *
    types: excelcell(50) type c.  " generic field type for file data uploaded by user

* Constants *
    " Used in fill_bapix method to differentiate between key fields and 'X' fields in bapix structures
    constants: c_bapix_type type rollname value 'BAPIUPDATE'.

* Variables *
    " IHDK900305
    class-data: gt_pctr_mast type standard table of zmat01_pctr_mast, " profit center/overhead group default values
                mv_action type char1, " C = Create, E = Extend; IHDK904473
                ms_err_log_db type zmm_t_mat01_log. " IHDK904473

    data: lv_tabix      type syst-tabix, " Current row number in loop pass on table
          lines_file(5) type c,          " File table line count
          lines_data(5) type c,          " Data table line count
          msg           type string,     " Message
          answer        type c,          " Answer from confirmation pop-up
          answer_blk    type c.          " Answer for blocked materials from confirmation pop-up

* FM Related *
* BAPI_MATERIAL_GET_ALL * " Used in extension to get reference data from reference material
    " Structures
    data: _clientdata    type bapi_mara_ga, " MARA
          _plantdata     type bapi_marc_ga, " MARC
          _valuationdata type bapi_mbew_ga, " MBEW
          _salesdata     type bapi_mvke_ga. " MVKE
    " Tables/wa's
    data: _materialdescription type table of bapi_makt_ga,  " MAKT
          _unitsofmeasure      type table of bapi_marm_ga,  " MARM
          _taxclassifications  type table of bapi_mlan_ga,  " MLAN
          _return_tab          type table of bapireturn,
          _return_wa           type bapireturn.

* BAPI_MATERIAL_GETINTNUMBER *
    data: t_material_number type table of bapimatinr, " material number table generated by bapi depending on
          " IHDK904473                                " how many material numbers are required
          wa_mat_num        type bapimatinr,          " WA for material_number
          mat_no_return     type bapireturn1.

* BAPI_MATERIAL_SAVEDATA *
    " Structures
    data: headdata             type bapimathead, " View Selection
          clientdata           type bapi_mara,   " MARA
          clientdatax          type bapi_marax,  " Checkbox
          plantdata            type bapi_marc,   " MARC
          plantdatax           type bapi_marcx,  " Checkbox
          storagelocationdata  type bapi_mard,   " MARD
          storagelocationdatax type bapi_mardx,  " Checkbox
          valuationdata        type bapi_mbew,   " MBEW
          valuationdatax       type bapi_mbewx,  " Checkbox
          salesdata            type bapi_mvke,   " MVKE
          salesdatax           type bapi_mvkex,  " Checkbox
          return               type bapiret2.    " return structure
    " Tables/wa's
    data: materialdescription type table of bapi_makt,        " MAKT
          wa_matdesc          type bapi_makt,
          unitsofmeasure      type table of bapi_marm,        " MARM
          wa_uom              type bapi_marm,
          unitsofmeasurex     type table of bapi_marmx,       " Checkbox
          wa_uomx             type bapi_marmx,
          taxclassifications  type table of bapi_mlan,        " MLAN
          wa_taxclass         type bapi_mlan,
          returnmessages      type table of bapi_matreturn2,  " Return messages from BAPI
          wa_retmessage       type bapi_matreturn2.
* Note on RETURN strcuture and RETURNESSAGES table in bapi_material_savedata *
    " Since numerous information and error messages can occur while the material is being processed,
    " the messages created are collected in this table (RETURNMESSAGES).
    " The message processed last is transferred to the return parameter (RETURN).

* STEUERTAB_IDENTIFY *
    data: steuertab type table of mg03steuer, " Tax types for sal. org. / distr. channel
          wa_steuer type mg03steuer.

* BAPI_OBJCL_CHANNGE * See private section for other related declarations
    data: classif_status type bapi1003_key-status.  " Needs to be public for access in subclasses
    " Success/Failure of classification operation

* BAPI_MATINSPCTRL_SAVEREPLICA, MAP2E_QMAT_TO_BAPI1001004_QMAT *
* See private section for other related declarations *
    data: insp_status    type flag. " Needs to be public for access in subclasses
    " Success/Failure of inspection setup operation
    data: s_art    type rseloption, " select option - insp_types may vary as per material type
          s_art_wa type rsdsselopt.

* Methods specific to this class not in interface *
    methods:
      constructor,  " Empty constructor

      set_func_buttons, " IHDK901148; additional function buttons to selection screen

      set_frame_title       " Set block-frame titles based on radio button selected(On selection screen)
        changing
          dwnld type any    " frame title of file format download block
          upld  type any,   " file upload block

      modify_screen
        raising lcx_generic_error,  " IHDK904062

      file_open             " Load file from user front-end
        raising lcx_generic_error,

      handle_sel_screen_ucomm,  " IHDK901148; handle button events on selection screen

      head_views_from_pstat " Map letters of status field(pstat) into views and mark views to be selected accordingly
        importing
          value(pstat) type bapi_mara_ga-maint_stat
        changing
          headdata     type bapimathead,

      disp_message          " Generate message string and structure, fill message table(only 1 at a time), and display as a pop-up
        importing
          value(type) type bapiret2-type
          value(id)   type bapiret2-id
          value(no)   type bapiret2-number
          value(var1) type bapiret2-message_v1 optional
          value(var2) type bapiret2-message_v2 optional
          value(var3) type bapiret2-message_v3 optional
          value(var4) type bapiret2-message_v4 optional.

* Implemented interfaces *
    interfaces lif_mm.

* Short-hand notations *
    aliases:
    invoke                      for lif_mm~invoke,
    file_to_tab                 for lif_mm~file_to_tab,
    check_if_tab_empty          for lif_mm~check_if_tab_empty,
    file_format_adjust          for lif_mm~file_format_adjust,
    convert_data_to_bapi_format for lif_mm~convert_data_to_bapi_format,
    material_validation         for lif_mm~material_validation,
    confirm_action              for lif_mm~confirm_action,
    material_create_extend      for lif_mm~material_create_extend,  " Rename - IHDK900652
    material_number_get         for lif_mm~material_number_get,     " IHDK904473
    fill_bapix                  for lif_mm~fill_bapix,
    get_tax_classification      for lif_mm~get_tax_classification,    " IRDK930708
    maint_material_classif      for lif_mm~maint_material_classif,
    maint_material_insp_setup   for lif_mm~maint_material_insp_setup,
    extend_to_val_class         for lif_mm~extend_to_val_class,
    maintain_mrp_area           for lif_mm~maintain_mrp_area,   " Rev 14
    extend_to_stor_loc          for lif_mm~extend_to_stor_loc,  " IHDK900652
    update_db_log               for lif_mm~update_db_log,       " IHDK904473
    log_output                  for lif_mm~log_output,
    log_alv_display             for lif_mm~log_alv_display.     " IHDK900690

  protected section.
    " Protected content

  private section.
* Data used only in this class, not available in subclasses *
    data: it_raw type truxs_t_text_data.  " For internal processing, type-pool truxs
    data: v_file type rlgrap-filename.    " TEXT_CONVERT_XLS_TO_SAP

* BAPI_OBJCL_CHANGE * " Material Classification View
    " Header data, key fields
    data: objectkey   type bapi1003_key-object,       " Material
          objecttable type bapi1003_key-objecttable,  " MARA - CONSTANT
          classnum    type bapi1003_key-classnum,     " Class name/number - > Eg ZSFG
          classtype   type bapi1003_key-classtype.    " Class type -> Eg 023, Batch -> TCLAT
    " Maintain specific characteristic values if required
    data: allocvaluesnumnew  type table of bapi1003_alloc_values_num,
          allocvaluescharnew type table of bapi1003_alloc_values_char,
          allocvaluescurrnew type table of bapi1003_alloc_values_curr,
          mat_class_ret      type table of bapiret2.

* BAPI_MATINSPCTRL_SAVEREPLICA, MAP2E_QMAT_TO_BAPI1001004_QMAT *  " Material Quality View, Inspection setup
    data: tq34           type table of tq34,              " Default values for inspection type -> Get data from
          wa_tq34        type tq34,
          qmat           type qmat,                       " Inspection type - material parameters <- Store into
          bapi_qmat      type bapi1001004_qmat,           " MAP2E_QMAT_TO_BAPI1001004_QMAT
          inspectionctrl type table of bapi1001004_qmat,  " BAPI_MATINSPCTRL_SAVEREPLICA
          wa_inspctrl    type bapi1001004_qmat,
          insp_return    type table of bapiret2,
          wa_insp_ret    type bapiret2.

* balw_bapireturn_get2 *
* finb_bapiret2_display *
    " Used in disp_message method
    data: it_messages type /eacc/t_bapiret2,
          wa_message  type bapiret2.

* File output related *
* cl_gui_frontend_services=>file_open_dialog *
* cl_gui_frontend_services=>file_save_dialog *
* cl_gui_frontend_services=>gui_download *

* File header used in gui_download *
    data: it_head type table of fieldnames, " log header(field names) in gui_download(DBF file format)
          wa_head type fieldnames.

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

* local variables for method: convert to bapi format *  " RTTS: ABAP Run Time Type Services
    data: lo_descr          type ref to cl_abap_typedescr.  " type info of supplied data
    data: lv_relative_name  type string.    " data element of supplied variable
    data: lv_data_element   type rollname.
    data: lv_function       type funcname.  " CONV. EXIT Name
    data: wa_dd04l          type dd04l.     " Data elements - get converion exit for data element
    data: lv_index          type sy-index.  " Loop Index

* for fill_headdata_views_using_pstat *
    data: lv_pstatlen type i. " length of string/char data in pstat

* for dynamic frame title on selection screen *
    data: ty_title(118) type c.
    " Static data, accessed directly using class name, value is common and constant for all intances of the class
    class-data: objectname type rs38m-programm,
                tpool      type standard table of textpool.
endclass.
* ---- end base class definition ---- *

class lcl_zfgm_zlfg_create definition deferred.  " so that it can be declared as a friend class in helper class
class lcl_ztrd_create definition deferred.
class lcl_zraw_zpkg_zpku_create definition deferred.
class lcl_zsfg_create definition deferred.
class lcl_zmco_create definition deferred.
class lcl_yzspr_yspi_zeu2_create definition deferred.
* ---- begin material creation helper class definition ---- *
class lcl_create_helper definition
                        inheriting from lcl_mm
                        friends lcl_zfgm_zlfg_create
                                lcl_ztrd_create
                                lcl_zraw_zpkg_zpku_create
                                lcl_zsfg_create
                                lcl_zmco_create
                                lcl_yzspr_yspi_zeu2_create.
  " IHDK904473
  public section.
    methods: constructor.

  private section.
    data: it_makt type standard table of makt,  " Material Description
          wa_makt type makt.

    methods:
* Check if supplied material already exists *
      material_validate
        changing it_data type standard table
                 it_log  type standard table.
endclass.
* ---- end creation helper class definition ---- *

class lcl_zfgm_zlfg_ext_plant definition deferred. " so that it can be declared as a friend class in helper class
class lcl_zfgm_zlfg_ext_depot definition deferred.
class lcl_ztrd_ext_plant definition deferred. " so that it can be declared as a friend class in helper class
class lcl_ztrd_ext_depot definition deferred.
class lcl_zraw_zpkg_zpku_ext_plt_dpt definition deferred.
class lcl_zsfg_ext_plant definition deferred.
class lcl_zmco_ext_plant definition deferred.
class lcl_yzspr_yspi_zeu2_ext_plt definition deferred.
* ---- begin extension helper class definition ---- *
class lcl_extend_helper definition
                        inheriting from lcl_mm
                        friends lcl_zfgm_zlfg_ext_plant    " Friends have access to private data members of this class
                                lcl_zfgm_zlfg_ext_depot
                                lcl_ztrd_ext_plant
                                lcl_ztrd_ext_depot
                                lcl_zraw_zpkg_zpku_ext_plt_dpt
                                lcl_zsfg_ext_plant
                                lcl_zmco_ext_plant
                                lcl_yzspr_yspi_zeu2_ext_plt.
  " IHDK904473
  public section.
    methods: constructor.

  private section.
    types: begin of ty_mat,         " Copy from?
             matnr        type bapi_mara_ga-material,   " mara-matnr,
             source_bukrs type bapi0002_1-comp_code,    " t001k-bukrs,
             target_bukrs type bapi0002_1-comp_code,    " t001k-bukrs,
             source_werks type bapi_marc_ga-plant,      " marc-werks,
             target_werks type bapi_marc_ga-plant,      " marc-werks,
*             vkorg        TYPE mvke-vkorg,              " sales org
*             vtweg        TYPE mvke-vtweg,              " distr channel
*             lgort TYPE mard-lgort,
           end of ty_mat.

    data: it_mara type table of mara,      " Material master
          wa_mara type mara,

          it_marc type table of marc,      " Material Plant
          wa_marc type marc,

          it_mat  type table of ty_mat,    " Copy material data from?
          wa_mat  type ty_mat.

    data: v_dist_ch type mvke-vtweg.       " Distribution Channel (used to fetch sales data for material)

    data: v_block_material_count type i.

    data: begin of blocked_material, " IRDK930708
            matnr type matnr,
          end of blocked_material,
          blocked_materials like standard table of blocked_material.
* Check extensibility of supplied materials and gather reference data to be copied from *
    methods:
      material_validate
        changing it_data type standard table
                 it_log  type standard table,

      filter_out_blocked_materials            " IRDK930708
        changing it_data type standard table
                 it_log  type standard table.
endclass.
* ---- end extension helper class definition ---- *

* ---- Begin class definition for ZFGM/ZLFG type material creation ---- *
class lcl_zfgm_zlfg_create definition inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
* Each subclass will have the following declarations unique(name will be same) depending on material type, hence private *
    types: begin of ty_file.        " Data from uploaded file, Sequence as per file
        include type zif_mm_mat01=>ty_fg_create_file.
    types: end of ty_file,

    begin of ty_data.             " Sequence as per BAPI
        include type zif_mm_mat01=>ty_fg_create_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_crt_helper type ref to lcl_create_helper.  " reference to helper class for material validation
endclass.
* ---- end ZFGM/ZLFG class definition ---- *

* ---- begin ZFGM/ZLFG Plant extension class definition *
class lcl_zfgm_zlfg_ext_plant definition
                         inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_fg_plt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_fg_plt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- end ZFGM/ZLFG Plant ext. class definition *

* ---- begin ZFGM/ZLFG Depot extension class definition *
class lcl_zfgm_zlfg_ext_depot definition
                         inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_fg_dpt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_fg_dpt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " Headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- end ZFGM/ZLFG Depot ext. class definition ---- *

* ---- Begin class definition for ZTRD type material creation ---- *
class lcl_ztrd_create definition inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
* Each subclass will have the following declarations unique(name will be same) depending on material type, hence private *
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_trd_create_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_trd_create_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_crt_helper type ref to lcl_create_helper.  " reference to helper class for material validation
endclass.
* ---- End ZTRD class definition ---- *

* ---- Begin ZTRD Plant Extension class definition ---- *
class lcl_ztrd_ext_plant definition
                         inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_trd_plt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_trd_plt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- End ZTRD plant extend class definition ---- *

* ---- Begin ZTRD Depot Extension class definition ---- *
class lcl_ztrd_ext_depot definition
                         inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_trd_dpt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_trd_dpt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- End ZTRD plant depot class definition ---- *

* ---- Begin class definition for ZRAW, ZPKG, ZPKU type material creation ---- *
class lcl_zraw_zpkg_zpku_create definition inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
* Each subclass will have the following declarations unique(name will be same) depending on material type, hence private *
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_raw_create_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_raw_create_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_crt_helper type ref to lcl_create_helper.  " reference to helper class for material validation
endclass.
* ---- End ZRAW, ZPKG, ZPKU class definition ---- *

* ---- Begin ZRAW, ZPKG, ZPKU plant depot extension class definition ---- *
class lcl_zraw_zpkg_zpku_ext_plt_dpt definition
                                     inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_raw_plt_dpt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_raw_plt_dpt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log,

    begin of ty_mbew,
      matnr type mbew-matnr,
      bwkey type mbew-bwkey,
      bwtar type mbew-bwtar,
      bklas type mbew-bklas,
    end of ty_mbew.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log,

          it_mbew     type table of ty_mbew,
          wa_mbew     type ty_mbew.

    data:  lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- End ZRAW, ZPKG, ZPKU plant depot extension class definition ---- *

* ---- Begin class definition for ZSFG type material creation ---- *
class lcl_zsfg_create definition inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
* Each subclass will have the following declarations unique(name will be same) depending on material type, hence private *
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_sfg_create_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_sfg_create_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_crt_helper type ref to lcl_create_helper.  " reference to helper class for material validation
endclass.
* ---- end ZSFG class definition ---- *

* ---- begin ZSFG Plant extension class definition *
class lcl_zsfg_ext_plant definition
                         inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_sfg_plt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_sfg_plt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- end ZSFG Plant ext. class definition *

* ---- Begin class definition for ZMCO type material creation ---- *
class lcl_zmco_create definition inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
* Each subclass will have the following declarations unique(name will be same) depending on material type, hence private *
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_mco_create_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_mco_create_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_crt_helper type ref to lcl_create_helper.  " reference to helper class for material validation
endclass.
* ---- end ZMCO class definition ---- *

* ---- begin ZMCO Plant extension class definition *
class lcl_zmco_ext_plant definition
inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652

  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_mco_plt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_mco_plt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- end ZMCO Plant ext. class definition *

* ---- Begin class definition for Y/ZSPR, YSPI, ZEU2, ZCON type material creation ---- *
class lcl_yzspr_yspi_zeu2_create definition inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
* Each subclass will have the following declarations unique(name will be same) depending on material type, hence private *
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_spr_create_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_spr_create_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data: lo_crt_helper type ref to lcl_create_helper.  " reference to helper class for material validation
endclass.
* ---- end Y/ZSPR, YSPI, ZEU2, ZCON class definition ---- *

* ---- Begin Y/ZSPR, YSPI, ZEU2, ZCON plant extension class definition ---- *
class lcl_yzspr_yspi_zeu2_ext_plt definition
inheriting from lcl_mm.
  public section.
    methods:
      constructor,
      " Invoke acts as trigger point for calling all other methods
      " Cannot be re-used from superclass, as data to be passed for processing is specific(private) to child class(different for each child class)
      invoke redefinition,
      " implementation is specific to each material type and type of creation/extension
      material_validation redefinition,
      material_create_extend redefinition.  " Rename - IHDK900652
  protected section.

  private section.
    types: begin of ty_file.
        include type zif_mm_mat01=>ty_spr_plt_ext_file.
    types: end of ty_file,

    begin of ty_data.
        include type zif_mm_mat01=>ty_spr_plt_ext_data.
    types: end of ty_data,

    begin of ty_log,              " Structure for log file
      index(15) type c,
      log       type bapi_msg. "excelcell,
        include type ty_file.
    types: end of ty_log.

    data: it_file     type table of ty_file,   " File data in character format
          wa_file     type ty_file,

          it_data     type table of ty_data,   " Data in BAPI format, to be passed to BAPI
          wa_data     type ty_data,

          it_log      type table of ty_log,    " Log table, used to download log in excel format
          wa_log      type ty_log,

          log_header1 type ty_log,             " headers from uploaded file, extracted and stored for later use
          log_header2 type ty_log.

    data:  lo_ext_helper type ref to lcl_extend_helper.  " reference to helper class for material validation
endclass.
* ---- End Y/ZSPR, YSPI, ZEU2 plant extension class definition ---- *

* ---- begin main class definition, main method ---- *
class lcl_main definition.
  public section.
* Dynamic object creation OF dynamic CLASS TYPE
    class-data:
      cl_name type seoclsname,    " holds class/reference type eg: 'LCL_ZFGM_ZLFG_CREATE'
      lo      type ref to object. " object of any reference type,
    " we will make it refer to class in cl_name @ runtime

    " static main method
    class-methods:
      main.

  protected section.

  private section.
endclass.
* ---- end main class definition ---- *
* ---- end class definitions ---- *
