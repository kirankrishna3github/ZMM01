*&---------------------------------------------------------------------*
*&  Include           ZMM_INC_MAT_EXT_DATA_TOP
*&---------------------------------------------------------------------*
* ---- Global Data and Selection Screen ---- *
tables: sscrfields.

type-pools: abap, icon.

data: material  type mara-matnr, " For select-options
      plant     type marc-werks,
      strg_loc  type mard-lgort,
      sales_org type mvke-vkorg,
      dist_ch   type mvke-vtweg,
      division  type mara-spart,
      val_key   type mbew-bwkey.

data: gv_1001_ucomm like sy-ucomm.

field-symbols: <lt_output> type standard table.

data: lo_cx type ref to cx_root.  " get reference to root exception class for global use in exception handling

define mat_type_rad.
  selection-screen begin of line.
  parameters: &1 radiobutton group rad.
  selection-screen comment 3(50) &2.
  selection-screen end of line.
end-of-definition.

selection-screen begin of block typ with frame title text-typ.
*{   REPLACE        SBXK900030                                        1
*\PARAMETER: p_fgm_pl RADIOBUTTON GROUP typ DEFAULT 'X',
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Thursday, October 25, 2018 14:51:00
* Changed By - ABAP01 - Bhushan Mehta
* Purpose    - Simplification list - Changes Syntax
* Solution   - PARAMETER replace with PARAMETERS
* TR         - SBXK900030 - S4H:BM:Simplification List:03.10.2018
*--------------------------------------------------------------------*
mat_type_rad p_rad1  text-r01.
mat_type_rad p_rad2  text-r02.
mat_type_rad p_rad3  text-r03.
mat_type_rad p_rad4  text-r04.
mat_type_rad p_rad5  text-r05.
mat_type_rad p_rad6  text-r06.
mat_type_rad p_rad7  text-r07.
mat_type_rad p_rad8  text-r08.
mat_type_rad p_rad9  text-r09.
mat_type_rad p_rad10 text-r10.
mat_type_rad p_rad11 text-r11.
mat_type_rad p_rad12 text-r12.
mat_type_rad p_rad13 text-r13.
mat_type_rad p_rad14 text-r14.
selection-screen end of block typ.
selection-screen begin of block sel with frame title text-001.
select-options: s_matnr for material,
                s_werks for plant,
                s_lgort for strg_loc,
                s_vkorg for sales_org,
                s_vtweg for dist_ch,
                s_spart for division,
                s_bwkey for val_key.
selection-screen end of block sel.

selection-screen begin of screen 1001 as window.
selection-screen begin of block ddn with frame title text-ddn.  " drill down
parameters: p_stloc radiobutton group ddn default 'X',
            p_sales radiobutton group ddn,
            p_valtn radiobutton group ddn.
selection-screen end of block ddn.
selection-screen end of screen 1001.
* ---- Selection screen end ---- *
