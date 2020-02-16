*&---------------------------------------------------------------------*
*&  Include           ZMM_INC_MAT_MAST_TOP
*&---------------------------------------------------------------------*
* ---- Global Data ---- *
* Tables *
* Statement not allowed in oo context in classes *
tables: makt, mara, marc, mard, marm, mbew, mlan, mvke, sscrfields.

* Field-Symbols *
* Field symbols cannot be declared globally in classes *
field-symbols: <fs_tab>  type standard table,
               <fs_wa>   type any,
               <fs_head> type any,
               <fs>      type any.

* Global object references *
class lcl_mm definition deferred.

data: lo_mm type ref to lcl_mm,   " get reference to base class for global use in message build and display
      lo_cx type ref to cx_root.  " get reference to root exception class for global use in exception handling

define mat_type_rad.
  selection-screen begin of line.
  parameters: &1 radiobutton group rad modif id &3. " IHDK904062
  selection-screen comment 3(50) &2 modif id &3.  " IHDK904062
  selection-screen end of line.
end-of-definition.

* Selection Screen *
* Radio buttons for different material types *
selection-screen begin of block b1 with frame title text-001.
selection-screen begin of line.
parameters: p_rad1 radiobutton group rad user-command abc default 'X' modif id crt.
selection-screen comment 3(50) text-r01 modif id crt.
selection-screen end of line.

mat_type_rad p_rad2  text-r02 ext.
mat_type_rad p_rad3  text-r03 ext.
mat_type_rad p_rad4  text-r04 crt.
mat_type_rad p_rad5  text-r05 ext.
mat_type_rad p_rad6  text-r06 ext.
mat_type_rad p_rad7  text-r07 crt.
mat_type_rad p_rad8  text-r08 ext.
mat_type_rad p_rad9  text-r09 crt.
mat_type_rad p_rad10 text-r10 ext.
mat_type_rad p_rad11 text-r11 crt.
mat_type_rad p_rad12 text-r12 ext.
mat_type_rad p_rad13 text-r13 crt.
mat_type_rad p_rad14 text-r14 ext.
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

* Function key *
selection-screen: function key 1.
