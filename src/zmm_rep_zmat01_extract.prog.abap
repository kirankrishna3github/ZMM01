*&---------------------------------------------------------------------*
*& Report  ZMM_REP_ZMAT01_EXTRACT
*&---------------------------------------------------------------------*
*& Transaction            : ZMM079
*& Creation Date          : 17.11.2017
*& Author                 : ABAP02 - SaurabhK
*& Functional             : Kamalakar Varma
*& Requested/Approved By  : Kamalakar Varma
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK930157
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*& 1. This report simplifies data extraction for the extension of materials via ZMAT01
*& 2. The default level 1 ALV structure is identical to the extension file format for these material types
*& 3. Level 1 will display material, all plants of that material, 1 stor loc per plant, 1 val type per plant
*& 4. The hotspot on plant can be used to display either all stor loc's or all val types(if avaialble) of a given material-plant combination
*& 5. This is the level 2 ALV displayed by trigerring a link_click event
*& 6. The hotspot on material can be used to navigate to mm03 for additional details
*&---------------------------------------------------------------------*
*======================================================================*
*& * ---- Revision History ---- *
*& Revision #                 : 01
*& Rev. Date                  : Thursday, November 23, 2017 14:49:00
*& Rev. Author                : ABAP02, SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930215
*& Rev. Description           : 1. Add division column for RAW,Packing materials
*&                              2. Display 0 values as blank
*&---------------------------------------------------------------------*
*& Revision #                 : 02
*& Rev. Date                  : Friday, November 24, 2017 10:22:22
*& Rev. Author                : ABAP02, SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930227
*& Rev. Description           : 1. RAW/Packing, move div column to the end
*&---------------------------------------------------------------------*
*& Revision #                 : 03
*& Rev. Date                  : Friday, November 24, 2017 10:35:56
*& Rev. Author                : ABAP02, SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930229
*& Rev. Description           : 1. FGM: Display moving average price column- verpr as blank
*&                              2. TRD: Fix: Planned delv. -> plifz - displayed twice ( reason webaz is also typed as plifz)
*&---------------------------------------------------------------------*
*& Revision #                 : 04  # Rev 04
*& Rev. Date                  : Friday, December 01, 2017 16:17:22
*& Rev. Author                : ABAP02, SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930357, IRDK930359
*& Rev. Description           : 1. Add order by clause in select queries for sorting, helps in displaying correct strg loc (first in table)
*&---------------------------------------------------------------------*
*& Revision #                 : 05  # Rev 05
*& Rev. Date                  : Monday, July 08, 2019 15:42:29
*& Rev. Author                : 6010859, SaurabhK
*& Rev. Requested/Approved By : VG/Kamalakar Varma
*& Rev. Request#              : IHDK902288, IHDK902332
*& Rev. Description           : 1. MM: S_K: ZMM079: incorporate & update all mat types: 5.7.19
*&                              2. MM: S_K: ZMM079 - auth check, tpool/ZMAT01: tpool: 8.7.19
*&---------------------------------------------------------------------*

report zmm_rep_zmat01_extract.

include zmm_inc_zmat01_extrct_top.
include zmm_inc_zmat01_extrct_lcl_def.
include zmm_inc_zmat01_extrct_lcl_impl.

" IRDK930359, Rev 04

at selection-screen.
  if sscrfields-ucomm eq 'ONLI'.
    if s_matnr is initial and s_werks is initial.
      set cursor field 'S_MATNR'.
      message id '00' type 'S' number '055' display like 'E'.
      stop.
    endif.
  endif.
  if sy-dynnr eq '1001'.
    gv_1001_ucomm = sscrfields-ucomm.
  endif.
  " End IRDK930359, Rev 04

* Start of Main *
start-of-selection.
  try.
      lcl_main=>main( ).
    catch cx_root into lo_cx.
      message lo_cx->get_text( ) type 'S' display like 'E'.
      return.
  endtry.

end-of-selection.
