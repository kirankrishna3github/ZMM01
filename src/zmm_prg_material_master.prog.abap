*&---------------------------------------------------------------------*
*& Report  ZMM_PRG_MATERIAL_MASTER
*&---------------------------------------------------------------------*
*& Transaction            : ZMAT01
*& Creation Date          : 28.08.2017
*& Author                 : ABAP02 - SaurabhK
*& Functional             : Kamalakar Varma
*& Requested/Approved By  : Kamalakar Varma
*& Application Area       : MM
*& Package                : ZMM01
*& Initial Request#       : IRDK928948
*&---------------------------------------------------------------------*
*& * ---- Description: ---- *
*& 1. Material creation/extension using BAPI_MATERIAL_SAVEDATA
*& 2. -> Input: Excel file for material creation/extension
*& 3. output ->: Excel file for output log after material creation/extension
*& 4. Currently this program supports the following material types:
*&    a. FGM creation
*&    b. FGM extension plant
*&    c. FGM extension depot
*&    d. TRD creation
*&    e. TRD extension plant
*&    f. TRD extension depot
*&    g. RAW/PKG/PKU/CON creation
*&    h. RAW/PKG/PKU/CON extension plant/depot
*&    i. SFG creation
*&    j. SFG extension plant
*&    k. MCO creation
*&    l. MCO extension plant
*&    m. Y/ZSPR, YSPI, ZEU2 creation
*&    n. Y/ZSPR, YSPI, ZEU2 ext. plant
*& 5. The input file format for each of these is different, (can be found @ https://filebin.ca/3iTd38fsGVRY/ZMAT01fileformats.xlsx)
*&    containing material parameters required to be maintained for each of these material types
*& 6. For extension, there's logic to determine the reference data(plant, company code etc), derived based on the material no. to be extended
*&    and the target plant to which it will be extended
*&---------------------------------------------------------------------*
*======================================================================*
*& * ---- Revision History ---- *
*** Note: There have been previous revision iterations that have not been logged in the revision history ***
*** Please check the request trail in the version history in PRD for the same ***
*** Verbose logging started on Saturday, November 25, 2017 11:31:17 ***
*&---------------------------------------------------------------------*
*& Revision #                 : 01
*& Rev. Date                  : Saturday, November 25, 2017 11:31:17
*& Rev. Author                : ABAP02 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930243
*& Rev. Description           : 1. Add default material tax indicator '0' in TRD create, extend(both plant/depot)
*&---------------------------------------------------------------------*
*& Revision #                 : 02    # Rev 02
*& Rev. Date                  : Wednesday, December 06, 2017 09:38:26
*& Rev. Author                : ABAP02 - SaurabhK
*& Rev. Requested/Approved By : Pending development
*& Rev. Request#              : IRDK930381
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add priliminary file format dwnload functn
*&---------------------------------------------------------------------*
*& Revision #                 : 03    # Rev 03
*& Rev. Date                  : Tuesday, January 02, 2018 09:23:13
*& Rev. Author                : ABAP02 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930617
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add SFG create function
*&---------------------------------------------------------------------*
*& Revision #                 : 04    # Rev 04  # IRDK930708
*& Rev. Date                  : Thursday, January 11, 2018 15:39:46
*& Rev. Author                : ABAP02 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930708
*& Rev. Description           : 1.  During extension to plant/depot for all material type:
*&                                  a.  Check if material exists
*&                                  b.  Check if material has already been extended to target plant
*&                                  c.  Re-maintain tax classification as in case of creation rather than relying on source plant classification.
*&                                 	d.  Check if material is blocked for any reason
*&                              2.  Renamed ZFGM to ( ZFGM, ZLFG ) to enhance clarity.
*&---------------------------------------------------------------------*
*& Revision #                 : 05    # Rev 05
*& Rev. Date                  : Wednesday, February 07, 2018 14:48:18
*& Rev. Author                : ABAP02 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK930982
*& Rev. Description           : 1. MM: S_K: ZMAT01: Implement synchronous updates via wait parameter in commit
*&---------------------------------------------------------------------*
*& Revision #                 : 06    # Rev 06
*& Rev. Date                  : Monday, February 12, 2018 11:08:13
*& Rev. Author                : ABAP02 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK931006
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add SFG extend to plant function
*&---------------------------------------------------------------------*
*& Revision #                 : 07    # Rev 07
*& Rev. Date                  : Friday, March 16, 2018 09:45:07
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK931413
*& Rev. Description           : 1. MM: S_K: ZMAT01: mrp type = nd for pkg/pku depot extend
*&---------------------------------------------------------------------*
*& Revision #                 : 08    # Rev 08
*& Rev. Date                  : Monday, April 02, 2018 10:06:13
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK931550
*& Rev. Description           : 1. MM: S_K: ZMAT01: default tick hkmat, ekalr in raw/pkg mat
*&                              2. Indicate valuation class extension status in index field of output file (* => failed)
*&---------------------------------------------------------------------*
*& Revision #                 : 09    # Rev 09
*& Rev. Date                  : Thursday, April 26, 2018 16:37:47
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK931823
*& Rev. Description           : 1. Add ZMCO create/extend to plant
*&---------------------------------------------------------------------*
*& Revision #                 : 10    # Rev 10
*& Rev. Date                  : Thursday, May 31, 2018 14:12:15
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK932322, IRDK932334
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add Engg Spares type material creation
*&---------------------------------------------------------------------*
*& Revision #                 : 11    # Rev 11
*& Rev. Date                  : Friday, June 01, 2018 15:59:28
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK932350
*& Rev. Description           : 1. MM: S_K: ZMAT01: Engg. spares extend to plant added
*&---------------------------------------------------------------------*
*& Revision #                 : 12    # Rev 12
*& Rev. Date                  : Friday, June 15, 2018 15:12:15
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK932498
*& Rev. Description           : 1. MM: S_K: ZMAT01: Fixes for log o/p for bwtar ext: 15/06/18
*&---------------------------------------------------------------------*
*& Revision #                 : 13    # Rev 13
*& Rev. Date                  : Friday, June 22, 2018 14:50:32
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma
*& Rev. Request#              : IRDK932590
*& Rev. Description           : 1. MM: S_K: ZMAT01: Disable insp type 02 for ZFGM/ZLFG: 22/06
*&---------------------------------------------------------------------*
*& Revision #                 : 14    # Rev 14
*& Rev. Date                  : Tuesday, December 11, 2018 23:20:20
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma/Anish Verma
*& Rev. Request#              : SBXK900429
*& Rev. Description           : 1. S4H Requirement: Add MRP Area
*&---------------------------------------------------------------------*
*& Revision #                 : 15    # Rev 15
*& Rev. Date                  : Tuesday, January 15, 2019 12:57:19
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma/VG
*& Rev. Request#              : IHDK900227
*& Rev. Description           : 1. YSPR/ZSPR extension to plant: pick HSN code(subject to certain conditions) and MRP type from file
*&                              2. Save o/p log to default directory desktop/MM_log -> requested by venu sir
*&---------------------------------------------------------------------*
*& Revision #                 : 16    # Rev 16
*& Rev. Date                  : Tuesday, January 22, 2019 12:34:49
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma/VG
*& Rev. Request#              : IHDK900305
*& Rev. Description           : 1. Authorisation check on material type and plant
*&                              2. Pick prctr and kosgr from zmat01_pctr_mast for all material types/removed from file formats
*&                              3. ZFGM/ZLFG: set default values for vprsv, peinh, stprs, remove verpr/removed fields from file format
*&                              4. ZFGM/ZLFG plant ext: do not copy dss prof from source plant
*&                              5. Display log in ALV format
*&                              6. Re-arrange message column in log
*&---------------------------------------------------------------------*
*& Revision #                 : 17    # Rev 17
*& Rev. Date                  : Thursday, February 07, 2019 10:05:14
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Kamalakar Varma/VG
*& Rev. Request#              : IHDK900502
*& Rev. Description           : 1. MM: S_K: ZMAT01: Catch exception from conv exit: 7.2.19
*&---------------------------------------------------------------------*
*& Revision #                 : 18    # Rev 18
*& Rev. Date                  : Monday, February 18, 2019 16:12:46
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK900652
*& Rev. Description           : MM: S_K: ZMAT01: Auto extend to storage loc: 18.2.19/Misc checks
*&---------------------------------------------------------------------*
*& Revision #                 : 19    # Rev 19
*& Rev. Date                  : Wednesday, February 20, 2019 09:57:03
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK900679, IHDK900685
*& Rev. Description           : 1. MM: S_K: ZMAT01: Improve except handling: 20.9.18
*&                              2. MM: S_K: ZMAT01: Add div to sloc ext: 20.2.19
*&---------------------------------------------------------------------*
*& Revision #                 : 20    # Rev 20
*& Rev. Date                  : Tuesday, March 12, 2019 00:41:11
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK900894
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add plant existence check: 11.3.19
*&---------------------------------------------------------------------*
*& Revision #                 : 21    # Rev 21
*& Rev. Date                  : 12.03.2019 09:31:15
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : Improvement
*& Rev. Request#              : IHDK900896, IHDK900903
*& Rev. Description           : 1. MM: S_K: ZMAT01: Auth re-org fixes: 12.3.19
*&---------------------------------------------------------------------*
*& Revision #                 : 22    # Rev 22
*& Rev. Date                  : 26.03.2019 08:48:48
*& Rev. Author                : 6010859 - SaurabhK/10106 - PS
*& Rev. Requested/Approved By : VG/Indoreagans
*& Rev. Request#              : IHDK900999
*& Rev. Description           : 1. Changes for incorporating Indoreagans(2800)
*&---------------------------------------------------------------------*
*& Revision #                 : 23    # Rev 23
*& Rev. Date                  : Monday, April 01, 2019 09:58:51
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK901148
*& Rev. Description           : 1. MM: S_K: ZMAT01: Changes for acc. ass. grp, classif: 1.4.19
*&---------------------------------------------------------------------*
*& Revision #                 : 24    # Rev 24
*& Rev. Date                  : Thursday, June 13, 2019 15:50:39
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK902087, IHDK902348
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add ZCON create/ext: 13.6.19
*&                              2. Move struct definitions to global interface
*&                              3. MM: S_K: ZMAT01: Fix file format chk zif_mm_mat01: 9.7.19
*&---------------------------------------------------------------------*
*& Revision #                 : 25    # Rev 25
*& Rev. Date                  : 16.10.2019
*& Rev. Author                : Varun Kolluru
*& Rev. Requested/Approved By : Punam
*& Rev. Request#              : IHDK903516
*& Rev. Description           : 1. Added new authorization code for plant and material type
*&---------------------------------------------------------------------*
*& Revision #                 : 24    # Rev 24
*& Rev. Date                  : Friday, October 25, 2019 12:38:56
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : KV
*& Rev. Request#              : IHDK903652
*& Rev. Description           : 1. MM: S_K: ZMAT01: RAW/PKG mbew-verpr non-zero: 25.10.19
*&---------------------------------------------------------------------*
*& Revision #                 : 25    # Rev 25
*& Rev. Date                  : Wednesday, November 13, 2019 12:25:15
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : KV
*& Rev. Request#              : IHDK903847
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add commit b4 classif: 13.11.19
*&---------------------------------------------------------------------*
*& Revision #                 : 26    # Rev 26
*& Rev. Date                  : Thursday, November 28, 2019 3:21:36 PM
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK904062
*& Rev. Description           : 1. MM: S_K: ZMAT01: Param en/disable create/extend: 28.11.19
*&---------------------------------------------------------------------*
*& Revision #                 : 27    # Rev 27
*& Rev. Date                  : Monday, January 06, 2020 12:49:25
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : KV
*& Rev. Request#              : IHDK904473
*& Rev. Description           : 1. Update log to DB(ZMM_T_MAT01_LOG), Re-org matnr generation
*&---------------------------------------------------------------------*
*& Revision #                 : 28    # Rev 28
*& Rev. Date                  : Wednesday, March 25, 2020 13:40:38
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : KV/VG
*& Rev. Request#              : IHDK905737
*& Rev. Description           : 1. MM: S_K: ZMAT01: Disable classif for RAW/PKG/PKU: 25.3.20
*&---------------------------------------------------------------------*
*& Revision #                 : 29    # Rev 29
*& Rev. Date                  : Friday, May 08, 2020 19:19:55
*& Rev. Author                : 6010859 - SaurabhK
*& Rev. Requested/Approved By : VG
*& Rev. Request#              : IHDK906383
*& Rev. Description           : 1. MM: S_K: ZMAT01: Add acc assign grp for RAW/PKG: 8.5.20
*&---------------------------------------------------------------------*
program zmm_prg_material_master message-id zmm.

include zmm_inc_mat_mast_top.       " Global Data
include zmm_inc_mat_mast_lcl_def.   " Local class/interface definitions
include zmm_inc_mat_mast_lcl_impl.  " Local class/interface implementations

* ---- load of program ---- *
load-of-program.
* Initialise base class so that its methods can be used in selection screen events *
  try.
      free lo_mm.
      lo_mm = new lcl_mm( ).
    catch cx_root into lo_cx.
      message lo_cx->get_text( ) type 'E'.
  endtry.

* ---- Initialization ---- *
* Set additional function button attributes *
  try.
      lo_mm->set_func_buttons( ).
    catch cx_root into lo_cx.
      message lo_cx->get_text( ) type 'E'.
  endtry.

* ---- Selection screen events ---- *
at selection-screen output.
  " IHDK904062
  try.
      lo_mm->modify_screen( ).
    catch lcx_generic_error into data(lox).
      message lox->get_text( ) type 'E'.
  endtry.

* Change frame titles on selection screen based on selected radio button *
  try.
      lo_mm->set_frame_title(
        changing
          dwnld = dwnld
          upld  = upld ).
    catch cx_root into lo_cx.
      message lo_cx->get_text( ) type 'E'.
  endtry.

at selection-screen on value-request for p_file.
* Load file from user front-end/F4 for file *
  try.
      lo_mm->file_open( ).
    catch lcx_generic_error.
      return.
  endtry.

at selection-screen.
* File is mandatory for execution *
  if sscrfields-ucomm eq 'ONLI' and p_file is initial.  " ONLI = Execute
    lo_mm->disp_message(
      type = 'E'
      id   = 'ZMM'
      no   = '003' ).
    try.
        raise exception type lcx_generic_error.
      catch lcx_generic_error.
        stop.
    endtry.
  endif.

* File format download/other selection screen functions *
  try.
      lo_mm->handle_sel_screen_ucomm( ).
    catch cx_root into lo_cx.
      message lo_cx->get_text( ) type 'E'.
  endtry.

* Start of Main *
start-of-selection.
  try.
      lcl_main=>main( ).
    catch cx_root into lo_cx.
      message lo_cx->get_text( ) type 'S' display like 'E'.
      return.
  endtry.

end-of-selection.
