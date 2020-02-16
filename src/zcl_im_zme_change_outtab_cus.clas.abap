class ZCL_IM_ZME_CHANGE_OUTTAB_CUS definition
  public
  final
  create public .

*"* public components of class ZCL_IM_ZME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ME_CHANGE_OUTTAB_CUS .
protected section.
*"* protected components of class ZCL_IM_ZME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_ZME_CHANGE_OUTTAB_CUS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_ZME_CHANGE_OUTTAB_CUS IMPLEMENTATION.


METHOD if_ex_me_change_outtab_cus~fill_outtab.


  " Thursday, March 29, 2018 23:46:03
  " IRDK931530 - MM: S_K: ZME_CHANGE_OUTTAB_CUS: BADI change ME28 output
  " Add delivery complete indicator to output of ME28
  " Works in combination with implicit enhancement: ZENH_DISABLE_RELEASE_FOR_DCI in include LMEREPI20
  " The implict enhancement disables the release/reset fucntion for completely delivered items

  DATA: ls_ekpo TYPE ekpo.

  FIELD-SYMBOLS: <fs_outtab> TYPE any,
                 <fs_ebeln>  TYPE ebeln,
                 <fs_ebelp>  TYPE ebelp,
                 <fs_elikz>  TYPE elikz.

  BREAK 6010859.
* check that a purchasing document view is displayed
  CHECK im_struct_name EQ 'MEREP_OUTTAB_PURCHDOC_REL'    "view: Output Table: Release of Purchasing Documents
    AND sy-tcode EQ 'ME28'.
* loop at the output table and assign a field symbol
  LOOP AT ch_outtab ASSIGNING <fs_outtab>.

*-- assign the purchasing document number to a field symbol
    ASSIGN COMPONENT 'EBELN' OF STRUCTURE <fs_outtab> TO <fs_ebeln>.
    CHECK sy-subrc = 0.
*-- assign the purchasing document item number to a field symbol
    ASSIGN COMPONENT 'EBELP' OF STRUCTURE <fs_outtab> TO <fs_ebelp>.
    CHECK sy-subrc = 0.
*-- assign the delivery complete indicator to a field symbol
    ASSIGN COMPONENT 'ELIKZ' OF STRUCTURE <fs_outtab> TO <fs_elikz>.
    CHECK sy-subrc = 0.

*-- read the corresponding purchasing document item
    CALL FUNCTION 'ME_EKPO_SINGLE_READ'
      EXPORTING
        pi_ebeln         = <fs_ebeln>
        pi_ebelp         = <fs_ebelp>
      IMPORTING
        po_ekpo          = ls_ekpo
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.
    CHECK sy-subrc = 0.

*-- assign the delivery complete status to delivery complete indicator field in the output table
    <fs_elikz> = ls_ekpo-elikz.

  ENDLOOP.

ENDMETHOD.
ENDCLASS.
