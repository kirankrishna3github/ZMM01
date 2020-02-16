*&---------------------------------------------------------------------*
*&  Include           IRMMVRZ00_ALT
*&---------------------------------------------------------------------*
*& Global Data Declaration for ALV display
*&---------------------------------------------------------------------*

* type-pools ***********************************************************
TYPE-POOLS: slis.

* structures types *****************************************************
TYPES:
  BEGIN OF listwa_type.
INCLUDE TYPE ZZplm_alv_230.
TYPES:
  END   OF listwa_type.

* tables types *********************************************************
TYPES: listtab_type TYPE STANDARD TABLE OF
  listwa_type WITH DEFAULT KEY.

* data *****************************************************************
DATA: gt_list TYPE listtab_type,
      listwa LIKE LINE OF gt_list.

* constants ************************************************************
CONSTANTS: c_marked(1)        TYPE c VALUE 'X',
           c_structure_name   TYPE tabname VALUE 'ZZPLM_ALV_230',
           c_save(1)          TYPE c VALUE 'A',
           c_werk(5)          TYPE c VALUE 'WERKS',
           c_matnr(5)          TYPE c VALUE 'MATNR'.
CONSTANTS:
  BEGIN OF cs_callback,
    program       TYPE syrepid VALUE 'RMMVRZ00',
    pf_status_set TYPE slis_formname VALUE '',
    user_command  TYPE slis_formname VALUE '',
    top_of_page   TYPE slis_formname VALUE '',
  END   OF cs_callback.
