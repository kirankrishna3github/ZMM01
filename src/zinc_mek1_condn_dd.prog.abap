*&---------------------------------------------------------------------*
*&  Include           ZINC_MEK1_CONDN_DD
*&---------------------------------------------------------------------*

* Data Declaration *
* Types *
TYPES: BEGIN OF ty_tab_445,
         kschl(20) TYPE c,
         matnr(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_445,

       BEGIN OF ty_tab_505,
         kschl(20) TYPE c,
         werks(20) TYPE c,
         lifnr(20) TYPE c,
         matnr(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_505,

       BEGIN OF ty_tab_507,
         kschl(20) TYPE c,
         werks(20) TYPE c,
         lifnr(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_507,

       BEGIN OF ty_tab_516,
         kschl(20) TYPE c,
         werks(20) TYPE c,
         matnr(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_516,

       BEGIN OF ty_tab_518,
         kschl(20) TYPE c,
         reswk(20) TYPE c,
         matnr(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_518,

       BEGIN OF ty_tab_540,
         kschl(20) TYPE c,
         reswk(20) TYPE c,
         matnr(20) TYPE c,
         werks(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_540,

       BEGIN OF ty_tab_541,
         kschl(20) TYPE c,
         reswk(20) TYPE c,
         werks(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_541.

* Tables *
DATA: it_tab_445 TYPE TABLE OF ty_tab_445,
      wa_tab_445 TYPE ty_tab_445,

      it_a445    TYPE TABLE OF a445,
      wa_a445    TYPE a445,

      it_tab_505 TYPE TABLE OF ty_tab_505,
      wa_tab_505 TYPE ty_tab_505,

      it_a505    TYPE TABLE OF a505,
      wa_a505    TYPE a505,

      it_tab_507 TYPE TABLE OF ty_tab_507,
      wa_tab_507 TYPE ty_tab_507,

      it_a507    TYPE TABLE OF a507,
      wa_a507    TYPE a507,

      it_tab_516 TYPE TABLE OF ty_tab_516,
      wa_tab_516 TYPE ty_tab_516,

      it_a516    TYPE TABLE OF a516,
      wa_a516    TYPE a516,

      it_tab_518 TYPE TABLE OF ty_tab_518,
      wa_tab_518 TYPE ty_tab_518,

      it_a518    TYPE TABLE OF a518,
      wa_a518    TYPE a518,

      it_tab_540 TYPE TABLE OF ty_tab_540,
      wa_tab_540 TYPE ty_tab_540,

      it_a540    TYPE TABLE OF a540,
      wa_a540    TYPE a540,

      it_tab_541 TYPE TABLE OF ty_tab_541,
      wa_tab_541 TYPE ty_tab_541,

      it_a541    TYPE TABLE OF a541,
      wa_a541    TYPE a541.

* Variables *
DATA: indx(4)   TYPE c,
      v_datum(10) TYPE c.
