*&---------------------------------------------------------------------*
*&  Include           ZINC_FV11_CONDN_DD
*&---------------------------------------------------------------------*

* Data Declaration *
* Types *
TYPES: BEGIN OF ty_tab_359,
         kschl(20) TYPE c,
         werks(20) TYPE c,
         matkl(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_359,

       BEGIN OF ty_tab_504,
         kschl(20) TYPE c,
         werks(20) TYPE c,
         lifnr(20) TYPE c,
         matnr(20) TYPE c,
         mwskz(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_504,

       BEGIN OF ty_tab_515,
         kschl(20) TYPE c,
         werks(20) TYPE c,
         lifnr(20) TYPE c,
         mwskz(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_515,

       BEGIN OF ty_tab_519,
         kschl(20) TYPE c,
         lifnr(20) TYPE c,
         mwskz(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_519,

       BEGIN OF ty_tab_536,
         kschl(20) TYPE c,
         mwskz(20) TYPE c,
         datab(20) TYPE c,
         datbi(20) TYPE c,
       END OF ty_tab_536.

* Tables *
DATA: it_tab_359 TYPE TABLE OF ty_tab_359,
      wa_tab_359 TYPE ty_tab_359,

      it_a359    TYPE TABLE OF a359,
      wa_a359    TYPE a359,

      it_tab_504 TYPE TABLE OF ty_tab_504,
      wa_tab_504 TYPE ty_tab_504,

      it_a504    TYPE TABLE OF a504,
      wa_a504    TYPE a504,

      it_tab_515 TYPE TABLE OF ty_tab_515,
      wa_tab_515 TYPE ty_tab_515,

      it_a515    TYPE TABLE OF a515,
      wa_a515    TYPE a515,

      it_tab_519 TYPE TABLE OF ty_tab_519,
      wa_tab_519 TYPE ty_tab_519,

      it_a519    TYPE TABLE OF a519,
      wa_a519    TYPE a519,

      it_tab_536 TYPE TABLE OF ty_tab_536,
      wa_tab_536 TYPE ty_tab_536,

      it_a536    TYPE TABLE OF a536,
      wa_a536    TYPE a536.

* Variables *
DATA: indx(4)     TYPE c,
      v_datum(10) TYPE c.
