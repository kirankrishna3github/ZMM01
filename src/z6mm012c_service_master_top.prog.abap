*&---------------------------------------------------------------------*
*&  Include           Z6MM010C_SERVICE_MASTER_TOP
*&---------------------------------------------------------------------*



data : begin of tdata occurs 0,
        ASNUM    like ASMD-ASNUM,
        ASKTX    like ASMDT-ASKTX,
        ASTYP    like ASMD-ASTYP,
        MEINS    like ASMD-MEINS,
        MATKL    like ASMD-MATKL,
        SPART    like ASMD-SPART,
        BKLAS    like ASMD-BKLAS,
        FORMELNR like ASMD-FORMELNR,
        ltext    type string,
       end of tdata.

data   bdcdata like bdcdata occurs 1000 with header line.
