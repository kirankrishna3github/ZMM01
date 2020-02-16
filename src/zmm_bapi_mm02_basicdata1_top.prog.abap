*&---------------------------------------------------------------------*
*& Include          ZMM_BAPI_MM02_BASICDATA1_TOP
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_data,
         matnr(18) TYPE c,         "MaTerial Code
         maktx(40) TYPE c,         "Material Desc.
         brgew(13) TYPE c,         "Gross Wt.
         ntgew(13) TYPE c,         "Net Wt.
         volum(13) TYPE c,         "Volum
         voleh(3)  TYPE c,         "Volum Unit
         extwg(18) TYPE c,         "External material group; IHDK901024
         werks(4)  TYPE c,         "Plant
         lgort(4)  TYPE c,         "Storage location
         lgpbe(10) TYPE c,         "Storage Bin
       END OF ty_data.

DATA: lt_data TYPE TABLE OF ty_data,
      lw_data TYPE ty_data.

"added by varun on 15.11.19 as said by saurabh
"structure for ZMM093
TYPES: BEGIN OF ty_data1,
         matnr(18) TYPE c,         "MaTerial Code
         maktx(40) TYPE c,         "Material Desc.
         werks(4)  TYPE c,         "Plant
         lgort(4)  TYPE c,         "Storage location
         lgpbe(10) TYPE c,         "Storage Bin
       END OF ty_data1.

DATA: it_file TYPE TABLE OF ty_data1,
      wa_file TYPE ty_data1.

TYPES: BEGIN OF ty_matnr,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         meins TYPE mara-meins,
         gewei TYPE mara-gewei,
       END OF ty_matnr.

DATA: lt_mara TYPE TABLE OF ty_matnr,
      lw_mara TYPE ty_matnr.

TYPES: BEGIN OF ty_msg,
         flag    TYPE char4,           "ICON-ID, "
         matnr   TYPE mara-matnr,
         type    TYPE bapiret2-type,
         message TYPE bapiret2-message,
       END OF ty_msg.

DATA: lt_msg TYPE TABLE OF ty_msg,
      lw_msg TYPE ty_msg.

*--------- BAPI Structure Decleration ---------------------------------*
DATA: lw_headdata TYPE bapimathead.                    "Header Data

DATA: lw_storagelocationdata  TYPE bapi_mard,          " Information on update for PLANNINGDATA
      lw_storagelocationdatax TYPE bapi_mardx.

DATA: lt_materialdescription TYPE TABLE OF bapi_makt,  "Material Description
      lw_materialdescription TYPE bapi_makt.

DATA: lt_unitsofmeasure TYPE TABLE OF bapi_marm,       "UoM
      lw_unitsofmeasure TYPE bapi_marm.

DATA: lt_unitsofmeasurex TYPE TABLE OF bapi_marmx,     "UoM X
      lw_unitsofmeasurex TYPE bapi_marmx.

DATA: lt_clientdata TYPE TABLE OF bapi_mara,
      lw_clientdata TYPE bapi_mara.

DATA: lt_clientdatax TYPE TABLE OF bapi_marax,
      lw_clientdatax TYPE bapi_marax.

DATA: lt_return TYPE TABLE OF bapiret2,
      lw_return TYPE bapiret2.

CONSTANTS: abap_true TYPE abap_bool VALUE 'X'.

*----------  ALV Data Decleration ---------------------*
DATA: lw_layout TYPE slis_layout_alv.
DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
      lw_fieldcat LIKE LINE OF lt_fieldcat.
