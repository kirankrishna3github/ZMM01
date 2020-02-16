FUNCTION-POOL zmm_vendor_plant.             "MESSAGE-ID ..
* dynpro output structure
TABLES: ci_ekkodb.
DATA: call_subscreen TYPE sy-dynnr,                         "#EC NEEDED
      call_prog TYPE sy-repid,                              "#EC NEEDED
      call_view TYPE REF TO cl_screen_view_mm,              "#EC NEEDED
     call_view_stack TYPE REF TO cl_screen_view_mm OCCURS 0,"#EC NEEDED
      global_framework TYPE REF TO cl_framework_mm,         "#EC NEEDED
      global_help_view TYPE REF TO cl_screen_view_mm,       "#EC NEEDED
      global_help_prog TYPE sy-repid.                       "#EC NEEDED
DATA wkafields TYPE string40.
DATA : lt_vendor TYPE TABLE OF zmm_vendor_tag.
DATA : wa_vend TYPE zmm_vendor_tag.
DATA : lv_region TYPE t005u-bezei.
  DATA : l_glintname TYPE char30.
  DATA : l_glintname1 TYPE char30.
  DATA : lt_mepo_topline  TYPE  mepo_topline.
  DATA : lt_ci_ekkodb TYPE ci_ekkodb.
  DATA : lv_ucomm TYPE sy-ucomm.
*   Field Symbols
  FIELD-SYMBOLS:
           <lfs_tab>   TYPE  mepo_topline,
           <lfs_tab1>  TYPE ci_ekkodb.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
*  SET PF-STATUS 'ZMM_VENDOR'.
*  SET TITLEBAR 'xxx'.

*IF sy-tcode = 'ME22N' OR sy-tcode = 'ME23N' OR sy-tcode = 'ME29N'.
    CLEAR: l_glintname.
    l_glintname = '(SAPLMEGUI)MEPO_TOPLINE'.
    ASSIGN (l_glintname) TO <lfs_tab>.
    lt_mepo_topline  = <lfs_tab>.

*ENDIF.

* Get Vendor Address Details from ztable ZMM_VENDOR_TAG
IF lt_mepo_topline-lifnr IS NOT INITIAL.
    SELECT * FROM zmm_vendor_tag
    INTO TABLE lt_vendor
    WHERE lifnr = lt_mepo_topline-lifnr
    AND  werks = ci_ekkodb-zzvendorplant.
    READ TABLE lt_vendor INTO wa_vend INDEX 1.
IF wa_vend IS NOT INITIAL.
    SELECT SINGLE bezei FROM t005u INTO (lv_region)
    WHERE land1 = wa_vend-country
    AND bland = wa_vend-region.
ENDIF.
ENDIF.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.
  IF sy-ucomm = 'MESAVE' OR sy-ucomm = space.
    CLEAR : wa_vend , lv_region.
  ENDIF.
ENDMODULE.                 " USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pbo OUTPUT.
  IF sy-ucomm = 'MESAVE' OR sy-ucomm = space.
    CLEAR : wa_vend , LV_region.
  ENDIF.
  CALL METHOD call_view->handle_event( 'PBO' ).
   wkafields = text-002.
ENDMODULE.                 " EVENT_PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EVENT_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE event_pai INPUT.
  CALL METHOD call_view->handle_event( 'PAI' ).

ENDMODULE.                 " EVENT_PAI  INPUT

FORM set_subscreen_and_prog USING dynnr TYPE sy-dynnr
                                  prog TYPE sy-repid
                                  view TYPE REF TO cl_screen_view_mm.
  call_subscreen = dynnr.
  call_prog = prog.
  call_view = view.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  F4HELP_FOR_PLANT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4help_for_plant INPUT.

* Data Declaration

TYPES : BEGIN OF ty_werks ,
          lifnr TYPE zmm_vendor_tag-lifnr,
          werks TYPE  zmm_vendor_tag-werks,
          city1 TYPE zmm_vendor_tag-city1,
          country TYPE zmm_vendor_tag-country,
          region TYPE zmm_vendor_tag-region,
          bezei TYPE t005u-bezei,
        END OF ty_werks.

TYPES : BEGIN OF ty_final,
           lifnr TYPE zmm_vendor_tag-lifnr,
          werks TYPE zmm_vendor_tag-werks,
          city1 TYPE zmm_vendor_tag-city1,
          country TYPE zmm_vendor_tag-country,
          bezei TYPE zmm_state,  " TYPE t005u-bezei,
        END OF ty_final.


  DATA : lt_plant TYPE TABLE OF zmm_vendor_tag,
*         lt_vendor TYPE TABLE OF zmm_vendor_tag,
         wa_plant TYPE zmm_vendor_tag.
*         gv_LIFNR type zmm_vendor_tag-lifnr.

  DATA : it_werks TYPE TABLE OF ty_werks.
  DATA : wa_werks LIKE LINE OF it_werks.


  DATA : lt_final TYPE TABLE OF ty_final.
  DATA : ls_final LIKE LINE OF lt_final.


  DATA :it_fill        TYPE TABLE OF zstru_amt WITH HEADER LINE,
        lit_dfldmap    TYPE TABLE OF dselc,
        lwa_dfldmap    LIKE LINE OF  lit_dfldmap,
        lwrk_cprog     TYPE          sy-cprog,
        lwrk_dynnr     TYPE          sy-dynnr.

  DATA : BEGIN OF lt_t005u OCCURS 0,
          land1 TYPE t005u-land1,
          bland TYPE t005u-bland,
          bezei TYPE t005u-bezei,
  END OF lt_t005u.

  DATA : ls_t005u LIKE LINE OF lt_t005u .

                           " Dummy Internal table
 CLEAR: l_glintname.
    l_glintname = '(SAPLMEGUI)MEPO_TOPLINE'.
    ASSIGN (l_glintname) TO <lfs_tab>.
    lt_mepo_topline  = <lfs_tab>.

* Implementation

REFRESH : lt_final , it_werks, lit_dfldmap.

  CLEAR: lt_plant, lwrk_cprog, lwrk_dynnr.
  lwrk_cprog = sy-cprog.
  lwrk_dynnr = sy-dynnr.

    SELECT lifnr
           werks
           city1
           country
      region
       FROM zmm_vendor_tag
    INTO TABLE it_werks WHERE lifnr = lt_mepo_topline-lifnr.

IF it_werks IS NOT INITIAL.
SELECT * FROM t005u INTO CORRESPONDING FIELDS OF TABLE lt_t005u
  FOR ALL ENTRIES IN it_werks
  WHERE land1 = it_werks-country
  AND bland = it_werks-region.
ENDIF.

LOOP AT it_werks INTO wa_werks.
  READ TABLE lt_t005u INTO ls_t005u WITH KEY  bland =  wa_werks-region.
  wa_werks-bezei = ls_t005u-bezei.
  MODIFY it_werks FROM wa_werks.
ENDLOOP.

LOOP AT it_werks INTO wa_werks.
  MOVE-CORRESPONDING wa_werks TO ls_final.
  APPEND ls_final TO lt_final.
ENDLOOP.

  REFRESH : lit_dfldmap.   CLEAR: lwa_dfldmap.

  CLEAR: lwa_dfldmap.
  lwa_dfldmap-fldname   = 'WERKS'.
  lwa_dfldmap-dyfldname = 'CI_EKKODB-ZZVENDORPLANT'.
  APPEND lwa_dfldmap TO lit_dfldmap.

  DATA : cn_value TYPE  help_info-fldvalue VALUE 'X'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
      retfield               = 'WERKS'
*   PVALKEY                = ' '
     dynpprog               = lwrk_cprog
     dynpnr                 = lwrk_dynnr
     dynprofield            = 'CI_EKKODB-ZZVENDORPLANT'
     value                  = cn_value
     value_org              = 'S'
    TABLES
  value_tab       = lt_final      "it_werks
  dynpfld_mapping = lit_dfldmap

   EXCEPTIONS
     parameter_error        = 1
     no_values_found        = 2
     OTHERS                 = 3
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.



ENDMODULE.                 " F4HELP_FOR_PLANT  INPUT
