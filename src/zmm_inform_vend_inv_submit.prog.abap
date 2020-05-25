*&---------------------------------------------------------------------*
*& Report ZMM_INFORM_VEND_INV_SUBMIT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMM_INFORM_VEND_INV_SUBMIT.
TABLES:BKPF , BSEG ,BSIS .


****Company code – BSIS-BUKRS
****Document Number – BSIS -BELNR
****Year - BSIS –GKAHR
****Document date – BSIS-BLDAT
****Plant – BSIS-WERKS

TYPES: BEGIN OF ty_BSIS,
        bukrs TYPE bsis-bukrs ,
        hkont TYPE bsis-hkont ,
        belnr TYPE bsis-belnr ,
        gjahr TYPE bsis-gjahr ,
        zuonr TYPE bsis-zuonr ,
        blart TYPE bsis-blart ,
        bldat TYPE bsis-bldat ,
        xblnr TYPE bsis-xblnr ,
        shkzg TYPE bsis-shkzg ,
        dmbtr TYPE bsis-dmbtr ,
        werks TYPE bsis-werks ,
       END OF ty_BSIS.
DATA: IT_BSIS TYPE TABLE OF ty_BSIS.

DATA : lv_save(1) TYPE c,
       lv_exit(1) TYPE c,
       ls_variant TYPE disvariant,
       gs_variant TYPE disvariant.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_Lifnr FOR bseg-lifnr,
                s_bukrs FOR bkpf-bukrs no-EXTENSION no INTERVALS OBLIGATORY,
                s_belnr FOR bkpf-belnr,
                s_GJAHR FOR bkpf-gjahr no-EXTENSION no INTERVALS OBLIGATORY,
                s_bldat FOR BSIS-bldat OBLIGATORY,
                s_werks FOR BSIS-werks OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

"ALV variant
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b2.


*&------------------------------------------------------------&*
*&                Field symbols Declaration
*&------------------------------------------------------------&*
FIELD-SYMBOLS <fs_x> TYPE x.

CLASS lcl_module DEFINITION.
  PUBLIC SECTION.
    METHODS: f4_variant,variant_exist,init_variant , process, get_data, call_sf, display_alv, send_mail ,
      change_col_text IMPORTING columnname TYPE lvc_fname
                                short      TYPE scrtext_s
                                medium     TYPE scrtext_m
                                long       TYPE scrtext_l,
      user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_module IMPLEMENTATION.
METHOD f4_variant.

    lv_save = 'A'.
    ls_variant-report = sy-repid.

    CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
      EXPORTING
        is_variant = ls_variant
        i_save     = lv_save
      IMPORTING
        e_exit     = lv_exit
        es_variant = gs_variant
      EXCEPTIONS
        not_found  = 2.

    IF sy-subrc EQ 2.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      IF lv_exit EQ space.
        p_vari = gs_variant-variant.
      ENDIF.
    ENDIF.
  ENDMETHOD.
METHOD variant_exist.
    DATA : lv_save(1) TYPE c.

    IF NOT p_vari IS INITIAL.

      MOVE p_vari TO gs_variant-variant.

      lv_save = 'A'.
      gs_variant-report = sy-repid.
      CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
        EXPORTING
          i_save     = lv_save
        CHANGING
          cs_variant = gs_variant.

    ELSE.
      init_variant( ).
    ENDIF.
ENDMETHOD.
METHOD init_variant.
    DATA : lv_save(1) TYPE c.

    CLEAR : gs_variant.

    lv_save = 'A'.
    gs_variant-report = sy-repid.

    CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
      EXPORTING
        i_save        = lv_save
      CHANGING
        cs_variant    = gs_variant
      EXCEPTIONS
        wrong_input   = 1
        not_found     = 2
        program_error = 3
        OTHERS        = 4.

    IF sy-subrc EQ 0.
      p_vari = gs_variant-variant.
    ENDIF.
ENDMETHOD.
METHOD process.

ENDMETHOD.
METHOD get_data.

ENDMETHOD.
METHOD call_sf.

ENDMETHOD.
METHOD display_alv.

ENDMETHOD.
METHOD send_mail.

endmethod.
METHOD change_col_text.

ENDMETHOD.
METHOD user_command.

ENDMETHOD.
ENDCLASS.

LOAD-OF-PROGRAM.
  DATA lo_mod TYPE REF TO lcl_module.
  lo_mod = NEW #( ).

START-OF-SELECTION.

  IF lo_mod IS BOUND.
    lo_mod->process( ).
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  lo_mod->f4_variant( ).

AT SELECTION-SCREEN.
  lo_mod->variant_exist( ).
