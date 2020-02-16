*&---------------------------------------------------------------------*
*& Report  Z6MM010C_SERVICE_MASTER_UPLD
*&
*&--------------------------------------------------------------------*&
*&  Program Name       : Z6MM010C_SERVICE_MASTER_UPLD                 *&
*&  Module             : MM                                           *&
*&  Package            : ZMM01                                        *&
*&  Developer          : Naveen Gureja (IBM)                          *&
*&  Transport Req      :                                              *&
*&  Program Type       : 1 - Executable                               *&
*&--------------------------------------------------------------------*&
*&  Object             : This Program will do the Service Master      *&
*&                     : Upload using BDC                             *&
*&                                                                    *&
*&--------------------------------------------------------------------*&
report Z6MM012C_SERVICE_MASTER_UPL.

*----------------------------------------------------------------------*
* OBJECT DESCRIPTION: Service Master
* OBJECT TYPE       : BDC - Upload       FUNC. CONSULTANT  : Girish
*          DEVELOPER: Ramakrishna
*      CREATION DATE: 30.06.2010
*        DEV REQUEST: IRDK900235
*             TCODE : ZMM012
*----------------------------------------------------------------------*
* REVISION HISTORY-----------------------------------------------------*
*
*        REVISION NO:   R***
*          DEVELOPER:                        DATE:   DD.MM.YYYY
*        DESCRIPTION:
*----------------------------------------------------------------------*

*&&   Top Include for Data Declaration ...
INCLUDE z6mm012c_service_master_top.

*&&   Selection Screen of the Program
INCLUDE z6mm012c_service_master_sel.

*&&   Subroutine Include of the Program
INCLUDE z6mm012c_service_master_sub.

*&&   Initialization Event is called
INITIALIZATION.
  CLEAR: tdata, tdata[].
  CLEAR: bdcdata, bdcdata[].
  CLEAR: p_fname.

*&&   Search help for File
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FNAME'
    IMPORTING
      file_name     = p_fname.

*&&   at selection screen event is called
AT SELECTION-SCREEN.

*&&   Start of selection
START-OF-SELECTION.

  PERFORM   get_data_from_file
                   TABLES  tdata
                   USING   p_fname.

  IF tdata[] IS INITIAL.
    MESSAGE 'No Data Found' TYPE 'S'.
    EXIT.
  ENDIF.

  PERFORM  upload_data
                   TABLES   tdata
                            bdcdata.

*&&   End of Selection Event is called
END-OF-SELECTION.
