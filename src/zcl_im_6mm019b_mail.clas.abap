class ZCL_IM_6MM019B_MAIL definition
  public
  final
  create public .

*"* public components of class ZCL_IM_6MM019B_MAIL
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_MB_DOCUMENT_BADI .
protected section.
*"* protected components of class ZCL_IM_6MM019B_MAIL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_6MM019B_MAIL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_6MM019B_MAIL IMPLEMENTATION.


method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_BEFORE_UPDATE.
  break ibmabap01.
endmethod.


method IF_EX_MB_DOCUMENT_BADI~MB_DOCUMENT_UPDATE.

  break ibmabap01.
endmethod.
ENDCLASS.
