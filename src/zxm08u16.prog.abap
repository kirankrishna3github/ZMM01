*&---------------------------------------------------------------------*
*&  Include           ZXM08U16
*&---------------------------------------------------------------------*
*& Friday, March 09, 2018 15:38:14
*& 6010859 : SaurabhK
*& IRDK931353
*&---------------------------------------------------------------------*
*& 1. Makes HSN/SAC code mandatory in MIRO
*& 2. For Service MIRO and Sub-Contracting MIRO SAC code maintained in zgst_vsac is the hsn/sac code  " IRDK931465
*& 3. For other/material MIRO control code maintained in PO is the hsn/sac code
*& 3.1 In case of other material PO's some times there are service lines like CHA or Freight, for such lines the user usually...
*& ... changes the vendor in details tab of miro and enters the sac code of this new vendor in place of hsn code for the line in miro that...
*& ... has condn type 'ZCH3' or 'FRC1' etc. To allow this we have added a separate section under other po's for such cases.
*& 4. This validation works in combination with the enhancement ZMM_HSNCODE_MIRO(LMRMDF0H)
*& 5. ZMM_HSNCODE_MIRO: Auto-sets the correct values for hsn sac code as above when the item is loaded in MIRO
*& and this validation serves the purpose of double checking during saving just in case user changes the hsn sac code
*& to an incorrect/blank value.
*&---------------------------------------------------------------------*

break 6010859.
BREAK 10106.
data: e_wdrseg like line of e_tdrseg.
data: it_poview type table of wb2_v_ekko_ekpo2,
      wa_poview like line of it_poview,
      msg       type string.

data: begin of sac,
        sac type zgst_vsac-hsn_sac,
      end of sac,
      sac_tab like standard table of sac.

check sy-tcode eq 'MIRO'
or sy-tcode eq '/WITS/IVWB2' or sy-tcode eq '/WITS/IVWB3'."<-- these transactions related to vendor payment module exalca - mail frm khushabu on 16.12.2019

check e_trbkpv-budat gt '20170630'.
check e_tdrseg[] is not initial.
select *
  from wb2_v_ekko_ekpo2
  into table it_poview
  for all entries in e_tdrseg[]
  where ebeln_i = e_tdrseg-ebeln
  and   ebelp_i = e_tdrseg-ebelp.

clear e_wdrseg.
loop at e_tdrseg into e_wdrseg where ebeln is not initial and wrbtr > 0  " IRDK931542 - check only lines where amount > 0,
                               and   selkz eq abap_true.  " only selected lines " since only those lines will be posted in doc
  clear wa_poview.
  read table it_poview into wa_poview with key ebeln_i = e_wdrseg-ebeln ebelp_i = e_wdrseg-ebelp.
  if sy-subrc = 0.
    if wa_poview-pstyp_i eq '9' " => This is a service PO, txz01 is description of service
      or wa_poview-pstyp_i = '3'. " Sub-contracting item. " => This is a material PO, txz01 is description of material
      refresh sac_tab.
      select hsn_sac from zgst_vsac into table sac_tab
        where lifnr = wa_poview-lifnr
        and   regio = ( select regio from lfa1 where lifnr = wa_poview-lifnr ).
      if sy-subrc <> 0.
        clear msg.
        concatenate e_wdrseg-ebeln e_wdrseg-ebelp ': SAC Code not mantained for vendor' wa_poview-lifnr into msg separated by space.
        set cursor field 'DRSEG-HSN_SAC' line sy-stepl.
        message msg type 'E'.
      else.
        clear sac.
        read table sac_tab into sac with key sac = e_wdrseg-hsn_sac.
        if sy-subrc <> 0.
          set cursor field 'DRSEG-HSN_SAC' line sy-stepl.
          message |{ e_wdrseg-ebeln } { e_wdrseg-ebelp } : Please select appropriate SAC code using ZFI079| type 'E'.
        endif.
      endif.
    else.  " other material po's
      " IRDK931526
      if e_wdrseg-kschl eq 'ZCH3' or e_wdrseg-kschl eq 'FRC1'
         or e_wdrseg-kschl eq 'FRB1' or e_wdrseg-kschl eq 'ZTC3'. " Added as per mail from Mr. Rakesh Patel on Fri 6/7/2018 5:45 PM
                                                                  " Sub: HSN code as per Bill and PO Line item different; IRDK932773
        refresh sac_tab.
        select hsn_sac from zgst_vsac into table sac_tab
        where lifnr = e_trbkpv-lifnr
        and   regio = ( select regio from lfa1 where lifnr = e_trbkpv-lifnr ).
        if sy-subrc <> 0.
          clear msg.
          concatenate e_wdrseg-ebeln e_wdrseg-ebelp ': SAC Code not mantained for vendor' e_trbkpv-lifnr into msg separated by space.
          set cursor field 'DRSEG-HSN_SAC' line sy-stepl.
          message msg type 'E'.
        else.
          clear sac.
          read table sac_tab into sac with key sac = e_wdrseg-hsn_sac.
          if sy-subrc <> 0.
            set cursor field 'DRSEG-HSN_SAC' line sy-stepl.
            message |{ e_wdrseg-ebeln } { e_wdrseg-ebelp } : Please select appropriate SAC code using ZFI079| type 'E'.
          endif.
        endif.
        " ---end IRDK931526
      else.
        if e_wdrseg-hsn_sac <> wa_poview-j_1bnbm_i. " IRDK931534
          clear msg.
          concatenate e_wdrseg-ebeln e_wdrseg-ebelp ': Please input correct HSN code' wa_poview-j_1bnbm_i into msg separated by space.
          set cursor field 'DRSEG-HSN_SAC' line sy-stepl.
          message msg type 'E'.
        endif.
      endif.

    endif.
  endif.
  clear: e_wdrseg, wa_poview.
endloop.

loop at e_tdrseg into e_wdrseg where ebeln is not initial and hsn_sac is initial and wrbtr > 0  " IRDK931542 - check only lines where amount > 0,
                               and   selkz eq abap_true.  " only selected lines " since only those lines will be posted in doc.
  clear msg.
  concatenate e_wdrseg-ebeln e_wdrseg-ebelp ': HSN/SAC Code is mandatory' into msg separated by space.
  set cursor field 'DRSEG-HSN_SAC' line sy-stepl.
  message msg type 'E'.
  clear e_wdrseg.
endloop.
