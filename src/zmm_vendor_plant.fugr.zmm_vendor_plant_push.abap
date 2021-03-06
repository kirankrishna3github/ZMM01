FUNCTION ZMM_VENDOR_PLANT_PUSH.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IM_DYNP_DATA) TYPE  CI_EKKODB
*"----------------------------------------------------------------------


CI_EKKODB = im_dynp_data.

* Get Vendor Address Details from ztable ZMM_VENDOR_TAG
if lt_mepo_topline-lifnr is not initial.
    select * from zmm_vendor_tag
    into table lt_vendor
    where lifnr = lt_mepo_topline-lifnr
    and  werks = CI_EKKODB-zzvendorplant.
    read table lt_vendor into wa_vend index 1.
endif.

if wa_vend is not initial.
    select single bezei from t005u into (lv_region)
    where land1 = wa_vend-country
    and bland = wa_vend-region.
endif.


ENDFUNCTION.
