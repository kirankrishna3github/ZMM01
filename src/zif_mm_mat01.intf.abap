interface zif_mm_mat01
  public .

  types: excelcell type c length 50.
  types: begin of ty_fg_create_file,
           matnr type excelcell,  " Headdata/Mara
           maktx type excelcell,  " Makt/MaterialDescription
           mtart type excelcell,
           steuc type excelcell,
           werks type excelcell,  " Marc/Plantdata
           vkorg type excelcell,  " Mvke/Salesdata
           vtweg type excelcell,
           spart type excelcell,
           meins type excelcell,  " Mara/Clientdata
           matkl type excelcell,
           brgew type excelcell,  " Marm/UnitsOfMeasure + alt_uom(meins)
           ntgew type excelcell,
           gewei type excelcell,
           ktgrm type excelcell,
           prodh type excelcell,
           mvgr1 type excelcell,
           mvgr2 type excelcell,
           mvgr3 type excelcell,
           mvgr4 type excelcell,
           mvgr5 type excelcell,
           aumng type excelcell,
           kondm type excelcell,
           sktof type excelcell,
           versg type excelcell,
           bstme type excelcell,
           vabme type excelcell,
           ekwsl type excelcell,
           mhdrz type excelcell,
           mhdhb type excelcell,
           prfrq type excelcell,
           herkr type excelcell,
           ekgrp type excelcell,
           kordb type excelcell,
           dismm type excelcell,
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           ausss type excelcell,
           beskz type excelcell,
           sobsl type excelcell,
           lgpro type excelcell,
           lgfsb type excelcell,
           plifz type excelcell,
           webaz type excelcell,
           fhori type excelcell,
           rwpro type excelcell,
           strgr type excelcell,
           mtvfp type excelcell,
           sbdkz type excelcell,
           uneto type excelcell,
           ueeto type excelcell,
           lgort type excelcell,  " Mard/Storagelocationdata + plant(werks)
           ncost type excelcell,
           awsls type excelcell,
           sobsk type excelcell,
           bklas type excelcell,  " Mbew/Valuationdata + val_area(bwkey) = plant(werks)
*            vprsv type excelcell,  " default value 'S',  IHDK900305
*            peinh type excelcell,  " default value '1', IHDK900305
*            verpr type excelcell,  " not to be maintained, IHDK900305
*            stprs type excelcell,  " default value '1', IHDK900305
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
           hrkft type excelcell,
*            kosgr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
           vrkme type excelcell,
           dwerk type excelcell,
           bonus type excelcell,
           bismt type excelcell,

           " Removed from clientdata
*            mbrsh      TYPE excelcell,  " Default Value
*            mtpos_mara TYPE excelcell,  " Default Value
*            tragr      TYPE excelcell,  " Default Value
*            xchpf      TYPE excelcell,  " Default Value

           " Removed from plantdata
*            ladgr      TYPE excelcell,  " Default Value
*            herkl      TYPE excelcell,  " Default Value
*            kzech      TYPE excelcell,  " Default Value
*            rgekz      TYPE excelcell,  " Default Value
*            dzeit      TYPE excelcell,  " Default Value
*            fevor      TYPE excelcell,  " Default Value
*            sfcpf      TYPE excelcell,  " Default Value
*            losgr      TYPE excelcell,  " Default Value

           " Removed from valuationdata
*            ekalr      TYPE excelcell,  " Default Value
*            hkmat      TYPE excelcell,  " Default Value
*            zplp1      TYPE excelcell,  " Marked for Removal
*            zpld1      TYPE excelcell,  " Marked for Removal
*            zplp2      TYPE excelcell,  " Marked for Removal
*            zpld2      TYPE excelcell,  " Marked for Removal
*            zplp3      TYPE excelcell,  " Marked for Removal
*            zpld3      TYPE excelcell,  " Marked for Removal

           " Removed from salesdata
*            mtpos      TYPE excelcell,  " Default Value
           " Tax Classification
         end of ty_fg_create_file,

         begin of ty_fg_create_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr,
           maktx type makt-maktx,
           mtart type mara-mtart,
           steuc type marc-steuc,
           werks type marc-werks,
           vkorg type mvke-vkorg,
           vtweg type mvke-vtweg,
           spart type mara-spart,
           meins type mara-meins,
           matkl type mara-matkl,
           brgew type marm-brgew,
           ntgew type mara-ntgew,
           gewei type mara-gewei,
           ktgrm type mvke-ktgrm,
           prodh type mara-prdha,
           mvgr1 type mvke-mvgr1,
           mvgr2 type mvke-mvgr2,
           mvgr3 type mvke-mvgr3,
           mvgr4 type mvke-mvgr4,
           mvgr5 type mvke-mvgr5,
           aumng type mvke-aumng,
           kondm type mvke-kondm,
           sktof type mvke-sktof,
           versg type mvke-versg,
           bstme type mara-bstme,
           vabme type mara-vabme,
           ekwsl type mara-ekwsl,
           mhdrz type mara-mhdrz,
           mhdhb type mara-mhdhb,
           prfrq type marc-prfrq,
           herkr type marc-herkr,
           ekgrp type marc-ekgrp,
           kordb type marc-kordb,
           dismm type marc-dismm,
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           ausss type marc-ausss,
           beskz type marc-beskz,
           sobsl type marc-sobsl,
           lgpro type marc-lgpro,
           lgfsb type marc-lgfsb,
           plifz type marc-plifz,
           webaz type marc-webaz,
           fhori type marc-fhori,
           rwpro type marc-rwpro,
           strgr type marc-strgr,
           mtvfp type marc-mtvfp,
           sbdkz type marc-sbdkz,
           uneto type marc-uneto,
           ueeto type marc-ueeto,
           lgort type mard-lgort,
           ncost type marc-ncost,
           awsls type marc-awsls,
           sobsk type marc-sobsk,
           bklas type mbew-bklas,
           hrkft type mbew-hrkft,
           vrkme type mvke-vrkme,
           dwerk type mvke-dwerk,
           bonus type mvke-bonus,
           bismt type mara-bismt,
         end of ty_fg_create_data,

         begin of ty_fg_plt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           werks type excelcell,
           bstmi type excelcell,
           beskz type excelcell,
           sobsl type excelcell,
           lgfsb type excelcell,
           mtvfp type excelcell,
           lgort type excelcell,
*            vprsv type excelcell,  " default value 'S', IHDK900305
*            verpr type excelcell,  " not maintained anymore, IHDK900305
*            stprs type excelcell,  " default value '1', IHDK900305
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
*            kosgr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_fg_plt_ext_file,

         begin of ty_fg_plt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           werks type marc-werks, " Plantdata
           bstmi type marc-bstmi,
           beskz type marc-beskz,
           sobsl type marc-sobsl,
           lgfsb type marc-lgfsb,
           mtvfp type marc-mtvfp,
*            prctr type marc-prctr, " pick from table zmat01_pctr_mast, , IHDK900305
           lgort type mard-lgort, " storagelocationdata
           " Valuationdata
*            vprsv type mbew-vprsv, " default value 'S', IHDK900305
*            verpr type mbew-verpr, " not maintained anymore, IHDK900305
*            stprs type mbew-stprs, " default value '1', IHDK900305
*            kosgr type mbew-kosgr, " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_fg_plt_ext_data,

         begin of ty_fg_dpt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           werks type excelcell,
           ekgrp type excelcell,
           bstmi type excelcell,
           beskz type excelcell,
           sobsl type excelcell,
           plifz type excelcell,
           lgfsb type excelcell,
           rwpro type excelcell,
           mtvfp type excelcell,
           lgort type excelcell,
*            vprsv type excelcell,  " default value 'S', IHDK900305
*            verpr type excelcell,  " not maintained anymore, IHDK900305
*            stprs type excelcell,  " default value '1', IHDK900305
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
*            kosgr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_fg_dpt_ext_file,

         begin of ty_fg_dpt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           werks type marc-werks, " Plantdata
           ekgrp type marc-ekgrp,
           bstmi type marc-bstmi,
           beskz type marc-beskz,
           sobsl type marc-sobsl,
           plifz type marc-plifz,
           lgfsb type marc-lgfsb,
           rwpro type marc-rwpro,
           mtvfp type marc-mtvfp,
*            prctr type marc-prctr, " pick from table zmat01_pctr_mast, IHDK900305
           lgort type mard-lgort, " Storagelocationdata
           " Valuationdata
*            vprsv type mbew-vprsv, " default value 'S', IHDK900305
*            verpr type mbew-verpr, " not maintained anymore, IHDK900305
*            stprs type mbew-stprs, " default value '1', IHDK900305
*            kosgr type mbew-kosgr, " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_fg_dpt_ext_data,

         begin of ty_trd_create_file,
           matnr type excelcell,  " Headdata/Mara
           maktx type excelcell,  " Makt/MaterialDescription
           mtart type excelcell,
           steuc type excelcell,
           werks type excelcell,  " Marc/Plantdata
           vkorg type excelcell,  " Mvke/Salesdata
           vtweg type excelcell,
           spart type excelcell,
           meins type excelcell,  " Mara/Clientdata
           matkl type excelcell,
           brgew type excelcell,  " Marm/UnitsOfMeasure + alt_uom(meins)
           ntgew type excelcell,
           gewei type excelcell,
           ktgrm type excelcell,
           prodh type excelcell,
           mvgr1 type excelcell,
           mvgr2 type excelcell,
           mvgr3 type excelcell,
           mvgr4 type excelcell,
           mvgr5 type excelcell,
           aumng type excelcell,
           kondm type excelcell,
           sktof type excelcell,
           versg type excelcell,
           bstme type excelcell,
           vabme type excelcell,
           ekwsl type excelcell,
           mhdrz type excelcell,
           mhdhb type excelcell,
*            qmpur TYPE excelcell,  " Default 'X'
           ssqss type excelcell,  " data element qsspur
           qzgtp type excelcell,  " data element qzgtyp
           herkr type excelcell,
           ekgrp type excelcell,
           kordb type excelcell,
           dismm type excelcell,
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           beskz type excelcell,
*            lgfsb TYPE excelcell,  " Default value = lgort
           plifz type excelcell,
           webaz type excelcell,
           fhori type excelcell,
           strgr type excelcell,
           mtvfp type excelcell,
           sbdkz type excelcell,
           lgort type excelcell,  " Mard/Storagelocationdata + plant(werks)
           awsls type excelcell,
           sobsk type excelcell,
           bklas type excelcell,  " Mbew/Valuationdata + val_area(bwkey) = plant(werks)
           vprsv type excelcell,
           peinh type excelcell,
           verpr type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
           hrkft type excelcell,
           vrkme type excelcell,
           dwerk type excelcell,
           bonus type excelcell,
           bismt type excelcell,

           " Removed from clientdata
*            mbrsh      TYPE excelcell,  " Default Value
*            mtpos_mara TYPE excelcell,  " Default Value
*            tragr      TYPE excelcell,  " Default Value
*            xchpf      TYPE excelcell,  " Default Value

           " Removed from plantdata
*            ladgr      TYPE excelcell,  " Default Value
*            herkl      TYPE excelcell,  " Default Value
*            kzech      TYPE excelcell,  " Default Value
*            rgekz      TYPE excelcell,  " Default Value
*            dzeit      TYPE excelcell,  " Default Value
*            fevor      TYPE excelcell,  " Default Value
*            sfcpf      TYPE excelcell,  " Default Value
*            losgr      TYPE excelcell,  " Default Value

           " Removed from valuationdata
*            ekalr      TYPE excelcell,  " Default Value
*            hkmat      TYPE excelcell,  " Default Value
*            zplp1      TYPE excelcell,  " Marked for Removal
*            zpld1      TYPE excelcell,  " Marked for Removal
*            zplp2      TYPE excelcell,  " Marked for Removal
*            zpld2      TYPE excelcell,  " Marked for Removal
*            zplp3      TYPE excelcell,  " Marked for Removal
*            zpld3      TYPE excelcell,  " Marked for Removal

           " Removed from salesdata
*            mtpos      TYPE excelcell,  " Default Value
           " Tax Classification
         end of ty_trd_create_file,

         begin of ty_trd_create_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr,
           maktx type makt-maktx,
           mtart type mara-mtart,
           steuc type marc-steuc,
           werks type marc-werks,
           vkorg type mvke-vkorg,
           vtweg type mvke-vtweg,
           spart type mara-spart,
           meins type mara-meins,
           matkl type mara-matkl,
           brgew type marm-brgew,
           ntgew type mara-ntgew,
           gewei type mara-gewei,
           ktgrm type mvke-ktgrm,
           prodh type mara-prdha,
           mvgr1 type mvke-mvgr1,
           mvgr2 type mvke-mvgr2,
           mvgr3 type mvke-mvgr3,
           mvgr4 type mvke-mvgr4,
           mvgr5 type mvke-mvgr5,
           aumng type mvke-aumng,
           kondm type mvke-kondm,
           sktof type mvke-sktof,
           versg type mvke-versg,
           bstme type mara-bstme,
           vabme type mara-vabme,
           ekwsl type mara-ekwsl,
           mhdrz type mara-mhdrz,
           mhdhb type mara-mhdhb,
           ssqss type marc-ssqss,
           qzgtp type marc-qzgtp,
           herkr type marc-herkr,
           ekgrp type marc-ekgrp,
           kordb type marc-kordb,
           dismm type marc-dismm,
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           beskz type marc-beskz,
           plifz type marc-plifz,
           webaz type marc-webaz,
           fhori type marc-fhori,
           strgr type marc-strgr,
           mtvfp type marc-mtvfp,
           sbdkz type marc-sbdkz,
           lgort type mard-lgort,
           awsls type marc-awsls,
           sobsk type marc-sobsk,
           bklas type mbew-bklas,
           vprsv type mbew-vprsv,
           peinh type mbew-peinh,
           verpr type mbew-verpr,
           hrkft type mbew-hrkft,
           vrkme type mvke-vrkme,
           dwerk type mvke-dwerk,
           bonus type mvke-bonus,
           bismt type mara-bismt,
         end of ty_trd_create_data,

         begin of ty_trd_plt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           werks type excelcell,
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           plifz type excelcell,
*             lgfsb TYPE excelcell, " Default = lgort
           webaz type excelcell,
           mtvfp type excelcell,
           lgort type excelcell,
           lgpro type excelcell,
           verpr type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_trd_plt_ext_file,

         begin of ty_trd_plt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           werks type marc-werks, " Plantdata
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           plifz type marc-plifz,
*             lgfsb TYPE marc-lgfsb, Default = lgort
           webaz type marc-webaz,
           mtvfp type marc-mtvfp,
*            prctr type marc-prctr, " pick from table zmat01_pctr_mast, IHDK900305
           lgort type mard-lgort, " storagelocationdata
           lgpro type marc-lgpro,
           verpr type mbew-verpr, " Valuationdata
         end of ty_trd_plt_ext_data,

         begin of ty_trd_dpt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           werks type excelcell,
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           plifz type excelcell,
*             lgfsb TYPE excelcell, " Default = lgort
           webaz type excelcell,
           mtvfp type excelcell,
           lgort type excelcell,
           verpr type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_trd_dpt_ext_file,

         begin of ty_trd_dpt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           werks type marc-werks, " Plantdata
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           plifz type marc-plifz,
*             lgfsb TYPE marc-lgfsb, Default = lgort
           webaz type marc-webaz,
           mtvfp type marc-mtvfp,
*            prctr type marc-prctr, " pick from table zmat01_pctr_mast, IHDK900305
           lgort type mard-lgort, " storagelocationdata
           verpr type mbew-verpr, " Valuationdata
         end of ty_trd_dpt_ext_data,

         begin of ty_raw_create_file,
           matnr     type excelcell,  " Headdata/Mara
           maktx     type excelcell,  " Makt/MaterialDescription
           mtart     type excelcell,
           taxim     type excelcell,
           steuc     type excelcell,
           werks     type excelcell,  " Marc/Plantdata
           spart     type excelcell,
           meins     type excelcell,  " Mara/Clientdata
           matkl     type excelcell,
           bstme     type excelcell,
           umrez     type excelcell,
           vabme     type excelcell,
           ekwsl     type excelcell,
           xchpf     type excelcell,
           mhdrz     type excelcell,
           mhdhb     type excelcell,
           prfrq     type excelcell,
           qmpur     type excelcell,
           ssqss     type excelcell,  " data element qsspur
           qzgtp     type excelcell,  " data element qzgtyp
           ekgrp     type excelcell,
           dismm     type excelcell,
           dispo     type excelcell,
           minbe     type excelcell,
           disls     type excelcell,
           bstmi     type excelcell,
           bstma     type excelcell,
           bstfe     type excelcell,
           eisbe     type excelcell,
           beskz     type excelcell,
           lgpro     type excelcell,
*             lgfsb     TYPE excelcell, " Default = lgort
           plifz     type excelcell,
           webaz     type excelcell,
           fhori     type excelcell,
           mtvfp     type excelcell,
           lgort     type excelcell,  " Mard/Storagelocationdata + plant(werks)
           hrkft     type excelcell,
           sobsk     type excelcell,
           losgr     type excelcell,
           vprsv     type excelcell,  " Mbew/Valuationdata + val_area(bwkey) = plant(werks)
           peinh     type excelcell,
           verpr     type excelcell,
*            prctr     type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
           bwtar_dom type excelcell,
           bwtar_imp type excelcell,


           " Removed from clientdata
*            mbrsh      TYPE excelcell,  " Default Value  ======
*            mtpos_mara TYPE excelcell,  " Default Value  ======
*            tragr      TYPE excelcell,  " Default Value  ======

           " Removed from plantdata
*            ladgr      TYPE excelcell,  " Default Value  ======
*            herkl      TYPE excelcell,  " Default Value
*            kzech      TYPE excelcell,  " Default Value
*            rgekz      TYPE excelcell,  " Default Value
*            dzeit      TYPE excelcell,  " Default Value
*            fevor      TYPE excelcell,  " Default Value
*            sfcpf      TYPE excelcell,  " Default Value

           " Removed from valuationdata
*            ekalr      TYPE excelcell,  " Default Value
*            hkmat      TYPE excelcell,  " Default Value
*            zplp1      TYPE excelcell,  " Marked for Removal
*            zpld1      TYPE excelcell,  " Marked for Removal
*            zplp2      TYPE excelcell,  " Marked for Removal
*            zpld2      TYPE excelcell,  " Marked for Removal
*            zplp3      TYPE excelcell,  " Marked for Removal
*            zpld3      TYPE excelcell,  " Marked for Removal

           " Removed from salesdata
*            mtpos      TYPE excelcell,  " Default Value  ======
           " Tax Classification
         end of ty_raw_create_file,

         begin of ty_raw_create_data,
           index     type i,
           sel       type flag,
           matnr     type mara-matnr,
           maktx     type makt-maktx,
           mtart     type mara-mtart,
           taxim     type mlan-taxim,
           steuc     type marc-steuc,
           werks     type marc-werks,
           spart     type mara-spart,
           meins     type mara-meins,
           matkl     type mara-matkl,
           bstme     type mara-bstme,
           umrez     type marm-umrez,
           vabme     type mara-vabme,
           ekwsl     type mara-ekwsl,
           xchpf     type mara-xchpf,
           mhdrz     type mara-mhdrz,
           mhdhb     type mara-mhdhb,
           prfrq     type marc-prfrq,
           qmpur     type mara-qmpur,
           ssqss     type marc-ssqss,
           qzgtp     type marc-qzgtp,
           ekgrp     type marc-ekgrp,
           dismm     type marc-dismm,
           dispo     type marc-dispo,
           minbe     type marc-minbe,
           disls     type marc-disls,
           bstmi     type marc-bstmi,
           bstma     type marc-bstma,
           bstfe     type marc-bstfe,
           eisbe     type marc-eisbe,
           beskz     type marc-beskz,
           lgpro     type marc-lgpro,
           plifz     type marc-plifz,
           webaz     type marc-webaz,
           fhori     type marc-fhori,
           mtvfp     type marc-mtvfp,
           lgort     type mard-lgort,
           hrkft     type mbew-hrkft,
           sobsk     type marc-sobsk,
           losgr     type marc-losgr,
           vprsv     type mbew-vprsv,
           peinh     type mbew-peinh,
           verpr     type mbew-verpr,
           bwtar_dom type mbew-bwtar,
           bwtar_imp type mbew-bwtar,
         end of ty_raw_create_data,

         begin of ty_raw_plt_dpt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           werks type excelcell,
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           plifz type excelcell,
*             lgfsb TYPE excelcell, " Default = lgort
           webaz type excelcell,
           mtvfp type excelcell,
           lgort type excelcell,
           lgpro type excelcell,
           verpr type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_raw_plt_dpt_ext_file,

         begin of ty_raw_plt_dpt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           werks type marc-werks, " Plantdata
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           plifz type marc-plifz,
*             lgfsb TYPE marc-lgfsb, Default = lgort
           webaz type marc-webaz,
           mtvfp type marc-mtvfp,
*            prctr type marc-prctr, " pick from table zmat01_pctr_mast, IHDK900305
           lgort type mard-lgort, " storagelocationdata
           lgpro type marc-lgpro,
           verpr type mbew-verpr, " Valuationdata
         end of ty_raw_plt_dpt_ext_data,

         begin of ty_sfg_create_file,
           matnr type excelcell,  " Headdata/Mara
           maktx type excelcell,  " Makt/MaterialDescription
           mtart type excelcell,
           steuc type excelcell,
           werks type excelcell,  " Marc/Plantdata
           vkorg type excelcell,  " Mvke/Salesdata
           vtweg type excelcell,
           spart type excelcell,
           meins type excelcell,  " Mara/Clientdata
           matkl type excelcell,
           gewei type excelcell,
           ktgrm type excelcell,
*             mvgr1 TYPE excelcell, " Default Value
           mvgr2 type excelcell,
           mvgr3 type excelcell,
           mvgr4 type excelcell,
*             mvgr5 TYPE excelcell, " Default Value
           aumng type excelcell,
           sktof type excelcell,
           versg type excelcell,
           mhdrz type excelcell,
           mhdhb type excelcell,
           prfrq type excelcell,
           dismm type excelcell,
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           ausss type excelcell,
           beskz type excelcell,
           sobsl type excelcell,
           lgpro type excelcell,
           lgfsb type excelcell,
           plifz type excelcell,
           fhori type excelcell,
           strgr type excelcell,
           mtvfp type excelcell,
           sbdkz type excelcell,
           uneto type excelcell,
           ueeto type excelcell,
           lgort type excelcell,  " Mard/Storagelocationdata + plant(werks)
           losgr type excelcell,
           awsls type excelcell,
           bklas type excelcell,  " Mbew/Valuationdata + val_area(bwkey) = plant(werks)
           vprsv type excelcell,
           peinh type excelcell,
           stprs type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
*            kosgr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
           vrkme type excelcell,
           dwerk type excelcell,

           " Removed from clientdata
*            mbrsh      TYPE excelcell,  " Default Value
*            mtpos_mara TYPE excelcell,  " Default Value
*            tragr      TYPE excelcell,  " Default Value
*            xchpf      TYPE excelcell,  " Default Value

           " Removed from plantdata
*            ladgr      TYPE excelcell,  " Default Value
*            herkl      TYPE excelcell,  " Default Value
*            kzech      TYPE excelcell,  " Default Value
*            rgekz      TYPE excelcell,  " Default Value
*            dzeit      TYPE excelcell,  " Default Value
*            fevor      TYPE excelcell,  " Default Value
*            sfcpf      TYPE excelcell,  " Default Value
*            losgr      TYPE excelcell,  " Default Value

           " Removed from valuationdata
*            ekalr      TYPE excelcell,  " Default Value
*            hkmat      TYPE excelcell,  " Default Value
*            zplp1      TYPE excelcell,  " Marked for Removal
*            zpld1      TYPE excelcell,  " Marked for Removal
*            zplp2      TYPE excelcell,  " Marked for Removal
*            zpld2      TYPE excelcell,  " Marked for Removal
*            zplp3      TYPE excelcell,  " Marked for Removal
*            zpld3      TYPE excelcell,  " Marked for Removal

           " Removed from salesdata
*            mtpos      TYPE excelcell,  " Default Value
           " Tax Classification
         end of ty_sfg_create_file,

         begin of ty_sfg_create_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr,
           maktx type makt-maktx,
           mtart type mara-mtart,
           steuc type marc-steuc,
           werks type marc-werks,
           vkorg type mvke-vkorg,
           vtweg type mvke-vtweg,
           spart type mara-spart,
           meins type mara-meins,
           matkl type mara-matkl,
           gewei type mara-gewei,
           ktgrm type mvke-ktgrm,
           mvgr2 type mvke-mvgr2,
           mvgr3 type mvke-mvgr3,
           mvgr4 type mvke-mvgr4,
           aumng type mvke-aumng,
           sktof type mvke-sktof,
           versg type mvke-versg,
           mhdrz type mara-mhdrz,
           mhdhb type mara-mhdhb,
           prfrq type marc-prfrq,
           dismm type marc-dismm,
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           ausss type marc-ausss,
           beskz type marc-beskz,
           sobsl type marc-sobsl,
           lgpro type marc-lgpro,
           lgfsb type marc-lgfsb,
           plifz type marc-plifz,
           fhori type marc-fhori,
           strgr type marc-strgr,
           mtvfp type marc-mtvfp,
           sbdkz type marc-sbdkz,
           uneto type marc-uneto,
           ueeto type marc-ueeto,
           lgort type mard-lgort,
           losgr type marc-losgr,
           awsls type marc-awsls,
           bklas type mbew-bklas,
           vprsv type mbew-vprsv,
           peinh type mbew-peinh,
           stprs type mbew-stprs,
           vrkme type mvke-vrkme,
           dwerk type mvke-dwerk,
         end of ty_sfg_create_data,

         begin of ty_sfg_plt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           werks type excelcell,
           bstmi type excelcell,
           beskz type excelcell,
           sobsl type excelcell,
           lgfsb type excelcell,
           mtvfp type excelcell,
           lgort type excelcell,
           vprsv type excelcell,
           verpr type excelcell,
           stprs type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
*            kosgr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_sfg_plt_ext_file,

         begin of ty_sfg_plt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           werks type marc-werks, " Plantdata
           bstmi type marc-bstmi,
           beskz type marc-beskz,
           sobsl type marc-sobsl,
           lgfsb type marc-lgfsb,
           mtvfp type marc-mtvfp,
*            prctr type marc-prctr, " pick from table zmat01_pctr_mast, IHDK900305
           lgort type mard-lgort, " storagelocationdata
           vprsv type mbew-vprsv, " Valuationdata
           verpr type mbew-verpr,
           stprs type mbew-stprs,
*            kosgr type mbew-kosgr, " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_sfg_plt_ext_data,

         begin of ty_mco_create_file,
           matnr type excelcell,  " Headdata/Mara
           maktx type excelcell,  " Makt/MaterialDescription
           mtart type excelcell,  " Mara/Clientdata
           taxim type excelcell,  " Mlan
           steuc type excelcell,  " Marc/Plantdata
           werks type excelcell,  " Marc/Plantdata
           spart type excelcell,  " Mara/Clientdata
           meins type excelcell,
           matkl type excelcell,
           bstme type excelcell,
           umrez type excelcell,
           vabme type excelcell,
           ekgrp type excelcell,  " Marc/Plantdata
           vprsv type excelcell,  " Mbew/Valuationdata
           peinh type excelcell,  " Mbew/Valuationdata
           verpr type excelcell,  " Mbew/Valuationdata
         end of ty_mco_create_file,

         begin of ty_mco_create_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr,      " Headdata/Mara
           maktx type makt-maktx,      " Makt/MaterialDescription
           mtart type mara-mtart,
*            mbrsh TYPE mara-mbrsh,      " Default Value
           " Tax Classification
           taxim type mlan-taxim,
           steuc type marc-steuc,
           werks type marc-werks,      " Marc/Plantdata
           spart type mara-spart,
           meins type mara-meins,      " Mara/Clientdata
           matkl type mara-matkl,
           bstme type mara-bstme,
           umrez type marm-umrez,
           vabme type mara-vabme,
*            mtpos_mara TYPE mara-mtpos_mara, " Default Value -> NLAG
           ekgrp type marc-ekgrp,
           vprsv type mbew-vprsv,      " Mbew/Valuationdata
           peinh type mbew-peinh,
           verpr type mbew-verpr,
         end of ty_mco_create_data,

         begin of ty_mco_plt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           werks type excelcell,
           ekgrp type excelcell,
         end of ty_mco_plt_ext_file,

         begin of ty_mco_plt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           werks type marc-werks, " Plantdata
           ekgrp type marc-ekgrp,
         end of ty_mco_plt_ext_data,

         begin of ty_spr_create_file,
           matnr type excelcell,  " Headdata/Mara
           maktx type excelcell,  " Makt/MaterialDescription
           mtart type excelcell,
           taxim type excelcell,  " Mlan/Tax classification
           steuc type excelcell,
           werks type excelcell,  " Marc/Plantdata
           spart type excelcell,
           meins type excelcell,  " Mara/Clientdata
           matkl type excelcell,
           bstme type excelcell,
           umrez type excelcell,
           vabme type excelcell,
           ekwsl type excelcell,
           ekgrp type excelcell,
           dismm type excelcell,
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           beskz type excelcell,
*             lgfsb     TYPE excelcell, " Default = 1701
           plifz type excelcell,
           webaz type excelcell,
           mtvfp type excelcell,
           lgort type excelcell,  " Mard/Storagelocationdata + plant(werks)
           vprsv type excelcell,  " Mbew/Valuationdata + val_area(bwkey) = plant(werks)
           peinh type excelcell,
           verpr type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
           bklas type excelcell,
         end of ty_spr_create_file,

         begin of ty_spr_create_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr,
           maktx type makt-maktx,
           mtart type mara-mtart,
           taxim type mlan-taxim,
           steuc type marc-steuc,
           werks type marc-werks,
           spart type mara-spart,
           meins type mara-meins,
           matkl type mara-matkl,
           bstme type mara-bstme,
           umrez type marm-umrez,
           vabme type mara-vabme,
           ekwsl type mara-ekwsl,
           ekgrp type marc-ekgrp,
           dismm type marc-dismm,
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           beskz type marc-beskz,
           plifz type marc-plifz,
           webaz type marc-webaz,
           mtvfp type marc-mtvfp,
           lgort type mard-lgort,
           vprsv type mbew-vprsv,
           peinh type mbew-peinh,
           verpr type mbew-verpr,
           bklas type mbew-bklas,
         end of ty_spr_create_data,

         begin of ty_spr_plt_ext_file,
           matnr type excelcell,
           maktx type excelcell, " Redundant field for display only, material description
           steuc type excelcell, " IHDK900227
           werks type excelcell,
           ekgrp type excelcell,
           dismm type excelcell, " IHDK900227
           dispo type excelcell,
           minbe type excelcell,
           disls type excelcell,
           bstmi type excelcell,
           bstma type excelcell,
           bstfe type excelcell,
           eisbe type excelcell,
           plifz type excelcell,
*             lgfsb TYPE excelcell, " Default = '1701'
           webaz type excelcell,
           mtvfp type excelcell,
*             lgort type excelcell, " Default = '1701'
           verpr type excelcell,
*            prctr type excelcell,  " pick from table zmat01_pctr_mast, IHDK900305
         end of ty_spr_plt_ext_file,

         begin of ty_spr_plt_ext_data,
           index type i,
           sel   type flag,
           matnr type mara-matnr, " Basic, Head
           maktx type makt-maktx, " Redundant field for display only, material description
           steuc type marc-steuc, " IHDK900227
           werks type marc-werks, " Plantdata
           ekgrp type marc-ekgrp,
           dismm type marc-dismm, " IHDK900227
           dispo type marc-dispo,
           minbe type marc-minbe,
           disls type marc-disls,
           bstmi type marc-bstmi,
           bstma type marc-bstma,
           bstfe type marc-bstfe,
           eisbe type marc-eisbe,
           plifz type marc-plifz,
*             lgfsb TYPE marc-lgfsb, Default = lgort
           webaz type marc-webaz,
           mtvfp type marc-mtvfp,
*            prctr type marc-prctr, " pick from table zmat01_pctr_mast, IHDK900305
*             lgort type mard-lgort, " storagelocationdata, Default = '1701'
           verpr type mbew-verpr, " Valuationdata
         end of ty_spr_plt_ext_data.
endinterface.
