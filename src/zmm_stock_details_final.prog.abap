*&---------------------------------------------------------------------*
*&  Include           ZMM_STOCK_DETAILS_FINAL
*&---------------------------------------------------------------------*
***         TAB_BATCH_STOCK TYPE TABLE OF ZATR_STRUC_BATCH_STOCK WITH HEADER LINE.

DATA : BEGIN OF TAB_BATCH_STOCK OCCURS 0,
        werks LIKE zatr_struc_batch_stock-werks,
        name1 LIKE zatr_struc_batch_stock-name1,
        matnr LIKE zatr_struc_batch_stock-matnr,
        maktx LIKE zatr_struc_batch_stock-maktx,
        charg LIKE zatr_struc_batch_stock-charg,
        clabs LIKE zatr_struc_batch_stock-clabs,
        meins LIKE zatr_struc_batch_stock-meins,
        vfdat LIKE zatr_struc_batch_stock-vfdat,
        laeda LIKE zatr_struc_batch_stock-laeda,
        hsdat LIKE zatr_struc_batch_stock-hsdat,
        wgbez LIKE zatr_struc_batch_stock-wgbez,
        ewbez LIKE zatr_struc_batch_stock-ewbez,
        CITYC TYPE T005H-CITYC," Region
        BEZEI TYPE T005H-BEZEI," region description
       END OF TAB_BATCH_STOCK,
       struc_BATCH_STOCK LIKE LINE OF TAB_BATCH_STOCK.
