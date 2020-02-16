class ZCL_IM_MM_ME_REQ_POST_MAIL definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ME_REQ_POSTED .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_MM_ME_REQ_POST_MAIL IMPLEMENTATION.


  method if_ex_me_req_posted~posted.


***************************************************************************************************************************************************
*****************************************  Mail trigger after final posting of relesing strategy for PR   *****************************************
*************************************** Changes Made by siddharth P., Adroit Infotech, Pune on 28.05.2018 *****************************************
***************************************************************************************************************************************************

    types : begin of ls_eban,
              banfn type banfn,
              bnfpo type bnfpo,
              bsart type bsart,
              frgkz type frgkz,
              frgrl type frgrl,
              frgzu type frgzu,
              banpr type banpr,
              werks type werks_d,
              ekgrp type ekgrp,
              ernam type ernam,
            end of ls_eban,

            begin of ls_params,
              progname type z6mma_params-progname,
              param1   type z6mma_params-param1,
              param2   type z6mma_params-param2,
              param3   type z6mma_params-param3,
              param4   type z6mma_params-param4,
              paramval type z6mma_params-paramval,
            end of ls_params,

            begin of ls_item,
              bnfpo type bnfpo,
              matnr type matnr,
              werks type werks_d,
            end of ls_item.

    data : it_eban    type table of ueban,
           wa_eban    like line of im_eban,

           lt_eban_dc type table of ueban,    " desending
           lt_eban_ac type table of ueban,    " ascending
           lw_eban    like line of im_eban,

           lt_item    type table of ls_item,
           lw_item    type ls_item,

           lt_param   type table of ls_params,
           lw_param   type ls_params.

    data: lt_mailsubject    type sodocchgi1,

          lt_mailrecipients type standard table of somlrec90,

          wa_mailrecipients like line of lt_mailrecipients,

          lt_mailtxt        type standard table of soli,

          wa_mailtxt        like line of lt_mailtxt.

    data : lv_email type so_recname,
           lv_pgtxt type char20.

    break: abap01, 10106, 6010859.

    if sy-tcode = 'ME55' or sy-tcode = 'ME54N'.

***    read table im_eban into wa_eban index 1.
      it_eban[] = im_eban[].
      lt_eban_ac[] = im_eban[].
      lt_eban_dc[] = im_eban[].

      sort lt_eban_ac by banfn bnfpo.
      sort lt_eban_dc by banfn bnfpo descending.
      sort it_eban by banfn bnfpo.

      delete adjacent duplicates from lt_eban_ac comparing banfn.
      delete adjacent duplicates from lt_eban_dc comparing banfn.


      loop at it_eban into wa_eban where banpr = '05'.  " Fully/final released only: IRDK932707.

        read table lt_eban_dc into lw_eban with key banfn = wa_eban-banfn bnfpo = wa_eban-bnfpo.
        if sy-subrc = 0.


***          lw_item-matnr = wa_eban-matnr.      " material
***          lw_item-bnfpo = wa_eban-bnfpo.      " line item
***          lw_item-werks = wa_eban-werks.      " plant
***
***          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
***          EXPORTING
***            input         = lw_item-matnr
***          IMPORTING
***            OUTPUT        = lw_item-matnr
***            .
***
***          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
***          EXPORTING
***            input         = lw_item-bnfpo
***          IMPORTING
***            OUTPUT        = lw_item-bnfpo
***            .
***
***          append lw_item to lt_item.
***
***          clear lw_item.

***        select banfn bnfpo bsart frgkz frgrl frgzu banpr werks ekgrp ernam
***          from eban
***          into table lt_eban
***          where banfn = wa_eban-banfn
***            and banpr <> '05'.
***            and ekgrp = '202'.

***        IF sy-subrc = 0 and lt_eban is initial.

***********          "Subject.

          lt_mailsubject-obj_name = 'PR Release'.

          lt_mailsubject-obj_langu = sy-langu.

          concatenate  'PR' wa_eban-banfn 'Released'

          into  lt_mailsubject-obj_descr

          separated by space.

************          " Mail content

*{   REPLACE        SBXK900029                                        1
*\          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
*--------------------------------------------------------------------*
*---<< S/4HANA >>---*
*--------------------------------------------------------------------*
* Changed On - Wednesday, October 11 2018
* Changed By - 6010859 - SaurabhK
* Purpose    - Simplification list - 2215424 - matnr field ext
* Solution   - pseudo comment since usage is ok
* TR         - SBXK900029
*--------------------------------------------------------------------*
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'  "#EC CI_FLDEXT_OK[2215424]
*}   REPLACE
        exporting
              input  = wa_eban-matnr
            importing
              output = wa_eban-matnr.


          wa_mailtxt = 'Hello Sir,'.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          select single eknam
            from t024
            into lv_pgtxt
            where ekgrp = wa_eban-ekgrp.

          concatenate 'Purchase Requisition No.' wa_eban-banfn 'in Plant' wa_eban-werks
                      'for Purchasing Group' wa_eban-ekgrp '(' lv_pgtxt ') has been released.'
          into wa_mailtxt separated by space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

***          LOOP AT lt_item into lw_item.
***
***            concatenate lw_item-bnfpo ' ---- ' lw_item-matnr ' ---- ' lw_item-werks into wa_mailtxt separated by space.
***
***            append wa_mailtxt to lt_mailtxt.
***            clear : wa_mailtxt, lw_item.
***          ENDLOOP.
***          append wa_mailtxt to lt_mailtxt.
***          clear wa_mailtxt.

          wa_mailtxt = space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = '(Note: This is auto-genrated mail. Please do not reply on this mail)'.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = space.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = 'Thanks & Regards,'.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = 'Indofil Industries Ltd.,'.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = 'Kalpataru Square 4th floor, Andheri-East,'.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.

          wa_mailtxt = 'Mumbai-400059'.
          append wa_mailtxt to lt_mailtxt.
          clear wa_mailtxt.
***********          " recipient

          " send mail to creator of PR: IRDK932707
          clear wa_mailrecipients.

          select single usrid_long
            from pa0105
            into lv_email
            where pernr = wa_eban-ernam
            and usrty = '0010' .

          " IHDK903261 - Wednesday, September 25, 2019 09:32:42
          if sy-subrc <> 0. " pick email address from SU01 if not found in HR master
                            " this happened for certain users Eg. 23609 which have a SAP user ID
                            " but corresponding employee record is absent in HR master
            data: lv_username type bapibname-bapibname.
            data: lt_addsmtp  type standard table of bapiadsmtp.
            data: lt_return   type standard table of bapiret2.

            clear: lv_username.
            refresh: lt_addsmtp, lt_return.

            lv_username = wa_eban-ernam.
            call function 'BAPI_USER_GET_DETAIL'
              exporting
                username             = lv_username
              tables
                return               = lt_return
                addsmtp              = lt_addsmtp.

            try.
                lv_email = lt_addsmtp[ std_no = abap_true home_flag = abap_true ]-e_mail.
              catch cx_sy_itab_line_not_found.
            endtry.
          endif.
          " End IHDK903261

          if lv_email is not initial.
            wa_mailrecipients-rec_type  = 'U'.
            wa_mailrecipients-receiver = lv_email.

            append wa_mailrecipients to  lt_mailrecipients .
          endif.
          " End - IRDK932707

          clear: wa_mailrecipients .                         " CC to all previous approver
          clear: lv_email.

          select progname
                 param1
                 param2
                 param3
                 param4
                 paramval
            from z6mma_params
            into table lt_param
            where progname = 'MM_PR_RELEASE'
              and param1 = wa_eban-ekgrp.

          if lt_param is initial.

            message 'No purchasing group found to send mail' type 'I'.

          elseif lt_param is not initial.

            loop at lt_param into lw_param.

              call function 'CONVERSION_EXIT_ALPHA_INPUT'
                exporting
                  input  = lw_param-param2
                importing
                  output = lw_param-param2.

              select single usrid_long
                from pa0105
                into lv_email
                where pernr = lw_param-param2
                  and usrty = '0010' .

              if sy-subrc <> 0. " IHDK903263

                clear: lv_username.
                refresh: lt_addsmtp, lt_return.

                lv_username = wa_eban-ernam.
                call function 'BAPI_USER_GET_DETAIL'
                  exporting
                    username             = lv_username
                  tables
                    return               = lt_return
                    addsmtp              = lt_addsmtp.

                try.
                    lv_email = lt_addsmtp[ std_no = abap_true home_flag = abap_true ]-e_mail.
                  catch cx_sy_itab_line_not_found.
                endtry.
              endif.  " End IHDK903263

              if lv_email is not initial.
                wa_mailrecipients-rec_type  = 'U'.
                wa_mailrecipients-copy  = 'X'.
                wa_mailrecipients-receiver = lv_email.
                append wa_mailrecipients to  lt_mailrecipients .
              endif.

              clear : wa_mailrecipients, lv_email, lw_param.
            endloop.

***          wa_mailrecipients-rec_type  = 'U'.
***
***          wa_mailrecipients-copy  = 'X'.
***
***          wa_mailrecipients-receiver = 'siddharth.p@adroitinfotech.com'.
***
***          APPEND wa_mailrecipients TO  lt_mailrecipients .

            call function 'SO_NEW_DOCUMENT_SEND_API1'
              exporting
                document_data              = lt_mailsubject
                commit_work                = 'X'
              tables
                object_content             = lt_mailtxt
                receivers                  = lt_mailrecipients
              exceptions
                too_many_receivers         = 1
                document_not_sent          = 2
                document_type_not_exist    = 3
                operation_no_authorization = 4
                parameter_error            = 5
                x_error                    = 6
                enqueue_error              = 7
                others                     = 8.
            if sy-subrc <> 0.
              message 'Incorrect parameter while sending mail' type 'I'.
            endif.
***        ENDIF.        " check enteris from eban table

            clear : lt_item.

***          else.
***
***            lw_item-matnr = wa_eban-matnr.      " material
***            lw_item-bnfpo = wa_eban-bnfpo.      " line item
***            lw_item-werks = wa_eban-werks.      " plant
***
***            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
***              EXPORTING
***                input         = lw_item-matnr
***             IMPORTING
***               OUTPUT        = lw_item-matnr
***                      .
***
***            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
***            EXPORTING
***              input         = lw_item-bnfpo
***            IMPORTING
***              OUTPUT        = lw_item-bnfpo
***              .
***
***            append lw_item to lt_item.
***
***            clear lw_item.

          endif.  " z6mmv_params table value check

        endif.    " check last line item of PR


        clear : wa_eban, lw_eban, wa_mailrecipients, wa_mailtxt, lt_mailsubject, lt_mailtxt, lt_mailrecipients, lw_item.
      endloop.      " main it_eban table loop


    endif.      " t-code check

***********************************************   End of Modification  ****************************************************************************
***************************************************************************************************************************************************
  endmethod.
ENDCLASS.
