CLASS y_payment_adv_job_scheduling DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES if_apj_dt_exec_object.
    INTERFACES if_apj_rt_exec_object.
       INTERFACES if_oo_adt_classrun .

       CLASS-METHODS READ_DATA
        RETURNING VALUE(RESULT12) TYPE STRING
         RAISING cx_static_check.

        CLASS-DATA: lx_bcs_mail TYPE REF TO cx_bcs_mail,
                    SY_DT TYPE SY-DATUM.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Y_PAYMENT_ADV_JOB_SCHEDULING IMPLEMENTATION.


METHOD if_apj_dt_exec_object~get_parameters.


et_parameter_def = VALUE #(
      ( selname = 'S_ID'    kind = if_apj_dt_exec_object=>select_option datatype = 'C' length = 10 param_text = 'My ID'                                      changeable_ind = abap_true )
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length = 80 param_text = 'My Description'   lowercase_ind = abap_true changeable_ind = abap_true )
      ( selname = 'P_COUNT' kind = if_apj_dt_exec_object=>parameter     datatype = 'I' length = 10 param_text = 'My Count'                                   changeable_ind = abap_true )
      ( selname = 'P_SIMUL' kind = if_apj_dt_exec_object=>parameter     datatype = 'C' length =  1 param_text = 'My Simulate Only' checkbox_ind = abap_true  changeable_ind = abap_true )
    ).


 et_parameter_val = VALUE #(
      ( selname = 'S_ID'    kind = if_apj_dt_exec_object=>select_option sign = 'I' option = 'EQ' low = '4711' )
      ( selname = 'P_DESCR' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = 'My Default Description' )
      ( selname = 'P_COUNT' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = '200' )
      ( selname = 'P_SIMUL' kind = if_apj_dt_exec_object=>parameter     sign = 'I' option = 'EQ' low = abap_true )
    ).




ENDMETHOD.


METHOD if_apj_rt_exec_object~execute.


 TYPES ty_id TYPE c LENGTH 10.

    DATA s_id    TYPE RANGE OF ty_id.
    DATA p_descr TYPE c LENGTH 80.
    DATA p_count TYPE i.
    DATA p_simul TYPE abap_boolean.

    LOOP AT it_parameters INTO DATA(ls_parameter).
      CASE ls_parameter-selname.
        WHEN 'S_ID'.
          APPEND VALUE #( sign   = ls_parameter-sign
                          option = ls_parameter-option
                          low    = ls_parameter-low
                          high   = ls_parameter-high ) TO s_id.
        WHEN 'P_DESCR'. p_descr = ls_parameter-low.
        WHEN 'P_COUNT'. p_count = ls_parameter-low.
        WHEN 'P_SIMUL'. p_simul = ls_parameter-low.
      ENDCASE.
    ENDLOOP.

    ME->read_data(  ).

 ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    TRY.
        DATA(return_data) = read_data(    ) .
    ENDTRY.
  ENDMETHOD.


METHOD READ_DATA.

DATA : COMCODE TYPE STRING ,
       FISCALYEAR TYPE STRING,
       fromdate TYPE STRING ,
       TODATE TYPE STRING ,
       VENDOR TYPE STRING ,
       CLEARINGDOCUMENT TYPE STRING ,
       REMARK TYPE STRING ,
       document TYPE STRING ,
       CLR_DOC  TYPE STRING.

        DATA: from_mail(512) TYPE c.
        DATA: cc_mail(512) TYPE c.
        DATA: to_mail(512) TYPE c.
        DATA: bcc_email(512) TYPE c.
*        DATA: bcc_email TYPE  I_AddressEmailAddress_2.
        DATA: to_mail1(512) TYPE c.
        DATA: to_mail2(512) TYPE c.
        DATA: mail(512) TYPE c.
        DATA: companyname(512) TYPE c.
        DATA :  Address TYPE string.
        DATA : Adress1  TYPE STRING .
        DATA  : Adress2 TYPE string.

        TYPES : BEGIN OF it_solisti1 ,
              line TYPE c LENGTH 255,
            END OF it_solisti1.

    DATA : i_objtx  TYPE STANDARD TABLE OF it_solisti1,
           i_objtxt TYPE it_solisti1,
          SUPPLIER_NAME TYPE STRING,
          lo_bcc_recipient TYPE REF TO cl_bcs_mail_message,
                 lv TYPE string,
                lv1 TYPE string,
            mail_id(512) TYPE C,
            indctr  TYPE C.

    DATA : com      TYPE string ,
           date     TYPE datum ,
           date1    TYPE string ,
           Supplier TYPE string ,
           doc1     TYPE string .





date = sy-datum .
date1 = |{ date+0(4) }|.

*SELECT A~CompanyCode, A~Supplier , A~AccountingDocument, A~ClearingJournalEntry , A~FISCALYEAR
*FROM I_OperationalAcctgDocItem AS A
*INNER JOIN I_JournalEntry AS B ON ( B~AccountingDocument = A~AccountingDocument AND B~CompanyCode = A~CompanyCode
*  AND B~FiscalYear = A~FiscalYear )
*LEFT OUTER JOIN zupload_testing AS TABLE ON (  TABLE~document_num = A~AccountingDocument AND TABLE~fiscalyear = A~FiscalYear
*                                            AND TABLE~companycode = A~CompanyCode   )
*WHERE
*A~FinancialAccountType = 'K' AND A~AccountingDocumentType = 'KZ'  AND table~posting_date = @date                   "A~PostingDate = @date
*AND A~SpecialGLCode <> 'F'  AND B~IsReversal = '' AND B~IsReversed = ''
*INTO TABLE @DATA(IT).


select
document_num as  AccountingDocument,
fiscalyear as FISCALYEAR,
companycode as CompanyCode,
vendor as Supplier
from zupload_testing
where posting_date = @date
INTO TABLE @DATA(IT).

LOOP AT it INTO DATA(WA).
  " If document is already processed, skip the iteration
  IF WA-AccountingDocument = document.
    CONTINUE.
  ENDIF.

  fromdate = WA-FISCALYEAR.
  todate = WA-FISCALYEAR.
  document = WA-AccountingDocument.
  comcode = WA-CompanyCode.
  supplier = WA-Supplier.


  document = WA-AccountingDocument.




DATA(PDF1) = ypayment_advice_for_mail=>read_posts(  fromdate    = fromdate
                                           todate            = todate
                                           vendor            = vendor
                                           document          = document
                                           comcode           = comcode
                                           remark            = remark
*                                           fiscal_year       = fiscalyear
                                           clearingdocument  = CLR_DOC

                                             ) .



    DATA(pdf2)    = xco_cp=>string( pdf1 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
* SELECT  FROM i_operationalacctgdocitem AS a
*    LEFT OUTER JOIN i_supplier AS b ON ( b~supplier = a~supplier  )
*    LEFT OUTER JOIN ZSUPPLIER_MAIL_ID    AS c ON ( c~addressid = b~addressid )
*    FIELDS
*    a~companycode,
*     a~accountingdocument,
*     b~supplier,
*     b~suppliername,
*     c~addressid,
*     c~emailaddress
*    WHERE A~AccountingDocumentType IN ( 'KZ' )
*    AND A~FiscalYear = @fromdate
*    AND A~AccountingDocument = @document
*    AND A~Supplier <> ''
*    AND A~ClearingDate = @date
*
*    INTO table @DATA(supplierdetail).
*    SORT supplierdetail BY CompanyCode AccountingDocument .
*    DELETE ADJACENT DUPLICATES FROM supplierdetail COMPARING CompanyCode AccountingDocument.
*

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""UTR LOGIC"""""""""""""""""

* SELECT  FROM i_operationalacctgdocitem AS a
*    LEFT OUTER JOIN i_supplier AS b ON ( b~supplier = a~supplier  )
*    LEFT OUTER JOIN ZSUPPLIER_MAIL_ID    AS c ON ( c~addressid = b~addressid )
*    LEFT OUTER JOIN I_JournalEntry AS D ON ( D~AccountingDocument = A~AccountingDocument AND D~FiscalYear = A~FiscalYear
*                                             AND D~CompanyCode = A~CompanyCode )
*    FIELDS
*    a~companycode,
*     a~accountingdocument,
*     b~supplier,
*     b~suppliername,
*     c~addressid,
*     c~emailaddress,
*     D~DocumentReferenceID
*    WHERE A~AccountingDocumentType IN ( 'KZ' )
*    AND A~FiscalYear = @fromdate
*    AND A~AccountingDocument = @document
*    AND A~Supplier <> ''
*    AND A~ClearingDate = @date
*    AND D~DocumentReferenceID <> ''
*
*    INTO table @DATA(supplierdetail).
*    SORT supplierdetail BY CompanyCode AccountingDocument .
*    DELETE ADJACENT DUPLICATES FROM supplierdetail COMPARING CompanyCode AccountingDocument.



 SELECT  FROM zupload_testing AS a
    LEFT OUTER JOIN i_supplier AS b ON ( b~supplier = a~vendor  )
    LEFT OUTER JOIN ZSUPPLIER_MAIL_ID    AS c ON ( c~addressid = b~addressid )

    FIELDS
     a~companycode,
     a~document_num,
     b~supplier,
     b~suppliername,
     c~addressid,
     c~emailaddress

where a~document_num =  @document
and a~fiscalyear = @fromdate and a~companycode = @comcode
and a~utr_no <> ''
and a~enq_status = 'Processed'
and c~emailaddress <> ''
into table @data(utr).




  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

     LOOP AT utr INTO DATA(wa1).


     clear : from_mail, cc_mail, to_mail, bcc_email , companyname.


          to_mail = wa1-emailaddress.
*          bcc_email = 'bhuvnesh.rajpurohit@novelveritas.com'.
*          bcc_email = 'babita.paliwal@novelveritas.com'.
*          to_mail2 = 'babita.paliwal@novelveritas.com'.
          companyname = 'RAJASTHAN BARYTES LIMITED'.
*          Address = 'SP2-5 AND 6, NIC MAJRAKATH JAPANESE ZONE,'.
*          adress1 = 'NEEMRANA  ALWAR' .
*          adress2 = 'RAJASTHAN 301705' .
          mail =  ''.



        CLEAR: i_objtx.

    i_objtxt-line = '<HTML> <BODY>'.
    APPEND i_objtxt TO i_objtx.
    CLEAR : i_objtxt.

    CONCATENATE '<p style="font-family:Calibri;font-size:16;">' 'Dear Sir/Madam,' INTO i_objtxt-line.
    APPEND i_objtxt TO i_objtx.
    CLEAR : i_objtxt.


    CONCATENATE '<p>' 'Please find your attached payment advice.' '<p>' INTO i_objtxt-line SEPARATED BY space.
    APPEND i_objtxt-line TO i_objtx.
    CLEAR : i_objtxt .


     CONCATENATE '<p>' 'Still, if you have any more queries, feel free to contact with concern person.' MAIL '<p>' INTO i_objtxt-line SEPARATED BY space.
    APPEND i_objtxt-line TO i_objtx.
    CLEAR : i_objtxt .



    i_objtxt-line = '</body> </html>'.
    APPEND i_objtxt TO i_objtx.
    CLEAR i_objtxt.


      i_objtxt-line = '<p> Best Regards </p>'.
    APPEND i_objtxt-line TO i_objtx.
    CLEAR : i_objtxt .

   CONCATENATE '<p><b>' companyname'</b></p>' INTO i_objtxt-line.
        APPEND i_objtxt TO i_objtx.
        CLEAR: i_objtxt.

  CONCATENATE '<p><b>' address'</b></p>' INTO i_objtxt-line.
        APPEND i_objtxt TO i_objtx.
        CLEAR: i_objtxt.


        CONCATENATE '<p><b>' adress1'</b></p>' INTO i_objtxt-line.
        APPEND i_objtxt TO i_objtx.
        CLEAR: i_objtxt.


      CONCATENATE '<p><b>' adress2 '</b></p>' INTO i_objtxt-line.
        APPEND i_objtxt TO i_objtx.
        CLEAR: i_objtxt.


        i_objtxt-line = '</body></html>'.
    APPEND i_objtxt TO i_objtx.
    CLEAR i_objtxt.


DATA :
      v_lines_txt TYPE i,
      v_lines_bin TYPE i.

    v_lines_txt = lines( i_objtx ).

    LOOP  AT i_objtx INTO i_objtxt .
      IF lv = ''.
        lv = i_objtxt.
      ELSE.
        CONCATENATE lv i_objtxt INTO lv.
        CONDENSE lv .
      ENDIF.
    ENDLOOP.

************************************************************************ MAIL ID ***************************************************************
DATA: lo_message     TYPE REF TO cl_bcs_mail_message,
      lv_result      TYPE string.

    TRY.
        DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
        lo_mail->set_sender( 'noreply_sap@rblresourcesl.com' ).
        lo_mail->add_recipient( to_mail ).

*        lo_mail->add_recipient( 'babita.paliwal@novelveritas.com' ).
*        lo_mail->add_recipient( 'Sugandh@rajasthanbarytes.com' ).
*        lo_mail->add_recipient( 'bankpayments@rajasthanbarytes.com ' ).
*        lo_mail->add_recipient( 'accounts@rajasthanbarytes.com' ).

*        lo_mail->add_recipient(
*                                iv_address = 'accounts@rajasthanbarytes.com'
*                                iv_copy    = cl_bcs_mail_message=>cc  ).

*        lo_mail->add_recipient(
*                                iv_address = 'bankpayments@rajasthanbarytes.com'
*                                iv_copy    = cl_bcs_mail_message=>cc  ).

*        lo_mail->add_recipient(
*                                iv_address = 'Sugandh@rajasthanbarytes.com'
*                                iv_copy    = cl_bcs_mail_message=>cc  ).
*
*        lo_mail->add_recipient(
*                                iv_address = 'dev.sharma@novelveritas.com'
*                                iv_copy    = cl_bcs_mail_message=>bcc  ).

*        lo_mail->add_recipient(
*                                iv_address = 'bhuvnesh.rajpurohit@novelveritas.com'
*                                iv_copy    = cl_bcs_mail_message=>bcc  ).






************************************************************************************************************************************************
    DATA DOC TYPE string .
*  CONCATENATE  document  '.pdf' into DOC.
  DOC = |{ document }({ WA1-SupplierName }).PDF|.

        lo_mail->set_subject( 'PDF of your Payment Advice' ).
        lo_mail->set_main( cl_bcs_mail_textpart=>create_instance(
        iv_content      = lv
        iv_content_type = 'text/html'
        ) ).

        lo_mail->add_attachment( cl_bcs_mail_binarypart=>create_instance(
          iv_content      =  pdf2
          iv_content_type = 'application/pdf'
          iv_filename     = DOC
         ) ).

        lo_mail->send( IMPORTING et_status = DATA(lt_status) ).
      CATCH cx_bcs_mail INTO lx_bcs_mail.
ENDTRY.

RESULT12 = 'Mail Send Successufully' .


CLEAR : lv,i_objtxt,i_objtx.
*

 CLEAR :cc_mail,from_mail ,to_mail ,bcc_email.

ENDLOOP.

"""""""""""""""""""""""""""""" """"""""""""""""""""""""""""""""""""""""""""""""""



*data : message TYPE string.
*DATA wa_yafter_automail TYPE yafter_automail.
*
*CLEAR wa_yafter_automail.
*
*wa_yafter_automail-companycode = wa1-companycode.
*wa_yafter_automail-fiscalyear = fromdate.
*wa_yafter_automail-accountingdocument = document.
*wa_yafter_automail-supplierfullname = wa1-suppliername.
*wa_yafter_automail-emailaddress = wa1-emailaddress.
*wa_yafter_automail-postingdate = date.
*
*
*MODIFY  yafter_automail from @wa_yafter_automail.
*IF sy-subrc = 0.
*  message = 'Data saved successfully for document:'.
*ELSE.
*  message = 'Error while saving data for document:'.
*ENDIF.

ENDLOOP.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
ENDMETHOD.
ENDCLASS.
