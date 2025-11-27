CLASS ycl_pdf_mail_pay_adv DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    CLASS-METHODS
      read_data
        IMPORTING document        TYPE string
                  fromdate        TYPE string
                  todate          TYPE string
                  vendor          TYPE string
                  comcode         TYPE string
                  clearingdocument  TYPE string
                  remark          TYPE string
        RETURNING VALUE(result12) TYPE string
        RAISING   cx_static_check .

    CLASS-DATA: lx_bcs_mail TYPE REF TO cx_bcs_mail.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_PDF_MAIL_PAY_ADV IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(config_instance) = cl_bcs_mail_system_config=>create_instance( ).
        "Add allowed domains
    TRY.
    "Add allowed domains
      CATCH cx_bcs_mail_config INTO DATA(write_error).
        "handle exception
    ENDTRY.
 ENDMETHOD.


 METHOD read_data.

     TYPES : BEGIN OF it_solisti1 ,
              line TYPE c LENGTH 255,
            END OF it_solisti1.

    DATA : i_objtx  TYPE STANDARD TABLE OF it_solisti1,
           i_objtxt TYPE it_solisti1,
          SUPPLIER_NAME TYPE STRING,
                 lv TYPE string,
                lv1 TYPE string,
            mail_id(512) TYPE C,
            indctr  TYPE C.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  SELECT SINGLE * FROM I_OperationalAcctgDocItem AS A LEFT OUTER JOIN I_supplier AS B ON ( A~Supplier = B~Supplier )
                       WHERE A~AccountingDocument = @document AND A~accountingdocumenttype = 'KZ' AND A~Supplier NE '' INTO @DATA(sup_name).
                       SUPPLIER_NAME = sup_name-b-SupplierName.
  SELECT SINGLE * FROM ZSUPPLIER_MAIL_ID WHERE AddressID = @sup_name-B-AddressID INTO @DATA(MAIL).
                       MAIL_ID = MAIL-EmailAddress.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
     data pdf2 type xstring .
     data(pdf21)  = ypayment_advice=>read_posts(  fromdate          = fromdate
                                                  todate            = todate
                                                  vendor            = vendor
                                                  document          = document
                                                  comcode           = comcode
                                                  clearingdocument  = clearingdocument
                                                   remark           = remark ) .
          pdf2    = xco_cp=>string( pdf21 )->as_xstring( xco_cp_binary=>text_encoding->base64 )->value.


    i_objtxt-line = '<HTML> <BODY>'.
    APPEND i_objtxt TO i_objtx.
    CLEAR : i_objtxt.

    CONCATENATE '<p style="font-family:Calibri;font-size:16;">' 'Dear Sir,' INTO i_objtxt-line.
    APPEND i_objtxt TO i_objtx.
    CLEAR : i_objtxt.


    CONCATENATE '<p>' 'Please find your attached payment advice.' '<p>' INTO i_objtxt-line SEPARATED BY space.
    APPEND i_objtxt-line TO i_objtx.
    CLEAR : i_objtxt .

    i_objtxt-line = '</body> </html>'.
    APPEND i_objtxt TO i_objtx.
    CLEAR i_objtxt.


      i_objtxt-line = '<p> Regards </p>'.
    APPEND i_objtxt-line TO i_objtx.
    CLEAR : i_objtxt .

    i_objtxt-line = '<p><b> RBL Natural Resources Limited </b></p>'.
    APPEND i_objtxt-line TO i_objtx.
    CLEAR : i_objtxt .


        i_objtxt-line = '</body> </html>'.
    APPEND i_objtxt TO i_objtx.
    CLEAR i_objtxt.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

    TRY.
        DATA(lo_mail) = cl_bcs_mail_message=>create_instance( ).
        lo_mail->set_sender( 'noreply_SAP@rblresourcesl.com' ).
*        lo_mail->set_sender( 'Sapnoreply123@rblresourcesl.com' ).
*        lo_mail->set_sender( 'noreply_sap@rajasthanbarytes.com' ).

*     IF sY-SYSID = 'YXD' OR SY-SYSID = 'YXC'.
        lo_mail->add_recipient( 'dipesh.tailor@novelveritas.com' ).
        lo_mail->add_recipient( 'MOHIT.TANWAR@novelveritas.com' ).

*        lo_mail->add_recipient( 'siddharth@rblresourcesl.com' ).
*     ELSE.
*         lo_mail->add_recipient( mail_id ).
*         lo_mail->add_recipient( 'Sugandh@rblresourcesl.com  ' ).
*         lo_mail->add_recipient( 'abhargava@rblresourcesl.com' ).
*         lo_mail->add_recipient( 'kamal.nyati@rblresourcesl.com' ).
*         lo_mail->add_recipient( 'bankpayments@rblresourcesl.com' ).
*     ENDIF.

  DATA DOC TYPE string .
  CONCATENATE document '.pdf' into DOC.

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
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF mail_id IS  INITIAL.
     result12 = 'Supplier mail ID missing'.
  ELSE.
    result12 = 'Mail send sucessfully.'.
  ENDIF.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  ENDMETHOD.
ENDCLASS.
