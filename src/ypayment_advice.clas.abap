CLASS ypayment_advice DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    CLASS-DATA : access_token TYPE string ,
                 xml_file     TYPE string ,
                 template     TYPE string ,
                 register      TYPE STRING.

    TYPES :
      BEGIN OF struct,
        xdp_template TYPE string,
        xml_data     TYPE string,
        form_type    TYPE string,
        form_locale  TYPE string,
        tagged_pdf   TYPE string,
        embed_font   TYPE string,
      END OF struct."

    CLASS-METHODS :

      create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check,

      read_posts
        IMPORTING VALUE(fromdate) TYPE string
                  VALUE(todate)   TYPE string
                  VALUE(vendor)   TYPE string
                  VALUE(document) TYPE string
                  VALUE(comcode)  TYPE string
                  VALUE(remark)   TYPE string
                  VALUE(clearingdocument)   TYPE string
*                  VALUE(date)   TYPE string
      RETURNING VALUE(result12) TYPE string
        RAISING   cx_static_check .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS lc_ads_render TYPE string VALUE '/ads.restapi/v1/adsRender/pdf'.
    CONSTANTS  lv1_url    TYPE string VALUE 'https://adsrestapi-formsprocessing.cfapps.eu10.hana.ondemand.com/v1/adsRender/pdf?templateSource=storageName&TraceLevel=2'  .
    CONSTANTS  lv2_url    TYPE string VALUE 'https://rbl-sap-86idswsa.authentication.eu10.hana.ondemand.com/oauth/token'  .
    CONSTANTS lc_storage_name TYPE string VALUE 'templateSource=storageName'.
    CONSTANTS  lc_template_name TYPE string VALUE 'PaymentAdvice/PaymentAdvice'.


ENDCLASS.



CLASS YPAYMENT_ADVICE IMPLEMENTATION.


  METHOD create_client .
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).

  ENDMETHOD .


  METHOD if_oo_adt_classrun~main.

    TRY.

    ENDTRY.

  ENDMETHOD.


  METHOD read_posts .



    DATA: where TYPE string.

    IF clearingdocument IS  INITIAL .
    SELECT  * FROM i_operationalacctgdocitem WHERE (where)
    AND accountingdocumenttype = 'KZ'
    AND accountingdocument = @document and FiscalYear = @fromdate and CompanyCode = @comcode INTO TABLE @DATA(tab1).
    ELSE .
    DATA(CLEARING)  = 'X' .

    SELECT  * FROM i_operationalacctgdocitem WHERE (where)
    AND ClearingJournalEntryFiscalYear = @todate
    AND ClearingJournalEntry = @document and FiscalYear = @fromdate and CompanyCode = @comcode INTO TABLE @tab1.



    ENDIF .

    READ TABLE tab1 WITH KEY transactiontypedetermination = 'WIT' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
    DATA(wt) = VALUE #( tab1[ sy-tabix ]-amountinfunctionalcurrency ).
    ENDIF.

    DATA(lt_tab) = tab1[].
    DELETE lt_tab WHERE supplier IS INITIAL.
    IF line_exists( lt_tab[ 1 ] ).
    DATA(supamt) = VALUE #( lt_tab[ 1 ]-amountinfunctionalcurrency ).
    ENDIF.
    READ TABLE tab1 INTO DATA(WT_TAB1) INDEX 1 .

    SELECT SINGLE * FROM i_operationalacctgdocitem WHERE (where) AND ClearingJournalEntryFiscalYear = @todate
    AND accountingdocumenttype = 'KZ'
    AND accountingdocument = @document  and ClearingDate <> '00000000' INTO  @DATA(tab2).

    READ TABLE lt_tab INTO DATA(WT_TAB2) INDEX 1 .

   IF clearingdocument IS  INITIAL .
******************************************NO CLEAR JOURNALENTRY supplier*****************************

    SELECT SINGLE * FROM i_operationalacctgdocitem WHERE (where)  AND accountingdocumenttype = 'KZ'
    AND accountingdocument = @WT_TAB2-ClearingJournalEntry and ClearingJournalEntryFiscalYear = @WT_TAB2-ClearingJournalEntryFiscalYear
    AND supplier <> '' and FinancialAccountType = 'K' INTO  @DATA(tab3).


    ELSE .
    CLEARING  = 'X' .
***************************************CLEARINGJOURNALENTRY supplier******************************

    SELECT SINGLE * FROM i_operationalacctgdocitem WHERE (where)  AND accountingdocumenttype = 'KZ'
    AND accountingdocument = @WT_TAB1-ClearingJournalEntry and ClearingJournalEntryFiscalYear = @WT_TAB1-ClearingJournalEntryFiscalYear
    AND supplier <> '' and FinancialAccountType = 'K' INTO  @tab3.

*    SELECT SINGLE * FROM i_supplier WHERE supplier = @tab3-supplier  INTO @supplier.
*
*   data(PAN)   = strlen( supplier-TaxNumber3 ).
*   DATA(VAR)   = supplier-TaxNumber3+2(15).
*   DATA(PANNO) = var+0(11).


    ENDIF.

    SELECT SUM( AmountInCompanyCodeCurrency ) FROM i_operationalacctgdocitem
    WHERE   ClearingJournalEntry = @document AND AccountingDocumenT NE @document
    AND ClearingDocFiscalYear = @WT_TAB1-ClearingDocFiscalYear and CompanyCode = @comcode  INTO @DATA(TOTALCLEARING)  .

    DATA xsml TYPE string.
    DATA amount TYPE string.

    template = 'PaymentAdvice/PaymentAdvice' .

*    DELETE ADJACENT DUPLICATES FROM tab1 COMPARING accountingdocument .
   IF clearing  IS INITIAL .
   delete tab1 where Supplier is initial .

   ELSE .

   delete tab1 where AccountingDocument = document .
   ENDIF .

    LOOP AT tab1 INTO DATA(wa_tab1).


    select single documentreferenceid, postingdate from I_JournalEntry where fiscalyear = @todate AND  accountingdocument = @document
    AND CompanyCode = @comcode  into @DATA(ref_id).


     SELECT * FROM i_operationalacctgdocitem
     WHERE   ClearingJournalEntry = @wa_tab1-ClearingJournalEntry
*     AND accountingdocumenttype <> 'KZ'
     AND fiscalyear = @wa_tab1-fiscalyear AND CompanyCode = @wa_tab1-CompanyCode
     AND AccountingDocument <> @document INTO TABLE @DATA(journalentrynum)  .


    IF clearing  IS INITIAL .
    LOOP AT journalentrynum INTO DATA(wa_tab2).

    SELECT SINGLE * FROM i_journalentry WHERE fiscalyear = @wa_tab2-fiscalyear AND accountingdocument = @wa_tab2-AccountingDocument
    AND CompanyCode = @wa_tab2-CompanyCode INTO @DATA(journalentry).

    ENDLOOP.

    ELSE .

    SELECT SINGLE * FROM i_journalentry WHERE fiscalyear = @wa_tab1-fiscalyear AND accountingdocument = @wa_tab1-AccountingDocument
    AND CompanyCode = @wa_tab1-CompanyCode INTO @journalentry.

    ENDIF.



    SELECT SINGLE *   FROM i_operationalacctgdocitem
WHERE HouseBank IS NOT INITIAL AND
 AccountingDocument = @wa_tab1-AccountingDocument and FiscalYear = @todate and CompanyCode = @comcode  INTO @DATA(TOTAL)  .
 IF sy-subrc <> 0 .
 SELECT SINGLE * FROM i_operationalacctgdocitem WHERE (where)
              AND AccountingDocument = @document and FiscalYear = @todate  AND HouseBank <> '' and CompanyCode = @comcode INTO @TOTAL.
 ENDIF.


IF clearing NE 'X' .

DATA(TOTAL1) = TOTAL-AmountInCompanyCodeCurrency  .
DATA(P)  = -1 .

ELSEIF clearing = 'X'  .
 P = -1.
TOTAL1 =  TOTALCLEARING   .


ENDIF .


***********************************Plantwise address ********************************************
SELECT single  * FROM i_operationalacctgdocitem where accountingdocument = @document
and FiscalYear = @fromdate and CompanyCode = @comcode INTO @DATA(PLANTADD).


loop at tab1 into data(wa).

SELECT SINGLE * FROM i_supplier WHERE supplier = @wa-Supplier  INTO @data(supplier).

   data(PAN)   = strlen( supplier-TaxNumber3 ).
   DATA(VAR)   = supplier-TaxNumber3+2(15).
   DATA(PANNO) = var+0(11).

ENDLOOP.


*   loop at tab1 into DATA(Wa_TAB).

    SELECT SINGLE AmountInCompanyCodeCurrency FROM i_operationalacctgdocitem WHERE (where)
      AND transactiontypedetermination = 'WIT' AND WithholdingTaxCode <> ''  AND
      accountingdocument = @wa_tab1-AccountingDocument and FiscalYear = @Wa_TAB1-FiscalYear and CompanyCode = @Wa_TAB1-CompanyCode INTO  @DATA(tab) .




* clear : Wa_TAB.
*
*  ENDLOOP.
   SELECT SINGLE * FROM ytb_company_name WHERE company_code = @Wa_TAB1-CompanyCode INTO @DATA(COMP_NAME).

     IF comp_name-company_code = '1000' AND Wa_TAB1-ClearingDate > '20250810'.
       register = COMP_NAME-new_company_name .    "'RBL NATURAL RESOURCES LIMITED'.
       DATA(FORMAL_NAME) = '(Formerly known as Natural Resources Limited)'.
*        FOR              = 'For : RBL Natural Resources Ltd.                '  .
*        DATA(FOR_BNK)       = 'RBL Natural Resources Ltd.'  .
        ELSE.
        register = 'RAJASTHAN BARYTES LTD'.
*        FOR       = 'For : Rajasthan Barytes Ltd.                '  .
*        FOR_BNK   = 'Rajasthan Barytes Ltd.'.
         ENDIF.


IF PLANTADD-CompanyCode ='1000'.
        Register  =  register.
  DATA(Regis_Add) =  'RBL House" Punjawati, Gaurav Path, Near R. K. Circle, Udaipur(Raj.) -313001                                '  .
  DATA(Corpo_Add) =  '                                               '  .
  DATA(Head_off)  =  ''  .
  DATA(Email)     =  'Email: INFO@RBLRESOURCESL.COM'  .
  DATA(WebSite)   =  'WWW.RBLRESOURCESL.COM Ph: 0294-2980146'  .

 ELSEIF PLANTADD-CompanyCode ='2000'.
  Register        =  'USHA MICRONS PVT. LTD.'  .
  Regis_Add       =  'Regd - LodhaSupremus, unit No 702 A, 7th Floor, SenapatiBapat Marg, Upper Worli, Mumbai, 400013(MH)'  .
  Corpo_Add       =  'Corpo - RBL House, Punjawati, Udaipur   '  .
  Head_off        =  ''  .
  Email           =  'Email: INFO@RBLRESOURCESL.COM'  .
  WebSite         =  'WWW.RBLRESOURCESL.COM Ph: 0294-2980146'  .
 ELSEIF PLANTADD-CompanyCode ='3000'.
  Register        =  'AMABAJI MINERALS'  .
* Regis_Add       =  'C/o Nr Nagrik Sahkari Bank, Above Kharodiya Bothers, Himatnagar, Sabarkantha, Gujarat, 383001'  .
  Corpo_Add       =  ''  .
  Head_off        =  ''  .
  Email           =  'Email: INFO@RBLRESOURCESL.COM'  .
  WebSite         =  'WWW.RBLRESOURCESL.COM Ph: 0294-2980146'  .
 ELSEIF PLANTADD-CompanyCode ='4000'.
  Register        =  'RBL LOGISTICS PRIVATE LIMITED'  .
  Regis_Add       =  'Lodha Supremus , Unit No. 702 A, 7th floor, Senapati Bapat Marg, Upper Worli Mumbai Mumbai City MH 400013'  .
  Corpo_Add       =  ''  .
  Head_off        =  '"RBL House" Punjawati, Gaurav Path, Near R. K. Circle, Udaipur(Raj.) -313001'  .
  Email           =  'Email: INFO@RBLRESOURCESL.COM'  .
  WebSite         =  'WWW.RBLRESOURCESL.COM Ph: 0294-2980146'  .

  ELSEIF PLANTADD-CompanyCode ='5000'.
*        register1  = 'RBL MINERALS & CHEMICALS PVT. LTD.'  .
        register  =  'RBL MINERALS & CHEMICALS PVT. LTD.'.
        regis_add =  'Works:- 24/02 and 24/03, Mudhol, Lokapur , Bhagalkot -587122, Karnataka, India' .
        corpo_add =  ''  .
        head_off  =  ''  .
        email     =  'Email: INFO@RBLRESOURCESL.COM'  .
        website   =  'WWW.RBLRESOURCESL.COM Ph: 0294-2980146'  .
*        for       =  'For : RBL MINERALS & CHEMICALS PVT. LTD.'  .
*        for1       =  'For : Mr. Lokendra Singh Rathore'  .

  ELSEIF PLANTADD-CompanyCode ='7000'.
*        register1  = 'PREMIUM SILICA INDIA PVT. LTD.'  .
        register  =  'PREMIUM SILICA INDIA PVT. LTD.'.
        regis_add =  'Works:- P1, RS NO 56/1,  KALI TALAVADI,BHUJ, KACHCHH(370105), GUJARAT' .
        corpo_add =  ''  .
        head_off  =  ''  .
        email     =  'Email: INFO@RBLRESOURCESL.COM'  .
        website   =  'WWW.RBLRESOURCESL.COM Ph: 0294-2980146'  .
*        for       =  'For : PREMIUM SILICA INDIA PVT. LTD.'  .
*        for1      =  'For : Mr. Joseph Poovatholil Varghese'  .

  ELSEIF PLANTADD-CompanyCode ='8000'.
*        register1  = 'PREMIUM SILICA INDIA PVT. LTD.'  .
        register  =  'RBL Matrix LLP.'.
        regis_add =  'RBL House, Punjawati Gaurav Path' .
        corpo_add =  'Nr. R.K.Circle,Udaipur 313004'  .
        head_off  =  ''  .
        email     =  'Email: INFO@RBLRESOURCESL.COM'  .
        website   =  'WWW.RBLRESOURCESL.COM Ph: 0294-2980146'  .
*        for       =  'For : PREMIUM SILICA INDIA PVT. LTD.'  .
*        for1      =  'For : Mr. Joseph Poovatholil Varghese'  .



 ENDIF.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
SELECT single utr_no from zupload_testing as a
where a~document_num =  @document
and a~fiscalyear = @fromdate and a~companycode = @comcode
and a~utr_no <> ''
and a~enq_status = 'Processed'
into @data(utr).




      DATA(lv_xml) = |<Form>| &&
           |<bdyMain>| &&
           |<Plantaddress>| &&
           |<Add1>{ Register }</Add1>| &&
           |<Address>{ FORMAL_NAME }</Address>| &&
           |<Add2>{ Regis_Add }</Add2>| &&
           |<Add3>{ Corpo_Add }</Add3>| &&
           |<Add4>{ Head_off }</Add4>| &&
           |</Plantaddress>| &&
            |<AddressLine1>{ supplier-SupplierName }</AddressLine1>| &&
*           |<AddressLine2>{ PANNO }</AddressLine2>| &&
            |<AddressLine3>{  supplier-TaxNumber3   }</AddressLine3>| &&
           |<AddressLine4>{ PANNO }</AddressLine4>| &&
*          |<dttCreationDate></dttCreationDate>| &&
           |<paymentdate>{ tab2-ClearingDate+6(2) }/{ tab2-ClearingDate+4(2) }/{ tab2-ClearingDate+0(4) }</paymentdate>| &&
*          |<txtAccountByCustomerSupplier>Mirum est ut animus agitatione motuque corporis excitetut.</txtAccountByCustomerSupplier>| &&
           |<txtCustomMessage>{ remark }</txtCustomMessage>| &&
            |<Date>{ ref_id-PostingDate }</Date>| &&
*            |<Ref>{ ref_id-DocumentReferenceID }</Ref>| &&
*           |<Ref>{ utr }</Ref>| &&
            |<tblLineItems>|.
      amount = supamt + wt.
      DATA(lv_xml2) =

                  |<rowLineItemNode>| &&
                  |<txtDocumentReference>{ journalentry-documentreferenceid  }</txtDocumentReference>| &&
*                  |<txtDocumentReference>{ wa-AccountingDocument  }</txtDocumentReference>| &&
                  |<dttDocumentDate>{ wa_tab1-DocumentDate }</dttDocumentDate>| &&
                  |<txtAccountingDocumentType>KZ</txtAccountingDocumentType>| &&
*                  |<decAmountInTransactionCurrency>{ wa_tab1-amountincompanycodecurrency }</decAmountInTransactionCurrency>| &&
*                 |<decAmountInTransactionCurrency>{ amount }</decAmountInTransactionCurrency>| &&
                  |<decAmountInTransactionCurrency>{ wa_tab1-AmountInCompanyCodeCurrency * P }</decAmountInTransactionCurrency>| &&
                  |<TDSAmount>{ tab }</TDSAmount>| &&  """"WithholdingTaxAmount
                  |</rowLineItemNode>| &&
                  |<rowLineItemNode1>| &&
*                 |<TOT>Mirum est</TOT>| &&
                  |<Totam>{ TOTAL1 * p  }</Totam>| &&
                  |</rowLineItemNode1>|.

      CONCATENATE xsml lv_xml2 INTO  xsml .

CLEAR : tab.
""""""""""""""""""""""""""""""""""""""""""""'for Save data in report
* data  : date TYPE datum,
*           date1    TYPE string .
*
*date = sy-datum .
**date1 = |{ date+0(4) }|.
* DATA: to_mail(512) TYPE c.
*
*SELECT  FROM i_operationalacctgdocitem AS a
*    LEFT OUTER JOIN i_supplier AS b ON ( b~supplier = a~supplier  )
*    LEFT OUTER JOIN ZSUPPLIER_MAIL_ID    AS c ON ( c~addressid = b~addressid )
*    FIELDS
*    a~companycode,
*     a~accountingdocument,
*     a~ValueDate,
*     b~supplier,
*     b~suppliername,
*     c~addressid,
*     c~emailaddress
*    WHERE A~AccountingDocumentType IN ( 'KZ' )
*    AND A~FiscalYear = @fromdate
*    AND A~AccountingDocument = @document
*    AND A~Supplier <> ''
**    AND A~PostingDate = @date
*    AND A~ClearingDate = @date
*
*    INTO table @DATA(supplierdetail).
*    SORT supplierdetail BY CompanyCode AccountingDocument .
*    DELETE ADJACENT DUPLICATES FROM supplierdetail COMPARING CompanyCode AccountingDocument.
*     LOOP AT supplierdetail INTO DATA(wa1).
*
*
*
*     clear :  to_mail.
*
*
*          to_mail = wa1-emailaddress.
*
*
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
*wa_yafter_automail-amount = TOTAL1 * p.
*wa_yafter_automail-utr = journalentry-documentreferenceid.
*
*
*MODIFY  yafter_automail from @wa_yafter_automail.
*IF sy-subrc = 0.
*  message = 'Data saved successfully for document:'.
*ELSE.
*  message = 'Error while saving data for document:'.
*ENDIF.
*
*
*
*ENDLOOP.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    ENDLOOP.

      SELECT SINGLE *  FROM i_housebankaccountlinkage
      WHERE  housebank = @TOTAL-HouseBank and CompanyCode = @comcode INTO @DATA(bank).

      SELECT single * from I_OUTGOINGCHECK
      where housebank = @bank-HouseBank and PaymentCompanyCode = @comcode AND PaymentDocument = @document
      INTO @DATA(CHEQUENO) .

    DATA(lv_xml3) =
                    |</tblLineItems>| &&
                     |<bankpayref></bankpayref>| &&
*                     |<Chequeno>{ CHEQUENO-OutgoingCheque }</Chequeno>| &&
                    |<Chequeno>{ UTR }</Chequeno>| &&
                    |<BankName>{ bank-bankname }</BankName>| &&
                    |<AccountNo>{ bank-bankaccount }</AccountNo>| &&
*               |<Amount>{}</Amount>| &&
                    |</bdyMain>| &&
                    |</Form>|.

    CONCATENATE lv_xml xsml lv_xml3 INTO lv_xml .

    REPLACE ALL OCCURRENCES OF '&' IN lv_xml WITH 'AND'.
    CALL METHOD ycl_test_adobe=>getpdf(
      EXPORTING
        xmldata  = lv_xml
        template = template
      RECEIVING
        result   = result12 ).





  ENDMETHOD.
ENDCLASS.
