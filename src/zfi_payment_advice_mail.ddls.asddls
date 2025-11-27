@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Payment Advice Mail'
@Metadata.ignorePropagatedAnnotations: true
define  view entity ZFI_PAYMENT_ADVICE_MAIL as select from I_OperationalAcctgDocItem as a 
left outer join I_Supplier  as b on ( b.Supplier = a.Supplier )
left outer join I_AddrCurDefaultEmailAddress as c on (  c.AddressID = b.AddressID )

{ 

  key a.CompanyCode,
  key a.FiscalYear,
  key a.AccountingDocument,
  key a.Supplier,
  key b.SupplierFullName,
  key c.EmailAddress
    
} where a.AccountingDocumentType = 'KZ'
    and a.FinancialAccountType = 'K'
    
