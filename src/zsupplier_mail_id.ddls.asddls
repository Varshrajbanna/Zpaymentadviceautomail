@AbapCatalog.sqlViewName: 'ZMAIL_SUP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS for Supplier E-Mail ID'
define view  ZSUPPLIER_MAIL_ID as select from I_Supplier as A 
 left outer join I_AddrCurDefaultEmailAddress as B on ( A.AddressID = B.AddressID )
{
  A.AddressID,
  A.Supplier,
  B.EmailAddress,
  B.AddressID    as ADDRESS_ID,
  B.AddressPersonID
  
 
}
