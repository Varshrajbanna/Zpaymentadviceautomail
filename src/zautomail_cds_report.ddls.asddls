@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'YAUTOMAIL_CDS_REPORT'
@Metadata.ignorePropagatedAnnotations: true
define  view entity ZAUTOMAIL_CDS_REPORT as  select from yafter_automail

{
key companycode as Companycode,
key fiscalyear as Fiscalyear,
key accountingdocument as Accountingdocument,
supplierfullname as Supplierfullname,
emailaddress as Emailaddress,
postingdate as Postingdate,
 amount as  Amount,
  utr as  Utr
}
