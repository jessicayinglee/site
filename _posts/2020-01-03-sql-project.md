---
layout: post
title: "Emailing custom database query results based on recipient"
subtitle: "Language: SQL"
date: 2020-01-03
categories: projects
---

The goal of this project is to send emails to Key Contacts for each product at each company that purchases that product. The email will contain a list of all other contacts in the client database that are associated with the Key Contact's product(s) and company. Roles are given to contacts at each company, for each product. 

Each person in the database is assigned a role. The roles are:

* Key Contact (KC). The KC is the "owner" of the product and signs the participation agreement. Each product has only one KC at each participating company. 
* Key Contact Cc (KCCC). The KCCCs are cc'd on all e-mails to the KC.
* Data Contact (DC). The DCs are contacted for matters related to data collection.
* Registered User (RU). RUs are people who make significant use of the product.

The queries will select from the following tables:

** insert image of tables **

# Methodology
The database and queries are written for MSSQL/MS Access.

1. Merge tables to collect all relevant data columns. 
   a. Join Company with Division, then join with Role, then join with Person. Resulting table is called "People"
   b. Join Product Role with Product Link Type (contains names for types of roles). Resulting table is called "Roles
   c. Join People with Roles, then join with Email. Store the final result in "AllContacts"
```sql
SELECT ProductID,
       ContactType,
       [First],
       [Last],
       [First] & Space(1) & [Last] AS FullName,
       CompanyName,
       DivisionName,
       Email
INTO   AllContacts
FROM   ((SELECT *
        FROM   ((SELECT DivisionID,
                        Nom AS DivisionName,
                        CompanyID,
                        CompanyName
                FROM 	tblDivision AS D
		INNER JOIN (SELECT CompanyID,
				   Nom AS CompanyName
			    FROM   tblCompany) AS C
		        ON C.CompanyID = D.CompanyID)
		INNER JOIN (SELECT RoleID,
				   DivisionID,
				   PersonID,
				   DivisionName,
				   CompanyName
			    FROM   tblRole) AS R
			ON D.DivisionID = R.DivisionID)
		INNER JOIN (SELECT PersonID,
				   [First],
				   [Last]
			    FROM   tblPerson) AS P
		        ON R.PersonID = P.PersonID) AS People
        INNER JOIN (SELECT *
                    FROM   ((SELECT ProductLinkTypeID AS LinkTypeID,
                                    Nom               AS ContactType
                            FROM   tblProductLinkType) AS LT
			    INNER JOIN (SELECT ProductID,
					       RoleID,
					       LinkTypeID
				        FROM   tblProductRole) AS PR
			            ON LT.LinkTypeID = PR.LinkTypeID)) AS Roles
                ON People.RoleID = Roles.RoleID)
INNER JOIN (SELECT PersonID,
		   Email
	    FROM   tblEmail
	    WHERE  Default <> 0) AS E
        ON People.PersonID = E.PersonID
WHERE  ( ProductID = "Product1" )
        OR ( ProductID = "Product2" )
        OR ( ProductID = "Product3" )
        OR ( ProductID = "Product4" )
        OR ( ProductID = "Product5" )
        OR ( ProductID = "Product6" )
        OR ( ProductID = "Product7" )
        OR ( ProductID = "Product8" ); 
```

2. From AllContacts, query all unique pairs of company and product IDs, and call it CM_ID. This will create a primary key index, stored in table "CompanyMembership"
```sql
CREATE TABLE CompanyMembership
  (
     CM_ID       AUTOINCREMENT PRIMARY KEY,
     ProductID   Text,
     CompanyName Text
  ); 
```
```sql
INSERT INTO CompanyMembership
            (ProductID, 
	     CompanyName)
SELECT DISTINCT AC.ProductID,
                AC.CompanyName
FROM   AllContacts AS AC; 
```

3. Create a table with each Key Contact and Key Contact CC listing in "AllContacts", using CM_ID as a foreign key (stored in tables "KeyContactMembership" and "KeyContactCCMembership"). We need to include Key Contact CCs as there are some entries for which there is no Key Contact, but there is/are Key Contact CC(s).
```sql
SELECT AC.First,
       AC.Last,
       AC.FullName,
       AC.Email,
       CM.CM_ID
INTO   KeyContactMembership
FROM   AllContacts AS AC
       LEFT JOIN CompanyMembership AS CM
       ON ( AC.ProductID = CM.ProductID )
            AND ( AC.CompanyName = CM.CompanyName )
WHERE  AC.ContactType = "KeyContact"; 
```
```sql
SELECT AC.First,
       AC.Last,
       AC.FullName,
       AC.Email,
       CM.CM_ID
INTO   KeyContactCCMembership
FROM   AllContacts AS AC
       LEFT JOIN CompanyMembership AS CM
              ON ( AC.ProductID = CM.ProductID )
                 AND ( AC.CompanyName = CM.CompanyName )
WHERE  AC.ContactType = "KeyContactCC"; 
```

4. Repeat step 3 but for Data Contacts, Registered Users, and attached Key Contact CCs (stored in "NonKeyContactMembership")
```sql
CREATE TABLE NonKeyContactMembership
  (
     NKCM_ID     AUTOINCREMENT PRIMARY KEY,
     FullName    Text,
     Email       Text,
     ContactType Text,
     CM_ID       Number
  ); 
```
```sql
INSERT INTO NonKeyContactMembership
            (FullName,
             Email,
             ContactType,
             CM_ID)
SELECT AC.FullName,
       AC.Email,
       AC.ContactType,
       CM.CM_ID
FROM   AllContacts AS AC
       LEFT JOIN CompanyMembership AS CM
              ON ( AC.ProductID = CM.ProductID )
                 AND ( AC.CompanyName = CM.CompanyName )
WHERE  (( ( AC.ContactType ) = "DataContact" )); 
```
```sql
INSERT INTO NonKeyContactMembership
            (FullName,
             Email,
             ContactType,
             CM_ID)
SELECT AC.FullName,
       AC.Email,
       AC.ContactType,
       CM.CM_ID
FROM   AllContacts AS AC
       LEFT JOIN CompanyMembership AS CM
              ON ( AC.CompanyName = CM.CompanyName )
                 AND ( AC.ProductID = CM.ProductID )
WHERE  (( ( AC.ContactType ) = "RegUser" )); 
```
```sql
INSERT INTO NonKeyContactMembership
            (FullName,
             Email,
             ContactType,
             CM_ID)
SELECT AC.FullName,
       AC.Email,
       AC.ContactType,
       CM.CM_ID
FROM   AllContacts AS AC
       LEFT JOIN CompanyMembership AS CM
              ON ( AC.CompanyName = CM.CompanyName )
                 AND ( AC.ProductID = CM.ProductID )
WHERE  (( ( AC.ContactType ) = "KeyContactCC" )); 
```

5. Left merge "NonKeyContactMembership" with "KeyContactMembership" (stored in "FinalKC_WN"). This will contain nulls for the people without Key Contacts. The nulls are saved for review before removal.
```sql
SELECT KCM.First        AS KeyContactFirstName,
       KCM.Last         AS KeyContactLastName,
       KCM.FullName     AS KeyContactFullName,
       KCM.Email        AS KeyContactEmail,
       NKCM.FullName    AS ContactName,
       NKCM.Email       AS ContactEmail,
       NKCM.ContactType AS ContactType,
       NKCM.CM_ID
INTO   FinalKC_WN
FROM   NonKeyContactMembership AS NKCM
       LEFT JOIN KeyContactMembership AS KCM
              ON KCM.CM_ID = NKCM.CM_ID
ORDER  BY KCM.FullName; 
```

6. Remove nulls from "FinalKC_WN" (stored in "FinalKC"), and replace product codes with full product names.
```sql
SELECT FinalKC_WN.KeyContactFirstName,
       FinalKC_WN.KeyContactLastName,
       FinalKC_WN.KeyContactFullName,
       FinalKC_WN.KeyContactEmail,
       CM.CompanyName,
       Switch(CM.ProductID = "Product1", "Product1 Name", 
       	      CM.ProductID = "Product2", "Product2 Name",
	      CM.ProductID = "Product3", "Product3 Name",
	      CM.ProductID = "Product4", "Product4 Name",
	      CM.ProductID = "Product5", "Product5 Name",
	      CM.ProductID = "Product6", "Product6 Name",
	      CM.ProductID = "Product7", "Product7 Name",
	      CM.ProductID = "Product8", "Product8 Name"
	      )                            
	      AS Product
       Switch(FinalKC_WN.ContactType = "DataContact", "Data Contact",
       	      FinalKC_WN.ContactType = "RegUser", "Registered User"
	      ) 
	      AS [Contact Type],
       FinalKC_WN.ContactName                                 
       	      AS [Contact Name],
       FinalKC_WN.ContactEmail
       	      AS [Contact Email]
INTO   FinalKC
FROM   FinalKC_WN
       LEFT JOIN CompanyMembership AS CM
              ON CM.CM_ID = FinalKC_WN.CM_ID
WHERE  FinalKC_WN.KeyContactFullName is not null; 
```

7. From reviewing the null rows in step 5, there were some product-company pairs that had two Key Contact Ccs and zero Key Contacts. The two KCCCs are treated as a single key contact, and their lists are stored in a separate table.

   Left merge the null rows in "FinalKC_WN" with "KeyContactCCMembership" (stored in "FinalKCCC_WN"). Again, the table is saved with nulls for review. 
```sql
SELECT DISTINCT KCCCM.First    AS KeyContactFirstName,
                KCCCM.Last     AS KeyContactLastName,
                KCCCM.FullName AS KeyContactFullName,
                KCCCM.Email    AS KeyContactEmail,
                B.ContactType,
                B.ContactName,
                B.ContactEmail,
                B.CM_ID
INTO   FinalKCCC_WN
FROM   (SELECT *
        FROM   FinalKC_WN
        WHERE  FinalKC_WN.KeyContactFullName IS NULL) AS B
       LEFT JOIN KeyContactCCMembership AS KCCCM
              ON B.CM_ID = KCCCM.CM_ID
ORDER  BY KCCCM.FullName; 
```

8. The nulls from the previous step were found to be extraneous. Remove nulls from "FinalKCCC_WN" and store in "FinalKCCC".
```sql
SELECT FinalKCCC_WN.KeyContactFirstName,
       FinalKCCC_WN.KeyContactLastName,
       FinalKCCC_WN.KeyContactFullName,
       FinalKCCC_WN.KeyContactEmail,
       CM.CompanyName,
       CM.ProductID              AS Product,
       FinalKCCC_WN.ContactType  AS [Contact Type],
       FinalKCCC_WN.ContactName  AS [Contact Name],
       FinalKCCC_WN.ContactEmail AS [Contact Email]
INTO   FinalKCCC
FROM   FinalKCCC_WN
       LEFT JOIN CompanyMembership AS CM
              ON CM.CM_ID = FinalKCCC_WN.CM_ID
WHERE  FinalKCCC_WN.KeyContactFullName is not null; 
```

## Result
The result is a table called FinalKC that looks like this (note the key contact first name, last name, and full name columns have been combined for display purposes):

| Key Contact Name | Key Contact Email | Company Name | Product   | Contact Type    | Contact Name | Contact Email  |
|------------------|-------------------|--------------|-----------|-----------------|--------------|----------------|
| KC1              | KC1-email         | Company A    | Product X | Data Contact    | Contact1     | contact1-email |
| KC1              | KC1-email         | Company A    | Product X | Data Contact    | Contact2     | contact2-email |
| KC1              | KC1-email         | Company A    | Product Y | Registered User | Contact3     | contact4-email |
| KC2              | KC2-email         | Company A    | Product Z | Key Contact Cc  | Contact4     | contact4-email |
| KC2              | KC2-email         | Company A    | Product Z | Data Contact    | Contact5     | contact5-email |

KC1 will receive an email with a table containing all the rows with the name KC1, i.e. rows 1 to 3. KC2 will receive an email with a table containing rows 4 to 5.

The emails were produced in MS Word using a table that queried all unique key contacts from FinalKC. Using the list key contacts, MS Word generated an email for each key contact (a.k.a a mail merge) that queried the database for the relevant rows. This was done using the field code below.
```
{ DATABASE  
\d URL_TO_DATABASE;
	Mode=Read;
	Extended Properties=\"\";
	Jet OLEDB:System database=\"\";
	Jet OLEDB:Registry Path=\"\";
	Jet OLEDB:Engine Type=6;
	Jet OLEDB:Database Locking Mode=1;
	Jet OLEDB:Global Partial Bulk Ops=2;
	Jet OLEDB:Global Bulk Transactions=1;
	Jet OLEDB:New Database Password=\"\";
	Jet OLEDB:Create System Database=False;
	Jet OLEDB:Encrypt Database=False;
	Jet OLEDB:Don't Copy Locale on Compact=False;
	Jet OLEDB:Compact Without Replica Repair=False;
	Jet OLEDB:SFP=False;
	Jet OLEDB:Support Complex Data=False;
	Jet OLEDB:Bypass UserInfo Validation=False;
	Jet OLEDB:Limited DB Caching=False;
	Jet OLEDB:Bypass ChoiceField Validation=False" 
\s "SELECT `Product`, `Contact Type`, `Contact Name`, `Contact Email` 
      FROM `FinalKC` 
      WHERE ((`KeyContactFullName` = '{ MERGEFIELD “KeyContactFullName” }'))"
\h 
\* MERGEFORMAT 
}
```
