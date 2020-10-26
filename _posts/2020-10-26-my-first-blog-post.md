---
layout: post
title: "SQL Project: Verifying contact information in the client database"
date: 2020-10-26
---

# Background
The company produces market research reports on a variety of topics; each report is referred to as a "product". For each product, the company works with multiple people from each client company. Contact information for all of these people are saving in the client database, along with their assigned role. Roles are given to contacts at each company, for each product. The roles are:

* Key Contact (KC). The KC is the "owner" of the product and signs the participation agreement. Each product has only one KC at each participating company. 
* Key Contact Cc (KCCC). The KCCCs are cc'd on all e-mails to the KC.
* Data Contact (DC). The DCs are contacted for matters related to data collection.
* Registered User (RU). RUs are people who make significant use of the product.

The goal of this project is to send each KC a list of KCCCs, DCs, and RUs at their company for each product they are involved in. This task requires multiple merge queries on the database. The queries will select from the following tables:

** insert image of tables **

# Methodology
The database and queries are stored and run in MS Access.

## 1. Merge tables to collect all relevant data columns
```sql
SELECT P_R_C_D_PR_LT.ProductID, P_R_C_D_PR_LT.ContactType, P_R_C_D_PR_LT.First, P_R_C_D_PR_LT.Last, P_R_C_D_PR_LT.First & Space(1) & P_R_C_D_PR_LT.Last AS FullName, P_R_C_D_PR_LT.CompanyName, P_R_C_D_PR_LT.DivisionName, E.Email INTO AllContacts
FROM (SELECT
	P_R_C_D.PersonID,
	P_R_C_D.First,
	P_R_C_D.Last,
	P_R_C_D.RoleID,
	P_R_C_D.DivisionName,
	P_R_C_D.CompanyName,
	PR_LT.ContactType,
	PR_LT.ProductID
	FROM
		(SELECT
			P.PersonID,
			P.First,
			P.Last,
			R_C_D.RoleID,
			R_C_D.DivisionName,
			R_C_D.CompanyName
		FROM
		tblPerson as P
			INNER JOIN
				(SELECT 
					R.RoleID, 
					R.DivisionID, 
					R.PersonID,
					C_D.DivisionName,
					C_D.CompanyName
				FROM 
					tblRole as R
				INNER JOIN
					(SELECT 
						D.DivisionID, 
						D.Nom AS DivisionName, 
						C.CompanyID, 
						C.CompanyName
					FROM
						tblDivision as D
					INNER JOIN
						(SELECT
							CompanyID, 
							Nom AS CompanyName
						FROM
							tblCompany
						) AS C
					ON C.CompanyID=D.CompanyID
					) AS C_D
				ON C_D.DivisionID = R.DivisionID
				) AS R_C_D
			ON R_C_D.PersonID = P.PersonID
		) AS P_R_C_D
	INNER JOIN
		(SELECT
			PR.ProductID,
			PR.RoleID,
			LT.ContactType
		FROM
		tblProductRole as PR
		INNER JOIN
			(SELECT 
				tblProductLinkType.ProductLinkTypeID AS LinkTypeID,
				tblProductLinkType.Nom AS ContactType
			FROM
				tblProductLinkType
			) AS LT
		ON PR.LinkTypeID = LT.LinkTypeID
		) AS PR_LT
	ON PR_LT.RoleID = P_R_C_D.RoleID
	)  AS P_R_C_D_PR_LT INNER JOIN (SELECT
		tblEmail.PersonID,
		tblEmail.Email
	FROM
		tblEmail 
	WHERE
		tblEmail.Default <> 0
	)  AS E ON P_R_C_D_PR_LT.PersonID = E.PersonID
WHERE (ProductID = 'CDB')
OR (ProductID = 'CSA')
OR (ProductID = 'CSP')
OR (ProductID = 'GUR')
OR (ProductID = 'IHM')
OR (ProductID = 'MVT')
OR (ProductID = 'PUR')
OR (ProductID = 'SPS');

```
