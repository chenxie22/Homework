
/*****************************************************
Problem Set 4, Question 3  
Stats 506, Fall 2018
Author: Chen Xie chenxie@umich.edu
 *****************************************************
*/


/*** Part a ***/


/* Import the data: */
DATA Medicare_PS_PUF;
	LENGTH
		npi              					$ 10
		nppes_provider_last_org_name 		$ 70
		nppes_provider_first_name 			$ 20
		nppes_provider_mi					$ 1
		nppes_credentials 					$ 20
		nppes_provider_gender				$ 1
		nppes_entity_code 					$ 1
		nppes_provider_street1 				$ 55
		nppes_provider_street2				$ 55
		nppes_provider_city 				$ 40
		nppes_provider_zip 					$ 20
		nppes_provider_state				$ 2
		nppes_provider_country				$ 2
		provider_type 						$ 55
		medicare_participation_indicator 	$ 1
		place_of_service					$ 1
		hcpcs_code       					$ 5
		hcpcs_description 					$ 256
		hcpcs_drug_indicator				$ 1
		line_srvc_cnt      					8
		bene_unique_cnt    					8
		bene_day_srvc_cnt   				8
		average_Medicare_allowed_amt   		8
		average_submitted_chrg_amt  		8
		average_Medicare_payment_amt   		8
		average_Medicare_standard_amt		8;
	INFILE '~\data\Medicare_Provider_Util_Payment_PUF_CY2016.txt'

		lrecl=32767
		dlm='09'x
		pad missover
		firstobs = 3
		dsd;

	INPUT
		npi             
		nppes_provider_last_org_name 
		nppes_provider_first_name 
		nppes_provider_mi 
		nppes_credentials 
		nppes_provider_gender 
		nppes_entity_code 
		nppes_provider_street1 
		nppes_provider_street2 
		nppes_provider_city 
		nppes_provider_zip 
		nppes_provider_state 
		nppes_provider_country 
		provider_type 
		medicare_participation_indicator 
		place_of_service 
		hcpcs_code       
		hcpcs_description 
		hcpcs_drug_indicator
		line_srvc_cnt    
		bene_unique_cnt  
		bene_day_srvc_cnt 
		average_Medicare_allowed_amt 
		average_submitted_chrg_amt 
		average_Medicare_payment_amt
		average_Medicare_standard_amt;

	LABEL
		npi     							= "National Provider Identifier"       
		nppes_provider_last_org_name 		= "Last Name/Organization Name of the Provider"
		nppes_provider_first_name 			= "First Name of the Provider"
		nppes_provider_mi					= "Middle Initial of the Provider"
		nppes_credentials 					= "Credentials of the Provider"
		nppes_provider_gender 				= "Gender of the Provider"
		nppes_entity_code 					= "Entity Type of the Provider"
		nppes_provider_street1 				= "Street Address 1 of the Provider"
		nppes_provider_street2 				= "Street Address 2 of the Provider"
		nppes_provider_city 				= "City of the Provider"
		nppes_provider_zip 					= "Zip Code of the Provider"
		nppes_provider_state 				= "State Code of the Provider"
		nppes_provider_country 				= "Country Code of the Provider"
		provider_type	 					= "Provider Type of the Provider"
		medicare_participation_indicator 	= "Medicare Participation Indicator"
		place_of_service 					= "Place of Service"
		hcpcs_code       					= "HCPCS Code"
		hcpcs_description 					= "HCPCS Description"
		hcpcs_drug_indicator				= "Identifies HCPCS As Drug Included in the ASP Drug List"
		line_srvc_cnt    					= "Number of Services"
		bene_unique_cnt  					= "Number of Medicare Beneficiaries"
		bene_day_srvc_cnt 					= "Number of Distinct Medicare Beneficiary/Per Day Services"
		average_Medicare_allowed_amt 		= "Average Medicare Allowed Amount"
		average_submitted_chrg_amt 			= "Average Submitted Charge Amount"
		average_Medicare_payment_amt 		= "Average Medicare Payment Amount"
		average_Medicare_standard_amt		= "Average Medicare Standardized Payment Amount";
RUN;

/*** Part b ***/

/* Subsetting: */
data reduce_b;
set Medicare_PS_PUF;
where hcpcs_description like '%MRI%' and hcpcs_code like '7%' ;
run;


/*** Part c ***/

/* Prepare for computing: */
data reduce_c;
 set reduce_b;
 payment=line_srvc_cnt*average_Medicare_payment_amt;
 keep hcpcs_description  line_srvc_cnt payment ;

 /* Sort: */
 proc sort data=reduce_c; 
 by hcpcs_description;

/* Compute sum of volumn, payment: */
proc summary data=reduce_c;
 by hcpcs_description;
 output out=totals
        sum(line_srvc_cnt) = volumn
        sum(payment) = total_payment;

/* Compute average payment: */
data q3c;
set totals;
average_payment=total_payment/volumn;
keep hcpcs_description volumn total_payment average_payment;

/* Get the highest values */
proc  sort data=q3c;
by DESCENDING volumn;

data q3c_volumn;
set q3c(obs=1);

proc  sort data=q3c;
by DESCENDING total_payment;

data q3c_total;
set q3c(obs=1);

proc  sort data=q3c;
by DESCENDING average_payment;

data q3c_average;
set q3c(obs=1);

/* Merge the highest values: */
data q3c_max;
merge q3c_volumn q3c_total q3c_average;
by  hcpcs_description;
run;


/*** Part d ***/
proc sql;

  /* Compute sum of volumn, payment, and average payment: */
  create table q3d as
    select hcpcs_description, hcpcs_code,
		   sum(line_srvc_cnt) as volumn, 
           sum(line_srvc_cnt*average_Medicare_payment_amt) as total_payment,
           sum(line_srvc_cnt*average_Medicare_payment_amt)/sum(line_srvc_cnt) 
           as average_payment
      from Medicare_PS_PUF
	  /*Subsetting */
	  where hcpcs_description like '%MRI%' and hcpcs_code like '7%'
      group by hcpcs_description,hcpcs_code;
 
 /* Determine the highest values: */
  create table q3d_max as
  select *
  from q3d
  having volumn = max(volumn) or 
         total_payment = max(total_payment) or 
         average_payment = max(average_payment) ;

  quit;
 
  
/*** Part e ***/


/* Export to csv: */
proc export data=q3c_max
  outfile = '~\ps4_q3c.csv'
  dbms=dlm replace; 
  delimiter  = ",";

proc export data=q3d_max
  outfile = '~\ps4_q3d.csv'
  dbms=dlm replace; 
  delimiter  = ",";

run;

