* ---------------------------------------------------------------------------- *
* Stats 506, F18, Problem Set 2, Question 1
*
* Author: Chen Xie, chenxie@umich.edu	          
*  
* Data:
*  	 imported from the address below:
* 
* https://www.eia.gov/consumption/residential/data/2015/csv/
* 	recs2015_public_v3.csv     	 
* ---------------------------------------------------------------------------- *

*--------------------- *
* Load and clean data *
*--------------------- *
clear
import delimited ///
https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv
save recs15.dta, replace
use recs15.dta,clear

// Keep only needed variables
keep nweight brrwt1-brrwt96 kwh cufeetng gallonlp gallonfo

*----------------------- *
* Prepare for computing *
*----------------------- *
// Generate the final weight as brrwt0
generate brrwt0=nweight

// Reshape to long using replicate weights,
*including the brrwt0 (the final weight),
*using "rw" to denote the nth replicate weight.
reshape long brrwt, i(nweight kwh cufeetng gallonlp gallonfo) j(rw)

// Generate the weighted usages
generate elec=kwh*brrwt
generate gas=cufeetng*brrwt
generate lp=gallonlp*brrwt
generate oil=gallonfo*brrwt

*------------------------ *
* Caculate the estimates *
*------------------------ *
// Estimate the total usage for every weight
preserve
collapse (sum) elec gas lp oil, by(rw)
save recs15_usage.dta, replace
restore

*----------------- *
* Caculate the se *
*----------------- *
// Load the summarized data
use recs15_usage.dta, clear

// Generate variables for computing se
generate elec_se =(elec-elec[1])^2
generate gas_se =(gas-gas[1])^2
generate lp_se =(lp-lp[1])^2
generate oil_se =(oil-oil[1])^2

// Incorporate replicate weights into one group
* prepare for computing se
replace rw = 1 if rw!=0

// Sum the squared differences
collapse (mean) elec gas lp oil /// 
(sum) elec_se gas_se lp_se oil_se, by(rw)

// Produce better format manually
replace elec= elec_se[2] if rw==1
replace gas = gas_se[2] if rw==1
replace lp = lp_se[2] if rw==1
replace oil = oil_se[2] if rw==1
keep rw elec gas lp oil

// Compute se
replace elec=sqrt(elec/24) if rw==1
replace gas=sqrt(gas/24) if rw==1
replace lp=sqrt(lp/24) if rw==1
replace oil=sqrt(oil/24) if rw==1

// Label the table 
rename rw stat
label define stat_type 0 "Estimates" 1 "Standard Errors"
label values stat stat_type 

// Export recs2015_usage.csv
export delimited recs2015_usage.csv, replace
