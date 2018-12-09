* ---------------------------------------------------------------------------- *
* Stats 506, F18, Problem Set 2, Question 2
*
* Author: Chen Xie, chenxie@umich.edu	          
*  
* Data:
*  	 imported from the address below:
* 
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT  
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT  	 
* ---------------------------------------------------------------------------- *

*------- *
* Set up *
*------  *		
log using ps2_q2.log, text replace 
clear

*------------------------------------------------*
* Part a, Import and merge the data sets by SEQN *
*------------------------------------------------*

// Import and save OHX_D.XPT
import sasxport https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OHX_D.XPT
save OHX.dta, replace

// Import and save DEMO_D.XPT and merge
import sasxport https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT
save DEMO.dta, replace
use DEMO.dta, clear
merge 1:1 seqn using OHX.dta

// Reduce to matched data
keep if _merge==3
save OH_DEMO_merge.dta, replace
use OH_DEMO_merge.dta, clear

// Reduce to the variables of interest
// Rename variables 
keep ohx04htc ridagemn riagendr ridreth1 ///
indfmpir wtmec2yr sdmvstra sdmvpsu
rename (ohx04htc ridagemn riagendr ridreth1 indfmpir) ///
(tooth age gender race pir)

**************************************************
*------------------------------------------------*
* Part b, Logistic Regression between Age and    *
*         the Probability of losing the Primary  *
*         upper right 2nd bicuspid               *
*------------------------------------------------*

// Collapse 'Permanent' group
recode tooth 2 5=2

// Decode missing values
mvdecode tooth, mv(9=.)

// Drop missing values
drop if missing(tooth)

// Create the response variable
generate tth = (tooth!=1)

// Decode variables
label define tth_codes ///
 1 "Primary" 2 "Permanent" 4 "Missing", replace  
label values tooth tth_codes 

// Label variables
label variable tth "Probability of losing primary" 
label variable age "Age in month" 
label variable race "Race/Ethnicity" 
label variable pir "Poverty Income Ratio" 

// Save for repeated use
save tth.dta, replace

// Drop and Save for regression
drop if missing(age)
save age.dta, replace

// Logistic regression between
// Age and Probability of losing primary tooth
logit tth age

// Display BIC of this model
estat ic

// Estimate ages
matrix coef=e(b)
svmat coef
local est_1 round((log(0.25/(1-0.25))-coef2)/coef1)
local est_2 round((log(0.5/(1-0.5))-coef2)/coef1)
local est_3 round((log(0.75/(1-0.75))-coef2)/coef1)

// The Estimated age at 25% losing Primary tooth
display `est_1'

// The Estimated age at 50% losing Primary tooth
display `est_2'

// The Estimated age at 75% losing Primary tooth
display `est_3'

// Compute the min and max of representative ages
// and display
local start floor(`est_1'/12)
local end ceil(`est_3'/12)

// The minimum of representative ages
display `start'

// The maximum of representative ages
display `end'

// The range of representative ages
numlist "8/12"
local age_range "`r(numlist)'"
display "`age_range'"


**************************************************
*------------------------------------------------*
* Part c, Model Selection by BIC                 *
*------------------------------------------------*

// Condider Gender

// Decode variables
label define gd_codes 1 "Male" 2 "Female", replace
label values gender gd_codes

// Logistic regression: add Gender
logit tth age i.gender

// Display BIC, drop Gender
estat ic


// Condider Race

// Collape groups
recode race 2 5=2

// Create indicator variables
// Using White as the reference
generate mxc = (race==1) 
generate blk = (race==4)
generate oth = (race==2)

// Decode variables
label define rc_codes 1 "Mexican" 2 ///
 "Other" 3 "White" 4 "Black", replace
label values race rc_codes

// Logistic regression: add Black
logit tth age i.blk

// Display BIC, retain Black
estat ic

// Logistic regression: add Mexican
logit tth age i.blk i.mxc

// Display BIC, drop Mexican
estat ic

// Logistic regression: add Other
logit tth age i.blk i.oth

// Display BIC, drop Other
estat ic


// Consider Poverty Income Ratio

// Drop missing value
drop if missing(pir)

// Save for repeated use
save pir.dta, replace

// Logistic regression: add pir
logit tth age i.blk i.oth pir

// Display BIC, retain pir
estat ic

// Label variable
label variable blk "Black indicator"

// The final regression model
logit tth age i.blk pir

**************************************************
*------------------------------------------------*
* Part d, Compute adjusted predictions at means, *
*         marginal effects at the mean, and      *
*         average marginal effects.              *
*------------------------------------------------*

// Adjusted predictions at the mean 
// At each of the representative age
margins, at(age=(96 108 120 132 144)) atmeans 

// Marginal effiects at the mean
// of Black at each representative age
margins, dydx(blk) at(age=(96 108 120 132 144)) atmeans 

// Average marginal effect 
// of Black at each representative age
margins, dydx(blk) at(age=(96 108 120 132 144))

**************************************************
*------------------------------------------------*
* Part e, Refit as if the data were a survey     *
*         design and compare                     *
*------------------------------------------------*

// Set up
svyset sdmvpsu [pweight=wtmec2yr], strata(sdmvstra) vce(linearized)

// Refit the model
svy: logit tth age i.blk pir 


log close
exit
