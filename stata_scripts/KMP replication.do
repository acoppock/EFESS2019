*****************************************************************************************************************
* EUROPEAN FIELD EXPERIMENTS SUMMER SCHOOL 2018
* Eline A. de Rooij, Alexander Coppock, Florian Foos
*****************************************************************************************************************
* DAY 3, SESSION 2
*****************************************************************************************************************

* The following analyses were carried out using Stata/SE 14.2 for Windows (64-bit x86-64) 

clear

* Uncomment this line to set the correct working directory:
*cd "~\Stata labs" 

* Open .dta data (from folder set as working directory):
use "data\example_KMP_data.dta", clear

* Original Specification --------------------------------------------------

tabulate prop_sd_fem2014
recode prop_sd_fem2014 (0=0) (.10/1=1) (.=.), generate(sd_onefem2014)
egen outcome_missing = rmiss(prop_sd_fem2014)
egen covariates_missing = rmiss(sd2014 age2014 attendees2014 prop_fem_attend2014 distance_100)
egen all_missing = rmiss(prop_sd_fem2014 sd2014 age2014 attendees2014 prop_fem_attend2014 distance_100)
keep if condition!=.

codebook outcome_missing covariates_missing all_missing

* These results match table 2 exactly
regress sd_onefem2014 i.condition if outcome_missing==0, cluster(county)
regress sd_onefem2014 i.condition sd2014 age2014 attendees2014 prop_fem_attend2014 distance_100 if all_missing==0, cluster(county)
probit sd_onefem2014 i.condition if outcome_missing==0, cluster(county)
probit sd_onefem2014 i.condition sd2014 age2014 attendees2014 prop_fem_attend2014 distance_100 if all_missing==0, cluster(county)
* Note the use of clustered ses  

* These results match table 3 exactly
regress prop_sd_fem2014 i.condition if outcome_missing==0, cluster(county)
regress prop_sd_fem2014 i.condition sd2014 age2014 attendees2014 prop_fem_attend2014 distance_100 if all_missing==0, cluster(county)
glm prop_sd_fem2014 i.condition if outcome_missing==0, family(binomial) link(logit) cluster(county)
glm prop_sd_fem2014 i.condition sd2014 age2014 attendees2014 prop_fem_attend2014 distance_100 if all_missing==0, family(binomial) link(logit) cluster(county)
* Note the use of clustered ses  


* Better! -----------------------------------------------------------------

* I would change 4 things about the above analysis.
* 1. Mean-impute missing covariates (why?)
* 2. Condition on compliance (i.e., the letter arriving.  Q: WHY IS THIS OK TO DO HERE?)
* 3. No need for cluster-robust standard errors (why?)
* 4. Basically no need to do probit or fractional logit.

* Also, I would directly test the interaction term.

* Mean-impute for missing covariates

generate sd2014_nona = sd2014
summarize sd2014, meanonly
scalar mu_sd2014=r(mean)
replace sd2014_nona=mu_sd2014 if sd2014_nona==.

generate age2014_nona = age2014
summarize age2014, meanonly
scalar mu_age2014=r(mean)
replace age2014_nona=mu_age2014 if age2014_nona==.

generate attendees2014_nona = attendees2014
summarize attendees2014, meanonly
scalar mu_attendees2014=r(mean)
replace attendees2014_nona=mu_attendees2014 if attendees2014_nona==.

generate prop_fem_attend2014_nona = prop_fem_attend2014
summarize prop_fem_attend2014, meanonly
scalar mu_prop_fem_attend2014=r(mean)
replace prop_fem_attend2014_nona=mu_prop_fem_attend2014 if prop_fem_attend2014_nona==.

generate distance_100_nona = distance_100
summarize distance_100, meanonly
scalar mu_distance_100=r(mean)
replace distance_100_nona=mu_distance_100 if distance_100_nona==.

* Condition on the letter arriving
tabulate letter
keep if letter==1
* 1842 observations, all complete - 154 MISSING ON OUTCOME, BUT THEY GET DROPPED IN ANALYSIS ANYWAY

regress sd_onefem2014 i.condition, vce(hc2)
regress sd_onefem2014 i.condition sd2014_nona age2014_nona attendees2014_nona prop_fem_attend2014_nona distance_100_nona, vce(hc2)
regress prop_sd_fem2014 i.condition, vce(hc2)
regress prop_sd_fem2014 i.condition sd2014_nona age2014_nona attendees2014_nona prop_fem_attend2014_nona distance_100_nona, vce(hc2)

* Test the interaction!
codebook condition
recode condition (1 3=0) (2 4=1) (.=.), generate(supply) 
recode condition (1 2=0) (3 4=1) (.=.), generate(demand) 

regress sd_onefem2014 supply##demand, vce(hc2)
regress prop_sd_fem2014 supply##demand, vce(hc2)

* What do we conclude?































