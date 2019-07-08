*****************************************************************************************************************
* EUROPEAN FIELD EXPERIMENTS SUMMER SCHOOL 2018
* Eline A. de Rooij, Alexander Coppock, Florian Foos
*****************************************************************************************************************
* LAB 2
*****************************************************************************************************************

* The following analyses were carried out using Stata/SE 14.2 for Windows (64-bit x86-64) 

clear

* Uncomment this line to set the correct working directory:
*cd "~\Stata labs" 

* Load the csv data (from folder set as working directory):
import delimited "data/lab_2.csv"

** Note: for Stata versions prior to Stata 13, use the command "insheet using"

* Difference-in-means estimate:
summarize y if z==1, meanonly
scalar mu_y_z1 = r(mean)
summarize y if z==0, meanonly
scalar mu_y_z0 = r(mean)
scalar ate_hat_dim = mu_y_z1 - mu_y_z0
scalar list 
** alternatively (although difference here is between control and treatment):
ttest y, by(z)

* Same as the unadjusted OLS estimate:
regress y z

* Estimated standard error (equation 3.6)
summarize y if z==1, detail
scalar var_yz1=r(Var)
summarize y if z==0, detail
scalar var_yz0=r(Var)

summarize z
scalar m = r(sum)
scalar N = _N

scalar est_SE=sqrt((var_yz0/(N-m)) + (var_yz1/m))
scalar list est_SE				

* Exactly the same as HC2 robust standard errors:
regress y z, vce(hc2)

* Note that these SEs are (a little) different from classical SEs:
regress y z

* If you're doing an experiment (and there's no clustering) HC2 standard errors are almost always the way to go.


* Randomization Inference -------------------------------------------------

* Step 1: Make Hypothesized Outcomes under sharp null hypothesis
generate y0_star=y
generate y1_star=y

* Exactly zero treatment effect for every unit
generate notreat= y1_star - y0_star
summarize notreat

* Step 2: set up loop
set obs `=_N+1500' 
generate j=_n
generate random=.
generate k=.
generate z_sim=.
generate y_sim=.
generate mu_1=.
generate mu_0=.
generate sim_ates= .

set more off 
forvalues i=1/2000{
replace random=runiform() if j<=500
sort random
replace k=_n if j<=500
replace z_sim=1 if k<=200 & k!=.
replace z_sim=0 if k>200 & k!=.
replace y_sim=y0_star*(1-z_sim)+y1_star*(z_sim)
summarize y_sim if z_sim==1, meanonly
replace mu_1=r(mean) 
summarize y_sim if z_sim==0, meanonly
replace mu_0=r(mean)
replace sim_ates = mu_1 - mu_0 if `i'==j
}

* Step 3: take a look at the null distribution and compare it to the observed value
histogram sim_ates, bin(12) frequency fcolor(none) lcolor(black) xlabel(-6(2)6) title(Histogram of simulated ates)
scalar N_new = _N
count if abs(sim_ates)>=ate_hat_dim
scalar p_value_ri=r(N)/N_new
scalar list

* compare to 
regress y z, vce(hc2)

* Alternative step 2 and 3: set up loop using ri2
drop if k==.
drop j-sim_ates

local sims = 2000
set matsize 2000
matrix sim_ates_2 = J(`sims', 1, .)

forvalues i=1/`sims'{

* Conduct complete random assignment
qui complete_ra Z_ri, m(200) replace

* Switching Equation
qui tempvar Y
qui gen `Y' = y1_star*(Z_ri) + y0_star*(1-Z_ri)

* Estimate difference in means
qui ttest `Y', by(Z_ri) unequal
qui matrix sim_ates_2[`i',1]= r(mu_2) - r(mu_1)

}

preserve
qui svmat sim_ates_2

mean sim_ates_2
egen SD=sd(sim_ates_2)
summarize SD, meanonly
scalar sc_SD = r(mean)
scalar list sc_SD

* Step 3: take a look at the null distribution and compare it to the observed value
histogram sim_ates_2, bin(12) frequency fcolor(none) lcolor(black) xlabel(-6(2)6) title(Histogram of simulated ates)
scalar N_new_2 = _N
count if abs(sim_ates_2)>=ate_hat_dim
scalar p_value_ri_2=r(N)/N_new_2
scalar list

* compare to 
regress y z, vce(hc2)


* Comparing Blocking to Covariate Adjustment ------------------------------

* Plot the outcome against the pre score, coloring by treatment assignment
twoway (scatter y pre_score if z==1, mcolor(ltblue)) (scatter y pre_score if z==0, mcolor(navy)), ylabel(0(25)100) xlabel(0(30)120) legend(order(1 "z=1" 2 "z=0") position(3))

* Goal: simulate the TRUE sampling distribution when
** A: the treatment is blocked by pre_score quantile and we use DIM
** B: we use complete random assignment and just control for pre_score quantile

xtile pre_score_quantile=pre_score, nq(5)

* Check the variable we made
codebook pre_score_quantile
set more off 
tabulate pre_score pre_score_quantile
label define ps_quant_lbl 1 "-6/31" 2 "32/43" 3 "44/54" 4 "55/66" 5 "67/111"
label values pre_score_quantile ps_quant_lbl 

* Check out the block random assignment procedure
block_ra Z_blocked, blocks(pre_score_quantile)  
tab pre_score_quantile Z_blocked

* SIMULATION A: block the treatment, use difference-in-means 

local sims = 1000
set matsize 1000
matrix estimates_A = J(`sims', 1, .)

forvalues i=1/`sims'{

* Conduct blocked random assignment
qui block_ra Z_sim, blocks(pre_score_quantile) replace
* Switching Equation
qui tempvar Y
qui gen `Y' = y_z_1*(Z_sim) + y_z_0*(1-Z_sim)
* Estimate difference in means
qui ttest `Y', by(Z_sim) unequal
qui matrix estimates_A[`i',1]= r(mu_2) - r(mu_1)

}

preserve
qui svmat estimates_A

mean estimates_A
egen SD_A=sd(estimates_A)
summarize SD_A, meanonly
scalar sc_SD_A = r(mean)
scalar list sc_SD_A

histogram estimates_A, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_A) 
summarize estimates_A 


* SIMULATION B: use complete_ra, use difference-in-means and OLS
* DIM

local sims = 1000
set matsize 1000
matrix estimates_B_dim = J(`sims', 1, .)

forvalues i=1/`sims'{

* Conduct blocked random assignment
qui complete_ra Z_sim, replace
* Switching Equation
qui tempvar Y
qui gen `Y' = y_z_1*(Z_sim) + y_z_0*(1-Z_sim)
* Estimate difference in means
qui ttest `Y', by(Z_sim) unequal
qui matrix estimates_B_dim[`i',1]= r(mu_2) - r(mu_1)

}

preserve
qui svmat estimates_B_dim

mean estimates_B_dim
egen SD_B_dim=sd(estimates_B_dim)
summarize SD_B_dim, meanonly
scalar sc_SD_B_dim = r(mean)
scalar list sc_SD_B_dim

histogram estimates_B_dim, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_B_dim) 
summarize estimates_B_dim 

* OLS

local sims = 1000
set matsize 1000
matrix estimates_B_ols = J(`sims', 1, .)

forvalues i=1/`sims'{

* Conduct blocked random assignment
qui complete_ra Z_sim, replace
* Switching Equation
qui tempvar Y
qui gen `Y' = y_z_1*(Z_sim) + y_z_0*(1-Z_sim)
* Estimate OLS
qui regress `Y' Z_sim i.pre_score_quantile
qui matrix estimates_B_ols[`i',1]= _b[Z_sim] 

}

preserve
qui svmat estimates_B_ols

mean estimates_B_ols
egen SD_B_ols=sd(estimates_B_ols)
summarize SD_B_ols, meanonly
scalar sc_SD_B_ols = r(mean)
scalar list sc_SD_B_ols

histogram estimates_B_ols, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_B_ols) 
summarize estimates_B_ols 

* compare:
scalar list sc_SD_A sc_SD_B_dim sc_SD_B_ols

* Bottom line: Blocking is great! But you can do approximately as well with OLS


* 2017 CODE ------------------------------------------------
* SIMULATION A: block the treatment, use difference-in-means
generate estimates_A= .
generate z_blocked=.

set more off 
forvalues i=1/10000{
replace random=runiform() if j<=500
sort pre_score_quantile random
bysort pre_score_quantile: replace k=_n if j<=500
bysort pre_score_quantile: replace z_blocked=1 if k<=_N/2 
bysort pre_score_quantile: replace z_blocked=0 if k>_N/2 
replace y_sim=y_z_0*(1-z_blocked)+y_z_1*(z_blocked)
summarize y_sim if z_blocked==1, meanonly
replace mu_1=r(mean) 
summarize y_sim if z_blocked==0, meanonly
replace mu_0=r(mean)
replace estimates_A = mu_1 - mu_0 if `i'==j
}


* SIMULATION B: use complete_ra, use difference-in-means and OLS
generate estimates_B_dim= .
generate estimates_B_ols= .

* Difference-in-means
set more off 
forvalues i=1/10000{
replace random=runiform() if j<=500
sort random
replace k=_n if j<=500
replace z_sim=1 if k<=250 & k!=.
replace z_sim=0 if k>250 & k!=.
replace y_sim=y_z_0*(1-z_sim)+y_z_1*(z_sim)
summarize y_sim if z_sim==1, meanonly
replace mu_1=r(mean) 
summarize y_sim if z_sim==0, meanonly
replace mu_0=r(mean)
replace estimates_B_dim = mu_1 - mu_0 if `i'==j
}

* OLS
set more off 
forvalues i=1/10000{
replace random=runiform() if j<=500
sort random
replace k=_n if j<=500
replace z_sim=1 if k<=250 & k!=.
replace z_sim=0 if k>250 & k!=.
replace y_sim=y_z_0*(1-z_sim)+y_z_1*(z_sim)
quietly regress y_sim z_sim i.pre_score_quantile
replace estimates_B_ols = _b[z_sim] if `i'==j
}


histogram estimates_A, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_A) 
histogram estimates_B_dim, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_B_dim) 
histogram estimates_B_ols, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_B_ols) 

summarize estimates_A estimates_B_dim estimates_B_ols

* Bottom line: Blocking is great! But you can do approximately as well with OLS


**********************************************************
* ALTERNATIVE CODE TO OBTAIN ESTIMATES A, B_dim AND B_ols:
** so far we have done the randomization ourselves, lets now use a shortcut, using a package called Randomize by Chris J. Kennedy and Christopher B. Mann (https://github.com/ck37/randomize_ado)
** first install it from within Stata:

*ssc install randomize

** the most recent version, however, can be downloaded from the site
** download the zip file from under "Manual install" on https://github.com/ck37/randomize_ado and replace the files in the "C:\ado\plus\r" folder
** to find the folder:
sysdir

randomize if j<=500, block(pre_score_quantile) 
tabulate pre_score_quantile _assignment

* SIMULATION A: block the treatment, use difference-in-means
generate estimates_A= .

set more off 
forvalues i=1/10000{
quietly randomize if j<=500, block(pre_score_quantile) replace
recode _assignment (1=0) (2=1)
replace y_sim=y_z_0*(1-_assignment)+y_z_1*(_assignment)
summarize y_sim if _assignment==1, meanonly
replace mu_1=r(mean) 
summarize y_sim if _assignment==0, meanonly
replace mu_0=r(mean)
replace estimates_A = mu_1 - mu_0 if `i'==j
}

* SIMULATION B: use complete_ra, use difference-in-means and OLS
generate estimates_B_dim= .
generate estimates_B_ols= .

* Difference-in-means
set more off 
forvalues i=1/10000{
quietly randomize if j<=500, replace
recode _assignment (1=0) (2=1)
replace y_sim=y_z_0*(1-_assignment)+y_z_1*(_assignment)
summarize y_sim if _assignment==1, meanonly
replace mu_1=r(mean) 
summarize y_sim if _assignment==0, meanonly
replace mu_0=r(mean)
replace estimates_B_dim = mu_1 - mu_0 if `i'==j
}

* OLS
set more off 
forvalues i=1/10000{
quietly randomize if j<=500, replace
recode _assignment (1=0) (2=1)
replace y_sim=y_z_0*(1-_assignment)+y_z_1*(_assignment)
quietly regress y_sim _assignment i.pre_score_quantile
replace estimates_B_ols = _b[_assignment] if `i'==j
}

histogram estimates_A, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_A) 
histogram estimates_B_dim, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_B_dim) 
histogram estimates_B_ols, bin(15) frequency fcolor(none) lcolor(black) xlabel(-2(2)8) title(Histogram of estimates_B_ols) 

summarize estimates_A estimates_B_dim estimates_B_ols

* Bottom line: Blocking is great! But you can do approximately as well with OLS
