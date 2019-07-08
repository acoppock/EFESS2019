*****************************************************************************************************************
* EUROPEAN FIELD EXPERIMENTS SUMMER SCHOOL 2018
* Eline A. de Rooij, Alexander Coppock, Florian Foos
*****************************************************************************************************************
* LAB 4
*****************************************************************************************************************

* The following analyses were carried out using Stata/SE 14.2 for Windows (64-bit x86-64) 
* Gerber, Alan S., et al. "Baseline, placebo, and treatment: Efficient estimation for three-group experiments." Political Analysis (2010): 297-315.

clear

* Uncomment this line to set the correct working directory:
*cd "~\Stata labs" 

* Open .dta data (from folder set as working directory):
use "data\Gerber_et_al_PA_2010.dta", clear

* Drop two-person households and those with missing outcome data
keep if hhcount==1 & p2008_fi!=.

encode treatmen, gen(treat) 
recode treat  (1=0 "control") (2=1 "treatment") (3=2 "placebo"), gen(Z)
drop treat
generate Y=p2008_fi

tabulate Z
tabulate Y Z
tabulate contact Z

* Imagine this is a "normal" phone call experiment ------------------------

generate D=contact if Z!=2

*ITT
regress Y Z if Z!=2, vce(hc2)
scalar itt_est=_b[Z]
*ITTD
regress D Z, vce(hc2)
scalar ittd_est=_b[Z]

scalar cace_est_1=itt_est/ittd_est

scalar list

* Equivalent to two-stage least squares:

ivregress 2sls Y (D=Z)
scalar cace_est_2sls=_b[D]

* Imagine this is only placebo controlled ---------------------------------
recode Z (0=.) (1=1 "treatment") (2=0 "placebo"), gen(Z_rec)
regress Y Z_rec if contact==1, vce(hc2)
scalar cace_est_2=_b[Z_rec]

scalar list

* These authors give a specialized estimator to combine these estimates.
