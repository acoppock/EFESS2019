*****************************************************************************************************************
* EUROPEAN FIELD EXPERIMENTS SUMMER SCHOOL 2018
* Eline A. de Rooij, Alexander Coppock, Florian Foos
*****************************************************************************************************************
* LAB 1
*****************************************************************************************************************

* The following analyses were carried out using Stata/SE 14.2 for Windows (64-bit x86-64) 

clear

* Uncomment this line to set the correct working directory:
*cd "~\Stata labs" 

* Load the csv data (from folder set as working directory):
import delimited "data/lab_1.csv"

** Note: for Stata versions prior to Stata 13, use the command "insheet using"

* Take a look at the data
list in 1/6
codebook 

* Find the true (not estimated) ATE
summarize y1, meanonly
scalar mu_y1 = r(mean)

summarize y0, meanonly
scalar mu_y0 = r(mean)

scalar true_ate = sum(mu_y1 - mu_y0)
scalar list mu_y1 mu_y0 true_ate

* Graph the potential outcomes against one another
twoway (scatter y1 y0, msize(small) jitter(5)) (function y=x, range(0 5)), legend(off)
* units above the 45 degree line have *positive* treatment effects

* Graph the true treatment effects for each unit
generate tau_i = y1-y0
mean tau_i
histogram tau_i, discrete frequency title(Histogram of tau_i)

* Conduct a simulation to see the sampling distribution of the ATE
** Note to self: run code until line 80 in one go

local sims = 2000
set matsize 2000
matrix ate_hats = J(`sims', 1, .)


forvalues i=1/`sims'{

* Conduct complete random assignment
qui complete_ra Z, replace

* Switching Equation
qui tempvar Y
qui gen `Y' = y1*Z + y0*(1-Z)

* Estimate difference in means
qui ttest `Y', by(Z) unequal
qui matrix ate_hats[`i',1]= r(mu_2) - r(mu_1)

}

preserve
qui svmat ate_hats

* Visualize the sampling distribution
histogram ate_hats, bin(200) frequency fcolor(none) lcolor(black) xline(.88, lwidth(thick) lcolor(red) noextend) title(Histogram of ate_hats)

* Unbiasedness
mean ate_hats
mean tau_i

* This is the SD of the sampling distribution
* It's also the *true* (not estimated) standard error
egen SD=sd(ate_hats)
summarize SD, meanonly
scalar sc_SD = r(mean)
scalar list sc_SD

* Check against equation 3.4

generate y1_2=(y1 - mu_y1)^ 2
summarize y1_2, meanonly
scalar popvar_y1 = r(mean)

generate y0_2=(y0 - mu_y0)^ 2
summarize y0_2, meanonly
scalar popvar_y0 = r(mean)

generate sc_y1mu=(y1 - mu_y1)
generate sc_y0mu=(y0 - mu_y0)
generate y1Xy0=sc_y1mu*sc_y0mu
summarize y1Xy0, meanonly
scalar pop_cov = r(mean)

scalar list 

* Only difference here is that default functions divide by N-1 rather than N
summarize y1, detail
scalar var_y1=r(Var)
correlate y1 y0, cov
scalar cov_y1y0=r(cov_12)

scalar list 

* Equation 3.4
scalar N=100
scalar m=50

scalar SE=sqrt(1/(N-1)*(popvar_y0*(m/(N-m)) + popvar_y1*((N-m)/m) + 2*pop_cov)) 
scalar list 

restore
