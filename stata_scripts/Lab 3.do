*****************************************************************************************************************
* EUROPEAN FIELD EXPERIMENTS SUMMER SCHOOL 2018
* Eline A. de Rooij, Alexander Coppock, Florian Foos
*****************************************************************************************************************
* LAB 3
*****************************************************************************************************************

* The following analyses were carried out using Stata/SE 14.2 for Windows (64-bit x86-64) 

clear

* Uncomment this line to set the correct working directory:
*cd "~\Stata labs" 

* Load the csv data (from folder set as working directory):

import delimited "data/lab_3.csv"

** Note: for Stata versions prior to Stata 13, use the command "insheet using"

* Take a look at the data

list in 1/6
codebook 

* Tables before regression:

mean y, over(chicago lowquality black)

* The regression is hard to read!

regress y black##chicago##lowquality, allbaselevels
regress y black##chicago##lowquality
matrix list e(b)

* you can go back and forth between the tables

scalar subpop_1=_b[_cons]
scalar list
mean y if chicago==0 & lowquality==0 & black==0

scalar subpop_2=_b[_cons]+_b[1.black]
scalar list
mean y if chicago==0 & lowquality==0 & black==1

* graphical analysis

bysort chicago lowquality black: egen group_mean=mean(y)
twoway (lfit group_mean black if lowquality==0) (lfit group_mean black if lowquality==1), ytitle(Group_mean) ylabel(0.05(.02).14) xtitle(Black) legend(order(1 "low quality" 2 "high quality")) by(chicago)

* Notice that there are 4 slopes with respect to "Black"... let's calculate each slope with regression

regress y black if chicago==0 & lowquality==0
estimates store A
regress y black if chicago==1 & lowquality==0
estimates store B
regress y black if chicago==0 & lowquality==1
estimates store C
regress y black if chicago==1 & lowquality==1
estimates store D

* we could also plot the slopes!
* find and install coefplot to plot regression slopes easily
* findit coefplot

coefplot (A, label(chicago=0 & lowquality=0)) (B, label(chicago=1 & lowquality=0)) (C, label(chicago=0 & lowquality=1)) (D, label(chicago=1 & lowquality=1)) , vertical drop(_cons) yline(0)

* Questions for discussion

* 1. What is the effect of low quality in Chicago? is it significant?
* 2. What is the effect of low quality in Boston? is it significant?
* 3. What is the *difference* in the effects of low quality across contexts?
* 4. The regression regress y black##chicago##lowquality reports 8 numbers. Which are causal? Which are descriptive?
