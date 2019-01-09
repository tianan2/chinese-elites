**********************************************************************************************************
**********************************************************************************************************
******************** 	SCRIPT TO REPLICATE TABLE a.18 IN THE ONLINE APPENDIX ****************************
**********************************************************************************************************
**********************************************************************************************************

** Author: Junyan Jiang (CUHK)
** Last Updated: May 30, 2018
*** Stata 14.2 SE
*** Windows 10 x64, Intel i-7-7500U
*** 16GB RAM

// Description: 
// This script allows the user to replicate the promotion result in Table A.18.


***************************************************************************** 
******************** Set Working Directory and load data ********************
*****************************************************************************
clear all

//** Change this to the folder that stores all replication files **//
cd "D:\Users\jiang\Dropbox\Academic\Research\Dissertation\Chapters\[A] Patronage and Development\Draft\Submissions\AJPS Final Draft\Replication files"
//cd "C:\Users\junya\Dropbox\Academic\Research\Dissertation\Chapters\[A] Patronage and Development\Draft\Submissions\AJPS Final Draft\Replication files"

//import delimited "performance_and_promotion.csv", encoding(UTF-8)
use "performance_and_promotion.dta", clear

*************** Check Performance and Connection **************


set more off

fvset base 2010 year


g minor=ethnicity!="汉族"
g agesq=age^2
g agecb=age^3
g female=sex=="女"
encode firstprovince, gen(firstprov)
label var female "Female"
label var minor "Ethnic minority"
label var age "Age"
label var agesq "Age$^2$"
label var agecb "Age$^3$"
label var actualcollege "Full-time college"
g bin_conn2currenttop=(conn2currenttop>0)
g bin_conn2currentsc=(conn2currentsc>0)
g bin_conn2currentpb=(conn2currentpb>0)
label var bin_conn2currenttop "Patron currently a PSC/PB member"


**********************************************************************


label var pf_f2gdpidx_dm1 "CRP under patron"
label var pf_f2gdpidx_dm2 "CRP under non-patron"
label var pf_f2gdpidx_dm3 "CRP (overall)"

label var pf_gdp_dm3 "Average relative city GDP"
label var pf_totalpop_dm3 "Average relative city population"
label var pf_f1logtransfer_dm3 "Average relative fiscal transfer"
label var firstbureau "Year first promoted to bureau-level"

foreach i of varlist pf*{
replace `i'=0 if `i'==.
}


egen firstrank=first(rank),by(name)
drop if firstrank>2






***********************************************************************************************
**** Table A.18: Particularistic Reward to Performance: Evidence from Promotion and Purge******
***********************************************************************************************

// Promotion
eststo clear

set more off
local pf  f2gdpidx_dm
local pf2 gdp_dm
local pf3 totalpop_dm
local pf4 f1logtransfer_dm


stset expid , failure(pro2vp_trueprize=1) origin(expid = 1) id(name)  
eststo m1:  stcox  i.year  age agesq pf_`pf'1   pf_`pf'2    pf_`pf2'3    pf_`pf3'3 pf_`pf4'3 ///
if year<=2011 , strata(firstprovince leadertype) robust  nohr
estat phtest

estadd sca df=r(df)
estadd sca chi2=r(chi2), replace
estadd sca prop=chi2tail(r(df), r(chi2)) 
estadd sca cluster=`e(N_clust)'
estadd sca fail=`e(N_fail)'
estadd local yfe "$\checkmark$"
test pf_`pf'1=pf_`pf'2
estadd sca diff=r(p), replace

stset expid , failure(pro2vp_trueprize=1) origin(expid = 1) id(name)  
eststo m2:  stcox  i.year age agesq  pf_`pf'1   pf_`pf'2    pf_`pf2'3    pf_`pf3'3 pf_`pf4'3 ///
 minor female actualcollege firstbureau  bin_conn2currenttop if year<=2011, strata(firstprovince leadertype) robust  nohr
estat phtest
estadd sca df=r(df)
estadd sca chi2=r(chi2), replace
estadd sca prop=chi2tail(r(df), r(chi2)) 
estadd sca cluster=`e(N_clust)'
estadd sca fail=`e(N_fail)'
estadd local yfe "$\checkmark$"
estadd local ct "$\checkmark$"
test pf_`pf'1=pf_`pf'2
estadd sca diff=r(p), replace



// Purge

set more off
local pf  f2gdpidx_dm
local pf2 gdp_dm
local pf3 totalpop_dm
local pf4 f1logtransfer_dm


stset expid , failure(purge=1) origin(expid = 1) id(name)  
eststo m3:  stcox   i.year   age agesq  pf_`pf'1   pf_`pf'2    pf_`pf2'3    pf_`pf3'3 pf_`pf4'3 ///
 if year<=2011 , strata(firstprovince leadertype) robust  nohr
estat phtest

estadd sca df=r(df)
estadd sca chi2=r(chi2), replace
estadd sca prop=chi2tail(r(df), r(chi2)) 
estadd sca cluster=`e(N_clust)'
estadd sca fail=`e(N_fail)'
estadd local yfe "$\checkmark$"
test pf_`pf'1=pf_`pf'2
estadd sca diff=r(p), replace


stset expid , failure(purge=1) origin(expid = 1) id(name)  
eststo m4:  stcox   i.year  age agesq   pf_`pf'1   pf_`pf'2    pf_`pf2'3   pf_`pf3'3 pf_`pf4'3 ///
minor female actualcollege firstbureau  bin_conn2currenttop  if year<=2011 , strata(firstprovince leadertype) robust  nohr
estat phtest

estadd sca df=r(df)
estadd sca chi2=r(chi2), replace
estadd sca prop=chi2tail(r(df), r(chi2)) 
estadd sca cluster=`e(N_clust)'
estadd sca fail=`e(N_fail)'
estadd local yfe "$\checkmark$"
estadd local ct "$\checkmark$"
test pf_`pf'1=pf_`pf'2
estadd sca diff=r(p), replace

esttab  m1 m2  m3 m4    using out.txt,  star( * 0.05 ** 0.01)  b(3) se(3) nomtitles booktabs nobaselevels drop(*year ) label nonote ///
order(pf_f2gdpidx_dm1  pf_f2gdpidx_dm2  pf_gdp_dm3 pf_totalpop_dm3 pf_f1logtransfer_dm3   age agesq  bin_conn2currenttop firstbureau  minor female actualcollege ) ///
mgroup( "Promotion" "Disciplinary Sanctions", pattern(1 0  1 0  ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) replace ///
stats(yfe  prop fail cluster N , labels("Year dummies"  "Proportional hazard test (p-value)" "Number of promotions" "Number of individuals" "Observations") fmt(0 3 0 0 0 ) )


