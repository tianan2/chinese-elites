*********************************************************************************************************************************
*********************************************************************************************************************************
******************** 	SCRIPT TO REPLICATE THE VALIDATION TESTS IN THE ONLINE APPENDIX ****************************
*********************************************************************************************************************************
*********************************************************************************************************************************

** Author: Junyan Jiang (CUHK)
** Last Updated: May 29, 2018
** Environment: 
*** Stata 14.2 SE
*** Windows 10 x64, Intel i-7-7500U
*** 16GB RAM

// Description: 
// This script allows the user to replicate results from the validation tests in Tables A.1 and A.2


********************************************* 
*********** Install Necessary Packages ******
*********************************************
net install snp7_1.pkg

**************************************************************************************
******************** Set Working Directory and Preprocess Data *********************** 
**************************************************************************************

clear all

//** Change this to the folder that stores all replication files **//
//cd "C:\Users\junya\Dropbox\Academic\Research\Dissertation\Chapters\[A] Patronage and Development\Draft\Submissions\AJPS Final Draft\Replication files"
cd "D:\Users\jiang\Dropbox\Academic\Research\Dissertation\Chapters\[A] Patronage and Development\Draft\Submissions\AJPS Final Draft\Replication files"

use "validation_data.dta",clear

keep if leadertype=="m" // Only use the sample of city leaders

encode name, gen(name_id) // convert name to labels

g college=edu=="本科"|edu=="硕士"|edu=="博士"|edu=="博士后"
label var college "College education"
g minor=ethnicity!="汉族"

g agesq=age^2
g agecb=age^3


egen firstage=min(age), by(name)
xtile agepct3=firstage , n(3)
xtile agepct4=firstage , n(4)
xtile agepct5=firstage , n(5)



local no 1 _op 

// Create binary indicators for various connection measures
foreach i of local no {
g conn2currenttop`i'=conn2currentsc`i'+conn2currentpb`i'
g bin_conn2currenttop`i'=(conn2currentsc`i'>0 |conn2currentpb`i'>0)

g bin_conn2currentsc`i'=(conn2currentsc`i'>0)
label var bin_conn2currentsc`i' "Connected to current PSC members"

g bin_conn2currentpb`i'=(conn2currentpb`i'>0)
label var bin_conn2currentpb`i' "Connected to current PB members"

}


label var bin_conn2currentsc_op "Connected to current PSC members (work overlap)"
label var bin_conn2currentpb_op "Connected to current PB members (work overlap)"




egen maxirank=max(rank),by(name)
g retire2=(age>60 & age!=.) if maxirank<=2
replace retire2=(age>63 & age!=.) if maxirank==2.5
replace retire2=(age>65 & age!=.) if maxirank==3
replace retire2=(age>67 & age!=.) if maxirank>=3.5

label var retire2 "Retired"

egen rk=group(rank)

g female=sex=="女"
label var female "Female"
label var minor "Ethnic minority"
label var age "Age"
label var agesq "Age$^2$"
label var agecb "Age$^3$"
label var bin_conn2currenttop1 "Connected to current PSC/PB members(1=yes)"

label var firstage "First age"

keep if age<=75 | age==.


capture drop expid
xtset name_id year
egen expid=seq(), by(name)

stset expid  , failure(anticorruption) // Sample restriction: remove observations after anticorruption
g c_samp=_st==1

stset expid  , failure(pro2vp_trueprize) // Sample restriction: remove observations after promotion
g p_samp=_st==1

spbase expid, gen(nsp_) nk(3)

encode firstprovince, gen(firstprov)



label var age "Age"
label var agesq "Age\(^2\)"


label var bin_conn2currenttop_op "Patrons became Politburo members (overlap-based measure)"
label var bin_conn2currenttop1 "Patrons became Politburo members (promotion-based measure)"

label var bin_conn2currentpb1 "Patrons became Politburo members (promotion-based)"
label var bin_conn2currentsc1  "Patrons became Standing Committee members (promotion-based)"

label var bin_conn2currentpb_op "Patrons became Politburo members (overlap-based)"
label var bin_conn2currentsc_op  "Patrons became Standing Committee members (overlap-based)"

label var conn2targets_op "Patrons became target of investigation (overlap-based)"
label var conn2targets  "Patrons became target of investigation (promotion-based)"

*********************************************************************
*********************************************************************
**************************** Analysis *******************************
*********************************************************************
*********************************************************************



********************************************************************************* 
************************	Table A.1: Promotion   ******************************
*********************************************************************************

set more off
eststo clear


eststo m1: logit pro2vp_trueprize  c.year##c.year##c.year (c.nsp*) i.firstprov     ///
bin_conn2currenttop1   ///
if p_samp==1  , robust  cluster(name)  asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)


eststo m2: logit pro2vp_trueprize  c.year##c.year##c.year (c.nsp*)  i.firstprov  minor female  college    age agesq   ///
bin_conn2currenttop1   ///
if p_samp==1 , robust  cluster(name) asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)

eststo m3: logit pro2vp_trueprize  c.year##c.year##c.year (c.nsp*)  i.firstprov    ///
bin_conn2currenttop_op   ///
if p_samp==1  , robust  cluster(name)   asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)

eststo m4: logit pro2vp_trueprize  c.year##c.year##c.year i.firstprov  (c.nsp*)  minor female  college    age agesq   ///
bin_conn2currenttop_op   ///
if p_samp==1 , robust  cluster(name)   asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)

esttab  m1 m2 m3 m4  using out.txt, noconstant eqlabels(none)  nogaps nonote nobaselevels   b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace  nodiscrete nomtitles keep(bin_conn2currenttop1 bin_conn2currenttop_op minor female  college) ///
order(bin_conn2currenttop1 bin_conn2currenttop_op minor female  college  age agesq) ///
mgroup("DV: Client Promoted to Next Level (1=yes)", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( dura r2_p aic bic nperson N, labels("Year, Tenure, and Age Cubic Spline" "Pseudo R$^2$" "AIC" "BIC" "Number of Individuals" "Observations") fmt(0 2 1 1 0 0) )




********************************************************************************* 
************************	Table A.2: Demotion/Purge   *************************
*********************************************************************************

eststo clear
eststo m1: logit anticorruption   i.year  i.firstprov (c.nsp*)    ///
 bin_conn2currentpb1  bin_conn2currentsc1 conn2targets     ///
if c_samp==1  , robust  cluster(name) asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)



eststo m2: logit anticorruption  i.year i.firstprov  (c.nsp*)  minor female  college    age agesq agecb  ///
bin_conn2currentpb1 bin_conn2currentsc1  conn2targets     ///
if c_samp==1  , robust  cluster(name)  asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)



eststo m3: logit anticorruption   i.year   (c.nsp*)    ///
bin_conn2currentpb_op bin_conn2currentsc_op  conn2targets_op    ///
if c_samp==1  , robust  cluster(name)  asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)



eststo m4: logit anticorruption   i.year i.firstprov (c.nsp*)  minor female  college    age agesq agecb  ///
bin_conn2currentpb_op bin_conn2currentsc_op   conn2targets_op    ///
if c_samp==1 , robust  cluster(name)  asis
estadd local dura "$\checkmark$"
estadd local pfe "$\checkmark$"
estadd scalar nperson=e(N_clust)




esttab  m1 m2 m3 m4  using out.txt, noconstant eqlabels(none)  nogaps nonote nobaselevels   b(3) se(3) ///
star(+ 0.1 * 0.05 ** 0.01) label booktabs replace  nodiscrete nomtitles keep(bin_conn2currentsc1 bin_conn2currentpb1  conn2targets  bin_conn2currentsc_op bin_conn2currentpb_op conn2targets_op   minor female  college  ) ///
order(bin_conn2currentsc1 bin_conn2currentpb1  conn2targets  bin_conn2currentsc_op bin_conn2currentpb_op conn2targets_op   minor female  college  ) ///
mgroup("DV: Client Investigated for Corruption (1=yes)", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( dura r2_p aic bic nperson N, labels("Year, Tenure, and Age Cubic Spline" "Pseudo R$^2$" "AIC" "BIC" "Number of Individuals" "Observations") fmt(0 2 2 2 0 0) )




