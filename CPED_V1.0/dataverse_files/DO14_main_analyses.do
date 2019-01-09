*********************************************************************************************************************************
*********************************************************************************************************************************
******************** 	SCRIPT TO REPLICATE THE MAIN DATA ANALYSES IN MAIN TEXT AND ONLINE APPENDIX ****************************
*********************************************************************************************************************************
*********************************************************************************************************************************

** Author: Junyan Jiang (CUHK)
** Last Updated: July 18, 2018
** Environment: 
*** Stata 14.2 SE
*** Windows 10 x64, Intel i-7-7500U
*** 16GB RAM

// Description: 
// This script allows the user to replicate most of the regression results in the main text
// and online appendix (except for the validation tests in Tables A.1 and A.2 and the promotion 
// results in Table A.18).



********************************************************** 
******************** Install Packages ********************
********************************************************** 


// Install the necessary package //
ssc inst egenmore // more egen options
net inst dm79.pkg  // more matrix options
ssc inst xtabond2  // GMM analysis

***************************************************************************** 
******************** Set Working Directory and load data ********************
*****************************************************************************




clear all

//** Change this to the folder that stores all replication files **//
cd "D:\Users\jiang\Dropbox\Academic\Research\Dissertation\Chapters\[A] Patronage and Development\Draft\Submissions\AJPS Final Draft\Replication files"
//cd "C:\Users\junya\Dropbox\Academic\Research\Dissertation\Chapters\[A] Patronage and Development\Draft\Submissions\AJPS Final Draft\Replication files"

use "citypanel_base.dta",clear


run "DO14_process_before_main" // Run the script for data preprocessing



// Define some global variable lists
global econ start*_loggdp  start*_logpop  start*_loginvest dep startm*gdpidx

global career  msec_female mayor_female msec_edu mayor_edu ///
mayorage  msecage m*in1  m*in3 m*in5

global fe i.year##i.provid

// Set the sample to before 2000
g samp=(year>=2000)  

// Remove Tibet
keep if provid!=540000 

***************************************************************************
***************************************************************************
*****************	  			Analysis 				*******************
***************************************************************************
***************************************************************************

set matsize 3000
set more off

****************************************************
*** Table 1: Main Results: Connection on Growth **** 
****************************************************
 
xtset cityid year

eststo clear
eststo m1: xtreg F2.gdpidx  $fe  bin_mleader2currentsec if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ ""
estadd local career ""
estadd local ncity `e(N_clust)'

eststo m2:xtreg F2.gdpidx  $fe $econ bin_mleader2currentsec if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career ""
estadd local ncity `e(N_clust)'

eststo m3:xtreg F2.gdpidx  $fe  $econ $career bin_mleader2currentsec   if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


eststo m4:xtreg F2.gdpidx_1st $fe $econ $career  bin_mleader2currentsec if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m5:xtreg F2.gdpidx_2nd $fe $econ $career bin_mleader2currentsec  if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m6:xtreg F2.gdpidx_3rd  $fe $econ $career bin_mleader2currentsec if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


esttab m1 m2 m3 m4 m5 m6   using out.txt, nonote nobaselevels keep(bin_mleader2currentsec) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
mtitles("Overall" "Overall" "Overall" "Agriculture" "Manufacturing" "Service") ///
 mgroup("Growth at \(t+2\) (last year=100)", pattern(1 0 0 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0   0 0 2 0 0) )





********************************* 
*** Figure 2: Parallel Trends *** 
********************************* 



xtreg  F2.gdpidx    $fe   $econ    $career    prior4  prior3 prior2 prior1  conn1 conn2 conn3   post1 post2  post3      if samp==1  , fe cluster(cityid)
margins,dydx( prior4 prior3 prior2 prior1  conn1 conn2 conn3   post1 post2 post3   ) noestimcheck 
marginsplot
expmat para2_overall

xtreg  F2.gdpidx_2nd    $fe   $econ    $career    prior4  prior3 prior2 prior1  conn1 conn2 conn3   post1 post2  post3      if samp==1    , fe cluster(cityid)
margins,dydx( prior4 prior3 prior2 prior1  conn1 conn2 conn3   post1 post2 post3   ) noestimcheck 
marginsplot
expmat para2_2nd

// Note: visualization of the matrices is performed in R. 
// The code can be found under the Visualization folder.

***********************************************************
***	Table 2: Effect of Connection on Data Falsification ***
***********************************************************

// Standardize the growth difference to facilitate interpretation //
capture drop grow_gap_*std
egen grow_gap_freight_std=std(grow_gap_freight)
egen grow_gap_power_std=std(grow_gap_power)
egen grow_gap_light_std=std(grow_gap_light)
egen grow_gap_std=std(growgap)

eststo clear
eststo m1: areg F2.grow_gap_freight_std   $fe $econ  $career  bin_mleader2currentsec if samp==1, a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m2:areg F2.grow_gap_power_std  $fe $econ $career  bin_mleader2currentsec if samp==1 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m3:areg F2.grow_gap_light_std  $fe $econ $career  bin_mleader2currentsec if samp==1 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


eststo m4:areg F2.grow_gap_std   $fe $econ  $career  bin_mleader2currentsec if samp==1 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

esttab m?  using out.txt, nonote nobaselevels keep(bin_mleader2currentsec) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
mtitles("Freight" "Power" "Brightness" "\({\small \frac{Freight+Power+Brightness}{3}}\)" ) ///
 mgroup("Growth difference between GDP and ... \(t+2\) (last year=100)", pattern(1 0 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0   0 0 2 0 0) )




*****************************************************************************************
*** Table 3: Effects of Connection on Distribution of Government-Controlled Resources ***
*****************************************************************************************

eststo clear

eststo m1: areg F.logloan  $fe $econ   $career   bin_mleader2currentsec if samp==1 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2004-2013"


eststo m2: areg F.log_allarea  $fe $econ  $career   bin_mleader2currentsec if samp==1 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2003-2011"


eststo m3: areg F.anysez_pv  $fe $econ   $career    bin_mleader2currentsec if samp==1 & year<=2006 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2006"


eststo m4: areg F.anysez_nat $fe $econ  $career     bin_mleader2currentsec if samp==1 & year<=2006 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2006"


esttab m1 m2 m3 m4  using out.txt, nonote nobaselevels keep(bin_mleader2currentsec) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
mtitles("log loan" "log land quota" "national SEZ" "provincial SEZ") ///
mgroup("Tangible Resource \((t+1)\)" "Policy Support \((t+1)\) ", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats(   yfe  econ career  r2_a period ncity N, labels( "City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "DV period" "Number of Cities" "Observations") fmt(  0  0 0  2 0 0 0) )




********************************************************** 
*** Table 4: Effect of Connection on Policy Priorities *** 
**********************************************************  
/*
// Classify topics into themes (see LDA topic output for reference)
g prodevelopment=(G20_topic1+G20_topic3+G20_topic10+G20_topic13+G20_topic14+G20_topic15)/6
g prosocial=G20_topic18
g proculture=(G20_topic2+G20_topic4+G20_topic6+G20_topic17)/4
g proadminreform=(G20_topic0+G20_topic12)/2
*/


eststo clear
eststo m1: xtreg   F.prodevelopment $fe  $econ  $career   bin_mleader2currentsec    if samp==1  , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2004-2012"

eststo m2: xtreg   F.prosocial $fe   $econ  $career  bin_mleader2currentsec   if samp==1   ,fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2004-2012"

eststo m3: xtreg   F.proculture $fe   $econ  $career  bin_mleader2currentsec   if samp==1   ,fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2004-2012"


eststo m4: xtreg   F.proadmin $fe   $econ  $career  bin_mleader2currentsec   if samp==1    , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2004-2012"



esttab  m1 m2 m3 m4 using out.txt, nonote nobaselevels keep( *bin_mleader2currentsec) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace  mtitles("development" "welfare" "culture" "administration") ///
 mgroup("\% of work report topics \((t+1)\)" , pattern(1 0 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe  econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0  0 0 2 0 0) )




*****************************************************************************************
*****************************************************************************************
******************************* Online Appendix **********************************
*****************************************************************************************
*****************************************************************************************

// Tables A.1 and A.2 can be replicated with codes from "DO14_validation.do".



***********************************************************************
***Table A.3 Estimated Effect of Connection by Timing of Promotion	***
***********************************************************************





eststo clear

eststo m1: xtreg F2.gdpidx  $fe $econ  $career  bin_mleader2pleader if samp==1 , fe  cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m2: xtreg F2.gdpidx  $fe $econ  $career bin_mleader2currentsec2 if samp==1 , fe  cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m3: xtreg F2.gdpidx  $fe $econ  $career bin_mleader2currentsec_nofirst6 bin_mleader2currentsec_first6 if samp==1 , fe  cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


esttab m1  m2 m3 using out.txt, nonote nobaselevels nogaps keep(bin_mleader2*) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
nomtitles mgroup("GDP growth at \(t+2\) (last year=100)", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe econ  r2_a ncity N, labels("City and province-year FE" "Economic and leadership controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0 0 2 0 0) )




**************************************************************
*** Table A.4 Estimation based on Detrended Growth and GMM ***
**************************************************************

eststo clear
eststo m1: xtreg F2.D.gdpidx  $fe bin_mleader2currentsec if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ ""
estadd local career ""
estadd local ncity `e(N_clust)'

eststo m2:xtreg F2.D.gdpidx $fe $econ  bin_mleader2currentsec   if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career ""
estadd local ncity `e(N_clust)'

eststo m3:xtreg F2.D.gdpidx  $fe  $econ  $career  bin_mleader2currentsec   if samp==1, fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


mata: mata set matafavor speed
xi i.cityid i.provid*i.year
eststo m4: xtabond2 F2.gdpidx  bin_mleader2currentsec _I*  gdpidx if samp==1, gmm(gdpidx) ///
iv(bin_mleader2currentsec _I* ) nolevel robust cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local ncity `e(N_clust)'

xi i.cityid i.provid*i.year
eststo m5: xtabond2 F2.gdpidx  bin_mleader2currentsec _I* $econ  gdpidx if samp==1, gmm(gdpidx) ///
iv(bin_mleader2currentsec _I* $econ ) nolevel robust cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local ncity `e(N_clust)'

xi i.cityid i.provid*i.year
eststo m6: xtabond2 F2.gdpidx  bin_mleader2currentsec _I* $econ  $career  gdpidx if samp==1, gmm(gdpidx) ///
iv(bin_mleader2currentsec _I* $econ  $career ) nolevel robust cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

esttab m1 m2 m3 m4 m5 m6 using out.txt, nonote nogaps nobaselevels keep(bin_mleader2currentsec) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
mtitles("OLS" "OLS" "OLS" "GMM" "GMM" "GMM" ) ///
mgroup("DV: Detrend GDP Growth at \(t+2\)" "DV: GDP Growth at \(t+2\)", pattern(1 0 0 1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe econ  r2_a ncity N, labels("City and province-year FE" "Economic and leadership controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0 0 2 0 0) )




******************************************************************************
*** Table A.5 Effects of Connections on Economic Performance by Sub-Sample ***
******************************************************************************
capture drop fusheng 
g fusheng=citylevel=="副省级城市" // Create an indicator to identify vice-provincial-level cities
eststo clear
eststo m1: xtreg F2.gdpidx  $fe $econ  $career bin_mleader2currentsec   if samp==1 & fusheng==0, fe  cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


eststo m2: xtreg F2.gdpidx  $fe $econ  $career  bin_mleader2currentsec   if samp==1 & auto==0, fe  cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m3: xtreg F2.gdpidx  $fe $econ  $career  bin_mleader2currentsec   if samp==1 & east==1, fe  cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m4: xtreg F2.gdpidx  $fe $econ  $career bin_mleader2currentsec   if samp==1 &   east==0, fe  cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


esttab m1 m2 m3 m4   using out.txt, nonote nobaselevels nogaps keep(bin_mleader2*) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
mtitle("Prefecture-level only" "Exclude \emph{zhou} and \emph{meng}" "East" "Non-East") mgroup("GDP growth at \(t+2\) (last year=100)", pattern(1 0 0 0  ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe econ  r2_a ncity N, labels("City and province-year FE" "Economic and leadership controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0 0 2 0 0) )




************************************************************************
*** Table A.6 Alternative Specification to Control for Tenure Length ***
************************************************************************
eststo clear
eststo m1: xtreg F2.gdpidx  $fe  $econ   msec_female mayor_female msec_edu mayor_edu ///
mayorage  msecage    i.msec_tenure3  i.mayor_tenure3 ///
bin_mleader2currentsec    if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m2: xtreg F2.gdpidx   $fe $econ  msec_female mayor_female msec_edu mayor_edu ///
mayorage  msecage     msec_tenure msec_tenure_sq    mayor_tenure mayor_tenure_sq  ///
bin_mleader2currentsec    if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m3: xtreg F2.gdpidx   $fe $econ  msec_female mayor_female msec_edu mayor_edu ///
mayorage  msecage    msec_tenure msec_tenure_sq msec_tenure_cb    mayor_tenure mayor_tenure_sq mayor_tenure_cb  ///
bin_mleader2currentsec    if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


esttab m1  m2 m3 using out.txt,   nonote nobaselevels keep(bin_mleader2currentsec *tenure3* msec_tenure msec_tenure_sq msec_tenure_cb    mayor_tenure mayor_tenure_sq mayor_tenure_cb ) b(3) se(3) ///
order(bin_mleader2currentsec *tenure3* msec_tenure mayor_tenure msec_tenure_sq mayor_tenure_sq msec_tenure_cb  mayor_tenure_cb )  ///
refcat(1.msec_tenure3 "City secretary's tenure (reference = 1 year)" 1.mayor_tenure3 "Mayor's tenure (reference = 1 year)", nolabel) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
nomtitles  mgroup("Growth at \(t+2\) (last year=100)", pattern(1 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe pxy econ career  r2_a ncity N, labels("Year and city FE"  "Province X Year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0 0 0 0 2 0 0) )



********************************************************************* 
*** Table A.7 Prior Growth Trends do not Affect City’s Connection ***
********************************************************************* 


eststo m1: areg F.bin_mleader2currentsec i.year   mv3_gdpidx_1st  mv3_gdpidx_2nd mv3_gdpidx_3rd mv3_growexp mv3_growpop    loggdp logpop govsize logltavg if samp==1 & final_conntime_either!=. ,  a(cityid)   
estadd local ncity `e(N_clust)'
estadd local yfe "$\checkmark$"



esttab m1    using out.txt, nonote nobaselevels nogaps noconstant drop(*year _cons) b(3) se(3)  ///
star(* 0.05 ** 0.01) label booktabs replace   nomtitle ///
rename(final_conntime2 final_conntime) varlabel(final_conntime "Years being unconnected") ///
mgroup("DV: Cities Becoming Connected at \(t+1\)", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe r2  N, labels("City and year fixed-effects" "R square"  "Observations") fmt(0 3 0 0 ) ) 



***********************************************************************
***	Table A.8: Effect of Connection by City Leaders’ Turnover Types	***
***********************************************************************

eststo m1: xtreg F2.gdpidx  $fe  $econ  $career  bin_mleader2currentsec_regular bin_mleader2currentsec_irregular if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'



esttab  m1 using out.txt, nonote nobaselevels keep(bin_mleader2*) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
nomtitles mgroup("GDP growth at \(t+2\) (last year=100)", pattern(1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe econ  r2_a ncity N, labels("City and province-year FE" "Economic and leadership controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0 0 2 0 0) )








*******************************************
*** Table A.9	Lag and Lead Structure	***
*******************************************
eststo clear

eststo m0:areg F2.gdpidx   $fe  $econ  $career   L2.bin_mleader2currentsec if samp==1  , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m1:areg F2.gdpidx   $fe   $econ  $career   L.bin_mleader2currentsec if samp==1  , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


eststo m2:areg F2.gdpidx   $fe   $econ  $career   bin_mleader2currentsec if samp==1  , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m3:areg F2.gdpidx   $fe   $econ  $career   F.bin_mleader2currentsec if samp==1  , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


eststo m4:areg F2.gdpidx   $fe   $econ  $career   F2.bin_mleader2currentsec if samp==1  , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m5:areg F2.gdpidx   $fe  $econ  $career   F3.bin_mleader2currentsec if samp==1  , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


esttab m0 m1 m2 m3 m4 m5 using out.txt, nonote nobaselevels ///
keep(*bin_mleader2currentsec) order(L* bin_mleader2currentsec F*) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
nomtitles mgroup("GDP growth at \(t+1\)", pattern(1 0 0 0 0 0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe econ  r2_a ncity N, labels("City and province-year FE" "Economic and leadership controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0 0 2 0 0) )





***************************************************
*** Table A.10:	Distributive Favoritism at t+2	***
***************************************************
eststo clear
eststo m1: areg F2.logloan  $fe $econ  $career   bin_mleader2currentsec if samp==1 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"

eststo m2: areg F2.log_allarea  $fe $econ  $career   bin_mleader2currentsec if samp==1 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"

eststo m3: areg F2.anysez_pv  $fe $econ  $career    bin_mleader2currentsec if samp==1 & year<=2006 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"

eststo m4: areg F2.anysez_nat $fe $econ  $career     bin_mleader2currentsec if samp==1 & year<=2006 , a(cityid) cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"

esttab m1 m2 m3 m4  using out.txt, nonote nobaselevels keep(bin_mleader2currentsec) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
mtitles("log loan" "log land quota" "national SEZ" "provincial SEZ") ///
mgroup("Tangible Resource \((t+2)\)" "Policy Support \((t+2)\) ", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats(   yfe  econ career  r2_a   ncity N, labels( "City and province-year FE" "City economic controls" "City leader controls"   "Adjusted R$^2$"  "Number of Cities" "Observations") fmt(  0  0 0  2 0 0  ) )




******************************************************************************
*** Table A.11 Empirical Association between Performance and Tenure Length ***
******************************************************************************

eststo clear
// Collapse the sample by secretary-city spells //
// in order to examine the relationship between //
// overall performance and total tenure length //
preserve 
collapse (mean) lgdpidx gdpidx f2gdpidx (max) msec_tenure   year, by(cityid msec)
eststo m1: reg  msec_tenure   i.year gdpidx
estadd local yfe "$\checkmark$"

eststo m2: reg  msec_tenure i.year lgdpidx
estadd local yfe "$\checkmark$"
restore


// Do the same for mayor-city spells
preserve
collapse (mean) gdpidx lgdpidx (max) mayor_tenure  year, by(cityid mayor)
eststo m3: reg   mayor_tenure  i.year  gdpidx
estadd local yfe "$\checkmark$"

eststo m4: reg   mayor_tenure  i.year  lgdpidx
estadd local yfe "$\checkmark$"

restore

esttab m1 m2 m3 m4 using out.txt, nonote nobaselevels keep(gdpidx lgdpidx) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace varlabel(lgdpidx "Average GDP growth (lagged 1 year)" gdpidx "Average GDP growth")  ///
nomtitles ///
 mgroup("City Secretary's Tenure" "Mayor's Tenure", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats(yfe r2_a   N, labels("Year FE" "Adjusted R$^2$"   "Observations") fmt(0 2 0 0) )


*********************************************************
***	Table 12. Accounting for Background Heterogeneity ***
*********************************************************



eststo clear
eststo m1:xtreg F2.gdpidx  $fe  $econ  $career  bin_mleader2currentsec msec_top_exp mayor_top_exp if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m2: xtreg F2.gdpidx   $fe  $econ  $career  bin_mleader2currentsec msec_soe_exp mayor_soe_exp if samp==1 ,fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m3: xtreg F2.gdpidx  $fe  $econ  $career  bin_mleader2currentsec msec_finance mayor_finance if samp==1 ,fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


eststo m4: xtreg F2.gdpidx  $fe  $econ  $career  bin_mleader2currentsec msec_localshare mayor_localshare if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

eststo m5: xtreg F2.gdpidx  $fe  $econ  $career  bin_mleader2currentsec msec_top_exp mayor_top_exp msec_soe_exp mayor_soe_exp ///
msec_finance mayor_finance  msec_localshare mayor_localshare if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'


esttab m1 m2 m3 m4 m5 using out.txt, nonote nogaps nobaselevels keep(bin_mleader2*  msec_top_exp mayor_top_exp msec_soe_exp mayor_soe_exp msec_finance mayor_finance *localshare ) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
nomtitles mgroup("GDP growth at \(t+2\) (last year=100)", pattern(1 0 0 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe  econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0  0 0 2 0 0) )




***********************************************************************************
*** Table A.13: Performance Premium by Outcomes of Former Provincial Secretary  ***
***********************************************************************************


eststo clear
// The results are comparable across different types of predecessors
eststo m1: xtreg F2.gdpidx  $fe  $econ   $career   conn_provretire conn_provtransfer    ///
if samp==1  , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

esttab m1   using out.txt, nonote nobaselevels keep(conn_provretire conn_provtransfer ) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace   ///
nomtitles ///
 mgroup("Growth at \(t+2\) (last year=100)", pattern(1 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe  econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0  0 0 2 0 0) )




***************************************************************************************
*** Table A.14: Placebo Test Using Recent Promotions by Former Provincial Secretaries ***
***************************************************************************************

eststo clear
eststo m1: xtreg F2.gdpidx  $fe  $econ   $career   ///
    bin_mleader_justbefore2  bin_mleader_justbefore1 bin_mleader_justafter1 bin_mleader2_aftermore1  ///
if samp==1 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'

esttab m1   using out.txt, nonote nobaselevels keep(bin_mleader_justbefore2  bin_mleader_justbefore1 bin_mleader_justafter1 bin_mleader2_aftermore1) b(3) se(3) ///
star(+ 0.1 * 0.05 ** 0.01) label booktabs replace   ///
nomtitles ///
mgroup("Growth at \(t+2\) (last year=100)", pattern(1 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe  econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0  0 0 2 0 0) )



lincom bin_mleader_justafter1-bin_mleader_justbefore1




********************************************************************************************
*** Table A.15: How Performance Premium Varies with Patrons’ and Clients’ Career Prospects ***
********************************************************************************************




eststo clear

eststo m1: xtreg F2.gdpidx   $fe $econ  msec_female mayor_female msec_edu mayor_edu ///
 m*in1  m*in3 m*in5   connXclientage50-connXclientage58   if year>=2000  , fe   cluster(cityid)
  
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2011"

lincom connXclientage56-connXclientage57
lincom connXclientage56-connXclientage58

eststo m2: xtreg F2.gdpidx   $fe $econ  $career    connXpsecage??  if year>=2000 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2011"
lincom connXpsecage64-connXpsecage65


esttab  m1 m2  using out.txt, nonote nogaps nobaselevels keep( connXclientage5? connXpsecage??  ) b(3) se(3) ///
star(* 0.1 ** 0.05 *** 0.01) label booktabs replace  nomtitles ///
 mgroup("GDP growth at \(t+2\)", pattern(1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe  econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0  0 0 2 0 0) )


**************************************************************
*** Table A.16: Performance Premium and Patron Preferences ***
**************************************************************


eststo clear
eststo m1: xtreg F2.gdpidx   $fe $econ  $career    bin_mleader2currentsec connXdominant   if year>=2000 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2011"

eststo m2: xtreg F2.gdpidx   $fe $econ  $career   bin_mleader2currentsec connXcorrupt   if year>=2000 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2011"

**** Use amount reported ****
eststo m3: xtreg F2.gdpidx   $fe $econ  $career    bin_mleader2currentsec connXcorrupt connXcorrupt_amount   if year>=2000 , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2011"


esttab  m1 m2 m3  using out.txt, nonote nogaps nobaselevels keep( bin_mleader2currentsec connXdominant connXcorrupt connXcorrupt_amount ) b(3) se(3) ///
star( * 0.05  ** 0.01) label booktabs replace  nomtitles ///
 mgroup("GDP growth at \(t+2\)", pattern(1 0  0 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe  econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0  0 0 2 0 0) )




************************************************************************** 
*** Table A.17: Effect of Connection on Fiscal and Legislative Effort ****
**************************************************************************

eststo m7: xtreg F2.growrev   $fe $econ    $career     bin_mleader2currentsec  if samp==1  , fe cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2011"

eststo m8: nbreg   F.laws   $fe  i.cityid $econ  $career   bin_mleader2currentsec      if samp==1   , robust cluster(cityid)
estadd local yfe "$\checkmark$"
estadd local cfe "$\checkmark$"
estadd local pxy "$\checkmark$"
estadd local econ "$\checkmark$"
estadd local career "$\checkmark$"
estadd local ncity `e(N_clust)'
estadd local period "2000-2012"

esttab  m7 m8 using out.txt, nonote nobaselevels keep( *bin_mleader2currentsec) b(3) se(3) ///
star(* 0.05 ** 0.01) label booktabs replace  mtitles("\(\% \Delta\) Revenue-GDP \((t+2)\)" "\# of laws \& directives issued  \((t+1)\)") ///
 mgroup("Fiscal Effort" "Legislative Effort", pattern(1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats( yfe  econ career  r2_a ncity N, labels("City and province-year FE" "City economic controls" "City leader controls" "Adjusted R$^2$" "Number of Cities" "Observations") fmt(0  0 0 2 0 0) )



// Replication codes for Table A.18 are available in "DO14_performance_and_promotion.do" //
