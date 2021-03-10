/*Ji and Jiang "Enlightened One-Party Rule? Ideological Differences between Chinese Communist Party Members and the Mass Public"
Replication File -- Tables 3, Appendix Tables. 
2019.4.19*/

clear all
set more off

*********************************************
* Table 3. Multivariate Regression Analysis *
*********************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

recode male (1=0)(0=1),gen(female)
label variable female "Female"

tab income5q,gen(income5q)
label variable income5q1 "Income level (reference: lowest 25\%)"
label variable income5q2 "\quad 25\%-50\%"
label variable income5q3 "\quad 50\%-75\%"
label variable income5q4 "\quad 75\%-100\%"
label variable income5q5 "\quad Income missing"

global baseline = "female age urban party income5q2 income5q3 income5q4 income5q5 i.survey i.proall"
eststo clear

eststo m1:reg social_value     $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m2:reg social_value     $baseline edulevel if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m3:reg political_value  $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m4:reg political_value  $baseline edulevel if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m5:reg intl_value       $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m6:reg intl_value       $baseline edulevel if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m7:reg modern_all_value $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m8:reg modern_all_value $baseline edulevel if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"

esttab m* using table3.txt, nonote nobaselevels b(4) se(4) ///
star(* 0.1 ** 0.05 *** 0.01) label booktabs replace  ///
mgroup("Social" "Political" "International" "Overall modern value", pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats(fe r2 N, labels("Province-survey FE" "R$^2$" "Observation") fmt(0 3 0 ) ) noconstant ///
nodepvars order(party edulevel female age urban income5q1 income5q2 income5q3 income5q4 income5q5) ///
nomtitles keep( party edulevel female age urban income5q1 income5q2 income5q3 income5q4 income5q5) 
eststo clear

*********************************
* Appendix A Summary Statistics *
*********************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

label variable social_value "Social value"
label variable party "Party"
label variable urban "Urban"

*Table A.1 Descriptive Statistics for CGSS2010
estpost tabstat a421-a424 social_value male-urban party party_yr cadre intellectual middle income5q if survey==1 ,statistics(count mean sd min max) columns(statistics)
esttab . using tablea1.txt, cells("count(label(Obs)) mean(label(Mean) fmt(3)) sd(label(SD) fmt(3)) min(label(Min)) max(label(Max))") replace noobs label booktabs nonum nomtitles

*Table A.2 Descriptive Statistics for CGSS2012
estpost tabstat a421-a424 social_value male-urban party party_yr cadre intellectual middle income5q if survey==3 ,statistics(count mean sd min max) columns(statistics)
esttab . using tablea2.txt, cells("count(label(Obs)) mean(label(Mean) fmt(3)) sd(label(SD) fmt(3)) min(label(Min)) max(label(Max))") replace noobs label booktabs nonum nomtitles

*Table A.3 Descriptive Statistics for CGSS2013
estpost tabstat a421-a424 social_value male-urban party party_yr cadre intellectual middle income5q if survey==4 ,statistics(count mean sd min max) columns(statistics)
esttab . using tablea3.txt, cells("count(label(Obs)) mean(label(Mean) fmt(3)) sd(label(SD) fmt(3)) min(label(Min)) max(label(Max))") replace noobs label booktabs nonum nomtitles

*Table A.4 Descriptive Statistics for CGSS2015
estpost tabstat a421-a424 social_value male-urban party party_yr cadre intellectual middle income5q if survey==6 ,statistics(count mean sd min max) columns(statistics)
esttab . using tablea4.txt, cells("count(label(Obs)) mean(label(Mean) fmt(3)) sd(label(SD) fmt(3)) min(label(Min)) max(label(Max))") replace noobs label booktabs nonum nomtitles

*Table A.5 Descriptive Statistics for CFPS2014
estpost tabstat qm1002-qm1103 social_value male-urban party party_yr cadre intellectual middle income5q if survey==5 ,statistics(count mean sd min max) columns(statistics)
esttab . using tablea5.txt, cells("count(label(Obs)) mean(label(Mean) fmt(3)) sd(label(SD) fmt(3)) min(label(Min)) max(label(Max))") replace noobs label booktabs nonum nomtitles

*Table A.6 Descriptive Statistics for ABS3(2011)
estpost tabstat d3c-f19i g6-g8 social_value-modern_all_value male-urban party intellectual income5q xyfw-kx if survey==2 ,statistics(count mean sd min max) columns(statistics)
esttab . using tablea6.txt, cells("count(label(Obs)) mean(label(Mean) fmt(3)) sd(label(SD) fmt(3)) min(label(Min)) max(label(Max))") replace noobs booktabs nonum nomtitles

*Table A.7 Descriptive Statistics for ABS4(2015)
estpost tabstat d3c-f19i h5-h7 social_value-modern_all_value male-urban party party_yr cadre intellectual middle income5q xyfw-kx if survey==7 ,statistics(count mean sd min max) columns(statistics)
esttab . using tablea7.txt, cells("count(label(Obs)) mean(label(Mean) fmt(3)) sd(label(SD) fmt(3)) min(label(Min)) max(label(Max))") replace noobs booktabs nonum nomtitles

*********************************
* Appendix D Weight Calibration *
*********************************

*Table A.11: Weight Calibration Result and the Cost
use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

*raked weights for cgss2010
recode party (0=1)(1=2),gen(partyrw)
gen fe=male==0 & party==1
recode fe (0=1)(1=2),gen(ferw)
gen daz=edulevel==4 & party==1
recode daz (0=1)(1=2),gen(dazrw)
recode age (17/35=1)(36/99=2),gen(agegprw)
replace agegprw=2 if party==0

generate byte _one = 1
matrix Census_partyrw =(11075,705)
matrix colnames Census_partyrw = 1 2
matrix coleq    Census_partyrw = _one
matrix rownames Census_partyrw = partyrw

matrix Census_ferw =(11622,158)
matrix colnames Census_ferw = 1 2
matrix coleq    Census_ferw = _one
matrix rownames Census_ferw = ferw

matrix Census_dazrw =(11518,262)
matrix colnames Census_dazrw = 1 2
matrix coleq    Census_dazrw = _one
matrix rownames Census_dazrw = dazrw

matrix Census_agegprw =(171,11609)
matrix colnames Census_agegprw = 1 2
matrix coleq    Census_agegprw = _one
matrix rownames Census_agegprw = agegprw

matrix list Census_partyrw, f(%10.0g)
matrix list Census_ferw, f(%10.0g)
matrix list Census_dazrw, f(%10.0g)
matrix list Census_agegprw, f(%10.0g)

ipfraking [pw=wcn] if survey==1, generate(wcn_cgss2010) ctotal(Census_partyrw Census_ferw Census_dazrw Census_agegprw) nograph

*raked weights for cgss2012
capture drop _one
generate byte _one = 1
matrix Census_partyrw =(11025,740)
matrix colnames Census_partyrw = 1 2
matrix coleq    Census_partyrw = _one
matrix rownames Census_partyrw = partyrw

matrix Census_ferw =(11589,176)
matrix colnames Census_ferw = 1 2
matrix coleq    Census_ferw = _one
matrix rownames Census_ferw = ferw

matrix Census_dazrw =(11469,296)
matrix colnames Census_dazrw = 1 2
matrix coleq    Census_dazrw = _one
matrix rownames Census_dazrw = dazrw

matrix Census_agegprw =(189,11576)
matrix colnames Census_agegprw = 1 2
matrix coleq    Census_agegprw = _one
matrix rownames Census_agegprw = agegprw

matrix list Census_partyrw, f(%10.0g)
matrix list Census_ferw, f(%10.0g)
matrix list Census_dazrw, f(%10.0g)
matrix list Census_agegprw, f(%10.0g)

ipfraking [pw=wcn] if survey==3, generate(wcn_cgss2012) ctotal(Census_partyrw Census_ferw Census_dazrw Census_agegprw) nograph

*raked weights for cgss2013
capture drop _one
generate byte _one = 1
matrix Census_partyrw =(10708,729)
matrix colnames Census_partyrw = 1 2
matrix coleq    Census_partyrw = _one
matrix rownames Census_partyrw = partyrw

matrix Census_ferw =(11260,177)
matrix colnames Census_ferw = 1 2
matrix coleq    Census_ferw = _one
matrix rownames Census_ferw = ferw

matrix Census_dazrw =(11134,303)
matrix colnames Census_dazrw = 1 2
matrix coleq    Census_dazrw = _one
matrix rownames Census_dazrw = dazrw

matrix Census_agegprw =(188,11249)
matrix colnames Census_agegprw = 1 2
matrix coleq    Census_agegprw = _one
matrix rownames Census_agegprw = agegprw

matrix list Census_partyrw, f(%10.0g)
matrix list Census_ferw, f(%10.0g)
matrix list Census_dazrw, f(%10.0g)
matrix list Census_agegprw, f(%10.0g)

ipfraking [pw=wcn] if survey==4, generate(wcn_cgss2013) ctotal(Census_partyrw Census_ferw Census_dazrw Census_agegprw) nograph

*raked weights for cgss2015
capture drop _one
generate byte _one = 1
matrix Census_partyrw =(10228,706)
matrix colnames Census_partyrw = 1 2
matrix coleq    Census_partyrw = _one
matrix rownames Census_partyrw = partyrw

matrix Census_ferw =(10757,177)
matrix colnames Census_ferw = 1 2
matrix coleq    Census_ferw = _one
matrix rownames Census_ferw = ferw

matrix Census_dazrw =(10621,313)
matrix colnames Census_dazrw = 1 2
matrix coleq    Census_dazrw = _one
matrix rownames Census_dazrw = dazrw

matrix Census_agegprw =(179,10755)
matrix colnames Census_agegprw = 1 2
matrix coleq    Census_agegprw = _one
matrix rownames Census_agegprw = agegprw

matrix list Census_partyrw, f(%10.0g)
matrix list Census_ferw, f(%10.0g)
matrix list Census_dazrw, f(%10.0g)
matrix list Census_agegprw, f(%10.0g)

ipfraking [pw=wcn] if survey==6, generate(wcn_cgss2015) ctotal(Census_partyrw Census_ferw Census_dazrw Census_agegprw) nograph

*raked weights for abs2015
capture drop _one
generate byte _one = 1
matrix Census_partyrw =(3766,260)
matrix colnames Census_partyrw = 1 2
matrix coleq    Census_partyrw = _one
matrix rownames Census_partyrw = partyrw

matrix Census_ferw =(3961,65)
matrix colnames Census_ferw = 1 2
matrix coleq    Census_ferw = _one
matrix rownames Census_ferw = ferw

matrix Census_dazrw =(3911,115)
matrix colnames Census_dazrw = 1 2
matrix coleq    Census_dazrw = _one
matrix rownames Census_dazrw = dazrw

matrix Census_agegprw =(66,3960)
matrix colnames Census_agegprw = 1 2
matrix coleq    Census_agegprw = _one
matrix rownames Census_agegprw = agegprw

matrix list Census_partyrw, f(%10.0g)
matrix list Census_ferw, f(%10.0g)
matrix list Census_dazrw, f(%10.0g)
matrix list Census_agegprw, f(%10.0g)

ipfraking [pw=wcn] if survey==7, generate(wcn_abs2015) ctotal(Census_partyrw Census_ferw Census_dazrw Census_agegprw) nograph

*raked weights for abs2011
capture drop _one
generate byte _one = 1
matrix Census_partyrw =(3243,229)
matrix colnames Census_partyrw = 1 2
matrix coleq    Census_partyrw = _one
matrix rownames Census_partyrw = partyrw

matrix Census_ferw =(3415,57)
matrix colnames Census_ferw = 1 2
matrix coleq    Census_ferw = _one
matrix rownames Census_ferw = ferw

matrix Census_dazrw =(3371,101)
matrix colnames Census_dazrw = 1 2
matrix coleq    Census_dazrw = _one
matrix rownames Census_dazrw = dazrw

matrix Census_agegprw =(58,3414)
matrix colnames Census_agegprw = 1 2
matrix coleq    Census_agegprw = _one
matrix rownames Census_agegprw = agegprw


matrix list Census_partyrw, f(%10.0g)
matrix list Census_ferw, f(%10.0g)
matrix list Census_dazrw, f(%10.0g)
matrix list Census_agegprw, f(%10.0g)

ipfraking [pw=wcn] if survey==2, generate(wcn_abs2011) ctotal(Census_partyrw Census_ferw Census_dazrw Census_agegprw) nograph

*raked weights for cfps2014
use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

recode party (0=1)(1=2),gen(partyrw)
gen fe=male==0 & party==1
recode fe (0=1)(1=2),gen(ferw)
gen daz=edulevel==4 & party==1
recode daz (0=1)(1=2),gen(dazrw)
recode age (18/35=1)(16 17 36/104=2),gen(agegprw)
replace agegprw=2 if party==0
sum partyrw ferw dazrw agegprw

generate byte _one = 1
matrix Census_partyrw =(32502,2229)
matrix colnames Census_partyrw = 1 2
matrix coleq    Census_partyrw = _one
matrix rownames Census_partyrw = partyrw

matrix Census_ferw =(34181,550)
matrix colnames Census_ferw = 1 2
matrix coleq    Census_ferw = _one
matrix rownames Census_ferw = ferw

matrix Census_dazrw =(33772,959)
matrix colnames Census_dazrw = 1 2
matrix coleq    Census_dazrw = _one
matrix rownames Census_dazrw = dazrw

matrix Census_agegprw =(571,34160)
matrix colnames Census_agegprw = 1 2
matrix coleq    Census_agegprw = _one
matrix rownames Census_agegprw = agegprw


matrix list Census_partyrw, f(%10.0g)
matrix list Census_ferw, f(%10.0g)
matrix list Census_dazrw, f(%10.0g)
matrix list Census_agegprw, f(%10.0g)

ipfraking [pw=wcn] if survey==5, generate(wcn_cfps2014) ctotal(Census_partyrw Census_ferw Census_dazrw Census_agegprw) nograph

**********************************************
* Appendix E Item Parameters from IRT Models *
**********************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

*Table A.12: Discrimination of IRT model in ABS 3,4
*ABS 4
irt grm d3c-c20h  [pw=wcn2] if survey==7, intmethod(ghermite)
estat report, byparm

irt grm f22a-f19i [pw=wcn2] if survey==7, intmethod(ghermite)
estat report, byparm

irt grm h5-h7     [pw=wcn2] if survey==7, intmethod(ghermite)
estat report, byparm

*ABS 3
irt grm d3c-c20h  [pw=wcn2] if survey==2, intmethod(ghermite)
estat report, byparm

irt grm f22a-f19i [pw=wcn2] if survey==2, intmethod(ghermite)
estat report, byparm

irt grm g6-g8     [pw=wcn2] if survey==2, intmethod(ghermite)
estat report, byparm

**Note: Copy the discrimination coefficient and 95% confidence interval**

************************
* Appendix G.1 Surveys *
************************

*Table A.13: Party Member’s Demographic Traits
use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

recode male (1=0)(0=1),gen(female)
label variable female "Female"

tab income5q,gen(income5q)
label variable income5q1 "Income level (reference: lowest 25\%)"
label variable income5q2 "\quad 25\%-50\%"
label variable income5q3 "\quad 50\%-75\%"
label variable income5q4 "\quad 75\%-100\%"
label variable income5q5 "\quad Income missing"

global baseline = "female age edulevel urban income5q2 income5q3 income5q4 income5q5 i.proall"

eststo clear
eststo m1:reg party $baseline [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m2:reg party $baseline if survey==1 [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m3:reg party $baseline if survey==2 [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m4:reg party $baseline if survey==3 [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m5:reg party $baseline if survey==4 [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m6:reg party $baseline if survey==5 [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m7:reg party $baseline if survey==6 [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m8:reg party $baseline if survey==7 [pw=wcn2],vce(cluster proall)
estadd local fe "$\checkmark$"
 
esttab m* using tablea13.txt, nonote nobaselevels b(4) se(4) ///
star(* 0.1 ** 0.05 *** 0.01) label booktabs replace  ///
nonumbers mtitles("All" "CGSS2010" "ABS3(2011)" "CGSS2012" "CGSS2013" "CFPS2014" "CGSS2015" "ABS4(2015)") ///
stats(fe r2 N, labels("Province FE" "R$^2$" "Observation") fmt(0 3 0 ) ) noconstant ///
nodepvars order(age female edulevel urban income5q1 income5q2 income5q3 income5q4 income5q5) ///
keep(age female edulevel urban income5q1 income5q2 income5q3 income5q4 income5q5)
eststo clear

*******************************************************
* Appendix I Numerical Results for Predicted Ideology *
*******************************************************

*Table A.15: Heterogeneous Party Effect
use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear
g college=edulevel==4
recode male (1=0)(0=1),gen(female)
label variable female "Female"
label variable college "College"

tab income5q,gen(inc5)
gen inc523=0
replace inc523=1 if inc52==1
replace inc523=1 if inc53==1
label variable inc51 "0\%-25\%"
label variable inc54 "75\%-100\%"
label variable inc55 "Income missing"
label variable inc52 "Party membership \(\times\) Income level"
label variable inc53 "Income level"

eststo clear
eststo m1: reg social_value     i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m2: reg political_value  i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m3: reg intl_value       i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m4: reg modern_all_value i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
estadd local fe "$\checkmark$"

esttab m* using tablea15.txt,varwidth(25) interaction(" \(\times\) ") nonote nobaselevels b(4) se(4) nogap /// 
order(1.party 1.party#c.female 1.party#c.age 1.party#c.college 1.party#c.urban inc52 1.party#1.inc51 1.party#1.inc54 1.party#1.inc55 female age college urban inc53 1.inc51 1.inc54 1.inc55) ///
keep (1.party 1.party#c.female 1.party#c.age 1.party#c.college 1.party#c.urban inc52 1.party#1.inc51 1.party#1.inc54 1.party#1.inc55 female age college urban inc53 1.inc51 1.inc54 1.inc55) ///
nonumbers mtitles("Social" "Political" "International" "Overall value") ///
star(* 0.1 ** 0.05 *** 0.01) label booktabs replace ///
stats(fe r2  N, labels("Province-survey FE" "R$^2$" "Observation") fmt(0 3 0 ) ) noconstant

********************************
* Appendix J Robustness Checks *
********************************

*Table A.16: Testing the Impact of Social Desirability Bias
use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

eststo clear
eststo:reg social_value     yl if party==0  [pw=wcn2],robust
eststo:reg social_value     yl if party==1  [pw=wcn2],robust
eststo:reg political_value  yl if party==0  [pw=wcn2],robust
eststo:reg political_value  yl if party==1  [pw=wcn2],robust
eststo:reg intl_value       yl if party==0  [pw=wcn2],robust
eststo:reg intl_value       yl if party==1  [pw=wcn2],robust
eststo:reg modern_all_value yl if party==0  [pw=wcn2],robust
eststo:reg modern_all_value yl if party==1  [pw=wcn2],robust

esttab using tablea16.txt, nonote nobaselevels   b(4) se(4) ///
star(* 0.1 ** 0.05 *** 0.01) label booktabs replace ///
mtitle("Non-party" "Party" "Non-party" "Party" "Non-party" "Party" "Non-party" "Party" ) mgroup("Social" "Political" "International" "Overall modern value", pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats(r2  N, labels("R$^2$" "Observation") fmt(3 0 ) ) noconstant
eststo clear

*Table A.17: Robustness of Multivariate Analysis
use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

recode male (1=0)(0=1),gen(female)
label variable female "Female"

tab edulevel,gen(edulevel)
label variable edulevel1 "Education level"
label variable edulevel2 "\quad Middle school"
label variable edulevel3 "\quad High school"
label variable edulevel4 "\quad College and above"

tab income5q,gen(income5q)
label variable income5q1 "Income level (reference: lowest 25\%)"
label variable income5q2 "\quad 25\%-50\%"
label variable income5q3 "\quad 50\%-75\%"
label variable income5q4 "\quad 75\%-100\%"
label variable income5q5 "\quad Income missing"

global baseline = "female age urban party income5q2 income5q3 income5q4 income5q5 i.survey i.proall"
eststo clear

eststo m1:reg social_value     $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m2:reg social_value     $baseline edulevel2 edulevel3 edulevel4 if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m3:reg political_value  $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m4:reg political_value  $baseline edulevel2 edulevel3 edulevel4 if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m5:reg intl_value       $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m6:reg intl_value       $baseline edulevel2 edulevel3 edulevel4 if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m7:reg modern_all_value $baseline          if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"
eststo m8:reg modern_all_value $baseline edulevel2 edulevel3 edulevel4 if edulevel!=. ,vce(cluster proall)
estadd local fe "$\checkmark$"

esttab m* using tablea17.txt, nonote nobaselevels b(4) se(4) ///
star(* 0.1 ** 0.05 *** 0.01) label booktabs replace  ///
mgroup("Social" "Political" "International" "Overall modern value", pattern(1 0 1 0 1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
stats(fe r2 N, labels("Province-survey FE" "R$^2$" "Observation") fmt(0 3 0 ) ) noconstant ///
nodepvars order(party edulevel1 edulevel2 edulevel3 edulevel4 female age urban income5q1 income5q2 income5q3 income5q4 income5q5) ///
nomtitles keep( party edulevel1 edulevel2 edulevel3 edulevel4 female age urban income5q1 income5q2 income5q3 income5q4 income5q5) 
eststo clear
