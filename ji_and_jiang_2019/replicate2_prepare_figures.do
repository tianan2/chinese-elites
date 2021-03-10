/*Ji and Jiang "Enlightened One-Party Rule? Ideological Differences between Chinese Communist Party Members and the Mass Public"
Replication File -- Tables 3, Appendix Tables. 
2019.4.19*/

clear all
set more off

***************************************************************************************
* Figure 1. Sample Deviation from Population Statistics: Before and After Reweighting *
***************************************************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

recode age (18/35=1)(16 17 36/104=0),gen(age35)
replace age35=1 if survey!=3 & age==17
gen dz=party==1&edulevel==4

**origin survey weights result
mean party [aw=wcn],over(survey)
mean male [aw=wcn] if party==1,over(survey)
mean dz [aw=wcn] if party==1,over(survey)
mean age35 [aw=wcn] if party==1,over(survey)

**raked weights result
mean party [aw=wcn2],over(survey)
mean male [aw=wcn2] if party==1,over(survey)
mean dz [aw=wcn2] if party==1,over(survey)
mean age35 [aw=wcn2] if party==1,over(survey)

**Note: Above results minus Party Census figures are our final results, which can be found in Fig.xlsx (sheet name is "Figure 1").  

*******************************************************
* Figure 2: Main Result: Weighted Difference in Means *
*******************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

mean social_value     [aw=wcn2] if party==0,over(survey)
mean social_value     [aw=wcn2] if party==1,over(survey)

mean political_value  [aw=wcn2] if party==0,over(survey)
mean political_value  [aw=wcn2] if party==1,over(survey)

mean intl_value       [aw=wcn2] if party==0,over(survey)
mean intl_value       [aw=wcn2] if party==1,over(survey)

mean modern_all_value [aw=wcn2] if party==0,over(survey)
mean modern_all_value [aw=wcn2] if party==1,over(survey)

**Note: Above means and confidence interval are our results for figure 2, which can be found in Fig.xlsx (sheet name is "Figure 2").  

***************************************************
* Figure 3: Citizen-Party Member-Cadre Comparison *
***************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

mean social_value     [aw=wcn2] ,over(cate)
mean political_value  [aw=wcn2] ,over(cate)
mean intl_value       [aw=wcn2] ,over(cate)
mean modern_all_value [aw=wcn2] ,over(cate)

**Note: Above means and confidence interval are our results for figure 3, which can be found in Fig.xlsx (sheet name is "Figure 3").  

*************************************************************************************
* Figure 4: Party-Public Comparison in Subsamples Less Affected Social Desirability *
*************************************************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear
keep if survey== 2 | survey==7 

mean social_value     [aw=wcn2] if wflq==1,over(party)
mean political_value  [aw=wcn2] if wflq==1,over(party)
mean intl_value       [aw=wcn2] if wflq==1,over(party)
mean modern_all_value [aw=wcn2] if wflq==1,over(party)

mean social_value     [aw=wcn2] if ymxx==1,over(party)
mean political_value  [aw=wcn2] if ymxx==1,over(party)
mean intl_value       [aw=wcn2] if ymxx==1,over(party)
mean modern_all_value [aw=wcn2] if ymxx==1,over(party)

mean social_value     [aw=wcn2] if xyfw==1,over(party)
mean political_value  [aw=wcn2] if xyfw==1,over(party)
mean intl_value       [aw=wcn2] if xyfw==1,over(party)
mean modern_all_value [aw=wcn2] if xyfw==1,over(party)

mean social_value     [aw=wcn2] if yl==1,over(party)
mean political_value  [aw=wcn2] if yl==1,over(party)
mean intl_value       [aw=wcn2] if yl==1,over(party)
mean modern_all_value [aw=wcn2] if yl==1,over(party)

mean social_value     [aw=wcn2] if kx==1,over(party)
mean political_value  [aw=wcn2] if kx==1,over(party)
mean intl_value       [aw=wcn2] if kx==1,over(party)
mean modern_all_value [aw=wcn2] if kx==1,over(party)

**Note: Above means and confidence interval are our results for figure 4, which can be found in Fig.xlsx (sheet name is "Figure 4").  

****************************************
* Figure 5: Heterogenous Party Effects *
****************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

recode male (1=0)(0=1),gen(female)
gen college=edulevel==4
tab income5q,gen(inc5)
gen inc523=0
replace inc523=1 if inc52==1
replace inc523=1 if inc53==1

reg social_value     i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
margins party, over(college) 
margins party, over(female)
margins party, over(urban)
margins party, at(age=(30 45 60))
margins party, over(inc51)
margins party, over(inc523)
margins party, over(inc54)

reg political_value  i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
margins party, over(college) 
margins party, over(female)
margins party, over(urban)
margins party, at(age=(30 45 60))
margins party, over(inc51)
margins party, over(inc523)
margins party, over(inc54)

reg intl_value       i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
margins party, over(college) 
margins party, over(female)
margins party, over(urban)
margins party, at(age=(30 45 60))
margins party, over(inc51)
margins party, over(inc523)
margins party, over(inc54)

reg modern_all_value i.party##c.(female age college urban) i.party##i.inc51 i.party##i.inc54 i.party##i.inc55 i.survey i.proall, vce(cluster proall)
margins party, over(college) 
margins party, over(female)
margins party, over(urban)
margins party, at(age=(30 45 60))
margins party, over(inc51)
margins party, over(inc523)
margins party, over(inc54)

**Note: Above marginal mean, standard error and confidence interval are our results for figure 5, which can be found in Fig.xlsx (sheet name is "Figure 5").  

************************************
* Figure A.1: PCA and Cronbach’s α *
************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

*results in cgss 10 12 13 15
pca a421-a424 [aw=wcn2] if survey==1
alpha a421-a424 if survey==1
pca a421-a424 [aw=wcn2] if survey==3
alpha a421-a424 if survey==3
pca a421-a424 [aw=wcn2] if survey==4
alpha a421-a424 if survey==4
pca a421-a424 [aw=wcn2] if survey==6
alpha a421-a424 if survey==6

*results in cfps2014
pca qm1002-qm1103 [aw=wcn2] if survey==5
alpha qm1002-qm1103 if survey==5

*results in abs3(2011)
pca d3c-c20h [aw=wcn2] if survey==2
alpha d3c-c20h if survey==2
pca f22a-f19i [aw=wcn2] if survey==2
alpha f22a-f19i if survey==2
pca g6-g8 [aw=wcn2] if survey==2
alpha g6-g8 if survey==2
pca d3c-c20h f22a-f19i g6-g8 [aw=wcn2] if survey==2
alpha d3c-c20h f22a-f19i g6-g8 if survey==2

*results in abs4(2015)
pca d3c-c20h [aw=wcn2] if survey==7
alpha d3c-c20h if survey==7
pca f22a-f19i [aw=wcn2] if survey==7
alpha f22a-f19i if survey==7
pca h5-h7 [aw=wcn2] if survey==7
alpha h5-h7 if survey==7
pca d3c-c20h f22a-f19i h5-h7 [aw=wcn2] if survey==7
alpha d3c-c20h f22a-f19i h5-h7 if survey==7

**Note: Above Eigenvalue, Proportion and Cronbach’s α are our results for figure A.1, which can be found in Fig.xlsx (sheet name is "Figure A.1").  

***********************************************************
* Figure A.4: Party-Public Value Differences (No Weights) *
***********************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

mean social_value      if party==0,over(survey)
mean social_value      if party==1,over(survey)

mean political_value   if party==0,over(survey)
mean political_value   if party==1,over(survey)

mean intl_value        if party==0,over(survey)
mean intl_value        if party==1,over(survey)

mean modern_all_value  if party==0,over(survey)
mean modern_all_value  if party==1,over(survey)

**Note: Above means and confidence interval are our results for figure A.4, which can be found in Fig.xlsx (sheet name is "Figure A.4").  

************************************************************************
* Figure A.5: Party-Public Value Differences (Original Survey Weights) *
************************************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

mean social_value     [aw=wcn] if party==0,over(survey)
mean social_value     [aw=wcn] if party==1,over(survey)

mean political_value  [aw=wcn] if party==0,over(survey)
mean political_value  [aw=wcn] if party==1,over(survey)

mean intl_value       [aw=wcn] if party==0,over(survey)
mean intl_value       [aw=wcn] if party==1,over(survey)

mean modern_all_value [aw=wcn] if party==0,over(survey)
mean modern_all_value [aw=wcn] if party==1,over(survey)

**Note: Above means and confidence interval are our results for figure A.5, which can be found in Fig.xlsx (sheet name is "Figure A.5").  

**************************************************************************************************
* Figure A.6: Correlations between Social and Political Domains for Party Members and NonMembers *
**************************************************************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

pwcorr social_value political_value [aw=wcn2] if party==0
pwcorr social_value political_value [aw=wcn2] if party==1

*******************************************
* Figure A.7: By Year of Party Enrollment *
*******************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear

recode party_yr (1901/1949=1)(1950/1965=2)(1966/1976=3)(1977/1989=4)(1990/2002=5)(2003/2012=6)(2012/2017=7),gen(partyyrgrp)

mean social_value     [aw=wcn2] if party==1,over(partyyrgrp)
mean political_value  [aw=wcn2] if party==1,over(partyyrgrp)
mean intl_value       [aw=wcn2] if party==1,over(partyyrgrp)
mean modern_all_value [aw=wcn2] if party==1,over(partyyrgrp)

**Note: Above means are our results for figure A.7, which can be found in Fig.xlsx (sheet name is "Figure A.7").  

**********************************************************************************
* Figure A.8: Comparing China with Other East Asian Countries/Regions (ABS 2015) *
**********************************************************************************

*official released abs4_merge.dta is needed 
*use abs4_merge, clear
*merge m:m idnumber using "C:\云同步\desktop\abs4s.dta", keepusing(wcn2 party) nogenerate
*replace wcn2=. if country!=4
*replace w=wcn2 if country==4
*drop if w==.

use "C:\Users\JCY\Dropbox\Ideology\replicate\abs4_merge_party.dta"

keep q60 q61 q62 q66 q67 q130 q132 q133 q137 q138 q139 q142 q143 q144 q147 q148 q150 q151 q152 q153 country w party

recode q60 q61 q62 q66 q67 q130 q132 q133 q137 q138 q139 q142 q143 q144 q147 q148 (-1 7/9=.)
recode q150 (-1 7/9=.)(1=5)(2=4)(4=2)(5=1)
recode q151 q152 (-1 7/9=.)
recode q153 (1=4)(2=3)(3=2)(4=1)(-1 7/9=.)

irt grm q139 q60 q66 q61 q67 q62 [pw=w], intmethod(ghermite)
capture drop social
predict social, latent

irt grm q137 q138 q142 q143 q144 q147 q148 q130 q132 q133 [pw=w], intmethod(ghermite)
capture drop political
predict political, latent

irt grm q151 q152 q153 [pw=w], intmethod(ghermite)
capture drop intl
predict intl, latent

irt grm q60 q61 q62 q66 q67 q130 q132 q133 q137 q138 q139 q142 q143 q144 q147 q148 q151 q152 q153 [pw=w], intmethod(ghermite)
capture drop modern_all
predict modern_all, latent

egen social_value=std(social)
egen political_value=std(political)
egen intl_value=std(intl)
egen modern_all_value=std(modern_all)

mean social_value-modern_all_value [aw=w],over(country)
mean social_value-modern_all_value [aw=w],over(party)

**Note: Above means are our results for figure A.8, which can be found in Fig.xlsx (sheet name is "Figure A.8").  

**********************************************************************************
* Figure A.9: Comparing China with Other East Asian Countries/Regions (ABS 2011) *
**********************************************************************************

*official released abs3_merge.dta is needed 
*use "C:\Users\JCY\Dropbox\Ideology\data\abs3_merge.dta", clear
*merge m:m idnumber using "C:\云同步\desktop\abs3s.dta", keepusing(wcn2) nogenerate
*replace wcn2=. if country!=4
*replace allweight=wcn2 if country==4
*drop if allweight==.
*keep q55 q56 q57 q61 q62 q139 q129 q131 q132 q136 q137 q141 q142 q143 q146 q147 q151 q152 q153 country allweight party

use "C:\Users\JCY\Dropbox\Ideology\replicate\abs3_merge_party.dta",clear
recode q55 q56 q57 q61 q62 q139 (7/9 = .)
irt grm q55 q56 q57 q61 q62 q139 [pw=allweight], intmethod(ghermite)
capture drop social
predict social, latent

recode q129 q131 q132 q136 q137 q141 q142 q143 q146 q147 (7/9 = .)
irt grm q129 q131 q132 q136 q137 q141 q142 q143 q146 q147 [pw=allweight], intmethod(ghermite)
capture drop political
predict political, latent

recode q151 q152 q153 (7/9=.)
irt grm q151 q152 q153 [pw=allweight], intmethod(ghermite)
capture drop intl
predict intl, latent

irt grm q55 q56 q57 q61 q62 q139 q129 q131 q132 q136 q137 q141 q142 q143 q146 q147 q151 q152 q153 [pw=allweight], intmethod(ghermite)
capture drop modern_all
predict modern_all, latent

egen social_value=std(social)
egen political_value=std(political)
egen intl_value=std(intl)
egen modern_all_value=std(modern_all)

mean social_value-modern_all_value [aw=allweight],over(country)
mean social_value-modern_all_value [aw=allweight],over(party)

**Note: Above means are our results for figure A.9, which can be found in Fig.xlsx (sheet name is "Figure A.9").  

******************************************************************
* Figure A.10: Compare Party with Intellectuals and Middle-Class *
******************************************************************

use "C:\Users\JCY\Dropbox\Ideology\replicate\data.dta", clear
drop if survey==2
gen middlenoparty=0
replace middlenoparty=1 if middle==1 & party==0

gen intellectualnoparty =0
replace intellectualnoparty=1 if intellectual ==1 & party==0
gen rest=0
replace rest=1 if party==0 & middle==0 & intellectual==0

mean social_value [aw=wcn2] if rest==1
mean social_value [aw=wcn2] if party==1
mean social_value [aw=wcn2] if middlenoparty==1
mean social_value [aw=wcn2] if intellectualnoparty==1

mean political_value [aw=wcn2] if rest==1
mean political_value [aw=wcn2] if party==1
mean political_value [aw=wcn2] if middlenoparty==1 
mean political_value [aw=wcn2] if intellectualnoparty==1

mean intl_value [aw=wcn2] if rest==1
mean intl_value [aw=wcn2] if party==1
mean intl_value [aw=wcn2] if middlenoparty==1
mean intl_value [aw=wcn2] if intellectualnoparty==1

mean modern_all_value [aw=wcn2] if rest==1
mean modern_all_value [aw=wcn2] if party==1
mean modern_all_value [aw=wcn2] if middlenoparty==1
mean modern_all_value [aw=wcn2] if intellectualnoparty==1

**Note: Above means are our results for figure A.10, which can be found in Fig.xlsx (sheet name is "Figure A.10").  
