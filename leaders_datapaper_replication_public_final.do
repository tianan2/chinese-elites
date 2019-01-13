* LEAD II Descriptive Paper *
* Updated September 25, 2015 *

capture log close
clear all
set linesize 120
macro drop _all
clear all
set more off
set scheme s2color

* change directory to the directory with the file *

log using leaders_datapaper_replication.log, replace

*/ Note - the dataset contains one fix from the paper itself - a coding error - this has already been implemented in the dataset */
* replace militarycareer=0 if leadername=="Palma" & ccode==90

*/ Table 1 */

describe idacr ccode leaderid leadername birthyear deathyear startdate enddate year ccode age gender exit entry milservice combat rebel miledu warwin warloss rebelwin rebelloss primaryedu boarding leveledu onlychild firstborn middle lastborn firstson firstdau parstatus orphan illegit royalty dadwork momwork kidhealth married marriedinpower divorced spousesinlife sons daughters childtotal adopted childrendied teacher journalism law engineering medicine science agriculture militarycareer religion labor activist careerpolitician writer filmmusic economics aristocratlandowner police interpreter yrsexper puppet physhealth mentalhealth, f

*/ Table 2 */

sum idacr ccode leaderid leadername birthyear deathyear startdate enddate year ccode age gender exit entry milservice combat rebel miledu warwin warloss rebelwin rebelloss primaryedu boarding leveledu onlychild firstborn middle lastborn firstson firstdau parstatus orphan illegit royalty dadwork momwork kidhealth married marriedinpower divorced spousesinlife sons daughters childtotal adopted childrendied teacher journalism law engineering medicine science agriculture militarycareer religion labor activist careerpolitician writer filmmusic economics aristocratlandowner police interpreter yrsexper puppet physhealth mentalhealth, f

*/ Figure 1: Military and rebel experience */

graph bar milservice combat rebel miledu, percentage blabel(bar,format(%12.1f)) title("Military and Rebel Experience" "in the LEAD Dataset") nolabel legend(label(1 "National Military Service") label(2 "National Military Service, Combat") label(3 "Rebel Experience") label(4 "Military Education") size(small) symxsize(small) forcesize cols(3) rows(4) region(lcolor(white))) graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig1_military_exp.gph, replace
graph export fig1_military_exp.png, as(png) replace
graph export fig1_military_exp.eps, replace
graph save fig1_military_exp.gph, replace

*/ Figure 2: Military service and combat by region */

gen region = 0
replace region = 1 if ccode <= 20
replace region = 2 if ccode >= 31 & ccode <= 165
* replace region = 3 if ccode >= 80 & ccode <= 95
replace region = 4 if ccode >= 200 & ccode <= 395
replace region = 5 if ccode >= 400 & ccode <= 591
replace region = 6 if ccode >= 600 & ccode <= 699
replace region = 7 if ccode >= 700
label define region 1 "North America" 2 "Latin/South America" 3 "Central America" 4 "Europe" 5 "Africa" 6 "Middle East" 7 "Asia"

tab region

label values region region
label variable region `"Region of the World"'

sort region year

graph bar (mean) milservice combat, blabel(bar,format(%12.2f)) over(region, label(angle(45))) title("Leaders with Regular Military Experience" "by Region") nolabel legend(label(1 "National Military Service") label(2 "National Military Service, Combat") size(small) symxsize(small) forcesize cols(4) region(lcolor(white))) note("Bars represent the proportion of leaders within each region with military backgrounds") graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig2_mil_by_region.gph, replace
graph export fig2_mil_by_region.png, as(png) replace
graph export fig2_mil_by_region.eps, replace
graph save fig2_mil_by_region.gph, replace

*/ Figure 3: Military service by region */

egen milservice_m = mean(milservice), by(year) 
label variable milservice_m `"Military Service By Region"'

twoway line milservice_m year if region==1 || line milservice_m year if region==2 || line milservice_m year if region==4 || line milservice_m year if region==5 || line milservice_m year if region==6 || line milservice_m year if region==7 || fpfit milservice_m year, estopts(degree(3)) title("Regular Military Experience" "by Year and Region") legend(label(1 "North America") label(2 "Latin/South America") label(3 "Europe") label(4 "Africa") label(5 "Middle East") label(6 "Asia") label(7 "Cubic Fit") region(lcolor(white))) xlab(#10) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall)) graphregion(fcolor(white) lcolor(white) ilcolor(white)) plotregion(fcolor(white) lcolor(white) ilcolor(white)) 
graph export fig3_cubic_mil_by_region_year_all.png, as(png) replace
graph save fig3_cubic_mil_by_region_year_all.gph, replace
graph export fig3_cubic_mil_by_region_year_all.png, as(png) replace
graph export fig3_cubic_mil_by_region_year_all.eps, replace

*/ Figure 4: Rebels by region */

graph bar (mean) rebel, blabel(bar,format(%12.2f)) over(region, label(angle(45))) title("Leaders with Rebel Experience" "by Region") nolabel legend(label(1 "Rebel Experience") size(small) symxsize(small) forcesize cols(4) region(lcolor(white))) note("Bars represent the proportion of leaders within each region with rebel experience") graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig4_rebel_by_region.gph, replace
graph export fig4_rebel_by_region.png, as(png) replace
graph export fig4_rebel_by_region.eps, replace
graph save fig4_rebel_by_region, replace

*/ Figure 5: Education */

tab leveledu, gen(edu)

graph bar (mean) edu1 edu2 edu3 edu4, title("Levels of Education" "in the LEAD Dataset") nolabel legend(lab(1 "Primary")lab(2 "Secondary")lab(3 "University")lab(4 "Graduate") size(small) symxsize(small) forcesize cols(4) region(lcolor(white))) graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig5_level_education.gph, replace
graph export fig5_level_education.png, as(png) replace
graph export fig5_level_education.eps, replace
graph save fig5_level_education.gph, replace

*/ Figure 6: Birth order */

graph bar (mean) firstborn lastborn middle onlychild, title("Birth Order" "in the LEAD Dataset") nolabel legend(lab(1 "First Born Child")lab(2 "Last Born Child")lab(3 "Middle Born Child")lab(4 "Only Child")size(small) symxsize(small) forcesize cols(3) region(lcolor(white))) note("Percentages") graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig6_birth_order.gph, replace
graph export fig6_birth_order.png, as(png) replace
graph export fig6_birth_order.eps, replace
graph save fig6_birth_order.gph, replace

*/ Figure 7: Orphans */

tab orphanbinary, gen(orphan2)

graph bar orphan21 orphan22, percentage title("Orphans" "in the LEAD Dataset") nolabel legend(lab(1 "Not An Orphan")lab(2 "Orphan")size(small) symxsize(small) forcesize cols(3) region(lcolor(white))) graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph export fig7_orphan.png, as(png) replace
graph export fig7_orphan.eps, replace
graph save fig7_orphan.gph, replace

*/ Figure 8: Marriage and Divorce */

graph bar married marriedinpower divorced, title("Marriage and Divorce" "in the LEAD Dataset") nolabel legend(lab(1 "Married (Ever)")lab(2 "Married in Power")lab(3 "Divorced (Ever)") size(small) symxsize(small) forcesize cols(3) region(lcolor(white))) graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig8_married_divorced.gph, replace
graph export fig8_married_divorced.png, as(png) replace
graph export fig8_married_divorced.eps, replace
graph save fig8_married_divorced.gph, replace

*/ Figure 9: Children */

graph bar (mean) childtotal sons daughters childrendied, title("Average Number of Leader's Children" "in the LEAD Dataset") nolabel legend(lab(1 "Total Children")lab(2 "Sons")lab(3 "Daughters")lab(4 "Adopted Children")lab(5 "Children Died") size(small) symxsize(small) forcesize cols(5) region(lcolor(white))) graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig9_children.gph, replace
graph export fig9_children.png, as(png) replace
graph export fig9_children.eps, replace
graph save fig9_children.gph, replace

*/ Figure 10: Occupations */

graph bar (mean) careerpolitician law militarycareer teacher activist aristocratlandowner journalism writer economics labor engineering agriculture medicine religion police science filmmusic interpreter, title("Distribution of Occupation" "in the LEAD Dataset") nolabel legend(size(small) symxsize(small) forcesize cols(4) region(lcolor(white))) graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig10_occupations.gph, replace
graph export fig10_occupations.png, as(png) replace
graph export fig10_occupations.eps, replace
graph save fig10_occupations.gph, replace

*/ Figure 11: Military service by polity */

egen z2milservice = std(milservice)
egen z2combat = std(combat)
 
drop if polity < -10

graph bar (mean) z2milservice z2combat, over(polity, label(angle(45))) title("Leaders with Regular Military and Combat Experience" "by Polity Score") nolabel legend(label(1 "National Military Service") label(2 "National Military Service, Combat") size(small) symxsize(small) forcesize cols(4) region(lcolor(white))) note("Bars represent the proportion of leaders for each Polity IV score with military and combat backgrounds" "Variables Standardized") graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig11_polity_milservice.gph, replace
graph export fig11_polity_milservice.png, as(png) replace
graph export fig11_polity_milservice.eps, replace

*/ Figure 12: Rebel service by polity */

egen z2rebel = std(rebel)

graph bar (mean) z2rebel, over(polity, label(angle(45))) title("Leaders with Rebel Experience" "by Polity Score") nolabel legend(label(1 "Rebel Experience") label(2 "National Military Service, Combat") size(small) symxsize(small) forcesize cols(4) region(lcolor(white))) note("Bars represent the proportion of leaders for each Polity IV score with Rebel Experience" "Variables Standardized") graphregion(color(white)) ylabel( , nogrid) ytitle(Percenatage of Leaders) ytitle(, margin(medsmall))
graph save fig12_polity_rebel.gph, replace
graph export fig12_polity_rebel.png, as(png) replace
graph export fig12_polity_rebel.eps, replace

log close
