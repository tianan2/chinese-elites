


*************************************************************************
******************** Script for Data Pre-Processing  ********************
*************************************************************************

// Uncomment the following code to install the necessary package //
* ssc inst egenmore 

g msec_female= msec_sex=="女" 
g mayor_female= mayor_sex=="女" 

merge 1:1 cityid year using econ_panel // Merge with other city covariates
drop _merge


******* Connection variables **********

label var msec2currentsec "City secretary connected to prov sec"
label var mayor2currentsec "Mayor connected to prov sec"

g mleader2currentsec=msec2currentsec+mayor2currentsec
g bin_mleader2currentsec=(mleader2currentsec>0)
label var bin_mleader2currentsec "Connected to prov sec"

g bin_mleader2currentsec2=(msec2currentsec2==1|mayor2currentsec2==1)
label var bin_mleader2currentsec2 "Connected to prov sec (include promotions under governor)"

g bin_mleader2currentgvn=(msec2currentgvn==1|mayor2currentgvn==1)
label var bin_mleader2currentgvn "Connection to governor"

g bin_mleader2pleader=(bin_mleader2currentsec==1 | bin_mleader2currentgvn==1)
label var bin_mleader2pleader "Connected to prov sec or governor"



xtset cityid year
capture drop prior? post? conn?
g prior1=(bin_mleader2currentsec==0 & F.bin_mleader2currentsec>0)
g prior2=(bin_mleader2currentsec==0 & F.bin_mleader2currentsec==0 & F2.bin_mleader2currentsec>0)
g prior3=(bin_mleader2currentsec==0 & F.bin_mleader2currentsec==0 & F2.bin_mleader2currentsec==0 & F3.bin_mleader2currentsec>0)
g prior4=(bin_mleader2currentsec==0 & F.bin_mleader2currentsec==0 & F2.bin_mleader2currentsec==0 & F3.bin_mleader2currentsec==0 & F4.bin_mleader2currentsec>0)
g prior5=(bin_mleader2currentsec==0 & F.bin_mleader2currentsec==0 & F2.bin_mleader2currentsec==0 & F3.bin_mleader2currentsec==0 & F4.bin_mleader2currentsec==0 & F5.bin_mleader2currentsec>0)


g post1=(bin_mleader2currentsec==0 & L.bin_mleader2currentsec>0) 
g post2=(bin_mleader2currentsec==0 & L.bin_mleader2currentsec==0 & L2.bin_mleader2currentsec>0)
g post3=(bin_mleader2currentsec==0 & L.bin_mleader2currentsec==0 & L2.bin_mleader2currentsec==0 & L3.bin_mleader2currentsec>0)
g post4=(bin_mleader2currentsec==0 & L.bin_mleader2currentsec==0 & L2.bin_mleader2currentsec==0 & L3.bin_mleader2currentsec==0 & L4.bin_mleader2currentsec>0)


g conn1=(bin_mleader2currentsec==1 & L.bin_mleader2currentsec==0)
g conn2=(bin_mleader2currentsec==1 & L.bin_mleader2currentsec==1 & L2.bin_mleader2currentsec==0 )
g conn3=(bin_mleader2currentsec==1 & conn1==0 & conn2==0)



******* Age *******

// City leader age
g msecage=year-msec_birthyear
label var msecage "City secretary: Age"
g mayorage=year-mayor_birthyear
label var mayorage "Mayor: Age"

// Clients' age (minimum if two clients)
g clientage=msecage if msec2currentsec==1 & mayor2currentsec==0
replace clientage=mayorage if msec2currentsec==0 & mayor2currentsec==1
replace clientage=min(msecage,mayorage) if msec2currentsec==1 & mayor2currentsec==1
replace clientage=0 if bin_mleader2currentsec==0




local type psec gvn
foreach i of local type {
forv j=60/65{
g `i'_age`j'=(`i'_age>=`j' & `i'_age!=.)
}
}

recode mayor_rank msec_rank (1.5=1)(2.5=3) (.=2)

recode *finance *_prov_exp *_central_exp *_finance msec_edu mayor_edu  (.=0)

g msec_top_exp=msec_prov_exp==1|msec_central_exp==1
label var msec_top_exp "City sec: higher-level exp"

g mayor_top_exp=mayor_prov_exp==1|mayor_central_exp==1
label var mayor_top_exp "City mayor: higher-level exp"

label var msec_soe_exp "City sec: SOE career"
label var mayor_soe_exp "Mayor: SOE career"

label var msec_finance "City sec: finance/development career"
label var mayor_finance "Mayor: finance/development career"

g msec_localshare=msec_localtime/msecage*100
g mayor_localshare=mayor_localtime/mayorage*100

label var msec_localshare "City sec: \% of local career"
label var mayor_localshare "Mayor: \% of local career"


g msec_minor=(msec_ethnicity!="汉族")
label var msec_minor "City secretary: Ethnic minority"
g mayor_minor=(mayor_ethnicity!="汉族")
label var mayor_minor "Mayor: Ethnic minority"



capture drop msec_in?  mayor_in? 
g msec_in1=msec_tenure==0 
g msec_in3=msec_tenure<=2 &  msec_tenure>0
g msec_in5=msec_tenure<=4 & msec_tenure>2

g mayor_in1=mayor_tenure==0
g mayor_in3=mayor_tenure<=2 &  mayor_tenure>0
g mayor_in5=mayor_tenure<=4 & mayor_tenure>2



foreach i of varlist msec_tenure mayor_tenure{
g `i'3=`i'
replace `i'3=3 if `i'>=3 & `i'!=.

}



label def ten 0 "1 year" 1 "2 years" 2 "3 years" 3 "\(\geq\) 4 years"
label val  msec_tenure3 mayor_tenure3 ten



label var msec_tenure "City secretary's tenure"
label var mayor_tenure "Mayor's tenure"

g msec_tenure_sq=msec_tenure^2
g msec_tenure_cb=msec_tenure^3
label var msec_tenure_sq "City secretary: tenure\(^2\)"
label var msec_tenure_cb "City secretary: tenure\(^3\)"

g mayor_tenure_sq=mayor_tenure^2
g mayor_tenure_cb=mayor_tenure^3
label var mayor_tenure_sq "Mayor: tenure\(^2\)"
label var mayor_tenure_cb "Mayor: tenure\(^3\)"

/// years connected: secretary
g change=(msec2currentsec!=L.msec2currentsec)
by cityid: g conngroup=sum(change)
g count=1
egen cg=group(cityid conngroup)
bysort cg: g noconntime=sum(count) if msec2currentsec==0 
xtset cityid year
replace noconntime=0 if noconntime==. & F.noconntime==1
g final_conntime_msec=L.noconntime
label var final_conntime_msec "Years being unconnected (city secretary)"

/// years connected: mayor
capture drop change cg noconntime conngroup
g change=(mayor2currentsec!=L.mayor2currentsec)
by cityid: g conngroup=sum(change)

egen cg=group(cityid conngroup)
bysort cg: g noconntime=sum(count) if mayor2currentsec==0 
xtset cityid year
replace noconntime=0 if noconntime==. & F.noconntime==1
g final_conntime_mayor=L.noconntime
label var final_conntime_mayor "Years being unconnected (mayor)"


/// years unconnected
capture drop change cg noconntime conngroup
g change=(bin_mleader2currentsec!=L.bin_mleader2currentsec)
by cityid: g conngroup=sum(change)

egen cg=group(cityid conngroup)
bysort cg: g noconntime=sum(count) if bin_mleader2currentsec==0 
xtset cityid year
replace noconntime=0 if noconntime==. & F.noconntime==1
g final_conntime_either=L.noconntime
label var final_conntime_either "Years being unconnected"





// Pin down mode of departure by former prov sec

encode current_psec,gen(psec_id)

g lastpsec=L.psec_id!=psec_id
g lastpsec_age=L.psec_age if lastpsec==1
egen lastpsec_age_all=min(lastpsec_age),by(provid current_psec)
g lastpsec_retire=lastpsec_age_all==65|lastpsec_age_all==66|lastpsec_age_all==67


g conn_provretire=bin_mleader2currentsec==1 & lastpsec_retire==1   
label var conn_provretire "Connected to prov sec (last prov sec left at retirement age)"
g conn_provtransfer=bin_mleader2currentsec==1 & lastpsec_retire==0  
label var conn_provtransfer "Connected to prov sec (last prov sec left before retirement age)"


******* Patron Preferences *************

g connXdominant=bin_mleader2currentsec*psec_dominant
label var connXdominant "Connected to prov sec \(\times\) Prov sec connected to gen sec"
g psec_corrupt=current_psec=="周永康"|current_psec=="苏荣"|current_psec=="周本顺"|current_psec=="王珉"| ///
current_psec=="白恩培"|current_psec=="孙政才"|current_psec=="王三运" |current_psec=="程维高"|current_psec=="刘方仁"
g connXcorrupt=bin_mleader2currentsec*psec_corrupt 
label var psec_corrupt "Corrupt prov sec"
label var connXcorrupt "Connected to prov sec \(\times\) Corrupt prov sec"

g psec_corrupt_amount=0 if psec_corrupt==0
replace psec_corrupt_amount= 12000 if current_psec=="周永康"
replace psec_corrupt_amount= 19600 if current_psec=="苏荣"
replace psec_corrupt_amount= 4100 if current_psec=="周本顺"
replace psec_corrupt_amount= 4600 if current_psec=="王珉"
replace psec_corrupt_amount= 24600 if current_psec=="白恩培"
replace psec_corrupt_amount= 677 if current_psec=="刘方仁"

replace psec_corrupt_amount= . if current_psec=="孙政才"|current_psec=="王三运" |current_psec=="程维高" 
replace psec_corrupt_amount=psec_corrupt_amount/10000
g connXcorrupt_amount=bin_mleader2currentsec*psec_corrupt_amount
label var connXcorrupt_amount "Connected to prov sec \(\times\) Corrupt prov sec (amount, in million yuan)"


******** Clients' and Patrons' Age ********

capture drop connXclientage5?
g connXclientage50=clientage<=50 & clientage!=0 if clientage!=.
g connXclientage55=clientage<=55 & clientage>50 if clientage!=.
g connXclientage56=clientage==56 if clientage!=.
g connXclientage57=clientage==57 if clientage!=.
g connXclientage58=clientage>=58 if clientage!=.

label var connXclientage50 "Client age \(\leqslant\) 50"
label var connXclientage55 "Client age \(\in (50,55]\)"
label var connXclientage56 "Client age \(=\) 56"
label var connXclientage57 "Client age  \(=\) 57"
label var connXclientage58 "Client age  \(\geqslant\) 58"


capture drop connXpsecage??
g connXpsecage57=bin_mleader2currentsec*(psec_age<=57)
g connXpsecage62=bin_mleader2currentsec*(psec_age<=62 & psec_age>57)
g connXpsecage63=bin_mleader2currentsec*(psec_age==63)
g connXpsecage64=bin_mleader2currentsec*(psec_age==64)
g connXpsecage65=bin_mleader2currentsec*(psec_age>=65)

label var connXpsecage57 "Patron age \(\leqslant\) 57"
label var connXpsecage62 "Patron age \(\in (57, 62]\)"
label var connXpsecage63 "Patron age \(=\) 63"
label var connXpsecage64 "Patron age \(=\) 64"
label var connXpsecage65 "Patron age \(\geqslant\) 65"



******* Classify topics into themes (see LDA topic output for reference) *******
g prodevelopment=(G20_topic1+G20_topic3+G20_topic10+G20_topic13+G20_topic14+G20_topic15)/6
g prosocial=G20_topic18
g proculture=(G20_topic2+G20_topic4+G20_topic6+G20_topic17)/4
g proadminreform=(G20_topic0+G20_topic12)/2


**** Relative promotion timing *******
g msec_relyr=msec_firstymml-psec_arrival_aspsec_ym
g mayor_relyr=mayor_firstymml-psec_arrival_aspsec_ym

g msec_tr=msec_relyr/365
g mayor_tr=msec_relyr/365
g msec_tr0=floor(msec_tr)
g mayor_tr0=floor(mayor_tr)

forv i=1/3{
g msec_justbefore`i'=msec_tr0==-`i'
g mayor_justbefore`i'=mayor_tr0==-`i'
g msec_justafter`i'=msec_tr0==`i' & msec2currentsec==1
g mayor_justafter`i'=mayor_tr0==`i' & mayor2currentsec==1
}
g msec_justbefore_2more=msec_justbefore1==0 & msec2currentsec==0 & msec_tr<0
g mayor_justbefore_2more=mayor_justbefore1==0 & mayor2currentsec==0 & mayor_tr<0
g msec_justafter_2more=msec_justafter1==0 & msec2currentsec==1 & msec_tr>=0
g mayor_justafter_2more=mayor_justafter1==0  & mayor2currentsec==1  & mayor_tr>=0


///// Address the issue of attrion: look at those who are promoted **barely** before the current prov sec /////
g bin_mleader_justbefore1=(msec_justbefore1==1|mayor_justbefore1==1)  
g bin_mleader_justbefore2=(msec_justbefore2==1|mayor_justbefore2==1)  

g bin_mleader_justafter1=(msec_justafter1==1 & msec2currentsec==1)|(mayor_justafter1==1  & mayor2currentsec==1)
g bin_mleader2_aftermore1=bin_mleader2currentsec==1 & bin_mleader_justafter1==0

label var  bin_mleader_justbefore2 "Unconnected city leader (promoted \(\leqslant 2\) yrs)"
label var  bin_mleader_justbefore1 "Unconnected city leader (promoted \(\leqslant 1\) yr)"
label var  bin_mleader_justafter1 "Connected city leader (promoted in \(\leqslant\)1 yrs \emph{\text{after}} incumbent's arrival)"
label var  bin_mleader2_aftermore1 "Connected city leader (promoted \(>\)1 yrs \emph{\text{after}} incumbent's arrival)"





******** Regular vs. Irregular turnover ****************
xtset cityid year
local vars  msec_tenure mayor_tenure msecage mayorage
foreach i of local vars{
g L`i'=L.`i'
replace L`i'=`i' if L`i'==.
egen startmsec_`i'=first(L`i'),by(cityid msec)
egen startmayor_`i'=first(L`i'),by(cityid mayor)

}

capture drop *regular
g bin_mleader2currentsec_regular=((startmsec_msec_tenure>=4|startmsec_msecage>=57) & msec2currentsec==1)|((startmayor_mayor_tenure>=4|startmayor_mayorage>=57) & mayor2currentsec==1)
g bin_mleader2currentsec_irregular=bin_mleader2currentsec-bin_mleader2currentsec_regular
label var bin_mleader2currentsec_regular "Connected to prov sec (through regular turnover)"
label var bin_mleader2currentsec_irregular "Connected to prov sec (through irregular turnover)"

capture drop *first6
g bin_mleader2currentsec_nofirst6=(msec2currentsec==1 & msec_relyr>=180)|(mayor2currentsec==1 & mayor_relyr>=180)
g bin_mleader2currentsec_first6=(msec2currentsec==1 & msec_relyr<180)|(mayor2currentsec==1 & mayor_relyr<180)
label var bin_mleader2currentsec_first6 "Connected to prov sec (promoted within first 6 months)"
label var bin_mleader2currentsec_nofirst6 "Connected to prov sec (promoted after first 6 months)"

