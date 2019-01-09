
			global dir "C:\Users\jgw12\Dropbox\Research\GWF book files"
			cd "$dir\Data\Final data files"
			
			
			use GWF,clear
			
			* Generate binary variables *
			gen milmerit_persB = milmerit_pers
			recode milmerit_persB (2=1) (1=0)  
			tsset gwf_caseid year
			gen newparty =support==1 & l.support==0
			gen yr = year if newparty==1
			egen yrs = max(yr), by(gwf_leaderid)
			tsset gwf_caseid year
			replace newparty=1 if l.newparty==1 & l.gwf_leaderid==gwf_leaderid & year==year[_n-1]+1
			gen createparty =militparty_new==1 | (newparty==1  & partyhistory_post==1) 
			
			* Label variables *
			global pvars1 = "partyexcom_pers partyrbr officepers createparty"
			global pvars2 = "milnotrial milmerit_persB paramil_pers sectyapp_pers"
			label var officepers "Appointments to high office"
			label var createparty "Create new party"
			label var partyexcom_pers "Party exec committee"
			label var partyrbr "Rubber stamp party"
			label var milmerit_persB "Military promotions"
			label var milnotrial "Military purge"
			label var sectyapp_pers "Security apparatus"
			label var paramil_pers "Paramilitary"
			set seed 2453456
			
			* IRT model *
			irt 2pl $pvars1 $pvars2 
			estat report $pvars1 $pvars2, byparm sort(b)
			predict pers_2pl, latent se(pers_se_2pl)
			
			* IRT plots *
			irtgraph iif  (sectyapp_pers,lcolor(blue)) (milmerit_pers,lcolor(red)) (milnotrial,lcolor(green)) ///
			(paramil_pers,lcolor(cyan)),legend(col(2) pos(6)) title(Security & military items) saving(t2,replace) ///
			ylab(,glcolor(gs15)) xtitle("Personalism ({&theta})")
			irtgraph iif  (officepers,lcolor(blue)) (partyexcom_pers,lcolor(red)) (partyrbr,lcolor(green)) ///
			(createparty,lcolor(cyan)),legend(col(2) pos(6))  title(Party & personnel items) saving(t1,replace) ///
			ylab(,glcolor(gs15)) xtitle("Personalism ({&theta})")
			gr combine t1.gph t2.gph, col(2)   ysize(5.5) xsize(9)  ycommon
			erase t1.gph
			erase t2.gph
			
			* Standardize, rescale *
			qui sum pers_2pl
			gen latent_personalism = (pers_2pl+abs(r(min))) / (r(max) - r(min))
			hist latent, bin(50)
			
			* Variance decomposition *
			qui xtset gwf_leaderid year
			qui xtsum `i'
			qui scalar sdb = r(sd_b)
			qui scalar sdw = r(sd_w)
			qui scalar vart= sdb + sdw
			qui scalar varr = sdw / vart
			scalar list sdw
			scalar list varr
			
			
