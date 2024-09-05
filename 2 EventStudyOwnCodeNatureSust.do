//OWN CODE FOR EVENTSTUDY
//Imitates eventstudy2, using a fixed estimation window before the event to define abnormal returns. 
//Code allows for weighted events (returns are weighted)
//For companies with several events, the database is expanded and each event is considered as independent 
//For sensitivity analysis, change eventstart, eventend, winsorise (line 18), factors line 78 86 (abnormal returns calculation) and 194 (single stage regresson)
cd "C:\Users\VENMANSF\OneDrive - London School of Economics\Research projects\Litigation\"
use MergedDataNatureSust, clear
//define parameters
	scalar eventstart=-1
	scalar eventend=1
	scalar eventlength=eventend-eventstart+1
	scalar estimationstart=-770
	scalar estimationend=-20
	scalar estimationlength=estimationend-estimationstart+1
	scalar min_est_length=125
//winsorize returns and oil price return
foreach xxx in RET { //add wti when data is available
	_pctile `xxx', p(99.5)
	replace `xxx'=r(r1) if `xxx'>r(r1)
	_pctile `xxx', p(0.5) 
	replace `xxx'=r(r1) if `xxx'<r(r1)
	}
	replace ReturnE=RET-rf 
//eliminate Peabody as outlier (in case there is no winsorizing)
	replace eventd=0 if name=="Peabody" & RET<-0.4
	replace impact=. if name=="Peabody" & RET<-0.4
//Expand database 
	bys id (date): gen  SumEvents=sum(eventd)   // if a company is filed 3 times, SumEvents is initially zero, switches to 1 on the first company event day (and following days),switches to 2 on the second company-event day,etc. 
	by  id: egen TotalEvents=total(eventd)     	//total number of events per company (constant)
	drop if TotalEvents==0
	expand TotalEvents, generate(dupl_temp)    	//if a company is filed 3 times, each of its observations is 3 times in the data, once with dupl_temp=0 and twice with dupl_temp=1
	bys id date (dupl_temp): gen duplicate=sum(dupl_temp) //dupl_temp is 0 for original obs and 1 for all duplicates; whereas duplicate is 0,1,2,3 if there are 4 events for 1 company
	egen id2=group(id duplicate) 				//id2 identifies company-events
	//make eventd indicate only one single event per duplicate, else 0
	bys id date (duplicate): replace eventd= eventd==1 & SumEvents==_n //on first event day, all duplicates will have SumEvents==1, on the second event day, all duplicates will have SumEvents==2 etc 
	//set event-related variables to missing for the non-relevant events. For example if a company sued 3 times, the event-related variable will be set to missing twice.
	foreach xxx in novelw courtlaww govplaintiffw interestw damagesw majorw novel_d courtlaw_d govplaintiff_d interest_d damages_d major_d  profile_sum Decision impact oldrank{
		replace `xxx'=. if eventd==0 
	}
//convert missings to the value on the day of the relevant company-event 
	foreach xxx in novel courtlaw govplaintiff interest damages major {
		bys id2: egen `xxx'd=max(`xxx'_d)
	}
	by id2: egen decision=max(Decision) //
	by id2: egen carbon_maj=max(carbon_major)
	by id2: egen oldrank_=min(oldrank)
	replace oldrank_=oldrank_==-2 //make oldrank_ a dummy , oldrank is the initial importance weighting in the database
	drop *_d Decision carbon_major oldrank

/*//Sensitivity analysis: make portfolios for the events with many companies. (comment this out in main approach)
	egen id3_temp=group(date) if eventd==1 //id3 is an identifier of a legal case (possibly against several companies)
	bys id2: egen id3= max(id3_temp) //this replaces all missings to id3 
	bys id3 date: egen RET_Portf=mean(RET) //following should give the same :bys date dif: egen RET_Portf2=mean(RET) 
	replace RET=RET_Portf
	by  id3 date: egen ReturnE_Portf=mean(ReturnE)
	replace ReturnE=ReturnE_Portf
	by id3 : egen carbon_maj_Portf=mean(carbon_maj)
	replace carbon_maj=carbon_maj_Portf>=0.5
	by id3 date: egen mcap_Portf=total(mcap)
	replace mcap=mcap_Portf
	drop id3_temp *Portf
	sort id3 date region id2 //if stocks from both regions are in a portfolio, use the marketportfolio of region 1 (US)
	//keep id id2 id3 date eventd profile_sum decision mcap carbon_maj region RET ReturnE mktrf smb hml wti decision noveld courtlawd govplaintiffd interestd damagesd majord impact name
	duplicates drop id3 date, force
	replace id2=id3 // */

//define event_window and estimation_window
	bys id2 (date): gen datenum=_n
	by  id2: gen  eventdate=datenum if eventd==1 //eventdatenum is missing for all obs except eventd 
	by  id2: egen eventdatenum=max(eventdate)    //eventdatenum is now a constant for each company-event 
	gen dif=datenum-eventdatenum				 //dif is 0 on the day of event, and increases (decreases) by 1 for each following (preceding) trading day
	gen event_window=	  dif>=eventstart      & dif<=eventend
	gen estimation_window=dif>=estimationstart & dif<=estimationend
	bys id2: egen estimation_length=total(estimation_window)
	drop if estimation_length<min_est_length
	keep if event_window==1 | estimation_window==1
	bys id2: egen eventyear=max(year(date))
//make weights 
	//event weighting(constant within id2)
    sum profile_sum if decision==0	//profile_sum is sum of all weightings for filings
	gen eventw=profile_sum/r(mean) //weight is standardaized to have mean=1. Originally, mean of weight is 9.98 median=12, p95=12, max=14	
	replace eventw=1 if impact==3  //impact is missing for filings
	replace eventw=-1 if impact==-3
	bys id2: egen weight=max(eventw) //if eventw is missing, max() gives the largest non-missing
	//Choose market cap instead of event weighting (will be a constant per id2 in the loop with results)
	gen ln_mcap=log10(mcap/1000) //
	qui sum ln_mcap if eventd==1 
	gen weight_ln_mcap=ln_mcap/r(mean) //standardize weights to one on average (for all companies). Not really necessary ln_mcap as weight should give the same

//make abnormal returns (residuals of regressions)
//Carbon Majors
	gen AR=.
	gen Beta=.
	quietly levelsof id2 if carbon_maj==1
	foreach i in `r(levels)' {   //faster: statsby, by(id2):reg ReturnE mktrf wti; predict AR2, residual   alternative asreg or rangestat but probaby no residuals. 
		display `i'
		quietly reg ReturnE mktrf if id2==`i' & estimation_window==1  //smb hml wti 
		quietly replace Beta=_b[mktrf] if id2==`i' & eventd==1
		quietly predict aRet      if id2==`i', residual
		quietly replace AR=aRet   if id2==`i'
		drop aRet
	}  
// non-Carbon Majors
	quietly levelsof id2 if carbon_maj==0
	foreach i in `r(levels)' {   
		quietly reg ReturnE mktrf if id2==`i' & estimation_window==1  //smb hml 
		quietly replace Beta=_b[mktrf] if id2==`i' & eventd==1
		quietly predict aRet      if id2==`i', residual
		quietly replace AR=aRet   if id2==`i'
		drop aRet
	}  
//replace wti=0 if carbon_maj==0 //required for regressions 
//cumulative returns & sd
	bys id2: egen CAR=total(AR) if event_window==1
	bys id2: egen AAR=mean(AR) if event_window==1
	by id2: egen Cum_Ret=total(RET) if event_window==1
	by id2: egen sd_AR_temp=sd(AR) if estimation_window==1
	by id2: egen sd_AR=max(sd_AR_temp)
	replace sd_AR=sd_AR*sqrt((estimation_length-1)/(estimation_length-4))
	gen SAR=AR/sd_AR
	gen ARxMCap=AR*mcap	*1e-9
//Prediction-error correction factor (Patell)
	by id2: egen Rm_mean_temp=mean(mktrf) if estimation_window==1
	by id2: egen Rm_mean=max(Rm_mean_temp)
	gen Rm_dev=mktrf-Rm_mean
	by id2: egen Rm_dev_sd_temp=sd(Rm_dev) if estimation_window==1
	by id2: egen Rm_dev_sd=max(Rm_dev_sd_temp)
	gen Rm_dev_sq=Rm_dev_sd^2*(estimation_length-1)
	gen Patell_corr=sqrt(1+1/estimation_length+(Rm_dev)^2/Rm_dev_sq)*sqrt((estimation_length-2)/(estimation_length-4)) //different factor for each obs
	gen SAR_patell=AR/sd_AR/Patell_corr  //standardized Abnormal Returns (standardized by company-specific sd and Patell correction facor for out-of sample variability)
	drop *_temp
//test individual company-events
	gen t_test=AAR/sd_AR*sqrt(eventlength) //CAR/sd_AR/sqrt(eventlength) is identical
	by id2: egen z_patell=mean(SAR_patell) if event_window==1
	replace z_patell=z_patell*sqrt(eventlength)
	by id2: egen z_patell2=total(SAR_patell) if event_window==1
	replace z_patell2=z_patell2/sqrt(eventlength)	//Identical 
//report data per company-events
	bys id2 (date): gen L1_RET=RET[_n-1]
	bys id2 (date): gen F1_RET=RET[_n+1]	
	sort decision date name
	list date decision carbon_maj noveld weight case2 name Cum_Ret CAR  z_patell  if eventd==1 //, ab(8) compress table
//save
	save MergedDataAll, replace


replace weight=1 if decision==1 & weight==. // there is one decision with missing information on positive or negative

//define empty matrix
matrix Results=J(56,9,.) // change dimensions here for a larger table
matrix colnames Results = #events CumReturn CumAbnRet Patell BMP t_EqVar CAR_regr t_reg t_reg2
matrix rownames Results = NegDec&Fil NegDec&FilCM NegDec&FilnC ///
	FilingsAll FilCM FilnC Fil_B19 Fil_S19 FilCM_B19 FilCM_S19 ///
	Novel NovelCM ///
	DecPos DecPosCM DecNeg DecNegCM DecNeg_B19 DecNeg_S19 ///
	Courtlaw Damages Govplaintiv Novel Interest ///
	CourtlawCM DamagesCM GovplaintivCM NovelCM InterestCM ///
	NegDec&Fil NegDec&FilCM NegDec&FilnC ///
	FilingsAll FilCM FilnC Fil_B19 Fil_S19 FilCM_B19 FilCM_S19 ///
	Novel NovelCM ///
	DecPos DecPosCM DecNeg DecNegCM DecNeg_B19 DecNeg_S19 ///
	Courtlaw Damages Govplaintiv Novel Interest ///
	CourtlawCM DamagesCM GovplaintivCM NovelCM InterestCM

local j=1
foreach w in weight_ln_mcap 1 {  //first loop is weighted, second loop is unweighted
foreach xxx in  "weight!=1" "weight!=1&carbon_maj==1" "weight!=1&carbon_maj==0" ///    //weight is 1 for positive decisions, -1 for neg decisions and a float between 0 and 1.43 for filings (but not 1)
	"decision==0" "decision==0&carbon_maj==1" "decision==0&carbon_maj==0" "decision==0&eventyear<2019" "decision==0&eventyear>=2019" "decision==0&carbon_maj==1&eventyear<2019" "decision==0&carbon_maj==1&eventyear>=2019" ///
	"noveld==1" "noveld==1&carbon_maj==1" ///
	"weight==1" "weight==1&carbon_maj==1" "weight==-1" "weight==-1&carbon_maj==1" "weight==-1&eventyear<2019" "weight==-1&eventyear>=2019" ///
	"courtlawd==1" "damagesd==1" "govplaintiffd==1" "noveld==1" "interestd==1" ///
	"courtlawd==1&carbon_maj==0" "damagesd==1&carbon_maj==0" "govplaintiffd==1&carbon_maj==0" "noveld==1&carbon_maj==0" "interestd==1&carbon_maj==0" { 
	display "`j'" " `xxx'" " `w'" 
	//make constant weights for each company-event 
		bys id2: egen temp__w=mean(`w') 	if estimation_window==1 &`xxx' 
		bys id2: egen __w=max(temp__w)		//set the same mean weight outside the estimation window (i.e. in the event window)
		quietly sum __w 					if event_window==1 &`xxx' 
		scalar N=r(N)						//number of companyevents times length event window
		scalar sum_w=r(sum)
	//make weighted variables	
		gen   AR__w=AR*__w 					if event_window==1 &`xxx' 
		gen   SAR__w=SAR*__w 				if event_window==1 &`xxx' 
		gen   SAR_patell__w=SAR_patell*__w 	if event_window==1 &`xxx' 
		gen   Cum_Ret__w=Cum_Ret*__w		if event_window==1 &`xxx' 
		gen   CAR__w=CAR*__w 				if event_window==1 &`xxx' 
	//t-test equal variances
		quietly sum AR 						if estimation_window==1 &`xxx' 
		gen sd_AR_equal__=r(sd)
		egen t_test_eq__w=total(AR__w) 		if event_window==1 &`xxx'  
		replace t_test_eq__w=sqrt(N)*t_test_eq__w/sum_w/sd_AR_equal__
	//t_test
		egen t_test__w=total(SAR__w) 		if event_window==1 &`xxx'  
		quietly sum  __w 					if event_window==1 &`xxx'  
		replace t_test__w=sqrt(r(N))*t_test__w/r(sum)    //r(N)=eventlength*#events (standardized AR has already standard deviation 1)
	//patell
		egen z_patell__w=total(SAR_patell__w) 	if event_window==1 &`xxx'  
		quietly sum __w 						if event_window==1 &`xxx' 
		replace z_patell__w=sqrt(r(N))*z_patell__w /r(sum)
	//BMP
		egen SAR_sd__=sd(SAR_patell)  			if event_window==1 &`xxx'
		gen z_BMP__w = z_patell__w  / SAR_sd__
		/*unweighted BMP
		egen z_BMP__=total(SAR_patell) 			if event_window==1 &`xxx'
		quietly sum __w 						if event_window==1 &`xxx' //r(N) is estimation lenght times number of companyevents.
		replace z_BMP__=z_BMP__ / r(N) * sqrt(r(N)) / SAR_sd__ */
	//fill in Table
		count if eventd==1  &`xxx' 
		matrix Results[`j',1]=r(N) 		
		quietly sum Cum_Ret__w
		matrix Results[`j',2]=r(mean)*100
		quietly sum CAR__w
		matrix Results[`j',3]=r(mean)*100
		//quietly sum t_test__w
		//matrix Results[`j',4]=r(mean)
		quietly sum z_patell__w
		matrix Results[`j',4]=r(mean)
		quietly sum z_BMP__w
		matrix Results[`j',5]=r(mean)
		quietly sum t_test_eq__w
		matrix Results[`j',6]=r(mean)


	//test for similar results when using regression (problem for results before after 2019, events of 2019 need data of 2018 in the estimation window.)
		reghdfe ReturnE event_window			 if `xxx'  [pweight=__w], absorb(id2#c.(mktrf ) id2) cluster(id2 id#date) //add smb hml wti //abnormal returns and coefficients are estimated in the same panel regression, very similar results
		matrix Results[`j',7]=_b[event_window]*eventlength*100
		matrix Results[`j',8]=_b[event_window]/_se[event_window]
		reghdfe ReturnE event_window			 if `xxx'  [pweight=__w], absorb(id2#c.(mktrf ) id2) cluster(id date)
		matrix Results[`j',9]=_b[event_window]/_se[event_window]
	//Total market value 
		//quietly sum ARxMCap 						if event_window==1 &`xxx' 
		//matrix Results[`j',8]=r(sum)

	drop *__*   
	local j=`j'+1
}
}
matrix list Results, format(%5.2f)

//////////////////Reviewer request on regression with all categories
foreach xxx in courtlawd damagesd govplaintiffd  noveld   interestd {
	replace `xxx'=(`xxx'==1&event_window==1)/eventlength
}
gen ref=(event_window==1)/eventlength
rename interestd spilloversd
reg AR  damagesd
reg AR  damagesd  noveld spilloversd 
reg AR  courtlawd damagesd govplaintiffd  noveld spilloversd 

//The following code gives indeed very similar results to what we report in the paper.
gen DecPos=(weight==1&event_window==1)/eventlength
gen DecNeg=(weight==-1&event_window==1)/eventlength
gen Filing=(decision==0&event_window==1)/eventlength
reg AR DecPos DecNeg Filing
	
///////////////////MORE HETEROGENEITY (unreported): 
//Make indicator for market cap below the median at time of the event
bys date :egen median_mcap=median(mcap)
gen small_=mcap<median_mcap & eventd==1
bys id2 : egen small=max(small) //indicator should be always 1 or 0 for all observations within a company-event
//Make indicator for negative return over the last 3 years (financially distressed company)
bys id2: egen TotalRet_=total (RET) if dif<-20 & dif>-770
by  id2: egen TotalRet=max(TotalRet_) //indicator should be always 1 or 0 for all observations within a company-event
gen TotalRetNeg=TotalRet<0
sort id2 date
//label sectors
destring gsector, replace
label define GIC 10 "Energy" 15 "Materials" 20 "Industrials" 25 "Consumer Discretionary" 30 "Consumer Staples" 40 "Financials" 55 "Utilities"
label values gsector GIC

