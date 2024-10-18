cd "C:\Users\VENMANSF\OneDrive - London School of Economics\Research projects\Litigation\"

//IMPORT EVENTS EXCELL SHEET
import excel "eventdates_Aug22_final.xlsx", sheet("GVKEY") firstrow clear 
destring gvkey, replace
keep gvkey name //gic_sector
duplicates drop gvkey , force 
save gvkey_names, replace

import excel "eventdates_Aug22_final", sheet("Decisions") firstrow clear 
save Decisions, replace

import excel "eventdates_Aug22_final", sheet("Filings") firstrow clear  
append using Decisions , generate(Decision)
replace region="1" if region=="North America"
replace region="2" if region== "Europe"
destring gvkey region,replace force
rename major_changes major
foreach xxx in novel courtlaw govplaintiff interest damages major {
	gen `xxx'w=`xxx'=="Yes" 
	replace `xxx'w=. if `xxx'==""
	}
replace novelw=novelw*2
replace courtlaww=courtlaww*3
replace interestw=interestw*2
replace damagesw=damagesw*3
replace majorw=majorw*3
//gen profile_sum= novelw+courtlaww+govplaintiffw+interestw+damagesw+majorw
duplicates drop date gvkey , force  
save Eventdates, replace

/*import excel "eventdates_Mar22.xlsx", sheet("Filings") firstrow clear
destring gvkey, replace
keep gvkey date oldrank
save Oldrank, replace */

//MERGE DATA
use returns.dta, clear  //returns_update for 21//11/2023 data (which are in original currency)
merge m:1 date region using factor_returns //factor_returns_update for 21//11/2023 data
drop if _merge==2
drop _merge
merge m:1 date gvkey using Eventdates
gen eventd=_merge==3
drop if _merge==2 
drop _merge
/*merge m:1 date gvkey using Oldrank
drop if _merge==2
drop _merge*/
merge m:1 gvkey using gvkey_names
drop if _merge==2
drop _merge
foreach xxx in novel courtlaw govplaintiff interest damages major {
gen `xxx'_d=1 if `xxx'w==1 |`xxx'w==2|`xxx'w==3 //create dummies for criteria with weights (missing if criteria is not met)
}
gen ReturnE=RET-rf
egen id=group(gvkey)
xtset id date
gen str case2=substr(case,1,50)
gen year=year(date)
drop novel courtlaw govplaintiff interest damages major
save MergedDataNatureSust, replace
