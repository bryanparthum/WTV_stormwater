** Written by: Bryan Parthum; bparthum@gmail.com ; July 2018

********************************************************************************
*******************************************************   Money and Time Surveys
**********************************************************************   Table 7
********************************************************************************

clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

**********************************
*************   Table 7 - Column 1
**********************************	 

** Load data
use data/chicago_money_and_time, clear
append using data/portland_money_and_time, gen(portland_sample)

** Generate negative monetized term 
gen nmoney = -money	
gen ntime  = -time

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm nmoney ntime"

** Estimate
qui{
global model "tab_7_col_1"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(2) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 7 - Column 2
**********************************	 

** Load data
use data/chicago_money_and_time, clear
append using data/portland_money_and_time, gen(portland_sample)

** Drop if wage is less than 8 or more than 500
drop if wage<8 | wage>500

** Generate negative monetized term 
gen nmoney = -money	
gen ntime  = -time

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm nmoney ntime"

** Estimate
qui{
global model "tab_7_col_2"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(2) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 7 - Column 3
**********************************	

** Load data
use data/chicago_money_and_time, clear
append using data/portland_money_and_time, gen(portland_sample)

** Drop if wage is less than 8 or more than 500
drop if wage<8 | wage>500

** Generate monetized time
gen wage_time = wage*time*1/3	

** Generate total cost (money + monetized time)
gen tcost = money + wage_time

** Generate negative cost term
gen ncost = -tcost	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm time ncost"

** Estimate
qui{
global model "tab_7_col_3"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

********************************************************************************
**********************************  LR TESTS  **********************************
********************************************************************************

forv i = 1/3 {
est use estimates/tab_7_col_`i'
sca ll_col`i' = e(ll)
}

forv i = 1/2{
sca LR_tstat_3v`i' = 2*(ll_col3-ll_col`i')
chitable 7
di LR_tstat_3v`i' 
est use estimates/tab_7_col_`i'
estadd sca LR_tstat = LR_tstat_3v`i'
eststo tab_7_col_`i'
est save estimates/tab_7_col_`i', replace
}

********************************************************************************
***********************************  Table 7  **********************************
********************************************************************************

forv i = 1/3 {
est use estimates/tab_7_col_`i'
est sto tab_7_col_`i'
}

esttab tab_7_col_1 tab_7_col_2 tab_7_col_3 ///
	   using output\Table_7.rtf, replace ///
	   title(Table 7: Regression Results on Data from Survey with Both Time and Money Costs) ///
	   mtitles("Whole Sample" "Non-zero Wage 1" "Non-zero Wage 2") ///
	   scalars("chi2 LR chi2" "p Prob > chi2" "mcfr2 McF. R2" "aic AIC" "ll Log lik.") ///
	   b(3) se(3) ///
	   label se 

** SD OF RANDOM PARAMETERS
forv i = 1/3 {
est use estimates/tab_7_col_`i'
mixlcov, sd
}

** END OF SCRIPT. Have a nice day! 