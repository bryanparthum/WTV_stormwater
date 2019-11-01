** Written by: Bryan Parthum; bparthum@gmail.com ; July 2018

********************************************************************************
*******************************************************   Reported Non-Zero Wage 
**********************************************************************   Table 5 		
********************************************************************************

clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

**********************************
*************   Table 5 - Column 1
**********************************	

** Load data
use data/chicago_money, clear
append using data/portland_money

** Drop if wage is less than 8 or more than 500
drop if wage<8 | wage>500

** Generate negative term 
gen nmoney = -money	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm nmoney"

qui{
global model "tab_5_col_1"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 5 - Column 2
**********************************	

** Load data
use data/chicago_time, clear
append using data/portland_time

** Drop if wage is less than 8 or more than 500
drop if wage<8 | wage>500

** Generate negative term 
gen ntime = -time	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm ntime"

qui{
global model "tab_5_col_2"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 5 - Column 3
**********************************	

** Load data
use data/chicago_time, clear
append using data/portland_time

** Drop if wage is less than 8 or more than 500
drop if wage<8 | wage>500

** Generate time interaction with wage 
gen wage_time = wage*time	

** Generate negative term 
gen ntime = -time	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm wage_time ntime"

qui{
global model "tab_5_col_3"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 5 - Column 4
**********************************	

** Load data
use data/chicago_time, clear
append using data/portland_time

** Drop if wage is less than 8 or more than 500
drop if wage<8 | wage>500

** Generate monetized time
gen wage_time = ((1/3)*wage)*time	

** Generate negative term 
gen ncost = -wage_time	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm ncost"

qui{
global model "tab_5_col_4"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}


**********************************
*************   Table 5 - Column 5
**********************************	

** Load data
use data/pooled_money_time, clear

** Drop if wage is less than 8 or more than 500
drop if wage<8 | wage>500

** Generate monetized time
gen wage_time = ((1/3)*wage)*time	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Generate negative costs
gen nmoney = -money 
gen ntime  = -wage_time	
gen ncost = nmoney if time_survey==0 
replace ncost = ntime if time_survey==1

** Generate interactions with time survey
gen time_asc = time_survey*asc 
gen time_fld_pctdec = time_survey*fld_pctdec 
gen time_hd_exc = time_survey*hd_exc 
gen time_pld_swm = time_survey*pld_swm 
gen time_ncost = time_survey*ncost

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm time_asc time_fld_pctdec time_hd_exc time_pld_swm time_ncost ncost"

qui{
global model "tab_5_col_5"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace

mat b = e(b)
mat g = b*0.01
mat $model = b + g
matsave $model, replace p("matrices") s

}

********************************************************************************
***********************************  Table 5  **********************************
********************************************************************************

forv i = 1/5 {
est use estimates/tab_5_col_`i'
eststo tab_5_col_`i'
}

**********************************
**********  LIKELIHOOD RATIO TESTS
**********************************	
est use estimates/tab_5_col_5
eststo tab_5_col_5
sca ll_col5 = e(ll)
est use estimates/tab_5_col_1
eststo tab_5_col_1
sca ll_col1 = e(ll)
est use estimates/tab_5_col_4
eststo tab_5_col_4
sca ll_col4 = e(ll)

sca LRTS = 2*((ll_col1+ll_col4)-ll_col5)
chitable 34
di LRTS 

est use estimates/tab_5_col_5
estadd sca LR_tstat = LRTS
eststo tab_5_col_5

esttab tab_5_col_1 tab_5_col_2 tab_5_col_3 tab_5_col_4 tab_5_col_5 ///
	   using output\Table_5.rtf, replace ///
	   title(Table 5: Subsample with Reported Non-Zero Wage) ///
	   mtitles("Cost (dollars)" "Cost (hours)" "Hours Heterogeneity" "Monetized Time" "Pooled w/ Interactions") ///
	   scalars("chi2 LR chi2" "p Prob > chi2" "mcfr2 McF. R2" "aic AIC" "ll Log lik." "LR_tstat LR Tstat") ///
	   b(3) se(3) ///
	   label se 

** SD OF RANDOM PARAMETERS
forv i = 1/6 {
est use estimates/tab_5_col_`i'
mixlcov, sd
}


** END OF SCRIPT. Have a nice day! 
