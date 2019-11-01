** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
****************************************************   Main Results and WTP,WTV	
*********************************************************************   Table 2 
********************************************************************************
clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

********************************************************************************
*******************************  PANEL A - MONEY *******************************
********************************************************************************

**********************************
**************************   Clean
**********************************	

** Load data
use data/chicago_money, clear
append using data/portland_money

** Generate negative term 
gen nmoney = -money	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm nmoney"

**********************************
*************   Table 2 - Column 1
**********************************	
qui{
global model "tab_2_col_1_money"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 2 - Column 2
**********************************	
qui{
global model "tab_2_col_2_money"
mixlogit choice if chic_dum==1, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 2 - Column 3
**********************************	
qui{
global model "tab_2_col_3_money"
mixlogit choice if chic_dum==0, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

********************************************************************************
*******************************  PANEL B - TIME  *******************************
********************************************************************************

**********************************
**************************   Clean
**********************************	

** Load data
use data/chicago_time, clear
append using data/portland_time

** Generate negative term 
gen ntime = -time	

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm ntime"

**********************************
*************   Table 2 - Column 1
**********************************	
qui{
global model "tab_2_col_1_time"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 2 - Column 2
**********************************	
qui{
global model "tab_2_col_2_time"
mixlogit choice if chic_dum==1, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
*************   Table 2 - Column 3
**********************************	
qui{
global model "tab_2_col_3_time"
mixlogit choice if chic_dum==0, rand($randvars) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

********************************************************************************
***********************************  Table 2  **********************************
********************************************************************************

forv i = 1/3 {
est use estimates/tab_2_col_`i'_money
eststo tab_2_col_`i'_money
est use estimates/tab_2_col_`i'_time
eststo tab_2_col_`i'_time
}

**********************************
**********  LIKELIHOOD RATIO TESTS
**********************************	
est use estimates/tab_2_col_1_money
eststo tab_2_col_1_money
sca ll_col1 = e(ll)
est use estimates/tab_2_col_2_money
eststo tab_2_col_2_money
sca ll_col2 = e(ll)
est use estimates/tab_2_col_3_money
eststo tab_2_col_3_money
sca ll_col3 = e(ll)
est use estimates/tab_2_col_1_time
eststo tab_2_col_1_time
sca ll_col1_time = e(ll)
est use estimates/tab_2_col_2_time
eststo tab_2_col_2_time
sca ll_col2_time = e(ll)
est use estimates/tab_2_col_3_time
eststo tab_2_col_3_time
sca ll_col3_time = e(ll)

sca LRTS_money = 2*((ll_col2+ll_col3)-ll_col1)
sca LRTS_time = 2*((ll_col2_time+ll_col3_time)-ll_col1_time)
chitable 45
di LRTS_money 
di LRTS_time

est use estimates/tab_2_col_1_money
estadd sca LR_tstat = LRTS_money
eststo tab_2_col_1_money
est use estimates/tab_2_col_1_time
estadd sca LR_tstat = LRTS_time
eststo tab_2_col_1_time

**********************************
***************  Table 2 - PANEL A
**********************************	

esttab tab_2_col_1_money tab_2_col_2_money tab_2_col_3_money ///
	   using output\Table_2_money.rtf, replace ///
	   title(Table 2: Money) ///
	   mtitles("Both Cities" "Chicago" "Portland") ///
	   scalars("chi2 LR chi2" "p Prob > chi2" "mcfr2 McF. R2" "aic AIC" "ll Log lik." "LR_tstat LR_tstat") ///
	   b(3) se(3) ///
	   label se 

** STANDARD DEVIATION OF RANDOM PARAMETERS
forv i = 1/3 { 
est use estimates/tab_2_col_`i'_money
mixlcov, sd
}

**********************************
***************  Table 2 - PANEL B
**********************************	

esttab tab_2_col_1_time tab_2_col_2_time tab_2_col_3_time ///
	   using output\Table_2_time.rtf, replace ///
	   title(Table 2: Time) ///
	   mtitles("Both Cities" "Chicago" "Portland") ///
	   scalars("chi2 LR chi2" "p Prob > chi2" "mcfr2 McF. R2" "aic AIC" "ll Log lik." "LR_tstat LR_tstat") ///
	   b(3) se(3) ///
	   label se 

** STANDARD DEVIATION OF RANDOM PARAMETERS
forv i = 1/3 { 
est use estimates/tab_2_col_`i'_time
mixlcov, sd
}

** END OF SCRIPT. Have a nice day! 
