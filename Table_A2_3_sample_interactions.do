** Written by: Bryan Parthum; bparthum@gmail.com ; July 2018

********************************************************************************
***********************   Sample Interactions        ***************************
***********************   Table *		 	  		 ***************************
********************************************************************************
clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

**********************************
***********  Table A2-4 - Column 4
**********************************	 

** Load data
use data/chicago_money_and_time, clear
append using data/portland_money_and_time, gen(portland_sample)
append using data/pooled_money_time, gen(separate_treatments)

** Generate sample terms
gen dual_treat  = time_survey==.
gen money_treat = time_survey==0
gen time_treat  = time_survey==1 

** Rebalance id variables
replace c_id = c_id+10000 if dual_treat==1
replace id = id+10000 if dual_treat==1

** Generate negative monetized term 
gen nmoney = -money	
recode nmoney .=0
gen ntime  = -time
recode ntime .=0

** Generate sample interactions
gen nmoney_mo = nmoney*money_treat
gen ntime_ti  = ntime*time_treat

** Generate alternative specific constant
gen asc = alt==3	

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Random variables for MMNL
global randvars "asc fld_pctdec hd_exc pld_swm nmoney_mo ntime_ti nmoney ntime"

** Estimate
qui{
global model "tab_A2_3_col_4"
mixlogit choice, rand($randvars) group(c_id) id(id) ln(2) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

********************************************************************************
***********************************  Table A2-4*********************************
********************************************************************************

est use estimates/tab_2_col_1_money
est sto tab_2_col_1_money
est use estimates/tab_2_col_1_time
est sto tab_2_col_1_time
est use estimates/tab_7_col_1
est sto tab_7_col_1
est use estimates/tab_A2_3_col_4
est sto tab_A2_3_col_4

esttab tab_2_col_1_money tab_2_col_1_time tab_7_col_1 tab_A2_3_col_4 ///
	   using output\Table_A2_3.rtf, replace ///
	   title(Table A2-3: Interactions between Payment Vehicle Treatments) ///
	   mtitles("Money" "Time" "Money and Time" "All") ///
	   scalars("chi2 LR chi2" "p Prob > chi2" "mcfr2 McF. R2" "aic AIC" "ll Log lik.") ///
	   b(3) se(3) ///
	   label se 

** SD OF RANDOM PARAMETERS
est use estimates/tab_5_col_1
mixlcov, sd
est use estimates/tab_5_col_2
mixlcov, sd
est use estimates/tab_7_col_2
mixlcov, sd
est use estimates/tab_A2_4_col_4
mixlcov, sd

** END OF SCRIPT. Have a nice day! 