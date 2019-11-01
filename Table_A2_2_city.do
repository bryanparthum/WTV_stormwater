** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
***********************   POOLED WTP,WTV	         ***************************
***********************   FROM Table 2 and Table 3   ***************************
********************************************************************************
clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

********************************************************************************
*******************************  PANEL A - MONEY *******************************
********************************************************************************

** Load data
use data/pooled_money_time, clear

** Generate negative term 
gen nmoney = -money * (1-time_survey) 	
gen ntime  = -time  * time_survey

** Generate alternative specific constant
gen asc = alt==3

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Generate interactions
gen asc_chi = asc*chic_dum
gen fld_pctdec_chi = fld_pctdec*chic_dum
gen hd_exc_chi = hd_exc*chic_dum
gen pld_swm_chi = pld_swm*chic_dum
gen nmoney_chi = nmoney*chic_dum
gen ntime_chi = ntime*chic_dum

** Random variables for MMNL
global randvars_1 "asc fld_pctdec hd_exc pld_swm asc_chi fld_pctdec_chi hd_exc_chi pld_swm_chi nmoney_chi nmoney"
global randvars_2 "asc fld_pctdec hd_exc pld_swm asc_chi fld_pctdec_chi hd_exc_chi pld_swm_chi ntime_chi ntime"

**********************************
**********   Table A2_2 - Column 1
**********************************	
qui{
global model "tab_A2_2_col_1"
mixlogit choice, rand($randvars_1) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}

**********************************
**********   Table A2_2 - Column 2
**********************************		
qui{
global model "tab_A2_2_col_2"
mixlogit choice, rand($randvars_2) group(c_id) id(id) ln(1) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
*mat b = e(b)
*mat g = b*0.01
*mat $model = b + g
*matsave $model, replace p("matrices") s
}

**********************************
**********   Table A2_2 - Column 3
**********************************	
** Load data
use data/pooled_money_time, clear

** Generate negative term 
gen nmoney = -money * (1-time_survey) 	
gen ntime  = -time  * time_survey
recode nmoney .=0
recode ntime  .=0
*gen nmoney_chi = nmoney*chic_dum
*gen ntime_chi = ntime*chic_dum

** Generate alternative specific constant
gen asc = alt==3

** Transform flood reduction from percent
replace fld_pctdec = fld_pctdec *100

** Generate interactions
gen asc_chi = asc*chic_dum
gen fld_pctdec_chi = fld_pctdec*chic_dum
gen hd_exc_chi = hd_exc*chic_dum
gen pld_swm_chi = pld_swm*chic_dum
gen nmoney_chi = nmoney*chic_dum
gen ntime_chi = ntime*chic_dum

** Random variables for MMNL
global randvars_3 "asc fld_pctdec hd_exc pld_swm asc_chi fld_pctdec_chi hd_exc_chi pld_swm_chi nmoney_chi ntime_chi nmoney ntime"

qui{
global model "tab_A2_2_col_3"
mixlogit choice, rand($randvars_3) group(c_id) id(id) ln(2) nrep(500) cluster(id) corr 
estadd sca mcfr2 = 1-(e(ll)/e(ll_0)), r
eststo $model
est save estimates/$model, replace
}


********************************************************************************
*********************************  Table A2_2  *********************************
********************************************************************************

forv i = 1/3 {
est use estimates/tab_A2_2_col_`i'
eststo tab_A2_2_col_`i'
}

**********************************
**************  Table A2 - PANEL A
**********************************	

esttab tab_A2_2_col_1 tab_A2_2_col_2 tab_A2_2_col_3 ///
	   using output\Table_A2_2.rtf, replace ///
	   title(Table A2-2: MMNL with Interactions to Test Parameter Differences between Cities) ///
	   mtitles("Money" "Time" "Both") ///
	   scalars("chi2 LR chi2" "p Prob > chi2" "mcfr2 McF. R2" "aic AIC" "ll Log lik.") ///
	   b(3) se(3) ///
	   label se 

** STANDARD DEVIATION OF RANDOM PARAMETERS
forv i = 1/3 { 
est use estimates/tab_A2_2_col_`i'
mixlcov, sd
}
