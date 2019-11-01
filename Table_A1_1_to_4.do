** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
****************************   Summary Tables   ********************************
********************************************************************************
clear all
set more off

** Set the working directory to the location of this file
cd ""

** Load data
use data/pooled_money_time, clear
append using data/chicago_money_and_time, gen(chitimo)
append using data/portland_money_and_time, gen(potimo)

** Generate sample indicators
gen sample_mo_ti = ""
replace sample_mo_ti = "money" if time_survey==0
replace sample_mo_ti = "time" if time_survey==1
gen sample_mo_ti_dum = .
replace sample_mo_ti_dum=1 if sample_mo_ti=="money"
replace sample_mo_ti_dum=0 if sample_mo_ti=="time"
gen sample_single_cost_dum = sample_mo_ti_dum!=.

** Generate a wage-condition indicator if reported wage is more than 8 and less than 500
gen wage_dum = wage>8 & wage<500

** Drop unecessary variables and duplicates
drop time_survey alt c_id choice money time hd_exc hd_good pld_swm pld_fsh fld_pctdec
duplicates drop 


**************************************
************************    Table A1-1
****************  Portland vs. Chicago
**************************************

balancetable chic_dum age hhsize vol_time flood_seen yrs_res ///
			 Employed Self_Employed Unemployed Homemaker Student Retired income1 income2 ///
			 income3 flood_exp seentool Basement Crawl_Space Both None ///
			 using "output/Table_A1_1.xlsx", replace nohead nofoot varla ///
			 ctitles("Portland" "Chicago" "Difference")  
			 
**************************************
************************    Table A1-2
**********************  Time vs. Money
**************************************

balancetable sample_mo_ti_dum age hhsize vol_time flood_seen yrs_res wage ///
			 Employed Self_Employed Unemployed Homemaker Student Retired income1 income2 ///
			 income3 flood_exp seentool Basement Crawl_Space Both None ///
			 using "output/Table_A1_2.xlsx", replace nohead nofoot varla ///
		     ctitles("Time" "Money" "Difference") 

**************************************
************************    Table A1-3
****************  Single vs. Dual Cost
**************************************
drop if sample_single_cost_dum==.
balancetable sample_single_cost_dum age hhsize vol_time flood_seen yrs_res wage ///
			 Employed Self_Employed Unemployed Homemaker Student Retired income1 income2 ///
			 income3 flood_exp seentool Basement Crawl_Space Both None ///
			 using "output/Table_A1_3.xlsx", replace nohead nofoot varla ///
		     ctitles("Any wage" "Any wage" "") 

**************************************
************************    Table A1-4
****************  Single vs. Dual Cost
**************************************
keep if wage_dum==1
balancetable sample_single_cost_dum age hhsize vol_time flood_seen yrs_res wage ///
			 Employed Self_Employed Unemployed Homemaker Student Retired income1 income2 ///
			 income3 flood_exp seentool Basement Crawl_Space Both None ///
			 using "output/Table_A1_4.xlsx", replace nohead nofoot varla
			 
** END OF SCRIPT. Have a great day!
