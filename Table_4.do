** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
*****************************************************   Main Results and WTP,WTV
**********************************************************************   Table 4
********************************************************************************
clear all
set more off, perm
set niceness 0

** YOU MUST SET YOUR WORKING DIRECTORY TO THE LOCATION OF THIS FILE
cd ""

********************************************************************************
*********************************   Table 4 - MONEY ****************************
********************************************************************************

** BUILD PARTS

sca chicago_pop = 1035436
sca portlan_pop = 256167
global var sqrt([l51]_b[_cons]*[l51]_b[_cons] + [l52]_b[_cons]*[l52]_b[_cons] + [l53]_b[_cons]*[l53]_b[_cons] + [l54]_b[_cons]*[l54]_b[_cons] + [l55]_b[_cons]*[l55]_b[_cons])

** SCENARIO 1
foreach sample in money time {
est use estimates/tab_2_col_1_`sample'
nlcom (WTP_1: (1/-exp(_b[n`sample']+ 0.5*$var^2)) * ( log(exp(_b[asc]*1 + _b[fld_pctdec]*0 + _b[hd_exc]*0 + _b[pld_swm]*0)) ///
									             - log(exp(_b[asc]*0 + _b[fld_pctdec]*0 + _b[hd_exc]*1 + _b[pld_swm]*1)))), post	
mat a = 12 * e(b)
mat b = 12 * (a * chicago_pop)/1000000
mat c = 12 * (a * portlan_pop)/1000000
mat col_1_`sample' = a\b\c

est use estimates/tab_2_col_2_`sample'
nlcom (WTP_1: (1/-exp(_b[n`sample']+ 0.5*$var^2)) * ( log(exp(_b[asc]*1 + _b[fld_pctdec]*0 + _b[hd_exc]*0 + _b[pld_swm]*0)) ///
									            - log(exp(_b[asc]*0 + _b[fld_pctdec]*0 + _b[hd_exc]*1 + _b[pld_swm]*1)))), post	
mat a = 12 * e(b)
mat b = 12 * (a * chicago_pop)/1000000
mat col_1_`sample' = col_1_`sample'\a\b

est use estimates/tab_2_col_3_`sample'
nlcom (WTP_1: (1/-exp(_b[n`sample']+ 0.5*$var^2)) * ( log(exp(_b[asc]*1 + _b[fld_pctdec]*0 + _b[hd_exc]*0 + _b[pld_swm]*0)) ///
									            - log(exp(_b[asc]*0 + _b[fld_pctdec]*0 + _b[hd_exc]*1 + _b[pld_swm]*1)))), post	
mat a = 12 * e(b)
mat b = 12 * (a * portlan_pop)/1000000
mat col_1_`sample' = col_1_`sample'\a\b
mat rownames col_1_`sample' = annual_per_capita annual_chi annual_por annual_per_capita_chi annual_chi_chi annual_per_capita_por annual_por_por
mat list col_1_`sample'
}

mat col_1 = col_1_money\col_1_time
mat colnames col_1 = "Scenario 1"
mat list col_1

** SCENARIO 2
foreach sample in money time {
est use estimates/tab_2_col_1_`sample'
nlcom (WTP_2: (1/-exp(_b[n`sample']+ 0.5*$var^2)) * ( log(exp(_b[asc]*1 + _b[fld_pctdec]*0 + _b[hd_exc]*0 + _b[pld_swm]*0)) ///
									             - log(exp(_b[asc]*0 + _b[fld_pctdec]*50 + _b[hd_exc]*1 + _b[pld_swm]*1)))), post	
mat a = 12 * e(b)
mat b = 12 * (a * chicago_pop)/1000000
mat c = 12 * (a * portlan_pop)/1000000
mat col_2_`sample' = a\b\c

est use estimates/tab_2_col_2_`sample'
nlcom (WTP_2: (1/-exp(_b[n`sample']+ 0.5*$var^2)) * ( log(exp(_b[asc]*1 + _b[fld_pctdec]*0 + _b[hd_exc]*0 + _b[pld_swm]*0)) ///
									            - log(exp(_b[asc]*0 + _b[fld_pctdec]*50 + _b[hd_exc]*1 + _b[pld_swm]*1)))), post
mat a = 12 * e(b)
mat b = 12 * (a * chicago_pop)/1000000
mat col_2_`sample' = col_2_`sample'\a\b

est use estimates/tab_2_col_3_`sample'
nlcom (WTP_2: (1/-exp(_b[n`sample']+ 0.5*$var^2)) * ( log(exp(_b[asc]*1 + _b[fld_pctdec]*0 + _b[hd_exc]*0 + _b[pld_swm]*0)) ///
									            - log(exp(_b[asc]*0 + _b[fld_pctdec]*50 + _b[hd_exc]*1 + _b[pld_swm]*1)))), post
mat a = 12 * e(b)
mat b = 12 * (a * portlan_pop)/1000000
mat col_2_`sample' = col_2_`sample'\a\b
mat rownames col_2_`sample' = annual_per_capita annual_chi annual_por annual_per_capita_chi annual_chi_chi annual_per_capita_por annual_por_por
mat list col_2_`sample'
}

mat col_2 = col_2_money\col_2_time
mat colnames col_2 = "Scenario 2"
mat list col_2

mat table_4 = col_1,col_2
mat list table_4
		
** EXPORT 
putexcel set output\Table_4,replace
putexcel A1 = matrix(table_4), names

** END OF SCRIPT. Have a nice day! 
