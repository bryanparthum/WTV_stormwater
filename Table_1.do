** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
************************************************************   SUMARY STATISTICS
**********************************************************************   TABLE 1 
********************************************************************************
clear all
set more off

** Set the working directory to the location of this file
cd ""

** Load data
use data/pooled_money_time, clear

** Generate a wage-condition indicator if reported wage is more than 8 and less than 500
gen wage_dum = wage>8 & wage<500

** Drop unecessary variables and duplicates
drop time_survey alt c_id choice money time hd_exc hd_good pld_swm pld_fsh fld_pctdec id
duplicates drop 

**************************************
***************************    TABLE 1
**************************************

outreg2 using output/Table_1.rtf if chic_dum==1, word ///
		replace label sum(log) eqkeep(N mean min max) dec(2) ///
		cttop("Chicago") ///
		title("Table 1: Summary Statistics by City Subsamples")
outreg2 using output/Table_1.rtf if chic_dum==0, word ///
		append label sum(log) eqkeep(N mean min max) dec(2) ///
		cttop("Portland") 
outreg2 using output/Table_1.rtf, word ///
		append label sum(log) eqkeep(N mean min max) dec(2) ///
		cttop("Both Cites")
		
** END OF SCRIPT. Have a great day!
