** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
*********************************   Table 3   **********************************
*********************************   MWTP and MWTV     **************************

clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

**************************
******* BOOTSTRAP PROGRAMS
**************************

capture program drop wtp_money
program wtp_money, rclass
          return scalar asc = asc/money
		  return scalar fld_pctdec = fld_pctdec/money
		  return scalar hd_exc = hd_exc/money
		  return scalar pld_swm = pld_swm/money
end

capture program drop wtp_time
program wtp_time, rclass
          return scalar asc = asc/time
		  return scalar fld_pctdec = fld_pctdec/time
		  return scalar hd_exc = hd_exc/time
		  return scalar pld_swm = pld_swm/time
end

**************************
******* BOOTSTRAP AND TEST
**************************

foreach j in money time{
  
forv i =2/3 {
qui {
est use estimates/tab_2_col_`i'_`j'
sca var = sqrt([l51]_b[_cons]*[l51]_b[_cons] + [l52]_b[_cons]*[l52]_b[_cons] + [l53]_b[_cons]*[l53]_b[_cons] + [l54]_b[_cons]*[l54]_b[_cons] + [l55]_b[_cons]*[l55]_b[_cons])
nlcom (n`j': exp([Mean]_b[n`j'] + 0.5*var^2)) ///
	  (asc: _b[asc]) ///
	  (fld_pctdec: _b[fld_pctdec]) ///
	  (hd_exc: _b[hd_exc]) ///
	  (pld_swm: _b[pld_swm]), post	 
sca `j' = _b[n`j']
sca `j'_sd = _se[n`j']
sca asc = _b[asc]
sca asc_sd = _se[asc]
sca fld_pctdec = _b[fld_pctdec]
sca fld_pctdec_sd = _se[fld_pctdec]
sca hd_exc = _b[hd_exc]
sca hd_exc_sd = _se[hd_exc]
sca pld_swm = _b[pld_swm]
sca pld_swm_sd = _se[pld_swm]

set seed 42
clear
set obs 10000
gen `j' = rnormal(`=`j'',0)
gen asc = rnormal(`=asc',`=asc_sd')
gen fld_pctdec = rnormal(`=fld_pctdec',`=fld_pctdec_sd')
gen hd_exc = rnormal(`=hd_exc',`=hd_exc_sd')
gen pld_swm = rnormal(`=pld_swm',`=pld_swm_sd')
}

bootstrap asc=r(asc) fld_pctdec=r(fld_pctdec) hd_exc=r(hd_exc) pld_swm=r(pld_swm), reps(2000): wtp_`j'

** CREATE SCALARS FOR TESTING SCALED BY APPROXIMATE SAMPLE SIZE
eststo wtp_`j'
sca b_asc_`i'       = _b[asc]
sca b_fld_pctdec_`i'= _b[fld_pctdec]
sca b_hd_exc_`i'    = _b[hd_exc]
sca b_pld_swm_`i'   = _b[pld_swm]
sca sd_asc_`i'       = _se[asc]*sqrt(200)
sca sd_fld_pctdec_`i'= _se[fld_pctdec]*sqrt(200)
sca sd_hd_exc_`i'    = _se[hd_exc]*sqrt(200)
sca sd_pld_swm_`i'   = _se[pld_swm]*sqrt(200)

}

** TESTING DIFFERENCES IN MEANS
foreach l in asc fld_pctdec hd_exc pld_swm {
ttesti 200 `=b_`l'_2' `=sd_`l'_2' 200 `=b_`l'_3' `=sd_`l'_3'
matrix ttest= (r(mu_1), r(mu_2), r(mu_1) - r(mu_2), r(se), r(p))
matrix rownames ttest= `l'_`j'
matrix colnames ttest= Chicago Portland "Mean Differences" "Std Error" pscore
mat2txt, matrix(ttest) sav("output/Table_A2_4.xls") append
}

}

** END OF SCRIPT. Have a great day! 
