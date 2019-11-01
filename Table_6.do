** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
*********************************   Table 6   **********************************
*********************************   Ratio of MWTP and MWTV  ********************
********************************************************************************

clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

** TABLE 6
qui { 
est use estimates/tab_5_col_1
sca var = sqrt([l51]_b[_cons]*[l51]_b[_cons] + [l52]_b[_cons]*[l52]_b[_cons] + [l53]_b[_cons]*[l53]_b[_cons] + [l54]_b[_cons]*[l54]_b[_cons] + [l55]_b[_cons]*[l55]_b[_cons])
nlcom (nmoney: -exp([Mean]_b[nmoney] + 0.5*var^2)) ///
	  (asc: _b[asc]) ///
	  (fld_pctdec: _b[fld_pctdec]) ///
	  (hd_exc: _b[hd_exc]) ///
	  (pld_swm: _b[pld_swm]), post	  
}
di "Table 6 Column 1"
wtp nmoney asc fld_pctdec hd_exc pld_swm, kr reps(2000)

qui { 
est use estimates/tab_5_col_2
sca var = sqrt([l51]_b[_cons]*[l51]_b[_cons] + [l52]_b[_cons]*[l52]_b[_cons] + [l53]_b[_cons]*[l53]_b[_cons] + [l54]_b[_cons]*[l54]_b[_cons] + [l55]_b[_cons]*[l55]_b[_cons])
nlcom (ntime: -exp(_b[ntime] + 0.5*var^2)) ///
	  (asc: _b[asc]) ///
	  (fld_pctdec: _b[fld_pctdec]) ///
	  (hd_exc: _b[hd_exc]) ///
	  (pld_swm: _b[pld_swm]), post	 
}
di "Table 6 Column 2"
wtp ntime asc fld_pctdec hd_exc pld_swm, kr reps(2000)


** END OF SCRIPT. Have a great day! 
