** Written by: Bryan Parthum; bparthum@gmail.com ; June 2019

********************************************************************************
**********************************************************************   Table 3
****************************************************************   MWTP and MWTV 
********************************************************************************

clear all
set more off, perm
set niceness 0

** Set the working directory to the location of this file
cd ""

** TABLE 3
forv i = 1/3 {
est use estimates/tab_2_col_`i'_money
sca var = sqrt([l51]_b[_cons]*[l51]_b[_cons] + [l52]_b[_cons]*[l52]_b[_cons] + [l53]_b[_cons]*[l53]_b[_cons] + [l54]_b[_cons]*[l54]_b[_cons] + [l55]_b[_cons]*[l55]_b[_cons])
nlcom (nmoney: -exp([Mean]_b[nmoney] + 0.5*var^2)) ///
	  (asc: _b[asc]) ///
	  (fld_pctdec: _b[fld_pctdec]) ///
	  (hd_exc: _b[hd_exc]) ///
	  (pld_swm: _b[pld_swm]), post	  
mat b = e(b)
mat V = e(V)
ereturn post b V
di "Table 3 Column `i' - Panel A (money)"
wtp nmoney asc fld_pctdec hd_exc pld_swm, kr reps(2000)
}
 
forv i = 1/3 {
est use estimates/tab_2_col_`i'_time
sca var = sqrt([l51]_b[_cons]*[l51]_b[_cons] + [l52]_b[_cons]*[l52]_b[_cons] + [l53]_b[_cons]*[l53]_b[_cons] + [l54]_b[_cons]*[l54]_b[_cons] + [l55]_b[_cons]*[l55]_b[_cons])
nlcom (ntime: -exp(_b[ntime] + 0.5*var^2)) ///
	  (asc: _b[asc]) ///
	  (fld_pctdec: _b[fld_pctdec]) ///
	  (hd_exc: _b[hd_exc]) ///
	  (pld_swm: _b[pld_swm]), post	  
mat b = e(b)
mat V = e(V)
ereturn post b V
di "Table 3 Column `i' - Panel B (time)"
wtp ntime asc fld_pctdec hd_exc pld_swm, kr reps(2000)
}

** END OF SCRIPT. Have a great day! 
