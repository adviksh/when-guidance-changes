/*****************************************************/
/* /\* This dofile builds the info vs. shifty graphs *\/ */
/*****************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/Documents/Github/COVID/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

/****************************/
/* 1. get update magnitudes */
/****************************/
use "$int/public_data" , clear 
keep if !mi(shifty)
destring tmt_order_id , replace 

/*************************************************************/
/* /\* This dofile generates heterogeneity by statement for the Inconsistent experiment  *\/ */
/*************************************************************/
gen shifty_1 = shifty * (tmt_order_id == 1 )
gen shifty_2 = shifty * (tmt_order_id == 2 ) 
gen steady_1 = (shifty==0) * (tmt_order_id == 1 ) 
gen steady_2 = (shifty==0) * (tmt_order_id == 2 )

la var shifty_1 "Inconsistent: Distancing"
la var shifty_2 "Inconsistent: Flu"
la var steady_1 "Consistent: Distancing"
la var steady_2 "Consistent: Flu"

eststo clear

/* changing x treatment 1 and 2 */ 
eststo: reghdfe update_mag_deaths shifty_1 shifty_2, absorb(stratum) vce(rob)
test _b[shifty_1] = _b[shifty_2]
local p: di %5.3f `r(p)'
estadd local wald= `p' 

/* steady x treatment 1 and 2 */ 
eststo: reghdfe update_mag_deaths steady_1 steady_2, absorb(stratum) vce(rob)
test _b[steady_1] = _b[steady_2]
local p: di %5.3f `r(p)'
estadd local wald = `p' 

/* changing x treatment 1 and 2 */ 
eststo: reghdfe update_covid shifty_1 shifty_2, absorb(stratum) vce(rob)
test _b[shifty_1] = _b[shifty_2]
local p: di %5.3f `r(p)'
estadd local wald = `p' 

/* steady x treatment 1 and 2 */ 
eststo: reghdfe update_covid steady_1 steady_2, absorb(stratum) vce(rob)
test _b[steady_1] = _b[steady_2]
local p: di %5.3f `r(p)'
estadd local wald = `p' 

esttab using out/changing_het.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude: \\ logs}" ///
    "\shortstack{Update magnitude: \\ logs}" ///
  "Propensity to update" ///
  "Propensity to update" ///
    ) scalars("wald \textit{p}-value: test that Flu = Distancing")

