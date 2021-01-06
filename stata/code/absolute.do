/**********************************************************/
/* Generate effect on alternate outcome of belief update  */
/**********************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

use "$int/public_data" , clear
keep if !mi(shifty)
eststo clear

la var shifty "Inconsistent"

/* column 1: absolute effect */
eststo: reghdfe abs_mag_deaths shifty, absorb(stratum) vce(rob)
matrix mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval `p'

/* column 2: winsorize */
sum abs_mag_deaths if shifty == 0, d
gen abs_mag_deaths_win_95 = min(abs(deaths_post - deaths_pre) , `r(p95)')
gen abs_mag_deaths_win_90 = min(abs(deaths_post - deaths_pre) , `r(p90)')

eststo: reghdfe abs_mag_deaths_win_95 shifty, absorb(stratum) vce(rob)
matrix mat = r(table)
local p: di %5.3f mat[4,1]
estadd local note "Winsorized: 95th percentile"
estadd local pval `p'

eststo: reghdfe abs_mag_deaths_win_90 shifty, absorb(stratum) vce(rob)
matrix mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval `p'
estadd local note "Winsorized: 90th percentile"

/* column 4: drop if exceeds 1 million */
sum deaths_pre if shifty == 0 , d
local pre_p95 = `r(p95)'
local pre_p90 = `r(p90)'

sum deaths_post if shifty == 0 , d
local post_p95 = `r(p95)'
local post_p90 = `r(p90)'

eststo: reghdfe abs_mag_deaths shifty if deaths_pre <= `pre_p95' & deaths_post <= `post_p95', absorb(stratum) vce(rob)
matrix mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval `p'
estadd local note "Drop if $>$ 95th percentile"

eststo: reghdfe abs_mag_deaths shifty if deaths_pre <= `pre_p90' & deaths_post <= `post_p90', absorb(stratum) vce(rob)
matrix mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval `p'
estadd local note "Drop if $>$ 90th percentile"

esttab using out/other_outcome.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude: \\ $|$ posterior $-$ prior $|$}" ///
    "\shortstack{Update magnitude: \\ $|$ posterior $-$ prior $|$}" ///
    "\shortstack{Update magnitude: \\ $|$ posterior $-$ prior $|$}" ///
    "\shortstack{Update magnitude: \\ $|$ posterior $-$ prior $|$}"    ///
    "\shortstack{Update magnitude: \\ $|$ posterior $-$ prior $|$}"    ///    
    ) scalars("pval \$p\$-value" "note Note")

