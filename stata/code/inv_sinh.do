/**************************************************************************************/
/* Generates a treatment effect for inverse hyperbolic sine transform of main effect  */
/**************************************************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

/*****************************************************/
/* Table 1: some variation, but always conservative  */
/*****************************************************/
use "$int/public_data" , clear
keep if shifty != .
la var shifty "Inconsistent" 
eststo clear

foreach c in 1 .1 .01 .001 {
  cap drop update_rob
  gen update_rob = ln(abs_mag_deaths + `c' )

    eststo: reghdfe update_rob shifty, absorb(stratum) vce(rob)
    mat mat = r(table)
    local p: di %5.3f mat[4,1]
    estadd local pval = `p' 
    estadd local constant = `c' 
}

esttab using out/ln_c.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior}|+c) \$}" ///
    "\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior}|+c) \$}" ///
    "\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior}|+c) \$}" ///
    "\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior}|+c) \$}" ) ///
scalars("pval \$p\$-value" "constant Value of constant \textit{c}" )


/*******************************************************/
/* /\* Table 2: Same effect if you look at invsinh *\/ */
/*******************************************************/

**** inverse hyperbolic sine definition from https://en.wikipedia.org/wiki/Inverse_hyperbolic_functions#Inverse_hyperbolic_sine ****/
gen invsinhupdate = ln(abs_mag_deaths + sqrt(abs_mag_deaths^2 + 1))

eststo clear 
eststo: reghdfe invsinhupdate shifty, absorb(stratum) vce(rob)
mat mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval = `p' 
estadd local sample = "All"

eststo: reghdfe invsinhupdate shifty if priors_below_info == 1, absorb(stratum) vce(rob)
mat mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval = `p' 
estadd local sample = "Priors below info"

eststo: reghdfe invsinhupdate shifty if priors_at_info == 1, absorb(stratum) vce(rob)
mat mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval = `p' 
estadd local sample = "Priors at info"

eststo: reghdfe invsinhupdate shifty if priors_above_info == 1, absorb(stratum) vce(rob)
mat mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval = `p' 
estadd local sample = "Priors above info"

esttab using out/sinh.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude: \\ \$ \sinh^{-1}(|\text{posterior}-\text{prior} |) \$}" ///
    "\shortstack{Update magnitude: \\ \$ \sinh^{-1}(|\text{posterior}-\text{prior} |) \$}" /// 
  "\shortstack{Update magnitude: \\ \$ \sinh^{-1}(|\text{posterior}-\text{prior} |) \$}" /// 
  "\shortstack{Update magnitude: \\ \$ \sinh^{-1}(|\text{posterior}-\text{prior} |) \$}" /// 
  "\shortstack{Update magnitude: \\ \$ \sinh^{-1}(|\text{posterior}-\text{prior} |) \$}" ///
    ) scalars("pval \$p\$-value" "sample Sample" )
















