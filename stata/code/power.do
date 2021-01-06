/****************************************************************************************/
/* This dofile creates a continuuous version for our main outcomes, per referee request */
/****************************************************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13
 
use "$int/public_data" , clear
destring stratum, replace

/**************************/
/* Panel A: main outcomes */
/**************************/
eststo clear

gen interaction = ln_deaths_pre * (shifty == 1)
la var interaction "ln(deaths) \$ \times \$ Inconsistent"
la var shifty "Inconsistent"

eststo: reghdfe update_covid interaction shifty ln_deaths_pre, absorb(stratum) vce(rob)
matrix mat = r(table)
estadd local sample "All"
local p  : di %5.3f mat[4,1]
estadd scalar p_value = `p' 

eststo: reghdfe update_mag_deaths interaction shifty ln_deaths_pre , absorb(stratum) vce(rob)
matrix mat = r(table)
estadd local sample "All"
local p  : di %5.3f mat[4,1]
estadd scalar p_value = `p' 

eststo: reghdfe gov_index_post_std  interaction shifty gov_index_pre_std ln_deaths_pre, absorb(stratum) vce(rob)
matrix mat = r(table)
estadd local sample "All"
local p  : di %5.3f mat[4,1]
estadd scalar p_value = `p' 

/**** only below certain level ****/
eststo: reghdfe update_covid interaction shifty ln_deaths_pre if deaths_pre < 1000000, absorb(stratum) vce(rob)
matrix mat = r(table)
estadd local sample "Restricted"
local p  : di %5.3f mat[4,1]
estadd scalar p_value = `p' 

eststo: reghdfe update_mag_deaths interaction shifty ln_deaths_pre if deaths_pre < 1000000 , absorb(stratum) vce(rob)
matrix mat = r(table)
estadd local sample "Restricted"
local p  : di %5.3f mat[4,1]
estadd scalar p_value = `p' 

eststo: reghdfe gov_index_post_std  interaction shifty gov_index_pre_std ln_deaths_pre if deaths_pre < 1000000, absorb(stratum) vce(rob)
matrix mat = r(table)
estadd local sample "Restricted"
local p  : di %5.3f mat[4,1]
estadd scalar p_value = `p' 



esttab using out/power_cts.tex, replace se label drop( ln_deaths_pre gov_index_pre_std _cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update propensity: \\ \$ 1(\text{post.} \neq \text{prior}) \$}" ///
    "\shortstack{Update mag.: \\ \$ \ln(|\text{post.}-\text{prior}|+1) \$}" ///
    "\shortstack{Govt. credibility \\ index}" ///
    "\shortstack{Update propensity: \\ \$ 1(\text{post.} \neq \text{prior}) \$}" ///
    "\shortstack{Update mag.: \\ \$ \ln(|\text{post.}-\text{prior}|+1) \$}" ///
    "\shortstack{Govt. credibility \\ index}") sfmt( 0 2  ) ///
    stats(N p_value sample  ,     ///
    label( "Observations" "\$ p \$-value of interaction" "Sample" ) ///
 )  




/*******************************************************************/
/* Panel B: main outcomes, excluding over 1 million deaths in pre  */
/*******************************************************************/
