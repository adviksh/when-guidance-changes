/*********************************************/
/* Makes treatment effect of strong action on belief updates  */
/*********************************************/

/*****************************************************/
/* /\* This dofile studies revealed pref outcomes *\/ */
/*****************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

use "$int/public_data" , clear
assert !mi(number_others) 
gen see_others = number_others > 0

reghdfe number_others shifty, absorb(stratum)
reghdfe number_others strong, absorb(stratum)

reghdfe see_others shifty, absorb(stratum)

reghdfe number_others shifty if info_above_priors == 1, absorb(stratum)
reghdfe number_others shifty if info_below_priors == 1, absorb(stratum)

reghdfe see_others shifty if info_above_priors == 1, absorb(stratum)
reghdfe see_others shifty if info_below_priors == 1, absorb(stratum)

reghdfe number_others shifty if q_0_25_deaths_pre == 1, absorb(stratum)

reghdfe number_others shifty if q_0_25_deaths_pre == 1, absorb(stratum)
