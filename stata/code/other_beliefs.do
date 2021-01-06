/***********************************************************************/
/* /\* Appendix table of effects on deaths, death rate, net update *\/ */
/***********************************************************************/
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
la var shifty "Inconsistent"

/**********************/
/* TABLE: net update  */
/**********************/
foreach outcome in ln_deaths ln_width {
  local ln_deathsnm "\shortstack{Net update: \\ ln(posterior)}"
  local ln_widthnm "\shortstack{Width of \\ confidence interval \\ (log)}"
  eststo clear
  
  eststo: reghdfe `outcome'_post shifty `outcome'_pre , absorb(stratum) vce(rob)
  estadd local sample "All"
  
  eststo: reghdfe `outcome'_post shifty `outcome'_pre if priors_below_info == 1, absorb(stratum) vce(rob)
  estadd local sample "Priors below info"
  
  eststo: reghdfe `outcome'_post shifty `outcome'_pre if priors_at_info == 1 , absorb(stratum) vce(rob)
  estadd local sample "Priors at info"
  
  eststo: reghdfe `outcome'_post shifty `outcome'_pre if priors_above_info == 1, absorb(stratum) vce(rob)
  estadd local sample "Priors above info"

  esttab using out/effects_`outcome'.tex, replace se label drop(_cons *pre) ///
      star(* 0.1 ** 0.05 *** 0.01) ///
      mlabel("``outcome'nm'" ///
      "``outcome'nm'" ///
      "``outcome'nm'" ///
      "``outcome'nm'") ///
      scalars("sample Sample:")             
}

eststo clear 
/******************************/
/* absolute effects  */
/******************************/
foreach outcome in death_rate_old death_rate_young {
  gen um_`outcome' = abs(`outcome'_post - `outcome'_pre)
}

gen um_dji = ln(abs(dji_post - dji_pre) +1 ) 

foreach outcome in death_rate_old death_rate_young dji {
  eststo: reghdfe um_`outcome' shifty, absorb(stratum)  vce(rob)
}

/*************************/
/* net effects  */
/*************************/
foreach outcome in death_rate_old death_rate_young ln_dji {
  eststo: reghdfe `outcome'_post shifty `outcome'_pre, absorb(stratum) vce(rob)
}

esttab using out/effects_excl_death.tex, replace se label drop(_cons *pre) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude: \\ death rate \\ (for ages: 50--80)}" ///
    "\shortstack{Update magnitude: \\ death rate \\ (for ages: 20--50)}" /// 
    "\shortstack{Update magnitude: \\ Dow Jones \\ Index}" /// 
    "\shortstack{Net update: \\ death rate \\ (for ages: 50--80)}" ///
    "\shortstack{Net update: \\ death rate \\ (for ages: 20--50)}" /// 
    "\shortstack{Net update: \\ Dow Jones \\ Index}"  /// 
    "\shortstack{Net update: \\ deaths \\ ln(posterior $-$ prior $+$ 1)}" )



