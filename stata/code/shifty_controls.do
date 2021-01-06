/*********************************************************************************************/
/* This generates a table of the steady/shifty experiment with various controls/no controls  */
/*********************************************************************************************/
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
assert !mi(hhi)
assert !mi(education)
assert !mi(ethnicity)
assert !mi(hispanic) 
gen race_eth = "white non-hispanic" if ethnicity == "1"
replace race_eth = "black non-hispanic" if ethnicity == "2"
replace race_eth = "hispanic" if inrange(real(hispanic),2,14) 
replace race_eth = "asian/other" if race_eth == "" 

destring hhi , replace 
gen hhi_bin = "low or missing" if inrange(hhi,1,4) | hhi == -3105
replace hhi_bin = "middle" if inrange(hhi,5,18) 
replace hhi_bin = "high" if inrange(hhi,19,.) 

destring education, replace 
cap drop educ_bin
gen educ_bin = "hs or less or missing" if inlist(education,1,2,-3105)
replace educ_bin = "some college" if inlist(education,3,4,5)
replace educ_bin = "college+" if inlist(education,6,7,8)

la var shifty "Inconsistent"

eststo clear
foreach outcome in update_mag_deaths update_covid {

  /***** randomization inference for all specs *****/
  /* no fe */
  ritest shifty _b[shifty], strata(ri_stratum) reps($reps) seed(100) : reg `outcome' shifty, vce(rob)
  matrix p = r(p)
  local p: di %5.3f p[1,1] 
  local p_ri_nofe = `p' 
  
  /* stratum fe */ 
  ritest shifty _b[shifty], strata(ri_stratum) reps($reps) seed(100) : reghdfe `outcome' shifty, vce(rob) absorb(stratum)  
  matrix p = r(p)
  local p: di %5.3f p[1,1] 
  local p_ri_stratum = `p' 

  /* demo + stratum fe */ 
  ritest shifty _b[shifty], strata(ri_stratum) reps($reps) seed(100) : reghdfe `outcome' shifty, vce(rob) absorb(stratum region race_eth educ_bin hhi_bin)  
  matrix p = r(p)
  local p: di %5.3f p[1,1] 
  local p_ri_group = `p'     
    
  /****** regression: no controls ******/ 
  eststo: reg `outcome' shifty, vce(rob)
  matrix mat_nofe = r(table)
  local p_nofe: di %5.3f mat_nofe[4,1]  
  estadd local p_rob = `p_nofe' 
  estadd local p_ri = `p_ri_nofe' 
  estadd local stratum "No"
  estadd local group "No"
  
  /******* stratum fixed effects *******/ 
  eststo: reghdfe `outcome' shifty, vce(rob) absorb(stratum) 
  matrix mat_stratum = r(table)
  local p_stratum: di %5.3f mat_stratum[4,1]
  estadd local p_rob = `p_stratum' 
  estadd local p_ri = `p_ri_stratum' 
  estadd local stratum "\$\checkmark\$"
  estadd local group "No"

  /* randomization inference */ 

  /******* stratum + demographic group fixed effects *****/     
  eststo: reghdfe `outcome' shifty, vce(rob) absorb(stratum region race_eth educ_bin hhi_bin) 
  matrix mat_group = r(table)
  local p_group: di %5.3f mat_group[4,1]
  estadd local p_rob = `p_group' 
  estadd local p_ri = `p_ri_group'
  estadd local stratum "\$\checkmark\$"
  estadd local group "\$\checkmark\$"
  
}

esttab using out/changing_fe.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude}" ///
    "\shortstack{Update magnitude}" ///
    "\shortstack{Update magnitude}" ///
    "\shortstack{Update propensity}" ///
    "\shortstack{Update propensity}" ///
    "\shortstack{Update propensity}" ///
    ) scalars("stratum Stratum FE" "group Demographic FE" ///
    "note \underline{\$p\$-value}:" /// 
    "p_rob \ \ \ \ \ \ \ \ Robust standard errors" "p_ri \ \ \ \ \ \ \ \ Permutation test") 
    



