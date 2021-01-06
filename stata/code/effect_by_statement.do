/***********************************/
/* treatment effect *by statement* */
/***********************************/
/* study the effect on preferences of all treatments */
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13


/*****************/
/* EXPERIMENT 1  */
/*****************/
use "${int}/launch_1_pol", clear

/* effect by statement */
eststo clear
la var strong_action "Strong action: pooled"
la var sa_national_emergency "National emergency"
la var sa_wartime_powers "Wartime powers"
la var sa_avoid_gatherings "Avoid gatherings"
la var sa_invisible_enemy "Invisible enemy"

assert sa_national_emergency + sa_wartime_powers + sa_avoid_gatherings + sa_invisible_enemy == 2 if strong_action == 1 

foreach outcome in death_rate_up cases_up deaths_up {

  * get "main effect" from tests 
  eststo: reghdfe `outcome' strong_action strong_info, absorb(group)   vce(rob)
  local main_effect  = _b[strong_action]
  estadd local f = ""
  estadd local same_main = ""
  estadd local same_main_excl_gatherings = ""
  estadd local eq_0 = ""
  
  eststo: reghdfe `outcome' strong_info sa_* , absorb(group)   vce(rob)
  estadd local f = ""

  * test whether the effect is equal to the "main effect" 
  test (_b[sa_national_emergency] = (`main_effect' / 2)) ///
  (_b[sa_wartime_powers] = (`main_effect'/2)) ///
  (_b[sa_avoid_gatherings] = (`main_effect'/2)) ///
      (_b[sa_invisible_enemy] = (`main_effect'/ 2))
  local p: di %5.3f `r(p)' 
  estadd local same_main = `p'
  
  test (_b[sa_national_emergency] = (`main_effect'/2)) ///
  (_b[sa_wartime_powers] = (`main_effect'/2)) ///
  (_b[sa_invisible_enemy] = (`main_effect'/2)) 
  local p: di %5.3f `r(p)' 
  estadd local same_main_excl_gatherings = `p' 
  
  test (_b[sa_national_emergency] = 0) ///
  (_b[sa_wartime_powers] = 0) ///
  (_b[sa_avoid_gatherings] = 0) ///
      (_b[sa_invisible_enemy] = 0)
  
  local p: di %5.3f `r(p)' 
  estadd local eq_0 = `p' 
}

esttab using out/sa_by_statement.tex, replace se ///
    label drop(strong_info _cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Updates up: \\ death rate} " "\shortstack{Updates up: \\ death rate} " "\shortstack{Updates up: \\ cases}" "\shortstack{Updates up: \\ cases}" "\shortstack{Updates up: \\ deaths}" "\shortstack{Updates up: \\ deaths}") ///
    scalars("f \ul{\textit{p}-value from Wald test of...}" "same_main All coefs = pooled coef / 2" "eq_0 All coefs = 0")

/******** weak action ***********/

/* effect by statement */
eststo clear
gen weak_action = strong_action == 0 
la var weak_action "Weak action: pooled"
la var wa_less_flu "Same as flu"
la var wa_no_quarantine "No national quarantine"
la var wa_dom_travel  "No travel restrictions"
la var wa_no_fo  "No federal orders"

foreach outcome in death_rate_up cases_up deaths_up {

  * get "main effect" from tests 
  eststo: reghdfe `outcome' weak_action strong_info, absorb(group)   vce(rob)
  local main_effect  = _b[weak_action]
  estadd local f = ""
  estadd local same_main = ""
  estadd local same_main_excl_gatherings = ""
  estadd local eq_0 = ""
  
  eststo: reghdfe `outcome' strong_info wa_* , absorb(group)   vce(rob)
  estadd local f = ""

  * test whether the effect is equal to the "main effect" 
  test (_b[wa_less_flu] = (`main_effect' / 2)) ///
  (_b[wa_no_quarantine] = (`main_effect'/2)) ///
  (_b[wa_dom_travel] = (`main_effect'/2)) ///
      (_b[wa_no_fo] = (`main_effect'/ 2))
  local p: di %5.3f `r(p)' 
  estadd local same_main = `p'
    
  * test whether the effect is equal to the "main effect" 
  test (_b[wa_less_flu] = 0 ) ///
  (_b[wa_no_quarantine] = 0) ///
  (_b[wa_dom_travel] = 0) /// 
      (_b[wa_no_fo] = 0)
  
  local p: di %5.3f `r(p)'
  estadd local eq_0 = `p' 
}

esttab using out/wa_by_statement.tex, replace se ///
    label drop(strong_info _cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Updates up: \\ death rate} " "\shortstack{Updates up: \\ death rate} " "\shortstack{Updates up: \\ cases}" "\shortstack{Updates up: \\ cases}" "\shortstack{Updates up: \\ deaths}" "\shortstack{Updates up: \\ deaths}") ///
    scalars("f \ul{\textit{p}-value from Wald test of...}" "same_main All coefs = pooled coef / 2" "eq_0 All coefs = 0")

/**************************************************************************/
/* generate a version of main table where we just drop these two results  */
/**************************************************************************/

/***************************/
/* i. drop if less than flu:  */
/***************************/
use "${int}/launch_1_pol", clear
la var strong_action "Strong Action treatment"

foreach statement in "wa_less_flu == 1" "sa_invisible_enemy == 1" "sa_invisible_enemy == 1 | wa_less_flu == 1" {
  qui  {
  preserve
  eststo clear
  
  drop if `statement'

  /* wald test */ 
      qui sureg (death_rate: death_rate_up strong_action strong_info i.group, rob) ///
          (deaths: deaths_up strong_action strong_info i.group, rob) ///
          (cases: cases_up strong_action strong_info i.group, rob)

    test ( [death_rate][strong_action] = 0) ( [deaths][strong_action] = 0) ( [cases][strong_action] = 0)
    local p: di %5.3f `r(p)'
    noisily di `p'
  
    foreach outcome in death_rate_up cases_up deaths_up {

      * get "main effect" from tests 
      eststo: reghdfe `outcome' strong_action strong_info, absorb(group)   vce(rob)

      if "`outcome'" == "death_rate_up" { 
        estadd local rp = `p' 
      }
  }

  local name: word 1 of `statement'
  if ("`statement'" == "sa_invisible_enemy == 1 | wa_less_flu == 1") local name = "both" 

    esttab using out/statement_`name'.tex, replace se ///
        label drop(strong_info _cons) star(* 0.1 ** 0.05 *** 0.01) ///
        mlabel("\shortstack{Updates up: \\ death rate} " "\shortstack{Updates up: \\ cases} " "\shortstack{Updates up: \\ deaths}") ///
        scalars("rp \textit{p}-value from joint Wald test")    

  restore
  }   
}



