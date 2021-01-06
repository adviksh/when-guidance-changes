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

eststo clear
keep if !mi(strong)
la var strong "Strong Action"

/***********************/
/* loop over outcomes  */
/***********************/
foreach direction in up down {
  foreach outcome in deaths death_rate_young death_rate_old dji { 

  * randomization inference
    ritest strong _b[strong] , strata(ri_stratum) reps($reps) seed(100): ///
      reghdfe update_`direction'_`outcome'  strong  , absorb(stratum)   vce(rob)  
    matrix p = r(p)
    local p: di %5.3f p[1,1] 
    
    * treatment effect    
    eststo: reghdfe update_`direction'_`outcome'  strong , absorb(stratum)
    estadd local ri = `p'    
    
  }
}


/* export to a table */ 
esttab using out/exp_4_strong_action.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update up: \\ total \\ deaths}" "\shortstack{Update up: \\ death rate \\ (for ages 20--50)}" ///
    "\shortstack{Update up: \\ death rate \\ (for ages 51--80)}"  "\shortstack{Update up: \\ Dow Jones}" ///
"\shortstack{Update down: \\ total \\ deaths}" "\shortstack{Update down: \\ death rate \\ (for ages 20--50)}" ///
    "\shortstack{Update down: \\ death rate \\ (for ages 51--80)}"  "\shortstack{Update down: \\ Dow Jones}" ) /// 
    scalars("ri \textit{p}-value: randomiz. inf." )



