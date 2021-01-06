/* study the effect on preferences of all treatments */
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

set matsize 10000
/*****************/
/* EXPERIMENT 1  */
/*****************/

use "$int/launch_1_pol" , clear 
eststo clear

la var strong_action "Strong Action" 
la var strong_info "Information" 
global testinfo = " "
global testaction = " "
global falkregvar = " "

* set up t test 
foreach var in takes_risks patience punish_you punish_others return_favor revenge good_intentions postpone_tasks give {
  global falkregvar = " $falkregvar (`var': `var' strong_info strong_action i.group, r)" 
  global testinfo = " $testinfo ([`var'][strong_info] = 0) "
  global testaction = " $testaction ([`var'][strong_action] = 0) "
}

sureg (dict_fract: dict_fract strong_info strong_action day_1 i.group, r ) $falkregvar

* convert to t tests 
test ([dict_fract][strong_info] = 0) $testinfo
global finfo: di %5.3f `r(p)'

test ([dict_fract][strong_action] = 0) $testaction
global faction: di %5.3f `r(p)'

test ([dict_fract][strong_info] = 0) ([dict_fract][strong_action]) $testinfo $testaction
global fboth: di %5.3f `r(p)'

* put into a file
eststo clear 
eststo: reghdfe dict_fract strong_info strong_action day_1, absorb(group)   vce(rob)
estadd local ftest = ""
estadd local finfo = $finfo
estadd local faction = $faction
estadd local fboth = $fboth

/* effects on preferences */ 
foreach f in takes_risks patience punish_you punish_others return_favor revenge good_intentions postpone_tasks give {
  eststo: reghdfe `f'_std strong_info strong_action, absorb(group)   vce(rob)
  estadd local ftest = ""
  estadd local finfo = ""
  estadd local faction = ""
  estadd local fboth = ""
}

esttab using out/exp_1_falk.tex, replace se label drop(day_1 _cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel(" \shortstack{Dictator game \\ (pp shared)}"  "Takes risks" "Patience" " \shortstack{Punish \\ (for self)}" " \shortstack{Punish \\ (for others)}" "\shortstack{Return \\ favor}" "\shortstack{Take \\ revenge}" " \shortstack{Others have \\ good \\ intentions} " "\shortstack{Postpone \\ tasks}" "Give to others"  ) ///
    scalars("ftest \ul{\textit{p}-value from Wald test of...}" "finfo Info treatment" "faction Action treatment" "fboth Both treatments") ///
    sfmt(%5.3f) 

/*****************/
/* Experiment 2  */
/*****************/

use "$int/public_data" , clear
la var strong "Strong action"
la var shifty "Changing action"

eststo clear 
eststo: reghdfe risky shifty, absorb(stratum)   vce(rob)
estadd local Sample "All"

eststo: reghdfe risky strong, absorb(stratum)   vce(rob)
estadd local Sample "All"

esttab using out/exp_2_pref.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("Risky gamble" "Risky gamble" "Risky gamble" "Risky gamble") scalars("Sample Sample")


