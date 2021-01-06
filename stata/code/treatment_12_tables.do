/******************************************************************/
/* /\* This makes the formal tables for the information and action experiments *\/ */
/******************************************************************/
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
use "$int/launch_1_pol" , clear

la var deaths_up "\shortstack{Update up: \\ deaths}"
la var cases_up "\shortstack{Update up: \\ cases}"
la var death_rate_up "\shortstack{Update up: \\ death rate}"

la var deaths_up "\shortstack{Update down \\ deaths}"
la var cases_up "\shortstack{Update down \\ cases}"
la var death_rate_up "\shortstack{Update down \\ death_rate}"

la var strong_info "Information"
la var strong_action "Strong Action"


/***********************/
/* omnibus wald tests  */
/***********************/
sureg (death_rate: death_rate_up strong_action strong_info i.group, rob) (deaths: deaths_up strong_action strong_info i.group, rob) (cases: cases_up strong_action strong_info i.group, rob)
test ( [death_rate][strong_info] = 0) ( [deaths][strong_info] = 0) ( [cases][strong_info] = 0)
local p: di %5.3f `r(p)'
local strong_info_up =  `p' 

test ( [death_rate][strong_action] = 0) ( [deaths][strong_action] = 0) ( [cases][strong_action] = 0)
local p: di %5.3f `r(p)'
local strong_action_up = `p'

sureg (death_rate: death_rate_down strong_action strong_info i.group, rob) (deaths: deaths_down strong_action strong_info i.group, rob) (cases: cases_down strong_action strong_info i.group, rob)
test ( [death_rate][strong_info] = 0) ( [deaths][strong_info] = 0) ( [cases][strong_info] = 0)
local p: di %5.3f `r(p)'
local strong_info_down = `p'

test ( [death_rate][strong_action] = 0) ( [deaths][strong_action] = 0) ( [cases][strong_action] = 0)
local p: di %5.3f `r(p)'
local strong_action_down = `p'

* loop over strong action and info treatments for exporting and doing RI 
foreach treatment in strong_info strong_action {
eststo clear

  foreach direction in up down { 

    foreach outcome in "deaths" "cases" "death_rate" {

        * randomization inference for each column 
        ritest `treatment' _b[`treatment'] , reps($reps) seed(100) : ///
              reghdfe `outcome'_`direction' strong_info strong_action  , absorb(group) vce(rob)

          matrix p = r(p)
          local p_`treatment': di %5.3f p[1,1]
        
        * run the outcome 
        eststo: reghdfe `outcome'_`direction' strong_info strong_action , absorb(group)  vce(rob)
        estadd local ri = `p_`treatment''
    
    if ("`outcome'" == "deaths") estadd local pwald_`treatment' = ``treatment'_`direction''
     
    }
  }
  
  if "`treatment'" == "strong_info" local other = "strong_action"
  if "`treatment'" == "strong_action" local other = "strong_info"

  * export 
esttab using out/exp_1_`treatment'.tex, replace se label drop(`other' _cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update up: \\ deaths}" "\shortstack{Update up: \\ cases}" "\shortstack{Update up: \\ death rate}" ///
    "\shortstack{Update down: \\ deaths}" "\shortstack{Update down: \\ cases}" "\shortstack{Update down: \\ death rate}" ) /// 
    scalars("head \underline{\textit{p}-value from}..." "ri Randomization inference" ///
      "pwald_`treatment' Joint Wald: Cols. 1--3 / 4--6") 
      
}
  

