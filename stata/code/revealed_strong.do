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

/*****************/
/* EXPERIMENT 1  */
/*****************/
use "$int/launch_1_pol" , clear

reg ln_wtp_index shifty if info_below_priors == 1

eststo clear

la var strong_action "Strong Action" 

sum memory if strong_action == 0 & strong_info == 0 
gen memory_std = (memory - `r(mean)' )/ `r(sd)' 

* randomization inference for each outcome
foreach outcome in memory_std ln_wtp_index dict_fract demand_info {

  * looping over each treatment

  ritest strong_action _b[strong_action] , reps($reps) seed(100): ///
      reghdfe `outcome' strong_info strong_action  , absorb(group)   vce(rob)  
  matrix p = r(p)

  * store p value as a local 
  local `outcome'_strong_action_p: di %5.3f p[1,1]

}

* memory 
eststo: reghdfe memory_std strong_info strong_action , absorb(group)   vce(rob)
estadd local ri_action = `memory_std_strong_action_p'

* wtp index 
eststo: reghdfe ln_wtp_index strong_info strong_action , absorb(group)   vce(rob)
estadd local ri_action = `ln_wtp_index_strong_action_p'

* Fraction shared, note dict_fract is in units of 0 to 100 
eststo: reghdfe dict_fract strong_info strong_action , absorb(group)   vce(rob)
estadd local ri_action = `dict_fract_strong_action_p'

* demand_info 
eststo: reghdfe demand_info strong_info strong_action , absorb(group)   vce(rob)
estadd local ri_action = `demand_info_strong_action_p'

esttab using out/exp_1.tex, replace se label drop(strong_info _cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("Memory task (sd)" "ln(WTP)" "Dictator game (pp shared)" "Demands additional info")  ///
    scalars("ri_action \textit{p}-value: randomization inference" ) 


/*****************/
/* EXPERIMENT 2  */
/*****************/

* Panel A: Performance on Effort Task
foreach treatment in strong   {
  use "$int/public_data" , clear
  la var strong "Strong Action"
  la var shifty "Changing Action"
  
  eststo clear

  * get randomization inference p values
  foreach task in covid metro {
    local covidnm COVID
    local metronm Metro
    
    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe index_`task' `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_`task': di %5.3f p[1,1] 

    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe frac_accurate_`task'_std `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_frac_`task': di %5.3f p[1,1]
    
    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe speed_`task'_std `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_speed_`task': di %5.3f p[1,1] 

    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe speed_alt_`task'_std `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_speed_alt_`task': di %5.3f p[1,1]    
  
    eststo: reghdfe index_`task' `treatment'  , absorb(stratum)
    estadd local ri = `rip_`task''
    estadd local task = "``task'nm'"
    
    eststo: reghdfe frac_accurate_`task'_std `treatment'  , absorb(stratum)
    estadd local ri = `rip_frac_`task''
    estadd local task = "``task'nm'"

    eststo: reghdfe speed_`task'_std `treatment'  , absorb(stratum)
    estadd local ri = `rip_speed_`task''
    estadd local task = "``task'nm'"

    eststo: reghdfe speed_alt_`task'_std `treatment'  , absorb(stratum)
    estadd local ri = `rip_speed_alt_`task''    
    estadd local task = "``task'nm'"
    
  }
  
    esttab using out/exp_2_effort`group'`treatment'.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
      mlabel("\shortstack{Index \\ (sd)}" ///
      "\shortstack{Accuracy \\ (sd)}" ///
      "\shortstack{Speed \\ clicks (sd)}" ///
      "\shortstack{Speed \\ time on page (sd)}" ///
      "\shortstack{Index \\ (sd)}" ///
      "\shortstack{Accuracy \\ (sd)}" ///
      "\shortstack{Speed \\ clicks (sd)}" ///
      "\shortstack{Speed \\ time on page (sd)}" ) /// 
      scalars("task Task" "ri \textit{p}-value: randomization inference") sfmt(%5.3f) 

      * Panel B: Demand for Info
      destring stratum, replace
      sureg (cute: dfi_cute `treatment' i.stratum, rob) (well: dfi_well `treatment' i.stratum, rob) (hi: dfi_hi `treatment' i.stratum, rob)
      test ([cute][`treatment'] = 0 ) ([well][`treatment'] = 0 ) ([hi][`treatment'] = 0 )

      local fp: di %5.4f `r(p)'

    * randomization inference
    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe dfi_cute `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_cute: di %5.3f p[1,1] 
    
    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe dfi_well `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_well: di %5.3f p[1,1]     

    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe dfi_hi `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_hi: di %5.3f p[1,1]     

    ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe dfi_death `treatment'  , absorb(stratum)   vce(rob)
    matrix p = r(p)
    local rip_death: di %5.3f p[1,1]        
    
    eststo clear 
    eststo: reghdfe dfi_cute `treatment' , absorb(stratum)   vce(rob)
    
    local sfp: di %5.3f `fp'
    estadd local ri = `rip_cute'    
    estadd local fp = `sfp'

    eststo: reghdfe dfi_well `treatment' , absorb(stratum)   vce(rob)
    estadd local ri = `rip_well'        
    estadd local fp = "" 

    eststo: reghdfe dfi_hi `treatment' , absorb(stratum)   vce(rob)
    estadd local ri = `rip_hi'        
    estadd local fp = "" 

    eststo: reghdfe dfi_death `treatment' , absorb(stratum)   vce(rob)
    estadd local ri = `rip_death'            
    estadd local fp = "" 

        esttab using out/exp_2_dfi`group'`treatment'.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
        mlabel("Cute animals" "Wellness" "Health insurance" "Death counts" ) ///
        scalars("ri \textit{p}-value: randomization inference" "fp \textit{p}-value: joint Wald test")  sfmt(%5.3f) 
    
        * Panel C: WTP
        cap gen wtp_index_ns = wtp_index - 4*wtp_sunscreen // subtract off demand for sunscreen from demand for each additional outcome 
        cap gen ln_wtp_ns = ln(wtp_index_ns + 1)

    * Randomization INference
    
    eststo clear     
    foreach outcome in index n95 purell toilet coffee sunscreen { 
      ritest `treatment' _b[`treatment'] , strata(ri_stratum) reps($reps) seed(100) : reghdfe ln_wtp_`outcome' `treatment' , absorb(stratum)   vce(rob)
      matrix p = r(p)
      local rip_`outcome': di %5.3f p[1,1]
      
      eststo: reghdfe ln_wtp_`outcome' `treatment' , absorb(stratum)   vce(rob)
      estadd local ri = `rip_`outcome'' 
    }
   
        esttab using out/exp_2_wtp`group'`treatment'.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
        mlabel("Index (excl. sunscreen)"  "N95" "Purell" "Toilet Paper" "Coffee" "Sunscreen")         ///
        scalars("ri \textit{p}-value: randomization inference")  sfmt(%5.3f) 

}

