/*****************************************************/
/* /\* This dofile builds the info vs. shifty graphs *\/ */
/*****************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/Documents/Github/COVID/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

/*****************************************/
/* looks at effect of social distancing  */
/*****************************************/
use "$int/public_data" , clear
keep if shifty != .
la var shifty "Inconsistent"

assert !mi(number_others) 


/* standardize relative to control group */
sum number_others if shifty == 0 
gen social_distancing_index_std = - (number_others - `r(mean)') / (`r(sd)') 

gen will_see_others = number_others > 0 

/* data task control */
assert !mi(index_covid) | !mi(index_metro) 
gen data_task_covid = index_covid != . 

foreach outcome in social_distancing_index_std will_see_others {
    local will_see_othersnm = "1(plans to see others)"   
    local social_distancing_index_stdnm = "\shortstack{Social distancing \\ index (sd)}"
  
    eststo clear
  
    eststo: reghdfe `outcome' shifty  , absorb(stratum) vce(rob)
    estadd local data "No"
    estadd local sample "All"

    eststo: reghdfe `outcome' shifty data_task_covid , absorb(stratum) vce(rob)
    estadd local data "\$\checkmark\$"
    estadd local sample "All"

    eststo: reghdfe `outcome' shifty data_task_covid if priors_above_info == 1 , absorb(stratum) vce(rob)
    estadd local data "\$\checkmark\$"
    estadd local sample "Priors above info"

    eststo: reghdfe `outcome' shifty data_task_covid if priors_at_info == 1 , absorb(stratum) vce(rob)
    estadd local data "\$\checkmark\$"
    estadd local sample "Priors at info"

    eststo: reghdfe `outcome' shifty data_task_covid if priors_below_info == 1 , absorb(stratum) vce(rob)
    estadd local data "\$\checkmark\$"
    estadd local sample "Priors below info"
  
    esttab using out/`outcome'_distancing.tex, ///
        label drop(_cons data_task_covid) mlabel("``outcome'nm'" ///
        "``outcome'nm'" ///
        "``outcome'nm'" ///
        "``outcome'nm'" ///
        "``outcome'nm'" ) ///
        scalars("data Data-task control" "sample Sample") replace se star(* 0.1 ** 0.05 *** 0.01)
}

la var gov_index_post_std "Government credibility index" 

eststo clear
eststo: ivreghdfe will_see_others (gov_index_post_std = shifty) gov_index_pre_std data_task_covid, absorb(stratum) rob
estadd local data "\$\checkmark\$"
estadd local sample "All"

eststo: ivreghdfe social_distancing_index_std (gov_index_post_std = shifty) gov_index_pre_std data_task_covid, absorb(stratum) rob
estadd local data "\$\checkmark\$"
estadd local sample "All"

esttab using out/iv_distancing.tex, ///
    label drop(gov_index_pre_std data_task_covid) mlabel("1(plans to see others)"  ///
    "\shortstack{Social distancing \\ index (sd)}" ) scalars("data Data-task control") replace se star(* 0.1 ** 0.05 *** 0.01)



