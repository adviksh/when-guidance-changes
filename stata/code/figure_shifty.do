/************************/
/* part 2 make figures  */
/************************/
/****************************/
/* /\* append the data *\/  */
/****************************/


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

/****************************/
/* 1. get update magnitudes */
/****************************/
use "$int/public_data" , clear 
keep if !mi(shifty) 

cap file close fh 
file open fh using out/update_shifty.csv, write replace
file write fh "outcome,cut,group,est,se" _n

foreach outcome in update_covid update_mag_deaths  { 

    /********************************************************/
    /* /\* 1. effect of shifty on propensity to update  *\/ */
    /********************************************************/
    reghdfe `outcome' shifty , absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome',,," (`est') "," (`se') _n

    /* BY PRIOR */ 
    reghdfe `outcome' shifty if inrange(deaths_pre,100000,240000), absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome',info_vs_priors,1," (`est') "," (`se') _n

    reghdfe `outcome' shifty if inrange(deaths_pre,0,99999), absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome',info_vs_priors,0," (`est') "," (`se') _n

    reghdfe `outcome' shifty if inrange(deaths_pre,240001,.), absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome',info_vs_priors,2," (`est') "," (`se') _n

}

/***************************/
/* outcomes with controls  */
/***************************/

foreach outcome in gov_index ln_deaths {

    reghdfe `outcome'_post `outcome'_pre shifty , absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome'_post,,," (`est') "," (`se') _n

    /* BY PRIOR */ 
    reghdfe `outcome'_post `outcome'_pre shifty if inrange(deaths_pre,100000,240000), absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome'_post,info_vs_priors,1," (`est') "," (`se') _n

    reghdfe `outcome'_post `outcome'_pre shifty if inrange(deaths_pre,0,99999), absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome'_post,info_vs_priors,0," (`est') "," (`se') _n

    reghdfe `outcome'_post `outcome'_pre shifty if inrange(deaths_pre,240001,.), absorb(stratum) vce(r)
    local est = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome'_post,info_vs_priors,2," (`est') "," (`se') _n

}

capture file close fh

/************************************/
/* 1. effect on updating, by update  */
/************************************/
import delimited using out/update_shifty.csv, clear

gen high_95 = est + 1.96 * se
gen low_95 = est - 1.96 * se

gen high_90 = est + 1.645 * se
gen low_90 = est - 1.645 * se

foreach outcome in update_covid update_mag_deaths {
  
  local xtitleupdate_covid = "proportion changing original estimate"
  local xtitleupdate_mag_deaths = "magnitude of belief update (log points)"
  local xtitlegov_index_post = "index of support for government response (sd)"
  local xtitleln_deaths_post = "{it:net} belief update (log points)"
  local xtitleabs_deaths = "magnitude of belief update (number of deaths)"
  local xtitleabs_ln_deaths_post = "magnitude of belief update {it:toward} projection"
  
    drop if cut == "party"
    replace group = 3 if group == .

    global ylab 0 "Priors < Info" 1 "Priors = Info" 2 "Priors > Info" 3 "All" 

    twoway ///
        (bar est group if group == 3 & outcome == "`outcome'", horizontal lcolor(white) ylab( $ylab , labsize(large)) color(gs12) ) ///
      (bar est group if group < 3 & outcome == "`outcome'", xlab(,labsize(large)) ///
      xtitle("Effect of Changing treatment on `xtitle`outcome''", size(large) ) horizontal lcolor(white) ylab( $ylab , ///
      labsize(large)) color($ltblue) ) ///
        (rcap high_95 low_95 group if outcome == "`outcome'", horizontal color(gs0) msize(vlarge) legend(off) ) ///
        (function y = 0, horizontal range(-.5 3.5) color(black) xsize(10 )  ) ///

    grout out/shifty_by_prior_`outcome' , pdf 
}
