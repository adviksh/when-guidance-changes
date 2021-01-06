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
label var shifty "Inconsistent" 
/************************************************/
/* /\* effect on update magnitude by party  *\/ */
/************************************************/
eststo clear

foreach outcome in update_mag_deaths update_covid  { 
  foreach party in 2 1 0 {
    local nm0 "Oppose Trump"
    local nm1 "Undecided"
    local nm2 "Support Trump"


/**** test of treatment effect heterogeneity ****/ 
    destring stratum, replace 
    qui reg `outcome' shifty i.stratum if party == 2 // Note that suest wont accept non-robust SEs for the preliminary
    estimates store `outcome'_rep

    qui reg `outcome' shifty i.stratum if party != 2 // Note that suest wont accept non-robust SEs for the preliminary
    estimates store `outcome'_not_rep

    qui suest `outcome'_rep `outcome'_not_rep, rob // Now you fit the simultaneous robust SEs
    test [`outcome'_rep_mean][shifty] = [`outcome'_not_rep_mean][shifty]
    local p: di %5.3f `r(p)' 
    
    qui eststo: reghdfe `outcome' shifty if party == `party', absorb(stratum) vce(rob)
    estadd local sample "`nm`party''"
    di "p is `p'"
    if (`party' == 2) estadd local pval = `p'
    if (`party' != 2) estadd local pval ""

    sum gov_index_pre_std if party == `party'
    local mp: di %5.3f `r(mean)'
    estadd local meanbyparty `mp' 
  }
}

esttab using out/partisanship.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update \\ magnitude}" ///
    "\shortstack{Update \\ magnitude}" ///
    "\shortstack{Update \\ magnitude}" ///    
  "\shortstack{Update \\ propensity}" ///
    "\shortstack{Update \\ propensity}" ///
  "\shortstack{Update \\ propensity}" ///    
    ) scalars("sample Sample" "meanbyparty Mean prior government credibility" /// 
    "note \ul{\textit{p}-value from:}" ///
    "pval Support Trump = Not Support Trump" )

/**********************************************************/
/* heterogeneity by whether attitudes are extreme or not  */
/**********************************************************/
eststo clear 
sum gov_index_pre_std ,d
gen extreme_attitudes = gov_index_pre_std >= `r(p75)' | gov_index_pre_std <= `r(p25)'

foreach outcome in update_mag_deaths update_covid  { 
  foreach extreme in 1 0  {
    local nm0 "Not Extreme"
    local nm1 "Extreme"
   
    /**** test of treatment effect heterogeneity ****/ 
    destring stratum, replace 
    qui reg `outcome' shifty i.stratum if extreme_attitudes == 1 // Note that suest wont accept non-robust SEs for the preliminary
    estimates store `outcome'_ext

    qui reg `outcome' shifty i.stratum if extreme_attitudes != 1 // Note that suest wont accept non-robust SEs for the preliminary
    estimates store `outcome'_not_ext

    qui suest `outcome'_ext `outcome'_not_ext, rob // Now you fit the simultaneous robust SEs
    test [`outcome'_ext_mean][shifty] = [`outcome'_not_ext_mean][shifty]
    local p: di %5.3f `r(p)' 

    qui eststo: reghdfe `outcome' shifty if extreme_attitudes == `extreme', absorb(stratum) vce(rob)
    estadd local samp = "`nm`extreme''"
    if (`extreme' == 1) estadd local pv = `p'
    if (`extreme' != 1) estadd local pv ""
    
  }
}

esttab using out/extreme.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update \\ magnitude}" ///
    "\shortstack{Update \\ magnitude}" ///
  "\shortstack{Update \\ propensity}" ///
  "\shortstack{Update \\ propensity}" ///
    ) scalars("samp Sample" ///
        "note \ul{\textit{p}-value from:}" ///
    "pv Extreme = Not Extreme" ///
    )




