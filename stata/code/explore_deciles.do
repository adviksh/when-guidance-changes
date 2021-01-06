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
keep if shifty != .
foreach decile of numlist 0(10)90 {
  local decile_plus_10 = `decile' + 10
  
  cap qreg width_pre, q(`decile')
  if _rc == 0 local low = _b[_cons]
  if _rc != 0 local low = 0
  
  cap qreg width_pre, q(`decile_plus_10')
  if _rc == 0 local high = _b[_cons]
  if _rc != 0 local high = . 
  
  gen q_width_`decile'_`decile_plus_10' = inrange(width_pre,`low',`high'-.00000001) 
  replace q_width_`decile'_`decile_plus_10' = . if mi(width_pre)

  cap qreg deaths_pre, q(`decile')
  if _rc == 0 local low = _b[_cons]
  if _rc != 0 local low = 0
  
  cap qreg deaths_pre, q(`decile_plus_10')
  if _rc == 0 local high = _b[_cons]
  if _rc != 0 local high = . 
  
  cap gen q_deaths_`decile'_`decile_plus_10' = inrange(deaths_pre,`low',`high'-.00000001) 
  replace q_deaths_`decile'_`decile_plus_10' = . if mi(deaths_pre) 
}

cap file close fh
cap file open fh using out/decile_width.csv, write replace
file write fh "outcome,q,b,se,by" _n

foreach decile of numlist 0(10)90 {
  local decile_plus_10 = `decile' + 10

  foreach outcome in update_mag_deaths update_covid { 
    reghdfe `outcome' shifty if q_width_`decile'_`decile_plus_10' == 1, vce(rob) absorb(stratum)
    local est = _b[shifty]
    local se = _se[shifty]

    file write fh "`outcome',`decile',`est',`se',width" _n

    reghdfe `outcome' shifty if q_deaths_`decile'_`decile_plus_10' == 1, vce(rob) absorb(stratum)
    local est = _b[shifty]
    local se = _se[shifty]

    file write fh "`outcome',`decile',`est',`se',deaths" _n
    
    qui reg `outcome' shifty if q_deaths_`decile'_`decile_plus_10' == 1, vce(rob) 
    local ratio = (_b[shifty])/(_b[_cons])
    
    file write fh "`outcome',`decile',`ratio',,diff" _n 

    
    
  }

}

/******************/
/* /\* graph  *\/ */
/******************/
cap file close fh
/****** coef ratio ******/ 
import delimited using out/decile_width, clear
keep if by == "diff" & outcome == "update_mag_deaths" 
twoway ///
    (scatter b q) ///
    (lfit b  q) ///
    
grout out/ratio_deaths , pdf 

import delimited using out/decile_width, clear
keep if by == "diff" & outcome == "update_covid" 
twoway ///
    (scatter b q, ) ///
    (lfit b  q) ///
    
grout out/ratio_frac , pdf 

import delimited using out/decile_width, clear
keep if by == "deaths" 
gen high = b + 1.96 * se
gen low = b - 1.96 * se

twoway /// 
    (scatter b q if outcome == "update_mag_deaths", yline(0, lpattern(dash) lcolor(gray) ) ///
    ytitle("Treatment effect on update magnitude") msymbol(O)) ///
    (lfit b q if outcome == "update_mag_deaths", lpattern(line) xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) )

grout out/decile_update_mag_deaths_deaths , pdf

twoway /// 
    (scatter b q if outcome == "update_covid", yline(0, lpattern(dash) lcolor(gray) ) ytitle("Treatment effect on update propensity") msymbol(O)) ///
    (lfit b q if outcome == "update_covid", lpattern(line) xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) )

grout out/decile_update_covid_deaths , pdf

import delimited using out/decile_width, clear
keep if by == "width" 
twoway /// 
(scatter b q if outcome == "update_mag_deaths") ///
    (lfit b q if outcome == "update_mag_deaths")

grout out/decile_update_mag_deaths_width , pdf

twoway /// 
(scatter b q if outcome == "update_covid") ///
    (lfit b q if outcome == "update_covid")

grout out/decile_update_covid_width , pdf

/**********************/
/* TRY WITH VENTILES  */
/**********************/
use "$int/public_data" , clear
keep if shifty != .
foreach ventile of numlist 0(20)80 {
  local ventile_plus_20 = `ventile' + 20
  
  cap qreg width_pre, q(`ventile')
  if _rc == 0 local low = _b[_cons]
  if _rc != 0 local low = 0
  
  cap qreg width_pre, q(`ventile_plus_20')
  if _rc == 0 local high = _b[_cons]
  if _rc != 0 local low = 0
  
  gen q_width_`ventile'_`ventile_plus_20' = inrange(width_pre,`low',`high'-.00000001) 
  replace q_width_`ventile'_`ventile_plus_20' = . if mi(width_pre)

  cap qreg deaths_pre, q(`ventile')
  if _rc == 0 local low = _b[_cons]
  if _rc != 0 local low = 0
  
  cap qreg deaths_pre, q(`ventile_plus_20')
  if _rc == 0 local high = _b[_cons]
  if _rc != 0 local low = 0
  
  cap gen q_deaths_`ventile'_`ventile_plus_20' = inrange(deaths_pre,`low',`high'-.00000001) 
  replace q_deaths_`ventile'_`ventile_plus_20' = . if mi(deaths_pre) 
}

cap file close fh
cap file open fh using out/ventile_width.csv, write replace
file write fh "outcome,q,b,se,by" _n

foreach ventile of numlist 0(20)90 {
  local ventile_plus_20 = `ventile' + 20

  foreach outcome in update_mag_deaths update_covid { 
    reghdfe `outcome' shifty if q_width_`ventile'_`ventile_plus_20' == 1, vce(rob) absorb(stratum)
    local est = _b[shifty]
    local se = _se[shifty]

    file write fh "`outcome',`ventile',`est',`se',width" _n

    reghdfe `outcome' shifty if q_deaths_`ventile'_`ventile_plus_20' == 1, vce(rob) absorb(stratum)
    local est = _b[shifty]
    local se = _se[shifty]

    file write fh "`outcome',`ventile',`est',`se',deaths" _n 

    
  }
}

/* graph  */

cap file close fh
import delimited using out/ventile_width, clear
keep if by == "deaths" 
gen high = b + 1.96 * se
gen low = b - 1.96 * se

twoway /// 
    (scatter b q if outcome == "update_mag_deaths") ///
    (rcap high low q if outcome == "update_mag_deaths") ///    
    (lfit b q if outcome == "update_mag_deaths")

grout out/ventile_update_mag_deaths_deaths , pdf

twoway /// 
    (scatter b q if outcome == "update_covid") ///
    (rcap high low q if outcome == "update_covid") ///        
    (lfit b q if outcome == "update_covid")

grout out/ventile_update_covid_deaths , pdf

import delimited using out/ventile_width, clear
keep if by == "width" 
twoway /// 
(scatter b q if outcome == "update_mag_deaths") ///
    (lfit b q if outcome == "update_mag_deaths")

grout out/ventile_update_mag_deaths_width , pdf

twoway /// 
(scatter b q if outcome == "update_covid") ///
    (lfit b q if outcome == "update_covid")

grout out/ventile_update_covid_width , pdf

