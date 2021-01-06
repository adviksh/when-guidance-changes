/*****************************************************/
/* /\* This dofile studies revealed pref outcomes *\/ */
/*****************************************************/
set seed 200 
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

/*** set up number of bootstraps ***/ 
local boot 500 

use "$int/public_data" , clear
keep if shifty != . 

cap file close fh
cap file open fh using out/quantile_abs.csv, write replace
file write fh "outcome,q,b,se" _n

/* loop over relevant quantiles */
foreach outcome in abs_mag_deaths update_mag_deaths { 
forv q = 58/99 {
  bsqreg `outcome' shifty, q(.`q') reps(`boot')
    local b = _b[shifty]
    local se = _se[shifty]
    
  file write fh "`outcome',`q',`b',`se'" _n 
  }
}

/**********/
/* graph  */
/**********/
cap file close fh 
import delimited using out/quantile_abs.csv, clear

gen high = b + 1.96 * se
gen low = b - 1.96 * se

foreach outcome in abs_mag_deaths update_mag_deaths {
  preserve
  
  * statement in text that omitted quantiles are statistically 0 
  assert high > 0 & low < 0 if q > 92 
   if "`outcome'" == "abs_mag_deaths" drop if q > 92
    
    twoway ///
        (rcap high low q if outcome == "`outcome'", color(black) ) ///
        (scatter b q if outcome == "`outcome'", color(black) legend(off) ///
          xtitle("Quantile") ytitle("Quantile regression estimate") yline(0) ylabel(#5,format(%09.0fc) ) )
  grout out/`outcome'_quantile_abs , pdf

restore  
    }    



