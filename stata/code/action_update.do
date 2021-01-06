/**************************************************************************/
/* /\* This dofile looks at direction of update for action treatment *\/  */
/**************************************************************************/

/*********************************************************/
/* This shows the effect of updating by specific priors  */
/*********************************************************/
do ../SetGlobals.do


/* helper function to write into a csv  */
cap pr drop write_est
cap pr define write_est
syntax, outcome(string) direction(string) [cov(string) value(string)] 

    local est = _b[strong]
    local se = _se[strong]
    file write fh "`outcome',`direction',`cov',`value'," (`est') "," (`se') _n 

end 

use "$int/public_data", clear 

cap file close fh
file open fh using out/update_strong.csv, write replace 
file write fh "outcome,direction,cov,value,est,se" _n

foreach outcome in death_rate_old death_rate_young deaths dji { 
foreach direction in up down {
  
  reghdfe update_`direction'_`outcome'  strong , absorb(stratum)
  write_est, outcome(`outcome') direction(`direction')
}
}
cap file close fh

/*******************/
/* convert to pdf  */
/*******************/
import delimited using out/update_strong.csv, clear
keep if mi(cov)
replace outcome = "a.dji" if outcome == "dji" // fix outcome for sort 
sort direction outcome  
gen n = _n
gen high = est + 1.96 * se
gen low = est - 1.96 * se

gr twoway ///
  (bar est n if outcome == "death_rate_old", color($ltblue) horizontal  ) ///  
  (bar est n if outcome == "death_rate_young", color($navy) horizontal  ) ///
  (bar est n if outcome == "deaths", color(gray) horizontal  )  /// 
  (bar est n if outcome == "a.dji", color($green) horizontal  )  ///
  (rcap high low n, color(black) horizontal ylab(1 "Update down" 2 "Update down" 3 "Update down" 4 "Update down" 5 "Update up" 6 "Update up" 7 "Update up" 8 "Update up", labsize(large)) ///
  legend(lab(1 "Death rate (51-80)") lab(2 "Death rate (20-50)") lab(3 "Deaths") lab(4 "Dow Jones") size(medlarge) cols(2) order(3 2 1 4) ) ) ///
  (function y=0, horizontal color(black) range(0.5 8.5)  ) ///
  (function y=4.5, xlab(-.15(.05).15, labsize(large)) xtitle("Fraction updating in the given direction", size(large)) color(gray) lpattern(dash) range(-.15 .15)  )

grout out/action_update_direction_l2 , pdf
