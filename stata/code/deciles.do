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

/***************************************************/
/* form deciles of the prior beliefs distribution  */
/***************************************************/
use "$int/public_data" , clear
keep if shifty != .
foreach decile of numlist 0(10)90 {
  assert !mi(deaths_pre)
  
  local decile_plus_10 = `decile'+10 
  cap qreg deaths_pre, q(`decile')
  if _rc == 0 local low = _b[_cons]
  if _rc != 0 local low = 0
  
  cap qreg deaths_pre, q(`decile_plus_10')
  if _rc == 0 local high = _b[_cons]
  if _rc != 0 local high = . 
  
  gen q_deaths_`decile'_`decile_plus_10' = inrange(deaths_pre,`low',`high'-1) 
}

assert q_deaths_0_10 + q_deaths_10_20 + q_deaths_20_30 + q_deaths_30_40 + q_deaths_40_50 + q_deaths_50_60 + q_deaths_60_70 + q_deaths_70_80 + q_deaths_80_90 + q_deaths_90_100  == 1 

/***********************/
/* put into a picture  */
/***********************/
cap file close fh
cap file open fh using out/decile_width.csv, write replace
file write fh "outcome,q,b,se,by" _n

foreach decile of numlist 0(10)90 {
  local decile_plus_10 = `decile' + 10

  foreach outcome in update_mag_deaths update_covid {

    /* get the effect on the outcome */
    qui reghdfe `outcome' shifty if q_deaths_`decile'_`decile_plus_10' == 1, vce(rob) absorb(stratum)
    local est = _b[shifty]
    local se = _se[shifty]

    file write fh "`outcome',`decile',`est',`se',deaths" _n

    /* get the effect on the ratio, excluding stratum FEs per the footnote in the paper */ 
    qui reg `outcome' shifty if q_deaths_`decile'_`decile_plus_10' == 1, vce(rob) 
    local ratio = (_b[shifty]+_b[_cons])/(_b[_cons])

    nlcom (_b[shifty]+_b[_cons])/_b[_cons]
    matrix var = r(V)
    local se = sqrt(var[1,1])
    file write fh "`outcome',`decile',`ratio',`se',diff" _n 
    
    
  }

}

/************************************************************/
/* helper function to write numbers into the text directly  */
/************************************************************/
cap pr drop writelatex
pr define writelatex
syntax, number(string) name(string) [digits(real 0)] 

    file write fh "\newcommand{\" 
    file write fh "`name'" 
    file write fh "}{"
    file write fh %5.`digits'fc (`number') "}" _n

end 

cap file close fh
cap file open fh using out/pvaldeciles.tex, write replace 

/******************/
/* /\* graph  *\/ */
/******************/
import delimited using out/decile_width, clear
gen high = b + 1.96 * se
gen low = b - 1.96 * se
keep if by == "diff" & outcome == "update_covid"

gen inv_var = 1/(se^2)

reg b q if by == "diff" & outcome == "update_covid" [pw=inv_var], rob
matrix mat = r(table)
local p: di %5.3f mat[4,1]

writelatex, name(pupdatecovidratio) number(`p') digits(3) 

twoway ///
    (scatter b q, yline(1, lcolor(red))  ytitle("Treatment effect ratio of Inconsistent to Consistent") msymbol(O)  ) ///n
    (lfit b q [pw=inv_var], lpattern(line) xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) ///
    text(1.15 57 "Test of H{sub:0}: slope = 0", place(e)) ///
    text(1.1 60 "{it:p}-value: `p'", place(e) ) )
    
grout out/ratio_frac , pdf 

/******* version with 95% CI ********/
 
twoway ///
    (rarea high low q, color(gs15) )     ///
    (scatter b q, mcolor(black)   ytitle("Treatment effect ratio of Inconsistent to Consistent") msymbol(O)   ///
 xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) ) ///
    (function y =1 , lpattern(solid) lcolor(red) legend(off) range(0 100)  ) 
  
    
grout out/ratio_frac_ci , pdf


/****** coef ratio ******/ 
import delimited using out/decile_width, clear
keep if by == "diff" & outcome == "update_mag_deaths"
gen high = b + 1.96 * se
gen low = b - 1.96 * se

gen inv_var = 1/(se^2)

reg b q if by == "diff" & outcome == "update_mag_deaths" [pw=inv_var], rob
matrix mat = r(table)
local p: di %5.3f mat[4,1]

twoway ///
    (scatter b q, yline(1, lcolor(red)) ytitle("Treatment effect ratio of Inconsistent to Consistent") msymbol(O)  ) ///
    (lfit b q [pw=inv_var], lpattern(line) xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) ///
    text(1.15 57 "Test of H{sub:0}: slope = 0", place(e)) ///
    text(1.1 60 "{it:p}-value: `p'", place(e) ) )
    
grout out/ratio_deaths , pdf

twoway ///
    (rarea high low q, color(gs15) )     ///
    (scatter b q, mcolor(black)   ytitle("Treatment effect ratio of Inconsistent to Consistent") msymbol(O)   ///
    xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) ) ///
(function y =1 , lcolor(red) legend(off) lpattern(solid) range(0 100) ) 

grout out/ratio_deaths_ci , pdf

writelatex, name(pupdatemagdeathsratio) number(`p') digits(3) 
cap file close fh


/****************************/
/* coef  by decile and width  */
/****************************/
import delimited using out/decile_width, clear
gen inv_var = 1/(se^2)

keep if by == "deaths" 
gen high = b + 1.96 * se
gen low = b - 1.96 * se

twoway /// 
    (scatter b q if outcome == "update_mag_deaths", yline(0, lpattern(dash) lcolor(gray) ) ///
    ytitle("Treatment effect on update magnitude") msymbol(O)) ///
    (lfit b q [pw=inv_var] if outcome == "update_mag_deaths", lpattern(line) xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) )

grout out/decile_update_mag_deaths_deaths , pdf

twoway /// 
    (scatter b q if outcome == "update_covid", yline(0, lpattern(dash) lcolor(gray) ) ytitle("Treatment effect on update propensity") msymbol(O)) ///
    (lfit b q [pw=inv_var] if outcome == "update_covid", lpattern(line) xtitle("Percentile of prior beliefs" "{&larr} more optimistic | more pessimistic {&rarr}") xlab(0 "0-9" 10 "10-19" ///
    20 "20-29"     30 "30-39" 40 "40-49" 50 "50-59" 60 "60-69" 70 "70-79" 80 "80-89" 90 "90-100" , angle(45)) legend(off) )

grout out/decile_update_covid_deaths , pdf
