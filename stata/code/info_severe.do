/*********************************************************/
/* This shows the effect of updating by specific priors  */
/*********************************************************/
do ../SetGlobals.do


/* helper function to write into a csv  */
cap pr drop write_est
cap pr define write_est
syntax, outcome(string) direction(string)

    local est = _b[strong_action]
    local se = _se[strong_action]
    file write fh "`outcome',`direction',action," (`est') "," (`se') _n 

    local est = _b[strong_info]
    local se = _se[strong_info]
    file write fh "`outcome',`direction',info," (`est') "," (`se') _n 

end 


/*********************************************************************/
/* This builds some graphs that show the effect of updating by info  */
/*********************************************************************/

cap file close fh
file open fh using out/info_severe.csv, write replace 
file write fh "outcome,direction,treatment,est,se" _n

use "$int/launch_1_pol", clear

/************************************************/
/* 1. get p-val for test that they are all = 0  */
/************************************************/

/* get p value */ 
sureg (death_rate: death_rate_up strong_action strong_info i.group, rob) (deaths: deaths_up strong_action strong_info i.group, rob) (cases: cases_up strong_action strong_info i.group, rob)
test ( [death_rate][strong_info] = 0) ( [deaths][strong_info] = 0) ( [cases][strong_info] = 0)
local p: di %5.3f `r(p)'
global info_up = "=" + " `p'" 
if `r(p)' < 0.001 {
    global info_up = "< 0.001" 
}

test ( [death_rate][strong_action] = 0) ( [deaths][strong_action] = 0) ( [cases][strong_action] = 0)
local p: di %5.3f `r(p)'
global action_up = "=" + " `p'" 
if `r(p)' < 0.001 {
    global action_up = "< 0.001" 
}

sureg (death_rate: death_rate_down strong_action strong_info i.group, rob) (deaths: deaths_down strong_action strong_info i.group, rob) (cases: cases_down strong_action strong_info i.group, rob)
test ( [death_rate][strong_info] = 0) ( [deaths][strong_info] = 0) ( [cases][strong_info] = 0)
local p: di %5.3f `r(p)'
global info_down = "="  + " `p'"
if `r(p)' < 0.001 {
    global info_down = "< 0.001" 
}

test ( [death_rate][strong_action] = 0) ( [deaths][strong_action] = 0) ( [cases][strong_action] = 0)
local p: di %5.3f `r(p)'
global action_down = "="  + " `p'"
if `r(p)' < 0.001 {
    global action_down = "< 0.001" 
}

foreach outcome in death_rate deaths cases { 
foreach direction in up down { 

  reghdfe `outcome'_`direction'  strong_action strong_info , absorb(group)  vce(rob)
  write_est, outcome(`outcome') direction(`direction')
  
}
}

cap file close fh

/********************/
/* export into pdf  */
/********************/
import delimited using out/info_severe.csv, clear 
keep if treatment == "info"
sort direction outcome  
gen n = _n
gen high = est + 1.96 * se
gen low = est - 1.96 * se

    gr twoway ///
    (bar est n if outcome == "death_rate", color($navy) horizontal  ) ///
    (bar est n if outcome == "deaths", color(gray) horizontal  )  /// 
    (bar est n if outcome == "cases", color($orange) horizontal  )  ///
    (rcap high low n, color(black) horizontal ylab(1 "Update down" 2 "Update down" 3 "Update down" 4 "Update up" 5 "Update up" 6 "Update up", labsize(large)) ///
    legend(lab(1 "Death rate") lab(2 "Deaths") lab(3 "Cases") cols(3) size(large) ) ) ///
    (function y=0, horizontal color(black) range(0.5 6.5) legend(order(1 2 3)) ///
    text(6 -.08 "H{sub:0}: all three coefficients = 0" "{it:p} ${info_up}" ) ///
    text(2 0.09  "H{sub:0}: all three coefficients = 0" "{it:p} ${info_down}" ) ) ///
    (function y=3.5, color(gray) xlab(-.15(.05).15, labsize(large))  lpattern(dash) ///
    xtitle("Fraction updating in the given direction", size(large)) range(-.15 .15) legend(order( 2 1 3)) )


grout out/info_update_direction , pdf


/********************/
/* export into pdf  */
/********************/
import delimited using out/info_severe.csv, clear 
keep if treatment == "action"
sort direction outcome  
gen n = _n
gen high = est + 1.96 * se
gen low = est - 1.96 * se

    gr twoway ///
    (bar est n if outcome == "death_rate", color($navy) horizontal  ) ///
    (bar est n if outcome == "deaths", color(gray) horizontal  )  /// 
    (bar est n if outcome == "cases", color($orange) horizontal  )  ///
    (rcap high low n, color(black) horizontal ylab(1 "Update down" 2 "Update down" 3 "Update down" 4 "Update up" 5 "Update up" 6 "Update up", labsize(large)) ///
    legend(lab(1 "Death rate") lab(2 "Deaths") lab(3 "Cases") cols(3) size(large) order(2 1 3) ) ) ///
    (function y=0, horizontal color(black) range(0.5 6.5) legend(order(1 2 3)) ///
    text(6 -.08 "H{sub:0}: all three coefficients = 0" "{it:p} ${action_up}" ) ///
    text(2 0.09  "H{sub:0}: all three coefficients = 0" "{it:p} ${action_down}" ) ) ///
    (function y=3.5, xlab(-.15(.05).15, labsize(large)) xtitle("Fraction updating in the given direction", size(large)) color(gray) lpattern(dash) range(-.15 .15) legend(order( 2 1 3)) )
    
grout out/action_update_direction , pdf
