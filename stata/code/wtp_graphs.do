/*****************************************************/
/* /\* This dofile builds the google trends data *\/ */
/*****************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13



/****************************/
/* /\* append the data *\/  */
/****************************/
use "$int/launch_1_pol", replace 
gen launch = 1
tostring political_party , replace
tostring stratum , replace

append using  "$int/public_data" 
replace launch = 2 if launch == .

/***************************************/
/* get a .csv of wtp  by prodcut for each date  */
/***************************************/
cap file close fh 
file open fh using out/compare_launches_wtp.csv, write replace 
file write fh "outcome,launch,party,est,se" _n 

foreach wtp in index pasta coffee n95 purell toilet sunscreen {

  /* wtp among all people */ 
  cap reghdfe ln_wtp_`wtp' strong_action strong_info if launch == 1 , absorb(stratum) vce(rob)

  if _rc == 0 {
      local est = _b[strong_action]
      local se = _se[strong_action]
      file write fh "`wtp',1,," (`est') "," (`se') _n 
  }
  else {
    file write fh _n
  }
  
  cap reghdfe ln_wtp_`wtp' strong if launch == 2 , absorb(stratum)  vce(rob) 
  if _rc == 0  { 
      local est = _b[strong]
      local se = _se[strong]
      file write fh "`wtp',2,," (`est') "," (`se') _n 
  }
  else {
    file write fh _n
  }

  /* by party */ 
  foreach party in 0 1 2 {
  cap reghdfe ln_wtp_`wtp' strong_action strong_info if launch == 1 & party == `party' , absorb(stratum) vce(rob)

if _rc == 0 {    
  local est = _b[strong_action]
  local se = _se[strong_action]
  file write fh "`wtp',1,`party'," (`est') "," (`se') _n 
    }
      else {
    file write fh _n
  }

  cap reghdfe ln_wtp_`wtp' strong if launch == 2  & party == `party', absorb(stratum) vce(rob)
  if _rc == 0 {
      local est = _b[strong]
      local se = _se[strong]
      file write fh "`wtp',2,`party'," (`est') "," (`se') _n 
    }
  else {
    file write fh _n
  }
    
  } 
} 

cap file close fh 

/**************************/
/* launch 1: wtp by good  */
/**************************/
    import delimited using out/compare_launches_wtp.csv, clear 
    keep if launch == 1
    gen high = est + 1.96 * se
    gen low = est - 1.96 * se

    keep if party == .
    gen n = . 
    replace n = 5 if outcome == "index"
    replace n = 4 if outcome == "n95"
    replace n = 3 if outcome == "purell"
    replace n = 2 if outcome == "coffee"
    replace n = 1 if outcome == "pasta"

    global ylab1 5 "Index (all)" 4 "N95 Mask" 3 "Purell" 2 "Coffee" 1 "Pasta" 

    twoway ///
        (bar est n if n == 5 , horizontal lcolor(white) ylab($ylab1 , labsize(large)) color(gray) ) /// 
        (bar est n if n < 5 , lcolor(white) color(ltblue) horizontal ///
        xtitle("Effect of Strong Action on WTP (log points)", size(large)) xlab(,labsize(large))  ) ///
        (rcap high low n, xlab(-0.4(.1).4) legend(off) horizontal color(black) ) ///
        (function y = 0, range(0.5 5.5) horizontal color(black)  ) ///

    grout out/wtp_launch_1 , pdf      

 
/**************************/
/* launch 2: wtp by good  */
/**************************/
import delimited using out/compare_launches_wtp.csv, clear 
keep if launch == 2
gen high = est + 1.96 * se
gen low = est - 1.96 * se

keep if party == .
gen n = . 
replace n = 6 if outcome == "index"
replace n = 5 if outcome == "n95"
replace n = 4 if outcome == "purell"
replace n = 3 if outcome == "toilet"
replace n = 2 if outcome == "coffee"
replace n = 1 if outcome == "sunscreen"

global ylab2 6  "Index (excl. sunscreen)*" 5 "N95 Mask" 4 "Purell" 3 "Toilet Paper" 2 "Coffee" 1 "Sunscreen" 

twoway ///
    (bar est n if n == 6 , horizontal lcolor(white) ylab($ylab2 , labsize(large)) color(gray) ) /// 
    (bar est n if n < 6 , lcolor(white) color(ltblue) horizontal ///
    xtitle("Effect of Strong Action on WTP (log)", size(large)) xlab(,labsize(large))  ) ///
    (rcap high low n, xlab(-0.4(.1)0.4) legend(off) horizontal color(black) ) ///
    (function y = 0, range(0.5 6.5) horizontal color(black) note("* pre-registered") ) ///

grout out/wtp_public_data , pdf      

/*********************************/
/* launch 1 and 2: wtp index by political orientation */
/*********************************/
import delimited using out/compare_launches_wtp.csv, clear 
replace party = 3 if party == . 
gen high = est + 1.96 * se
gen low = est - 1.96 * se
keep if outcome == "index"

global ylab 0 "Oppose Trump" 1 "Undecided" 2 "Support Trump" 3 "All" 

foreach i in 1 2 {
  
  twoway ///
      ( bar est party if launch == `i' & party == 0, lcolor(white) horizontal ///
      ylab($ylab , labsize(large ) ) color(ltblue) ) ///
      ( bar est party if launch == `i' & party == 1, lcolor(white) horizontal ///
      ylab($ylab , labsize(large ) ) color(lavender) ) ///
      ( bar est party if launch == `i' & party == 2, horizontal lcolor(white) color(maroon) ) ///
      ( bar est party if launch == `i' & party == 3, lcolor(white) color(gray ) ///                        
       ylab($ylab , labsize(large)) lcolor(white) horizontal ///
      xtitle("Effect of Strong Action on WTP (log points)", size(large)) xlab(,labsize(large))  ) ///
      (rcap high low party if launch == `i', xlab(-0.3(.1)0.3)  legend(off) horizontal color(black) ) ///
      (function y = 0, range(-.5 3.5)  lpattern(line) horizontal color(black) ) ///

  grout out/wtp_party_launch_`i' , pdf      
}


