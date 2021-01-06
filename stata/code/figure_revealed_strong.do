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

/****************************************************************/
/* For slides, make a figure of treatment effects for revealed pref outcomes */
/****************************************************************/
use "$int/launch_1_pol" , clear

sum memory if strong_action == 0 & strong_info == 0 
gen memory_std = (memory - `r(mean)' )/ `r(sd)' 

cap file close fh 
file open fh using out/revealed_strong_fig.csv, write replace 
file write fh "outcome,launch,est,se" _n 

foreach outcome in memory_std ln_wtp_index demand_info {

reghdfe `outcome' strong_action strong_info  , absorb(group) vce(rob)  
local est = _b[strong_action]
local se = _se[strong_action]
file write fh "`outcome',1," (`est') "," (`se') _n
  
}

reghdfe dict_fract strong_action strong_info day_1  , absorb(group) vce(rob)  
local est = _b[strong_action]
local se = _se[strong_action]
file write fh "dict_fract,1," (`est') "," (`se') _n

/*************/
/* launch 2  */
/*************/
clear
use  "$int/public_data" , clear
local treatment strong
destring stratum , replace 
* get pvalue
sureg (cute: dfi_cute `treatment' i.stratum, rob) (well: dfi_well `treatment' i.stratum, rob) (hi: dfi_hi `treatment' i.stratum, rob)
test ([cute][`treatment'] = 0 ) ([well][`treatment'] = 0 ) ([hi][`treatment'] = 0 )

global pdfi = round(`r(p)',0.001)

* demand for information  and willingness to pay 
foreach outcome of varlist dfi_death dfi_cute dfi_hi dfi_well  ln_wtp_sunscreen ln_wtp_coffee ln_wtp_toilet ln_wtp_purell ln_wtp_n95 ln_wtp_index {

    reghdfe `outcome' strong  ,       absorb(stratum)  vce(rob)  
    local est = _b[strong]
    local se = _se[strong]
    file write fh "`outcome',2," (`est') "," (`se') _n

}

* data entry 
foreach outcome in speed_metro_std frac_accurate_metro_std index_metro speed_covid_std frac_accurate_covid_std index_covid  {

    reghdfe `outcome' strong  ,      absorb(stratum)  vce(rob) 
    local est = _b[strong]
    local se = _se[strong]
    file write fh "`outcome',2," (`est') "," (`se') _n
  
}

cap file close fh

/******************/
/* make figures   */
/******************/
/********* main figure ***********/ 
import delimited using out/revealed_strong_fig.csv, clear

keep if launch == 2 
gen n = _n

* high and low 
gen high = est + 1.96 * se
gen low = est - 1.96 * se

gen label = ""
replace label = "{it:Cute animals}" if outcome == "dfi_cute"
replace label = "{it:Health insurance}" if outcome == "dfi_hi"
replace label = "{it:Death counts}" if outcome == "dfi_death"
replace label = "Demand for info: {it:Wellness}" if outcome == "dfi_well"

replace label = "WTP: Index (log points)" if outcome == "ln_wtp_index"
replace label = "{it:Mask}" if outcome == "ln_wtp_n95"
replace label = "{it:Purell}" if outcome == "ln_wtp_purell"
replace label = "{it:Coffee}" if outcome == "ln_wtp_coffee"
replace label = "{it:Sunscreen}" if outcome == "ln_wtp_sunscreen"
replace label = "{it:Toilet paper}" if outcome == "ln_wtp_toilet"

replace label = "{it:Speed}" if outcome == "speed_metro_std"
replace label = "{it:Accuracy}" if outcome == "frac_accurate_metro_std"
replace label = "Metro Task: Index (sd)" if outcome == "index_metro"

replace label = "{it:Speed}" if outcome == "speed_covid_std"
replace label = "{it:Accuracy}" if outcome == "frac_accurate_covid_std"
replace label = "Covid Task: Index (sd)" if outcome == "index_covid"

local N = _N
local ylab = "" 
forv i = 1/`N' {
  local name = label[`i']
  local ylab = `" `ylab' `i' "`name'"  "'
}

* get hlines
local hlines = "" 
foreach outcome in well ln_wtp_index index_metro index_covid {  
  sum n if regexm(outcome,"`outcome'") == 1
  local line = `r(mean)' + .5 
  local hlines = "`hlines' `line'" 
}

twoway /// 
( bar est n if regexm(outcome,"covid") == 1 , horizontal color(lavender) ) ///
( bar est n if regexm(outcome,"metro") == 1 , horizontal color(gs12) ) ///
( bar est n if regexm(outcome,"ln_wtp") == 1 , horizontal color($ltblue) ) ///
( bar est n if regexm(outcome,"dfi") == 1 , horizontal color($orange) ) ///
    ( rcap high low n , color(black) horizontal ylabel(`ylab', labsize(large) ) xtitle("Treatment effect of Strong Action", size(large)) ///
xsize(10)  xlab(,labsize(large)) ylab(,labsize(large))  yline(`hlines', lcolor(gray) lpattern(dash)) legend(off) ) ///
( function y = 0, horizontal lcolor(black) lpattern(line)  lwidth(medlarge)  range(0.5 `N'.5)   ) /// 


grout out/rp_l2 , pdf

/**********************/
/* * make subfigures  */
/**********************/

/* demand for info  */
preserve

keep if regexm(outcome,"dfi") == 1 
cap drop n
gen n = _n

local N = _N
local ylab = "" 
forv i = 1/`N' {
  local name = label[`i']
  local ylab = `" `ylab' `i' "`name'"  "'
}

di `" `ylab' "'

twoway /// 
( bar est n if regexm(outcome,"dfi") == 1 , horizontal color($orange) ) ///
    ( rcap high low n , color(black) horizontal ylabel(`ylab', labsize(large) ) xtitle("Treatment effect of Strong Action", size(large)) ///
    xlab(,labsize(large)) ylab(,labsize(large))  legend(off) ) ///
    ( function y = 0, horizontal lcolor(black) lpattern(line) lwidth(medlarge)  range(0.5 `N'.5) ///
xsize(10)      text(3 0.04 "Joint Wald test") text(2.8 0.05 "{it:p}-value: $pdfi")  )

grout out/dfi_l2 , pdf
 
restore


/* wtp */
preserve
keep if regexm(outcome,"ln_wtp") == 1
cap drop n
gen n = _n

    replace label = "{it:Sunscreen*}" if regexm(label,"Sunscreen") == 1

    local N = _N
    local ylab = "" 
    forv i = 1/`N' {
      local name = label[`i']
      local ylab = `" `ylab' `i' "`name'"  "'
    }

    di `" `ylab' "'

    twoway /// 
    ( bar est n , horizontal color($ltblue) ) ///
        ( rcap high low n , color(black) horizontal ylabel(`ylab', labsize(large) ) xtitle("Treatment effect of Strong Action", size(large)) ///
        xlab(,labsize(large)) ylab(,labsize(large))  legend(off) ) ///
        ( function y = 0, horizontal lcolor(black) lpattern(line) lwidth(medlarge)  range(0.5 `N'.5) ///
xsize(10)         note("*pre-registered placebo") ) 

    grout out/ln_wtp_l2 , pdf
 
restore


/* wtp */
preserve
    keep if regexm(outcome,"covid") == 1  | regexm(outcome,"metro") == 1 
    cap drop n
    gen n = _n

    local N = _N
    local ylab = "" 
    forv i = 1/`N' {
      local name = label[`i']
      local ylab = `" `ylab' `i' "`name'"  "'
    }

    di `" `ylab' "'

  twoway ///
      ( bar est n if regexm(outcome,"covid") == 1 , horizontal color(lavender) ) ///
      ( bar est n if regexm(outcome,"metro") == 1 , horizontal color(gs12) ) ///
      ( rcap high low n , color(black) horizontal ylabel(`ylab', labsize(large) ) xtitle("Treatment effect of Strong Action", size(large)) ///
        xlab(,labsize(large)) ylab(,labsize(large))  legend(off) ) ///
        ( function y = 0, horizontal lcolor(black) xsize(10) lpattern(line) lwidth(medlarge)  range(0.5 `N'.5) )

    grout out/data_l2 , pdf
 
restore

/******************/
/* make figures   */
/******************/
/********* main figure ***********/ 
import delimited using out/revealed_strong_fig.csv, clear
keep if launch == 1

gen n = _n
replace est = est / 100 if regexm(outcome , "dict_fract") == 1
replace se = se / 100 if regexm(outcome , "dict_fract") == 1

* high and low 
gen high = est + 1.96 * se
gen low = est - 1.96 * se

gen label = ""
replace label = "Memory task (sd)" if outcome == "memory_std"
replace label = "WTP: index (log points)" if outcome == "ln_wtp_index"
replace label = "Demands more info (pp)" if outcome == "demand_info"
replace label = "Dictator game (fraction shared)" if outcome == "dict_fract"

    local N = _N
    local ylab = "" 
    forv i = 1/`N' {
      local name = label[`i']
      local ylab = `" `ylab' `i' "`name'"  "'
    }



twoway ///
    ( bar est n if n == 4 , horizontal color($green) ) ///
    ( bar est n if n == 3 , horizontal color($orange) ) ///
    ( bar est n if n == 2 , horizontal color($ltblue) ) ///
    ( bar est n if n == 1 , horizontal color(gs12) ) ///            
  ( rcap high low n , color(black) horizontal ylabel(`ylab', labsize(large) ) xtitle("Treatment effect of Strong Action", size(large)) ///
    xlab(,labsize(large)) ylab(,labsize(large))  legend(off) ) ///
    ( function y = 0, horizontal lcolor(black) xsize(10) lpattern(line) lwidth(medlarge)  range(0.5 `N'.5) )

grout out/rp_l1 , pdf
