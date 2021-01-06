/*********************************************/
/* Makes treatment effect of strong action on belief updates  */
/*********************************************/
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

set seed 103086791

/* load data */ 
use "$int/public_data" , clear

/* generate lucid variables  */
gen dem = inrange(real(political_party),1,2)
gen other = inrange(real(political_party),3,8)
gen rep = inrange(real(political_party),9,10)

/* gen household income */
gen hhi_15 = inlist(hhi,"2","3")
gen hhi_35 = inlist(hhi,"6","7","8") 

gen age_40 = age == "40-49"
gen age_50 = age == "50-59"

/* gen race */
gen race_white = ethnicity == "1" & hispanic == "1" 

capture file close fh
file open fh using out/attrition.csv, write replace
file write fh "outcome,var,bias,n" _n

foreach var in race_white age_40 rep hhi_15 hhi_35  {

  local race_whitebase = 0.082
  local repbase = 0.064
  local hhi_15base = 0.071
  local hhi_35base = 0.090
  local age_40base = 0.083
  local age_50base = 0.073
  
  local race_whitediff = 0.028
  local repdiff = 0.051
  local hhi_15diff = 0.099
  local hhi_35diff = 0.072
  local age_40diff = 0.09
  local age_50diff = 0.038

  local race_whitevalue = 1  
  local repvalue = 1
  local hhi_15value = 1
  local hhi_35value = 1  
  local age_40value = 1
  local age_50value = 1
  
  /* get fraction to drop */ 
  local number = 1/(1-``var'base') * ``var'diff'
  
  
  /* get count of people to drop */ 
  count if shifty == ``var'value' & `var' == 1
  local cutoff = round(`number'*`r(N)',1)

  qui reghdfe update_covid shifty , absorb(stratum)
  local truebcovid = _b[shifty]

  qui reghdfe update_mag_deaths shifty , absorb(stratum)
  local truebdeaths = _b[shifty]
  
  forv mc = 1/1000 {
    
    preserve 

        /* in each MC, drop the random group of people to make the attrition align */ 
        gen rand = runiform()
        sort shifty `var' rand
        bys shifty `var': drop if _n <= `cutoff' & shifty == ``var'value' & `var' == 1
    
        qui reghdfe update_covid shifty , absorb(stratum)
        local b = `truebcovid' - _b[shifty]    
        file write fh "update_covid,`var'," (`b') "," (`cutoff') _n 

        qui reghdfe update_mag_deaths shifty , absorb(stratum)
        local b = `truebdeaths' - _b[shifty]    
        file write fh "update_deaths,`var'," (`b') "," (`cutoff') _n 
    
    restore     
  }
  
}

cap file close fh


/**************************************/
/* number of people dropped -> table  */
/**************************************/
import delimited using out/attrition.csv, clear

foreach outcome in deaths covid { 
  foreach var in race_white age_40 rep hhi_15 hhi_35 {
    sum bias if outcome == "update_`outcome'" & var == "`var'"
    insert_into_file using out/attrition_`outcome'.csv, key(`var'_bias) value(`r(mean)') format(%05.3f)

    sum bias if outcome == "update_`outcome'" & var == "`var'"    
    insert_into_file using out/attrition_`outcome'.csv, key(`var'_se) value(`r(sd)') format(%05.3f)
    
    sum n if outcome == "update_deaths" & var == "`var'"
    insert_into_file using out/attrition_`outcome'.csv, key(`var'_n) value(`r(mean)') format(%3.0f)

  }

  table_from_tpl, t(code/attrition_template.tex) r(out/attrition_`outcome'.csv) o(out/attrition_`outcome'.tex)      
}


