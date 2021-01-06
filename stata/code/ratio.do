/**************************************************************/
/* /\* This dofile generates the belief heterogeneity ratio and tests for the ratio (Table 3) *\/ */
/**************************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13
use "$int/public_data" , clear

cap rm -f out/ratio.csv


/* put observations into file */ 
count
insert_into_file using out/ratio.csv, key(allobs) val(`r(N)') format(%9.0fc) 
count if priors_below_info == 1 
insert_into_file using out/ratio.csv, key(belowobs) val(`r(N)') format(%9.0fc)  // "priors BELOW info"
count if priors_at_info == 1 
insert_into_file using out/ratio.csv, key(atobs) val(`r(N)') format(%9.0fc)  // "priors AT info"
count if priors_above_info == 1 
insert_into_file using out/ratio.csv, key(aboveobs) val(`r(N)') format(%9.0fc)  // "priors ABOVE info"


/* loop over each outcome */
foreach outcome in  update_covid update_mag_deaths { 

  reg `outcome' shifty, rob 
  nlcom (_b[shifty] + _b[_cons]) / _b[_cons]
  matrix beta = r(b)
  local b: di %5.3f beta[1,1]
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = `b'/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  count_stars, p(`pvalue') 
  insert_into_file using out/ratio.csv, key(`outcome'_starbeta) val(`b'`r(stars)') format(%5.3fc) 
  insert_into_file using out/ratio.csv, key(`outcome'_se) val(`std_err') format(%5.3fc) 
  
  /* individually regress above , at, below */
  reg `outcome' shifty if priors_above_info == 1 , 
  estimates store above

  reg `outcome' shifty if priors_at_info == 1 , 
  estimates store at

  reg `outcome' shifty if priors_below_info == 1 , 
  estimates store below

  /* all people who are not optimists */ 
  reg `outcome' shifty if priors_below_info == 0 , 
  estimates store not_below

  suest above at below , rob 

  nlcom ([above_mean]shifty + [above_mean]_cons) / [above_mean]_cons
  matrix beta = r(b)
  local b: di %5.3f beta[1,1]  
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = `b'/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  count_stars, p(`pvalue') 
  insert_into_file using out/ratio.csv, key(`outcome'_above_starbeta) val(`b'`r(stars)') format(%5.3fc) 
  insert_into_file using out/ratio.csv, key(`outcome'_above_se) val(`std_err') format(%5.3fc) 



  nlcom ([at_mean]shifty + [at_mean]_cons) / [at_mean]_cons
  matrix beta = r(b)
  local b: di %5.3f beta[1,1]  
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = `b'/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  count_stars, p(`pvalue') 
  insert_into_file using out/ratio.csv, key(`outcome'_at_starbeta) val(`b'`r(stars)') format(%5.3fc) 
  insert_into_file using out/ratio.csv, key(`outcome'_at_se) val(`std_err') format(%5.3fc) 


  nlcom ([below_mean]shifty + [below_mean]_cons) / [below_mean]_cons
  matrix beta = r(b)
  local b: di %5.3f beta[1,1]
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = `b'/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  count_stars, p(`pvalue') 
  insert_into_file using out/ratio.csv, key(`outcome'_below_starbeta) val(`b'`r(stars)') format(%5.3fc) 
  insert_into_file using out/ratio.csv, key(`outcome'_below_se) val(`std_err') format(%5.3fc) 

  /***** tests ******/
  /* above = at */ 
  nlcom ([above_mean]shifty + [above_mean]_cons) / [above_mean]_cons - ([at_mean]shifty + [at_mean]_cons) / [at_mean]_cons, post
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  insert_into_file using out/ratio.csv, key(`outcome'_p_at_above) val(`pvalue') format(%5.3fc) 

  /* at = below */
  suest above at below , rob 
  nlcom ([at_mean]shifty + [at_mean]_cons) / [at_mean]_cons - ([below_mean]shifty + [below_mean]_cons) / [below_mean]_cons
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  insert_into_file using out/ratio.csv, key(`outcome'_p_below_at) val(`pvalue') format(%5.3fc) 

  /* above = below */
  suest above at below , rob 
  nlcom ([above_mean]shifty + [above_mean]_cons) / [above_mean]_cons - ([below_mean]shifty + [below_mean]_cons) / [below_mean]_cons
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  insert_into_file using out/ratio.csv, key(`outcome'_p_below_above) val(`pvalue') format(%5.3fc) 

  /* test for above = not above */
  suest below not_below , rob
  nlcom ([below_mean]shifty + [below_mean]_cons) / [below_mean]_cons - ([not_below_mean]shifty + [not_below_mean]_cons) / [not_below_mean]_cons  
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  insert_into_file using out/ratio.csv, key(`outcome'_p_below_not) val(`pvalue') format(%5.3fc) 

}

table_from_tpl, t(code/ratio_p_template.tex) r(out/ratio.csv) o(out/ratio.tex)

cap file close fh
file open fh using out/ratio_est.tex,  write replace 

import delimited using out/ratio.csv, clear
drop if regexm(v1,"r2") == 1
drop if inlist(v1,"allobs","aboveobs","belowobs","atobs") == 1
local N = _N
    forv i = 1/`N' {
        local name = v1[`i']
        local name = subinstr("ratio`name'","_","",.)
        local value = v2[`i']
        local value = subinstr("`value'","*","",.)
        local value: di %5.3f round(real("`value'"),.001)
        file write fh "\newcommand{\" "`name'" "}{`value'}" _n
}

file close fh
