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
/* program that generates the update magnitudes */
/****************************/
cap pr drop make_update_table
cap pr define make_update_table
syntax, [sample(string)]

    /* load the sample */ 
    use "$int/public_data" , clear 
    keep if !mi(shifty)
    destring stratum, replace

    /**** allow a subsample of non-trump supporters if the option is invoked *****/
    if !mi("`sample'") assert inlist("`sample'","nontrump")
    if "`sample'" == "nontrump" keep if party != 2 

    assert !mi(update_mag_deaths)
    assert !mi(update_covid)

    cap !rm -f out/shifty_magnitude`sample'.csv

    /* put observations into file */ 
    count
    insert_into_file using out/shifty_magnitude`sample'.csv, key(allobs) val(`r(N)') format(%9.0fc) 
    count if priors_below_info == 1 
    insert_into_file using out/shifty_magnitude`sample'.csv, key(belowobs) val(`r(N)') format(%9.0fc)  // "priors BELOW info"
    count if priors_at_info == 1 
    insert_into_file using out/shifty_magnitude`sample'.csv, key(atobs) val(`r(N)') format(%9.0fc)  // "priors AT info"
    count if priors_above_info == 1 
    insert_into_file using out/shifty_magnitude`sample'.csv, key(aboveobs) val(`r(N)') format(%9.0fc)  // "priors ABOVE info"

    /* outcomes: update magnitude and update propensity  */
    foreach outcome in update_covid update_mag_deaths { 
      qui  reghdfe `outcome' shifty , absorb(stratum) vce(r)
          matrix mat = r(table)  
          store_est_tpl using out/shifty_magnitude`sample'.csv, name(`outcome') coef(shifty) all format(%9.3fc)
          local p = mat[4,1]
          insert_into_file using out/shifty_magnitude`sample'.csv, key(`outcome'_p) val(`p') format(%9.3fc)

        qui  reghdfe `outcome' shifty if priors_above_info == 1 , absorb(stratum) vce(rob)
          store_est_tpl using out/shifty_magnitude`sample'.csv, name(`outcome'_above) coef(shifty) all format(%9.3fc)

        qui  reghdfe `outcome' shifty if priors_at_info == 1 , absorb(stratum) vce(rob)
          store_est_tpl using out/shifty_magnitude`sample'.csv, name(`outcome'_at) coef(shifty) all format(%9.3fc)

        qui  reghdfe `outcome' shifty if priors_below_info == 1 , absorb(stratum) vce(rob)
          store_est_tpl using out/shifty_magnitude`sample'.csv, name(`outcome'_below) coef(shifty) all format(%9.3fc) 

    }

    /****************************/
    /* get mean for each group  */
    /****************************/
    foreach t in 0 1 {
      foreach outcome in update_covid update_mag_deaths {
        local n0 = "c" // "consistent"
        local n1 = "i" // "inconsistent"

        local update_mag_deathsf = "%9.2fc"
        local update_covidf = "%9.3fc"

        qui sum `outcome' if shifty == `t'    
        insert_into_file using out/shifty_magnitude`sample'.csv, key(`outcome'_shifty_`n`t''_mean) value(`r(mean)') format(``outcome'f')

        qui sum `outcome' if shifty == `t'        
        insert_into_file using out/shifty_magnitude`sample'.csv, key(`outcome'_shifty_`n`t''_n) value(`r(N)') format(%4.0fc)

        /* get means within each sample */
        foreach s in above at below {

            qui sum `outcome' if shifty == `t' & priors_`s'_info == 1 
            insert_into_file using out/shifty_magnitude`sample'.csv, key(`outcome'_`s'_shifty_`n`t''_mean) value(`r(mean)')  format(``outcome'f')

            qui sum `outcome' if shifty == `t'              
            insert_into_file using out/shifty_magnitude`sample'.csv, key(`outcome'_`s'_shifty_`n`t''_n) value(`r(N)') format(%4.0fc)

        }
      }
    }

    table_from_tpl, t(code/shifty_magnitude_template_nop.tex) r(out/shifty_magnitude`sample'.csv) o(out/shifty_magnitude`sample'.tex)

end

qui make_update_table
qui make_update_table, sample(nontrump)

/* load the sample */ 
use "$int/public_data" , clear 
keep if !mi(shifty)
destring stratum, replace



/************************/
/* 2. government index  */
/************************/
cap !rm -f out/gov_magnitude.csv

/* put observations into file */ 
count
insert_into_file using out/gov_magnitude.csv, key(allobs) val(`r(N)') format(%9.0fc) 
count if priors_below_info == 1 
insert_into_file using out/gov_magnitude.csv, key(belowobs) val(`r(N)') format(%9.0fc)  // "priors BELOW info"
count if priors_at_info == 1 
insert_into_file using out/gov_magnitude.csv, key(atobs) val(`r(N)') format(%9.0fc)  // "priors AT info"
count if priors_above_info == 1 
insert_into_file using out/gov_magnitude.csv, key(aboveobs) val(`r(N)') format(%9.0fc)  // "priors ABOVE info"

foreach outcome in gov_index gov_not_strong gov_app gov_overreact gov_has_private_info vote_trump  {
  assert !mi(`outcome'_post)
  assert !mi(`outcome'_pre)
  
  * regress on everyone 
  qui  reghdfe `outcome'_post_std shifty `outcome'_pre_std , absorb(stratum) vce(rob)
  matrix mat = r(table)    
  store_est_tpl using out/gov_magnitude.csv, name(`outcome') coef(shifty) all format(%9.3fc)
  local p = mat[4,1]
  insert_into_file using out/gov_magnitude.csv, key(`outcome'_p) val(`p') format(%9.3fc)

  * priors < info 
qui  reghdfe `outcome'_post_std shifty `outcome'_pre_std if priors_above_info == 1 , absorb(stratum) vce(rob)
  store_est_tpl using out/gov_magnitude.csv, name(`outcome'_above) coef(shifty) all format(%9.3fc)

  * priors = info 
  qui  reghdfe `outcome'_post_std shifty `outcome'_pre_std if priors_at_info == 1 , absorb(stratum) vce(rob)
  store_est_tpl using out/gov_magnitude.csv, name(`outcome'_at) coef(shifty) all format(%9.3fc)
  
  * priors > info
qui  reghdfe `outcome'_post_std shifty `outcome'_pre_std if priors_below_info == 1 , absorb(stratum) vce(rob)
  store_est_tpl using out/gov_magnitude.csv, name(`outcome'_below) coef(shifty) all format(%9.3fc)

}


/*************/
/* p-values  */
/*************/
reg gov_index_post_std shifty i.stratum gov_index_pre_std if priors_below_info == 1 
estimates store gov_index_post_std_below

reg gov_index_post_std shifty i.stratum gov_index_pre_std if priors_at_info == 1 
estimates store gov_index_post_std_at

reg gov_index_post_std shifty i.stratum gov_index_pre_std if priors_above_info == 1
estimates store gov_index_post_std_above

/* TEST 1: below vs at */ 

suest gov_index_post_std_below gov_index_post_std_at, rob 
test [gov_index_post_std_below_mean][shifty] = [gov_index_post_std_at_mean][shifty]
insert_into_file using out/gov_magnitude.csv, key(gov_index_p_below_at) val(`r(p)') format(%9.3fc)

/* TEST 2: above vs. at */ 
suest gov_index_post_std_above gov_index_post_std_at, rob 
test [gov_index_post_std_above_mean][shifty] = [gov_index_post_std_at_mean][shifty]
insert_into_file using out/gov_magnitude.csv, key(gov_index_p_at_above) val(`r(p)') format(%9.3fc)

/* TEST 3: below vs. above */ 
suest gov_index_post_std_above gov_index_post_std_below, rob 
test [gov_index_post_std_above_mean][shifty] = [gov_index_post_std_below_mean][shifty]
insert_into_file using out/gov_magnitude.csv, key(gov_index_p_below_above) val(`r(p)') format(%9.3fc)

table_from_tpl, t(code/gov_update_direction_template_nop.tex) r(out/gov_magnitude.csv) o(out/gov_update_direction.tex)

/*******************************/
/* put directly into the text  */
/*******************************/
import delimited using out/shifty_magnitude.csv, clear stringcols(2) 
drop if regexm(v1,"r2") == 1 
cap file close fh
file open fh using out/main_est.tex,  write replace 
local N = _N
    forv i = 1/`N' {
        local name = v1[`i']
        local name = subinstr("`name'","_","",.)
  local value: di %5.3f round(abs(real(v2[`i'])),.001)

  /* convert percent or pp */ 
  if (regexm("`name'","lnwidth") == 1 | regexm("`name'","updatemagdeaths") == 1 | regexm("`name'","lndeath") == 1 | regexm("`name'","updatecovid") == 1 ) & (regexm("`name'","beta") == 1 | regexm("`name'","se") == 1)  {
    local value: di %5.1f round(abs(100*real(v2[`i'])),.1)
  }
  
        file write fh "\newcommand{\" "`name'" "}{`value'}" _n
}

import delimited using out/gov_magnitude.csv, clear
drop if regexm(v1,"r2") == 1
drop if inlist(v1,"allobs","aboveobs","belowobs","atobs") == 1
local N = _N
    forv i = 1/`N' {
        local name = v1[`i']
        local name = subinstr("`name'","_","",.)
        local value: di %5.3f round(abs(real(v2[`i'])),.001)
        file write fh "\newcommand{\" "`name'" "}{`value'}" _n
}

file close fh



 
