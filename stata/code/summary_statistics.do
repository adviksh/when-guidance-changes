/*********************************************************************/
/* This dofile generates summary statistics for the appendix table   */
/* showing the distribution of pre, post, changes, fraction updating */
/* for deaths                                                        */
/*                                                                   */
/* and then also for the government outcomes (before standardizing)  */
/*********************************************************************/
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
use "$int/public_data" , clear 

/* average change  */
gen change_deaths = deaths_post - deaths_pre

/**** loop over outcomes ****/
foreach treatment in 0 1 { 
foreach var in deaths_pre deaths_post change_deaths abs_mag_deaths width_pre width_post update_covid {
  
    /**** loop over treatments ****/ 
      if "`var'" != "update_covid" { 
    foreach percentile in p5 p25 p50 p75 p95 mean {
          qui sum `var' if shifty == `treatment' , d
        insert_into_file using out/summary_stats_`treatment'.csv, key(`var'_`percentile') val(`r(`percentile')') format(%15.0fc)
      }
      qui sum `var' if shifty == `treatment' , d
      insert_into_file using out/summary_stats_`treatment'.csv, key(`var'_n) val(`r(N)') format(%15.0fc)      
    }
    
  if "`var'" == "update_covid" {
      qui sum `var' if shifty == `treatment', d    
      insert_into_file using out/summary_stats_`treatment'.csv, key(`var'_mean) val(`r(mean)') format(%9.3f)
      
      qui sum `var' if shifty == `treatment' , d
      insert_into_file using out/summary_stats_`treatment'.csv, key(`var'_n) val(`r(N)') format(%15.0fc)      

      
    }
    
}

  table_from_tpl, t(code/summary_template.tex) r(out/summary_stats_`treatment'.csv) o(out/summary_stats_`treatment'.tex)
} 

/****************************/
/* write into a latex file  */
/****************************/
cap file close fh
cap file open fh using out/summary_stats_input.tex , write replace 
foreach treatment in 0 1 {
  local n0 = "s" 
  local n1 = "c" 
  
  import delimited using out/summary_stats_`treatment'.csv, clear stringcols(2) 
  drop if regexm(v1,"r2") == 1 
  local N = _N
    forv i = 1/`N' {
    local name = v1[`i']
    local name = subinstr("`name'","_","",.)    
    if regexm("`name'","p50") == 1  local name = subinstr("`name'","p50","med",.)
    if regexm("`name'","p95") == 1  local name = subinstr("`name'","p50","top",.)
    
    local value: di %15.3f round(abs(real(v2[`i'])),.001)

  /* convert percent or pp */ 
    if regexm("`name'","update_covid") == 1  { 
      local value: di %5.1f round(abs(100*real(v2[`i'])),.1)
    }

    if regexm("`name'","mean") == 1 | regexm("`name'","med") == 1 | regexm("`name'","top") == 1 { 
      file write fh "\newcommand{\" "t`n`treatment''`name'" "}{`value'}" _n
    }
  }
}
cap file close fh





  
