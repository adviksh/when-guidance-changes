/******************************************************************************/
/* This dofile creates versions of the main check  in the data appendix where */
/* we winsorize deaths updating at several different places                   */
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
keep if !mi(shifty)
destring stratum, replace
la var shifty "Inconsistent" 
/****************************************************************************/
/* /\* generate various winsorizations: at p99 and p95 of control group *\/ */
/****************************************************************************/
sum abs_mag_deaths if shifty == 0 , d

eststo clear 
foreach wins in 99 95 90 {

  sum abs_mag_deaths, d 
  /* obtain the absolute outcome */ 
  gen abs_mag_deaths_win_`wins' = min(abs_mag_deaths,`r(p`wins')')

  /* generate ln winsorize versions */
  gen update_mag_deaths_win_`wins' = ln(abs_mag_deaths_win_`wins'+1) 

  eststo: reghdfe update_mag_deaths_win_`wins' shifty , absorb(stratum) vce(r)
  mat matrix = r(table)
  local pval: di %5.3f matrix[4,1]
  estadd local pval `pval'
  estadd local winsor `wins'   
}

esttab using out/winsorize.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior} | + 1) \$}" ///
    "\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior} | + 1) \$}" ///
"\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior} | + 1) \$}" ///    
    ) scalars("pval \$p\$-value" "winsor Percentile for winsorization"  )

/*********************/
/* attention checks  */
/*********************/
use "$int/public_data" , clear
eststo clear 
keep if !mi(shifty)
destring stratum, replace
la var shifty "Inconsistent"

/*** keep only people who pass BOTH attention checks ***/
gen fails_7 = real(q112_16) != 7
gen fails_puce = regexm(lower(q196),"puce") != 1
assert (fails_7 == 0 | fails_puce == 0)
keep if !fails_7 & !fails_puce

/********************************************************************/
/* /\* This dofile compiles numbers in the text to add manually *\/ */
/********************************************************************/
cap pr drop writelatex
pr define writelatex
syntax, number(string) name(string) [digits(real 0)] 

    file write fh "\newcommand{\" 
    file write fh "`name'" 
    file write fh "}{"
    file write fh %5.`digits'fc (`number') "}" _n

end 
cap file close fh 
file open fh using out/attention_estimates.tex , write replace

/**** update magnitude ***/
eststo: reghdfe update_mag_deaths shifty , absorb(stratum) vce(r)
matrix mat = r(table)
local p: di %5.3f mat[4,1]

writelatex, number(_b[shifty]) digits(3) name(attentionupdatemagdeathsbeta)
writelatex, number(_se[shifty]) digits(3) name(attentionupdatemagdeathsse)
writelatex, number(`p') digits(3) name(attentionupdatemagdeathsp)

estadd local pval = `p'

/*** update propensity ***/
eststo: reghdfe update_covid shifty , absorb(stratum) vce(r)
matrix mat = r(table)
local p: di %5.3f mat[4,1]
local est = _b[shifty] * 100
local se = _se[shifty] * 100

writelatex, number(`est') digits(1) name(attentionupdatecovidbeta)
writelatex, number(`se') digits(1) name(attentionupdatecovidse)
writelatex, number(`p') digits(3) name(attentionupdatecovidp)

estadd local pval = `p'


/*** gov index ***/
eststo: reghdfe gov_index_post_std shifty gov_index_pre_std , absorb(stratum) vce(r)
matrix mat = r(table)
local p: di %5.3f mat[4,1]
estadd local pval = `p'

cap file close fh 

esttab using out/drop_attention_cx.tex, replace se label drop(_cons gov_index_pre_std) star(* 0.1 ** 0.05 *** 0.01) ///
    mlabel("\shortstack{Update magnitude: \\ \$ \ln(|\text{posterior}-\text{prior} | + 1) \$}" ///
    "\shortstack{Update propensity: \\ 1(chooses to update)}" ///
"\shortstack{Government \\ credibility index}" ///    
    ) scalars("pval \$p\$-value" )

