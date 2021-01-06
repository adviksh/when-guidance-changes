/*********************************************************************************************/
/* This generates a table of the steady/shifty experiment with various controls/no controls  */
/*********************************************************************************************/
/**********************************************************/
/* Generate effect on alternate outcome of belief update  */
/**********************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

/*****************/
/* EXPERIMENT 2  */
/*****************/
* Panel A: Performance on Effort Task
  use "$int/public_data" , clear
  la var shifty "Inconsistent"
  keep if shifty != .

  eststo clear

  * get randomization inference p values
  foreach task in covid metro {
    local covidnm COVID
    local metronm Metro
     
  eststo: reghdfe index_`task' shifty  , absorb(stratum) vce(rob) 
  matrix mat = r(table)
  local p: di %5.3f mat[4,1]
    estadd local task = "``task'nm'"
    
  eststo: reghdfe frac_accurate_`task'_std shifty  , absorb(stratum) vce(rob) 
  matrix mat = r(table)
  local p: di %5.3f mat[4,1]  
    estadd local task = "``task'nm'"

    eststo: reghdfe speed_`task'_std shifty  , absorb(stratum) vce(rob) 
    matrix mat = r(table)
    local p: di %5.3f mat[4,1]
    estadd local task = "``task'nm'"

    eststo: reghdfe speed_alt_`task'_std shifty  , absorb(stratum) vce(rob) 
    matrix mat = r(table)
    local p: di %5.3f mat[4,1]
    estadd local task = "``task'nm'"
    
  }
  
    esttab using out/exp_2_effortshifty.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
      mlabel("\shortstack{Index \\ (sd)}" ///
      "\shortstack{Accuracy \\ (sd)}" ///
      "\shortstack{Speed \\ clicks (sd)}" ///
      "\shortstack{Speed \\ time on page (sd)}" ///
      "\shortstack{Index \\ (sd)}" ///
      "\shortstack{Accuracy \\ (sd)}" ///
      "\shortstack{Speed \\ clicks (sd)}" ///
      "\shortstack{Speed \\ time on page (sd)}" ) /// 
      scalars("task Task") sfmt(%5.3f) 

      * Panel B: Demand for Info
    destring stratum, replace
    foreach outcome in cute well hi {
      reg dfi_`outcome' shifty i.stratum
      estimates store `outcome'
    }
    suest cute well hi , rob 
    test ([cute_mean][shifty] = 0 ) ([well_mean][shifty] = 0 ) ([hi_mean][shifty] = 0 )

      local fp: di %5.3f `r(p)'

    eststo clear 
    eststo: reghdfe dfi_cute shifty , absorb(stratum)   vce(rob)
    matrix mat = r(table)
    local p: di %5.3f mat[4,1]
    estadd local fp = `fp'

    eststo: reghdfe dfi_well shifty , absorb(stratum)   vce(rob)
    matrix mat = r(table)
    local p: di %5.3f mat[4,1]
    estadd local fp = "" 

    eststo: reghdfe dfi_hi shifty , absorb(stratum)   vce(rob)
    matrix mat = r(table)
    local p: di %5.3f mat[4,1]
    estadd local fp = "" 

    eststo: reghdfe dfi_death shifty , absorb(stratum)   vce(rob)
    matrix mat = r(table)
    local p = round(mat[4,1],.001)
    estadd local fp = "" 

    eststo: reghdfe risky shifty , absorb(stratum)   vce(rob)
    matrix mat = r(table)
    local p: di %5.3f mat[4,1]
    estadd local fp = "" 

        esttab using out/exp_2_dfishifty.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
        mlabel("Article: cute animals" "Article: wellness" "Article: health insurance" "Article: death counts" "Risky gamble" ) ///
        scalars("fp \textit{p}-value: joint test (articles)")  
    
        * Panel C: WTP
        cap gen wtp_index_ns = wtp_index - 4*wtp_sunscreen // subtract off demand for sunscreen from demand for each additional outcome 
        cap gen ln_wtp_ns = ln(wtp_index_ns + 1)

eststo clear
/* wtp */ 
    foreach outcome in index n95 purell toilet coffee sunscreen {       
      eststo: reghdfe ln_wtp_`outcome' shifty , absorb(stratum)   vce(rob)
      matrix mat = r(table)
      local p: di %5.3f mat[4,1]  
    }
   
        esttab using out/exp_2_wtpshifty.tex, replace se label drop(_cons) star(* 0.1 ** 0.05 *** 0.01) ///
        mlabel("Index (excl. sunscreen)"  "N95" "Purell" "Toilet Paper" "Coffee" "Sunscreen")         ///
sfmt(%5.3f) 


