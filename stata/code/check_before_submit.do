/****************************************************************/
/* /\**** contains statements that do not autopopulate *****\/  */
/****************************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/Documents/Github/COVID/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear
version 13

/****** OTHER NUMBERS THAT DO NOT AUTOPOPULATE *****/
/* robustness: changse in beliefs */
use "$int/public_data" , clear
count if abs_mag > 1000000
di `r(N)'/_N

sum update_covid
di 1-`r(mean)'

gen data_task_covid = index_covid != .

/* standardize relative to control group */
sum number_others if shifty == 0
gen social_distancing_index_std = - (number_others - `r(mean)') / (`r(sd)')

/****** social distancing p value *****/
eststo: reghdfe social_distancing_index_std shifty data_task_covid , absorb(stratum) vce(rob)

/**** median of the dont know variable is 4 ****/
sum dont_know,d
