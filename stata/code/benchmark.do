/****************************************************************/
/* This graph generates a natural benchmark for the two primary */
/* outcomes that we employ                                      */
/****************************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

use "$int/public_data" , clear
gen above_median_knowledge = dont_know_pre <= 4 
reg update_mag_deaths above_median_knowledge 

gen above_median_trump = vote_trump_pre >= 5 
reg update_mag_deaths above_median_trump
