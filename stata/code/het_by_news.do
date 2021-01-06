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
 
use "$int/public_data" , clear
destring stratum , replace

/*********************************************/
/* loop over levels of dont follow the news  */
/*********************************************/
capture file close fh
file open fh using out/het_news.csv, write replace 
file write fh "outcome,level,est,se" _n 
forv dont_know = 0/10 {
  foreach outcome in update_mag_death update_covid {

    reghdfe `outcome' shifty if dont_know == `dont_know', absorb(stratum)
    local b = _b[shifty]
    local se = _se[shifty]
    file write fh "`outcome',`dont_know'," (`b') "," (`se') _n
    
  }
}
  
cap file close fh
import delimited using out/het_news.csv, clear

twoway /// 
    ( scatter est level if outcome == "update_mag_death" ) ///
    ( lfit est level if outcome == "update_mag_death" ) ///
    
grout out/news_update_mag_death , pdf

twoway /// 
    ( scatter est level if outcome == "update_covid" ) ///
    ( lfit est level if outcome == "update_covid" ) ///
    
grout out/news_update_covid , pdf

twoway /// 
    ( scatter est level if outcome == "update_covid" ) ///
    ( lfit est level if outcome == "update_covid" & level != 9 ) ///
    
grout out/news_update_covid_excl9 , pdf

