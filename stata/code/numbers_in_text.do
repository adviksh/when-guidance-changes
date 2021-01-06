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
file open fh using out/additional_estimates.tex , write replace

/********************************/
/* /\* 1: effect of shifty *\/  */
/********************************/
use "$int/public_data" , clear 
keep if !mi(shifty) 

/*** total N ***/
reghdfe update_mag_deaths shifty, absorb(stratum) vce(rob)
local number = e(N)
writelatex, digits(0) number(`number') name(numberofparticipants)

local mainlog = _b[shifty]
writelatex, digits(3) number(`mainlog') name(mainlog) 

local mainlog = abs(_b[shifty]*100)
writelatex, digits(1) number(`mainlog') name(mainpctpp) 

local mainconversion = exp(_b[shifty])-1
writelatex, digits(3) number(`mainconversion') name(mainconversion) 

local mainconversionpct = abs(100*(exp(_b[shifty])-1))
writelatex, digits(1) number(`mainconversionpct') name(mainconversionpct) 

nlcom exp(_b[shifty])-1
matrix var = r(V)
local mainconversionpctse = sqrt(var[1,1])*100
writelatex, digits(1) number(`mainconversionpctse') name(mainconversionpctse) 

/*** number in each group ***/
count if shifty == 0
local number = `r(N)'
writelatex, digits(0) number(`number') name(numberofcontrol) 

count if shifty == 1
local number = `r(N)'
writelatex, digits(0) number(`number') name(numberoftreat) 

/*** main estimate in numbers that are useful ***/ 
reghdfe update_covid shifty, absorb(stratum) vce(rob)
local mainpp = abs(_b[shifty]*100)
writelatex, digits(1) number(`mainpp') name(mainfracpp) 
local mainppse = abs(_se[shifty]*100)
writelatex, digits(1) number(`mainppse') name(mainfracppse) 

/*** get estimate in terms of control group ***/ 
sum update_covid if shifty == 0
local mean = abs(_b[shifty] * 100 / `r(mean)') 
writelatex, digits(1) number(`mean') name(updaterelativecontrol) 

/***************************************/
/* /\*** effect for 3-day sample ***\/ */
/***************************************/
use "$int/public_data" , clear
reghdfe update_covid shifty, absorb(stratum) vce(rob)
local number_2 = `e(N)' 

use "$int/public_data_3days" , clear
reghdfe update_covid shifty, absorb(stratum) vce(rob)
local number_3 = `e(N)' 
local diff = `number_3' - `number_2' 
writelatex, digits(0) number(`diff') name(extradays) 

keep if !mi(shifty) 
reghdfe update_covid shifty, absorb(stratum) vce(rob)
matrix mat = r(table)
local b = abs(_b[shifty]*100)
writelatex, digits(1) number(`b') name(mainthreeday) 

local se = _se[shifty]*100
writelatex, digits(1) number(`se') name(mainthreedayse) 

local p = mat[4,1]
writelatex, digits(3) number(`p') name(mainthreedayp) 

sum update_covid if shifty == 0
local mean = abs(_b[shifty] * 100 / `r(mean)')
writelatex, digits(1) number(`mean') name(updaterelativecontrolthreeday) 

/* get effect on update magnitudes */ 
reghdfe update_mag_deaths shifty, absorb(stratum) vce(rob)
local b = abs(_b[shifty] * 100)
local se = _se[shifty] * 100
matrix mat = r(table)
local p = mat[4,1]

writelatex, digits(1) number(`b') name(magthreeday) 
writelatex, digits(1) number(`se') name(magthreedayse) 
writelatex, digits(3) number(`p') name(magthreedayp) 

/****************************************/
/* /\*** benchmark of main effect ***\/ */
/****************************************/
use "$int/public_data" , clear

/**** fraction with knowledge ***/ 
gen above_median_knowledge = dont_know_pre <= 4
sum above_median_knowledge, d
local frac = `r(mean)' * 100
writelatex, digits(1) number(`frac') name(fracknowledge)

sum dont_know_pre, d
writelatex, digits(1) number(`r(sd)') name(sdnews)


reg update_mag_deaths above_median_knowledge, robust
local b = abs(_b[above_median_knowledge]*100)
writelatex, digits(1) number(`b') name(knowledgebenchmarkbeta)

local se = _se[above_median_knowledge]*100
writelatex, digits(1) number(`se') name(knowledgebenchmarkse) 

reghdfe update_mag_deaths shifty , absorb(stratum) vce(rob)

local benchmark = abs(_b[shifty] * 100 / `b')
writelatex, digits(1) number(`benchmark') name(benchmark) 

/* benchmark in terms of propensity */ 
reg update_covid above_median_knowledge, robust

local b = abs(_b[above_median_knowledge]*100)
writelatex, digits(1) number(`b') name(knowledgebenchmarkppbeta)

local se = _se[above_median_knowledge]*100
writelatex, digits(1) number(`se') name(knowledgebenchmarkppse) 

reghdfe update_covid shifty, absorb(stratum) vce(rob)
local benchmarkpp = abs(_b[shifty] * 100 / `b')
writelatex, digits(1) number(`benchmarkpp') name(benchmarkpp)

/*********************/
/* INTENSIVE MARGIN  */
/*********************/
reghdfe update_mag_deaths shifty if update_covid == 1, absorb(stratum) vce(rob)
local b = abs(_b[shifty]*100)
local se = abs(_se[shifty]*100)
matrix mat = r(table)
local p = mat[4,1]
writelatex, digits(1) number(`b') name(intensivelogb)
writelatex, digits(1) number(`se') name(intensivelogse)
writelatex, digits(3) number(`p') name(intensivelogp)

/*********************/
/* DIFF: OPT AND PESS  */
/*********************/
destring stratum, replace 
foreach outcome in update_mag_deaths update_covid {
  local update_mag_deathsnm updatemagdeaths
  local update_covidnm updatecovid
  
    qui reg `outcome' shifty i.stratum if priors_below_info == 1
    estimates store `outcome'_below

    qui reg `outcome' shifty i.stratum if priors_above_info == 1
    estimates store `outcome'_above
  
    qui suest `outcome'_below `outcome'_above, rob
    test [`outcome'_below_mean][shifty] = [`outcome'_above_mean][shifty]
    writelatex, digits(3) number(`r(p)') name(``outcome'nm'pessoptp)

}

/****************************************/
/* Prediction 4 - heterogeneity by gov  */
/****************************************/
qui reg gov_index_post_std shifty gov_index_pre_std i.stratum if priors_below_info == 1 
estimates store gov_below

qui reg gov_index_post_std shifty gov_index_pre_std i.stratum if priors_below_info == 0
estimates store gov_above

suest gov_below gov_above, rob
nlcom [gov_below_mean][shifty] - [gov_above_mean][shifty]
mat b = r(b)
local b = b[1,1]
mat var = r(V)
local se = sqrt(var[1,1])

writelatex, digits(3) number(`b') name(abovemedgovbeta)
writelatex, digits(3) number(`se') name(abovemedgovse)

/**********************************************************************************/
/* data appendix: fraction of participants with posteriors or priors are too low  */
/**********************************************************************************/
count if deaths_post == deaths_pre & regexm(q121,"best guess") != 1
local percent = `r(N)'/_N * 100 
writelatex, digits(1) number(`percent') name(percentdatabestguess) 

/*********************************************/
/* Referee ask: propensity to vote for trump */
/*********************************************/
use "$int/public_data" , clear
reg gov_index_pre_std vote_trump_pre_std, robust
local b = _b[vote_trump_pre_std]
writelatex, digits(3) number(`b') name(votetrumpgovtrust) 

local se = _se[vote_trump_pre_std]
writelatex, digits(3) number(`se') name(votetrumpgovtrustse) 

reghdfe gov_index_post_std shifty gov_index_pre, absorb(stratum)
local pct = abs(_b[shifty] / `b'  * 100)
di `pct'
writelatex, digits(0) number(`pct') name(votetrumpgovtrustpct) 


cap file close fh 

