/*****************************************************/
/* /\* This dofile builds the google trends data *\/ */
/*****************************************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

* we will want to merge the randomization inference stratum
import delimited using "${int}/public_data_ri.csv", clear

tempfile ri
save `ri' 

import delimited using "$raw/qualtrics_pol/Covid Anxiety Experiment II - Fielding 02_June 14, 2020_12.18.csv", clear varnames(1)

/* a few participants appear more than once, drop them */
bys rid: gen max = _N
unique rid if max > 1 & !mi(rid) 
assert `r(unique)' <= 20
drop if max > 1 

/* drop if missing rid */ 
drop if mi(rid)
drop if regexm(rid,"ImportId") == 1

/* drop zip code */
drop zip 

/* drop people who do not OK, via IRB */ 
drop if regexm(irb, "I do not wish") == 1
drop if status == "Survey Preview"
drop if finished == "False" 

/* drop people who enter the survey late */
drop if !strpos(startdate,"20")
keep if strpos(startdate,"2020-04-03") | strpos(startdate,"2020-04-04") | strpos(startdate, "2020-04-05")

/* drop people who fail attention checks */
drop if regexm(lower(q196),"puce") != 1 & real(q112_16) != 7 

/* merge to get RI stratum */ 
merge 1:1 rid using `ri', assert(using match) 
keep if _merge == 3
drop _merge

/* generate treatments */
gen experiment_steady_shifty = real(experiment_0_steadyshifty_1_weak) == 0
gen experiment_action = real(experiment_0_steadyshifty_1_weak) == 1

gen strong = real(tmt_group) == 1 & experiment_action == 1
replace strong = . if experiment_action == 0 
gen shifty = real(tmt_group) == 1 & experiment_steady_shifty == 1
replace shifty = . if experiment_action == 1

/***** only keep the changing/steady treatment ****/
keep if !mi(shifty) 

/**********************************/
/* baseline government questions  */
/**********************************/
/* knowledge */
gen double dont_know_pre = real(q112_4) 

/* trump support */
gen double vote_trump_pre = real( q112_9 )
gen double vote_trump_post = real(q151_9)
recode vote_trump_pre 0 1 2 = 0 ///
  3 4 5 6 7 = 1 ///
  8 9 10 = 2 , gen(party) 

recode vote_trump_post 0 1 2 = 0 ///
  3 4 5 6 7 = 1 ///
  8 9 10 = 2 , gen(party_post) 

/* gov handling appropriately */
gen double gov_app_pre = real(q112_5) 
gen double gov_not_strong_pre = real(q112_6) 
gen double gov_overreact_pre = real(q112_7) 

/* gov handling appropriately */
gen double gov_app_post = real(q151_5) 
gen double gov_not_strong_post = real(q151_6) 
gen double gov_overreact_post = real(q151_7) 

/* treatments */
gen double frac_accurate_covid =     ( (real(q117_4) == 3) + (real(q117_5) == 5) + ///
    (real(q117_6) == 4) + (real(q117_7) == 2) + ///
    (real(q117_8) == 8) + (real(q117_9) == 6) + ///
    (real(q117_10) == 1) + (real(q117_11) == 7) ) / 8 
replace frac_accurate_covid = . if mi(q181_pagesubmit)
replace frac_accurate_covid = 0 if !mi(q181_pagesubmit) & mi(frac_accurate_covid) 

gen double frac_accurate_metro = ///
    ( (real(q108_1) == 2) + (real(q108_2) == 3) + ///
    (real(q108_3) == 4) + (real(q108_4) == 6) + ///
    (real(q108_5) == 5) + (real(q108_6) == 1) + ///
    (real(q108_7) == 8) + (real(q108_8) == 7) )  / 8 
replace frac_accurate_metro = . if mi(q179_pagesubmit)
replace frac_accurate_metro = 0 if !mi(q179_pagesubmit) & mi(frac_accurate_metro) 

/* time between clicks */ 
gen double time_covid = real(q181_lastclick) - real(q181_firstclick)
gen double time_metro = real(q179_lastclick) - real(q179_firstclick) 

/* total time */
gen double time_alt_covid = real(q181_pagesubmit)
gen double time_alt_metro = real(q179_pagesubmit) 

/****** make standardized measure for productivity outcome ******/
foreach outcome in covid metro {
    foreach measurement in time time_alt frac_accurate {
      cap gen double `measurement'_`outcome'_std = . 
      sum `measurement'_`outcome' if shifty == 0 & !strpos(startdate, "2020-04-05")
      replace `measurement'_`outcome'_std = (`measurement'_`outcome' - `r(mean)') / `r(sd)' 
    }
  
  gen double index_`outcome' = (- time_`outcome'_std + frac_accurate_`outcome'_std) / 2    
}

/* * generate a variable that is *speed* vs. *time*  */
foreach var in covid_std alt_covid_std metro_std alt_metro_std {
  gen double speed_`var' = -time_`var'
}

/************************************/
/* /\* generate demand for info *\/ */
/************************************/
gen dfi_cute = regexm(q90,"cute") == 1 
gen dfi_hi = regexm(q90,"Senate") == 1
gen dfi_death = regexm(q90,"death") == 1
gen dfi_well = regexm(q90,"wellness") == 1

/* updates */
gen update_covid = regexm(q121,"best guess")==0 
gen update_covid_up = (regexm(q121,"low")==1) 
gen update_covid_down = (regexm(q121,"high")==1) 

/* risk aversion */
gen risky = regexm(q104,"chance") 

/* gov has private info */
gen double gov_has_private_info_pre = real(q112_8)
recode gov_has_private_info_pre 0 1 2 3 4 5 6 7 8 = 0 ///
  9 10 = 1 , gen(info_pre) 
gen double gov_has_private_info_post = real(q151_8) 
recode gov_has_private_info_post 0 1 2 3 4 5 6 7 8 = 0 ///
  9 10 = 1 , gen(info_post) 

/*****************/
/* wtp  */
/*****************/
destring placebo_wtp_*, replace
destring q336_*, replace 
ren (placebo_wtp_1 placebo_wtp_2 placebo_wtp_3) (wtp_toilet wtp_coffee wtp_sunscreen)
ren (q336_1 q336_2) (wtp_n95 wtp_purell) 

egen wtp_index = rowtotal(wtp*) 

foreach var of varlist wtp* {
  gen double ln_`var' = ln(`var'+1) 
}

/***********/
/* deaths  */
/***********/
/* kill commas */ 
foreach var of varlist *{
  cap replace `var' = subinstr(`var',",","",.)
}

gen double deaths_pre = real(q96)

gen double width_post_high = min(real(q238), 330000000) 
gen double width_post_low = min(real(q237), 330000000)

gen double width_pre_high = min(real(q235), 330000000) 
gen double width_pre_low = min(real(q99),330000000) 

gen double width_pre = width_pre_high - width_pre_low
gen double width_post = width_post_high - width_post_low


/***** winsorize pre and post width at 330 million, population of US, except for one person who put an incoherent response of "30%" *****/
count if width_pre > 330000000 | width_post > 330000000
assert `r(N)' < 10 

replace width_pre = min(width_pre,330000000) if q99 != "30%"
replace width_pre = . if q99 == "30%"
replace width_post = min(width_post,330000000)

foreach elicit in pre post {
  foreach val in "" _high _low {
    gen double ln_width_`elicit'`val' = ln(width_`elicit'`val'+1)
  }
}

gen double deaths_post = deaths_pre if regexm(q121,"best guess") == 1 
replace deaths_post = real(q123) if regexm(q121,"best guess") == 0
replace deaths_post = . if deaths_post > 10e8
replace deaths_pre = . if deaths_pre > 10e8

replace update_covid = 0 if deaths_post == deaths_pre & !mi(deaths_post) & !mi(deaths_pre)
gen inconsistent_deaths = deaths_post - deaths_pre > 0 if !mi(deaths_post - deaths_pre) ///
    & regexm(q121,"low") != 1
replace inconsistent_deaths = deaths_post - deaths_pre < 0 if !mi(deaths_post - deaths_pre) ///
    & regexm(q121,"high") != 1

/* update magnitudes */
gen double abs_mag_deaths = abs(deaths_post - deaths_pre)
gen double update_mag_deaths = ln(abs_mag_deaths + 1 )

/* log deaths */
gen double ln_deaths_post = ln(deaths_post + 1)
gen double ln_deaths_pre = ln(deaths_pre + 1)

/* Beliefs about DJI */
gen double dji_pre = real(dow_jones)
gen double dji_post = real(q131)

gen double ln_dji_pre = ln(dji_pre+1)
gen double ln_dji_post = ln(dji_post+1)

/* death rates */ 
destring q78*, replace 
recode q78_1 0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_young_pre)

recode q78_2 ///
    0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_old_pre)

gen double death_rate_pre = .5 * (death_rate_young_pre + death_rate_old_pre) 

destring q199*, replace 
recode q199_1 0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_young_post)

recode q199_2 ///
    0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_old_post)

gen double death_rate_post = 0.5 * (death_rate_young_post + death_rate_old_post) 

/* heterogeneity by deaths */ 
sum deaths_pre if !strpos(startdate, "2020-04-05") ,d 
gen q_0_25_deaths_pre = inrange(deaths_pre,`r(min)',`r(p25)')
gen q_10_25_deaths_pre = inrange(deaths_pre,`r(p10)'+.00001,`r(p25)')
gen q_25_50_deaths_pre = inrange(deaths_pre,`r(p25)'+.00001,`r(p50)')
gen q_50_75_deaths_pre = inrange(deaths_pre,`r(p50)'+.00001,`r(p75)')
gen q_75_90_deaths_pre = inrange(deaths_pre,`r(p75)'+.00001,`r(p90)')
gen q_0_10_deaths_pre = inrange(deaths_pre,`r(min)'+.00001,`r(p10)')
gen q_90_100_deaths_pre = inrange(deaths_pre,`r(p90)'+.00001,`r(max)')
gen q_75_100_deaths_pre = inrange(deaths_pre,`r(p75)'+.00001,`r(max)')

/* other people seen  */
gen double number_others = real(q224) 

/******************/
/* prior beliefs  */
/******************/
gen priors_below_info = inrange(deaths_pre, 0, 99999)
gen priors_above_info = inrange(deaths_pre, 240001,.) 
gen priors_at_info = inrange(deaths_pre, 100000,240000)

assert priors_below_info + priors_above_info + priors_at_info == 1 

/***********************/
/* government support  */
/***********************/
cap drop gov*std 
* construct government support index
foreach time in pre post {
  foreach var in gov_not_strong gov_app gov_overreact gov_has_private_info vote_trump {
    cap gen double `var'_`time'_std = . 
   
    sum `var'_`time' if shifty == 0 & !strpos(startdate, "2020-04-05")    
    replace `var'_`time'_std = (`var'_`time' - `r(mean)')/ `r(sd)' if shifty != .
    
  }
}

* create an index for government support
gen double gov_index_post_std = (- gov_not_strong_post_std + gov_app_post_std - gov_overreact_post_std + gov_has_private_info_post_std) / 4
gen double gov_index_pre_std = (- gov_not_strong_pre_std + gov_app_pre_std - gov_overreact_pre_std + gov_has_private_info_pre_std) / 4

version 13

/* version with 3 days */
save "$int/public_data_3days", replace 

/* drop extra day */ 
drop if strpos(startdate, "2020-04-05")

/* version with 2 days */ 
save "$int/public_data", replace 



