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

/* drop if missing rid */ 
drop if mi(rid)
drop if regexm(rid,"ImportId") == 1

/* drop zip code */
drop zip 

/* drop people who do not OK, via IRB */ 
drop if regexm(irb, "I do not wish") == 1
drop if status == "Survey Preview"
drop if finished == "False" 

/* one participant appears 7 times, drop this participant */
bys rid: gen max = _N
drop if max > 1 

/* drop people who enter the survey late */
drop if !strpos(startdate,"20")
keep if strpos(startdate,"2020-04-03") | strpos(startdate,"2020-04-04") | strpos(startdate, "2020-04-05")

/* drop people who fail attention checks */
drop if regexm(lower(q196),"puce") != 1 & real(q112_16) != 7 

/* merge to get RI stratum */ 
merge 1:1 rid using `ri', nogen assert(using match) keep(match) 

/* drop people who finish in less than 2 minutes [0 people]  */
gen len = length(startdate) 
gen starttime = clock(substr(startdate,len-7,8),"hms")
gen endtime = clock(substr(enddate,len-7,8),"hms")
gen length_of_survey = endtime - starttime
replace length = endtime + hms(23,59,59) if endtime < starttime
format length %tc 
drop if length < hms(0,2,0)


/* generate treatments */
gen experiment_steady_shifty = real(experiment_0_steadyshifty_1_weak) == 0
gen experiment_action = real(experiment_0_steadyshifty_1_weak) == 1

gen strong = real(tmt_group) == 1 & experiment_action == 1
replace strong = . if experiment_action == 0 
gen shifty = real(tmt_group) == 1 & experiment_steady_shifty == 1
replace shifty = . if experiment_action == 1

local i = 0 
forv num = 1/11 {
  local i = `i' + 1 
  gen anxiety_`num' = .
  replace anxiety_`num' = 0 if regexm(q105_`num',"Not at all") == 1
  replace anxiety_`num' = 1 if regexm(q105_`num',"Mildly") == 1
  replace anxiety_`num' = 2 if regexm(q105_`num',"Moderate") == 1
  replace anxiety_`num' = 3 if regexm(q105_`num',"Severe") == 1
  assert !mi(anxiety_`num')
}

local i = 0 
forv num = 12/21 {
  local i = `i' + 1 
  gen anxiety_`num' = .
  replace anxiety_`num' = 0 if regexm(q191_`i',"Not at all") == 1
  replace anxiety_`num' = 1 if regexm(q191_`i',"Mildly") == 1
  replace anxiety_`num' = 2 if regexm(q191_`i',"Moderate") == 1
  replace anxiety_`num' = 3 if regexm(q191_`i',"Severe") == 1
  assert !mi(anxiety_`num')
}

egen anxiety_index = rowtotal(anxiety_*)

/* anxiety */ 
gen anxiety_std = . 
sum anxiety_index  if strong == 0 & experiment_action == 1 & !strpos(startdate, "2020-04-05")
replace anxiety_std = (anxiety_index - `r(mean)' ) / `r(sd)'  if experiment_action == 1

sum anxiety_index  if shifty == 0 & experiment_action == 0 & !strpos(startdate, "2020-04-05")
replace anxiety_std = (anxiety_index - `r(mean)' ) / `r(sd)'  if experiment_action == 0

/* knowledge */
gen dont_know_pre = real(q112_4) 

/* trump support */
gen vote_trump_pre = real( q112_9 )
gen vote_trump_post = real(q151_9)
recode vote_trump_pre 0 1 2 = 0 ///
  3 4 5 6 7 = 1 ///
  8 9 10 = 2 , gen(party) 

recode vote_trump_post 0 1 2 = 0 ///
  3 4 5 6 7 = 1 ///
  8 9 10 = 2 , gen(party_post) 

/* gov handling appropriately */
gen gov_app_pre = real(q112_5) 
gen gov_not_strong_pre = real(q112_6) 
gen gov_overreact_pre = real(q112_7) 

/* gov handling appropriately */
gen gov_app_post = real(q151_5) 
gen gov_not_strong_post = real(q151_6) 
gen gov_overreact_post = real(q151_7) 

/* treatments */
cap drop prod_accurate_covid 
gen prod_accurate_covid = ///
    real(q117_4) == 3 & real(q117_5) == 5 & ///
    real(q117_6) == 4 & real(q117_7) == 2 & ///
    real(q117_8) == 8 & real(q117_9) == 6 & ///
    real(q117_10) == 1 & real(q117_11) == 7
replace prod_accurate_covid = . if mi(q181_pagesubmit)
replace prod_accurate_covid = 0 if !mi(q181_pagesubmit) & mi(prod_accurate_covid) 

gen frac_accurate_covid =     ( (real(q117_4) == 3) + (real(q117_5) == 5) + ///
    (real(q117_6) == 4) + (real(q117_7) == 2) + ///
    (real(q117_8) == 8) + (real(q117_9) == 6) + ///
    (real(q117_10) == 1) + (real(q117_11) == 7) ) / 8 
replace frac_accurate_covid = . if mi(q181_pagesubmit)
replace frac_accurate_covid = 0 if !mi(q181_pagesubmit) & mi(frac_accurate_covid) 

gen prod_accurate_metro = ///
    real(q108_1) == 2 & real(q108_2) == 3 & ///
    real(q108_3) == 4 & real(q108_4) == 6 & ///
    real(q108_5) == 5 & real(q108_6) == 1 & ///
    real(q108_7) == 8 & real(q108_8) == 7
replace prod_accurate_metro = . if mi(q179_pagesubmit)
replace prod_accurate_metro = 0 if !mi(q181_pagesubmit) & mi(prod_accurate_metro) 

gen frac_accurate_metro = ///
    ( (real(q108_1) == 2) + (real(q108_2) == 3) + ///
    (real(q108_3) == 4) + (real(q108_4) == 6) + ///
    (real(q108_5) == 5) + (real(q108_6) == 1) + ///
    (real(q108_7) == 8) + (real(q108_8) == 7) )  / 8 
replace frac_accurate_metro = . if mi(q179_pagesubmit)
replace frac_accurate_metro = 0 if !mi(q179_pagesubmit) & mi(frac_accurate_metro) 

gen time_alt_covid = real(q181_pagesubmit)
gen time_alt_metro = real(q179_pagesubmit) 

gen time_covid = real(q181_lastclick) - real(q181_firstclick)
gen time_metro = real(q179_lastclick) - real(q179_firstclick) 

/****** make standardized measure for productivity outcome ******/
foreach outcome in covid metro {
  foreach treatment in strong shifty {
    foreach measurement in time time_alt frac_accurate {
      cap gen `measurement'_`outcome'_std = . 
      sum `measurement'_`outcome' if `treatment' == 0 & !strpos(startdate, "2020-04-05")
      replace `measurement'_`outcome'_std = (`measurement'_`outcome' - `r(mean)') / `r(sd)' if !mi(`r(mean)')      
    }
  }
  
  gen index_`outcome' = (-time_`outcome'_std + frac_accurate_`outcome'_std) / 2    
}

/* * generate a variable that is *speed* vs. *time*  */
foreach var in covid_std alt_covid_std metro_std alt_metro_std {
  gen speed_`var' = -time_`var'
}

/************************************/
/* /\* generate demand for info *\/ */
/************************************/
gen dfi_cute = regexm(q90,"cute") == 1 
gen dfi_hi = regexm(q90,"Senate") == 1
gen dfi_death = regexm(q90,"death") == 1
gen dfi_well = regexm(q90,"wellness") == 1

/* updatees */
gen update_covid = regexm(q121,"best guess")==0 
gen update_covid_up = (regexm(q121,"low")==1) 
gen update_covid_down = (regexm(q121,"high")==1) 

/* risk aversion */
gen risky = regexm(q104,"chance") 

/* gov has private info */
gen gov_has_private_info_pre = real(q112_8)
recode gov_has_private_info_pre 0 1 2 3 4 5 6 7 8 = 0 ///
  9 10 = 1 , gen(info_pre) 
gen gov_has_private_info_post = real(q151_8) 
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
  gen ln_`var' = ln(`var'+1) 
}

/***********/
/* priors  */
/***********/
/* kill commas */ 
foreach var of varlist *{
  cap replace `var' = subinstr(`var',",","",.)
}

gen deaths_pre = real(q96)
gen width_pre = real(q235) - real(q99)
gen width_pre_high = real(q235)
gen width_pre_low = real(q99)
gen width_post = real(q238) - real(q237)
gen width_post_high = real(q238)
gen width_post_low = real(q237)
replace width_pre = . if width_pre > 100000000
replace width_post = . if width_post > 100000000

foreach elicit in pre post {
  foreach val in "" _high _low {
    replace width_`elicit'`val' = . if width_`elicit' > 100000000
    gen ln_width_`elicit'`val' = ln(width_`elicit'`val')
  }
}

gen deaths_post = deaths_pre if regexm(q121,"best guess") == 1 
replace deaths_post = real(q123) if regexm(q121,"best guess") == 0
replace deaths_post = . if deaths_post > 10e8
replace deaths_pre = . if deaths_pre > 10e8

replace update_covid = 0 if deaths_post == deaths_pre
replace update_covid_up = 1 if deaths_post > deaths_pre & ( mi(deaths_post)  | mi(deaths_pre) )
replace update_covid_up = 0 if deaths_post < deaths_pre & ( mi(deaths_post) | mi(deaths_pre) )
replace update_covid_down = 1 if deaths_post < deaths_pre & ( mi(deaths_post) | mi(deaths_pre) )
replace update_covid_down = 0 if deaths_post > deaths_pre & (mi(deaths_post) | mi(deaths_pre) ) 

gen deaths_update = deaths_post - deaths_pre 
gen inconsistent_deaths = deaths_post - deaths_pre > 0 if !mi(deaths_post - deaths_pre) ///
    & regexm(q121,"low") != 1
replace inconsistent_deaths = deaths_post - deaths_pre < 0 if !mi(deaths_post - deaths_pre) ///
    & regexm(q121,"high") != 1

gen ln_deaths_post = ln(deaths_post+1)
gen ln_deaths_pre = ln(deaths_pre+1)

/* Beliefs about DJI */
gen dji_pre = real(dow_jones)
gen dji_post = real(q131)

replace dji_pre = . if dji_pre > 100000
replace dji_post = . if dji_post > 100000
gen ln_dji_pre = ln(dji_pre+1)
gen ln_dji_post = ln(dji_post+1)

destring q78*, replace 
recode q78_1 0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_young_pre)

recode q78_2 ///
    0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_old_pre)

gen death_rate_pre = .5 * (death_rate_young_pre + death_rate_old_pre) 

destring q199*, replace 
recode q199_1 0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_young_post)

recode q199_2 ///
    0 = .01 13 = .1 25 = .2 38 = .5 50 = 1 ///
    63 = 2 75 = 5 88 = 10 100 = 100 , gen(death_rate_old_post)

gen death_rate_post = 0.5 * (death_rate_young_post + death_rate_old_post) 
/**************************************/
/* heterogeneity by baseline beliefs  */
/**************************************/
sum death_rate_pre if !strpos(startdate, "2020-04-05") ,d  

gen q_0_25_death_rate_pre = inrange(death_rate_pre,`r(min)',`r(p25)')
gen q_0_10_death_rate_pre = inrange(death_rate_pre,`r(min)',`r(p10)')
gen q_90_100_death_rate_pre = inrange(death_rate_pre,`r(p90)'+1,`r(max)')
gen q_10_25_death_rate_pre = inrange(death_rate_pre,`r(p10)'+1,`r(p25)')
gen q_25_50_death_rate_pre = inrange(death_rate_pre,`r(p25)'+1,`r(p50)')
gen q_50_75_death_rate_pre = inrange(death_rate_pre,`r(p50)'+1,`r(p75)')
gen q_75_90_death_rate_pre = inrange(death_rate_pre,`r(p75)'+1,`r(p90)')
gen q_75_100_death_rate_pre = inrange(death_rate_pre,`r(p75)'+1,`r(max)')

sum deaths_pre if !strpos(startdate, "2020-04-05") ,d 

gen q_0_25_deaths_pre = inrange(deaths_pre,`r(min)',`r(p25)')
gen q_10_25_deaths_pre = inrange(deaths_pre,`r(p10)'+1,`r(p25)')
gen q_25_50_deaths_pre = inrange(deaths_pre,`r(p25)'+1,`r(p50)')
gen q_50_75_deaths_pre = inrange(deaths_pre,`r(p50)'+1,`r(p75)')
gen q_75_90_deaths_pre = inrange(deaths_pre,`r(p75)'+1,`r(p90)')
gen q_0_10_deaths_pre = inrange(deaths_pre,`r(min)',`r(p10)')
gen q_90_100_deaths_pre = inrange(deaths_pre,`r(p90)'+1,`r(max)')
gen q_75_100_deaths_pre = inrange(deaths_pre,`r(p75)'+1,`r(max)')


/**************************/
/* anxiety decomposition  */
/**************************/
gen decomp_fin_me = real(q89_1)
gen decomp_stay_home = real(q89_2)
gen decomp_chaos = real(q89_4)
gen decomp_sick_me = real(q89_5)
gen decomp_sick_others = real(q89_6)
gen decomp_pol = real(q89_7)
gen decomp_fin_others = real(q89_8)
gen decomp_other = real(q89_9)

/*****************/
/* other people  */
/*****************/
gen number_others = real(q224) 

gen info_above_priors = inrange(deaths_pre, 0, 99999)
gen info_below_priors = inrange(deaths_pre, 240000,.) 
gen info_at_priors = inrange(deaths_pre, 100000,239999)

/*********************/
/* granular updates  */
/*********************/
gen update_up_deaths = update_covid_up
gen update_down_deaths = update_covid_down

gen update_up_death_rate_young = death_rate_young_post > death_rate_young_pre if !mi(death_rate_young_post) & !mi(death_rate_young_pre)
gen update_up_death_rate_old = death_rate_old_post > death_rate_old_pre if !mi(death_rate_old_post) & !mi(death_rate_old_pre)
gen update_up_dji = ln_dji_post < ln_dji_pre if !mi(ln_dji_post) & !mi(ln_dji_pre) // dow jones up is good 

gen update_down_death_rate_young = death_rate_young_post < death_rate_young_pre if !mi(death_rate_young_post) & !mi(death_rate_young_pre)
gen update_down_death_rate_old = death_rate_old_post < death_rate_old_pre if !mi(death_rate_old_post) & !mi(death_rate_old_pre)
gen update_down_dji = ln_dji_post > ln_dji_pre if !mi(ln_dji_post) & !mi(ln_dji_pre) // dow jones up is good 


/***********************/
/* government support  */
/***********************/
cap drop gov*std 
* construct government support index
foreach time in pre post {
  foreach var in gov_not_strong gov_app gov_overreact gov_has_private_info vote_trump {
    cap gen `var'_`time'_std = . 
    sum `var'_`time' if strong == 0 & !strpos(startdate, "2020-04-05")    
    replace `var'_`time'_std = (`var'_`time' - `r(mean)')/ `r(sd)' if strong != .
    
    sum `var'_`time' if shifty == 0 & !strpos(startdate, "2020-04-05")    
    replace `var'_`time'_std = (`var'_`time' - `r(mean)')/ `r(sd)' if shifty != .
    
  }
}

* create an index for government support
gen gov_index_post_std = (- gov_not_strong_post_std + gov_app_post_std - gov_overreact_post_std + gov_has_private_info_post_std) / 4
gen gov_index_pre_std = (- gov_not_strong_pre_std + gov_app_pre_std - gov_overreact_pre_std + gov_has_private_info_pre_std) / 4


version 13

/* update magnitudes */
gen update_mag_deaths = ln(abs(deaths_post - deaths_pre) + 1 )
gen abs_mag_deaths = abs(deaths_post - deaths_pre) + 1 

/* version with 3 days */
save "$int/public_data_3days_both", replace 

/* drop extra day */ 
drop if strpos(startdate, "2020-04-05")

/* version with 2 days */ 
save "$int/public_data_both", replace 



