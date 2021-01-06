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

/* load raw data */ 

import delimited using "$raw/qualtrics_pol/Covid Anxiety Experiment - Fielding 02_April 26, 2020_11.31.csv", clear varnames(1)
drop if !strpos(startdate,"2020")
drop if !strpos(startdate,"03-19")  & !strpos(startdate,"03-20") 
gen day_1 = 1 
tempfile launch_1
save `launch_1'

import delimited using "$raw/qualtrics_pol/Covid Anxiety Experiment - Fielding 03_April 26, 2020_11.34.csv", clear varnames(1)
drop if !strpos(startdate,"2020")
drop if !strpos(startdate,"03-19")  & !strpos(startdate,"03-20") 
append using `launch_1' 
replace day_1 = 0 if day_1 == . 
sum info_severity_do_q145

/* drop tests */
drop if rid == "test12345" 
drop if rid == "[%RID%]"
drop if mi(rid)

/* drop the several obs where rid is not unique */ 
bys rid: drop if _N != 1

/* drop zip code */
drop zip 

/* drop people who do not OK, via IRB */ 
drop if regexm(irb, "I do not wish") == 1
drop if status == "Survey Preview"
drop if finished == "False" 

/* drop people who fail attention checks */
drop if regexm(lower(q63),"puce") != 1 & real(q158_6) != 7 

/* drop people who finish in fewer than 2 mins [0 people] */
gen len = length(startdate) 
gen starttime = clock(substr(startdate,len-7,8),"hms")
gen endtime = clock(substr(enddate,len-7,8),"hms")
gen length_of_survey = endtime - starttime
replace length = endtime + hms(23,59,59) if endtime < starttime
format length %tc 
drop if length < hms(0,2,0)

/* generate information treatments */
destring info*, replace
gen mod_info = info_severity_do_q145 == 1
gen strong_info = info_severity_do_q135 == 1

/* generate action treatments  */
gen strong_action = show_strong_policy == "1"
gen interaction = strong_info * strong_action 

/**************************************/
/* /\* generate effect on anxiety *\/ */
/**************************************/
local i = 0 
forv num = 1/21 {
  local i = `i' + 1 
  gen anxiety_`num' = .
  replace anxiety_`num' = 0 if regexm(q105_`num',"Not at all") == 1
  replace anxiety_`num' = 1 if regexm(q105_`num',"Mildly") == 1
  replace anxiety_`num' = 2 if regexm(q105_`num',"Moderate") == 1
  replace anxiety_`num' = 3 if regexm(q105_`num',"Severe") == 1
  assert !mi(anxiety_`num')
  sum anxiety_`num'  if strong_info == 0 & strong_action == 0 
  gen std_anxiety_`num' = (anxiety_`num' - `r(mean)' ) / `r(sd)'  
}

egen anxiety_index = rowtotal(anxiety_*)
sum anxiety_index  if strong_info == 0 & strong_action == 0 
gen anxiety_std = (anxiety_index - `r(mean)' ) / `r(sd)'

egen anxiety_index_within_std = rowtotal(std_anxiety_*)
replace anxiety_index_within_std = anxiety_index_within / 21 

/************/
/* get gad  */
/************/
local i = 0 
forv num = 1/7 {
  local i = `i' + 1
  gen gad_`num' = . 
  replace gad_`num' = 0 if regexm(q89_`num',"Rarely") == 1
  replace gad_`num' = 1 if regexm(q89_`num',"Several") == 1
  replace gad_`num' = 2 if regexm(q89_`num',"Over") == 1
  replace gad_`num' = 3 if regexm(q89_`num',"Nearly") == 1  
  assert !mi(gad_`num')
}

egen gad_index = rowtotal(gad*)
sum gad_index if strong_info == 0 & strong_action == 0 
gen gad_std = (gad_index - `r(mean)' ) / `r(sd)'

gen ag = (gad_index / 7 + anxiety_index / 21 ) / 2
sum ag  if strong_info == 0 & strong_action == 0 
gen ag_std = (ag - `r(mean)' ) / `r(sd)'

gen wtp_n95 = real(q134_1) 
gen wtp_purell = real(q134_2) 
gen wtp_pasta = real(q134_3) 
gen wtp_coffee = real(q134_4) 
egen wtp_index = rowtotal(wtp*)
gen wtp_covid = wtp_index - 4 * wtp_coffee

/****************/
/* get beliefs  */
/****************/
gen cases_pre = real(q11)
gen death_rate_pre = real(q15)
gen deaths_pre = real(q96)

gen deaths_post = real(qdeaths)
gen cases_post = real(q134)
gen death_rate_post = real(q135)

gen cases_update = cases_post - cases_pre
gen death_rate_update = death_rate_post - death_rate_pre
gen deaths_update = deaths_post - deaths_pre

foreach var of varlist *update {
  sum `var' if strong_info == 0 & strong_action == 0, d 
  gen `var'_std = ( `var' - `r(mean)' )  / `r(sd)'
}


/* clean observations that are implausible  */
replace cases_pre = . if cases_pre > 300000000
replace cases_post = . if cases_post > 300000000

replace death_rate_post = . if death_rate_post > 1000
replace death_rate_pre = . if death_rate_pre > 1000

replace deaths_pre = . if deaths_pre > 300000000
replace deaths_post = . if deaths_post > 300000000

gen ln_deaths_post = ln(deaths_post)
gen ln_deaths_pre = ln(deaths_pre)
gen ln_cases_pre = ln(cases_pre)
gen ln_cases_post = ln(cases_post)

gen covid_index = cases_update_std + death_rate_update_std + deaths_update_std
replace covid_index = . if covid_index > 10

gen width_pre = real(q53) - real(q52) 
replace width_pre = . if real(q53) < real(q52)
replace width_pre =  . if real(q53) > 1000 | real(q52) > 1000

sum width_pre , d 
gen confident = width_pre < `r(p50)' 

gen width_post = real(q65) - real(q64) 
replace width_post = . if real(q65) < real(q64)
replace width_post =  . if real(q65) > 1000 | real(q64) > 1000

/* update in width */
sum width_pre if strong_info == 0 & strong_action == 0 , d 
gen certain = width_pre < `r(p50)' 

/****************************/
/* get whether follow news  */
/****************************/
gen follow_news_covid = real(q21_5)

/****************************/
/* get Falke  */
/****************************/
gen risk = real(q156_4) /*willing to take risks */
gen time = real(q157_1) /*willing to wait to benefit in the future */


/**************************/
/* get support for trump  */
/**************************/
/**** support for trump ****/
gen vote_trump_pre = real( q112_9 )
gen vote_trump_post = real( q151_9 )
gen vote_trump_update = vote_trump_post - vote_trump_pre
sum vote_trump_pre if strong_info == 0 & strong_action == 0 
gen vote_trump_update_std = vote_trump_update / `r(sd)'

gen dont_know = real(q112_4) 

gen trust_government_pre = real(q112_11) 
gen trust_government_post = real(q151_11) 

/**** gov has private info ****/ 
gen gov_has_private_info_pre = real(q112_8)
gen gov_has_private_info_post = real(q151_8)

/***** respondent knows what gov is doing ****/
gen know_what_gov_is_doing_pre = real(q112_4)
gen know_what_gov_is_doing_post = real(q151_4)

gen gov_app_pre = real(q112_5) 
gen gov_app_post = real(q151_5)
gen gov_overreact_pre = real(q112_7)
gen gov_overreact_post = real(q151_7)
gen gov_not_strong_pre = real(q112_6) 
gen gov_not_strong_post = real(q151_6) 

/*****************************/
/* /\**** memory task ****\/ */
/*****************************/
gen close_contact_correct = real(q37) == 6
gen handwashing_correct = real(q118) == 20
gen soap_correct = regexm(q126,"Soap and water") == 1 
gen sanitizer_correct = real(q124) == 60
gen days_home_correct = real(q125) == 3 
egen memory_index = rowtotal(*correct)

/* demands more info */
gen demand_info = q138 == "Yes" 

/* generate heterogeneity */
sum vote_trump_pre , d 
gen trump_voter = vote_trump_pre >= `r(p50)'

sum gov_has_private_info_pre , d 
gen priv_info = gov_has_private_info_pre >= `r(p50)'

/* generate altruistic */
gen dict_fract = real(q129_1)
replace dict_fract = real(q129_1) * 10 if day_1 == 0
sum dict_fract, d
gen is_altruistic = dict_fract >= `r(p50)'

/****************************/
/* /\* put into parties *\/ */
/****************************/
recode vote_trump_pre 0 1 2 = 0 ///
  3 4 5 6 7 = 1 ///
  8 9 10 = 2 , gen(party) 

/**************************************************/
/* /\* establish gov has private info or not *\/  */
/**************************************************/
recode gov_has_private_info_pre 0 1 2 3 4 5 6 7 8 = 0 ///
  9 10 = 1 , gen(info) 

/*********/
/* falk  */
/*********/
gen takes_risks = real(q156_4)
gen patience = real(q157_1)
gen punish_you = real(q157_2)
gen punish_others = real(q157_3)
gen give = real(q157_4)
gen return_favor = real(q158_1)
gen revenge = real(q158_2)
gen good_intentions = real(q158_3) 
gen good_math = real(q158_4)
gen postpone_tasks = real(q158_5)



foreach f in takes_risks patience punish_you punish_others give return_favor revenge good_intentions good_math postpone_tasks  {
  sum `f' if strong_info == 0 & strong_action == 0, d 
  gen `f'_std = (`f' - `r(mean)') / `r(sd)'
}


/***************************/
/* treatment-by-treatment  */
/***************************/
gen sa_national_emergency = info_strong_policy_do_q59 != .
gen sa_wartime_powers = info_strong_policy_do_q60 != .
gen sa_avoid_gatherings = info_strong_policy_do_q61 != .
gen sa_invisible_enemy = info_strong_policy_do_q62 != .
gen wa_less_flu = info_weak_policy_do_q55 != . 
gen wa_no_quarantine = info_weak_policy_do_q56 != . 
gen wa_dom_travel = info_weak_policy_do_q57 != . 
gen wa_no_fo = info_weak_policy_do_q63 != .

/**************************************/
/* heterogeneity by baseline beliefs  */
/**************************************/
sum death_rate_pre ,d 

gen q_0_25_death_rate_pre = inrange(death_rate_pre,`r(min)',`r(p25)')
gen q_0_10_death_rate_pre = inrange(death_rate_pre,`r(min)',`r(p10)')
gen q_90_100_death_rate_pre = inrange(death_rate_pre,`r(p90)'+1,`r(max)')
gen q_10_25_death_rate_pre = inrange(death_rate_pre,`r(p10)',`r(p25)')
gen q_25_50_death_rate_pre = inrange(death_rate_pre,`r(p25)'+1,`r(p50)')
gen q_50_75_death_rate_pre = inrange(death_rate_pre,`r(p50)'+1,`r(p75)')
gen q_75_90_death_rate_pre = inrange(death_rate_pre,`r(p75)'+1,`r(p90)')
gen q_75_100_death_rate_pre = inrange(death_rate_pre,`r(p75)'+1,`r(max)')

sum deaths_pre ,d 

gen q_0_25_deaths_pre = inrange(deaths_pre,`r(min)',`r(p25)')
gen q_10_25_deaths_pre = inrange(deaths_pre,`r(p10)',`r(p25)')
gen q_25_50_deaths_pre = inrange(deaths_pre,`r(p25)'+1,`r(p50)')
gen q_50_75_deaths_pre = inrange(deaths_pre,`r(p50)'+1,`r(p75)')
gen q_75_90_deaths_pre = inrange(deaths_pre,`r(p75)'+1,`r(p90)')
gen q_0_10_deaths_pre = inrange(deaths_pre,`r(min)',`r(p10)')
gen q_90_100_deaths_pre = inrange(deaths_pre,`r(p90)'+1,`r(max)')
gen q_75_100_deaths_pre = inrange(deaths_pre,`r(p75)'+1,`r(max)')

sum cases_pre ,d 

gen q_0_25_cases_pre = inrange(cases_pre,`r(min)',`r(p25)')
gen q_10_25_cases_pre = inrange(cases_pre,`r(p10)',`r(p25)')
gen q_25_50_cases_pre = inrange(cases_pre,`r(p25)'+1,`r(p50)')
gen q_50_75_cases_pre = inrange(cases_pre,`r(p50)'+1,`r(p75)')
gen q_75_90_cases_pre = inrange(cases_pre,`r(p75)'+1,`r(p90)')
gen q_0_10_cases_pre = inrange(cases_pre,`r(min)',`r(p10)')
gen q_90_100_cases_pre = inrange(cases_pre,`r(p90)'+1,`r(max)')
gen q_75_100_cases_pre = inrange(cases_pre,`r(p75)'+1,`r(max)')

/**********************/
/* update up or down  */
/**********************/
gen death_rate_up = death_rate_post > death_rate_pre 
gen deaths_up = deaths_post > deaths_pre
gen cases_up = cases_post > cases_pre

gen cases_down = cases_post < cases_pre
gen deaths_down = deaths_post < deaths_pre
gen death_rate_down = death_rate_post < death_rate_pre

foreach var in death_rate deaths cases {
    foreach dir in up down {
      replace `var'_`dir' = . if mi(`var'_post) | mi(`var'_pre)
    }
}



/*********************/
/* clean lucid vars  */
/*********************/
drop if regexm(political_party,"ANES") 
destring political_party, replace
cor vote_trump_pre political_party
cor vote_trump_pre political_party if inlist(political_party,0,1,2,8,9,10)
cor vote_trump_pre political_party if inlist(political_party,1,10)

tab age 

gen age_group = . 
replace age_group = 1 if inlist(age, "18-29","30-39") | inrange(real(age),18,39 ) 
replace age_group = 2 if inlist(age,"40-49","50-59") | inrange(real(age),40,59) 
replace age_group = 3 if inlist(age,"60-69","70-79", "80+") | inrange(real(age),60,90 )

gen  race_eth = "white" if ethnicity == "1"
replace  race_eth = "black" if ethnicity == "2"
replace  race_eth = "asian" if inlist(ethnicity,"4","5","6","7","8","9","10")
replace  race_eth = "hispanic" if inlist(hispanic,"2","3")
replace  race_eth = "other" if race_eth == "" 

egen stratum = group(age_group gender political_party)
egen group = group(age_group gender region race_eth)
destring political_party, replace 

gen ln_wtp_index = ln(wtp_index+1)
gen ln_wtp_coffee = ln(wtp_coffee+1)
gen ln_wtp_n95 = ln(wtp_n95+1)
gen ln_wtp_purell = ln(wtp_purell+1)
gen ln_wtp_pasta = ln(wtp_pasta+1)
gen ln_death_rate_post = ln(death_rate_post)
gen ln_death_rate_pre = ln(death_rate_pre)


/*************************/
/* index of gov support  */
/*************************/

* construct government support index
foreach time in pre post {
  foreach var in gov_not_strong gov_app gov_overreact {
    cap gen `var'_`time'_std = . 
    sum `var'_`time' if strong_info == 0 & strong_action == 0
    replace `var'_`time'_std = (`var'_`time' - `r(mean)')/ `r(sd)' 
    
  }
}

* create an index for government support
gen gov_index_post_std = (- gov_not_strong_post_std + gov_app_post_std - gov_overreact_post_std) / 3 
gen gov_index_pre_std = (- gov_not_strong_pre_std + gov_app_pre_std - gov_overreact_pre_std) / 3 

save "$int/launch_1_pol", replace 




/*




reg anxiety_std strong_info strong_action if q_0_10_death_rate_pre == 1 , r
reg anxiety_std strong_info strong_action if q_0_25_death_rate_pre == 1 , r
reg anxiety_std strong_info strong_action if q_25_50_death_rate_pre == 1 , r
reg anxiety_std strong_info strong_action if q_50_75_death_rate_pre == 1 , r
reg anxiety_std strong_info strong_action if q_75_100_death_rate_pre == 1 , r
reg anxiety_std strong_info strong_action if q_90_100_death_rate_pre == 1 , r
reg anxiety_std strong_info strong_action if q_75_90_death_rate_pre == 1 , r

reg anxiety_std strong_info strong_action if q_0_10_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_0_25_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_25_50_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_50_75_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_75_100_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_90_100_deaths_pre == 1 , r

reg anxiety_std strong_info strong_action if q_0_10_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_10_25_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_25_50_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_50_75_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_75_90_deaths_pre == 1 , r
reg anxiety_std strong_info strong_action if q_90_100_deaths_pre == 1 , r

reg anxiety_std strong_info strong_action if q_0_10_cases_pre == 1 , r
reg anxiety_std strong_info strong_action if q_10_25_cases_pre == 1 , r
reg anxiety_std strong_info strong_action if q_25_50_cases_pre == 1 , r
reg anxiety_std strong_info strong_action if q_50_75_cases_pre == 1 , r
reg anxiety_std strong_info strong_action if q_75_90_cases_pre == 1 , r
reg anxiety_std strong_info strong_action if q_90_100_cases_pre == 1 , r

/*********************************************************************/
/* WTP effects - info has a big effect, action has a smaller effect  */
/*********************************************************************/
reg wtp_index  strong_info strong_action if q_0_10_death_rate_pre == 1 , r
reg wtp_covid  strong_info strong_action if q_0_10_death_rate_pre == 1 , r
reg wtp_purell  strong_info strong_action if q_0_10_death_rate_pre == 1 , r
reg wtp_n95  strong_info strong_action if q_0_10_death_rate_pre == 1 , r

reg wtp_coffee  strong_info strong_action if q_0_10_death_rate_pre == 1 , r
reg wtp_pasta  strong_info strong_action if q_0_10_death_rate_pre == 1 , r

reg wtp_index  strong_info strong_action if wtp_coffee < 10 & wtp_pasta < 10 
reg wtp_coffee  strong_info strong_action if wtp_coffee < 10
reg wtp_pasta  strong_info strong_action if wtp_pasta < 10

reg wtp_purell  strong_info strong_action if wtp_pasta < 10 & wtp_coffee < 10 
reg wtp_n95  strong_info strong_action if wtp_pasta < 10 & wtp_coffee < 10

reg wtp_covid  strong_info strong_action if wtp_pasta < 10 & wtp_coffee < 10 
reg wtp_index  strong_info strong_action if wtp_coffee < 10 & q_0_10_death_rate_pre == 1 , 
reg wtp_index  strong_info strong_action if wtp_coffee < 10 & q_0_10_death_rate_pre == 1 , 

reg ln_wtp_index  strong_info strong_action if q_0_10_death_rate_pre == 1, 
reg ln_wtp_coffee  strong_info strong_action if q_0_10_death_rate_pre == 1, 
reg ln_wtp_n95  strong_info strong_action if q_0_10_death_rate_pre == 1, 
reg ln_wtp_purell  strong_info strong_action if q_0_10_death_rate_pre == 1, 
reg ln_wtp_pasta  strong_info strong_action if q_0_10_death_rate_pre == 1, 

reg ln_wtp_index  strong_info strong_action if q_0_10_deaths_pre == 1
reg ln_wtp_n95  strong_info strong_action if q_0_10_deaths_pre == 1
reg ln_wtp_purell  strong_info strong_action if q_0_10_deaths_pre == 1
reg ln_wtp_pasta  strong_info strong_action if q_0_10_deaths_pre == 1
reg ln_wtp_coffee  strong_info strong_action if q_0_10_deaths_pre == 1




reg wtp_index  strong_info strong_action if q_75_90_death_rate_pre == 1 , r
reg wtp_covid  strong_info strong_action if q_75_90_death_rate_pre == 1 , r 
reg wtp_purell  strong_info strong_action if q_75_90_death_rate_pre == 1 , r
reg wtp_n95  strong_info strong_action if q_75_90_death_rate_pre == 1 , r
reg wtp_coffee  strong_info strong_action if q_75_90_death_rate_pre == 1 , r
reg wtp_pasta  strong_info strong_action if q_75_90_death_rate_pre == 1 , r

reg wtp_index  strong_info strong_action if q_75_90_deaths_pre == 1 , r
reg wtp_purell  strong_info strong_action if q_75_90_deaths_pre == 1 , r
reg wtp_n95  strong_info strong_action if q_75_90_deaths_pre == 1 , r
reg wtp_coffee  strong_info strong_action if q_75_90_deaths_pre == 1 , r
reg wtp_pasta  strong_info strong_action if q_75_90_deaths_pre == 1 , r

reg wtp_index  strong_info strong_action if q_90_100_deaths_pre == 1 , r
reg wtp_purell  strong_info strong_action if q_90_100_deaths_pre == 1 , r
reg wtp_n95  strong_info strong_action if q_90_100_deaths_pre == 1 , r
reg wtp_coffee  strong_info strong_action if q_90_100_deaths_pre == 1 , r
reg wtp_pasta  strong_info strong_action if q_90_100_deaths_pre == 1 , r

reg wtp_purell strong_info strong_action if q_0_10_deaths_pre == 1 , r
reg wtp_n95 strong_info strong_action  if q_0_10_deaths_pre == 1 , r
reg wtp_coffee  strong_info strong_action  if q_0_10_deaths_pre == 1 , r
reg wtp_pasta  strong_info strong_action  if q_0_10_deaths_pre == 1 , r

reg dict_fract strong_info strong_action day_1 if q_0_10_death_rate_pre == 1 , r
reg dict_fract strong_info strong_action day_1 if q_0_10_deaths_pre == 1 , r
reg dict_fract strong_info strong_action day_1 if q_0_10_cases_pre == 1 , r

reg dict_fract strong_info strong_action day_1 if q_90_100_death_rate_pre == 1 , r
reg dict_fract strong_info strong_action day_1 if q_90_100_deaths_pre == 1 , r
reg dict_fract strong_info strong_action day_1 if q_90_100_cases_pre == 1 , r

/************************/
/* /\* REGRESSIONS *\/  */
/************************/



reg anxiety_std sa_* , r
reg gad_std strong_info strong_action , r
reg anxiety_std strong_info strong_action , r

reg anxiety_std strong_info strong_action if death_rate_pre < 10 , r
reg anxiety_std strong_info strong_action if deaths_pre < 200 , r
reg anxiety_std strong_info strong_action if cases_pre < 1000 , r

reg anxiety_std strong_info strong_action if death_rate_pre > 200 , r
reg anxiety_std strong_info strong_action if deaths_pre > 20000 , r
reg anxiety_std strong_info strong_action if deaths_pre > 70000 , r
reg anxiety_std strong_info strong_action if deaths_pre < 30  , r
reg anxiety_std strong_info strong_action if deaths_pre < 30  , r
reg anxiety_std strong_info strong_action if death_rate_pre < 4   , r
reg anxiety_std strong_info strong_action if death_rate_pre < 50   , r






reg wtp_covid strong_info strong_action if deaths_pre < 200   , r
reg wtp_purell strong_info strong_action if deaths_pre < 4   , r
reg wtp_purell strong_info strong_action if deaths_pre < 200   , r
reg wtp_purell strong_info strong_action if deaths_pre > 20000   , r

reg dict_fract strong_info strong_action day_1  , r 

/* Result 1: action makes people IN THE MIDDLE more anxious */
reg anxiety_std strong_info strong_action if inrange(vote_trump_pre,3,7)  , r
reg anxiety_std strong_info strong_action if inrange(vote_trump_pre,0,2)  , r 
reg anxiety_std strong_info strong_action if inrange(vote_trump_pre,8,10)  , r

/* Democrats tend to think it is much more serious (i.e., probability of a bad state is higher) */
reg ln_deaths_pre if inrange(vote_trump_pre,8,10) , r
reg ln_deaths_pre if inrange(vote_trump_pre,3,7)  , r
reg ln_deaths_pre if inrange(vote_trump_pre,0,2)  , r

reg priv if inrange(vote_trump_pre,0,2)  , r
reg priv if inrange(vote_trump_pre,3,7)  , r
reg priv if inrange(vote_trump_pre,8,10)  , r

/* and for these people there is also a big effect on death */ 
reg ln_deaths_post strong_info strong_action ln_deaths_pre if inrange(gov_has_private_info_pre,9,10)  , r
reg ln_cases_post strong_info strong_action ln_cases_pre if inrange(gov_has_private_info_pre,9,10)  , r
reg death_rate_post strong_info strong_action death_rate_pre if inrange(gov_has_private_info_pre,9,10)  , r
reg wtp_covid strong_info strong_action if inrange(gov_has_private_info_pre,9,10)  , r
reg wtp_covid strong_info strong_action , r


reg wtp_covid strong_info strong_action if party == 1  , r

reg anxiety_std strong_info strong_action


/* Result 2: it tends to be correlated with people who think the gov has private info */
reg anxiety_std strong_info strong_action if inrange(gov_has_private_info_pre,9,10)
reg anxiety_std strong_info strong_action if inrange(gov_has_private_info_pre,0,5)

/********************************************************************************************************************************************************************/
/* /\* anxiety is LOWER among people who think the government is not taking appropriate actions - i.e. people for whom, if the gov acts, they would feel better *\/ */
/* reg anxiety_std strong_info strong_action if inrange(not_strong_enough_pre,4,10) , r                                                                             */
/* reg anxiety_std strong_info strong_action if inrange(not_strong_enough_pre,1,3) , r                                                                              */
/* reg anxiety_std strong_info strong_action if inrange(not_strong_enough_pre,0,0) , r                                                                              */
/********************************************************************************************************************************************************************/

/****************************************/
/* /\* mediation analysis fails tho *\/ */
/****************************************/
reg anxiety_std  strong_info strong_action death_rate_pre death_rate_post ///
    width_pre width_post ///
    ln_deaths_post ln_deaths_pre ///
    ln_cases_pre ln_cases_post demand_info if inrange(gov_has_private_info_pre,9,10)  , r 

/* Actually, it is really among people who meet BOTH of these criteria */
reg anxiety_std strong_info strong_action if inrange(gov_has_private_info_pre,6,10) & inrange(vote_trump_pre,3,7)
reg anxiety_std strong_info strong_action if !inrange(gov_has_private_info_pre,6,10) | !inrange(vote_trump_pre,3,7)

reg ln_deaths_post strong_info strong_action ln_deaths_pre if inrange(gov_has_private_info_pre,6,10) & inrange(vote_trump_pre,3,7)
reg ln_cases_post strong_info strong_action ln_cases_pre if inrange(gov_has_private_info_pre,6,10) & inrange(vote_trump_pre,3,7)
reg death_rate_post strong_info strong_action death_rate_pre if inrange(gov_has_private_info_pre,6,10) & inrange(vote_trump_pre,3,7)

reg ln_deaths_post strong_info strong_action ln_deaths_pre , r 

reg deaths_post strong_info strong_action deaths_pre  , r
reg ln_deaths_post strong_info strong_action ln_deaths_pre  , r
reg death_rate_post strong_info strong_action death_rate_pre , r 


* good intentions: some effect 
reg good_intentions_std strong_info strong_action if party == 1 , r 
reg return_favor_std strong_info strong_action , r

reghdfe ln_wtp_index strong_info strong_action if party == 1 , absorb(stratum) cluster(stratum)


/*****************/
/* analyze falk  */
/*****************/
foreach f in takes_risks patience punish_you punish_others give return_favor revenge good_intentions good_math postpone_tasks  {
  reg `f'_std strong_info strong_action , r
  reg `f'_std strong_info strong_action if party == 1 , r

}

foreach f in takes_risks patience punish_you punish_others give return_favor revenge good_intentions good_math postpone_tasks  {
  reg `f'_std strong_info strong_action if q_0_10_death_rate_pre == 1 , r
}

foreach f in takes_risks patience punish_you punish_others give return_favor revenge good_intentions good_math postpone_tasks  {
  reg `f'_std strong_info strong_action if q_0_10_deaths_pre == 1 , r
}

reg ln_death_rate_post strong_info strong_action ln_death_rate_pre , r 

/*****************************/
/* demand for info + memory  */
/*****************************/

/* some suggestive evidence that non-trumpers have higher demand for info after action */ 
reg demand_info strong_info strong_action if inrange(vote_trump_pre,0,7)
reg demand_info strong_info strong_action if inrange(vote_trump_pre,0,7) 

reg dict_fract strong_info strong_action if inrange(vote_trump_pre,0,2)
reg dict_fract strong_info strong_action day_1 if inrange(vote_trump_pre,3,7)
reg dict_fract strong_info strong_action day_1 if q_0_10_death_rate_pre 


/*********************************************************************/
/* /\* Result: action makes trump supporters more likely to vote *\/ */
/*********************************************************************/
reg vote_trump_post strong_info strong_action vote_trump_pre strong_action#c.vote_trump_pre, r
reg vote_trump_post strong_info strong_action vote_trump_pre , r

/* Result weakly significant: strong action makes people find gov reaction more appropriate. Effect not stronger for Trump supporters */
* reg appropriate_post strong_action appropriate_pre , r

/* Sanity check: Dems agree less that Gov has private info*/
reg gov_has_private_info_pre vote_trump_pre





