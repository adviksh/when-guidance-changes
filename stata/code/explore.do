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

  /* individually regress above , at, below */
  reg update_mag shifty if priors_above_info == 1 , 
  estimates store above

  reg update_mag shifty if priors_at_info == 1 , 
  estimates store at


  suest above at , rob 

lincom  [at_mean][shifty] - [above_mean][shifty] 

reg update_mag shifty priors_at_info 1.priors_at_info#1.shifty if priors_below_info ==0 , rob 



  reg update_mag shifty if priors_below_info == 1 , 
  estimates store below

  /* all people who are not optimists */ 
  reg update_mag shifty if priors_below_info == 0 , 
  estimates store not_below

  suest above at below , rob 




gen race_eth = "white non-hispanic" if ethnicity == "1"
replace race_eth = "black non-hispanic" if ethnicity == "2"
replace race_eth = "hispanic" if inrange(real(hispanic),2,14) 
replace race_eth = "asian/other" if race_eth == "" 

destring hhi , replace 
gen hhi_bin = "low or missing" if inrange(hhi,1,4) | hhi == -3105
replace hhi_bin = "middle" if inrange(hhi,5,18) 
replace hhi_bin = "high" if inrange(hhi,19,.) 

destring education, replace 
cap drop educ_bin
gen educ_bin = "hs or less or missing" if inlist(education,1,2,-3105)
replace educ_bin = "some college" if inlist(education,3,4,5)
replace educ_bin = "college+" if inlist(education,6,7,8)






reghdfe update_mag shifty, vce(rob) absorb(stratum)
destring stratum, replace
reg update_mag shifty, vce(rob) absorb(stratum)

reghdfe update_mag shifty, rob absorb(stratum)


  nlcom (_b[shifty] + _b[_cons]) / _b[_cons]
  matrix beta = r(b)
  local b: di %5.3f beta[1,1]
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = `b'/`std_err'
  local pvalue = 2*normal(-abs(`z'))
  count_stars, p(`pvalue') 
  

  /***** tests ******/
  /* above = at */ 
  nlcom ([above_mean]shifty + [above_mean]_cons) / [above_mean]_cons - ([at_mean]shifty + [at_mean]_cons) / [at_mean]_cons, post
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))

  /* at = below */
  suest above at below , rob 
  nlcom ([at_mean]shifty + [at_mean]_cons) / [at_mean]_cons - ([below_mean]shifty + [below_mean]_cons) / [below_mean]_cons
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))

  /* above = below */
  suest above at below , rob 
  nlcom ([above_mean]shifty + [above_mean]_cons) / [above_mean]_cons - ([below_mean]shifty + [below_mean]_cons) / [below_mean]_cons
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))

  /* test for above = not above */
  suest below not_below , rob
  nlcom ([below_mean]shifty + [below_mean]_cons) / [below_mean]_cons - ([not_below_mean]shifty + [not_below_mean]_cons) / [not_below_mean]_cons  
  matrix b = r(b)
  matrix V = r(V)
  local std_err = sqrt(V[1,1])
  local z = b[1,1]/`std_err'
  local pvalue = 2*normal(-abs(`z'))





/* get the effect on the ratio, excluding stratum FEs per the footnote in the paper */ 
reg update_covid shifty if inrange(deaths_pre,0,99999) == 1, 
estimates store opt
global ratio_optimist = (_b[shifty]+_b[_cons])/(_b[_cons])

reg update_covid shifty if inrange(deaths_pre,100000,240000) == 1, 
estimates store at
global ratio_at = (_b[shifty]+_b[_cons])/(_b[_cons])

reg update_covid shifty if inrange(deaths_pre,240001,.) == 1, 
estimates store pess 
global ratio_above = (_b[shifty]+_b[_cons])/(_b[_cons])

suest opt at pess, rob
test ([opt_mean]shifty = [at_mean]shifty)

reg update_covid 1.shifty priors_above_info priors_at_info ///
    1.shifty#priors_above_info 1.shifty#priors_at_info, rob





reghdfe update_mag shifty, absorb(stratum) 
nlcom exp(_b[shifty])-1


/* get the effect on the ratio, excluding stratum FEs per the footnote in the paper */ 
reg update_covid shifty if inrange(deaths_pre,0,99999) == 1, 
estimates store opt
global ratio_optimist = (_b[shifty]+_b[_cons])/(_b[_cons])

reg update_covid shifty if inrange(deaths_pre,100000,240000) == 1, 
estimates store at
global ratio_at = (_b[shifty]+_b[_cons])/(_b[_cons])

reg update_covid shifty if inrange(deaths_pre,240001,.) == 1, 
estimates store pess 
global ratio_above = (_b[shifty]+_b[_cons])/(_b[_cons])


nlcom ([opt_mean]shifty - [at_mean]shifty)

nlcom ([opt_mean][shifty] + [opt_mean][_cons]) / ([opt_mean][_cons]) - ([at_mean][shifty] + [at_mean][_cons]) / ([at_mean][_cons])




nlcom ([at_mean][shifty] + [at_mean][_cons]) / ([at_mean][_cons]) - ([pess_mean][shifty] + [pess_mean][_cons]) / ([pess_mean][_cons])
nlcom ([opt_mean][shifty] + [opt_mean][_cons]) / ([opt_mean][_cons]) - ([pess_mean][shifty] + [pess_mean][_cons]) / ([pess_mean][_cons])




di ${ratio_optimist}
di ${ratio_at}
di ${ratio_above}





reghdfe gov_index_post_std shifty c.gov_index_pre_std ln_deaths_pre c.ln_deaths_pre#shifty if ln_deaths_pre<=ln(1000000), absorb(stratum)

reghdfe update_mag_deaths, 

reghdfe gov_app_post_std shifty c.gov_app_pre_std ln_deaths_pre c.ln_deaths_pre#shifty, absorb(stratum)
reghdfe gov_has_private_info_post_std shifty c.gov_has_private_info_pre_std ln_deaths_pre c.ln_deaths_pre#shifty, absorb(stratum)

reghdfe update_mag_deaths shifty ln_deaths_pre c.ln_deaths_pre#shifty, absorb(stratum)
reghdfe update_covid shifty ln_deaths_pre c.ln_deaths_pre#shifty, absorb(stratum)

reghdfe update_mag_deaths shifty ln_deaths_pre c.ln_deaths_pre#shifty, absorb(stratum)





reg shifty update_mag_deaths ln_deaths_pre







test update_mag_deaths ln_deaths_pre

reg ln_deaths_pre shifty
estimates store test_1

reg update_mag_deaths shifty
estimates store test_2

suest test_1 test_2 
test [test_1_mean]shifty [test_2_mean]shifty

reg shifty ln_deaths_pre update_mag_deaths
test ln_deaths_pre update_mag_deaths

reg update_mag_deaths shifty i.stratum if party == 0 & _n < 100 
estimates store update_mag_deaths_below

reg update_mag_deaths shifty i.stratum if party != 0
estimates store update_mag_deaths_above

suest update_mag_deaths_below update_mag_deaths_above, rob
test [update_mag_deaths_above_mean]12.stratum [update_mag_deaths_below_mean]11.stratum 
test [update_mag_deaths_above_mean]13.stratum [update_mag_deaths_below_mean]5.stratum 
test [update_mag_deaths_above_mean]13.stratum [update_mag_deaths_below_mean]18.stratum  [update_mag_deaths_below_mean]5.stratum 







reghdfe update_covid shifty, absorb(stratum)

reghdfe update_mag shifty, absorb(stratum)



destring stratum , replace
destring political, replace

gen rand = runiform() 
drop if rand < 0.05 & party == 2 & shifty == 1
reghdfe update_covid shifty, absorb(stratum)
reghdfe update_mag_deaths shifty, absorb(stratum) 

reg update_mag_deaths i.political_party



reg update_covid i.party


test [update_mag_deaths_below_mean][shifty] = [update_mag_deaths_above_mean][shifty]


[update_mag_deaths_above_mean][shifty]


ivreghdfe update_covid (gov_app_post_std = shifty) gov_app_pre_std if priors_at_info, absorb(stratum)
ivreghdfe update_mag_deaths (gov_app_post_std = shifty) gov_app_pre_std, absorb(stratum)

reghdfe gov_app_post_std shifty gov_app_pre_std if priors_at_info , absorb(stratum)





estimates store update_covid_rep

reg update_covid shifty i.stratum if party != 2 // Note that suest wont accept non-robust SEs for the preliminary reg
estimates store update_covid_not_rep

suest update_covid_rep update_covid_not_rep, rob // Now you fit the simultaneous robust SEs


list q96 q123 if regexm(q121, "best guess") == 1 
assert q96==q123  if regexm(q121, "best guess") == 1 
keep if shifty != .

reg update_mag_deaths shifty if party == 0 
reg update_mag_deaths shifty if party == 1
reg update_mag_deaths shifty if party == 2

reg ln_deaths_pre shifty 


reghdfe update_covid shifty if party == 0 , absorb(stratum)
reghdfe update_covid shifty if party == 1, absorb(stratum)
reghdfe update_covid shifty if party == 2, absorb(stratum)

reghdfe update_covid shifty 2.party 2.party#shifty, absorb(stratum)





reghdfe gov_index_post_std shifty gov_index_pre_std 1.priors_below_info 1.priors_below_info#1.shifty, absorb(stratum) vce(rob)


reghdfe update_covid shifty 1.priors_at_info 1.priors_at_info#1.shifty 1.priors_above_info 1.priors_above_info#1.shifty, absorb(stratum) vce(rob)
test 1.priors_above_info#1.shifty + shifty = 0
di ttail(`r(df_r)',sqrt(`r(F)'))
test 1.priors_above_info#1.shifty = 0
di ttail(`r(df_r)',sqrt(`r(F)'))

reghdfe update_mag_deaths shifty 1.priors_at_info 1.priors_at_info#1.shifty 1.priors_above_info 1.priors_above_info#1.shifty, absorb(stratum) vce(rob)
test 1.priors_above_info#1.shifty+shifty = 0
test 1.priors_above_info#1.shifty = 0
di ttail(`r(df_r)',sqrt(`r(F)'))

reghdfe update_mag_deaths shifty 1.priors_at_info 1.priors_at_info#1.shifty 1.priors_above_info 1.priors_above_info#1.shifty, absorb(stratum) vce(rob)
test 1.priors_above_info#1.shifty = shifty
test 1.priors_above_info#1.shifty = 0
di ttail(`r(df_r)',sqrt(`r(F)'))





reghdfe update_covid shifty 1.priors_at_info 1.priors_at_info#1.shifty 1.priors_above_info 1.priors_above_info#1.shifty, absorb(stratum) vce(rob)
test 1.priors_above_info#1.shifty
test 1.priors_at_info#1.shifty
di ttail(`r(df_r)',sqrt(`r(F)'))

reghdfe update_mag_deaths shifty 1.priors_at_info 1.priors_at_info#1.shifty 1.priors_above_info 1.priors_above_info#1.shifty, absorb(stratum) vce(rob)
test 1.priors_above_info#1.shifty
test 1.priors_at_info#1.shifty
di ttail(`r(df_r)',sqrt(`r(F)'))

reghdfe gov_index_post_std shifty gov_index_pre_std 1.priors_at_info 1.priors_at_info#1.shifty 1.priors_above_info 1.priors_above_info#1.shifty, absorb(stratum) vce(rob)
test 1.priors_above_info#1.shifty
test 1.priors_at_info#1.shifty
di ttail(`r(df_r)',sqrt(`r(F)'))









gen test = ln(abs_mag + .001)
gen test2 = ln(abs_mag + .01)
gen test3 = ln(abs_mag + .1)




reg test shifty, absorb(stratum)
reg test2 shifty, absorb(stratum)
reg test3 shifty, absorb(stratum)




gen female = gender == "2" 
reg female shifty 
reg female shifty 
gen young = age == "18-29"
reg young shifty

gen middle = age == "60-69" 
gen middle_2 = age == "50-59" 
gen middle_3 = age == "30-39" 
gen hs = educ == "2"
gen poor = hhi == "1"
gen white = eth == "1"
gen black = eth == "2"

destring region gender educ eth hhi, replace
destring political_party, replace
egen age_group = group(age)
destring hispanic, replace
gen partyl = 1 if inrange(political_party,0,2)
replace partyl = 2 if inrange(political_party,3,7)
replace partyl = 3 if inrange(political_party,8,10)
destring stratum, replace

reg shifty white black i.gender i.partyl i.age_group 


reg poor shifty, rob  
reg hs shifty , rob

sureg (y: young shifty) (a: middle shifty) (a2: middle_2 shifty) (a3: middle_3 shifty) ///
    (p: poor shifty) (h: hs shifty) (w: white shifty) (b: black shifty)

test ([y][shifty] = 0)  ([a][shifty] = 0) ([a2][shifty] = 0) ([a3][shifty] = 0) ([p][shifty] = 0) ([h][shifty] = 0) ///
    ([w][shifty] = 0 ) ([b][shifty] = 0)  


reghdfe poor shifty , absorb(stratum)
reghdfe update_covid shifty , absorb(stratum hhi educ region eth hisp gender )
reghdfe update_mag_deaths shifty , absorb(stratum hhi educ region eth hisp gender age )



reghdfe update_mag_deaths shifty if info_above_priors, absorb(stratum)


reghdfe update_mag_deaths shifty if info_at_priors, absorb(stratum)
reghdfe update_mag_deaths shifty if info_above_priors, absorb(stratum)


gen above_median =  q_0_25_deaths_pre == 1 | q_25_50_deaths_pre == 1
reg gov_index_post_std shifty gov_index_pre_std above_median 1.above_median#1.shifty
reg gov_index_post_std shifty gov_index_pre_std info_at_priors 1.info_at_priors#1.shifty if !info_above_priors
reg gov_index_post_std shifty gov_index_pre_std info_above_priors 1.info_above_priors#1.shifty

reghdfe update_mag_deaths shifty c.deaths_pre#1.shifty deaths_pre, absorb(stratum)
reghdfe update_mag_deaths shifty c.ln_deaths_pre#1.shifty ln_deaths_pre, absorb(stratum)




reg update_mag_deaths shifty info_below_priors 1.info_below_priors#1.shifty 
reghdfe update_mag_deaths shifty info_below_priors 1.info_below_priors#1.shifty , absorb(stratum)
reghdfe update_covid shifty info_below_priors 1.info_below_priors#1.shifty , absorb(stratum)

sum dont_know_pre if shifty == 0 
gen dont_know_pre_std = (dont_know_pre - `r(mean)') / `r(sd)' 

reg update_covid dont_know_pre_std
reg update_covid vote_trump_pre_std

reg update_covid if dont_know_pre <= 4
reg update_covid if dont_know_pre >= 5


reg update_covid shifty if dont_know <= 4
reg update_covid shifty if dont_know >= 5 

cap drop *certain*
sum width_pre , d 
gen certain = width_pre <= `r(mean)' 
gen very_certain = width_pre <= `r(p25)' 
gen very_very_certain = width_pre <= `r(p10)' 



reghdfe update_mag_deaths shifty ln_width_pre certain 1.certain#1.shifty, absorb(stratum)
reghdfe update_covid shifty ln_width_pre certain 1.certain#1.shifty, absorb(stratum)
reghdfe update_covid shifty ln_width_pre very_very_certain 1.very_very_certain#1.shifty, absorb(stratum)

reghdfe update_mag_deaths shifty if q_0_10_deaths_pre == 1 , absorb(stratum)
gen above_median =  q_0_25_deaths_pre == 1 | q_25_50_deaths_pre == 1
reghdfe update_mag_deaths shifty if q_0_10_deaths_pre == 1 , absorb(stratum)

reghdfe update_mag_deaths shifty above_median 1.above_median#1.shifty, absorb(stratum) 
reghdfe update_covid shifty above_median 1.above_median#1.shifty, absorb(stratum) 

gen abs_update_deaths = abs(deaths_update)
reghdfe abs_update_deaths shifty if deaths_pre < 1000000 & deaths_post < 1000000, absorb(stratum)

destring stratum, replace

qreg abs_update_deaths shifty, q(60)
qreg abs_update_deaths shifty, q(60)

qreg abs_update_deaths shifty i.stratum, q(60)


bsqreg abs_update_deaths shifty i.stratum, q(95) 
gen update_mag_deaths = ln(abs(deaths_post - deaths_pre) + 1 )
qreg update_mag_deaths shifty i.stratum, q(99) 

drop info_above_priors
drop info_below_priors

gen update_mag_deaths = ln(abs(deaths_post - deaths_pre) + 1 )

gen info_above_priors = inrange(deaths_pre, 0, 99999)
gen info_below_priors = inrange(deaths_pre, 100000, .)
gen info_at_priors = inrange(deaths_pre, 100000, 240000)

/**** constructing absolute value to aggregate ****/ 
foreach var in ln_deaths_post ln_deaths_pre shifty {
  cap drop abs_`var' 
  gen abs_`var' = `var'
  replace abs_`var' = -`var' if deaths_pre >= 170000 & !mi(deaths_pre) 
}

reg abs_ln_deaths_post shifty abs_ln_deaths_pre if info_below_priors == 1 & info_at_priors == 0
reg ln_deaths_post shifty ln_deaths_pre if info_above_priors == 1 

reg abs_ln_deaths_post shifty abs_ln_deaths_pre if deaths_pre >= 170000
reghdfe abs_ln_deaths_post shifty abs_ln_deaths_pre, absorb(stratum)

reghdfe ln_deaths_post shifty ln_deaths_pre if info_at_priors == 0, absorb(stratum)

/***** constructing wedge *****/
cap drop wedge* 
gen wedge = - (ln_deaths_pre - ln(170000))
gen wedgeXshifty = wedge * shifty 
gen abs_dist = abs(ln_deaths_pre-170000)

reghdfe update_covid wedgeXshifty if info_at_priors == 0, absorb(stratum)
reghdfe update_covid shifty, absorb(stratum)

gen ln_post_prior = ln(deaths_post/deaths_pre)
reghdfe ln_deaths_post shifty ln_deaths_pre, absorb(stratum)


reghdfe update_mag_deaths shifty if info_above_priors == 1, absorb(stratum)
reghdfe update_mag_deaths shifty if info_below_priors == 1, absorb(stratum)


reghdfe update_mag_deaths shifty if info_above_priors == 1 & info_at_priors == 0, absorb(stratum)
reghdfe update_mag_deaths shifty if info_below_priors == 1 & info_at_priors == 0, absorb(stratum)
reghdfe update_mag_deaths shifty if info_at_priors == 1, absorb(stratum)

reghdfe update_mag_deaths shifty ln_deaths_pre c.ln_deaths_pre#1.shifty, absorb(stratum)



reghdfe ln_deaths_post abs_shifty ln_deaths_pre, absorb(stratum)

/* heterogeneity */ 

gen shifty_1 = shifty * (tmt_order_id == 1 )
gen shifty_2 = shifty * (tmt_order_id == 2 ) 
gen steady_1 = (shifty==0) * (tmt_order_id == 1 ) 
gen steady_2 = (shifty==0) * (tmt_order_id == 2 )

/* note that flu = 2 */
destring tmt_order, replace 
reghdfe update_covid shifty, absorb(stratum)
global mainest = _b[shifty]

reghdfe update_mag_deaths 1.shifty#i.tmt_order_id if shifty != . , absorb(stratum)
test (1.shifty#1.tmt_order_id = 1.shifty#2.tmt_order_id) 

reghdfe update_mag_deaths 0.shifty#i.tmt_order_id if shifty != . , absorb(stratum)
test (0.shifty#1.tmt_order_id = 0.shifty#2.tmt_order_id) 

reghdfe update_covid 1.shifty#i.tmt_order_id if shifty != . , absorb(stratum)
test (1.shifty#1.tmt_order_id = 1.shifty#2.tmt_order_id) 

reghdfe update_covid 0.shifty#i.tmt_order_id if shifty != . , absorb(stratum)
test (0.shifty#1.tmt_order_id = 0.shifty#2.tmt_order_id) 

gen deaths_win_post = deaths_post
replace deaths_win_post = min(deaths_post,500000)
gen deaths_win_pre = min(deaths_pre,500000)
gen abs = min(abs(deaths_win_post - deaths_win_pre),500000)
reg abs shifty 



gen toward_signal = update_covid_up if inrange(deaths_post,0,99999) == 1
replace toward_signal = update_covid_down if inrange(deaths_pre,240000,.) == 1

reg signal shifty 

reg number shifty if info_below_priors == 1
reg number shifty if info_above_priors == 1

reg index_covid shifty if info_below_priors == 1
reg index_covid shifty if info_above_priors == 1

reg wtp_index shifty if info_above_priors == 1
reg wtp_index shifty if info_below_priors == 1

reghdfe number shifty, absorb(stratum)
reghdfe number strong, absorb(stratum)

reg index_covid shifty if info_below_priors == 1
reg index_covid shifty if info_above_priors == 1







reg ln_wtp_index shifty if info_below_priors == 1





gen smartphone = (q181_clickcount == "0" & !mi(q117_4))

reg speed_alt_covid_std strong if smartphone == 0
reg speed_covid_std strong if smartphone == 0
reg speed_alt_covid_std strong if smartphone == 1

gen index_covid_alt = (frac_accurate_covid_std + speed_alt_covid_std )/2

reg index_covid strong i.smartphone 
reg index_covid_alt strong i.smartphone 


count if mi(time_covid_std) & !mi(frac_accurate_covid)
count if !mi(time_covid_std) & mi(frac_accurate_covid)

count if mi(time_metro_std) & !mi(frac_accurate_metro)
count if !mi(time_metro_std) & mi(frac_accurate_metro)

count if mi(q117_4) & mi(time_metro_std)
tab time_covid_std if mi(q117_4) & mi(time_metro_std)
list q117* if mi(q117_4) & mi(time_metro_std)


count if mi(q117_4) & mi(time_metro_std) 





assert !mi(number_others) 
gen see_others = number_others > 0

reghdfe number_others shifty, absorb(stratum)
reghdfe number_others strong, absorb(stratum)

reghdfe see_others shifty, absorb(stratum)

reghdfe number_others shifty if info_above_priors == 1, absorb(stratum)
reghdfe number_others shifty if info_below_priors == 1, absorb(stratum)

reghdfe see_others shifty if info_above_priors == 1, absorb(stratum)
reghdfe see_others shifty if info_below_priors == 1, absorb(stratum)

reghdfe number_others shifty if q_0_25_deaths_pre == 1, absorb(stratum)

reghdfe number_others shifty if q_0_25_deaths_pre == 1, absorb(stratum)
