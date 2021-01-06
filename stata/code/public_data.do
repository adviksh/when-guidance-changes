/********************************/
/* constructs a public dataset  */
/********************************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

use "$int/launch_2" , clear
lab var update_mag "ln(|priors - posteriors| + 1) (update magnitude)"
lab var deaths_pre "priors"
lab var deaths_post "posteriors"
lab var update_covid "1(priors not equal to posteriors) (update propensity)"
lab var shifty "1 = Inconsistent treatment, 0 = Consistent treatment"

foreach suffix in "" "_std" {
  local name = "" 
  if "`suffix'" != "" local name = "standardized, " 
  lab var gov_index_post`suffix' "faith in government index (`name'posterior)"
  lab var gov_index_pre`suffix' "faith in government index (`name'prior)"
  lab var gov_not_strong_post`suffix' "government not acting strong enough (`name'posterior)"
  lab var gov_not_strong_pre`suffix' "government not acting strong enough (`name'prior)"
  lab var gov_app_post`suffix' "government acting appropriately (`name'posterior)"
  lab var gov_app_pre`suffix' "government acting appropriately (`name'prior)"
  lab var gov_overreact_post`suffix' "government is overreacting (`name'posterior)"
  lab var gov_overreact_pre`suffix' "government is overreacting (`name'prior)"
  lab var gov_has_private_info_post`suffix' "government has private information (`name'posterior)"
  lab var gov_has_private_info_pre`suffix' "government has private information (`name'prior)"
  lab var vote_trump_post`suffix' "vote for trump (`name'posterior)"
  lab var vote_trump_pre`suffix' "vote for trump (`name'prior)"
}

keep startdate enddate dow_* age-region hispanic guess* tmt_order_id stratum  ri_stratum dont_know_pre vote_trump* party frac* time* index* speed* dfi*  update_mag deaths_pre deaths_post update_covid shifty *wtp* risky abs_mag* q_* q196 q112_16 q121 number_others *width* *dji* death_rate* priors*info gov* ln_deaths*
save "${int}/public_data", replace 
