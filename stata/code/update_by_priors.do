/*********************************************************/
/* This shows the effect of updating by specific priors  */
/*********************************************************/
do ../SetGlobals.do

/***************************************/
/* helper function to write to a file  */
/***************************************/
capture pr drop writetreat
capture pr define writetreat
syntax, outcome(string) bin(string) distrib(string)

    if "`bin'" == "0_10" local binname = "q0-p10"
    if "`bin'" == "10_25" local binname = "p11-p25"
    if "`bin'" == "25_50" local binname = "p26-p50"
    if "`bin'" == "50_75" local binname = "p51-p75"
    if "`bin'" == "75_90" local binname = "p76-p90"
    if "`bin'" == "90_100" local binname = "p91-p100"
    if "`bin'" == "0_25" local binname = "p0-p25"
    if "`bin'" == "75_100" local binname = "p76-p100"

    local actionest = _b[strong_action] 
    local actionse = _se[strong_action]

    local infoest = _b[strong_info] 
    local infose = _se[strong_info]

    file write fh "`outcome',info,1,"  (`infoest') "," (`infose') ",`binname',`distrib'" _n
    file write fh "`outcome',action,1," (`actionest') "," (`actionse') ",`binname',`distrib'" _n

end

/***************************************/
/* helper function to get the p value  */
/***************************************/
capture pr drop extract_p
capture pr define extract_p
syntax, name(string) [pos(int 3) ]

    matrix rtable = r(table) 
    local placeholder = rtable[4,`pos']
    global `name': di %5.3f `placeholder'
    if (${`name'} < 0.001 ) global ${`name'} = "{&le} 0.001"

end


/************************************************/
/* write estimates to a .csv for easy graphing  */
/************************************************/
capture file close fh
file open fh using out/update_by_priors.csv , write replace 
file write fh "outcome,treatment,launch,est,se,bin,distrib" _n

/* LAUNCH 1  */
use "$int/launch_1_pol", clear
gen above_median = (q_0_25_death_rate_pre == 1 | q_25_50_death_rate_pre == 1)

reghdfe ln_wtp_index strong_info strong_action, absorb(group) vce(rob)
global ln_wtp_index_avg : di %5.3f _b[strong_info]
global ln_wtp_index_avg_se : di %5.3f _se[strong_info]

reghdfe ln_wtp_index strong_info strong_action above_median 1.strong_info#1.above_median 1.strong_action#1.above_median , absorb(group) vce(rob)
extract_p, name(ln_wtp_index_med_l1) pos(4)

reghdfe ln_wtp_index strong_info strong_action q_0_25_death_rate_pre 1.strong_info#1.q_0_25_death_rate_pre 1.strong_action#1.q_0_25_death_rate_pre , absorb(group) vce(rob)
extract_p, name(ln_wtp_index_25_l1) pos(4)

reghdfe gad_std strong_info strong_action above_median 1.strong_info#1.above_median 1.strong_action#1.above_median  , absorb(group) vce(rob)
extract_p, name(gad_std_med_l1) pos(4)

reghdfe gad_std strong_info strong_action q_0_25_death_rate_pre 1.strong_info#1.q_0_25_death_rate_pre 1.strong_action#1.q_0_25_death_rate_pre , absorb(group) vce(rob)
extract_p, name(gad_std_25_l1) pos(4)

reghdfe gad_std strong_info strong_action, absorb(group) vce(rob) 
global gad_std_avg : di %5.3f _b[strong_info]
global gad_std_avg_se : di %5.3f _se[strong_info]

qui {

    /* loop over bins */ 
    foreach bin in 0_10 10_25 0_25 25_50 50_75 75_90 90_100 75_100 {

    /* regress on update */ 
      reghdfe death_rate_post strong_info strong_action death_rate_pre if q_`bin'_death_rate_pre == 1 , absorb(group) vce(rob) 
      writetreat, outcome(death_rate_post) bin(`bin') distrib(death_rate)

    /* regress on update */ 
      reghdfe ln_deaths_post strong_info strong_action ln_deaths_pre if q_`bin'_deaths_pre == 1 , absorb(group) vce(rob)
      writetreat, outcome(ln_deaths_post) bin(`bin') distrib(deaths)
  
    /* regress on anxiety */ 
      reghdfe anxiety_std strong_info strong_action if q_`bin'_death_rate_pre == 1 , absorb(group)  vce(rob) 
      writetreat, outcome(anxiety) bin(`bin') distrib(death_rate)

    /* regress on gad */   
      reghdfe gad_std strong_info strong_action if q_`bin'_death_rate_pre == 1 , absorb(group)  vce(rob)
      writetreat, outcome(gad_std) bin(`bin') distrib(death_rate)

    /* dicatorship fraction */ 
      reghdfe dict_fract strong_info strong_action day_1 if q_`bin'_death_rate_pre == 1 ,  absorb(group) vce(rob)
      writetreat, outcome(dict_fract)  bin(`bin') distrib(death_rate)

    /* reghdferess on ln_wtp indices */ 
        foreach wtp in index coffee pasta purell n95 { 
          reghdfe ln_wtp_`wtp' strong_info strong_action if q_`bin'_death_rate_pre == 1 , absorb(group) vce(rob)
          writetreat, outcome(ln_wtp_`wtp') bin(`bin') distrib(death_rate)
}

/* look at update propensity */ 
foreach outcome in cases death_rate deaths {
  
  reghdfe `outcome'_up strong_info strong_action if q_`bin'_`outcome'_pre == 1 , absorb(group) vce(rob)
  writetreat, outcome(`outcome'_up) bin(`bin') distrib(`outcome')
  
  reghdfe `outcome'_down strong_info strong_action if q_`bin'_`outcome'_pre == 1 , absorb(group) vce(rob)
  writetreat, outcome(`outcome'_down) bin(`bin') distrib(`outcome')
  
    }

}
}

/* LAUNCH 2  */
use "$int/public_data" , clear
gen above_median = (q_0_25_deaths_pre==1 | q_25_50_deaths_pre==1)

foreach outcome in update_up_deaths update_down_deaths {
    reghdfe `outcome' strong, absorb(stratum) vce(rob)
    global `outcome'_avg: di %5.3f _b[strong]
    global `outcome'_avg_se: di %5.3f _se[strong]
}

reghdfe ln_deaths_post strong ln_deaths_pre, absorb(stratum) vce(rob)
global ln_deaths_post_avg: di %5.3f _b[strong]
global ln_deaths_post_avg_se: di %5.3f _se[strong]


reghdfe update_up_deaths strong above_median 1.above_median#1.strong  , absorb(stratum)   vce(rob)
extract_p, name(update_up_deaths_med)

reghdfe update_down_deaths strong above_median 1.above_median#1.strong  , absorb(stratum)   vce(rob)
extract_p, name(update_down_deaths_med)

reghdfe update_up_deaths strong q_0_25_deaths_pre 1.q_0_25_deaths_pre#1.strong  , absorb(stratum)   vce(rob)
extract_p, name(update_up_deaths_25)

reghdfe update_down_deaths strong q_0_25_deaths_pre 1.q_0_25_deaths_pre#1.strong  , absorb(stratum)   vce(rob)
extract_p, name(update_down_deaths_25)

reghdfe ln_deaths_post strong ln_deaths_pre above_median 1.above_median#1.strong  , absorb(stratum)   vce(rob)
extract_p, name(ln_deaths_post_med) pos(4)

reghdfe ln_deaths_post strong ln_deaths_pre q_0_25_deaths_pre 1.q_0_25_deaths_pre#1.strong  , absorb(stratum)   vce(rob)
extract_p, name(ln_deaths_post_25) pos(4)

/* loop over bins */
foreach bin in 0_10 10_25 0_25 25_50 50_75 75_90 90_100 75_100 {
    if "`bin'" == "0_10" local binname = "p0-p10"
    if "`bin'" == "10_25" local binname = "p11-p25"
    if "`bin'" == "25_50" local binname = "p26-p50"
    if "`bin'" == "50_75" local binname = "p51-p75"
    if "`bin'" == "75_90" local binname = "p76-p90"

    if "`bin'" == "90_100" local binname = "p90-p100"
    if "`bin'" == "0_25" local binname = "p0-p25"
    if "`bin'" == "75_100" local binname = "p76-p100"


    /* regress on likelihood of updating */
    reghdfe update_up_deaths strong if q_`bin'_deaths_pre == 1 , absorb(stratum)   vce(rob)

    local est = _b[strong]
    local se = _se[strong]
    file write fh "update_up_deaths,action,2," (`est') "," (`se') ",`binname',deaths" _n

    reghdfe update_down_deaths strong if q_`bin'_deaths_pre == 1 , absorb(stratum)   vce(rob)

    local est = _b[strong]
    local se = _se[strong]
    file write fh "update_down_deaths,action,2," (`est') "," (`se') ",`binname',deaths" _n

    /* regress on ln deaths */ 
    reghdfe ln_deaths_post strong ln_deaths_pre if q_`bin'_deaths_pre == 1 , absorb(stratum)   vce(rob)

    local est = _b[strong]
    local se = _se[strong]
    file write fh "ln_deaths_post,action,2," (`est') "," (`se') ",`binname',deaths" _n

    /* reghdferess on ln_wtp indices */ 
    foreach wtp in index { 
      reghdfe ln_wtp_`wtp' strong  if q_`bin'_deaths_pre == 1 , absorb(stratum)  vce(rob)
      local est = _b[strong]
      local se = _se[strong]
      file write fh "ln_wtp_index,action,2," (`est') "," (`se') ",`binname',deaths" _n

      reghdfe ln_wtp_`wtp' shifty  if q_`bin'_deaths_pre == 1 , absorb(stratum)  vce(rob)
      local est = _b[shifty]
      local se = _se[shifty]
      file write fh "ln_wtp_index,shifty,2," (`est') "," (`se') ",`binname',deaths" _n
  
    }  
}

file close fh

/**************/
/* load .csv  */
/**************/
import delimited using out/update_by_priors.csv, clear
keep if launch == 1 
gen high = est + 1.96 * se
gen low = est - 1.96 * se

drop if inlist(bin,"p0-p25","p76-p100")
sort outcome treatment bin
bys outcome treatment: gen n = 6-_n
global ylab 5 "p0-p10" 4  "p11-p25" 3  "p26-p50" 2 "p51-p75" 1 "p76-p90" 0 "p91-p100"



foreach outcome in ln_wtp_index cases_up cases_down deaths_up deaths_down death_rate_up death_rate_down gad_std  death_rate_post ln_deaths_post {

    foreach treatment in info action {
local infonm Info
local actionnm Action

   local outcome_ln_deaths_post = "total deaths (update, log points)"
  local outcome_death_rate_post = "death rate (update, deaths per 1,000)"
  local outcome_anxiety = "anxiety"
    local outcome_gad_std = "GAD-7 anxiety index"  
    local outcome_dict_fract = "percent donated"
   local outcome_ln_wtp_index = "WTP index (log points)"

  local text_med_ln_wtp_index = "H{sub:0} of p0-50 = p51-100"
  local text_25_ln_wtp_index = "H{sub:0} of p0-25 = p26-100"
  local text_2_med_ln_wtp_index = "{it:p}-value: ${ln_wtp_index_med_l1}"
    local text_2_25_ln_wtp_index = "{it:p}-value: ${ln_wtp_index_25_l1}"
  local loc_med_ln_wtp_index = "3.5 .5"
  local loc_25_ln_wtp_index = "2.5 .5"
  local loc_2_med_ln_wtp_index = "3.25 .5"
  local loc_2_25_ln_wtp_index = "2.25 .5"

  local text_med_gad_std = "H{sub:0}: p0-50 = p51-100"
  local text_25_gad_std = "H{sub:0}: p0-25 = p26-100"
  local text_2_med_gad_std = "{it:p}-value: ${gad_std_med_l1}"
    local text_2_25_gad_std = "{it:p}-value: ${gad_std_25_l1}"
  local loc_med_gad_std = "3.5 .3"
  local loc_25_gad_std = "2.5 .3"
  local loc_2_med_gad_std = "3.25 .3"
  local loc_2_25_gad_std = "2.25 .3"

local loc_avg_ln_wtp_index = "1.5 .5"
local text_avg_ln_wtp_index = "Average effect: $ln_wtp_index_avg, se: $ln_wtp_index_avg_se"

local loc_avg_gad_std = "1.25 .3"
local text_avg_gad_std = "Average effect: $gad_std_avg, se: $gad_std_avg_se"


if inlist("`outcome'","ln_wtp_index","gad_std") local text = `"  text(`loc_med_`outcome'' "`text_med_`outcome''", place(e) )  text(`loc_2_med_`outcome'' "`text_2_med_`outcome''", place(e) )    text(`loc_25_`outcome'' " `text_25_`outcome'' ", place(e))  text(`loc_2_25_`outcome'' "`text_2_25_`outcome''", place(e) ) note("`text_avg_`outcome''") "'
if !inlist("`outcome'","ln_wtp_index","gad_std") local text = ""

    twoway  /// 
        (bar est n if outcome == "`outcome'" & treatment == "`treatment'" & n > 2, ylab($ylab , labsize(large) ) horizontal ///
    color($ltblue) lcolor(white) xtitle("Effect of ``treatment'nm' on `outcome_`outcome''", size(large)) ) ///
        (bar est n if outcome == "`outcome'" & treatment == "`treatment'" & n <= 2, ylab($ylab , labsize(large) ) horizontal ///
        color($orange) lcolor(white) xtitle("Effect of ``treatment'nm' on `outcome_`outcome''", size(large)) ) ///    
      (rcap high low n if outcome == "`outcome'" & treatment == "`treatment'", ///
      legend(off) horizontal color(black) `text'   ) ///
      (function y = 0, ytitle("Percentile: prior beliefs, death rate" "{&larr} pessimistic | optimistic {&rarr}", size(large)) color(black) xlabel(,labsize(large)) lpattern(line) range(-0.5 5.5) horizontal ) 

    grout out/`treatment'_`outcome'_quantile , pdf

    } 
} 

/*********************/
/* /\* launch 2  *\/ */
/*********************/
import delimited using out/update_by_priors.csv, clear
keep if launch == 2
keep if treatment == "action" 
gen high = est + 1.96 * se
gen low = est - 1.96 * se

drop if inlist(bin,"p0-p25","p76-p100")
sort outcome treatment bin
bys outcome treatment: gen n = 6-_n
global ylab 5 "p0-p10" 4  "p11-p25" 3  "p26-p50" 2 "p51-p75" 1 "p76-p90" 0 "p91-p100"

foreach outcome in ln_deaths_post update_down_deaths update_up_deaths ln_wtp_index     {

  local outcome_ln_deaths_post = "total deaths (update, log points)"
  local outcome_death_rate_post = "death rate (update, deaths per 1,000)"

  local outcome_anxiety = "anxiety"
  local outcome_dict_fract = "percent donated"
  local outcome_wtp_index = "willingness to pay index (log points)"
  local outcome_update_up_deaths = "propensity to update beliefs {it:up}"
  local outcome_update_down_deaths = "propensity to update beliefs {it:down}"

  local outcome_ln_deaths_post = "net update about deaths (log points)"

  local text_med_update_up_deaths = "H{sub:0}: p0-50 = p51-100"
  local text_med_update_down_deaths = "H{sub:0}: p0-50 = p51-100"
  local text_med_ln_deaths_post = "H{sub:0}: p0-50 = p51-100"

  local text_25_update_up_deaths = "H{sub:0}: p0-25 = p26-100"
  local text_25_update_down_deaths = "H{sub:0}: p0-25 = p26-100"
  local text_25_ln_deaths_post = "H{sub:0}: p0-25 = p26-100"

    local text_2_med_update_up_deaths = "{it:p}-value: ${`outcome'_med}"
    local text_2_med_update_down_deaths = "{it:p}-value: ${`outcome'_med}"
    local text_2_med_ln_deaths_post = "{it:p}-value: ${`outcome'_med}"

    local text_2_25_update_up_deaths = "{it:p}-value: ${`outcome'_25}"
    local text_2_25_update_down_deaths = "{it:p}-value: ${`outcome'_25}"
    local text_2_25_ln_deaths_post = "{it:p}-value: ${`outcome'_25}"

  local loc_med_update_down_deaths = "5.5 0.01"
  local loc_med_update_up_deaths = "5.5 -.2"
  local loc_med_ln_deaths_post = "5.5 -.42"

  local loc_25_update_down_deaths = "4.5 0.01"
  local loc_25_update_up_deaths = "4.5 -.2"
  local loc_25_ln_deaths_post = "4.5 -.42"

  local loc_2_med_update_down_deaths = "5.25 0"
  local loc_2_med_update_up_deaths = "5.25 -.2"
  local loc_2_med_ln_deaths_post = "5.25 -.42"

  local loc_2_25_update_down_deaths = "4.25 0"
  local loc_2_25_update_up_deaths = "4.25 -.2"
  local loc_2_25_ln_deaths_post = "4.25 -.42"

  local loc_avg_update_down_deaths = "3.25 0"
  local loc_avg_update_up_deaths = "3.25 -.2"
  local loc_avg_ln_deaths_post = "3.25 -.42"

local text_avg_update_up_deaths = "Average effect: $update_up_deaths_avg, se: $update_up_deaths_avg_se " 
local text_avg_update_down_deaths = "Average effect: $update_down_deaths_avg, se: $update_down_deaths_avg_se "
local text_avg_ln_deaths_post = "Average effect: $ln_deaths_post_avg, se: $ln_deaths_post_avg_se " 

if inlist("`outcome'","update_up_deaths","update_down_deaths","ln_deaths_post") local text = `"  text(`loc_med_`outcome'' "`text_med_`outcome''", place(e) )  text(`loc_2_med_`outcome'' "`text_2_med_`outcome''", place(e) )    text(`loc_25_`outcome'' " `text_25_`outcome'' ", place(e))  text(`loc_2_25_`outcome'' "`text_2_25_`outcome''", place(e) ) note("`text_avg_`outcome''")  "'
if !inlist("`outcome'","update_up_deaths","update_down_deaths","ln_deaths_post") local text = "" 

    twoway  /// 
  (bar est n if outcome == "`outcome'" & treatment == "action" & n >= 3, ylab($ylab) horizontal ///
  color($ltblue) lcolor(white) xtitle("Effect of Strong Action on `outcome_`outcome''") ) ///
  (bar est n if outcome == "`outcome'" & treatment == "action" & n <= 2, ylab($ylab) horizontal ///
  color($orange) lcolor(white) xtitle("Effect of Strong Action on `outcome_`outcome''") ) ///  
  (rcap high low n if outcome == "`outcome'" & treatment == "action", ///
  legend(off) horizontal color(black) ytitle(" ")  `text' ) ///  
  (function y = 0, ytitle("Percentile: prior beliefs about number of deaths" "{&larr} pessimistic | optimistic {&rarr}" ) color(black) lpattern(line) range(-0.5 5.5) horizontal ) 

  grout out/action_`outcome'_quantile_public_data , pdf

} 


