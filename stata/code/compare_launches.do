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



/****************************/
/* /\* append the data *\/  */
/****************************/
use "$int/launch_1_pol", replace 
gen launch = 1
tostring political_party , replace
tostring stratum , replace

append using  "$int/public_data" 
replace launch = 2 if launch == .

* standardize voting for trump
sum vote_trump_pre if strong_action == 0 & strong_info == 0 & launch == 1
replace vote_trump_post_std = (vote_trump_post - `r(mean)' ) / `r(sd)' if launch == 1
replace vote_trump_pre_std = (vote_trump_pre - `r(mean)' ) / `r(sd)' if launch == 1

/***************************************/
/* get a .csv of anxiety by each date  */
/***************************************/
cap file close fh 
file open fh using out/compare_launches.csv, write replace 
file write fh "outcome,launch,cut,group,est,se" _n 

/***** AGGREGATE EFFECT ON ANXIETY *****/
reghdfe anxiety_std strong_action strong_info if launch == 1 , absorb(group) vce(rob)
local est = _b[strong_action]
local se = _se[strong_action]

file write fh "anxiety,1,all,," (`est') "," (`se') _n

reghdfe anxiety_std strong if launch == 2 , absorb(stratum)  vce(rob)
local est = _b[strong]
local se = _se[strong]

file write fh "anxiety,2,all,," (`est') "," (`se') _n


  /***** government *******/ 
    reghdfe gov_index_post_std strong_action gov_index_pre_std if launch == 1 , absorb(group) vce(rob)
    local est = _b[strong_action]
    local se = _se[strong_action]

    file write fh "gov_index,1,,," (`est') "," (`se') _n
  
    reghdfe gov_index_post_std strong gov_index_pre_std if launch == 2 , absorb(stratum)  vce(rob)
    local est = _b[strong]
    local se = _se[strong]

    file write fh "gov_index,2,,," (`est') "," (`se') _n

  /***** government *******/ 
    reghdfe vote_trump_post_std strong_action vote_trump_pre_std if launch == 1 , absorb(group) vce(rob)
    local est = _b[strong_action]
    local se = _se[strong_action]

    file write fh "vote_trump,1,,," (`est') "," (`se') _n
  
    reghdfe vote_trump_post_std strong vote_trump_pre_std if launch == 2 , absorb(stratum)  vce(rob)
    local est = _b[strong]
    local se = _se[strong]

    file write fh "vote_trump,2,,," (`est') "," (`se') _n


/***** BY PARTY ********/ 
foreach party in 0 1 2 {

  /****** anxiety *****/
    reghdfe anxiety_std strong_action strong_info if launch == 1 & party == `party' , absorb(group) vce(rob)
    local est = _b[strong_action]
    local se = _se[strong_action]

    file write fh "anxiety,1,party,`party'," (`est') "," (`se') _n
  
    reghdfe anxiety_std strong if launch == 2  & party == `party' , absorb(stratum)  vce(rob)
    local est = _b[strong]
    local se = _se[strong]

    file write fh "anxiety,2,party,`party'," (`est') "," (`se') _n


  /***** government *******/ 
    reghdfe gov_index_post_std strong_action gov_index_pre_std if launch == 1 & party == `party' , absorb(group) vce(rob)
    local est = _b[strong_action]
    local se = _se[strong_action]

    file write fh "gov_index,1,party,`party'," (`est') "," (`se') _n
  
    reghdfe gov_index_post_std strong gov_index_pre_std if launch == 2  & party == `party' , absorb(stratum)   vce(rob)
    local est = _b[strong]
    local se = _se[strong]

    file write fh "gov_index,2,party,`party'," (`est') "," (`se') _n


  /***** voting for trump *******/ 
    reghdfe vote_trump_post strong_action vote_trump_pre_std if launch == 1 & party == `party' , absorb(group)  vce(rob)
    local est = _b[strong_action]
    local se = _se[strong_action]

    file write fh "vote_trump,1,party,`party'," (`est') "," (`se') _n
  
    reghdfe vote_trump_post strong vote_trump_pre_std if launch == 2  & party == `party' , absorb(stratum)   vce(rob)
    local est = _b[strong]
    local se = _se[strong]

    file write fh "vote_trump,2,party,`party'," (`est') "," (`se') _n  
}

/* DIFFERENCE IN PRIORS */
reghdfe ln_deaths_pre i.launch i.party b1.launch#b0.party , absorb(stratum)  vce(rob)
local rep_est = _b[2.launch#2.party]
local rep_se = _se[2.launch#2.party]

local ind_est = _b[2.launch#1.party]
local ind_se = _se[2.launch#1.party]


file write fh "ln_deaths_pre,,party,1," (`ind_est') "," (`ind_se') _n 
file write fh "ln_deaths_pre,,party,2," (`rep_est') "," (`rep_se') _n 

cap file close fh

/********************************/
/* /\* load and graph data *\/  */
/********************************/

/* RESULTS BY PARTY  */
import delimited using out/compare_launches.csv, clear

* anxiety 
preserve
    gen high = est + 1.96 * se
    gen low = est -  1.96 * se
    replace group = 3 if group == . 
    keep if outcome == "anxiety" 
    sort launch group 

    global ylab 0 "Oppose Trump" 1 "Undecided" 2 "Support Trump" 3 "All" 

    foreach launch in 1 2 { 
      twoway ///
          ( bar est group if launch == `launch' & group == 3, lcolor(white) horizontal ylab($ylab, labsize(large) ) color(gray) ) ///
          ( bar est group if launch == `launch' & group == 0, lcolor(white) horizontal ylab($ylab, labsize(large) ) color($ltblue) ) ///
          ( bar est group if launch == `launch' & group == 1, lcolor(white) horizontal ylab($ylab , labsize(large) ) color(lavender) ) ///
          ( bar est group if launch == `launch' & group == 2, lcolor(white) horizontal ylab($ylab , labsize(large) ) color($red) ) ///            
            (rcap high low group if launch == `launch', horizontal color(black) ///
            ytitle("") xtitle("Treatment effect on anxiety (sd)", size(large)) ) ///
              (function y = 0, horizontal color(black) range(-0.5 3.5) xlab(,labsize(large)) lpattern(line) legend(off) )
          grout out/anxiety_party_launch_`launch' , pdf  
        }
    restore

* faith in gov 
preserve
    gen high = est + 1.96 * se
    gen low = est -  1.96 * se
    replace group = 3 if group == . 
    keep if outcome == "gov_index" 
    sort launch group 

    global ylab 0 "Oppose Trump" 1 "Undecided" 2 "Support Trump" 3 "All" 

    foreach launch in 1 2 { 
      twoway ///
          ( bar est group if launch == `launch' & group == 3, lcolor(white) horizontal ylab($ylab, labsize(large) ) color(gray) ) ///
          ( bar est group if launch == `launch' & group == 0, lcolor(white) horizontal ylab($ylab, labsize(large) ) color($ltblue) ) ///
          ( bar est group if launch == `launch' & group == 1, lcolor(white) horizontal ylab($ylab , labsize(large) ) color(lavender) ) ///
          ( bar est group if launch == `launch' & group == 2, lcolor(white) horizontal ylab($ylab , labsize(large) ) color($red) ) ///            
            (rcap high low group if launch == `launch', horizontal color(black) ///
            ytitle("") xtitle("Treatment effect on government approval (sd)", size(large)) ) ///
              (function y = 0, xlab(-0.1(0.05)0.15) horizontal color(black) range(-0.5 3.5) xlab(,labsize(large)) lpattern(line) legend(off) )
          grout out/gov_approval_launch_`launch' , pdf  
        }
    restore

* trump support 
preserve
    gen high = est + 1.96 * se
    gen low = est -  1.96 * se
    replace group = 3 if group == . 
    keep if outcome == "vote_trump" 
    sort launch group 

    global ylab 0 "Oppose Trump" 1 "Undecided" 2 "Support Trump" 3 "All" 

    foreach launch in 1 2 { 
      twoway ///
          ( bar est group if launch == `launch' & group == 3, lcolor(white) horizontal ylab($ylab, labsize(large) ) color(gray) ) ///
          ( bar est group if launch == `launch' & group == 0, lcolor(white) horizontal ylab($ylab, labsize(large) ) color($ltblue) ) ///
          ( bar est group if launch == `launch' & group == 1, lcolor(white) horizontal ylab($ylab , labsize(large) ) color(lavender) ) ///
          ( bar est group if launch == `launch' & group == 2, lcolor(white) horizontal ylab($ylab , labsize(large) ) color($red) ) ///            
            (rcap high low group if launch == `launch', horizontal color(black) ///
            ytitle("") xtitle("Treatment effect on Trump support (sd)", size(large)) ) ///
              (function y = 0, xlab(-0.2(0.1)0.3) horizontal color(black) range(-0.5 3.5) xlab(,labsize(large)) lpattern(line) legend(off) )
          grout out/vote_trump_launch_`launch' , pdf  
        }
    restore

/*********************************/
/* /\* DIFFERENCE IN PRIORS  *\/ */
/*********************************/
preserve
    gen high = est + 1.96 * se
    gen low = est -  1.96 * se

    keep if cut == "party" & outcome == "ln_deaths_pre"
    sort launch group
    global ylab 1 "Undecided" 2 "Support Trump" 

    twoway ///
    ( bar est group  if group == 1, ///
    lcolor(white) horizontal color(lavender) ) ///
    ( bar est group  if group == 2, ///
    lcolor(white) horizontal ylab($ylab , labsize(large))  ///
    color($red) xlab(,labsize(large)) ) ///    
    (rcap high low group , horizontal color(black) ///
    xtitle("Difference in priors about number of deaths (log points), elicited pre-treatment" "Relative to Oppose Trump", ///
    size(large) ) ) ///
    (function y = 0, horizontal color(black) range(0.5 2.5) lpattern(line) legend(off) xsize(10) )

    grout out/beliefs_launch , pdf  
restore 





