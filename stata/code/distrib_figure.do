/****************/
/* Distributions */
/****************/
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

use "$int/public_data" , clear
keep if shifty != .

/***************************************************************/
/* Panel A: alternate, distribution of priors for whole sample */
/***************************************************************/
/**** text: median and mean of consistent ****/ 
qreg deaths_pre if shifty == 0
local control_median: di %7.0fc _b[_cons]
local control_median_se: di %5.0fc round(_se[_cons],10)

reg deaths_pre if shifty == 0
local control_mean: di %7.0fc round(_b[_cons],1000)
local control_mean_se: di %7.0fc round(_se[_cons],1000)

/**** text: median and mean of inconsistent ****/ 
qreg deaths_pre if shifty == 1
local treat_median: di %7.0fc round(_b[_cons],1000)
local treat_median_se: di %5.0fc round(_se[_cons],10)

reg deaths_pre if shifty == 1
local treat_mean: di %7.0fc round(_b[_cons],1000)
local treat_mean_se: di %7.0fc round(_se[_cons],1000)

/* fraction in each part of the distribution */ 
sum priors_below_info
local fraction_opt: di %3.0f 100*`r(mean)'
sum priors_at_info
local fraction_at: di %3.0f 100*`r(mean)'
sum priors_above_info
local fraction_pess: di %3.0f 100*`r(mean)'

/* winsorize logs for the picture */ 
gen ln_priors_wins = max(min(log10(deaths_pre),7.001),2)
replace ln_priors_wins = 2 if deaths_pre == 0  // put the 2 people w 0 deaths in the lowest bin
sort deaths_pre 
local bottom_projection = 100000 
local top_projection = 240000 
gen low = 0 if inrange(deaths_pre,`bottom_projection' ,`top_projection')
gen high = 0.34 if inrange(deaths_pre, `bottom_projection' ,`top_projection')

twoway  ///
    (rarea high low ln_priors_wins, legend(off) lcolor(gs9) fcolor(gs8) ) ///    
    ( histogram ln_priors_wins, lcolor(black) fraction start(2) width(.25) ///
    fcolor(lavender )  xtitle("Prior beliefs about deaths in 6 months, entire sample (log{sub:10} scale)") ///
    ylab(0(0.05)0.3) xlab(2 "{&le} 100" 3 "1,000" 4 "10,000" 5 "100,000" 6 "1 million" 7 "{&ge} 10 million" , format(%09.1fc)  angle(45))  ///
    text(.32 1.8 "Below 100,000: `fraction_opt'%", place(e) ) ///
    text(.30 1.81 "At 100,000-240,000: `fraction_at'%", place(e)) ///
    text(.28 1.8 "Above 240,000: `fraction_pess'%", place(e)) ///
    text(.24 1.7 "{bf:Median:}", place(e)) ///    
    text(.22 2.0 "Inconsistent: `treat_median' (SE: `treat_median_se')", place(e)) ///
    text(.20 2.0 "Consistent: `control_median' (SE: `control_median_se')", place(e) ) ///
    text(.16 1.7 "{bf:Mean:}", place(e)) ///    
  text(.14 2.0 "Inconsistent: `treat_mean' (SE: `treat_mean_se')", place(e))     ///
    text(.12 2.0 "Consistent: `control_mean' (SE: `control_mean_se')", place(e)) ///
    text(.25 5.7 "Government projection:", place(e) color(gs8) ) ///
    text(.23 5.7 "100,000-240,000", place(e) color(gs8) ) ) ///
    (pcarrowi .26 5.65 .27 5.42 , xsc(titlegap(*-18))   ytitle("Fraction") color(gs8) ) 
   
grout out/ln_priors_hist , pdf

/**********************/
/* Panel B: Distribution of Log Changes */
/**********************/
/* winsorize beliefs for picture */
gen update_mag_deaths_wins = min(update_mag_deaths, 15.001) 


/* get fraction who update */ 
reg update_covid if shifty == 0
local control: di %5.3f _b[_cons] 
local control_se: di %5.3f _se[_cons] 

reg update_covid if shifty == 1
local treat: di %5.3f _b[_cons] 
local treat_se: di %5.3f _se[_cons] 

/* get difference in means */ 
reg update_covid shifty, robust
local diff_frac: di %5.3f _b[shifty]
local diff_frac_se: di %5.3f _se[shifty]


/* get update magnitude */ 
reg update_mag_deaths if shifty == 0
local control_mean: di %5.2fc _b[_cons]
local control_mean_se : di %4.3fc _se[_cons]

reg update_mag_deaths if shifty == 1
local treat_mean: di %5.2fc _b[_cons]
local treat_mean_se : di %4.3fc _se[_cons]

/* get difference in means */ 
reg update_mag_deaths shifty, rob
local diff_mag: di %5.3f _b[shifty]
local diff_mag_se: di %4.3f _se[shifty]

/**** plot distribution of changes ****/ 
twoway  ///
    ( histogram update_mag_deaths_wins if shifty == 0, ///
    legend(lab(1 "Consistent")) fraction start(0) ///
    width(1) lcolor($ltblue ) fcolor($ltblue )  ) /// 
    ( histogram update_mag_deaths_wins if shifty == 1 , legend(order(2 1 ) lab(2 "Inconsistent") ///
    col(2) ) xtitle("Update magnitude: ln(|posterior - prior| + 1)") ///
    fraction start(0) width(1) lcolor(black) fcolor(none) ///
    ylab(0(0.1)0.6) xlab(0(1)15  15 "{&ge}15" , format(%09.0fc) )  ///
    text(.6 1.3 "{bf:Fraction choosing to update (posterior {&ne} prior):}", place(e)) ///
    text(.55 3 "Inconsistent: `treat' (SE:  `treat_se')", place(e) ) ///    
    text(.5 3 "Consistent: `control' (SE: `control_se')", place(e) ) ///
    text(.45 2.75 "{it:Difference}: `diff_frac' (SE: `diff_frac_se')", place(e) ) ///
    text(.35 1.5 "{bf:Mean update magnitude:}", place(e) ) ///
    text(.3 3 "Inconsistent: `treat_mean' (SE: `treat_mean_se')", place(e) ) ///        
    text(.25 3 "Consistent: `control_mean' (SE: `control_mean_se')", place(e) ) ///
    text(.2 2.75 "{it:Difference}: `diff_mag' (SE: `diff_mag_se')", place(e) ) ///        
    )

grout out/distrib_update_mag_deaths , pdf
 
