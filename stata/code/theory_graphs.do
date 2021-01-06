/**************************************/
/* /\**** graphs of the theory ****\/ */
/**************************************/
clear

/* study the effect on preferences of all treatments */
if "`c(username)'" == "pierre-lucvautrey" {
  do ~/documents/github/covid/SetGlobals.do
}
else{
  do ../SetGlobals.do
}
clear 
version 13

/*********************************/
/* 1. graph of steady posterior  */
/*********************************/
global alpha = 0.75
global q_0 = 0.2

twoway /// 
    ( function y = ($alpha * $q_0 * x)/($alpha * $q_0 + .5 *(1-${q_0})) + ///
    (1-${alpha})*${q_0} * (1-x)/ ((1-${alpha})*${q_0} + .5 * (1-${q_0})), color(gray) range(0 1) ) /// 
( function y = (1-${alpha}) * ${q_0} * (1-x) / ((1-${alpha})* ${q_0} + 0.25 * (1-${q_0})),  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Steady government signal") lab(2 "Changing government signal") ) xtitle("Prior belief about a crisis ({&mu}{sub:0})") ///
    ytitle("Posterior belief about accuracy of govt signal") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/steady_shifty_posterior_low_precision


global alpha = 0.75
global q_0 = 0.7

twoway /// 
    ( function y = ($alpha * $q_0 * x)/($alpha * $q_0 + .5 *(1-${q_0})) + ///
    (1-${alpha})*${q_0} * (1-x)/ ((1-${alpha})*${q_0} + .5 * (1-${q_0})), color(gray) range(0 1) ) /// 
( function y = (1-${alpha}) * ${q_0} * (1-x) / ((1-${alpha})* ${q_0} + 0.25 * (1-${q_0})),  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Steady government signal") lab(2 "Changing government signal") ) xtitle("Prior belief about a crisis ({&mu}{sub:0})") ///
    ytitle("Posterior belief about accuracy of govt signal") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/steady_shifty_posterior_high_precision


    
    

/******************************************************/
/* 2. graph of strong and weak info effect on \hat{\mu}_1    */
/******************************************************/

global q_0 = 0.2

twoway /// 
    ( function y = (1-${q_0}) / ((1 - x * ${q_0})) * x, color(gray) range(0 1) ) /// 
( function y = (1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x,  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Weak action") lab(2 "Strong action") ) xtitle("Prior belief about a crisis ({&mu}{sub:1})") ///
    ytitle("Posterior belief about a crisis at time 1 HAT XXX  ({&mu}{sub:1})") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/strong_weak_info_effect_low_precision


global q_0 = 0.2

twoway /// 
    ( function y = (1-${q_0}) / ((1 - x * ${q_0})) * x, color(gray) range(0 1) ) /// 
( function y = (1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x,  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Weak action") lab(2 "Strong action") ) xtitle("Prior belief about a crisis ({&mu}{sub:1})") ///
    ytitle("Posterior belief about a crisis at time 1 HAT XXX ({&mu}{sub:1})") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/strong_weak_info_effect_high_precision


/******************************************************/
/* 3. graph of strong and weak action effect on \hat{\mu}_2    */
/******************************************************/

global f11 = 0.5
global f10 = 1
global f00 = 0.5
global f01 = 1

global alpha = 0.75

twoway /// 
    ( function y = ${alpha} * x * ${f10} + (1 - ${alpha})*(1-x)*${f00}, color(gray) range(0 1) ) /// 
( function y = ${alpha} * x * ${f11} + (1 - ${alpha})*(1-x)*${f01},  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Weak action") lab(2 "Strong action") ) xtitle("Posterior belief about a crisis HAT XXX ({&mu}{sub:1})") ///
    ytitle("Posterior belief about a crisis at time 2 HAT XXX ({&mu}{sub:2})") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/strong_weak_action_effect

/******************************************************/
/* 4. graph of strong and weak info+action effect on \hat{\mu}_2    */
/******************************************************/

global f11 = 0.1
global f10 = 1
global f00 = 0.1
global f01 = 1

global alpha = 0.75
global q_0 = 0.9


twoway /// 
    ( function y = ${alpha} * ((1-${q_0}) / ((1 - x * ${q_0})) * x) * ${f10} + ///
	 (1 - ${alpha})*(1-((1-${q_0}) / ((1 - x * ${q_0})) * x))*${f00}, color(gray) range(0 1) ) /// 
( function y = ${alpha} * ((1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x) * ${f11} + ///
 (1 - ${alpha})*(1-((1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x))*${f01},  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Weak action") lab(2 "Strong action") ) xtitle("Prior belief about a crisis ({&mu}{sub:1})") ///
    ytitle("Posterior belief about a crisis at time 2 HAT XXX ({&mu}{sub:2})") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/strong_weak_total_effect_republican // extreme trump supporter believes trump is right (q_0 high) and Trump actions if appropriate always work (fii very small)

global f11 = 0.8
global f10 = 1
global f00 = 0.8
global f01 = 1

global alpha = 0.75
global q_0 = 0.2


twoway /// 
    ( function y = ${alpha} * ((1-${q_0}) / ((1 - x * ${q_0})) * x) * ${f10} + ///
	 (1 - ${alpha})*(1-((1-${q_0}) / ((1 - x * ${q_0})) * x))*${f00}, color(gray) range(0 1) ) /// 
( function y = ${alpha} * ((1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x) * ${f11} + ///
 (1 - ${alpha})*(1-((1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x))*${f01},  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Weak action") lab(2 "Strong action") ) xtitle("Prior belief about a crisis ({&mu}{sub:1})") ///
    ytitle("Posterior belief about a crisis at time 2 HAT XXX ({&mu}{sub:2})") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/strong_weak_total_effect_democrat // extreme democrat believes trump is noise (q_0 low) and that trump actions wont do much good (fii closer to 1)


global f11 = 0.5
global f10 = 1
global f00 = 0.5
global f01 = 1

global alpha = 0.7
global q_0 = 0.7


twoway /// 
    ( function y = ${alpha} * ((1-${q_0}) / ((1 - x * ${q_0})) * x) * ${f10} + ///
	 (1 - ${alpha})*(1-((1-${q_0}) / ((1 - x * ${q_0})) * x))*${f00}, color(gray) range(0 1) ) /// 
( function y = ${alpha} * ((1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x) * ${f11} + ///
 (1 - ${alpha})*(1-((1 + ${q_0}) / ( 1 + 2 * x * ${q_0} - ${q_0}) * x))*${f01},  ///
    lcolor(black) range(0 1) lpattern(dash) legend(col(2) lab(1 "Weak action") lab(2 "Strong action") ) xtitle("Prior belief about a crisis ({&mu}{sub:1})") ///
    ytitle("Posterior belief about a crisis at time 2 HAT XXX ({&mu}{sub:2})") ylab(0(.2)1) xlab(0(0.2)1)  )

grout out/strong_weak_total_effect_independent // independent believes signals are relatively informative q_0 = 0.7, and policy matters but does not solve everything fii = 0.5

