set seed 50

/* Tables 2 and 4: Inconsistent effect on belief updates and gov credibility */
do code/changing.do

/* Figure 1: Inconsistent effects on the distribution */
do code/distrib_figure.do

/* Table 3: Inconsistent effects on the distribution */
do code/ratio.do

/* Appendix: Ratio, and Appendix: Heterogeneity by decile */
do code/deciles.do

/* Appendix: add/subtract controls for Table 2 */
do code/shifty_controls.do

/* Appendix: additional belief outcomes for changing */
do code/other_beliefs.do

/* Appendix: table of revealed preference outcomes for changing */
do code/revealed_changing.do

/* Appendix: heterogeneity by statement */
do code/heterogeneity_by_statement.do

/* Appendix: inverse hyperbolic sine */
do code/inv_sinh.do

/* Appendix: update magnitude without taking logarithms */
do code/absolute.do

/* Appendix: social distancing */
do code/social_distancing.do

/* Appendix: power figures */
do code/power.do

/* Data appendix tables */
do code/data_appendix.do

/* Appendix: heterogeneity by support for trump */
do code/heterogeneity_by_party.do

/* numbers in the text that autopopulate */
do code/numbers_in_text.do

/* small additional numbers to manually check */
do code/check_before_submit.do
