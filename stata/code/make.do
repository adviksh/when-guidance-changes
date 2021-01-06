/* build launches */
/* 
do code/build_launch.do
do code/build_public_data.do
*/

OLD 
stop 
/* show the effects of strong action treatment by launch */
do code/compare_launches.do

/* wtp graphs */
do code/wtp_graphs.do 

/*  look at updating for the severe info and action */
do code/info_severe.do 

/* show the effects by info heterogeneity */
do code/update_by_priors.do

/* graphs that look at updating for the action update in the second launch */
do code/action_update.do 

/* table for changing */
do code/info_steady.do
do code/info_steady_appendix.do

/* figures of belief updates */ 
do code/figure_shifty.do 

/* tables that show revealed preference outcomes */
do code/revealed_strong.do

/* table that shows economic preferences outcomes */
do code/prefs_both.do

/* robustness by statement */
do code/effect_by_statement.do 

/* formal table for information and action treatment */
do code/treatment_12_tables.do
do code/treatment_4_tables.do

/* numbers in the text */
do code/numbers_in_text.do


/**** slides only
do code/figure_revealed_strong.do
