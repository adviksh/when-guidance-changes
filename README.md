# when-guidance-changes
This repo contains replication code and data for "When Guidance Changes: Government Stances and Public Beliefs" by Charlie Rafkin, Advik Shreekumar, and Pierre-Luc Vautrey.

To replicate the analysis, clone the repo and execute `run_code.sh`.
```
git clone git@github.com:adviksh/when-guidance-changes.git
cd when-guidance-changes
./run_code.sh
```
The code may take up to a half hour to run. All tables, figures, and point estimates will populate in the `out/` directory. Log files will be saved in `stata/log/` and `R/log/`.

## Software Requirements
We performed most of the analysis using Stata 13. The following packages are required:
- unique
- reghdfe
- ritest
- estout
- ivreghdfe
- ftools
- ivreg2
- ranktest

We performed some remaining analyses in R version R 4.0.2, with scripts controlled by GNU Make version 3.81. The following R packages are required:
- tidyverse 1.3.0
- glue 1.4.2
- here 0.1
- estimatr 0.28.0
- broom 0.7.0
- ggthemes 4.2.0
- gtable 0.3.0
- systemfit 1.1-24
- fastDummies 1.6.1
- lmtest 0.9-38
- sandwich 2.5-1

## Data Description
This `data/` directory contains the following preprocessed, ready-for-analysis data files:
- `public_data.dta` is used for most of the analyses. It includes data from all respondents who started the survey during the study window (April 3-4, 2020), consent to study, passed at least one of two attention checks, and finished the survey.
- `public_data_3day.dta` is used for several robustness checks. It includes all the responses from `public_data.dta`, adding data from respondents who started the survey on April 5, 2020 and also meet the above screening criteria.
- `public_data.rds`is used to analyze the sample composition (Table 1) and attrition (Table D.1). It includes all respondents who started the survey during the study window (April 3-4, 2020) and consent to the study.
- `demographics_usa.csv` contains demographic data from the 2018 American Community Survey. It is used to analyze the sample composition.
