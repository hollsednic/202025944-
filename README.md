# EN-Diss-Repository
This repository contains R code and data pipeline for a two-way fixed effects analysis of public transport frequency and car ownership across England and Wales, 2004–2023. The analysis tests whether increases in public transport frequency (trips per hour) are associated with reductions in car ownership at the small-area (LSOA) level, and whether this relationship varies with local income, population density, and median age. The code covers the full pipeline, from raw data cleaning and construction through to the final statistical models and results figures used in the dissertation.

Raw source data is **Not** included in this repository due to file size. Access to data can be found in the appendix of the dissertation in the Data Table (Appendix B).
The repository contains the following code:
/data-construction/
  01_ptf_frequency_constructor.R   — builds the base PT frequency panel
  02_car_count_constructor.R       — builds and merges car ownership data
  03_income_constructor.R          — builds household income variable
  04_age_constructor.R             — builds median age variable
  05_density_constructor.R         — builds population density variable
/modelling/
  06_twfe_main_analysis.R          — Stage 1-3 models + robustness checks
  07_twfe_morgan_robustness.R      — 2004-2018 Morgan-only robustness check
/figures/
  08_coefficient_plots.R           — produces all dissertation figures
README.md

RUNNING ORDER
It is important that the data pipeline is run in the correct order, as several constructors rely on others having been made in an R session and being saved to the environment. The two-way fixed effects model also cannot be successfully run without completed variable constructor codes.
The code should be run the following order.
1.Run 01_ptf_frequency_constructor.R
2. Run 02_car_count_constructor.R, 03_income_constructor.R, 04_age_constructor.R, 05_density_constructor.R in any order — each attaches one variable to ptf_cleaned_2307.
3. Run 06_twfe_main_analysis.R - main analysis, requires above variables to run correctly
4. 07_twfe_morgan_robustness.R and 08_coefficient_plots.R can be run afterwards. ****08 requires 06 to have been run in the same session****

This code uses the following packages, "install.packages" be necessary to install some of these:
tidyverse
data.table
fixest
gridExtra
zoo

R Version 2026.08.1 was used. 

Notes:
Train data availability is very low between 2014 and 2017, this has been zeroed in the code for the TWFE analysis for consistency.
Some LSOAs show negative public transport frequency, these are zeroed in the code
