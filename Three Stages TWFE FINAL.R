rm(list = ls())

#### Final TWFE Model Code #### ensure ptf has all previous variables included ####

library(tidyverse)
library(dplyr)
library(fixest)

#### load data in and clean ####
load("ptf_cleaned_2307_FINAL_FINAL.RData")


### zero out frequencies which are negative ###
ptf_cleaned_2307 <- ptf_cleaned_2307 %>%
  mutate(
    bus_tph   = pmax(bus_tph, 0),
    train_tph = pmax(train_tph, 0)
  )

## check if worked, should be no negative figures ##
summary(ptf_cleaned_2307[c("bus_tph", "train_tph", "overall_tph")])

### NA train data for 2014-2017 as 97 percent of LSOAs have no coverage ###
##retain bus data for those years##
ptf_cleaned_2307_na_zero <- ptf_cleaned_2307 %>%
  mutate(
    train_tph   = if_else(year %in% 2014:2017, NA_real_, train_tph),
    overall_tph = if_else(year %in% 2014:2017, bus_tph, bus_tph + train_tph)
  )

## check if worked should have TRUE for 2014 to 2017 for train_tph, but
## overall_tph should now show FALSE (not missing) throughout ##
table(ptf_cleaned_2307_na_zero$year, is.na(ptf_cleaned_2307_na_zero$train_tph))
table(ptf_cleaned_2307_na_zero$year, is.na(ptf_cleaned_2307_na_zero$overall_tph))
ptf_cleaned_2307 <- ptf_cleaned_2307_na_zero # add NA to the final clean dataset, this is used throughout workflow

#### Building time period datasets ####
##AVERAGE WEEKDAY, AVERAGE WEEKEND, WEEKDAY PEAK## 

ptf_weekday_avg <- ptf_cleaned_2307 %>% ## making weekday average tph; filter to only weekday lsoas then calc average for each ##
  filter(day_type == "weekday") %>%
  group_by(lsoa_id, year) %>%
  summarise(
    cars_per_1000 = first(cars_per_1000),
    bus_tph = mean(bus_tph, na.rm = TRUE),## take means of each mode tph ##
    train_tph = mean(train_tph, na.rm = TRUE),
    overall_tph = mean(overall_tph, na.rm = TRUE),
    pop_density = first(pop_density), # creating one row of pop dens, income and median per LSOA year. 
    income = first(income), ## there is only one pop dens per year so that should be applied to each LSOA for the year##
    median_age = first(median_age), 
    .groups = "drop"
  ) %>%
  mutate(across(c(train_tph, overall_tph), ~ na_if(., NaN))) %>% # if NaNs produced, turn into NA value #
  mutate(log_density = log(pop_density + 1)) %>% ## make population density log density so it can better deal with outlier values ##
  drop_na(cars_per_1000, bus_tph, pop_density, income, median_age)

#### use same code for other variables, change name and filter names to fit ####

ptf_weekend_avg <- ptf_cleaned_2307 %>%  ## calculating weekend average tph ## for robustness checks after main analysis #
  filter(day_type == "saturday" | day_type == "sunday") %>%
  group_by(lsoa_id, year) %>%
  summarise(
    cars_per_1000 = first(cars_per_1000),
    bus_tph = mean(bus_tph, na.rm = TRUE),
    train_tph = mean(train_tph, na.rm = TRUE),
    overall_tph = mean(overall_tph, na.rm = TRUE),
    pop_density = first(pop_density), 
    income = first(income), 
    median_age = first(median_age), 
    .groups = "drop"
  ) %>%
  mutate(across(c(train_tph, overall_tph), ~ na_if(., NaN))) %>% # if NaNs produced, turn into NA value #
  mutate(log_density = log(pop_density + 1)) %>% ## make population density log density so it can better deal with outlier values ##
  drop_na(cars_per_1000, bus_tph, pop_density, income, median_age)

ptf_peak_time <- ptf_cleaned_2307 %>%  ## calculating peak time average tph ## also for robustness #
  filter(day_type == "weekday", is_peak == TRUE) %>%
  group_by(lsoa_id, year) %>%
  summarise(
    cars_per_1000 = first(cars_per_1000),
    bus_tph = mean(bus_tph, na.rm = TRUE),
    train_tph = mean(train_tph, na.rm = TRUE),
    overall_tph = mean(overall_tph, na.rm = TRUE),
    pop_density = first(pop_density), 
    income = first(income), 
    median_age = first(median_age), 
    .groups = "drop"
  ) %>%
  mutate(across(c(train_tph, overall_tph), ~ na_if(., NaN))) %>% 
  mutate(log_density = log(pop_density + 1)) %>% 
  drop_na(cars_per_1000, bus_tph, pop_density, income, median_age)

## rescaling income to thousands of pounds, this improves interpretation of coefficients ##

ptf_weekday_avg <- ptf_weekday_avg %>%
  mutate(income_1000s = income / 1000)

ptf_weekend_avg <- ptf_weekend_avg %>%
  mutate(income_1000s = income / 1000)

ptf_peak_time <- ptf_peak_time %>%
  mutate(income_1000s = income / 1000)

#### Mean Centering the controls ### Do this to imporve interpretation, density or age can never be zero ###
ptf_weekday_avg <- ptf_weekday_avg %>%
  mutate(
    log_density_c = log_density - mean(log_density, na.rm = TRUE),
    income_1000s_c = income_1000s - mean(income_1000s, na.rm = TRUE),
    median_age_c = median_age - mean(median_age, na.rm = TRUE)
  )

#### Running the TWFE Models with feols ####
baseline_overall <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year, # don't wprry about removed NA values #
  data = ptf_weekday_avg, vcov = ~lsoa_id  ## use vcov to cluster standard errors, "cluster" also works but vcov within fixest ##
)
baseline_bus <- feols(
  cars_per_1000 ~ bus_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id
)
baseline_rail <- feols(
  cars_per_1000 ~ train_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id  
)

## Check number of observations used in each ##
cat("Baseline overall N:", nobs(baseline_overall), "\n")
cat("Baseline bus N:", nobs(baseline_bus), "\n")
cat("Baseline rail N:", nobs(baseline_rail), "\n")

## checking the model has worked and is in the expected class, should be "fixest" ##
class(baseline_overall)
class(baseline_bus)
class(baseline_rail)

## check convergence, fixest has built in checker, should be close to zero ##

baseline_conv_check_overall <- check_conv_feols(baseline_overall)
baseline_conv_check_bus <- check_conv_feols(baseline_bus)
baseline_conv_check_rail <- check_conv_feols(baseline_rail)

summary(baseline_conv_check_overall) ## all of these close to zero
summary(baseline_conv_check_bus)
summary(baseline_conv_check_rail)

## Output Table of results ##
etable(baseline_overall, baseline_bus, baseline_rail,
       headers = c("Overall", "Bus", "Rail"))

#### Stage 2: Interaction Models ####
interaction_overall <- feols(
  cars_per_1000 ~ overall_tph * (log_density_c + income_1000s_c + median_age_c) | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id ## use the mean centered controls ## 
)
interaction_bus <- feols(
  cars_per_1000 ~ bus_tph * (log_density_c + income_1000s_c + median_age_c) | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id # don't worry if 133 308 observations are removed, that is the missing train data #
)
interaction_rail <- feols(
  cars_per_1000 ~ train_tph * (log_density_c + income_1000s_c + median_age_c) | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id
)

## checking the model has worked and is in the expected class, should be "fixest" ##
class(interaction_overall)
class(interaction_bus)
class(interaction_rail)

## Checking the convergence of the model, should be very close to zero ##

summary(check_conv_feols(interaction_overall))
summary(check_conv_feols(interaction_bus))
summary(check_conv_feols(interaction_rail))

# Check number of observations used in each #
cat("Baseline overall N:", nobs(baseline_overall), "\n") ## NAs should not be included in baseline and rail ## Bus is alright ##
cat("Baseline bus N:", nobs(baseline_bus), "\n")
cat("Baseline rail N:", nobs(baseline_rail), "\n")


## Output results in table ##
etable(interaction_overall, interaction_bus, interaction_rail, 
       headers = c("Overall", "Bus", "Rail")) 

#### Stage 3: Quartile split analysis #### 
## Build our quartile groupings ## Calculating average weekday frequency across the whole dataset for each LSOA ##
quartile_avg_tph <- ptf_weekday_avg %>%
  group_by(lsoa_id) %>% ## group by LSOA and not year we want time invariant averages ##
  summarise(overall_avg = mean(overall_tph, na.rm = TRUE), .groups = "drop") %>% ## mean tph for each year per LSOA ##
  mutate(overall_quartile = ntile(overall_avg, 4)) ## ntile from dpylr; splits frequency data into 4 ranked groups ## 

ptf_weekday_avg <- ptf_weekday_avg %>%
  left_join(quartile_avg_tph %>% select(lsoa_id, overall_quartile), by = "lsoa_id") ## join calculated quartile rankings to main weekday dataset ##

## running feols on each frequency quartile ## 
first_quartile <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg %>% filter(overall_quartile == 1),
  vcov = ~lsoa_id
) # don't worry if 133 308 observations are removed, that is the missing train data #

second_quartile <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg %>% filter(overall_quartile == 2),
  vcov = ~lsoa_id
)

third_quartile <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg %>% filter(overall_quartile == 3),
  vcov = ~lsoa_id
)

fourth_quartile <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg %>% filter(overall_quartile == 4),
  vcov = ~lsoa_id
)

## checking class should be fixest ##
class(first_quartile)
class(second_quartile)
class(third_quartile)
class(fourth_quartile)

## checking convergence, close to zero ##

summary(check_conv_feols(first_quartile))
summary(check_conv_feols(second_quartile))
summary(check_conv_feols(third_quartile))
summary(check_conv_feols(fourth_quartile))

### viewing results ### simple main table ###
etable(first_quartile, second_quartile, third_quartile, fourth_quartile,
       headers = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))


quartile_coefs <- bind_rows(
  tibble(quartile = 1, estimate = summary(first_quartile)$coeftable["overall_tph", "Estimate"],
         se = summary(first_quartile)$coeftable["overall_tph", "Std. Error"]),
  tibble(quartile = 2, estimate = summary(second_quartile)$coeftable["overall_tph", "Estimate"],
         se = summary(second_quartile)$coeftable["overall_tph", "Std. Error"]),
  tibble(quartile = 3, estimate = summary(third_quartile)$coeftable["overall_tph", "Estimate"],
         se = summary(third_quartile)$coeftable["overall_tph", "Std. Error"]),
  tibble(quartile = 4, estimate = summary(fourth_quartile)$coeftable["overall_tph", "Estimate"],
         se = summary(fourth_quartile)$coeftable["overall_tph", "Std. Error"])
)

print(quartile_coefs)

#### detailed tables for each quartile ####
extract_full_coeftable <- function(model, quartile_label) {
  ct <- as.data.frame(summary(model)$coeftable)
  ct$variable <- rownames(ct)
  ct$quartile <- quartile_label
  rownames(ct) <- NULL
  ct
}

quartile_full_details <- bind_rows(
  extract_full_coeftable(first_quartile, "Q1 (lowest)"),
  extract_full_coeftable(second_quartile, "Q2"),
  extract_full_coeftable(third_quartile, "Q3"),
  extract_full_coeftable(fourth_quartile, "Q4 (highest)")
)

# reorder columns so quartile and variable appear first
quartile_full_details <- quartile_full_details %>%
  select(quartile, variable, everything())

print(quartile_full_details)

#### Applying feols to different time periods for robustness check ####
## weekend and peak times (evening and morning in the week) ##
## baseline tph ##
baseline_weekend <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekend_avg, vcov = ~lsoa_id
)

baseline_peak <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_peak_time, vcov = ~lsoa_id
)

## bus tph ## 
bus_weekend <- feols(
  cars_per_1000 ~ bus_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekend_avg, vcov = ~lsoa_id
)

bus_peak <- feols(
  cars_per_1000 ~ bus_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_peak_time, vcov = ~lsoa_id
)
## rail tph ##
rail_weekend <- feols(
  cars_per_1000 ~ train_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekend_avg, vcov = ~lsoa_id
)

rail_peak <- feols(
  cars_per_1000 ~ train_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_peak_time, vcov = ~lsoa_id
)


### making comparison tables ###
etable(baseline_overall, baseline_weekend, baseline_peak,
       headers = c("Weekday average", "Weekend average", "Weekday peak"))

## estimating coefficients for each time period studied ##
robustness_summary <- tibble(
  time_period = rep(c("Weekday (primary)", "Weekend", "Weekday peak"), 3),
  frequency_type = rep(c("Overall", "Bus", "Rail"), each = 3),
  estimate = c(
    coef(baseline_overall)["overall_tph"], coef(baseline_weekend)["overall_tph"], coef(baseline_peak)["overall_tph"],
    coef(baseline_bus)["bus_tph"], coef(bus_weekend)["bus_tph"], coef(bus_peak)["bus_tph"],
    coef(baseline_rail)["train_tph"], coef(rail_weekend)["train_tph"], coef(rail_peak)["train_tph"]
  ),
  se = c(
    se(baseline_overall)["overall_tph"], se(baseline_weekend)["overall_tph"], se(baseline_peak)["overall_tph"],
    se(baseline_bus)["bus_tph"], se(bus_weekend)["bus_tph"], se(bus_peak)["bus_tph"],
    se(baseline_rail)["train_tph"], se(rail_weekend)["train_tph"], se(rail_peak)["train_tph"]
  )
)

print(robustness_summary)

