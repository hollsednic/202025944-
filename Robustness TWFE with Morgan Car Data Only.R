#### Final TWFE Model Code — 2004-2018 Morgan-Extended Robustness Check ####
library(tidyverse)
library(dplyr)
library(fixest)

## before running ensure that "ptf_cleaned_2307_0218_to18.RData" has been created with "Robustness Check Car Count Maker" code

#### load data in and clean ####
rm(list = ls())
load("ptf_cleaned_2307_0218_to18.RData")  # adjust filename/extension if saved differently

### zero out frequencies which are negative ###
ptf0218 <- ptf0218 %>%
  mutate(
    bus_tph   = pmax(bus_tph, 0),
    train_tph = pmax(train_tph, 0)
  )

## check if worked, should be no negative figures ##
summary(ptf0218[c("bus_tph", "train_tph", "overall_tph")])

### NA train data for 2014-2017 as 97 percent of LSOAs have no coverage ###
##retain bus data for those years##
ptf0218_na_zero <- ptf0218 %>%
  mutate(
    train_tph   = if_else(year %in% 2014:2017, NA_real_, train_tph),
    overall_tph = if_else(year %in% 2014:2017, bus_tph, bus_tph + train_tph)
  )

table(ptf0218_na_zero$year, is.na(ptf0218_na_zero$train_tph))
table(ptf0218_na_zero$year, is.na(ptf0218_na_zero$overall_tph))
ptf0218 <- ptf0218_na_zero

#### Building time period datasets ####
ptf_weekday_avg <- ptf0218 %>%
  filter(day_type == "weekday") %>%
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

ptf_weekend_avg <- ptf0218 %>%
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
  mutate(across(c(train_tph, overall_tph), ~ na_if(., NaN))) %>%
  mutate(log_density = log(pop_density + 1)) %>%
  drop_na(cars_per_1000, bus_tph, pop_density, income, median_age)

ptf_peak_time <- ptf0218 %>%
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

## rescaling income to thousands of pounds ##
ptf_weekday_avg <- ptf_weekday_avg %>% mutate(income_1000s = income / 1000)
ptf_weekend_avg <- ptf_weekend_avg %>% mutate(income_1000s = income / 1000)
ptf_peak_time   <- ptf_peak_time %>% mutate(income_1000s = income / 1000)

#### Mean Centering the controls ####
ptf_weekday_avg <- ptf_weekday_avg %>%
  mutate(
    log_density_c = log_density - mean(log_density, na.rm = TRUE),
    income_1000s_c = income_1000s - mean(income_1000s, na.rm = TRUE),
    median_age_c = median_age - mean(median_age, na.rm = TRUE)
  )

#### Running the TWFE Models with feols — STAGE 1 ####
baseline_overall <- feols(
  cars_per_1000 ~ overall_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id
)
baseline_bus <- feols(
  cars_per_1000 ~ bus_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id
)
baseline_rail <- feols(
  cars_per_1000 ~ train_tph + log_density + income_1000s + median_age | lsoa_id + year,
  data = ptf_weekday_avg, vcov = ~lsoa_id
)

cat("Baseline overall N:", nobs(baseline_overall), "\n")
cat("Baseline bus N:", nobs(baseline_bus), "\n")
cat("Baseline rail N:", nobs(baseline_rail), "\n")

class(baseline_overall); class(baseline_bus); class(baseline_rail)

summary(check_conv_feols(baseline_overall))
summary(check_conv_feols(baseline_bus))
summary(check_conv_feols(baseline_rail))

etable(baseline_overall, baseline_bus, baseline_rail,
       headers = c("Overall", "Bus", "Rail"))