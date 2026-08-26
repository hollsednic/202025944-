library(tidyverse)
library(dplyr)

ptf_cleaned_2207 <- ptf_cleaned_2207 %>%
  select(-matches("^income"))  ## previously got income wrong, remove if in the dataset ##

names(ptf_cleaned_2207)   # confirm income is gone before rebuilding it

(
  ptf_cleaned_2207 %>%
    count(lsoa_id, year, day_type, time_of_day, is_peak) %>%
    filter(n > 1) %>%
    nrow() == 0
)

### Load in our raw income  data from 2012 to 2023; skip so headings are all the same and visible ###
income_2012_weekly <- read_csv("2012_income_weekly.csv", skip = 4)
income_2014_weekly <- read_csv("2014_income_weekly.csv", skip = 4)
income_2016_annual <- read_csv("2016_income_annual.csv", skip = 4)
income_2018_annual <- read_csv("2018_income_annual.csv", skip = 4)
income_2020_annual <- read_csv("2020_income_annual.csv", skip = 4)
income_2023_annual <- read_csv("2023_income_annual.csv", skip = 3)

## convert weekly income data to annual ##
income_2012_annual <- income_2012_weekly %>%
  mutate(income = `Net weekly income (£)` * 52, year = 2012)
income_2014_annual <- income_2014_weekly %>%
  mutate(income = `Net weekly income (£)` * 52, year = 2014)

### changing and harmonising the column names of income data ###
income_2012_annual <- rename(
  income_2012_annual,
  msoa_id   = `MSOA code`,
  msoa_name = `MSOA name`,
  lad_id    = `Local authority code`,
  lad_name  = `Local authority name`
)
income_2014_annual <- rename(
  income_2014_annual,
  msoa_id   = `MSOA code`,
  msoa_name = `MSOA name`,
  lad_id    = `Local authority code`,
  lad_name  = `Local authority name`
)
income_2016_annual <- rename(
  income_2016_annual,
  msoa_id   = `MSOA code`,
  msoa_name = `MSOA name`,
  lad_id    = `Local authority code`,
  lad_name  = `Local authority name`,
  income    = `Net annual income (£)`
)
income_2018_annual <- rename(
  income_2018_annual,
  msoa_id   = `MSOA code`,
  msoa_name = `MSOA name`,
  lad_id    = `Local authority code`,
  lad_name  = `Local authority name`,
  income    = `Net annual income (£)`
)
income_2020_annual <- rename(
  income_2020_annual,
  msoa_id   = `MSOA code`,
  msoa_name = `MSOA name`,
  lad_id    = `Local authority code`,
  lad_name  = `Local authority name`,
  income    = `Net annual income (£)`
)
income_2023_annual <- rename(
  income_2023_annual,
  msoa_id   = `MSOA code`,
  msoa_name = `MSOA name`,
  lad_id    = `Local authority code`,
  lad_name  = `Local authority name`,
  income    = `Disposable (net) annual income (£)`
)

## Adding year columns for bind ##
income_2016_annual <- mutate(income_2016_annual, year = 2016)
income_2018_annual <- mutate(income_2018_annual, year = 2018)
income_2020_annual <- mutate(income_2020_annual, year = 2020)
income_2023_annual <- mutate(income_2023_annual, year = 2023)

### combining income data into single set ###
income_msoa <- bind_rows(
  income_2012_annual, income_2014_annual, income_2016_annual,
  income_2018_annual, income_2020_annual, income_2023_annual
) %>%
  filter(!is.na(income))

### Loading in local authority disposable income growth data for projecting back ###
local_auth_9823 <- read_csv("local authority household disposable.csv", skip = 1)

### filtering local authority data to years missing (2004-2012) ###
local_auth_0412 <- local_auth_9823 %>%
  select(`LAD code`, `Region name`, all_of(as.character(2004:2012))) # Do as character because columns are text #

## Change local authority names to align with income data ##
local_auth_0412 <- rename(
  local_auth_0412,
  lad_id = `LAD code`,
  region_name = `Region name`
)
names(local_auth_0412)

### Interpolate income from 2012 to 2020 to fill in missing years ### Leave out 2023 as it has different boundaries ###
library(zoo) ## load zoo to complete interpolation using nas approx function ##

income_historical <- income_msoa %>%
  filter(year <= 2020) ## removig 2023 data ##

income_historical %>% distinct(year) %>% arrange(year) ## checking if 2023 has been removed ##

income_msoa_complete <- income_historical %>%
  group_by(msoa_id) %>%
  complete(year = 2012:2020) %>%
  arrange(year) %>%
  fill(msoa_name, lad_id, lad_name, .direction = "downup") %>%
  mutate(income = na.approx(income, x = year, na.rm = FALSE)) %>%
  ungroup()

income_msoa_complete %>% distinct(year) %>% arrange(year) ## confirming years exist ##
summary(income_msoa_complete) # check that the interpolation worked;only check the income column; if NAs exist is because they never got carried over from original file##

## extend 2020 data to 2021 as no 2021 income data exists ##
income_2021 <- income_msoa_complete %>% filter(year == 2020) %>% mutate(year = 2021)
## extend 2023 to 2022 for same reason ##
income_2022 <- income_2023_annual %>%
  select(msoa_id, income) %>%
  mutate(year = 2022)

## check both new income dfs to make sure they have the data in them ##
summary(income_2021$income)
summary(income_2022$income)
nrow(income_msoa_complete %>% filter(year == 2020))
nrow(income_2021) ## should align with 2020 number of rows ##
nrow(income_2022) ## should align with 2023 ##
nrow(income_2023_annual)

### preparing local authority data for backcasting to 2004 ###
## local authority data is in wide format, need to pivot to wide to match income data for join ##
local_auth_long <- local_auth_0412 %>%
  pivot_longer(cols = `2004`:`2012`, names_to = "year", values_to = "growth") %>%
  mutate(year = as.numeric(year))
 
names(local_auth_long) # are the names all there# # also view to make sure pivot worked#

# load local authority lookup local authority data is in 2023 boundaries, local authorities have been reorganised in England so will need to use lookup to see which msoas fit into which LA##
lad_lookup <- read_csv("lad_lookup.csv")
names(lad_lookup)   # check what the actual LAD11CD/LAD21CD columns are called

### manual lookup for local authorities affected by major boundary changes ###
### not covered cleanly by the standard lookup file (Cumbria, North ###
### Yorkshire, and Somerset reorganisations) ###
manual_lookup <- tribble(
  ~old_code, ~new_code,
  "E07000026", "E06000063",  # Allerdale -> Cumberland
  "E07000028", "E06000063",  # Carlisle -> Cumberland
  "E07000029", "E06000063",  # Copeland -> Cumberland
  "E07000027", "E06000064",  # Barrow -> Westmorland & Furness
  "E07000030", "E06000064",  # Eden -> Westmorland & Furness
  "E07000031", "E06000064",  # South Lakeland -> Westmorland & Furness
  "E07000163", "E06000065",  # Craven -> North Yorkshire
  "E07000164", "E06000065",  # Hambleton -> North Yorkshire
  "E07000165", "E06000065",  # Harrogate -> North Yorkshire
  "E07000166", "E06000065",  # Richmondshire -> North Yorkshire
  "E07000167", "E06000065",  # Ryedale -> North Yorkshire
  "E07000168", "E06000065",  # Scarborough -> North Yorkshire
  "E07000169", "E06000065",  # Selby -> North Yorkshire
  "E07000187", "E06000066",  # Mendip -> Somerset
  "E07000188", "E06000066",  # Sedgemoor -> Somerset
  "E07000189", "E06000066",  # South Somerset -> Somerset
  "E07000246", "E06000066"   # Taunton Deane/West Somerset -> Somerset
)

## make a 2012 anchor year, with LAD codes harmonised to current boundaries, ready for backcasting ###
income_2012_fixed <- income_msoa_complete %>%
  filter(year == 2012) %>%
  select(msoa_id, msoa_name, lad_id, lad_name, income) %>%
  left_join(lad_lookup %>% select(LAD11CD, LAD21CD), by = c("lad_id" = "LAD11CD")) %>%
  mutate(lad_id = coalesce(LAD21CD, lad_id)) %>%
  left_join(manual_lookup, by = c("lad_id" = "old_code")) %>%
  mutate(lad_id = coalesce(new_code, lad_id)) %>%
  select(msoa_id, msoa_name, lad_id, lad_name, income)

## check the harmonisation worked — should be 0 unmatched LADs against
## the growth data before proceeding to the backcast ##
income_2012_fixed %>%
  anti_join(local_auth_long %>% filter(year == 2012), by = "lad_id") %>%
  distinct(lad_id, lad_name)

### time to backcast data to 2004 from 2012 ###
income_backcast <- income_2012_fixed %>%
  mutate(year = 2012) ## select only 2012 to start from ##

for (yr in 2012:2005) {
  
  previous_year <- income_backcast %>%
    filter(year == yr) %>%
    left_join(
      local_auth_long %>% filter(year == yr) %>% select(lad_id, growth),
      by = "lad_id"
    ) %>%
    mutate(
      income = income / (1 + growth / 100), ## reverse the growth rate to estimate the prior year's income from this year's value ##
      year   = yr - 1
    ) %>%
    select(msoa_id, msoa_name, lad_id, lad_name, income, year)
  
  income_backcast <- bind_rows(income_backcast, previous_year) 
}

income_backcast %>% distinct(year) %>% arrange(year) ## did it produce the years ##

summary(income_backcast$income) ## checking income distribution and whether there are NAs ##

income_backcast %>%
  count(year) %>%
  arrange(year) # rows should be identical and match 2012 to 2020 data from before; 7201 #

### combine our backcasted data set with the 2012 to 2021 dataset ###
## remove 2012 from backcast data and combine with recent data ##
income_0421 <- bind_rows(
  income_backcast %>% filter(year < 2012),   # 2004-2011 only, 2012 excluded here
  income_msoa_complete,                       # includes the 2012 row
  income_2021
)
income_0421 %>% count(msoa_id, year) %>% filter(n > 1) %>% nrow() # check if any dupes; should be zero #

### bringing 2023 and 2022 datasets together with income_0421 ###
## combine 2022 and 2023 into their own MSOA21-coded set ##
income_2223_years <- bind_rows(
  income_2022,
  income_2023_annual %>% select(msoa_id, income) %>% mutate(year = 2023)
)

nrow(income_2223_years) # when divided by 2 should be 7264 #

income_0423 <- bind_rows(
  income_0421,
  income_2223_years
)
income_0423 %>% distinct(year) %>% arrange(year)
income_0423 %>% count(msoa_id, year) %>% filter(n > 1) %>% nrow()  # expect 0

## bridging lsoa 21 bounds to msoa 11 bounds as no official lookup exists ##
## load in lookup tables ##
lsoa_msoa_lookup <- read_csv("lsoa_msoa_lookup.csv")
msoa_2011_msoa_2021 <- read_csv("msoa_2011_msoa_2021.csv")

income_msoa21_years <- income_2223_years

lsoa_msoa_bridge <- lsoa_msoa_lookup %>%
  select(lsoa_id = LSOA21CD, msoa_id = MSOA21CD) %>%
  distinct() %>%
  left_join(
    msoa_2011_msoa_2021 %>% select(msoa_id = MSOA21CD, msoa11_id = MSOA11CD) %>% distinct(),
    by = "msoa_id"
  )
## joining at the lsoa level to prepare to joining to ptf file ##
income_lsoa_hist_raw <- lsoa_msoa_bridge %>%
  filter(!is.na(msoa11_id)) %>%
  select(lsoa_id, msoa_id = msoa11_id) %>%
  left_join(
    income_0423 %>% select(msoa_id, year, income),
    by = "msoa_id",
    relationship = "many-to-many"
  )

## earlier work found duplicates between years with merges and new lsoas/msoas ## check if any duplicates

duplicate_lsoa_years <- income_lsoa_hist_raw %>%
  count(lsoa_id, year) %>%
  filter(n > 1)

duplicate_check <- income_lsoa_hist_raw %>%
  semi_join(duplicate_lsoa_years, by = c("lsoa_id", "year")) %>%
  group_by(lsoa_id, year) %>%
  summarise(
    income_min = min(income, na.rm = TRUE),
    income_max = max(income, na.rm = TRUE),
    pct_diff = 100 * (income_max - income_min) / mean(c(income_min, income_max)),
    .groups = "drop"
  )

## check how many ##
nrow(duplicate_check)
quantile(duplicate_check$pct_diff, probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE)
summary(duplicate_check$pct_diff)

## since there are a large number of duplicates with large differences, we will exclude them from the final dataset ##
income_lsoa_hist_clean <- income_lsoa_hist_raw %>%
  anti_join(duplicate_lsoa_years, by = c("lsoa_id", "year"))

income_lsoa_hist_clean %>% count(lsoa_id, year) %>% filter(n > 1) %>% nrow() # check how many dupes remain, should be 0 #

### now previous income data have been joined with old boundaries, add new bounds from 2022 and 2023 directly to lsoa level ###
income_lsoa_msoa21_years <- lsoa_msoa_lookup %>%
  select(lsoa_id = LSOA21CD, msoa_id = MSOA21CD) %>%
  distinct() %>%
  left_join(income_msoa21_years, by = "msoa_id")

income_lsoa_msoa21_years %>% count(lsoa_id, year) %>% filter(n > 1) %>% nrow() # check for dupes again, should still be zero #

### create final income data set with 2004 to 2021 and 2022 and 2023 ###
income_lsoa_final <- bind_rows(
  income_lsoa_hist_clean %>% select(lsoa_id, year, income),
  income_lsoa_msoa21_years %>% select(lsoa_id, year, income)
)

income_lsoa_final %>% count(lsoa_id, year) %>% filter(n > 1) %>% nrow() # check for dupes, should be zero #
income_lsoa_final %>% distinct(year) %>% arrange(year) # should show 2004-2023 #

#### can now be joined to ptf data ####
ptf_cleaned_2307 <- ptf_cleaned_2307 %>%
  select(-any_of("income")) %>%
  left_join(income_lsoa_final, by = c("lsoa_id", "year"))

## checking what percentage of rows are missing income, by year ##
ptf_cleaned_2307 %>%
  group_by(year) %>%
  summarise(pct_na_income = mean(is.na(income))) %>%
  arrange(year) %>%
  print(n = Inf)

## how many distinct LSOAs are missing income at least once across the whole panel ##
ptf_cleaned_2307 %>%
  filter(is.na(income)) %>%
  distinct(lsoa_id) %>%
  nrow()

## overall summary of income once joined ##
summary(ptf_cleaned_2307$income)

