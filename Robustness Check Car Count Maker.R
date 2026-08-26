#### Loading Morgan Car Count from 2001 to 2018, aggregating into total cars and removing Scotland ####
#### ROBUSTNESS CHECK VERSION: extended to 2018 rather than 2009 ####
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
## ensure ptf is loaded in ##

count0218 <- read_csv("2002_2018_CAR_COUNT_Historical_Car_Emissions_LSOA_public.csv")

# cars in Morgan 2021 are split into fuel type but have an "all cars" value, sum that for value by LSOA
count0218_total <- count0218 %>%
  group_by(year, LSOA) %>%
  summarise(total_cars = sum(AllCars, na.rm = TRUE), .groups = "drop")
## na.rm=TRUE excludes NA fuel-type rows (<=4 cars) from the sum, equivalent to
## treating them as zero when aggregating across fuel types ##

## change the column names to match ptf and restrict to areas needed for the study 2004-2018 ##
count0218_pre2019 <- count0218_total %>%
  rename(lsoa_id = LSOA, Cars_Private_Licensed = total_cars) %>%
  filter(year >= 2004, year <= 2018)

## filter to remove scottish "datazones" as they are not used in the study ##
count0218_pre2019 <- count0218_pre2019 %>%
  filter(str_detect(lsoa_id, "^[EW]01[0-9]{6}$"))

##### Converting morgan 2021 data into 2021 LSOA bounds from 2011 bounds #####
## load in ONS look up ## filter to necessary columns ##
lsoa_lookup_2011_2021 <- read_csv("lsoa11 to lsoa21.csv") %>%
  select(LSOA11CD, LSOA21CD, CHGIND)

## Join the car count data to look up table ##
count0218_recoded <- count0218_pre2019 %>%
  left_join(lsoa_lookup_2011_2021, by = c("lsoa_id" = "LSOA11CD"))

## as changed LSOAs are not able to provide accurate counts across boundary changes, they will be excluded ##
count0218_final <- count0218_recoded %>%
  filter(CHGIND %in% c("U", "M")) %>% ## keep unchanged and merged LSOAs ## remove splits or "complex" ##
  group_by(LSOA21CD, year) %>%
  summarise(Cars_Private_Licensed = sum(Cars_Private_Licensed, na.rm = TRUE), .groups = "drop") %>%
  rename(lsoa_id = LSOA21CD)

#### Attaching to PTF item and removing any previous car counts ####
ptf0218 <- ptf_cleaned_2307 %>%
  filter(year <= 2018) %>%
  select(-any_of("Cars_Private_Licensed")) %>%
  left_join(count0218_final, by = c("lsoa_id", "year"))

#### Calculating cars per 1000 people ####
ptf0218 <- ptf0218 %>%
  select(-any_of("cars_per_1000")) %>%
  mutate(cars_per_1000 = (Cars_Private_Licensed / all_ages) * 1000)

# Zero-population LSOA-years produce Inf; coded as missing
ptf0218 <- ptf0218 %>%
  mutate(cars_per_1000 = if_else(is.infinite(cars_per_1000), NA_real_, cars_per_1000))

#### Save ptf file for use in robustness check ####
save(ptf0218, file = "ptf_cleaned_2307_0218_to18.RData")