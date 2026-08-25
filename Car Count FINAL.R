#### Carcount constructor, previous car count quadrupled car data by summing quartes of cars ####
## bringing together Morgan 2021 data and DfT quarterly Data ##
## ensure ptf is loaded in the environment before running to attach car counts to it ##

library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)

#### Loading Morgan Car Count from 2001 to 2018, aggregating into total cars and removing Scotland ####
count0218 <- read_csv("2002_2018_CAR_COUNT_Historical_Car_Emissions_LSOA_public.csv")

# cars in Morgan 2021 are split into fuel type but have an "all cars" value, sum that for value by LSOA
count0218_total <- count0218 %>%
  group_by(year, LSOA) %>%
  summarise(total_cars = sum(AllCars, na.rm = TRUE), .groups = "drop")

## na.rm=TRUE excludes NA fuel-type rows (<=4 cars) from the sum, equivalent to
## treating them as zero when aggregating across fuel types ##

head(count0218_total) # check that total cars has been made #

## change the column names to match ptf and restrict to areas needed for the study 2004-2009 ##
count0218_pre2010 <- count0218_total %>%
  rename(lsoa_id = LSOA, Cars_Private_Licensed = total_cars) %>%
  filter(year >= 2004, year < 2010)

cat("count0218_pre2010 years present:",
    paste(sort(unique(count0218_pre2010$year)), collapse = ", "), "\n")

## filter to remove scottish "datazones" as they are not used in the study ##
count0218_pre2010 <- count0218_pre2010 %>%
  filter(str_detect(lsoa_id, "^[EW]01[0-9]{6}$"))

cat("Distinct lsoa_id codes (England & Wales only):", ## checking that no LSOAs have been lost, should be 34753##
    n_distinct(count0218_pre2010$lsoa_id), "\n")

##### Converting morgan 2021 data into 2021 LSOA bounds from 2011 bounds #####
## load in ONS look up ## filter to necessary columns ##
lsoa_lookup_2011_2021 <- read_csv("lsoa11 to lsoa21.csv") %>%
  select(LSOA11CD, LSOA21CD, CHGIND)

lsoa_lookup_2011_2021 %>% count(CHGIND) ## check how many LSOAs have been changed/merged ect. ##

## Join the car count data to look up table ##
count0218_recoded <- count0218_pre2010 %>%
  left_join(lsoa_lookup_2011_2021, by = c("lsoa_id" = "LSOA11CD"))

# have any LSOA codes failed to match, has the lookup worked? #
unmatched_2011 <- count0218_recoded %>% filter(is.na(LSOA21CD)) %>% distinct(lsoa_id)
cat("2011 LSOA codes with no match in the lookup (expect ~0 now):", nrow(unmatched_2011), "\n")

## as changed LSOAs are not able to provide accurate counts across boundary changes, they will be excluded ##
count0218_final <- count0218_recoded %>%
  filter(CHGIND %in% c("U", "M")) %>% ## keep unchanged and merged LSOAs ## remove splits or "complex" ##
  group_by(LSOA21CD, year) %>%
  summarise(Cars_Private_Licensed = sum(Cars_Private_Licensed, na.rm = TRUE), .groups = "drop") %>%
  rename(lsoa_id = LSOA21CD)

## Checking how many LSOAs have been excluded ##
excluded_lsoas <- count0218_recoded %>% filter(CHGIND %in% c("S", "X"))

cat("Old (2011) LSOAs excluded (splits and complex changes):",
    n_distinct(excluded_lsoas$lsoa_id), "\n")


#### Load in Department for Transport 2010-2025 Quarterly Car Licnesing Data and filter to necessary car counts #####
veh0125 <- read_csv("df_VEH0125 CSV.csv")

veh_filtered <- veh0125 %>%
  filter(Keepership == "PRIVATE", ## filter to private licensed, company cars includes fleets with some LSOAs have extremely high numbers ##
         LicenceStatus == "Licensed",
         BodyType == "Cars")

## veh1025 is in wide format, pivot to long to match ptf and morgan 2021 and complete averaging ##
veh_long <- veh_filtered %>%
  pivot_longer(
    cols = matches("^20[0-9]{2} Q[1-4]$"),
    names_to = "quarter",
    values_to = "cars"
  ) %>%
  mutate(
    year = as.integer(substr(quarter, 1, 4)),
    cars = as.numeric(cars)
  ) %>%
  filter(year >= 2010, year <= 2023) ## filter to years 2010 to 2023, these are covered in the study period ##

#### Averaging each year's car licensing counts as data notes the licensed vehicles currently registered not new registrations####
count1023_total <- veh_long %>%
  group_by(LSOA21CD, year) %>%
  summarise(Cars_Private_Licensed = round(mean(cars, na.rm = TRUE)), .groups = "drop") %>%
  rename(lsoa_id = LSOA21CD)

sort(unique(count1023_total$year)) # check all years are in the final data ## should be 10-23 ##

#### Combining both Datasets together ####

car_count_combined <- bind_rows(count0218_final, count1023_total)

## check if years are all there 2003-2023 ##
sort(unique(car_count_combined$year))


#### Attaching combined car data into the main ptf dataset ####

ptf_cleaned_2307 <- ptf_cleaned_2307 %>%
  select(-any_of("Cars_Private_Licensed")) %>%
  left_join(car_count_combined, by = c("lsoa_id", "year"))

head(ptf_cleaned_2307) ## has it joined## 

#### Checks to ensure the join has worked correctly with no dupes ####

## summing and outputting whole year car counts
## should show a smooth growth no quad car counts from 2010 onwards ##
ptf_cleaned_2307 %>%
  filter(year >= 2010) %>%
  distinct(lsoa_id, year, Cars_Private_Licensed) %>%
  group_by(year) %>%
  summarise(total_cars = sum(Cars_Private_Licensed, na.rm = TRUE)) %>%
  print(n = Inf)

## check for dupes ##
ptf_cleaned_2307 %>%
  count(lsoa_id, year, day_type, time_of_day, is_peak) %>%
  filter(n > 1) %>%
  nrow() == 0

## checking what percentage of rows are missing due to exclusions / NAing ##
ptf_cleaned_2307 %>%
  group_by(year) %>%
  summarise(pct_na_cars = mean(is.na(Cars_Private_Licensed))) %>%
  arrange(year) %>%
  print(n = Inf)

## how many across the whole panel ##
ptf_cleaned_2307 %>%
  filter(is.na(Cars_Private_Licensed)) %>%
  distinct(lsoa_id) %>%
  nrow()

#### Calculating cars per 1000 people ####
ptf_cleaned_2307 <- ptf_cleaned_2307 %>%
  select(-any_of("cars_per_1000")) %>%
  mutate(cars_per_1000 = (Cars_Private_Licensed / all_ages) * 1000)

# Zero-population LSOA-years produce Inf; coded as missing
ptf_cleaned_2307 <- ptf_cleaned_2307 %>%
  mutate(cars_per_1000 = if_else(is.infinite(cars_per_1000), NA_real_, cars_per_1000))
