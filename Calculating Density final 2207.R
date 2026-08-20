library(tidyverse)

### loading in population data; this is population.rds so just need to click to load in ###
## align names with ptf file ##

population_ptf_aligned <- population_update %>%
  rename(
    lsoa_id = LSOA21CD
  )

### filtering out 2002 and 2024 to only the study time period ###

population_ptf_aligned <- population_ptf_aligned %>%
  filter(year != 2002, year != 2024)
## did this work; check years ##
population_ptf_aligned %>% 
  distinct(year) %>%
  print(n = 21) ## it did! ##

## removing columns which aren't needed ##
names(population_ptf_aligned) #check that before and after running the below code#

population_ptf_aligned <- population_ptf_aligned %>%
  select(year, all_ages, lsoa_id, households_est, all_properties)

## removing Scotland data making it just england and wales ##
population_ptf_aligned <- population_ptf_aligned %>%
  filter(str_starts(lsoa_id, "E") | str_starts(lsoa_id, "W"))

## check that with the same str_starts ##
sum(str_starts(population_ptf_aligned$lsoa_id, "S"))

###Now that population data is prepared we load in the size of LSOA data. Make sure to use KM version for people per km###
sam_data <- read_csv("sam_lsoa_2021_km.csv")

## adjust column names to align with population data and ptf ##
names(population_ptf_aligned)
names(sam_data) ## check before and after ##


sam_data <- sam_data %>%
  rename(
    lsoa_id = LSOA21CD
  )

sam_data <- sam_data %>%
  rename(
    area_km2 = `Extent of the Realm (Area in KM2)`## rename, doesn't behave well in the join later if not## ##also using this in line with ONS guidnance on sam data##
  )

## check for unmatched LSOA codes between datasets ##

unmatched_population <- population_ptf_aligned %>%
  anti_join(sam_data, by = "lsoa_id") %>%
  distinct(lsoa_id)

cat("Population LSOAs with no match in SAM lookup:", nrow(unmatched_population), "\n") ##Check how many obs in environment none there. should be zero##

### Calculating population density ###
population_density_ptf_aligned <- population_ptf_aligned %>%
  left_join(sam_data %>% select(lsoa_id, area_km2), by = "lsoa_id") %>%
  mutate(pop_density = all_ages / area_km2) %>%
  select(year, all_ages, lsoa_id, households_est, all_properties, pop_density)

population_density_ptf_aligned <- population_density_ptf_aligned %>%
  filter(year >= 2004, year <= 2023)  #remove 2003 data because you left it in#

## Do some checks now to make sure join and calculation has worked ##

names(population_density_ptf_aligned) ## check names to ensure join has happened. Then summary so previous issues r.e massive outliers haven't reoccured#
summary(population_density_ptf_aligned)

population_density_ptf_aligned %>% #should have 0#
  filter(is.na(pop_density)) %>%
  distinct(lsoa_id) %>%
  nrow()

population_density_ptf_aligned %>% # should have 0#
  filter(pop_density < 0) %>%
  nrow()

population_density_ptf_aligned %>%
  count(lsoa_id, year) %>%
  filter(n > 1) %>%
  nrow() == 0

### time to remove old density data and attach to ptf data ###
ptf_no_scot_income <- ptf_no_scot_income %>%
  select(-any_of(c("pop_density", "households_est", "all_properties")))

names(ptf_no_scot_income)

ptf_no_scot_income <- ptf_no_scot_income %>% # attaching # 
  left_join(
    population_density_ptf_aligned %>%
      select(lsoa_id, year, pop_density, households_est, all_properties),
    by = c("lsoa_id", "year")
  ) 

# Confirm no duplicate keys were introduced
ptf_no_scot_income %>%
  count(lsoa_id, year, day_type, time_of_day, is_peak) %>%
  filter(n > 1) %>%
  nrow()   # expect 0

save(ptf_no_scot_income, file = "ptf_no_scot_income_CLEANED_2207.RData")

ptf_cleaned_2207 <- ptf_no_scot_income
