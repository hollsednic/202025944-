####loading our median age data in; skip first three lines for 2021 bounds data ####
#as it includes the headers#
library(tidyverse)
library(readr)
library(dplyr)

## dont need to skip for old (2004-2010) data — different file format ##
ages_2004 <- read_csv("2004 ages person.csv")
ages_2005 <- read_csv("2005 ages person.csv")
ages_2006 <- read_csv("2006 ages person.csv")
ages_2007 <- read_csv("2007 ages person.csv")
ages_2008 <- read_csv("2008 ages person.csv")
ages_2009 <- read_csv("2009 ages person.csv")
ages_2010 <- read_csv("2010 ages person.csv")

## 2011-2023 on 2021 boundaries, need to skip header rows ##
ages_2011 <- read_csv("2011 ages man woman 2021 bound.csv", skip = 3)
ages_2012 <- read_csv("2012 ages man woman 2021 bound.csv", skip = 3)
ages_2013 <- read_csv("2013 ages man woman 2021 bound.csv", skip = 3)
ages_2014 <- read_csv("2014 ages man woman 2021 bound.csv", skip = 3)
ages_2015 <- read_csv("2015 ages man woman 2021 bound.csv", skip = 3)
ages_2016 <- read_csv("2016 ages man woman 2021 bound.csv", skip = 3)
ages_2017 <- read_csv("2017 ages man woman 2021 bound.csv", skip = 3)
ages_2018 <- read_csv("2018 ages man woman 2021 bound.csv", skip = 3)
ages_2019 <- read_csv("2019 ages man woman 2021.csv", skip = 3)
ages_2020 <- read_csv("2020 ages man woman 2021.csv", skip = 3)
ages_2021 <- read_csv("2021 ages man woman 2021.csv", skip = 3)
ages_2022 <- read_csv("2022 ages man woman 2021.csv", skip = 3)
ages_2023 <- read_csv("2023 ages man woman 2021.csv", skip = 3)
 
### median age function before applying to each ages item ####

calc_median_age <- function(counts) {
  
  counts <- replace(counts, is.na(counts), 0)
  
  total_pop <- sum(counts)
  
  if (total_pop == 0) return(NA_real_)
  
  half_pop <- total_pop / 2
  
  ages <- 0:(length(counts) - 1)

  ages[which(cumsum(counts) >= half_pop)[1]]
}

### Running on data between 2004 and 2010 ###
## 2004 to 2010 — old format, single "p" columns per age, LSOA11 codes ##
process_old_file <- function(df, year) {
  
  age_cols <- c(paste0("p", 0:89), "p90plus")
  
  median_age <- apply(df[, age_cols], 1, calc_median_age)
  
  tibble(
    lsoa_id = df$LSOA11CD,
    year = year,
    median_age = median_age
  )
}

### Running on data 2011 to 2023 ### 
process_new_file <- function(df, year) {  # making a function to 
  
  age_counts <- sapply(
    0:90,
    function(i) df[[paste0("F", i)]] + df[[paste0("M", i)]] ## add men and women counts togehter before applying median age function ##
  )
  
  median_age <- apply(age_counts, 1, calc_median_age)
  
  tibble(
    lsoa_id = df$`LSOA 2021 Code`,
    year = year,
    median_age = median_age
  )
}
## generating median age for every year ##
median_age_all_raw <- bind_rows(
  
  process_old_file(ages_2004, 2004),
  process_old_file(ages_2005, 2005),
  process_old_file(ages_2006, 2006),
  process_old_file(ages_2007, 2007),
  process_old_file(ages_2008, 2008),
  process_old_file(ages_2009, 2009),
  process_old_file(ages_2010, 2010),
  
  process_new_file(ages_2011, 2011),
  process_new_file(ages_2012, 2012),
  process_new_file(ages_2013, 2013),
  process_new_file(ages_2014, 2014),
  process_new_file(ages_2015, 2015),
  process_new_file(ages_2016, 2016),
  process_new_file(ages_2017, 2017),
  process_new_file(ages_2018, 2018),
  process_new_file(ages_2019, 2019),
  process_new_file(ages_2020, 2020),
  process_new_file(ages_2021, 2021),
  process_new_file(ages_2022, 2022),
  process_new_file(ages_2023, 2023)
  
)

## checking if it worked ## 
nrow(median_age_all_raw)
median_age_all_raw %>% distinct(year) %>% arrange(year)


### converting 2004 to 2010 lsoas which are in 2011 bounds to 2021 boundaries ###
## 94 percent of lsoas can be moved over; those which are cant are dropped ##
lsoa_lookup_2011_2021 <- read_csv("lsoa11 to lsoa21.csv") %>%
  select(LSOA11CD, LSOA21CD, CHGIND)

lookup_unchanged <- lsoa_lookup_2011_2021 %>% ## filter to just unchanged lsoas before moving on ##
  filter(CHGIND == "U") %>%
  select(LSOA11CD, LSOA21CD)

old_part <- median_age_all_raw %>%
  filter(year <= 2010) %>%
  inner_join(lookup_unchanged, by = c("lsoa_id" = "LSOA11CD")) %>%
  mutate(lsoa_id = LSOA21CD) %>%
  select(lsoa_id, year, median_age)

new_part <- median_age_all_raw %>%
  filter(year >= 2011)

median_age_all <- bind_rows(old_part, new_part)

nrow(median_age_all) ## this should be around 699,200 shows number of lsoa-year rows

## confirm no dupes ## dealing with dupes has been a feature of this data wrangling work ##
median_age_all %>% count(lsoa_id, year) %>% filter(n > 1) %>% nrow() == 0
# Check how many pre-2011 rows were dropped due to split/merged/complex LSOAs
rows_before <- median_age_all_raw %>% filter(year <= 2010) %>% nrow()
rows_after  <- old_part %>% nrow()

cat("Pre-2011 rows before conversion:", rows_before, "\n")
cat("Pre-2011 rows after conversion:", rows_after, "\n")
cat("Rows dropped:", rows_before - rows_after, "\n")

# How many distinct LSOAs does that represent?
median_age_all_raw %>% filter(year <= 2010) %>% distinct(lsoa_id) %>% nrow()
old_part %>% distinct(lsoa_id) %>% nrow()

### joining to main panel ### make sure main ptf panel is loaded in before 

ptf_cleaned_2307 <- ptf_cleaned_2307 %>%
  select(-any_of("median_age")) %>%
  left_join(median_age_all, by = c("lsoa_id", "year"))

## confirm no dupes ##
ptf_cleaned_2307 %>%
  count(lsoa_id, year, day_type, time_of_day, is_peak) %>%
  filter(n > 1) %>% 
  nrow()   # expect 0

sum(is.na(ptf_cleaned_2307$median_age))
mean(is.na(ptf_cleaned_2307$median_age))

ptf_cleaned_2307 %>%
  filter(is.na(median_age)) %>%
  distinct(lsoa_id) %>%
  nrow()

ptf_cleaned_2307 %>%
  filter(is.na(median_age)) %>%
  count(year) %>%
  arrange(year)

####Validating the median age we have just calculated####

official_median_age_2022 <- read_csv("median_age_england_and_wales.csv") ## load a modern ONS median age data table ##

validation <- median_age_all %>%
  filter(year == 2022) %>%
  left_join(official_median_age_2022, by = c("lsoa_id" = "LSOA21CD")) ## joining the calculated median ages to the 2022 table

cor(validation$median_age, validation$`median aged 2022`, use = "complete.obs") # calculate correlation between the two#

mean(abs(validation$median_age - validation$`median aged 2022`), na.rm = TRUE) # check the average difference, should have R of 0.99 #

## sanity check on the distribution in ptf ## 
summary(ptf_cleaned_2307$median_age)

