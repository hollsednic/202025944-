##### STEP 1. Initial PTF dataset. Pivoting table and merging rail frequencies into one #####
##making the key data set to be used throughout the project 
## this is made of a previous piece of messy code, the name will  be changed to ptf_cleaned_2307 at the end to facilitate joins and merges
## COMPLETE THIS CODE BEFORE ANYTHING ELSE ##

library(tidyverse)
library(data.table)

#####Loading in raw ptf frequency file from carbon and place #####

pt_frequency <- read_csv("PT_FREQUENCY.csv", guess_max = 20000) ## increased to reduce risk of incorrect column types guessed from sample
problems(pt_frequency)

#### Pivot to long format, current data wide and inappropriate for analysis ####

pt_frequency <- as.data.table(pt_frequency)
ptf <- melt(pt_frequency, # melt chosen as faster than pivot longer due to the size of this file #
            id.vars = "zone_id",
            variable.name = "variable",
            value.name = "tph") # turns into one row per LSOA-variable combination

#### Decoding encoded column names to give each variable it's own column ##
## each original column name encodes year, day type, time of day, peak
## status and transport mode in a single string; this extracts each piece
## into its own column ##
ptf[, `:=`(
  year        = as.integer(str_extract(variable, "\\d{4}")),
  mode        = as.integer(str_extract(variable, "\\d+$")), ## mode code is the trailing number ##
  day_type    = fcase(
    variable %like% "weekday", "weekday",
    variable %like% "Sat",     "saturday",
    variable %like% "Sun",     "sunday",
    default = NA_character_
  ),
  time_of_day = fcase(
    variable %like% "Morning",   "morning",
    variable %like% "Afternoon", "afternoon",
    variable %like% "Midday",    "midday",
    variable %like% "Evening",   "evening",
    variable %like% "Night",     "night",
    default = NA_character_
  ),
  is_peak = variable %like% "Peak"
)]

## remove rows that could not be classified 
ptf <- ptf[!is.na(day_type) & !is.na(time_of_day)]
ptf[, variable := NULL] ## no longer needed once decoded ##

#### Group Transport Modes into bus, rail and other #### Bus and rail will be used in analysis 
## mode codes: 0 = tram, 1 = metro, 2 = rail, 3 = bus; 

## tram/metro/rail are combined under "train" as rail-based transport, this is reported as rail in the final dissertation project, artefact from earlier coding ##
ptf[, mode_group := fcase(
  mode == 3,        "bus",
  mode %in% 0:2,    "train",
  default =         "other"
)]

#### Aggregate to final panel structure #### 
ptf_grouped <- ptf[, .(
  bus_tph     = sum(tph[mode_group == "bus"],   na.rm = TRUE),
  train_tph   = sum(tph[mode_group == "train"], na.rm = TRUE),
  overall_tph = sum(tph, na.rm = TRUE) ## sum ALL modes (bus, train, and "other" e.g. tram/metro) for overall figure ##
), by = .(lsoa_id = zone_id, year, day_type, time_of_day, is_peak)]

glimpse(ptf_grouped) ## expect 8 columns: lsoa_id, year, day_type, time_of_day, is_peak, bus_tph, train_tph, overall_tph ##
head(ptf_grouped) 

#### verify the aggregation and join worked ####
## check overall_tph is always >= bus_tph + train_tph, since it also includes "other" modes ##
ptf_grouped[, all(overall_tph >= bus_tph + train_tph)]

colSums(is.na(ptf_grouped)) ## check for missing values ##

unique(ptf_grouped$year) |> sort() ## confirm years present ## 2004-2023

## remove rows with missing lsoa_id, a single LSOA was found as missing ##
ptf_grouped <- ptf_grouped[!is.na(lsoa_id)]

## remove Scotland, as it uses "Data Zones" rather than LSOAs and is not
## included in this study ##

ptf_grouped <- ptf_grouped[str_detect(lsoa_id, "^[EW]01[0-9]{6}$")]

##### Renaming to be in line with the other variable creation code and TWFE ####
ptf_cleaned_2307 <- ptf_grouped