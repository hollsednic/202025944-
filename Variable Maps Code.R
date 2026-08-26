#### Creating maps for Methodology Section Showing VAriation in Key Explanatory Variables ####
##Ensure ptf_cleaned is loaded in##

library(sf)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

#### Loading in Super Generalised LSOA boundaries ####

lsoa_boundaries <- readRDS("bounds_lsoa_GB_super_generalised.Rds")

# Confirm structure before proceeding
class(lsoa_boundaries)
st_crs(lsoa_boundaries)
names(lsoa_boundaries)

#### claculate panel average for each variable across dataset ####

avg_by_lsoa <- ptf_cleaned_2307 %>%
  group_by(lsoa_id) %>%
  summarise(
    mean_overall_tph = mean(overall_tph, na.rm = TRUE),
    mean_pop_density = mean(pop_density, na.rm = TRUE),
    mean_income = mean(income, na.rm = TRUE),
    mean_median_age = mean(median_age, na.rm = TRUE)
  ) %>%
  mutate(mean_log_density = log(mean_pop_density + 1))

#### Join to super generalised boundaries #### # this could take a little long depending on memory##

map_data <- lsoa_boundaries %>%
  left_join(avg_by_lsoa, by = c("LSOA21CD" = "lsoa_id"))

# Check missingness, expect this to be roughly the share of Scotland
# (and any known data-quality exclusions) not covered by your study data
mean(is.na(map_data$mean_overall_tph))

#### Exclude Scottish Data ####

map_data <- map_data %>%
  filter(str_starts(LSOA21CD, "E") | str_starts(LSOA21CD, "W"))

# Confirm Scotland has been removed
cat("Remaining LSOAs after excluding Scotland:", nrow(map_data), "\n")
cat("Any 'S' (Scottish Data Zone) codes remaining:",
    sum(str_starts(map_data$LSOA21CD, "S")), "\n")

# Re-check missingness now that Scotland is excluded — this should drop
# substantially
mean(is.na(map_data$mean_overall_tph))


#### Building maps for each explanatory variable ####

##PT frequency ### scale for the map, but not the data itself has been log transformed to aid interpretation and show meaningful differences ##
map_freq <- ggplot(map_data) +
  geom_sf(aes(fill = mean_overall_tph), colour = NA) +
  scale_fill_viridis_c(
    name = "Mean tph",
    option = "magma",
    trans = "log1p",
    breaks = c(0, 1, 5, 20, 100, 500)
  ) +
  labs(title = "Average PT Frequency", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

## Density ## Actually log transformed from the PT pt frequency data ## This was completed after TWFE modelling ##
map_density <- ggplot(map_data) +
  geom_sf(aes(fill = mean_log_density), colour = NA) +
  scale_fill_viridis_c(name = "Log Density", option = "viridis") +
  labs(title = "Average log Population Density", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )


## Income ## Nothing has been changed in terms of scale ##
map_income <- ggplot(map_data) +
  geom_sf(aes(fill = mean_income), colour = NA) +
  scale_fill_viridis_c(name = "Income (£)", option = "plasma", labels = label_comma()) +
  labs(title = "Average household income", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

## Age ##
map_age <- ggplot(map_data) +
  geom_sf(aes(fill = mean_median_age), colour = NA) +
  scale_fill_viridis_c(name = "Median Age", option = "cividis") +
  labs(title = "Average Median Age", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank()
  )

#### Combining maps into 2x2 grid ####
library(gridExtra)

combined_maps <- grid.arrange(
  map_freq, map_density, map_income, map_age,
  ncol = 2, nrow = 2,
  top = "National distribution of key study variables by LSOA (panel average, 2004-2023)"
)

ggsave("variable_maps_2x2.jpg", combined_maps, 
       width = 7, height = 4.8, units = "in", dpi = 300, quality = 100)
