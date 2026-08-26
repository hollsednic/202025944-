#### Making Coefficient Plots for each stage's results ####
##This code is only to be run in the same session after "Three Stages TWFE FINAL.R"

### Stage 1 Baseline ### Coefplot is within fixest but ggplot gives more control over styling and sizing ect. ##
# pulling out the frequency coefficient for each mode for stage 1 #

freq_only_coefs <- tibble(
  model = c("Overall tph", "Bus tph", "Train tph"),
  estimate = c(
    coef(baseline_overall)["overall_tph"],
    coef(baseline_bus)["bus_tph"],
    coef(baseline_rail)["train_tph"]
  ),
  se = c(
    se(baseline_overall)["overall_tph"],
    se(baseline_bus)["bus_tph"],
    se(baseline_rail)["train_tph"]
  )
)

figure1_baseline <- ggplot(freq_only_coefs, aes(x = model, y = estimate)) +
  geom_hline(yintercept = 0, colour = "red", linewidth = 1) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), ## 1.96*se to add 95 percent confidence interval, this is in all plots ##
                  linewidth = 1.25, size = 0.5) +
  labs(x = NULL, y = "Coefficient Estimate") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "lightcyan", colour = NA),
    axis.text.x = element_text(size = 14)   # larger x-axis label font
  )
## this produces a plot with a white, checkered area and light cyan background, this same style is used for all plots ##

ggsave("figure1_baseline_higher.jpg", figure1_baseline,
       width = 9.5, height = 4, units = "in", dpi = 400) ## save 

#### Stage 2 Interaction Models ####
# extracting coefficients as with stage 1 # # overall tph #
interaction_overall_coefs <- tibble(
  moderator = c("Density", "Income (£1,000s)", "Median age"),
  estimate = c(
    coef(interaction_overall)["overall_tph:log_density_c"],
    coef(interaction_overall)["overall_tph:income_1000s_c"],
    coef(interaction_overall)["overall_tph:median_age_c"]
  ),
  se = c(
    se(interaction_overall)["overall_tph:log_density_c"],
    se(interaction_overall)["overall_tph:income_1000s_c"],
    se(interaction_overall)["overall_tph:median_age_c"]
  )
)

# bus coefficient #
interaction_bus_coefs <- tibble(
  moderator = c("Density", "Income (£1,000s)", "Median age"),
  estimate = c(
    coef(interaction_bus)["bus_tph:log_density_c"],
    coef(interaction_bus)["bus_tph:income_1000s_c"],
    coef(interaction_bus)["bus_tph:median_age_c"]
  ),
  se = c(
    se(interaction_bus)["bus_tph:log_density_c"],
    se(interaction_bus)["bus_tph:income_1000s_c"],
    se(interaction_bus)["bus_tph:median_age_c"]
  )
)

# rail coefficient #
interaction_rail_coefs <- tibble(
  moderator = c("Density", "Income (£1,000s)", "Median age"),
  estimate = c(
    coef(interaction_rail)["train_tph:log_density_c"],
    coef(interaction_rail)["train_tph:income_1000s_c"],
    coef(interaction_rail)["train_tph:median_age_c"]
  ),
  se = c(
    se(interaction_rail)["train_tph:log_density_c"],
    se(interaction_rail)["train_tph:income_1000s_c"],
    se(interaction_rail)["train_tph:median_age_c"]
  )
)

#### Plotting Stage 2 Results ####
# load grid extra to merge all three into one image at the end #
library(gridExtra)

#Overall Frequency Interaction Plot#

figure2_overall <- ggplot(interaction_overall_coefs, aes(x = moderator, y = estimate)) +
  geom_hline(yintercept = 0, colour = "red", linewidth = 1) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  linewidth = 1.25, size = 0.5) +
  labs(x = NULL, y = "Coefficient Estimate", title = "Overall") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "lightcyan", colour = NA),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 6, face = "bold") ## making font size smaller since this wil be three plots in one image 
  )

#Bus Frequency Interaction Plot#
figure2_bus <- ggplot(interaction_bus_coefs, aes(x = moderator, y = estimate)) +
  geom_hline(yintercept = 0, colour = "red", linewidth = 1) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  linewidth = 1.25, size = 0.5) +
  labs(x = NULL, y = NULL, title = "Bus") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "lightcyan", colour = NA),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 6, face = "bold"))
  

#Rail Frequency Interaction Plot#
figure2_rail <- ggplot(interaction_rail_coefs, aes(x = moderator, y = estimate)) +
  geom_hline(yintercept = 0, colour = "red", linewidth = 1) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  linewidth = 1.25, size = 0.5) +
  labs(x = NULL, y = NULL, title = "Rail") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "lightcyan", colour = NA),
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 6, face = "bold"))
  

# Combine into one row, three columns
figure2_combined <- grid.arrange(
  figure2_overall, figure2_bus, figure2_rail,
  ncol = 3
)

## save##
ggsave("figure2_combined.jpg", figure2_combined,
       width = 19, height = 8, units = "cm", dpi = 400)

#### Stage 3 Quartile Model Plot ####
## extract coefficients from each quartile model ## 
# this is already in the "Three Stages TWFE but here if necessary for stage 3 plots #
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

# plotting quartiles in single image #
ggplot(quartile_coefs, aes(x = factor(quartile), y = estimate)) +
  geom_hline(yintercept = 0, colour = "red", linewidth = 1) +
  geom_pointrange(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se),
                  linewidth = 1.25, size = 0.5) +
  scale_x_discrete(labels = c("Q1\n(lowest)", "Q2", "Q3", "Q4\n(highest)")) +
  labs(x = "Frequency quartile", y = "Coefficient (overall_tph)",
       title = "TWFE coefficient on frequency, by frequency quartile") +
  theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "lightcyan", colour = NA)
  )


