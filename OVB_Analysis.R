library(tidyverse)
library(lme4)
library(modelsummary)

# models
fit_noUrban <- readRDS("data/glmer_noUrban.rds")
fit_withUrban <- readRDS("data/glmer_withUrban.rds")

# clean
district_data <- svy20_modeling |>
  group_by(cd) |>
  summarize(
    trump_rate = mean(trump),
    pct_white = first(pct_white_elec),
    classes = first(classes),
    .groups = "drop"
  ) |>
  mutate(
    is_suburban = as.numeric(classes == "Suburban"),
    is_urban = as.numeric(classes == "Urban")
  )

# Extract coefficients
beta_white_short <- fixef(fit_noUrban)["pct_white_elec"]
beta_white_long <- fixef(fit_withUrban)["pct_white_elec"]
beta_suburban <- fixef(fit_withUrban)["classesSuburban"]
beta_urban <- fixef(fit_withUrban)["classesUrban"]

suburban <- lm(is_suburban ~ pct_white, data = district_data)
urban <- lm(is_urban ~ pct_white, data = district_data)

gamma_suburban <- coef(suburban)["pct_white"]
gamma_urban <- coef(urban)["pct_white"]

# Calculate bias components
observed_bias <- beta_white_short - beta_white_long

## expected biases now
eb_suburban <- beta_suburban * gamma_suburban
eb_urban <- beta_urban * gamma_urban
eb_total <- eb_suburban + eb_urban

pct_explained <- (eb_total / observed_bias) * 100

# Contribution breakdown
contrib_suburban_pct <- (eb_suburban / eb_total) * 100
contrib_urban_pct <- (eb_urban / eb_total) * 100


