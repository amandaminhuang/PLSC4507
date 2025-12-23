library(gt)
library(modelsummary)
library(tidyverse)

# Load models -- downloaded from Google Colab
fit_noUrban <- read_rds("data/glmer_noUrban.rds")
fit_withUrban <- read_rds("data/glmer_withUrban.rds")

# ANOVA
anova_result <- anova(fit_noUrban, fit_withUrban)
print(anova_result)

# AIC
aic_comparison <- AIC(fit_noUrban, fit_withUrban)
aic_comparison

# Calculate AIC difference
aic_diff <- AIC(fit_noUrban) - AIC(fit_withUrban)
aic_diff

# BIC
BIC(fit_noUrban)
BIC(fit_withUrban)


bic_diff <- BIC(fit_noUrban) - BIC(fit_withUrban)
bic_diff

## Plot -----------------------------------------------
comparison <- tibble(
  Model = c("No Urban", "With Urban"),
  AIC   = c(AIC(fit_noUrban), AIC(fit_withUrban)),
  BIC   = c(BIC(fit_noUrban), BIC(fit_withUrban))
) |>
  mutate(
    "AIC Difference" = (AIC - AIC[Model == "No Urban"]) * -1,
    "BIC Difference" = (BIC - BIC[Model == "No Urban"]) * -1
  )

gt_comparison <- comparison |>
  gt() |>
  tab_header(
    title = "Model Comparison: Urban Classification in MRP",
    subtitle = "Lower AIC/BIC indicate better model fit"
  ) |>
  fmt_number(
    columns = c(AIC, BIC, "AIC Difference", "BIC Difference"),
    decimals = 1
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )
