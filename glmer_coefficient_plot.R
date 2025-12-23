library(broom.mixed)
library(ggplot2)
library(tidyverse)

# Load models -- downloaded from Google Colab
fit_noUrban <- read_rds("data/glmer_noUrban.rds")
fit_withUrban <- read_rds("data/glmer_withUrban.rds")

# extract fixed effects
coef_noUrban <- tidy(fit_noUrban, effects = "fixed", conf.int = TRUE) %>%
  mutate(model = "Without Urban")

coef_withUrban <- tidy(fit_withUrban, effects = "fixed", conf.int = TRUE) %>%
  mutate(model = "With Urban")

# Combine
coefs_all <- bind_rows(coef_noUrban, coef_withUrban) %>%
  filter(!str_detect(term, "Intercept"))

# ------------------------------------------------ PLOT ------------------------------------------------
selected_vars <- c(
  "raceOther", "raceHispanic", "raceBlack",
  "race_Other:pct_raceother_elec",
  "race_Hispanic:pct_hispanic_elec",
  "race_Black:pct_black_elec",
  "pct_white_elec",
  "female",
  "educSome College",
  "educPost-Grad",
  "educ4-Year"
)

coefs_selected <- coefs_all %>%
  filter(term %in% selected_vars)

comparison_plot <- ggplot(coefs_selected, 
       aes(x = estimate,
           y = term, 
           color = model)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(0.5), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Coefficient Comparison between Models",
    x = "Coefficient Estimate (log-odds)",
    y = "Predictor",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c(
      "Without Urban" = "skyblue",
      "With Urban"    = "forestgreen"
    )
  ) +
  scale_y_discrete(labels = c(
    "raceBlack"                    = "Black",
    "raceHispanic"                 = "Hispanic",
    "raceOther"                     = "Other",
    "race_Black:pct_black_elec"     = "Black × % Black",
    "race_Hispanic:pct_hispanic_elec" = "Hispanic × % Hispanic",
    "race_Other:pct_raceother_elec"   = "Other × % Other",
    "pct_white_elec"                = "% White",
    "female"                        = "Female",
    "educSome College"               = "Some College",
    "educ4-Year"                     = "4-Year Degree",
    "educPost-Grad"                  = "Post-Grad"
  ))
