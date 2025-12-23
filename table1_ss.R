library(tidyverse)
library(gt)
library(scales)
library(glue)

# Load data
est <- read_csv("data/mrp-ests_by-cd-race.csv")
urban_class <- read_csv("data/rural-urban-sub-classification.csv") |>
  rename(cd = District, classes = `Most Prevalent`) |>
  filter(str_detect(cd, "-")) |>
  select(cd, classes)

# Clean data by year, all cd, etc
data_full <- est |>
  filter(year == 2020) |>
  left_join(urban_class, by = "cd") |>
  filter(!is.na(classes)) |>
  mutate(st = str_sub(cd, 1, 2)) |>
  left_join(select(ccesMRPprep::states_key, st, state, region, division), by = "st")

calc_voting_stats <- function(data, ...) {
  data |>
    group_by(..., classes) |>
    summarize(
      w_mean = mean(p_mrp_twway[race == "White"], na.rm = TRUE),
      w_se = sd(p_mrp_twway[race == "White"], na.rm = TRUE) / sqrt(sum(race == "White")),
      b_mean = mean(p_mrp_twway[race == "Black"], na.rm = TRUE),
      b_se = sd(p_mrp_twway[race == "Black"], na.rm = TRUE) / sqrt(sum(race == "Black")),
      h_mean = mean(p_mrp_twway[race == "Hispanic"], na.rm = TRUE),
      h_se = sd(p_mrp_twway[race == "Hispanic"], na.rm = TRUE) / sqrt(sum(race == "Hispanic")),
      rvg_mean = mean(p_mrp_twway[race == "White"], na.rm = TRUE) - 
        mean(p_mrp_twway[race != "White"], na.rm = TRUE),
      rvg_se = sd(p_mrp_twway[race == "White"] - mean(p_mrp_twway[race != "White"], na.rm = TRUE)) / 
        sqrt(sum(race == "White")),
      .groups = "drop"
    )
}

# National & Region (4 Parts)
national_avg <- calc_voting_stats(data_full) |> 
  mutate(region = "National Average")

regional_avg <- calc_voting_stats(data_full, region)

# by Z-Score
add_significance <- function(regional, national) {
  
  vars <- c("w", "b", "h", "rvg")
  
  regional_sig <- regional |>
    left_join(
      national |> select(classes, ends_with("_mean")) |> 
        rename_with(~paste0(., "_nat"), ends_with("_mean")),
      by = "classes"
    )
  
  for (var in vars) {
    regional_sig <- regional_sig |>
      mutate(
        "{var}_z" := (!!sym(paste0(var, "_mean")) - !!sym(paste0(var, "_mean_nat"))) / 
          !!sym(paste0(var, "_se")),
        "{var}_sig" := case_when(
          abs(!!sym(paste0(var, "_z"))) > 2.58 ~ "**",
          abs(!!sym(paste0(var, "_z"))) > 1.96 ~ "*",
          TRUE ~ ""
        )
      )
  }
  
  regional_sig
}

regional_with_sig <- add_significance(regional_avg, national_avg)

combined <- bind_rows(
  national_avg |> select(-ends_with("_se")),
  regional_with_sig |> select(region, classes, ends_with("_mean"), ends_with("_sig"))
) |>
  mutate(region = fct_relevel(region, "National Average"))

### -------- PLOT ------------------------------------------------------------------

display_table <- combined |>
  mutate(
    White = if_else(region == "National Average",
                    sprintf("%.1f%%", w_mean * 100),
                    sprintf("%.1f%% %s", w_mean * 100, w_sig)),
    Black = if_else(region == "National Average",
                    sprintf("%.1f%%", b_mean * 100),
                    sprintf("%.1f%% %s", b_mean * 100, b_sig)),
    Hispanic = if_else(region == "National Average",
                       sprintf("%.1f%%", h_mean * 100),
                       sprintf("%.1f%% %s", h_mean * 100, h_sig)),
    Gap = if_else(region == "National Average",
                  sprintf("%.1f%%", rvg_mean * 100),
                  sprintf("%.1f%% %s", rvg_mean * 100, rvg_sig))
  ) |>
  select(region, classes, White, Black, Hispanic, Gap)

gt_table <- display_table |>
  gt(groupname_col = "region") |>
  tab_header(
    title = "Trump Vote by Race: Urban Context Across Regions (2020)",
    subtitle = "Stars indicate significant difference from national average: * p<0.05, ** p<0.01"
  ) |>
  cols_label(
    classes = "Urban Type",
    White = "White",
    Black = "Black",
    Hispanic = "Hispanic",
    Gap = "W-NW Gap"
  ) |>
  tab_style(
    style = list(cell_fill(color = "#FFE6E6"), cell_text(weight = "bold")),
    locations = cells_row_groups(groups = "National Average")
  ) |>
  tab_options(
    table.font.size = 12,
    heading.title.font.size = 14
  )

gt_table
gtsave(gt_table, "figures/summarytable1.pdf",
       vwidth = 700,
       vheight = 800,
       expand = 0)
