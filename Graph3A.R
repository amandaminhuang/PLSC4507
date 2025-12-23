library(sf)
library(janitor)
library(tidyverse)
library(scales)
library(patchwork)
library(haven)

dat <- read_dta("data/cces_by-agg-level.dta")
cd <- read_rds("data/shp_cd.rds")
st <- read_rds("data/shp_st.rds")

# clean data 

dat <- dat |>
  mutate(
    race = haven::as_factor(race),
    race = case_when(
      race == "White" ~ "white",
      race == "All Non-Whites" ~ "nonwhite",
      race == "Black" ~ "black",
      race == "Hispanic" ~ "hispanic",
      TRUE ~ NA
    ),
    race = factor(race, levels = c(
      "white",
      "nonwhite",
      "black",
      "hispanic"
      )),
    division = as_factor(division),
    region = as_factor(region)
    )

race_gap <- dat |>
  filter(
    race %in% c("white", "nonwhite"),
    level == "cd"
  ) |>
  pivot_wider(
    id_cols = c(year, cd), 
    names_from = race,
    values_from = p_mrp_twway
  ) |>
  mutate(rvg_pp = (white - nonwhite)*100)


map_data <- cd |> 
  left_join(race_gap, by = "cd") |> 
  filter(year == 2020)

# sort(race_gap$rvg_pp) --> range from 70.64 to 3.8

map_data |>
  ggplot(aes(fill = rvg_pp)) +
  geom_sf(color = "white", size = 0.01) +
  scale_fill_fermenter( 
    palette   = "YlOrRd", # RColorBrewer
    limits    = c(-0, 80),
    breaks    = seq(0, 80, by = 10),
    labels    = function(x) paste0(x, " pp"),
    direction = -1
  ) +
  labs(
    fill = "White – Nonwhite\nVote Gap (pp)",
    title = "Racial Vote Gap by Congressional District (2020)"
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position = "right"
  )

