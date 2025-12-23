library(tidyverse)
library(ccesMRPprep)
library(gt)
library(sf)
library(scales)
library(glue)
library(ggplot2)



# from before
est_original <- read_csv("data/mrp-ests_by-cd-race.csv")
urban_class_cd <- read_csv("data/rural-urban-sub-classification.csv")
cd_shp <- read_rds("data/shp_cd.rds")
st_shp <- read_rds("data/shp_st.rds")

# cleaning
urban_class <- urban_class_cd |>
  rename(cd = District, urban_class = "Most Prevalent") |>
  filter(stringr::str_detect(cd, "-")) |>
  select(cd, urban_class) |>
  mutate(urban_class = factor(urban_class))

# Merge with urban/rural
combined <- est_original |>
  left_join(urban_class, by = "cd") |>
  filter(!is.na(urban_class))


### ----------- Analysis -------------------------------------

# racial voting patterns by urban/rural type
summary_by_urbanrural <- combined |>
  filter(year == 2020, race != "Other") |>
  group_by(urban_class, race) |>
  summarize(
    mean_trump = mean(p_mrp_twway),
    sd_trump = sd(p_mrp_twway),
    n_districts = n(),
    .groups = "drop"
  )

# Racial gaps by urban/rural
racial_gaps <- combined |>
  filter(year == 2020) |>
  group_by(cd, urban_class) |>
  summarize(
    w_vote = p_mrp_twway[race == "White"],
    nw_vote = mean(p_mrp_twway[race != "White" & race != "Other"]),
    racial_gap = w_vote - nw_vote,
    .groups = "drop"
  )

### ------------------- Plot -------------------------------------------


# Trump Vote 
p_vote <- combined |>
  filter(year == 2020, race %in% c("White", "Black", "Hispanic")) |>
  mutate(
    urban_class = factor(
      urban_class,
      levels = c("Urban", "Suburban", "Rural")
    ),
    race = factor(
      race,
      levels = c("White", "Black", "Hispanic")
    )
  ) |>
  ggplot(aes(x = urban_class, 
             y = p_mrp_twway, 
             fill = race)) +
  
  geom_boxplot(
    width = 0.5,
    position = position_dodge(width = 0.7),
    alpha = 0.85
  ) +
  
  scale_fill_manual(
    values = c(
      "White"    = "#4E79A7",
      "Black"    = "#E15759",
      "Hispanic" = "#59A14F"
    )
  ) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = NULL,
    y = "Predicted Trump Vote Share",
    fill = "Race",
    title = "Predicted Trump Vote Share by Race and Urban Context (2020)",
    subtitle = "Estimates from MRP model"
  ) +
  theme_minimal() +
  
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11)
  )

ggsave(
  filename = "figures/figure2.pdf",
  plot = p_vote,
  width = 8,
  height = 8,
  dpi = 300)


### ---------- map ----------------
gap_cd_plot <- cd_shp |>
  left_join(racial_gaps, by = "cd") |>
  filter(!is.na(urban_class)) |>
  ggplot(aes(fill = racial_gap)) +
  facet_wrap(~ urban_class, nrow = 1) +
  geom_sf(color = "white", size = 0.1) +
  geom_sf(data = st_shp, fill = "transparent", size = 0.3) +
  scale_fill_fermenter(
    palette = "OrRd",
    limits = c(0, .70),
    breaks = seq(.10, .70, by = 0.10),
    labels = unit_format(scale = 100, unit = "pp"),
    direction = 1
  ) +
  ggthemes::theme_map() +
  labs(
    fill = "White – Nonwhite Vote Gap (pp)",
    title = "Racial Voting Gaps Across Urban–Rural Contexts"
  ) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.5, "in"),
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),

    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ))

ggsave(
  filename = "figures/figure3.pdf",
  plot = gap_cd_plot,
  width = 8,
  height = 8,
  dpi = 300
)
