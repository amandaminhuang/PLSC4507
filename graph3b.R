library(tidyverse)
library(dplyr)
library(ccesMRPprep)
library(stringr)
library(glue)


#Each model predicts the proportion of each racial group electorate in each congressional district that is registered Republicans.
#Statistics in each facet show Root-Mean-Square Error across districts and the Mean Error (estimate minus truth) across 27 districts.
#Uncertainty intervals are 80% credible intervals. 

mrp_fullpost <- read_csv("data/mrp-full-posterior.csv.gz")

# racial gap = white - other races


rvg <- mrp_fullpost |>
  group_by(year, cd, iter) |>
  summarize(
    w_nw = sum(p_mrp_twway * (race == "White")) -
      (sum(p_mrp_twway * (race != "White") * N) / sum((race != "White") * N)),
    
    # poststrat
    w_b = sum(p_mrp_twway * (race == "White")) - 
      sum(p_mrp_twway * (race == "Black")),
    
    w_h = sum(p_mrp_twway * (race == "White")) - 
      sum(p_mrp_twway * (race == "Hispanic")),
    
    w_o = sum(p_mrp_twway * (race == "White")) - 
      sum(p_mrp_twway * (race == "Other")),  # Add this
    
    .groups = "drop"
  )

rvg_summary <- rvg |>
  pivot_longer(-c(year, cd, iter), 
               names_to = "race", 
               values_to = "diff") |>
  group_by(year, cd, race) |>
  summarize(
    diff_mean = mean(diff),
    diff_se = sd(diff),
    diff_q10 = quantile(diff, 0.10),
    diff_q90 = quantile(diff, 0.90),
    
    .groups = "drop"
  )

# Districts are sorted first by Division in facets (from West to East), 
# then by state (alphabetical within Division)
# then by district number. 
#States are annotated and alternatively colored black and gray for visual clarity.

rvg_plot <- rvg_summary |>
  mutate(st = str_sub(cd, 1, 2)) |>
  left_join(select(ccesMRPprep::states_key, st, division, region)) |>
  group_by(division, st) |>
  arrange(region, division, st, cd) |>
  mutate(st_order = cur_group_id()) |>
  mutate(division = str_replace_all(division, "\\s", "\n")) |>
  ungroup() |>
  mutate(st_color = st_order %% 2 == 0) |>
  mutate(cd = fct_inorder(cd),
         division = fct_rev(fct_inorder(division)))  

# plot
plot_rvg <- function(tbl, show, race_labs) {
  plot_dat <- filter(tbl, race == show, year == 2020)

  plot_dat <- plot_dat |>
    group_by(division) |>
    mutate(cd_in_facet = 1:n()) |>
    ungroup()
  
  state_labs <- plot_dat |>
    group_by(division, st, st_color) |>
    summarize(
      x = mean(cd_in_facet),
      y = quantile(diff_q90, 0.8),
      n = n(),
      .groups = "drop"
    )
  
  
  ggplot(plot_dat, aes(x = cd, y = diff_mean, color = st_color)) +  
    geom_hline(yintercept = 0, color = "red") +
    geom_errorbar(aes(ymin = diff_q10, ymax = diff_q90), 
                  width = 0, alpha = 0.3) +
    geom_point(size = 0.5) +
    geom_text(data = state_labs, 
              aes(x = x, y = y, label = st, size = n)) +
    facet_grid(~ division, scales = "free_x", space = "free") +
    scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "gray")) + 
    scale_x_discrete(labels = NULL) +
    scale_y_continuous(labels = scales::percent) +
    coord_cartesian(ylim = c(-0.07, 0.83)) +
    guides(color = "none", size = "none") +  
    labs(y = glue("White minus {race_labs} Trump Vote"), x = NULL) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank())
}

plot_nw <- plot_rvg(rvg_plot, "w_nw", "Non-White Voters") 
plot_wb <- plot_rvg(rvg_plot, "w_b", "Black Voters")
plot_wh <- plot_rvg(rvg_plot, "w_h", "Hispanic Voters")
plot_other <- plot_rvg(rvg_plot, "w_o", "Other Voters")

ggsave("figures/graph3b.pdf", 
       plot = plot_nw)
