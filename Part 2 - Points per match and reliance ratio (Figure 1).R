# ==============================================================================
# Script 2: Season-Level Analysis & Visualization
# Description: Generates Figure 1 (Reliance vs. Points) and performs the
#              Bayesian Multilevel Regression to test the association.
# ==============================================================================

# ---- 1. Setup and Packages ----
library(tidyverse)  # Data manipulation & plotting
library(ggrepel)    # Text labels for plots
library(brms)       # Bayesian regression
library(tidybayes)  # Extracting draws from brms models
library(scales)     # Formatting scales
library(patchwork)  # Combining plots

# ---- 2. Data Loading & Preparation ----
rel_ratio <- read_csv("Dataset/team_ratio_complete2.csv", show_col_types = FALSE) %>%
  mutate(
    league    = str_squish(league),
    Team_name = str_squish(Team_name)
  )

# Function to map final rank to specific groups (Top 4, Top-Mid, etc.)
# Note: League sizes differ (Bundesliga=18, others=20, Championship=24)
rank_group_map <- function(league, final_rank) {
  case_when(
    # Premier League, LaLiga, Serie A (20 teams)
    league %in% c("Premier League", "LaLiga", "Serie A") & final_rank %in% 1:4   ~ "Top 4",
    league %in% c("Premier League", "LaLiga", "Serie A") & final_rank %in% 5:10  ~ "Top-Mid",
    league %in% c("Premier League", "LaLiga", "Serie A") & final_rank %in% 11:16 ~ "Bottom-Mid",
    league %in% c("Premier League", "LaLiga", "Serie A") & final_rank %in% 17:20 ~ "Bottom 4",
    
    # Bundesliga (18 teams)
    league == "Bundesliga"  & final_rank %in% 1:4   ~ "Top 4",
    league == "Bundesliga"  & final_rank %in% 5:9   ~ "Top-Mid",
    league == "Bundesliga"  & final_rank %in% 10:14 ~ "Bottom-Mid",
    league == "Bundesliga"  & final_rank %in% 15:18 ~ "Bottom 4",
    
    # Championship (24 teams)
    league == "Championship" & final_rank %in% 1:4   ~ "Top 4",
    league == "Championship" & final_rank %in% 5:12  ~ "Top-Mid",
    league == "Championship" & final_rank %in% 13:20 ~ "Bottom-Mid",
    league == "Championship" & final_rank %in% 21:24 ~ "Bottom 4",
    
    TRUE ~ NA_character_
  )
}

# Load abbreviations map (ensure this file exists in your Dataset folder)
# If this file is missing, the code will default to the first 3 letters of the team name
if(file.exists("Dataset/team_abbr_template_2024_25.csv")) {
  abbr_map <- read_csv("Dataset/team_abbr_template_2024_25.csv", show_col_types = FALSE)
} else {
  # Create dummy empty tibble if file missing to prevent crash
  abbr_map <- tibble(league = character(), Team_name = character(), abbr = character())
}

# Prepare plotting data
plot_data <- rel_ratio %>%
  mutate(
    mean_points_per_match = total_points / total_games,
    team_abbr_fallback = str_to_upper(str_sub(Team_name, 1, 3)),
    rank_group = rank_group_map(league, final_rank)
  ) %>%
  left_join(abbr_map, by = c("league", "Team_name")) %>%
  mutate(team_abbr = ifelse(!is.na(abbr) & nchar(abbr) > 0, abbr, team_abbr_fallback)) %>%
  mutate(
    league = factor(league, levels = c("Premier League", "LaLiga", "Bundesliga", "Serie A", "Championship")),
    season = factor(season, levels = sort(unique(season), decreasing = TRUE)),
    rank_group = factor(rank_group, levels = c("Top 4", "Top-Mid", "Bottom-Mid", "Bottom 4"))
  ) %>%
  drop_na(rank_group)

# ------------------------------------------------------------------------------
# PART 1: Figure 1 Generation (Visualisation)
# ------------------------------------------------------------------------------
rank_cols <- c("Top 4"="#1b9e77", "Top-Mid"="#d95f02", "Bottom-Mid"="#7570b3", "Bottom 4"="#e7298a")
rank_shapes <- c("Top 4"=16, "Top-Mid"=17, "Bottom-Mid"=15, "Bottom 4"=18)

# Base Plot
g_all_base <- ggplot(plot_data, aes(x = reliance_ratio, y = mean_points_per_match)) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dotted", 
              linewidth = 0.4, colour = "grey55") +
  geom_point(aes(shape = rank_group, colour = rank_group), size = 2.4, stroke = 0.6) +
  geom_text_repel(aes(label = team_abbr, colour = rank_group),
                  size = 2.7, box.padding = 0.22, point.padding = 0.18,
                  segment.size = 0.2, min.segment.length = 0, 
                  max.overlaps = Inf, show.legend = FALSE) +
  scale_colour_manual(values = rank_cols, name = "Final Rank Group") +
  scale_shape_manual(values = rank_shapes, name = "Final Rank Group") +
  scale_x_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, 0.1)) +
  scale_y_continuous(limits = c(0.2, 2.7), breaks = seq(0.5, 2.5, 0.5),
                     expand = expansion(mult = c(0, .02))) +
  labs(x = "Individual Reliance Ratio", y = "Mean Points per Match") +
  facet_grid(rows = vars(season), cols = vars(league)) +
  theme_minimal(base_size = 9.5) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Direction Bars (Bottom Annotation)
dir_row_data <- tibble(league = levels(plot_data$league))
dir_row <- ggplot(dir_row_data) +
  facet_grid(cols = vars(league)) +
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  annotate("segment", x = 0.15, xend = 0.85, y = 0.35, yend = 0.35,
           linewidth = 0.4, arrow = arrow(length = unit(5, "pt"), ends = "both")) +
  annotate("text", x = 0.25, y = 0.68, label = "Team-balanced", size = 3) +
  annotate("text", x = 0.75, y = 0.68, label = "Standout-driven", size = 3) +
  theme(strip.text = element_blank())

# Assemble Final Plot
final_plot <- g_all_base / dir_row +
  plot_layout(heights = c(1, 0.10), guides = "collect") &
  theme(legend.position = "bottom")

# Display Plot
print(final_plot)

# Save
# ggsave("Figures/Figure1_Reliance_Points.png", final_plot, width = 16, height = 9, dpi = 300)

# ------------------------------------------------------------------------------
# PART 2: Bayesian Statistical Modeling
# ------------------------------------------------------------------------------

# Prepare Data for Modeling
team_season <- rel_ratio %>%
  mutate(
    league    = str_squish(league),
    Team_name = str_squish(Team_name),
    team_id   = interaction(league, Team_name, drop = TRUE),
    mean_ppm  = total_points / total_games
  ) %>%
  select(league, season, team_id, reliance_ratio, mean_ppm, final_rank) %>%
  drop_na(reliance_ratio, mean_ppm) %>%
  mutate(
    z_rel      = as.numeric(scale(reliance_ratio)[,1]),
    mean_ppm_z = as.numeric(scale(mean_ppm)[,1])
  )

# Orthogonal polynomial basis (Linear + Curvature)
poly_obj <- poly(team_season$z_rel, degree = 2)
team_season$poly_rel1 <- poly_obj[, 1] # Linear term
team_season$poly_rel2 <- poly_obj[, 2] # Quadratic term

# Set Priors
priors_main <- c(
  set_prior("normal(0, 1)", class = "b"),        # Fixed effects
  set_prior("student_t(3, 0, 0.3)", class = "sd"), # Random effects SDs
  set_prior("student_t(3, 0, 1)", class = "sigma"), # Residual SD
  set_prior("gamma(2, 0.2)", class = "nu")       # Student-t degrees of freedom
)

# Fit Main Model (Reliance vs Points)
fit_ppm <- brm(
  mean_ppm_z ~ poly_rel1 + poly_rel2 +
    (1 + poly_rel1 || league) +  # League intercept + slope (uncorrelated)
    (1 | season) +               # Season intercepts
    (1 | team_id),               # Team intercepts
  data    = team_season,
  family  = student(),
  prior   = priors_main,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  chains  = 4, iter = 4000, warmup = 1000, seed = 77,
  file    = "Models/fit_ppm_main" # Caches the model to avoid re-running
)

# ------------------------------------------------------------------------------
# PART 3: Model Reporting
# ------------------------------------------------------------------------------

# 1. Fixed Effects Summary (Linear + Curvature)
fx_summ <- spread_draws(fit_ppm, b_poly_rel1, b_poly_rel2) %>%
  summarise(
    lin_mean    = mean(b_poly_rel1),
    lin_l95     = quantile(b_poly_rel1, .025),
    lin_u95     = quantile(b_poly_rel1, .975),
    prob_pos    = mean(b_poly_rel1 > 0), # Probability slope is positive
    curv_mean   = mean(b_poly_rel2),
    curv_l95    = quantile(b_poly_rel2, .025),
    curv_u95    = quantile(b_poly_rel2, .975)
  )
print(fx_summ)

# 2. Bayesian R-squared
r2_full <- bayes_R2(fit_ppm)
print(r2_full)

# 3. Variance Components
draws <- as_draws_df(fit_ppm)
var_comp <- tibble(
  team_var   = mean(draws$sd_team_id__Intercept^2),
  league_var = mean(draws$sd_league__Intercept^2),
  season_var = mean(draws$sd_season__Intercept^2),
  resid_var  = mean(draws$sigma^2)
)
print(var_comp)