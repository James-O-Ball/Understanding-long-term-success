library(tidyverse)
library(ggrepel)
library(brms)
library(ggplot2)
library(tidybayes)
library(scales)
library(zoo)
library(patchwork)
library(lubridate)
# -------------------------------------------------------------------
# Load + prep
# -------------------------------------------------------------------
rel_ratio <- read.csv("Dataset/team_ratio_complete2.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(
    league    = stringr::str_squish(league),
    Team_name = stringr::str_squish(Team_name)
  )

# Helper to build league-specific final-rank groups (as you had)
rank_group_map <- function(league, final_rank){
  dplyr::case_when(
    league %in% c("Premier League","LaLiga","Serie A") & final_rank %in% 1:4   ~ "Top 4",
    league %in% c("Premier League","LaLiga","Serie A") & final_rank %in% 5:10  ~ "Top-Mid",
    league %in% c("Premier League","LaLiga","Serie A") & final_rank %in% 11:16 ~ "Bottom-Mid",
    league %in% c("Premier League","LaLiga","Serie A") & final_rank %in% 17:20 ~ "Bottom 4",
    league == "Bundesliga"  & final_rank %in% 1:4   ~ "Top 4",
    league == "Bundesliga"  & final_rank %in% 5:9   ~ "Top-Mid",
    league == "Bundesliga"  & final_rank %in% 10:14 ~ "Bottom-Mid",
    league == "Bundesliga"  & final_rank %in% 15:18 ~ "Bottom 4",
    league == "Championship" & final_rank %in% 1:4   ~ "Top 4",
    league == "Championship" & final_rank %in% 5:12  ~ "Top-Mid",
    league == "Championship" & final_rank %in% 13:20 ~ "Bottom-Mid",
    league == "Championship" & final_rank %in% 21:24 ~ "Bottom 4",
    TRUE ~ NA_character_
  )
}

# ----- palette + shapes (consistent across facets)
rank_cols <- c("Top 4"="#1b9e77","Top-Mid"="#d95f02","Bottom-Mid"="#7570b3","Bottom 4"="#e7298a")
rank_shapes <- c("Top 4"=16,"Top-Mid"=17,"Bottom-Mid"=15,"Bottom 4"=18)

# Team abbreviations (now covering all seasons)
abbr_map <- read.csv("team_abbr_template_2024_25.csv") %>% as_tibble()

# Build plotting data for ALL seasons
plot_data <- rel_ratio %>%
  mutate(
    mean_points_per_match = total_points / total_games,
    team_abbr_fallback = str_to_upper(str_sub(Team_name, 1, 3)),
    rank_group = rank_group_map(league, final_rank)
  ) %>%
  left_join(abbr_map, by = c("league","Team_name")) %>%         # if your abbr file has `season`, add it to the join
  mutate(team_abbr = ifelse(!is.na(abbr) & nchar(abbr) > 0, abbr, team_abbr_fallback)) %>%
  mutate(
    # order facets
    league = factor(league, levels = c("Premier League","LaLiga","Bundesliga","Serie A","Championship")),
    # Put latest season on the TOP row; adjust ordering to your preference
    season = factor(season, levels = sort(unique(season), decreasing = TRUE)),
    rank_group = factor(rank_group, levels = c("Top 4","Top-Mid","Bottom-Mid","Bottom 4"))
  ) %>%
  drop_na(rank_group)

# -------------------------------------------------------------------
# Arrow annotations (top row only = latest season)
# -------------------------------------------------------------------
top_season <- levels(plot_data$season)[1]  # first level because we set decreasing=TRUE

arrow_df <- expand.grid(
  league = levels(plot_data$league),
  season = top_season,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(x = 0.22, xend = 0.78, y = 2.62, y_label = 2.69)

label_df <- bind_rows(
  arrow_df %>% transmute(league, season, x = 0.28, y = y_label, lab = "Team-balanced matches"),
  arrow_df %>% transmute(league, season, x = 0.72, y = y_label, lab = "Standout-driven matches")
)

# -------------------------------------------------------------------
# Faceted plot: 3 (season) × 5 (league)
# -------------------------------------------------------------------
set.seed(123)
g_all_base <- ggplot(plot_data, aes(x = reliance_ratio, y = mean_points_per_match)) +
  # light dotted trend line per facet (season × league)
  geom_smooth(
    aes(x = reliance_ratio, y = mean_points_per_match),
    method = "lm", se = FALSE, linetype = "dotted",
    linewidth = 0.4, colour = "grey55",
    inherit.aes = FALSE
  ) +
  geom_point(aes(shape = rank_group, colour = rank_group), size = 2.4, stroke = 0.6) +
  ggrepel::geom_text_repel(
    aes(label = team_abbr, colour = rank_group),
    size = 2.7, box.padding = 0.22, point.padding = 0.18,
    segment.size = 0.2, min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE
  ) +
  scale_colour_manual(values = rank_cols, name = "Final Rank Group") +
  scale_shape_manual(values = rank_shapes,  name = "Final Rank Group") +
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
    axis.ticks.length = unit(3.5, "pt"),
    strip.text = element_text(face = "bold"),
    panel.spacing.x = unit(8, "pt"),
    panel.spacing.y = unit(10, "pt"),
    legend.position = "bottom",                # move legend
    legend.box = "horizontal",
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 9),
    plot.margin = margin(6, 8, 2, 4)
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 3)),
    shape  = guide_legend(nrow = 1, byrow = TRUE)
  )

# ---- Direction bars: one per league column ----
dir_row_data <- tibble(league = levels(plot_data$league))

dir_row <- ggplot(dir_row_data) +
  facet_grid(cols = vars(league)) +     # aligns with the 5 main columns
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  annotate(
    "segment", x = 0.15, xend = 0.85, y = 0.35, yend = 0.35,
    linewidth = 0.4, arrow = arrow(length = unit(5, "pt"), ends = "both")
  ) +
  annotate("text", x = 0.25, y = 0.68, label = "Team-balanced matches", size = 3) +
  annotate("text", x = 0.75, y = 0.68, label = "Standout-driven matches", size = 3) +
  theme(
    strip.text = element_blank(),       # no facet labels on this row
    panel.spacing.x = unit(8, "pt"),
    plot.margin = margin(0, 8, 6, 8)
  )

# ---- Stack main plot over the direction bar; legend collected at bottom ----
final_plot <- g_all_base / dir_row +
  plot_layout(heights = c(1, 0.10), guides = "collect") &
  theme(legend.position = "bottom")

final_plot

# save a wide figure for manuscript
# ggsave("fig1_reliance_points.png", final_plot, width = 16, height = 9, dpi = 300)


# -------------------------------------------------------------------
# Build the team-season table
# -------------------------------------------------------------------
team_season <- rel_ratio %>%
  mutate(
    league     = stringr::str_squish(league),
    Team_name  = stringr::str_squish(Team_name),
    team_id    = interaction(league, Team_name, drop = TRUE),  # unique per league
    mean_ppm   = total_points / total_games
  ) %>%
  select(league, season, team_id, reliance_ratio, mean_ppm, final_rank) %>%
  drop_na(reliance_ratio, mean_ppm) %>%
  mutate(
    z_rel      = as.numeric(scale(reliance_ratio)[,1]),
    mean_ppm_z = as.numeric(scale(mean_ppm)[,1])
  )

# Orthogonal polynomial basis (kills collinearity of raw quadratic)
# Keep the poly object to reuse its attributes for any future predictions
poly_obj <- poly(team_season$z_rel, degree = 2)  # orthogonal basis
team_season$poly_rel1 <- poly_obj[, 1]
team_season$poly_rel2 <- poly_obj[, 2]

# -------------------------------------------------------------------
# Bayesian multilevel regression (final model)
# -------------------------------------------------------------------
priors2 <- c(
  set_prior("normal(0, 1)", class = "b"),           # fixed effects
  set_prior("student_t(3, 0, 0.3)", class = "sd"),    # RE sds on ~unit scale
  set_prior("student_t(3, 0, 1)", class = "sigma"), # residual scale
  set_prior("gamma(2, 0.2)", class = "nu")         # discourages ultra-heavy tails
)

fit_ppm_v3 <- brm(
  mean_ppm_z ~ poly_rel1 + poly_rel2 +
    (1 + poly_rel1 || league) +  # league intercept + slope (uncorrelated)
    (1 | season) +               # season intercepts
    (1 | team_id),               # team intercepts (already nested via id)
  data    = team_season,
  family  = student(),
  prior   = priors2,
  control = list(adapt_delta = 0.999, max_treedepth = 15),
  chains  = 4, iter = 4000, warmup = 1000, seed = 77
)

# -------------------------------------------------------------------
# Summaries to report
# -------------------------------------------------------------------
# Fixed effects: linear (poly_rel1) and curvature (poly_rel2)
fx_summ <- spread_draws(fit_ppm_v3, b_poly_rel1, b_poly_rel2) %>%
  summarise(
    lin_mean    = mean(b_poly_rel1),
    lin_l95     = quantile(b_poly_rel1, .025),
    lin_u95     = quantile(b_poly_rel1, .975),
    pr_lin_gt0  = mean(b_poly_rel1 > 0),
    curv_mean   = mean(b_poly_rel2),
    curv_l95    = quantile(b_poly_rel2, .025),
    curv_u95    = quantile(b_poly_rel2, .975)
  )
fx_summ

# Model fit
bayes_R2(fit_ppm_v3)
# Marginal R² (fixed effects only; re_formula = NA)
bayes_R2(fit_ppm_v3, re_formula = NA)

# Posterior predictive check (quick visual)
pp_check(fit_ppm_v3, ndraws = 200)

dr <- as_draws_df(fit_ppm_v3)

# Variance components from the random effects
v_team    <- dr$sd_team_id__Intercept^2
v_league  <- dr$sd_league__Intercept^2          # league intercept variance
v_season  <- dr$sd_season__Intercept^2
v_resid   <- dr$sigma^2

# (optional) league slope variance for poly_rel1
v_league_slope <- dr$sd_league__poly_rel1^2

# Posterior ICC for team
icc_team <- v_team / (v_team + v_league + v_season + v_resid)
quantile(icc_team, c(.025, .5, .975))

# Quick summaries you might report
post_summ <- function(x) c(mean = mean(x), l95 = quantile(x, .025), u95 = quantile(x, .975))
post_summ(v_team); post_summ(v_league); post_summ(v_season); post_summ(v_resid)
post_summ(v_league_slope)


##### Checking final rank ####
# # ---- 1) Build analysis table (team-seasons) ----
# team_season <- rel_ratio %>%
#   mutate(
#     team_id    = interaction(league, Team_name, drop = TRUE),  # unique per league
#     mean_ppm   = total_points / total_games,
#     rank_group = rank_group_map(league, final_rank)
#   ) %>%
#   select(league, season, team_id, Team_name, final_rank, rank_group,
#          reliance_ratio, mean_ppm) %>%
#   drop_na(reliance_ratio, mean_ppm, rank_group)
# 
# # Sanity check: rank-group counts by league
# chk_counts <- team_season %>% count(league, rank_group) %>% arrange(league, rank_group)
# print(chk_counts)
# 
# # Center/scale for stable priors; league×season key for random slopes
# team_season <- team_season %>%
#   mutate(
#     z_rel       = as.numeric(scale(reliance_ratio)[,1]),
#     mean_ppm_z  = as.numeric(scale(mean_ppm)[,1]),
#     ls_group    = interaction(league, season, drop = TRUE),
#     rank_group  = factor(rank_group, levels = c("Top 4","Top-Mid","Bottom-Mid","Bottom 4"))
#   )
# 
# # Orthogonal polynomial basis for z_rel (linear + curvature)
# poly_obj <- poly(team_season$z_rel, degree = 2)
# team_season$poly_rel1 <- poly_obj[,1]   # linear (orthogonal)
# team_season$poly_rel2 <- poly_obj[,2]   # quadratic (orthogonal)
# 
# # ---- 2) Priors, save_pars, and controls ----
# priors <- c(
#   set_prior("normal(0, 1)", class = "b"),              # fixed effects ~ N(0,1) on z-scale
#   set_prior("student_t(3, 0, 0.3)", class = "sd"),     # RE SDs
#   set_prior("student_t(3, 0, 1)",   class = "sigma"),  # residual SD
#   set_prior("gamma(2, 0.2)",        class = "nu")      # Student df, discourages ultra-heavy tails
# )
# sp <- save_pars(all = TRUE)  # <-- needed for PSIS-LOO moment matching
# 
# ctrl <- list(adapt_delta = 0.999, max_treedepth = 15)
# 
# # ---- 3) Base multilevel model (no rank-group interaction) ----
# fit_base <- brm(
#   mean_ppm_z ~ poly_rel1 + poly_rel2 +
#     (1 | team_id) + (1 | season) +
#     (1 + poly_rel1 || ls_group),                     # random slope by league×season
#   data = team_season, family = student(),
#   prior = priors, save_pars = sp, control = ctrl,
#   chains = 4, iter = 4000, warmup = 1000, seed = 77
# )
# 
# # ---- 4) Interaction model (reliance × final-rank group) ----
# fit_interact <- brm(
#   mean_ppm_z ~ poly_rel1*rank_group + poly_rel2 +
#     (1 | team_id) + (1 | season) +
#     (1 + poly_rel1 || ls_group),
#   data = team_season, family = student(),
#   prior = priors, save_pars = sp, control = ctrl,
#   chains = 4, iter = 4000, warmup = 1000, seed = 88
# )
# 
# # If you saw even a single divergence, you can tighten and re-update:
# fit_interact <- update(fit_interact, control = list(adapt_delta = 0.9999, max_treedepth = 15),
#                        recompile = FALSE, seed = 88)
# 
# # ---- 5) PSIS-LOO with moment matching (stable) ----
# loo_base_mm     <- loo(fit_base,     moment_match = TRUE)
# loo_interact_mm <- loo(fit_interact, moment_match = TRUE)
# lc <- loo_compare(loo_base_mm, loo_interact_mm)
# print(lc)
# 
# # Grab ΔELPD and its SE
# d_elpd <- as.numeric(loo_interact_mm$estimates["elpd_loo", "Estimate"] -
#                        loo_base_mm$estimates["elpd_loo", "Estimate"])
# se_elpd <- sqrt(loo_interact_mm$estimates["elpd_loo","SE"]^2 +
#                   loo_base_mm$estimates["elpd_loo","SE"]^2) # conservative
# 
# # ---- 6) Global linear + curvature effects ----
# fx <- tidybayes::spread_draws(fit_interact, b_poly_rel1, b_poly_rel2) %>%
#   summarise(
#     lin_mean = mean(b_poly_rel1),
#     lin_l95  = quantile(b_poly_rel1, .025),
#     lin_u95  = quantile(b_poly_rel1, .975),
#     curv_mean= mean(b_poly_rel2),
#     curv_l95 = quantile(b_poly_rel2, .025),
#     curv_u95 = quantile(b_poly_rel2, .975)
#   )
# print(fx)
# 
# # ---- 7) League×season random-slope variance (poly_rel1) ----
# dr   <- posterior::as_draws_df(fit_interact)
# v_ls  <- dr$sd_ls_group__poly_rel1^2
# v_sum <- setNames(
#   c(mean(v_ls), quantile(v_ls, .025), quantile(v_ls, .975)),
#   c("mean","l95","u95")
# )
# v_sum
# 
# # Optional overall R2
# r2_full <- bayes_R2(fit_interact)
# r2_fix  <- bayes_R2(fit_interact, re_formula = NA)
# print(r2_full); print(r2_fix)
# 
# # ---- 8) Rank-group specific slopes (robust extraction) ----
# draws <- posterior::as_draws_df(fit_interact)
# b_lin <- draws[["b_poly_rel1"]]
# 
# get_col <- function(pattern){
#   nm <- grep(pattern, colnames(draws), value = TRUE)
#   if (length(nm) == 0) return(rep(0, nrow(draws)))
#   draws[[nm[1]]]
# }
# 
# # handle punctuation variants converted by brms to dots
# int_topmid <- get_col("^b_.*poly_rel1.*rank_groupTop[\\.-]?Mid$")
# int_botmid <- get_col("^b_.*poly_rel1.*rank_groupBottom[\\.-]?Mid$")
# int_bot4   <- get_col("^b_.*poly_rel1.*rank_groupBottom[\\.-]?4$")
# 
# slope_top4   <- b_lin
# slope_topmid <- b_lin + int_topmid
# slope_botmid <- b_lin + int_botmid
# slope_bot4   <- b_lin + int_bot4
# 
# sum_slope <- function(x) tibble(mean=mean(x), l95=quantile(x,.025), u95=quantile(x,.975), pr_gt0=mean(x>0))
# fx_int <- bind_rows(
#   sum_slope(slope_top4)   |> mutate(group="Top 4"),
#   sum_slope(slope_topmid) |> mutate(group="Top-Mid"),
#   sum_slope(slope_botmid) |> mutate(group="Bottom-Mid"),
#   sum_slope(slope_bot4)   |> mutate(group="Bottom 4")
# ) |> relocate(group) |> mutate(across(where(is.numeric), ~round(.x, 3)))
# print(fx_int)
# 
# # ---- 9) Optional: per-panel OLS slopes ± 95% CI (aligns with the figure) ----
# # Slope scaled per 0.1 reliance for interpretability
# panel_data <- team_season %>% mutate(rel10 = reliance_ratio/0.1, mean_ppm = mean_ppm)
# 
# panel_stats <- panel_data %>%
#   group_by(league, season) %>%
#   do({
#     m  <- lm(mean_ppm ~ rel10, data = .)
#     est <- broom::tidy(m, conf.int = TRUE) %>% filter(term == "rel10")
#     tibble(slope_pts_per_0p1 = est$estimate,
#            ci_low = est$conf.low,
#            ci_high = est$conf.high,
#            r2 = broom::glance(m)$r.squared)
#   }) %>% ungroup()
# 
# print(panel_stats %>% arrange(league, season))
# 
# # ---- 10) Quick numbers to drop into the manuscript ----
# round3 <- function(x) format(round(x, 3), nsmall = 3)
# cat("\nΔELPD (interact − base): ", round3(d_elpd), " (SE ", round3(se_elpd), ")\n", sep = "")
# cat("Global linear mean (95% CrI): ", round3(fx$lin_mean), " [", round3(fx$lin_l95), ", ", round3(fx$lin_u95), "]\n", sep = "")
# cat("Global curvature mean (95% CrI): ", round3(fx$curv_mean), " [", round3(fx$curv_l95), ", ", round3(fx$curv_u95), "]\n", sep = "")
# cat("Var(league×season slope) mean (95% CrI): ", round3(v_sum["mean"]), " [", round3(v_sum["l95"]), ", ", round3(v_sum["u95"]), "]\n", sep = "")
