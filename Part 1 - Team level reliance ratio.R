# ============================================================
# Study 3: Reliance ratio pipeline
# ============================================================

# ---- Packages ----
library(tidyverse)
library(ggplot2)
library(scales)
library(zoo)
library(patchwork)
library(lubridate)
library(lme4)
library(lmerTest)   # p-values in summary(glmer)
library(ggeffects)  # marginal effects / predicted probs
library(splines)    # optional spline for non-linear time trend
library(emmeans)

# ---- Load league-season CSVs ----
prem_24_25 <- read.csv("Dataset/WhoScored/Prem_24_25.csv", header = TRUE, stringsAsFactors = FALSE)
prem_23_24 <- read.csv("Dataset/WhoScored/Prem_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
prem_22_23 <- read.csv("Dataset/WhoScored/Prem_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_24_25 <- read.csv("Dataset/WhoScored/LaLiga_24_25.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_23_24 <- read.csv("Dataset/WhoScored/LaLiga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
laliga_22_23 <- read.csv("Dataset/WhoScored/Laliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
bund_24_25 <- read.csv("Dataset/WhoScored/Bundesliga_24_25.csv", header = TRUE, stringsAsFactors = FALSE)
bund_23_24 <- read.csv("Dataset/WhoScored/Bundesliga_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
bund_22_23 <- read.csv("Dataset/WhoScored/Bundesliga_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
seriea_24_25 <- read.csv("Dataset/WhoScored/Seriea_24_25.csv", header = TRUE, stringsAsFactors = FALSE)
seriea_23_24 <- read.csv("Dataset/WhoScored/Seriea_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
seriea_22_23 <- read.csv("Dataset/WhoScored/Seriea_22_23.csv", header = TRUE, stringsAsFactors = FALSE)
champ_24_25 <- read.csv("Dataset/WhoScored/Championship_24_25.csv", header = TRUE, stringsAsFactors = FALSE)
champ_23_24 <- read.csv("Dataset/WhoScored/Championship_23_24.csv", header = TRUE, stringsAsFactors = FALSE)
champ_22_23 <- read.csv("Dataset/WhoScored/Championship_22_23.csv", header = TRUE, stringsAsFactors = FALSE)

# ---- Bind all rows ----
all_stats <- bind_rows(
  bund_22_23, bund_23_24, bund_24_25,
  prem_22_23, prem_23_24, prem_24_25,
  laliga_22_23, laliga_23_24, laliga_24_25,
  seriea_22_23, seriea_23_24, seriea_24_25,
  champ_22_23, champ_23_24, champ_24_25
)

# Consistent naming
all_stats <- all_stats %>%
  rename(Team_name = team)

# ============================================================
# Derive match-level classification and team-season summaries
# ============================================================

# Team mean/sd per match per team (safe with NA guards)
all_stats_clean <- all_stats %>%
  group_by(season, league, match_id, Date, Team_name) %>%
  mutate(
    team_mean = if (all(is.na(Rating))) NA_real_ else mean(Rating, na.rm = TRUE),
    team_sd   = if (all(is.na(Rating))) NA_real_ else sd(Rating, na.rm = TRUE),
    standout  = ifelse(!is.na(Rating) & !is.na(team_mean) & !is.na(team_sd),
                       Rating > team_mean + 2 * team_sd, NA)
  ) %>%
  ungroup()

# Classify each team-match as "Individual" vs "Team"
match_classification <- all_stats_clean %>%
  group_by(season, league, match_id, Date, Team_name) %>%
  summarise(
    team_mean         = mean(Rating, na.rm = TRUE),
    team_sd           = sd(Rating, na.rm = TRUE),
    standout_players  = sum(Rating > team_mean + 2 * team_sd, na.rm = TRUE),
    game_type         = ifelse(standout_players >= 1, "Individual", "Team"),
    .groups = "drop"
  )

# Unique outcome per team-match
match_outcomes <- all_stats_clean %>%
  select(season, league, match_id, Date, Team_name, outcome) %>%
  distinct()

# Join classification and outcome
classified_matches <- match_classification %>%
  inner_join(match_outcomes, by = c("season","league","match_id","Date","Team_name"))

# Assign points (3/1/0) and "points lost" (0/2/3)
classified_matches <- classified_matches %>%
  mutate(
    points = case_when(
      outcome == "win"  ~ 3,
      outcome == "draw" ~ 1,
      outcome == "loss" ~ 0,
      TRUE ~ NA_real_
    ),
    points_lost = case_when(
      outcome == "win"  ~ 0,
      outcome == "draw" ~ 2,
      outcome == "loss" ~ 3,
      TRUE ~ NA_real_
    )
  )

# Per-team-season total points
team_points <- classified_matches %>%
  group_by(season, league, Team_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")

# Per-team-season points by game type
points_by_type <- classified_matches %>%
  group_by(season, league, Team_name, game_type) %>%
  summarise(
    points_earned = sum(points, na.rm = TRUE),
    points_lost   = sum(points_lost, na.rm = TRUE),
    games_played  = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = game_type,
    values_from = c(points_earned, points_lost, games_played),
    values_fill = 0
  ) %>%
  rename(
    points_from_individual = points_earned_Individual,
    points_from_team       = points_earned_Team,
    points_lost_individual = points_lost_Individual,
    points_lost_team       = points_lost_Team,
    games_individual       = games_played_Individual,
    games_team             = games_played_Team
  )

# Unique standout players per team-season
standout_player_counts <- all_stats_clean %>%
  filter(standout == TRUE) %>%
  group_by(season, league, Team_name) %>%
  summarise(num_unique_standouts = n_distinct(PlayerName), .groups = "drop")

# Team-season summary (this is your rel_ratio table)
team_level_analysis <- points_by_type %>%
  left_join(team_points, by = c("season","league","Team_name")) %>%
  mutate(
    total_games         = games_individual + games_team,
    reliance_ratio      = ifelse(total_games > 0, games_individual / total_games, NA_real_),
    max_possible_points = total_games * 3,
    pct_points_achieved = round(100 * total_points / max_possible_points, 1)
  ) %>%
  left_join(standout_player_counts, by = c("season","league","Team_name"))

# Save (optional) and set rel_ratio = team_level_analysis (so downstream code works)
write.csv(team_level_analysis, "Dataset/team_ratio_complete2.csv", row.names = FALSE)