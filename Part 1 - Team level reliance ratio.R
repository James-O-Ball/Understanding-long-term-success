# ==============================================================================
# Script 1: Data Processing & Reliance Ratio Calculation
# Description: Imports raw match data, classifies matches as Standout vs. Balanced,
#              and aggregates data to the team-season level.
# ==============================================================================

# ---- 1. Setup and Packages ----
library(tidyverse)  # Handles dplyr, readr, tidyr, ggplot2
library(zoo)        # For rolling averages (if needed in downstream steps)

# ---- 2. Data Loading ----
# List all CSV files in the data folder
file_paths <- list.files(path = "Dataset/WhoScored", pattern = "\\.csv$", full.names = TRUE)

# Read and combine all files into one dataframe
all_stats <- file_paths %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE) %>% 
            mutate(source_file = basename(.x))) # Optional: keeps track of source

# Clean column names
all_stats <- all_stats %>%
  rename(Team_name = team)

# ---- 3. Match-Level Classification ----
# Calculate match statistics and classify as 'Individual' (Standout) or 'Team' (Balanced)

match_classification <- all_stats %>%
  group_by(season, league, match_id, Date, Team_name) %>%
  summarise(
    # Calculate team-level metrics for the specific match
    team_mean = mean(Rating, na.rm = TRUE),
    team_sd   = sd(Rating, na.rm = TRUE),
    
    # Identify number of players > 2SD above team mean
    standout_players = sum(Rating > (team_mean + 2 * team_sd), na.rm = TRUE),
    
    # Classify match type
    game_type = ifelse(standout_players >= 1, "Individual", "Team"),
    
    # Capture the match outcome (assuming outcome is constant for the team-match group)
    outcome = first(outcome), 
    .groups = "drop"
  )

# ---- 4. Points Calculation ----
# Assign league points based on match outcome
match_data_points <- match_classification %>%
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

# ---- 5. Aggregation to Team-Season Level ----

# A. Calculate points earned by game type
points_by_type <- match_data_points %>%
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

# B. Calculate total season points
total_points_summary <- match_data_points %>%
  group_by(season, league, Team_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")

# C. Calculate Repeat Standout Performers (Player Recurrence)
# We go back to the raw player-level data for this
standout_recurrence <- all_stats %>%
  group_by(season, league, match_id, Team_name) %>%
  mutate(
    team_mean = mean(Rating, na.rm = TRUE),
    team_sd   = sd(Rating, na.rm = TRUE),
    is_standout = Rating > (team_mean + 2 * team_sd)
  ) %>%
  ungroup() %>%
  filter(is_standout == TRUE) %>%
  group_by(season, league, Team_name) %>%
  summarise(num_unique_standouts = n_distinct(PlayerName), .groups = "drop")

# ---- 6. Final Join & Metric Calculation ----
team_level_analysis <- points_by_type %>%
  left_join(total_points_summary, by = c("season", "league", "Team_name")) %>%
  left_join(standout_recurrence, by = c("season", "league", "Team_name")) %>%
  mutate(
    total_games         = games_individual + games_team,
    reliance_ratio      = ifelse(total_games > 0, games_individual / total_games, NA_real_),
    max_possible_points = total_games * 3,
    pct_points_achieved = round(100 * total_points / max_possible_points, 1)
  ) %>%
  # Replace NA in num_unique_standouts with 0 (for teams with NO standout games)
  mutate(num_unique_standouts = replace_na(num_unique_standouts, 0))

# ---- 7. Export ----
# Ensure the directory exists
if(!dir.exists("Dataset")) dir.create("Dataset")

write_csv(team_level_analysis, "Dataset/team_ratio_complete2.csv")

message("Script 1 Complete: Data processed and saved to Dataset/team_ratio_complete2.csv")