# ==============================================================================
# Script 3: Within-Season Dynamics (Rolling Averages)
# Description: Calculates the rolling reliance ratio for top European teams and 
#              visualises trajectories across the season (Figure 2).
# ==============================================================================

# ---- 1. Setup and Packages ----
library(tidyverse)  # Data manipulation & plotting
library(zoo)        # Rolling averages
library(ggrepel)    # Text labels
library(lubridate)  # Date parsing
library(scales)     # Formatting

# ---- 2. Data Loading & Preparation ----
# Use dynamic loading map (same as Script 1)
file_paths <- list.files(path = "Dataset/WhoScored", pattern = "\\.csv$", full.names = TRUE)

all_stats <- file_paths %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE) %>% 
            mutate(source_file = basename(.x))) %>%
  rename(Team_name = team)

# Load reliance ratio data (for final rank info)
rel_ratio <- read_csv("Dataset/team_ratio_complete2.csv", show_col_types = FALSE)

# Join final rank into the match data
all_stats <- all_stats %>% 
  left_join(
    rel_ratio %>% select(season, league, Team_name, final_rank),
    by = c("season", "league", "Team_name")
  )

# ---- 3. Date Parsing Helper ----
# Robustly parse mixed date formats (ISO, d/m/Y, Excel serials)
parse_mixed_date <- function(x) {
  ch  <- as.character(x)
  num <- suppressWarnings(as.numeric(ch))
  dt  <- suppressWarnings(parse_date_time(
    ch, orders = c("Y-m-d", "Ymd", "d/m/Y", "d-m-Y", "d.b.Y", "d b Y"),
    exact = FALSE, quiet = TRUE
  ))
  # Handle Excel serial dates
  is_serial <- !is.na(num) & num > 20000 & num < 60000
  dt[is_serial & is.na(dt)] <- as.Date(num[is_serial & is.na(dt)], origin = "1899-12-30")
  as.Date(dt)
}

# Apply cleaning
all_stats <- all_stats %>%
  mutate(
    Date   = parse_mixed_date(Date),
    Rating = suppressWarnings(as.numeric(Rating))
  )

# ---- 4. Match Classification & Ordering ----
# Flag standout games (2 SD above team match mean)
standout_flags <- all_stats %>%
  group_by(match_id, Team_name) %>%
  summarise(
    team_mean = mean(Rating, na.rm = TRUE),
    team_sd   = sd(Rating,   na.rm = TRUE),
    standout  = any(Rating > (team_mean + 2 * team_sd), na.rm = TRUE),
    .groups = "drop"
  )

# Determine game order and % of season completed
team_matches <- all_stats %>%
  select(match_id, Date, season, league, Team_name) %>%
  distinct() %>%
  left_join(standout_flags, by = c("match_id", "Team_name")) %>%
  group_by(Team_name, season) %>%
  arrange(Date, match_id, .by_group = TRUE) %>%
  mutate(
    game_number    = row_number(),
    n_games_season = max(game_number),
    pct_season     = 100 * game_number / n_games_season
  ) %>%
  ungroup()

# ---- 5. Filter for European Qualifiers (Top Teams) ----
# Define qualification cutoffs for each league/season
# (Note: You can adjust these if UEFA allocation rules change)
european_cutoffs <- tribble(
  ~league,           ~season,     ~cutoff,
  "Premier League",  "2022/2023", 6, "Premier League",  "2023/2024", 5, "Premier League",  "2024/2025", 6,
  "LaLiga",          "2022/2023", 6, "LaLiga",          "2023/2024", 5, "LaLiga",          "2024/2025", 6,
  "Bundesliga",      "2022/2023", 6, "Bundesliga",      "2023/2024", 5, "Bundesliga",      "2024/2025", 5,
  "Serie A",         "2022/2023", 6, "Serie A",         "2023/2024", 5, "Serie A",         "2024/2025", 5,
  "Championship",    "2022/2023", 6, "Championship",    "2023/2024", 6, "Championship",    "2024/2025", 6
)

# Identify qualifying teams
euro_teams <- rel_ratio %>%
  select(Team_name, season, league, final_rank) %>%
  distinct() %>%
  left_join(european_cutoffs, by = c("league", "season")) %>%
  filter(!is.na(cutoff), final_rank <= cutoff) %>%
  select(Team_name, season, league)

# Filter match data
euro_matches <- team_matches %>%
  semi_join(euro_teams, by = c("Team_name", "season", "league")) %>%
  filter(!is.na(standout)) # Remove games where rating data was missing

# ---- 6. Calculate Rolling Average ----
win_size  <- 7  # Rolling window size (matches)
bin_width <- 5  # Bin size for plotting (% of season)

rolling_pct <- euro_matches %>%
  group_by(league, season, Team_name) %>%
  arrange(game_number, .by_group = TRUE) %>%
  mutate(
    rel_roll = rollapplyr(standout, width = win_size, FUN = mean,
                          fill = NA, partial = FALSE)
  ) %>%
  ungroup()

# Aggregating for plotting (Mean reliance per bin)
league_pct <- rolling_pct %>%
  mutate(pct_bin = pmin(100, floor(pct_season / bin_width) * bin_width)) %>%
  group_by(season, league, pct_bin) %>%
  summarise(
    mean_rel = mean(rel_roll, na.rm = TRUE),
    n_teams  = sum(!is.na(rel_roll)),
    .groups  = "drop"
  ) %>%
  filter(!is.na(mean_rel), n_teams >= 4) %>% # Ensure minimum sample size
  arrange(season, league, pct_bin)

# ---- 7. Figure 2: Visualisation ----
plot2 <- ggplot(league_pct, aes(pct_bin, mean_rel, colour = league)) +
  # Reference line at 0.5 (Balanced mix)
  geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 0.4, colour = "grey55") +
  
  # Main Trend Line
  geom_line(linewidth = 1.1) +
  facet_wrap(~ season, nrow = 1) +
  
  # League Labels (at the end of the line)
  geom_text_repel(
    data = league_pct %>%
      group_by(season, league) %>%
      slice_max(pct_bin, n = 1) %>%
      mutate(x_lab = pmin(102, pct_bin + 2)),
    aes(x = x_lab, y = mean_rel, label = league, colour = league),
    inherit.aes = FALSE,
    direction = "y", hjust = 0, nudge_x = 0,
    box.padding = 0.15, point.padding = 0.1,
    min.segment.length = 0, max.overlaps = Inf, show.legend = FALSE
  ) +
  
  # Scales and Styling
  scale_x_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, 0.1)) +
  coord_cartesian(xlim = c(0, 104), ylim = c(0.2, 0.8), clip = "off") +
  guides(colour = "none") +
  labs(x = "% of season completed", y = "Rolling Individual Reliance Ratio") +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(5, 26, 5, 5), # Extra right margin for labels
    axis.line = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks = element_line(linewidth = 0.5, colour = "black")
  )

print(plot2)

# Save
# ggsave("Figures/Figure2_Rolling_Reliance.png", plot2, width = 14, height = 6, dpi = 300)