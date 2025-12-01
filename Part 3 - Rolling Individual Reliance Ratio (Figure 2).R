###############################################################################
# Libraries
###############################################################################
library(tidyverse)
library(ggplot2)
library(changepoint)
library(ggrepel)
library(ggbreak)
library(zoo)
library(broom)
library(lubridate)
library(scales)
library(cowplot)
library(grid)

###############################################################################
# Load Data
###############################################################################
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

# Combine dataframes
all_stats <- bind_rows(
  bund_22_23, bund_23_24, bund_24_25,
  prem_22_23, prem_23_24, prem_24_25,
  laliga_22_23, laliga_23_24, laliga_24_25,
  seriea_22_23, seriea_23_24, seriea_24_25,
  champ_22_23, champ_23_24, champ_24_25
)

rel_ratio <- read.csv("Dataset/team_ratio_complete2.csv", header = TRUE, stringsAsFactors = FALSE)

# Join final rank into the all_stats dataframe
all_stats <- all_stats %>% 
  left_join(
    rel_ratio %>% select(season, league, Team_name, final_rank),
    by = c("season", "league", "team" = "Team_name")
  )

###############################################################################
# 0) Helpers
###############################################################################
# Robustly parse mixed date formats (ISO, d/m/Y, d-m-Y, d-b-Y, Excel serials)
parse_mixed_date <- function(x) {
  ch  <- as.character(x)
  num <- suppressWarnings(as.numeric(ch))
  dt  <- suppressWarnings(lubridate::parse_date_time(
    ch,
    orders = c(
      "Y-m-d","Ymd",
      "d/m/Y","d/m/y",
      "d-m-Y","d-m-y",
      "d.b.Y","d.b.y",
      "d-b-Y","d-b-y",
      "b d Y","b d, Y",
      "d b Y","d b y"
    ),
    exact = FALSE, quiet = TRUE
  ))
  is_serial <- !is.na(num) & num > 20000 & num < 60000
  dt[is_serial & is.na(dt)] <- as.Date(num[is_serial & is.na(dt)], origin = "1899-12-30")
  as.Date(dt)
}

###############################################################################
# Normalising and cleaning
###############################################################################
all_stats <- all_stats %>%
  mutate(
    Date   = parse_mixed_date(Date),
    Rating = suppressWarnings(as.numeric(Rating))
  )

###############################################################################
# Flag standout games (2 SD above team match mean)
###############################################################################
standout_flags <- all_stats %>%
  group_by(match_id, team) %>%
  mutate(
    team_mean = mean(Rating, na.rm = TRUE),
    team_sd   = sd(Rating,   na.rm = TRUE),
    standout  = Rating > (team_mean + 2 * team_sd)
  ) %>%
  summarise(standout_game = any(standout, na.rm = TRUE), .groups = "drop")

###############################################################################
# Robust team game order + % of season
###############################################################################
team_matches <- all_stats %>%
  select(match_id, Date, season, league, team) %>%
  distinct() %>%
  left_join(standout_flags, by = c("match_id","team")) %>%
  group_by(team, season) %>%
  arrange(Date, match_id, .by_group = TRUE) %>%
  mutate(
    game_number    = row_number(),
    n_games_season = max(game_number),
    pct_season     = 100 * game_number / n_games_season
  ) %>%
  ungroup()

###############################################################################
# Keep only European-qualifying teams (adjust cutoffs if you like)
###############################################################################
european_cutoffs <- tribble(
  ~league,           ~season,     ~cutoff,
  "Premier League",  "2022/2023", 6,
  "Premier League",  "2023/2024", 5,
  "Premier League",  "2024/2025", 6,
  "LaLiga",          "2022/2023", 6,
  "LaLiga",          "2023/2024", 5,
  "LaLiga",          "2024/2025", 6,
  "Bundesliga",      "2022/2023", 6,
  "Bundesliga",      "2023/2024", 5,
  "Bundesliga",      "2024/2025", 5,
  "Serie A",         "2022/2023", 6,
  "Serie A",         "2023/2024", 5,
  "Serie A",         "2024/2025", 5,
  "Championship",    "2022/2023", 6,
  "Championship",    "2023/2024", 6,
  "Championship",    "2024/2025", 6
)

team_ranks <- all_stats %>%
  select(team, season, league, final_rank) %>%
  distinct()

euro_teams <- team_ranks %>%
  left_join(european_cutoffs, by = c("league","season")) %>%
  filter(!is.na(cutoff), !is.na(final_rank), final_rank <= cutoff) %>%
  select(team, season, league)

euro_matches <- team_matches %>%
  semi_join(euro_teams, by = c("team","season","league")) %>%
  filter(!is.na(standout_game))

###############################################################################
# Rolling share (window = 7 matches) and bin by % of season
###############################################################################
win       <- 7        # rolling window in matches
bin_width <- 5        # % bins: 1=finest; try 2 or 5 to smooth

rolling_pct <- euro_matches %>%
  group_by(league, season, team) %>%
  arrange(game_number, .by_group = TRUE) %>%
  mutate(
    rel_roll = rollapplyr(standout_game, width = win, FUN = mean,
                          fill = NA, partial = FALSE)
  ) %>%
  ungroup()

# Aggregate with IQR ribbon (q25–q75) and add a normalized team-count strip
league_pct <- rolling_pct %>%
  mutate(pct_bin = pmin(100, floor(pct_season / bin_width) * bin_width)) %>%
  group_by(season, league, pct_bin) %>%
  summarise(
    mean_rel = mean(rel_roll, na.rm = TRUE),
    n_teams  = sum(!is.na(rel_roll)),
    q25      = quantile(rel_roll, 0.25, na.rm = TRUE),
    q75      = quantile(rel_roll, 0.75, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  filter(!is.na(mean_rel), n_teams >= 4) %>%
  group_by(season) %>%
  mutate(n_norm = scales::rescale(n_teams, to = c(0, 0.06))) %>%  # 0–6% panel height
  ungroup() %>%
  arrange(season, league, pct_bin)

end_labels <- league_pct %>%
  group_by(season, league) %>%
  slice_max(pct_bin, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(pct_bin = pmin(102, pct_bin + 2))  # nudge labels slightly right

###############################################################################
# Plot (IQR ribbon + availability strip + tighter y-range)
###############################################################################
# No IQR
plot1 <- ggplot(league_pct, aes(pct_bin, mean_rel, colour = league)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 0.4, colour = "grey55") +
  geom_line(linewidth = 1.1) +
  facet_wrap(~ season, nrow = 1) +
  # end labels (one per league×season), nudged right
  geom_text_repel(
    data = league_pct %>%
      dplyr::group_by(season, league) %>%
      dplyr::arrange(pct_bin, .by_group = TRUE) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::mutate(x_lab = pmin(102, pct_bin + 2)),
    aes(x = x_lab, y = mean_rel, label = league, colour = league),
    inherit.aes   = FALSE,
    direction     = "y",       # spread labels vertically
    hjust         = 0,         # left-justify at the x position
    nudge_x       = 0,         # already nudged via x_lab
    box.padding   = 0.15,      # tweak spacing
    point.padding = 0.1,
    segment.size  = 0.2,
    min.segment.length = 0,
    max.overlaps  = Inf,       # don’t drop labels
    show.legend   = FALSE
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20), labels = function(x) paste0(x, "%")) +
  scale_y_continuous(limits = c(0.2, 0.8), breaks = seq(0.2, 0.8, 0.1)) +
  guides(colour = "none") +
  labs(x = "% of season completed", y = "Rolling Individual Reliance Ratio") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid   = element_blank(),
    strip.text   = element_text(face = "bold"),
    plot.margin  = margin(5, 26, 5, 5),   # space for labels outside panel
    axis.line  = element_line(linewidth = 0.6, colour = "black"),
    axis.ticks = element_line(linewidth = 0.5, colour = "black")
  ) +
  coord_cartesian(xlim = c(0, 104), ylim = c(0.2, 0.8), clip = "off")

plot1