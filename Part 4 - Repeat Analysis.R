# ==============================================================================
# Script 4: Player Recurrence Analysis (Figure 3)
# Description: Visualises the distribution of standout performances per player
#              (Ridgeline plot) and summarizes repeat performers (Mean ± SD).
# ==============================================================================

# ---- 1. Setup and Packages ----
library(tidyverse)  # Data manipulation & plotting
library(ggplot2)    # Base plotting
library(ggridges)   # Ridgeline density plots
library(patchwork)  # Combining plots
library(scales)     # Formatting

# ---- 2. Data Loading & Preparation ----
# Dynamic loading of all league CSVs
file_paths <- list.files(path = "Dataset/WhoScored", pattern = "\\.csv$", full.names = TRUE)

all_stats <- file_paths %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE) %>% 
            mutate(source_file = basename(.x))) %>%
  rename(Team_name = team) %>%
  mutate(
    # Clean Date and Rating columns
    Date   = suppressWarnings(as.Date(as.character(Date))),
    Rating = suppressWarnings(as.numeric(Rating))
  )

# ---- 3. Identify Standout Performances ----
# Calculate match-level mean/sd and flag standout players (>2SD)
all_stats_clean <- all_stats %>%
  group_by(season, league, match_id, Date, Team_name) %>%
  mutate(
    team_mean = if(all(is.na(Rating))) NA_real_ else mean(Rating, na.rm = TRUE),
    team_sd   = if(all(is.na(Rating))) NA_real_ else sd(Rating, na.rm = TRUE),
    standout  = if_else(!is.na(Rating) & !is.na(team_mean) & !is.na(team_sd),
                        Rating > (team_mean + 2 * team_sd), FALSE)
  ) %>%
  ungroup()

# ---- 4. Calculate Player Totals ----
# Get list of unique players per season-league
players_ls <- all_stats_clean %>%
  distinct(season, league, PlayerName)

# Count standouts per player
standout_counts <- all_stats_clean %>%
  filter(standout == TRUE) %>%
  group_by(season, league, PlayerName) %>%
  summarise(n_standout = n(), .groups = "drop")

# Join back to include players with ZERO standouts
player_totals_league <- players_ls %>%
  left_join(standout_counts, by = c("season", "league", "PlayerName")) %>%
  mutate(n_standout = replace_na(n_standout, 0L))

# Pool totals across seasons (Total standouts per player across 3 years)
player_totals_pool <- player_totals_league %>%
  group_by(league, PlayerName) %>%
  summarise(n_standout = sum(n_standout), .groups = "drop") %>%
  mutate(league = factor(league, 
                         levels = c("Championship", "Serie A", "Bundesliga", "LaLiga", "Premier League")))

# ---- 5. Figure 3 Generation ----

# --- A) Ridgeline Density (Distribution of ALL players) ---
# Determine x-axis limit
x_upper <- max(8, ceiling(quantile(player_totals_pool$n_standout, 0.995, na.rm = TRUE)))

fig3_dist_all <- ggplot(player_totals_pool, aes(x = n_standout, y = league)) +
  geom_density_ridges(
    fill = "grey85", color = "grey20",
    linewidth = 0.4, scale = 1.05, rel_min_height = 0.003,
    bandwidth = 0.22
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", linewidth = 0.35) +
  scale_x_continuous(breaks = 0:x_upper, expand = expansion(mult = c(0.01, 0.02))) +
  coord_cartesian(xlim = c(0, x_upper)) +
  labs(
    x = "# of standout performances per player (including 0)",
    y = "League"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y  = element_text(color = "grey20"),
    axis.ticks.y = element_blank(),
    axis.line.y  = element_blank(),
    axis.line.x  = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.x = element_line(colour = "black")
  )

# --- B) Repeat Performers Summary (Mean ± SD) ---
# Filter for "Repeaters" (>= 2 standouts in a season)
repeaters <- player_totals_league %>%
  filter(n_standout >= 2)

league_repeat_summary <- repeaters %>%
  group_by(league) %>%
  summarise(
    x_mean = mean(n_standout),
    x_sd   = sd(n_standout),
    n_players = n(),
    .groups = "drop"
  ) %>%
  mutate(
    x_lo = pmax(0, x_mean - x_sd),
    x_hi = x_mean + x_sd,
    # Match order to palette
    league = factor(league, levels = c("Bundesliga", "Championship", "LaLiga", "Premier League", "Serie A"))
  )

# Colour Palette
cb_pal <- c(
  "Bundesliga"     = "#D55E00",
  "Championship"   = "#999933",
  "LaLiga"         = "#009E73",
  "Premier League" = "#0072B2",
  "Serie A"        = "#CC79A7"
)

fig3_repeaters_sd <- ggplot(league_repeat_summary, aes(y = league, x = x_mean, colour = league)) +
  geom_errorbarh(aes(xmin = x_lo, xmax = x_hi), height = 0.25, size = 0.9) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1) +
  scale_colour_manual(values = cb_pal, guide = "none") +
  scale_x_continuous(limits = c(0, 6), breaks = 0:6) +
  labs(
    x = "# of standout performances (per repeating player)",
    y = "League"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line  = element_line(colour = "black")
  )

# --- C) Assemble Final Figure ---
fig3_final <- fig3_dist_all | fig3_repeaters_sd
fig3_final <- fig3_final + plot_layout(widths = c(1.2, 1))

print(fig3_final)

# Save
# ggsave("Figures/Figure3_Recurrence_Final.png", fig3_final, width = 11, height = 6.5, dpi = 300, bg = "white")