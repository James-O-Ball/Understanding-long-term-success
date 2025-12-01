# ===============================
# Figure 3: Recurrence / Concentration (final)
# ===============================

# ---- Packages ----
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggridges)   # <- for ridgeline densities

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

# ---- Bind all rows & light normalisation ----
all_stats <- bind_rows(
  bund_22_23, bund_23_24, bund_24_25,
  prem_22_23, prem_23_24, prem_24_25,
  laliga_22_23, laliga_23_24, laliga_24_25,
  seriea_22_23, seriea_23_24, seriea_24_25,
  champ_22_23, champ_23_24, champ_24_25
) %>%
  rename(Team_name = team) %>%
  mutate(
    Date   = suppressWarnings(as.Date(as.character(Date))),
    Rating = suppressWarnings(as.numeric(Rating))
  )

# ============================================================
# 1) Standout flag (≥ 2 SD above team match mean)
# ============================================================
all_stats_clean <- all_stats %>%
  group_by(season, league, match_id, Date, Team_name) %>%
  mutate(
    team_mean = if (all(is.na(Rating))) NA_real_ else mean(Rating, na.rm = TRUE),
    team_sd   = if (all(is.na(Rating))) NA_real_ else sd(Rating, na.rm = TRUE),
    standout  = ifelse(!is.na(Rating) & !is.na(team_mean) & !is.na(team_sd),
                       Rating > team_mean + 2 * team_sd, NA)
  ) %>%
  ungroup()

# ============================================================
# 2) Per-player totals by league-season (INCLUDE ZEROS)
# ============================================================
players_ls <- all_stats_clean %>%
  distinct(season, league, PlayerName)

standout_counts <- all_stats_clean %>%
  filter(standout %in% TRUE) %>%
  group_by(season, league, PlayerName) %>%
  summarise(n_standout = n(), .groups = "drop")

player_totals_league <- players_ls %>%
  left_join(standout_counts, by = c("season","league","PlayerName")) %>%
  mutate(n_standout = replace_na(n_standout, 0L))

# Pool across seasons for a single distribution per league (as per supervisor)
player_totals_pool <- player_totals_league %>%
  group_by(league, PlayerName) %>%
  summarise(n_standout = sum(n_standout), .groups = "drop") %>%
  mutate(league = factor(league, levels = c("Championship", "Serie A","Bundesliga","LaLiga", "Premier League")))

# ============================================================
# 3) Two views required for Figure 3
#    (a) Ridgeline density across ALL players (incl. zeros)
#    (b) Mean ± SD among REPEATERS (n_standout ≥ 2)
# ============================================================

# Palette + base theme
cb_pal <- c(
  "Bundesliga"     = "#D55E00",
  "Championship"   = "#999933",
  "LaLiga"         = "#009E73",
  "Premier League" = "#0072B2",
  "Serie A"        = "#CC79A7"
)
base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )
no_grid <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black", linewidth = 0.4),
  axis.ticks = element_line(colour = "black"),
  axis.ticks.length = unit(4, "pt")
)

# make sure 'league' is a factor in the order you want
player_totals_pool <- player_totals_pool |>
  mutate(league = factor(league,
                         levels = c("Championship","Serie A","Bundesliga","LaLiga","Premier League")))

# --- (a) Ridgeline density including zeros (per league) ---
# choose a sensible right cap without dropping tails
x_upper <- max(8, ceiling(quantile(player_totals_pool$n_standout, 0.995, na.rm = TRUE)))

fig3_dist_all <- ggplot(player_totals_pool, aes(x = n_standout, y = league)) +
  geom_density_ridges(
    fill = "grey85", color = "grey20",
    linewidth = 0.4, scale = 1.05, rel_min_height = 0.003,
    bandwidth = 0.22
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25", linewidth = 0.35) +
  scale_x_continuous(breaks = 0:x_upper, expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_discrete(drop = FALSE) +                        # keep all leagues
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

fig3_dist_all

# --- (b) Mean ± SD among repeaters (players with ≥2) ------------------------
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
    league = factor(league, levels = names(cb_pal))
  )

fig3_repeaters_sd <- ggplot(league_repeat_summary,
                            aes(y = league, x = x_mean, colour = league)) +
  geom_errorbarh(aes(xmin = x_lo, xmax = x_hi), height = 0.25, size = 0.9) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1) +
  scale_colour_manual(values = cb_pal, guide = "none") +
  scale_x_continuous(limits = c(0, max(5, ceiling(max(league_repeat_summary$x_hi, na.rm = TRUE)))),
                     breaks = 0:5) +
  labs(
    x = "# of standout performances (per repeating player)",
    y = "League"
  ) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank())

fig3_repeaters_sd

# ---- Assemble & save (two-panel version used in Results) -------------------
fig3_final <- fig3_dist_all | fig3_repeaters_sd
fig3_final <- fig3_final + plot_layout(widths = c(1.2, 1))

ggsave("Figure3_recurrence_final.png", fig3_final,
       width = 11, height = 6.5, dpi = 300, bg = "white")

# (Optional) save panels separately
ggsave("fig3_recurrence.png", fig3_dist_all,
       width = 10, height = 6, dpi = 300, bg = "white")
ggsave("Figure3B_repeaters_mean_sd.png", fig3_repeaters_sd,
       width = 6, height = 4, dpi = 300, bg = "white")
