# ==============================================================================
# Elder Creek — Spring Recession Timing Diagnostic
# COMID: 12068798
#
#   PART 1 — Year-by-year SP_Tim: Baseline vs each scenario
#   PART 2 — SP_Tim deviation from Baseline (scenario - Baseline, same year)
#   PART 3 — Wet years only: SP_Tim on hydrograph
#   PART 4 — SP_Tim vs annual hydrology metrics
#
# REQUIREMENTS
#   Runs standalone after Elder_Creek_FFC_analysis.R has been run.
#   Reads elder_ffm_patched.csv and Elder_Available.csv
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)

#setwd("C:/Users/adrianal/SCCWRP/CEFF - FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
#setwd("C:/Users/kristinet/SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
setwd("/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

results_dir <- "./Data/outputs/Elder_Creek/FFM/results"
input_dir   <- "./Data/inputs/Elder_Creek"
diag_dir    <- "./Data/outputs/Elder_Creek/SP_Tim_diagnostic"
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

# NOTE: this diagnostic deliberately checks the spring recession timing
# artifact across all four candidate wet-season baseflow percentile tiers
# (25th, 50th, 75th, 90th), not only the two (50th, 75th) carried into the
# final reporting scenarios, to confirm the artifact's behavior held across
# a broader range of thresholds during the investigation.
scenario_levels <- c("Baseline","90_20","80_50","FF_BF25","FF_BF50","FF_BF75","FF_BF90","FF_Pk2")
scenario_labels <- c("Baseline","90/20","80/50","FF-BF25","FF-BF50","FF-BF75","FF-BF90","FF-Pk2")

scenario_colors <- c(
  "Baseline" = "grey40",
  "90/20"    = "#E41A1C",
  "80/50"    = "#D4A017",
  "FF-BF25"  = "#FF7F00",
  "FF-BF50"  = "#00E676",
  "FF-BF75"  = "#CE93D8",
  "FF-BF90"  = "#8C564B",
  "FF-Pk2"   = "#F06292"
)

# Wet years and the focus subset for PART 3 hydrograph plots are both derived
# dynamically from the FFC output below (see LOAD DATA section).
wet_yrs <- NULL
focus_wet_yrs <- NULL

theme_diag <- theme_minimal(base_size = 14) +
  theme(plot.background   = element_rect(fill = "white", color = NA),
        panel.background  = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
        axis.title        = element_text(size = 13, face = "bold"),
        axis.text         = element_text(size = 11),
        legend.position   = "bottom",
        legend.title      = element_text(face = "bold", size = 12),
        legend.text       = element_text(size = 11),
        plot.title        = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle     = element_text(size = 11, hjust = 0.5, color = "grey40"))

# ==============================================================================
# LOAD DATA
# ==============================================================================

nfd_start <- 1950L
nfd_end   <- 2014L

ffm_raw <- read_csv(file.path(results_dir, "elder_ffm_patched.csv"),
                    show_col_types = FALSE) %>%
  filter(!Year %in% c("Avg","Std","CV")) %>%
  mutate(Year         = as.integer(Year),
         SP_Tim       = as.numeric(SP_Tim),
         scenario_code = factor(scenario_code,
                                levels = scenario_levels,
                                labels = scenario_labels)) %>%
  filter(WYT %in% c("wet","moderate","dry"),
         Year >= nfd_start, Year <= nfd_end)

# Derive wet years from data
wet_yrs       <- ffm_raw %>% filter(WYT == "wet", scenario_code == "Baseline") %>%
                   pull(Year) %>% unique() %>% sort()
focus_wet_yrs <- head(wet_yrs, 5)   # first 5; update after reviewing the data

message("Wet years (", length(wet_yrs), "): ", paste(wet_yrs, collapse = ", "))
message("Focus wet years for hydrograph plots: ", paste(focus_wet_yrs, collapse = ", "))

sp_tim <- ffm_raw %>%
  select(Year, WYT, scenario_code, SP_Tim) %>%
  filter(!is.na(SP_Tim)) %>%
  mutate(is_wet = Year %in% wet_yrs)

message("SP_Tim rows: ", nrow(sp_tim))
message("Years with SP_Tim: ", length(unique(sp_tim$Year)))

# ==============================================================================
# PART 1 — Year-by-year SP_Tim: all scenarios overlaid
# ==============================================================================

message("\n── PART 1: Year-by-year SP_Tim ─────────────────────────────────────────")

baseline_sp <- sp_tim %>% filter(scenario_code == "Baseline") %>%
  select(Year, SP_Tim_baseline = SP_Tim)

sp_dev <- sp_tim %>%
  filter(scenario_code != "Baseline") %>%
  left_join(baseline_sp, by = "Year")

yr_min <- min(sp_tim$Year)
yr_max <- max(sp_tim$Year)

# PART 1A — all years, all scenarios vs Baseline
p1a <- ggplot(sp_tim %>% filter(scenario_code == "Baseline"),
              aes(x = Year, y = SP_Tim)) +
  geom_line(color = "grey40", linewidth = 1.2) +
  geom_line(data  = sp_tim %>% filter(scenario_code != "Baseline"),
            aes(color = scenario_code), linewidth = 0.7, alpha = 0.8) +
  geom_point(data = sp_tim %>% filter(scenario_code == "Baseline", is_wet),
             aes(x = Year, y = SP_Tim),
             shape = 21, fill = "steelblue", color = "white", size = 2.5) +
  scale_color_manual(name   = "Scenario",
                     values = scenario_colors[scenario_labels[-1]]) +
  scale_x_continuous(breaks = pretty(c(yr_min, yr_max), n = 8)) +
  labs(title    = paste0("Elder Creek — Spring Recession Timing: Annual Values ",
                         yr_min, "-", yr_max),
       subtitle = "Grey = Baseline (observed)  |  Colored = diversion scenarios  |  Blue dots = wet years",
       x = "Water Year", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_annual_all_scenarios.png"),
       p1a, width = 16, height = 6, dpi = 300)
message("  Saved: SP_Tim_annual_all_scenarios.png")

# PART 1B — wet years only
p1b <- ggplot(sp_tim %>% filter(is_wet, scenario_code == "Baseline"),
              aes(x = factor(Year), y = SP_Tim)) +
  geom_col(fill = "grey80", color = "grey60", width = 0.6) +
  geom_point(data = sp_tim %>% filter(is_wet, scenario_code != "Baseline"),
             aes(color = scenario_code), size = 3, alpha = 0.85,
             position = position_dodge(width = 0.6)) +
  scale_color_manual(name   = "Scenario",
                     values = scenario_colors[scenario_labels[-1]]) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title    = "Elder Creek — Spring Recession Timing: Wet Years Only",
       subtitle = "Bars = Baseline  |  Points = diversion scenarios  |  Convergence = climate-driven; Divergence = diversion-driven",
       x = "Water Year", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_wet_years_comparison.png"),
       p1b, width = 16, height = 6, dpi = 300)
message("  Saved: SP_Tim_wet_years_comparison.png")

# ==============================================================================
# PART 2 — SP_Tim deviation from Baseline (same year)
# ==============================================================================

message("\n── PART 2: SP_Tim deviation from Baseline ───────────────────────────────")

p2a <- ggplot(sp_dev, aes(x = Year, y = SP_Tim - SP_Tim_baseline,
                           color = scenario_code)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6, linetype = "dashed") +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(data = sp_dev %>% filter(is_wet),
             aes(shape = "Wet year"), size = 2, alpha = 0.7) +
  scale_color_manual(name   = "Scenario",
                     values = scenario_colors[scenario_labels[-1]]) +
  scale_shape_manual(name = "", values = c("Wet year" = 16)) +
  scale_x_continuous(breaks = pretty(c(yr_min, yr_max), n = 8)) +
  labs(title    = "Elder Creek — SP_Tim Deviation from Baseline (Scenario − Baseline, Same Year)",
       subtitle = "Positive = later spring timing than Baseline  |  Climate signal removed  |  Circles = wet years",
       x = "Water Year", y = "SP_Tim deviation (days later than Baseline)") +
  facet_wrap(~ scenario_code, ncol = 3) +
  theme_diag +
  theme(legend.position = "none")

ggsave(file.path(diag_dir, "SP_Tim_deviation_from_baseline.png"),
       p2a, width = 16, height = 10, dpi = 300)
message("  Saved: SP_Tim_deviation_from_baseline.png")

p2b <- sp_dev %>%
  mutate(deviation = SP_Tim - SP_Tim_baseline,
         WYT = factor(WYT, levels = c("wet","moderate","dry"),
                      labels = c("Wet","Moderate","Dry"))) %>%
  group_by(scenario_code, WYT) %>%
  summarise(mean_dev = mean(deviation, na.rm = TRUE),
            sd_dev   = sd(deviation, na.rm = TRUE),
            n        = n(), .groups = "drop") %>%
  ggplot(aes(x = scenario_code, y = mean_dev, fill = scenario_code)) +
  geom_col(width = 0.65, color = "grey20", linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_dev - sd_dev, ymax = mean_dev + sd_dev),
                width = 0.3, linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5, linetype = "dashed") +
  scale_fill_manual(name   = "Scenario",
                    values = scenario_colors[scenario_labels[-1]],
                    guide  = "none") +
  scale_x_discrete(guide = guide_axis(angle = 35)) +
  facet_wrap(~ WYT, ncol = 3) +
  labs(title    = "Elder Creek — Mean SP_Tim Deviation from Baseline by WYT",
       subtitle = "Error bars = ±1 SD  |  Positive = later spring onset than Baseline",
       x = NULL, y = "Mean deviation from Baseline (days)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_mean_deviation_by_WYT.png"),
       p2b, width = 14, height = 6, dpi = 300)
message("  Saved: SP_Tim_mean_deviation_by_WYT.png")

# ==============================================================================
# PART 3 — SP_Tim on hydrographs: wet years
# ==============================================================================

message("\n── PART 3: SP_Tim marked on wet year hydrographs ───────────────────────")

raw <- read_csv(file.path(input_dir, "Elder_Available.csv"),
                show_col_types = FALSE) %>%
  mutate(date = mdy(date))

raw_8050 <- read_csv(file.path(input_dir, "Elder_80_50_wiener_all.csv"),
                     show_col_types = FALSE) %>%
  mutate(date = as.Date(Date)) %>%
  select(date, streamflow = Streamflow, diversion_cfs)

scenario_avail <- c(
  "Baseline"  = NA,
  "90/20"     = "90/20",
  "80/50"     = NA,           # built from raw_8050
  "FF-BF25"   = "25_bf_50_20",
  "FF-BF50"   = "50_bf_50_20",
  "FF-BF75"   = "75_bf_50_20",
  "FF-BF90"   = "90_bf_50_20",
  "FF-Pk2"    = "peak_2_20"
)

key_scenarios <- c("Baseline","90/20","80/50","FF-BF50","FF-BF75")

daily <- map_dfr(key_scenarios, function(sc) {
  col <- scenario_avail[[sc]]
  df  <- if (sc == "Baseline") {
    raw %>% transmute(date, flow = discharge_cfs)
  } else if (sc == "80/50") {
    raw_8050 %>% transmute(date, flow = pmax(streamflow - diversion_cfs, 0))
  } else {
    raw %>% transmute(date, flow = pmax(discharge_cfs - .data[[col]], 0))
  }
  df %>% mutate(scenario_code = sc)
})

dowy_to_date <- function(wy, dowy) {
  as.Date(paste0(wy - 1, "-10-01")) + dowy - 1
}

for (wy in focus_wet_yrs) {
  date_start <- make_date(wy - 1L, 10L, 1L)
  date_end   <- make_date(wy,       9L, 30L)

  obs_wy <- daily %>% filter(date >= date_start, date <= date_end)

  sp_lines <- sp_tim %>%
    filter(Year == wy, scenario_code %in% key_scenarios) %>%
    mutate(sp_date = dowy_to_date(wy, SP_Tim),
           color   = scenario_colors[as.character(scenario_code)])

  if (nrow(sp_lines) == 0) next

  y_max <- obs_wy %>%
    filter(date >= make_date(wy, 2L, 1L),
           date <= make_date(wy, 6L, 30L)) %>%
    pull(flow) %>% max(na.rm = TRUE) * 1.15

  p3 <- ggplot(obs_wy %>% filter(scenario_code == "Baseline"),
               aes(x = date, y = flow)) +
    annotate("rect",
             xmin = make_date(wy-1L,12L,1L), xmax = make_date(wy,3L,31L),
             ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +
    geom_line(color = "grey40", linewidth = 2.0) +
    geom_line(data = obs_wy %>% filter(scenario_code %in% c("90/20","FF-BF50","FF-BF75")),
              aes(color = scenario_code), linewidth = 0.9, linetype = "dashed") +
    geom_vline(data = sp_lines,
               aes(xintercept = sp_date, color = scenario_code),
               linewidth = 1.2, linetype = "solid", alpha = 0.9) +
    geom_text(data = sp_lines,
              aes(x = sp_date, y = y_max * 0.95, label = scenario_code,
                  color = scenario_code),
              angle = 90, hjust = 1, vjust = -0.3, size = 3.5, fontface = "bold") +
    scale_color_manual(values = c(
      "Baseline" = "grey40",
      "90/20"    = "#E41A1C",
      "80/50"    = "#D4A017",
      "FF-BF50"  = "#00E676",
      "FF-BF75"  = "#CE93D8"
    ), guide = "none") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(make_date(wy, 2L, 1L), make_date(wy, 6L, 30L)),
                 expand = expansion(0)) +
    scale_y_continuous(labels = comma, limits = c(0, y_max),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title    = paste0("Elder Creek WY", wy, " — Spring Recession Timing on Hydrograph"),
         subtitle = "Solid vertical lines = FFC-identified spring onset (SP_Tim) per scenario",
         x = NULL, y = "Discharge (cfs)") +
    coord_cartesian(clip = "off") +
    theme_diag

  ggsave(file.path(diag_dir, paste0("SP_Tim_hydro_WY", wy, ".png")),
         p3, width = 14, height = 6, dpi = 300)
  message("  Saved: SP_Tim_hydro_WY", wy, ".png")
}

# ==============================================================================
# PART 4 — SP_Tim vs annual hydrology metrics
# ==============================================================================

message("\n── PART 4: SP_Tim vs annual hydrology metrics ────────────────────────────")

ann_max <- raw %>%
  mutate(wy = if_else(month(date) >= 10, year(date) + 1L, year(date))) %>%
  group_by(wy) %>%
  summarise(ann_max_flow = max(discharge_cfs, na.rm = TRUE), .groups = "drop") %>%
  rename(Year = wy)

sp_hydro <- sp_tim %>%
  left_join(ann_max, by = "Year") %>%
  left_join(
    ffm_raw %>%
      select(Year, scenario_code, Wet_BFL_Mag_50) %>%
      mutate(Wet_BFL_Mag_50 = as.numeric(Wet_BFL_Mag_50)),
    by = c("Year","scenario_code")
  )

p4a <- ggplot(sp_hydro,
              aes(x = ann_max_flow, y = SP_Tim, color = scenario_code)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.0) +
  scale_color_manual(name   = "Scenario", values = scenario_colors) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~ WYT, ncol = 3) +
  labs(title    = "Elder Creek — SP_Tim vs Annual Maximum Flow",
       subtitle = "Parallel lines = climate-driven  |  Diverging lines = scenario-specific effect",
       x = "Annual Maximum Flow (cfs)", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_vs_AnnMaxFlow.png"),
       p4a, width = 14, height = 6, dpi = 300)
message("  Saved: SP_Tim_vs_AnnMaxFlow.png")

p4b <- ggplot(sp_hydro %>% filter(!is.na(Wet_BFL_Mag_50)),
              aes(x = Wet_BFL_Mag_50, y = SP_Tim, color = scenario_code)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.0) +
  scale_color_manual(name   = "Scenario", values = scenario_colors) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~ WYT, ncol = 3) +
  labs(title    = "Elder Creek — SP_Tim vs Wet-Season Baseflow Magnitude (50th pct)",
       subtitle = "Parallel lines = climate-driven  |  Diverging lines = scenario-specific effect",
       x = "Wet_BFL_Mag_50 (cfs)", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_vs_WetBFLMag50.png"),
       p4b, width = 14, height = 6, dpi = 300)
message("  Saved: SP_Tim_vs_WetBFLMag50.png")

message("\nDone. All diagnostic figures written to: ", normalizePath(diag_dir))
