# ==============================================================================
# Scott River — Spring Recession Timing Diagnostic
#
# Investigates the mechanism behind SP_Tim deviation from unimpaired:
#
#   PART 1 — Year-by-year SP_Tim: Baseline vs each scenario
#            Scatter/line plot showing whether timing tracks together
#            (climate-driven) or diverges (diversion-driven)
#
#   PART 2 — SP_Tim deviation from Baseline (scenario - Baseline, same year)
#            Removes climate signal entirely — only shows diversion effect
#            Positive = scenario is later than Baseline that year
#
#   PART 3 — Wet years only: SP_Tim on hydrograph
#            Vertical lines marking SP_Tim for Baseline + key scenarios
#            on representative wet year hydrographs (1983, top wet years)
#
#   PART 4 — SP_Tim vs Peak_Tim_2 correlation
#            Checks whether late spring timing just follows late peak timing
#            (if yes = climatically driven; if no = potentially diversion effect)
#
# REQUIREMENTS
#   Runs standalone after Scott_River_FFC_analysis.R has been run.
#   Reads scott_ffm_patched.csv and Scott_Available.csv
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

results_dir  <- "./Data/outputs/Scott_River/FFM/results"
input_dir    <- "./Data/inputs/Scott_River"
diag_dir     <- "./Data/outputs/Scott_River/SP_Tim_diagnostic"
dir.create(diag_dir, showWarnings = FALSE, recursive = TRUE)

nfd_start <- 1950L
nfd_end   <- 2014L

scenario_levels <- c("Baseline","90_20","80_50","Ecohydraulic","FF_WS50","FF_WS75","FF_Pk2")
scenario_labels <- c("Baseline","90/20","80/50","Ecohydraulic","FF-WS50","FF-WS75","FF-Pk2")

# named by display labels — matches factor levels after read_csv factoring
scenario_colors <- c(
  "Baseline"     = "grey40",
  "90/20"        = "#E41A1C",
  "80/50"        = "#D4A017",
  "Ecohydraulic" = "#29B6F6",
  "FF-WS50"      = "#00E676",
  "FF-WS75"      = "#CE93D8",
  "FF-Pk2"       = "#F06292"
)

# WYT classification from FFC
wet_yrs <- c(1951,1952,1953,1954,1956,1958,1963,1965,1970,1971,
             1974,1978,1982,1983,1986,1995,1996,1997,1998,1999,
             2003,2006,2016,2017)  # 1950-2014 only

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

ffm_raw <- read_csv(file.path(results_dir, "scott_ffm_patched.csv"),
                    show_col_types = FALSE) %>%
  filter(!Year %in% c("Avg","Std","CV")) %>%
  mutate(Year         = as.integer(Year),
         SP_Tim       = as.numeric(SP_Tim),
         scenario_code = factor(scenario_code,
                                levels = scenario_levels,
                                labels = scenario_labels)) %>%
  filter(Year >= nfd_start, Year <= nfd_end,
         WYT %in% c("wet","moderate","dry"))

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

# separate Baseline for reference line
baseline_sp <- sp_tim %>% filter(scenario_code == "Baseline") %>%
  select(Year, SP_Tim_baseline = SP_Tim)

sp_dev <- sp_tim %>%
  filter(scenario_code != "Baseline") %>%
  left_join(baseline_sp, by = "Year")

# PART 1A — all years, all scenarios vs Baseline
p1a <- ggplot(sp_tim %>% filter(scenario_code == "Baseline"),
              aes(x = Year, y = SP_Tim)) +
  # Baseline as grey ribbon reference
  geom_line(color = "grey40", linewidth = 1.2) +
  # diversion scenarios
  geom_line(data  = sp_tim %>% filter(scenario_code != "Baseline"),
            aes(color = scenario_code), linewidth = 0.7, alpha = 0.8) +
  # highlight wet years
  geom_point(data = sp_tim %>% filter(scenario_code == "Baseline", is_wet),
             aes(x = Year, y = SP_Tim),
             shape = 21, fill = "steelblue", color = "white", size = 2.5) +
  scale_color_manual(name   = "Scenario",
                     values = scenario_colors[scenario_labels[-1]]) +
  scale_x_continuous(breaks = seq(1950, 2014, by = 10)) +
  labs(title    = "Scott River — Spring Recession Timing: Annual Values 1950-2014",
       subtitle = "Grey = Baseline (observed)  |  Colored = diversion scenarios  |  Blue dots = wet years",
       x = "Water Year", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_annual_all_scenarios.png"),
       p1a, width = 16, height = 6, dpi = 300)
message("  Saved: SP_Tim_annual_all_scenarios.png")

# PART 1B — wet years only, spread out
p1b <- ggplot(sp_tim %>% filter(is_wet, scenario_code == "Baseline"),
              aes(x = factor(Year), y = SP_Tim)) +
  geom_col(fill = "grey80", color = "grey60", width = 0.6) +
  geom_point(data = sp_tim %>% filter(is_wet, scenario_code != "Baseline"),
             aes(color = scenario_code), size = 3, alpha = 0.85,
             position = position_dodge(width = 0.6)) +
  scale_color_manual(name   = "Scenario",
                     values = scenario_colors[scenario_labels[-1]]) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title    = "Scott River — Spring Recession Timing: Wet Years Only",
       subtitle = "Bars = Baseline  |  Points = diversion scenarios  |  Convergence = climate-driven; Divergence = diversion-driven",
       x = "Water Year", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_wet_years_comparison.png"),
       p1b, width = 16, height = 6, dpi = 300)
message("  Saved: SP_Tim_wet_years_comparison.png")

# ==============================================================================
# PART 2 — SP_Tim deviation from Baseline (same year)
# Removes climate signal — isolates diversion effect directly
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
  scale_x_continuous(breaks = seq(1950, 2014, by = 10)) +
  labs(title    = "Scott River — SP_Tim Deviation from Baseline (Scenario − Baseline, Same Year)",
       subtitle = "Positive = later spring timing than Baseline  |  Climate signal removed  |  Circles = wet years",
       x = "Water Year", y = "SP_Tim deviation (days later than Baseline)") +
  facet_wrap(~ scenario_code, ncol = 3) +
  theme_diag +
  theme(legend.position = "none")

ggsave(file.path(diag_dir, "SP_Tim_deviation_from_baseline.png"),
       p2a, width = 16, height = 10, dpi = 300)
message("  Saved: SP_Tim_deviation_from_baseline.png")

# summary: mean deviation from Baseline by scenario and WYT
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
  labs(title    = "Scott River — Mean SP_Tim Deviation from Baseline by WYT",
       subtitle = "Error bars = ±1 SD  |  Positive = later spring onset than Baseline",
       x = NULL, y = "Mean deviation from Baseline (days)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_mean_deviation_by_WYT.png"),
       p2b, width = 14, height = 6, dpi = 300)
message("  Saved: SP_Tim_mean_deviation_by_WYT.png")

# ==============================================================================
# PART 3 — SP_Tim on hydrographs: wet years
# Mark the day of spring recession onset as a vertical line
# ==============================================================================

message("\n── PART 3: SP_Tim marked on wet year hydrographs ───────────────────────")

raw <- read_csv(file.path(input_dir, "Scott_Available.csv"),
                show_col_types = FALSE) %>%
  mutate(date = mdy(date))

scenario_avail <- c(
  "Baseline"     = NA,
  "90/20"        = "90/20",
  "80/50"        = "80/50",
  "Ecohydraulic" = "362 (Ecohydraulic)",
  "FF-WS50"      = "50_bf_50_20",
  "FF-WS75"      = "75_bf_50_20",
  "FF_Pk2"       = "peak_2_20"
)

# build daily flows for key scenarios
key_scenarios <- c("Baseline","90/20","80/50","Ecohydraulic")

daily <- map_dfr(key_scenarios, function(sc) {
  col <- scenario_avail[[sc]]
  df  <- if (is.na(col)) {
    raw %>% transmute(date, flow = discharge_cfs)
  } else {
    raw %>% transmute(date, flow = pmax(discharge_cfs - .data[[col]], 0))
  }
  df %>% mutate(scenario_code = sc)
})

# SP_Tim = day of water year -> convert to calendar date
dowy_to_date <- function(wy, dowy) {
  # day 1 = Oct 1 of prior year
  as.Date(paste0(wy - 1, "-10-01")) + dowy - 1
}

# focus on top 5 wet years with most diversion contrast
focus_wet_yrs <- c(1983, 1974, 2006, 1986, 1996)

for (wy in focus_wet_yrs) {
  date_start <- make_date(wy - 1L, 10L, 1L)
  date_end   <- make_date(wy,       9L, 30L)

  obs_wy <- daily %>%
    filter(date >= date_start, date <= date_end)

  # SP_Tim for this year
  sp_lines <- sp_tim %>%
    filter(Year == wy,
           scenario_code %in% key_scenarios) %>%
    mutate(sp_date = dowy_to_date(wy, SP_Tim),
           color   = scenario_colors[as.character(scenario_code)])

  if (nrow(sp_lines) == 0) next

  y_max <- obs_wy %>%
    filter(date >= make_date(wy, 2L, 1L),
           date <= make_date(wy, 6L, 30L)) %>%
    pull(flow) %>% max(na.rm = TRUE) * 1.15

  # map scenario_code back to codes for color lookup
  sc_label_to_code <- setNames(scenario_levels, scenario_labels)

  p3 <- ggplot(obs_wy %>% filter(scenario_code == "Baseline"),
               aes(x = date, y = flow)) +
    # wet season shading
    annotate("rect",
             xmin = make_date(wy-1L,12L,1L), xmax = make_date(wy,3L,31L),
             ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +
    # observed flow
    geom_line(color = "grey40", linewidth = 2.0) +
    # diversion scenario flows
    geom_line(data = obs_wy %>%
                filter(scenario_code %in% c("90/20","80/50","Ecohydraulic")),
              aes(color = scenario_code), linewidth = 0.9, linetype = "dashed") +
    # SP_Tim vertical lines
    geom_vline(data = sp_lines,
               aes(xintercept = sp_date, color = scenario_code),
               linewidth = 1.2, linetype = "solid", alpha = 0.9) +
    # SP_Tim labels at top
    geom_text(data = sp_lines,
              aes(x = sp_date, y = y_max * 0.95, label = scenario_code,
                  color = scenario_code),
              angle = 90, hjust = 1, vjust = -0.3, size = 3.5, fontface = "bold") +
    scale_color_manual(values = c(
      "Baseline"     = "grey40",
      "90/20"        = "#E41A1C",
      "80/50"        = "#D4A017",
      "Ecohydraulic" = "#29B6F6"
    ), guide = "none") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month",
                 limits = c(make_date(wy, 2L, 1L),   # Feb 1
                            make_date(wy, 6L, 30L)),  # Jun 30
                 expand = expansion(0)) +
    scale_y_continuous(labels = comma, limits = c(0, y_max),
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title    = paste0("Scott River WY", wy, " — Spring Recession Timing on Hydrograph"),
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
# Checks whether late spring timing is driven by climate (flow magnitude/timing)
# rather than diversions. Uses:
#   - Annual maximum flow (from Scott_Available.csv) — overall wet season size
#   - Wet_BFL_Mag_50 — wet season baseflow magnitude (varies annually)
# If correlation is the same across all scenarios: climate-driven.
# If scenario lines diverge: diversion-independent effect.
# ==============================================================================

message("\n── PART 4: SP_Tim vs annual hydrology metrics ────────────────────────────")

# annual max flow from Baseline (observed discharge)
ann_max <- raw %>%
  mutate(wy = if_else(month(date) >= 10, year(date) + 1L, year(date))) %>%
  group_by(wy) %>%
  summarise(ann_max_flow = max(discharge_cfs, na.rm = TRUE), .groups = "drop") %>%
  filter(wy >= nfd_start, wy <= nfd_end) %>%
  rename(Year = wy)

# join SP_Tim with annual max flow and Wet_BFL_Mag_50
sp_hydro <- sp_tim %>%
  left_join(ann_max, by = "Year") %>%
  left_join(
    ffm_raw %>%
      select(Year, scenario_code, Wet_BFL_Mag_50) %>%
      mutate(Wet_BFL_Mag_50 = as.numeric(Wet_BFL_Mag_50)),
    by = c("Year","scenario_code")
  )

# PART 4A — SP_Tim vs annual max flow
p4a <- ggplot(sp_hydro,
              aes(x = ann_max_flow, y = SP_Tim, color = scenario_code)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.0) +
  scale_color_manual(name   = "Scenario",
                     values = scenario_colors) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~ WYT, ncol = 3) +
  labs(title    = "Scott River — SP_Tim vs Annual Maximum Flow",
       subtitle = "Parallel lines = climate-driven  |  Diverging lines = scenario-specific effect",
       x = "Annual Maximum Flow (cfs)", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_vs_AnnMaxFlow.png"),
       p4a, width = 14, height = 6, dpi = 300)
message("  Saved: SP_Tim_vs_AnnMaxFlow.png")

# PART 4B — SP_Tim vs Wet_BFL_Mag_50
p4b <- ggplot(sp_hydro %>% filter(!is.na(Wet_BFL_Mag_50)),
              aes(x = Wet_BFL_Mag_50, y = SP_Tim, color = scenario_code)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.0) +
  scale_color_manual(name   = "Scenario",
                     values = scenario_colors) +
  scale_x_continuous(labels = comma) +
  facet_wrap(~ WYT, ncol = 3) +
  labs(title    = "Scott River — SP_Tim vs Wet-Season Baseflow Magnitude (50th pct)",
       subtitle = "Parallel lines = climate-driven  |  Diverging lines = scenario-specific effect",
       x = "Wet_BFL_Mag_50 (cfs)", y = "SP_Tim (day of water year)") +
  theme_diag

ggsave(file.path(diag_dir, "SP_Tim_vs_WetBFLMag50.png"),
       p4b, width = 14, height = 6, dpi = 300)
message("  Saved: SP_Tim_vs_WetBFLMag50.png")

message("\nDone. All diagnostic figures written to: ", normalizePath(diag_dir))
message("\nFigures:")
message("  PART 1 — SP_Tim_annual_all_scenarios.png  (year-by-year, all scenarios)")
message("  PART 1 — SP_Tim_wet_years_comparison.png  (wet years bar + points)")
message("  PART 2 — SP_Tim_deviation_from_baseline.png  (scenario - Baseline, climate removed)")
message("  PART 2 — SP_Tim_mean_deviation_by_WYT.png  (summary bar by WYT)")
message("  PART 3 — SP_Tim_hydro_WY*.png  (timing on hydrographs, 5 wet years)")
message("  PART 4 — SP_Tim_vs_AnnMaxFlow.png      (SP_Tim vs annual max flow — climate signal)")
message("  PART 4 — SP_Tim_vs_WetBFLMag50.png     (SP_Tim vs wet-season baseflow magnitude)")
