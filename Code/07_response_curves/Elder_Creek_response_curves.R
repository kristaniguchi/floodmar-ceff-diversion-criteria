# ==============================================================================
# Elder Creek — FFM Deviation Response Curves (standalone)
# COMID: 12068798
#
# Shows the relationship between recharge opportunity (annual diversion volume,
# TAF) and FFM deviation from unimpaired, for each scenario.
#
# BEFORE RUNNING:
#   Requires ./Data/inputs/Elder_Creek/elder_annual_TAF.csv (annual diversion
#   volume per scenario per water year, for 90/20 and the FF-BF/Peak2
#   scenarios) and ./Data/inputs/Elder_Creek/Elder_80_50_wiener_all.csv
#   (daily diversion_af for 80/50, summed by water year below — no annual
#   summary file exists for 80/50, so it's computed directly here instead).
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "readxl", "scales"))
#   Requires outputs from Elder_Creek_FFC_analysis.R:
#     ./Data/outputs/Elder_Creek/FFM/results/elder_ffm_patched.csv
#     ./Data/outputs/Elder_Creek/FFM/results/elder_percentiles.csv
# ==============================================================================

library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

#setwd("C:/Users/adrianal/SCCWRP/CEFF - FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
#setwd("C:/Users/kristinet/SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
setwd("/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

results_dir   <- "./Data/outputs/Elder_Creek/FFM/results"
recharge_path <- "./Data/inputs/Elder_Creek/elder_annual_TAF.csv"
response_dir  <- "./Data/outputs/Elder_Creek/response_curves"

dir.create(response_dir, showWarnings = FALSE, recursive = TRUE)

# Scenario map: analysis code -> column name in elder_annual_TAF.csv
scenario_af_map <- tribble(
  ~scenario_code, ~label,      ~af_col,
  "90_20",        "90/20",     "90/20",
  "FF_BF50",      "FF-BF50",   "50_bf_50_20",
  "FF_BF75",      "FF-BF75",   "75_bf_50_20",
  "FF_Pk2",       "FF-Pk2",    "peak_2_20"
)
# Note: 80_50 is not in this map — its annual diversion volume has no
# summary-file source and is instead computed directly from the daily
# Elder_80_50_wiener_all.csv file in the af_raw construction below. It is
# still included in scenario_levels/scenario_labels/scenario_colors above
# and appears in every response curve.
# Note: Baseline excluded — zero diversions = zero recharge opportunity

# Scenario display order and colors
# 80/50 is added back here even though it's not in scenario_af_map/
# elder_annual_TAF.csv — its annual diversion volume is instead computed
# directly from Elder_80_50_wiener_all.csv's daily diversion_af column
# (see Part 1 below), since no annual summary file includes it.
scenario_levels <- c(scenario_af_map$scenario_code, "80_50")
scenario_labels <- c(scenario_af_map$label, "80/50")
scenario_colors <- c(
  "90_20"   = "#E41A1C",
  "FF_BF50" = "#00E676",
  "FF_BF75" = "#CE93D8",
  "FF_Pk2"  = "#F06292",
  "80_50"   = "#D4A017"
)

# Key metrics for response curves
response_metrics <- c(
  "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Wet_BFL_Dur", "Wet_Tim",
  "Peak_2",         "Peak_Dur_2",     "Peak_Fre_2",
  "Peak_5",         "Peak_Dur_5",     "Peak_Fre_5",
  "SP_Mag",         "SP_Tim",         "SP_Dur"
)

metric_labels <- c(
  Wet_BFL_Mag_10 = "Wet-Season BF Magnitude\n10th percentile (cfs)",
  Wet_BFL_Mag_50 = "Wet-Season BF Magnitude\n50th percentile (cfs)",
  Wet_BFL_Dur    = "Wet-Season Duration (days)",
  Wet_Tim        = "Wet-Season Timing (WYD)",
  Peak_2         = "Peak 2-yr Magnitude (cfs)",
  Peak_Dur_2     = "Peak 2-yr Duration (days)",
  Peak_Fre_2     = "Peak 2-yr Frequency",
  Peak_5         = "Peak 5-yr Magnitude (cfs)",
  Peak_Dur_5     = "Peak 5-yr Duration (days)",
  Peak_Fre_5     = "Peak 5-yr Frequency",
  SP_Mag         = "Spring Recession Magnitude (cfs)",
  SP_Tim         = "Spring Recession Timing (WYD)",
  SP_Dur         = "Spring Recession Duration (days)"
)

magnitude_metrics <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50",
                       "Peak_2", "Peak_5", "Peak_10", "SP_Mag",
                       "DS_Mag_50", "DS_Mag_90", "FA_Mag")
timing_metrics    <- c("Wet_Tim", "SP_Tim", "DS_Tim", "FA_Tim")

metric_type <- function(m) {
  if (m %in% magnitude_metrics) "magnitude"
  else if (m %in% timing_metrics) "timing"
  else "duration_freq"
}

metric_units <- c(
  Wet_BFL_Mag_10 = "cfs",   Wet_BFL_Mag_50 = "cfs",
  Wet_BFL_Dur    = "days",  Wet_Tim        = "water year day",
  Peak_2         = "cfs",   Peak_5         = "cfs",   Peak_10    = "cfs",
  Peak_Dur_2     = "days",  Peak_Dur_5     = "days",  Peak_Dur_10 = "days",
  Peak_Fre_2     = "events",Peak_Fre_5     = "events",Peak_Fre_10 = "events",
  SP_Mag         = "cfs",   SP_Tim         = "water year day",
  SP_Dur         = "days",  FA_Mag         = "cfs",
  FA_Tim         = "water year day", FA_Dur = "days"
)

# ==============================================================================
# PART 1 — Load and wrangle data
# ==============================================================================

message("Loading data...")

# ── Annual FFM values (patched) ───────────────────────────────────────────────
ffm_raw <- read_csv(file.path(results_dir, "elder_ffm_patched.csv"),
                    show_col_types = FALSE) %>%
  filter(!Year %in% c("Avg","Std","CV")) %>%
  mutate(Year = as.integer(Year),
         scenario_code = factor(scenario_code,
                                levels = scenario_levels,
                                labels = scenario_labels)) %>%
  mutate(across(matches("^Peak_Dur_|^Peak_Fre_"), ~ replace_na(as.numeric(.x), 0)))

nfd_start <- 1950L
nfd_end   <- 2014L

ffm_raw <- ffm_raw %>%
  filter(WYT %in% c("wet","moderate","dry"),
         Year >= nfd_start, Year <= nfd_end)

n_yrs <- length(unique(ffm_raw$Year))
message("  Years with valid WYT: ", n_yrs,
        " (", min(ffm_raw$Year), "-", max(ffm_raw$Year), ")")

# ── Natural flows API predicted percentiles (unimpaired reference) ─────────────
pct_raw <- read_csv(file.path(results_dir, "elder_percentiles.csv"),
                    show_col_types = FALSE)

pct_cols_present <- intersect(c("p50_predicted","p50"), names(pct_raw))
p50_col <- pct_cols_present[1]

unimpaired_ref <- pct_raw %>%
  filter(scenario_code == "Baseline",
         WYT %in% c("wet","moderate","dry","any"),
         metric %in% response_metrics) %>%
  select(metric, WYT, unimpaired_p50 = all_of(p50_col)) %>%
  mutate(
    unimpaired_p50 = as.numeric(unimpaired_p50),
    WYT = if_else(WYT == "any", "any", str_to_title(WYT))
  )

wyt_levels <- c("Wet","Moderate","Dry")
unimp_any  <- unimpaired_ref %>% filter(WYT == "any")
unimp_wyt  <- unimpaired_ref %>% filter(WYT != "any")

metrics_missing_wyt <- setdiff(unique(unimp_any$metric), unique(unimp_wyt$metric))
if (length(metrics_missing_wyt) > 0) {
  fill_rows <- unimp_any %>%
    filter(metric %in% metrics_missing_wyt) %>%
    select(metric, unimpaired_p50) %>%
    crossing(WYT = wyt_levels)
  unimp_wyt <- bind_rows(unimp_wyt, fill_rows)
}
unimpaired_ref <- unimp_wyt

# ── Annual recharge opportunity (TAF per scenario per year) ───────────────────
if (!file.exists(recharge_path)) {
  stop("Recharge metrics file not found: ", recharge_path)
}

# NOTE: despite the filename, verify whether elder_annual_TAF.csv's raw values
# are already in thousand acre-feet (TAF) or in acre-feet (AF, needing the
# /1000 conversion below). Values in the thousands for annual diversion at a
# site the size of Elder Creek are far more consistent with acre-feet than
# already being in TAF (which would imply diversions in the millions of
# acre-feet) — kept the /1000 conversion as the working assumption, but this
# should be confirmed rather than taken for granted.
af_main <- read_csv(recharge_path, show_col_types = FALSE) %>%
  select(water_year, all_of(scenario_af_map$af_col)) %>%
  rename_with(~ scenario_af_map$scenario_code[match(.x, scenario_af_map$af_col)],
              all_of(scenario_af_map$af_col)) %>%
  pivot_longer(cols      = -water_year,
               names_to  = "scenario_code",
               values_to = "diversion_af")

# 80/50's annual diversion volume isn't in any annual summary file for Elder
# Creek — computed here instead from the daily wiener file's own diversion_af
# column, summed by water year. Date format is m/d/yyyy (mdy()) — using
# as.Date() here would silently fail to parse and produce all-NA dates (the
# same date-parsing bug found and fixed elsewhere in this project).
af_8050 <- read_csv("./Data/inputs/Elder_Creek/Elder_80_50_wiener_all.csv",
                    show_col_types = FALSE) %>%
  mutate(date       = mdy(Date),
         water_year = if_else(month(date) >= 10, year(date) + 1L, year(date))) %>%
  group_by(water_year) %>%
  summarise(diversion_af = sum(diversion_af, na.rm = TRUE), .groups = "drop") %>%
  mutate(scenario_code = "80_50") %>%
  select(water_year, scenario_code, diversion_af)

af_raw <- bind_rows(af_main, af_8050) %>%
  mutate(diversion_taf = diversion_af / 1000,
         scenario_code = factor(scenario_code,
                                levels = scenario_levels,
                                labels = scenario_labels)) %>%
  filter(water_year >= nfd_start, water_year <= nfd_end)

message("  Recharge data (NFD): WYs ", min(af_raw$water_year), "-", max(af_raw$water_year))

# ── FFM long format ───────────────────────────────────────────────────────────
ffm_long <- ffm_raw %>%
  filter(scenario_code != "Baseline") %>%
  select(Year, WYT, scenario_code,
         all_of(intersect(response_metrics, names(ffm_raw)))) %>%
  pivot_longer(cols     = -c(Year, WYT, scenario_code),
               names_to = "metric",
               values_to = "ffm_value") %>%
  filter(!is.na(ffm_value),
         WYT %in% c("wet","moderate","dry")) %>%
  mutate(WYT = str_to_title(WYT))

# ── Join: FFM + unimpaired reference + recharge opportunity ───────────────────
# ── Baseline per-year values, for deviation-from-baseline comparisons ────────
# ffm_raw's scenario_code was already relabeled against scenario_levels
# (which excludes Baseline), so Baseline rows there are NA. Re-read the raw
# CSV directly instead, before any relabeling, to recover Baseline's own
# per-year values as a reference point distinct from the unimpaired natural
# reference.
baseline_lookup <- read_csv(file.path(results_dir, "elder_ffm_patched.csv"),
                            show_col_types = FALSE) %>%
  filter(!Year %in% c("Avg","Std","CV"), scenario_code == "Baseline") %>%
  mutate(Year = as.integer(Year),
         across(matches("^Peak_Dur_|^Peak_Fre_"), ~ replace_na(as.numeric(.x), 0))) %>%
  filter(Year >= nfd_start, Year <= nfd_end) %>%
  select(Year, all_of(intersect(response_metrics, names(.)))) %>%
  pivot_longer(cols = -Year, names_to = "metric", values_to = "baseline_value")

message("  Baseline lookup: ", nrow(baseline_lookup), " rows (Year x metric)")

response_df <- ffm_long %>%
  left_join(unimpaired_ref, by = c("metric","WYT")) %>%
  left_join(af_raw %>% rename(Year = water_year),
            by = c("Year","scenario_code")) %>%
  left_join(baseline_lookup, by = c("Year","metric")) %>%
  filter(!is.na(unimpaired_p50),
         !is.na(diversion_taf),
         unimpaired_p50 != 0) %>%
  mutate(
    pct_deviation = (ffm_value - unimpaired_p50) / abs(unimpaired_p50) * 100,
    WYT           = factor(WYT, levels = c("Wet","Moderate","Dry")),
    metric_label  = metric_labels[metric],
    raw_deviation = ffm_value - unimpaired_p50,
    raw_deviation_from_baseline = ffm_value - baseline_value,
    pct_deviation_from_baseline = if_else(
      baseline_value != 0,
      (ffm_value - baseline_value) / abs(baseline_value) * 100,
      NA_real_
    )
  )

message("  Response data: ", nrow(response_df), " rows ready for plotting")

# ==============================================================================
# PART 2 — Plot theme
# ==============================================================================

theme_response <- theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey92", linewidth = 0.3),
    axis.title        = element_text(size = 12, face = "bold"),
    axis.text         = element_text(size = 10),
    strip.text        = element_text(size = 11, face = "bold"),
    strip.background  = element_rect(fill = "grey95", color = NA),
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold", size = 11),
    legend.text       = element_text(size = 10),
    plot.title        = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 10, hjust = 0.5, color = "grey40"),
    plot.margin       = margin(10, 10, 10, 10)
  )

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================================================================
# PART 3 — Raw / pct deviation response curves (one per metric)
# ==============================================================================

message("\n── PART 3: Raw/pct deviation response curves ──────────────────────────")

make_response_plot <- function(df_met, met, y_var, y_label, y_format = NULL) {
  ggplot(df_met, aes(x = diversion_taf, y = .data[[y_var]], color = scenario_code)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6, linetype = "dashed") +
    geom_point(aes(shape = WYT), alpha = 0.4, size = 2) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                alpha = 0.12, linewidth = 1.1) +
    scale_color_manual(name = "Scenario",
                       values = setNames(scenario_colors, scenario_labels)) +
    scale_shape_manual(name = "Water Year Type",
                       values = c("Wet" = 16, "Moderate" = 17, "Dry" = 15)) +
    scale_x_continuous(labels = comma,
                       name = "Annual Diversion Volume (TAF)\n← Less recharge    More recharge →") +
    scale_y_continuous(labels = y_format %||% waiver(), name = y_label) +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3), nrow = 1),
           shape = guide_legend(nrow = 1)) +
    labs(title    = paste0("Elder Creek Response Curve: ", metric_labels[[met]]),
         subtitle = "Each point = one water year  |  LOESS smooth  |  y=0: WYT-matched unimpaired median") +
    theme_response
}

for (met in response_metrics) {
  df_met <- response_df %>% filter(metric == met)
  if (nrow(df_met) < 10) { message("  Skipping ", met, " — insufficient data"); next }
  mtype <- metric_type(met)
  units <- metric_units[[met]]
  if (mtype == "magnitude") {
    ggsave(file.path(response_dir, paste0("response_", met, "_raw.png")),
           make_response_plot(df_met, met, "raw_deviation",
                              paste0("Deviation from Unimpaired (", units, ")"), comma),
           width = 10, height = 7, dpi = 300)
    ggsave(file.path(response_dir, paste0("response_", met, "_pct.png")),
           make_response_plot(df_met, met, "pct_deviation",
                              "Deviation from Unimpaired (%)",
                              function(x) paste0(x, "%")),
           width = 10, height = 7, dpi = 300)
    message("  Saved: response_", met, "_raw.png + _pct.png")
  } else {
    ggsave(file.path(response_dir, paste0("response_", met, ".png")),
           make_response_plot(df_met, met, "raw_deviation",
                              paste0("Deviation from Unimpaired (", units, ")"), comma),
           width = 10, height = 7, dpi = 300)
    message("  Saved: response_", met, ".png")
  }
}

# ==============================================================================
# PART 4 — Combined facet response curves
# ==============================================================================

message("\n── PART 4: Combined facet response curves ──────────────────────────────")

make_facet_plot <- function(metrics_subset, title_str) {
  df_sub <- response_df %>%
    filter(metric %in% metrics_subset) %>%
    mutate(metric_label = factor(metric_labels[metric],
                                 levels = metric_labels[metrics_subset]))
  ggplot(df_sub, aes(x = diversion_taf, y = raw_deviation, color = scenario_code)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.5, linetype = "dashed") +
    geom_point(aes(shape = WYT), alpha = 0.3, size = 1.5) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1.0) +
    scale_color_manual(name = "Scenario",
                       values = setNames(scenario_colors, scenario_labels)) +
    scale_shape_manual(name = "WYT",
                       values = c("Wet" = 16, "Moderate" = 17, "Dry" = 15)) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    facet_wrap(~ metric_label, scales = "free", ncol = 3) +
    guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
    labs(title    = paste0("Elder Creek FFM Response to Recharge: ", title_str),
         subtitle = "y = raw deviation from WYT-matched unimpaired median  |  x = annual diversion (TAF)",
         x = "Annual Diversion Volume (TAF)", y = "Deviation from Unimpaired") +
    theme_response
}

ggsave(file.path(response_dir, "response_combined_wet_season.png"),
       make_facet_plot(c("Wet_BFL_Mag_10","Wet_BFL_Mag_50","Wet_BFL_Dur","Wet_Tim"),
                       "Wet-Season Baseflow"),
       width = 14, height = 8, dpi = 300)
ggsave(file.path(response_dir, "response_combined_peak.png"),
       make_facet_plot(c("Peak_2","Peak_Dur_2","Peak_Fre_2",
                         "Peak_5","Peak_Dur_5","Peak_Fre_5"), "Peak Flow"),
       width = 14, height = 8, dpi = 300)
ggsave(file.path(response_dir, "response_combined_spring.png"),
       make_facet_plot(c("SP_Mag","SP_Tim","SP_Dur"), "Spring Recession"),
       width = 14, height = 6, dpi = 300)
message("  Saved: response_combined_*.png (3 files)")

# ==============================================================================
# PART 5 — Natural range reference data (needed by Part 10, below)
#
# Builds unimp_range_any: the natural flow reference percentiles (p10-p90)
# for the "any" water-year-type case, used by Part 10's deviation-from-
# Baseline calculations. An earlier version of this section also built
# exploratory "proportion of years within natural range" plots; those plots
# are not part of the final analysis and have been removed, but this
# underlying reference data is still required downstream.
# ==============================================================================

message("\n── PART 5: Natural range reference data ────────────────────────────────")

pct_range_cols    <- c("p10_predicted","p25_predicted","p50_predicted","p75_predicted","p90_predicted")
pct_range_present <- intersect(pct_range_cols, names(pct_raw))

unimpaired_range <- pct_raw %>%
  filter(scenario_code == "Baseline",
         WYT %in% c("any","wet","moderate","dry"),
         metric %in% response_metrics) %>%
  select(metric, WYT, all_of(pct_range_present)) %>%
  rename_with(~ str_remove(.x, "_predicted"), all_of(pct_range_present)) %>%
  mutate(
    across(any_of(c("p10","p25","p50","p75","p90")), as.numeric),
    WYT = if_else(WYT == "any", "any", str_to_title(WYT))
  )

unimp_range_any <- unimpaired_range %>% filter(WYT == "any")

# ==============================================================================
# PART 9 — Screening additional recharge opportunity metrics
#
# Parts 1-8 use annual diversion volume (TAF) as the sole measure of "recharge
# opportunity." Jason's Elder_Additional_Annual_Recharge_Metrics.csv provides
# three additional annual recharge metrics per scenario:
#   - n_runs               — number of separate diversion events in the year
#   - n_diversion_days     — total days diverted in the year
#   - avg_diversion_length — average length (days) of each diversion event
#
# DATA LIMITATION: this file does not include an "80/50" column (unlike
# Scott River's version, which does) — only 90/20 and the FF-BF/Peak2
# percentile-based scenarios. 80/50's annual diversion volume (TAF) is
# available (computed separately from the daily wiener file — see Part 1),
# so 80/50 still appears in the TAF-based screening column below, but it is
# excluded from the three additional recharge metrics (n_runs,
# n_diversion_days, avg_diversion_length), since no per-run/per-day
# breakdown exists for 80/50 at Elder Creek.
#
# PURPOSE: exploratory screening only. Does not replace or alter Parts 1-8.
# ==============================================================================

message("\n── PART 9: Screening additional recharge opportunity metrics ──────────")

baseline_dev_dir <- file.path(response_dir, "deviation_from_baseline")
dir.create(baseline_dev_dir, showWarnings = FALSE, recursive = TRUE)

additional_recharge_path <- "./Data/inputs/Elder_Creek/Elder_Additional_Annual_Recharge_Metrics.csv"

scenario_runs_map <- tribble(
  ~scenario_code, ~runs_col,
  "90_20",   "90/20",
  "FF_BF50", "50_bf_50_20",
  "FF_BF75", "75_bf_50_20",
  "FF_Pk2",  "peak_2_20"
)
# Note: no 80_50 row — see data limitation note above.

extract_runs_block <- function(raw, start_col, n_cols, metric_name) {
  header <- as.character(unlist(raw[2, start_col:(start_col + n_cols - 1)]))
  block <- raw[3:nrow(raw), start_col:(start_col + n_cols - 1)]
  names(block) <- header
  # Each 19-column block is padded with trailing blank/NaN-named columns —
  # dplyr verbs (filter, mutate, etc.) error on a data frame with any
  # missing column name, so drop these before further processing.
  block <- block[, !is.na(names(block)), drop = FALSE]
  block %>%
    filter(!is.na(water_year)) %>%
    mutate(water_year = as.integer(water_year),
           across(-water_year, as.numeric)) %>%
    pivot_longer(cols = -water_year, names_to = "runs_col", values_to = "value") %>%
    inner_join(scenario_runs_map, by = "runs_col") %>%
    mutate(recharge_metric = metric_name) %>%
    select(water_year, scenario_code, recharge_metric, value)
}

runs_raw <- read_csv(additional_recharge_path, col_names = FALSE, show_col_types = FALSE)

# 80/50 is missing from the Additional Recharge Metrics file entirely (see
# note above) — computed here instead from the daily wiener file's
# diversion_cfs column, using run-length encoding to identify consecutive
# diverting days as one "event." This mirrors the same logic used to fill
# in 80/50's annual diversion volume (TAF) in Part 1, applied to the three
# additional recharge metrics instead. ASSUMPTION: a "run"/event is defined
# here as any maximal consecutive stretch of days with diversion_cfs > 0 —
# this should match Jason's own definition for the other scenarios, but
# hasn't been directly confirmed against his methodology.
compute_runs_stats <- function(diverting_vec) {
  r <- rle(diverting_vec)
  run_lengths <- r$lengths[r$values]
  tibble(
    n_runs               = length(run_lengths),
    n_diversion_days     = sum(run_lengths),
    avg_diversion_length = if (length(run_lengths) == 0) NA_real_ else mean(run_lengths)
  )
}

runs_8050_long <- read_csv("./Data/inputs/Elder_Creek/Elder_80_50_wiener_all.csv",
                          show_col_types = FALSE) %>%
  mutate(date       = mdy(Date),
         water_year = if_else(month(date) >= 10, year(date) + 1L, year(date)),
         diverting  = diversion_cfs > 0) %>%
  arrange(date) %>%
  group_by(water_year) %>%
  summarise(compute_runs_stats(diverting), .groups = "drop") %>%
  pivot_longer(cols = -water_year, names_to = "recharge_metric", values_to = "value") %>%
  mutate(scenario_code = "80_50") %>%
  select(water_year, scenario_code, recharge_metric, value)

recharge_screen <- bind_rows(
  extract_runs_block(runs_raw, 1,  17, "n_runs"),
  extract_runs_block(runs_raw, 21, 17, "n_diversion_days"),
  extract_runs_block(runs_raw, 41, 17, "avg_diversion_length"),
  runs_8050_long
) %>%
  mutate(scenario_code = factor(scenario_code, levels = scenario_levels, labels = scenario_labels)) %>%
  filter(water_year >= nfd_start, water_year <= nfd_end)

recharge_screen <- bind_rows(
  recharge_screen,
  af_raw %>%
    transmute(water_year, scenario_code, recharge_metric = "diversion_taf", value = diversion_taf)
)

recharge_metric_labels <- c(
  diversion_taf         = "Annual Diversion Volume (TAF)",
  n_runs                = "Number of Diversion Events",
  n_diversion_days      = "Total Diversion Days",
  avg_diversion_length  = "Average Diversion Event Length (days)"
)
recharge_metric_levels <- names(recharge_metric_labels)

recharge_screen <- recharge_screen %>%
  mutate(recharge_metric = factor(recharge_metric, levels = recharge_metric_levels,
                                  labels = recharge_metric_labels[recharge_metric_levels]))

message("  Recharge metrics parsed: ", paste(levels(recharge_screen$recharge_metric), collapse = "; "))

write_csv(recharge_screen, file.path(response_dir, "elder_recharge_metrics_screened.csv"))
message("  Saved: elder_recharge_metrics_screened.csv")

# ==============================================================================
# PART 10 — Deviation from Baseline + raw flow metric values
# ==============================================================================

message("\n── PART 10: Deviation from Baseline + raw flow metric values ──────────")

key_metrics_p10 <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Peak_Dur_2", "Peak_Fre_2")

make_response_plot_b <- function(df_met, met, y_var, y_label, y_format = NULL,
                                 subtitle_note = "y=0: Baseline (that water year)") {
  ggplot(df_met, aes(x = diversion_taf, y = .data[[y_var]], color = scenario_code)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6, linetype = "dashed") +
    geom_point(aes(shape = WYT), alpha = 0.4, size = 2) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, alpha = 0.12, linewidth = 1.1) +
    scale_color_manual(name = "Scenario", values = setNames(scenario_colors, scenario_labels)) +
    scale_shape_manual(name = "Water Year Type",
                       values = c("Wet" = 16, "Moderate" = 17, "Dry" = 15)) +
    scale_x_continuous(labels = comma,
                       name = "Annual Diversion Volume (TAF)\n← Less recharge    More recharge →") +
    scale_y_continuous(labels = y_format %||% waiver(), name = y_label) +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3), nrow = 1),
           shape = guide_legend(nrow = 1)) +
    labs(title    = paste0("Elder Creek Response Curve: ", metric_labels[[met]]),
         subtitle = paste0("Each point = one water year  |  LOESS smooth  |  ", subtitle_note)) +
    theme_response
}

for (met in key_metrics_p10) {
  df_met <- response_df %>% filter(metric == met, !is.na(pct_deviation_from_baseline))
  if (nrow(df_met) < 10) { message("  Skipping ", met, " — insufficient data"); next }
  units <- metric_units[[met]]
  mtype <- metric_type(met)
  if (mtype == "magnitude") {
    ggsave(file.path(baseline_dev_dir, paste0("response_", met, "_fromBaseline_raw.png")),
           make_response_plot_b(df_met, met, "raw_deviation_from_baseline",
                                paste0("Deviation from Baseline (", units, ")"), comma),
           width = 10, height = 7, dpi = 300)
    ggsave(file.path(baseline_dev_dir, paste0("response_", met, "_fromBaseline_pct.png")),
           make_response_plot_b(df_met, met, "pct_deviation_from_baseline",
                                "Deviation from Baseline (%)", function(x) paste0(x, "%")),
           width = 10, height = 7, dpi = 300)
  } else {
    ggsave(file.path(baseline_dev_dir, paste0("response_", met, "_fromBaseline.png")),
           make_response_plot_b(df_met, met, "raw_deviation_from_baseline",
                                paste0("Deviation from Baseline (", units, ")"), comma),
           width = 10, height = 7, dpi = 300)
  }
  message("  Saved: response_", met, "_fromBaseline.*")
}

# Raw value plots
baseline_for_raw <- baseline_lookup %>%
  filter(metric %in% key_metrics_p10) %>%
  rename(ffm_value = baseline_value) %>%
  mutate(scenario_code = "Baseline", diversion_taf = 0)

raw_plot_df <- bind_rows(
  response_df %>% filter(metric %in% key_metrics_p10) %>%
    select(Year, metric, scenario_code, diversion_taf, ffm_value),
  baseline_for_raw %>% select(Year, metric, scenario_code, diversion_taf, ffm_value)
) %>%
  mutate(scenario_code = factor(scenario_code, levels = c("Baseline", scenario_labels)))

nat_band_raw <- unimp_range_any %>%
  filter(metric %in% key_metrics_p10) %>%
  mutate(metric_label = metric_labels[metric])

make_raw_value_plot <- function(met) {
  df_met  <- raw_plot_df %>% filter(metric == met)
  nat_met <- nat_band_raw %>% filter(metric == met)
  p <- ggplot(df_met, aes(x = diversion_taf, y = ffm_value, color = scenario_code))
  if (nrow(nat_met) > 0) {
    p <- p +
      geom_rect(data = nat_met, aes(ymin = p10, ymax = p90), xmin = -Inf, xmax = Inf,
                fill = "#B3E5FC", alpha = 0.35, inherit.aes = FALSE) +
      geom_rect(data = nat_met, aes(ymin = p25, ymax = p75), xmin = -Inf, xmax = Inf,
                fill = "#0288D1", alpha = 0.20, inherit.aes = FALSE) +
      geom_hline(data = nat_met, aes(yintercept = p50), color = "#0288D1",
                 linewidth = 0.7, linetype = "dashed", inherit.aes = FALSE)
  }
  p +
    geom_point(alpha = 0.35, size = 1.6) +
    geom_smooth(data = df_met %>% filter(scenario_code != "Baseline"),
                method = "loess", formula = y ~ x, se = FALSE, linewidth = 1.0) +
    scale_color_manual(name = "Scenario",
                       values = setNames(c("grey40", scenario_colors), c("Baseline", scenario_labels))) +
    scale_x_continuous(labels = comma, name = "Annual Diversion Volume (TAF)  (Baseline shown at 0)") +
    labs(title    = paste0("Elder Creek Raw Flow Metric Value: ", metric_labels[[met]]),
         subtitle = "Baseline shown at its own points (no diversion)  |  Blue bands: natural flow p10-p90 (light), p25-p75 (dark)  |  Blue dashed: natural p50",
         y = metric_labels[[met]]) +
    theme_response
}

for (met in key_metrics_p10) {
  ggsave(file.path(response_dir, paste0("response_", met, "_rawvalue.png")),
         make_raw_value_plot(met), width = 10, height = 7, dpi = 300)
  message("  Saved: response_", met, "_rawvalue.png")
}

# ==============================================================================
# PART 11 — Correlation screening (deviation from Baseline)
# ==============================================================================

message("\n── PART 11: Correlation screening (deviation from Baseline) ───────────")

# Elder does not have a documented Spring Recession detection artifact the
# way Scott River does, but Spring Recession Duration and Magnitude were
# excluded from screening anyway: both showed as "robust" in an earlier
# pass, but on review the correlation looks like it's more likely tracking
# water year type (wet/moderate/dry) than a genuine diversion-scenario
# effect, since Spring Recession timing/magnitude is itself largely a
# function of water year type rather than diversion volume. Treating that
# as robust would risk featuring a spurious relationship. Excluded here for
# the same practical reason Scott excludes its Spring Recession metrics,
# even though the underlying justification is a suspected confound rather
# than a confirmed detection artifact.
screening_metrics_p11 <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Wet_BFL_Dur",
                          "Peak_Dur_2", "Peak_Fre_2", "Peak_Dur_5", "Peak_Fre_5")

corr_input <- response_df %>%
  filter(metric %in% screening_metrics_p11, !is.na(pct_deviation_from_baseline)) %>%
  select(Year, scenario_code, metric, pct_deviation_from_baseline) %>%
  inner_join(recharge_screen %>% rename(Year = water_year),
             by = c("Year", "scenario_code"),
             relationship = "many-to-many")

corr_screen <- corr_input %>%
  group_by(metric, recharge_metric, scenario_code) %>%
  summarise(
    n   = sum(!is.na(value) & !is.na(pct_deviation_from_baseline)),
    rho = if (n >= 10) suppressWarnings(cor(value, pct_deviation_from_baseline,
                                            method = "spearman", use = "complete.obs")) else NA_real_,
    .groups = "drop"
  )

corr_summary <- corr_screen %>%
  filter(!is.na(rho)) %>%
  group_by(metric, recharge_metric) %>%
  summarise(mean_rho = mean(rho), sd_rho = sd(rho), n_scenarios = n(), .groups = "drop") %>%
  mutate(
    metric_label = factor(metric_labels[metric], levels = metric_labels[screening_metrics_p11]),
    robust = abs(mean_rho) >= 0.4
  )

write_csv(corr_screen %>% arrange(metric, recharge_metric, scenario_code),
          file.path(baseline_dev_dir, "elder_correlation_screening_by_scenario.csv"))
write_csv(corr_summary %>% arrange(desc(abs(mean_rho))),
          file.path(baseline_dev_dir, "elder_correlation_screening_summary.csv"))

p_corr_heat <- ggplot(corr_summary, aes(x = recharge_metric, y = metric_label, fill = mean_rho)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", mean_rho)), size = 4.2) +
  scale_fill_gradient2(name = "Mean Spearman ρ",
                       low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  labs(title = "Elder Creek Correlation Screening: Recharge Metrics vs. FFM Deviation from Baseline",
       x = NULL, y = NULL) +
  theme_response +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), panel.grid = element_blank())

ggsave(file.path(baseline_dev_dir, "correlation_screening_heatmap.png"),
       p_corr_heat, width = 11, height = 7, dpi = 300)
message("  Saved: correlation_screening_heatmap.png")

message("\n  Metric x recharge-metric pairs flagged as robust:")
corr_summary %>% filter(robust) %>% arrange(desc(abs(mean_rho))) %>%
  select(metric, recharge_metric, mean_rho, sd_rho) %>% print(n = 30)

# ==============================================================================
# PART 12 — Featured tradeoff curves
#
# Wet-Season BF Magnitude 50th percentile vs. Annual Diversion Volume (TAF)
# and vs. Total Diversion Days were selected as the featured pairings —
# both show the strongest correlation of any FFM metric in the screening
# (mean rho = -0.52 and -0.51 respectively), narrowly missing the sd <= 0.25
# robustness cutoff (sd = 0.28 for both) while comfortably clearing the
# |mean rho| >= 0.4 strength threshold. Spring Recession Duration technically
# cleared both thresholds (rho ~0.40-0.48, sd <= 0.13), but Wet-Season BF
# Magnitude 50th was chosen instead: the correlation itself is stronger, and
# it keeps the featured finding consistent with Scott River, where the same
# metric (against a different pair of recharge metrics) was the featured
# result — a deliberate judgment call favoring correlation strength and
# cross-site narrative consistency over the sd cutoff, which is a starting
# heuristic rather than a hard rule.
# ==============================================================================

message("\n── PART 12: Featured tradeoff curves ───────────────────────────────────")

# Built from the full screening_metrics_p11 scope (Part 11) for consistency
# with how this section is structured at the other two sites.
screen_df_baseline_full <- response_df %>%
  filter(metric %in% screening_metrics_p11, !is.na(pct_deviation_from_baseline)) %>%
  select(Year, WYT, scenario_code, metric, metric_label, ffm_value, baseline_value,
         raw_deviation_from_baseline, pct_deviation_from_baseline) %>%
  inner_join(recharge_screen %>% rename(Year = water_year),
             by = c("Year", "scenario_code"),
             relationship = "many-to-many")

make_featured_tradeoff <- function(met, rmetric) {
  df_feat <- screen_df_baseline_full %>% filter(metric == met, recharge_metric == rmetric)
  ggplot(df_feat, aes(x = value, y = pct_deviation_from_baseline, color = scenario_code)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6, linetype = "dashed") +
    geom_point(aes(shape = WYT), alpha = 0.4, size = 2) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, alpha = 0.12, linewidth = 1.1) +
    scale_color_manual(name = "Scenario", values = setNames(scenario_colors, scenario_labels)) +
    scale_shape_manual(name = "Water Year Type",
                       values = c("Wet" = 16, "Moderate" = 17, "Dry" = 15)) +
    scale_x_continuous(labels = comma, name = as.character(rmetric)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), name = "Deviation from Baseline (%)") +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3), nrow = 1),
           shape = guide_legend(nrow = 1)) +
    labs(title    = paste0("Elder Creek ", metric_labels[[as.character(met)]], " vs. ", rmetric),
         subtitle = "Each point = one water year  |  LOESS smooth  |  y=0: Baseline (that water year)") +
    theme_response
}

p_feat_1 <- make_featured_tradeoff("Wet_BFL_Mag_50", "Annual Diversion Volume (TAF)")
ggsave(file.path(baseline_dev_dir, "featured_WetBFLMag50_vs_TAF.png"),
       p_feat_1, width = 10, height = 7, dpi = 300)
message("  Saved: featured_WetBFLMag50_vs_TAF.png (mean rho = -0.52)")

p_feat_2 <- make_featured_tradeoff("Wet_BFL_Mag_50", "Total Diversion Days")
ggsave(file.path(baseline_dev_dir, "featured_WetBFLMag50_vs_TotalDiversionDays.png"),
       p_feat_2, width = 10, height = 7, dpi = 300)
message("  Saved: featured_WetBFLMag50_vs_TotalDiversionDays.png (mean rho = -0.51)")


# ==============================================================================
# PART 13 -- Tail-sensitivity diagnostic
#
# Response curves are LOESS smooths, which can be pulled around by a thin
# tail of high-recharge-metric years for any single scenario -- producing
# visually dramatic trends (or reversals) that a handful of years are driving
# rather than a broad, well-supported pattern.
#
# For each featured metric x recharge-metric pairing used in Part 12, this
# checks -- for every scenario -- whether its behavior at the high end of its
# range is backed by a reasonable number of years, or is a low-n tail
# artifact.
#
#   n_years                  : total water years available for that scenario
#   max_value                : highest recharge-metric value reached by that
#                               scenario
#   n_years_above_pooled_p90 : how many of that scenario's years exceed the
#                               90th percentile of the metric's value POOLED
#                               ACROSS ALL SCENARIOS (a consistent "high"
#                               threshold, not a scenario-specific one)
#
# The top N years by value are also listed individually per scenario so any
# unusual/influential points are visible directly, not just summarized away.
# ==============================================================================

message("\n-- PART 13: Tail-sensitivity diagnostic ----------------------------------")

tail_diagnostic <- function(df, met, rmetric, top_n = 5) {
  df_sub <- df %>% filter(metric == met, recharge_metric == rmetric)

  pooled_p90 <- quantile(df_sub$value, 0.90, na.rm = TRUE)

  summary_tbl <- df_sub %>%
    group_by(scenario_code) %>%
    summarise(
      n_years                  = n(),
      max_value                = max(value, na.rm = TRUE),
      n_years_above_pooled_p90 = sum(value > pooled_p90, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(metric = met, recharge_metric = rmetric, pooled_p90 = pooled_p90)

  tail_years <- df_sub %>%
    group_by(scenario_code) %>%
    slice_max(order_by = value, n = top_n, with_ties = FALSE) %>%
    ungroup() %>%
    select(scenario_code, Year, WYT, value, pct_deviation_from_baseline) %>%
    mutate(metric = met, recharge_metric = rmetric) %>%
    arrange(scenario_code, desc(value))

  list(summary = summary_tbl, tail_years = tail_years)
}

# Featured pairs used in this site's tradeoff curves (Part 12). Update this
# list if the featured pairings change.
tail_pairs <- list(
  c("Wet_BFL_Mag_50", "Annual Diversion Volume (TAF)"),
  c("Wet_BFL_Mag_50", "Total Diversion Days")
)

tail_summaries  <- list()
tail_years_list <- list()

for (pair in tail_pairs) {
  res <- tail_diagnostic(screen_df_baseline_full, pair[1], pair[2])
  key <- paste(pair, collapse = "__")
  tail_summaries[[key]]  <- res$summary
  tail_years_list[[key]] <- res$tail_years
}

tail_summary_out <- bind_rows(tail_summaries)
tail_years_out   <- bind_rows(tail_years_list)

message("\nTail-sensitivity summary (n_years, max value, years above pooled 90th pct):")
print(tail_summary_out)

message("\nTop years by recharge-metric value, per scenario:")
print(tail_years_out, n = 100)

write_csv(tail_summary_out, file.path(response_dir, "elder_tail_sensitivity_summary.csv"))
write_csv(tail_years_out,   file.path(response_dir, "elder_tail_sensitivity_years.csv"))

message("  Saved: elder_tail_sensitivity_summary.csv, elder_tail_sensitivity_years.csv")


# ==============================================================================
# PART 14 -- Raw data export for featured curves
#
# Exports the individual water-year points behind this site's featured
# tradeoff curve(s) -- exact recharge-metric value and % deviation from
# baseline, per scenario, per year. This is the same data the LOESS smooths
# in Part 12 are drawn through, so any visual reading of those curves
# (line ordering, crossovers, etc.) can be checked directly against numbers
# instead of pixels.
# ==============================================================================

message("\n-- PART 14: Raw data export for featured curves --------------------------")

elder_featured_curve_data <- screen_df_baseline_full %>%
  filter(
    (metric == "Wet_BFL_Mag_50" & recharge_metric == "Annual Diversion Volume (TAF)") |
    (metric == "Wet_BFL_Mag_50" & recharge_metric == "Total Diversion Days") |
    (metric == "Peak_Dur_2" & recharge_metric == "Annual Diversion Volume (TAF)")
  ) %>%
  select(Year, WYT, scenario_code, metric, recharge_metric, value,
         ffm_value, baseline_value, raw_deviation_from_baseline,
         pct_deviation_from_baseline) %>%
  arrange(metric, recharge_metric, scenario_code, value)

write_csv(elder_featured_curve_data, file.path(response_dir, "elder_featured_curve_raw_data.csv"))
message("  Saved: elder_featured_curve_raw_data.csv (", nrow(elder_featured_curve_data), " rows)")
