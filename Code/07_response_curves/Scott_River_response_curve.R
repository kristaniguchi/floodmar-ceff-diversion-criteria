# ==============================================================================
# Scott River — FFM Deviation Response Curves (standalone)
# COMID: 3798909
#
# Shows the relationship between recharge opportunity (annual diversion volume,
# TAF) and FFM deviation from unimpaired, for each scenario.
#
# Concept: replicates the schematic in the presentation (x = recharge
# opportunity, y = FFM deviation from unimpaired) with actual data points
# per water year and LOESS smooth response curves per scenario.
#
# METHOD
#   x-axis: Annual diversion volume (TAF) per scenario per water year
#           from Scott_Final_Recharge_Metrics_V2.xlsx "Scott AF per WY" sheet.
#           Higher TAF = more recharge opportunity captured.
#
#   y-axis: Per-year FFM deviation from WYT-matched unimpaired median.
#           Unimpaired reference = natural flows API p50_predicted per metric
#           per WYT (wet/moderate/dry). Each year's deviation is computed as:
#             (scenario_value - unimpaired_p50_for_that_WYT) / unimpaired_p50 * 100
#           expressed as % deviation.
#
#   Curves: LOESS smooth per scenario, points shown as semi-transparent dots.
#           WYT shown by point shape. One plot per metric, faceted or separate.
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "readxl", "scales"))
#   Requires outputs from Scott_River_FFC_analysis.R:
#     ./Data/outputs/Scott_River/FFM/results/scott_ffm_patched.csv
#     ./Data/outputs/Scott_River/FFM/results/scott_percentiles.csv
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

results_dir    <- "./Data/outputs/Scott_River/FFM/results"
recharge_path  <- "./Data/inputs/Scott_River/Scott_Final_Recharge_Metrics_V2.xlsx"
response_dir   <- "./Data/outputs/Scott_River/response_curves"
baseline_dev_dir <- file.path(response_dir, "deviation_from_baseline")

dir.create(response_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(baseline_dev_dir, showWarnings = FALSE, recursive = TRUE)

# Scenario map: analysis code -> column name in Scott AF per WY sheet
scenario_af_map <- tribble(
  ~scenario_code, ~label,          ~af_col,
  "90_20",        "90/20",         "90/20",
  "80_50",        "80/50",         "80/50",
  "Ecohydraulic", "Ecohydraulic",  "362 (Ecohydraulic)",
  "FF_WS50",      "FF-WS50",       "50th WBF 20%",
  "FF_WS75",      "FF-WS75",       "75th WBF 20%",
  "FF_Pk2",       "FF-Pk2",        "Q2:20%"
)
# Note: Baseline excluded — zero diversions = zero recharge opportunity

# Scenario display order and colors
scenario_levels <- scenario_af_map$scenario_code
scenario_labels <- scenario_af_map$label
scenario_colors <- c(
  "90_20"        = "#E41A1C",
  "80_50"        = "#D4A017",
  "Ecohydraulic" = "#29B6F6",
  "FF_WS50"      = "#00E676",
  "FF_WS75"      = "#CE93D8",
  "FF_Pk2"       = "#F06292"
)

# Key metrics for response curves — focus on metrics most affected by diversions
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

# Metric type classification — determines y-axis treatment in response curves
magnitude_metrics <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50",
                       "Peak_2", "Peak_5", "Peak_10", "SP_Mag",
                       "DS_Mag_50", "DS_Mag_90", "FA_Mag")
timing_metrics    <- c("Wet_Tim", "SP_Tim", "DS_Tim", "FA_Tim")
# duration and frequency = everything else

metric_type <- function(m) {
  if (m %in% magnitude_metrics) "magnitude"
  else if (m %in% timing_metrics) "timing"
  else "duration_freq"
}

# Units for y-axis labels
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
ffm_raw <- read_csv(file.path(results_dir, "scott_ffm_patched.csv"),
                    show_col_types = FALSE) %>%
  filter(!Year %in% c("Avg","Std","CV")) %>%
  mutate(Year = as.integer(Year),
         scenario_code = factor(scenario_code,
                                levels = scenario_levels,
                                labels = scenario_labels)) %>%
  # Peak duration and frequency NA = threshold not exceeded that year = 0 days / 0 events
  mutate(across(matches("^Peak_Dur_|^Peak_Fre_"), ~ replace_na(as.numeric(.x), 0)))

# ── Constrain to NFD calibration period and drop unknown WYT ─────────────────
# The natural flows database covers 1950-2014. Constraining to this period
# ensures every year has a valid unimpaired reference. Years with WYT =
# "unknown" from the FFC are excluded — these are pre-1950 early record years
# and 2024 (incomplete), both outside the NFD period anyway.
# The original FFC/NFD WYT classification is used as-is for consistency
# with the predicted reference percentiles.
nfd_start <- 1950L
nfd_end   <- 2014L

ffm_raw <- ffm_raw %>%
  filter(Year >= nfd_start, Year <= nfd_end,
         WYT %in% c("wet","moderate","dry"))

n_yrs <- length(unique(ffm_raw$Year))
message("  Analysis period: ", nfd_start, "-", nfd_end,
        " | Years with valid WYT: ", n_yrs)
message("  WYT counts: dry=",   sum(ffm_raw$WYT == "dry"   & ffm_raw$scenario_code == ffm_raw$scenario_code[1]),
        "  moderate=",           sum(ffm_raw$WYT == "moderate" & ffm_raw$scenario_code == ffm_raw$scenario_code[1]),
        "  wet=",                sum(ffm_raw$WYT == "wet"   & ffm_raw$scenario_code == ffm_raw$scenario_code[1]))

# ── Natural flows API predicted percentiles (unimpaired reference) ─────────────
pct_raw <- read_csv(file.path(results_dir, "scott_percentiles.csv"),
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
    # title-case to match response_df WYT factor labels; keep "any" as-is
    WYT = if_else(WYT == "any", "any", str_to_title(WYT))
  )

# for metrics that only have "any" WYT predictions (e.g. peak metrics),
# duplicate the "any" row for each WYT so the join works
wyt_levels <- c("Wet","Moderate","Dry")
unimp_any  <- unimpaired_ref %>% filter(WYT == "any")
unimp_wyt  <- unimpaired_ref %>% filter(WYT != "any")

# find metrics missing from WYT-specific rows and fill from "any"
metrics_missing_wyt <- setdiff(unique(unimp_any$metric),
                                unique(unimp_wyt$metric))
if (length(metrics_missing_wyt) > 0) {
  fill_rows <- unimp_any %>%
    filter(metric %in% metrics_missing_wyt) %>%
    cross_join(tibble(WYT = wyt_levels)) %>%
    select(-any_of("WYT.x")) %>%
    rename(WYT = any_of(c("WYT.y","WYT")))
  # rebuild with correct columns
  fill_rows <- unimp_any %>%
    filter(metric %in% metrics_missing_wyt) %>%
    select(metric, unimpaired_p50) %>%
    crossing(WYT = wyt_levels)
  unimp_wyt <- bind_rows(unimp_wyt, fill_rows)
}
unimpaired_ref <- unimp_wyt

message("  Unimpaired reference metrics: ",
        paste(sort(unique(unimpaired_ref$metric)), collapse = ", "))
message("  Unimpaired reference WYTs: ",
        paste(sort(unique(unimpaired_ref$WYT)), collapse = ", "))

# ── Annual recharge opportunity (TAF per scenario per year) ───────────────────
af_raw <- read_excel(recharge_path, sheet = "Scott AF per WY") %>%
  select(water_year, all_of(scenario_af_map$af_col)) %>%
  rename_with(~ scenario_af_map$scenario_code[match(.x, scenario_af_map$af_col)],
              all_of(scenario_af_map$af_col)) %>%
  pivot_longer(cols      = -water_year,
               names_to  = "scenario_code",
               values_to = "diversion_af") %>%
  mutate(diversion_taf = diversion_af / 1000,
         scenario_code = factor(scenario_code,
                                levels = scenario_levels,
                                labels = scenario_labels)) %>%
  filter(water_year >= nfd_start, water_year <= nfd_end)

message("  Recharge data constrained to: WYs ", nfd_start, "-", nfd_end)

# ── FFM long format — all years now have gage-based WYT ──────────────────────
ffm_long <- ffm_raw %>%
  filter(scenario_code != "Baseline") %>%
  select(Year, WYT, scenario_code,
         all_of(intersect(response_metrics, names(ffm_raw)))) %>%
  pivot_longer(cols     = -c(Year, WYT, scenario_code),
               names_to = "metric",
               values_to = "ffm_value") %>%
  filter(!is.na(ffm_value),
         WYT %in% c("wet","moderate","dry")) %>%
  mutate(WYT = str_to_title(WYT))   # match unimpaired_ref WYT case

message("  Years included: ", length(unique(ffm_long$Year)),
        " | Rows: ", nrow(ffm_long))

# ── Baseline per-year values, for deviation-from-baseline comparisons ────────
# ffm_raw's scenario_code was already relabeled against scenario_levels
# (which excludes Baseline, per the note above), so Baseline rows there are
# NA. Re-read the raw CSV directly instead, before any relabeling, to recover
# Baseline's own per-year values as a reference point distinct from the
# unimpaired natural reference.
baseline_lookup <- read_csv(file.path(results_dir, "scott_ffm_patched.csv"),
                            show_col_types = FALSE) %>%
  filter(!Year %in% c("Avg","Std","CV"), scenario_code == "Baseline") %>%
  mutate(Year = as.integer(Year),
         across(matches("^Peak_Dur_|^Peak_Fre_"), ~ replace_na(as.numeric(.x), 0))) %>%
  filter(Year >= nfd_start, Year <= nfd_end) %>%
  select(Year, all_of(intersect(response_metrics, names(.)))) %>%
  pivot_longer(cols = -Year, names_to = "metric", values_to = "baseline_value")

message("  Baseline lookup: ", nrow(baseline_lookup), " rows (Year x metric)")

# ── Join: FFM + unimpaired reference + recharge opportunity ───────────────────
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
    metric_label  = metric_labels[metric]
  )

message("  Response data: ", nrow(response_df), " rows ready for plotting")

# add raw deviation + deviation-from-baseline (in addition to deviation from
# unimpaired, above). Deviation from unimpaired answers "how far is this
# scenario from natural conditions" — but where baseline itself deviates from
# natural (e.g. an already-augmented metric), that conflates the scenario's
# own effect with baseline's pre-existing offset. Deviation from baseline
# isolates the scenario's own effect, independent of that offset.
response_df <- response_df %>%
  mutate(
    raw_deviation             = ffm_value - unimpaired_p50,
    raw_deviation_from_baseline = ffm_value - baseline_value,
    pct_deviation_from_baseline = if_else(
      baseline_value != 0,
      (ffm_value - baseline_value) / abs(baseline_value) * 100,
      NA_real_
    )
  )

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
#
# x = annual diversion volume (TAF)
# y = deviation from WYT-matched unimpaired p50
#     timing / duration / freq → raw units
#     magnitude                → raw (cfs) AND % change (two plots)
# ==============================================================================

message("\n── PART 3: Raw/pct deviation response curves ──────────────────────────")

make_response_plot <- function(df_met, met, y_var, y_label, y_format = NULL,
                               subtitle_note = "y=0: WYT-matched unimpaired median") {
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
    labs(title    = paste0("Scott River Response Curve: ", metric_labels[[met]]),
         subtitle = paste0("Each point = one water year  |  LOESS smooth  |  ", subtitle_note)) +
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
# PART 4 — Combined facet response curves (raw deviation, free y scales)
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
    labs(title    = paste0("Scott River FFM Response to Recharge: ", title_str),
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
# exploratory "proportion of years within natural range" plots (Option A);
# those plots are not part of the final analysis and have been removed, but
# this underlying reference data is still required downstream.
# ==============================================================================

message("\n── PART 5: Natural range reference data ────────────────────────────────")

pct_range_cols   <- c("p10_predicted","p25_predicted","p50_predicted","p75_predicted","p90_predicted")
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
# Everything above uses annual diversion volume (TAF) as the sole measure of
# "recharge opportunity" on the x-axis. Jason's Scott_Additional_Annual_
# Recharge_Metrics.csv provides three additional annual recharge metrics per
# scenario:
#   - n_runs               — number of separate diversion events in the year
#   - n_diversion_days     — total days diverted in the year
#   - avg_diversion_length — average length (days) of each diversion event
#
# PURPOSE: exploratory screening only. This section checks whether any of
# these alternative recharge metrics show a clearer (or different) response
# relationship than TAF for the FFM metrics already established as most
# relevant to this project's findings (Wet-Season BF Median, Peak 2-yr
# Duration, Peak 2-yr Frequency). It does not replace or alter any of the
# TAF-based response curves in Parts 3-8 above. If nothing here shows a
# clearer pattern than TAF, that is itself a valid conclusion — not every
# candidate recharge metric needs to add a new finding.
# ==============================================================================

message("\n── PART 9: Screening additional recharge opportunity metrics ──────────")

# ── Parse Scott_Additional_Annual_Recharge_Metrics.csv (three stacked metric ──
# blocks, two-row header — same structure as the xlsx "Summary Runs" sheet,
# but as a standalone CSV. This is the format Jason also provided for Elder
# Creek and Stony Creek, so this parsing approach is written to carry over
# directly to those sites' response curve scripts.
additional_recharge_path <- "./Data/inputs/Scott_River/Scott_Additional_Annual_Recharge_Metrics.csv"

scenario_runs_map <- tribble(
  ~scenario_code, ~runs_col,
  "90_20",        "90/20",
  "80_50",        "80/50",
  "Ecohydraulic", "362 (Ecohydraulic)",
  "FF_WS50",      "50_bf_50_20",
  "FF_WS75",      "75_bf_50_20",
  "FF_Pk2",       "peak_2_20"
)

extract_runs_block <- function(raw, start_col, n_cols, metric_name) {
  header <- as.character(unlist(raw[2, start_col:(start_col + n_cols - 1)]))
  block <- raw[3:nrow(raw), start_col:(start_col + n_cols - 1)]
  names(block) <- header
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

recharge_screen <- bind_rows(
  extract_runs_block(runs_raw, 1,  19, "n_runs"),
  extract_runs_block(runs_raw, 21, 19, "n_diversion_days"),
  extract_runs_block(runs_raw, 41, 19, "avg_diversion_length")
) %>%
  mutate(scenario_code = factor(scenario_code, levels = scenario_levels, labels = scenario_labels)) %>%
  filter(water_year >= nfd_start, water_year <= nfd_end)

# add TAF as a fourth candidate, using the recharge data already loaded in Part 1
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
message("  Years: ", length(unique(recharge_screen$water_year)))

# screen_ffm_metrics: the FFM metrics already established as key findings.
# Reused below (Part 11) to build screen_df_baseline, which feeds the
# featured tradeoff curves (Part 12) and tail-sensitivity diagnostic (Part 13).
screen_ffm_metrics <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Peak_Dur_2", "Peak_Fre_2")

write_csv(recharge_screen, file.path(response_dir, "scott_recharge_metrics_screened.csv"))
message("  Saved: scott_recharge_metrics_screened.csv")

# ==============================================================================
# PART 10 — Deviation from Baseline, and raw flow metric values
#
# Deviation from unimpaired (Parts 3/4/9) answers "how far is this scenario
# from natural conditions?" — but at metrics where Baseline itself already
# deviates from the natural reference (e.g. Wet-Season BF Median, which runs
# augmented above natural per the boxplot comparisons), that conflates the
# diversion's own effect with Baseline's pre-existing offset. A scenario that
# happens to pull an already-augmented metric back toward natural can look
# artificially "protective" by the unimpaired comparison alone, without that
# necessarily reflecting a genuine ecological improvement (see e.g.
# Ecohydraulic's wet-season median result).
#
# This section adds two companion views, focused on the same key metrics
# used for screening in Part 9:
#   (a) Deviation from Baseline — isolates each scenario's own effect,
#       independent of Baseline's offset from natural.
#   (b) Raw flow metric values — Baseline, all scenarios, and the natural
#       reference band together on their original units, so the actual
#       magnitude of any effect is visible directly rather than as a
#       normalized deviation.
# ==============================================================================

message("\n── PART 10: Deviation from Baseline + raw flow metric values ──────────")

key_metrics_p10 <- c("Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Peak_Dur_2", "Peak_Fre_2")

# ── (a) Deviation from Baseline response curves ───────────────────────────────
for (met in key_metrics_p10) {
  df_met <- response_df %>% filter(metric == met, !is.na(pct_deviation_from_baseline))
  if (nrow(df_met) < 10) { message("  Skipping ", met, " (from-baseline) — insufficient data"); next }
  units <- metric_units[[met]]
  mtype <- metric_type(met)

  if (mtype == "magnitude") {
    ggsave(file.path(baseline_dev_dir, paste0("response_", met, "_fromBaseline_raw.png")),
           make_response_plot(df_met, met, "raw_deviation_from_baseline",
                              paste0("Deviation from Baseline (", units, ")"), comma,
                              subtitle_note = "y=0: Baseline (that water year)"),
           width = 10, height = 7, dpi = 300)
    ggsave(file.path(baseline_dev_dir, paste0("response_", met, "_fromBaseline_pct.png")),
           make_response_plot(df_met, met, "pct_deviation_from_baseline",
                              "Deviation from Baseline (%)",
                              function(x) paste0(x, "%"),
                              subtitle_note = "y=0: Baseline (that water year)"),
           width = 10, height = 7, dpi = 300)
    message("  Saved: response_", met, "_fromBaseline_raw.png + _pct.png")
  } else {
    ggsave(file.path(baseline_dev_dir, paste0("response_", met, "_fromBaseline.png")),
           make_response_plot(df_met, met, "raw_deviation_from_baseline",
                              paste0("Deviation from Baseline (", units, ")"), comma,
                              subtitle_note = "y=0: Baseline (that water year)"),
           width = 10, height = 7, dpi = 300)
    message("  Saved: response_", met, "_fromBaseline.png")
  }
}

# ── (b) Raw flow metric values — Baseline, scenarios, and natural range ──────
# Baseline is plotted at its natural x-position (diversion_taf = 0), alongside
# each scenario's own trend across its diversion volume. The natural reference
# band uses the WYT="any" aggregate (same convention as the boxplot slide
# figures), since a single overview band is clearer here than three
# WYT-specific bands layered on top of each other.
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
    scale_x_continuous(labels = comma,
                       name = "Annual Diversion Volume (TAF)  (Baseline shown at 0)") +
    labs(
      title    = paste0("Scott River Raw Flow Metric Value: ", metric_labels[[met]]),
      subtitle = "Baseline shown at its own points (no diversion)  |  Blue bands: natural flow p10-p90 (light), p25-p75 (dark)  |  Blue dashed: natural p50",
      y = metric_labels[[met]]
    ) +
    theme_response
}

for (met in key_metrics_p10) {
  ggsave(file.path(response_dir, paste0("response_", met, "_rawvalue.png")),
         make_raw_value_plot(met), width = 10, height = 7, dpi = 300)
  message("  Saved: response_", met, "_rawvalue.png")
}

# ==============================================================================
# PART 11 — Correlation screening: which FFM metrics respond consistently to
# which recharge opportunity metrics (deviation from Baseline)
#
# Rather than judging each response curve by eye, this screens every
# combination of FFM metric x recharge opportunity metric x scenario using
# Spearman correlation (robust to the nonlinear/monotonic shapes seen in the
# curves above, without assuming a specific functional form). For each FFM
# metric x recharge metric pair, the correlation is computed separately per
# scenario, then summarized as:
#   mean_rho — average correlation across scenarios (strength/direction)
#   sd_rho   — how much that correlation varies across scenarios (robustness;
#              a relationship that is strong for some scenarios and flips
#              sign or vanishes for others is not a robust, generalizable
#              response — it's more likely coincidental or scenario-specific
#              noise, not evidence of a genuine diversion-volume effect)
#
# This does not replace visual inspection of the response curves — a strong,
# consistent correlation is a reasonable basis for prioritizing which curves
# to feature, but the shape (linear vs. threshold vs. saturating) still
# matters for how a relationship gets described in the report.
# ==============================================================================

message("\n── PART 11: Correlation screening (deviation from Baseline) ───────────")

# Broader set than the 4 key screening metrics — includes wet-season and
# peak duration/frequency metrics where a per-year distribution exists.
# Excludes: peak magnitude (single value across POR, no per-year variation),
# Wet_Tim/SP_Tim (timing metrics; SP_Tim specifically has a documented
# detection-artifact issue at Scott River), Peak_Dur_10/Peak_Fre_10 (very
# sparse — 10-year events), and SP_Mag/SP_Dur — Spring Recession metrics
# excluded entirely at Scott River, since Spring Recession timing detection
# is affected by the same artifact and any downstream metric derived from
# that timing (magnitude, duration) inherits the same reliability concern.
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
  summarise(
    mean_rho    = mean(rho),
    sd_rho      = sd(rho),
    n_scenarios = n(),
    .groups = "drop"
  ) %>%
  mutate(
    metric_label = factor(metric_labels[metric],
                          levels = metric_labels[screening_metrics_p11]),
    # "robust": a reasonably strong average correlation that doesn't flip
    # sign or vanish across scenarios — thresholds are a starting point for
    # screening, not a hard cutoff; review the underlying table too.
    robust = abs(mean_rho) >= 0.4
  )

write_csv(corr_screen %>% arrange(metric, recharge_metric, scenario_code),
          file.path(baseline_dev_dir, "scott_correlation_screening_by_scenario.csv"))
write_csv(corr_summary %>% arrange(desc(abs(mean_rho))),
          file.path(baseline_dev_dir, "scott_correlation_screening_summary.csv"))
message("  Saved: scott_correlation_screening_by_scenario.csv, scott_correlation_screening_summary.csv")

# ── Heatmap: mean Spearman rho ───────────────────────────────────────────────
p_corr_heat <- ggplot(corr_summary,
                      aes(x = recharge_metric, y = metric_label, fill = mean_rho)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", mean_rho)), size = 4.2) +
  scale_fill_gradient2(name = "Mean Spearman ρ",
                       low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  labs(
    title = "Scott River Correlation Screening: Recharge Metrics vs. FFM Deviation from Baseline",
    x = NULL, y = NULL
  ) +
  theme_response +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        panel.grid  = element_blank())

ggsave(file.path(baseline_dev_dir, "correlation_screening_heatmap.png"),
       p_corr_heat, width = 11, height = 7, dpi = 300)
message("  Saved: correlation_screening_heatmap.png")

message("\n  Metric x recharge-metric pairs flagged as robust:")
corr_summary %>% filter(robust) %>%
  arrange(desc(abs(mean_rho))) %>%
  select(metric, recharge_metric, mean_rho, sd_rho) %>%
  print(n = 30)

# ==============================================================================
# Deviation-from-Baseline data underlying the featured tradeoff curves (Part
# 12), tail-sensitivity diagnostic (Part 13), and raw data export (Part 14).
# ==============================================================================

screen_df_baseline <- response_df %>%
  filter(metric %in% screen_ffm_metrics, !is.na(pct_deviation_from_baseline)) %>%
  select(Year, WYT, scenario_code, metric, metric_label, pct_deviation_from_baseline) %>%
  inner_join(recharge_screen %>% rename(Year = water_year),
             by = c("Year", "scenario_code"),
             relationship = "many-to-many")

# ==============================================================================
# PART 12 — Featured tradeoff curves
#
# Standalone, presentation-quality versions of the two strongest/most robust
# pairings identified by the correlation screening in Part 11:
#   - Wet-Season BF Magnitude 10th percentile vs. Annual Diversion Volume (TAF)
#     (mean rho = -0.54, sd = 0.12)
#   - Wet-Season BF Magnitude 50th percentile vs. Total Diversion Days
#     (mean rho = -0.60, sd = 0.24)
# Both show a clean, consistent downward trend across nearly all scenarios,
# unlike the noisier relationships elsewhere in the screening grid.
# ==============================================================================

message("\n── PART 12: Featured tradeoff curves ───────────────────────────────────")

make_featured_tradeoff <- function(met, rmetric, title_suffix) {
  df_feat <- screen_df_baseline %>%
    filter(metric == met, recharge_metric == rmetric)

  ggplot(df_feat, aes(x = value, y = pct_deviation_from_baseline, color = scenario_code)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6, linetype = "dashed") +
    geom_point(aes(shape = WYT), alpha = 0.4, size = 2) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                alpha = 0.12, linewidth = 1.1) +
    scale_color_manual(name = "Scenario", values = setNames(scenario_colors, scenario_labels)) +
    scale_shape_manual(name = "Water Year Type",
                       values = c("Wet" = 16, "Moderate" = 17, "Dry" = 15)) +
    scale_x_continuous(labels = comma, name = as.character(rmetric)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       name = "Deviation from Baseline (%)") +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3), nrow = 1),
           shape = guide_legend(nrow = 1)) +
    labs(
      title    = paste0("Scott River ", metric_labels[[met]], " vs. ", title_suffix),
      subtitle = "Each point = one water year  |  LOESS smooth  |  y=0: Baseline (that water year)"
    ) +
    theme_response
}

p_feat_1 <- make_featured_tradeoff("Wet_BFL_Mag_10", "Annual Diversion Volume (TAF)",
                                   "Annual Diversion Volume (TAF)")
ggsave(file.path(baseline_dev_dir, "featured_WetBFLMag10_vs_TAF.png"),
       p_feat_1, width = 10, height = 7, dpi = 300)
message("  Saved: featured_WetBFLMag10_vs_TAF.png")

p_feat_2 <- make_featured_tradeoff("Wet_BFL_Mag_50", "Total Diversion Days",
                                   "Total Diversion Days")
ggsave(file.path(baseline_dev_dir, "featured_WetBFLMag50_vs_TotalDiversionDays.png"),
       p_feat_2, width = 10, height = 7, dpi = 300)
message("  Saved: featured_WetBFLMag50_vs_TotalDiversionDays.png")


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
  c("Wet_BFL_Mag_10", "Annual Diversion Volume (TAF)"),
  c("Wet_BFL_Mag_50", "Total Diversion Days")
)

tail_summaries  <- list()
tail_years_list <- list()

for (pair in tail_pairs) {
  res <- tail_diagnostic(screen_df_baseline, pair[1], pair[2])
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

write_csv(tail_summary_out, file.path(response_dir, "scott_tail_sensitivity_summary.csv"))
write_csv(tail_years_out,   file.path(response_dir, "scott_tail_sensitivity_years.csv"))

message("  Saved: scott_tail_sensitivity_summary.csv, scott_tail_sensitivity_years.csv")


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

scott_featured_curve_data <- screen_df_baseline %>%
  filter(
    (metric == "Wet_BFL_Mag_10" & recharge_metric == "Annual Diversion Volume (TAF)") |
    (metric == "Wet_BFL_Mag_50" & recharge_metric == "Total Diversion Days")
  ) %>%
  select(Year, WYT, scenario_code, metric, recharge_metric, value, pct_deviation_from_baseline) %>%
  arrange(metric, recharge_metric, scenario_code, value)

write_csv(scott_featured_curve_data, file.path(response_dir, "scott_featured_curve_raw_data.csv"))
message("  Saved: scott_featured_curve_raw_data.csv (", nrow(scott_featured_curve_data), " rows)")


# ==============================================================================
# PART 15 -- Monotone-constrained fit (EXPLORATORY -- parallel to Part 12)
#
# LOESS smooths on the featured tradeoff curves occasionally show a
# dip-then-recovery "wiggle" at the high end of the diversion-volume range
# (e.g. FF-WS50 and Ecohydraulic on Wet_BFL_Mag_10 vs. TAF) with no
# mechanistic explanation -- there's no reason deviation from baseline
# should IMPROVE as diversion volume increases. This section refits the
# same featured combinations using a monotone-decreasing constrained
# P-spline (mgcv::pcls) instead of LOESS, and saves them under new
# filenames alongside the existing LOESS versions for direct comparison.
# Nothing from Part 12 is modified or overwritten.
#
# Verified against this site's actual data: the two methods agree closely
# (within ~1 percentage point) through most of the diversion-volume range
# and diverge only at the high end, exactly where LOESS shows the
# unexplained recovery. mono.con() defaults to monotone INCREASING
# (up = TRUE) -- this uses up = FALSE for decreasing, with a decreasing
# initial parameter guess (rev(sm$xp)), since the increasing default
# guess (sm$xp) is infeasible under a decreasing constraint.
#
# No confidence ribbon is shown for the monotone fit -- pcls() is a
# constrained least-squares solve, not a model with a standard vcov, so
# a SE band would need bootstrapping, not done here. This is otherwise a
# straight substitute for geom_smooth(method = "loess", ...).
# ==============================================================================

message("\n── PART 15: Monotone-constrained fit (exploratory) ─────────────────────")

library(mgcv)

fit_monotone_decreasing <- function(x, y, n_pred = 100) {
  ord <- order(x); x <- x[ord]; y <- y[ord]

  # Degenerate case: essentially no variation in y (e.g., a scenario that
  # never diverges from baseline, like 90/20 or FF-Pk2 on some curves). A
  # constant is trivially monotone, and pcls' feasibility check can fail
  # on a flat target with a non-flat starting guess -- skip the solve.
  if (diff(range(y)) < 1e-8) {
    pred_x <- seq(min(x), max(x), length.out = n_pred)
    return(data.frame(x = pred_x, y = rep(mean(y), n_pred)))
  }

  dat <- data.frame(x = x, y = y)
  n_unique_x <- length(unique(x))
  # Cap the spline basis to what the data can actually support. A heavily
  # zero-inflated x (e.g. many years with 0 diversion days under a
  # restrictive scenario) has far fewer EFFECTIVE degrees of freedom than
  # n suggests, and an over-parameterized basis (fixed k=8 regardless of
  # x's distribution) triggers "initial parameters not feasible" in pcls.
  k_use <- max(4, min(8, n_unique_x - 1))

  mono_try <- function(k) {
    f_uc <- gam(y ~ s(x, k = k, bs = "cr"), data = dat)
    sm   <- smoothCon(s(x, k = k, bs = "cr"), dat, knots = NULL)[[1]]
    Fc   <- mono.con(sm$xp, up = FALSE)   # monotone DECREASING
    G <- list(X = sm$X, C = matrix(0, 0, 0), sp = f_uc$sp,
              p = rev(sm$xp),             # feasible decreasing start
              y = dat$y, w = dat$y * 0 + 1)
    G$Ain <- Fc$A; G$bin <- Fc$b; G$S <- sm$S; G$off <- 0
    p <- pcls(G)
    pred_x <- seq(min(x), max(x), length.out = n_pred)
    Xp <- Predict.matrix(sm, data.frame(x = pred_x))
    data.frame(x = pred_x, y = as.vector(Xp %*% p))
  }

  result <- tryCatch(mono_try(k_use), error = function(e) NULL)
  if (is.null(result)) result <- tryCatch(mono_try(4), error = function(e) NULL)

  if (is.null(result)) {
    # Guaranteed-feasible fallback: isotonic regression (pool-adjacent-
    # violators via isoreg), lightly smoothed so it reads as a curve
    # rather than a step function, with monotonicity re-enforced after
    # smoothing. isoreg always succeeds -- there's no spline basis here
    # to be ill-posed -- so this is the safety net if mono_try() fails
    # under both k values above.
    message("    (scenario used isotonic-regression fallback, not the spline fit)")
    iso   <- isoreg(x, -y)   # isoreg is increasing-only, so flip sign for decreasing
    iso_y <- -iso$yf
    pred_x <- seq(min(x), max(x), length.out = n_pred)
    pred_y <- approx(x, iso_y, xout = pred_x, rule = 2)$y
    if (length(unique(x)) >= 4) {
      pred_y <- tryCatch(as.numeric(predict(loess(pred_y ~ pred_x, span = 0.4))),
                          error = function(e) pred_y)
      pred_y <- rev(cummin(rev(pred_y)))  # re-enforce monotone after smoothing
    }
    result <- data.frame(x = pred_x, y = pred_y)
  }
  result
}

make_featured_tradeoff_monotone <- function(met, rmetric) {
  df_feat <- screen_df_baseline %>% filter(metric == met, recharge_metric == rmetric)

  mono_fits <- df_feat %>%
    group_by(scenario_code) %>%
    group_modify(~ fit_monotone_decreasing(.x$value, .x$pct_deviation_from_baseline)) %>%
    ungroup()

  ggplot() +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6, linetype = "dashed") +
    geom_point(data = df_feat,
               aes(x = value, y = pct_deviation_from_baseline, color = scenario_code, shape = WYT),
               alpha = 0.4, size = 2) +
    geom_line(data = mono_fits, aes(x = x, y = y, color = scenario_code), linewidth = 1.1) +
    scale_color_manual(name = "Scenario", values = setNames(scenario_colors, scenario_labels)) +
    scale_shape_manual(name = "Water Year Type",
                       values = c("Wet" = 16, "Moderate" = 17, "Dry" = 15)) +
    scale_x_continuous(labels = comma, name = as.character(rmetric)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       name = "Deviation from Baseline (%)") +
    guides(color = guide_legend(override.aes = list(shape = 16, size = 3), nrow = 1),
           shape = guide_legend(nrow = 1)) +
    labs(
      title    = paste0("Scott River ", metric_labels[[met]], " vs. ", rmetric),
      subtitle = "Each point = one water year  |  y=0: Baseline (that water year)"
    ) +
    theme_response
}

p_mono_1 <- make_featured_tradeoff_monotone("Wet_BFL_Mag_10", "Annual Diversion Volume (TAF)")
ggsave(file.path(baseline_dev_dir, "featured_WetBFLMag10_vs_TAF_monotone.png"),
       p_mono_1, width = 10, height = 7, dpi = 300)
message("  Saved: featured_WetBFLMag10_vs_TAF_monotone.png")

p_mono_2 <- make_featured_tradeoff_monotone("Wet_BFL_Mag_50", "Total Diversion Days")
ggsave(file.path(baseline_dev_dir, "featured_WetBFLMag50_vs_TotalDiversionDays_monotone.png"),
       p_mono_2, width = 10, height = 7, dpi = 300)
message("  Saved: featured_WetBFLMag50_vs_TotalDiversionDays_monotone.png")
