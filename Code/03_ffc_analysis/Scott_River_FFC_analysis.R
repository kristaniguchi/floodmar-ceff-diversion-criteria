# ==============================================================================
# Scott River — FloodMAR FFC Analysis
# COMID: 3798909
#
# WORKFLOW
#   1. Load Scott_Available.csv — observed discharge + water available to divert
#      by scenario. Subtract diversions from observed to get instream flow for
#      each scenario. Save individual FFC-ready daily flow CSVs (date, flow).
#
#   2. Fetch unimpaired peak magnitude thresholds from the natural flows API
#      (once per site, before the loop).
#
#   3. Loop over scenarios:
#        a. POST daily flow CSV to FFC API -> raw FFMs, alteration, percentiles
#        b. Save raw FFC output_metrics to disk (original annual results CSV)
#        c. Patch Peak_Dur/Fre in the annual results CSV using unimpaired
#           magnitude thresholds -> save patched version to disk
#        d. Compute Peak_Tim_2/5/10 (excluded from FFC output) at same thresholds
#        e. Rerun alteration assessment using patched annual results + API
#           predicted percentile reference -> corrected alteration table
#        f. Collect all results
#
#   4. Export all results tables
#
# SCENARIOS
#   Baseline     — observed flow, no diversions (reference condition)
#   90/20        — flow > 90th percentile; 20% daily diversion cap
#   80/50        — cumulative flow > 80th pctile; wet-season median bypass; 20% cap
#   Ecohydraulic — flow > 362 cfs; 20% daily diversion cap
#   FF-WS50      — flow > wet-season 50th pctile; preserve key floods; 20% cap
#   FF-WS75      — flow > wet-season 75th pctile; preserve key floods; 20% cap
#   FF-Pk2       — flow > 2-year peak threshold;  preserve key floods; 20% cap
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "httr", "readr"))
#   peak_fix_integration.R in ./Code/
# ==============================================================================

library(tidyverse)
library(lubridate)
library(httr)
library(readr)

# set working directory
#setwd("C:/Users/adrianal/SCCWRP/CEFF - FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
#setwd("C:/Users/kristinet/SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
setwd("/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")

# source code to fix peak freq, duration, timing calculations
source("./Code/peak_fix_integration.R")

# ==============================================================================
# fix_ws_mag_baseline_window(csv_path, scenario_daily, baseline_window, out_path)
#
# SCOTT RIVER ONLY — not part of peak_fix_integration.R, since this correction
# is only needed at sites where Spring Recession timing (SP_Tim) diverges
# across scenarios relative to Baseline. Check Spring Recession timing
# boxplots first; only apply this fix if timing actually diverges (see
# tech memo, Functional Flows Alteration Assessment section).
#
# PROBLEM
# Wet_BFL_Mag_10/50 are computed over each scenario's own detected wet-season
# window (Wet_Tim to SP_Tim). If diversion shifts SP_Tim (spring recession
# timing) relative to Baseline, the window itself changes, so magnitude
# comparisons across scenarios are no longer apples-to-apples with Baseline.
#
# FIX
# Recompute Wet_BFL_Mag_10/50 for every scenario using BASELINE's wet-season
# window (Wet_Tim to SP_Tim) for the matching water year, applied to that
# scenario's own daily flow series. This isolates the true magnitude effect
# of diversion from the window-shift artifact.
#
# Arguments:
#   csv_path        Path to the annual results CSV, already patched by
#                    fix_and_write_csv() (Peak_Dur/Fre/Tim corrected).
#   scenario_daily   data.frame(date = Date, flow = numeric) for this scenario.
#   baseline_window  data.frame(Year, Wet_Tim, SP_Tim) from Baseline's own
#                    patched results — Wet_Tim/SP_Tim in day-of-water-year
#                    (day 1 = Oct 1).
#   out_path         Where to write. Defaults to csv_path (overwrites in place).
#
# NOTE ON PERCENTILE METHOD: matches FFC's own calc_fall_winter_baseflow(),
# which uses np.nanpercentile(flow_data, 10) and np.nanpercentile(flow_data,
# 50) — equivalent to R's default quantile(type = 7) and median().
#
# NOTE ON WINDOW BOUNDARIES: this matches FFC's actual window as-is, which is
# (Wet_Tim, SP_Tim] — i.e. it EXCLUDES the Wet_Tim day itself and INCLUDES
# the SP_Tim day itself. Confirmed empirically against
# Baseline_annual_flow_result.csv (Scott River, Reference calculator): this
# convention reproduces the reported Wet_BFL_Mag_10/50 exactly across all 65
# years tested; a fully inclusive [Wet_Tim, SP_Tim] window does not. This
# code intentionally matches FFC's behavior as-is rather than a corrected
# version — see tech memo for the discrepancy flagged to TNC as a possible bug.
# ==============================================================================

fix_ws_mag_baseline_window <- function(csv_path,
                                        scenario_daily,
                                        baseline_window,
                                        out_path = csv_path) {

  raw       <- read_csv(csv_path, show_col_types = FALSE, col_types = cols(.default = "c"))
  data_mask <- !raw$Year %in% c("Avg", "Std", "CV")
  df        <- raw[data_mask, ]
  df$Year   <- as.integer(df$Year)

  char_cols    <- c("Overall_Int_Class", "Int_Class", "WYT")
  numeric_cols <- setdiff(names(df), c("Year", char_cols))
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) suppressWarnings(as.numeric(x)))

  mat <- .build_flow_matrix(scenario_daily)   # reuses helper from peak_fix_integration.R

  new_mag10 <- rep(NA_real_, nrow(df))
  new_mag50 <- rep(NA_real_, nrow(df))

  for (i in seq_len(nrow(df))) {
    wy_chr <- as.character(df$Year[i])
    if (!wy_chr %in% colnames(mat)) next

    bw <- baseline_window[baseline_window$Year == df$Year[i], ]
    if (nrow(bw) == 0 || is.na(bw$Wet_Tim[1]) || is.na(bw$SP_Tim[1])) next

    start_day <- bw$Wet_Tim[1] + 1L   # matches FFC as-is — window excludes Wet_Tim
    end_day   <- bw$SP_Tim[1]         # matches FFC as-is — window includes SP_Tim
    if (is.na(start_day) || is.na(end_day) || end_day < start_day) next

    flow_window <- mat[start_day:end_day, wy_chr]
    flow_window <- flow_window[!is.na(flow_window)]
    if (length(flow_window) == 0) next

    new_mag10[i] <- as.numeric(quantile(flow_window, 0.10, na.rm = TRUE))
    new_mag50[i] <- as.numeric(median(flow_window, na.rm = TRUE))
  }

  df$Wet_BFL_Mag_10 <- new_mag10
  df$Wet_BFL_Mag_50 <- new_mag50

  raw[data_mask, ] <- df %>% mutate(across(everything(), as.character))
  write_csv(raw, out_path)
  message("  WS baseflow magnitude patched (Baseline window) and written to: ", out_path)
  invisible(out_path)
}

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# ── Inputs ────────────────────────────────────────────────────────────────────
input_dir            <- "./Data/inputs/Scott_River"
scott_available_path <- file.path(input_dir, "Scott_Available.csv")

# Daily flow timeseries CSVs written here (two columns: date, flow)
# Upload any of these to the FFC online tool for visual gut checks
ffc_input_dir        <- file.path(input_dir, "ffc_inputs")

# ── Outputs ───────────────────────────────────────────────────────────────────
output_dir           <- "./Data/outputs/Scott_River"

# FFC annual results CSVs (output_metrics from API)
ffm_original_dir     <- file.path(output_dir, "FFM/original")  # raw from API
ffm_patched_dir      <- file.path(output_dir, "FFM/patched")   # Peak_Dur/Fre corrected

# Combined results tables (all scenarios bound together)
results_dir          <- file.path(output_dir, "FFM/results")

# Placeholder dirs for future outputs (hydrographs, boxplots, etc.)
# hydrograph_dir     <- file.path(output_dir, "hydrographs")
# boxplot_dir        <- file.path(output_dir, "boxplots")

# ── Site parameters ───────────────────────────────────────────────────────────
comid   <- 3798909
ffc_url <- "https://flowcalculator.codefornature.org/api/calculator"

dir.create(ffc_input_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(ffm_original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(ffm_patched_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir,      showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# PART 1 — Build instream flow timeseries for each scenario
#
# Scott_Available.csv columns:
#   discharge_cfs        — observed daily flow at the gage (no diversions)
#   <scenario columns>   — water available to divert (cfs) under each scenario
#
# Instream flow = discharge_cfs - water_available (floored at 0)
# Baseline = discharge_cfs with no subtraction
# ==============================================================================

raw <- read_csv(scott_available_path, show_col_types = FALSE) %>%
  mutate(date = mdy(date))

# NFD calibration period — constrain FFC submission and alteration assessment
# to match the natural flows database reference period (1950 water year start
# = Oct 1 1949, through end of WY2014 = Sep 30 2014)
nfd_start_date <- as.Date("1949-10-01")   # start of WY1950
nfd_end_date   <- as.Date("2014-09-30")   # end of WY2014

raw_nfd <- raw %>% filter(date >= nfd_start_date, date <= nfd_end_date)

message("NFD period: ", nfd_start_date, " to ", nfd_end_date,
        " (", nrow(raw_nfd), " days)")

# Scenario map: code, display label, and which available-water column to subtract
scenario_map <- tribble(
  ~scenario_code, ~label,          ~available_col,
  "Baseline",     "Baseline",      NA,                    # no diversion
  "90_20",        "90/20",         "90/20",
  "80_50",        "80/50",         "80/50",
  "Ecohydraulic", "Ecohydraulic",  "362 (Ecohydraulic)",
  "FF_WS50",      "FF-WS50",       "50_bf_50_20",
  "FF_WS75",      "FF-WS75",       "75_bf_50_20",
  "FF_Pk2",       "FF-Pk2",        "peak_2_20"
)

# Two versions of daily flows:
#   daily_flows     — full record (all years) used for hydrograph plots
#   daily_flows_nfd — NFD period only, used for FFC submission + peak fix
daily_flows     <- list()
daily_flows_nfd <- list()

for (i in seq_len(nrow(scenario_map))) {

  sc  <- scenario_map$scenario_code[i]
  col <- scenario_map$available_col[i]

  # full record
  df_full <- if (is.na(col)) {
    raw %>% transmute(date, flow = discharge_cfs)
  } else {
    raw %>% transmute(date, flow = pmax(discharge_cfs - .data[[col]], 0))
  }

  # NFD period only
  df_nfd <- if (is.na(col)) {
    raw_nfd %>% transmute(date, flow = discharge_cfs)
  } else {
    raw_nfd %>% transmute(date, flow = pmax(discharge_cfs - .data[[col]], 0))
  }

  daily_flows[[sc]]     <- df_full
  daily_flows_nfd[[sc]] <- df_nfd

  # write NFD-period CSV for FFC submission
  out_path <- file.path(ffc_input_dir, paste0(sc, "_flow.csv"))
  write_csv(df_nfd, out_path)
  message("Saved daily flow CSV (", nfd_start_date, " to ", nfd_end_date, "): ", out_path)
}

# ==============================================================================
# PART 2 — Fetch unimpaired peak magnitude thresholds (once per site)
#
# Returns predicted unimpaired Peak_2/5/10 magnitudes at p10-p90 from the
# natural flows database. We use $p50 — the only percentile directly comparable
# to the API's Peak_Dur_*/Peak_Fre_* reference percentiles, which were computed
# by the natural flows model using the median unimpaired magnitude as threshold.
# ==============================================================================

thresholds <- get_peak_thresholds(comid = comid)

# ==============================================================================
# PART 3 — Loop over scenarios
# ==============================================================================

ffm_df_all       <- data.frame()
ffm_precorr_all  <- data.frame()   # Peak_Dur/Fre-corrected, but NOT Wet_BFL_Mag-corrected —
                                    # this is what FFC actually reported for Wet_BFL_Mag_10/50,
                                    # kept for comparison against the wsmag-corrected version
alt_df_all  <- data.frame()
pct_df_all  <- data.frame()
meta_df_all <- data.frame()
timing_list <- list()

# Captured from Baseline (processed first in scenario_map) — used to correct
# Wet_BFL_Mag_10/50 for the Spring Recession timing artifact (Scott River only).
baseline_ws_window <- NULL

for (sc in scenario_map$scenario_code) {

  label         <- scenario_map$label[scenario_map$scenario_code == sc]
  daily_csv     <- file.path(ffc_input_dir,    paste0(sc, "_flow.csv"))
  ffm_orig_csv  <- file.path(ffm_original_dir, paste0(sc, "_annual_flow_result.csv"))
  ffm_patch_csv <- file.path(ffm_patched_dir,  paste0(sc, "_annual_flow_result_patched.csv"))
  ffm_wsmag_csv <- file.path(ffm_patched_dir,  paste0(sc, "_annual_flow_result_patched_wsmag.csv"))

  message("\n── ", label, " (", sc, ") ──────────────────────────────")

  # ── a. POST daily flow CSV to FFC API ───────────────────────────────────────
  message("  Submitting to FFC API...")
  response <- POST(
    url    = ffc_url,
    body   = list(
      calculator = "recommended",
      comid      = as.character(comid),
      box_plots  = FALSE,
      file       = upload_file(daily_csv)
    ),
    encode = "multipart"
  )

  if (status_code(response) != 200) {
    warning("FFC API call failed for '", sc, "' (HTTP ", status_code(response), "). Skipping.")
    next
  }

  result <- content(response, as = "parsed")

  if (!is.null(result$warnings) && nchar(result$warnings) > 0)
    message("  API warnings: ", result$warnings)

  # ── b. Parse and save raw FFC outputs ───────────────────────────────────────
  # output_metrics: annual per-year FFM values — save as the original results CSV
  ffm_df  <- read.csv(text = result$output_metrics,     header = TRUE)
  alt_df  <- read.csv(text = result$alteration_results, header = TRUE)
  pct_df  <- read.csv(text = result$percentiles,        header = TRUE)
  meta_df <- read.csv(text = result$metadata_file,      header = TRUE)

  write_csv(ffm_df, ffm_orig_csv)
  message("  Saved original FFM results: ", ffm_orig_csv)

  # ── c. Patch Peak_Dur/Fre in the annual results CSV ─────────────────────────
  # fix_and_write_csv() reads the saved annual results CSV, replaces
  # Peak_Dur_2/5/10 and Peak_Fre_2/5/10 with values recomputed at the
  # unimpaired p50 magnitude threshold, and writes the corrected version.
  # The original ffm_orig_csv is untouched.
  fix_and_write_csv(
    csv_path       = ffm_orig_csv,
    scenario_daily = daily_flows_nfd[[sc]],   # NFD period only — matches FFC submission
    thresholds     = thresholds$p50,
    out_path       = ffm_patch_csv
  )
  message("  Saved patched FFM results: ", ffm_patch_csv)

  # ── c2. Patch Wet_BFL_Mag_10/50 using Baseline's wet-season window ─────────
  # Scott River only — Spring Recession timing diverges under diversion here,
  # which shifts the detected wet-season window and contaminates magnitude
  # comparisons. Baseline is processed first, so its window is captured below
  # and reused for every subsequent scenario (including a no-op pass on
  # Baseline itself, which should reproduce its own original values exactly).
  #
  # Writes to a SEPARATE file (ffm_wsmag_csv) rather than overwriting
  # ffm_patch_csv a second time — this file lives in a OneDrive-synced
  # folder, and writing + immediately re-reading the same path twice within
  # one loop iteration was intermittently stalling on a sync lock.
  if (sc == "Baseline") {
    baseline_ws_window <- read_csv(ffm_patch_csv, show_col_types = FALSE) %>%
      filter(!Year %in% c("Avg", "Std", "CV")) %>%
      mutate(Year = as.integer(Year)) %>%
      select(Year, Wet_Tim, SP_Tim)
  }

  if (!is.null(baseline_ws_window)) {
    fix_ws_mag_baseline_window(
      csv_path        = ffm_patch_csv,
      scenario_daily  = daily_flows_nfd[[sc]],
      baseline_window = baseline_ws_window,
      out_path        = ffm_wsmag_csv
    )
    final_ffm_csv <- ffm_wsmag_csv
  } else {
    final_ffm_csv <- ffm_patch_csv
  }

  # read the PRE-wsmag-correction version too (Peak_Dur/Fre-corrected only) —
  # this is what FFC actually reports for Wet_BFL_Mag_10/50, kept for
  # side-by-side comparison against the corrected version in the FFM plots
  ffm_precorr_df <- read_csv(ffm_patch_csv, show_col_types = FALSE) %>%
    filter(!Year %in% c("Avg", "Std", "CV")) %>%
    mutate(Year = as.integer(Year))
  ffm_precorr_all <- bind_rows(ffm_precorr_all, ffm_precorr_df %>% mutate(scenario_code = sc, scenario_label = label))

  # read final results back as data frame (either wsmag-corrected or, if the
  # correction hasn't started yet — shouldn't happen post-Baseline — the
  # Peak_Dur/Fre-only patched version)
  ffm_patched_df <- read_csv(final_ffm_csv, show_col_types = FALSE) %>%
    filter(!Year %in% c("Avg", "Std", "CV")) %>%
    mutate(Year = as.integer(Year))

  # ── d. Peak timing ──────────────────────────────────────────────────────────
  # Peak_Tim_2/5/10 are intentionally excluded from FFC CSV output.
  # Compute at same unimpaired thresholds and same NFD period for consistency.
  timing_list[[sc]] <- get_peak_timing(
    scenario_daily = daily_flows_nfd[[sc]],   # NFD period only
    thresholds     = thresholds$p50,
    scenario_name  = sc
  ) %>%
    rename(scenario_code = scenario) %>%
    mutate(scenario_label = label)

  # ── e. Collect ──────────────────────────────────────────────────────────────
  ffm_df_all  <- bind_rows(ffm_df_all,  ffm_patched_df %>% mutate(scenario_code = sc, scenario_label = label))
  alt_df_all  <- bind_rows(alt_df_all,  alt_df         %>% mutate(scenario_code = sc, scenario_label = label))
  pct_df_all  <- bind_rows(pct_df_all,  pct_df         %>% mutate(scenario_code = sc, scenario_label = label))
  meta_df_all <- bind_rows(meta_df_all, meta_df        %>% mutate(scenario_code = sc, scenario_label = label))

  message("  Done: ", label)
}

# ==============================================================================
# PART 4 — Peak timing summary
# ==============================================================================

all_timing <- bind_rows(timing_list)
# Columns: wy, Peak_Tim_2, Peak_Tim_5, Peak_Tim_10, scenario_code, scenario_label
# Values = day-of-water-year (1 = Oct 1). NA = no exceedance that year.

timing_summary <- all_timing %>%
  group_by(scenario_code, scenario_label) %>%
  summarise(
    Peak_Tim_2_median  = median(Peak_Tim_2,  na.rm = TRUE),
    Peak_Tim_5_median  = median(Peak_Tim_5,  na.rm = TRUE),
    Peak_Tim_10_median = median(Peak_Tim_10, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# PART 5 — Export combined results tables
# ==============================================================================

write_csv(ffm_df_all,      file.path(results_dir, "scott_ffm_patched.csv"))
write_csv(ffm_precorr_all, file.path(results_dir, "scott_ffm_precorrection.csv"))   # Wet_BFL_Mag as originally reported by FFC (Peak_Dur/Fre-corrected only)
write_csv(alt_df_all,     file.path(results_dir, "scott_alteration.csv"))
write_csv(pct_df_all,     file.path(results_dir, "scott_percentiles.csv"))
write_csv(meta_df_all,    file.path(results_dir, "scott_metadata.csv"))
write_csv(all_timing,     file.path(results_dir, "scott_peak_timing_annual.csv"))
write_csv(timing_summary, file.path(results_dir, "scott_peak_timing_summary.csv"))

message("\nAll results written to: ", results_dir)
message("\nPeak timing summary:")
print(timing_summary)
