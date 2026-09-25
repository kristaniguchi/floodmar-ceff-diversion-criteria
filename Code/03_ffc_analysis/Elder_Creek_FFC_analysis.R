# ==============================================================================
# Elder Creek — FloodMAR FFC Analysis
# COMID: 12068798
#
# WORKFLOW
#   1. Load Elder_Available.csv — observed discharge + water available to divert
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
#   Baseline  — observed flow, no diversions (reference condition)
#   90/20     — flow > 90th percentile; 20% daily diversion cap
#   FF-BF25   — flow > wet-season 25th pctile baseflow; 20% cap
#   FF-BF50   — flow > wet-season 50th pctile baseflow; 20% cap
#   FF-BF75   — flow > wet-season 75th pctile baseflow; 20% cap
#   FF-BF90   — flow > wet-season 90th pctile baseflow; 20% cap
#   FF-Pk2    — flow > 2-year peak threshold; 20% cap
#
# INPUT
#   Elder_Available.csv (copy to ./Data/inputs/Elder_Creek/ before running)
#   Source: 03_Elder/Data/raw_data/Elder_Available.csv
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
# CONFIGURATION
# ==============================================================================

# ── Inputs ────────────────────────────────────────────────────────────────────
input_dir             <- "./Data/inputs/Elder_Creek"
elder_available_path  <- file.path(input_dir, "Elder_Available.csv")
elder_8050_path       <- file.path(input_dir, "Elder_80_50_wiener_all.csv")

# Daily flow timeseries CSVs written here (two columns: date, flow)
# Upload any of these to the FFC online tool for visual gut checks
ffc_input_dir         <- file.path(input_dir, "ffc_inputs")

# ── Outputs ───────────────────────────────────────────────────────────────────
output_dir            <- "./Data/outputs/Elder_Creek"

# FFC annual results CSVs (output_metrics from API)
ffm_original_dir      <- file.path(output_dir, "FFM/original")  # raw from API
ffm_patched_dir       <- file.path(output_dir, "FFM/patched")   # Peak_Dur/Fre corrected

# Combined results tables (all scenarios bound together)
results_dir           <- file.path(output_dir, "FFM/results")

# ── Site parameters ───────────────────────────────────────────────────────────
comid   <- 12068798
ffc_url <- "https://flowcalculator.codefornature.org/api/calculator"

dir.create(ffc_input_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(ffm_original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(ffm_patched_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir,      showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# PART 1 — Build instream flow timeseries for each scenario
#
# Two source files:
#   Elder_Available.csv          — observed discharge + available-water columns
#                                  for all standard scenarios (subtract to get instream)
#   Elder_80_50_wiener_all.csv   — Streamflow + diversion_cfs for 80/50 rule
#                                  (instream = Streamflow - diversion_cfs, floor at 0)
#
# daily_flows     = full period of record (used for hydrograph display)
# daily_flows_nfd = constrained to NFD period WY1950-2014 (Oct 1 1949 – Sep 30 2014),
#                   matching the Natural Flows Database calibration window.
#                   Used for FFC submission and peak patching.
# ==============================================================================

raw <- read_csv(elder_available_path, show_col_types = FALSE) %>%
  mutate(date = mdy(date))

message("Elder_Available.csv period: ", min(raw$date), " to ", max(raw$date),
        " (", nrow(raw), " days)")

raw_8050 <- read_csv(elder_8050_path, show_col_types = FALSE) %>%
  mutate(date = mdy(Date)) %>%
  select(date, streamflow = Streamflow, diversion_cfs)

message("Elder_80_50_wiener_all.csv period: ", min(raw_8050$date), " to ", max(raw_8050$date),
        " (", nrow(raw_8050), " days)")

# Scenario map: code, display label, and which available-water column to subtract.
# 80_50 is handled separately below (different source file).
scenario_map <- tribble(
  ~scenario_code, ~label,      ~available_col,
  "Baseline",     "Baseline",  NA,               # no diversion
  "90_20",        "90/20",     "90/20",
  "80_50",        "80/50",     NA_character_,    # built from Elder_80_50_wiener_all.csv
  "FF_BF25",      "FF-BF25",   "25_bf_50_20",
  "FF_BF50",      "FF-BF50",   "50_bf_50_20",
  "FF_BF75",      "FF-BF75",   "75_bf_50_20",
  "FF_BF90",      "FF-BF90",   "90_bf_50_20",
  "FF_Pk2",       "FF-Pk2",    "peak_2_20"
)

nfd_start_date <- as.Date("1949-10-01")   # start of WY1950
nfd_end_date   <- as.Date("2014-09-30")   # end of WY2014

daily_flows     <- list()
daily_flows_nfd <- list()

for (i in seq_len(nrow(scenario_map))) {

  sc  <- scenario_map$scenario_code[i]
  col <- scenario_map$available_col[i]

  df <- if (sc == "Baseline") {
    raw %>% transmute(date, flow = discharge_cfs)
  } else if (sc == "80_50") {
    raw_8050 %>% transmute(date, flow = pmax(streamflow - diversion_cfs, 0))
  } else {
    raw %>% transmute(date, flow = pmax(discharge_cfs - .data[[col]], 0))
  }

  daily_flows[[sc]]     <- df
  daily_flows_nfd[[sc]] <- df %>% filter(date >= nfd_start_date, date <= nfd_end_date)

  out_path <- file.path(ffc_input_dir, paste0(sc, "_flow.csv"))
  write_csv(daily_flows_nfd[[sc]], out_path)
  message("Saved daily flow CSV: ", out_path)
}

# ==============================================================================
# PART 2 — Fetch unimpaired peak magnitude thresholds (once per site)
#
# Returns predicted unimpaired Peak_2/5/10 magnitudes at p10-p90 from the
# natural flows database. We use $p50 — the only percentile directly comparable
# to the API's Peak_Dur_*/Peak_Fre_* reference percentiles.
# ==============================================================================

thresholds <- get_peak_thresholds(comid = comid)

# ==============================================================================
# PART 3 — Loop over scenarios
# ==============================================================================

ffm_df_all  <- data.frame()
alt_df_all  <- data.frame()
pct_df_all  <- data.frame()
meta_df_all <- data.frame()
timing_list <- list()

for (sc in scenario_map$scenario_code) {

  label         <- scenario_map$label[scenario_map$scenario_code == sc]
  daily_csv     <- file.path(ffc_input_dir,    paste0(sc, "_flow.csv"))
  ffm_orig_csv  <- file.path(ffm_original_dir, paste0(sc, "_annual_flow_result.csv"))
  ffm_patch_csv <- file.path(ffm_patched_dir,  paste0(sc, "_annual_flow_result_patched.csv"))

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

  if (status_code(response) < 200 || status_code(response) >= 300) {
    warning("FFC API call failed for '", sc, "' (HTTP ", status_code(response), "). Skipping.")
    next
  }

  result <- content(response, as = "parsed")

  if (!is.null(result$warnings) && nchar(result$warnings) > 0)
    message("  API warnings: ", result$warnings)

  # ── b. Parse and save raw FFC outputs ───────────────────────────────────────
  ffm_df  <- read.csv(text = result$output_metrics,     header = TRUE)
  alt_df  <- read.csv(text = result$alteration_results, header = TRUE)
  pct_df  <- read.csv(text = result$percentiles,        header = TRUE)
  meta_df <- read.csv(text = result$metadata_file,      header = TRUE)

  write_csv(ffm_df, ffm_orig_csv)
  message("  Saved original FFM results: ", ffm_orig_csv)

  # ── c. Patch Peak_Dur/Fre in the annual results CSV ─────────────────────────
  fix_and_write_csv(
    csv_path       = ffm_orig_csv,
    scenario_daily = daily_flows_nfd[[sc]],
    thresholds     = thresholds$p50,
    out_path       = ffm_patch_csv
  )
  message("  Saved patched FFM results: ", ffm_patch_csv)

  # read patched results back as data frame
  ffm_patched_df <- read_csv(ffm_patch_csv, show_col_types = FALSE) %>%
    filter(!Year %in% c("Avg", "Std", "CV")) %>%
    mutate(Year = as.integer(Year))

  # ── d. Peak timing ──────────────────────────────────────────────────────────
  timing_list[[sc]] <- get_peak_timing(
    scenario_daily = daily_flows_nfd[[sc]],
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

write_csv(ffm_df_all,     file.path(results_dir, "elder_ffm_patched.csv"))
write_csv(alt_df_all,     file.path(results_dir, "elder_alteration.csv"))
write_csv(pct_df_all,     file.path(results_dir, "elder_percentiles.csv"))
write_csv(meta_df_all,    file.path(results_dir, "elder_metadata.csv"))
write_csv(all_timing,     file.path(results_dir, "elder_peak_timing_annual.csv"))
write_csv(timing_summary, file.path(results_dir, "elder_peak_timing_summary.csv"))

message("\nAll results written to: ", results_dir)
message("\nPeak timing summary:")
print(timing_summary)

