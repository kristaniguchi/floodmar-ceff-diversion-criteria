# ==============================================================================
# Stony Creek — FloodMAR FFC Analysis
# COMID: 12074576
#
# WORKFLOW
#   1. Load Stony_Available.csv — observed discharge + water available to divert
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
# SCENARIOS (Stony Creek)
#   Baseline     — observed flow, no diversions (reference condition)
#   90_20        — 90/20 rule: divert when flow > 90th pctile, 20% daily cap
#   80_50        — 80/50 rule: divert when flow > 80th pctile, 50% daily cap
#   FF_BF25      — functional flows: wet-season baseflow 25th percentile bypass, 20% cap
#   FF_BF50      — functional flows: wet-season baseflow 50th percentile bypass, 20% cap
#   FF_BF75      — functional flows: wet-season baseflow 75th percentile bypass, 20% cap
#   FF_PEAK2     — functional flows: 2-year peak preservation, 20% cap
#
# NOTE ON DATA FORMAT
#   Stony Creek uses three source files:
#     1. daily_div_90_20_stony.xlsx       — observed flow + 90/20 available water
#     2. Stony_80_50_wiener_all.csv       — observed flow + 80/50 diversion_cfs
#     3. Stony_Creek_all_scenarios_ESA_Processed_functionalflows.xlsx — FF scenarios
#   Update input section below when new data becomes available.
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "httr", "readr", "readxl"))
#   peak_fix_integration.R in ./Code/
# ==============================================================================

library(tidyverse)
library(lubridate)
library(httr)
library(readr)
library(readxl)

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
input_dir              <- "./Data/inputs/Stony_Creek"
stony_9020_path        <- file.path(input_dir, "ffc_inputs", "daily_div_90_20_stony.xlsx")
stony_8050_path        <- file.path(input_dir, "Stony_80_50_wiener_all.csv")
stony_ffm_path         <- file.path(input_dir, "ffc_inputs", "Stony_Creek_all_scenarios_ESA_Processed_functionalflows.xlsx")

# Daily flow timeseries CSVs written here (two columns: date, flow)
ffc_input_dir          <- file.path(input_dir, "ffc_inputs")

# ── Outputs ───────────────────────────────────────────────────────────────────
output_dir             <- "./Data/outputs/Stony_Creek"

# FFC annual results CSVs (output_metrics from API)
ffm_original_dir       <- file.path(output_dir, "FFM/original")
ffm_patched_dir        <- file.path(output_dir, "FFM/patched")

# Combined results tables (all scenarios bound together)
results_dir            <- file.path(output_dir, "FFM/results")

# ── Site parameters ───────────────────────────────────────────────────────────
comid   <- 12074576
ffc_url <- "https://flowcalculator.codefornature.org/api/calculator"

dir.create(ffc_input_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(ffm_original_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(ffm_patched_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(results_dir,      showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# PART 1 — Build instream flow timeseries for each scenario
#
# Stony Creek uses three source files:
#   1. daily_div_90_20_stony.xlsx:
#      - Sheet: 'daily_div_90_20_stony_data'
#      - Columns: Date, WY, X_00060_00003 (observed flow), divert_update_Delta_plus_HMNC
#
#   2. Stony_80_50_wiener_all.csv:
#      - Columns: Date, Streamflow (observed flow), diversion_cfs
#      - instream = Streamflow - diversion_cfs (floored at 0)
#
#   3. Stony_Creek_all_scenarios_ESA_Processed_functionalflows.xlsx:
#      - Columns: date, water_year, p25_20, p50_20, p75_20, q2_20, ...
#        (instream flow under each FF/20% cap scenario — already computed)
#
# NOTE: For FF scenarios the xlsx already contains instream flow values.
# For 90_20 and 80_50: instream = observed - diversion (floored at 0).
# Baseline = observed flow with no subtraction.
# ==============================================================================

message("Reading Stony Creek input files...")

# -- Source file 1: 90/20 and Baseline --
raw_9020 <- read_excel(stony_9020_path, sheet = "daily_div_90_20_stony_data") %>%
  mutate(date = lubridate::as_date(Date),
         water_year = WY,
         observed_cfs = X_00060_00003,
         available_9020 = divert_update_Delta_plus_HMNC) %>%
  select(date, water_year, observed_cfs, available_9020)

# -- Source file 2: 80/50 --
raw_8050 <- read_csv(stony_8050_path, show_col_types = FALSE) %>%
  mutate(date = lubridate::as_date(Date)) %>%
  select(date, observed_cfs = Streamflow, diversion_cfs_8050 = diversion_cfs)

# -- Source file 3: FF scenarios (instream flow already computed) --
raw_ffm <- read_excel(stony_ffm_path) %>%
  mutate(date = lubridate::as_date(date))

message("Stony Creek period of record: ", min(raw_9020$date), " to ", max(raw_9020$date))
message("80/50 period: ", min(raw_8050$date), " to ", max(raw_8050$date))
message("FF scenario period: ", min(raw_ffm$date), " to ", max(raw_ffm$date))

# ==============================================================================
# Scenario map
# Three scenario "types" handled differently:
#   type = "baseline"      -> observed flow, no subtraction
#   type = "subtract"      -> instream = observed - available_col (floored at 0),
#                             from raw_9020
#   type = "csv_subtract"  -> instream = Streamflow - diversion_cfs (floored at 0),
#                             from a standalone CSV (used for 80_50)
#   type = "direct"        -> instream taken directly from raw_ffm column
# ==============================================================================

scenario_map <- tribble(
  ~scenario_code, ~label,      ~type,            ~source_col,
  "Baseline",     "Baseline",  "baseline",        NA,
  "90_20",        "90/20",     "subtract",        "available_9020",
  "80_50",        "80/50",     "csv_subtract",    NA,
  "FF_BF50",      "FF-BF50",   "direct",          "p50_20",
  "FF_BF75",      "FF-BF75",   "direct",          "p75_20",
  "FF_PEAK2",     "FF-Pk2",    "direct",          "q2_20"
)

# Build daily flow list for all scenarios
# Full period of record — no NFD date filter applied (unlike Scott River)
daily_flows <- list()

for (i in seq_len(nrow(scenario_map))) {
  sc   <- scenario_map$scenario_code[i]
  type <- scenario_map$type[i]
  col  <- scenario_map$source_col[i]

  if (type == "baseline") {
    df <- raw_9020 %>% transmute(date, flow = observed_cfs)

  } else if (type == "subtract") {
    df <- raw_9020 %>%
      transmute(date, flow = pmax(observed_cfs - .data[[col]], 0))

  } else if (type == "csv_subtract") {
    # 80/50: subtract diversion_cfs from Streamflow (floored at 0)
    df <- raw_8050 %>%
      transmute(date, flow = pmax(observed_cfs - diversion_cfs_8050, 0))

  } else if (type == "direct") {
    # FF scenarios: instream flow is in raw_ffm, join on date
    df <- raw_ffm %>%
      transmute(date, flow = as.numeric(.data[[col]])) %>%
      filter(!is.na(flow))
  }

  daily_flows[[sc]] <- df

  # Write CSV for FFC submission
  out_path <- file.path(ffc_input_dir, paste0(sc, "_flow.csv"))
  write_csv(df, out_path)
  message("Saved daily flow CSV: ", out_path,
          " (", nrow(df), " days, ", min(df$date), " to ", max(df$date), ")")
}

# ==============================================================================
# PART 2 — Fetch unimpaired peak magnitude thresholds (once per site)
# ==============================================================================

message("Fetching peak thresholds from natural flows API (COMID ", comid, ")...")
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

  if (!(status_code(response) %in% c(200, 299))) {
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

  # ── c. Patch Peak_Dur/Fre ───────────────────────────────────────────────────
  fix_and_write_csv(
    csv_path       = ffm_orig_csv,
    scenario_daily = daily_flows[[sc]],
    thresholds     = thresholds$p50,
    out_path       = ffm_patch_csv
  )
  message("  Saved patched FFM results: ", ffm_patch_csv)

  ffm_patched_df <- read_csv(ffm_patch_csv, show_col_types = FALSE) %>%
    filter(!Year %in% c("Avg", "Std", "CV")) %>%
    mutate(Year = as.integer(Year))

  # ── d. Peak timing ──────────────────────────────────────────────────────────
  timing_list[[sc]] <- get_peak_timing(
    scenario_daily = daily_flows[[sc]],
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

write_csv(ffm_df_all,     file.path(results_dir, "stony_ffm_patched.csv"))
write_csv(alt_df_all,     file.path(results_dir, "stony_alteration.csv"))
write_csv(pct_df_all,     file.path(results_dir, "stony_percentiles.csv"))
write_csv(meta_df_all,    file.path(results_dir, "stony_metadata.csv"))
write_csv(all_timing,     file.path(results_dir, "stony_peak_timing_annual.csv"))
write_csv(timing_summary, file.path(results_dir, "stony_peak_timing_summary.csv"))

message("\nAll results written to: ", results_dir)
message("\nPeak timing summary:")
print(timing_summary)

