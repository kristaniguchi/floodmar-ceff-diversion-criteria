# ==============================================================================
# Stony Creek — Hydrograph Figures (standalone)
# COMID: 12074576
#
# Produces for each target water year:
#   1. Build series of PNGs — each adds one scenario over observed flow
#      (designed for PowerPoint progressive builds / animations)
#   2. Combined PNG with all scenarios overlaid
#
# Elements on every plot:
#   - Grey line:              observed (Baseline) daily flow
#   - Colored dashed lines:   each diversion scenario
#   - Grey shaded rectangle:  wet season diversion window (Dec 1 – Mar 31)
#   - Horizontal lines:       Peak 2/5/10-year and WS50/WS75 thresholds
#   - Right-side labels:      threshold values in cfs
#
# STANDALONE — does not require Stony_Creek_FFC_analysis.R to be loaded.
# Reads input files directly and fetches thresholds from the API.
#
# NOTE: Uses full period of record (no NFD 1950-2014 filter).
#       Target years should be chosen from whatever years are available in data.
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "scales", "httr", "readr", "readxl"))
#   source("./Code/peak_fix_integration.R")
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(readr)
library(readxl)

# set working directory
#setwd("C:/Users/adrianal/SCCWRP/CEFF - FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
#setwd("C:/Users/kristinet/SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
setwd("/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")

source("./Code/peak_fix_integration.R")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# ── Inputs ────────────────────────────────────────────────────────────────────
input_dir       <- "./Data/inputs/Stony_Creek"
stony_9020_path <- file.path(input_dir, "ffc_inputs", "daily_div_90_20_stony.xlsx")
stony_ffm_path  <- file.path(input_dir, "ffc_inputs", "Stony_Creek_all_scenarios_ESA_Processed_functionalflows.xlsx")

# ── Outputs ───────────────────────────────────────────────────────────────────
hydrograph_dir <- "./Data/outputs/Stony_Creek/hydrographs"
dir.create(hydrograph_dir, showWarnings = FALSE, recursive = TRUE)

# Percentiles output from Stony_Creek_FFC_analysis.R (source of WS50/WS75 thresholds)
results_dir           <- "./Data/outputs/Stony_Creek/FFM/results"
stony_percentiles_csv <- file.path(results_dir, "stony_percentiles.csv")

# ── Site parameters ───────────────────────────────────────────────────────────
comid <- 12074576

# ── Target water years ────────────────────────────────────────────────────────
# Selected based on FFC WYT classifications and diversion contrast analysis.
# wet:      1995 — strongest diversion separation of any year in the record
# moderate: 1997 — best contrast among moderate years
# dry:      2002 — only dry year with meaningful diversion contrast
target_years <- list(
  wet      = 1995,
  moderate = 1997,
  dry      = 2002
)

# ── Scenario build order (progressive reveal for PPT) ─────────────────────────
build_order <- c("90_20", "80_50", "FF_BF50", "FF_BF75", "FF_PEAK2")

# ── Scenario map: code -> display label, color, linetype ──────────────────────
scenario_styles <- tribble(
  ~scenario_code, ~label,          ~color,     ~linetype,
  "90_20",        "90/20",         "#E41A1C",  "dashed",
  "80_50",        "80/50",         "#D4A017",  "dashed",
  "FF_BF50",      "FF-WS50",    "#29B6F6",  "dashed",
  "FF_BF75",      "FF-WS75",    "#CE93D8",  "dashed",
  "FF_PEAK2",     "FF-Pk2",     "#F06292",  "dashed"
)

# ==============================================================================
# PART 1 — Build daily flow timeseries for each scenario
# ==============================================================================

message("Reading Stony Creek input files...")

raw_9020 <- read_excel(stony_9020_path, sheet = "daily_div_90_20_stony_data") %>%
  mutate(date = lubridate::as_date(Date),
         observed_cfs = X_00060_00003,
         available_9020 = divert_update_Delta_plus_HMNC) %>%
  select(date, observed_cfs, available_9020)

raw_8050 <- read_csv(file.path(input_dir, "Stony_80_50_wiener_all.csv"),
                     show_col_types = FALSE) %>%
  mutate(date = lubridate::as_date(Date)) %>%
  select(date, observed_cfs = Streamflow, diversion_cfs_8050 = diversion_cfs)

raw_ffm <- read_excel(stony_ffm_path) %>%
  mutate(date = lubridate::as_date(date))

# Baseline = observed flow, no diversion
daily_flows <- list()
daily_flows[["Baseline"]] <- raw_9020 %>% transmute(date, flow = observed_cfs)

# 90_20: subtract available water from observed
daily_flows[["90_20"]] <- raw_9020 %>%
  transmute(date, flow = pmax(observed_cfs - available_9020, 0))

# 80_50: subtract diversion_cfs from Streamflow
daily_flows[["80_50"]] <- raw_8050 %>%
  transmute(date, flow = pmax(observed_cfs - diversion_cfs_8050, 0))

# FF scenarios: instream flow taken directly from raw_ffm columns (_20 = with 20% diversion cap)
ff_col_map <- c(
  FF_BF50  = "p50_20",
  FF_BF75  = "p75_20",
  FF_PEAK2 = "q2_20"
)

for (sc in names(ff_col_map)) {
  col <- ff_col_map[[sc]]
  daily_flows[[sc]] <- raw_ffm %>%
    transmute(date, flow = as.numeric(.data[[col]])) %>%
    filter(!is.na(flow))
}

message("Daily flow timeseries built for ", length(daily_flows), " scenarios.")

# ── Export consolidated daily flow timeseries, all scenarios in one file ─────
# daily_flows is already a unified in-memory list at this point regardless of
# how many separate source files went into building it. This flattens it to
# one long-format CSV (scenario_code, date, flow) so the full daily record
# doesn't have to be reconstructed from the original per-scenario input files
# every time it's needed elsewhere.
daily_flows_long <- bind_rows(
  lapply(names(daily_flows), function(sc) {
    daily_flows[[sc]] %>% mutate(scenario_code = sc) %>% select(scenario_code, date, flow)
  })
)
write_csv(daily_flows_long, file.path(hydrograph_dir, "stony_daily_flows_all_scenarios.csv"))
message("  Saved: stony_daily_flows_all_scenarios.csv (", nrow(daily_flows_long), " rows, ",
        length(unique(daily_flows_long$scenario_code)), " scenarios)")
message("Period of record: ", min(daily_flows[["Baseline"]]$date),
        " to ", max(daily_flows[["Baseline"]]$date))

# ==============================================================================
# PART 2 — Fetch unimpaired peak magnitude thresholds from natural flows API
# ==============================================================================

message("Fetching peak thresholds from natural flows API (COMID ", comid, ")...")
thresholds <- get_peak_thresholds(comid = comid)
peak_thresh <- thresholds$p50

# ==============================================================================
# PART 2B — Pull WS50/WS75 thresholds from stony_percentiles.csv
#
# Both WS50 and WS75 come from the same metric, Wet_BFL_Mag_50 (wet-season
# baseflow magnitude, 50th-percentile-within-year definition) — WS50 is the
# p50_predicted value across years, WS75 is the p75_predicted value across
# years. Requires Stony_Creek_FFC_analysis.R to have been run already.
# ==============================================================================

if (!file.exists(stony_percentiles_csv)) {
  stop("stony_percentiles.csv not found at: ", stony_percentiles_csv,
       "\nRun Stony_Creek_FFC_analysis.R first to generate it.")
}

pct_raw <- read_csv(stony_percentiles_csv, show_col_types = FALSE)

get_ws_threshold <- function(pct_df, metric_name, pct_col) {
  row <- pct_df %>%
    filter(scenario_code == "Baseline", WYT == "any", metric == metric_name)
  if (nrow(row) == 0 || !pct_col %in% names(row)) {
    message("  WARNING: could not find ", metric_name, " / ", pct_col,
            " (Baseline, WYT=any) in stony_percentiles.csv — line will be omitted.")
    return(NA_real_)
  }
  as.numeric(row[[pct_col]][1])
}

ws50_cfs <- get_ws_threshold(pct_raw, "Wet_BFL_Mag_50", "p50_predicted")
ws75_cfs <- get_ws_threshold(pct_raw, "Wet_BFL_Mag_50", "p75_predicted")

message("  WS50 threshold: ", ifelse(is.na(ws50_cfs), "NOT FOUND", paste0(round(ws50_cfs), " cfs")))
message("  WS75 threshold: ", ifelse(is.na(ws75_cfs), "NOT FOUND", paste0(round(ws75_cfs), " cfs")))

# ==============================================================================
# PART 3 — Build threshold lines data frame
# ==============================================================================

thresh_lines <- bind_rows(
  tibble(
    yval     = as.numeric(peak_thresh["10"]),
    label    = "Peak 10-Year",
    linetype = "dotted"
  ),
  tibble(
    yval     = as.numeric(peak_thresh["5"]),
    label    = "Peak 5-Year",
    linetype = "dotted"
  ),
  tibble(
    yval     = as.numeric(peak_thresh["2"]),
    label    = "Peak 2-Year",
    linetype = "dashed"
  ),
  tibble(
    yval     = ws75_cfs,
    label    = "WS75",
    linetype = "dashed"
  ),
  tibble(
    yval     = ws50_cfs,
    label    = "WS50",
    linetype = "dashed"
  )
)

# ==============================================================================
# PART 4 — Plot theme
# ==============================================================================

theme_hydro <- theme_minimal(base_size = 16) +
  theme(
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(color = "grey90", linewidth = 0.3),
    axis.title         = element_text(size = 15, face = "bold"),
    axis.text          = element_text(size = 13),
    legend.position    = "bottom",
    legend.background  = element_rect(fill = "white", color = NA),
    legend.title       = element_text(face = "bold", size = 13),
    legend.text        = element_text(size = 12),
    plot.margin        = margin(10, 160, 10, 10)
  )

# ==============================================================================
# PART 5 — Core plot builder
# ==============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

build_hydro_plot <- function(wy,
                              scenarios_to_show = character(0),
                              show_thresholds   = TRUE,
                              y_max             = NULL) {

  date_start <- make_date(wy - 1L, 10L,  1L)
  date_end   <- make_date(wy,       9L, 30L)
  ws_start   <- make_date(wy - 1L, 12L,  1L)
  ws_end     <- make_date(wy,       3L, 31L)

  obs <- daily_flows[["Baseline"]] %>%
    filter(date >= date_start, date <= date_end)

  if (nrow(obs) == 0) {
    message("  No Baseline data for WY", wy, " — skipping")
    return(invisible(NULL))
  }

  scen_long <- if (length(scenarios_to_show) == 0) {
    tibble(date = as.Date(character()), flow = numeric(),
           scenario_code = character(), label = character(),
           color = character(), linetype = character())
  } else {
    map_dfr(scenarios_to_show, function(sc) {
      daily_flows[[sc]] %>%
        filter(date >= date_start, date <= date_end) %>%
        mutate(scenario_code = sc)
    }) %>%
      left_join(scenario_styles %>% select(scenario_code, label, color, linetype),
                by = "scenario_code")
  }

  y_upper  <- y_max %||% (max(obs$flow, na.rm = TRUE) * 1.08)
  label_x  <- date_end + days(2)

  color_vals    <- c("Observed Flow" = "grey40",
                     setNames(scenario_styles$color,    scenario_styles$label))
  linetype_vals <- c("Observed Flow" = "solid",
                     setNames(scenario_styles$linetype, scenario_styles$label))
  legend_breaks <- c("Observed Flow",
                     scenario_styles$label[scenario_styles$scenario_code %in% scenarios_to_show])

  p <- ggplot() +

    # wet season shading
    annotate("rect",
             xmin = ws_start, xmax = ws_end,
             ymin = -Inf,     ymax = Inf,
             fill = "grey85", alpha = 0.6) +

    # observed flow
    geom_line(data = obs,
              aes(x = date, y = flow,
                  color    = "Observed Flow",
                  linetype = "Observed Flow"),
              linewidth = 2.5) +

    # scenario lines
    { if (nrow(scen_long) > 0)
        geom_line(data = scen_long,
                  aes(x = date, y = flow,
                      color    = label,
                      linetype = label),
                  linewidth = 0.8)
    } +

    scale_color_manual(name   = "Scenarios",
                       values = color_vals,
                       breaks = legend_breaks) +
    scale_linetype_manual(name   = "Scenarios",
                          values = linetype_vals,
                          breaks = legend_breaks) +

    scale_x_date(date_labels = "%b",
                 date_breaks = "2 months",
                 limits      = c(date_start, date_end + days(25)),
                 expand      = expansion(0)) +
    scale_y_continuous(labels = comma,
                       limits = c(0, y_upper),
                       expand = expansion(mult = c(0, 0.02))) +

    labs(x = NULL, y = "Discharge (cfs)",
         title = paste0("Stony Creek — WY", wy)) +
    coord_cartesian(clip = "off") +
    theme_hydro

  # Add threshold lines using fixed aesthetics
  if (show_thresholds) {
    for (i in seq_len(nrow(thresh_lines))) {
      vjust_val <- -0.3   # default above line
      # stagger WS50 label below its line to avoid overlap with WS75
      if (thresh_lines$label[i] == "WS50")
        vjust_val <- 1.3

      p <- p +
        geom_hline(yintercept = thresh_lines$yval[i],
                   linetype   = thresh_lines$linetype[i],
                   color      = "black",
                   linewidth  = 0.5) +
        annotate("text",
                 x      = label_x,
                 y      = thresh_lines$yval[i],
                 label  = thresh_lines$label[i],
                 hjust  = 0,
                 vjust  = vjust_val,
                 size   = 7,
                 color  = "black")
    }
  }

  p
}

# ==============================================================================
# PART 6 — Generate figures for each target year
# ==============================================================================

# Filter to years with actual data
target_years_valid <- Filter(function(wy) {
  !is.null(wy) && any(daily_flows[["Baseline"]]$date >= make_date(wy - 1L, 10L, 1L) &
                      daily_flows[["Baseline"]]$date <= make_date(wy, 9L, 30L))
}, target_years)

if (length(target_years_valid) == 0) {
  message("\nNo target years defined yet — set target_years at top of script and re-run.")
  message("Available years in data:")
  yr_range <- range(lubridate::year(daily_flows[["Baseline"]]$date))
  message("  Approximate range: ", yr_range[1], " to ", yr_range[2])
} else {

  for (yr_name in names(target_years_valid)) {

    wy <- target_years_valid[[yr_name]]
    message("\n── WY ", wy, " (", yr_name, ") ──────────────────────────────")

    obs_year <- daily_flows[["Baseline"]] %>%
      filter(date >= make_date(wy - 1L, 10L, 1L),
             date <= make_date(wy,       9L, 30L))
    y_max <- max(obs_year$flow, na.rm = TRUE) * 1.08

    yr_dir <- file.path(hydrograph_dir, paste0("WY", wy, "_", yr_name))
    dir.create(yr_dir, showWarnings = FALSE, recursive = TRUE)

    plot_specs <- list(
      list(suffix = "1_observed_only",   scenarios = character(0),  thresholds = FALSE),
      list(suffix = "2_observed_thresh", scenarios = character(0),  thresholds = TRUE),
      list(suffix = "3_90_20",           scenarios = "90_20",       thresholds = TRUE),
      list(suffix = "4_80_50",           scenarios = "80_50",       thresholds = TRUE),
      list(suffix = "5_FF_BF50",         scenarios = "FF_BF50",     thresholds = TRUE),
      list(suffix = "6_FF_BF75",         scenarios = "FF_BF75",     thresholds = TRUE),
      list(suffix = "7_FF_PEAK2",        scenarios = "FF_PEAK2",    thresholds = TRUE),
      list(suffix = "8_all_scenarios",   scenarios = build_order,   thresholds = TRUE)
    )

    for (spec in plot_specs) {
      p <- build_hydro_plot(wy,
                            scenarios_to_show = spec$scenarios,
                            show_thresholds   = spec$thresholds,
                            y_max             = y_max)
      if (is.null(p)) next
      fname <- file.path(yr_dir, paste0("Hydro", wy, "_", spec$suffix, ".png"))
      ggsave(fname, p, width = 14, height = 6, dpi = 300)
      message("  Saved: ", spec$suffix)
    }
  }
}

# ==============================================================================
# PART 7 — Additional comparison figures
#
# Scenario-comparison facet panels (2×2 layout) — comparable to Scott River
# Part 7B. Replace year values once target_years are finalized.
# ==============================================================================

get_y_max <- function(wy) {
  obs <- daily_flows[["Baseline"]] %>%
    filter(date >= make_date(wy - 1L, 10L, 1L),
           date <= make_date(wy,       9L, 30L))
  if (nrow(obs) == 0) return(NA_real_)
  max(obs$flow, na.rm = TRUE) * 1.08
}

library(patchwork)

# Computes total diverted volume (TAF) for a scenario over a water year,
# directly from the same daily_flows object build_hydro_plot plots from --
# diverted = Baseline flow minus scenario flow, summed over the water year
# and converted from cfs-days to acre-feet (1 cfs-day = 1.983459 acre-ft).
compute_taf <- function(wy, scenario_code) {
  date_start <- make_date(wy - 1L, 10L, 1L)
  date_end   <- make_date(wy,       9L, 30L)

  obs <- daily_flows[["Baseline"]] %>% filter(date >= date_start, date <= date_end)
  scn <- daily_flows[[scenario_code]] %>% filter(date >= date_start, date <= date_end)

  merged <- obs %>%
    rename(obs_flow = flow) %>%
    inner_join(scn %>% rename(scn_flow = flow), by = "date")

  diverted_cfs_days <- sum(pmax(merged$obs_flow - merged$scn_flow, 0), na.rm = TRUE)
  round(diverted_cfs_days * 1.983459 / 1000, 1)
}

build_panel <- function(wy, scenario_code, panel_label, y_max, show_thresh = TRUE) {
  p <- build_hydro_plot(wy,
                   scenarios_to_show = scenario_code,
                   show_thresholds   = show_thresh,
                   y_max             = y_max)
  if (is.null(p)) return(invisible(NULL))

  taf_val     <- compute_taf(wy, scenario_code)
  badge_color <- scenario_styles$color[scenario_styles$scenario_code == scenario_code]
  badge_x     <- make_date(wy, 9L, 30L)

  p +
    labs(title = panel_label) +
    annotate("label", x = badge_x, y = y_max * 0.95,
             label = paste0(taf_val, " TAF"),
             hjust = 1, vjust = 1, size = 7, fontface = "bold",
             fill = badge_color, color = "white", label.size = 0) +
    theme(legend.position = "none",
          plot.title      = element_text(size = 24, face = "bold"),
          axis.text       = element_text(size = 20),    # bumped for legibility when pasted into Word
          axis.title      = element_text(size = 24, face = "bold"))  # bumped for legibility when pasted into Word
}

comparison_dir <- file.path(hydrograph_dir, "comparisons")
dir.create(comparison_dir, showWarnings = FALSE, recursive = TRUE)

# Wet year panel
if (!is.null(target_years$wet) && !is.na(get_y_max(target_years$wet))) {
  wy_wet <- target_years$wet
  y_wet  <- get_y_max(wy_wet)

  pA <- build_panel(wy_wet, "90_20",    "A. 90/20",   y_wet)
  pB <- build_panel(wy_wet, "80_50",    "C. 80/50",   y_wet)
  pC <- build_panel(wy_wet, "FF_BF75",  "B. FF-WS75", y_wet)
  pD <- build_panel(wy_wet, "FF_PEAK2", "D. FF-Pk2",  y_wet)

  if (!any(sapply(list(pA, pB, pC, pD), is.null))) {
    combined_wet <- (pA + pC) / (pB + pD) +
      plot_annotation(
        title    = paste0("Stony Creek WY", wy_wet, ": Wet Year Scenario Comparison"),
        theme = theme(plot.title    = element_text(size = 30, face = "bold", hjust = 0.5),
                      plot.background = element_rect(fill = "white", color = NA))
      )
    ggsave(file.path(comparison_dir, paste0("WY", wy_wet, "_scenario_comparison_panels.png")),
           combined_wet, width = 22, height = 12, dpi = 300)
    message("  Saved: WY", wy_wet, "_scenario_comparison_panels.png")
  }
}

# Dry year panel
if (!is.null(target_years$dry) && !is.na(get_y_max(target_years$dry))) {
  wy_dry <- target_years$dry
  y_dry  <- get_y_max(wy_dry)

  pA14 <- build_panel(wy_dry, "90_20",    "A. 90/20",   y_dry)
  pB14 <- build_panel(wy_dry, "80_50",    "C. 80/50",   y_dry)
  pC14 <- build_panel(wy_dry, "FF_BF75",  "B. FF-WS75", y_dry)
  pD14 <- build_panel(wy_dry, "FF_PEAK2", "D. FF-Pk2",  y_dry)

  if (!any(sapply(list(pA14, pB14, pC14, pD14), is.null))) {
    combined_dry <- (pA14 + pC14) / (pB14 + pD14) +
      plot_annotation(
        title    = paste0("Stony Creek WY", wy_dry, ": Dry Year Scenario Comparison"),
        theme = theme(plot.title    = element_text(size = 30, face = "bold", hjust = 0.5),
                      plot.background = element_rect(fill = "white", color = NA))
      )
    ggsave(file.path(comparison_dir, paste0("WY", wy_dry, "_scenario_comparison_panels.png")),
           combined_dry, width = 22, height = 12, dpi = 300)
    message("  Saved: WY", wy_dry, "_scenario_comparison_panels.png")
  }
}

message("\nDone. All figures written to: ", normalizePath(hydrograph_dir))

