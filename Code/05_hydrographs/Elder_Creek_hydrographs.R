# ==============================================================================
# Elder Creek — Hydrograph Figures (standalone)
# COMID: 12068798
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
# STANDALONE — does not require Elder_Creek_FFC_analysis.R to be loaded.
# Reads Elder_Available.csv directly and fetches thresholds from the API.
#
# BEFORE RUNNING:
#   1. WS50/WS75 thresholds are pulled automatically from elder_percentiles.csv
#      (Elder_Creek_FFC_analysis.R must be run first so that file exists).
#   2. Update target_years with representative wet/moderate/dry years for
#      Elder Creek (inspect the elder_ffm_patched.csv WYT column).
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "scales", "httr", "readr", "patchwork"))
#   source("./Code/peak_fix_integration.R")
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(readr)

# set working directory
#setwd("C:/Users/adrianal/SCCWRP/CEFF - FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
#setwd("C:/Users/kristinet/SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")
setwd("/Users/kristq/Library/CloudStorage/OneDrive-SharedLibraries-OneDrive-SharedLibraries-SCCWRP/CEFF - General/CEFF_GW/FloodMAR_CEFF/FloodMAR_InstreamFlow_Action_Team/05_updated_directory_all")

source("./Code/peak_fix_integration.R")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

# ── Inputs ────────────────────────────────────────────────────────────────────
input_dir             <- "./Data/inputs/Elder_Creek"
elder_available_path  <- file.path(input_dir, "Elder_Available.csv")
elder_8050_path       <- file.path(input_dir, "Elder_80_50_wiener_all.csv")

# ── Outputs ───────────────────────────────────────────────────────────────────
hydrograph_dir <- "./Data/outputs/Elder_Creek/hydrographs"
dir.create(hydrograph_dir, showWarnings = FALSE, recursive = TRUE)

# Percentiles output from Elder_Creek_FFC_analysis.R (source of WS50/WS75 thresholds)
results_dir           <- "./Data/outputs/Elder_Creek/FFM/results"
elder_percentiles_csv <- file.path(results_dir, "elder_percentiles.csv")

# ── Site parameters ───────────────────────────────────────────────────────────
comid <- 12068798

# ── Target water years ────────────────────────────────────────────────────────
# wet/moderate/dry selections below are based on the flow-based WYT
# classification computed in Part 1C (from Elder's own Baseline record), not
# the externally supplied WYT tag in elder_ffm_patched.csv -- the two do not
# always agree (see Part 1C header comment). All target years fall within the
# NFD period of record, WY1950-2014.
#
target_years <- list(
  wet      = 1983,   # confirmed: best scenario-contrast wet year for Elder (spread 20.6 TAF)
  dry      = 1962    # confirmed genuinely Dry by flow-based WYT (1960 was mistakenly
                     # tagged Dry by FFC but is actually Moderate by annual volume;
                     # 2014 is genuinely Dry but shows ~0 TAF for all scenarios).
                     # 1962 has the best scenario contrast among years that are
                     # actually Dry: 90/20=1.9, FF-BF50=3.8, FF-BF75=3.3, FF-Pk2=0.6,
                     # 80/50=0.0 TAF. Alternates if 1962 has data issues: 1961 (3.5
                     # TAF spread), 1957 (3.1 TAF spread).
)

# ── Scenario build order (progressive reveal for PPT) ─────────────────────────
build_order <- c("90_20", "80_50", "FF_BF50", "FF_BF75", "FF_Pk2")

# ── Scenario map: code -> display label, color, linetype ──────────────────────
# available_col = NA for 80_50 — built from Elder_80_50_wiener_all.csv, not Elder_Available.csv
scenario_styles <- tribble(
  ~scenario_code, ~label,      ~color,     ~linetype,  ~available_col,
  "90_20",        "90/20",     "#E41A1C",  "dashed",   "90/20",
  "80_50",        "80/50",     "#D4A017",  "dashed",   NA_character_,
  "FF_BF50",      "FF-WS50",   "#00E676",  "dashed",   "50_bf_50_20",
  "FF_BF75",      "FF-WS75",   "#CE93D8",  "dashed",   "75_bf_50_20",
  "FF_Pk2",       "FF-Pk2",    "#F06292",  "dashed",   "peak_2_20"
)

# ── Wet-season baseflow thresholds (cfs) ──────────────────────────────────────
# Pulled automatically from elder_percentiles.csv (see PART 2B below).

# ==============================================================================
# PART 1 — Build daily flow timeseries for each scenario
# ==============================================================================

message("Reading Elder_Available.csv...")
raw <- read_csv(elder_available_path, show_col_types = FALSE) %>%
  mutate(date = mdy(date))

raw_8050 <- read_csv(elder_8050_path, show_col_types = FALSE) %>%
  mutate(date = mdy(Date)) %>%
  select(date, streamflow = Streamflow, diversion_cfs)

# Baseline = observed flow, no diversion
daily_flows <- list()
daily_flows[["Baseline"]] <- raw %>% transmute(date, flow = discharge_cfs)

# Diversion scenarios
for (i in seq_len(nrow(scenario_styles))) {
  sc  <- scenario_styles$scenario_code[i]
  col <- scenario_styles$available_col[i]
  daily_flows[[sc]] <- if (sc == "80_50") {
    raw_8050 %>% transmute(date, flow = pmax(streamflow - diversion_cfs, 0))
  } else {
    raw %>% transmute(date, flow = pmax(discharge_cfs - .data[[col]], 0))
  }
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
write_csv(daily_flows_long, file.path(hydrograph_dir, "elder_daily_flows_all_scenarios.csv"))
message("  Saved: elder_daily_flows_all_scenarios.csv (", nrow(daily_flows_long), " rows, ",
        length(unique(daily_flows_long$scenario_code)), " scenarios)")

# ==============================================================================
# PART 1C — Flow-based water year type (WYT) classification
#
# Classifies each water year as Wet/Moderate/Dry directly from Elder's own
# observed (Baseline) daily flow record, rather than relying on an externally
# supplied WYT tag. Method: total water-year volume (sum of daily mean cfs,
# Oct 1 - Sep 30), ranked into terciles across the NFD period of record
# (WY1950-2014).
#
# This matters because the FFC-derived WYT tag and this flow-based
# classification do not always agree -- e.g. WY1960 is tagged Dry by FFC but
# classifies as Moderate by actual annual volume, and WY1984 (the wet-to-dry
# example) classifies as Moderate overall despite starting wet, since the
# late-season collapse pulls the annual total down. Use this table, not the
# FFC tag, when selecting representative wet/moderate/dry years for Elder.
# ==============================================================================

message("\n── PART 1C: Flow-based WYT classification ──────────────────────────────")

wy_volumes <- daily_flows[["Baseline"]] %>%
  mutate(WY = year(date) + as.integer(month(date) >= 10)) %>%
  filter(WY >= 1950, WY <= 2014) %>%
  group_by(WY) %>%
  summarise(annual_volume = sum(flow, na.rm = TRUE), .groups = "drop")

wyt_breaks <- quantile(wy_volumes$annual_volume, probs = c(1/3, 2/3))

wy_volumes <- wy_volumes %>%
  mutate(WYT_flow_based = case_when(
    annual_volume <= wyt_breaks[1] ~ "Dry",
    annual_volume >= wyt_breaks[2] ~ "Wet",
    TRUE                           ~ "Moderate"
  ))

write_csv(wy_volumes, file.path(hydrograph_dir, "elder_wyt_flow_based.csv"))
message("  Saved: elder_wyt_flow_based.csv (", nrow(wy_volumes), " water years classified)")
message("  Tercile breakpoints (annual volume, sum of daily mean cfs): ",
        round(wyt_breaks[1]), " / ", round(wyt_breaks[2]))

# ==============================================================================
# PART 2 — Fetch unimpaired peak magnitude thresholds from natural flows API
# ==============================================================================

message("Fetching peak thresholds from natural flows API (COMID ", comid, ")...")
thresholds  <- get_peak_thresholds(comid = comid)
peak_thresh <- thresholds$p50

# ==============================================================================
# PART 2B — Pull WS50/WS75 thresholds from elder_percentiles.csv
#
# Both WS50 and WS75 come from the same metric, Wet_BFL_Mag_50 (wet-season
# baseflow magnitude, 50th-percentile-within-year definition) — WS50 is the
# p50_predicted value across years, WS75 is the p75_predicted value across
# years. Requires Elder_Creek_FFC_analysis.R to have been run already.
# ==============================================================================

if (!file.exists(elder_percentiles_csv)) {
  stop("elder_percentiles.csv not found at: ", elder_percentiles_csv,
       "\nRun Elder_Creek_FFC_analysis.R first to generate it.")
}

pct_raw <- read_csv(elder_percentiles_csv, show_col_types = FALSE)

get_ws_threshold <- function(pct_df, metric_name, pct_col) {
  row <- pct_df %>%
    filter(scenario_code == "Baseline", WYT == "any", metric == metric_name)
  if (nrow(row) == 0 || !pct_col %in% names(row)) {
    message("  WARNING: could not find ", metric_name, " / ", pct_col,
            " (Baseline, WYT=any) in elder_percentiles.csv — line will be omitted.")
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

thresh_lines_base <- bind_rows(
  tibble(yval = as.numeric(peak_thresh["10"]),
         label    = "Peak 10-Year",
         linetype = "dotted"),
  tibble(yval = as.numeric(peak_thresh["5"]),
         label    = "Peak 5-Year",
         linetype = "dotted"),
  tibble(yval = as.numeric(peak_thresh["2"]),
         label    = "Peak 2-Year",
         linetype = "dashed")
)

# Add WS thresholds only if values have been filled in
ws_thresh_lines <- if (!is.na(ws50_cfs) && !is.na(ws75_cfs)) {
  bind_rows(
    tibble(yval = ws75_cfs, label = "WS75", linetype = "dashed"),
    tibble(yval = ws50_cfs, label = "WS50", linetype = "dashed")
  )
} else {
  message("NOTE: ws50_cfs and ws75_cfs are not set — WS threshold lines will not be shown.")
  message("      Update these values in the CONFIGURATION section after running FFC_analysis.R.")
  tibble(yval = numeric(0), label = character(0), linetype = character(0))
}

thresh_lines <- bind_rows(thresh_lines_base, ws_thresh_lines)

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

    annotate("rect",
             xmin = ws_start, xmax = ws_end,
             ymin = -Inf,     ymax = Inf,
             fill = "grey85", alpha = 0.6) +

    geom_line(data = obs,
              aes(x = date, y = flow,
                  color    = "Observed Flow",
                  linetype = "Observed Flow"),
              linewidth = 2.5) +

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

    labs(x = NULL, y = "Discharge (cfs)") +
    coord_cartesian(clip = "off") +
    theme_hydro

  if (show_thresholds && nrow(thresh_lines) > 0) {
    for (i in seq_len(nrow(thresh_lines))) {

      vjust_val <- if (!is.na(ws50_cfs) &&
                       thresh_lines$label[i] == "WS50") 1.3
                   else -0.3

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

for (yr_name in names(target_years)) {

  wy <- target_years[[yr_name]]
  message("\n── WY ", wy, " (", yr_name, ") ──────────────────────────────")

  obs_year <- daily_flows[["Baseline"]] %>%
    filter(date >= make_date(wy - 1L, 10L, 1L),
           date <= make_date(wy,       9L, 30L))

  if (nrow(obs_year) == 0) {
    message("  Skipping WY", wy, " — no data in Elder_Available.csv for this year")
    next
  }

  y_max <- max(obs_year$flow, na.rm = TRUE) * 1.08

  yr_dir <- file.path(hydrograph_dir, paste0("WY", wy, "_", yr_name))
  dir.create(yr_dir, showWarnings = FALSE, recursive = TRUE)

  plot_specs <- list(
    list(suffix = "1_observed_only",    scenarios = character(0),   thresholds = FALSE),
    list(suffix = "2_observed_thresh",  scenarios = character(0),   thresholds = TRUE),
    list(suffix = "3_90_20",            scenarios = "90_20",        thresholds = TRUE),
    list(suffix = "4_80_50",            scenarios = "80_50",        thresholds = TRUE),
    list(suffix = "5_FF_BF50",          scenarios = "FF_BF50",      thresholds = TRUE),
    list(suffix = "6_FF_BF75",          scenarios = "FF_BF75",      thresholds = TRUE),
    list(suffix = "7_FF_Pk2",           scenarios = "FF_Pk2",       thresholds = TRUE),
    list(suffix = "8_all_scenarios",    scenarios = build_order,    thresholds = TRUE)
  )

  for (spec in plot_specs) {
    p <- build_hydro_plot(wy,
                          scenarios_to_show = spec$scenarios,
                          show_thresholds   = spec$thresholds,
                          y_max             = y_max)
    fname <- file.path(yr_dir, paste0("Hydro", wy, "_", spec$suffix, ".png"))
    ggsave(fname, p, width = 14, height = 6, dpi = 300)
    message("  Saved: ", spec$suffix)
  }
}

message("\nDone. All figures written to: ", normalizePath(hydrograph_dir))

# ==============================================================================
# PART 7 — Additional comparison figures
#
# Scenario-comparison facet panels for the representative wet year and dry
# year (see target_years above), each showing 90/20, 80/50, FF-WS75, and
# FF-Pk2 over the full water year.
# ==============================================================================

library(patchwork)

get_y_max <- function(wy) {
  obs <- daily_flows[["Baseline"]] %>%
    filter(date >= make_date(wy - 1L, 10L, 1L),
           date <= make_date(wy,       9L, 30L))
  max(obs$flow, na.rm = TRUE) * 1.08
}

comparison_dir <- file.path(hydrograph_dir, "comparisons")
dir.create(comparison_dir, showWarnings = FALSE, recursive = TRUE)

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
  taf_val     <- compute_taf(wy, scenario_code)
  badge_color <- scenario_styles$color[scenario_styles$scenario_code == scenario_code]
  badge_x     <- make_date(wy, 9L, 30L)

  build_hydro_plot(wy,
                   scenarios_to_show = scenario_code,
                   show_thresholds   = show_thresh,
                   y_max             = y_max) +
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

# ── Wet year 2x2 panel ────────────────────────────────────────────────────────
wy_wet    <- target_years[["wet"]]
y_wet_max <- get_y_max(wy_wet)

pA <- build_panel(wy_wet, "90_20",   "A. 90/20",   y_wet_max)
pB <- build_panel(wy_wet, "80_50",   "C. 80/50",   y_wet_max)
pC <- build_panel(wy_wet, "FF_BF75", "B. FF-WS75", y_wet_max)
pD <- build_panel(wy_wet, "FF_Pk2",  "D. FF-Pk2",  y_wet_max)

combined_wet <- (pA + pC) / (pB + pD) +
  plot_annotation(
    title    = paste0("Elder Creek WY", wy_wet, ": Wet Year Scenario Comparison"),
    theme = theme(plot.title    = element_text(size = 30, face = "bold", hjust = 0.5),
                  plot.background = element_rect(fill = "white", color = NA))
  )

ggsave(file.path(comparison_dir, paste0("WY", wy_wet, "_scenario_comparison_panels.png")),
       combined_wet, width = 22, height = 12, dpi = 300)
message("Saved: WY", wy_wet, "_scenario_comparison_panels.png")

# ── Dry year 2x2 panel ────────────────────────────────────────────────────────
wy_dry    <- target_years[["dry"]]
y_dry_max <- get_y_max(wy_dry)

pA14 <- build_panel(wy_dry, "90_20",   "A. 90/20",   y_dry_max)
pB14 <- build_panel(wy_dry, "80_50",   "C. 80/50",   y_dry_max)
pC14 <- build_panel(wy_dry, "FF_BF75", "B. FF-WS75", y_dry_max)
pD14 <- build_panel(wy_dry, "FF_Pk2",  "D. FF-Pk2",  y_dry_max)

combined_dry <- (pA14 + pC14) / (pB14 + pD14) +
  plot_annotation(
    title    = paste0("Elder Creek WY", wy_dry, ": Dry Year Scenario Comparison"),
    theme = theme(plot.title    = element_text(size = 30, face = "bold", hjust = 0.5),
                  plot.background = element_rect(fill = "white", color = NA))
  )

ggsave(file.path(comparison_dir, paste0("WY", wy_dry, "_scenario_comparison_panels.png")),
       combined_dry, width = 22, height = 12, dpi = 300)
message("Saved: WY", wy_dry, "_scenario_comparison_panels.png")

message("\nAll comparison figures written to: ", normalizePath(comparison_dir))

