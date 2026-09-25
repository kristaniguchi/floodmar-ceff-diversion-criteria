# ==============================================================================
# Scott River — Hydrograph Figures (standalone)
# COMID: 3798909
#
# Produces for each target water year:
#   1. Build series of PNGs — each adds one scenario over observed flow
#      (designed for PowerPoint progressive builds / animations)
#   2. Combined PNG with all scenarios overlaid
#
# Elements on every plot:
#   - Grey ribbon:            observed (Baseline) daily flow
#   - Colored dashed lines:   each diversion scenario
#   - Grey shaded rectangle:  wet season diversion window (Dec 1 – Mar 31)
#   - Horizontal lines:       Peak 2/5/10-year and WS50/WS75 thresholds
#   - Right-side labels:      threshold values in cfs
#
# STANDALONE — does not require Scott_River_FFC_analysis.R to be loaded.
# Reads Scott_Available.csv directly and fetches thresholds from the API.
#
# REQUIREMENTS
#   install.packages(c("tidyverse", "lubridate", "scales", "httr", "readr"))
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
input_dir            <- "./Data/inputs/Scott_River"
scott_available_path <- file.path(input_dir, "Scott_Available.csv")

# ── Outputs ───────────────────────────────────────────────────────────────────
hydrograph_dir <- "./Data/outputs/Scott_River/hydrographs"
dir.create(hydrograph_dir, showWarnings = FALSE, recursive = TRUE)

# Percentiles output from Scott_River_FFC_analysis.R (source of WS50/WS75 thresholds)
results_dir           <- "./Data/outputs/Scott_River/FFM/results"
scott_percentiles_csv <- file.path(results_dir, "scott_percentiles.csv")

# ── Site parameters ───────────────────────────────────────────────────────────
comid <- 3798909

# ── Target water years ────────────────────────────────────────────────────────
# wet:      1983 — strong diversions Dec-Mar for all scenarios, clean spring
#           recession signal for 80/50 and Ecohydraulic; best wet year to show
#           (1998 excluded — 80/50 never fires due to dry start to water year)
# moderate: 2015 — highest diversion separation of any moderate year
#           (1999 is actually classified wet by FFC)
# dry:      2014 — Ecohydraulic and FF-WS scenarios still active; 80/50 and
#           FF-Pk2 = zero, which illustrates conservative behavior in dry years
#           (2021 excluded — near-zero diversions under all scenarios)
target_years <- list(
  wet      = 1983,
  moderate = 2015,
  dry      = 2014
)

# ── Scenario build order (progressive reveal for PPT) ─────────────────────────
build_order <- c("90_20", "Ecohydraulic", "80_50", "FF_Pk2", "FF_WS75", "FF_WS50")

# ── Scenario map: code -> display label, color, linetype ──────────────────────
scenario_styles <- tribble(
  ~scenario_code, ~label,          ~color,     ~linetype,  ~available_col,
  "90_20",        "90/20",         "#E41A1C",  "dashed",   "90/20",
  "Ecohydraulic", "Ecohydraulic",  "#29B6F6",  "dashed",   "362 (Ecohydraulic)",
  "80_50",        "80/50",         "#D4A017",  "dashed",   "80/50",
  "FF_Pk2",       "FF-Pk2",        "#F06292",  "dashed",   "peak_2_20",
  "FF_WS75",      "FF-WS75",       "#CE93D8",  "dashed",   "75_bf_50_20",
  "FF_WS50",      "FF-WS50",       "#00E676",  "dashed",   "50_bf_50_20"   # lime green — visible against grey observed flow
)

# ── Wet-season baseflow thresholds (cfs) ──────────────────────────────────────
# Pulled automatically from scott_percentiles.csv (see PART 2B below).

# ==============================================================================
# PART 1 — Build daily flow timeseries for each scenario
# ==============================================================================

message("Reading Scott_Available.csv...")
raw <- read_csv(scott_available_path, show_col_types = FALSE) %>%
  mutate(date = mdy(date))

# Baseline = observed flow, no diversion
daily_flows <- list()
daily_flows[["Baseline"]] <- raw %>% transmute(date, flow = discharge_cfs)

# Diversion scenarios: instream = discharge - water_available (floor at 0)
for (i in seq_len(nrow(scenario_styles))) {
  sc  <- scenario_styles$scenario_code[i]
  col <- scenario_styles$available_col[i]
  daily_flows[[sc]] <- raw %>%
    transmute(date, flow = pmax(discharge_cfs - .data[[col]], 0))
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
write_csv(daily_flows_long, file.path(hydrograph_dir, "scott_daily_flows_all_scenarios.csv"))
message("  Saved: scott_daily_flows_all_scenarios.csv (", nrow(daily_flows_long), " rows, ",
        length(unique(daily_flows_long$scenario_code)), " scenarios)")

# ==============================================================================
# PART 2 — Fetch unimpaired peak magnitude thresholds from natural flows API
# ==============================================================================

message("Fetching peak thresholds from natural flows API (COMID ", comid, ")...")
thresholds <- get_peak_thresholds(comid = comid)
peak_thresh <- thresholds$p50   # p50 = canonical threshold (matches API reference basis)

# ==============================================================================
# PART 2B — Pull WS50/WS75 thresholds from scott_percentiles.csv
#
# Both WS50 and WS75 come from the same metric, Wet_BFL_Mag_50 (wet-season
# baseflow magnitude, 50th-percentile-within-year definition) — WS50 is the
# p50_predicted value across years, WS75 is the p75_predicted value across
# years. Requires Scott_River_FFC_analysis.R to have been run already.
# ==============================================================================

if (!file.exists(scott_percentiles_csv)) {
  stop("scott_percentiles.csv not found at: ", scott_percentiles_csv,
       "\nRun Scott_River_FFC_analysis.R first to generate it.")
}

pct_raw <- read_csv(scott_percentiles_csv, show_col_types = FALSE)

get_ws_threshold <- function(pct_df, metric_name, pct_col) {
  row <- pct_df %>%
    filter(scenario_code == "Baseline", WYT == "any", metric == metric_name)
  if (nrow(row) == 0 || !pct_col %in% names(row)) {
    message("  WARNING: could not find ", metric_name, " / ", pct_col,
            " (Baseline, WYT=any) in scott_percentiles.csv — line will be omitted.")
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

#' Build one hydrograph plot for a given water year and set of scenarios.
#'
#' @param wy                water year (integer, Oct 1 to Sep 30)
#' @param scenarios_to_show character vector of scenario_codes to overlay
#'                          (empty = observed flow only)
#' @param y_max             y-axis ceiling; NULL = auto from observed peak
build_hydro_plot <- function(wy,
                              scenarios_to_show = character(0),
                              show_thresholds   = TRUE,
                              y_max             = NULL,
                              x_range           = NULL) {

  date_start <- x_range[1] %||% make_date(wy - 1L, 10L,  1L)
  date_end   <- x_range[2] %||% make_date(wy,       9L, 30L)
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

    # wet season shading
    annotate("rect",
             xmin = ws_start, xmax = ws_end,
             ymin = -Inf,     ymax = Inf,
             fill = "grey85", alpha = 0.6) +

    # observed flow — thick grey line
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
                 date_breaks = if (!is.null(x_range)) "1 month" else "2 months",
                 limits      = c(date_start, date_end + days(if (!is.null(x_range)) 5 else 25)),
                 expand      = expansion(0)) +
    scale_y_continuous(labels = comma,
                       limits = c(0, y_upper),
                       expand = expansion(mult = c(0, 0.02))) +

    labs(x = NULL, y = "Discharge (cfs)") +
    coord_cartesian(clip = "off") +
    theme_hydro

  # threshold lines and labels added AFTER scale definitions
  # using fixed aesthetics (not aes()) so they don't interfere with
  # the scenario color/linetype scales
  if (show_thresholds) {
    for (i in seq_len(nrow(thresh_lines))) {

      # stagger WS50 and WS75 labels to avoid overlap — WS50 goes below its line,
      # WS75 goes above, since they're only ~237 cfs apart
      vjust_val <- if (thresh_lines$label[i] == "WS50") 1.3
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
  y_max <- max(obs_year$flow, na.rm = TRUE) * 1.08

  yr_dir <- file.path(hydrograph_dir, paste0("WY", wy, "_", yr_name))
  dir.create(yr_dir, showWarnings = FALSE, recursive = TRUE)

  # define all individual plots to produce
  # each entry: filename suffix, scenarios_to_show, show_thresholds
  plot_specs <- list(
    list(suffix = "1_observed_only",    scenarios = character(0),   thresholds = FALSE),
    list(suffix = "2_observed_thresh",  scenarios = character(0),   thresholds = TRUE),
    list(suffix = "3_90_20",            scenarios = "90_20",        thresholds = TRUE),
    list(suffix = "4_Ecohydraulic",     scenarios = "Ecohydraulic", thresholds = TRUE),
    list(suffix = "5_80_50",            scenarios = "80_50",        thresholds = TRUE),
    list(suffix = "6_FF_Pk2",           scenarios = "FF_Pk2",       thresholds = TRUE),
    list(suffix = "7_FF_WS75",          scenarios = "FF_WS75",      thresholds = TRUE),
    list(suffix = "8_FF_WS50",          scenarios = "FF_WS50",      thresholds = TRUE),
    list(suffix = "9_all_scenarios",    scenarios = build_order,    thresholds = TRUE)
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
# Scenario-comparison facet panels for WY1983 (representative wet year) and
# WY2014 (representative dry year), each showing 90/20, 80/50, FF-WS75, and
# FF-Pk2 over the full water year.
# ==============================================================================

# helper to get y_max for a water year
get_y_max <- function(wy) {
  obs <- daily_flows[["Baseline"]] %>%
    filter(date >= make_date(wy - 1L, 10L, 1L),
           date <= make_date(wy,       9L, 30L))
  max(obs$flow, na.rm = TRUE) * 1.08
}

comparison_dir <- file.path(hydrograph_dir, "comparisons")
dir.create(comparison_dir, showWarnings = FALSE, recursive = TRUE)

# Build individual panel data for WY1983
# Same structure as build_hydro_plot but returns a plot without legend
# (legend added once at the bottom of the combined figure)
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

build_panel <- function(wy, scenario_code, panel_label, y_max, show_thresh = TRUE, x_range = NULL) {
  taf_val      <- compute_taf(wy, scenario_code)
  badge_color  <- scenario_styles$color[scenario_styles$scenario_code == scenario_code]
  badge_x      <- x_range[2] %||% make_date(wy, 9L, 30L)

  build_hydro_plot(wy,
                   scenarios_to_show = scenario_code,
                   show_thresholds   = show_thresh,
                   y_max             = y_max,
                   x_range           = x_range) +
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

library(patchwork)

wy83      <- 1983L
y83_max   <- get_y_max(wy83)

pA <- build_panel(wy83, "90_20",   "A. 90/20",   y83_max)
pB <- build_panel(wy83, "80_50",   "C. 80/50",   y83_max)
pC <- build_panel(wy83, "FF_WS75", "B. FF-WS75", y83_max)
pD <- build_panel(wy83, "FF_Pk2",  "D. FF-Pk2",  y83_max)

panel_annotation <- plot_annotation(
  title    = "Scott River WY1983 — Wet Year Scenario Comparison",
  subtitle = paste0("Grey ribbon = observed flow  |  Colored dashed = scenario  |  ",
                    "Grey shading = wet season (Dec-Mar)  |  Lines = peak/WS thresholds"),
  theme = theme(plot.title    = element_text(size = 30, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 20, hjust = 0.5, color = "grey40"),
                plot.background = element_rect(fill = "white", color = NA))
)

combined_83 <- (pA + pC) / (pB + pD) + panel_annotation
ggsave(file.path(comparison_dir, "WY1983_scenario_comparison_panels.png"),
       combined_83, width = 22, height = 12, dpi = 300)
message("  Saved: WY1983_scenario_comparison_panels.png")

# ── WY2014 dry year scenario comparison ───────────────────────────────────────
# Same four scenarios as wet year for direct comparison.
# Note: 80/50 has near-zero diversion in dry years (conservative by design);
# FF-Pk2 also minimal. 90/20 and FF-WS50 still fire at lower flows.
wy14    <- 2014L
y14_max <- get_y_max(wy14)

pA14 <- build_panel(wy14, "90_20",   "A. 90/20",   y14_max)
pB14 <- build_panel(wy14, "80_50",   "C. 80/50",   y14_max)
pC14 <- build_panel(wy14, "FF_WS75", "B. FF-WS75", y14_max)
pD14 <- build_panel(wy14, "FF_Pk2",  "D. FF-Pk2",  y14_max)

panel_annotation_14 <- plot_annotation(
  title    = "Scott River WY2014 — Dry Year Scenario Comparison",
  subtitle = paste0("Grey ribbon = observed flow  |  Colored dashed = scenario  |  ",
                    "Grey shading = wet season (Dec-Mar)  |  Lines = peak/WS thresholds"),
  theme = theme(plot.title    = element_text(size = 30, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 20, hjust = 0.5, color = "grey40"),
                plot.background = element_rect(fill = "white", color = NA))
)

combined_14 <- (pA14 + pC14) / (pB14 + pD14) + panel_annotation_14
ggsave(file.path(comparison_dir, "WY2014_scenario_comparison_panels.png"),
       combined_14, width = 22, height = 12, dpi = 300)
message("  Saved: WY2014_scenario_comparison_panels.png")

message("\nAll comparison figures written to: ", normalizePath(comparison_dir))

