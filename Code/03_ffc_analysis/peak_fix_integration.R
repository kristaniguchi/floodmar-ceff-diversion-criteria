# ==============================================================================
# Peak Flow Metric Fix — Integration Functions (COMID only)
#
# PROBLEM
# The FFC sets its peak magnitude threshold from each scenario's own annual
# maxima. Diversion scenarios have clipped peaks -> lower threshold ->
# artificially inflated Peak_Dur and Peak_Fre.
#
# FIX
# Use predicted unimpaired peak magnitudes from the natural flows database
# (flow-api.codefornature.org) as the threshold instead.
#
# CRITICAL CONSTRAINT
# assess_alteration() reads metric data from a CSV file path, not a data frame.
# So the fix must patch the file on disk before assess_alteration() reads it.
#
# INTEGRATION — two calls added to her existing loop:
#
#   source("peak_fix_integration.R")
#
#   # ONCE before the loop — fetch unimpaired thresholds for this site
#   thresholds <- get_peak_thresholds(comid = 12345678)
#
#   # INSIDE the loop — after FFC writes its CSV, before assess_alteration()
#   fix_and_write_csv(
#     csv_path       = ffc_output_path,
#     scenario_daily = scenario_daily_df,
#     thresholds     = thresholds$p50   # p50 matches the API reference basis
#   )
#   # assess_alteration() then reads the patched file, unchanged call
#
# REQUIREMENTS: dplyr, lubridate, httr, readr
# ==============================================================================

library(dplyr)
library(lubridate)
library(httr)
library(readr)

# ==============================================================================
# get_peak_thresholds(comid, wyt)
#
# Call ONCE before the loop.
#
# Fetches predicted unimpaired Peak_2/5/10 magnitude percentiles from the
# natural flows database for the given COMID and returns all five as separate
# threshold vectors so you can see the full range of what "unimpaired" means
# at this reach.
#
# Arguments:
#   comid           NHD reach COMID
#   wyt             water year type: "all" (default), "wet", "dry", "moderate"
#   return_periods  c(2, 5, 10) — matches the FFC's three peak output columns.
#                   Only change this if you have a reason to.
#
# Returns a named list with five elements ($p10, $p25, $p50, $p75, $p90),
# each a named numeric vector of flow thresholds in cfs:
#   c("2" = <cfs>, "5" = <cfs>, "10" = <cfs>)
#
# Pass ONE of these to fix_and_write_csv():
#   thresholds$p50  — RECOMMENDED. The natural flows API's own Peak_Dur_* and
#                     Peak_Fre_* reference percentiles were computed using the
#                     median unimpaired peak magnitude as their threshold, so
#                     p50 is the only choice directly comparable to the reference.
#   thresholds$p25 / thresholds$p75  — for sensitivity runs.
# ==============================================================================

get_peak_thresholds <- function(comid,
                                 wyt            = "all",
                                 return_periods = c(2, 5, 10)) {

  message("Fetching predicted unimpaired peak magnitudes for COMID ", comid, "...")
  url  <- paste0("https://flow-api.codefornature.org/v2/ffm/?comids=", comid)
  resp <- GET(url)
  if (status_code(resp) != 200L)
    stop("Natural flows API request failed for COMID ", comid,
         " (HTTP ", status_code(resp), ")\n",
         "Check that this COMID is within the natural flows model study area.")

  df      <- read_csv(content(resp, as = "text", encoding = "UTF-8"),
                      show_col_types = FALSE)
  wyt_val <- if (wyt == "all") "all" else wyt
  df      <- df %>% filter(.data$wyt == wyt_val, source %in% c("model", "inferred"))

  mag_ffms <- setNames(paste0("peak_", return_periods), as.character(return_periods))
  mag_df   <- df %>% filter(ffm %in% mag_ffms)

  if (nrow(mag_df) == 0L)
    stop("No peak magnitude metrics (peak_2/5/10) found for COMID ", comid,
         " / wyt = '", wyt, "'. Check the COMID or try wyt = 'all'.")

  pct_cols   <- c("p10", "p25", "p50", "p75", "p90")
  thresholds <- lapply(setNames(pct_cols, pct_cols), function(pct) {
    vals <- setNames(mag_df[[pct]][match(mag_ffms, mag_df$ffm)], names(mag_ffms))
    if (any(is.na(vals)))
      warning("NA threshold at ", pct, " for some return periods. ",
              "Check COMID or wyt.")
    vals
  })

  message("Predicted unimpaired peak magnitude thresholds (cfs):")
  for (pct in pct_cols)
    cat(sprintf("  %-4s  Peak_2: %6.1f  Peak_5: %6.1f  Peak_10: %6.1f\n",
                pct,
                thresholds[[pct]][["2"]],
                thresholds[[pct]][["5"]],
                thresholds[[pct]][["10"]]))

  thresholds
}

# ==============================================================================
# fix_and_write_csv(csv_path, scenario_daily, thresholds, out_path)
#
# Call INSIDE the loop after the FFC writes its annual_flow_result CSV
# and before assess_alteration() reads it.
#
# Reads the CSV, replaces Peak_Dur_2/5/10, Peak_Fre_2/5/10, Peak_Tim_2/5/10
# with values recomputed using the supplied unimpaired magnitude thresholds,
# then writes the file back to disk. All other columns are untouched.
#
# Arguments:
#   csv_path        Path to the *_annual_flow_result.csv the FFC just wrote.
#   scenario_daily  data.frame(date = Date, flow = numeric) for this scenario.
#   thresholds      One percentile vector from get_peak_thresholds(), e.g.
#                   thresholds$p50. Do not pass the full list.
#   out_path        Where to write the patched CSV. Defaults to csv_path
#                   (overwrites in place). Supply a different path to keep
#                   the original FFC output untouched.
#
# Returns out_path invisibly.
# ==============================================================================

fix_and_write_csv <- function(csv_path,
                               scenario_daily,
                               thresholds,
                               out_path = csv_path) {

  if (is.list(thresholds) && "p50" %in% names(thresholds))
    stop("Pass a single percentile vector, e.g. thresholds$p50, not the full list.")

  # read CSV preserving all rows including Avg/Std/CV summary rows
  raw       <- read_csv(csv_path, show_col_types = FALSE, col_types = cols(.default = "c"))
  data_mask <- !raw$Year %in% c("Avg", "Std", "CV")
  df        <- raw[data_mask, ]
  df$Year   <- as.integer(df$Year)

  # these columns are character — preserve them untouched
  char_cols   <- c("Overall_Int_Class", "Int_Class", "WYT")
  numeric_cols <- setdiff(names(df), c("Year", char_cols))
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) suppressWarnings(as.numeric(x)))

  # recompute peak dur/fre/tim at unimpaired thresholds
  mat   <- .build_flow_matrix(scenario_daily)
  fixed <- .calc_peak_metrics(mat, thresholds)
  wy_chr <- as.character(df$Year)

  for (rp in names(thresholds)) {
    dur_col <- paste0("Peak_Dur_", rp)
    fre_col <- paste0("Peak_Fre_", rp)
    tim_col <- paste0("Peak_Tim_", rp)
    if (dur_col %in% names(df)) df[[dur_col]] <- as.numeric(fixed$duration[[rp]][wy_chr])
    if (fre_col %in% names(df)) df[[fre_col]] <- as.numeric(fixed$freq[[rp]][wy_chr])
    if (tim_col %in% names(df)) df[[tim_col]] <- as.numeric(fixed$timing[[rp]][wy_chr])
  }

  # write back, preserving summary rows
  raw[data_mask, ] <- df %>% mutate(across(everything(), as.character))
  write_csv(raw, out_path)
  message("Peak metrics patched and written to: ", out_path)
  invisible(out_path)
}

# ==============================================================================
# Internal helpers
# ==============================================================================

.build_flow_matrix <- function(df) {
  df <- df %>%
    mutate(
      wy   = if_else(month(date) >= 10L, year(date) + 1L, year(date)),
      dowy = as.integer(date - make_date(wy - 1L, 9L, 30L))
    ) %>%
    filter(dowy >= 1L, dowy <= 366L)
  years <- sort(unique(df$wy))
  mat   <- matrix(NA_real_, nrow = 366L, ncol = length(years),
                  dimnames = list(NULL, as.character(years)))
  for (i in seq_along(years)) {
    rows <- df %>% filter(wy == years[i])
    mat[rows$dowy, i] <- rows$flow
  }
  mat
}

.median_of_time <- function(x) {
  n <- length(x)
  if (n == 0L) return(NA_real_)
  if (n == 1L) return(x[1L])
  if (n %% 2L == 1L) return(x[ceiling(n / 2L)])
  if (n == 2L) return(mean(x))
  mean(c(x[n %/% 2L], x[n %/% 2L + 2L]))
}

.max_consec_na <- function(x) {
  r <- rle(is.na(x))
  if (!any(r$values)) return(0L)
  max(r$lengths[r$values])
}

.calc_peak_metrics <- function(mat, thresholds,
                                max_zero   = 270L,
                                max_nan    = 36L,
                                max_consec = 7L) {
  rps   <- names(thresholds)
  years <- colnames(mat)
  n_yr  <- ncol(mat)

  tim <- setNames(lapply(rps, function(r) rep(NA_real_, n_yr)), rps)
  dur <- setNames(lapply(rps, function(r) rep(NA_real_, n_yr)), rps)
  frq <- setNames(lapply(rps, function(r) rep(NA_real_, n_yr)), rps)

  for (ci in seq_len(n_yr)) {
    col  <- mat[, ci]
    skip <- sum(is.na(col))             > max_nan   ||
            sum(col == 0, na.rm = TRUE) > max_zero  ||
            .max_consec_na(col)         > max_consec
    if (skip) next

    for (rp in rps) {
      thresh   <- thresholds[[rp]]
      in_event <- FALSE; event_dur <- 0L; total_dur <- 0L
      starts   <- integer(0L)
      for (day in seq_len(nrow(mat))) {
        q     <- col[day]
        above <- !is.na(q) && q >= thresh
        if      ( above && !in_event) { in_event <- TRUE; event_dur <- 1L; starts <- c(starts, day) }
        else if ( above &&  in_event) { event_dur <- event_dur + 1L }
        else if (!above &&  in_event) { total_dur <- total_dur + event_dur; event_dur <- 0L; in_event <- FALSE }
      }
      if (in_event) total_dur <- total_dur + event_dur
      n_ev          <- length(starts)
      tim[[rp]][ci] <- if (n_ev      > 0L) .median_of_time(starts) else NA_real_
      dur[[rp]][ci] <- if (total_dur > 0L) total_dur               else NA_integer_
      frq[[rp]][ci] <- if (n_ev      > 0L) n_ev                    else NA_integer_
    }
  }
  for (rp in rps) {
    names(tim[[rp]]) <- years
    names(dur[[rp]]) <- years
    names(frq[[rp]]) <- years
  }
  list(timing = tim, duration = dur, freq = frq)
}

# ==============================================================================
# get_peak_timing(scenario_daily, thresholds, scenario_name)
#
# Peak timing (median day-of-water-year of the first flow exceedance above the
# peak magnitude threshold) is intentionally excluded from the FFC's
# annual_flow_result CSV — dict_to_array() skips the 'timings' key for the
# winter/peak section entirely. It cannot be patched into the file or assessed
# via assess_alteration().
#
# This function computes Peak_Tim_2, Peak_Tim_5, and Peak_Tim_10 directly from
# the scenario daily timeseries using the same unimpaired magnitude thresholds
# used for duration and frequency. Column names match FFC convention so results
# can be compared directly across scenarios.
#
# Arguments:
#   scenario_daily  data.frame(date = Date, flow = numeric)
#   thresholds      One percentile vector from get_peak_thresholds(), e.g.
#                   thresholds$p50. Use the same percentile you pass to
#                   fix_and_write_csv() so all three peak metrics (magnitude
#                   threshold, duration, frequency, timing) are consistent.
#   scenario_name   Optional string label added as a "scenario" column.
#                   Recommended when binding rows across scenarios in a loop.
#
# Returns a data frame with one row per water year and columns:
#   wy              water year (integer)
#   Peak_Tim_2      day-of-water-year of first exceedance above Peak_2 threshold
#   Peak_Tim_5      day-of-water-year of first exceedance above Peak_5 threshold
#   Peak_Tim_10     day-of-water-year of first exceedance above Peak_10 threshold
#                   (day 1 = Oct 1; NA if no exceedance events that year)
#   scenario        scenario_name, if supplied
#
# Example — collect timing across all scenarios then compare:
#
#   timing_list <- list()
#   for (scenario in scenarios) {
#     timing_list[[scenario]] <- get_peak_timing(
#       scenario_daily = daily_flows[[scenario]],
#       thresholds     = thresholds$p50,
#       scenario_name  = scenario
#     )
#   }
#   all_timing <- bind_rows(timing_list)
#
#   # median timing by scenario and return period
#   all_timing %>%
#     group_by(scenario) %>%
#     summarise(across(starts_with("Peak_Tim_"), median, na.rm = TRUE))
# ==============================================================================

get_peak_timing <- function(scenario_daily,
                             thresholds,
                             scenario_name = NULL) {

  if (is.list(thresholds) && "p50" %in% names(thresholds))
    stop("Pass a single percentile vector, e.g. thresholds$p50, not the full list.")

  # thresholds must cover exactly 2, 5, 10 to match FFC column naming
  missing_rps <- setdiff(c("2", "5", "10"), names(thresholds))
  if (length(missing_rps) > 0)
    warning("thresholds missing return period(s): ",
            paste(missing_rps, collapse = ", "),
            ". Corresponding Peak_Tim_* columns will be absent.")

  mat   <- .build_flow_matrix(scenario_daily)
  fixed <- .calc_peak_metrics(mat, thresholds)

  # build wide: one row per water year, one column per return period
  years <- colnames(mat)
  df    <- data.frame(wy = as.integer(years), stringsAsFactors = FALSE)

  for (rp in names(thresholds)) {
    col_name    <- paste0("Peak_Tim_", rp)
    df[[col_name]] <- as.numeric(fixed$timing[[rp]][years])
  }

  # ensure columns appear in order Peak_Tim_2, Peak_Tim_5, Peak_Tim_10
  tim_cols <- intersect(c("Peak_Tim_2", "Peak_Tim_5", "Peak_Tim_10"), names(df))
  df       <- df[, c("wy", tim_cols), drop = FALSE]

  if (!is.null(scenario_name)) df$scenario <- scenario_name

  df
}
