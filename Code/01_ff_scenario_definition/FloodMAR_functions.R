
# loop able functions



calc_ffm_availability <- function(
    gage_file,
    ffm_file,
    gageid,
    site,
    bf_metrics = c("p25", "p50"),
    wet_months = c(12, 1, 2, 3)
) {
  
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(janitor)
  
  # -----------------------
  # Read FFM once
  # -----------------------
  ffm <- read.csv(ffm_file) %>%
    dplyr::filter(wyt == "all", source == "model")
  
  
  get_ffm_value <- function(ffm_df, metric, quantile) {
    
    if (!metric %in% ffm_df$ffm) {
      stop(paste("FFM metric not found:", metric))
    }
    
    if (!quantile %in% names(ffm_df)) {
      stop(paste("Quantile not found:", quantile))
    }
    
    ffm_df %>%
      dplyr::filter(.data$ffm == metric) %>%
      dplyr::pull(.data[[quantile]]) %>%
      .[1]
  }
  
  
  peaks <- list(
    peak2  = get_ffm_value(ffm, "peak_2",  "p50"),
    peak5  = get_ffm_value(ffm, "peak_5",  "p50"),
    peak10 = get_ffm_value(ffm, "peak_10", "p50")
  )
  
  # -----------------------
  # Read gage once
  # -----------------------
  gage <- read.csv(gage_file) %>%
    clean_names()
  
  gage$date <- as.Date(gage$date)
  
  gage <- gage %>%
    mutate(
      site_number = gageid,
      water_year = ifelse(month(date) >= 10, year(date) + 1, year(date)),
      month = month(date)
    )
  
  years <- unique(gage$water_year)
  out <- list()
  
  # =======================
  # LOOP OVER BF METRICS
  # =======================
  for (bf in bf_metrics) {
    
    
    bf_threshold <- if (bf %in% c("p25","p50","p75","p90")) {
      
      get_ffm_value(ffm, "wet_bfl_mag_50", bf)
      
    } else if (bf %in% c("peak_2","peak_5","peak_10")) {
      
      get_ffm_value(ffm, bf, "p50")
      
    } else {
      stop(paste("Unsupported bf_metric:", bf))
    }
    
    
    yearly_results <- list()
    
    # -----------------------
    # Loop over water years
    # -----------------------
    for (yr in years) {
      
      gage.wy <- gage %>% filter(water_year == yr)
      
      gage.ffm <- gage.wy %>%
        filter(
          month %in% wet_months,
          discharge_cfs > bf_threshold
        )
      
      thresholds <- gage.ffm %>%
        filter(discharge_cfs > peaks$peak2) %>%
        mutate(
          threshold = case_when(
            discharge_cfs > peaks$peak10 ~ "p10",
            discharge_cfs > peaks$peak5  ~ "p5",
            TRUE                         ~ "p2"
          )
        ) %>%
        group_by(water_year, threshold) %>%
        mutate(count = row_number()) %>%
        ungroup()
      
      gage.ffm <- gage.ffm %>%
        left_join(
          thresholds,
          by = c("date", "water_year", "discharge_cfs")
        ) %>%
        mutate(
          available_ffm = case_when(
            threshold == "p10" & count < 2 ~ discharge_cfs - peaks$peak10,
            threshold == "p5"  & count < 2 ~ discharge_cfs - peaks$peak5,
            threshold == "p2"  & count < 4 ~ discharge_cfs - peaks$peak2,
            TRUE                           ~ discharge_cfs - bf_threshold
          )
        )
      
      gage.other <- gage.wy %>%
        anti_join(gage.ffm, by = "date") %>%
        mutate(
          available_ffm = 0,
          threshold = NA_character_,
          count = 0
        )
      
      gage.final <- bind_rows(gage.ffm, gage.other) %>%
        arrange(date) %>%
        mutate(
          instream_ffm = discharge_cfs - available_ffm,
          available_20 = pmin(available_ffm, discharge_cfs * 0.2),
          available_40 = pmin(available_ffm, discharge_cfs * 0.4),
          instream_20  = discharge_cfs - available_20,
          instream_40  = discharge_cfs - available_40
        )
      
      metric_cols <- c(
        "available_ffm", "instream_ffm",
        "available_20", "available_40",
        "instream_20", "instream_40"
      )
      
      yearly_results[[as.character(yr)]] <-
        gage.final %>%
        pivot_longer(
          cols = all_of(metric_cols),
          names_to = c("metric", "scenario"),
          names_pattern = "(.*)_(.*)",
          values_to = "value"
        ) %>%
        mutate(
          bf_metric = bf,
          bf_threshold = bf_threshold
        ) %>%
        select(site_number, date, water_year,
               discharge_cfs, bf_metric, bf_threshold,
               metric, scenario, value)
    }
    
    out[[bf]] <- bind_rows(yearly_results)
  }
  
  bind_rows(out) %>%
    mutate(site = site)
}
