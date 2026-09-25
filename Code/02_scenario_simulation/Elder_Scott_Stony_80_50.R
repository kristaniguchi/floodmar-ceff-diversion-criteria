####### wet season cumulative Diversion Analysis ######
### Developed by Environmental Science Associates
### Developed for DWR under TOS-ESA-4600015545
### Code and supporting files provided "as is"
### Dated: 8/2026

library(dplyr)
library(lubridate)
library(dataRetrieval)

elder <- read.csv("Elder_flow_SWB_clean.csv")
names(elder)[2] <- "Streamflow"
elder$Date <- as.Date(elder$Date, format = "%m/%d/%Y")

stony <- read.csv("Stony_flows_SWB_clean.csv")
names(stony)[2] <- "Streamflow"
stony$Date <- as.Date(stony$Date, format = "%m/%d/%Y")

scott <- readNWISdv(11519500, "00060", "1941-10-01", "2025-09-30")
names(scott)[4] <- "Streamflow"

prep_cumflow_percentiles <- function(df) {
  
  df <- df %>%
    mutate(
      Date = as.Date(Date),
      
      # Convert cfs → acre-ft/day
      flow_af = Streamflow * 1.98346,
      
      # Water Year (USGS)
      wy = if_else(month(Date) >= 10, year(Date) + 1, year(Date)),
      
      # Day of Water Year
      wy_start = as.Date(paste0(wy - 1, "-10-01")),
      dowy = as.integer(Date - wy_start) + 1
    ) %>%
    arrange(Date)
  
  # Compute cumulative flow per water year
  df <- df %>%
    group_by(wy) %>%
    mutate(cum_flow = cumsum(flow_af)) %>%
    ungroup()
  
  # Compute percentiles by day-of-water-year
  pct_tbl <- df %>%
    group_by(dowy) %>%
    summarize(
      p50 = quantile(cum_flow, 0.5, na.rm = TRUE),
      p80 = quantile(cum_flow, 0.8, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(pct_tbl)
}
simulate_diversions <- function(df, pct_tbl, threshold_cfs = NULL) {
  
  df <- df %>%
    mutate(
      Date = as.Date(Date),
      flow_af = Streamflow * 1.98346,
      
      wy = if_else(month(Date) >= 10, year(Date) + 1, year(Date)),
      wy_start = as.Date(paste0(wy - 1, "-10-01")),
      dowy = as.integer(Date - wy_start) + 1,
      
      month = month(Date),
      day = day(Date)
    ) %>%
    left_join(pct_tbl, by = "dowy") %>%
    arrange(Date)
  
  n <- nrow(df)
  
  diversion_cfs <- numeric(n)
  diversion_af  <- numeric(n)
  reason <- character(n)
  
  cum_flow <- 0
  diversion_on <- FALSE
  current_wy <- df$wy[1]
  
  for (i in 1:n) {
    
    # Reset at new water year
    if (df$wy[i] != current_wy) {
      cum_flow <- 0
      diversion_on <- FALSE
      current_wy <- df$wy[i]
    }
    
    # Wet season check
    in_wet_season <- (df$month[i] == 12) |
      (df$month[i] %in% c(1,2,3))
    
    flow_cfs <- df$Streamflow[i]
    flow_af  <- df$flow_af[i]
    
    div_cfs <- 0
    day_reason <- NA
    
    if (!in_wet_season) {
      
      cum_flow <- cum_flow + flow_af
      day_reason <- "non wet season"
      
    } else {
      
      # Update cumulative BEFORE decision
      cum_flow <- cum_flow + flow_af
      
      # Turn ON
      if (!diversion_on && cum_flow > df$p80[i]) {
        diversion_on <- TRUE
      }
      
      # Turn OFF
      if (diversion_on && cum_flow < df$p50[i]) {
        diversion_on <- FALSE
        day_reason <- "below 50th"
      }
      
      if (diversion_on) {
        
        # Check threshold first
        if (!is.null(threshold_cfs) && flow_cfs <= threshold_cfs) {
          
          div_cfs <- 0
          day_reason <- "below threshold"
          
        } else {
          
          # 20% rule
          max_div_cfs <- 0.2 * flow_cfs
          
          # Apply threshold constraint
          if (!is.null(threshold_cfs)) {
            max_div_cfs <- min(max_div_cfs, flow_cfs - threshold_cfs)
            max_div_cfs <- max(max_div_cfs, 0)
          }
          
          div_cfs <- max_div_cfs
          day_reason <- "diverting"
        }
        
      } else {
        
        # Not diverting because not wet enough
        if (is.na(day_reason)) {
          day_reason <- "below 80th"
        }
      }
      
      # Apply diversion to cumulative flow
      div_af <- div_cfs * 1.98346
      cum_flow <- cum_flow - div_af
    }
    
    diversion_cfs[i] <- div_cfs
    diversion_af[i]  <- div_cfs * 1.98346
    reason[i] <- day_reason
  }
  
  df$diversion_cfs <- diversion_cfs
  df$diversion_af  <- diversion_af
  df$reason        <- reason
  
  return(df %>% select(Date, Streamflow, diversion_cfs, diversion_af, reason))
}

elder_80_50 <- prep_cumflow_percentiles(elder)
elder_80_50_sim <- simulate_diversions(df = elder, pct_tbl = elder_80_50,threshold_cfs = 125)
write.csv(elder_80_50_sim, "Elder_80_50_wiener_all.csv")

stony_80_50 <- prep_cumflow_percentiles(stony)
stony_80_50_sim <- simulate_diversions(df = stony, pct_tbl = stony_80_50,threshold_cfs = 645)
write.csv(stony_80_50_sim, "Stony_80_50_wiener_all.csv")

scott_80_50 <- prep_cumflow_percentiles(scott)
scott_80_50_sim <- simulate_diversions(df = scott, pct_tbl = scott_80_50,threshold_cfs = 538)
write.csv(scott_80_50_sim, "Scott_80_50_wiener_all.csv")
