
#calc_ffm_availability <- function(
#    gage_file,
#    ffm_file,
#    gageid,
#    site,
#    bf_metrics = c("p25", "p50"),
#    wet_months = c(12, 1, 2, 3)
#)
  
library(dataRetrieval); library(dplyr); library(ggplot2); library(lubridate); library(tidyr); library(plotly)

## 1. pull gauge data ####
#site="Stony Creek"
#gageid="STN"
#comid= "12074576"
#or 12074210
# ds of the dam

ffm <- read.csv("C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Elder Creek\\ffm_12068798.csv", header=T)
ffm <- ffm %>% filter(wyt == "all", source == "model")

gage = read.csv("C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Elder Creek\\Elder_daily_flows.csv")
gage$Date <- as.Date(gage$Date, format = "%m/%d/%Y")
gage$site_number = paste0(gageid)

# Create water year
gage$water_year <- ifelse(
  as.integer(format(gage$Date, "%m")) >= 10,
  as.integer(format(gage$Date, "%Y")) + 1,
  as.integer(format(gage$Date, "%Y"))
)

#gage <- gage %>%
  #rename(date = Date, "discharge_cfs" = "flow_cfs")

gage$month <- month(gage$Date)
write.csv(gage,"C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Elder Creek\\Elder_daily_flows.csv")


results <- calc_ffm_availability(
  ffm_file = "C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Elder Creek\\ffm_12068798.csv",
  gage_file  = "C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Elder Creek\\Elder_daily_flows.csv",
  gageid    = "11379500",
  site      = "Elder Creek",
  bf_metrics = c("p25", "p50", "p75","p90", "peak_2")
)


write.csv(results,  "C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Elder Creek\\Elder_Creek_all_scenarios.csv")

bf_to_plot <- "p75"   # or "p50", "p75", "peak_2"


plot_data <- results %>%
  filter(bf_metric == bf_to_plot)

instream_data <- plot_data %>%
  filter(metric == "instream")


bf_threshold <- plot_data$bf_threshold[1]

# Peaks can be computed once from the SAME ffm file used earlier
peak2  <- ffm[which(ffm$ffm=="peak_2"),]$p50
peak5  <- ffm[which(ffm$ffm=="peak_5"),]$p50
peak10 <- ffm[which(ffm$ffm=="peak_10"),]$p50


fig <- plot_ly(
  plot_data,
  x = ~date, y = ~discharge_cfs,
  type = "scatter", mode = "lines",
  name = "Current flow",
  line = list(color = "lightgrey", width = 6)
)

fig1 <- fig %>%
  add_trace(
    x = ~date, y = bf_threshold,
    mode = "lines", name = paste0("Baseflow (", bf_to_plot, ")"),
    line = list(color = "black", width = 1, dash = "dot")
  ) %>%
  add_trace(
    x = ~date, y = peak2,
    mode = "lines", name = "2-year peak",
    line = list(color = "black", width = 1, dash = "dot")
  ) %>%
  add_trace(
    x = ~date, y = peak5,
    mode = "lines", name = "5-year peak",
    line = list(color = "black", width = 1, dash = "dot")
  ) %>%
  add_trace(
    x = ~date, y = peak10,
    mode = "lines", name = "10-year peak",
    line = list(color = "black", width = 1, dash = "dot")
  ) %>%
  
  # ---- Instream flows by scenario ----
add_trace(
  data = instream_data %>% filter(scenario == "ffm"),
  x = ~date, y = ~value, mode = "lines",
  name = "Functional flows (FFM)",
  line = list(color = "green", width = 3, dash = "dash")
) %>%
  add_trace(
    data = instream_data %>% filter(scenario == "20"),
    x = ~date, y = ~value, mode = "lines",
    name = "Functional flows, 20% max",
    line = list(color = "blue", width = 3, dash = "dash")
  ) %>%
  add_trace(
    data = instream_data %>% filter(scenario == "40"),
    x = ~date, y = ~value, mode = "lines",
    name = "Functional flows, 40% max",
    line = list(color = "red", width = 3, dash = "dash")
  ) %>%
  
  layout(
    title = paste0(
      "Instream flow by scenario: ",
      unique(plot_data$site),
      ", gage ",
      gageid,
      " (BF = ", bf_to_plot, ")"
    ),
    yaxis = list(title = "Discharge (cfs)"),
    xaxis = list(title = NULL)
  )

