cat("\014") ;  rm(list=ls())

library(dataRetrieval); library(dplyr); library(ggplot2); library(lubridate); library(tidyr);library(janitor);
library(xts);library(plotly)
library(here)
source(here("C:/Users/jessica.ayers/Box/My CEFF/FloodMAR/Functions/gaged_wyt.R"))
source(here("C:/Users/jessica.ayers/Box/My CEFF/FloodMAR/Functions/pull_gage.R"))

## 1. pull gauge data ####
site="Stony Creek"
gageid="STN"
comid= "12074576"
#or 12074210
# ds of the dam

ffm <- read.csv("C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Stony Creek\\ffm_12074576.csv", header=T)
ffm <- ffm %>% filter(wyt == "all", source == "model")

# minimum is 440 cfs

peak2=ffm[which(ffm$ffm == "peak_2"),]$p50
peak5=ffm[which(ffm$ffm == "peak_5"),]$p50
peak10=ffm[which(ffm$ffm == "peak_10"),]$p50


bf25=ffm[which(ffm$ffm == "wet_bfl_mag_50"),]$p25
bf50=ffm[which(ffm$ffm == "wet_bfl_mag_50"),]$p50
bf75=ffm[which(ffm$ffm == "wet_bfl_mag_50"),]$p75
bf90=ffm[which(ffm$ffm == "wet_bfl_mag_50"),]$p90


#gage<-pull_gage(gageid,"","") #pull and reformat gage data
#gage<-gage%>%
#  clean_names()%>%
#  mutate(day=day(date))

gage = read.csv("C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Stony Creek\\Stony_flows_SWB_clean.csv")
gage$Date <- as.Date(gage$Date, format = "%m/%d/%Y")
gage$site_number = paste0(gageid)

# Create water year
gage$water_year <- ifelse(
  as.integer(format(gage$Date, "%m")) >= 10,
  as.integer(format(gage$Date, "%Y")) + 1,
  as.integer(format(gage$Date, "%Y"))
)


gage <- gage %>%
  rename(date = Date, "discharge_cfs" = "flow_cfs")

gage$month <- month(gage$date)

# Define the years you want to loop over
years <- unique(gage$water_year)  # replace with your years

# Initialize a list to store results for each year
results_list <- list()

for (y in 1:length(years)) {
  year = years[y]
  #mets = ffm[which(ffm$Year == year),]
  
  bf_threshold = bf25 #mets$Wet_BFL_Mag_50 # wet season median flow for wet year
  
  #bf_threshold = ifelse(bf_threshold < 440, 440, bf_threshold)
  
  gage.wy <- gage[which(gage$water_year == year),]
  
  gage.ffm <- gage.wy %>%
    filter(month %in% c(12,1,2,3), discharge_cfs > bf_threshold)
  
  
  thresholds <- gage.ffm %>%
    filter(discharge_cfs > peak2) %>%
    mutate(threshold = case_when(
      discharge_cfs > peak10 ~ "p10",
      discharge_cfs > peak5  ~ "p5",
      TRUE                   ~ "p2"
    )) %>%
    filter(!is.na(threshold)) %>%                    # <-- not threshold != 0
    group_by(water_year, threshold) %>%
    mutate(count = dplyr::row_number()) %>%
    ungroup()
  
  
  gage.ffm <- gage.ffm %>%
    # be explicit about join keys if needed, e.g., by = c("date")
    left_join(thresholds, by = intersect(names(gage.ffm), names(thresholds))) %>%
    mutate(
      threshold     = as.character(threshold),       # <-- force character
      count         = as.integer(count),
      available_ffm = case_when(
        threshold == "p10" & count < 2 ~ discharge_cfs - peak10,
        threshold == "p5"  & count < 2 ~ discharge_cfs - peak5,
        threshold == "p2"  & count < 4 ~ discharge_cfs - peak2,
        TRUE                            ~ discharge_cfs - bf_threshold
      )
    )
  
  
  keys <- c("date")  # or c("site_number","date") if multiple sites
  gageother <- gage.wy %>%
    anti_join(gage.ffm %>% select(all_of(keys)) %>% distinct(), by = keys) %>%
    mutate(
      threshold     = NA_character_,   # <-- character, not 0
      count         = 0L,
      available_ffm = 0.0
    )
  
  #set the persent of flow available for diversion 
  
  
  gage.ffm2 <- bind_rows(gageother, gage.ffm) %>%
    mutate_all(~replace(., is.na(.), 0))%>%
    mutate(instream_ffm=discharge_cfs-available_ffm,
           available_20=ifelse(available_ffm>discharge_cfs*.2, discharge_cfs*.2, available_ffm),
           available_40=ifelse(available_ffm>discharge_cfs*.4, discharge_cfs*.4, available_ffm),
           instream_20=(discharge_cfs-available_20),
           instream_40=(discharge_cfs-available_40)) %>%
    
    arrange(date)
  
  
  metric_cols <- c("available_ffm", "instream_ffm",
                   "available_20", "available_40",
                   "instream_20", "instream_40")
  
  all_results_long <- gage.ffm2 %>%
    pivot_longer(
      cols = all_of(metric_cols),
      names_to = c("metric", "scenario"),
      names_pattern = "(.*)_(.*)",   # splits at the last underscore
      values_to = "value"
    ) %>%
    select(site_number, date, discharge_cfs, water_year,
           metric, scenario, value)
  
  
  
  # Save results for this year
  results_list[[as.numeric(y)]] <- data.frame(all_results_long)
}

# Combine results for all years into one dataframe if needed
all_results <- bind_rows(results_list)
all_results$threshold = "bf25"

write.csv(all_results, "C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Data\\Stony Creek\\WS_bf25_flowreq.csv", row.names = F)

## 5. create plots ####

# Filter instream-only data
instream_data <- all_results %>%
  filter(metric == "instream")

fig <- plot_ly(all_results, 
               x = ~date, y = ~discharge_cfs,
               type = "scatter", mode = "lines",
               name = "Current flow",
               line = list(color = "lightgrey", width = 6))

fig1 <- fig %>%
  add_trace(x = ~date, y = bf_threshold,
            mode = "lines", name = "baseflow",
            line = list(color = "black", width = 1, dash = "dot")) %>%
  add_trace(x = ~date, y = peak2,
            mode = "lines", name = "2 year peak",
            line = list(color = "black", width = 1, dash = "dot")) %>%
  add_trace(x = ~date, y = peak5,
            mode = "lines", name = "5 year peak",
            line = list(color = "black", width = 1, dash = "dot")) %>%
  add_trace(x = ~date, y = peak10,
            mode = "lines", name = "10 year peak",
            line = list(color = "black", width = 1, dash = "dot")) %>%
  
  # ---- Instream Flows by Scenario (using tidy data) ----
add_trace(data = instream_data %>% filter(scenario == "ffm"),
          x = ~date, y = ~value, mode = "lines",
          name = "Functional flows (FFM)",
          line = list(color = "green", width = 3, dash = "dash")) %>%
  
  add_trace(data = instream_data %>% filter(scenario == "20"),
            x = ~date, y = ~value, mode = "lines",
            name = "Functional flows, 20% max",
            line = list(color = "blue", width = 3, dash = "dash")) %>%
  
  add_trace(data = instream_data %>% filter(scenario == "40"),
            x = ~date, y = ~value, mode = "lines",
            name = "Functional flows, 40% max",
            line = list(color = "red", width = 3, dash = "dash")) %>%
  
  layout(
    title = paste0("Instream flow by scenario: ", site, ", gage ", gageid),
    yaxis = list(title = "Discharge (cfs)"),
    xaxis = list(title = NA)
  )

htmlwidgets::saveWidget(
  fig1,
  file = "C:/Users/jessica.ayers/Box/My CEFF/FloodMAR/Plots/Stony Creek/instream_flows_plot_bf25.html"
)



fig<- plot_ly(all, x=~date, y=~discharge_cfs, type="scatter", name="Current flow", mode='lines',line=list(color="lightgrey", alpha=.5,dash='solid', width=6))
fig1 <- fig %>%
  add_trace(x=~date, y=bf_threshold, mode='lines', name="baseflow",line=list(color="black", width=1, dash="dot"))%>%
  add_trace(x=~date, y=peak2, mode='lines', name="2 year peak",line=list(color="black", width=1, dash="dot"))%>%
  add_trace(x=~date, y=peak5, mode='lines', name="5 year peak",line=list(color="black", width=1, dash="dot"))%>%
  add_trace(x=~date, y=peak10, mode='lines', name="10 year peak",line=list(color="black", width=1, dash="dot"))%>%
  add_trace(x=~date, y=~instream_80, mode='lines',name="Functional flows, 20% max",line=list(color="blue", width=3, dash="dash"))%>%
  add_trace(x=~date, y=~instream_60, mode='lines',name="Functional flows, 40% max",line=list(color="red", width=3, dash="dash"))%>%
  add_trace(x=~date, y=~instream_ffm, mode='lines',name="Functional flows",line=list(color="green", width=3, dash="dash"))%>%
  
  layout(title = paste0("Instream flow by scenario: ", site, ", gage ", gageid), yaxis=list(title="Discharge (cfs)"), xaxis=list(title=NA))

htmlwidgets::saveWidget(fig1, file = "C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Plots\\Scott River\\instream_flows_plot_peak.html")


available<- plot_ly(all, x=~date, y=~discharge_cfs, type="scatter", name="Current flow", mode='lines',line=list(color="lightblue", alpha=.5,dash='solid', width=6))
available1<- available %>%
  add_trace(x=~date, y=~available_ffm, mode='lines',name="Functional flows available",line=list(color="green", width=3, dash="dot"))%>%
  add_trace(x=~date, y=~available_80, mode='lines',name="Functional flows, 20% max",line=list(color="blue", width=3, dash="dash"))%>%
  add_trace(x=~date, y=~available_60, mode='lines',name="Functional flows, 40% max",line=list(color="red", width=3, dash="dash"))%>%
  add_trace(x=~date, y=bf_threshold, mode='lines', name="baseflow",line=list(color="black", width=1, dash="dash"))%>%
  add_trace(x=~date, y=peak2, mode='lines', name="2 year peak",line=list(color="black", width=1, dash="dot"))%>%
  add_trace(x=~date, y=peak5, mode='lines', name="5 year peak",line=list(color="black", width=1, dash="dot"))%>%
  add_trace(x=~date, y=peak10, mode='lines', name="10 year peak",line=list(color="black", width=1, dash="dot"))%>%
  layout(title = paste0("Available for diversion by scenario: ", site, ", gage ", gageid), yaxis=list(title="Discharge (cfs)"), xaxis=list(title=NA))

htmlwidgets::saveWidget(available1, file = "C:\\Users\\jessica.ayers\\Box\\My CEFF\\FloodMAR\\Plots\\Scott River\\available_flows_plot_peak.html")



# Create a plot of diverable days
# divertable days

AF.output.ffm<-all %>%
  mutate(af1=ifelse(available_ffm<35, available_ffm,35),
         af=af1*1.9834710990151,
         available_801=ifelse(available_80<35, available_80,35),
         af_80=available_801*1.9834710990151,
         available_601=ifelse(available_60<35, available_60,35),
         af_60=available_601*1.9834710990151)%>%
  group_by(water_year)%>%
  summarize(days.ffm=sum(af>0),
            days.ffm80=sum(af_80>0),
            days.ffm60=sum(af_60>0),
            
            divertable.af.ffm=round(sum(af, na.rm=T),0),
            divertable.af.ffm80=round(sum(af_80,na.rm=T),0),
            divertable.af.ffm60=round(sum(af_60,na.rm=T),0))


wyt<-gaged_wyt(gage)%>%
  select(water_year,year_type)%>%
  unique()

# Number of divertable days and number of diverable water under each scenario over the year
table1<-AF.output.ffm %>%
  full_join(wyt)


table.wyt<-table1 %>%
  #group_by(year_type)%>%
  summarize(ffm_peaks=mean(days.ffm,na.rm=T),
            ffm_80per=round(mean(days.ffm80, na.rm=T),0))

kable(table.wyt, digits=0, caption="Mean divertable days by scenario")

table.wyt3<-table1 %>%
  #group_by(year_type)%>%
  summarize(ffm_peaks=max(days.ffm,na.rm=T),
            ffm_80=max(days.ffm80,na.rm=T))

kable(table.wyt3, digits=0, caption="Max divertable days by scenario")

table.wyt2<-AF.output.ffm %>%
  # group_by(year_type)%>%
  summarize(ffm_peaks=round(sum(divertable.af.ffm,na.rm=T),0),
            ffm_80per=round(sum(divertable.af.ffm80, na.rm=T),0))

kable(table.wyt2, digits=0, format.args=list(big.mark=','),caption="Mean divertable volume (AF) by scenario")

#divertable volume by day (reality check)


