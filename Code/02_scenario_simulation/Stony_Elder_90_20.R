####### Stony Creek and Elder Creek Diversion Analysis ######
### Developed by Environmental Science Associates
### Developed for DWR under TOS-ESA-4600015545
### Code and supporting files provided "as is"
### Post-processing for meeting additional diversion criteria (i.e., delta conditions and downstream water users completed in Excel)
### Dated: 8/2026

####### Read and Set-up Data and Initial Processing ##########
library(zoo)
library(lubridate)

## Load Diversion functions from source
# this assumes that you are using a project-based workflow and working from the same directory both scripts are found
source("FloodMAR_Diversion_Functions_V2_TMH.R")

## Load pre-processed input flow data for case study locations
## Input data are same as used for State Water Board temporary permit 21479 (Stony) and temporary permit 21485 (Elder)
stony <- read.csv("Stony_SWB.csv")
elder <- read.csv("Elder_SWB.csv")

# pre-process data for diversion functions
stony$Date <- as.Date(stony$Date, format = "%m/%d/%Y")
elder$Date <- as.Date(elder$Date, format = "%m/%d/%Y")
stony <- stony[complete.cases(stony),]
elder <- elder[complete.cases(elder),]

stony_prep <- prep_data(stony, date_col = "Date", flow_col = "X_00060_00003")
elder_prep <- prep_data(elder, date_col = "Date", flow_col = "X_00060_00003")
stony_JD_90_Ptile <- percentiles_90(stony_prep, flow_col = "X_00060_00003", prob = 0.9)
elder_JD_90_Ptile <- percentiles_90(elder_prep, flow_col = "X_00060_00003", prob = 0.9)

##### Run 90/20 Diversion Analysis ######
stony_div_90_20 <- div_anal(
  data = stony_prep,
  JD_90_Ptile = stony_JD_90_Ptile,
  div_cap = 100000, # max diversion capacity cfs, arbitrary high number for Case Study
  div_percent_FNF = 0.2, # 20% of max flow
  POD_correction = 1, # Set to 1 for analysis at Gage
  flow_col = "X_00060_00003"
)

elder_div_90_20 <- div_anal(
  data = elder_prep,
  JD_90_Ptile = elder_JD_90_Ptile,
  div_cap = 100000, # max diversion capacity cfs, arbitrary high number for Case Study
  div_percent_FNF = 0.2, # 20% of max flow
  POD_correction = 1, # Set to 1 for analysis at Gage
  flow_col = "X_00060_00003"
)

##### Write outputs to working directory ######
write.csv(
  stony_div_90_20[[1]],
  "stony_div_90_20_hydro.csv"
)

write.csv(
  elder_div_90_20[[1]],
  "elder_div_90_20_hydro.csv"
)

write.csv(
  stony_div_90_20[[2]],
  "stony_div_90_20_AF.csv"
)

write.csv(
  elder_div_90_20[[2]],
  "elder_div_90_20_AF.csv"
)
