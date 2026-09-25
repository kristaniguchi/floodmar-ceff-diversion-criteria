####### Scott River Diversion Analysis ######
### Developed by Environmental Science Associates
### Developed for DWR under TOS-ESA-4600015545
### Code and supporting files provided "as is"
### Dated: 8/2026

####### Read and Set-up Data and Initial Processing ##########
library(dataRetrieval) #requires USGS dataRetrieval package is installed

## Load Diversion functions from source
# this assumes that you are using a project-based workflow and working from the same directory both scripts are found
source("FloodMAR_Diversion_Functions_V2_TMH.R")

## Use dataRetrieval to read in flows from Scott River USGS Gage 11519500 (first set of code is updated function but does not always work, second code chunk is older function)
scott <- read_waterdata_daily(
  monitoring_location_id = "USGS-11519500",
  parameter_code = "00060",
  time = c("1941-10-01", "2025-09-30")
)
# scott <- readNWISdv(siteNumbers = "11519500", parameterCd = "00060", startDate = "1941-10-01", endDate =  "2025-09-30")

# read_waterdata_daily returns sf object; dropping spatial geometry because not used in this analysis
scott = sf::st_drop_geometry(scott)

## Subset Flows to Impaired period of record
# This step is optional for other gages and based on site specific information
# Rationale for this time period is based on CDFW instream flow study of Scott River
# scott_imp <- scott[which(scott$Date > "1971-09-30"),] Uncomment if subsetting record to this period
scott_imp <- scott

## Prepare Data using Diversion Functions
scott_imp <- prep_data(scott_imp)
JD_90_Ptile <- percentiles_90(scott_imp)

##### Run 90/20 Diversion Analysis ######
div_90_20 <- div_anal(
  data = scott_imp,
  JD_90_Ptile = JD_90_Ptile,
  div_cap = 100000, # max diversion capacity cfs, arbitrary high number for Case Study
  div_percent_FNF = 0.2, # 20% of max flow
  POD_correction = 1, # Set to 1 for analysis at Gage
)

##### Run Alternative Diversion Analysis ######
div_alt <- div_anal2(
  data = scott_imp,
  div_cap = 100000, # max diversion capacity cfs, arbitrary high number for Case Study
  div_crit = 362, # set min Fort Jones Diversion threshold(s)
  div_percent_FNF = 0.2, # set % of flow at POD criteria
  POD_correction = 1, # Set to 1 for analysis at Gage
)

##### Write outputs to working directory ######
write.csv(
  div_90_20[[1]],
  "div_90_20_hydro.csv"
)

write.csv(
  div_90_20[[2]],
  "div_90_20_AF.csv"
)

write.csv(
  div_alt[[1]],
  "div_alt_hydro.csv"
)

write.csv(div_alt[[2]], "div_alt_AF.csv")

write.csv(
  JD_90_Ptile,
  "div_90_20_thres.csv"
)
