####### Functions for Diversion Analysis ######
### Developed by Environmental Science Associates
### Developed for DWR under TOS-ESA-4600015545
### Code and supporting files provided "as is"
### Dated: 8/2026

##### Diversion Analysis Functions ######
# Requires following packages are installed: lubridate, 
# Running Functions requires input of flow data, 'data', with at a minimum a Date column, Y-M-D format, and Flow column

# Function to assign water year
water_year <- function(x) {
  x_lt <- as.POSIXlt(x)
  # POSIXlt stores year as years since 1900
  x_lt$year + 1900L + ifelse(x_lt$mon + 1L >= 10L, 1L, 0L)
}

# Function to check leap year
is_leap_year <- function(year) {
  year %% 400 == 0 | (year %% 4 == 0 & year %% 100 != 0)
}

# Function to assign julian day based on leap year
fudge_jd <- function(year, jd) {
  ifelse(is_leap_year(year) & jd >= 60, jd - 1, jd)
}

## Prepare Data Function
# Prepares input data by adding year, water year, month and julian day columns
# Adds columns for flow update and diversion used by later functions
# flow update set equal to input flow and diversion set equal to zero
prep_data <- function(data, date_col = "time", flow_col = "value") {
  # add data columns for Julian days 1-365 with 2/29 same as 2/28
  # year, water year, and month
  data$year <- lubridate::year(data[[date_col]])
  data$WY <- water_year(data[[date_col]])
  data$month <- lubridate::month(data[[date_col]])
  data$JD <- fudge_jd(data$year, lubridate::yday(data[[date_col]]))
  data
}

## Get 90th percentile values for Julian Days Function
# Takes output from "prep_data" and returns dataframe of julian days with 90th percentile flows
percentiles_90 <- function(data, flow_col = "value", prob = 0.90) {
  JD_90_Ptile <- aggregate(
    x = as.formula(paste(flow_col, "~ JD")),
    data = data,
    FUN = "quantile",
    probs = prob
  )
  colnames(JD_90_Ptile) <- c("JD", "q90")
  JD_90_Ptile
}

## Diversion Analysis Function - 90/20 Approach
# Computes daily diversion based on SWB 90/20 guidance
# Requires input of outputs from "prep_data" and "percentiles_90" functions
# Includes other arguments for diversion percent, set = 20, and scalar to convert flow at input to flow at POD, set = 1
# Returns List, first element,'daily' is dataframe of daily outputs including input data and diversions
# second element, 'total_div' is total diversions by water year for December - March

div_anal <- function(
  data,
  JD_90_Ptile,
  div_cap,
  div_percent_FNF = 0.2,
  POD_correction = 1,
  flow_col = "value"
) {
  # data is flow data, JD_90_Ptile is daily 90th percentile threshold,
  # div_cap is maximum diversion based on physical limit (this can be set to artificially high value for no limit)
  # div_percent_FNF is max percent of flow that can be diverted at POD, default should be 20%
  # POD_correction is a correction factor if flow at gage differs by scalar than at POD, if same set to 1

  # join 90th percentiles to prepared data
  comb <- merge(data, JD_90_Ptile, by = "JD", all.x = TRUE)
  comb$divert_flag <- comb[[flow_col]] > comb[["q90"]] &
    comb$month %in% c(12, 1:3) # diversion months
  # max percent of flow
  comb$max_percent_flow <- div_percent_FNF * (comb[[flow_col]] / POD_correction)
  # available flow above 90th percentile threshold
  comb$available_flow <- comb[[flow_col]] - comb[["q90"]]
  comb$divert <- ifelse(
    # check if daily flow is greater than 90th percentile threshold
    comb$divert_flag,
    # If yes divert min of following: max diversion, max percent of flow, available flow above 90th percentile threshold
    # pmin provides the rowwise minimum (with pcap being filled automatically)
    pmin(div_cap, comb$max_percent_flow, comb$available_flow),
    # else branch indicates no diversions
    0
  )

  comb$Q_update <- comb[[flow_col]] - comb$divert
  comb$divert_af <- comb$divert * 1.983 # convert cfs to AF

  # sum AF by WY
  tot_div <- aggregate(divert_af ~ WY, data = comb, FUN = sum)

  list(daily = comb, total = tot_div)
}

## Diversion Analysis Functions - Diversion Threshold Approach
# Computes daily diversion based on bespoke diversion criteria
# Requires input of outputs from "prep_data
# Includes other arguments for flow when diversion can occur, 'div_crit'; cap on diversion capacity, 'div_cap';
# diversion percent set = 20 and scalar to convert flow at input to flow at POD set = 1
# Returns dataframe of total diversions by water year for December - March

div_anal2 <- function(
  data,
  div_cap,
  div_crit,
  div_percent_FNF,
  POD_correction,
  flow_col = "value"
) {
  # data is flow data, div_cap is maximum diversion based on physical limit (this can be set to artificially high value for no limit)
  # div_crit is threshold before diversion can begin
  # div_percent_FNF is max percent of flow that can be diverted at POD, default should be 20%
  # POD_correction is a correction factor if flow at gage differs by scalar than at POD, if same set to 1

  data$divert_flag <- data[[flow_col]] > div_crit &
    data$month %in% c(12, 1:3) # diversion months
  # max percent of flow
  data$max_percent_flow <- div_percent_FNF * (data[[flow_col]] / POD_correction)
  # available flow above div_crit
  data$available_flow <- data[[flow_col]] - div_crit

  data$divert <- ifelse(
    !data$divert_flag,
    0, # return 0 when divert_flag is FALSE; else fall through to remaining rules
    ifelse(
      data$max_percent_flow >= div_cap & data$available_flow >= div_cap,
      # if both percent_max_flow and available_flow are greater than div_cap then divert div_cap
      div_cap,
      ifelse(
        data[[flow_col]] - data$max_percent_flow > div_crit,
        data$max_percent_flow,
        data$available_flow
      )
    )
  )

  data$Q_update <- data[[flow_col]] - data$divert
  data$divert_af <- data$divert * 1.983 # convert cfs to AF

  # sum AF by WY
  tot_div <- aggregate(divert_af ~ WY, data = data, FUN = sum)

  list(daily = data, total = tot_div)
}
