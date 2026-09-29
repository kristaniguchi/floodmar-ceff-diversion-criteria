# Functions to pull gage data and format for the rest of the analysis

#' pull_gage.R
# Functions to pull daily discharge gage data from USGS. Provide gageid, start date, and end date.
#' @param gageid gage number of USGS gage
#' @param startDate first day of observations (generally Oct. 1 of any year)
#' @param endDate last day of observations (generally Sep. 30 of any year)
#' 
#' @dependencies dataRetrieval, tidyverse, lubridate
#' 
#' @example pull_gage(gageid = 11467200, startDate = "1960-10-01", endDate = "2000-09-30)
#'
#' gaged_wyt.R
#' 
#' Function to calculate water year type for gaged data. Gaged data requires Date, Discharge.cfs, month, and year columns
#' 
#' @param dat dataframe with gaged discharge values. Columns must include: "Date", "Discharge.cfs", "month", "year"
#' @return original dataframe of gaged discharge with associated water month types
#' 
#' @example gaged_wyt(gage_data)
#' 
