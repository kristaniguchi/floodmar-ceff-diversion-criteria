# Functions to pull daily discharge gage data from USGS. Provide gageid, start date, and end date.
#' 
#' @param gageid gage number of USGS gage
#' @param startDate first day of observations (generally Oct. 1 of any year)
#' @param endDate last day of observations (generally Sep. 30 of any year)
#' 
#' @dependencies dataRetrieval, tidyverse, lubridate
#' 
#' @example pull_gage(gageid = 11467200, startDate = "1960-10-01", endDate = "2000-09-30)
#'
