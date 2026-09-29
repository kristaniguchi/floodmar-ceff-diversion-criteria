#' pull_gage.R
#' 
#' Function to pull daily discharge gage data from USGS. Provide gageid, start date, and end date.
#' 
#' @param gageid gage number of USGS gage
#' @param startDate first day of observations (generally Oct. 1 of any year)
#' @param endDate last day of observations (generally Sep. 30 of any year)
#' 
#' @dependencies dataRetrieval, tidyverse, lubridate
#' 
#' @example pull_gage(gageid = 11467200, startDate = "1960-10-01", endDate = "2000-09-30)
#'

pull_gage <- function(gageid, startDate, endDate){
  gage <- dataRetrieval::readNWISdv(siteNumbers = gageid,
                                    startDate = startDate,
                                    endDate = endDate,
                                    parameterCd = "00060")
  
  gage_rename <- gage %>% 
    rename("Agency" = agency_cd, 
           "Site Number" = site_no, 
           "Date" = Date, 
           "discharge_cfs" = X_00060_00003, 
           "Approval Code" = X_00060_00003_cd)
  
  gage_date <- gage_rename %>% 
    mutate(date = ymd(Date),
           month = month(date),
           year = year(date),
           water_year = ifelse(month > 9,
                               year + 1,
                               year))
  
  gage_clean <- gage_date %>% 
    select('Site Number', date, discharge_cfs, month, year, water_year)
  
  return(gage_clean)
}
