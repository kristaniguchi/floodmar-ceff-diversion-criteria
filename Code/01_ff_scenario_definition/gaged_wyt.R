#' gaged_wyt.R
#' 
#' Function to calculate water year type for gaged data. Gaged data requires Date, Discharge.cfs, month, and year columns
#' 
#' @param dat dataframe with gaged discharge values. Columns must include: "Date", "Discharge.cfs", "month", "year"
#' @return original dataframe of gaged discharge with associated water month types
#' 
#' @example gaged_wyt(gage_data)
#' 

gaged_wyt <- function(dat){
  
  #calculate water year
  dat_wy <- dat %>% 
    mutate(water_year = ifelse(month(date) > 9, year(date) + 1, year(date)))
  
  #calculate flow exceedence 
  df.exceedence <- dat_wy %>% 
    group_by(water_year) %>% 
    summarize(maf = mean(discharge_cfs)) %>% 
    arrange(desc(maf)) %>% 
    mutate(id = row_number()) %>% 
    mutate(exceedance_wy=(id/(max(id+1))*100), #exceedence interval formula
           year_type=case_when(exceedance_wy<33.333 ~ "wet", 
                                exceedance_wy<66.666 ~ "moderate", 
                                TRUE ~ "dry"))
  
  #join back to original dataframe and select columns of interest
  df.wyt <- df.exceedence %>% 
    select(water_year, year_type)
  
  df_wyt<-dat_wy %>% 
    left_join(df.wyt, by = "water_year")
  
  
  
  return(df_wyt)
}
