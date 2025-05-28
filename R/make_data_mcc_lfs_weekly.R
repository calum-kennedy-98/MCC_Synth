# Name of script: make_data_mcc_lfs_weekly
# Description: Function to aggregate MCC data to weekly level
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-03-2025

# Args: MCC LFS pollution raw data, mortality variables to summarise
# Description: Aggregate data to weekly level taking sum of deaths and mean of pollution/temperature
#              If all observations NA, return NA in the sum/mean functions. Sets 'treated' = 1
#              if any observation is classed as 'treated' in the daily data
# Values: Data frame at week-year-city level

make_data_mcc_lfs_weekly <- function(data_mcc_lfs,
                                     climatic_vars,
                                     mortality_vars,
                                     group_vars){
  
  data_mcc_lfs_weekly <- data_mcc_lfs %>%
    
    summarise(
      
      # Take mean of temperature and pollution variables
      across(all_of(climatic_vars), ~ ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE))),
      
      # Take sum of mortality variables (return NA if all are NA)
      across(all_of(mortality_vars), ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))),
      
      # Set 'treated' = 1 if any observation in that week is treated in daily data
      treated = ifelse(all(treated == 0), 0, 1),
      
      # Extract month, year and date indicators (in case where week split across month/year
      # set equal to month/year with most days in it)
      month = round(mean(month, na.rm = TRUE)),
      year = round(mean(year, na.rm = TRUE)),
      date = min(date, na.rm = TRUE),
      
      .by = group_vars
      )
  
  return(data_mcc_lfs_weekly)
}