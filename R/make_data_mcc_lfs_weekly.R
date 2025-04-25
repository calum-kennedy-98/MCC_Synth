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
                                     mortality_vars){
  
  data_mcc_lfs_weekly <- data_mcc_lfs %>%
    
    # Create unique week ID variable which carries over across years
    mutate(
      days_since_start = as.integer(date - min(date)),
      week_id = days_since_start %/% 7 + 1
    ) %>%
    
    # Retain only complete weeks for analysis
    filter(n() == 7,
           .by = c(column_label,
                   week_id)) %>%
    
    summarise(
      
      # Take mean of temperature and pollution variables
      across(all_of(climatic_vars), ~ ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE))),
      
      # Take sum of mortality variables (return NA if all are NA)
      across(all_of(mortality_vars), ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))),
      
      # Set 'treated' = 1 if any observation in that week is treated in daily data
      treated = ifelse(all(treated == 0), 0, 1),
      
      # Extract start date of week
      date = min(date, na.rm = TRUE),
      
      .by = c(column_label,
              week_id,
              region)
      )
  
  return(data_mcc_lfs_weekly)
}