# Name of script: merge_data_mcc_lfs
# Description: Function to merge cleaned MCC data with LFS air pollution data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-03-2025

# Comments ---------------------------------------------------------------------

# Define function to merge MCC and LFS air pollution data ----------------------

merge_data_mcc_lfs <- function(data_mcc,
                               path_data_lfs,
                               n_lags_all,
                               n_lags_fire_PM25,
                               id_var){
  
  # Load LFS air pollution data from path
  data_lfs <- readRDS(path_data_lfs)
  
  # Left join LFS data to MCC using city and date identifier
  data_mcc_lfs <- left_join(
    data_mcc,
    data_lfs,
    by = c("column_label",
           "date")
    ) %>%
    
    # Filter cities where all fire PM2.5 estimates are missing
    filter(
      any(!is.na(pred_fire_PM25)),
      .by = column_label
    ) %>%
    
    # Mutate new column with estimated non-fire PM2.5 levels
    mutate(
      pred_nonfire_PM25 = pred_total_PM25 - pred_fire_PM25
      ) %>%
    
    # Mutate moving averages of pollution and mortality variables
    mutate(
      all_ma = slide_index_dbl(all, date, mean, .before = days(n_lags_all)),
      pred_fire_PM25_ma = slide_index_dbl(pred_fire_PM25, date, mean, .before = days(n_lags_fire_PM25)),
      pred_nonfire_PM25_ma = slide_index_dbl(pred_nonfire_PM25, date, mean, .before = days(n_lags_fire_PM25)),
      .by = {{id_var}}
    )
  
  return(data_mcc_lfs)
}