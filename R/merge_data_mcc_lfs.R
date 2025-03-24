# Name of script: merge_data_mcc_lfs
# Description: Function to merge cleaned MCC data with LFS air pollution data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-03-2025

# Comments ---------------------------------------------------------------------

# Define function to merge MCC and LFS air pollution data ----------------------

merge_data_mcc_lfs <- function(data_mcc,
                               path_data_lfs){
  
  # Load LFS air pollution data from path
  data_lfs <- readRDS(path_data_lfs)
  
  # Left join LFS data to MCC using city and date identifier
  data_mcc_lfs <- left_join(data_mcc,
                            data_lfs,
                            by = c("column_label",
                                   "date"))
  
  return(data_mcc_lfs)
}