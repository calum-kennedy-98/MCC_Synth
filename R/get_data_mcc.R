# Name: get_data_mcc
# Description: Script to load and merge main MCC dataset
# Author: CK
# Date: 01-03-2025

# Define function to load and process MCC data ---------------------------------

get_data_mcc <- function(path_data_mcc,
                         id_var,
                         vars_to_select){
  
  # Load data
  load(path_data_mcc)
  
  # Clean 'dewp' column
  dlist <- lapply(dlist, function(df) {
    if ("dewp" %in% colnames(df)) {
      df$dewp <- as.double(df$dewp)
    }
    return(df)
  })
  
  # Merge dlist items into single MCC dataset and retain data from 2000-2019 only
  # Keep distinct city-date rows and remove duplicates
  # Generate smoothed moving averages of mortality and pollution variables
  data_mcc <- bind_rows(
    dlist, .id = "column_label"
    ) %>%
    left_join(
      cities, by = c("column_label" = "city")
      ) %>%
    clean_names() %>%
    as_tibble() %>%
    filter(
      between(year, 2000, 2019)
      ) %>%
    mutate(
      cityname = factor(cityname),
      column_label = factor(column_label),
      week = week(date)
    ) %>%
    mutate(
      id = cur_group_id(), .by = {{id_var}}
      ) %>%
    distinct(
      column_label, date, .keep_all = TRUE
      ) %>%
    select(
      vars_to_select
      )
  
  # Return merged dataset
  return(data_mcc)
  
}