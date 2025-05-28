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
    
    # Left join to cities dataframe
    left_join(
      cities, by = c("column_label" = "city")
      ) %>%
    
    # Clean names and set as tibble
    clean_names() %>%
    
    as_tibble() %>%
    
    # Keep data between 2000-2019
    filter(
      between(year, 2000, 2019)
      ) %>%
    
    # Set character columns to factors and arrays to numeric
    mutate(
      across(
        where(is.character),
        as.factor
        ),
      across(
        where(is.array),
        as.numeric
        )
    ) %>%
    
    # Create unique day and week ID variable which carries over across years
    mutate(
      day_id = as.numeric(date - min(date)),
      week_id = day_id %/% 7 + 1
    ) %>%
    
    # Keep distinct unit-time pairs (some duplicates in raw data)
    distinct(
      column_label, date, .keep_all = TRUE
      ) %>%
    
    # Select relevant columns
    select(
      vars_to_select
      )
  
  # Return merged dataset
  return(data_mcc)
  
}