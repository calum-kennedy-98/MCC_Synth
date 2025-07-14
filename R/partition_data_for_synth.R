# Name of script: partition_data_for_synth
# Description: Function to partition main MCC-LFS data into small subsets for
# synthetic control analysis
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 14-07-2025
# Latest update by: Calum Kennedy
# Latest update on: 14-07-2025

# Comments ---------------------------------------------------------------------

# @ param ... 

# Function ---------------------------------------------------------------------

partition_data_for_synth <- function(data,
                                     unit_id_var,
                                     time_id_var,
                                     region_id_var,
                                     treated_var){
  
  # Generate quoted variables to passs to function below
  region_id_var_quo <- as_string(ensym(region_id_var))
  unit_id_var_quo <- as_string(ensym(unit_id_var))
  
  # Generate indicator variable for each treatment episode by location
  # To do this, generate indicator variable for whenever a treated period ends,
  # and generate a counting variable (cumulative sum) of this indicator variable by location
  data_with_treatment_episode <- data %>%
    
    mutate(end_exposure = ifelse({{treated_var}} - lag({{treated_var}}, default = 0) == -1, 1, 0),
           treatment_episode = cumsum(end_exposure),
           .by = {{unit_id_var}}) %>%
    
    # Generate start and end date for each treatment episode
    mutate(start_date = min({{time_id_var}}, na.rm = TRUE),
           end_date = max({{time_id_var}}, na.rm = TRUE),
           .by = c({{unit_id_var}},
                   treatment_episode))
  
  # Generate dataframe of distinct location-treatment pairs,
  # remove final treatment episode for each unit (as correspond to last section of data
  # where there may or may not be a treatment). For example, if max(treatment_episode) == 0
  # then it means that the given unit did not have any expoure during the entire timeframe
  unit_treatment_pairs <- distinct(data_with_treatment_episode, 
                                   {{unit_id_var}}, 
                                   {{region_id_var}},
                                   treatment_episode,
                                   start_date,
                                   end_date) %>%
    
    filter(treatment_episode < max(treatment_episode, na.rm = TRUE),
           .by = {{unit_id_var}})
  
  # Convert to list
  unit_treatment_pairs <- split(unit_treatment_pairs, seq(nrow(unit_treatment_pairs)))
  
  # Generate data subsets to pass to synthetic control methods
  list_data_raw_for_synth <- lapply(unit_treatment_pairs, function(x){
    
    data_subset <- data_with_treatment_episode %>%
      
      # For each item in 'unit_treatment_pairs', take subset of control units in same region
      # as treated unit and filter date between start/end date
      filter({{region_id_var}} == x[[region_id_var_quo]],
             between({{time_id_var}}, 
                     x[["start_date"]],
                     x[["end_date"]])) %>%
      
      # Remove control units with any non-zero treatment status
      filter(sum({{treated_var}}) == 0 | {{unit_id_var}} == x[[unit_id_var_quo]],
                 .by = {{unit_id_var}})
    
    # Extract treatment start date
    treatment_start_date <- data_subset %>%
      filter({{treated_var}} == 1) %>%
      pull({{time_id_var}}) %>%
      min()
    
    # Create treated/post indicators
    data_final <- data_subset %>%
      
      mutate(treated = ifelse({{unit_id_var}} == x[[unit_id_var_quo]], 1, 0),
             post = ifelse({{time_id_var}} >= treatment_start_date, 1, 0)) %>%
      
      # Remove auxiliary columns
      select(!c(treatment_episode,
                end_exposure,
                start_date,
                end_date))
    
    return(data_final)
    
  })
  
}