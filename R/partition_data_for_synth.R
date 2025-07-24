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
                                     treated_var,
                                     outcome_var,
                                     min_periods_pre,
                                     min_control_units,
                                     min_weekly_mortality){
  
  # Generate quoted variables to pass to function below
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
                end_date)) %>% 
      
      # Retain only observational units with complete outcome data
      filter(
        all(!is.na({{outcome_var}})),
        .by = {{unit_id_var}}
      ) %>%
      
      # Retain only control units with data available during same window as the
      # treated unit
      filter(
        n() == sum(.$treated == 1),
        .by = {{unit_id_var}}
      )
    
    return(data_final)
    
  })
  
  # Filter list elements to retain only data subsets which have non-missing data
  # for the outcome variable in the treated unit. To do this, filter data for
  # treated unit and check that nrow > 0. If nrow == 0 then this implies that
  # there is missing data for the treated unit
  list_data_raw_for_synth <- keep(list_data_raw_for_synth, function(x){
    
    ifelse(
      nrow(
          filter(x, treated == 1)
        ) > 0,
      TRUE,
      FALSE
      )
  })
  
  # Filter list elements to retain subsets with at least 'min_periods_pre' 
  # pre-treatment periods - min_periods_pre must be set by the user (default = 10)
  # To do this, filter data to retain rows for treated unit in pre-treatment period
  # and check the number of rows
  list_data_raw_for_synth <- keep(list_data_raw_for_synth, function(x){
    
    ifelse(
      nrow(
          filter(x, post == 0, treated == 1)
      ) >= min_periods_pre,
      TRUE,
      FALSE
    )
  })
  
  # Filter list elements to retain subsets with at least 'min_control_units'
  # control units - min_control_units must be set by the user (default = 5)
  # To do this, filter data to retain control units  only, extract distinct control 
  # unit IDs, and check if number of rows is larger than 'min_control_units'
  list_data_raw_for_synth <- keep(list_data_raw_for_synth, function(x){
    
    ifelse(
      nrow(
        distinct(
        filter(x, treated == 0),
        {{unit_id_var}}
        )
      ) >= min_control_units,
      TRUE,
      FALSE
    )
  })
  
  # Filter data to retain subsets with sufficient signal-noise ratio (i.e. a large
  # enough total weekly mortality), defined as 'min_weekly_mortality'. To do this,
  # extract vector of pre-treatment outcomes in treated unit and check if mean is
  # greater than or equal to 'min_weekly_mortality'
  list_data_raw_for_synth <- keep(list_data_raw_for_synth, function(x){
    
    ifelse(
      mean(
        pull(
          filter(x, treated == 1, post == 0),
          {{outcome_var}}
          )
        ) >= min_weekly_mortality,
      TRUE,
      FALSE
    )
  })
  
}