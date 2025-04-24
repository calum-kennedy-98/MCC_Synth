# Name of script: generate_treatment_indicator
# Description: Function to generate treated status for MCC cities
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 23-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 23-04-2025

# Comments ---------------------------------------------------------------------

# Args - R dataframe, identifiers for ID variable and fire PM variables, threshold to set
# cutoff for acute LFS pollution exposure
# Function - Generates an indicator variable for whether a city is defined as 'treated'
# by an acute LFS pollution episode at time t. The indicator is defined as follows:
# 1. Set 'treated' equal to 1 if the 3-day moving avg LFS pollution exceeds 'fire_PM25_threshold'
# for 'n_days_above_threshold' consecutive days
# 2. Treated stays equal to 1 until there are 'n_days_below_threshold' where the moving avg
# LFS pollution is less than 'fire_PM25_threshold', when it is reset to 0

# Function ---------------------------------------------------------------------

generate_treatment_indicator <- function(data,
                                         id_var,
                                         fire_pm_ma_var,
                                         fire_pm_var,
                                         quantile_threshold,
                                         n_days_above_threshold,
                                         n_days_below_threshold){
  
  # Generate absolute threshold above which to define acute exposure
  fire_PM25_threshold <- quantile(data[[fire_pm_var]], probs = c(quantile_threshold), na.rm = TRUE)
  
  # Generate treatment indicator
  data_with_treatment <- data %>%
    mutate(
      above_threshold = ifelse({{fire_pm_ma_var}} > fire_PM25_threshold, 1, 0),
      .by = {{id_var}}
      ) %>%
    mutate(
      treated = {
        n <- n()
        treated_vec <- rep(0, n)
        in_treatment <- FALSE
        below_threshold_counter <- 0
        
        for (i in seq_len(n)) {
          if (!in_treatment) {
            if (i >= n_days_above_threshold && all(above_threshold[(i - n_days_above_threshold + 1):i] == 1)) {
              in_treatment <- TRUE
              treated_vec[i] <- 1
            }
          } else {
            if (pred_fire_PM25_ma[i] < fire_PM25_threshold) {
              below_threshold_counter <- below_threshold_counter + 1
            } else {
              below_threshold_counter <- 0
            }
            
            treated_vec[i] <- 1
            
            if (below_threshold_counter >= n_days_below_threshold) {
              in_treatment <- FALSE
              below_threshold_counter <- 0
            }
          }
        }
        treated_vec
      },
      .by = {{id_var}}
    )
  
  return(data_with_treatment)
}
