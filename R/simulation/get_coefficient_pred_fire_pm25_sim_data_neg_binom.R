# Name of script: get_coefficient_pred_fire_pm25_sim_data_neg_binom
# Description: Function to extract dataframe of point estimates and upper/lower quantiles
# for the coefficient on predicted LFS PM2.5 exposure using a negative binomial model
# from the simulated outcome data. By construction, the true effect of predicted fire PM2.5
# in the simulated data is zero, so this function allows us to check how well a `wrong` model
# recovers the true effect of zero.
# Created by: Calum Kennedy (calum.kennedy.25@ucl.ac.uk)
# Created on: 05-11-2025
# Latest update by: Calum Kennedy
# Latest update on: 05-11-2025

# Comments ---------------------------------------------------------------------

# @ param `list_data_simulated` - list of simulated outcome datasets
# @ param `unit_id_var` - name of unit ID variable
# @ param `time_id_var` - name of time ID variable
# @ param `week_id_var` - name of week ID variable
# @ param `treated_var` - name of treated variable
# @ param `outcome_var` - name of outcome variable
# @ param `year_var` - name of year ID variable
# @ param `linear_predictors` - name of predictors OTHER than "pred_fire_PM25" that we wish to include
# @ param `temp_var` - name of temperature variable
# @ param `spline_df_per_year` - number of degrees of freedom for natural spline of time in the negative binomial model
# @ param `spline_df_temp` - degrees of freedom for natural spline of temperature in the negative binomial model
# @ return `result` - tibble with four columns: mean, q05, q95, column_label

# Function ---------------------------------------------------------------------


get_coefficient_pred_fire_pm25_sim_data_neg_binom <- function(list_data_simulated,
                                                              unit_id_var,
                                                              time_id_var,
                                                              week_id_var,
                                                              treated_var,
                                                              outcome_var,
                                                              year_var,
                                                              linear_predictors,
                                                              temp_var,
                                                              spline_df_per_year,
                                                              spline_df_temp){
  
  # Extract list of unique locations from data
  unique_locations <- pull(distinct(list_data_simulated[[1]], {{unit_id_var}}))
  
  # Append pred_fire_PM25 variable to linear predictors
  linear_predictors <- c(linear_predictors, "pred_fire_PM25")
  
  # Set quoted variables to pass to future_map
  time_id_var_quo <- as_name(ensym(time_id_var))
  outcome_var_quo <- as_name(ensym(outcome_var))
  temp_var_quo <- as_name(ensym(temp_var))
  year_var_quo <- as_name(ensym(year_var))
  unit_id_var_quo <- as_name(ensym(unit_id_var))

list_results_coef_pred_fire_PM25 <- future_map(list_data_simulated, function(data){
  
  # Export ns function to workers
  ns
  
  # Extract list of location-specific datasets from main data
  list_data_location_specific <- lapply(unique_locations, function(x){filter(data, .data[[unit_id_var_quo]] == x)})
  
  # Get list of estimated coefficients on pred_fire_PM25 from each location
  list_coef_pred_fire_PM25 <- map(list_data_location_specific, 
                                                 function(x){
                                                   
                                                   # Estimate negative binomial regression model and extract output
                                                   model <- estimate_neg_binomial_model(data = x,
                                                                                        outcome_var_quo,
                                                                                        year_var_quo,
                                                                                        linear_predictors,
                                                                                        time_id_var_quo,
                                                                                        temp_var_quo,
                                                                                        spline_df_per_year,
                                                                                        spline_df_temp)
                                                   
                                                   # Extract coefficient on pred_fire_PM25
                                                   model$coefficients[["pred_fire_PM25"]]
                                                   
                                                 })
  
})

# Transpose results to get list of coefficient estimates by location across simulated datasets
list_results_coef_pred_fire_PM25_by_location <- list_results_coef_pred_fire_PM25 %>%
  transpose() 

# Get mean of coefficient + upper and lower quantiles across all simulation runs
results <- list_results_coef_pred_fire_PM25_by_location %>%
  map_dfr(~ {
    vals <- unlist(.x)
    tibble(
      mean = mean(vals),
      q05  = quantile(vals, 0.05),
      q95  = quantile(vals, 0.95)
    )
  })

# Bind the original vector of locations
result <- tibble(result, column_label = unique_locations)

}