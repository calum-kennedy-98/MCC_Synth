# Name of script: make_list_sim_data_negative_binomial_model
# Description: Wrapper function to generate K replications of simulated data from
# negative binomial data using parallel processing
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

make_list_data_negative_binomial_model <- function(n_sims,
                                                   data,
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
  unique_locations <- pull(distinct(data, {{unit_id_var}}))
  
  # Extract list of location-specific datasets from main data
  list_data_location_specific <- lapply(unique_locations, function(x){filter(data, {{unit_id_var}} == x)})
  
  # Set quoted variables to pass to future_map
  time_id_var_quo <- as_name(ensym(time_id_var))
  outcome_var_quo <- as_name(ensym(outcome_var))
  temp_var_quo <- as_name(ensym(temp_var))
  year_var_quo <- as_name(ensym(year_var))
  
  # Simulate outcome data from negative binomial model
  list_data_simulated_neg_binomial <- future_map(list_data_location_specific, 
                                                 .options = furrr_options(seed = TRUE), 
                                                 function(x){
    
    # Set ns function as global
    ns
    
    # Estimate negative binomial regression model and extract output
    model <- estimate_neg_binomial_model(data = x,
                                         outcome_var_quo,
                                         year_var_quo,
                                         linear_predictors,
                                         time_id_var_quo,
                                         temp_var_quo,
                                         spline_df_per_year,
                                         spline_df_temp)
    
    # Generate 'n_sims' simulations of outcome variable from the fitted model,
    # assuming an underlying negative binomial distribution (changing seed each time)
    list_data_simulated <- future_map(seq_len(n_sims), function(i){
      sim_data_negative_binomial_model(
        data = x,
        model = model)
      }, .options = furrr_options(seed = TRUE))
    
  })

  # Generate final list comprised of vectors of simulated outcomes
  list_outcome_sim_neg_binom_model <- map(transpose(list_data_simulated_neg_binomial), unlist)
  
  return(list_outcome_sim_neg_binom_model)
  
}