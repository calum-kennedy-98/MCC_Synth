# Name of script: make_list_data_negative_binomial_model
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
                                                   id_var,
                                                   outcome_var,
                                                   date_var,
                                                   linear_predictors,
                                                   time_var,
                                                   temp_var,
                                                   spline_df_per_year,
                                                   spline_df_temp){
  
  # Extract list of unique locations
  unique_locations <- unique(data[[id_var]])
  
  # Set plan for parallel computing
  plan(multisession, workers = 10)
  
  list_data_simulated_neg_binomial <- future_map(unique_locations, function(x){
    
    # Set ns function as global
    ns
    
    # Extract location specific data to pass to negative binomial model
    data_for_model <- data[data[[id_var]] == x,]
    
    # Estimate negative binomial regression model and extract output
    model <- estimate_neg_binomial_model(data_for_model,
                                         outcome_var,
                                         date_var,
                                         linear_predictors,
                                         time_var,
                                         temp_var,
                                         spline_df_per_year,
                                         spline_df_temp)
    
    # Generate 'n_sims' simulations of outcome variable from the fitted model,
    # assuming an underlying negative binomial distribution (changing seed each time)
    list_data_simulated <- lapply(seq(1:n_sims),
                                  sim_data_negative_binomial_model,
                                  data = data_for_model,
                                  model = model)
    
    # Here we could adapt this to only simulate the subset of data we actually want
    
  })
  
  # Generate final list of data frames by recombining location-specific data frames
  list_data_transposed <- transpose(list_data_simulated_neg_binomial)
  
  list_data_out <- map(list_data_transposed, bind_rows)
  
  # Rest plan to sequential
  plan(sequential)
  
  return(list_data_out)
  
}