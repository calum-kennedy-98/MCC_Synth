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
                                                   treated_var,
                                                   outcome_var,
                                                   date_var,
                                                   linear_predictors,
                                                   week_var,
                                                   temp_var,
                                                   spline_df_per_year,
                                                   spline_df_temp){
  
  # Set seed
  set.seed(42)
  
  # Extract list of unique locations from data
  unique_locations <- pull(distinct(data, {{id_var}}))
  
  # Extract list of location-specific datasets from main data
  list_data_location_specific <- lapply(unique_locations, function(x){filter(data, {{id_var}} == x)})
  
  # Extract unit and time treatment probabilities
  unit_time_treatment_probs <- estimate_assignment_probabilities(data = data,
                                                                 treated_var = {{treated_var}},
                                                                 id_var = {{id_var}},
                                                                 week_var = {{week_var}})
  
  # Set quoted variables to pass to future_map
  week_var_quo <- deparse(substitute(week_var))
  outcome_var_quo <- deparse(substitute(outcome_var))
  date_var_quo <- deparse(substitute(date_var))
  temp_var_quo <- deparse(substitute(temp_var))
  
  # Set plan for parallel computing
  plan(multisession, workers = 10)
  
  list_data_simulated_neg_binomial <- future_map(list_data_location_specific, 
                                                 .options = furrr_options(seed = TRUE), 
                                                 function(x){
    
    # Set ns function as global
    ns
    
    # Estimate negative binomial regression model and extract output
    model <- estimate_neg_binomial_model(data = x,
                                         outcome_var_quo,
                                         date_var_quo,
                                         linear_predictors,
                                         week_var_quo,
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
  
  # Reset plan to sequential
  plan(sequential)

  # Generate final list of data frames by recombining location-specific data frames
  list_data_transposed <- map(transpose(list_data_simulated_neg_binomial), bind_rows)
  
  # For each data frame in list, sample treated unit and time using empirical treatment probabilities
  # and subset data around the treatment time
  list_data_out <- lapply(list_data_transposed, function(x) {

    # Sample treated unit and treated time
    treated_unit <- sample(unit_time_treatment_probs[["unit_names"]], 1, prob = unit_time_treatment_probs[["treatment_prob_unit"]])
    treated_time <- sample(unit_time_treatment_probs[["week_ids"]], 1, prob = unit_time_treatment_probs[["treatment_prob_time"]])

    # Subset data based on sampled treatment time and generate treatment assignment
    # indicator for treated unit, then remove units with missing time periods
    data_subset <- x %>%
      filter(
        between(
          {{week_var}}, treated_time - 26, treated_time + 25
          )
      ) %>%
      mutate(
        treated = ifelse({{id_var}} == treated_unit, 1, 0),
        post = ifelse({{week_var}} >= treated_time, 1, 0)
      )
    }
  )
  
  return(list_data_out)
  
}