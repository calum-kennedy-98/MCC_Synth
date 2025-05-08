# Name of script: extract_tau_hat_synth_results
# Description: Function to extract estimated tau hats from a list of synthetic control
# model outputs. The function takes inputs from a generic synthetic control model
# class, assuming that the true observed outcome is "Y1", the predicted outcome
# is "Y1_hat" and the first treated period is "first_treated_period"
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 05-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 05-03-2025

# Comments ---------------------------------------------------------------------

# This function returns estimated period tau hats for a list of outputs from a
# fitted synthetic control model. The true observed outcome is assumed to be "Y1",
# the predicted outcome is "Y1_hat". The function returns a dataframe with four
# columns: the model run indicator, time period, post-treatment indicator, and estimated
# tau_hat

# Function ---------------------------------------------------------------------

extract_tau_hat_synth_results <- function(results_synth_model_simulated,
                                          time_var){
  
  # Generate ID for model run and add to list of outputs
  model_run <- seq(1:length(results_synth_model_simulated))
  
  results_synth_model_simulated_with_id <- lapply(seq_along(results_synth_model_simulated),
                                                  function(x) {
                                                    c(results_synth_model_simulated[[x]], model_run = x)
                                                  })
  
  # Get list of estimated period-specific tau hats from each model run
  list_tau_hat <- lapply(results_synth_model_simulated_with_id, function(x){
    
    # Extract post-treatment true and predicted outcome
    Y1 <- c(x[["Y1"]])
    Y1_hat <- c(x[["Y1_hat"]])
    
    # Estimate tau_hat
    tau_hat <- Y1 - Y1_hat
    
    # Generate vector of time periods
    t <- seq(1:length(Y1))
    
    # Generate post-treatment indicator
    post <- c(rep(0, x[["first_treated_period"]] - min(x[["data"]][[time_var]])), rep(1, max(x[["data"]][[time_var]]) - x[["first_treated_period"]] + 1))
    
    # Return tibble of tau_hat, t_post, and model run
    results <- tibble("model_run" = x[["model_run"]],
                      "t" = t,
                      "post" = post,
                      "tau_hat" = tau_hat)
    
  }) 
  
  # Bind rows into one dataframe and return
  results_synth_tau_hat <- bind_rows(list_tau_hat)
  
  return(results_synth_tau_hat)
}