# Name of script: extract_tau_hat_synth_results
# Description: Function to extract true vs. predicted tau hats from synth model replications
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 05-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 05-03-2025

# Comments ---------------------------------------------------------------------


# Define function to simulate n_sims runs of synth model -----------------------

extract_tau_hat_synth_results <- function(results_synth_model_simulated,
                                          id_var,
                                          time_var){
  
  # Vector of model runs to iterate over
  model_run <- seq(1:length(results_synth_model_simulated))
  
  list_tau_hat <- map(model_run, function(x){
    
    # Get max t from dataprep object
    t_max <- length(results_synth_model_simulated[[x]][["data_prepared"]][["Y1plot"]])
    
    # Extract true 'y_natural' and 'y_obs' vector from simulated data (treated unit is unit with smallest ID)
    y_natural <- results_synth_model_simulated[[x]][["data"]] %>% filter({{id_var}} == min({{id_var}}) & {{time_var}} <= t_max) %>% pull(y_natural)
    y_obs <- results_synth_model_simulated[[x]][["data"]] %>% filter({{id_var}} == min({{id_var}}) & {{time_var}} <= t_max) %>% pull(y)
    
    # Extract optimal weights and do matrix/vector multiplication with control outcomes
    # to get synthetic control predicted y_natural_pred
    w_opt <- results_synth_model_simulated[[x]][["synth_out"]][["solution.w"]]
    y_controls <- results_synth_model_simulated[[x]][["data_prepared"]][["Y0plot"]]
    y_pred <- c(y_controls %*% w_opt)
    
    # Estimate true vs predicted treatment effects
    tau <- y_obs - y_natural
    tau_hat <- y_obs - y_pred
    
    # Get difference between tau and tau_hat
    diff_tau_hat <- tau_hat - tau
    
    # Generate dataframe of results
    results <- data.frame(tau,
                          tau_hat,
                          diff_tau_hat,
                          model = rep(x, t_max),
                          t = seq(1:t_max))
    
  })
  
  # Bind rows into one dataframe and return
  results_synth_tau_hat <- bind_rows(list_tau_hat)
  
  return(results_synth_tau_hat)
}