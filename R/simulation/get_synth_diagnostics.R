# Name of script: get_synth_diagnostics
# Description: Wrapper function to get diagnostics from a list of synth model outputs
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 10-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 10-03-2025

# Comments ---------------------------------------------------------------------

# Calls the 'extract_tau_hat_synth_results.R' function, which returns the true
# and estimated tau from the simulated data and the synth model, for each draw
# of the data. Then calls the 'get_rmse_synth.R' function, which returns a list
# of the RMSE over a specified period for the synth estimated counterfactual
# against the true outcome. Normally, this will be evaluated over the pre-treatment
# period

# Define wrapper function ------------------------------------------------------

get_synth_diagnostics <- function(results_synth_model_simulated,
                                  id_var,
                                  time_var,
                                  start_index,
                                  end_index){
  
  # Get estimated vs. true tau hat
  results_synth_tau_hat <- extract_tau_hat_synth_results(results_synth_model_simulated,
                                                         {{id_var}},
                                                         {{time_var}})
  
  
  # Get RMSE over specified period
  results_rmse <- get_rmse_synth(results_synth_model_simulated,
                                 {{id_var}},
                                 start_index,
                                 end_index)
  
  # Store final output in list
  list_synth_diagnostics <- list(results_synth_tau_hat = results_synth_tau_hat,
                                 results_rmse = results_rmse)
  
  return(list_synth_diagnostics)
  
}