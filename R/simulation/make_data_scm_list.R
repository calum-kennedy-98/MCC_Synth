# Name of script: make_data_scm_list
# Description: Runs 'sim_data_scm' script 'n_iters' times to generate list of dataframes
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Define function to generate simulated dataset of covariates ------------------

make_data_scm_list <- function(n_sims,
                               data,
                               id_var,
                               time_var_mcc,
                               seed = x,
                               exposure_start_time,
                               exposure_end_time,
                               exposure_amplitude,
                               exposure_gamma){
  
  # Generate list to store output of iterations
  iters <- c(1:n_sims)
  
  # Simulate data from different seed, run synth model, and return output for each iteration
  list_data_simulated <- map(iters, function(x){
    
    # Sim data from the structural causal model
    data <- sim_data_scm(data,
                         {{id_var}},
                         {{time_var_mcc}},
                         seed = x,
                         exposure_start_time,
                         exposure_end_time,
                         exposure_amplitude,
                         exposure_gamma)
    
  })
  return(list_data_simulated)
}