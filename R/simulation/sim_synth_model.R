# Name of script: sim_synth_model
# Description: Runs synthetic control model on n simulated datasets and returns desired outputd
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Comments ---------------------------------------------------------------------


# Define function to run synth model -------------------------------------------

sim_synth_model <- function(n_sims,
                            data,
                            id_var,
                            week_var,
                            time_var_mcc,
                            exposure_start_time,
                            exposure_end_time,
                            exposure_amplitude,
                            exposure_gamma,
                            special_predictors,
                            time_predictors_prior,
                            dep_var,
                            time_var_synth,
                            time_optimise_ssr,
                            time_plot){ # Here we might want to specify a type of output to access e.g. RMSE
  
  # Extract id var in quotes to pass to synth command
  id_var_synth <- deparse(substitute(id_var))
  
  # Extract identifiers for treated unit (smallest id number) and controls (all other units by default)
  id_vec <- distinct(data, {{id_var}}) %>% deframe()
  treated_id <- min(id_vec)
  controls_id <- id_vec[! id_vec %in% treated_id]
  
  # Generate list to store output of iterations
  iters <- c(1:n_sims)
  
  # Simulate data from different seed, run synth model, and return output for each iteration
  output <- map(iters, function(x){
    
    # Sim data from the structural causal model
    data <- sim_data_scm(data,
                  {{id_var}},
                  {{time_var_mcc}},
                  seed = x,
                  exposure_start_time,
                  exposure_end_time,
                  exposure_amplitude,
                  exposure_gamma)
    
    # Aggregate data to weekly level to pass to synth
    data_weekly <- data %>% summarise(y = mean(y, na.rm = TRUE),
                                      e = mean(e, na.rm = TRUE),
                                      y_natural = mean(y_natural, na.rm = TRUE),
                                      e_natural = mean(e_natural, na.rm = TRUE),
                                      .by = c({{week_var}}, {{id_var}}))

    # Run synth command and get output
    synth_output <- run_synth_model(data_weekly,
                              special_predictors,
                              time_predictors_prior,
                              dep_var,
                              id_var_synth,
                              time_var_synth,
                              treated_id = treated_id,
                              controls_id = controls_id,
                              time_optimise_ssr,
                              time_plot)
    
  })
  
  return(output)
} 

test <- sim_synth_model(100, 
                        data_mcc_scm_imp, 
                        id, 
                        week, 
                        doy, 
                        180, 
                        240, 
                        30, 
                        10, 
                        list(list("y", 1:25, "mean")), 
                        1:25, 
                        "y", 
                        "week", 
                        1:25, 
                        1:50)