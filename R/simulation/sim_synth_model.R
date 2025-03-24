# Name of script: sim_synth_model
# Description: Runs synthetic control model on n simulated datasets and returns desired outputd
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Comments ---------------------------------------------------------------------


# Define function to simulate n_sims runs of synth model -----------------------

sim_synth_model <- function(list_data_simulated,
                            id_var,
                            predictors,
                            special_predictors,
                            time_predictors_prior,
                            dep_var,
                            time_var_synth,
                            time_optimise_ssr,
                            time_plot){
  
  # Extract unit.variable as character from id var
  unit_variable <- as.character(substitute(id_var))
  
  # For each element in the list of data frames, summarise to weekly level and apply synth model
  output <- map(list_data_simulated, function(data){
    
    # Extract identifiers for treated unit (smallest id number) and controls (all other units by default)
    id_vec <- distinct(data, {{id_var}}) %>% deframe()
    treated_id <- min(id_vec)
    controls_id <- id_vec[! id_vec %in% treated_id]
    
    # Run synth command and get output
    synth_output <- run_synth_model(data = data,
                                    predictors= predictors,
                                    special_predictors = special_predictors,
                                    time_predictors_prior = time_predictors_prior,
                                    dep_var = dep_var,
                                    unit_variable = unit_variable,
                                    time_variable = time_var_synth,
                                    treated_id = treated_id,
                                    controls_id = controls_id,
                                    time_optimise_ssr = time_optimise_ssr,
                                    time_plot = time_plot)
    
  })
  
  return(output)
} 