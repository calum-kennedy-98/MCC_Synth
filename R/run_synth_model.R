# Name of script: run_synth_model
# Description: Runs synthetic control model on specified dataset and returns 'synth' object
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Comments ---------------------------------------------------------------------


# Define function to run synth model -------------------------------------------

run_synth_model <- function(data,
                            special_predictors,
                            time_predictors_prior,
                            dep_var,
                            unit_var,
                            time_var,
                            treated_id,
                            controls_id,
                            time_optimise_ssr,
                            time_plot){
  
  # Generate 'dataprep.out' object from 'Synth' package
  data_prepared <- dataprep(foo = data,
                            special.predictors = special_predictors,
                            time.predictors.prior = time_predictors_prior,
                            dependent = dep_var,
                            unit.variable = unit_var,
                            time.variable = time_var,
                            treatment.identifier = treated_id,
                            controls.identifier = controls_id,
                            time.optimize.ssr = time_optimise_ssr,
                            time.plot = time_plot)
  
  # Generate synth object
  synth_out <- synth(data_prepared, method = "BFGS")
  
  # Return list of data_prepared and synth_out
  synth_output <- list(data_prepared = data_prepared,
                       synth_out = synth_out)
  
  return(synth_output)
}