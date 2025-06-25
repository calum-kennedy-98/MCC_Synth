# Name of script: objective_function_adh
# Description: Function to find optimal synthetic control predictions using the
# classical synthetic control method proposed by Abadie, Diamond, and Hainmueller (ADH)
# The ADH synthetic control finds a convex combination of control units which best approximates
# the pre-treatment trend in the treated units. Here, we do not allow an intercept term
# and we constrain the weights to lie in the unit interval and sum to 1. The synthetic control
# unit is therefore a projection of the treated unit outcomes onto the convex hull of the control
# unit outcomes. 
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 01-05-2025
# Latest update by: Calum Kennedy
# Latest update on: 25-06-2025

# Comments ---------------------------------------------------------------------

# @ param data, an input dataframe
# @ param id_var, unit ID column
# @ param outcome_var, name of the outcome variable column
# @ param time_var, column name for time variable
# @ param treated_id_var, binary ID variable for the treated unit
# @ param treated_time_var, binary ID variable for the treatment time
# @ param predictors, vector of quoted variables to use as predictors in synth
# @ return a list with elements data, Y1, Y1_hat, W_opt, mu_opt (equal to 0), first_treated_period

# Function ---------------------------------------------------------------------

objective_function_adh <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   initial_margin,
                                   max_attempts,
                                   margin_increment){
  
  # Define relevant matrices for synth command
  X1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  X0 <- Y_controls_pre
  Z1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  Z0 <- Y_controls_pre
  
  # Generate synth object using 'retry_synth' - available in 'utility_functions.R'
  synth_out <- retry_synth(X1 = X1,
                           X0 = X0,
                           Z1 = Z1,
                           Z0 = Z0,
                           initial_margin = initial_margin,
                           max_attempts = max_attempts,
                           margin_increment = margin_increment)
  
  # If optimisation succeeded, generate relevant outputs
  if(!is.null(synth_out)){
  
  # Extract optimal weights, intercept, and weight matrix V (mu = 0 by design)
  W_opt <- synth_out[["solution.w"]]
  mu_opt <- 0
  
  # Rescale weights to remove very small weights (symptom of quadratic programming algos)
  W_opt <- rescale_small_weights(W_opt,
                                 tolerance = 1e-6,
                                 scale = TRUE)
  
  # If optimisation failed, return NA for missing outputs
  } else {
    W_opt <- NA
    mu_opt <- 0
  }
  
  # Store results in list
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt)
  
  return(results)
  
}