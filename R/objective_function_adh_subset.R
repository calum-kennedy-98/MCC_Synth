# Name of script: objective_function_adh_subset
# Description: Function to find optimal synthetic control predictions using the
# classical synthetic control method proposed by Abadie, Diamond, and Hainmueller (ADH) with
# pre-optimisation subset selection to reduce the size of the donor pool.
# The ADH synthetic control finds a convex combination of control units which best approximates
# the pre-treatment trend in the treated units. Here, we do not allow an intercept term
# and we constrain the weights to lie in the unit interval and sum to 1. The synthetic control
# unit is therefore a projection of the treated unit outcomes onto the convex hull of the control
# unit outcomes. The subset of controls is determined by taking the 'n_controls' units with the smallest
# L2 norm with respect to the treated unit in the pre-treatment period.
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 01-05-2025
# Latest update by: Calum Kennedy
# Latest update on: 25-06-2025

# Comments ---------------------------------------------------------------------

# @ param ...

# Function ---------------------------------------------------------------------

objective_function_adh_subset <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   n_controls,
                                   initial_margin,
                                   max_attempts,
                                   margin_increment){
  
  # Determine best subset of controls to use by taking L2 norm
  l2_norms <- colSums((Y_controls_pre - Y_treated_pre)^2)
  idx <- order(l2_norms)[1:n_controls]
  
  # Define relevant matrices for synth command
  X1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  X0 <- Y_controls_pre[, idx, drop = FALSE]
  Z1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  Z0 <- Y_controls_pre[, idx, drop = FALSE]
  
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
    
    # Extract optimal weights, intercept, and weight matrix V (mu = 0 by design) - include zero weight for all
    # units not used to construct the SC
    W_opt <- rep(0, ncol(Y_controls_pre))
    W_opt[idx] <- synth_out[["solution.w"]]
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