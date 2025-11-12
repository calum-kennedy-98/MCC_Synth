# Name of script: objective_function_did
# Description: Function to find optimal counterfactual predictions using 
# difference-in-differences. As for the ADH_subset method, we choose a set of 'n_controls'
# control units which are closest (in terms of L2 norm) to the treated unit in the
# pre-treatment period
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 07-07-2025
# Latest update by: Calum Kennedy
# Latest update on: 07-07-2025

# Comments ---------------------------------------------------------------------

# @ param ...

# Function ---------------------------------------------------------------------

objective_function_did <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   n_controls){
  
  # Determine best subset of controls to use by taking L2 norm
  l2_norms <- colSums((Y_controls_pre - Y_treated_pre)^2)
  idx <- order(l2_norms)[1:n_controls]
  Y_controls_pre_subset <- Y_controls_pre[, idx, drop = FALSE]
  
  # Estimate optimal mu (average difference between pre-treatment outcomes
  # for average of control units and the treated unit)
  mean_Y_controls_pre <- rowMeans(Y_controls_pre_subset)
  mu_opt <- mean(Y_treated_pre - mean_Y_controls_pre)
  
  # Estimate optimal weights as 1 divided by number of control units (DID sets
  # equal weights by design) for units in the subset, and zero otherwise
  W_opt <- rep(0, ncol(Y_controls_pre))
  W_opt[idx] <- 1 / n_controls
  
  # Store results in list
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt)
  
  return(results)
  
}