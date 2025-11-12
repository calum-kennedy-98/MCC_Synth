# Name of script: objective_function_1NN_matching
# Description: Function to find optimal counterfactual predictions using 1-nearest neighbour
# matching. Nearest neighbour selected to minimise L2 norm between treated unit and 
# control in pre-treatment period
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 01-05-2025
# Latest update by: Calum Kennedy
# Latest update on: 25-06-2025

# Comments ---------------------------------------------------------------------

# @ param ...

# Function ---------------------------------------------------------------------

objective_function_1NN_matching <- function(Y_treated_pre,
                                          Y_controls_pre,
                                          n_controls,
                                          initial_margin,
                                          max_attempts,
                                          margin_increment){
  
  # Determine nearest neighbour control using L2 norm
  l2_norms <- colSums((Y_controls_pre - Y_treated_pre)^2)
  idx <- order(l2_norms)[1]
  
  # Select optimal weight vector (weight = 1 for nearest neighbour, 0 otherwise)
  W_opt <- rep(0, ncol(Y_controls_pre))
  W_opt[idx] <- 1
  
  # Set mu = 0
  mu_opt <- 0
  
  # Store results in list
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt)
  
  return(results)
  
}