# Name of script: objective_function_did
# Description: Function to find optimal counterfactual predictions using 
# difference-in-differences
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 07-07-2025
# Latest update by: Calum Kennedy
# Latest update on: 07-07-2025

# Comments ---------------------------------------------------------------------

# @ param ...

# Function ---------------------------------------------------------------------

objective_function_did <- function(Y_treated_pre,
                                   Y_controls_pre){
  
  # Estimate optimal mu (average difference between pre-treatment outcomes
  # for average of control units and the treated unit)
  mean_Y_controls_pre <- rowMeans(Y_controls_pre)
  mu_opt <- mean(Y_treated_pre - mean_Y_controls_pre)
  
  # Estimate optimal weights as 1 divided by number of control units (DID sets
  # equal weights by design)
  W_opt <- rep(1 / ncol(Y_controls_pre), ncol(Y_controls_pre))
  
  # Store results in list
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt)
  
  return(results)
  
}