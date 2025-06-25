# Name of script: objective_function_psc
# Description: Function to find optimal penalised synthetic control using the
# penalised SC method proposed by Abadie and L'Hour (2021). We first optimise the
# L1 penalty parameter lambda using leave-one-out cross-validation on predictions for
# the control units in the post-treatment period
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 02-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 02-06-2025

# Comments ---------------------------------------------------------------------

# At present, not using weight vector V - in future could look to incorporate

# @ param ...

# Function ---------------------------------------------------------------------

objective_function_psc <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   Y_controls_post,
                                   lambda_init,
                                   lower_bound_lambda){
  
  # Initialise starting value for lambda
  par <- c("lambda" = lambda_init)
  
  # Optimise lambda for penalised SC objective function using pseudo-treated units
  results_lambda <- optimx(par,
                           get_hyperparam_loss_penalised_sc,
                           lower = lower_bound_lambda,
                           upper = Inf,
                           Y_controls_pre = Y_controls_pre,
                           Y_controls_post = Y_controls_post,
                           method = "L-BFGS-B") # Could maybe try a cv.glmnet or grid search here
  
  # Re-order results to find best solution (assume using minimisation in optimx)
  results_lambda <- results_lambda[order(results_lambda$value,decreasing=FALSE),]
  
  # Retain optimal solution (first row in re-ordered results_opt)
  results_lambda <- results_lambda[1,]
  
  # Extract optimal hyperparameters (if NA, return initial hyperparams)
  lambda_opt <- ifelse(!is.na(results_lambda[["lambda"]]), as.numeric(results_lambda[["lambda"]]), lambda_init)
  
  # Re-run optimisation to get optimal synthetic control weights for treated unit -------------
  # at the optimal level of the hyperparameters
  
  # Run PSC algorithm at optimal parameters on the true treated unit during the pre-treatment period
  W_opt <- get_penalised_sc_weights(Y_treated_pre,
                                    Y_controls_pre,
                                    lambda_opt)
  
  # Set mu_opt (equal to 0 by design)
  mu_opt <- 0
  
  # Return list of final outputs
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "lambda_opt" = lambda_opt)
  
  return(results)
  
}