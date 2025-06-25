# Name of script: objective_function_difp
# Description: Function to find optimal synthetic control predictions using the
# elastic net penalty proposed by Doudchenko and Imbens. The hyperparameters are
# selected via a modified cross validation procedure where each control unit is 
# used as a pseudo-treated unit, an optimal synthetic control is found for each pseudo-control,
# and the MSPE is evaluated in the post-treatment period. The hyperparameter values
# are the values which minimise the average MSPE across all pseudo-treated units and
# post-treatment periods
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 25-06-2025

# Comments ---------------------------------------------------------------------

# This function finds optimal parameters for the synthetic control estimator
# in the style of Doudchenko and Imbens (2017). In particular, we consider only
# pre-treatment outcomes Y as our predictors, and allow for a constant level shift
# (intercept) between treated and synthetic control

# @ param data, an input dataframe
# @ param alpha, initial value for hyperparameter alpha
# @ param lambda, initial value for hyperparameter lambda
# @ param outcome_var, name of the outcome variable column
# @ param treated_id_var, binary ID variable for the treated unit
# @ param treated_time_var, binary ID variable for the treatment time
# @ return a list with elements data, Y1, Y1_hat, W_opt, mu_opt (equal to 0), alpha_opt, lambda_opt, first_treated_period

# We assume that the optimx function is performing minimisation, not maximisation

# Define function to manually find optimal synth using optimx ------------------

objective_function_difp <- function(Y_treated_pre,
                                    Y_controls_pre,
                                    Y_controls_post,
                                    alpha_init,
                                    lambda_init){
  
  # Initialise starting values for hyperparameters
  alpha_init <- setNames(alpha_init, "alpha")
  lambda_init <- setNames(lambda_init, "lambda")
  
  hyper_params <- c(alpha_init,
           lambda_init)
  
  # Optimise hyperparameters for elastic net objective function using pseudo-treated units
  results_hyperparams <- optimx(hyper_params,
                                get_hyperparam_loss_elastic_net,
                                lower = c(0, 0.1), # Set minimum lambda for stability
                                upper = c(1, Inf),
                                Y_controls_pre = Y_controls_pre,
                                Y_controls_post = Y_controls_post,
                                method = "L-BFGS-B") # Could maybe try a cv.glmnet or grid search here
  
  # Re-order results to find best solution (assume using minimisation in optimx)
  results_hyperparams <- results_hyperparams[order(results_hyperparams$value,decreasing=FALSE),]
  
  # Retain optimal solution (first row in re-ordered results_opt)
  results_hyperparams <- results_hyperparams[1,]
  
  # Extract optimal hyperparameters (if NA, return initial hyperparams)
  alpha_opt <- ifelse(!is.na(results_hyperparams[["alpha"]]), as.numeric(results_hyperparams[["alpha"]]), alpha_init)
  lambda_opt <- ifelse(!is.na(results_hyperparams[["lambda"]]), as.numeric(results_hyperparams[["lambda"]]), lambda_init)
  
  # Re-run optimisation to get optimal synthetic control weights for treated unit -------------
  # at the optimal level of the hyperparameters
  
  # Run glm model at optimal parameters on the true treated unit during the pre-treatment period
  fit <- glmnet(Y_controls_pre, 
                Y_treated_pre, 
                alpha = alpha_opt,
                lambda = lambda_opt,
                nlambda = 1)
  
  # Extract optimal model coefficients
  W_opt <- fit[["beta"]]
  mu_opt <- fit[["a0"]]
  
  # Return list of final outputs
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "alpha_opt" = alpha_opt,
                  "lambda_opt" = lambda_opt)
  
  return(results)
} 