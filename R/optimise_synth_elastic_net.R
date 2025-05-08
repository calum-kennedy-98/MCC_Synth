# Name of script: optimise_synth_elastic_net
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
# Latest update on: 13-03-2025

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

optimise_synth_elastic_net <- function(data,
                                  alpha_init,
                                  lambda_init,
                                  outcome_var,
                                  time_var,
                                  treated_id_var,
                                  treated_time_var){
  
  # Extract vector of outcomes for treated unit
  Y1 <- data %>% 
    filter({{treated_id_var}} == 1) %>%
    pull({{outcome_var}}) %>% 
    as.numeric()
  
  # Extract pre-treatment vector of outcomes for treated unit
  Y1_pre <- data %>% 
    filter({{treated_id_var}} == 1 &
             {{treated_time_var}} == 0) %>%
    pull({{outcome_var}}) %>% 
    as.numeric()
  
  # Extract dimension of time variable to reshape Y0
  n_periods <- length(Y1)
  n_periods_pre <- length(Y1_pre)
  
  # Generate vector of pre-treatment time periods - we need to extract the time
  # vector only for one unit, since the panel is balanced by design
  t_vec_pre_treatment <- data %>%
    filter(
      {{treated_time_var}} == 0,
      {{treated_id_var}} == 1
    ) %>%
    pull({{time_var}})
  
  # Generate indicator for first treatment period
  first_treated_period <- max(t_vec_pre_treatment) + 1
  
  # Extract matrix of outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y0 <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull({{outcome_var}}) %>%
    matrix(nrow = n_periods)
  
  # Extract matrix of control outcomes in pre-treatment period
  Y0_pre <- Y0[1:n_periods_pre,]
  
  # Extract post-treatment vector of treated outcomes Y1_post and matrix of untreated outcomes Y0_post
  Y1_post <- Y1[(n_periods_pre + 1):n_periods]
  Y0_post <- Y0[(n_periods_pre + 1):n_periods,]
  
  # Initialise starting values for hyperparameters
  alpha_init <- setNames(alpha_init, "alpha")
  lambda_init <- setNames(lambda_init, "lambda")
  
  hyper_params <- c(alpha_init,
           lambda_init)
  
  # Optimise hyperparameters for elastic net objective function using pseudo-treated units
  results_hyperparams <- optimx(hyper_params,
                                get_hyperparam_loss_elastic_net,
                                lower = c(0, 0),
                                upper = c(1, Inf),
                                Y0_pre = Y0_pre,
                                Y0_post = Y0_post,
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
  fit <- glmnet(Y0_pre, 
                Y1_pre, 
                alpha = alpha_opt,
                lambda = lambda_opt,
                nlambda = 1)
  
  # Get predictions on outcomes over the entire pre/post period
  Y1_hat <- predict(fit, Y0, type = "response")
  
  # Extract optimal model coefficients
  W_opt <- fit[["beta"]]
  mu_opt <- fit[["a0"]]
  
  # Return list of final outputs
  results <- list("data" = data,
                  "Y1" = Y1,
                  "Y1_hat" = Y1_hat,
                  "W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "alpha_opt" = alpha_opt,
                  "lambda_opt" = lambda_opt,
                  "first_treated_period" =  first_treated_period)
  
  return(results)
} 