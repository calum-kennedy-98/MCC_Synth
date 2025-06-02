# Name of script: optimise_synth_elastic_net_ensemble
# Description: Function to find optimal synthetic control predictions using an
# ensemble of individual synthetic controls estimated using random subsets of the
# control units. To generate each synthetic control, we use the elastic net method
# proposed by Doudchenko and Imbens (2017).
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 13-03-2025

# Comments ---------------------------------------------------------------------

# We first estimate optimal elastic net hyperparameters (alpha, lambda). Then,
# estimate n_synth synthetic controls using random subsets of the control units.
# We take a weighted average of these synthetic controls, weighted by the inverse
# of each synthetic control's average prediction error in the pre-treatment period.
# For each synthetic control, we choose N/3 randomly selected control units.

# @ param data, an input dataframe
# @ param n_synth, number of synthetic controls to build
# @ param alpha, initial value for hyperparameter alpha
# @ param lambda, initial value for hyperparameter lambda
# @ param outcome_var, name of the outcome variable column
# @ param treated_id_var, binary ID variable for the treated unit
# @ param treated_time_var, binary ID variable for the treatment time
# @ return a list with elements data, Y1, Y1_hat, W_opt, mu_opt (equal to 0), alpha_opt, lambda_opt, first_treated_period

# Define function find optimal synth -------------------------------------------

optimise_synth_elastic_net_ensemble <- function(data,
                                                n_synth,
                                                alpha_init,
                                                lambda_init,
                                                outcome_var,
                                                time_var,
                                                treated_id_var,
                                                treated_time_var,
                                                n_periods_pre,
                                                n_periods_post){
  
  # Extract first treated period and subset data to pre- / post-treatment intervals 
  # based on `n_periods_pre` and `n_periods_post`
  first_treated_period <- min(data %>%
                                filter({{treated_time_var}} == 1) %>%
                                pull({{time_var}})
  )
  
  data <- data %>%
    filter(
      between(
        {{time_var}}, 
        first_treated_period - n_periods_pre, 
        first_treated_period + n_periods_post - 1
      )
    )
  
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
  
  # Find 'n_synth' synthetic controls on random subsets of the control units of size ((n_controls)/3)
  n_controls <- ncol(Y0)
  n_controls_to_subset <- floor(n_controls/3)
  
  # Return list of matrices of random subsets of control units
  # Check if need to set seed somewhere for reproducibility
  list_Y0_subset <- replicate(
    n = n_synth,
    expr = {
      subset_controls <- sample(ncol(Y0), n_controls_to_subset, replace = FALSE)
      Y0[, subset_controls, drop = FALSE]
    },
    simplify = FALSE
  )
  
  # Estimate optimal synthetic control weights on subset of controls using elastic net
  list_fit_elastic_net <- lapply(list_Y0_subset, function(x){
    
    # Extract Y0_pre
    Y0_pre <- x[1:n_periods_pre,]
    
    # Optimise elastic net via glmnet
    fit <- glmnet(x = Y0_pre,
                  y = Y1_pre,
                  alpha = alpha_opt,
                  lambda = lambda_opt,
                  nlambda = 1)
    
    # Get predictions over entire pre/post-treatment period
    Y1_hat <- predict(fit, x, type = "response")
    
    # Get MSPE in pre-treatment period
    Y1_hat_pre <- Y1_hat[1:n_periods_pre]
    
    MSPE_Y1_hat_pre <- mean((Y1_pre - Y1_hat_pre)^2)
    
    # Extract optimal model coefficients
    #W_opt <- fit[["beta"]]
    #mu_opt <- fit[["a0"]]
    
    # Return outputs
    return(list("Y1_hat" = Y1_hat,
                "MSPE_Y1_hat_pre" = MSPE_Y1_hat_pre))
    
  })
  
  # Store matrix of predicted Y1_hat from each synthetic control run
  mat_Y1_hat <- do.call(cbind, lapply(list_fit_elastic_net, function(x) x$Y1_hat))
  
  # Generate vector of MSPE of each synthetic control in pre-treatment period
  vec_mspe <- sapply(list_fit_elastic_net, function(x) x$MSPE_Y1_hat_pre)
  
  # Take inverse to use as weights (upweight synthetic controls with lower MSPE)
  vec_prediction_weights <- 1 / vec_mspe
  
  # Normalise weights to sum to 1
  vec_prediction_weights_normalised <- vec_prediction_weights / sum(vec_prediction_weights)
  
  # Return optimal weighted Y1_hat 
  Y1_hat <- mat_Y1_hat %*% vec_prediction_weights_normalised
  
  # Return results
  results <- list("data" = data,
                  "Y1" = Y1,
                  "Y1_hat" = Y1_hat,
                  "prediction_weights" = vec_prediction_weights_normalised)
  
  return(results)
  
}