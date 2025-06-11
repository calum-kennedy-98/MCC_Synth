# Name of script: optimise_synth_penalised_sc
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

optimise_synth_penalised_sc <- function(data,
                                        lambda_init,
                                        lower_bound_lambda,
                                        outcome_var,
                                        time_var,
                                        treated_id_var,
                                        treated_time_var,
                                        n_periods_pre,
                                        n_periods_post){
  
  # Extract dimension of time variable
  n_periods <- n_periods_pre + n_periods_post
  
  # Generate indicator for post-treatment period
  post <- c(rep(0, n_periods_pre), rep(1, n_periods_post))
  
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
  
  # Extract vector of observed outcomes for treated unit
  Y_treated <- data %>% 
    filter({{treated_id_var}} == 1) %>%
    pull({{outcome_var}}) %>% 
    as.numeric()
  
  # Extract pre- and post-treatment vector of outcomes for treated unit
  Y_treated_pre <- Y_treated[1:n_periods_pre]
  Y_treated_post <- Y_treated[(n_periods_pre + 1):n_periods]
  
  # Extract matrix of outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y_controls <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull({{outcome_var}}) %>%
    matrix(nrow = n_periods)
  
  # Extract matrix of control outcomes in pre- and post-treatment period
  Y_controls_pre <- Y_controls[1:n_periods_pre,]
  Y_controls_post <- Y_controls[(n_periods_pre + 1):n_periods,]
  
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
  
  # Get predictions on outcomes over the entire pre/post period
  Y0_treated_hat <- c(Y_controls %*% W_opt)
  
  # Set mu_opt (equal to 0 by design)
  mu_opt <- 0
  
  # Return list of final outputs
  results <- list("Y_treated" = Y_treated,
                  "Y0_treated_hat" = Y0_treated_hat,
                  "post" = post,
                  "W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "lambda_opt" = lambda_opt,
                  "first_treated_period" =  first_treated_period,
                  "method" = "penalised_sc")
  
  return(results)
  
}