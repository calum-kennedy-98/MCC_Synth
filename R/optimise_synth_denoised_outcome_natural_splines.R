# Name of script: optimise_synth_denoised_outcome_natural_splines
# Description: FUnction which estimates optimal SC weights (using preferred method)
# on outcome series denoised using natural splines
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

# @ param ...

optimise_synth_denoised_outcome_natural_splines <- function(data,
                                                            unit_id_var,
                                                            time_id_var,
                                                            outcome_var,
                                                            spline_df,
                                                            lambda_init,
                                                            lower_bound_lambda,
                                                            treated_id_var,
                                                            treated_time_var,
                                                            n_periods_pre,
                                                            n_periods_post){
  
  # Set outcome_var and time_id_var to quoted characters
  outcome_var_quo <- as_name(ensym(outcome_var))
  time_id_var_quo <- as_name(ensym(time_id_var))
  
  # Extract unit ID vars
  unit_ids <- pull(distinct(data, {{unit_id_var}}))
  
  # Convert function arguments to formula to pass to GLM
  formula <- as.formula(paste(outcome_var_quo, "~",
                          paste("ns(", time_id_var_quo, ", df = ", spline_df,")")))
  
  # Generate augmented dataset for each unique location by estimating natural 
  # spline model on outcome series for each unique location
  list_data_natural_spline <- map(unit_ids, function(x){
    
    ns
    
    # Extract location-specific data
    data_location_specific <- filter(data, {{unit_id_var}} == x)
    
    # Estimate natural spline model on location-specific data
    fit <- glm(formula, family = "quasipoisson", data = data_location_specific)
    
    # Get predicted outcome from natural spline model
    outcome_denoised <- predict(fit, data_location_specific, type = "response")
    
    data_location_specific$outcome_denoised <- outcome_denoised
    
    return(data_location_specific)
    
  })
  
  # Bind rows to generate augmented data to pass to synth
  data_for_synth <- bind_rows(list_data_natural_spline)
  
  # Get optimal SC weights (currently using PSC)
  # Extract dimension of time variable
  n_periods <- n_periods_pre + n_periods_post
  
  # Generate indicator for post-treatment period
  post <- c(rep(0, n_periods_pre), rep(1, n_periods_post))
  
  # Extract first treated period and subset data to pre- / post-treatment intervals 
  # based on `n_periods_pre` and `n_periods_post`
  first_treated_period <- min(data %>%
                                filter({{treated_time_var}} == 1) %>%
                                pull({{time_id_var}})
  )
  
  data <- data_for_synth %>%
    filter(
      between(
        {{time_id_var}}, 
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
  
  # Extract matrix of true outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y_controls <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull({{outcome_var}}) %>%
    matrix(nrow = n_periods)
  
  # Extract matrix of denoised outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y_controls_denoised <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull(outcome_denoised) %>%
    matrix(nrow = n_periods)
  
  # Extract matrix of denoised control outcomes in pre- and post-treatment period
  Y_controls_denoised_pre <- Y_controls_denoised[1:n_periods_pre,]
  Y_controls_denoised_post <- Y_controls_denoised[(n_periods_pre + 1):n_periods,]
  
  # Initialise starting value for lambda
  par <- c("lambda" = lambda_init)
  
  # Optimise lambda for penalised SC objective function using pseudo-treated units
  results_lambda <- optimx(par,
                           get_hyperparam_loss_penalised_sc,
                           lower = lower_bound_lambda,
                           upper = Inf,
                           Y_controls_pre = Y_controls_denoised_pre,
                           Y_controls_post = Y_controls_denoised_post,
                           method = "L-BFGS-B") # Could maybe try a cv.glmnet or grid search here
  
  # Re-order results to find best solution (assume using minimisation in optimx)
  results_lambda <- results_lambda[order(results_lambda$value,decreasing=FALSE),]
  
  # Retain optimal solution (first row in re-ordered results_opt)
  results_lambda <- results_lambda[1,]
  
  # Extract optimal hyperparameters (if NA, return initial hyperparams)
  lambda_opt <- ifelse(!is.na(results_lambda[["lambda"]]), as.numeric(results_lambda[["lambda"]]), lambda_init)
  
  # Re-run optimisation to get optimal synthetic control weights for treated unit -------------
  # at the optimal level of the hyperparameters
  
  # Run PSC algorithm at optimal parameters on the true treated unit during the pre-treatment period,
  # using the de-noised outcome series for the control units
  W_opt <- get_penalised_sc_weights(Y_treated_pre,
                                    Y_controls_denoised_pre,
                                    lambda_opt)
  
  # Get predictions on outcomes over the entire pre/post period using the optimal weights
  # on the de-noised outcome series and the true matrix of outcomes for the control units
  Y0_treated_hat <- c(Y_controls_denoised %*% W_opt)
  
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
                  "method" = "penalised_sc_denoised")
  
  return(results)
  
}