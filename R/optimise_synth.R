# Name of script: optimise_synth
# Description: Function to optimise synthetic control weights, given an input
# dataframe, data pre-processing steps, and objective function
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

# @ param ...

optimise_synth <- function(data,
                           demean_outcomes,
                           denoise_outcomes,
                           objective_function,
                           n_periods_pre,
                           n_periods_post,
                           outcome_var,
                           treated_id_var,
                           treated_time_var,
                           time_var,
                           spline_df = NULL){
  
  # Checks
  if(denoise_outcomes == TRUE & is.null(spline_df)) stop("If `denoise_outcomes' == TRUE, please specify a value for `spline_df'")
  if(!objective_function %in% c("ADH", "PSC", "DIFP", "DID")) stop("Please set `objective_function' to one of `ADH', `PSC', `DIFP', or 'DID'")
  
  # Prepare data for synth
  data_prepared <- prepare_data_for_synth(data,
                                          demean_outcomes,
                                          denoise_outcomes,
                                          n_periods_pre,
                                          n_periods_post,
                                          {{outcome_var}},
                                          {{treated_id_var}},
                                          {{treated_time_var}},
                                          {{time_var}},
                                          spline_df)
  
  # Extract necessary objects from data_prepared
  Y_treated <- data_prepared$Y_treated
  Y_controls <- data_prepared$Y_controls
  Y_treated_bar_pre_treatment <- data_prepared$Y_treated_bar_pre_treatment
  Y_controls_bar_pre_treatment <- data_prepared$Y_controls_bar_pre_treatment
  post <- data_prepared$post
  first_treated_period <- data_prepared$first_treated_period
  
  # If `demean_outcomes' is TRUE, demean the treated/control outcomes
  if(demean_outcomes == TRUE){
    Y_treated <- Y_treated - Y_treated_bar_pre_treatment
    Y_controls <- t(t(Y_controls) - Y_controls_bar_pre_treatment)
  }
  
  # If `denoise_outcomes` is TRUE, denoise the control unit outcomes to pass to the
  # optimisation routine (note that we want to retain the original outcomes to
  # generate the final counterfactual predictions)
  if(denoise_outcomes == TRUE){
    # Generate time vector to pass to spline denoising function
    t_vec <- 1:length(post)
    Y_controls_to_optimise <- apply(Y_controls, 2, function(x){
      fit <- glm(x ~ ns(t_vec, df = spline_df))
      predict(fit, type = "response")
    })
  } else if(denoise_outcomes == FALSE){
    Y_controls_to_optimise <- Y_controls
  }
  
  # Extract pre- and post-treatment vector of outcomes for treated unit after pre-processing
  Y_treated_pre <- Y_treated[1:n_periods_pre]
  Y_treated_post <- Y_treated[(n_periods_pre + 1):length(Y_treated)]
  
  # Extract matrix of control outcomes in pre- and post-treatment period after pre-processing
  Y_controls_to_optimise_pre <- Y_controls_to_optimise[1:n_periods_pre,]
  Y_controls_to_optimise_post <- Y_controls_to_optimise[(n_periods_pre + 1):length(Y_treated),]
  
  # Optimise synthetic control weights using specified objective function
  if(objective_function == "ADH"){
    results_optimisation <- objective_function_adh(Y_treated_pre = Y_treated_pre,
                                                   Y_controls_pre = Y_controls_to_optimise_pre,
                                                   initial_margin = 0.0005,
                                                   max_attempts = 20,
                                                   margin_increment = 0.0005)
  } else if(objective_function == "DIFP"){
    results_optimisation <- objective_function_difp(Y_treated_pre = Y_treated_pre,
                                                    Y_controls_pre = Y_controls_to_optimise_pre,
                                                    Y_controls_post = Y_controls_to_optimise_post,
                                                    alpha_init = 0.5,
                                                    lambda_init = 2)
  } else if(objective_function == "PSC"){
    results_optimisation <- objective_function_psc(Y_treated_pre = Y_treated_pre,
                                                   Y_controls_pre = Y_controls_to_optimise_pre,
                                                   Y_controls_post = Y_controls_to_optimise_post,
                                                   lambda_init = 1,
                                                   lower_bound_lambda = 1e-6)
  } else if(objective_function == "DID"){
    results_optimisation <- objective_function_did(Y_treated_pre = Y_treated_pre,
                                                   Y_controls_pre = Y_controls_to_optimise_pre)
  }
  
  # Extract optimal weights and intercept from optimisation routine
  W_opt <- results_optimisation$W_opt
  mu_opt <- results_optimisation$mu_opt
  
  # Generate counterfactual predictions - if using de-meaned outcomes,
  # use pre-treatment mean Y_treated + Y_controls %*% W_opt
  if(demean_outcomes == TRUE){
    
    # If optimisation failed, return NA for Y0_treated_hat
    if(any(is.na(W_opt))){
      
      Y0_treated_hat <- rep(NA, length(post))
     
       # If optimisation succeeded, generate Y0_treated_hat and recover original Y_treated 
    } else {
      
    Y0_treated_hat <- Y_treated_bar_pre_treatment + as.vector(Y_controls %*% W_opt)
    
    }
    Y_treated <- Y_treated_bar_pre_treatment + Y_treated
  }
  
  # If not de-meaning, set Y0_treated_hat = mu_opt + Y_controls %*% W_opt (mu = 0 by design in ADH/PSC)
  else if(demean_outcomes == FALSE){
    
    # If optimisation failed, return NA for Y0_treated_hat
    if(any(is.na(W_opt))){
      
      Y0_treated_hat <- rep(NA, length(post))
      
    } else {
      
    Y0_treated_hat <- mu_opt + as.vector(Y_controls %*% W_opt)
    
    }
  }
  
  # Return results
  results <- list("Y_treated" = Y_treated,
                  "Y0_treated_hat" = Y0_treated_hat,
                  "post" = post,
                  "W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "first_treated_period" = first_treated_period,
                  "method" = objective_function)
  
}