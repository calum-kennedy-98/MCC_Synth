# Name of script: optimise_synth_elastic_ensemble
# Description: ***
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 13-03-2025

# Comments ---------------------------------------------------------------------

# For each synthetic control, we choose N/3 randomly selected control units.

# @ param data, an input dataframe
# @ param method, synthetic control family method to use
# @ param n_synth, number of synthetic controls to build
# @ param id_var, name of ID column variable
# @ param outcome_var, name of the outcome variable column
# @ param treated_id_var, binary ID variable for the treated unit
# @ param treated_time_var, binary ID variable for the treatment time
# @ return a list with elements data, Y1, Y1_hat, W_opt, mu_opt (equal to 0), alpha_opt, lambda_opt, first_treated_period

# Define function find optimal synth -------------------------------------------

optimise_synth_ensemble <- function(data,
                                    method,
                                    n_synth,
                                    id_var,
                                    outcome_var,
                                    time_var,
                                    treated_id_var,
                                    treated_time_var,
                                    n_periods_pre,
                                    n_periods_post){
  
  # Extract treated unit ID
  treated_unit <- data %>%
    filter({{treated_id_var}} == 1) %>%
    distinct({{id_var}}) %>%
    pull({{id_var}})
  
  # Extract control unit IDs
  control_units <- data %>%
    filter({{treated_id_var}} == 0) %>%
    distinct({{id_var}}) %>%
    pull({{id_var}})
  
  # Set number of control units to sample (initially set to n_controls / 3)
  n_controls <- length(control_units)
  n_controls_to_sample <- n_controls / 3
  
  # Generate list of subsetted data based on random sampling from the control units
  list_data_subset <- lapply(1:n_synth, function(x){
    
    sample_controls <- sample(control_units, n_controls_to_sample)
    
    data_subset <- data %>% 
      filter({{id_var}} %in% c(sample_controls, treated_unit))
  })
  
  # Optimise preferred SC family method on resulting list
  list_results_synth <- lapply(list_data_subset, function(x) {
    
    optimise_synth_adh(x,
                       id_var = column_label,
                       outcome_var = outcome_pred,
                       time_var = week_id,
                       treated_id_var = treated,
                       treated_time_var = post,
                       n_periods_pre = 26,
                       n_periods_post = 26,
                       predictors = NULL,
                       optimxmethod = c("Nelder-Mead", "BFGS"),
                       initial_margin = 0.0005,
                       max_attempts = 20,
                       margin_increment = 0.0005)
  }
  )
  
  # Extract Y1 and Y1_hat in pre-treatment period and estimate MSPE
  mspe_pre_treatment <- sapply(list_results_synth, function(x){
    
    Y1_pre <- x$Y1[1:n_periods_pre]
    Y1_hat_pre <- x$Y1_hat[(n_periods_pre + 1):length(x$Y1)]
    tau_hat_pre <- Y1_hat_pre - Y1_pre
    mspe_pre <- mean(tau_hat_pre^2)
  })
  
  # Generate normalised weights based on inverse of squared MSPE
  weights_inverse_mspe <- 1 / mspe_pre_treatment^2
  weights_normalised <- weights_inverse_mspe / sum(weights_inverse_mspe)
  
  # Generate matrix of predicted Y1_hat vectors
  mat_Y1_hat <- do.call(cbind, lapply(list_results_synth, function(x) x$Y1_hat))
  
  # Extract final predicted Y1_hat as weighted combination of individual Y1_hat predictions
  Y1_hat <- mat_Y1_hat %*% weights_normalised
  
  # Extract true Y1
  Y1 <- list_results_synth[[1]][["Y1"]]
  
  # Extract results
  results <- list("Y1" = Y1,
                    "Y1_hat" = Y1_hat,
                    "weights_normalised" = weights_normalised,
                    "method" = "ensemble_adh")
  
  return(results)
  
}