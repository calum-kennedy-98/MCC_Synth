# Name of script: make_list_sim_data_factor_model
# Description: Generates list of simulated outcomes using a low-rank factor model
# and an AR(2) process for the error matrix. Currently based on the simulations
# used in Synthetic Difference-in-Differences paper (arkhangelsky et al., 2021)
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 03-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 03-06-2025

# Comments ---------------------------------------------------------------------

# @ param ...

# Function ---------------------------------------------------------------------

make_list_sim_data_factor_model <- function(n_sims,
                                        data,
                                        unit_id_var,
                                        time_id_var,
                                        outcome_var,
                                        week_id_var,
                                        treated_var,
                                        rank){
  
  # Extract n_units and n_periods
  n_units <- length(pull(distinct(data, {{unit_id_var}})))
  n_periods <- length(pull(distinct(data, {{time_id_var}})))
  
  # Estimate low-rank factor model and extract systematic/error components
  results_decomposition <- estimate_factor_model(data,
                                                 unit_id_var = {{unit_id_var}},
                                                 time_id_var = {{time_id_var}},
                                                 outcome_var = {{outcome_var}},
                                                 rank = rank)
  
  # Extract systematic component
  L <- results_decomposition$L
  
  # Estimate AR(2) coefficients on error matrix
  E <- results_decomposition$E
  ar2_coef <- fit_ar_2(E)
  
  # Estimate correlation matrix from AR(2) model and convert to covariance matrix
  cor_mat <- ar2_correlation_matrix(ar2_coef, n_periods)
  scale_sd <- norm(t(E) %*% E / n_units,'f') / norm(cor_mat, 'f')
  cov_mat <- cor_mat * scale_sd
  
  # Generate list of vectors with outcomes simulated from latent factor model with rank == `rank'
  list_outcome_sim_factor_model <- future_map(seq_len(n_sims), function(i){
    
    # Simulate outcome matrix from factor model
    mat_outcome_pred_factor_model <- L + rmvnorm(n_units, sigma = cov_mat)
    
    # Transform to vector
    outcome_pred_factor_model <- round(as.vector(t(mat_outcome_pred_factor_model)))
    
    return(outcome_pred_factor_model)
    
  },
  .options = furrr_options(seed = TRUE))
  
  return(list_outcome_sim_factor_model)
  
}