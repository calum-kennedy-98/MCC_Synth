# Name of script: estimate_factor_model
# Description: Estimates low-rank factor model on arbitrary matrix of outcomes
# and returns matrix of systematic components (W) and error matrix (E).
# Adapted from `SynthDID` GitHub repo (https://github.com/synth-inference/synthdid/blob/master/R/placebo-simulations.R)
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 03-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 03-06-2025

# Comments ---------------------------------------------------------------------

# @ param data, ...

# Function ---------------------------------------------------------------------

estimate_factor_model <- function(data,
                                  unit_id_var,
                                  time_id_var,
                                  outcome_var,
                                  rank){
  
  # Extract n_units and n_periods
  n_units <- length(pull(distinct(data, {{unit_id_var}})))
  n_periods <- length(pull(distinct(data, {{time_id_var}})))
  
  # Extract outcomes and reshape to n_units x n_periods matrix
  mat_Y <- t(matrix(pull(data, {{outcome_var}}), nrow = n_periods))
  
  # Generate singular value decomposition of Y and extract first `rank' unit/time factors
  svd_Y <- svd(mat_Y)
  factor_unit <- as.matrix(svd_Y$u[,1:rank])
  factor_time <- as.matrix(svd_Y$v[,1:rank])
  magnitude <- svd_Y$d[1:rank]
  
  # Generate systematic component L
  L <- factor_unit %*% diag(magnitude, nrow = rank, ncol = rank) %*% t(factor_time)
  
  # Generate error matrix
  E <- mat_Y - L
  
  # Return results
  return(list(L = L, E = E))
  
}