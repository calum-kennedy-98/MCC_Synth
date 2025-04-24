# Name of script: sim_data_factor_model
# Description: Generates simulated dataset from a factor model with the error
# terms given by an autoregressive process. Currently assumes null treatment
# effect
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 22-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 22-04-2025

# Define function to generate simulated data using factor model ----------------

estimate_factor_model <- function(Y,
                                  rank){
  
  # Store N and T dimensions of outcome matrix
  N <- dim(Y)[1]
  T <- dim(Y)[2]
  
  # Estimate singular value decomposition of outcome matrix
  svd_mat <- svd(Y)
  
  # Extract unit and time factors from svd
  factor_unit <- as.matrix(svd_mat$u[,1:rank] * sqrt(N))
  factor_time <- as.matrix(svd_mat$v[,1:rank] * sqrt(T))
  
  # Extract first 'rank' scaling factors from svd
  magnitude <- svd_mat$d[1:rank]/sqrt(N*T)
  
  # Estimate the low-rank factor matrix L
  L <- factor_unit %*% diag(magnitude, 
                            nrow = rank, 
                            ncol = rank) %*% t(factor_time)
  
  # Calculate error matrix E as difference between Y and L
  E <- Y - L
  
  # Fit AR(2) model to errors
  ar_coef <- fit_ar2(E)
  
  # Estimate correlation matrix for error terms
  cor_matrix <- ar2_correlation_matrix(ar_coef,T)
  
  # Return output
  return(list(E = E, L = L))
  
}