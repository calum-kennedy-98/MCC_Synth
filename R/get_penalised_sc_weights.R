# Name of script: get_penalised_sc_weights
# Description: Function to find optimal penalised synthetic control weights
# using the LowRankQP solver. This code adapted from L'Hour (2021)
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 02-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 02-06-2025

# Comments ---------------------------------------------------------------------

# At present, not using weight vector V - in future could look to incorporate

# @ param X1, m x 1 vector of predictors for the treated unit
# @ param X0, m x n matrix of predictors for the control units
# @ param lambda, L1 penalty parameter
# @ output W_opt, n x 1 vector of optimal penalised SC weights 

# Function ---------------------------------------------------------------------

get_penalised_sc_weights <- function(X1, X0, lambda){
  
  # Extract number of control units
  n_controls <- ncol(X0)
  
  # Set up inputs for quadratic programming algorithm
  pairwise_discrepancies <- diag(t(X0 - matrix(rep(1, n_controls), ncol = n_controls) %x% X1) %*% (X0 - matrix(rep(1, n_controls), ncol = n_controls) %x% X1))
  H <- 2*t(X0) %*% X0
  d <- t(-2 * t(X0) %*% X1 + lambda * pairwise_discrepancies)
  
  # Find optimal weights using LowRankQP
  res <- LowRankQP(Vmat = H,
                   dvec = d,
                   Amat = matrix(1, ncol = n_controls),
                   bvec = 1,
                   uvec = rep(1,n_controls), 
                   method = "LU")
  
  # Get optimal weights
  W_opt <- res$alpha
  
  # Trim very small weights and rescale
  W_opt <- rescale_small_weights(W_opt,
                                 tolerance = 1e-6,
                                 scale = TRUE)
  
  return(W_opt)
  
}