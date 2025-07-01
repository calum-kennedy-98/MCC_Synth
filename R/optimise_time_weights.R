# Name of script: optimise_time_weights
# Description: ***
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 13-03-2025

# Comments ---------------------------------------------------------------------

# Define function find optimal synth -------------------------------------------

# @ param Y_controls (T x N) matrix of outcomes for control units (rows time periods, columns units)
# @ post, (T x 1) vector of indicators for pre-treatment (post == 0) and post-treatment (post == 1)

optimise_time_weights <- function(Y_controls,
                                  post){
  
  # De-mean outcomes
  Y_controls <- t(apply(Y_controls, 1, function(row){row - mean(row)}))
  
  # Separate Y_controls matrix into Y_pre and Y_post
  n_pre <- sum(post == 0)
  Y_controls_pre <- Y_controls[1:n_pre,]
  Y_controls_post <- Y_controls[(n_pre+1):length(post),]
  
  # Extract number of time periods
  n_periods <- nrow(Y_controls_pre)
  
  # Get mean outcomes in post-treatment period
  Y_bar_controls_post <- colMeans(Y_controls_post)
  
  # Set up inputs for Low rank QP
  H <- Y_controls_pre %*% t(Y_controls_pre)
  d <- t(Y_controls_pre %*% Y_bar_controls_post)
  
  # Low rank QP
  res <- LowRankQP(Vmat = H,
                   dvec = d,
                   Amat = matrix(1, ncol = n_periods),
                   bvec = 1,
                   uvec = rep(1,n_periods), 
                   method = "LU")
  
  # Get optimal weights
  W_opt <- res$alpha
  
  return(W_opt)
  
}