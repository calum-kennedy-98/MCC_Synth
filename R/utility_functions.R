# Name: utility_functions
# Description: Utility functions for main MCC project
# Author: CK
# Date: 03-03-2025

# Define utility functions -----------------------------------------------------

# Function to generate values from a Cauchy-like distribution
generate_fat_tail <- function(start_index, 
                              end_index, 
                              amplitude, 
                              gamma) {
  indices <- seq(start_index, 
                 end_index, 
                 length.out = end_index - start_index + 1)
  midpoint <- mean(indices)
  cauchy_like <- unlist(lapply(list(start_index:end_index), function(t){cauchy_val <- amplitude / (1 + ((t - midpoint)^2) / gamma^2)}))
  return(cauchy_like)
}

# Function to impute missing values in arbitrary dataset using mice package
impute_missing_data <- function(data,
                                maxit,
                                m,
                                seed){
  
  data_imp <- mice(data, maxit = maxit, m = m, seed = seed)
  data_complete <- complete(data_imp)
  
  return(data_complete)
}

# Generic function to map another function to a list of inputs
map_func <- function(func,
                     list,
                     ...){
  
  results <- map(list, function(x) {
    
    output <- func(x,
                   ...)
    
  })
  
  return(results)
  
}

# Function to try synth optimisation with various levels of margin.ipop until success
# Sometimes, there are problems with convergence for small values of margin.ipop
retry_synth <- function(X1,
                        X0,
                        Z1,
                        Z0,
                        initial_margin,
                        max_attempts,
                        margin_increment) {
  
  attempts <- 0
  current_margin <- initial_margin
  result <- NULL
  
  while(attempts < max_attempts && is.null(result)) {
    attempts <- attempts + 1
    
    tryCatch({
      result <- synth(X1 = X1,
                      X0 = X0,
                      Z1 = Z1,
                      Z0 = Z0,
                      Margin.ipop = current_margin)
      
      message(paste("Optimization succeeded with Margin.ipop =", current_margin))
      
    }, error = function(e) {
      
      # Change optimisation method to 'all' for next attempt
      current_margin <- current_margin + margin_increment
    })
  }
  
  if(is.null(result)) {
    message(paste("Optimization failed after", max_attempts, "attempts"))
  }
  
  return(result)
}

# Auxiliary function to convert small weights to zero - symptomatic of some quadratic programming
# routines that many weights end up being very small but non-zero. This function converts small
# weights to zero and re-scales the remaining weights

# @ param w, vector of weights
# @ param tolerance, maximum weight allowed to be non-zero
# @ param scale, if TRUE rescale so remaining non-zero weights sum to 1

rescale_small_weights <- function(w, 
                                  tolerance = 1e-6,
                                  scale = TRUE){
  
  w_rescaled <- ifelse(w < tolerance, 0, w)
  if(scale) w_rescaled <- w_rescaled / sum(w_rescaled)
  
  return(w_rescaled)
}

# Function to fit an AR(p) model to a matrix with rows as time series

# @ param E, an error matrix 

fit_ar_2 <- function(E){
  
  # Get n_periods
  n_periods <- dim(E)[2]
  
  E_ts <- E[,3:n_periods]
  E_lag_1 <- E[,2:(n_periods-1)]
  E_lag_2 <- E[,1:(n_periods-2)]
  
  a_1 <- sum(diag(E_lag_1 %*% t(E_lag_1)))
  a_2 <- sum(diag(E_lag_2 %*% t(E_lag_2)))
  a_3 <- sum(diag(E_lag_1 %*% t(E_lag_2)))
  
  matrix_factor <- rbind(c(a_1,a_3),c(a_3,a_2))
  
  b_1 <- sum(diag(E_lag_1 %*% t(E_ts)))
  b_2 <- sum(diag(E_lag_2 %*% t(E_ts)))
  
  ar_coef <- solve(matrix_factor) %*% c(b_1,b_2)
  
  return(ar_coef)
}

# FUnction to estimate correlation matrix of AR(2) model

ar2_correlation_matrix <- function(ar_coef, n_periods) {

  result <- rep(0, n_periods)
  result[1] <- 1
  result[2] <- ar_coef[1]/(1-ar_coef[2])
  for (t in 3:n_periods){
    result[t] <-  ar_coef[1]*result[t-1] + ar_coef[2]*result[t-2] 
  }
  
  index_matrix <- outer(1:n_periods, 1:n_periods, function(x,y){ abs(y-x)+1 })
  cor_matrix <- matrix(result[index_matrix], ncol = n_periods, nrow = n_periods)
  
  return(cor_matrix)
}