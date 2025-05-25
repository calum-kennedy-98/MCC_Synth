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
retry_synth <- function(data_prepared, 
                         optimxmethod,
                         initial_margin,
                         max_attempts,
                         margin_increment) {
  
  attempts <- 0
  current_margin <- initial_margin
  result <- NULL
  
  while(attempts < max_attempts && is.null(result)) {
    attempts <- attempts + 1
    
    tryCatch({
      result <- synth(data_prepared,
                      optimxmethod = optimxmethod,
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