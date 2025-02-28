# Name of script: make_data_scm
# Description: Generates simulated dataset conforming to the assumed Structural Causal Model
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Define function to generate simulated dataset of covariates ------------------

make_data_scm <- function(n_units,
                             n_periods,
                             n_covars){
  
  # Set seed
  set.seed(42)
  
  # Create panel dataset
  data <- expand.grid(
    unit = 1:n_units,
    time = 1:n_periods
  )
  
  # Generate unobserved time-invariant factor a from negative exponential distribution
  a <- rep(rexp(n_units), n_periods)
  
  # Simulate observed time-varying variable x (e.g. temperature)
  
  return(data)
}

test <- make_data_scm(100,100,0)
