# Name of script: sim_data_negative_binomial_model
# Description: Simulates mortality data based on city-specific negative binomial time
# series models
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

sim_data_negative_binomial_model <- function(seed,
                                             data,
                                             model){
  
    
    # Set seed
    set.seed(seed)
    
    # Extract model predicted values for the linear link function
    # and take exponential to find expected value of outcome variable distribution 
    outcome_exp_val <- exp(predict(model, data))
    
    # Extract dispersion parameter theta
    theta <- model$theta
    
    # Draw real and counterfactual outcome series from negative binomial distribution
    # with specified mean and variance
    outcome_pred <- unlist(lapply(outcome_exp_val, rnegbin, n = 1, theta = theta))
    
    # Append outcome predictions and counterfactual LFS air pollution exposures to the real data
    data_out <- cbind(data, 
                      outcome_pred)
  
  return(data_out)
  
}