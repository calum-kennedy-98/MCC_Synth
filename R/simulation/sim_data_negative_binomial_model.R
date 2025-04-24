# Name of script: sim_data_negative_binomial_model
# Description: Simulates mortality data based on city-specific negative binomial time
# series models
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

sim_data_negative_binomial_model <- function(data,
                                             id_var,
                                             outcome_var,
                                             year_var,
                                             linear_predictors,
                                             time_var,
                                             temp_var,
                                             spline_df_per_year,
                                             spline_df_temp){
  
  # Extract list of unique locations
  unique_locations <- distinct(data, {{id_var}}) %>% deframe()
  
  # Generate list of predictions from negative binomial model for each location
  list_data_negative_binomial <- map(unique_locations, function(x){
    
    # Set seed
    set.seed(42)
    
    # Extract location-specific data to send to glm.nb call
    data_for_neg_binomial_model <- filter(data, {{id_var}} == x)
    
    # Estimate negative binomial model
    res <- estimate_neg_binomial_model(data_for_neg_binomial_model,
                                       outcome_var,
                                       year_var,
                                       linear_predictors,
                                       time_var,
                                       temp_var,
                                       spline_df_per_year,
                                       spline_df_temp)
    
    # Generate counterfactual data where LFS air pollution in treated weeks is set to 
    # the LFS air pollution level in the final pre-treatment week
    data_counterfactual <- data_for_neg_binomial_model %>% 
      mutate(
        last_zero_outcome = if_else(treated == 0, pred_fire_PM25, NA_real_)
        ) %>%
      fill(
        last_zero_outcome, .direction = "down"
        ) %>% 
      mutate(
        pred_fire_PM25 = if_else(treated == 1, last_zero_outcome, pred_fire_PM25)
        )  %>% 
      dplyr::select(-last_zero_outcome)
    
    # Extract counterfactual LFS air pollution predictions
    pred_fire_PM25_counterfactual <- data_counterfactual$pred_fire_PM25
    
    # Extract model predicted values for the linear link function against real and synthetic data
    # and take exponential to find expected value of outcome variable distribution 
    outcome_exp_val_real <- exp(predict(res, data_for_neg_binomial_model))
    outcome_exp_val_counterfactual <- exp(predict(res, data_counterfactual))
    
    # Extract dispersion parameter theta
    theta <- res$theta
    
    # Draw real and counterfactual outcome series from negative binomial distribution
    # with specified mean and variance
    outcome_pred_real <- unlist(lapply(outcome_exp_val_real, rnegbin, n = 1, theta = theta))
    outcome_pred_counterfactual <- unlist(lapply(outcome_exp_val_counterfactual, rnegbin, n = 1, theta = theta))
    
    # Append outcome predictions and counterfactual LFS air pollution exposures to the real data
    data_with_pred <- cbind(data_for_neg_binomial_model, 
                            outcome_pred_real,
                            outcome_pred_counterfactual,
                            pred_fire_PM25_counterfactual)
    
  })
  
  # Append the list of datasets with predicted outcomes
  data_out <- bind_rows(list_data_negative_binomial)
  
  return(data_out)
  
}