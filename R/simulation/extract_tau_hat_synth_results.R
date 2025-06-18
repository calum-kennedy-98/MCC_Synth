# Name of script: extract_tau_hat_synth_results
# Description: Function to extract estimated tau hats from a list of synthetic control
# model outputs. The function takes inputs from a generic synthetic control model
# class, assuming that the true observed outcome is "Y1", the predicted outcome
# is "Y1_hat" and the first treated period is "first_treated_period"
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 05-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 05-03-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

extract_tau_hat_synth_results <- function(results_synth_model_simulated,
                                          treatment_effect_type,
                                          constant_additive_effect = NULL,
                                          constant_multiplicative_effect = NULL,
                                          peak_height_param = NULL,
                                          peak_scale_param = NULL){
  
  # Checks
  if(!treatment_effect_type %in% c("placebo", "constant additive", "constant multiplicative", "dynamic")) stop("Please set treatment_effect_type to one of 'placebo', 
                                                                           'constant additive', 'constant multiplicative' or 'dynamic'")
  
  if(treatment_effect_type == "constant additive" & !is.numeric(constant_additive_effect)) stop("Please specify a numeric value for 'constant_additive_treatment_effect")
  if(treatment_effect_type == "constant multiplicative" & !is.numeric(constant_multiplicative_effect)) stop("Please specify a numeric value for 'constant_multiplicative_effect")
  if(treatment_effect_type == "dynamic" & !is.numeric(peak_height_param)) stop("Please set a numeric value for 'peak_height_param'")
  if(treatment_effect_type == "dynamic" & !is.numeric(peak_scale_param)) stop("Please set a numeric value for 'peak_scale_param'")
  
  # Generate ID for model run and add to list of outputs
  model_run <- seq(1:length(results_synth_model_simulated))
  
  results_synth_model_simulated_with_id <- lapply(seq_along(results_synth_model_simulated),
                                                  function(x) {
                                                    c(results_synth_model_simulated[[x]], model_run = x)
                                                  })
  
  # Get list of outputs from each model run
  list_tau_hat <- lapply(results_synth_model_simulated_with_id, function(x){
    
    # Extract method
    method <- x[["method"]]
    
    # Extract true untreated potential outcomes Y0 and estimated Y0_hat for treated unit
    Y0_treated <- x[["Y_treated"]]
    Y0_treated_hat <- c(x[["Y0_treated_hat"]])
    
    # Extract post-treatment indicator and vector of time ids (re-centred so t = 0 is first treated period)
    post <- x[["post"]]
    t <- 0:(length(post) - 1) - length(post[post == 0])
    
    # Assign treatment effects based on treatment effect type - note required arguments
    # differ by treatment effect mechanism 
    if(treatment_effect_type == "placebo"){
      
      Y1_treated <- Y0_treated 
      
    } else if(treatment_effect_type == "constant additive"){
      
      Y1_treated <- Y0_treated + constant_additive_effect * post
      
    } else if(treatment_effect_type == "constant multiplicative"){
      
      Y1_treated <- round(Y0_treated + (constant_multiplicative_effect - 1) * post * Y0_treated)
      
    } else if(treatment_effect_type == "dynamic"){
      
      peak_location <- median(t[post == 1])
      peak_height <- peak_height_param * mean(Y0_treated)
      peak_scale <- peak_scale_param * mean(Y0_treated)
      
      treatment_effect_dynamic <- peak_height * dcauchy(t, 
                                               location = peak_location, 
                                               scale = peak_scale) /
        dcauchy(peak_location, 
                location = peak_location, 
                scale = peak_scale)
      
      Y1_treated <- round(Y0_treated + treatment_effect_dynamic * post)
    }
    
    # Extract true tau and estimated tau hat
    tau <- Y1_treated - Y0_treated
    tau_hat <- Y1_treated - Y0_treated_hat
    
    # Get normalised tau_hat (scaled by standard deviation of tau_hat in post-treatment period)
    tau_hat_normalised <- (tau_hat - tau) / sd(tau_hat[post == 1])
    
    # Return tibble of results
    results <- tibble("method" = method,
                      "model_run" = x[["model_run"]],
                      "t" = t,
                      "post" = post,
                      "tau" = tau,
                      "tau_hat" = tau_hat,
                      "tau_hat_normalised" = tau_hat_normalised,
                      "Y1_treated" = Y1_treated,
                      "Y0_treated" = Y0_treated,
                      "Y0_treated_hat" = Y0_treated_hat)
    
  }) 
  
  # Bind rows into one dataframe and return
  results_synth_tau_hat <- bind_rows(list_tau_hat)
  
  return(results_synth_tau_hat)
}