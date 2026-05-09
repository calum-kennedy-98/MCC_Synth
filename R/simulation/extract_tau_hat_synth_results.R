#' Extract Treatment Effect Estimates from Simulation Results
#'
#' @description
#' Processes a list of synthetic control model outputs from a simulation study
#' and returns a tidy tibble of per-period true treatment effects (\eqn{\tau_t})
#' and estimated effects (\eqn{\hat\tau_t}). The true effects are constructed
#' from the untreated potential outcomes stored in each result object, according
#' to the specified data-generating process (\code{treatment_effect_type}).
#' Supported DGPs are: placebo (zero effect), constant additive, constant
#' multiplicative, and dynamic Cauchy-shaped.
#'
#' @param results_synth_model_simulated A list of synthetic control result
#'   objects (as returned by \code{optimise_synth}), one per simulation run.
#'   Each must contain named fields \code{method}, \code{Y_treated},
#'   \code{Y0_treated_hat}, and \code{post}.
#' @param treatment_effect_type Character string. One of \code{"placebo"},
#'   \code{"constant additive"}, \code{"constant multiplicative"}, or
#'   \code{"dynamic"}.
#' @param constant_additive_effect Numeric scalar. Required when
#'   \code{treatment_effect_type = "constant additive"}. The fixed additive
#'   treatment effect applied to post-treatment periods.
#' @param constant_multiplicative_effect Numeric scalar. Required when
#'   \code{treatment_effect_type = "constant multiplicative"}. The
#'   multiplicative factor applied to post-treatment potential outcomes.
#' @param peak_height_param Numeric scalar. Required when
#'   \code{treatment_effect_type = "dynamic"}. Scales the peak height of the
#'   Cauchy-shaped effect relative to the pre-treatment mean outcome.
#' @param peak_scale_param Numeric scalar. Required when
#'   \code{treatment_effect_type = "dynamic"}. Controls the spread (scale) of
#'   the Cauchy-shaped dynamic effect relative to the pre-treatment mean.
#'
#' @return A tibble with one row per simulation-run / period combination
#'   containing columns: \code{method}, \code{model_run}, \code{t}
#'   (event-time index), \code{post}, \code{tau} (true effect), \code{tau_hat}
#'   (estimated effect), \code{tau_hat_normalised}, \code{Y1_treated},
#'   \code{Y0_treated}, \code{Y0_treated_hat}.
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
    
    # Get normalised tau_hat (scale by value of Y1_treated)
    tau_hat_normalised <- tau_hat / Y1_treated
    
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