#' Wrapper: Extract Tau-Hat Estimates and RMSE Diagnostics from Simulation
#'
#' @description
#' Orchestrates two downstream diagnostics functions for a simulation study:
#' calls \code{\link{extract_tau_hat_synth_results}} to obtain per-period
#' true and estimated treatment effects, then calls \code{\link{get_rmse_synth}}
#' to compute RMSE over a user-specified evaluation window. Results are returned
#' as a named list.
#'
#' @param results_synth_model_simulated A list of synthetic control result
#'   objects (as returned by \code{optimise_synth}), one per simulation run.
#' @param id_var Bare (unquoted) name of the unit identifier column (tidy-eval).
#' @param time_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param start_index Integer. Start of the evaluation window (time index).
#' @param end_index Integer. End of the evaluation window (time index).
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{results_synth_tau_hat}{Tibble of tau and tau-hat estimates per
#'     period and simulation run.}
#'   \item{results_rmse}{RMSE results over the specified evaluation window.}
#' }

# Define wrapper function ------------------------------------------------------

get_synth_diagnostics <- function(results_synth_model_simulated,
                                  id_var,
                                  time_var,
                                  start_index,
                                  end_index){
  
  # Get estimated vs. true tau hat
  results_synth_tau_hat <- extract_tau_hat_synth_results(results_synth_model_simulated,
                                                         {{id_var}},
                                                         {{time_var}})
  
  
  # Get RMSE over specified period
  results_rmse <- get_rmse_synth(results_synth_model_simulated,
                                 {{id_var}},
                                 start_index,
                                 end_index)
  
  # Store final output in list
  list_synth_diagnostics <- list(results_synth_tau_hat = results_synth_tau_hat,
                                 results_rmse = results_rmse)
  
  return(list_synth_diagnostics)
  
}