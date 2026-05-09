#' Run the Synth Package SC Model Across Multiple Simulated Datasets
#'
#' @description
#' Iterates over a list of simulated datasets and runs the \pkg{Synth}
#' package synthetic control model on each via \code{\link{run_synth_model}}.
#' The treated unit is identified as the unit with the smallest numeric ID in
#' each dataset; all remaining units serve as controls. Returns a list of
#' \code{synth.out} result objects, one per simulated dataset.
#'
#' @param list_data_simulated A list of data frames, one per simulation
#'   replicate, each containing a panel of units and time periods in the format
#'   expected by \code{\link[Synth]{dataprep}}.
#' @param id_var Bare (unquoted) name of the numeric unit identifier column
#'   (tidy-eval).
#' @param predictors Character vector of predictor variable names passed to
#'   \code{\link[Synth]{dataprep}}.
#' @param special_predictors A list of special predictor specifications
#'   (period-specific outcome values) passed as \code{special.predictors} to
#'   \code{\link[Synth]{dataprep}}.
#' @param time_predictors_prior Numeric vector. Pre-treatment time periods used
#'   to compute predictor means.
#' @param dep_var Character string. Name of the dependent variable.
#' @param time_var_synth Character string. Name of the time variable column
#'   passed to \code{\link[Synth]{dataprep}}.
#' @param time_optimise_ssr Numeric vector. Time periods over which the SSR
#'   is minimised.
#' @param time_plot Numeric vector. Time periods to include in the output for
#'   plotting.
#'
#' @return A list of length \code{length(list_data_simulated)}, where each
#'   element is the output of \code{\link{run_synth_model}} (a named list
#'   containing \code{data}, \code{data_prepared}, and \code{synth_out}).

# Define function to simulate n_sims runs of synth model -----------------------

sim_synth_model <- function(list_data_simulated,
                            id_var,
                            predictors,
                            special_predictors,
                            time_predictors_prior,
                            dep_var,
                            time_var_synth,
                            time_optimise_ssr,
                            time_plot){
  
  # Extract unit.variable as character from id var
  unit_variable <- as.character(substitute(id_var))
  
  # For each element in the list of data frames, summarise to weekly level and apply synth model
  output <- map(list_data_simulated, function(data){
    
    # Extract identifiers for treated unit (smallest id number) and controls (all other units by default)
    id_vec <- distinct(data, {{id_var}}) %>% deframe()
    treated_id <- min(id_vec)
    controls_id <- id_vec[! id_vec %in% treated_id]
    
    # Run synth command and get output
    synth_output <- run_synth_model(data = data,
                                    predictors= predictors,
                                    special_predictors = special_predictors,
                                    time_predictors_prior = time_predictors_prior,
                                    dep_var = dep_var,
                                    unit_variable = unit_variable,
                                    time_variable = time_var_synth,
                                    treated_id = treated_id,
                                    controls_id = controls_id,
                                    time_optimise_ssr = time_optimise_ssr,
                                    time_plot = time_plot)
    
  })
  
  return(output)
} 