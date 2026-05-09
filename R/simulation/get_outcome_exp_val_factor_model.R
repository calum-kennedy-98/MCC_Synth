#' Extract Expected Outcome Values from a Factor Model Decomposition
#'
#' @description
#' Fits a low-rank factor model to the observed outcome panel via
#' \code{\link{estimate_factor_model}}, extracts the systematic (low-rank)
#' component \eqn{L}, and returns it as a long vector ordered by unit then
#' time. The resulting vector represents the expected (noise-free) outcome
#' for each unit-period combination and is used to seed the factor-model
#' simulation DGP.
#'
#' @param data A data frame in long format containing all units and time
#'   periods. Rows must be sorted by unit then time.
#' @param unit_id_var Bare (unquoted) name of the unit identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param outcome_var Bare (unquoted) name of the outcome variable (tidy-eval).
#' @param week_id_var Bare (unquoted) name of the week identifier column
#'   (tidy-eval; accepted for API consistency but not directly used here).
#' @param treated_var Bare (unquoted) name of the treatment indicator column
#'   (tidy-eval; accepted for API consistency but not directly used here).
#' @param rank Integer. Rank of the factor model (number of singular vectors
#'   to retain).
#'
#' @return A numeric vector of length \eqn{n \times T} containing the
#'   low-rank systematic expected values, ordered first by time within each
#'   unit.
get_outcome_exp_val_factor_model <- function(data,
                                            unit_id_var,
                                            time_id_var,
                                            outcome_var,
                                            week_id_var,
                                            treated_var,
                                            rank){
  
  # Extract n_units and n_periods
  n_units <- length(pull(distinct(data, {{unit_id_var}})))
  n_periods <- length(pull(distinct(data, {{time_id_var}})))
  
  # Estimate low-rank factor model and extract systematic/error components
  results_decomposition <- estimate_factor_model(data,
                                                 unit_id_var = {{unit_id_var}},
                                                 time_id_var = {{time_id_var}},
                                                 outcome_var = {{outcome_var}},
                                                 rank = rank)
  
  # Extract systematic component
  L <- results_decomposition$L
  
  # Extract expected value of outcome as a vector
  outcome_exp_val <- c(t(L))
  
}