#' Compute Conformal Inference P-Values Over a Grid of Null Hypotheses
#'
#' @description
#' Iterates over a user-supplied grid of null hypothesis values, constructing
#' potential-outcome data under each null via \code{\link{get_Y0_null}},
#' estimating synthetic control residuals via
#' \code{\link{get_residuals_conformal_inference}}, and computing a
#' block-permutation p-value via \code{\link{get_p_value}}. The result is a
#' tibble mapping each null hypothesis to its corresponding p-value, which can
#' be inverted to obtain a confidence interval using
#' \code{\link{get_confidence_interval}}.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param method Character string. Synthetic control method used to generate
#'   residuals. Must be one of \code{"ADH"}, \code{"ADH subset"},
#'   \code{"DID"}, \code{"DIFP"}, \code{"PSC"}, or \code{"1NN matching"}.
#' @param outcome_var Character string. Quoted name of the outcome variable.
#' @param treated_var Character string. Quoted name of the binary treatment
#'   indicator column (1 = treated unit, 0 = control).
#' @param post_var Character string. Quoted name of the binary post-treatment
#'   period indicator (1 = post, 0 = pre).
#' @param time_var Character string. Quoted name of the time identifier column.
#' @param id_var Character string. Quoted name of the unit identifier column.
#' @param null_grid Numeric vector. Grid of null hypothesis values (posited
#'   treatment effects \eqn{\tau_0}) to evaluate.
#' @param q Numeric. Degree of the \eqn{L_q} norm used in the conformal test
#'   statistic. Default is \code{1} (L1 norm).
#'
#' @return A tibble with columns \code{p_val} (numeric, the conformal p-value)
#'   and \code{null_hypothesis} (numeric, the corresponding null hypothesis
#'   value), with one row per element of \code{null_grid}.
get_p_value_grid <- function(data,
                             method,
                             outcome_var,
                             treated_var,
                             post_var,
                             time_var,
                             id_var,
                             null_grid,
                             q = 1){
  
  # Get grid of p-values for different posited effects under the null
  p_val_grid <- future_map(null_grid, function(null_hypothesis){
    
    # Generate data under the null
    data_null <- get_Y0_null(data,
                             outcome_var,
                             treated_var,
                             post_var,
                             null_hypothesis)
    
    # Get residuals using SC estimator
    residuals_df <- get_residuals_conformal_inference(data_null,
                                                      method = method,
                                                      outcome_var = "Y0_null",
                                                      treated_var,
                                                      time_var,
                                                      id_var,
                                                      post_var)
    
    # Extract p-value
    p_val <- get_p_value(residuals_df,
                         residuals_var = "residuals",
                         post_var = "post",
                         q)
    
  })
  
  # Extract results as dataframe
  results <- tibble("p_val" = p_val_grid, 
                    "null_hypothesis" = null_grid)
  
  return(results)
}