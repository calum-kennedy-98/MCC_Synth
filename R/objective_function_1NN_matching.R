#' 1-Nearest Neighbour Matching Counterfactual
#'
#' @description
#' Identifies the single control unit whose pre-treatment outcome series is
#' closest to the treated unit in terms of the Euclidean (L2) norm, and
#' assigns it a weight of 1. All other control units receive a weight of 0.
#' The function signature mirrors that of the other \code{objective_function_*}
#' helpers so that \code{optimise_synth} can call any of them
#' interchangeably; the arguments \code{n_controls}, \code{initial_margin},
#' \code{max_attempts}, and \code{margin_increment} are accepted but not used
#' in this method.
#'
#' @param Y_treated_pre Numeric vector of length \eqn{T_{\text{pre}}}. Observed
#'   pre-treatment outcome series for the treated unit.
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}. Pre-treatment outcome series for all
#'   \eqn{N} control units.
#' @param n_controls Integer. Not used; present for API consistency.
#' @param initial_margin Numeric. Not used; present for API consistency.
#' @param max_attempts Integer. Not used; present for API consistency.
#' @param margin_increment Numeric. Not used; present for API consistency.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{W_opt}{Numeric vector of length \eqn{N}. Weight vector with 1 for
#'     the nearest-neighbour control unit and 0 for all others.}
#'   \item{mu_opt}{Numeric scalar, always 0 (no intercept shift).}
#' }
objective_function_1NN_matching <- function(Y_treated_pre,
                                          Y_controls_pre,
                                          n_controls,
                                          initial_margin,
                                          max_attempts,
                                          margin_increment){
  
  # Determine nearest neighbour control using L2 norm
  l2_norms <- colSums((Y_controls_pre - Y_treated_pre)^2)
  idx <- order(l2_norms)[1]
  
  # Select optimal weight vector (weight = 1 for nearest neighbour, 0 otherwise)
  W_opt <- rep(0, ncol(Y_controls_pre))
  W_opt[idx] <- 1
  
  # Set mu = 0
  mu_opt <- 0
  
  # Store results in list
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt)
  
  return(results)
  
}