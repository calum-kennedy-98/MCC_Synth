#' Classical ADH Synthetic Control
#'
#' @description
#' Implements the classical Abadie–Diamond–Hainmueller (ADH) synthetic control
#' method using the full donor pool. Finds a convex combination of control
#' units that best approximates the treated unit's pre-treatment outcome series
#' by minimising the pre-treatment mean squared prediction error subject to
#' non-negativity and sum-to-one constraints on the weights (no intercept).
#' The quadratic programme is solved via \code{\link{retry_synth}}, which
#' retries with increasing \code{Margin.ipop} values if the optimiser fails to
#' converge.
#'
#' @param Y_treated_pre Numeric vector of length \eqn{T_{\text{pre}}}. Observed
#'   pre-treatment outcome series for the treated unit.
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}. Pre-treatment outcome series for all
#'   \eqn{N} control units.
#' @param initial_margin Numeric. Initial value of \code{Margin.ipop} for the
#'   ipop solver; controls the convergence tolerance.
#' @param max_attempts Integer. Maximum number of optimisation re-attempts,
#'   each with an incremented margin.
#' @param margin_increment Numeric. Amount added to \code{Margin.ipop} after
#'   each failed attempt.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{W_opt}{Numeric vector of length \eqn{N}. Optimal convex weights
#'     over the full donor pool. \code{NA} if optimisation fails.}
#'   \item{mu_opt}{Numeric scalar, always 0 (no intercept by design).}
#' }
#'
#' @references
#' Abadie, A., Diamond, A. & Hainmueller, J. (2010). Synthetic control methods
#' for comparative case studies. \emph{Journal of the American Statistical
#' Association}, 105(490), 493–505.
objective_function_adh <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   initial_margin,
                                   max_attempts,
                                   margin_increment){
  
  # Define relevant matrices for synth command
  X1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  X0 <- Y_controls_pre
  Z1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  Z0 <- Y_controls_pre
  
  # Generate synth object using 'retry_synth' - available in 'utility_functions.R'
  synth_out <- retry_synth(X1 = X1,
                           X0 = X0,
                           Z1 = Z1,
                           Z0 = Z0,
                           initial_margin = initial_margin,
                           max_attempts = max_attempts,
                           margin_increment = margin_increment)
  
  # If optimisation succeeded, generate relevant outputs
  if(!is.null(synth_out)){
  
  # Extract optimal weights, intercept, and weight matrix V (mu = 0 by design)
  W_opt <- synth_out[["solution.w"]]
  mu_opt <- 0
  
  # Rescale weights to remove very small weights (symptom of quadratic programming algos)
  W_opt <- rescale_small_weights(W_opt,
                                 tolerance = 1e-6,
                                 scale = TRUE)
  
  # If optimisation failed, return NA for missing outputs
  } else {
    W_opt <- NA
    mu_opt <- 0
  }
  
  # Store results in list
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt)
  
  return(results)
  
}