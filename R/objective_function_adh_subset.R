#' ADH Synthetic Control with Pre-Optimisation Donor Pool Subset Selection
#'
#' @description
#' Implements the Abadie–Diamond–Hainmueller (ADH) synthetic control method
#' on a pre-selected subset of the donor pool. Before running the ADH
#' quadratic programme, the \code{n_controls} control units with the smallest
#' pre-treatment Euclidean (L2) distance to the treated unit are selected.
#' The ADH optimisation is then run on this reduced pool using
#' \code{\link{retry_synth}}. Weights for units outside the subset are set to
#' zero; the remaining weights are rescaled to sum to one via
#' \code{\link{rescale_small_weights}}.
#'
#' @param Y_treated_pre Numeric vector of length \eqn{T_{\text{pre}}}. Observed
#'   pre-treatment outcome series for the treated unit.
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}. Pre-treatment outcome series for the full
#'   donor pool of \eqn{N} control units.
#' @param n_controls Integer. Number of nearest control units (by L2 norm) to
#'   retain for the ADH optimisation.
#' @param initial_margin Numeric. Initial value of \code{Margin.ipop} passed
#'   to \code{\link{retry_synth}}; controls the convergence tolerance of the
#'   ipop quadratic programme solver.
#' @param max_attempts Integer. Maximum number of re-attempts allowed if the
#'   optimisation fails, each time incrementing the margin.
#' @param margin_increment Numeric. Amount added to \code{Margin.ipop} on each
#'   failed optimisation attempt.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{W_opt}{Numeric vector of length \eqn{N}. Optimal ADH weights over
#'     the full donor pool (zero for units outside the selected subset).
#'     \code{NA} if optimisation fails.}
#'   \item{mu_opt}{Numeric scalar, always 0 (no intercept by design).}
#' }
#'
#' @references
#' Abadie, A., Diamond, A. & Hainmueller, J. (2010). Synthetic control methods
#' for comparative case studies. \emph{Journal of the American Statistical
#' Association}, 105(490), 493–505.
objective_function_adh_subset <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   n_controls,
                                   initial_margin,
                                   max_attempts,
                                   margin_increment){
  
  # Determine best subset of controls to use by taking L2 norm
  l2_norms <- colSums((Y_controls_pre - Y_treated_pre)^2)
  idx <- order(l2_norms)[1:n_controls]
  
  # Define relevant matrices for synth command
  X1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  X0 <- Y_controls_pre[, idx, drop = FALSE]
  Z1 <- matrix(Y_treated_pre, nrow = length(Y_treated_pre))
  Z0 <- Y_controls_pre[, idx, drop = FALSE]
  
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
    
    # Extract optimal weights, intercept, and weight matrix V (mu = 0 by design) - include zero weight for all
    # units not used to construct the SC
    W_opt <- rep(0, ncol(Y_controls_pre))
    W_opt[idx] <- synth_out[["solution.w"]]
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