#' Penalised Synthetic Control (PSC) — Abadie & L'Hour (2021)
#'
#' @description
#' Implements the penalised synthetic control estimator of Abadie & L'Hour
#' (2021). The L1 penalty parameter \eqn{\lambda} is selected by minimising
#' the LOO cross-validation MSPE via
#' \code{\link{get_hyperparam_loss_penalised_sc}} and
#' \code{\link[optimx]{optimx}} with L-BFGS-B. At the optimal \eqn{\lambda},
#' the final weights are obtained by \code{\link{get_penalised_sc_weights}}.
#' The estimator encourages weights to be placed on controls that are similar
#' to the treated unit by penalising pairwise discrepancies.
#'
#' @param Y_treated_pre Numeric vector of length \eqn{T_{\text{pre}}}. Observed
#'   pre-treatment outcome series for the treated unit.
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}. Pre-treatment outcome series for the
#'   control units.
#' @param Y_controls_post Numeric matrix of dimensions
#'   \eqn{T_{\text{post}} \times N}. Post-treatment outcome series for the
#'   control units (used for hyperparameter tuning via LOO-CV only).
#' @param lambda_init Numeric (\eqn{> 0}). Initial value for the L1 penalty
#'   parameter \eqn{\lambda}.
#' @param lower_bound_lambda Numeric. Lower bound on \eqn{\lambda} during
#'   optimisation (prevents degenerate solutions).
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{W_opt}{Numeric vector of length \eqn{N}. Optimal penalised SC
#'     weights summing to one.}
#'   \item{mu_opt}{Numeric scalar, always 0 (no intercept by design).}
#'   \item{lambda_opt}{Numeric. Optimised penalty parameter \eqn{\lambda}.}
#' }
#'
#' @references
#' Abadie, A. & L'Hour, J. (2021). A penalized synthetic control estimator
#' for disaggregated data. \emph{Journal of the American Statistical
#' Association}, 116(536), 1817–1834.
objective_function_psc <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   Y_controls_post,
                                   lambda_init,
                                   lower_bound_lambda){
  
  # Initialise starting value for lambda
  par <- c("lambda" = lambda_init)
  
  # Optimise lambda for penalised SC objective function using pseudo-treated units
  results_lambda <- optimx(par,
                           get_hyperparam_loss_penalised_sc,
                           lower = lower_bound_lambda,
                           upper = Inf,
                           Y_controls_pre = Y_controls_pre,
                           Y_controls_post = Y_controls_post,
                           method = "L-BFGS-B") # Could maybe try a cv.glmnet or grid search here
  
  # Re-order results to find best solution (assume using minimisation in optimx)
  results_lambda <- results_lambda[order(results_lambda$value,decreasing=FALSE),]
  
  # Retain optimal solution (first row in re-ordered results_opt)
  results_lambda <- results_lambda[1,]
  
  # Extract optimal hyperparameters (if NA, return initial hyperparams)
  lambda_opt <- ifelse(!is.na(results_lambda[["lambda"]]), as.numeric(results_lambda[["lambda"]]), lambda_init)
  
  # Re-run optimisation to get optimal synthetic control weights for treated unit -------------
  # at the optimal level of the hyperparameters
  
  # Run PSC algorithm at optimal parameters on the true treated unit during the pre-treatment period
  W_opt <- get_penalised_sc_weights(Y_treated_pre,
                                    Y_controls_pre,
                                    lambda_opt)
  
  # Set mu_opt (equal to 0 by design)
  mu_opt <- 0
  
  # Return list of final outputs
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "lambda_opt" = lambda_opt)
  
  return(results)
  
}