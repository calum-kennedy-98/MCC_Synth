#' Doudchenko–Imbens Elastic Net Penalised Synthetic Control (DIFP)
#'
#' @description
#' Implements the elastic net penalised synthetic control (DIFP) estimator of
#' Doudchenko & Imbens (2017). Hyperparameters \eqn{\alpha} (elastic net
#' mixing) and \eqn{\lambda} (penalty strength) are jointly tuned by
#' minimising the LOO cross-validation MSPE via
#' \code{\link{get_hyperparam_loss_elastic_net}} and
#' \code{\link[optimx]{optimx}} with L-BFGS-B. At the optimal
#' hyperparameters, \code{\link[glmnet]{glmnet}} is fitted on the full
#' pre-treatment data to obtain final weights and an intercept. Unlike the ADH
#' method, weights are unconstrained (can be negative) and an intercept
#' \code{mu_opt} is estimated.
#'
#' @param Y_treated_pre Numeric vector of length \eqn{T_{\text{pre}}}. Observed
#'   pre-treatment outcome series for the treated unit.
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}. Pre-treatment outcome series for the
#'   control units.
#' @param Y_controls_post Numeric matrix of dimensions
#'   \eqn{T_{\text{post}} \times N}. Post-treatment outcome series for the
#'   control units (used only for hyperparameter tuning via LOO-CV).
#' @param alpha_init Numeric in \eqn{[0, 1]}. Initial value for the elastic
#'   net mixing parameter \eqn{\alpha} (0 = ridge, 1 = lasso).
#' @param lambda_init Numeric (\eqn{> 0}). Initial value for the penalty
#'   strength \eqn{\lambda}.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{W_opt}{A sparse coefficient vector (dgCMatrix) of length \eqn{N}.
#'     Elastic net weights for the control units.}
#'   \item{mu_opt}{Numeric scalar. Estimated intercept from the fitted glmnet
#'     model.}
#'   \item{alpha_opt}{Numeric. Optimised \eqn{\alpha} value.}
#'   \item{lambda_opt}{Numeric. Optimised \eqn{\lambda} value.}
#' }
#'
#' @references
#' Doudchenko, N. & Imbens, G.W. (2017). Balancing, regression, difference-in-
#' differences and synthetic control methods: A synthesis. NBER Working Paper
#' No. 22791.
objective_function_difp <- function(Y_treated_pre,
                                    Y_controls_pre,
                                    Y_controls_post,
                                    alpha_init,
                                    lambda_init){
  
  # Initialise starting values for hyperparameters
  alpha_init <- setNames(alpha_init, "alpha")
  lambda_init <- setNames(lambda_init, "lambda")
  
  hyper_params <- c(alpha_init,
           lambda_init)
  
  # Optimise hyperparameters for elastic net objective function using pseudo-treated units
  results_hyperparams <- optimx(hyper_params,
                                get_hyperparam_loss_elastic_net,
                                lower = c(0, 0.1), # Set minimum lambda for stability
                                upper = c(1, Inf),
                                Y_controls_pre = Y_controls_pre,
                                Y_controls_post = Y_controls_post,
                                method = "L-BFGS-B") # Could maybe try a cv.glmnet or grid search here
  
  # Re-order results to find best solution (assume using minimisation in optimx)
  results_hyperparams <- results_hyperparams[order(results_hyperparams$value,decreasing=FALSE),]
  
  # Retain optimal solution (first row in re-ordered results_opt)
  results_hyperparams <- results_hyperparams[1,]
  
  # Extract optimal hyperparameters (if NA, return initial hyperparams)
  alpha_opt <- ifelse(!is.na(results_hyperparams[["alpha"]]), as.numeric(results_hyperparams[["alpha"]]), alpha_init)
  lambda_opt <- ifelse(!is.na(results_hyperparams[["lambda"]]), as.numeric(results_hyperparams[["lambda"]]), lambda_init)
  
  # Re-run optimisation to get optimal synthetic control weights for treated unit -------------
  # at the optimal level of the hyperparameters
  
  # Run glm model at optimal parameters on the true treated unit during the pre-treatment period
  fit <- glmnet(Y_controls_pre, 
                Y_treated_pre, 
                alpha = alpha_opt,
                lambda = lambda_opt,
                nlambda = 1)
  
  # Extract optimal model coefficients
  W_opt <- fit[["beta"]]
  mu_opt <- fit[["a0"]]
  
  # Return list of final outputs
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "alpha_opt" = alpha_opt,
                  "lambda_opt" = lambda_opt)
  
  return(results)
} 