#' Leave-One-Out Cross-Validation Loss for the Penalised Synthetic Control Lambda
#'
#' @description
#' Evaluates the leave-one-out (LOO) cross-validation loss for a given value
#' of the L1 penalty parameter \eqn{\lambda} in the penalised synthetic
#' control (PSC) estimator of Abadie & L'Hour (2021). Each control unit is
#' held out as a pseudo-treated unit; optimal PSC weights are found via
#' \code{\link{get_penalised_sc_weights}}, post-treatment outcomes are
#' predicted, and the mean squared prediction error (MSPE) is recorded. The
#' function returns the average MSPE across all pseudo-treated units. Intended
#' for use as the objective passed to a numerical optimiser when selecting
#' \eqn{\lambda}.
#'
#' @param par Numeric scalar. The candidate value of the L1 penalty parameter
#'   \eqn{\lambda} (\eqn{\geq 0}).
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}, containing pre-treatment outcome series
#'   for all \eqn{N} control units.
#' @param Y_controls_post Numeric matrix of dimensions
#'   \eqn{T_{\text{post}} \times N}, containing post-treatment outcome series
#'   for all \eqn{N} control units.
#'
#' @return A scalar numeric value: the average LOO-CV MSPE across all
#'   pseudo-treated control units.
#'
#' @references
#' Abadie, A. & L'Hour, J. (2021). A penalized synthetic control estimator
#' for disaggregated data. \emph{Journal of the American Statistical
#' Association}, 116(536), 1817–1834.
get_hyperparam_loss_penalised_sc <- function(par,
                                             Y_controls_pre,
                                             Y_controls_post){
  
  # Extract starting value of hyperparameter
  lambda <- par
  
  # Generate list of integers to iteratively define treated unit
  n_controls <- seq(1:ncol(Y_controls_pre))
  
  loss_pseudo_controls <- lapply(n_controls, function(x){
    
    # Generate pseudo treatment group for pre/post treatment
    pseudo_Y_treated_pre <- Y_controls_pre[,x]
    pseudo_Y_treated_post <- Y_controls_post[,x]
    
    # Generate pseudo-control group for pre/post treatment
    pseudo_Y_controls_pre <- Y_controls_pre[,-x]
    pseudo_Y_controls_post <- Y_controls_post[,-x]
    
    # Find optimal penalised SC weights
    W_opt <- get_penalised_sc_weights(pseudo_Y_treated_pre,
                                      pseudo_Y_controls_pre,
                                      lambda)
    
    # Get post-treatment predictions
    pseudo_Y_treated_post_hat <- pseudo_Y_controls_post %*% W_opt
    
    # Generate mean squared difference from true pseudo post-treatment outcomes
    mean_squared_pred_error <- mean((pseudo_Y_treated_post - pseudo_Y_treated_post_hat)^2)
    
    # Return mean squared prediction error for each pseudo-treated unit in list
    return(mean_squared_pred_error)
    
  })
  
  # Take mean of mean_squared_error over all pseudo-treated units to get overall error estimate
  loss_lambda <- mean(unlist(loss_pseudo_controls))
  
  return(loss_lambda)
  
}