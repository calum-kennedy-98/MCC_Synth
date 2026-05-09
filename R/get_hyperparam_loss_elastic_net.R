#' Leave-One-Out Cross-Validation Loss for Elastic Net Hyperparameters
#'
#' @description
#' Evaluates the leave-one-out (LOO) cross-validation loss for a given pair of
#' elastic net hyperparameters (\eqn{\lambda}, \eqn{\alpha}). Each control
#' unit is held out in turn as a pseudo-treated unit; elastic net weights are
#' estimated on the remaining controls during the pre-treatment period, and the
#' mean squared prediction error (MSPE) on the pseudo-post-treatment outcomes
#' is computed. The function returns the average MSPE across all pseudo-treated
#' units. Intended for use as the objective passed to a numerical optimiser
#' (e.g. \code{\link[optimx]{optimx}}) when tuning the Doudchenko–Imbens
#' DIFP estimator.
#'
#' @param par Named numeric vector with elements \code{"lambda"} (penalty
#'   strength, \eqn{\geq 0}) and \code{"alpha"} (mixing parameter,
#'   \eqn{[0, 1]}, where 0 = ridge and 1 = lasso).
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}, containing pre-treatment outcome series
#'   for all \eqn{N} control units.
#' @param Y_controls_post Numeric matrix of dimensions
#'   \eqn{T_{\text{post}} \times N}, containing post-treatment outcome series
#'   for all \eqn{N} control units.
#'
#' @return A scalar numeric value: the average LOO-CV MSPE across all
#'   pseudo-treated control units.
get_hyperparam_loss_elastic_net <- function(par,
                                           Y_controls_pre,
                                           Y_controls_post){
  
  # Extract values of hyperparameters from par vector
  lambda <- par["lambda"]
  alpha <- par["alpha"]
  
  # Generate list of integers to iteratively define treated unit
  n_controls <- seq(1:ncol(Y_controls_pre))
  
  # Find optimal synthetic control weights for each pseudo-treated unit in pre-treatment
  # period, generate predictions in post-treatment period, and return average squared
  # prediction error across all pseudo-treated and post-treatment periods
  loss_pseudo_controls <- map_dbl(n_controls, function(x){
    
    # Generate pseudo treatment group for pre/post treatment
    pseudo_Y_treated_pre <- Y_controls_pre[,x]
    pseudo_Y_treated_post <- Y_controls_post[,x]
    
    # Generate pseudo-control group for pre/post treatment
    pseudo_Y_controls_pre <- Y_controls_pre[,-x]
    pseudo_Y_controls_post <- Y_controls_post[,-x]
    
    # Get predictions on pseudo-post-treatment outcomes
    pseudo_Y_treated_post_hat <- predict_Y_post_elastic_net(Y_treated_pre = pseudo_Y_treated_pre,
                                                   Y_treated_post = pseudo_Y_treated_post,
                                                   Y_controls_pre = pseudo_Y_controls_pre,
                                                   Y_controls_post = pseudo_Y_controls_post,
                                                   alpha = alpha,
                                                   lambda = lambda)
    
    # Generate mean squared difference from true pseudo post-treatment outcomes
    mean_squared_pred_error <- mean((pseudo_Y_treated_post - pseudo_Y_treated_post_hat)^2)
    
    # Return mean squared prediction error for each pseudo-treated unit in list
    return(mean_squared_pred_error)
    
  })
  
  # Take mean of mean_squared_error over all pseudo-treated units to get overall error estimate
  loss_hyperparams <- mean(loss_pseudo_controls)
  
  return(loss_hyperparams)
  
}