#' Elastic Net Synthetic Control: Fit and Predict Post-Treatment Outcomes
#'
#' @description
#' Fits an elastic net model (\code{\link[glmnet]{glmnet}}) using the
#' pre-treatment control outcomes as features and the pre-treatment treated
#' outcomes as the response, at fixed hyperparameters \code{alpha} and
#' \code{lambda} (no internal cross-validation). Returns out-of-sample
#' predictions for the post-treatment period. This function is used as a
#' building block inside the LOO cross-validation loop in
#' \code{\link{get_hyperparam_loss_elastic_net}}.
#'
#' @param Y_treated_pre Numeric vector of length \eqn{T_{\text{pre}}}. Observed
#'   pre-treatment outcomes for the treated (or pseudo-treated) unit.
#' @param Y_treated_post Numeric vector of length \eqn{T_{\text{post}}}. Observed
#'   post-treatment outcomes for the treated unit (used only to construct
#'   the prediction target when this function is called from LOO-CV).
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}. Pre-treatment outcomes for the control
#'   units (used as predictors in the elastic net).
#' @param Y_controls_post Numeric matrix of dimensions
#'   \eqn{T_{\text{post}} \times N}. Post-treatment outcomes for the control
#'   units (used as new data for prediction).
#' @param alpha Numeric in \eqn{[0, 1]}. Elastic net mixing parameter
#'   (0 = ridge, 1 = lasso).
#' @param lambda Numeric (\eqn{> 0}). Regularisation penalty strength.
#'
#' @return A numeric matrix of dimensions \eqn{T_{\text{post}} \times 1}
#'   containing the predicted post-treatment outcomes.
predict_Y_post_elastic_net <- function(Y_treated_pre,
                                     Y_treated_post,
                                     Y_controls_pre,
                                     Y_controls_post,
                                     alpha,
                                     lambda){
  
  # Run optimisation using glm net with lambda fixed (i.e. no gradient descent
  # and no iterations over lambda. The reason for this is that in our application
  # we want to choose a lambda which performs best across all pseudo-controls, 
  # so is a higher-level parameter)
  fit <- glmnet(Y_controls_pre, 
                Y_treated_pre, 
                alpha = alpha, 
                lambda = lambda, 
                nlambda = 1)
  
  # Get predictions on pseudo-post-treatment outcomes
  Y_treated_post_hat <- predict(fit, Y_controls_post, type = "response")
  
  return(Y_treated_post_hat)
  
}