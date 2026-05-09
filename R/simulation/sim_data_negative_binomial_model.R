#' Simulate Outcomes from a Fitted Negative Binomial Model
#'
#' @description
#' Draws one realisation of the outcome variable from a fitted
#' \code{glm.nb} model by computing expected values from the linear predictor
#' (on the response scale) and then sampling from the corresponding negative
#' binomial distribution for each observation. Used as the inner function
#' inside the parallelised loop in
#' \code{\link{make_list_data_negative_binomial_model}}.
#'
#' @param data A data frame containing the covariate values at which to
#'   generate predictions from \code{model}.
#' @param model A fitted \code{glm.nb} model object (as returned by
#'   \code{\link{estimate_neg_binomial_model}}).
#'
#' @return A numeric vector of length \code{nrow(data)} containing integer
#'   counts drawn from the negative binomial distribution with the model-
#'   implied mean and dispersion.
sim_data_negative_binomial_model <- function(data,
                                        model){
    
    # Extract model predicted values for the linear link function
    # and take exponential to find expected value of outcome variable distribution 
    outcome_exp_val <- exp(predict(model, data))
    
    # Extract dispersion parameter theta
    theta <- model$theta
    
    # Draw real and counterfactual outcome series from negative binomial distribution
    # with specified mean and variance
    outcome_pred <- as.numeric(unlist(lapply(outcome_exp_val, rnegbin, n = 1, theta = theta)))
  
  return(outcome_pred)
  
}
