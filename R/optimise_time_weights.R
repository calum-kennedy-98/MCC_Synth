#' Optimise Time-Period Weights for Synthetic Control
#'
#' @description
#' Finds optimal time-period weights for use in the synthetic difference-in-
#' differences (SDiD) framework. Outcomes are first de-meaned across units
#' within each time period. The weights are chosen to minimise the discrepancy
#' between the weighted pre-treatment average of control outcomes and the
#' post-treatment average of control outcomes, using \code{\link[LowRankQP]{LowRankQP}}.
#' The resulting weights sum to one and are non-negative.
#'
#' @param Y_controls Numeric matrix of dimensions \eqn{T \times N}, with rows
#'   indexing time periods and columns indexing control units.
#' @param post Integer or logical vector of length \eqn{T}. Post-treatment
#'   indicator (0 = pre-treatment, 1 = post-treatment).
#'
#' @return A numeric vector of length \eqn{T_{\text{pre}}} containing the
#'   optimal time-period weights.

# Define function find optimal synth -------------------------------------------

optimise_time_weights <- function(Y_controls,
                                  post){
  
  # De-mean outcomes
  Y_controls <- t(apply(Y_controls, 1, function(row){row - mean(row)}))
  
  # Separate Y_controls matrix into Y_pre and Y_post
  n_pre <- sum(post == 0)
  Y_controls_pre <- Y_controls[1:n_pre,]
  Y_controls_post <- Y_controls[(n_pre+1):length(post),]
  
  # Extract number of time periods
  n_periods <- nrow(Y_controls_pre)
  
  # Get mean outcomes in post-treatment period
  Y_bar_controls_post <- colMeans(Y_controls_post)
  
  # Set up inputs for Low rank QP
  H <- Y_controls_pre %*% t(Y_controls_pre)
  d <- t(Y_controls_pre %*% Y_bar_controls_post)
  
  # Low rank QP
  res <- LowRankQP(Vmat = H,
                   dvec = d,
                   Amat = matrix(1, ncol = n_periods),
                   bvec = 1,
                   uvec = rep(1,n_periods), 
                   method = "LU")
  
  # Get optimal weights
  W_opt <- res$alpha
  
  return(W_opt)
  
}