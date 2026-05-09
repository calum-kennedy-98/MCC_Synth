#' Solve for Optimal Penalised Synthetic Control Weights
#'
#' @description
#' Finds the convex combination of control unit pre-treatment outcomes that
#' best approximates the treated unit's pre-treatment outcomes, subject to an
#' L1 penalty on pairwise discrepancies between the treated unit and each
#' control. The quadratic programme is solved using \code{\link[LowRankQP]{LowRankQP}}.
#' Very small weights are subsequently trimmed and the remaining weights are
#' rescaled to sum to one via \code{\link{rescale_small_weights}}. Adapted
#' from L'Hour (2021).
#'
#' @param X1 Numeric vector of length \eqn{m}. Pre-treatment outcome series
#'   (or predictor vector) for the treated unit.
#' @param X0 Numeric matrix of dimensions \eqn{m \times n}. Pre-treatment
#'   outcome series (or predictor matrix) for the \eqn{n} control units.
#' @param lambda Numeric scalar (\eqn{\geq 0}). L1 penalty parameter
#'   controlling the degree to which the algorithm discourages placing weight
#'   on controls that are dissimilar to the treated unit.
#'
#' @return A numeric vector of length \eqn{n} containing the optimal
#'   penalised synthetic control weights. Weights sum to one and are
#'   non-negative.
#'
#' @references
#' Abadie, A. & L'Hour, J. (2021). A penalized synthetic control estimator
#' for disaggregated data. \emph{Journal of the American Statistical
#' Association}, 116(536), 1817–1834.
get_penalised_sc_weights <- function(X1, X0, lambda){
  
  # Extract number of control units
  n_controls <- ncol(X0)
  
  # Set up inputs for quadratic programming algorithm
  pairwise_discrepancies <- diag(t(X0 - matrix(rep(1, n_controls), ncol = n_controls) %x% X1) %*% (X0 - matrix(rep(1, n_controls), ncol = n_controls) %x% X1))
  H <- 2*t(X0) %*% X0
  d <- t(-2 * t(X0) %*% X1 + lambda * pairwise_discrepancies)
  
  # Find optimal weights using LowRankQP
  res <- LowRankQP(Vmat = H,
                   dvec = d,
                   Amat = matrix(1, ncol = n_controls),
                   bvec = 1,
                   uvec = rep(1,n_controls), 
                   method = "LU")
  
  # Get optimal weights
  W_opt <- res$alpha
  
  # Trim very small weights and rescale
  W_opt <- rescale_small_weights(W_opt,
                                 tolerance = 1e-6,
                                 scale = TRUE)
  
  return(W_opt)
  
}