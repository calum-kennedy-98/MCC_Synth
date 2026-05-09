#' Difference-in-Differences Counterfactual
#'
#' @description
#' Constructs a difference-in-differences (DiD) counterfactual by selecting
#' the \code{n_controls} control units closest to the treated unit in
#' pre-treatment Euclidean (L2) distance, assigning them equal weights of
#' \eqn{1 / \texttt{n\_controls}}, and estimating an intercept (\code{mu_opt})
#' equal to the mean pre-treatment level difference between the treated unit
#' and the average of the selected controls. Control units outside the selected
#' subset receive a weight of zero.
#'
#' @param Y_treated_pre Numeric vector of length \eqn{T_{\text{pre}}}. Observed
#'   pre-treatment outcome series for the treated unit.
#' @param Y_controls_pre Numeric matrix of dimensions
#'   \eqn{T_{\text{pre}} \times N}. Pre-treatment outcome series for all
#'   \eqn{N} control units.
#' @param n_controls Integer. Number of nearest control units (by L2 norm) to
#'   include in the DiD comparison group.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{W_opt}{Numeric vector of length \eqn{N}. Equal weights
#'     (\eqn{1/\texttt{n\_controls}}) for the selected controls and 0
#'     elsewhere.}
#'   \item{mu_opt}{Numeric scalar. Estimated mean pre-treatment level
#'     difference between the treated unit and the equally-weighted control
#'     average.}
#' }
objective_function_did <- function(Y_treated_pre,
                                   Y_controls_pre,
                                   n_controls){
  
  # Determine best subset of controls to use by taking L2 norm
  l2_norms <- colSums((Y_controls_pre - Y_treated_pre)^2)
  idx <- order(l2_norms)[1:n_controls]
  Y_controls_pre_subset <- Y_controls_pre[, idx, drop = FALSE]
  
  # Estimate optimal mu (average difference between pre-treatment outcomes
  # for average of control units and the treated unit)
  mean_Y_controls_pre <- rowMeans(Y_controls_pre_subset)
  mu_opt <- mean(Y_treated_pre - mean_Y_controls_pre)
  
  # Estimate optimal weights as 1 divided by number of control units (DID sets
  # equal weights by design) for units in the subset, and zero otherwise
  W_opt <- rep(0, ncol(Y_controls_pre))
  W_opt[idx] <- 1 / n_controls
  
  # Store results in list
  results <- list("W_opt" = W_opt,
                  "mu_opt" = mu_opt)
  
  return(results)
  
}