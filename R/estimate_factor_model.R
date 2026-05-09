#' Estimate a Low-Rank Factor Model
#'
#' @description
#' Decomposes an outcome matrix into a low-rank systematic component \eqn{L}
#' and a residual error matrix \eqn{E} using singular value decomposition (SVD).
#' Adapted from the \href{https://github.com/synth-inference/synthdid}{SynthDID}
#' GitHub repository.
#'
#' @param data A data frame in long format containing the outcome variable and
#'   unit and time identifiers. Rows must be sorted by unit then time.
#' @param unit_id_var Bare (unquoted) name of the column identifying units
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the column identifying time
#'   periods (tidy-eval).
#' @param outcome_var Bare (unquoted) name of the outcome variable (tidy-eval).
#' @param rank Integer. The number of singular vectors retained in the
#'   decomposition, i.e. the rank \eqn{r} of the factor model.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{L}{An \eqn{n \times T} matrix containing the low-rank systematic
#'     component.}
#'   \item{E}{An \eqn{n \times T} residual error matrix
#'     (\code{outcome\_matrix - L}).}
#' }
estimate_factor_model <- function(data,
                                  unit_id_var,
                                  time_id_var,
                                  outcome_var,
                                  rank){
  
  # Extract n_units and n_periods
  n_units <- length(pull(distinct(data, {{unit_id_var}})))
  n_periods <- length(pull(distinct(data, {{time_id_var}})))
  
  # Extract outcomes and reshape to n_units x n_periods matrix
  mat_Y <- t(matrix(pull(data, {{outcome_var}}), nrow = n_periods))
  
  # Generate singular value decomposition of Y and extract first `rank' unit/time factors
  svd_Y <- svd(mat_Y)
  factor_unit <- as.matrix(svd_Y$u[,1:rank])
  factor_time <- as.matrix(svd_Y$v[,1:rank])
  magnitude <- svd_Y$d[1:rank]
  
  # Generate systematic component L
  L <- factor_unit %*% diag(magnitude, nrow = rank, ncol = rank) %*% t(factor_time)
  
  # Generate error matrix
  E <- mat_Y - L
  
  # Return results
  return(list(L = L, E = E))
  
}