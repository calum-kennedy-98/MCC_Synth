#' Generate Simulated Outcome Vectors from a Low-Rank Factor Model with AR(2) Errors
#'
#' @description
#' Produces \code{n_sims} independent draws of simulated untreated potential
#' outcomes using a factor model DGP. The systematic component \eqn{L} is
#' estimated from the real data via \code{\link{estimate_factor_model}} at the
#' specified rank. Error matrices are drawn from a multivariate normal
#' distribution whose covariance is estimated from the empirical AR(2)
#' correlation structure (via \code{\link{fit_ar_2}} and
#' \code{\link{ar2_correlation_matrix}}) and scaled by location-specific
#' standard deviations. Negative simulated outcomes are set to zero. The
#' approach follows Arkhangelsky et al. (2021).
#'
#' @param n_sims Integer. Number of simulation replicates to generate.
#' @param data A data frame in long format, used to estimate the factor model
#'   parameters. Rows must be sorted by unit then time.
#' @param unit_id_var Bare (unquoted) name of the unit identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param outcome_var Bare (unquoted) name of the outcome variable (tidy-eval).
#' @param week_id_var Bare (unquoted) name of the week identifier column
#'   (tidy-eval; accepted for API consistency, not used here).
#' @param treated_var Bare (unquoted) name of the treatment indicator column
#'   (tidy-eval; accepted for API consistency, not used here).
#' @param rank Integer. Rank of the factor model.
#'
#' @return A list of \code{n_sims} integer vectors, each of length
#'   \eqn{n \times T}, containing simulated outcomes ordered by unit then time.
#'
#' @references
#' Arkhangelsky, D., Athey, S., Hirshberg, D.A., Imbens, G.W. & Wager, S.
#' (2021). Synthetic difference-in-differences. \emph{American Economic
#' Review}, 111(12), 4088–4118.
make_list_sim_data_factor_model <- function(n_sims,
                                        data,
                                        unit_id_var,
                                        time_id_var,
                                        outcome_var,
                                        week_id_var,
                                        treated_var,
                                        rank){
  
  # Extract n_units and n_periods
  n_units <- length(pull(distinct(data, {{unit_id_var}})))
  n_periods <- length(pull(distinct(data, {{time_id_var}})))
  
  # Estimate low-rank factor model and extract systematic/error components
  results_decomposition <- estimate_factor_model(data,
                                                 unit_id_var = {{unit_id_var}},
                                                 time_id_var = {{time_id_var}},
                                                 outcome_var = {{outcome_var}},
                                                 rank = rank)
  
  # Extract systematic component
  L <- results_decomposition$L
  
  # Estimate AR(2) coefficients on error matrix
  E <- results_decomposition$E
  ar2_coef <- fit_ar_2(E)
  
  # Estimate correlation matrix from AR(2) model and estimate diagonal SD matrix to scale errors by location
  cor_mat <- ar2_correlation_matrix(ar2_coef, n_periods)
  sd_mat <- diag(apply(E, 1, sd, na.rm = TRUE))
  
  # Generate list of vectors with outcomes simulated from latent factor model with rank == `rank'
  list_outcome_sim_factor_model <- future_map(seq_len(n_sims), function(i){
    
    # Generate error matrix using AR(2) correlation matrix
    err_mat <- rmvnorm(n_units, sigma = cor_mat)
    
    # Scale by location-specific standard deviation in the errors
    err_mat_scaled <- sd_mat %*% err_mat 
    
    # Simulate outcome matrix from factor model
    mat_outcome_pred_factor_model <- L + err_mat_scaled
    
    # Set negative values to 0
    mat_outcome_pred_factor_model <- pmax(mat_outcome_pred_factor_model, 0)
    
    # Transform to vector
    outcome_pred_factor_model <- round(as.vector(t(mat_outcome_pred_factor_model)))
    
    return(outcome_pred_factor_model)
    
  },
  .options = furrr_options(seed = TRUE))
  
  return(list_outcome_sim_factor_model)
  
}