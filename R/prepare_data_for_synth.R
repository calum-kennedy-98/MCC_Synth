#' Prepare Panel Data for Synthetic Control Optimisation
#'
#' @description
#' Extracts and structures the matrices and vectors required by the
#' \code{objective_function_*} helpers. Subsets the input panel to the
#' \code{n_periods_pre + n_periods_post} window centred on the first treated
#' period, separates treated and control unit outcomes into vectors and
#' matrices, computes pre-treatment means (used for de-meaning estimators),
#' and constructs the post-treatment indicator. This function is called
#' internally by \code{\link{optimise_synth}}.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param demean_outcomes Logical. Accepted for API consistency; validated
#'   but not applied here (de-meaning occurs in \code{\link{optimise_synth}}).
#' @param denoise_outcomes Logical. Accepted for API consistency; validated
#'   but not applied here.
#' @param n_periods_pre Integer. Number of pre-treatment periods to retain.
#' @param n_periods_post Integer. Number of post-treatment periods to retain.
#' @param outcome_var Bare (unquoted) name of the outcome variable (tidy-eval).
#' @param treated_id_var Bare (unquoted) name of the binary treated-unit
#'   indicator (1 = treated; tidy-eval).
#' @param treated_time_var Bare (unquoted) name of the binary post-treatment
#'   period indicator (1 = post; tidy-eval).
#' @param time_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param spline_df Integer or \code{NULL}. Accepted for API consistency;
#'   not used within this function.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{Y_treated}{Numeric vector. Full observed outcome series for the
#'     treated unit (pre + post).}
#'   \item{Y_controls}{Numeric matrix (\eqn{T \times N}). Full outcome
#'     series for all control units.}
#'   \item{Y_treated_bar_pre_treatment}{Numeric scalar. Pre-treatment mean
#'     of the treated unit's outcomes.}
#'   \item{Y_controls_bar_pre_treatment}{Numeric vector of length \eqn{N}.
#'     Pre-treatment means for each control unit.}
#'   \item{post}{Integer vector (0/1). Post-treatment period indicator.}
#'   \item{first_treated_period}{The time index of the first treated period.}
#' }
prepare_data_for_synth <- function(data,
                                   demean_outcomes,
                                   denoise_outcomes,
                                   n_periods_pre,
                                   n_periods_post,
                                   outcome_var,
                                   treated_id_var,
                                   treated_time_var,
                                   time_var,
                                   spline_df){
  
  if(!is.logical(demean_outcomes) | !is.logical(denoise_outcomes)) stop("Please set `demean_outcomes' and `denoise_outcomes' equal to TRUE or FALSE")
  
  # Extract dimension of time variable
  n_periods <- n_periods_pre + n_periods_post
  
  # Generate indicator for post-treatment period
  post <- c(rep(0, n_periods_pre), rep(1, n_periods_post))
  
  # Extract first treated period and subset data to pre- / post-treatment intervals 
  # based on `n_periods_pre` and `n_periods_post`
  first_treated_period <- min(data %>%
                                filter({{treated_time_var}} == 1) %>%
                                pull({{time_var}})
  )
  
  data <- data %>%
    filter(
      between(
        {{time_var}}, 
        first_treated_period - n_periods_pre, 
        first_treated_period + n_periods_post - 1
      )
    )
  
  # Extract vector of observed outcomes for treated unit
  Y_treated <- data %>% 
    filter({{treated_id_var}} == 1) %>%
    pull({{outcome_var}}) %>% 
    as.numeric()
  
  # Extract matrix of outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y_controls <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull({{outcome_var}}) %>%
    matrix(nrow = n_periods)
  
  # Extract pre-treatment mean in treated and control units (necessary for de-meaning estimators)
  Y_treated_bar_pre_treatment <- mean(Y_treated[1:n_periods_pre])
  Y_controls_bar_pre_treatment <- colMeans(Y_controls[1:n_periods_pre,])
  
  # Return results
  out <- list("Y_treated" = Y_treated,
              "Y_controls" = Y_controls,
              "Y_treated_bar_pre_treatment" = Y_treated_bar_pre_treatment,
              "Y_controls_bar_pre_treatment" = Y_controls_bar_pre_treatment,
              "post" = post,
              "first_treated_period" = first_treated_period)
  
  return(out)
  
}