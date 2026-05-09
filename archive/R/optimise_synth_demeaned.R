#' ADH Synthetic Control on De-meaned Outcomes (Intercept-Adjusted)
#'
#' @description
#' Fits the ADH synthetic control on de-meaned pre-treatment outcomes, which
#' is equivalent to allowing an additive intercept shift between the treated
#' and synthetic control units. The approach follows Ferman & Pinto (2021) and
#' Doudchenko & Imbens (2017). Outcomes for each unit are de-meaned by
#' subtracting the unit's own pre-treatment mean before the quadratic programme
#' is solved; the pre-treatment mean of the treated unit is then added back to
#' recover counterfactual predictions on the original scale.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param id_var Bare (unquoted) name of the unit identifier column (tidy-eval).
#' @param outcome_var Bare (unquoted) name of the outcome variable (tidy-eval).
#' @param time_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param treated_id_var Bare (unquoted) name of the binary indicator for the
#'   treated unit (1 = treated, 0 = control; tidy-eval).
#' @param treated_time_var Bare (unquoted) name of the binary indicator for
#'   the post-treatment period (1 = post, 0 = pre; tidy-eval).
#' @param n_periods_pre Integer. Number of pre-treatment periods to include.
#' @param n_periods_post Integer. Number of post-treatment periods to include.
#' @param optimxmethod Character string. Optimisation method passed to
#'   \code{\link[optimx]{optimx}} inside \code{\link[Synth]{synth}}.
#' @param initial_margin Numeric. Initial \code{Margin.ipop} value for the
#'   ipop solver.
#' @param max_attempts Integer. Maximum optimisation re-attempts.
#' @param margin_increment Numeric. Increment added to \code{Margin.ipop}
#'   after each failed attempt.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{data}{The subsetted input data frame.}
#'   \item{Y1}{Numeric vector. Observed outcomes for the treated unit.}
#'   \item{Y1_hat}{Numeric vector. Counterfactual predictions on the original
#'     scale (\code{NA} if optimisation fails).}
#'   \item{W_opt}{Optimal ADH weights (\code{NA} if optimisation fails).}
#'   \item{mu_opt}{Numeric scalar. Pre-treatment mean of the treated unit
#'     (used as the intercept).}
#'   \item{first_treated_period}{The time index of the first treated period.}
#'   \item{method}{Character string \code{"adh_demeaned"}.}
#' }
#'
#' @references
#' Ferman, B. & Pinto, C. (2021). Synthetic controls with imperfect
#' pre-treatment fit. \emph{Quantitative Economics}, 12(4), 1197–1221.
optimise_synth_demeaned <- function(data,
                                    id_var,
                                    outcome_var,
                                    time_var,
                                    treated_id_var,
                                    treated_time_var,
                                    n_periods_pre,
                                    n_periods_post,
                                    optimxmethod,
                                    initial_margin,
                                    max_attempts,
                                    margin_increment){
  
  # Set to dataframe
  data <- as.data.frame(data)
  
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
  
  # Generate numeric ID column for synth
  data <- data %>%
    mutate(
      id_numeric = cur_group_id(),
      .by = {{id_var}}
    )
  
  # Generate vector of pre-treatment time periods - we need to extract the time
  # vector only for one unit, since the panel is balanced by design
  t_vec_pre_treatment <- data %>%
    filter(
      {{treated_time_var}} == 0,
      {{treated_id_var}} == 1
    ) %>%
    pull({{time_var}})
  
  # Extract vector of outcomes for treated unit
  Y1 <- data %>% 
    filter({{treated_id_var}} == 1) %>%
    pull({{outcome_var}}) %>% 
    as.numeric()
  
  # Generate pre-treatment average outcome for treated unit
  Y1_bar_pre_treatment <- mean(Y1[1:length(t_vec_pre_treatment)])
  
  # Store total number of periods 'n_periods'
  n_periods <- length(Y1)
  
  # Extract matrix of outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y0 <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull({{outcome_var}}) %>%
    matrix(nrow = n_periods)
  
  # Extract vector of pre-treatment means for control units
  Y0_bar_pre_treatment <- colMeans(Y0[1:length(t_vec_pre_treatment),])
  
  # Extract matrix of de-meaned outcomes for control units
  Y0_demeaned <- t(t(Y0) - Y0_bar_pre_treatment)
  
  # Enquote variables to pass to dataprep function
  id_numeric_quo <- "id_numeric"
  outcome_var_quo <- as_string(ensym(outcome_var))
  time_var_quo <- as_string(ensym(time_var))
  
  # Generate list of period-specific outcome variables to pass to dataprep
  # Here we assume that we would like to use each specific pre-treatment
  # outcome realisation as a predictor, as opposed to e.g. the average outcome
  # across the whole pre-treatment period
  list_outcome_predictors <- lapply(t_vec_pre_treatment, function(x){
    list(outcome_var_quo, x, "mean")
  })
  
  # Generate ID vectors for treated and untreated units
  treated_id <- data %>%
    filter(
      {{treated_id_var}} == 1
    ) %>%
    distinct(id_numeric) %>%
    pull()
  
  controls_id <- data %>%
    filter(
      {{treated_id_var}} == 0
    ) %>%
    distinct(id_numeric) %>%
    pull()
  
  # De-mean outcome variable using pre-treatment averages
  data_demeaned <- data %>% 
    mutate(
      {{outcome_var}} := {{outcome_var}} - mean({{outcome_var}}[{{treated_time_var}} == 0], na.rm = TRUE),
      .by = {{id_var}}
    )
  
  # Prepare data to pass to synth
  data_prepared <- dataprep(foo = data_demeaned,
                            predictors = NULL,
                            special.predictors = list_outcome_predictors,
                            time.predictors.prior = t_vec_pre_treatment,
                            dependent = outcome_var_quo,
                            unit.variable = id_numeric_quo,
                            time.variable = time_var_quo,
                            treatment.identifier = treated_id,
                            controls.identifier = controls_id,
                            time.optimize.ssr = t_vec_pre_treatment)
  
  # Generate synth object using 'retry_synth' - available in 'utility_functions.R'
  synth_out <- retry_synth(data_prepared, 
                           optimxmethod = optimxmethod,
                           initial_margin = initial_margin,
                           max_attempts = max_attempts,
                           margin_increment = margin_increment)
  
  # If optimisation succeeded, generate relevant outputs
  if(!is.null(synth_out)){
    
    # Extract optimal weights and intercept (mu_opt = pre-treatment average for treated unit)
    W_opt <- synth_out[["solution.w"]]
    mu_opt <- Y1_bar_pre_treatment
    
    # Generate Y1_hat using Y0 and optimal weights
    Y1_hat <- c(mu_opt + Y0_demeaned %*% W_opt)
    
    # If optimisation failed, return NA for missing outputs
  } else {
    
    W_opt <- NA
    mu_opt <- Y1_bar_pre_treatment
    Y1_hat <- rep(NA, n_periods)
    
  }
  
  # Store results in list
  results <- list("data" = data,
                  "Y1" = Y1,
                  "Y1_hat" = Y1_hat,
                  "W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "first_treated_period" = first_treated_period,
                  "method" = "adh_demeaned")
  
  return(results)
  
}