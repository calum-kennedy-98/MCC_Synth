#' Get Period-Specific Confidence Intervals via Conformal Inference
#'
#' @description
#' Constructs period-specific confidence intervals for treatment effects in the
#' empirical application using conformal inference. For each post-treatment
#' period \eqn{t}, an augmented dataset comprising all pre-treatment periods
#' plus period \eqn{t} is formed. A p-value grid is then computed over a range
#' of null hypotheses using \code{\link{get_p_value_grid}}, and the confidence
#' interval is extracted at the desired significance level via
#' \code{\link{get_confidence_interval}}.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param method Character string. Synthetic control method used to generate
#'   counterfactual predictions. Must be one of \code{"ADH"},
#'   \code{"ADH subset"}, \code{"DID"}, \code{"DIFP"}, \code{"PSC"}, or
#'   \code{"1NN matching"}.
#' @param outcome_var Character string. Quoted name of the outcome variable.
#' @param treated_var Character string. Quoted name of the binary treatment
#'   indicator column (1 = treated unit, 0 = control).
#' @param post_var Character string. Quoted name of the binary post-treatment
#'   period indicator (1 = post-treatment, 0 = pre-treatment).
#' @param time_var Character string. Quoted name of the time identifier column.
#' @param id_var Character string. Quoted name of the unit identifier column.
#' @param null_grid Numeric vector. Grid of null hypothesis values
#'   (posited treatment effects) over which p-values are evaluated.
#' @param q Numeric. Degree of the \eqn{L_q} norm used in the conformal test
#'   statistic. Default is \code{1} (L1 norm).
#' @param alpha Numeric. One-sided significance level for the confidence
#'   interval. Default is \code{0.05}.
#'
#' @return A tibble with one row per post-treatment period containing columns
#'   for the lower and upper confidence interval bounds, the period index
#'   \code{t}, and \code{method}.
get_df_conf_int_period_specific <- function(data,
                                            method,
                                            outcome_var,
                                            treated_var,
                                            post_var,
                                            time_var,
                                            id_var,
                                            null_grid,
                                            q = 1,
                                            alpha = 0.05){
  
  # Extract vector of time periods for post treatment period
  post_treatment_periods <- data %>% 
    filter(.data[[post_var]] == 1) %>%
    distinct(.data[[time_var]]) %>%
    pull()
  
  # Extract vector of time periods for post treatment period
  n_periods <- data %>% 
    distinct(.data[[time_var]]) %>% 
    pull() %>% 
    length()
  
  # For each post-treatment time period, generate dataset of all pre-treatment periods plus that time period,
  # and generate confidence interval 
  list_conf_int_period_specific <- lapply(post_treatment_periods, function(t){
    
    # Generate augmented dataset with pre-treatment periods and period t
    data_augmented <- data %>%
      filter(post == 0 | .data[[time_var]] == t)
    
    # Get p-values over the specified grid
    p_val_grid <- get_p_value_grid(data_augmented,
                                   method,
                                   outcome_var,
                                   treated_var,
                                   post_var,
                                   time_var,
                                   id_var,
                                   null_grid,
                                   q = 1)
    
    # Get df with confidence intervals
    df_conf_int <- get_confidence_interval(p_val_grid,
                                           p_val_var = "p_val",
                                           null_hypothesis_var = "null_hypothesis",
                                           alpha = alpha)
    
  })
  
  # Combine results into tibble
  results_df <- bind_rows(list_conf_int_period_specific) %>% mutate(t = (n_periods - length(post_treatment_periods) + 1):n_periods, # Match up time period with main results
                                                                    method = method)
  
  return(results_df)
  
}