#' Summarise Simulation Performance Metrics Across SC Methods and DGPs
#'
#' @description
#' Computes simulation performance metrics from the output of
#' \code{\link{extract_tau_hat_synth_results}} for a single DGP type. For
#' each SC method, the function reports: (1) absolute bias and standard
#' deviation of the estimated average treatment effect on the treated (ATT),
#' and (2) average absolute bias and standard deviation of the per-period
#' treatment effect estimates across the post-treatment window. Results are
#' returned as a single summary tibble with a \code{dgp_type} column.
#'
#' @param data_synth_results A data frame as returned by
#'   \code{\link{extract_tau_hat_synth_results}}, containing at minimum columns
#'   \code{post}, \code{method}, \code{model_run}, \code{t}, and the variables
#'   named in \code{tau_hat_var} and \code{tau_var}.
#' @param tau_hat_var Bare (unquoted) name of the estimated treatment effect
#'   column (tidy-eval).
#' @param tau_var Bare (unquoted) name of the true treatment effect column
#'   (tidy-eval).
#' @param post_var Bare (unquoted) name of the binary post-treatment period
#'   indicator column (tidy-eval).
#' @param dgp_type Character string. Label for the data-generating process.
#'   Must be one of \code{"negative_binomial"} or \code{"factor"}.
#'
#' @return A tibble with one row per SC method containing columns:
#'   \code{method}, \code{abs_bias_att}, \code{sd_att},
#'   \code{avg_abs_bias_per_period}, \code{avg_sd_per_period},
#'   \code{dgp_type}.
make_summary_table_synth_diagnostics <- function(data_synth_results,
                                                 tau_hat_var,
                                                 tau_var,
                                                 post_var,
                                                 dgp_type){
  
  # Checks
  if(!dgp_type %in% c("negative_binomial", "factor")) stop("Please set dgp_type equal to 'negative_binomial' or 'factor'")
  
  # Generate summary stats for ATT estimand
  summary_stats_att <- data_synth_results %>% 
    
    # Filter observations from post-treatment period
    filter({{post_var}} == 1) %>%
    
    # Estimate ATT by model run and method
    summarise(att = mean({{tau_hat_var}} - {{tau_var}}, na.rm = TRUE),
              .by = c(model_run, method)) %>%
    
    # Generate estimates of bias and variance of ATT parameter
    summarise(abs_bias_att = abs(mean(att, na.rm = TRUE)),
              sd_att = sd(att, na.rm = TRUE),
              .by = method)
  
  # Generate summary stats for per-period effects
  summary_stats_per_period <- data_synth_results %>%
    
    # Filter observations from post-treatment period
    filter({{post_var}} == 1) %>%
    
    # Estimate bias and variance in per-period effects
    summarise(abs_bias_per_period = abs(mean({{tau_hat_var}} - {{tau_var}}, na.rm = TRUE)),
              sd_per_period = sd({{tau_hat_var}} - {{tau_var}}, na.rm = TRUE),
              .by = c(t, method)) %>%
    
    # Collapse by averaging average bias and variance over time
    summarise(avg_abs_bias_per_period = mean(abs_bias_per_period),
              avg_sd_per_period = mean(sd_per_period),
              .by = method)
  
  # Combine into overall summary table
  tbl_summary_stats <- left_join(summary_stats_att,
                                 summary_stats_per_period,
                                 by = "method") %>%
    
    mutate(dgp_type = dgp_type)

  # # Generate summary statistics by method and DGP
  # tbl_summary_stats <- data_synth_results %>%
  #   
  #   # Filter observations from post-treatment period
  #   filter({{post_var}} == 1) %>%
  #   
  #   # Estimate bias in per-period effects
  #   mutate(abs_error_per_period = abs(mean({{tau_hat_var}} - {{tau_var}}, na.rm = TRUE)),
  #          .by = c(t, method)) %>%
  #   
  #   # Estimate error and squared error by method and model run
  #   summarise(error = mean({{tau_hat_var}} - {{tau_var}}),
  #             abs_error_per_period = mean(abs_error_per_period),
  #             squared_error = mean(({{tau_hat_var}} - {{tau_var}})^2),
  #             .by = c(method,
  #                     model_run)) %>%
  #   
  #   # Generate summary stats
  #   summarise(per_period_rmse = sqrt(mean(squared_error, na.rm = TRUE)),
  #             agg_rmse = sqrt(mean(error^2, na.rm = TRUE)),
  #             avg_per_period_bias = mean(abs_error_per_period, na.rm = TRUE),
  #             agg_bias = abs(mean(error, na.rm = TRUE)),
  #             .by = method) %>%
  #   
  #   # Add column for DGP type
  #   mutate(dgp_type = dgp_type)
  
  return(tbl_summary_stats)
  
}