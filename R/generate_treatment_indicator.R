#' Generate a Treatment Indicator for Acute Fire PM2.5 Exposure Episodes
#'
#' @description
#' Creates a binary \code{treated} column that flags city-days affected by an
#' acute landscape-fire smoke (LFS) PM2.5 episode. Treatment onset is triggered
#' when the moving-average fire PM2.5 concentration exceeds a data-driven
#' threshold for \code{n_days_above_threshold} consecutive days. Treatment
#' ends once the moving average falls back below the threshold for
#' \code{n_days_below_threshold} consecutive days. The threshold is computed as
#' the \code{quantile_threshold} quantile of the pooled empirical distribution
#' of raw fire PM2.5 across all units.
#'
#' @param data A data frame containing at minimum the unit identifier, a
#'   moving-average fire PM2.5 variable, and a raw fire PM2.5 variable. Data
#'   must be sorted by unit then date.
#' @param id_var Bare (unquoted) name of the unit (city) identifier column
#'   (tidy-eval).
#' @param fire_pm_ma_var Bare (unquoted) name of the fire PM2.5 moving-average
#'   variable used to determine within-episode treated status (tidy-eval).
#' @param fire_pm_var Bare (unquoted) name of the raw fire PM2.5 variable used
#'   to compute the quantile threshold (tidy-eval).
#' @param quantile_threshold Numeric in \eqn{[0, 1]}. Quantile of the pooled
#'   fire PM2.5 distribution used as the exposure threshold.
#' @param n_days_above_threshold Integer. Number of consecutive days the
#'   moving-average must exceed the threshold before treatment onset is
#'   declared.
#' @param n_days_below_threshold Integer. Number of consecutive days the
#'   moving-average must remain below the threshold before treatment is
#'   considered to have ended.
#'
#' @return The input data frame with an additional integer column \code{treated}
#'   (1 = treated, 0 = untreated) and an intermediate logical column
#'   \code{above_threshold}.
generate_treatment_indicator <- function(data,
                                         id_var,
                                         fire_pm_ma_var,
                                         fire_pm_var,
                                         quantile_threshold,
                                         n_days_above_threshold,
                                         n_days_below_threshold){
  
  # Generate absolute threshold above which to define acute exposure
  fire_PM25_threshold <- quantile(data[[fire_pm_var]], probs = c(quantile_threshold), na.rm = TRUE)
  
  data_with_treatment <- data %>%
    
    # Generate indicator for LFS PM2.5 above 99th percentile of empirical distribution
    mutate(
      above_threshold = ifelse({{fire_pm_ma_var}} > fire_PM25_threshold, 1, 0),
      .by = {{id_var}}
      ) %>%
    
    # Create treatment indicator
    mutate(
      treated = {
        
        # Initially, generate zero vector for treatment
        n <- n()
        treated_vec <- rep(0, n)
        
        # Begin with "in_treatment" set to FALSE and zero days below 99th percentile threshold
        in_treatment <- FALSE
        below_threshold_counter <- 0
        
        # For each unit, execute following loop:
        for (i in seq_len(n)) {
          
          # If not currently in treatment, check if
            # 1. day index is larger than n_days_above threshold (rules out exposures in first few days of sample)
            # 2. Previous n_days_above_threshold values are all above the threshold
          # If yes, change in_treatment to TRUE and set treated == 1
          if (!in_treatment) {
            if (i >= n_days_above_threshold && all(above_threshold[(i - n_days_above_threshold + 1):i] == 1)) {
              in_treatment <- TRUE
              treated_vec[i] <- 1
            }
            
            # Else if currently in treatment, check if
              # 1. Current predicted LFS PM2.5 is below the threshold
            # if yes, add 1 to the below_threshold_counter variable
            # if no, reset below_threshold_counter to zero
            # Set treated == 1
          } else {
            if (pred_fire_PM25_ma[i] < fire_PM25_threshold) {
              below_threshold_counter <- below_threshold_counter + 1
            } else {
              below_threshold_counter <- 0
            }
            
            treated_vec[i] <- 1
            
            # Lastly, check if below_threshold_counter larger than n_days_below_threshold
            # If so, set in_treatment back to FALSE and reset counter
            if (below_threshold_counter >= n_days_below_threshold) {
              in_treatment <- FALSE
              below_threshold_counter <- 0
            }
          }
        }
        treated_vec
      },
      .by = {{id_var}}
    )
  
  return(data_with_treatment)
}
