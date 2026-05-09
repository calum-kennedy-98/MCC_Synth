#' Merge MCC Mortality Data with LFS Air Pollution Estimates
#'
#' @description
#' Left-joins LFS (landscape fire smoke) PM2.5 pollution estimates onto the
#' cleaned MCC mortality dataset by city and date. Cities for which all fire
#' PM2.5 estimates are missing are dropped. A non-fire PM2.5 column is derived
#' as the difference between total and fire PM2.5. Rolling moving-average
#' columns are appended for both mortality and PM2.5 variables using
#' \code{\link[slider]{slide_index_dbl}}.
#'
#' @param data_mcc A data frame of cleaned MCC observations, as returned by
#'   \code{\link{get_data_mcc}}. Must contain \code{column_label} and
#'   \code{date} columns for the join.
#' @param path_data_lfs Character string. Full file path to the LFS pollution
#'   \code{.rds} file containing columns \code{column_label}, \code{date},
#'   \code{pred_fire_PM25}, and \code{pred_total_PM25}.
#' @param n_lags_all Integer. Number of lag days used to compute the
#'   mortality moving average (\code{all_ma}).
#' @param n_lags_fire_PM25 Integer. Number of lag days used to compute the
#'   fire PM2.5 and non-fire PM2.5 moving averages.
#' @param id_var Bare (unquoted) name of the unit (city) identifier column
#'   used as the grouping variable when computing moving averages (tidy-eval).
#'
#' @return A tibble containing all columns from \code{data_mcc} plus the LFS
#'   pollution columns, \code{pred_nonfire_PM25}, \code{all_ma},
#'   \code{pred_fire_PM25_ma}, and \code{pred_nonfire_PM25_ma}. Cities with no
#'   valid fire PM2.5 observations are excluded.

# Define function to merge MCC and LFS air pollution data ----------------------

merge_data_mcc_lfs <- function(data_mcc,
                               path_data_lfs,
                               n_lags_all,
                               n_lags_fire_PM25,
                               id_var){
  
  # Load LFS air pollution data from path
  data_lfs <- readRDS(path_data_lfs)
  
  # Left join LFS data to MCC using city and date identifier
  data_mcc_lfs <- left_join(
    data_mcc,
    data_lfs,
    by = c("column_label",
           "date")
    ) %>%
    
    # Filter cities where all fire PM2.5 estimates are missing
    filter(
      any(!is.na(pred_fire_PM25)),
      .by = column_label
    ) %>%
    
    # Mutate new column with estimated non-fire PM2.5 levels
    mutate(
      pred_nonfire_PM25 = pred_total_PM25 - pred_fire_PM25
      ) %>%
    
    # Mutate moving averages of pollution and mortality variables
    mutate(
      all_ma = slide_index_dbl(all, date, mean, .before = days(n_lags_all)),
      pred_fire_PM25_ma = slide_index_dbl(pred_fire_PM25, date, mean, .before = days(n_lags_fire_PM25)),
      pred_nonfire_PM25_ma = slide_index_dbl(pred_nonfire_PM25, date, mean, .before = days(n_lags_fire_PM25)),
      .by = {{id_var}}
    )
  
  return(data_mcc_lfs)
}