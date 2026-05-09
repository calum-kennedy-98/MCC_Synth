#' Aggregate Daily MCC-LFS Data to Weekly Level
#'
#' @description
#' Collapses a daily MCC-LFS panel data frame to the week level. Climatic and
#' pollution variables are summarised by their mean; mortality variables are
#' summed. If all daily values within a week are \code{NA}, the aggregated
#' value is also \code{NA}. A week is flagged as treated (\code{treated = 1})
#' if any day within it carries a treatment indicator of 1 in the daily data.
#' Month and year are set to the modal value (rounded mean) within each week,
#' and \code{date} is set to the first day of the week.
#'
#' @param data_mcc_lfs A data frame of daily MCC-LFS observations containing
#'   at minimum a \code{date} column (class \code{Date}), a \code{treated}
#'   column, and all columns named in \code{climatic_vars},
#'   \code{mortality_vars}, and \code{group_vars}.
#' @param climatic_vars Character vector of column names for climatic and
#'   pollution variables to aggregate by mean.
#' @param mortality_vars Character vector of column names for mortality
#'   variables to aggregate by sum.
#' @param group_vars Unquoted variable names (or a character vector) passed to
#'   \code{.by} in \code{\link[dplyr]{summarise}} to define grouping (e.g.
#'   city and week identifiers).
#'
#' @return A tibble aggregated to the city-week level, containing all
#'   \code{group_vars}, the summarised \code{climatic_vars} and
#'   \code{mortality_vars}, \code{treated}, \code{month}, \code{year},
#'   and \code{date} (first day of the week).
make_data_mcc_lfs_weekly <- function(data_mcc_lfs,
                                     climatic_vars,
                                     mortality_vars,
                                     group_vars){
  
  data_mcc_lfs_weekly <- data_mcc_lfs %>%
    
    summarise(
      
      # Take mean of temperature and pollution variables
      across(all_of(climatic_vars), ~ ifelse(all(is.na(.)), NA, mean(., na.rm = TRUE))),
      
      # Take sum of mortality variables (return NA if all are NA)
      across(all_of(mortality_vars), ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))),
      
      # Set 'treated' = 1 if any observation in that week is treated in daily data
      treated = ifelse(all(treated == 0), 0, 1),
      
      # Extract month, year and date indicators (in case where week split across month/year
      # set equal to month/year with most days in it)
      month = round(mean(month, na.rm = TRUE)),
      year = round(mean(year, na.rm = TRUE)),
      date = min(date, na.rm = TRUE),
      
      .by = group_vars
      )
  
  return(data_mcc_lfs_weekly)
}