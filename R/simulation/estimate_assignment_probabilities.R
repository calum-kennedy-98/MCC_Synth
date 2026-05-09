#' Estimate Empirical Treatment Assignment Probabilities
#'
#' @description
#' Computes unit-level and time-level treatment assignment probabilities from
#' the observed MCC data for use in the simulation study. Unit probabilities
#' are proportional to the number of acute exposure events experienced by each
#' city. Time probabilities are proportional to the number of treated city-days
#' at each time point, restricted to time periods that allow sufficient pre-
#' and post-treatment windows. These probabilities are used to sample treatment
#' assignments in \code{\link{make_list_data_simulated}}.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param treated_var Bare (unquoted) name of the binary treatment indicator
#'   column (tidy-eval).
#' @param unit_id_var Bare (unquoted) name of the unit (city) identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param week_id_var Bare (unquoted) name of the week identifier column used
#'   to filter time periods at the edges of the sample (tidy-eval).
#' @param n_periods_pre Integer. Number of pre-treatment periods required;
#'   time points with fewer than this many preceding observations are excluded.
#' @param n_periods_post Integer. Number of post-treatment periods required;
#'   time points with fewer than this many following observations are excluded.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{unit_names}{Character vector of unit (city) names.}
#'   \item{time_ids}{Numeric vector of eligible time IDs.}
#'   \item{treatment_prob_unit}{Numeric vector of unit-level assignment
#'     probabilities (sums to 1).}
#'   \item{treatment_prob_time}{Numeric vector of time-level assignment
#'     probabilities (sums to 1).}
#' }
estimate_assignment_probabilities <- function(data,
                                              treated_var,
                                              unit_id_var,
                                              time_id_var,
                                              week_id_var,
                                              n_periods_pre,
                                              n_periods_post) {
  
  # Extract unit treatment probability
  data_treatment_prob_unit <- data %>%
    mutate(
      change_treatment = {{treated_var}} - lag({{treated_var}}), .by = {{unit_id_var}}
    ) %>%
    summarise(
      n_events = sum(change_treatment == 1, na.rm = TRUE), .by = {{unit_id_var}}
    ) %>%
    mutate(
      treatment_prob = n_events / sum(n_events)
      ) %>%
    rename(
      unit_name = {{unit_id_var}}
    )
  
  unit_names <- data_treatment_prob_unit$unit_name
  treatment_prob_unit <- data_treatment_prob_unit$treatment_prob
  
  # Extract treatment time probability (filter weeks at beginning/end of sample to ensure
  # sufficient pre and post-periods for SC methods to work)
  data_treatment_prob_time <- data %>%
    filter(
      between({{week_id_var}}, min({{week_id_var}} + n_periods_pre), max({{week_id_var}} - n_periods_post + 1))
    ) %>%
    summarise(
      n_events = sum({{treated_var}} == 1, na.rm = TRUE), .by = {{time_id_var}}
    ) %>%
    mutate(
      treatment_prob = n_events / sum(n_events)
      ) %>%
    rename(
      time_id_var = {{time_id_var}}
    )
  
  time_ids <- data_treatment_prob_time$time_id_var
  treatment_prob_time <- data_treatment_prob_time$treatment_prob
  
  # Return assigned unit and treatment status
  return(list("unit_names" = unit_names,
              "time_ids" = time_ids,
              "treatment_prob_unit" = treatment_prob_unit,
              "treatment_prob_time" = treatment_prob_time))
  
}