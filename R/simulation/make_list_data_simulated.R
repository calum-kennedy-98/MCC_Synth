#' Generate Simulated Datasets with Empirically Distributed Treatment Assignment
#'
#' @description
#' Creates a list of analysis-ready data frames by attaching simulated
#' untreated potential outcomes (from both the negative binomial and factor
#' model DGPs) to the real MCC panel, then drawing treated units and treatment
#' times according to their empirical frequencies in the observed data via
#' \code{\link{estimate_assignment_probabilities}}. This reproduces the
#' realistic assignment mechanism for power and size assessments.
#'
#' @param data A data frame in long format containing all units and time
#'   periods; used as the structural template and to estimate assignment
#'   probabilities.
#' @param unit_id_var Bare (unquoted) name of the unit identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param week_id_var Bare (unquoted) name of the week identifier column used
#'   to restrict eligible treatment times (tidy-eval).
#' @param treated_var Bare (unquoted) name of the binary treatment indicator
#'   column used to estimate empirical probabilities (tidy-eval).
#' @param list_outcome_sim_neg_binomial A list of numeric vectors (length =
#'   number of simulation replicates), each containing simulated outcomes from
#'   the negative binomial DGP for all unit-periods.
#' @param list_outcome_sim_factor A list of numeric vectors (length = number
#'   of simulation replicates), each containing simulated outcomes from the
#'   factor model DGP for all unit-periods.
#' @param n_periods_pre Integer. Number of pre-treatment periods to retain
#'   and to restrict eligible treatment times.
#' @param n_periods_post Integer. Number of post-treatment periods to retain
#'   and to restrict eligible treatment times.
#'
#' @return A list of data frames, one per simulation replicate, each subsetted
#'   to the treatment window and augmented with \code{treated} (1/0),
#'   \code{post} (1/0), \code{Y0_treated_neg_binom}, and
#'   \code{Y0_treated_factor} columns.
make_list_data_simulated <- function(data,
                                     unit_id_var,
                                     time_id_var,
                                     week_id_var,
                                     treated_var,
                                     list_outcome_sim_neg_binomial,
                                     list_outcome_sim_factor,
                                     n_periods_pre,
                                     n_periods_post){
  
  # Extract unit and time treatment probabilities
  unit_time_treatment_probs <- estimate_assignment_probabilities(data = data,
                                                                 treated_var = {{treated_var}},
                                                                 unit_id_var = {{unit_id_var}},
                                                                 time_id_var = {{time_id_var}},
                                                                 week_id_var = {{week_id_var}},
                                                                 n_periods_pre = n_periods_pre,
                                                                 n_periods_post = n_periods_post)
  
  # Generate list of simulated datasets alongside true data
  list_data_with_sims <- map2(
    list_outcome_sim_neg_binomial,
    list_outcome_sim_factor,
    ~ data %>% mutate(Y0_treated_neg_binom = .x,
                      Y0_treated_factor = .y)
  )
  
  # For each data frame in list, sample treated unit and time using empirical treatment probabilities
  # and subset data around the treatment time
  list_data_out <- lapply(list_data_with_sims, function(x) {

    # Sample treated unit and treated time
    treated_unit <- sample(unit_time_treatment_probs[["unit_names"]], 1, prob = unit_time_treatment_probs[["treatment_prob_unit"]])
    treated_time <- sample(unit_time_treatment_probs[["time_ids"]], 1, prob = unit_time_treatment_probs[["treatment_prob_time"]])

    # Generate treat/post indicator for treated unit
    data_with_treatment <- x %>%
      mutate(
        treated = ifelse({{unit_id_var}} == treated_unit, 1, 0),
        post = ifelse({{time_id_var}} >= treated_time, 1, 0)
      ) %>%
      
      filter(between({{time_id_var}}, treated_time - n_periods_pre, treated_time + n_periods_post - 1)) %>%
      
      mutate(across(where(is.character), as.factor))
  }
  )
  
  return(list_data_out)
  
}