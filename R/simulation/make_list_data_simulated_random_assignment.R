#' Generate Simulated Datasets with Uniformly Random Treatment Assignment
#'
#' @description
#' Creates a list of analysis-ready data frames by attaching simulated
#' untreated potential outcomes (from both the negative binomial and factor
#' model DGPs) to the real MCC panel, then randomly drawing a treated unit
#' and treatment time with equal probability for each replicate. This produces
#' a placebo-style simulation under random (rather than empirically
#' distributed) assignment, useful for assessing size control.
#'
#' @param data A data frame in long format containing all units and time
#'   periods; used as the structural template.
#' @param unit_id_var Bare (unquoted) name of the unit identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval).
#' @param list_outcome_sim_neg_binomial A list of numeric vectors (length =
#'   number of simulation replicates), each containing simulated outcomes from
#'   the negative binomial DGP for all unit-periods.
#' @param list_outcome_sim_factor A list of numeric vectors (length = number
#'   of simulation replicates), each containing simulated outcomes from the
#'   factor model DGP for all unit-periods.
#' @param n_periods_pre Integer. Minimum number of pre-treatment periods
#'   required; restricts the set of eligible treatment times.
#' @param n_periods_post Integer. Minimum number of post-treatment periods
#'   required; restricts the set of eligible treatment times.
#'
#' @return A list of data frames, one per simulation replicate, each subsetted
#'   to the treatment window and augmented with \code{treated} (1/0),
#'   \code{post} (1/0), \code{Y0_treated_neg_binom}, and
#'   \code{Y0_treated_factor} columns.
make_list_data_simulated_random_assignment <- function(data,
                                     unit_id_var,
                                     time_id_var,
                                     list_outcome_sim_neg_binomial,
                                     list_outcome_sim_factor,
                                     n_periods_pre,
                                     n_periods_post){
  
  # Extract unit and time vectors to assign to treatment (restrict time_ids to ensure sufficient pre/post treatment periods)
  unit_ids <- pull(distinct(data, {{unit_id_var}}))
  time_ids <- data %>% filter(between({{time_id_var}}, 
                                      min({{time_id_var}}) + n_periods_pre, 
                                      max({{time_id_var}}) - n_periods_post + 1)) %>%
    distinct({{time_id_var}}) %>%
    pull({{time_id_var}})
  
  
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
    
    # Sample treated unit and treated time randomly
    treated_unit <- sample(unit_ids, 1)
    treated_time <- sample(time_ids, 1)
    
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