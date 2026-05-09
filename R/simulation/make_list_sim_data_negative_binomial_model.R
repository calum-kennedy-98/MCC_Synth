#' Generate K Replicates of Simulated Outcomes from Negative Binomial Models
#'
#' @description
#' For each city, fits a negative binomial GLM via
#' \code{\link{estimate_neg_binomial_model}} and draws \code{n_sims}
#' independent outcome replicates from the fitted distribution using
#' \code{\link{sim_data_negative_binomial_model}}. The per-city lists of
#' replicates are then transposed and concatenated so that the output is a
#' list of \code{n_sims} vectors, each containing a full panel of simulated
#' outcomes across all cities. Uses \pkg{furrr} for parallel processing.
#'
#' @param n_sims Integer. Number of simulation replicates.
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param unit_id_var Bare (unquoted) name of the unit identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the continuous time index column
#'   used as the spline argument (tidy-eval).
#' @param week_id_var Bare (unquoted) name of the week identifier column
#'   (tidy-eval; accepted for API consistency, not used here).
#' @param treated_var Bare (unquoted) name of the treatment indicator column
#'   (tidy-eval; accepted for API consistency, not used here).
#' @param outcome_var Bare (unquoted) name of the count outcome variable
#'   (tidy-eval).
#' @param year_var Bare (unquoted) name of the year variable (tidy-eval).
#' @param linear_predictors Character vector of additional linear covariate
#'   names.
#' @param temp_var Bare (unquoted) name of the temperature variable (tidy-eval).
#' @param spline_df_per_year Numeric. Degrees of freedom per year for the
#'   time-trend spline.
#' @param spline_df_temp Numeric. Degrees of freedom for the temperature spline.
#'
#' @return A list of \code{n_sims} numeric vectors, each of length equal to
#'   the total number of city-period observations, containing simulated outcome
#'   counts.
make_list_data_negative_binomial_model <- function(n_sims,
                                                   data,
                                                   unit_id_var,
                                                   time_id_var,
                                                   week_id_var,
                                                   treated_var,
                                                   outcome_var,
                                                   year_var,
                                                   linear_predictors,
                                                   temp_var,
                                                   spline_df_per_year,
                                                   spline_df_temp){
  
  # Extract list of unique locations from data
  unique_locations <- pull(distinct(data, {{unit_id_var}}))
  
  # Extract list of location-specific datasets from main data
  list_data_location_specific <- lapply(unique_locations, function(x){filter(data, {{unit_id_var}} == x)})
  
  # Set quoted variables to pass to future_map
  time_id_var_quo <- as_name(ensym(time_id_var))
  outcome_var_quo <- as_name(ensym(outcome_var))
  temp_var_quo <- as_name(ensym(temp_var))
  year_var_quo <- as_name(ensym(year_var))
  
  # Simulate outcome data from negative binomial model
  list_data_simulated_neg_binomial <- future_map(list_data_location_specific, 
                                                 .options = furrr_options(seed = TRUE), 
                                                 function(x){
    
    # Set ns function as global
    ns
    
    # Estimate negative binomial regression model and extract output
    model <- estimate_neg_binomial_model(data = x,
                                         outcome_var_quo,
                                         year_var_quo,
                                         linear_predictors,
                                         time_id_var_quo,
                                         temp_var_quo,
                                         spline_df_per_year,
                                         spline_df_temp)
    
    # Generate 'n_sims' simulations of outcome variable from the fitted model,
    # assuming an underlying negative binomial distribution (changing seed each time)
    list_data_simulated <- future_map(seq_len(n_sims), function(i){
      sim_data_negative_binomial_model(
        data = x,
        model = model)
      }, .options = furrr_options(seed = TRUE))
    
  })

  # Generate final list comprised of vectors of simulated outcomes
  list_outcome_sim_neg_binom_model <- map(transpose(list_data_simulated_neg_binomial), unlist)
  
  return(list_outcome_sim_neg_binom_model)
  
}