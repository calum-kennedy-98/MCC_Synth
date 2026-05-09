#' Extract Expected Outcome Values from City-Level Negative Binomial Models
#'
#' @description
#' Fits a separate negative binomial GLM (via
#' \code{\link{estimate_neg_binomial_model}}) to each city's outcome time
#' series using parallel processing (\pkg{furrr}), and extracts the
#' model-implied expected values (on the response scale) for every
#' city-period. The concatenated vector of expected values is used to seed
#' the negative binomial simulation DGP.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param unit_id_var Bare (unquoted) name of the unit (city) identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the continuous time index column
#'   used as the spline argument (tidy-eval).
#' @param week_id_var Bare (unquoted) name of the week identifier column
#'   (tidy-eval; accepted for API consistency but not directly used here).
#' @param treated_var Bare (unquoted) name of the treatment indicator column
#'   (tidy-eval; accepted for API consistency but not directly used here).
#' @param outcome_var Bare (unquoted) name of the count outcome variable
#'   (tidy-eval).
#' @param year_var Bare (unquoted) name of the year variable (tidy-eval).
#' @param linear_predictors Character vector of additional linear covariate
#'   names (excluding temperature and time trend, which are modelled as
#'   splines).
#' @param temp_var Bare (unquoted) name of the temperature variable (tidy-eval).
#' @param spline_df_per_year Numeric. Degrees of freedom per year for the
#'   time-trend spline.
#' @param spline_df_temp Numeric. Degrees of freedom for the temperature spline.
#'
#' @return A numeric vector of length \eqn{n \times T} containing fitted
#'   expected values (on the count scale) concatenated across all cities.
get_outcome_exp_val_negative_binomial_model <- function(data,
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
  
  # Get expected value of outcome by location
  list_exp_val_neg_binomial <- future_map(list_data_location_specific, 
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
                                                   # Extract model predicted values for the linear link function
                                                   # and take exponential to find expected value of outcome variable distribution 
                                                   outcome_exp_val <- exp(predict(model, x))
                                                   
                                                 })
  
  exp_val_neg_binomial <- c(unlist(list_exp_val_neg_binomial))
  
}