#' Extract Fire PM2.5 Coefficients from Negative Binomial Models Fitted to Simulated Data
#'
#' @description
#' Fits a negative binomial regression model (via
#' \code{\link{estimate_neg_binomial_model}}) to each city's simulated
#' outcome series, including fire PM2.5 as a covariate, and extracts the
#' estimated coefficient and 95% confidence interval for fire PM2.5 from each
#' city-level model. Because fire PM2.5 has no causal role in the simulation
#' DGPs, the true coefficient is zero; this function enables a check on whether
#' the regression-based approach correctly recovers a zero effect.
#'
#' @param data A data frame of simulated outcome data containing all variables
#'   referenced by the other arguments.
#' @param unit_id_var Bare (unquoted) name of the unit (city) identifier column
#'   (tidy-eval).
#' @param time_id_var Bare (unquoted) name of the continuous time index column
#'   used as the spline argument (tidy-eval).
#' @param week_id_var Bare (unquoted) name of the week identifier column
#'   (tidy-eval; accepted but not directly used in this function).
#' @param treated_var Bare (unquoted) name of the treatment indicator column
#'   (tidy-eval; accepted but not directly used in this function).
#' @param outcome_var Bare (unquoted) name of the count outcome variable
#'   (tidy-eval).
#' @param year_var Bare (unquoted) name of the year variable (tidy-eval).
#' @param linear_predictors Character vector of covariate names to include as
#'   linear terms (in addition to \code{"pred_fire_PM25"}, which is appended
#'   automatically).
#' @param temp_var Bare (unquoted) name of the temperature variable (tidy-eval).
#' @param spline_df_per_year Numeric. Degrees of freedom per year for the
#'   time-trend spline.
#' @param spline_df_temp Numeric. Degrees of freedom for the temperature spline.
#'
#' @return A tibble with one row per city containing columns \code{term} (city
#'   name), \code{estimate} (fire PM2.5 coefficient), \code{lower}, and
#'   \code{upper} (95% confidence interval bounds).
get_coefficient_pred_fire_pm25_sim_data_neg_binom <- function(data,
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
  
  # Append pred_fire_PM25 variable to linear predictors
  linear_predictors <- c(linear_predictors, "pred_fire_PM25")
  
  # Set quoted variables to pass to future_map
  time_id_var_quo <- as_name(ensym(time_id_var))
  outcome_var_quo <- as_name(ensym(outcome_var))
  temp_var_quo <- as_name(ensym(temp_var))
  year_var_quo <- as_name(ensym(year_var))
  unit_id_var_quo <- as_name(ensym(unit_id_var))
  
  # Extract list of location-specific datasets from main data
  list_data_location_specific <- lapply(unique_locations, function(x){filter(data, .data[[unit_id_var_quo]] == x)})
  
  # Get list of estimated coefficients on pred_fire_PM25 from each location
  list_coef_pred_fire_PM25 <- future_map(list_data_location_specific, 
                                                 function(x){
                                                   
                                                   # Export ns function to workers
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
                                                   
                                                   # Extract coefficient on pred_fire_PM25
                                                   coef <- model$coefficients[["pred_fire_PM25"]]
                                                   
                                                   # Extract confidence interval
                                                   ci <- confint(model)["pred_fire_PM25", ]
                                                   
                                                   # Combine into a named vector or small data frame
                                                   result <- c(estimate = coef,
                                                               lower = ci[1],
                                                               upper = ci[2])
                                                   
                                                 })
  

# Set unique locations to names of list of output
names(list_coef_pred_fire_PM25) <- unique_locations

# Get mean of coefficient + upper and lower quantiles across all simulation runs
#results <- tibble(bind_rows(list_coef_pred_fire_PM25), column_label = unique_locations)
results <- imap_dfr(list_coef_pred_fire_PM25, ~ tibble(term = .y, !!!as.list(.x)))
  
}