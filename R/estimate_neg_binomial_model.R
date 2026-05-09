#' Estimate a Negative Binomial Regression Model
#'
#' @description
#' Fits a negative binomial generalised linear model (GLM) to a single unit's
#' time series. The linear predictor includes user-supplied covariates plus
#' natural cubic splines for a long-run time trend and temperature, allowing
#' flexible control of seasonality and confounding. Returns the fitted model
#' object, which can be used to extract coefficients or generate predictions.
#'
#' @param data A data frame containing all variables referenced by the other
#'   arguments.
#' @param outcome_var Character string. Name of the count outcome variable
#'   (e.g. daily mortality).
#' @param year_var Character string. Name of the year variable, used to
#'   calculate total spline degrees of freedom for the time trend
#'   (\code{spline_df_per_year * n_years}).
#' @param linear_predictors Character vector. Names of additional covariates to
#'   include as linear terms in the model formula.
#' @param time_id_var Character string. Name of the continuous time index
#'   variable used as the argument to the time-trend spline.
#' @param temp_var Character string. Name of the temperature variable used as
#'   the argument to the temperature spline.
#' @param spline_df_per_year Numeric. Degrees of freedom per year allocated to
#'   the natural spline for the long-run time trend.
#' @param spline_df_temp Numeric. Degrees of freedom for the natural spline
#'   applied to temperature.
#'
#' @return A \code{glm.nb} model object (class \code{"negbin"}) as returned by
#'   \code{\link[MASS]{glm.nb}}.
estimate_neg_binomial_model <- function(data,
                                        outcome_var,
                                        year_var,
                                        linear_predictors,
                                        time_id_var,
                                        temp_var,
                                        spline_df_per_year,
                                        spline_df_temp){
  
  # Find total degrees of freedom for time trends by multiplying per-year degrees
  # of freedom by N years
  spline_df_time_trend <- spline_df_per_year * length(unique(data[[year_var]]))
  
  # Convert function arguments to string and then to a formula to pass to glm.nb
  formula_string <- paste(outcome_var, "~", 
                          paste(linear_predictors, collapse = " + "), " + ", 
                          paste("ns(", time_id_var, ", df = ", spline_df_time_trend, ") + ns(", temp_var, ", df = ", spline_df_temp, ")"))
  
  fmla <- as.formula(formula_string)
  
  # Run GLM negative binomial model and return model coefficients
  res <- glm.nb(fmla, data = data)
  
  return(res)
  
}