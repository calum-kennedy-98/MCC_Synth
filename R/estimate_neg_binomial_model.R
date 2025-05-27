# Name of script: estimate_neg_binomial_model
# Description: Estimates negative binomial regression model on a single unit time
# series and returns model coefficients
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------

# Args: Data frame (time series on one unit), outcome variable, linear predictors for
# negative binomial model, time variable and temperature variables to pass to GLM as natural cubic splines,
# degrees of freedom per year for time trends (to multiply by total number of years) and degrees
# of freedom for temperature splines function

# Value: Fitted negative binomial regression model object

# Function ---------------------------------------------------------------------

estimate_neg_binomial_model <- function(data,
                                        outcome_var,
                                        date_var,
                                        linear_predictors,
                                        time_id_var,
                                        temp_var,
                                        spline_df_per_year,
                                        spline_df_temp){
  
  # Find total degrees of freedom for time trends by multiplying per-year degrees
  # of freedom by N years
  spline_df_time_trend <- spline_df_per_year * length(unique(year(data[[date_var]])))
  
  # Convert function arguments to string and then to a formula to pass to glm.nb
  formula_string <- paste(outcome_var, "~", 
                          paste(linear_predictors, collapse = " + "), " + ", 
                          paste("ns(", time_id_var, ", df = ", spline_df_time_trend, ") + ns(", temp_var, ", df = ", spline_df_temp, ")"))
  
  fmla <- as.formula(formula_string)
  
  # Run GLM negative binomial model and return model coefficients
  res <- glm.nb(fmla, data = data)
  
  return(res)
  
}