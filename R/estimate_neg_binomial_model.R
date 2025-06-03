# Name of script: estimate_neg_binomial_model
# Description: Estimates negative binomial regression model on a single unit time
# series and returns model coefficients
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------


# Function ---------------------------------------------------------------------

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