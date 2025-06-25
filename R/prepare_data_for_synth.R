# Name of script: prepare_data_for_synth
# Description: Function to prepare data to pass to synthetic control objective
# function
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

# @ param ...

prepare_data_for_synth <- function(data,
                                   demean_outcomes,
                                   denoise_outcomes,
                                   n_periods_pre,
                                   n_periods_post,
                                   outcome_var,
                                   treated_id_var,
                                   treated_time_var,
                                   time_var,
                                   spline_df){
  
  if(!is.logical(demean_outcomes) | !is.logical(denoise_outcomes)) stop("Please set `demean_outcomes' and `denoise_outcomes' equal to TRUE or FALSE")
  
  # Extract dimension of time variable
  n_periods <- n_periods_pre + n_periods_post
  
  # Generate indicator for post-treatment period
  post <- c(rep(0, n_periods_pre), rep(1, n_periods_post))
  
  # Extract first treated period and subset data to pre- / post-treatment intervals 
  # based on `n_periods_pre` and `n_periods_post`
  first_treated_period <- min(data %>%
                                filter({{treated_time_var}} == 1) %>%
                                pull({{time_var}})
  )
  
  data <- data %>%
    filter(
      between(
        {{time_var}}, 
        first_treated_period - n_periods_pre, 
        first_treated_period + n_periods_post - 1
      )
    )
  
  # Extract vector of observed outcomes for treated unit
  Y_treated <- data %>% 
    filter({{treated_id_var}} == 1) %>%
    pull({{outcome_var}}) %>% 
    as.numeric()
  
  # Extract matrix of outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y_controls <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull({{outcome_var}}) %>%
    matrix(nrow = n_periods)
  
  # Extract pre-treatment mean in treated and control units (necessary for de-meaning estimators)
  Y_treated_bar_pre_treatment <- mean(Y_treated[1:n_periods_pre])
  Y_controls_bar_pre_treatment <- colMeans(Y_controls[1:n_periods_pre,])
  
  # Return results
  out <- list("Y_treated" = Y_treated,
              "Y_controls" = Y_controls,
              "Y_treated_bar_pre_treatment" = Y_treated_bar_pre_treatment,
              "Y_controls_bar_pre_treatment" = Y_controls_bar_pre_treatment,
              "post" = post,
              "first_treated_period" = first_treated_period)
  
  return(out)
  
}