# Name of script: optimise_synth_adh
# Description: Function to find optimal synthetic control predictions using the
# classical synthetic control method proposed by Abadie, Diamond, and Hainmueller (ADH)
# The ADH synthetic control finds a convex combination of control units which best approximates
# the pre-treatment trend in the treated units. Here, we do not allow an intercept term
# and we constrain the weights to lie in the unit interval and sum to 1. The synthetic control
# unit is therefore a projection of the treated unit outcomes onto the convex hull of the control
# unit outcomes. 
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 01-05-2025
# Latest update by: Calum Kennedy
# Latest update on: 01-05-2025

# Comments ---------------------------------------------------------------------

# @ param data, an input dataframe
# @ param id_var, unit ID column
# @ param outcome_var, name of the outcome variable column
# @ param time_var, column name for time variable
# @ param treated_id_var, binary ID variable for the treated unit
# @ param treated_time_var, binary ID variable for the treatment time
# @ param predictors, vector of quoted variables to use as predictors in synth
# @ return a list with elements data, Y1, Y1_hat, W_opt, mu_opt (equal to 0), first_treated_period

# Function ---------------------------------------------------------------------

optimise_synth_adh <- function(data,
                               id_var,
                               outcome_var,
                               time_var,
                               treated_id_var,
                               treated_time_var,
                               predictors){
  
  # Generate numeric ID column for synth
  data <- data %>%
    mutate(
      id_numeric = cur_group_id(),
      .by = {{id_var}}
    )
  
  # Generate vector of pre-treatment time periods - we need to extract the time
  # vector only for one unit, since the panel is balanced by design
  t_vec_pre_treatment <- data %>%
    filter(
      {{treated_time_var}} == 0,
      {{treated_id_var}} == 1
    ) %>%
    pull({{time_var}})
  
  # Generate indicator for first treatment period
  first_treated_period <- max(t_vec_pre_treatment) + 1
  
  # Extract vector of outcomes for treated unit
  Y1 <- data %>% 
    filter({{treated_id_var}} == 1) %>%
    pull({{outcome_var}}) %>% 
    as.numeric()
  
  # Store total number of periods 'n_periods'
  n_periods <- length(Y1)
  
  # Extract matrix of outcomes for control units (T x J matrix, where T = n_periods
  # and J = n_controls)
  Y0 <- data %>%
    filter({{treated_id_var}} == 0) %>%
    pull({{outcome_var}}) %>%
    matrix(nrow = n_periods)
  
  # Enquote variables to pass to dataprep function
  id_numeric_quo <- "id_numeric"
  outcome_var_quo <- as_string(ensym(outcome_var))
  time_var_quo <- as_string(ensym(time_var))
  
  # Generate list of period-specific outcome variables to pass to dataprep
  # Here we assume that we would like to use each specific pre-treatment
  # outcome realisation as a predictor, as opposed to e.g. the average outcome
  # across the whole pre-treatment period
  list_outcome_predictors <- lapply(t_vec_pre_treatment, function(x){
    list(outcome_var_quo, x, "mean")
  })
  
  # Generate ID vectors for treated and untreated units
  treated_id <- data %>%
    filter(
      {{treated_id_var}} == 1
    ) %>%
    distinct(id_numeric) %>%
    pull()
  
  controls_id <- data %>%
    filter(
      {{treated_id_var}} == 0
    ) %>%
    distinct(id_numeric) %>%
    pull()
  
  # Prepare data to pass to synth
  data_prepared <- dataprep(foo = data,
                            predictors = predictors,
                            special.predictors = list_outcome_predictors,
                            time.predictors.prior = t_vec_pre_treatment,
                            dependent = outcome_var_quo,
                            unit.variable = id_numeric_quo,
                            time.variable = time_var_quo,
                            treatment.identifier = treated_id,
                            controls.identifier = controls_id,
                            time.optimize.ssr = t_vec_pre_treatment)
  
  # Generate synth object
  synth_out <- synth(data_prepared, 
                     optimxmethod = "BFGS",
                     Margin.ipop = 0.002)
  
  # Extract optimal weights and intercept (mu = 0 by design)
  W_opt <- synth_out[["solution.w"]]
  mu_opt <- 0
  
  # Generate Y1_hat using Y0 and optimal weights
  Y1_hat <- c(Y0 %*% W_opt)
  
  # Store results in list
  results <- list("data" = data,
                  "Y1" = Y1,
                  "Y1_hat" = Y1_hat,
                  "W_opt" = W_opt,
                  "mu_opt" = mu_opt,
                  "first_treated_period" = first_treated_period)
  
  return(results)
  
}