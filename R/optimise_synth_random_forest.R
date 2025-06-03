# Name of script: optimise_synth_random_forest
# Description: ***
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 13-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 13-03-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

optimise_synth_random_forest <- function(data,
                                         id_var,
                                         outcome_var,
                                         time_var,
                                         treated_id_var,
                                         treated_time_var,
                                         n_periods_pre,
                                         n_periods_post){
  
  # Extract identity of treated unit to pass to model
  treated_unit <- data %>%
    filter({{treated_id_var}} == 1) %>%
    distinct({{id_var}}) %>%
    pull({{id_var}})
  
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
  
  # Reshape to wide format
  data_wide <- data %>%
    pivot_wider(id_cols = {{time_var}},
                names_from = {{id_var}},
                values_from = {{outcome_var}})
  
  # Rename treated unit to `treated`
  data_wide <- data_wide %>%
    rename(treated = treated_unit)
  
  # Extract training and testing set 
  data_train <- data_wide %>%
    filter({{time_var}} < first_treated_period)
  
  data_test <- data_wide %>%
    filter({{time_var}} >= first_treated_period)
  
  # Set up tidymodels recipe
  recipe <- recipe(treated ~ ., data = data_train) %>%
    update_role(week_id, new_role = "ID")
  
  # Create folds
  folds <- vfold_cv(data_train, v = 10)
  
  # Set up random forest regression tree
  rf_mod <- rand_forest(trees = tune(),
                        mtry = tune(),
                        min_n = tune()) %>% 
    set_engine("ranger") %>%
    set_mode("regression")
  
  # Set model workflow
  workflow <- workflow() %>%
    add_model(rf_mod) %>%
    add_recipe(recipe)
  
  # Fit model to training data using
  rf_res <- workflow %>% 
    tune_grid(folds)
  
  # Get best model from tuning results
  rf_mod_best <- rf_res %>%
    select_best(metric = "rmse")
  
  # Update final workflow
  final_workflow <- workflow %>%
    finalize_workflow(rf_mod_best)
  
  # Get final model fit
  rf_fit <- final_workflow %>%
    fit(data_train)
  
  # Get predictions on testing set
  data_pred <- predict(rf_fit, data_test) %>%
    bind_cols(data_test)
  
  return(data_pred)
  
}