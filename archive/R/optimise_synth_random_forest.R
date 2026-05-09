#' Random Forest Counterfactual via Tidymodels
#'
#' @description
#' Generates post-treatment counterfactual predictions for the treated unit
#' using a random forest model trained on control units' pre-treatment outcome
#' series (pivoted to wide format). Hyperparameters (\code{trees},
#' \code{mtry}, \code{min_n}) are tuned via 10-fold cross-validation using
#' the \pkg{tidymodels} / \pkg{ranger} framework. The model is trained on
#' pre-treatment data and applied to the post-treatment period to generate
#' out-of-sample predictions.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param id_var Bare (unquoted) name of the unit identifier column (tidy-eval).
#' @param outcome_var Bare (unquoted) name of the outcome variable (tidy-eval).
#' @param time_var Bare (unquoted) name of the time identifier column
#'   (tidy-eval). Must be named \code{week_id} in the underlying data (used as
#'   an ID role in the recipe).
#' @param treated_id_var Bare (unquoted) name of the binary treated-unit
#'   indicator (1 = treated; tidy-eval).
#' @param treated_time_var Bare (unquoted) name of the binary post-treatment
#'   period indicator (1 = post; tidy-eval).
#' @param n_periods_pre Integer. Number of pre-treatment periods to include.
#' @param n_periods_post Integer. Number of post-treatment periods to include.
#'
#' @return A tibble of post-treatment period predictions, containing the
#'   predicted values (\code{.pred}) bound to the test-set columns.
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