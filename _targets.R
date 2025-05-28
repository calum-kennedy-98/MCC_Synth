# Comments here

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(tidylog)
library(here)
library(ggplot2)
library(viridis)
library(crew)
library(future)

# Conflict preferences
conflicted::conflict_prefer_all("tidylog", c("dplyr", "mice", "stats", "MASS"))
conflicted::conflicts_prefer(dplyr::lag)

# Set target options:
tar_option_set(
  packages = c(
    "MASS",
    "here",
    "janitor",
    "dplyr",
    "tibble",
    "lubridate",
    "mice",
    "purrr",
    "optimx",
    "Synth",
    "synthdid",
    "glmnet",
    "stringr",
    "tidylog",
    "conflicted",
    "slider",
    "splines",
    "furrr",
    "rlang"
  ),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE
  #controller = crew_controller_local(workers = 10,
                                     #seconds_idle = 10)
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Source functions
tar_source("functions.R")

# Set future plan to multisession
plan(multisession)

# Define analysis pipeline -----------------------------------------------------

list(
  
  # Data cleaning and preparation
  tar_target(path_data_mcc_raw, here("data/main/MCC/Main/MCCdata_20250307_R.RData"),
             format = "file"),
  
  tar_target(path_data_lfs_raw, here("data/main/LFS_pollution/MCC_PM_O3_df.rds")),
  
  tar_target(data_mcc_cleaned, get_data_mcc(path_data_mcc = path_data_mcc_raw,
                                           id_var = column_label,
                                           vars_to_select = c("column_label",
                                                              "day_id",
                                                              "week_id",
                                                              "date",
                                                              "year",
                                                              "month",
                                                              "dow",
                                                              "nonext",
                                                              "tmean",
                                                              "all",
                                                              "cvd",
                                                              "resp",
                                                              "accident",
                                                              "cityname",
                                                              "countryname",
                                                              "region"))),
  
  tar_target(data_mcc_lfs, merge_data_mcc_lfs(data_mcc_cleaned,
                                              path_data_lfs_raw,
                                              n_lags_all = 2,
                                              n_lags_fire_PM25 = 2,
                                              id_var = column_label)),
  
  tar_target(data_mcc_lfs_with_treatment, generate_treatment_indicator(data = data_mcc_lfs,
                                                                       id_var = column_label,
                                                                       fire_pm_ma_var = pred_fire_PM25_ma,
                                                                       fire_pm_var = "pred_fire_PM25",
                                                                       quantile_threshold = 0.99,
                                                                       n_days_above_threshold = 3,
                                                                       n_days_below_threshold = 28)),
  
  tar_target(data_mcc_lfs_weekly, make_data_mcc_lfs_weekly(data_mcc_lfs = data_mcc_lfs_with_treatment,
                                                           climatic_vars = c("tmean",
                                                                             "pred_fire_PM25",
                                                                             "pred_total_PM25",
                                                                             "pred_nonfire_PM25"),
                                                           mortality_vars = c("all",
                                                                              "nonext",
                                                                              "cvd",
                                                                              "resp"),
                                                           group_vars = c("column_label",
                                                                          "week_id",
                                                                          "countryname",
                                                                          "region"))),
  
  # Make datasets for simulation exercise --------------------------------------------------------
  
  # Generate data for simulation (conditions to ensure that no missing data - Korea data only goes up 
  # to 22/12/2018 and filter Taiwan since data only foes up to 2014 and there are only 3 locations)
  tar_target(data_for_simulation, subset_data(data = data_mcc_lfs_weekly, 
                                           region == "Eastern Asia",
                                           countryname != "Taiwan",
                                           between(date, 
                                                   as.Date("2000-01-01"), 
                                                   as.Date("2018-12-22")),
                                           vars_to_select = c("column_label",
                                                              "week_id",
                                                              "tmean",
                                                              "pred_fire_PM25",
                                                              "pred_nonfire_PM25",
                                                              "all",
                                                              "treated",
                                                              "date")) %>%
               
               # Keep only data with non-missing temperature and mortality data
               filter(if_all(all, ~ !is.na(.)),
                      if_all(tmean, ~ !is.na(.)))),
  
  tar_target(list_data_simulated_neg_binomial, make_list_data_negative_binomial_model(n_sims = 250, 
                                                                                      data = data_for_simulation, 
                                                                                      unit_id_var = column_label, 
                                                                                      time_id_var = week_id,
                                                                                      week_id_var = week_id,
                                                                                      treated_var = treated,
                                                                                      outcome_var = all, 
                                                                                      date_var = date,
                                                                                      linear_predictors = "pred_nonfire_PM25", 
                                                                                      temp_var = tmean, 
                                                                                      spline_df_per_year = 7, 
                                                                                      spline_df_temp = 4)),
  
  # Get simulation results -----------------------------------------------------
  
  # Results from elastic net
  tar_target(results_synth_elastic_net_neg_binom, future_map(list_data_simulated_neg_binomial,
                                                             ~ optimise_synth_elastic_net(.,
                                                                                          alpha_init = 0.5,
                                                                                          lambda_init = 2,
                                                                                          outcome_var = outcome_pred,
                                                                                          time_var = week_id,
                                                                                          treated_id_var = treated,
                                                                                          treated_time_var = post,
                                                                                          n_periods_pre = 26,
                                                                                          n_periods_post = 26))),
  
  # Results from ADH synth without covariates
  tar_target(results_synth_adh_no_covars_neg_binom, future_map(list_data_simulated_neg_binomial,
                                                             ~ optimise_synth_adh(.,
                                                                                  id_var = column_label,
                                                                                  outcome_var = outcome_pred,
                                                                                  time_var = week_id,
                                                                                  treated_id_var = treated,
                                                                                  treated_time_var = post,
                                                                                  n_periods_pre = 26,
                                                                                  n_periods_post = 26,
                                                                                  predictors = NULL,
                                                                                  optimxmethod = c("Nelder-Mead", "BFGS"),
                                                                                  initial_margin = 0.0005,
                                                                                  max_attempts = 20,
                                                                                  margin_increment = 0.0005))),
  
  # Results from ADH synth with covariates
  tar_target(results_synth_adh_covars_neg_binom, future_map(list_data_simulated_neg_binomial,
                                                               ~ optimise_synth_adh(.,
                                                                                    id_var = column_label,
                                                                                    outcome_var = outcome_pred,
                                                                                    time_var = week_id,
                                                                                    treated_id_var = treated,
                                                                                    treated_time_var = post,
                                                                                    n_periods_pre = 26,
                                                                                    n_periods_post = 26,
                                                                                    predictors = c("tmean", "pred_nonfire_PM25"),
                                                                                    optimxmethod = c("Nelder-Mead", "BFGS"),
                                                                                    initial_margin = 0.0005,
                                                                                    max_attempts = 20,
                                                                                    margin_increment = 0.0005))),
  
  # Results from ADH synth on de-meaned outcomes
  tar_target(results_synth_adh_demeaned_neg_binom, future_map(list_data_simulated_neg_binomial,
                                                            ~ optimise_synth_demeaned(.,
                                                                                 id_var = column_label,
                                                                                 outcome_var = outcome_pred,
                                                                                 time_var = week_id,
                                                                                 treated_id_var = treated,
                                                                                 treated_time_var = post,
                                                                                 n_periods_pre = 26,
                                                                                 n_periods_post = 26,
                                                                                 optimxmethod = c("Nelder-Mead", "BFGS"),
                                                                                 initial_margin = 0.0005,
                                                                                 max_attempts = 20,
                                                                                 margin_increment = 0.0005)))
  
)