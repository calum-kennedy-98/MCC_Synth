# Comments here

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(tidylog)
library(here)
library(ggplot2)
library(viridis)
library(crew)

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
    "furrr"
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

# Run the R scripts in the R/ folder with your custom functions:
tar_source("functions.R")

# Define analysis pipeline -----------------------------------------------------

list(
  
  # Data cleaning and preparation
  tar_target(path_data_mcc_raw, here("data/main/MCC/Main/MCCdata_20250307_R.RData"),
             format = "file"),
  
  tar_target(path_data_lfs_raw, here("data/main/LFS_pollution/MCC_PM_O3_df.rds")),
  
  tar_target(data_mcc_cleaned, get_data_mcc(path_data_mcc = path_data_mcc_raw,
                                           id_var = column_label,
                                           vars_to_select = c("id",
                                                              "column_label",
                                                              "date",
                                                              "year",
                                                              "month",
                                                              "week",
                                                              "doy",
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
                                                              "tmean",
                                                              "week_id",
                                                              "pred_fire_PM25",
                                                              "pred_nonfire_PM25",
                                                              "all",
                                                              "treated",
                                                              "date")) %>%
               
               # Keep only data with non-missing temperature and mortality data
               filter(if_all(all, ~ !is.na(.)),
                      if_all(tmean, ~ !is.na(.)))),
  
  tar_target(list_data_simulated_neg_binomial, make_list_data_negative_binomial_model(n_sims = 500, 
                                                                                      data = data_for_simulation, 
                                                                                      id_var = column_label, 
                                                                                      treated_var = treated,
                                                                                      outcome_var = all, 
                                                                                      date_var = date,
                                                                                      linear_predictors = "pred_nonfire_PM25", 
                                                                                      week_var = week_id,
                                                                                      temp_var = tmean, 
                                                                                      spline_df_per_year = 7, 
                                                                                      spline_df_temp = 4))
  
  # Get simulation results -----------------------------------------------------
  
  
  # Old stuff below
  
  # tar_target(list_data_simulated, make_data_scm_list(n_sims = 100,
  #                                                    data = data_for_simulation, 
  #                                                    id_var = column_label, 
  #                                                    week_var = week, # May need to rethink this if go with moving averages 
  #                                                    fire_pm_var = pred_fire_PM25,
  #                                                    time_var_mcc = doy)),
  # 
  # tar_target(results_synth_model_simulated, sim_synth_model(list_data_simulated = list_data_simulated, 
  #                                                           id_var = id, 
  #                                                           predictors = NULL,
  #                                                           special_predictors = list(list("y", 1:29, "mean")), 
  #                                                           time_predictors_prior = 1:29, 
  #                                                           dep_var = "y", 
  #                                                           time_var_synth = "week", 
  #                                                           time_optimise_ssr = 20:29, 
  #                                                           time_plot = 1:50)),
  # 
  # tar_target(results_tau_hat_synth, extract_tau_hat_synth_results(results_synth_model_simulated = results_synth_model_simulated,
  #                                                                 id_var = id,
  #                                                                 time_var = week)),
  # 
  # tar_target(results_synth_diagnostics, get_synth_diagnostics(results_synth_model_simulated = results_synth_model_simulated,
  #                                                             id_var = id,
  #                                                             time_var = week,
  #                                                             start_index = 1,
  #                                                             end_index = 29)),
  # 
  # # Here have defined separate target for elastic net method - ultimately we
  # # probably want to combine these into some generic wrapper function (editing the 'sim_synth_model'
  # # function call)
  # tar_target(results_elastic_net_simulated, map(list_data_simulated, 
  #                                               ~ optimise_synth_elastic_net(.,
  #                                                                            alpha_init = 0.5,
  #                                                                            lambda_init = 2,
  #                                                                            outcome_var = y,
  #                                                                            treated_id_var = treated_unit,
  #                                                                            treated_time_var = treated_time,
  #                                                                            time_var = week)))
)