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
    "rlang",
    "mvtnorm",
    "LowRankQP",
    "gt",
    "patchwork"
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

# Set seed
set.seed(42)

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
  
  # Partition main data into list of datasets to pass to synth optimisation
  tar_target(list_data_for_synth, partition_data_for_synth(data = data_mcc_lfs_weekly,
                                                           unit_id_var = column_label,
                                                           time_id_var = week_id,
                                                           region_id_var = region,
                                                           treated_var = treated,
                                                           outcome_var = all,
                                                           min_periods_pre = 10,
                                                           min_control_units = 5,
                                                           min_weekly_mortality = 100)),
  
  # Make datasets for simulation exercise --------------------------------------------------------
  
  # Generate data for simulation (conditions to ensure that no missing data - Korea data only goes up 
  # to 22/12/2018 and filter Taiwan since data only goes up to 2014 and there are only 3 locations)
  tar_target(data_for_simulation, subset_data(data = data_mcc_lfs_weekly, 
                                           region == "Eastern Asia",
                                           countryname != "Taiwan",
                                           between(date, 
                                                   as.Date("2000-01-01"), 
                                                   as.Date("2018-12-22")),
                                           vars_to_select = c("column_label",
                                                              "countryname",
                                                              "week_id",
                                                              "month",
                                                              "year",
                                                              "tmean",
                                                              "pred_fire_PM25",
                                                              "pred_nonfire_PM25",
                                                              "all",
                                                              "treated")) %>%
               
               # Keep only data with non-missing temperature and mortality data
               filter(if_all(all, ~ !is.na(.)),
                      if_all(tmean, ~ !is.na(.))) %>%
               
               # Filter out locations with mean weekly deaths < 100
               mutate(mean_mortality = mean(all, na.rm = TRUE), .by = column_label) %>%
               
               filter(mean_mortality > 100) %>%
               
               select(-mean_mortality)),
  
  # List of simulated outcome data from negative binomial model - untreated potential outcome (Y(0))
  tar_target(list_outcome_sim_neg_binomial, make_list_data_negative_binomial_model(n_sims = 500, 
                                                                                   data = data_for_simulation, 
                                                                                   unit_id_var = column_label, 
                                                                                   time_id_var = week_id,
                                                                                   week_id_var = week_id,
                                                                                   treated_var = treated,
                                                                                   outcome_var = all, 
                                                                                   year_var = year,
                                                                                   linear_predictors = "pred_nonfire_PM25", 
                                                                                   temp_var = tmean, 
                                                                                   spline_df_per_year = 7, 
                                                                                   spline_df_temp = 4)),
  
  # List of simulated outcome data from factor model - untreated potential outcome (Y(0))
  tar_target(list_outcome_sim_factor, make_list_sim_data_factor_model(n_sims = 500,
                                                                  data = data_for_simulation,
                                                                  unit_id_var = column_label, 
                                                                  time_id_var = week_id,
                                                                  week_id_var = week_id,
                                                                  treated_var = treated,
                                                                  outcome_var = all,
                                                                  rank = 4)),
  
  # Generate final combined simulated data with untreated potential outcomes Y0
  # We do not need to simulated treated potential outcomes Y1 here since the optimal
  # synthetic control unit will not change - we assign Y1 later
  tar_target(list_data_simulated, make_list_data_simulated(data = data_for_simulation, 
                                                           unit_id_var = column_label, 
                                                           time_id_var = week_id,
                                                           week_id_var = week_id,
                                                           treated_var = treated,
                                                           list_outcome_sim_neg_binomial = list_outcome_sim_neg_binomial,
                                                           list_outcome_sim_factor = list_outcome_sim_factor,
                                                           n_periods_pre = 26,
                                                           n_periods_post = 26)),
  
  # Generate final combined simulated data under random assignment
  tar_target(list_data_simulated_random_assignment, make_list_data_simulated_random_assignment(data = data_for_simulation, 
                                                           unit_id_var = column_label, 
                                                           time_id_var = week_id,
                                                           list_outcome_sim_neg_binomial = list_outcome_sim_neg_binomial,
                                                           list_outcome_sim_factor = list_outcome_sim_factor,
                                                           n_periods_pre = 26,
                                                           n_periods_post = 26)),
  
  # Get optimal SC from simulations --------------------------------------------
  
  # 1. No demeaning/denoising, negative binomial model, assignment based on empirical distribution 
  
  # ADH
  tar_target(results_synth_adh_neg_binom, future_map(list_data_simulated,
                                                             ~ optimise_synth(.,
                                                                              demean_outcomes = FALSE,
                                                                              denoise_outcomes = FALSE,
                                                                              objective_function = "ADH",
                                                                              n_periods_pre = 26,
                                                                              n_periods_post = 26,
                                                                              outcome_var = Y0_treated_neg_binom,
                                                                              treated_id_var = treated,
                                                                              treated_time_var = post,
                                                                              time_var = week_id,
                                                                              spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_neg_binom, future_map(list_data_simulated,
                                                               ~ optimise_synth(.,
                                                                                demean_outcomes = FALSE,
                                                                                denoise_outcomes = FALSE,
                                                                                objective_function = "DIFP",
                                                                                n_periods_pre = 26,
                                                                                n_periods_post = 26,
                                                                                outcome_var = Y0_treated_neg_binom,
                                                                                treated_id_var = treated,
                                                                                treated_time_var = post,
                                                                                time_var = week_id,
                                                                                spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_neg_binom, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "PSC",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_neg_binom,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # DID
  tar_target(results_synth_did_neg_binom, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "DID",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_neg_binom,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # 2. No demeaning/denoising, factor model, assignment based on empirical distribution
  
  # ADH
  tar_target(results_synth_adh_factor, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "ADH",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_factor,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_factor, future_map(list_data_simulated,
                                                      ~ optimise_synth(.,
                                                                       demean_outcomes = FALSE,
                                                                       denoise_outcomes = FALSE,
                                                                       objective_function = "DIFP",
                                                                       n_periods_pre = 26,
                                                                       n_periods_post = 26,
                                                                       outcome_var = Y0_treated_factor,
                                                                       treated_id_var = treated,
                                                                       treated_time_var = post,
                                                                       time_var = week_id,
                                                                       spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_factor, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "PSC",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_factor,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # DID
  tar_target(results_synth_did_factor, future_map(list_data_simulated,
                                                  ~ optimise_synth(.,
                                                                   demean_outcomes = FALSE,
                                                                   denoise_outcomes = FALSE,
                                                                   objective_function = "DID",
                                                                   n_periods_pre = 26,
                                                                   n_periods_post = 26,
                                                                   outcome_var = Y0_treated_factor,
                                                                   treated_id_var = treated,
                                                                   treated_time_var = post,
                                                                   time_var = week_id,
                                                                   spline_df = NULL))),
  
  # 3. No demeaning/denoising, negative binomial model, random assignment 
  
  # ADH
  tar_target(results_synth_adh_neg_binom_random_assignment, future_map(list_data_simulated_random_assignment,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "ADH",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_neg_binom,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_neg_binom_random_assignment, future_map(list_data_simulated_random_assignment,
                                                      ~ optimise_synth(.,
                                                                       demean_outcomes = FALSE,
                                                                       denoise_outcomes = FALSE,
                                                                       objective_function = "DIFP",
                                                                       n_periods_pre = 26,
                                                                       n_periods_post = 26,
                                                                       outcome_var = Y0_treated_neg_binom,
                                                                       treated_id_var = treated,
                                                                       treated_time_var = post,
                                                                       time_var = week_id,
                                                                       spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_neg_binom_random_assignment, future_map(list_data_simulated_random_assignment,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "PSC",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_neg_binom,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # DID
  tar_target(results_synth_did_neg_binom_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                       ~ optimise_synth(.,
                                                                                        demean_outcomes = FALSE,
                                                                                        denoise_outcomes = FALSE,
                                                                                        objective_function = "DID",
                                                                                        n_periods_pre = 26,
                                                                                        n_periods_post = 26,
                                                                                        outcome_var = Y0_treated_neg_binom,
                                                                                        treated_id_var = treated,
                                                                                        treated_time_var = post,
                                                                                        time_var = week_id,
                                                                                        spline_df = NULL))),
  
  # 4. No demeaning/denoising, factor model, random assignment
  
  # ADH
  tar_target(results_synth_adh_factor_random_assignment, future_map(list_data_simulated_random_assignment,
                                                  ~ optimise_synth(.,
                                                                   demean_outcomes = FALSE,
                                                                   denoise_outcomes = FALSE,
                                                                   objective_function = "ADH",
                                                                   n_periods_pre = 26,
                                                                   n_periods_post = 26,
                                                                   outcome_var = Y0_treated_factor,
                                                                   treated_id_var = treated,
                                                                   treated_time_var = post,
                                                                   time_var = week_id,
                                                                   spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_factor_random_assignment, future_map(list_data_simulated_random_assignment,
                                                   ~ optimise_synth(.,
                                                                    demean_outcomes = FALSE,
                                                                    denoise_outcomes = FALSE,
                                                                    objective_function = "DIFP",
                                                                    n_periods_pre = 26,
                                                                    n_periods_post = 26,
                                                                    outcome_var = Y0_treated_factor,
                                                                    treated_id_var = treated,
                                                                    treated_time_var = post,
                                                                    time_var = week_id,
                                                                    spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_factor_random_assignment, future_map(list_data_simulated_random_assignment,
                                                  ~ optimise_synth(.,
                                                                   demean_outcomes = FALSE,
                                                                   denoise_outcomes = FALSE,
                                                                   objective_function = "PSC",
                                                                   n_periods_pre = 26,
                                                                   n_periods_post = 26,
                                                                   outcome_var = Y0_treated_factor,
                                                                   treated_id_var = treated,
                                                                   treated_time_var = post,
                                                                   time_var = week_id,
                                                                   spline_df = NULL))),
  
  # DID
  tar_target(results_synth_did_factor_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                    ~ optimise_synth(.,
                                                                                     demean_outcomes = FALSE,
                                                                                     denoise_outcomes = FALSE,
                                                                                     objective_function = "DID",
                                                                                     n_periods_pre = 26,
                                                                                     n_periods_post = 26,
                                                                                     outcome_var = Y0_treated_factor,
                                                                                     treated_id_var = treated,
                                                                                     treated_time_var = post,
                                                                                     time_var = week_id,
                                                                                     spline_df = NULL))),
  
  # 5. De-meaned outcomes, negative binomial model, assignment based on empirical distribution 
  
  # ADH
  tar_target(results_synth_adh_neg_binom_demeaned, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = TRUE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "ADH",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_neg_binom,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_neg_binom_demeaned, future_map(list_data_simulated,
                                                      ~ optimise_synth(.,
                                                                       demean_outcomes = TRUE,
                                                                       denoise_outcomes = FALSE,
                                                                       objective_function = "DIFP",
                                                                       n_periods_pre = 26,
                                                                       n_periods_post = 26,
                                                                       outcome_var = Y0_treated_neg_binom,
                                                                       treated_id_var = treated,
                                                                       treated_time_var = post,
                                                                       time_var = week_id,
                                                                       spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_neg_binom_demeaned, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = TRUE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "PSC",
                                                                      n_periods_pre = 26,
                                                                      n_periods_post = 26,
                                                                      outcome_var = Y0_treated_neg_binom,
                                                                      treated_id_var = treated,
                                                                      treated_time_var = post,
                                                                      time_var = week_id,
                                                                      spline_df = NULL))),
  
  # 6. De-meaned outcomes, factor model, assignment based on empirical distribution
  
  # ADH
  tar_target(results_synth_adh_factor_demeaned, future_map(list_data_simulated,
                                                  ~ optimise_synth(.,
                                                                   demean_outcomes = TRUE,
                                                                   denoise_outcomes = FALSE,
                                                                   objective_function = "ADH",
                                                                   n_periods_pre = 26,
                                                                   n_periods_post = 26,
                                                                   outcome_var = Y0_treated_factor,
                                                                   treated_id_var = treated,
                                                                   treated_time_var = post,
                                                                   time_var = week_id,
                                                                   spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_factor_demeaned, future_map(list_data_simulated,
                                                   ~ optimise_synth(.,
                                                                    demean_outcomes = TRUE,
                                                                    denoise_outcomes = FALSE,
                                                                    objective_function = "DIFP",
                                                                    n_periods_pre = 26,
                                                                    n_periods_post = 26,
                                                                    outcome_var = Y0_treated_factor,
                                                                    treated_id_var = treated,
                                                                    treated_time_var = post,
                                                                    time_var = week_id,
                                                                    spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_factor_demeaned, future_map(list_data_simulated,
                                                  ~ optimise_synth(.,
                                                                   demean_outcomes = TRUE,
                                                                   denoise_outcomes = FALSE,
                                                                   objective_function = "PSC",
                                                                   n_periods_pre = 26,
                                                                   n_periods_post = 26,
                                                                   outcome_var = Y0_treated_factor,
                                                                   treated_id_var = treated,
                                                                   treated_time_var = post,
                                                                   time_var = week_id,
                                                                   spline_df = NULL))),
  
  # 7. De-meaned outcomes, negative binomial model, random assignment 
  
  # ADH
  tar_target(results_synth_adh_neg_binom_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                       ~ optimise_synth(.,
                                                                                        demean_outcomes = TRUE,
                                                                                        denoise_outcomes = FALSE,
                                                                                        objective_function = "ADH",
                                                                                        n_periods_pre = 26,
                                                                                        n_periods_post = 26,
                                                                                        outcome_var = Y0_treated_neg_binom,
                                                                                        treated_id_var = treated,
                                                                                        treated_time_var = post,
                                                                                        time_var = week_id,
                                                                                        spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_neg_binom_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                        ~ optimise_synth(.,
                                                                                         demean_outcomes = TRUE,
                                                                                         denoise_outcomes = FALSE,
                                                                                         objective_function = "DIFP",
                                                                                         n_periods_pre = 26,
                                                                                         n_periods_post = 26,
                                                                                         outcome_var = Y0_treated_neg_binom,
                                                                                         treated_id_var = treated,
                                                                                         treated_time_var = post,
                                                                                         time_var = week_id,
                                                                                         spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_neg_binom_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                       ~ optimise_synth(.,
                                                                                        demean_outcomes = TRUE,
                                                                                        denoise_outcomes = FALSE,
                                                                                        objective_function = "PSC",
                                                                                        n_periods_pre = 26,
                                                                                        n_periods_post = 26,
                                                                                        outcome_var = Y0_treated_neg_binom,
                                                                                        treated_id_var = treated,
                                                                                        treated_time_var = post,
                                                                                        time_var = week_id,
                                                                                        spline_df = NULL))),
  
  # 8. De-meaned outcomes, factor model, random assignment
  
  # ADH
  tar_target(results_synth_adh_factor_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                    ~ optimise_synth(.,
                                                                                     demean_outcomes = TRUE,
                                                                                     denoise_outcomes = FALSE,
                                                                                     objective_function = "ADH",
                                                                                     n_periods_pre = 26,
                                                                                     n_periods_post = 26,
                                                                                     outcome_var = Y0_treated_factor,
                                                                                     treated_id_var = treated,
                                                                                     treated_time_var = post,
                                                                                     time_var = week_id,
                                                                                     spline_df = NULL))),
  
  # DIFP
  tar_target(results_synth_difp_factor_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                     ~ optimise_synth(.,
                                                                                      demean_outcomes = TRUE,
                                                                                      denoise_outcomes = FALSE,
                                                                                      objective_function = "DIFP",
                                                                                      n_periods_pre = 26,
                                                                                      n_periods_post = 26,
                                                                                      outcome_var = Y0_treated_factor,
                                                                                      treated_id_var = treated,
                                                                                      treated_time_var = post,
                                                                                      time_var = week_id,
                                                                                      spline_df = NULL))),
  
  # PSC
  tar_target(results_synth_psc_factor_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                    ~ optimise_synth(.,
                                                                                     demean_outcomes = TRUE,
                                                                                     denoise_outcomes = FALSE,
                                                                                     objective_function = "PSC",
                                                                                     n_periods_pre = 26,
                                                                                     n_periods_post = 26,
                                                                                     outcome_var = Y0_treated_factor,
                                                                                     treated_id_var = treated,
                                                                                     treated_time_var = post,
                                                                                     time_var = week_id,
                                                                                     spline_df = NULL))),
  
  # Extract results across simulations and assign treatment effects ------------
  
  # 1. Empirical distribution assignment
  
  tar_target(data_tau_hat_neg_binom, map(list(results_synth_adh_neg_binom,
                                              results_synth_difp_neg_binom,
                                              results_synth_psc_neg_binom,
                                              results_synth_did_neg_binom), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  tar_target(data_tau_hat_neg_binom_demeaned, map(list(results_synth_adh_neg_binom_demeaned,
                                              results_synth_difp_neg_binom_demeaned,
                                              results_synth_psc_neg_binom_demeaned,
                                              results_synth_did_neg_binom), # Don't need to demean DID since estimator is identical 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  tar_target(data_tau_hat_factor, map(list(results_synth_adh_factor,
                                           results_synth_difp_factor,
                                           results_synth_psc_factor,
                                           results_synth_did_factor), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  tar_target(data_tau_hat_factor_demeaned, map(list(results_synth_adh_factor_demeaned,
                                                    results_synth_difp_factor_demeaned,
                                                    results_synth_psc_factor_demeaned,
                                                    results_synth_did_factor), # DID estimator is identical under demeaning 
                                      ~extract_tau_hat_synth_results(.,
                                                                     treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  # 2. Random assignment
  
  tar_target(data_tau_hat_neg_binom_random_assignment, map(list(results_synth_adh_neg_binom_random_assignment,
                                              results_synth_difp_neg_binom_random_assignment,
                                              results_synth_psc_neg_binom_random_assignment,
                                              results_synth_did_neg_binom_random_assignment), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  tar_target(data_tau_hat_neg_binom_demeaned_random_assignment, map(list(results_synth_adh_neg_binom_demeaned_random_assignment,
                                                       results_synth_difp_neg_binom_demeaned_random_assignment,
                                                       results_synth_psc_neg_binom_demeaned_random_assignment,
                                                       results_synth_did_neg_binom_random_assignment), # DID estimator is identical under demeaning 
                                                  ~extract_tau_hat_synth_results(.,
                                                                                 treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  tar_target(data_tau_hat_factor_random_assignment, map(list(results_synth_adh_factor_random_assignment,
                                           results_synth_difp_factor_random_assignment,
                                           results_synth_psc_factor_random_assignment,
                                           results_synth_did_factor_random_assignment), 
                                      ~extract_tau_hat_synth_results(.,
                                                                     treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  tar_target(data_tau_hat_factor_demeaned_random_assignment, map(list(results_synth_adh_factor_demeaned_random_assignment,
                                                    results_synth_difp_factor_demeaned_random_assignment,
                                                    results_synth_psc_factor_demeaned_random_assignment,
                                                    results_synth_did_factor_random_assignment), # DID estimator is identical under demeaning 
                                               ~extract_tau_hat_synth_results(.,
                                                                              treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  ########################################################################################################
  ### RESULTS FROM MAIN ANALYSIS #########################################################################
  ########################################################################################################
  
  tar_target(results_synth_main_all_cause, future_map(list_data_for_synth,
                                                      function(x){
                                                        n_periods_pre <- nrow(filter(x, 
                                                                                     treated == 1,
                                                                                     post == 0))
                                                        n_periods_post <- nrow(filter(x,
                                                                                      treated == 1,
                                                                                      post == 1))
                                                        
                                                        # Set maximum value of n_periods_pre to 100
                                                        n_periods_pre <- min(n_periods_pre, 100)
                                                        
                                                        optimise_synth(data = x,
                                                                       demean_outcomes = TRUE,
                                                                       denoise_outcomes = FALSE,
                                                                       objective_function = "PSC",
                                                                       n_periods_pre = n_periods_pre,
                                                                       n_periods_post = n_periods_post,
                                                                       outcome_var = all,
                                                                       treated_id_var = treated,
                                                                       treated_time_var = post,
                                                                       time_var = week_id,
                                                                       spline_df = NULL)
                                                      })),
  
  # Extract results from main analysis
  tar_target(data_results_synth_main_all_cause, extract_synth_results_main(list_data_for_synth = list_data_for_synth,
                                                                           list_results_synth = results_synth_main_all_cause)),
  
  ##############################################################################################################################
  ### PLOTS FROM SIMULATION STUDY ##############################################################################################
  ##############################################################################################################################
  
  # Make output plots - simulation study ------------------------------------------------------------------------
  
  tar_target(patchwork_plot_line_real_vs_sim_data, (make_patchwork_plot(list = list(make_line_plot_real_vs_sim_data(list_data = list_data_simulated, 
                                                                                                                   location_var = column_label, 
                                                                                                                   location_string = "toky.jap7220", 
                                                                                                                   time_var = week_id, 
                                                                                                                   min_time_var = 900, 
                                                                                                                   outcome_var = all, 
                                                                                                                   outcome_sim_var = Y0_treated_neg_binom) +
                                                                                      ggtitle("A: Tokyo, Negative Binomial") +
                                                                                      labs(x = "",
                                                                                           y = "All-cause\nmortality"),
                                                                                   make_line_plot_real_vs_sim_data(list_data = list_data_simulated, 
                                                                                                                   location_var = column_label, 
                                                                                                                   location_string = "toky.jap7220", 
                                                                                                                   time_var = week_id, 
                                                                                                                   min_time_var = 900, 
                                                                                                                   outcome_var = all, 
                                                                                                                   outcome_sim_var = Y0_treated_factor) + 
                                                                                     ggtitle("B: Tokyo, Factor") +
                                                                                     labs(x = "",
                                                                                          y = ""),
                                                                                   make_line_plot_real_vs_sim_data(list_data = list_data_simulated, 
                                                                                                                   location_var = column_label, 
                                                                                                                   location_string = "srwk.mal0019", 
                                                                                                                   time_var = week_id, 
                                                                                                                   min_time_var = 900, 
                                                                                                                   outcome_var = all, 
                                                                                                                   outcome_sim_var = Y0_treated_neg_binom) +
                                                                                     ggtitle("C: Sarawak, Negative Binomial") +
                                                                                     labs(x = "Week",
                                                                                          y = ""),
                                                                                   make_line_plot_real_vs_sim_data(list_data = list_data_simulated, 
                                                                                                                   location_var = column_label, 
                                                                                                                   location_string = "srwk.mal0019", 
                                                                                                                   time_var = week_id, 
                                                                                                                   min_time_var = 900, 
                                                                                                                   outcome_var = all, 
                                                                                                                   outcome_sim_var = Y0_treated_factor) +
                                                                                     ggtitle("D: Sarawak, Factor") + 
                                                                                     labs(x = "Week",
                                                                                          y = "")
                                                                                   ),
                                                                       guides = "collect",
                                                                       ncol = 2
                                                                       )) %>%
               
               ggsave("Output/Figures/Simulation/patchwork_plot_line_real_vs_sim_data.png", ., height = 5, width = 8, dpi = 700, create.dir = TRUE),
             format = "file"
             ),
  
  # 1. Assignment based on empirical distribution, no de-meaning/de-noising
  
  # Density plot of tau hat from negative binomial model and factor model - placebo effect
  tar_target(patchwork_density_tau_hat, (make_patchwork_plot(
    
    list = list(
      
      make_density_plot_synth_results(
      data = data_tau_hat_neg_binom[data_tau_hat_neg_binom$post == 1, ],
      density_var = tau_hat_normalised,
      method_var = method,
      model_run_var = model_run,
      palette = cbbPalette) +
        ggtitle("A: Negative Binomial") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor[data_tau_hat_factor$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1)
      ),
    ncol = 2,
    guides = "collect",
    legend_position = "bottom"
    )) %>%
      
      ggsave("Output/Figures/Simulation/patchwork_density_tau_hat.png", ., width = 8, height = 5, dpi = 700, create.dir = TRUE)
    ),
  
  # 2. Empirical distribution assignment, de-meaned data
  # Density plot of tau hat from negative binomial model and factor model - placebo effect
  tar_target(patchwork_density_tau_hat_demeaned, (make_patchwork_plot(
    
    list = list(
      
      make_density_plot_synth_results(
        data = data_tau_hat_neg_binom_demeaned[data_tau_hat_neg_binom_demeaned$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("A: Negative Binomial") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor_demeaned[data_tau_hat_factor_demeaned$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1)
    ),
    ncol = 2,
    guides = "collect",
    legend_position = "bottom"
  )) %>%
    
    ggsave("Output/Figures/Simulation/patchwork_density_tau_hat_demeaned.png", ., width = 8, height = 5, dpi = 700, create.dir = TRUE)
  ),
  
  # 4. Random assignment, no de-meaning or de-noising
  
  # Density plot of tau hat from negative binomial model and factor model - placebo effect
  tar_target(patchwork_density_tau_hat_random_assignment, (make_patchwork_plot(
    
    list = list(
      
      make_density_plot_synth_results(
        data = data_tau_hat_neg_binom_random_assignment[data_tau_hat_neg_binom_random_assignment$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("A: Negative Binomial") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor_random_assignment[data_tau_hat_factor_random_assignment$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1)
    ),
    ncol = 2,
    guides = "collect",
    legend_position = "bottom"
  )) %>%
    
    ggsave("Output/Figures/Simulation/patchwork_density_tau_hat_random_assignment.png", ., width = 8, height = 5, dpi = 700, create.dir = TRUE)
  ),
  
  # 5. Random assignment, de-meaned data
  # Density plot of tau hat from negative binomial model and factor model - placebo effect
  tar_target(patchwork_density_tau_hat_demeaned_random_assignment, (make_patchwork_plot(
    
    list = list(
      
      make_density_plot_synth_results(
        data = data_tau_hat_neg_binom_demeaned_random_assignment[data_tau_hat_neg_binom_demeaned_random_assignment$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("A: Negative Binomial") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor_demeaned_random_assignment[data_tau_hat_factor_demeaned_random_assignment$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = "Tau hat",
             y = "Density") +
        xlim(-1,1)
    ),
    ncol = 2,
    guides = "collect",
    legend_position = "bottom"
  )) %>%
    
    ggsave("Output/Figures/Simulation/patchwork_density_tau_hat_demeaned_random_assignment.png", ., width = 8, height = 5, dpi = 700, create.dir = TRUE)
  ),
  
  # Plots of absolute bias in normalised tau hat against Y -----------------------------------------------------------------------------------------------
  
  tar_target(scatter_plot_abs_bias_mean_y_neg_binom, (data_tau_hat_neg_binom_demeaned %>%
                                              
                                              # Estimate mean absolute bias and mean outcome during post-treatment period
                                              filter(post == 1) %>%
                                              
                                              summarise(mean_abs_bias = mean(abs(tau_hat_normalised), na.rm = TRUE),
                                                        mean_y = mean(Y1_treated, na.rm = TRUE),
                                                        .by = c(method,
                                                                model_run)) %>%
                                              
                                              ggplot() +
                                              geom_point(aes(x = mean_y,
                                                             y = mean_abs_bias * 100,
                                                             colour = method),
                                                         alpha = 0.4) +
                                              scale_colour_manual(values = cbbPalette) +
                                              facet_wrap(~method,
                                                         scales = "fixed") +
                                              scatter_plot_opts +
                                                labs(x = "Mean weekly mortality - post treatment period",
                                                     y = "Mean absolute bias\nin tau hat (%)",
                                                     colour = "Method")) %>%
               
               ggsave("Output/Figures/Simulation/scatter_plot_abs_bias_mean_y_neg_binom.png", ., width = 8, height = 5),
             format = "file"),
  
  tar_target(scatter_plot_abs_bias_mean_y_factor, (data_tau_hat_factor_demeaned %>%
                                                        
                                                        # Estimate mean absolute bias and mean outcome during post-treatment period
                                                        filter(post == 1) %>%
                                                        
                                                        summarise(mean_abs_bias = mean(abs(tau_hat_normalised), na.rm = TRUE),
                                                                  mean_y = mean(Y1_treated, na.rm = TRUE),
                                                                  .by = c(method,
                                                                          model_run)) %>%
                                                        
                                                        ggplot() +
                                                        geom_point(aes(x = mean_y,
                                                                       y = mean_abs_bias * 100,
                                                                       colour = method),
                                                                   alpha = 0.4) +
                                                        scale_colour_manual(values = cbbPalette) +
                                                        facet_wrap(~method,
                                                                   scales = "fixed") +
                                                        scatter_plot_opts +
                                                     labs(x = "Mean weekly mortality - post treatment period",
                                                          y = "Mean absolute bias\n in tau hat (%)",
                                                          colour = "Method")) %>%
               
               ggsave("Output/Figures/Simulation/scatter_plot_abs_bias_mean_y_factor.png", ., width = 8, height = 5),
             format = "file"),
  
  #######################################################################################################################################
  ### TABLES FROM SIMULATION STUDY ######################################################################################################
  #######################################################################################################################################
  
  # Summary tables of synth diagnostics by treatment effect type and method -------------------------------------------------------------
  
  # Empirical assignment, no de-meaning/de-noising
  tar_target(tbl_summary_synth_diagnostics, 
             
             bind_rows(make_summary_table_synth_diagnostics(data_tau_hat_neg_binom, 
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "negative_binomial"), 
                       make_summary_table_synth_diagnostics(data_tau_hat_factor,
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "factor")) %>%
               
               # Pivot wider
               pivot_wider(
                 names_from = dgp_type,
                 values_from = c(indiv_rmse,
                                 agg_rmse,
                                 indiv_abs_bias,
                                 agg_abs_bias),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   indiv_rmse_negative_binomial,
                   agg_rmse_negative_binomial,
                   indiv_abs_bias_negative_binomial,
                   agg_abs_bias_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   indiv_rmse_factor,
                   agg_rmse_factor,
                   indiv_abs_bias_factor,
                   agg_abs_bias_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 indiv_rmse_negative_binomial = "Indiv RMSE",
                 agg_rmse_negative_binomial = "Aggregate RMSE",
                 indiv_abs_bias_negative_binomial = "Indiv |Bias|",
                 agg_abs_bias_negative_binomial = "Aggregate |Bias|",
                 indiv_rmse_factor = "Indiv RMSE",
                 agg_rmse_factor = "Aggregate RMSE",
                 indiv_abs_bias_factor = "Indiv |Bias|",
                 agg_abs_bias_factor = "Aggregate |Bias|"
               ) %>%
               
               fmt_number(
                 column = -method,
                 decimals = 2
               ) %>%
               
               gtsave("Output/Tables/Simulation/tbl_summary_synth_diagnostics.tex"),
             format = "file"),
  
  tar_target(tbl_summary_synth_diagnostics_demeaned, 
             
             bind_rows(make_summary_table_synth_diagnostics(data_tau_hat_neg_binom_demeaned, 
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "negative_binomial"), 
                       make_summary_table_synth_diagnostics(data_tau_hat_factor_demeaned,
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "factor")) %>%
               
               # Pivot wider
               pivot_wider(
                 names_from = dgp_type,
                 values_from = c(indiv_rmse,
                                 agg_rmse,
                                 indiv_abs_bias,
                                 agg_abs_bias),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   indiv_rmse_negative_binomial,
                   agg_rmse_negative_binomial,
                   indiv_abs_bias_negative_binomial,
                   agg_abs_bias_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   indiv_rmse_factor,
                   agg_rmse_factor,
                   indiv_abs_bias_factor,
                   agg_abs_bias_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 indiv_rmse_negative_binomial = "Indiv RMSE",
                 agg_rmse_negative_binomial = "Aggregate RMSE",
                 indiv_abs_bias_negative_binomial = "Indiv |Bias|",
                 agg_abs_bias_negative_binomial = "Aggregate |Bias|",
                 indiv_rmse_factor = "Indiv RMSE",
                 agg_rmse_factor = "Aggregate RMSE",
                 indiv_abs_bias_factor = "Indiv |Bias|",
                 agg_abs_bias_factor = "Aggregate |Bias|"
               ) %>%
               
               fmt_number(
                 column = -method,
                 decimals = 2
               ) %>%
               
               gtsave("Output/Tables/Simulation/tbl_summary_synth_diagnostics_demeaned.tex"),
             format = "file"),
  
  # Random assignment
  tar_target(tbl_summary_synth_diagnostics_random_assignment, 
             
             bind_rows(make_summary_table_synth_diagnostics(data_tau_hat_neg_binom_random_assignment, 
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "negative_binomial"), 
                       make_summary_table_synth_diagnostics(data_tau_hat_factor_random_assignment,
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "factor")) %>%
               
               # Pivot wider
               pivot_wider(
                 names_from = dgp_type,
                 values_from = c(indiv_rmse,
                                 agg_rmse,
                                 indiv_abs_bias,
                                 agg_abs_bias),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   indiv_rmse_negative_binomial,
                   agg_rmse_negative_binomial,
                   indiv_abs_bias_negative_binomial,
                   agg_abs_bias_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   indiv_rmse_factor,
                   agg_rmse_factor,
                   indiv_abs_bias_factor,
                   agg_abs_bias_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 indiv_rmse_negative_binomial = "Indiv RMSE",
                 agg_rmse_negative_binomial = "Aggregate RMSE",
                 indiv_abs_bias_negative_binomial = "Indiv |Bias|",
                 agg_abs_bias_negative_binomial = "Aggregate |Bias|",
                 indiv_rmse_factor = "Indiv RMSE",
                 agg_rmse_factor = "Aggregate RMSE",
                 indiv_abs_bias_factor = "Indiv |Bias|",
                 agg_abs_bias_factor = "Aggregate |Bias|"
               ) %>%
               
               fmt_number(
                 column = -method,
                 decimals = 2
               ) %>%
               
               gtsave("Output/Tables/Simulation/tbl_summary_synth_diagnostics_random_assignment.tex"),
             format = "file"),
  
  # De-meaned outcomes
  tar_target(tbl_summary_synth_diagnostics_demeaned_random_assignment, 
             
             bind_rows(make_summary_table_synth_diagnostics(data_tau_hat_neg_binom_demeaned_random_assignment, 
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "negative_binomial"), 
                       make_summary_table_synth_diagnostics(data_tau_hat_factor_demeaned_random_assignment,
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "factor")) %>%
               
               # Pivot wider
               pivot_wider(
                 names_from = dgp_type,
                 values_from = c(indiv_rmse,
                                 agg_rmse,
                                 indiv_abs_bias,
                                 agg_abs_bias),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   indiv_rmse_negative_binomial,
                   agg_rmse_negative_binomial,
                   indiv_abs_bias_negative_binomial,
                   agg_abs_bias_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   indiv_rmse_factor,
                   agg_rmse_factor,
                   indiv_abs_bias_factor,
                   agg_abs_bias_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 indiv_rmse_negative_binomial = "Indiv RMSE",
                 agg_rmse_negative_binomial = "Aggregate RMSE",
                 indiv_abs_bias_negative_binomial = "Indiv |Bias|",
                 agg_abs_bias_negative_binomial = "Aggregate |Bias|",
                 indiv_rmse_factor = "Indiv RMSE",
                 agg_rmse_factor = "Aggregate RMSE",
                 indiv_abs_bias_factor = "Indiv |Bias|",
                 agg_abs_bias_factor = "Aggregate |Bias|"
               ) %>%
               
               fmt_number(
                 column = -method,
                 decimals = 2
               ) %>%
               
               gtsave("Output/Tables/Simulation/tbl_summary_synth_diagnostics_demeaned_random_assignment.tex"),
             format = "file"),
  
  ##########################################################################################################
  ### EMPIRICAL APPLICATION ################################################################################
  ##########################################################################################################
  
  ### Select data subset for analysis
  tar_target(data_for_case_study, list_data_for_synth[["567"]]
  ),
  
  tar_target(results_case_study_adh, optimise_synth(data = data_for_case_study,
                                                    demean_outcomes = TRUE,
                                                    denoise_outcomes = FALSE,
                                                    objective_function = "ADH",
                                                    n_periods_pre = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 0]),
                                                    n_periods_post = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 1]),
                                                    outcome_var = all,
                                                    treated_id_var = treated,
                                                    treated_time_var = post,
                                                    time_var = week_id,
                                                    spline_df = NULL)
             ),
  
  tar_target(results_case_study_psc, optimise_synth(data = data_for_case_study,
                                                    demean_outcomes = TRUE,
                                                    denoise_outcomes = FALSE,
                                                    objective_function = "PSC",
                                                    n_periods_pre = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 0]),
                                                    n_periods_post = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 1]),
                                                    outcome_var = all,
                                                    treated_id_var = treated,
                                                    treated_time_var = post,
                                                    time_var = week_id,
                                                    spline_df = NULL)
  ),
  
  tar_target(results_case_study_difp, optimise_synth(data = data_for_case_study,
                                                    demean_outcomes = TRUE,
                                                    denoise_outcomes = FALSE,
                                                    objective_function = "DIFP",
                                                    n_periods_pre = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 0]),
                                                    n_periods_post = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 1]),
                                                    outcome_var = all,
                                                    treated_id_var = treated,
                                                    treated_time_var = post,
                                                    time_var = week_id,
                                                    spline_df = NULL)
  ),
  
  tar_target(results_case_study_did, optimise_synth(data = data_for_case_study,
                                                     demean_outcomes = TRUE,
                                                     denoise_outcomes = FALSE,
                                                     objective_function = "DID",
                                                     n_periods_pre = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 0]),
                                                     n_periods_post = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 1]),
                                                     outcome_var = all,
                                                     treated_id_var = treated,
                                                     treated_time_var = post,
                                                     time_var = week_id,
                                                     spline_df = NULL)
  ),
  
  # Store results in tibble
  tar_target(data_results_case_study, tibble(outcome = c(results_case_study_adh$Y_treated,
                                                        results_case_study_adh$Y0_treated_hat,
                                                        results_case_study_psc$Y0_treated_hat,
                                                        results_case_study_difp$Y0_treated_hat,
                                                        results_case_study_did$Y0_treated_hat),
                                             method = c(rep("True", length(data_for_case_study$post[data_for_case_study$treated == 1])), 
                                                        rep("ADH", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("PSC", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("DIFP", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("DID", length(data_for_case_study$post[data_for_case_study$treated == 1]))),
                                             t = rep(1:length(data_for_case_study$post[data_for_case_study$treated == 1]), 5))
             ),
  
  # Plot outcome trajectories
  tar_target(plot_results_case_study, (data_results_case_study %>%
               
               ggplot() +
               
               geom_line(aes(x = t,
                             y = outcome,
                             colour = method)) +
               
               geom_vline(xintercept = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 0]),
                          linetype = "dashed") +
               
               ylim(0, 800) +
               
               labs(x = "Week",
                    y = "Weekly mortality",
                    colour = "Method") +
                 
                 theme(legend.position = "bottom") +
               
               scatter_plot_opts +
                 
                 scale_colour_manual(values = cbbPalette)) %>%
               
               ggsave("Output/Figures/Main/plot_results_case_study.png", ., width = 8, height = 5)
             )
)