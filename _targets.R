# =============================================================================
# _targets.R — Analysis Pipeline for MCC Synthetic Control Study
# =============================================================================
#
# Author:  Calum Kennedy (calum.kennedy.25@ucl.ac.uk)
# Project: Estimating the causal effect of acute landscape fire smoke (LFS)
#          PM2.5 exposure on mortality using synthetic control methods.
#          Data: Multi-Country Multi-City (MCC) Collaborative Research Network.
#
# -----------------------------------------------------------------------------
# OVERVIEW
# -----------------------------------------------------------------------------
# This file defines the targets pipeline (via the {targets} package) for the
# full analysis. It is the single entry point for running all data preparation,
# simulation, and empirical estimation steps. Running `targets::tar_make()`
# from the project root executes the pipeline; `targets::tar_visnetwork()`
# renders the dependency graph.
#
# All R functions called here are defined in R/ and R/simulation/, and are
# loaded via `tar_source("functions.R")`. Parallel execution uses {furrr}
# (multisession) for simulation loops and is configured at the top of this
# file.
#
# -----------------------------------------------------------------------------
# DATA SOURCES
# -----------------------------------------------------------------------------
# Both datasets are stored in the data/ folder (not tracked by git).
#
# - MCC mortality data:
#     Daily city-level mortality counts (all-cause, CVD, respiratory,
#     non-external), mean temperature, and city metadata for cities in the
#     MCC network. Access is restricted to MCC network collaborators.
#
# - LFS pollution data:
#     Daily city-level predicted fire and non-fire PM2.5 concentrations
#     (and total PM2.5, O3), matched to MCC cities.
#
# -----------------------------------------------------------------------------
# KEY ANALYSIS PARAMETERS
# -----------------------------------------------------------------------------
# Treatment definition (acute LFS episode onset):
#   - Threshold:      99th percentile of predicted fire PM2.5 (city-specific)
#   - Onset rule:     >= 3 consecutive days above threshold
#   - Offset rule:    >= 28 consecutive days below threshold
#
# Temporal aggregation: daily -> weekly (primary), daily -> monthly (secondary)
#
# Simulation study sample:
#   - Region:         Eastern Asia only (excludes Taiwan; Korea truncated
#                     at 2018-12-22 to avoid missing data)
#   - Date range:     2000-01-01 to 2018-12-22
#   - Eligibility:    mean weekly mortality > 100 deaths, complete tmean data
#   - Replicates:     500 simulations per DGP
#   - Pre/post window: 26 weeks each
#
# Empirical case study:
#   - Episode ID:     "567" from list_data_for_synth
#   - Eligibility:    mean weekly mortality > 100 deaths
#   - CI method:      conformal inference (block permutation, L1 norm, alpha = 0.05)
#   - Null grid:      seq(-200, 200, length.out = 100)
#
# -----------------------------------------------------------------------------
# PIPELINE STAGES
# -----------------------------------------------------------------------------
#
# Stage 1 — Data ingestion and preprocessing
#   Loads MCC and LFS data, merges them (with 2-lag moving averages for fire
#   PM2.5 and all pollutants), generates weekly treatment indicators, and
#   aggregates to weekly and monthly panels.
#   Key targets: data_mcc_cleaned, data_mcc_lfs, data_mcc_lfs_with_treatment,
#                data_mcc_lfs_weekly, data_mcc_lfs_monthly
#
# Stage 2 — SC data partitioning
#   Splits the weekly and monthly panels into a named list of per-episode
#   datasets, applying eligibility filters (minimum pre-treatment periods,
#   minimum control units, minimum weekly mortality).
#   Key targets: list_data_for_synth, list_data_for_synth_monthly
#
# Stage 3 — Simulation study: DGP fitting and outcome simulation
#   Restricts to the Eastern Asia simulation sample and fits two DGPs:
#   (1) Negative binomial GLM with natural spline time trend and temperature
#       (7 df/year, 4 df temperature), non-fire PM2.5 as linear predictor.
#   (2) Low-rank factor model (rank 4) on the pre-/post-treatment panel.
#   Draws 500 independent outcome replicates from each DGP, and constructs
#   simulation datasets under both empirical and uniform-random treatment
#   assignment. Also extracts the DGP-implied expected values for the treated
#   unit in each replicate (used as the truth benchmark).
#   Key targets: list_outcome_sim_neg_binomial, list_outcome_sim_factor,
#                list_data_simulated, list_data_simulated_random_assignment,
#                outcome_exp_val_neg_binom, outcome_exp_val_factor,
#                outcome_exp_val_neg_binom_treated_units,
#                outcome_exp_val_factor_treated_units
#
# Stage 4 — Simulation study: SC estimation
#   Runs all six SC estimators across every simulation replicate, DGP, and
#   assignment scheme, under both raw and de-meaned outcomes. Each combination
#   is a separate target, parallelised via future_map().
#   Estimators: ADH, ADH subset, DIFP, PSC, DID, 1-NN matching
#   DGPs:       Negative binomial, Factor model
#   Assignment: Empirical distribution, Uniform random
#   Demeaning:  Raw outcomes, De-meaned outcomes
#   (48 results_synth_* targets in total)
#
# Stage 5 — Simulation study: result extraction and diagnostics
#   Extracts estimated and true treatment effects from all simulation results,
#   merges in DGP-implied expected values, and computes a normalised treatment
#   effect estimate. Also fits a negative binomial model to factor-model
#   outcomes to assess misspecification (data_coef_pred_fire_PM25_*).
#   Key targets: data_tau_hat_neg_binom, data_tau_hat_factor,
#                data_tau_hat_*_demeaned, data_tau_hat_*_random_assignment
#
# Stage 6 — Simulation study: plots and tables
#   Produces all simulation output figures and LaTeX tables:
#   - Line plots comparing real vs. simulated outcome trajectories
#   - Density plots of normalised tau-hat by method and DGP
#   - Scatter plots of absolute bias vs. mean outcome level
#   - Scatter plots of pre- vs. post-treatment RMSPE
#   - Summary tables of ATT bias/SD and per-period bias/SD (saved as .tex)
#   Output directory: Output/Figures/Simulation/, Output/Tables/Simulation/
#
# Stage 7 — Empirical case study
#   Applies all six SC estimators to episode "567" (filtered to cities with
#   mean weekly mortality > 100). Constructs conformal inference confidence
#   intervals for each method by inverting a null hypothesis grid over the
#   post-treatment period. Produces the main results figure showing per-period
#   treatment effect estimates with confidence bands.
#   Key targets: data_for_case_study, results_case_study_*,
#                df_confidence_intervals_case_study_*,
#                data_results_case_study_conf_int,
#                plot_results_case_study_diff
#   Output directory: Output/Figures/Main/
#
# =============================================================================

# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(tidylog)
library(here)
library(ggplot2)
library(viridis)
library(crew)
library(crew.cluster)
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
  garbage_collection = TRUE#,
  #controller = crew_controller_local(workers = 10,
                                     #seconds_idle = 10)
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  #
    # controller = crew.cluster::crew_controller_sge(
    #   workers = 8,
    #   seconds_idle = 120,
    #   options_cluster = crew_options_sge(
    #   script_lines = c(
    #     "module -f unload compilers mpi gcc-libs",
    #     "module load cmake/3.27.3",
    #     "module load pandoc",
    #     "module load glpk",
    #     "module load r/r-4.4.2_bc-3.20",
    #     "#$ -l mem=4G",
    #     "#$ -pe smp 8"
    #     )
    #   )
    # )
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
  
  tar_target(
    data_mcc_lfs_monthly, make_data_mcc_lfs_monthly(data_mcc_lfs = data_mcc_lfs_with_treatment,
                                                   climatic_vars = c("tmean",
                                                                     "pred_fire_PM25",
                                                                     "pred_total_PM25",
                                                                     "pred_nonfire_PM25"),
                                                   mortality_vars = c("all",
                                                                      "nonext",
                                                                      "cvd",
                                                                      "resp"),
                                                   group_vars = c("column_label",
                                                                  "month_id",
                                                                  "countryname",
                                                                  "region"))
  ),
  
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
  
  tar_target(list_data_for_synth_monthly, partition_data_for_synth(data = data_mcc_lfs_monthly,
                                                           unit_id_var = column_label,
                                                           time_id_var = month_id,
                                                           region_id_var = region,
                                                           treated_var = treated,
                                                           outcome_var = all,
                                                           min_periods_pre = 10,
                                                           min_control_units = 5,
                                                           min_weekly_mortality = 700)),
  
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
               
               select(-mean_mortality)
               ),
  
  # Get expected value of outcome series - negative binomial model
  tar_target(outcome_exp_val_neg_binom, tibble(outcome_exp_val = get_outcome_exp_val_negative_binomial_model(data = data_for_simulation, 
                                                                                    unit_id_var = column_label, 
                                                                                    time_id_var = week_id,
                                                                                    week_id_var = week_id,
                                                                                    treated_var = treated,
                                                                                    outcome_var = all, 
                                                                                    year_var = year,
                                                                                    linear_predictors = "pred_nonfire_PM25", 
                                                                                    temp_var = tmean, 
                                                                                    spline_df_per_year = 7, 
                                                                                    spline_df_temp = 4),
                                               week_id = data_for_simulation$week_id,
                                               column_label = data_for_simulation$column_label)
             ),
  
  # Expected value of outcome series - factor model
  tar_target(outcome_exp_val_factor, tibble(outcome_exp_val = get_outcome_exp_val_factor_model(data = data_for_simulation,
                                                                      unit_id_var = column_label,
                                                                      time_id_var = week_id,
                                                                      outcome_var = all,
                                                                      treated_var = treated,
                                                                      week_id_var = week_id,
                                                                      rank = 4),
                                            week_id = data_for_simulation$week_id,
                                            column_label = data_for_simulation$column_label)
             ),
  
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
  
  # Get list of treated units from simulated data
  tar_target(list_treated_unit_time, map(list_data_simulated, ~ {
    
    treated_unit <- as.character(pull(distinct(filter(.x, .x$treated == 1), column_label)))
    min_t <- min(.x$week_id)
    max_t <- max(.x$week_id)
    return(list(treated_unit = treated_unit,
                min_t = min_t,
                max_t = max_t))
    }
    )
    ),
  
  # Get list of treated units from simulated data
  tar_target(list_treated_unit_time_random_assignment, map(list_data_simulated_random_assignment, ~ {
    
    treated_unit <- as.character(pull(distinct(filter(.x, .x$treated == 1), column_label)))
    min_t <- min(.x$week_id)
    max_t <- max(.x$week_id)
    return(list(treated_unit = treated_unit,
                min_t = min_t,
                max_t = max_t))
  }
  )
  ),
  
  # Get vector of true expected value of outcome series for the treated units/time periods for 
  # negative binomial model (replicate six times because we use six estimators)
  tar_target(outcome_exp_val_neg_binom_treated_units, map(list_treated_unit_time, ~ outcome_exp_val_neg_binom %>% 
                                                            filter(between(week_id, .x$min_t, .x$max_t), 
                                                                   column_label == .x$treated_unit) %>% 
                                                            pull(outcome_exp_val)) %>% 
               unlist() %>%
               rep(6)),
  
  # Get vector of true expected value of outcome series for the treated units/time periods for 
  # negative binomial model (replicate six times because currently use six estimators)
  tar_target(outcome_exp_val_factor_treated_units, map(list_treated_unit_time, ~ outcome_exp_val_factor %>% 
                                                            filter(between(week_id, .x$min_t, .x$max_t), 
                                                                   column_label == .x$treated_unit) %>% 
                                                            pull(outcome_exp_val)) %>% 
               unlist() %>%
               rep(6)),
  
  # Get vector of true expected value of outcome series for the treated units/time periods under random assignment for 
  # negative binomial model (replicate six times because we use six estimators)
  tar_target(outcome_exp_val_neg_binom_treated_units_random_assignment, map(list_treated_unit_time_random_assignment, ~ outcome_exp_val_neg_binom %>% 
                                                            filter(between(week_id, .x$min_t, .x$max_t), 
                                                                   column_label == .x$treated_unit) %>% 
                                                            pull(outcome_exp_val)) %>% 
               unlist() %>%
               rep(6)),
  
  # Get vector of true expected value of outcome series for the treated units/time periods under random assignment for 
  # negative binomial model (replicate six times because currently use six estimators)
  tar_target(outcome_exp_val_factor_treated_units_random_assignment, map(list_treated_unit_time_random_assignment, ~ outcome_exp_val_factor %>% 
                                                         filter(between(week_id, .x$min_t, .x$max_t), 
                                                                column_label == .x$treated_unit) %>% 
                                                         pull(outcome_exp_val)) %>% 
               unlist() %>%
               rep(6)),
  
  tar_target(list_data_simulated_full, map2(list_outcome_sim_neg_binomial,
                                            list_outcome_sim_factor,
                                            ~ data_for_simulation %>% 
                                              mutate(Y0_treated_neg_binom = .x,
                                                     Y0_treated_factor = .y))
             ),
  
  # Generate simulated data for estimation of `wrong` parametric model
  tar_target(data_simulated_for_model, list_data_simulated_full[[1]]
             ),
  
  # Extract coefficient on pred_fire_PM25 from running 'wrong' negative binomial model
  # on outcomes generated from factor model
  tar_target(data_coef_pred_fire_PM25_factor_outcome_neg_binom_model, 
             
             get_coefficient_pred_fire_pm25_sim_data_neg_binom(
               
               data = data_simulated_for_model, 
               unit_id_var = column_label, 
               time_id_var = week_id,
               week_id_var = week_id,
               treated_var = treated,
               outcome_var = Y0_treated_factor, 
               year_var = year,
               linear_predictors = c("pred_nonfire_PM25"), 
               temp_var = tmean, 
               spline_df_per_year = 7, 
               spline_df_temp = 4)
             
             ),
  
  # Get optimal SC from simulations --------------------------------------------
  
  # 1. No demeaning/denoising, negative binomial model, assignment based on empirical distribution f
  
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
  
  # ADH subset
  tar_target(results_synth_adh_subset_neg_binom, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "ADH subset",
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
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_neg_binom, future_map(list_data_simulated,
                                                     ~ optimise_synth(.,
                                                                      demean_outcomes = FALSE,
                                                                      denoise_outcomes = FALSE,
                                                                      objective_function = "1NN matching",
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
  
  # ADH subset
  tar_target(results_synth_adh_subset_factor, future_map(list_data_simulated,
                                                  ~ optimise_synth(.,
                                                                   demean_outcomes = FALSE,
                                                                   denoise_outcomes = FALSE,
                                                                   objective_function = "ADH subset",
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
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_factor, future_map(list_data_simulated,
                                                  ~ optimise_synth(.,
                                                                   demean_outcomes = FALSE,
                                                                   denoise_outcomes = FALSE,
                                                                   objective_function = "1NN matching",
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
  
  # ADH
  tar_target(results_synth_adh_subset_neg_binom_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                       ~ optimise_synth(.,
                                                                                        demean_outcomes = FALSE,
                                                                                        denoise_outcomes = FALSE,
                                                                                        objective_function = "ADH subset",
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
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_neg_binom_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                       ~ optimise_synth(.,
                                                                                        demean_outcomes = FALSE,
                                                                                        denoise_outcomes = FALSE,
                                                                                        objective_function = "1NN matching",
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
  
  # ADH subset
  tar_target(results_synth_adh_subset_factor_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                    ~ optimise_synth(.,
                                                                                     demean_outcomes = FALSE,
                                                                                     denoise_outcomes = FALSE,
                                                                                     objective_function = "ADH subset",
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
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_factor_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                    ~ optimise_synth(.,
                                                                                     demean_outcomes = FALSE,
                                                                                     denoise_outcomes = FALSE,
                                                                                     objective_function = "1NN matching",
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
  
  # ADH subset
  tar_target(results_synth_adh_subset_neg_binom_demeaned, future_map(list_data_simulated,
                                                              ~ optimise_synth(.,
                                                                               demean_outcomes = TRUE,
                                                                               denoise_outcomes = FALSE,
                                                                               objective_function = "ADH subset",
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
  
  # DID
  tar_target(results_synth_did_neg_binom_demeaned, future_map(list_data_simulated,
                                                              ~ optimise_synth(.,
                                                                               demean_outcomes = TRUE,
                                                                               denoise_outcomes = FALSE,
                                                                               objective_function = "DID",
                                                                               n_periods_pre = 26,
                                                                               n_periods_post = 26,
                                                                               outcome_var = Y0_treated_neg_binom,
                                                                               treated_id_var = treated,
                                                                               treated_time_var = post,
                                                                               time_var = week_id,
                                                                               spline_df = NULL))),
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_neg_binom_demeaned, future_map(list_data_simulated,
                                                              ~ optimise_synth(.,
                                                                               demean_outcomes = TRUE,
                                                                               denoise_outcomes = FALSE,
                                                                               objective_function = "1NN matching",
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
  
  # ADH subset
  tar_target(results_synth_adh_subset_factor_demeaned, future_map(list_data_simulated,
                                                           ~ optimise_synth(.,
                                                                            demean_outcomes = TRUE,
                                                                            denoise_outcomes = FALSE,
                                                                            objective_function = "ADH subset",
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
  
  # DID
  tar_target(results_synth_did_factor_demeaned, future_map(list_data_simulated,
                                                           ~ optimise_synth(.,
                                                                            demean_outcomes = TRUE,
                                                                            denoise_outcomes = FALSE,
                                                                            objective_function = "DID",
                                                                            n_periods_pre = 26,
                                                                            n_periods_post = 26,
                                                                            outcome_var = Y0_treated_factor,
                                                                            treated_id_var = treated,
                                                                            treated_time_var = post,
                                                                            time_var = week_id,
                                                                            spline_df = NULL))),
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_factor_demeaned, future_map(list_data_simulated,
                                                           ~ optimise_synth(.,
                                                                            demean_outcomes = TRUE,
                                                                            denoise_outcomes = FALSE,
                                                                            objective_function = "1NN matching",
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
  
  # ADH subset
  tar_target(results_synth_adh_subset_neg_binom_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                                ~ optimise_synth(.,
                                                                                                 demean_outcomes = TRUE,
                                                                                                 denoise_outcomes = FALSE,
                                                                                                 objective_function = "ADH subset",
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
  
  # DID
  tar_target(results_synth_did_neg_binom_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                                ~ optimise_synth(.,
                                                                                                 demean_outcomes = TRUE,
                                                                                                 denoise_outcomes = FALSE,
                                                                                                 objective_function = "DID",
                                                                                                 n_periods_pre = 26,
                                                                                                 n_periods_post = 26,
                                                                                                 outcome_var = Y0_treated_neg_binom,
                                                                                                 treated_id_var = treated,
                                                                                                 treated_time_var = post,
                                                                                                 time_var = week_id,
                                                                                                 spline_df = NULL))),
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_neg_binom_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                                ~ optimise_synth(.,
                                                                                                 demean_outcomes = TRUE,
                                                                                                 denoise_outcomes = FALSE,
                                                                                                 objective_function = "1NN matching",
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
  
  # ADH subset
  tar_target(results_synth_adh_subset_factor_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                             ~ optimise_synth(.,
                                                                                              demean_outcomes = TRUE,
                                                                                              denoise_outcomes = FALSE,
                                                                                              objective_function = "ADH subset",
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
  
  # DID
  tar_target(results_synth_did_factor_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                             ~ optimise_synth(.,
                                                                                              demean_outcomes = TRUE,
                                                                                              denoise_outcomes = FALSE,
                                                                                              objective_function = "DID",
                                                                                              n_periods_pre = 26,
                                                                                              n_periods_post = 26,
                                                                                              outcome_var = Y0_treated_factor,
                                                                                              treated_id_var = treated,
                                                                                              treated_time_var = post,
                                                                                              time_var = week_id,
                                                                                              spline_df = NULL))),
  
  # 1-NN matching
  tar_target(results_synth_1NN_matching_factor_demeaned_random_assignment, future_map(list_data_simulated_random_assignment,
                                                                             ~ optimise_synth(.,
                                                                                              demean_outcomes = TRUE,
                                                                                              denoise_outcomes = FALSE,
                                                                                              objective_function = "1NN matching",
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
                                              results_synth_adh_subset_neg_binom,
                                              results_synth_difp_neg_binom,
                                              results_synth_psc_neg_binom,
                                              results_synth_did_neg_binom,
                                              results_synth_1NN_matching_neg_binom), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_neg_binom_treated_units) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  tar_target(data_tau_hat_neg_binom_demeaned, map(list(results_synth_adh_neg_binom_demeaned,
                                                       results_synth_adh_subset_neg_binom_demeaned,
                                              results_synth_difp_neg_binom_demeaned,
                                              results_synth_psc_neg_binom_demeaned,
                                              results_synth_did_neg_binom_demeaned,
                                              results_synth_1NN_matching_neg_binom_demeaned), # Don't need to demean DID since estimator is identical 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_neg_binom_treated_units) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  tar_target(data_tau_hat_factor, map(list(results_synth_adh_factor,
                                           results_synth_adh_subset_factor,
                                           results_synth_difp_factor,
                                           results_synth_psc_factor,
                                           results_synth_did_factor,
                                           results_synth_1NN_matching_factor), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_factor_treated_units) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  tar_target(data_tau_hat_factor_demeaned, map(list(results_synth_adh_factor_demeaned,
                                                    results_synth_adh_subset_factor_demeaned,
                                                    results_synth_difp_factor_demeaned,
                                                    results_synth_psc_factor_demeaned,
                                                    results_synth_did_factor_demeaned,
                                                    results_synth_1NN_matching_factor_demeaned),
                                      ~extract_tau_hat_synth_results(.,
                                                                     treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_factor_treated_units) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  # 2. Random assignment
  
  tar_target(data_tau_hat_neg_binom_random_assignment, map(list(results_synth_adh_neg_binom_random_assignment,
                                                                results_synth_adh_subset_neg_binom_random_assignment,
                                              results_synth_difp_neg_binom_random_assignment,
                                              results_synth_psc_neg_binom_random_assignment,
                                              results_synth_did_neg_binom_random_assignment,
                                              results_synth_1NN_matching_neg_binom_random_assignment), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_neg_binom_treated_units_random_assignment) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  tar_target(data_tau_hat_neg_binom_demeaned_random_assignment, map(list(results_synth_adh_neg_binom_demeaned_random_assignment,
                                                                         results_synth_adh_subset_neg_binom_demeaned_random_assignment,
                                                       results_synth_difp_neg_binom_demeaned_random_assignment,
                                                       results_synth_psc_neg_binom_demeaned_random_assignment,
                                                       results_synth_did_neg_binom_demeaned_random_assignment,
                                                       results_synth_1NN_matching_neg_binom_demeaned_random_assignment), 
                                                  ~extract_tau_hat_synth_results(.,
                                                                                 treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_neg_binom_treated_units_random_assignment) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  tar_target(data_tau_hat_factor_random_assignment, map(list(results_synth_adh_factor_random_assignment,
                                                             results_synth_adh_subset_factor_random_assignment,
                                           results_synth_difp_factor_random_assignment,
                                           results_synth_psc_factor_random_assignment,
                                           results_synth_did_factor_random_assignment,
                                           results_synth_1NN_matching_factor_random_assignment), 
                                      ~extract_tau_hat_synth_results(.,
                                                                     treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_factor_treated_units_random_assignment) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  tar_target(data_tau_hat_factor_demeaned_random_assignment, map(list(results_synth_adh_factor_demeaned_random_assignment,
                                                                      results_synth_adh_subset_factor_demeaned_random_assignment,
                                                    results_synth_difp_factor_demeaned_random_assignment,
                                                    results_synth_psc_factor_demeaned_random_assignment,
                                                    results_synth_did_factor_demeaned_random_assignment,
                                                    results_synth_1NN_matching_factor_demeaned_random_assignment),
                                               ~extract_tau_hat_synth_results(.,
                                                                              treatment_effect_type = "placebo")) %>%
               bind_rows() %>%
               mutate(Y0_treated_exp_val = outcome_exp_val_factor_treated_units_random_assignment) %>%
               mutate(diff_pred_systematic = Y0_treated_hat - Y0_treated_exp_val)),
  
  ##############################################################################################################################
  ### PLOTS FROM SIMULATION STUDY ##############################################################################################
  ##############################################################################################################################
  
  # Make output plots - simulation study ------------------------------------------------------------------------
  
  tar_target(patchwork_plot_line_real_vs_sim_data, (make_patchwork_plot(list = list(make_line_plot_real_vs_sim_data(list_data = list_data_simulated_full, 
                                                                                                                   location_var = column_label, 
                                                                                                                   location_string = "aich.jap7220", 
                                                                                                                   time_var = week_id, 
                                                                                                                   min_time_var = 900, 
                                                                                                                   outcome_var = all, 
                                                                                                                   outcome_sim_var = Y0_treated_neg_binom) +
                                                                                      ggtitle("A: Aichi, Negative Binomial") +
                                                                                      labs(x = "",
                                                                                           y = "All-cause\nmortality"),
                                                                                   make_line_plot_real_vs_sim_data(list_data = list_data_simulated_full, 
                                                                                                                   location_var = column_label, 
                                                                                                                   location_string = "aich.jap7220", 
                                                                                                                   time_var = week_id, 
                                                                                                                   min_time_var = 900, 
                                                                                                                   outcome_var = all, 
                                                                                                                   outcome_sim_var = Y0_treated_factor) + 
                                                                                     ggtitle("B: Aichi, Factor") +
                                                                                     labs(x = "",
                                                                                          y = ""),
                                                                                   make_line_plot_real_vs_sim_data(list_data = list_data_simulated_full, 
                                                                                                                   location_var = column_label, 
                                                                                                                   location_string = "srwk.mal0019", 
                                                                                                                   time_var = week_id, 
                                                                                                                   min_time_var = 900, 
                                                                                                                   outcome_var = all, 
                                                                                                                   outcome_sim_var = Y0_treated_neg_binom) +
                                                                                     ggtitle("C: Sarawak, Negative Binomial") +
                                                                                     labs(x = "Week",
                                                                                          y = ""),
                                                                                   make_line_plot_real_vs_sim_data(list_data = list_data_simulated_full, 
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
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor[data_tau_hat_factor$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
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
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor_demeaned[data_tau_hat_factor_demeaned$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
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
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor_random_assignment[data_tau_hat_factor_random_assignment$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
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
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
        xlim(-1,1),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor_demeaned_random_assignment[data_tau_hat_factor_demeaned_random_assignment$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = expression(hat(tau)),
             y = "Density",
             colour = "Method") +
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
                                                labs(x = "Mean weekly mortality",
                                                     y = "Mean percentage bias (%)",
                                                     colour = "Method")+
                                                theme(legend.position = "none")) %>%
               
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
                                                          colour = "Method") + 
                                                     theme(legend.position = "none"))
               %>%
               
               ggsave("Output/Figures/Simulation/scatter_plot_abs_bias_mean_y_factor.png", ., width = 8, height = 5),
             format = "file"),
  
  # Plot of estimated coefficients on fire PM2.5 from negative binomial model estimated on outcomes generated from a factor model
  tar_target(scatter_plot_coef_pred_fire_pm25_factor_outcome_neg_binom_model, (data_coef_pred_fire_PM25_factor_outcome_neg_binom_model %>% 
               
               ggplot() + 
               geom_point(aes(x = reorder(term, estimate), 
                              y = estimate)) + 
               geom_hline(yintercept = 0, linetype = "dashed") + 
               geom_errorbar(aes(x = reorder(term, estimate), 
                                 y = estimate, 
                                 ymin = `lower.2.5 %`, 
                                 ymax = `upper.97.5 %`)) + 
               coord_flip() + 
               scatter_plot_opts + 
                labs(x = "", y = expression("Coefficient on LFS PM"[2.5]*" exposure"))
             ) %>% 
               ggsave("Output/Figures/Simulation/scatter_plot_coef_pred_fire_pm25_factor_outcome_neg_binom_model.png", ., width = 8, height = 8)
             ),
  
  # Plot ratio of pre- / post-treatment RMSPE
  tar_target(scatter_plot_ratio_pre_post_abs_bias,
             
             (data_tau_hat_neg_binom %>%
                    
                    # Estimate RMSPE by method and model run
                    summarise(rmspe = sqrt(mean((tau_hat - tau)^2)),
                              .by = c(method,
                                      model_run, post)) %>%
               
               pivot_wider(id_cols = c(method, model_run), 
                           names_from = post, 
                           names_prefix = "post_", 
                           values_from = rmspe) %>% 
              
               ggplot() + 
               geom_point(aes(x = post_0, 
                              y = post_1,
                              colour = method),
                          alpha = 0.25) + 
               geom_abline(linetype = "dashed",
                           alpha = 0.5) + 
               facet_wrap(~method) + 
               xlim(0,150) + 
               ylim(0,150) +
               scatter_plot_opts +
               scale_colour_manual(values = cbbPalette) +
               labs(x = "RMSPE (pre-treatment)",
                    y = "RMSPE\n(post-treatment)",
                    colour = "") +
               theme(legend.position = "none")) %>%
               ggsave("Output/Figures/Simulation/scatter_plot_ratio_pre_post_abs_bias.png", ., width = 8, height = 5, dpi = 700)
    
  ),
  
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
                 values_from = c(abs_bias_att,
                                 sd_att,
                                 avg_abs_bias_per_period,
                                 avg_sd_per_period),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   abs_bias_att_negative_binomial,
                   sd_att_negative_binomial,
                   avg_abs_bias_per_period_negative_binomial,
                   avg_sd_per_period_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   abs_bias_att_factor,
                   sd_att_factor,
                   avg_abs_bias_per_period_factor,
                   avg_sd_per_period_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 abs_bias_att_negative_binomial = "Abs bias ATT",
                 sd_att_negative_binomial = "sd ATT",
                 avg_abs_bias_per_period_negative_binomial = "Avg abs bias per period",
                 avg_sd_per_period_negative_binomial = "Avg sd per period",
                 abs_bias_att_factor = "Abs bias ATT",
                 sd_att_factor = "sd ATT",
                 avg_abs_bias_per_period_factor = "Avg abs bias per period",
                 avg_sd_per_period_factor = "Avg sd per period"
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
                 values_from = c(abs_bias_att,
                                 sd_att,
                                 avg_abs_bias_per_period,
                                 avg_sd_per_period),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   abs_bias_att_negative_binomial,
                   sd_att_negative_binomial,
                   avg_abs_bias_per_period_negative_binomial,
                   avg_sd_per_period_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   abs_bias_att_factor,
                   sd_att_factor,
                   avg_abs_bias_per_period_factor,
                   avg_sd_per_period_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 abs_bias_att_negative_binomial = "Abs bias ATT",
                 sd_att_negative_binomial = "sd ATT",
                 avg_abs_bias_per_period_negative_binomial = "Avg abs bias per period",
                 avg_sd_per_period_negative_binomial = "Avg sd per period",
                 abs_bias_att_factor = "Abs bias ATT",
                 sd_att_factor = "sd ATT",
                 avg_abs_bias_per_period_factor = "Avg abs bias per period",
                 avg_sd_per_period_factor = "Avg sd per period"
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
                 values_from = c(abs_bias_att,
                                 sd_att,
                                 avg_abs_bias_per_period,
                                 avg_sd_per_period),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   abs_bias_att_negative_binomial,
                   sd_att_negative_binomial,
                   avg_abs_bias_per_period_negative_binomial,
                   avg_sd_per_period_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   abs_bias_att_factor,
                   sd_att_factor,
                   avg_abs_bias_per_period_factor,
                   avg_sd_per_period_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 abs_bias_att_negative_binomial = "Abs bias ATT",
                 sd_att_negative_binomial = "sd ATT",
                 avg_abs_bias_per_period_negative_binomial = "Avg abs bias per period",
                 avg_sd_per_period_negative_binomial = "Avg sd per period",
                 abs_bias_att_factor = "Abs bias ATT",
                 sd_att_factor = "sd ATT",
                 avg_abs_bias_per_period_factor = "Avg abs bias per period",
                 avg_sd_per_period_factor = "Avg sd per period"
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
                 values_from = c(abs_bias_att,
                                 sd_att,
                                 avg_abs_bias_per_period,
                                 avg_sd_per_period),
                 names_glue = "{.value}_{dgp_type}"
               ) %>%
               
               # Set as gt object and format
               gt() %>%
               
               tab_spanner(
                 label = "Negative Binomial",
                 columns = c(
                   abs_bias_att_negative_binomial,
                   sd_att_negative_binomial,
                   avg_abs_bias_per_period_negative_binomial,
                   avg_sd_per_period_negative_binomial
                 )
               ) %>%
               
               tab_spanner(
                 label = "Factor",
                 columns = c(
                   abs_bias_att_factor,
                   sd_att_factor,
                   avg_abs_bias_per_period_factor,
                   avg_sd_per_period_factor
                 )
               ) %>%
               
               cols_label(
                 method = "Method",
                 abs_bias_att_negative_binomial = "Abs bias ATT",
                 sd_att_negative_binomial = "sd ATT",
                 avg_abs_bias_per_period_negative_binomial = "Avg abs bias per period",
                 avg_sd_per_period_negative_binomial = "Avg sd per period",
                 abs_bias_att_factor = "Abs bias ATT",
                 sd_att_factor = "sd ATT",
                 avg_abs_bias_per_period_factor = "Avg abs bias per period",
                 avg_sd_per_period_factor = "Avg sd per period"
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
  
  tar_target(
    plot_pm25_case_study, (data_for_case_study %>% 
      
      ggplot() + 
      
      geom_line(aes(x = week_id, 
                    y = pred_fire_PM25, 
                    group = column_label), 
                alpha = 0.1) + 
      
      geom_line(data = filter(data_for_case_study, treated == 1), 
                aes(x = week_id, y = pred_fire_PM25)) + 
      
      geom_vline(xintercept = min(data_for_case_study$week_id[data_for_case_study$post==1]), 
                 linetype = "dashed") + 
      
      annotate("rect",
               xmin = min(data_for_case_study$week_id[data_for_case_study$post==1]),
               xmax = Inf,
               ymin = -Inf,
               ymax = Inf,
               alpha = 0.1) +
      
      annotate("text",
               x = 802,
               y = 60,
               label = "Treatment start") +
      
      scatter_plot_opts + 
      
      labs(x = "Week",
           y = bquote(atop("Predicted LFS", PM[2.5] / m^3)))) %>% 
      
      ggsave("Output/Figures/Main/plot_pm25_case_study.png", ., width = 8, height = 5)
  ),
  
  ### Select data subset for analysis
  tar_target(data_for_case_study, list_data_for_synth[["567"]] %>% 
               
               # Filter out locations with mean weekly deaths < 100
               mutate(mean_mortality = mean(all, na.rm = TRUE), .by = column_label) %>%
               
               filter(mean_mortality > 100) %>%
               
               select(-mean_mortality)
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
  
  tar_target(results_case_study_adh_subset, optimise_synth(data = data_for_case_study,
                                                    demean_outcomes = TRUE,
                                                    denoise_outcomes = FALSE,
                                                    objective_function = "ADH subset",
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
  
  tar_target(results_case_study_1NN_matching, optimise_synth(data = data_for_case_study,
                                                    demean_outcomes = TRUE,
                                                    denoise_outcomes = FALSE,
                                                    objective_function = "1NN matching",
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
                                                        results_case_study_adh_subset$Y0_treated_hat,
                                                        results_case_study_psc$Y0_treated_hat,
                                                        results_case_study_difp$Y0_treated_hat,
                                                        results_case_study_did$Y0_treated_hat,
                                                        results_case_study_1NN_matching$Y0_treated_hat),
                                             method = c(rep("True", length(data_for_case_study$post[data_for_case_study$treated == 1])), 
                                                        rep("ADH", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("ADH subset", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("PSC", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("DIFP", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("DID", length(data_for_case_study$post[data_for_case_study$treated == 1])),
                                                        rep("1NN matching", length(data_for_case_study$post[data_for_case_study$treated == 1]))),
                                             t = rep(1:length(data_for_case_study$post[data_for_case_study$treated == 1]), 7)) %>%
               
               # Generate new column with difference in outcome vs. true trajectory
               mutate(outcome_diff = outcome[method == "True"] - outcome,
                      .by = t)
             ),
  
  # Get confidence intervals using conformal inference ---------------------------------------------------------
  tar_target(df_confidence_intervals_case_study_adh,
             
             get_df_conf_int_period_specific(data = data_for_case_study, 
                                             method = "ADH", 
                                             outcome_var = "all", 
                                             treated_var = "treated", 
                                             post_var = "post", 
                                             time_var = "week_id", 
                                             id_var = "column_label", 
                                             null_grid = seq(-200,200, length.out = 100), 
                                             q = 1, 
                                             alpha = 0.05)
             ),
  
  tar_target(df_confidence_intervals_case_study_adh_subset,
             
             get_df_conf_int_period_specific(data = data_for_case_study, 
                                             method = "ADH subset", 
                                             outcome_var = "all", 
                                             treated_var = "treated", 
                                             post_var = "post", 
                                             time_var = "week_id", 
                                             id_var = "column_label", 
                                             null_grid = seq(-200,200, length.out = 100), 
                                             q = 1, 
                                             alpha = 0.05)
  ),
  
  tar_target(df_confidence_intervals_case_study_did,
             
             get_df_conf_int_period_specific(data = data_for_case_study, 
                                             method = "DID", 
                                             outcome_var = "all", 
                                             treated_var = "treated", 
                                             post_var = "post", 
                                             time_var = "week_id", 
                                             id_var = "column_label", 
                                             null_grid = seq(-200,200, length.out = 100), 
                                             q = 1, 
                                             alpha = 0.05)
  ),
  
  tar_target(df_confidence_intervals_case_study_1NN_matching,
             
             get_df_conf_int_period_specific(data = data_for_case_study, 
                                             method = "1NN matching", 
                                             outcome_var = "all", 
                                             treated_var = "treated", 
                                             post_var = "post", 
                                             time_var = "week_id", 
                                             id_var = "column_label", 
                                             null_grid = seq(-200,200, length.out = 100), 
                                             q = 1, 
                                             alpha = 0.05)
  ),
  
  tar_target(df_confidence_intervals_case_study_difp,
             
             get_df_conf_int_period_specific(data = data_for_case_study, 
                                             method = "DIFP", 
                                             outcome_var = "all", 
                                             treated_var = "treated", 
                                             post_var = "post", 
                                             time_var = "week_id", 
                                             id_var = "column_label", 
                                             null_grid = seq(-200,200, length.out = 100), 
                                             q = 1, 
                                             alpha = 0.05)
  ),
  
  tar_target(df_confidence_intervals_case_study_psc,
             
             get_df_conf_int_period_specific(data = data_for_case_study, 
                                             method = "PSC", 
                                             outcome_var = "all", 
                                             treated_var = "treated", 
                                             post_var = "post", 
                                             time_var = "week_id", 
                                             id_var = "column_label", 
                                             null_grid = seq(-200,200, length.out = 100), 
                                             q = 1, 
                                             alpha = 0.05)
  ),
  
  # Join confidence intervals to main case study data
  tar_target(
    data_results_case_study_conf_int, data_results_case_study %>%
      
      left_join(bind_rows(df_confidence_intervals_case_study_1NN_matching,
                          df_confidence_intervals_case_study_did,
                          df_confidence_intervals_case_study_adh,
                          df_confidence_intervals_case_study_adh_subset,
                          df_confidence_intervals_case_study_difp,
                          df_confidence_intervals_case_study_psc), by = c("method", "t")) %>%
      
      # If conf int is NA, either because it is pre-treatment or because it is the True trajectory - in either case, set conf int to zero
      mutate(conf_int_upper = if_else(is.na(conf_int_upper), 0, conf_int_upper),
             conf_int_lower = if_else(is.na(conf_int_lower), 0, conf_int_lower))
  ),
  
  # Plot outcome trajectories relative to truth
  tar_target(plot_results_case_study_diff, (data_results_case_study_conf_int %>%
               
               filter(method != "True") %>%
               
               ggplot() +
               
               geom_point(aes(x = t,
                             y = outcome_diff,
                             colour = method)) +
               
               geom_linerange(aes(x = t,
                                  ymin = conf_int_lower,
                                  ymax = conf_int_upper,
                                  colour = method)) + 
               
               geom_vline(xintercept = length(data_for_case_study$post[data_for_case_study$treated == 1 & data_for_case_study$post == 0]),
                          linetype = "dashed") +
               
               geom_hline(yintercept = 0, linetype = "dashed") +
               
               ylim(-200,200) +
               
               labs(x = "Week",
                    y = expression(hat(tau)),
                    colour = "Method") +
               
               scatter_plot_opts +
               
               scale_colour_manual(values = cbbPalette) + 
               
               facet_wrap(~method, nrow = 3) +
               
               theme(legend.position = "none")
             
             ) %>% 
               
               ggsave("Output/Figures/Main/plot_results_case_study_diff.png", ., height = 5, width = 8, dpi = 700)
  )
)
