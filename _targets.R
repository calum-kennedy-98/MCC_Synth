# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
library(tidylog)
library(here)
library(ggplot2)
library(viridis)
library(crew)

# Set target options:
tar_option_set(
  packages = c(
    "here",
    "janitor",
    "dplyr",
    "tibble",
    "lubridate",
    "mice",
    "purrr",
    "optimx",
    "Synth",
    "glmnet"
  ),
  format = "qs",
  memory = "transient",
  garbage_collection = TRUE
  # controller = crew::crew_controller_local()
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
  tar_target(path_data_mcc_raw, here("data/main/MCC/Main/MCCdata_20250307.RData"),
             format = "file"),
  
  tar_target(path_data_lfs_raw, here("data/main/LFS_pollution/MCC_PM_O3_df.rds")),
  
  tar_target(data_mcc_merged, get_data_mcc(path_data_mcc = path_data_mcc_raw,
                                           id_var = column_label)),
  
  tar_target(data_mcc_lfs, merge_data_mcc_lfs(data_mcc_merged,
                                              path_data_lfs_raw)),
  
  # Simulation exercise
  tar_target(data_mcc_scm, subset_data(data = data_mcc_merged, 
                                           region == "Eastern Asia",
                                           year == 2019,
                                           vars_to_select = c("column_label",
                                                              "id",
                                                              "doy",
                                                              "tmean",
                                                              "week"))),
  
  tar_target(data_mcc_scm_imp, impute_missing_data(data = data_mcc_scm,
                                                   maxit = 5,
                                                   m = 2,
                                                   seed = 42)),
  
  tar_target(list_data_simulated, make_data_scm_list(n_sims = 100,
                                                     data = data_mcc_scm_imp, 
                                                     id_var = id, 
                                                     week_var = week, # May need to rethink this if go with moving averages 
                                                     time_var_mcc = doy, 
                                                     exposure_start_time = 210, 
                                                     exposure_end_time = 270, 
                                                     exposure_amplitude = 0, 
                                                     exposure_gamma = 10)),
  
  tar_target(results_synth_model_simulated, sim_synth_model(list_data_simulated = list_data_simulated, 
                                                            id_var = id, 
                                                            predictors = NULL,
                                                            special_predictors = list(list("y", 1:29, "mean")), 
                                                            time_predictors_prior = 1:29, 
                                                            dep_var = "y", 
                                                            time_var_synth = "week", 
                                                            time_optimise_ssr = 20:29, 
                                                            time_plot = 1:50)),
  
  tar_target(results_tau_hat_synth, extract_tau_hat_synth_results(results_synth_model_simulated = results_synth_model_simulated,
                                                                  id_var = id,
                                                                  time_var = week)),
  
  tar_target(results_synth_diagnostics, get_synth_diagnostics(results_synth_model_simulated = results_synth_model_simulated,
                                                              id_var = id,
                                                              time_var = week,
                                                              start_index = 1,
                                                              end_index = 29)),
  
  # Here have defined separate target for elastic net method - ultimately we
  # probably want to combine these into some generic wrapper function (editing the 'sim_synth_model'
  # function call)
  tar_target(results_elastic_net_simulated, map(list_data_simulated, 
                                                ~ optimise_synth_elastic_net(.,
                                                                             alpha_init = 0.5,
                                                                             lambda_init = 2,
                                                                             outcome_var = y,
                                                                             treated_id_var = treated_unit,
                                                                             treated_time_var = treated_time,
                                                                             time_var = week)))
)