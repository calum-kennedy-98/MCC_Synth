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
                      if_all(tmean, ~ !is.na(.)))),
  
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
                                                           list_outcome_sim_factor = list_outcome_sim_factor)),
  
  # Get optimal SC from simulations --------------------------------------------
  
  # Negative binomial model
  
  # Elastic Net
  tar_target(results_synth_elastic_net_neg_binom, future_map(list_data_simulated,
                                                             ~ optimise_synth_elastic_net(.,
                                                                                          alpha_init = 0.5,
                                                                                          lambda_init = 2,
                                                                                          outcome_var = Y0_treated_neg_binom,
                                                                                          time_var = week_id,
                                                                                          treated_id_var = treated,
                                                                                          treated_time_var = post,
                                                                                          n_periods_pre = 26,
                                                                                          n_periods_post = 26))),
  
  # ADH synth without covariates
  tar_target(results_synth_adh_no_covars_neg_binom, future_map(list_data_simulated,
                                                             ~ optimise_synth_adh(.,
                                                                                  id_var = column_label,
                                                                                  outcome_var = Y0_treated_neg_binom,
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
  
  # Results from penalised SC
  tar_target(results_synth_penalised_neg_binom, future_map(list_data_simulated,
                                                            ~ optimise_synth_penalised_sc(.,
                                                                                          lambda_init = 1,
                                                                                          lower_bound_lambda = 1e-6,
                                                                                          outcome_var = Y0_treated_neg_binom,
                                                                                          time_var = week_id,
                                                                                          treated_id_var = treated,
                                                                                          treated_time_var = post,
                                                                                          n_periods_pre = 26,
                                                                                          n_periods_post = 26))),
  
  # Results from penalised SC on de-noised outcome series
  tar_target(results_synth_penalised_denoised_neg_binom, future_map(list_data_simulated,
                                                                    ~ optimise_synth_denoised_outcome_natural_splines(.,
                                                                                                                      unit_id_var = column_label,
                                                                                                                      time_id_var = week_id,
                                                                                                                      outcome_var = Y0_treated_neg_binom,
                                                                                                                      spline_df = 140,
                                                                                                                      lambda_init = 1,
                                                                                                                      lower_bound_lambda = 1e-6,
                                                                                                                      treated_id_var = treated,
                                                                                                                      treated_time_var = post,
                                                                                                                      n_periods_pre = 26,
                                                                                                                      n_periods_post= 26))),
  
  # Results for factor model 
  
  # Elastic Net
  tar_target(results_synth_elastic_net_factor, future_map(list_data_simulated,
                                                             ~ optimise_synth_elastic_net(.,
                                                                                          alpha_init = 0.5,
                                                                                          lambda_init = 2,
                                                                                          outcome_var = Y0_treated_factor,
                                                                                          time_var = week_id,
                                                                                          treated_id_var = treated,
                                                                                          treated_time_var = post,
                                                                                          n_periods_pre = 26,
                                                                                          n_periods_post = 26))),
  
  # ADH synth without covariates
  tar_target(results_synth_adh_no_covars_factor, future_map(list_data_simulated,
                                                               ~ optimise_synth_adh(.,
                                                                                    id_var = column_label,
                                                                                    outcome_var = Y0_treated_factor,
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
  
  # Results from penalised SC
  tar_target(results_synth_penalised_factor, future_map(list_data_simulated,
                                                           ~ optimise_synth_penalised_sc(.,
                                                                                         lambda_init = 1,
                                                                                         lower_bound_lambda = 1e-6,
                                                                                         outcome_var = Y0_treated_factor,
                                                                                         time_var = week_id,
                                                                                         treated_id_var = treated,
                                                                                         treated_time_var = post,
                                                                                         n_periods_pre = 26,
                                                                                         n_periods_post = 26))),
  
  # Results from penalised SC on de-noised outcome series
  tar_target(results_synth_penalised_denoised_factor, future_map(list_data_simulated,
                                                                    ~ optimise_synth_denoised_outcome_natural_splines(.,
                                                                                                                      unit_id_var = column_label,
                                                                                                                      time_id_var = week_id,
                                                                                                                      outcome_var = Y0_treated_factor,
                                                                                                                      spline_df = 140,
                                                                                                                      lambda_init = 1,
                                                                                                                      lower_bound_lambda = 1e-6,
                                                                                                                      treated_id_var = treated,
                                                                                                                      treated_time_var = post,
                                                                                                                      n_periods_pre = 26,
                                                                                                                      n_periods_post= 26))),
  
  # Extract results across simulations and assign treatment effects ------------
  
  # Placebo study
  
  # Negative binomial model
  tar_target(data_tau_hat_neg_binom_placebo, map(list(results_synth_adh_no_covars_neg_binom,
                                              results_synth_penalised_neg_binom,
                                              results_synth_elastic_net_neg_binom,
                                              results_synth_penalised_denoised_neg_binom), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
  tar_target(data_tau_hat_factor_placebo, map(list(results_synth_adh_no_covars_factor,
                                              results_synth_penalised_factor,
                                              results_synth_elastic_net_factor,
                                              results_synth_penalised_denoised_factor), 
                                         ~extract_tau_hat_synth_results(.,
                                                                        treatment_effect_type = "placebo")) %>%
               bind_rows()),
  
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
                                                                       legend_position = "bottom",
                                                                       ncol = 2
                                                                       )) %>%
               
               ggsave("Output/Figures/Simulation/patchwork_plot_line_real_vs_sim_data.png", ., height = 5, width = 8, dpi = 700, create.dir = TRUE),
             format = "file"
             ),
  
  # Placebo study
  
  # Density plot of tau hat from negative binomial model and factor model - placebo effect
  tar_target(patchwork_density_tau_hat_placebo, (make_patchwork_plot(
    
    list = list(
      
      make_density_plot_synth_results(
      data = data_tau_hat_neg_binom_placebo[data_tau_hat_neg_binom_placebo$post == 1, ],
      density_var = tau_hat_normalised,
      method_var = method,
      model_run_var = model_run,
      palette = cbbPalette) +
        ggtitle("A: Negative Binomial") +
        labs(x = "Tau hat (normalised)",
             y = "Density"),
      
      make_density_plot_synth_results(
        data = data_tau_hat_factor_placebo[data_tau_hat_factor_placebo$post == 1, ],
        density_var = tau_hat_normalised,
        method_var = method,
        model_run_var = model_run,
        palette = cbbPalette) +
        ggtitle("B: Factor") +
        labs(x = "Tau hat (normalised)",
             y = "Density")
      ),
    ncol = 2,
    guides = "collect"
    )) %>%
      
      ggsave("Output/Figures/Simulation/patchwork_density_tau_hat_placebo.png", ., width = 8, height = 5, dpi = 700, create.dir = TRUE)
    ),
  
  # Density plot of mean normalised tau hat from negative binomial model - placebo effect
  tar_target(plot_density_mean_tau_hat_neg_binom_placebo, (data_tau_hat_neg_binom_placebo %>%
                                                          
                                                          # Keep tau hat from post-treatment period only
                                                          filter(post == 1) %>%
                                                          
                                                          # Generate mean tau hat by model run and method
                                                          summarise(tau_hat_normalised = mean(tau_hat_normalised),
                                                                    .by = c(method,
                                                                            model_run)) %>%
                                                          
                                                          make_density_plot_synth_results(density_var = tau_hat_normalised,
                                                                                          method_var = method,
                                                                                          model_run_var = model_run,
                                                                                          palette = cbbPalette) +
                                                            
                                                            labs(x = "Mean tau hat (normalised)",
                                                                 y = "Density") +
                                                            scale_colour_manual(labels = c("adh_no_covars" = "ADH",
                                                                                           "elastic_net" = "DIFP",
                                                                                           "penalised_sc" = "PSC",
                                                                                           "penalised_sc_denoised" = "PSC (denoised)"))
  ) %>%
    
    ggsave("Output/Figures/Simulation/plot_density_mean_tau_hat_neg_binom_placebo.png", ., dpi = 700, width = 8, height = 5, create.dir = TRUE),
  format = "file"),
  
  # Density plot of mean normalised tau hat from factor model - placebo effect
  tar_target(plot_density_mean_tau_hat_factor_placebo, (data_tau_hat_factor_placebo %>%
                                                     
                                                     # Keep tau hat from post-treatment period only
                                                     filter(post == 1) %>%
                                                     
                                                     # Generate mean tau hat by model run and method
                                                     summarise(tau_hat_normalised = mean(tau_hat_normalised),
                                                               .by = c(method,
                                                                       model_run)) %>%
                                                     
                                                     make_density_plot_synth_results(density_var = tau_hat_normalised,
                                                                                     method_var = method,
                                                                                     model_run_var = model_run,
                                                                                     palette = cbbPalette) +
                                                       
                                                       labs(x = "Mean tau hat (normalised)",
                                                            y = "Density") +
                                                       scale_colour_manual(labels = c("adh_no_covars" = "ADH",
                                                                                      "elastic_net" = "DIFP",
                                                                                      "penalised_sc" = "PSC",
                                                                                      "penalised_sc_denoised" = "PSC (denoised)"))
  ) %>%
    
    ggsave("Output/Figures/Simulation/plot_density_mean_tau_hat_factor_placebo.png", ., dpi = 700, width = 8, height = 5, create.dir = TRUE),
  format = "file"),
  
  # Scatter plots of estimated tau hat coefficients against time by method (averaging across model runs)
  tar_target(plot_scatter_tau_hat_time_by_method_neg_binom, (data_tau_hat_neg_binom_placebo %>%
                                                               
                                                               summarise(mean_tau_hat = mean(tau_hat, na.rm = TRUE), 
                                                                         se_tau_hat = sd(tau_hat, na.rm = TRUE) / sqrt(sum(!is.na(tau_hat))), 
                                                                         .by = c(t, method)) %>%
                                                               
                                                               # Make scatter plot with facet wrap by method
                                                               make_scatter_plot_tau_hat_time(tau_hat_var = mean_tau_hat, 
                                                                                              se_var = se_tau_hat, 
                                                                                              time_var = t, 
                                                                                              facet_var = method, 
                                                                                              palette = cbbPalette) +
                                                               
                                                               labs(x = "Time (centred)",
                                                                    y = "Tau hat (mean)")
                                                             ) %>%
               
               ggsave("Output/Figures/Simulation/plot_scatter_tau_hat_time_by_method_neg_binom.png", ., height = 5, width = 8, create.dir = TRUE),
             format = "file"
             ),
  
  tar_target(plot_scatter_tau_hat_time_by_method_factor, (data_tau_hat_factor_placebo %>%
                                                               
                                                               summarise(mean_tau_hat = mean(tau_hat, na.rm = TRUE), 
                                                                         se_tau_hat = sd(tau_hat, na.rm = TRUE) / sqrt(sum(!is.na(tau_hat))), 
                                                                         .by = c(t, method)) %>%
                                                               
                                                               # Make scatter plot with facet wrap by method
                                                               make_scatter_plot_tau_hat_time(tau_hat_var = mean_tau_hat, 
                                                                                              se_var = se_tau_hat, 
                                                                                              time_var = t, 
                                                                                              facet_var = method, 
                                                                                              palette = cbbPalette) +
                                                            
                                                            labs(x = "Time (centred)",
                                                                 y = "Tau hat (mean)")
  ) %>%
    
    ggsave("Output/Figures/Simulation/plot_scatter_tau_hat_time_by_method_factor.png", ., height = 5, width = 8, create.dir = TRUE),
  format = "file"
  ),
  
  # Scatter plots of relative bias in SC predicted Y0_hat
  tar_target(plot_abs_relative_bias_tau_hat_neg_binom, (data_tau_hat_neg_binom_placebo %>%
                                                          
                                                          mutate(rel_bias_Y0_hat = (Y0_treated_hat - Y0_treated)/Y0_treated) %>% 
                                                          
                                                          summarise(abs_rel_bias_Y0_hat = abs(mean(rel_bias_Y0_hat, na.rm = TRUE)), 
                                                                    mean_Y0_treated = mean(Y0_treated, na.rm = TRUE),
                                                                    .by = c(model_run, method)) %>%
                                                          
                                                          # Remove extreme values for ease of visualisation
                                                          filter(abs_rel_bias_Y0_hat <= 0.3) %>%
                                                          
                                                          ggplot() +
                                                          
                                                          geom_point(aes(x = mean_Y0_treated,
                                                                         y = abs_rel_bias_Y0_hat)) +
                                                          
                                                          facet_wrap(~method) +
                                                          
                                                          scatter_plot_opts
                                                        ) %>%
               
               ggsave("Output/Figures/Simulation/plot_abs_relative_bias_tau_hat_neg_binom.png", ., height = 5, width = 8, create.dir = TRUE),
             format = "file"
             ),
  
  tar_target(plot_abs_relative_bias_tau_hat_factor, (data_tau_hat_factor_placebo %>%
               
               mutate(rel_bias_Y0_hat = (Y0_treated_hat - Y0_treated)/Y0_treated) %>% 
               
               summarise(abs_rel_bias_Y0_hat = abs(mean(rel_bias_Y0_hat, na.rm = TRUE)), 
                         mean_Y0_treated = mean(Y0_treated, na.rm = TRUE),
                         .by = c(model_run, method)) %>%
                 
                 # Remove extreme values for ease of visualisation
                 filter(abs_rel_bias_Y0_hat <= 0.3) %>%
               
               ggplot() +
               
               geom_point(aes(x = mean_Y0_treated,
                              y = abs_rel_bias_Y0_hat)) +
               
               facet_wrap(~method) +
               
               scatter_plot_opts
  ) %>%
    
    ggsave("Output/Figures/Simulation/plot_abs_relative_bias_tau_hat_factor.png", ., height = 5, width = 8, create.dir = TRUE),
  format = "file"
  ),
  
  # Summary tables of synth diagnostics by treatment effect type and method -------------------------------------------------------------
  tar_target(tbl_summary_synth_diagnostics_placebo, 
             
             bind_rows(make_summary_table_synth_diagnostics(data_tau_hat_neg_binom_placebo, 
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "negative_binomial"), 
                       make_summary_table_synth_diagnostics(data_tau_hat_factor_placebo,
                                                            tau_hat, 
                                                            tau, 
                                                            post,
                                                            "factor")) %>%
               
               # Mutate names of methods
               mutate(method = case_when(
                 method == "adh_no_covars" ~ "ADH",
                 method == "elastic_net" ~ "DIFP",
                 method == "penalised_sc" ~ "PSC",
                 method == "penalised_sc_denoised" ~ "PSC (denoised)"
               )
               ) %>%
               
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
               
               gtsave("Output/Tables/Simulation/tbl_summary_synth_diagnostics_placebo.tex"),
             format = "file")

)