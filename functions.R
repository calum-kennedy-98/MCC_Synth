# Name of script: functions
# Description:  Sources all necessary functions to build analysis pipeline
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Source functions -------------------------------------------------------------

# Main functions 

source(here("R/load_env.R"))
source(here("R/get_data_mcc.R"))
source(here("R/generate_treatment_indicator.R"))
source(here("R/make_data_mcc_lfs_weekly.R"))
source(here("R/subset_data.R"))
source(here("R/utility_functions.R"))
source(here("R/ggplot_functions.R"))
source(here("R/get_hyperparam_loss_elastic_net.R"))
source(here("R/predict_Y_post_elastic_net.R"))
source(here("R/merge_data_mcc_lfs.R"))
source(here("R/optimise_synth_elastic_net.R"))
source(here("R/optimise_synth_adh.R"))
source(here("R/optimise_synth_demeaned.R"))
source(here("R/estimate_neg_binomial_model.R"))
source(here("R/estimate_factor_model.R"))
source(here("R/get_hyperparam_loss_penalised_sc.R"))
source(here("R/get_penalised_sc_weights.R"))
source(here("R/optimise_synth_penalised_sc.R"))
source(here("R/optimise_synth_denoised_outcome_natural_splines.R"))
source(here("R/make_scatter_plot_tau_hat_time.R"))

# Simulation functions

source(here("R/simulation/sim_data_scm.R"))
source(here("R/simulation/make_list_data_simulated.R"))
source(here("R/simulation/get_synth_diagnostics.R"))
source(here("R/simulation/make_data_scm_list.R"))
source(here("R/simulation/extract_tau_hat_synth_results.R"))
source(here("R/simulation/get_rmse_synth.R"))
source(here("R/simulation/sim_data_negative_binomial_model.R"))
source(here("R/simulation/make_list_sim_data_negative_binomial_model.R"))
source(here("R/simulation/make_list_sim_data_factor_model.R"))
source(here("R/simulation/estimate_assignment_probabilities.R"))
source(here("R/simulation/make_density_plot_synth_results.R"))
source(here("R/simulation/make_summary_table_synth_diagnostics.R"))