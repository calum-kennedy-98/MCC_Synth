# Name of script: functions
# Description:  Sources all necessary functions to build analysis pipeline
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Source functions -------------------------------------------------------------

source(here("R/load_env.R"))
source(here("R/get_data_mcc.R"))
source(here("R/subset_data.R"))
source(here("R/utility_functions.R"))
source(here("R/simulation/sim_data_scm.R"))
source(here("R/simulation/sim_synth_model.R"))
source(here("R/run_synth_model.R"))
source(here("R/simulation/extract_tau_hat_synth_results.R"))
source(here("R/simulation/make_data_scm_list.R"))