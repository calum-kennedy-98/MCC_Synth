# Name of script: simulate_synthetic_control_method
# Description: Function which simulates a specified synthetic control method
# on a given arbitrary dataset, and returns list of outputs for diagnostics
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 24-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 24-04-2025

# Comments ---------------------------------------------------------------------

# Current set up is to probabilistically assign treatment based on the empirical
# frequency of wildfire events, calculated from the real MCC-LFS data.
# The function will extract a random subset of 52 weeks (one year) from the
# dataset, assign treatment to one unit at T0 = 26 weeks (i.e. halfway through the
# covered period), and build an optimal synthetic control for the treated unit
# using the set of controls. Currently, we work under a placebo assumption - the
# synthetic data has been generated so that the wildfire exposure has no effect
# on the outcome

# This function flow could be improved - e.g. we don't need to re-estimate probabilities each time as they are static

# Function ---------------------------------------------------------------------

simulate_synthetic_control_method <- function(data,
                                              id_var,
                                              week_var,
                                              treated_var){
  
  # Extract unit treatment probability using empirical frequency in the real MCC data
  data_treatment_prob_unit <- data %>%
    mutate(
      change_treatment = {{treated_var}} - lag({{treated_var}}), .by = {{id_var}}
      ) %>%
    summarise(
      n_events = sum(change_treatment == 1, na.rm = TRUE), .by = {{id_var}}
      ) %>%
    mutate(treatment_prob = n_events / sum(n_events)) %>%
    rename(
      unit_name = {{id_var}}
    )
  
  unit_names <- data_treatment_prob_unit$unit_name
  treatment_prob_unit <- data_treatment_prob_unit$treatment_prob
  
  # Extract week treatment probability using empirical frequency in real MCC data
  data_treatment_prob_time <- data %>%
    filter(
      between({{week_var}}, min({{week_var}} + 26), max({{week_var}} - 25))
    ) %>%
    summarise(
      n_events = sum({{treated_var}} == 1, na.rm = TRUE), .by = {{week_var}}
    ) %>%
    mutate(treatment_prob = n_events / sum(n_events)) %>%
    rename(
      week_id = {{week_var}}
    )
  
  week_ids <- data_treatment_prob_time$week_id
  treatment_prob_time <- data_treatment_prob_time$treatment_prob
  
  # Sample treated unit and time using empirical treatment probabilities
  treated_unit <- sample(unit_names, 1, prob = treatment_prob_unit)
  treated_time <- sample(week_ids, 1, prob = treatment_prob_time)
  
  # Subset data based on sampled treatment time and generate treatment assignment
  # indicator for treated unit
  data_for_synth <- data %>%
    filter(
      between({{week_var}}, treated_time - 26, treated_time + 25)
    ) %>%
    mutate(treated = ifelse({{id_var}} == treated_unit, 1, 0),
           post = ifelse({{week_var}} >= treated_time, 1, 0))
  
  # Build optimal synthetic control unit using specified method - NEED TO ADD FLEXIBILITY TO DECIDE WHICH METHOD (CURRENTLY ELASTIC NET)
  synth_out <- optimise_synth_elastic_net(data_for_synth,
                                          alpha_init = 0.5,
                                          lambda_init = 2,
                                          outcome_var = outcome_pred,
                                          treated_id_var = treated,
                                          treated_time_var = post,
                                          time_var = {{week_var}}) # Add these into function args 
}