# Name of script: estimate_assignment_probabilities
# Description: Function which estimates time and unit assignment probabilities
# for acute LFS fire exposure using the MCC data. To estimate the assignment
# probabilities we use the empirical frequency of acute events across units and time
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 30-04-2025
# Latest update by: Calum Kennedy
# Latest update on: 30-04-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

estimate_assignment_probabilities <- function(data,
                                              treated_var,
                                              id_var,
                                              week_var) {
  
  # Extract unit treatment probability
  data_treatment_prob_unit <- data %>%
    mutate(
      change_treatment = {{treated_var}} - lag({{treated_var}}), .by = {{id_var}}
    ) %>%
    summarise(
      n_events = sum(change_treatment == 1, na.rm = TRUE), .by = {{id_var}}
    ) %>%
    mutate(
      treatment_prob = n_events / sum(n_events)
      ) %>%
    rename(
      unit_name = {{id_var}}
    )
  
  unit_names <- data_treatment_prob_unit$unit_name
  treatment_prob_unit <- data_treatment_prob_unit$treatment_prob
  
  # Extract week treatment probability
  data_treatment_prob_time <- data %>%
    filter(
      between({{week_var}}, min({{week_var}} + 26), max({{week_var}} - 25))
    ) %>%
    summarise(
      n_events = sum({{treated_var}} == 1, na.rm = TRUE), .by = {{week_var}}
    ) %>%
    mutate(
      treatment_prob = n_events / sum(n_events)
      ) %>%
    rename(
      week_id = {{week_var}}
    )
  
  week_ids <- data_treatment_prob_time$week_id
  treatment_prob_time <- data_treatment_prob_time$treatment_prob
  
  # Return assigned unit and treatment status
  return(list("unit_names" = unit_names,
              "week_ids" = week_ids,
              "treatment_prob_unit" = treatment_prob_unit,
              "treatment_prob_time" = treatment_prob_time))
  
}