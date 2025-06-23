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
                                              unit_id_var,
                                              time_id_var,
                                              week_id_var,
                                              n_periods_pre,
                                              n_periods_post) {
  
  # Extract unit treatment probability
  data_treatment_prob_unit <- data %>%
    mutate(
      change_treatment = {{treated_var}} - lag({{treated_var}}), .by = {{unit_id_var}}
    ) %>%
    summarise(
      n_events = sum(change_treatment == 1, na.rm = TRUE), .by = {{unit_id_var}}
    ) %>%
    mutate(
      treatment_prob = n_events / sum(n_events)
      ) %>%
    rename(
      unit_name = {{unit_id_var}}
    )
  
  unit_names <- data_treatment_prob_unit$unit_name
  treatment_prob_unit <- data_treatment_prob_unit$treatment_prob
  
  # Extract treatment time probability (filter weeks at beginning/end of sample to ensure
  # sufficient pre and post-periods for SC methods to work)
  data_treatment_prob_time <- data %>%
    filter(
      between({{week_id_var}}, min({{week_id_var}} + n_periods_pre), max({{week_id_var}} - n_periods_post + 1))
    ) %>%
    summarise(
      n_events = sum({{treated_var}} == 1, na.rm = TRUE), .by = {{time_id_var}}
    ) %>%
    mutate(
      treatment_prob = n_events / sum(n_events)
      ) %>%
    rename(
      time_id_var = {{time_id_var}}
    )
  
  time_ids <- data_treatment_prob_time$time_id_var
  treatment_prob_time <- data_treatment_prob_time$treatment_prob
  
  # Return assigned unit and treatment status
  return(list("unit_names" = unit_names,
              "time_ids" = time_ids,
              "treatment_prob_unit" = treatment_prob_unit,
              "treatment_prob_time" = treatment_prob_time))
  
}