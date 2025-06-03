# Name of script: make_list_data_simulated
# Description: Wrapper function to generate simulated data under several data
# generating processes. Generates lists of predicted outcomes under different DGPs,
# then makes list of simulated datasets comprised of true data merged with
# simulated data
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 03-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 03-06-2025

# Comments ---------------------------------------------------------------------

# @ param ...

# Function ---------------------------------------------------------------------

make_list_data_simulated <- function(data,
                                     unit_id_var,
                                     time_id_var,
                                     week_id_var,
                                     treated_var,
                                     list_outcome_sim_neg_binomial,
                                     list_outcome_sim_factor){
  
  # Extract unit and time treatment probabilities
  unit_time_treatment_probs <- estimate_assignment_probabilities(data = data,
                                                                 treated_var = {{treated_var}},
                                                                 unit_id_var = {{unit_id_var}},
                                                                 time_id_var = {{time_id_var}},
                                                                 week_id_var = {{week_id_var}})
  
  # Generate list of simulated datasets alongside true data
  list_data_with_sims <- map2(
    list_outcome_sim_neg_binomial,
    list_outcome_sim_factor,
    ~ data %>% mutate(outcome_pred_neg_binom = .x,
                      outcome_pred_factor = .y)
  )
  
  # For each data frame in list, sample treated unit and time using empirical treatment probabilities
  # and subset data around the treatment time
  list_data_out <- lapply(list_data_with_sims, function(x) {

    # Sample treated unit and treated time
    treated_unit <- sample(unit_time_treatment_probs[["unit_names"]], 1, prob = unit_time_treatment_probs[["treatment_prob_unit"]])
    treated_time <- sample(unit_time_treatment_probs[["time_ids"]], 1, prob = unit_time_treatment_probs[["treatment_prob_time"]])

    # Generate treat/post indicator for treated unit
    data_with_treatment <- x %>%
      mutate(
        treated = ifelse({{unit_id_var}} == treated_unit, 1, 0),
        post = ifelse({{time_id_var}} >= treated_time, 1, 0)
      )
  }
  )
  
  return(list_data_out)
  
}