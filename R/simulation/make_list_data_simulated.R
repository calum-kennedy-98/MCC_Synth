# Name of script: make_list_data_simulated
# Description: Function to generate final dataset with outcomes simulated
# from the specified DGPs and treatment assignments sampled from their empirical
# distribution in the real data
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
                                     list_outcome_sim_factor,
                                     n_periods_pre,
                                     n_periods_post){
  
  # Extract unit and time treatment probabilities
  unit_time_treatment_probs <- estimate_assignment_probabilities(data = data,
                                                                 treated_var = {{treated_var}},
                                                                 unit_id_var = {{unit_id_var}},
                                                                 time_id_var = {{time_id_var}},
                                                                 week_id_var = {{week_id_var}},
                                                                 n_periods_pre = n_periods_pre,
                                                                 n_periods_post = n_periods_post)
  
  # Generate list of simulated datasets alongside true data
  list_data_with_sims <- map2(
    list_outcome_sim_neg_binomial,
    list_outcome_sim_factor,
    ~ data %>% mutate(Y0_treated_neg_binom = .x,
                      Y0_treated_factor = .y)
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
      ) %>%
      
      select({{time_id_var}}, {{unit_id_var}}, Y0_treated_neg_binom, Y0_treated_factor, post, treated) %>%
      
      filter(between({{time_id_var}}, treated_time - n_periods_pre, treated_time + n_periods_post - 1)) %>%
      
      mutate(across(where(is.character), as.factor))
  }
  )
  
  return(list_data_out)
  
}