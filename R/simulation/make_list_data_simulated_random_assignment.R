# Name of script: make_list_data_simulated_random_assignment
# Description: Function to generate final datasets including outcomes from the
# specified DGP, under random treatment assignment
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 21-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 21-06-2025

# Comments ---------------------------------------------------------------------

# Function ---------------------------------------------------------------------

# @ param...

make_list_data_simulated_random_assignment <- function(data,
                                     unit_id_var,
                                     time_id_var,
                                     list_outcome_sim_neg_binomial,
                                     list_outcome_sim_factor,
                                     n_periods_pre,
                                     n_periods_post){
  
  # Extract unit and time vectors to assign to treatment (restrict time_ids to ensure sufficient pre/post treatment periods)
  unit_ids <- pull(distinct(data, {{unit_id_var}}))
  time_ids <- data %>% filter(between({{time_id_var}}, 
                                      min({{time_id_var}}) + n_periods_pre, 
                                      max({{time_id_var}}) - n_periods_post + 1)) %>%
    distinct({{time_id_var}}) %>%
    pull({{time_id_var}})
  
  
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
    
    # Sample treated unit and treated time randomly
    treated_unit <- sample(unit_ids, 1)
    treated_time <- sample(time_ids, 1)
    
    # Generate treat/post indicator for treated unit
    data_with_treatment <- x %>%
      mutate(
        treated = ifelse({{unit_id_var}} == treated_unit, 1, 0),
        post = ifelse({{time_id_var}} >= treated_time, 1, 0)
      ) %>%
      
      select({{time_id_var}}, Y0_treated_neg_binom, Y0_treated_factor, post, treated) %>%
      
      filter(between({{time_id_var}}, treated_time - n_periods_pre, treated_time + n_periods_post - 1))
  }
  )
  
  return(list_data_out)
  
}