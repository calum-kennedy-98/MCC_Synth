# Name of script: make_data_scm_list
# Description: Runs 'sim_data_scm' script 'n_iters' times to generate list of dataframes
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Define function to generate simulated dataset of covariates ------------------

make_data_scm_list <- function(n_sims,
                               data,
                               id_var,
                               fire_pm_var,
                               week_var,
                               time_var_mcc,
                               seed = x){
  
  # Generate list to store output of iterations
  iters <- c(1:n_sims)
  
  # Simulate data from different seed, run synth model, and return output for each iteration
  list_data_simulated <- map(iters, function(x){
    
    # Sim data from the structural causal model
    data <- sim_data_scm(data,
                         {{id_var}},
                         {{time_var_mcc}},
                         {{fire_pm_var}},
                         seed = x)
    
    # Aggregate data to weekly level to pass to models (NOTE: we may want to consider moving averages instead)
    data_weekly <- data %>% summarise(y = mean(y, na.rm = TRUE),
                                      treated_unit = mean(treated_unit, na.rm = TRUE),
                                      #treated_time = ifelse(mean(treated_time, na.rm = TRUE) > 0, 1, 0), # Define week as 'treated' if ANY day in that week is treated
                                      #e = mean(e, na.rm = TRUE),
                                      fire_pm_25 = mean(fire_pm_25, na.rm = TRUE),
                                      u = mean(u, na.rm = TRUE),
                                      #y_natural = mean(y_natural, na.rm = TRUE),
                                      #e_natural = mean(e_natural, na.rm = TRUE),
                                      #u_natural = mean(u_natural, na.rm = TRUE),
                                      growth_rate = mean(growth_rate, na.rm = TRUE),
                                      temp_squared_deviation = mean(temp_squared_deviation, na.rm = TRUE),
                                      .by = c({{week_var}}, 
                                              {{id_var}}))
    
  })
  return(list_data_simulated)
}