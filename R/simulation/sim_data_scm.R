# Name of script: sim_data_scm
# Description: Generates simulated dataset conforming to the assumed Structural Causal Model
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Define function to generate simulated dataset of covariates ------------------

sim_data_scm <- function(data_mcc_scm,
                         id_var,
                         time_var,
                         fire_pm_var,
                         seed){
  
  # Set seed
  set.seed(seed)
  
  # Drop ids with missing time points (ie. n data points < max data points by group)
  data_mcc_scm <- data_mcc_scm %>% 
    group_by({{id_var}}) %>% 
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n == max(n))
  
  # Extract unique row IDs to pass to function below
  id_vec <- distinct(data_mcc_scm, {{id_var}}) %>% deframe()
  
  # Extract N units and time periods from MCC-derived data
  n_units <- nrow(distinct(data_mcc_scm, {{id_var}}))
  n_periods <- nrow(distinct(data_mcc_scm, {{time_var}}))
  
  list_data_unit_level <- map(id_vec, function(x){
    
    # Generate treated indicator
    treated_unit <- ifelse(x == min(id_vec), rep(1, n_periods), rep(0, n_periods))
    
    # Generate unobserved time-invariant factor a from negative exponential distribution
    a <- rexp(1)*100
    
    # Extract vector of fire PM2.5 levels for each location
    fire_pm_25 <- data_mcc_scm %>%
      filter({{id_var}} == x) %>%
      select({{fire_pm_var}}) %>%
      deframe()
    
    # Get mean temperature by location to centre temperature effects
    temp <- data_mcc_scm %>% 
      filter({{id_var}} == x) %>% 
      select(tmean) %>% 
      deframe()
    temp_mean <- mean(temp, na.rm = TRUE)
    
    # Draw initial at-risk population from exponential distribution
    u1 <- 500 * rexp(1) + 5 * a
    
    # Precompute squared terms (vectorized)
    temp_squared_deviation <- (temp - temp_mean)^2
    fire_pm_25_squared <- fire_pm_25^2
    #e_natural_squared <- e_natural^2
    growth_rate <- 0.2 + 0.0001 * temp_squared_deviation + 0.0001 * fire_pm_25_squared
    #growth_rate_natural <- 0.2 + 0.0001 * temp_squared_deviation + 0.0001 * e_natural_squared
    
    # Vectorized u and y computations
    error_u <- rnorm(n_periods, mean = 0, sd = u1/500) # Standard deviation of error scales with u1
    error_y <- rnorm(n_periods, mean = 0, sd = u1/500)
    
    # Create vectors of length n_periods to store results
    u <- numeric(n_periods)
    y <- numeric(n_periods)
    #u_natural <- numeric(n_periods)
    #y_natural <- numeric(n_periods)
    
    # Assign starting values
    u[1] <- u1
    y[1] <- growth_rate[1] * u[1] + error_y[1]
    #u_natural[1] <- u1
    #y_natural[1] <- growth_rate[1] * u_natural[1] + error_y[1]
    
    # Loop over n_periods to generate data
    for(t in 2:n_periods){
      
      # Update observed u_t and y_t based on previous values
      u[t] <- u[t-1] - y[t-1] + growth_rate[t-1] * u[1] + error_u[t]
      y[t] <- growth_rate[t] * u[t] + error_y[t]
      #u_natural[t] <- u_natural[t-1] - y_natural[t-1] + growth_rate_natural[t-1] * u_natural[1] + error_u[t]
      #y_natural[t] <- growth_rate_natural[t] * u_natural[t] + error_y[t]
    }
    
    # Combine results into a data frame
    data_unit_level <- data.frame(treated_unit = treated_unit,
                                  #treated_time = treated_time,
                                  y = y, 
                                  u = u, 
                                  fire_pm_25 = fire_pm_25,
                                  growth_rate = growth_rate,
                                  temp_squared_deviation = temp_squared_deviation)
    
    return(data_unit_level)
    
  })
  
  # Combine all unit data into one data frame
  data_outcome <- bind_rows(list_data_unit_level)
  
  # Make dataset
  data_scm <- cbind(data_mcc_scm, data_outcome)
  
  return(data_scm)
}
