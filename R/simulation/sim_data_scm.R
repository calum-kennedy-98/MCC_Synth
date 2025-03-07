# Name of script: make_data_scm
# Description: Generates simulated dataset conforming to the assumed Structural Causal Model
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Define function to generate simulated dataset of covariates ------------------

sim_data_scm <- function(data_mcc_scm,
                          id_var,
                          time_var,
                          seed,
                          exposure_start_time,
                          exposure_end_time,
                          exposure_amplitude,
                          exposure_gamma){
  
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
  
  # Generate shared pollution time trend from a sine wave
  pollution_time_trend <- sin(seq(1:n_periods)/n_periods*2*pi)*5
  
  # Simulate an acute air pollution exposure using a Cauchy-like distribution
  exposure <- generate_fat_tail(start_index = exposure_start_time,
                                end_index = exposure_end_time,
                                amplitude = exposure_amplitude,
                                gamma = exposure_gamma)
  
  list_data_unit_level <- map(id_vec, function(x){
    
    # Generate unobserved time-invariant factor a from negative exponential distribution
    a <- rexp(1)*100
    
    # Generate baseline pollution as function of random intercept + an AR(1) process
    pollution_intercept <- runif(1, 10, 20)
    
    e <- as.numeric(arima.sim(list(order = c(1,0,0), ar = .95), n = n_periods) + pollution_intercept + pollution_time_trend)
    
    # Set 'natural value' of pollution
    e_natural <- e
    
    # Get mean temperature by location to centre temperature effects
    temp <- data_mcc_scm %>% 
      filter({{id_var}} == x) %>% 
      select(tmean) %>% deframe()
    temp_mean <- mean(temp, na.rm = TRUE)
    
    # If unit is the first unit by id, add the acute exposure to the pollution level
    if(x == min(id_vec)) e[exposure_start_time:exposure_end_time] <- e[exposure_start_time:exposure_end_time] + exposure
    
    # Draw initial at-risk population from exponential distribution
    u1 <- 500 * rexp(1) + 5 * a
    
    # Precompute squared terms (vectorized)
    temp_squared_deviation <- (temp - temp_mean)^2
    e_squared <- e^2
    e_natural_squared <- e_natural^2
    growth_rate <- 0.2 + 0.0002 * temp_squared_deviation + 0.0002 * e_squared
    growth_rate_natural <- 0.2 + 0.0002 * temp_squared_deviation + 0.0002 * e_natural_squared
    
    # Vectorized u and y computations
    error_u <- rnorm(n_periods, mean = 0, sd = u1/500) # Standard deviation of error scales with u1
    error_y <- rnorm(n_periods, mean = 0, sd = u1/500)
    
    # Create vectors of length n_periods to store results
    u <- numeric(n_periods)
    y <- numeric(n_periods)
    u_natural <- numeric(n_periods)
    y_natural <- numeric(n_periods)
    
    # Assign starting values
    u[1] <- u1
    y[1] <- growth_rate[1] * u[1] + error_y[1]
    u_natural[1] <- u1
    y_natural[1] <- growth_rate[1] * u_natural[1] + error_y[1]
    
    # Loop over n_periods to generate data
    for(t in 2:n_periods){
      
      # Update observed u_t and y_t based on previous values
      u[t] <- u[t-1] - y[t-1] + growth_rate[t-1] * u[1] + error_u[t]
      y[t] <- growth_rate[t] * u[t] + error_y[t]
      u_natural[t] <- u_natural[t-1] - y_natural[t-1] + growth_rate_natural[t-1] * u_natural[1] + error_u[t]
      y_natural[t] <- growth_rate_natural[t] * u_natural[t] + error_y[t]
    }
    
    # Combine results into a data frame
    data_unit_level <- data.frame(y = y, 
                                  y_natural = y_natural,
                                  u = u, 
                                  u_natural = u_natural,
                                  e = e,
                                  e_natural = e_natural,
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