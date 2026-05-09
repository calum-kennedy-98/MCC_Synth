#' Generate a List of Structural Causal Model Datasets
#'
#' @description
#' Calls \code{\link{sim_data_scm}} \code{n_sims} times with incrementing
#' seeds to produce a list of independently simulated datasets, each
#' conforming to the assumed structural causal model (SCM). Within each
#' simulation, data are immediately aggregated from the daily to weekly level.
#'
#' @param n_sims Integer. Number of independent simulation replicates to
#'   generate.
#' @param data A data frame of real MCC observations used as the structural
#'   template (covariates, population sizes, etc.) for the SCM.
#' @param id_var Bare (unquoted) name of the unit identifier column (tidy-eval).
#' @param fire_pm_var Bare (unquoted) name of the fire PM2.5 variable used in
#'   the SCM equations (tidy-eval).
#' @param week_var Bare (unquoted) name of the week identifier used for
#'   weekly aggregation (tidy-eval).
#' @param time_var_mcc Bare (unquoted) name of the daily time identifier
#'   passed to \code{\link{sim_data_scm}} (tidy-eval).
#' @param seed Integer. Base random seed; each replicate uses seed \eqn{i}
#'   for \eqn{i = 1, \ldots, n\_sims}.
#'
#' @return A list of \code{n_sims} data frames, each aggregated to the
#'   city-week level.

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