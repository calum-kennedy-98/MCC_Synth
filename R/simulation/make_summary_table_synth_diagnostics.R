# Name of script: make_summary_table_synth_diagnostics
# Description: Function to produce summary table of diagnostics from simulation
# study using various synthetic control estimators and DGPs
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 11-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 11-06-2025

# Comments ---------------------------------------------------------------------

# At present, we focus on three performance metrics - individual RMSE, aggregate
# RMSE, and absolute bias

# Function ---------------------------------------------------------------------

# @ param ...

make_summary_table_synth_diagnostics <- function(data_synth_results,
                                                 tau_hat_var,
                                                 tau_var,
                                                 dgp_type){
  
  # Checks
  if(!dgp_type %in% c("negative_binomial", "factor")) stop("Please set dgp_type equal to 'negative_binomial' or 'factor'")
  
  # Generate summary statistics by method and DGP
  tbl_summary_stats <- data_synth_results %>%
    
    # Estimate error and squared error by method and model run
    summarise(error = mean({{tau_hat_var}} - {{tau_var}}),
              squared_error = mean(({{tau_hat_var}} - {{tau_var}})^2),
              .by = c(method,
                      model_run)) %>%
    
    # Generate summary stats
    summarise(indiv_rmse = sqrt(mean(squared_error, na.rm = TRUE)),
              agg_rmse = sqrt(mean(error^2, na.rm = TRUE)),
              abs_bias = abs(mean(error, na.rm = TRUE)),
              .by = method) %>%
    
    # Add column for DGP type
    mutate(dgp_type = dgp_type)
  
  return(tbl_summary_stats)
  
}