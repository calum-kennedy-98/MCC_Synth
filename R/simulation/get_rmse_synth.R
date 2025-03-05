# Name of script: sim_synth_model
# Description: Runs synthetic control model on n simulated datasets and returns desired outputd
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 26-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 26-02-2025

# Comments ---------------------------------------------------------------------


# Wrapper function to 'sim_synth_model' which extracts RMSE of predicted
# vs. natural value of Y in treated unit (i.e. the missing counterfactual)

get_rmse_synth <- function(list_synth_output,
                           id_var,
                           start_index,
                           end_index){
  
  # Define function to extract y_natural from each dataset, generate the counterfactual
  # prediction y_0 and return the RMSE in the post-treatment period
  rmse_list <- map(list_synth_output, function(x){
    
    # Extract true vs. predicted counterfactual
    y_natural <- x[["data"]] %>% filter({{id_var}} == min({{id_var}})) %>% pull(y_natural)
    y_pred <- x[["data_prepared"]][["Y0plot"]] %*% x[["synth_out"]][["solution.w"]]
    
    # Extract desired indices
    y_natural <- y_natural[start_index:end_index]
    y_pred <- y_pred[start_index:end_index]
    
    # Get RMSE of prediction error
    rmse <- sqrt(mean((y_pred - y_natural)^2))
  })
  
  # Return mean of resulting list
  return(mean(unlist(rmse_list)))
}