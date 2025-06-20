# Name: ggplot_functions
# Description: ggplot functions for main MCC project manuscript
# Author: CK
# Date: 03-03-2025

# Define functions -------------------------------------------------------------

make_line_plot_real_vs_sim_data <- function(list_data,
                                            location_var,
                                            location_string,
                                            time_var,
                                            min_time_var,
                                            outcome_var,
                                            outcome_sim_var){
  
  # Extract data for specific location from each data frame in list, then bind 
  # rows together and create a model_run variable
  data_to_plot <- lapply(list_data, function(x){filter(x, {{location_var}} == location_string)}) %>%
    
    bind_rows() %>%
    
    mutate(model_run = row_number(),
           .by = {{time_var}})
  
  # Get max of outcome var to plot
  y_max <- max(pull(data_to_plot, {{outcome_var}}))
  
  # Make line plot of true outcome data against simulated outcome data by model run
  # Specify time period to plot using `min_time_var` argument
  plot <- data_to_plot %>%
    
    filter({{time_var}} >= min_time_var) %>%
    
    ggplot() +
    
    geom_line(aes(x = {{time_var}},
                  y = {{outcome_var}}),
              colour = "#0072b2",
              linewidth = 1) + 
    
    geom_line(aes(x = {{time_var}},
                  y = {{outcome_sim_var}},
                  group = model_run),
              alpha = 0.005) + 
    
    scatter_plot_opts + 
    
    ylim(0, y_max * 1.1)
  
  return(plot)
  
}

# Define function to produce patchwork of ggplot objects using 'reduce' --------

make_patchwork_plot <- function(list, 
                                legend_position = NULL,
                                ...){
  
  # Make patchwork object by applying `+` operator to list of objects
  patchwork <- Reduce(`+`, list) +
    
    # Set plot layout
    plot_layout(...) &
    
    # Optional legend position
    theme(legend.position = legend_position,
          legend.box = "vertical")
  
  return(patchwork)
  
}