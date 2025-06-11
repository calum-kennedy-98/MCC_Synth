# Name of script: make_density_plot_synth_results
# Description: Function to produce density plot of results from synthetic control
# simulations
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 11-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 11-06-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

# @ param ...

make_density_plot_synth_results <- function(data,
                                            density_var,
                                            method_var,
                                            model_run_var,
                                            palette){
  
  # Make density plot
  plot <- ggplot(data) +
    
    geom_density(
      aes(
        x = {{density_var}}, 
        colour = {{method_var}}
      ),
      linewidth = 0.5
    ) +
    
    geom_vline(xintercept = 0,
               linetype = "dashed") +
    
    scatter_plot_opts +
    
    scale_colour_manual(values = palette)
  
  return(plot)
  
}