# Name of script: make_scatter_plot_tau_hat_time
# Description: Function to produce scatter plot of mean estimated tau hat
# coefficients vs time (including SE error bars)
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 18-06-2025
# Latest update by: Calum Kennedy
# Latest update on: 18-06-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

# @ param ...

make_scatter_plot_tau_hat_time <- function(data,
                                           tau_hat_var,
                                           se_var,
                                           time_var,
                                           facet_var = NULL,
                                           palette){
  
  # Make plot
  plot <- ggplot(data) + 
    
    geom_point(
      aes(x = {{time_var}},
          y = {{tau_hat_var}})
      ) +
    
    geom_errorbar(
      aes(x = {{time_var}},
          ymin = {{tau_hat_var}} - 1.96 * {{se_var}},
          ymax = {{tau_hat_var}} + 1.96 * {{se_var}})
      ) +
    
    # Horizontal line at tau_hat = 0
    geom_hline(aes(yintercept = 0),
               linetype = "dashed") +
    
    # Vertical line at t = 0 (first treated period)
    geom_vline(aes(xintercept = 0,
                   linetype = "dashed")) +
    
    # Optional facet wrap
    facet_wrap(vars({{facet_var}})) +
    
    scatter_plot_opts +
    
    scale_colour_manual(values = palette)
  
  return(plot)
  
}