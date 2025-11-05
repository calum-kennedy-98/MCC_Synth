# Name of script: 0_LoadEnv
# Description:  Loads plot options, palette, etc
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 04-02-2025
# Latest update by: Calum Kennedy
# Latest update on: 04-02-2025

# Set global plot options ------------------------------------------------------

# Scatter plot options
scatter_plot_opts <- list(
  
  # Main theme
  theme_bw(base_size = 12),
  
  # Theme customisation
  theme(
    # Title options
    plot.title = element_text(face = "bold"),
    
    # Panel options
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Legend options
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    
    # Facet options
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    
    # Axis options
    axis.line = element_line(),
    axis.title.y=element_text(angle=0))
)

# Discrete colour palette
cbbPalette <- c("#E69F00",
                "#0072B2",
                "#009E73",
                "#56B4E9",
                "#000000",
                "#F0E442",
                "#D55E00",
                "#CC79A7",
                "#bbbbbb",
                "#ee3377")