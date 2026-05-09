# load_env.R
#
# Sets up shared plot aesthetics used throughout the MCC synthetic control
# project. Source this file at the start of any analysis script to ensure
# consistent styling.
#
# Objects defined here:
#   scatter_plot_opts  - A list of ggplot2 theme elements applied to all plots.
#   cbbPalette         - A 10-colour, colorblind-friendly palette (character
#                        vector of hex codes).

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
                "#ee3377",
                "#F0E442",
                "#D55E00",
                "#CC79A7",
                "#bbbbbb")