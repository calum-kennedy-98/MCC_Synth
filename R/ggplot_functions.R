# ggplot helper functions for the MCC synthetic control project.

#' Plot Observed vs Simulated Outcome Data
#'
#' @description
#' Filters each data frame in \code{list_data} to a single location and
#' produces a \code{ggplot2} line plot overlaying the observed outcome series
#' (bold) with all simulated replicates (faint, semi-transparent). Useful for
#' visually assessing how well the simulation data-generating process reproduces
#' the observed distribution.
#'
#' @param list_data A list of data frames, each containing the same structure
#'   with observed and simulated outcome columns.
#' @param location_var Bare (unquoted) name of the location identifier column
#'   (tidy-eval).
#' @param location_string Character string. The specific location value to
#'   filter on (matched against \code{location_var}).
#' @param time_var Bare (unquoted) name of the time variable used for the
#'   x-axis (tidy-eval).
#' @param min_time_var Numeric. Minimum value of \code{time_var} to include in
#'   the plot (earlier periods are excluded).
#' @param outcome_var Bare (unquoted) name of the observed outcome variable
#'   (tidy-eval).
#' @param outcome_sim_var Bare (unquoted) name of the simulated outcome variable
#'   (tidy-eval).
#'
#' @return A \code{ggplot2} object.
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
              linewidth = 1) + 
    
    geom_line(aes(x = {{time_var}},
                  y = {{outcome_sim_var}},
                  group = model_run),
              alpha = 0.005) + 
    
    scatter_plot_opts + 
    
    ylim(0, y_max * 1.1)
  
  return(plot)
  
}

#' Combine ggplot Objects into a Patchwork Layout
#'
#' @description
#' Reduces a list of \code{ggplot2} objects into a single
#' \code{\link[patchwork]{patchwork}} composition using the \code{+} operator,
#' and applies a shared layout and legend position across all panels.
#'
#' @param list A list of \code{ggplot2} objects to combine.
#' @param legend_position Character string passed to
#'   \code{\link[ggplot2]{theme}} specifying the legend position (e.g.
#'   \code{"bottom"}, \code{"none"}). Default is \code{NULL} (use each plot's
#'   own legend position).
#' @param ... Additional arguments passed to
#'   \code{\link[patchwork]{plot_layout}} (e.g. \code{ncol}, \code{nrow},
#'   \code{guides}).
#'
#' @return A \code{patchwork} object.
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