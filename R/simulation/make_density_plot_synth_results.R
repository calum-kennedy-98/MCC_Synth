#' Density Plot of Synthetic Control Simulation Results by Method
#'
#' @description
#' Produces a \code{ggplot2} density plot of a user-specified variable (e.g.
#' \eqn{\hat\tau - \tau}) for each synthetic control method, with a vertical
#' dashed reference line at zero. Useful for comparing the bias and spread of
#' estimators across simulation replicates.
#'
#' @param data A data frame of simulation results, typically as returned by
#'   \code{\link{extract_tau_hat_synth_results}}.
#' @param density_var Bare (unquoted) name of the variable to plot on the
#'   x-axis of the density (tidy-eval).
#' @param method_var Bare (unquoted) name of the SC method identifier column
#'   used to colour the densities (tidy-eval).
#' @param model_run_var Bare (unquoted) name of the simulation run identifier
#'   column (tidy-eval; accepted but not directly used in the current plot
#'   aesthetics).
#' @param palette Character vector of colour hex codes or named colours passed
#'   to \code{\link[ggplot2]{scale_colour_manual}}.
#'
#' @return A \code{ggplot2} object.
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