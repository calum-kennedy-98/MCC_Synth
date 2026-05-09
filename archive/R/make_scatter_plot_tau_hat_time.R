#' Scatter Plot of Estimated Treatment Effects Over Event Time
#'
#' @description
#' Produces a \code{ggplot2} scatter plot of estimated treatment effects
#' (\eqn{\hat\tau_t}) against event time, with symmetric 95% error bars
#' (\eqn{\pm 1.96 \times \text{SE}}). Dashed reference lines are drawn at
#' \eqn{\hat\tau = 0} and \eqn{t = 0} (treatment onset). Supports optional
#' faceting by a grouping variable.
#'
#' @param data A data frame containing the variables referenced by the other
#'   arguments.
#' @param tau_hat_var Bare (unquoted) name of the column containing the
#'   estimated treatment effect \eqn{\hat\tau_t} (tidy-eval).
#' @param se_var Bare (unquoted) name of the column containing standard errors
#'   for \eqn{\hat\tau_t} (tidy-eval).
#' @param time_var Bare (unquoted) name of the event-time column used for the
#'   x-axis (tidy-eval).
#' @param facet_var Bare (unquoted) name of an optional variable used to
#'   facet the plot via \code{\link[ggplot2]{facet_wrap}}. Pass \code{NULL}
#'   (default) for no faceting.
#' @param palette Character vector of colour hex codes or named colours passed
#'   to \code{\link[ggplot2]{scale_colour_manual}}.
#'
#' @return A \code{ggplot2} object.
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
    geom_vline(aes(xintercept = 0),
                   linetype = "dashed") +
    
    # Optional facet wrap
    facet_wrap(vars({{facet_var}})) +
    
    scatter_plot_opts +
    
    scale_colour_manual(values = palette)
  
  return(plot)
  
}