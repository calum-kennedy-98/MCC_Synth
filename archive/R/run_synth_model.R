#' Run Synth Package Synthetic Control Model
#'
#' @description
#' A thin wrapper around \code{\link[Synth]{dataprep}} and
#' \code{\link[Synth]{synth}} from the \pkg{Synth} package. Prepares the
#' \code{dataprep.out} object and fits the synthetic control model using
#' BFGS optimisation. Returns both the prepared data and the fitted synth
#' object in a single list for downstream use with \code{\link[Synth]{synth.tab}}
#' or \code{\link[Synth]{path.plot}}.
#'
#' @param data A data frame (not necessarily the full panel) to pass to
#'   \code{\link[Synth]{dataprep}}.
#' @param predictors Character vector of predictor variable names passed to
#'   \code{dataprep}.
#' @param special_predictors A list of special predictor specifications (period-
#'   specific outcome values), passed as \code{special.predictors} to
#'   \code{dataprep}.
#' @param time_predictors_prior Numeric vector of pre-treatment time periods
#'   used to compute predictor means.
#' @param dep_var Character string. Name of the dependent (outcome) variable.
#' @param unit_variable Character string. Name of the numeric unit identifier
#'   column.
#' @param time_variable Character string. Name of the numeric time identifier
#'   column.
#' @param treated_id Numeric scalar. Numeric ID of the treated unit.
#' @param controls_id Numeric vector. Numeric IDs of the control units.
#' @param time_optimise_ssr Numeric vector. Time periods over which the
#'   pre-treatment SSR is minimised.
#' @param time_plot Numeric vector. Time periods to include in the output for
#'   plotting.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{data}{The input data frame.}
#'   \item{data_prepared}{The \code{dataprep.out} object.}
#'   \item{synth_out}{The fitted \code{synth.out} object.}
#' }

# Define function to run synth model -------------------------------------------

run_synth_model <- function(data,
                            predictors,
                            special_predictors,
                            time_predictors_prior,
                            dep_var,
                            unit_variable,
                            time_variable,
                            treated_id,
                            controls_id,
                            time_optimise_ssr,
                            time_plot){
  
  # Generate 'dataprep.out' object from 'Synth' package
  data_prepared <- dataprep(foo = data,
                            predictors = predictors,
                            special.predictors = special_predictors,
                            time.predictors.prior = time_predictors_prior,
                            dependent = dep_var,
                            unit.variable = unit_variable,
                            time.variable = time_variable,
                            treatment.identifier = treated_id,
                            controls.identifier = controls_id,
                            time.optimize.ssr = time_optimise_ssr,
                            time.plot = time_plot)
  
  # Generate synth object
  synth_out <- synth(data_prepared, 
                     optimxmethod = "BFGS")
  
  # Return list of data_prepared and synth_out
  synth_output <- list(data = data,
                       data_prepared = data_prepared,
                       synth_out = synth_out)
  
  return(synth_output)
}