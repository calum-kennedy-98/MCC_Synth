# Utility functions for the MCC synthetic control project.
# Covers: simulation helpers, synthetic control weight rescaling,
# AR(2) modelling, and conformal inference primitives.

#' Generate Cauchy-Like Fat-Tail Values
#'
#' @description
#' Produces a vector of values following a Cauchy-like (fat-tailed) distribution
#' centred at the midpoint of the index range. Useful for simulating
#' dynamic treatment effects that peak at a given period and decay slowly.
#'
#' @param start_index Integer. Start of the index range.
#' @param end_index Integer. End of the index range.
#' @param amplitude Numeric. Peak height of the Cauchy-like curve at the
#'   midpoint.
#' @param gamma Numeric. Scale parameter controlling the spread of the
#'   distribution; larger values produce heavier tails.
#'
#' @return A numeric vector of length \code{end_index - start_index + 1}.
generate_fat_tail <- function(start_index, 
                              end_index, 
                              amplitude, 
                              gamma) {
  indices <- seq(start_index, 
                 end_index, 
                 length.out = end_index - start_index + 1)
  midpoint <- mean(indices)
  cauchy_like <- unlist(lapply(list(start_index:end_index), function(t){cauchy_val <- amplitude / (1 + ((t - midpoint)^2) / gamma^2)}))
  return(cauchy_like)
}

#' Multiple Imputation of Missing Data
#'
#' @description
#' Imputes missing values in a data frame using the \pkg{mice} package
#' (multivariate imputation by chained equations). Returns a single completed
#' data set.
#'
#' @param data A data frame potentially containing \code{NA} values.
#' @param maxit Integer. Number of iterations for the MICE algorithm.
#' @param m Integer. Number of imputed data sets to generate (only the first
#'   completed set is returned).
#' @param seed Integer. Random seed for reproducibility.
#'
#' @return A data frame with missing values imputed.
impute_missing_data <- function(data,
                                maxit,
                                m,
                                seed){
  
  data_imp <- mice(data, maxit = maxit, m = m, seed = seed)
  data_complete <- complete(data_imp)
  
  return(data_complete)
}

#' Apply a Function Over a List
#'
#' @description
#' A thin wrapper around \code{\link[purrr]{map}} that applies \code{func} to
#' each element of \code{list}, passing additional arguments via \code{...}.
#'
#' @param func A function to apply to each element of \code{list}.
#' @param list A list of inputs to iterate over.
#' @param ... Additional arguments passed to \code{func}.
#'
#' @return A list of results, one per element of \code{list}.
map_func <- function(func,
                     list,
                     ...){
  
  results <- map(list, function(x) {
    
    output <- func(x,
                   ...)
    
  })
  
  return(results)
  
}

#' Retry Synth Optimisation with Increasing Margin.ipop
#'
#' @description
#' Calls \code{\link[Synth]{synth}} in a \code{while} loop, incrementing
#' \code{Margin.ipop} after each failed attempt until the optimisation
#' converges or \code{max_attempts} is reached. This is necessary because the
#' ipop quadratic programme solver can fail to converge for small margin values
#' with certain donor pool configurations.
#'
#' @param X1 Numeric matrix (\eqn{m \times 1}). Predictor matrix for the
#'   treated unit, passed to \code{\link[Synth]{synth}}.
#' @param X0 Numeric matrix (\eqn{m \times N}). Predictor matrix for the
#'   control units.
#' @param Z1 Numeric matrix (\eqn{k \times 1}). Outcome predictor matrix for
#'   the treated unit.
#' @param Z0 Numeric matrix (\eqn{k \times N}). Outcome predictor matrix for
#'   the control units.
#' @param initial_margin Numeric. Starting value of \code{Margin.ipop}.
#' @param max_attempts Integer. Maximum number of attempts before giving up.
#' @param margin_increment Numeric. Amount added to \code{Margin.ipop} after
#'   each failed attempt.
#'
#' @return The \code{synth.out} object if optimisation succeeds, or
#'   \code{NULL} if all \code{max_attempts} fail.
retry_synth <- function(X1,
                        X0,
                        Z1,
                        Z0,
                        initial_margin,
                        max_attempts,
                        margin_increment) {
  
  attempts <- 0
  current_margin <- initial_margin
  result <- NULL
  
  while(attempts < max_attempts && is.null(result)) {
    attempts <- attempts + 1
    
    tryCatch({
      result <- synth(X1 = X1,
                      X0 = X0,
                      Z1 = Z1,
                      Z0 = Z0,
                      Margin.ipop = current_margin)
      
      message(paste("Optimization succeeded with Margin.ipop =", current_margin))
      
    }, error = function(e) {
      
      # Change optimisation method to 'all' for next attempt
      current_margin <- current_margin + margin_increment
    })
  }
  
  if(is.null(result)) {
    message(paste("Optimization failed after", max_attempts, "attempts"))
  }
  
  return(result)
}

#' Trim Near-Zero Weights and Rescale to Sum to One
#'
#' @description
#' Sets weights below \code{tolerance} to zero and (optionally) rescales the
#' remaining weights so they sum to one. Quadratic programming solvers often
#' produce very small but non-zero weights due to numerical precision; this
#' function cleans them up.
#'
#' @param w Numeric vector of weights to clean.
#' @param tolerance Numeric. Weights strictly less than this value are set to
#'   zero. Default is \code{1e-6}.
#' @param scale Logical. If \code{TRUE} (default), rescale the surviving
#'   weights so they sum to one.
#'
#' @return A numeric vector of cleaned (and optionally rescaled) weights.
rescale_small_weights <- function(w, 
                                  tolerance = 1e-6,
                                  scale = TRUE){
  
  w_rescaled <- ifelse(w < tolerance, 0, w)
  if(scale) w_rescaled <- w_rescaled / sum(w_rescaled)
  
  return(w_rescaled)
}

#' Fit an AR(2) Model to an Error Matrix
#'
#' @description
#' Estimates AR(2) coefficients by pooling across all rows (units) of the
#' error matrix \code{E}. Each row is treated as a time series, and the
#' coefficients are obtained via OLS on the stacked system of lagged residuals.
#' Used in the factor-model simulation to generate autocorrelated errors.
#'
#' @param E Numeric matrix of dimensions \eqn{n \times T}, where rows are
#'   units and columns are time periods.
#'
#' @return A numeric vector of length 2 containing the AR(2) coefficients
#'   \eqn{(\phi_1, \phi_2)}.
fit_ar_2 <- function(E){
  
  # Get n_periods
  n_periods <- dim(E)[2]
  
  E_ts <- E[,3:n_periods]
  E_lag_1 <- E[,2:(n_periods-1)]
  E_lag_2 <- E[,1:(n_periods-2)]
  
  a_1 <- sum(diag(E_lag_1 %*% t(E_lag_1)))
  a_2 <- sum(diag(E_lag_2 %*% t(E_lag_2)))
  a_3 <- sum(diag(E_lag_1 %*% t(E_lag_2)))
  
  matrix_factor <- rbind(c(a_1,a_3),c(a_3,a_2))
  
  b_1 <- sum(diag(E_lag_1 %*% t(E_ts)))
  b_2 <- sum(diag(E_lag_2 %*% t(E_ts)))
  
  ar_coef <- solve(matrix_factor) %*% c(b_1,b_2)
  
  return(ar_coef)
}

#' Build Correlation Matrix from AR(2) Coefficients
#'
#' @description
#' Computes the theoretical autocorrelation function (ACF) implied by an
#' AR(2) process with coefficients \code{ar_coef}, and assembles it into a
#' symmetric \eqn{T \times T} correlation (Toeplitz-like) matrix. Used to
#' generate correlated error draws in the factor-model simulation.
#'
#' @param ar_coef Numeric vector of length 2. AR(2) coefficients
#'   \eqn{(\phi_1, \phi_2)} as returned by \code{\link{fit_ar_2}}.
#' @param n_periods Integer. Number of time periods \eqn{T}; determines the
#'   dimension of the output matrix.
#'
#' @return A symmetric numeric matrix of dimensions \eqn{T \times T}
#'   containing the implied autocorrelations.
ar2_correlation_matrix <- function(ar_coef, n_periods) {

  result <- rep(0, n_periods)
  result[1] <- 1
  result[2] <- ar_coef[1]/(1-ar_coef[2])
  for (t in 3:n_periods){
    result[t] <-  ar_coef[1]*result[t-1] + ar_coef[2]*result[t-2] 
  }
  
  index_matrix <- outer(1:n_periods, 1:n_periods, function(x,y){ abs(y-x)+1 })
  cor_matrix <- matrix(result[index_matrix], ncol = n_periods, nrow = n_periods)
  
  return(cor_matrix)
}

# Conformal inference primitives -----------------------------------------------

#' Lq Norm Test Statistic for Conformal Inference
#'
#' @description
#' Computes the \eqn{L_q} norm of a residual vector. Used as the test statistic
#' in the block-permutation conformal inference procedure.
#'
#' @param residual_vec Numeric vector of residuals.
#' @param q Numeric. Degree of the \eqn{L_q} norm. Default is \code{1} (L1
#'   norm, i.e. sum of absolute values).
#'
#' @return A scalar numeric value: \eqn{\left(\sum_t |r_t|^q\right)^{1/q}}.
test_statistic <- function(residual_vec,
                           q = 1){
  
  s <- sum(abs(residual_vec)^q, na.rm = TRUE)^(1/q) 
  return(s)
}

#' Generate a Rolling Block Permutation Matrix
#'
#' @description
#' Creates a matrix of all cyclic (rolling) permutations of the input vector
#' \code{x}. Each column is a shifted version of \code{x}. Used internally by
#' \code{\link{block_subset}} to enumerate block permutations of the residual
#' series for conformal inference.
#'
#' @param x A vector to permute.
#'
#' @return A square matrix of dimensions \eqn{n \times n}, where \eqn{n =
#'   \texttt{length(x)}} and each column is a cyclic rotation of \code{x}.
roll_matrix <- function(x) {
  n <- length(x)
  idx <- outer(seq_len(n), 0:(n - 1),
               function(i, k) ((i - k - 1) %% n) + 1)
  matrix(x[idx], nrow = n)
}

#' Generate Block Permutation Matrix Subsetted to Post-Treatment Periods
#'
#' @description
#' Calls \code{\link{roll_matrix}} to generate all cyclic permutations of
#' \code{x} and then returns only the rows corresponding to post-treatment
#' periods (\code{g == 1}). The resulting matrix is used to evaluate the test
#' statistic under each permutation in \code{\link{get_p_value}}.
#'
#' @param x A numeric vector of residuals (length \eqn{T}).
#' @param g An integer or logical vector of length \eqn{T}. Post-treatment
#'   indicator (1 = post-treatment period, 0 = pre-treatment).
#'
#' @return A numeric matrix with \eqn{T_{\text{post}}} rows and \eqn{T}
#'   columns. Each column is a cyclic permutation of \code{x}, restricted to
#'   the post-treatment rows.
block_subset <- function(x, g) {
  stopifnot(length(x) == length(g))
  
  M <- roll_matrix(x)
  
  M[g == 1, , drop = FALSE]
}

#' Compute Conformal P-Value via Block Permutation of Residuals
#'
#' @description
#' Computes an exact conformal p-value by comparing the observed \eqn{L_q}
#' norm test statistic (applied to the post-treatment residuals) against the
#' distribution of test statistics obtained from all cyclic block permutations
#' of the full residual series. The p-value is the proportion of permutations
#' whose test statistic is at least as large as the observed value.
#'
#' @param data A data frame containing the residuals and post-treatment
#'   indicator columns.
#' @param residuals_var Character string. Quoted name of the residuals column.
#' @param post_var Character string. Quoted name of the post-treatment
#'   indicator column.
#' @param q Numeric. Degree of the \eqn{L_q} norm used in the test statistic.
#'
#' @return A scalar numeric value in \eqn{[0, 1]}: the exact conformal
#'   p-value.
get_p_value <- function(data,
                        residuals_var,
                        post_var,
                        q){
  
  # Extract residuals and post variables
  residuals <- pull(data, .data[[residuals_var]])
  post <- pull(data, .data[[post_var]])
  
  # Generate rolling block permutations of residuals and subset to post-treatment period
  block_permutations <- block_subset(residuals,
                                     post)
  
  # Get value of test statistic for each block of permutations
  vec_test_statistic <- apply(block_permutations, 2, test_statistic, q = q)
  
  # Return exact p-value (original residuals are first element in vector)
  p_val <- sum(vec_test_statistic >= vec_test_statistic[1]) / length(vec_test_statistic)
  
  return(p_val)
  
}

#' Generate Potential Outcomes Under the Null Hypothesis
#'
#' @description
#' Constructs a modified outcome column \code{Y0_null} representing the
#' untreated potential outcomes of the treated unit under a specified null
#' hypothesis \eqn{\tau_0}. Post-treatment treated outcomes are adjusted as
#' \eqn{Y^0_{\text{post}} = Y_{\text{obs,post}} - \tau_0}; pre-treatment
#' treated outcomes and all control outcomes are left unchanged. This
#' null-adjusted dataset is then passed to the synthetic control residual
#' estimation in \code{\link{get_residuals_conformal_inference}}.
#'
#' @param data A data frame in long format containing all units and time
#'   periods.
#' @param outcome_var Character string. Quoted name of the observed outcome
#'   variable.
#' @param treated_var Character string. Quoted name of the binary treatment
#'   indicator (1 = treated unit, 0 = control).
#' @param post_var Character string. Quoted name of the binary post-treatment
#'   indicator.
#' @param null_hypothesis Numeric scalar. The posited treatment effect
#'   \eqn{\tau_0} under the null.
#'
#' @return The input data frame with an additional column \code{Y0_null}:
#'   for the treated unit, post-treatment values are shifted by
#'   \eqn{-\tau_0}; all other values equal the observed outcome.
get_Y0_null <- function(data,
                        outcome_var,
                        treated_var,
                        post_var,
                        null_hypothesis){
  
  # Extract Y_treated_pre & Y_treated_post
  Y_treated_pre <- data %>% 
    filter(.data[[treated_var]]==1 & .data[[post_var]]==0) %>%
    pull(.data[[outcome_var]])
  
  Y_treated_post <- data %>% 
    filter(.data[[treated_var]]==1 & .data[[post_var]]==1) %>%
    pull(.data[[outcome_var]])
  
  # Add treatment effect to post-intervention outcomes
  Y0_treated_post_null <- Y_treated_post - null_hypothesis
  
  # Get vector Y0 under null
  Y0_treated_null <- c(Y_treated_pre, Y0_treated_post_null)
  
  data_with_Y0 <- data %>%
    
    mutate(Y0_null = rep(Y0_treated_null, nrow(data) / length(Y0_treated_null))) %>%
    mutate(Y0_null = if_else(.data[[treated_var]]==1,Y0_null,.data[[outcome_var]]))
  
  return(data_with_Y0)
  
}

#' Extract Confidence Interval from a P-Value Grid
#'
#' @description
#' Inverts a grid of conformal p-values (as returned by
#' \code{\link{get_p_value_grid}}) to form a confidence interval. All null
#' hypothesis values for which the p-value meets or exceeds \code{alpha} are
#' retained; the lower and upper endpoints of the confidence interval are the
#' minimum and maximum of these retained values.
#'
#' @param p_val_grid A data frame with one row per null hypothesis value,
#'   containing at least the p-value column and the null hypothesis column.
#' @param p_val_var Character string. Quoted name of the p-value column.
#' @param null_hypothesis_var Character string. Quoted name of the null
#'   hypothesis column.
#' @param alpha Numeric. Significance level. Null hypotheses with
#'   \code{p_val >= alpha} are included in the confidence interval.
#'   Default is \code{0.05}.
#'
#' @return A tibble with two columns: \code{conf_int_lower} and
#'   \code{conf_int_upper}, representing the endpoints of the
#'   \eqn{(1 - \alpha)} confidence interval.
get_confidence_interval <- function(p_val_grid,
                                    p_val_var,
                                    null_hypothesis_var,
                                    alpha = 0.05){
  
  # Subset dataframe to include only large p-values
  p_val_grid_large <- filter(p_val_grid, .data[[p_val_var]] >= alpha)
  
  # Extract largest and smallest values of null hypothesis var to form confidence interval (note that remove NA values)
  conf_int_lower <- min(pull(p_val_grid_large, .data[[null_hypothesis_var]]), na.rm = TRUE)
  conf_int_upper <- max(pull(p_val_grid_large, .data[[null_hypothesis_var]]), na.rm = TRUE)
  
  # Compile results in tibble
  results <- tibble("conf_int_lower" = conf_int_lower,
                    "conf_int_upper" = conf_int_upper)
  
  return(results)
  
}