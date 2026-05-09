#' Subset a Data Frame by Multiple Filter Conditions
#'
#' @description
#' Filters a data frame based on one or more arbitrary logical expressions
#' supplied via \code{...}, and optionally selects a specified subset of
#' columns. All filter conditions are captured as quosures and spliced into
#' \code{\link[dplyr]{filter}} using tidy evaluation. If \code{vars_to_select}
#' is provided, the function checks that all named columns exist before
#' selecting them (errors informatively if any are missing).
#'
#' @param data A data frame to filter and optionally column-select.
#' @param ... Logical filter expressions (unquoted, tidy-eval), passed to
#'   \code{\link[dplyr]{filter}}. Multiple conditions are combined with AND.
#' @param vars_to_select Character vector of column names to retain after
#'   filtering. Pass \code{NULL} (default) to keep all columns.
#'
#' @return A data frame (or tibble) containing only the rows that satisfy all
#'   filter conditions, and only the columns in \code{vars_to_select} (if
#'   provided).

# Define function to subset data -----------------------------------------------

subset_data <- function(data,
                        ...,
                        vars_to_select = NULL){
  
  conditions <- enquos(...)
  
  # If select_columns is provided, check if those columns exist
  if (!is.null(vars_to_select)) {
    missing_select_vars <- setdiff(vars_to_select, names(data))
    
    if (length(missing_select_vars) > 0) {
      stop("The following columns used in selection do not exist in the dataframe: ", 
           paste(missing_select_vars, collapse = ", "))
    }
  }
  
  # Apply filtering and selection
  data <- data %>%
    filter(!!!conditions)
  
  if (!is.null(vars_to_select)) {
    data <- data %>% tidylog::select(all_of(vars_to_select))
  }
  
  return(data)
  
}