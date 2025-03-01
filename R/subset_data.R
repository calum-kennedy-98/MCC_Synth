# Name: subset_data
# Description: Script to select subset of arbitrary dataset based on set of user conditions
# Author: CK
# Date: 01-03-2025

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
    data <- data %>% select(all_of(vars_to_select))
  }
  
  return(data)
  
}