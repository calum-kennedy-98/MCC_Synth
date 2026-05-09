#' Load and Pre-Process the MCC Dataset
#'
#' @description
#' Loads the Multi-Country Multi-City (MCC) collaborative mortality dataset
#' from a saved \code{.RData} file, merges city-level data frames into a
#' single panel, retains observations from 2000–2019, and applies standard
#' cleaning steps (type coercion, deduplication, creation of continuous day
#' and week ID variables). Returns only the columns specified by
#' \code{vars_to_select}.
#'
#' @param path_data_mcc Character string. Full file path to the \code{.RData}
#'   file containing the MCC dataset objects \code{dlist} (a list of city
#'   data frames) and \code{cities} (a city-level metadata data frame).
#' @param id_var Bare (unquoted) name of the unit (city) identifier column
#'   (tidy-eval). Used downstream; not directly filtered on in this function.
#' @param vars_to_select Character vector of column names to retain in the
#'   returned data frame.
#'
#' @return A tibble with one row per city-date observation (2000–2019),
#'   containing the columns specified in \code{vars_to_select} plus
#'   \code{day_id} (integer days since dataset start) and \code{week_id}
#'   (integer weeks since dataset start).
#'
#' @details
#' The function expects the \code{.RData} file to expose objects named
#' \code{dlist} and \code{cities} in the calling environment upon
#' \code{\link{load}}. City names are matched via the \code{column_label}
#' / \code{city} key.

# Define function to load and process MCC data ---------------------------------

get_data_mcc <- function(path_data_mcc,
                         id_var,
                         vars_to_select){
  
  # Load data
  load(path_data_mcc)
  
  # Clean 'dewp' column
  dlist <- lapply(dlist, function(df) {
    if ("dewp" %in% colnames(df)) {
      df$dewp <- as.double(df$dewp)
    }
    return(df)
  })
  
  # Merge dlist items into single MCC dataset and retain data from 2000-2019 only
  # Keep distinct city-date rows and remove duplicates
  # Generate smoothed moving averages of mortality and pollution variables
  data_mcc <- bind_rows(
    dlist, .id = "column_label"
    ) %>%
    
    # Left join to cities dataframe
    left_join(
      cities, by = c("column_label" = "city")
      ) %>%
    
    # Clean names and set as tibble
    clean_names() %>%
    
    as_tibble() %>%
    
    # Keep data between 2000-2019
    filter(
      between(year, 2000, 2019)
      ) %>%
    
    # Set character columns to factors and arrays to numeric
    mutate(
      across(
        where(is.character),
        as.factor
        ),
      across(
        where(is.array),
        as.numeric
        )
    ) %>%
    
    # Create unique day and week ID variable which carries over across years
    mutate(
      day_id = as.numeric(date - min(date)),
      week_id = day_id %/% 7 + 1
    ) %>%
    
    # Keep distinct unit-time pairs (some duplicates in raw data)
    distinct(
      column_label, date, .keep_all = TRUE
      ) %>%
    
    # Select relevant columns
    select(
      vars_to_select
      )
  
  # Return merged dataset
  return(data_mcc)
  
}