# Name of script: extract_synth_results_main
# Description: Function to extract estimated tau hats from list of main synthetic control
# model outputs. Appends the geographic region from the 'list_data_for_synth' 
# argument
# Created by: Calum Kennedy (calum.kennedy.20@ucl.ac.uk)
# Created on: 05-03-2025
# Latest update by: Calum Kennedy
# Latest update on: 05-03-2025

# Comments ---------------------------------------------------------------------



# Function ---------------------------------------------------------------------

# @ param ...

extract_synth_results_main <- function(list_data_for_synth,
                                       list_results_synth){
  
  # Generate ID for event ID and add to synth results and synth data
  event_id <- seq(1:length(list_results_synth))
  
  list_data_for_synth_with_id <- lapply(seq_along(list_data_for_synth),
                                       function(x) {
                                         list_data_for_synth[[x]] %>% mutate(event_id = x)
                                       })
  
  list_results_synth_with_id <- lapply(seq_along(list_results_synth),
                                                  function(x) {
                                                    c(list_results_synth[[x]], event_id = x)
                                                  })
  
  
  # Get list of results from synth model
  list_results_to_merge <- lapply(list_results_synth_with_id, function(x){
    
    # Extract method
    method <- x[["method"]]
    
    # Extract true observed outcomes Y and estimated Y0_hat for treated unit
    Y_treated <- x[["Y_treated"]]
    Y0_treated_hat <- c(x[["Y0_treated_hat"]])
    
    # Extract post-treatment indicator and vector of time ids (re-centred so t = 0 is first treated period)
    post <- x[["post"]]
    t <- 0:(length(post) - 1) - length(post[post == 0])
    
    # Extract estimated tau hat
    tau_hat <- Y_treated - Y0_treated_hat
    
    # Return tibble of results
    results <- tibble("method" = method,
                      "event_id" = x[["event_id"]],
                      "t" = t,
                      "post" = post,
                      "tau_hat" = tau_hat,
                      "Y_treated" = Y_treated,
                      "Y0_treated_hat" = Y0_treated_hat)
    
  })
  
  # Extract meta-data from 'list_data_for_synth'
  list_metadata_to_merge <- lapply(list_data_for_synth_with_id, function(x){
    
    # Extract metadata
    event_id <- x %>% filter(treated == 1) %>% distinct(event_id) %>% pull()
    region <- x %>% filter(treated == 1) %>% distinct(region) %>% pull()
    countryname <- x %>% filter(treated == 1) %>% distinct(countryname) %>% pull()
    column_label <- x %>% filter(treated == 1) %>% distinct(column_label) %>% pull()
    
    metadata <- tibble("event_id" = event_id,
                       "region" = region,
                       "countryname" = countryname,
                       "column_label" = column_label)
    
  })
  
  # Bind results and metadata into single dataframe and merge
  metadata_synth <- bind_rows(list_metadata_to_merge)
  
  results_synth <- bind_rows(list_results_to_merge) %>% left_join(metadata_synth, by = "event_id")
  
  return(results_synth)
  
}