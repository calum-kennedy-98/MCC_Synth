# Name: _MCC_sandbox
# Description: Sandbox script to get to grips with MCC data
# Author: CK
# Date: 06-02-2025

# Comments ---------------------------------------------------------

# Sandbox ----------------------------------------------------------

library(dplyr)
library(here)
library(janitor)
library(Synth)
library(httpgd)
library(languageserver)
library(ggplot2)
library(tibble)
library(lubridate)

# Load data
load(here("//wsl.localhost/Ubuntu/home/calumkennedy98/MCC_synthetic_control/Analysis/data/toy/January_2023/Data_December_2022/MCCdata_20221216.RData"))

# Bind list together into data frame and perform data cleaning tasks
dlist <- lapply(dlist, function(df) {
  if ("dewp" %in% colnames(df)) {
    df$dewp <- as.double(df$dewp)
  }
  return(df)
})

dat <- bind_rows(dlist, .id = "column_label") %>%
  filter(!is.na(all)) %>%
  left_join(cities, by = c("column_label" = "city")) %>%
  clean_names() %>%
  select(
    column_label, year, date, doy, all, tmean, tmax, tmin, rhum, pm25,
    o3, no2, so2, co, countryname, cityname, region, kgclzone
  ) %>%
  as_tibble() %>%
  mutate(
    id = as.integer(factor(cityname)),
    week = week(date)
  ) %>%
  summarise(all = sum(all, na.rm = TRUE),
            pm25 = mean(pm25, na.rm = TRUE),
            .by = c(
            week,
            year,
            cityname,
            region
  ))