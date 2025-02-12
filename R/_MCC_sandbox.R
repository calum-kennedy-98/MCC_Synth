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

# Load data
load(here("data/toy/January_2023/Data_December_2022/MCCdata_20221216.RData"))

# Bind list together into data frame and perform data cleaning tasks
dlist <- lapply(dlist, function(df) {
  if ("dewp" %in% colnames(df)) {
    df$dewp <- as.double(df$dewp)
  }
  return(df)
})

dat <- bind_rows(dlist, .id = "column_label") %>%
  filter(year == 2015) %>%
  left_join(cities, by = c("column_label" = "city")) %>%
  clean_names() %>%
  filter(countryname == "Canada") %>%
  select(
    column_label, year, doy, all, tmean, tmax, tmin, rhum, pm25,
    o3, no2, so2, co, countryname, cityname, region, kgclzone
  ) %>%
  mutate(id = as.integer(factor(cityname))) %>%
  filter(!is.na(pm25) & !is.na(all)) %>%

dataprep_out <- dataprep(
  foo = dat,
  predictors = c("pm25"),
  predictors.op = "mean",
  dependent = "all",
  unit.variable = c("id"),
  time.variable = c("doy"),
  special.predictors = list(
    list("all", 1:179, "mean")
  ),
  treatment.identifier = 17,
  controls.identifier = c(1:5, 7:9, 11:13, 15, 16, 18:26),
time.predictors.prior = 1:179,
time.optimize.ssr = 150:179,
time.plot = 1:365,
unit.names.variable = "cityname"
)

synth_out <- synth(dataprep.out, method = "BFGS")

path.plot(
  synth.res = synth_out,
  dataprep.res = dataprep_out,
  Xlab = "DOY",
  Ylab = "Mortality - all"
)

gaps.plot(
  synth.res = synth_out, 
  dataprep.res = dataprep_out, 
  Xlab = "DOY",
  Ylab = "Mortality - all"
)
