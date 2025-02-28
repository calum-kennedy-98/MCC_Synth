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
library(ggplot2)
library(tibble)
library(lubridate)

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
  ) 

dat %>% 
  filter(year == 2020 & 
           region == "Eastern Asia" &
           week < 52) %>% 
  filter(!is.na(all)) %>% 
  summarise(all = sum(all, na.rm = TRUE), 
            days = n(), 
            .by = c(column_label, week)) %>% 
  group_by(column_label) %>% 
  filter(any(all > 400)) %>% 
  ggplot(aes(x = week, y = all, group = column_label)) + 
  geom_line(alpha = 0.3) + 
  scatter_plot_opts +
  ggtitle("Weekly mortality in East Asian cities - 2019") +
  labs(x = "Week",
        y = "Total\nmortality")

ggsave(here("Output/Figures/mortality_2019_east_asia.png"), width = 8, height = 5)
