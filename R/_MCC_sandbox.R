# Name: _MCC_sandbox
# Description: Sandbox script to get to grips with MCC data
# Author: CK
# Date: 06-02-2025

# Comments ---------------------------------------------------------

# Sandbox ----------------------------------------------------------

library(dplyr)
library(here)
library(janitor)

# Load data
load(here("data/toy/January_2023/Data_December_2022/MCC_AgeCause_20221216.RData"))

# Bind list together into data frame and perform data cleaning tasks
dat <- bind_rows(dlist, .id = "column_label") %>%
    clean_names()
