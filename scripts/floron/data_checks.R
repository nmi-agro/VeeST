### ### ### ### ### ### ### 
### VeeST Data checks ### 
### ### ### ### ### ### ### 

# Setup----
## Load packages----
library(tidyverse)
library(here)

## Load data----
veg_files <- 
  list.files(
    here("VEG", "ODK"),
    pattern = "VeeST_Veldformulier_vegetatie_v240626_results",
    full.names = TRUE
  )
veg_data <- 
  veg_files |> 
  pluck(
    which.max(file.mtime(veg_files))
  ) |> 
  read_csv() |> 
  filter(
    !is.na(SlootID),
    str_detect(SlootID, pattern = "Test", negate = TRUE)
  )

# Data checks
veg_data |> 
  select(contains("dat")) |> 
  View()
  