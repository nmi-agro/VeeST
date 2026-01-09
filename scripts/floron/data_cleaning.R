### ### ### ### ### ### ### 
### VeeST DATA CLEANING ### 
### ### ### ### ### ### ### 

# Setup----
## Load packages----
library(tidyverse)
library(here)

## Load custom functions----
source("scripts", "fn_SlootID_cleaning.R")


# Vegetatie----
## List all files----
veg_files <- 
  list.files(
    here("VEG", "ODK"),
    pattern = "VeeST_Veldformulier_vegetatie_v240626_results",
    full.names = TRUE
  )
## Load latest data----
veg_raw <- 
  veg_files |> 
  pluck(
    which.max(file.mtime(veg_files))
  ) |> 
  read_csv()

# Data cleaning steps
veg_clean <- 
  veg_raw |> 
  ## Filter Test data
  filter(
    !is.na(SlootID),
    str_detect(SlootID, pattern = "Test", negate = TRUE)
  ) |> 
  ## Remove 'traject' from column names
  rename_with(
    ~ str_remove(.x, pattern = "traject_")
  ) |> 
  ## Standardise 'Slootcodes' with regex
  slootid_clean_veg()

# Abiotiek data----
## Load data----
abio_files <- 
  list.files(
    here("ABIO", "ODK"),
    pattern = "VeeST_Veldformulier", full.names = TRUE
  ) 
## Load latest data----
abio_raw <- 
  abio_files |>
  pluck(
    which.max(file.mtime(abio_files))
  ) |> 
  read_csv()

## Data cleaning----
abio_clean <- 
  abio_raw |> 
  ## Filter Test data
  filter(
    !is.na(SlootID),
    str_detect(SlootID, pattern = "Test", negate = TRUE)
  ) |> 
  ## Remove 'traject' from column names
  rename_with(
    ~ str_remove(.x, pattern = "traject_")
  ) |> 
  ## Standardise 'Slootcodes' with regex
  slootid_clean_abio()

# Temp ----
veg_sel <- 
  veg_clean |> 
  rename(
    "date_veg" = Datemanual
  ) |> 
  select(
    SlootID, date_veg
  )

abio_sel <-
  abio_clean |> 
  rename(
    "date_abio" = Datemanual
  ) |> 
  select(
    SlootID, date_abio
  )

veg_sel |> 
  full_join(abio_sel) |> 
  filter(
    is.na(date_veg) | is.na(date_abio)
    ) |> 
  View()
