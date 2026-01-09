### ### ### ### ### ### ### ### ### 
### CREATE LINES AND GEOPACKAGES ###
### ### ### ### ### ### ### ### ### 

# SETUP----
## Load packages----
library(sf)
library(tidyverse)
library(here)
library(sfheaders)

## Load data----
wp1_locs <- 
  list.files(
    here("data"),
    pattern = "Groslijst_gebieden", full.names = TRUE
  ) |> 
  readxl::read_xlsx(
  sheet = "locs_wp1"
) |> 
  janitor::clean_names()

wp2_locs <- list.files(
  here("data"),
  pattern = "Groslijst_gebieden", full.names = TRUE
) |> 
  readxl::read_xlsx(
    sheet = "locs_wp2"
  ) |> 
  janitor::clean_names()

# PREPARATION ----
## Filter lines
wp1_lines <- 
  wp1_locs |> 
  filter(
    !is.na(x_begin) & !is.na(y_eind)
  ) |> 
  rename(
    Begin_X = x_begin, Begin_Y = y_begin, Eind_X = x_eind, Eind_Y = y_eind
  ) |> 
  # Split coordinates from Start-Eind into seperate rows
  pivot_longer(
    Begin_X:Eind_Y,
    names_to = c("Type", ".value"), names_sep = "_") |> 
  st_as_sf(
    coords = c("Y", "X"), crs = 4326
  ) |> 
  st_transform(crs = 28992) |> 
  group_by(unieke_code_vegetatie_oever_penetrometer) |> 
  summarise() |> 
  st_cast("LINESTRING") |> 
  left_join(
    y = wp1_locs |> select(-c(x:y_eind)),
    by = "unieke_code_vegetatie_oever_penetrometer"
  ) |> 
  group_by(gebied_5)

wp2_lines <-
  wp2_locs |> 
  filter(
    !is.na(x_begin) & !is.na(y_eind)
  ) |> 
  rename(
    Begin_X = x_begin, Begin_Y = y_begin, Eind_X = x_eind, Eind_Y = y_eind
  ) |> 
  pivot_longer(
    Begin_X:Eind_Y,
    names_to = c("Type", ".value"), names_sep = "_") |> 
  st_as_sf(
    coords = c("Y", "X"), crs = 4326
  ) |> 
  st_transform(crs = 28992) |> 
  group_by(unieke_code_vegetatie_oever_penetrometer_oevermonster) |> 
  summarise() |> 
  st_cast("MULTIPOINT") |> 
  st_cast("LINESTRING") |> 
  left_join(
    y = wp2_locs |> select(-c(x:y_eind)),
    by = "unieke_code_vegetatie_oever_penetrometer_oevermonster"
  ) |> 
  group_by(gebied_10)

## Filter points
wp1_points <-
  wp1_locs |> 
  filter(
    !is.na(x) | (!is.na(x_begin) & is.na(y_eind))
  ) |> 
  mutate(
    x = case_when(
      is.na(x) ~ x_begin,
      TRUE ~ x
    ),
    y = case_when(
      is.na(y) ~ y_begin,
      TRUE ~ y
    )
  ) |> 
  st_as_sf(
    coords = c("y", "x"), crs = 4326
  ) |> 
  st_transform(crs = 28992) |> 
  group_by(gebied_5)
  
wp2_points <-
  wp2_locs |> 
  filter(
    !is.na(x) | (!is.na(x_begin) & is.na(y_eind))
  ) |> 
  mutate(
    x = case_when(
      is.na(x) ~ x_begin,
      TRUE ~ x
    ),
    y = case_when(
      is.na(y) ~ y_begin,
      TRUE ~ y
    )
  ) |> 
  st_as_sf(
    coords = c("y", "x"), crs = 4326
  ) |> 
  st_transform(crs = 28992) |> 
  group_by(gebied_10)

# Export ----
# st_write(wp1_points, here("output", "240703_bekende_locaties.gpkg"),  layer = "wp1_punt")
# st_write(wp1_lines, here("output", "240703_bekende_locaties.gpkg"),  layer = "wp1_traject")
# st_write(wp2_points, here("output", "240703_bekende_locaties.gpkg"),  layer = "wp2_punt")
# st_write(wp2_lines, here("output", "240703_bekende_locaties.gpkg"),  layer = "wp2_traject")
