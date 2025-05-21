# source("scripts/setup/00_01_load_packages.R")
# source("scripts/setup/00_02_load_functions.R")
source("./setup/00_03_load_paths.R")
library(tidyverse)
library(sf)
library(tigris)
#options(tigris_use_cache = TRUE)
#source("scripts/setup/00_04_load_settings.R")
# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Aggregates 10 km grid smokePM predictions to county level.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set date range to aggregate
start_date = "20060101" # "20060101"
end_date = "20241231" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

# smoke PM predictions 
smokePM <- readRDS(file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM",  "revisions", "predictions_marissa", sprintf("smokePM_predictions_10km_%s_%s.rds", start_date, end_date)))

#year of data to aggregate
years <- 2006:2024
# Initialize an empty list to store results from each year
consistent_county_aggregated_list <- list()

for (year_i in years) {#year_i = 2006
  
  # Use 2023 crosswalk for 2024 to ensure consistency
  crosswalk_year <- if (year_i == 2024) 2023 else year_i #we use 2023 pop weights for 2024
  
  print(crosswalk_year)
  
  # Load population crosswalks
  unit_cross <- readRDS(file.path(
    path_data_sherlock, "1_grids", "county_crosswalks",
    sprintf("consistent_counties_grid_pop_crosswalk_%s.rds", crosswalk_year)
  ))
  
  # Load smoke PM predictions for the given year 
  smokePM_year <- smokePM %>% 
    filter(year(date) == year_i)
  
  # Identify smoke-days per grid unit
  unit_smoke_days <- smokePM_year %>%
    # Add unit information (GEOID), duplicating rows if grids belong to multiple counties
    left_join(unit_cross %>%
                st_drop_geometry() %>%
                select(grid_id_10km, GEOID),
              by = "grid_id_10km") %>%
    # Drop grid cells that don't match to a unit
    filter(!is.na(GEOID)) %>%
    # Select only the relevant columns
    select(date, GEOID) %>%
    # Keep only unique unit-days
    distinct()
  
  # Join unit-days with crosswalk and smoke PM predictions
  unit_smokePM <- unit_smoke_days %>%
    # Join all grid-cells for each unit
    left_join(unit_cross %>% st_drop_geometry(), by = "GEOID") %>%
    # Join smoke PM predictions
    left_join(smokePM, by = c("grid_id_10km", "date"))
  
  # Fill missing smokePM values with 0 and calculate population-weighted average
  avg_unit_smokePM <- unit_smokePM %>%
    # Replace missing predictions with 0
    replace_na(list(smokePM_pred = 0)) %>%
    # Group by unit and date
    group_by(GEOID, date) %>%
    #replace pop = 0 for 1 in places where all cells are 0
    mutate(pop = ifelse(rep(all(pop == 0), n()), 1, pop)) %>% 
    # Calculate population-weighted mean smoke PM
    summarise(smokePM_pred = weighted.mean(smokePM_pred, pop), .groups = "drop") %>%
    # Add year column for future filtering or analysis
    mutate(year = year_i)
  
  # Save this year's result to the list
  consistent_county_aggregated_list[[as.character(year_i)]] <- avg_unit_smokePM
  
  print(paste0("processed year ", crosswalk_year))

}

# Combine all years into a single data frame
consistent_county_aggregated <- bind_rows(consistent_county_aggregated_list)

saveRDS(consistent_county_aggregated, 
        file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM",  "revisions", "aggregated",
                  sprintf("consistent_counties_smokePM_predictions_yearly_weighted_%s-%s.rds",  start_date, end_date)))

