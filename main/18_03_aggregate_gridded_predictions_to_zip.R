# source("scripts/setup/00_01_load_packages.R")
# source("scripts/setup/00_02_load_functions.R")
source("/scratch/users/mmarti04/smokePM-prediction/scripts/setup/00_03_load_paths.R")
# source("scripts/setup/00_04_load_settings.R")
library(tidyverse)
library(doParallel)
library(foreach)
library(progressr)
library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Aggregates 10 km grid smokePM predictions to zip level.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set date range to aggregate
start_date = "20060101" # "20060101"
end_date = "20241231" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

#unit <- "zcta" # alternatively, "county"
# # load shapefile, plus 10km grid transformed to match the crs
# if (unit == "county") {
#   unit_sf <- counties() %>% 
#     filter(STATEFP %in% nonContig_stateFIPS == F)
# } else if (unit == "tract") {
#   unit_sf <- states() %>% 
#     filter(STATEFP %in% nonContig_stateFIPS == F) %>% 
#     pull(STATEFP) %>% 
#     map_dfr(function(x){
#       tracts(x, year = 2019) %>% select(STATEFP, GEOID)
#     })
# } else if (unit == "zcta") {
#   unit_sf <- states() %>% 
#     filter(STATEFP %in% nonContig_stateFIPS == F) %>% 
#     pull(STATEFP) %>% 
#     map_dfr(function(x){
#       zctas(x, year = 2019) %>% select(ZCTA5CE10, GEOID10)
#     })
# } else {
#   stop("only allowed units are \"tract\" or \"zcta\" or \"county\"")
# }

# read in the grid
# unit_cross <- readRDS(file.path(path_data_sherlock, "1_grids", paste0(unit, "_grid_area_crosswalk.rds")))

# smoke PM predictions 
smokePM <- readRDS(file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM", "revisions", "predictions_marissa",  sprintf("smokePM_predictions_10km_%s_%s.rds", start_date, end_date)))

# code negative values to 0
#smokePM <- smokePM %>% mutate(smokePM_pred = pmax(0, smokePM_pred))

# # population by grid cell
# pop <- list.files(file.path(path_data_sherlock, "2_from_EE", "populationDensity_10km_subgrid"),
#                   full.names = T) %>% purrr::map_dfr(read.csv)

zcta_aggregated_list<- list()

years <- 2006:2024


for (year_i in years) {#year_i = 2006
  
  # Use 2023 crosswalk for 2024 to ensure consistency
  crosswalk_year <- if (year_i == 2024) 2023 else year_i #we use 2023 pop weights for 2024
  
  print(crosswalk_year)
  
  # Load population crosswalks
  unit_cross <- readRDS(file.path(
    path_data_sherlock, "1_grids", "zcta_crosswalks",
    sprintf("tigris_zcta_grid_pop_crosswalk_%s.rds", crosswalk_year)
  ))
  
  
  # Load smoke PM predictions for the given year 
  smokePM_year <- smokePM %>% 
    filter(year(date) == year_i)
    
  avg_unit_smokePM<- smokePM_year %>%
    left_join(unit_cross %>% select(grid_id_10km, ZCTA5CE20), by = "grid_id_10km") %>% # add on all the tract/county unit IDs that are among the grid cells with smoke
    filter(!is.na(ZCTA5CE20)) %>% # drop grid cells that don't match to a unit
    select(ZCTA5CE20, date) %>%   # full set of unit-days with smoke 
    distinct() %>%
    left_join(unit_cross, by = "ZCTA5CE20") %>%  # join in all grid-cells for each unit
    mutate(year = year(date)) %>%
    #left_join(pop %>% select(grid_id_10km = ID, grid_pop = total_pop, year)) %>%   # join in population 
    left_join(smokePM_year, by = c("grid_id_10km", "date")) %>% #and smoke PM predictions
    replace_na(list(smokePM_pred = 0)) %>%  # fill missing smoke PM values with zero
    # mutate(area = unclass(area),
    #      pop = grid_pop_per_m2*area) %>%
    group_by(ZCTA5CE20, date) %>%  # for each unit-date calculate pop-weighted avg 
    mutate(pop = ifelse(rep(all(pop == 0), n()), 1, pop)) %>% #change places with all pop = 0 in all cells to 1
    summarise(smokePM_pred = weighted.mean(smokePM_pred, pop),
              .groups = "drop")
  
  # Save this year's result to the list
  zcta_aggregated_list[[as.character(year_i)]] <- avg_unit_smokePM  
  
  print(paste0("processed year ", year_i))
  
}

zcta_aggregated<- bind_rows(consistent_tract_aggregated_list)


saveRDS(zcta_aggregated, 
        file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM",  "revisions", "aggregated",
                  sprintf("zcta_smokePM_predictions_yearly_weighted_%s-%s.rds",  start_date, end_date)))


#------------------------------------------------------------------------------------------------------
# loop through work with one date at a time? rbind the data frames from the dates together
# if (unit == "zcta") {
#   unit_cross = unit_cross %>% rename(GEOID = GEOID10)
# }
# avg_unit_smokePM <- smokePM %>% 
#   pull(date) %>% 
#   unique %>% 
#   purrr::map(function(i){
#     print(i)
#     # filter to the date of interest
#     smokePM %>% 
#       filter(date == i) %>%
#       # add on all the tract/county unit IDs that are among the grid cells with smoke 
#       left_join(unit_cross %>% select(grid_id_10km, GEOID),
#                 by = "grid_id_10km") %>% 
#       filter(!is.na(GEOID)) %>% # drop grid cells that don't match to a unit
#       # full set of unit-days with smoke 
#       select(GEOID, date) %>% 
#       unique %>%
#       # join in all grid-cells for each unit
#       left_join(unit_cross, by = "GEOID") %>% 
#       # join in population and smoke PM predictions
#       left_join(pop %>% select(grid_id_10km = ID, grid_pop_per_m2 = mean),
#                 by = "grid_id_10km") %>% 
#       left_join(smokePM %>% filter(date == i), 
#                 by = c("grid_id_10km", "date")) %>%
#       # fill missing smoke PM values with zero
#       replace_na(list(smokePM_pred = 0)) %>%
#       mutate(area = unclass(area),
#              pop = grid_pop_per_m2*area) %>%
#       # for each unit-date calculate pop-weighted avg 
#       group_by(GEOID, date) %>%
#       summarise(smokePM_pred = weighted.mean(smokePM_pred, pop),
#                 .groups = "drop") 
#   }) %>% 
#   list_rbind
# if (unit == "zcta") {
#   avg_unit_smokePM = avg_unit_smokePM %>% rename(GEOID10 = GEOID)
# }


