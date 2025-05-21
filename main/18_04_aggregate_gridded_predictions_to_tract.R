# source("scripts/setup/00_01_load_packages.R")
# source("scripts/setup/00_02_load_functions.R")
source("/scratch/users/mmarti04/smokePM-prediction/scripts/setup/00_03_load_paths.R")
#source("scripts/setup/00_04_load_settings.R")
library(tidyverse)
library(doParallel)
library(purrr)
library(sf)
# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Aggregates 10 km grid smokePM predictions to census tract level.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set date range to aggregate
start_date = "20060101" # "20060101"
end_date = "20241231" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

unit <- "tract" # alternatively, "county"
# # load shapefile, plus 10km grid transformed to match the crs
# if(unit == "county"){
#   unit_sf <- counties() %>% 
#     filter(STATEFP %in% nonContig_stateFIPS == F)
# } else if(unit == "tract"){
#   unit_sf <- states() %>% 
#     filter(STATEFP %in% nonContig_stateFIPS == F) %>% 
#     pull(STATEFP) %>% 
#     map_dfr(function(x){
#       tracts(x, year = 2019) %>% select(STATEFP, GEOID)
#     })
# } else{
#   stop("only allowed units are \"tract\" or \"county\"")
# }
# 
# # read in the grid
# grid_10km <- st_read(file.path(path_final, "10km_grid", "10km_grid_wgs84", "10km_grid_wgs84.shp")) %>% 
#   st_transform(st_crs(unit_sf))
# 
# # make a crosswalk with intersection area with grid cells
# unit_cross = st_intersection(unit_sf,
#                              grid_10km) %>% 
#   select(GEOID, grid_id_10km = ID) %>% 
#   {cbind(st_drop_geometry(.), 
#          area = st_area(.))} 
# 
# # save the crosswalk since it takes a while to make 
# saveRDS(unit_cross, file.path(path_data, "1_grids", sprintf("%s_grid_area_crosswalk.rds", unit)))

#unit_cross <- readRDS(file.path(path_data_sherlock, "1_grids", paste0(unit, "_grid_area_crosswalk.rds")))

# smoke PM predictions 
smokePM <- readRDS((file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM",  "revisions", "predictions_marissa", sprintf("smokePM_predictions_10km_%s_%s.rds", start_date, end_date))))

# code negative values to 0
#smokePM <- smokePM %>% mutate(smokePM_pred = pmax(0, smokePM_pred))

# population by grid cell
# pop <- list.files(file.path(path_data_sherlock, "2_from_EE", "populationDensity_10km_subgrid"),
#                   full.names = T) %>% purrr::map_dfr(read.csv)
consistent_tract_aggregated_list<- list()

years <- 2006:2024

for (year_i in years) {#year_i = 2006
  
  # Use 2023 crosswalk for 2024 to ensure consistency
  crosswalk_year <- if (year_i == 2024) 2023 else year_i #we use 2023 pop weights for 2024
  
  print(crosswalk_year)
  
  # Load population crosswalks
  unit_cross <- readRDS(file.path(
    path_data_sherlock, "1_grids", "tracts_crosswalks",
    sprintf("consistent_tracts_grid_pop_crosswalk_%s.rds", crosswalk_year)
  ))
  
  # Load smoke PM predictions for the given year 
  smokePM_year <- smokePM %>% 
    filter(year(date) == year_i)
  

# Unique dates to iterate over
#unique_years <- unique(year(smokePM$date))

# avg_unit_smokePM <- map_dfr(unique_years, function(i) {
#   message("Processing year: ", i)  # Print current year
  
 # smoke_year <- smokePM %>% filter(year(date) == i)
#  pop_year <- pop %>% filter(year == i)
  
  avg_unit_smokePM <-   smokePM_year %>%
    left_join(unit_cross %>% select(grid_id_10km, GEOID), by = "grid_id_10km") %>%
    filter(!is.na(GEOID)) %>%
    select(GEOID, date) %>%
    distinct() %>%
    left_join(unit_cross, join_by("GEOID")) %>%
    mutate(year = year(date)) %>%
    # left_join(pop_year %>% select(grid_id_10km = ID, grid_pop = pop),
    #           by = c("grid_id_10km", "year")) %>%
    left_join(smokePM_year, by = c("grid_id_10km", "date")) %>%
    replace_na(list(smokePM_pred = 0)) %>%
    group_by(GEOID, date) %>%
    mutate(pop = ifelse(rep(all(pop == 0), n()), 1, pop)) %>%
    summarise(smokePM_pred = weighted.mean(smokePM_pred, pop), .groups = "drop")
#})
  
  # Save this year's result to the list
  consistent_tract_aggregated_list[[as.character(year_i)]] <- avg_unit_smokePM  
  
  print(paste0("processed year ", year_i))
 
}

# Combine all years into a single data frame
consistent_tract_aggregated <- bind_rows(consistent_tract_aggregated_list)

saveRDS(consistent_tract_aggregated, 
        file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM",  "revisions", "aggregated",
                  sprintf("consistent_tract_smokePM_predictions_yearly_weighted_%s-%s.rds",  start_date, end_date)))

# test<- readRDS(file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM", "revisions", "aggregated", 
#                          sprintf("%s_smokePM_predictions_%s-%s.rds", unit, start_date, end_date)))
# 
#--------------------------------------------------------------
# i <- unique_years[1]
# 
# smoke_day <- smokePM %>% filter(lubridate::year(date) == i)
# 
# df <- smoke_day %>%
#   left_join(unit_cross %>% select(grid_id_10km, GEOID), by = "grid_id_10km") %>%
#   filter(!is.na(GEOID)) %>%
#   select(GEOID, date) %>%
#   distinct() %>%
#   left_join(unit_cross, by = "GEOID") %>%
#   mutate(year = year(date)) %>%
#   left_join(pop %>% select(grid_id_10km = ID, grid_pop = total_pop, year)) %>%
#   left_join(smoke_day, by = c("grid_id_10km", "date")) %>%
#   replace_na(list(smokePM_pred = 0)) %>%
#   group_by(GEOID, date) %>%
#   mutate(grid_pop = ifelse(rep(all(grid_pop == 0), n()), 1, grid_pop)) %>%
#   summarise(smokePM_pred = weighted.mean(smokePM_pred, grid_pop), .groups = "drop")
# 
# head(df)
# 
