library(lubridate)
library(sf)
library(dplyr)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li, Marissa Childs - Updated by Mariana Martins Aug 2024 to follow the format of the API data
# Gets EPA monitor and station locations.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set dates to use as available inputs
start_input = "20060101" # "19990101"
end_input = "20231231" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

year_months = format(seq.Date(ymd(start_input), ymd(end_input), by = "month"), "%Y-%m")

# Load the 10 km grid 
grid <- read_sf(file.path(path_data, "1_grids", "grid_10km_wgs84"))

monitor_ll = vector("list", length(year_months))
monitor_obs = vector("list", length(year_months))
p = txtProgressBar(min = 0, max = length(year_months), style = 3)
for (i in 1:length(year_months)) {
  year_month = year_months[i]
  year = as.integer(substr(year_month, 1, 4))
  month = as.integer(substr(year_month, 6, 7))
  
  
  # Load EPA data
  pm_data = readRDS(file.path(path_data, "EPA", "monitor_level_pm25_data", 
                              sprintf("epa_monitor_level_pm25_data_%s_%s.rds", year, month))) %>% 
    filter(mdy(date) %within% (ymd(start_input) %--% ymd(end_input)))
  
  # Get monitor locations
  epa_ll = pm_data %>% 
    dplyr::select(station_id, monitor_id, lon, lat, 
           state, state_code, county, county_code, cbsa, cbsa_code) %>% 
    unique() %>% 
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326, remove = FALSE)
  
  monitor_ll[[i]] = epa_ll
  
  # Get count of observations at each station per month
  epa_count = pm_data %>% dplyr::count(station_id, lon, lat, year, month)
  
  monitor_obs[[i]] = epa_count
  
  setTxtProgressBar(p, i)
}
monitor_ll = unique(bind_rows(monitor_ll))

# Attach 10 km grid IDs to monitor locations
monitor_to_grid = st_intersects(monitor_ll, grid)
monitor_ll$grid_10km = grid$ID[as.numeric(monitor_to_grid)]

# Save shapefile of monitor locations
write_sf(monitor_ll %>% dplyr::select(stn_id = station_id, mon_id = monitor_id, lon, lat, 
                               state, stat_cd = state_code, 
                               county, cnty_cd = county_code, 
                               cbsa, cbsa_cd = cbsa_code, 
                               grid_10km), 
         file.path(path_data, "EPA",  "epa_monitor_locations"), 
         "epa_monitor_locations", 
         driver = "ESRI Shapefile")

# Aggregate to the station level
monitor_obs = bind_rows(monitor_obs) %>% 
  group_by(station_id, lon, lat) %>% 
  dplyr::summarize(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(station_id) %>% 
  slice_max(n) %>% 
  ungroup()

# Combine with other columns
station_ll = monitor_obs %>% 
  dplyr::select(-n) %>% 
  left_join(monitor_ll %>% 
              st_drop_geometry() %>% 
              distinct(station_id, state, state_code, 
                       county, county_code, cbsa, cbsa_code)) %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326, remove = FALSE)

# Attach 10 km grid IDs to station locations
station_to_grid = st_intersects(station_ll, grid)
station_ll$grid_10km = grid$ID[as.numeric(station_to_grid)]

# Save shapefile of station locations
write_sf(station_ll %>% dplyr::select(stn_id = station_id, lon, lat, 
                               state, stat_cd = state_code, 
                               county, cnty_cd = county_code, 
                               cbsa, cbsa_cd = cbsa_code, 
                               grid_10km), 
         file.path(path_data, "EPA",  "epa_station_locations", 
         "epa_station_locations"), 
         driver = "ESRI Shapefile")
