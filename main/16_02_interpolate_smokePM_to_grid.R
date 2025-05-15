library(tidyverse)
library(magrittr)
library(terra) 
library(raster) 
library(sf)  
library(fixest)  
library(lubridate)
source("scripts/setup/00_03_load_paths.R")

# if the folder for saving the output doesn't exist, make it 
if(!dir.exists(file.path(path_output_sherlock, "version1.1", "smokePM_interpolation", "revisions", "interpoaltions_full_model"))){
  dir.create(file.path(path_output_sherlock, "version1.1", "smokePM_interpolation", "revisions", "interpoaltions_full_model"), recursive = T)
}

start_month = "2006-01" 
end_month = "2024-12" 

# set crs for interpolation 
interp_crs = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"

# load full station PM2.5 panel 
year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")
station_full_panel <- year_months %>% map(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out = readRDS(file.path(path_data_sherlock, "3_intermediate", "station_smokePM_auto", sprintf("station_smokePM_%s_%s.rds", y, m))) %>% 
    dplyr::filter(!is.na(pm25) &  !is.na(smokePM)) # interpolate even for the smoke_missing dates, just use the filled smoke PM2.5
  return(out)
}) %>% list_rbind()
station_full_panel %<>% ungroup


# load 10km grid cell cetroids
grid_cent <- readRDS(file.path(path_data_sherlock, "1_grids", "10km_cell_centroids.rds"))

# load epa locations
epa_ll <- st_read(file.path(path_data_sherlock, "EPA", "epa_station_locations_auto"))

# how the folds are constructed from 16_02 
grid_merra = raster(file.path(path_data_sherlock, "1_grids", "merra_folds.nc"))
# match epa stations to the merra-determined folds via the 10km grid
grid_cent %<>% mutate(cell_merra = raster::cellFromXY(grid_merra,
                                                      cbind(x = lon, y = lat) %>% as.matrix))
epa_ll %<>% left_join(grid_cent %>% dplyr::select(grid_10km = grid_id_10km, cell_merra), 
                      by = "grid_10km") %>%
  mutate(fold = grid_merra[cell_merra])

# join station lat-lon information onto the station panel for interpolating, 
# and the centroid of the associated grid cell for extracting all in the 
# grid's original projection system so units are meters for interpolation
station_full_panel %<>% left_join(st_transform(epa_ll, crs = interp_crs) %>% 
                                    st_coordinates() %>% 
                                    as.data.frame() %>% 
                                    cbind(id = epa_ll$stn_id, 
                                          fold = epa_ll$fold) %>% 
                                    dplyr::select(id, fold, station_x = X, station_y = Y), 
                                  by = "id") 
station_full_panel %<>% filter(!is.na(station_x) & !is.na(station_y)) 

# drop variable we don't need 
station_full_panel %<>% dplyr::select(id, fold, station_x, station_y, 
                                      year, month, date, smoke_day, smokePM)

# load best interpolation params 
interp_parameters <- readRDS(file.path(path_output_sherlock, "version1.1", "smokePM_interpolation", "revisions",
                                       "interp_tune_params_drop9999_200601_202412.rds")) %>%
  magrittr::extract2("Best_Par") %>%
  as.list

# function to interpolate for each date, and save output for specified obs 
interp_over_dates = function(interp_df, # data frame with x, y, date, and smokePM
                             extract_cells, # data frame with date, id, raster cell numbers to extract 
                             interp_params){ # list of interpolation parameters
  # if you were fancier here, could cut into "equal sized pieces" based on the number of workers rather than arbitratily doing it by year
  full_join(interp_df %>% 
              nest_by(date, .key = "interp_input_i"), 
            extract_cells %>% 
              nest_by(date, .key = "extract_loc_i"),
            by = "date") %>% 
    filter(!is.null(extract_loc_i) & !is.null(interp_input_i)) %>% 
    pmap(function(date, interp_input_i, extract_loc_i){
      data.frame(interp = exec(gstat::idw,
                               !!!c(list(formula = smokePM~1,  
                                         locations = st_as_sf(interp_input_i, 
                                                              coords = c("station_x", "station_y"), remove = F, 
                                                              crs = interp_crs), 
                                         newdata = st_as_sf(extract_loc_i, 
                                                            coords = c("grid_x", "grid_y"), remove = T, 
                                                            crs = interp_crs), 
                                         debug.level = 0), 
                                    interp_params)) %>% 
                   pull(var1.pred), 
                 date = date,
                 id = extract_loc_i$id) %>%
        return()
    }) %>% list_rbind %>% 
    return()
}

# loop over months, load smoke days, interpolate for each day, grab values for all smoke days, save interpolated values
print("starting interpolation")

map_chr(year_months, function(y_m){
  print(y_m)
  y_i = substr(y_m, 1, 4) %>% as.numeric
  m_i = substr(y_m, 6, 7) %>% as.numeric
  
  out_file <- file.path(path_output_sherlock, "version1.1", "smokePM_interpolation", "revisions", 
                        paste0("smokePM_interpolated_", gsub("-", "_", y_m), ".rds"))
  
  smoke_day_cells <- readRDS(file.path(path_data_sherlock, "3_intermediate", "filled_smoke", paste0("filled_smoke_days_", gsub("-", "_", y_m), ".rds"))) %>% 
    left_join(grid_cent %>% dplyr::select(grid_id_10km, grid_x = COORDX, grid_y = COORDY), by = "grid_id_10km")
  
  print(paste0(nrow(smoke_day_cells), " smoke day-grid cells"))
  
  interp_over_dates(station_full_panel %>% filter(year == y_i & month == m_i), # data frame with x, y, date_col, and value_col
                    smoke_day_cells %>% rename(id = grid_id_10km), 
                    list(idp = interp_parameters$pow,
                         maxdist = interp_parameters$dist,
                         nmax= interp_parameters$max_point)) %>%
    rename(grid_id_10km = id) %>%
    saveRDS(out_file)
  return(out_file)
}) -> out

print(out)