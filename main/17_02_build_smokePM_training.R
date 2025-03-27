#source("scripts/setup/00_01_load_packages.R")
#source("scripts/setup/00_02_load_functions.R")
source("./setup/00_03_load_paths.R")
#source("scripts/setup/00_04_load_settings.R")
library(tidyverse)
library(sf)
library(data.table)
#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Makes smoke PM2.5 training dataset.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set months for sample to span
start_month = "2006-01" # "2005-08"
end_month = "2023-06" # format(today() - months(2), "%Y-%m")
#-#-----------------------------------------------------------------------------


year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

# station locations and crosswalk to 10km grid
epa_ll <- read_sf(file.path(path_data_sherlock, "EPA", "EPA_station_locations_auto")) %>% 
  # stations with NA for grid cell are in HI or AK
  rename(grid_id_10km = grid_10km)
epa_grid_cells <- epa_ll$grid_id_10km %>% unique()

# smoke ----
# results in the 10km grid ids and dates that are smoke days
smoke_missing_dates = readRDS(file.path(path_data_sherlock, "smoke", "smoke_dates", "smoke_dates_not_online.rds"))
smoke_missing_dates = ymd(smoke_missing_dates)


smoke_days <- year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out_file = file.path(path_data_sherlock, "3_intermediate", "filled_smoke", 
                       sprintf("filled_smoke_days_%s_%s.rds", y, m))
  if (file.exists(out_file)) {
    out = readRDS(out_file)
  } else {
    out = NULL
  }
  return(out)
}) %>% 
  filter(grid_id_10km %in% epa_grid_cells)
# everything not in the missing list and not in smoke_days is a zero

# smoke PM----
smokePM <- year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out = readRDS(file.path(path_data_sherlock, "3_intermediate", "station_smokePM_auto", sprintf("station_smokePM_%s_%s.rds", y, m))) %>% 
    filter(smoke_day == 1 & !is.na(pm25) & !smoke_missing_date, lubridate::year(date) == as.integer(y))
  return(out)
})

# cross sectional variables ----
# elevation
elev <- read.csv(file.path(path_data_sherlock, "2_from_EE", "elevation_avg_sd_10km_grid.csv")) %>% 
  filter(ID %in% epa_grid_cells) %>%
  transmute(grid_id_10km = ID,
            elevation_stdDev = stdDev_stdDev,
            elevation_mean = mean)

# nlcd
nlcd <- read.csv(file.path(path_data_sherlock, "2_from_EE", "NLCD_areas_10km_grid.csv")) %>% 
  filter(ID %in% epa_grid_cells) %>%    
  select(ID, groups) %>%
  mutate(groups = gsub("\\[|\\]", "", groups), # get rid of outer brackets
         groups = strsplit(groups, "\\}, \\{")) %>% # split on commas between brackets
  unnest(groups, keep_empty = T) %>% # groups is now a list that we want to unnest (i.e. lengthen)
  mutate(groups = gsub("\\{|\\}", "", groups)) %>%  # drop the extra brackets left behind
  separate(groups, into = c("landcover", "area"), sep = ",") %>% # split in commas to get land cover class and area
  mutate(landcover = trimws(gsub("landcover=", "", landcover, fixed = T)), # drop "landcover"
         area = trimws(gsub("sum=", "", area, fixed = T)) %>% as.numeric, # drop "sum"
         landcover = recode(landcover, # recode the landcover variables to their classes
                            "1.0" = "water",
                            "2.0" = "developed",
                            "3.0" = "barren",
                            "4.0" = "forest",
                            "5.0" = "shrubland",
                            "7.0" = "herbaceous",
                            "8.0" = "cultivated",
                            "9.0" = "wetlands")) %>%
  pivot_wider(names_from = landcover, values_from = area, # make it wider, one row for each grid cell, filling missings with 0s because that land class wasn't in the grid cell
              values_fill = 0) %>%
  mutate(total = water + developed + barren + forest + shrubland + herbaceous + cultivated + wetlands) %>% # calculate total area for the grid cell
  mutate(across(!total & !ID, ~.x/total)) %>%   # calculate percentages in each landcover class
  rename(grid_id_10km = ID)

# add latitude and longitude
cell_cent <- st_read(file.path(path_data_sherlock, "1_grids", "grid_10km_wgs84")) %>% 
  filter(ID %in% epa_grid_cells) %>%
  st_centroid %>%
  {cbind(., 
         st_coordinates(.))} %>% 
  st_drop_geometry() %>% 
  rename(lat = Y, 
         lon = X)

# add cross sectional info to epa_grid_cells
epa_grid_cells <- cell_cent %>% select(grid_id_10km = ID, lat, lon) %>% 
  full_join(elev, by = "grid_id_10km") %>% 
  full_join(nlcd %>% select(-total), by = "grid_id_10km") 

# time varying ----

# AOT ----
aot_anom = year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  x = readRDS(file.path(path_data_sherlock, "3_intermediate", "aot_anom_auto", sprintf("aot_anom_%s_%s.rds", y, m))) %>% 
    filter(grid_id_10km %in% epa_grid_cells$grid_id_10km, lubridate::year(date) == as.integer(y))
  return(x)
}) %>%
  arrange(grid_id_10km, date) %>% 
  group_by(grid_id_10km) %>% 
  mutate(aot_anom_lag1 = lag(aot_anom, 1),
         aot_anom_lag2 = lag(aot_anom, 2),
         aot_anom_lag3 = lag(aot_anom, 3)) %>% 
  ungroup()


#ERA5

era5_combined_grids<- c(year_months,
                        seq.Date(ymd(paste0(tail(year_months, 1), "-01")),
                                 by = "1 month", length.out = 2)[2] %>% 
                          format("%Y-%m")) %>% 
  map_dfr(function(year_month) { 
    y = substr(year_month, 1, 4)
    m = substr(year_month, 6, 7)
    x = readRDS(file.path(path_data_sherlock, "ERA5_variables", "automated", "joined_grids", sprintf("era5_joined_grid_%s_%s", y, m))) %>% 
      filter(id_grid %in% epa_grid_cells$grid_id_10km)
    return(x)
  })


#change it to data.table format to filter ou NA's
setDT(era5_combined_grids)
era5_combined_grids <- era5_combined_grids[, lapply(.SD, function(x) if (all(is.na(x))) NA else x[which.max(!is.na(x))]), 
                                           by = .(id_grid, date)]
# lag precipitation since its 1 day off (first make sure its ordered by date)
setorder(era5_combined_grids, date)
era5_combined_grids <- era5_combined_grids[, total_precipitation_daily_total := shift(total_precipitation_daily_total, 1, NA, "lead"),
                                           by = .(id_grid)]

setDF(era5_combined_grids) #change it back to data frame


# filled fire ----
fire_dates_not_online = readRDS(file.path(path_data_sherlock, "fire", "fire_dates_auto", "fire_dates_not_online.rds"))
fire_dates_clusters_too_small = readRDS(file.path(path_data_sherlock, "fire", "fire_dates_auto", "fire_dates_clusters_too_small.rds"))
filled_fire = year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out_file = file.path(path_data_sherlock, "3_intermediate", "filled_fire_auto", sprintf("filled_distance_to_fire_cluster_%s_%s.rds", y, m))
  if (file.exists(out_file)) {
    out = readRDS(out_file) %>% 
      filter(id_grid %in% epa_grid_cells$grid_id_10km) %>% 
      select(id_grid, date, km_dist, area, num_points)
  } else {
    out = NULL
  }
  return(out)
})

fire_dist = year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out = readRDS(file.path(path_data_sherlock, "distance_to_fire_cluster_auto", sprintf("grid_distance_to_fire_cluster_%s_%s.rds", y, m))) %>% 
    filter(id_grid %in% epa_grid_cells$grid_id_10km) %>% 
    select(id_grid, date, km_dist, area, num_points)
  return(out)
}) %>% 
  left_join(filled_fire, by = c("id_grid", "date")) %>% 
  mutate(km_dist = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), km_dist.y, km_dist.x), 
         area = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), area.y, area.x), 
         num_points = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), num_points.y, num_points.x)) %>%
  select(id_grid, date, km_dist, area, num_points)

rm(filled_fire)



aod_pred <- list.files(file.path(path_output_sherlock, sprintf("version%s", model_version), "anomAOD", "revisions", "predictions", "10km_smoke_days"),
                       full.names = TRUE, pattern = "rds") %>%
  purrr::map_dfr(function(x){readRDS(x) %>% filter(grid_id_10km %in% epa_grid_cells$grid_id_10km)})


# combine data sets ----
pred_data <- smokePM %>% 
  ungroup() %>%
  select(id, date, grid_id_10km, month, smokePM) %>%
  left_join(epa_grid_cells, 
            by = c("grid_id_10km")) %>%
  left_join(aot_anom, 
            by = c("grid_id_10km", "date")) %>% 
  left_join(aod_pred %>% mutate(across(.fns = unname)), 
            by = c("grid_id_10km", "date")) %>% 
  # left_join(aod_missing %>% select(grid_id_10km, AODmissing, date), 
  #           by = c("grid_id_10km", "date")) %>% 
  left_join(fire_dist %>% select(grid_id_10km = id_grid, 
                                 date, 
                                 fire_dist_km = km_dist, 
                                 closest_fire_area = area, 
                                 closest_fire_num_points = num_points), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(era5_combined_grids %>% select(grid_id_10km = id_grid, date, 
                                           pbl_max = `boundary_layer_height_daily_maximum`,
                                           pbl_mean = `boundary_layer_height_daily_mean`,
                                           pbl_min = `boundary_layer_height_daily_minimum`,
                                           sea_level_pressure = `mean_sea_level_pressure_daily_mean`,
                                           wind_u = `10m_u_component_of_wind_daily_mean`,
                                           wind_v = `10m_v_component_of_wind_daily_mean`,
                                           dewpoint_temp_2m = `2m_dewpoint_temperature_daily_mean`,
                                           temp_2m = `2m_temperature_daily_mean`,
                                           surface_pressure = `surface_pressure_daily_mean`,
                                           precip = `total_precipitation_daily_total`), 
            by = c("grid_id_10km", "date"))


# partition according to 5 spatial folds based on merra2 grid cells
grid_merra = raster(file.path(path_data_sherlock, "1_grids", "merra_folds.nc"))
grid_10km = st_read(file.path(path_data_sherlock, "1_grids", "grid_10km_wgs84"))
epa_grid_cells$cell_merra <- raster::cellFromXY(grid_merra, 
                                                grid_10km %>% filter(ID %in% epa_grid_cells$grid_id_10km) %>% st_centroid() %>% st_coordinates)
epa_grid_cells = epa_grid_cells %>% mutate(fold = grid_merra[cell_merra])

# add month and fold information 
smokePM_training = pred_data %>% 
  mutate(month = as.factor(month)) %>% 
  left_join(epa_grid_cells %>% select(grid_id_10km, fold), by = "grid_id_10km") 

# station_id by fold
stn_fold <- smokePM_training %>% dplyr::select(id, fold) %>% unique

# load the smokePM interpolations 
interp_all = expand.grid(x = c(1:5), 
            y = c(1:5)) %>% 
  filter(x >= y) %>% 
  pmap(function(x, y){
    j = c(x, y)
    readRDS(file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM_interpolation", "revisions", "best_interp",
                      paste0("interp_drop", paste0(j, collapse = ""), "_200601_202306.rds"))) %>% 
      mutate(drop_fold = paste0(unique(j), collapse = ""))
  }) %>% list_rbind

# combine interpolated values to have out of sample interpolations for fold_i (for prediction), 
# and out of sample for fold_i and 1:5 (for training). 
smokePM_interp = map(c(1:5),
    function(fold_i){
      interp_all %>% 
        filter(grepl(fold_i, drop_fold)) %>% 
        left_join(stn_fold) %>% 
        # fold_i has predictions in other places, but we don't want those
        filter(!(drop_fold != fold_i & fold == fold_i)) %>% 
        dplyr::select(id, fold, date, interp) %>% 
        rename(!!paste0("fold", fold_i, "_interp") := interp) %>%
        return
    }) %>% reduce(full_join) %>% 
# fold99 values have out of sample predictions for each fold for training the final (non-CV) model
  mutate(fold99_interp = case_when(fold == 1 ~ fold1_interp, 
                                   fold == 2 ~ fold2_interp, 
                                   fold == 3 ~ fold3_interp, 
                                   fold == 4 ~ fold4_interp, 
                                   fold == 5 ~ fold5_interp))
# merge interpolated values onto the full dataset 
smokePM_training %<>% left_join(smokePM_interp)

# save final data set, ensuring it doesn't have any missing smoke dates 
smokePM_training %>% 
  distinct %>% 
  filter(!(date %in% smoke_missing_dates)) %>% 
  drop_na(smokePM) %>% 
  saveRDS(file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM", "revisions", "smokePM_CV_training.rds"))
