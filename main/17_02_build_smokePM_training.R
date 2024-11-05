source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

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
epa_ll <- read_sf(file.path(path_data, "EPA", "epa_station_locations")) %>% 
  # stations with NA for grid cell are in HI or AK
  rename(grid_id_10km = grid_10km)
epa_grid_cells <- epa_ll$grid_id_10km %>% unique()

# smoke ----
# results in the 10km grid ids and dates that are smoke days
smoke_missing_dates = readRDS(file.path(path_data, "smoke", "smoke_dates_not_online.rds"))
smoke_missing_dates = ymd(smoke_missing_dates)


smoke_days <- year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out_file = file.path(path_data, "3_intermediate", "filled_smoke", 
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
  out = readRDS(file.path(path_data, "3_intermediate", "station_smokePM", sprintf("station_smokePM_%s_%s.rds", y, m))) %>% 
    filter(smoke_day == 1 & !is.na(pm25) & !smoke_missing_date, lubridate::year(date) == as.integer(y))
  return(out)
})

# cross sectional variables ----
# elevation
elev <- read.csv(file.path(path_data, "2_from_EE", "elevation_avg_sd_10km_grid.csv")) %>% 
  filter(ID %in% epa_grid_cells) %>%
  transmute(grid_id_10km = ID,
            elevation_stdDev = stdDev_stdDev,
            elevation_mean = mean)

# nlcd
nlcd <- read.csv(file.path(path_data, "2_from_EE", "NLCD_areas_10km_grid.csv")) %>% 
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
cell_cent <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>% 
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
  x = readRDS(file.path(path_data, "3_intermediate", "aot_anom", sprintf("aot_anom_%s_%s.rds", y, m))) %>% 
    filter(grid_id_10km %in% epa_grid_cells$grid_id_10km, lubridate::year(date) == as.integer(y))
  return(x)
}) %>%
  arrange(grid_id_10km, date) %>% 
  group_by(grid_id_10km) %>% 
  mutate(aot_anom_lag1 = lag(aot_anom, 1),
         aot_anom_lag2 = lag(aot_anom, 2),
         aot_anom_lag3 = lag(aot_anom, 3)) %>% 
  ungroup()


# ERA5 ----
era5_global_vars = c("boundary_layer_height_daily_maximum_of_1-hourly", 
                     "boundary_layer_height_daily_mean_of_1-hourly", 
                     "boundary_layer_height_daily_minimum_of_1-hourly", 
                     "mean_sea_level_pressure_daily_mean_of_1-hourly")
era5_global = expand.grid(year_month = year_months, variable = era5_global_vars)
era5_global <- 1:nrow(era5_global) %>% map_dfr(function(row) {
  year_month = era5_global[row, "year_month"]
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  
  v = era5_global[row, "variable"]
  
  subdir = v
  if (year_month == year_months[1]) print(paste(v))
  new_name = as.character(v)
  s = gsub("_of_1-hourly", "", v)
  v = gsub("_daily_.*", "", v)
  old_name = v
  s = gsub(paste0("^", v), "", s)
  
  subsubdir = gsub(paste0(v, "_"), "", subdir)
  
  out = readRDS(file.path(path_data, "ERA5_variables", "Global", sprintf("%s", v), "USA", "10km_grid", "UTC-0600", 
                          sprintf("%s", subsubdir), sprintf("grid_%s%s_%s_%s.rds", v, s, y, m))) %>% 
    filter(id_grid %in% epa_grid_cells$grid_id_10km) %>% 
    rename(value := !!old_name) %>% 
    mutate(var = new_name)
  
  return(out)
})
era5_global %<>% pivot_wider(values_from = value, names_from = var)

era5_land_vars = c("10m_u_component_of_wind_daily_mean_of_1-hourly", 
                   "10m_v_component_of_wind_daily_mean_of_1-hourly", 
                   "2m_dewpoint_temperature_daily_mean_of_1-hourly", 
                   "2m_temperature_daily_mean_of_1-hourly", 
                   "surface_pressure_daily_mean_of_1-hourly", 
                   "total_precipitation_daily_maximum_of_1-hourly")
era5_land = expand.grid(year_month = year_months, variable = era5_land_vars, stringsAsFactors = FALSE)
era5_land <- 1:nrow(era5_land) %>% map_dfr(function(row) {
  year_month = era5_land[row, "year_month"]
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  
  v = era5_land[row, "variable"]
  subdir = v
  if (year_month == year_months[1]) print(paste(v))
  new_name = v
  s = gsub("_of_1-hourly", "", v)
  v = gsub("_daily_.*", "", v)
  old_name = v
  s = gsub(paste0("^", v), "", s)
  
  subsubdir = gsub(paste0(v, "_"), "", subdir)
  
  
  out = readRDS(file.path(path_data, "ERA5_variables", "Land", sprintf("%s", v), "USA", "10km_grid", 
                          ifelse(v == "total_precipitation", "UTC+0000", "UTC-0600"), 
                          sprintf("%s", subsubdir), sprintf("grid_%s%s_%s_%s.rds", v, s, y, m))) %>% 
    filter(id_grid %in% epa_grid_cells$grid_id_10km) %>% 
    rename(value := !!old_name) %>% 
    mutate(var = new_name)
  # rename(!!new_name := !!old_name)
  
  return(out)
})

era5_land %<>% pivot_wider(values_from = value, names_from = var)

# filled fire ----
fire_dates_not_online = readRDS(file.path(path_data, "fire", "fire_dates_not_online.rds"))
fire_dates_clusters_too_small = readRDS(file.path(path_data, "fire", "fire_dates_clusters_too_small.rds"))
filled_fire = year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out_file = file.path(path_data, "3_intermediate", "filled_fire", sprintf("filled_distance_to_fire_cluster_%s_%s.rds", y, m))
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
  out = readRDS(file.path(path_data, "distance_to_fire_cluster", sprintf("grid_distance_to_fire_cluster_%s_%s.rds", y, m))) %>% 
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

# # AOD predictions ----
# aod_pred = year_months %>% map_dfr(function(year_month) {
#     out = list.files(file.path(path_output, sprintf("version%s", model_version), "anomAOD", "predictions", "10km_smoke_days"))
#     out = out[ymd(gsub("^anomAOD_predictions_1km_|\\.rds$", "", out)) %within% (ym(start_month) %--% (ym(end_month) + months(1) - days(1)))]
#     out = out %>% 
#       map_dfr(readRDS) %>% 
#       filter(grid_id_10km %in% epa_grid_cells$grid_id_10km)
#     return(out)
#   })


aod_pred <- list.files(file.path(path_output, sprintf("version%s", model_version), "anomAOD", "predictions", "10km_smoke_days"),
                       full.names = TRUE, pattern = "rds") %>%
  purrr::map_dfr(function(x){readRDS(x) %>% filter(grid_id_10km %in% epa_grid_cells$grid_id_10km)})


# AOD percent missing ----
aod_missing = year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out = list.files(file.path(path_data, "2_from_EE", "maiac_AODmissings"), 
                   pattern = sprintf("^aod_pctMissing_10km_subgrid_[0-9]*_%s_%s.csv$", y, m), 
                   full.names = T) %>% 
    map_dfr(function(file) {
      out = read.csv(file) %>% 
        mutate(date = as.Date(as.character(start_date), format = "%Y%m%d")) %>% 
        rename(AODmissing = mean, 
               grid_id_10km = ID) %>% 
        filter(grid_id_10km %in% epa_grid_cells$grid_id_10km)
      return(out)
    })
  return(out)
})

era5_global <- era5_global %>%
  group_by(id_grid, date) %>%
  summarize_all(~ ifelse(all(is.na(.)), NA, first(na.omit(.))))

era5_land <- era5_land %>%
  group_by(id_grid, date) %>%
  summarize_all(~ ifelse(all(is.na(.)), NA, first(na.omit(.))))

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
  left_join(aod_missing %>% select(grid_id_10km, AODmissing, date), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(fire_dist %>% select(grid_id_10km = id_grid, 
                                 date, 
                                 fire_dist_km = km_dist, 
                                 closest_fire_area = area, 
                                 closest_fire_num_points = num_points), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(era5_global %>% select(grid_id_10km = id_grid, 
                                   date, 
                                   pbl_max = `boundary_layer_height_daily_maximum_of_1-hourly`,
                                   pbl_mean = `boundary_layer_height_daily_mean_of_1-hourly`,
                                   pbl_min = `boundary_layer_height_daily_minimum_of_1-hourly`,
                                   sea_level_pressure = `mean_sea_level_pressure_daily_mean_of_1-hourly`), 
            by = c("grid_id_10km", "date")) %>% 
  left_join(era5_land %>% select(grid_id_10km = id_grid, 
                                 date, 
                                 wind_u = `10m_u_component_of_wind_daily_mean_of_1-hourly`,
                                 wind_v = `10m_v_component_of_wind_daily_mean_of_1-hourly`,
                                 dewpoint_temp_2m = `2m_dewpoint_temperature_daily_mean_of_1-hourly`,
                                 temp_2m = `2m_temperature_daily_mean_of_1-hourly`,
                                 surface_pressure = `surface_pressure_daily_mean_of_1-hourly`,
                                 precip = `total_precipitation_daily_maximum_of_1-hourly`), 
            by = c("grid_id_10km", "date"))

# partition according to 5 spatial folds based on merra2 grid cells
grid_merra = raster(file.path(path_data, "1_grids", "merra_folds.nc"))
grid_10km = st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
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
    readRDS(file.path(path_output, sprintf("version%s", model_version), "smokePM_interpolation", 
                      paste0("interp_drop", paste0(j, collapse = ""), ".rds"))) %>% 
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
  saveRDS(file.path(path_output, sprintf("version%s", model_version), "smokePM", "smokePM_training.rds"))
