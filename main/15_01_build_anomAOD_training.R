source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
#source("scripts/setup/00_04_load_settings.R")
#path_data = path_data_sherlock
#path_output = path_output_sherlock
# ------------------------------------------------------------------------------
# Written by: Marissa Childs
#updated by: Mariana Martins (April 2024)
# Makes AOD training dataset.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set months for sample to span
start_month = "2006-01" # "2005-08"
end_month = "2023-06" # format(today() - months(2), "%Y-%m")
#-#-----------------------------------------------------------------------------

n_fold <- 4

year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

# crosswalk between grids -----
crosswalk <- readRDS(file.path(path_data, "1_grids", "grid_crosswalk_1km_10km.rds"))

# subsample to get 1000 locations instead of 5000, ensuring they are 5km away from each other  ----
pop_1km <- purrr::map_dfr(list.files(file.path(path_data, "2_from_EE", "populationDensity_1km_subgrid"), full.names = T), 
                          function(x){
                            read.csv(x)
                          })

grid_orig <- st_read(file.path(path_data, "1_grids", "1km_aod_grid_wgs84_training"))
grid_dists <- st_distance(st_centroid(grid_orig))
grid_5km_dist <- (units::drop_units(grid_dists) < 5000)

pop_remain <- pop_1km %>% filter(grid_id %in% grid_orig$grid_id)

sample_ids <- c()
set.seed(10001)
while(length(sample_ids) < 1000 & nrow(pop_remain) > 0){
  print(nrow(pop_remain))
  # sample a point
  new_samp <- sample(pop_remain$grid_id, 1, prob = pop_remain$mean)
  # add it to the sampled list 
  sample_ids %<>% c(new_samp)
  
  # identify cells within 5km 
  nearby_cell_ids <- which(grid_orig$grid_id == new_samp) %>% # identify the index of new_samp
    {magrittr::extract(grid_5km_dist, .,)} %>% # extract that row from the distance matrix 
    which %>% # identify which in indices are < 5km away 
    {magrittr::extract(grid_orig$grid_id, .)}# identify the associated grid_ids
  # exclude them from future sampling
  pop_remain %<>% filter(!(grid_id %in% nearby_cell_ids))
}

rm(pop_remain, nearby_cell_ids, new_samp, 
   grid_orig, grid_dists, grid_5km_dist)

# smoke ----
# results in the 10km grid ids and dates that are smoke days
smoke_missing_dates = readRDS(file.path(path_data, "smoke", "smoke_dates", "smoke_dates_not_online.rds"))
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
})
# everything not in the missing list and not in smoke_days is a zero

# aod data ----
aod_null_value <- -999999

aod <- year_months %>% 
  map_dfr(function(year_month) {
    y = substr(year_month, 1, 4)
    m = substr(year_month, 6, 7)
    out = read_csv(file.path(path_data, "2_from_EE", "maiac_AOD_training", 
                             sprintf("aod_1km_grid_train_%s_%s.csv", y, m))) %>% 
      filter(grid_id %in% sample_ids)%>%  
      mutate(year = substr(start_date, 1,4), #filtering for present year only
             month = substr(start_date, 5,6)) %>%
      filter(year == y,
             month == m) %>%
      select(-year, -month)
    
    return(out)
  }) %>% 
  transmute(grid_id_1km = grid_id, 
            date = as.Date(as.character(start_date), format = "%Y%m%d"),
            aod = median) %>% 
  filter(date >= ymd(sprintf("%s-01", start_month))) %>%
  mutate(month = lubridate::month(date), 
         year = lubridate::year(date)) %>% 
  left_join(crosswalk, by = "grid_id_1km") %>%
  left_join(smoke_days, 
            by = c("date", "grid_id_10km")) %>%
  replace_na(list(smoke_day = 0)) %>% 
  filter(aod != aod_null_value) %>%
  {left_join(., 
             nonsmoke_medians(filter(., !(date %in% smoke_missing_dates)), 
                              aod, smoke_day, grid_id_1km, month, year), 
             by = c("grid_id_1km", "month", "year"))} %>% 
  mutate(aod_anom = aod - aod_med_3yr)

# use aod, smoke, and crosswalk between grids to make a panel of the training 1km grid cell-days with smoke----
aod_cells <- data.frame(grid_id_1km = sample_ids) %>% 
  left_join(crosswalk, by = "grid_id_1km")

# assumption was 5k grid cells X 365 days x 15 years x 5% smoke days x 50% missing AOD ~ 700K obs
# we ended up with ~1.3 M obs, partly because there were ~2M smoke day obs

# elevation
elev <- list.files(file.path(path_data, "2_from_EE", "elevation_1km_subgrid"), full.names = T) %>% 
  map_dfr(function(x) read.csv(x) %>% filter(grid_id %in% aod_cells$grid_id_1km)) %>% 
  rename_with(function(x) {paste0("elevation_", x)}, .cols = !grid_id) %>% 
  rename(elevation_stdDev = elevation_stdDev_stdDev,
         grid_id_1km = grid_id)

# nlcd
nlcd <- list.files(file.path(path_data, "2_from_EE", "NLCD_1km_subgrid"), full.names = T) %>% 
  map_dfr(function(x) read.csv(x) %>% filter(grid_id %in% aod_cells$grid_id_1km)) %>%  #  
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
  mutate(across(!total & !grid_id, ~.x/total)) %>%   # calculate percentages in each landcover class
  rename(grid_id_1km = grid_id)

# add cross sectional info to aod_cells
aod_cells %<>% full_join(elev, by = "grid_id_1km") %>% 
  full_join(nlcd %>% select(-total), by = "grid_id_1km")
rm(elev, nlcd)

# add latitude and longitude
cell_cent <- st_read(file.path(path_data, "1_grids", "1km_aod_grid_wgs84_training")) %>% 
  filter(grid_id %in% aod_cells$grid_id_1km) %>%
  st_centroid %>%
  {cbind(., 
         st_coordinates(.))} %>% 
  st_drop_geometry() %>% 
  rename(lat = Y, 
         lon = X)

aod_cells %<>% left_join(cell_cent, by = c("grid_id_1km" = "grid_id"))

# fire distance and cluster size ----
fire_dist = year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out = readRDS(file.path(path_data, "distance_to_fire_cluster_auto", sprintf("grid_distance_to_fire_cluster_%s_%s.rds", y, m))) %>% 
    filter(id_grid %in% aod_cells$grid_id_10km) %>% 
    select(id_grid, date, km_dist, area, num_points) %>% 
    mutate(date= ymd(date))
  return(out)
})

# ERA5 weather ----
# era5_global_vars = c("boundary_layer_height_daily_maximum_of_1-hourly", 
#                      "boundary_layer_height_daily_mean_of_1-hourly", 
#                      "boundary_layer_height_daily_minimum_of_1-hourly", 
#                      "mean_sea_level_pressure_daily_mean_of_1-hourly")
# era5_global = expand.grid(year_month = year_months, variable = era5_global_vars)
# era5_global <- 1:nrow(era5_global) %>% map_dfr(function(row) {
#     year_month = era5_global[row, "year_month"]
#     y = substr(year_month, 1, 4)
#     m = substr(year_month, 6, 7)
# 
#     v = era5_global[row, "variable"]
#     if (year_month == year_months[1]) print(paste(v))
#     new_name = v
#     s = gsub("_of_1-hourly", "", v)
#     v = gsub("_daily_.*", "", v)
#     old_name = v
#     s = gsub(paste0("^", v), "", s)
# 
#     out = readRDS(file.path(path_data, "ERA5_variables", "Global", "USA", "10km_grid", "UTC-0600", sprintf("grid_%s_%s_%s_%s.rds", v, s, y, m))) %>% 
#       filter(id_grid %in% aod_cells$grid_id_10km) %>% 
#       rename(!!new_name := !!old_name)
# 
#     return(out)
#   })
# 
# era5_land_vars = c("10m_u_component_of_wind_daily_mean_of_1-hourly", 
#                    "10m_v_component_of_wind_daily_mean_of_1-hourly", 
#                    "2m_dewpoint_temperature_daily_mean_of_1-hourly", 
#                    "2m_temperature_daily_mean_of_1-hourly", 
#                    "surface_pressure_daily_mean_of_1-hourly", 
#                    "total_precipitation_daily_maximum_of_1-hourly")
# era5_land = expand.grid(year_month = year_months, variable = era5_land_vars)
# era5_land <- 1:nrow(era5_land) %>% map_dfr(function(row) {
#     year_month = era5_land[row, "year_month"]
#     y = substr(year_month, 1, 4)
#     m = substr(year_month, 6, 7)
# 
#     v = era5_land[row, "variable"]
#     if (year_month == year_months[1]) print(paste(v))
#     new_name = v
#     s = gsub("_of_1-hourly", "", v)
#     v = gsub("_daily_.*", "", v)
#     old_name = v
#     s = gsub(paste0("^", v), "", s)
# 
#     out = readRDS(file.path(path_data, "ERA5_variables", "Land", "USA", "10km_grid", ifelse(v == "total_precipitation", "UTC-0600", "UTC+0000"), sprintf("grid_%s_%s_%s_%s.rds", v, s, y, m))) %>% 
#       filter(id_grid %in% aod_cells$grid_id_10km) %>% 
#       rename(!!new_name := !!old_name)
# 
#     return(out)
#   })
# expand the range of year months by 1 since precip is on the following date 
era5_combined_grids<- c(year_months,
                        seq.Date(ymd(paste0(tail(year_months, 1), "-01")),
                                 by = "1 month", length.out = 2)[2] %>% 
                          format("%Y-%m")) %>% 
  map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  x = readRDS(file.path(path_data, "ERA5_variables", "automated", "joined_grids", sprintf("era5_joined_grid_%s_%s", y, m))) %>% 
    filter(id_grid %in% aod_cells$grid_id_10km)
  return(x)
})

# era5_combined_grids<- era5_combined_grids %>%
#   group_by(id_grid, date) %>%
#   summarize_all(~ ifelse(all(is.na(.)), NA, first(na.omit(.)))) %>% 
#   ungroup()

# era5_combined_grids <- era5_combined_grids %>%
#   group_by(id_grid, date) %>%
#   summarise(across(everything(), ~ ifelse(all(is.na(.)), NA, first(na.omit(.)))), .groups = "drop")

#change it to data.table format to filter ou NA's
setDT(era5_combined_grids)
era5_combined_grids <- era5_combined_grids[, lapply(.SD, function(x) if (all(is.na(x))) NA else x[which.max(!is.na(x))]), 
                                           by = .(id_grid, date)]
# lag precipitation since its 1 day off (first make sure its ordered by date)
setorder(era5_combined_grids, date)
era5_combined_grids <- era5_combined_grids[, total_precipitation_daily_total := shift(total_precipitation_daily_total, 1, NA, "lead"),
                                           by = .(id_grid)]
                                           
setDF(era5_combined_grids) #change it back to data frame



# AOT ----
aot = year_months %>% map_dfr(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  x = readRDS(file.path(path_data, "3_intermediate", "aot_anom_auto", sprintf("aot_anom_%s_%s.rds", y, m))) %>% 
    filter(grid_id_10km %in% aod_cells$grid_id_10km)
  return(x)
}) %>%
  arrange(grid_id_10km, date) %>% 
  group_by(grid_id_10km) %>% 
  mutate(aot_anom_lag1 = lag(aot_anom, 1),
         aot_anom_lag2 = lag(aot_anom, 2),
         aot_anom_lag3 = lag(aot_anom, 3)) %>% 
  ungroup()

# combine data sets into aod panel ----


aod_smoke_panel <- aod %>%
  filter(grid_id_1km %in% sample_ids) %>% 
  filter(smoke_day == 1) %>% 
  select(grid_id_1km, grid_id_10km, date, month, aod_anom) %>% 
  left_join(aot, 
            by = c("grid_id_10km", "date")) %>% 
  left_join(fire_dist %>% select(grid_id_10km = id_grid, 
                                 date, 
                                 fire_dist_km = km_dist, 
                                 closest_fire_area = area, 
                                 closest_fire_num_points = num_points), 
            by = c("grid_id_10km", "date")) %>% 
  # left_join(era5_global %>% select(grid_id_10km = id_grid, date, 
  #                                  pbl_max = `boundary_layer_height_daily_maximum_of_1-hourly`,
  #                                  pbl_mean = `boundary_layer_height_daily_mean_of_1-hourly`,
  #                                  pbl_min = `boundary_layer_height_daily_minimum_of_1-hourly`,
  #                                  sea_level_pressure = `mean_sea_level_pressure_daily_mean_of_1-hourly`), 
  #           by = c("grid_id_10km", "date")) %>% 
  # left_join(era5_land %>% select(grid_id_10km = id_grid, date, 
  #                                wind_u = `10m_u_component_of_wind_daily_mean_of_1-hourly`,
  #                                wind_v = `10m_v_component_of_wind_daily_mean_of_1-hourly`,
  #                                dewpoint_temp_2m = `2m_dewpoint_temperature_daily_mean_of_1-hourly`,
  #                                temp_2m = `2m_temperature_daily_mean_of_1-hourly`,
  #                                surface_pressure = `surface_pressure_daily_mean_of_1-hourly`,
  #                                precip = `total_precipitation_daily_maximum_of_1-hourly`), 
  #           by = c("grid_id_10km", "date")) %>% 
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

set.seed(1)
aod_smoke_panel %<>% 
  # add splits for CV on model tuning 
  left_join(aod_cells %>% {.[sample.int(nrow(.), nrow(.), replace = F),]} %>% # reorder for randomness before assigning to folds
              mutate(fold = mod(row_number(), n_fold)), 
            by = c("grid_id_1km", "grid_id_10km"))

# we occasionally get NAs for aot_anom because there aren't any obs to make a location-month specific median
anomAOD_training = aod_smoke_panel %>% 
  filter(!is.na(aod_anom)) %>% 
  filter(!(date %in% smoke_missing_dates)) %>% 
  mutate(month = factor(month, levels = 1:12))

#run some checks
# should only get 1 fold per 1km grid id 
aod_smoke_panel %>% summarise(n_fold = n_distinct(fold), .by = grid_id_1km) %>% pull(n_fold) %>% unique
# should get 4 different folds 
aod_smoke_panel %>% pull(fold) %>% n_distinct 
# all obs should have a fold assigned (i.e. 0 rows with NA fold) 
aod_smoke_panel %>% filter(is.na(fold)) %>% nrow 


saveRDS(anomAOD_training, file.path(path_output, sprintf("version%s", model_version), "anomAOD", "anomAOD_training_auto.rds"))
