#-#-----------------------------------------------------------------------------
# Run the line below uncommented on local machine to copy data folder in Dropbox 
# repository from local machine to Oak
# system(sprintf("rsync -azvP %s %s@dtn.sherlock.stanford.edu:%s", path_data, SUNetID, path_data_sherlock))
#-#-----------------------------------------------------------------------------

sherlock = T
library(rlang)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(xgboost)
library(foreach)
library(doParallel)
library(data.table)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs, Jessica Li
# Gets anomalous AOD 1 km predictions and 10 km aggregations.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set date range to predict
data_start_date = "20060101"

args <- commandArgs(TRUE)
start_date = as.character(args[1]) #"20060101" 
end_date = as.character(args[2]) #"20230630" # format(today() - days(1), "%Y%m%d")
print(paste0("predicting from ", start_date, " to ", end_date))
# Set whether to overwrite preexisting files or not
overwrite = F
#-#-----------------------------------------------------------------------------

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK"))
xgb_nthread =  as.numeric(args[3])
num_par = floor(num_cores/xgb_nthread)
print(paste0("Using ", num_cores, " CPUs with ", xgb_nthread, " threads per xgb, ", num_par, " parallel clusters"))

# check if the relevant directories exist for the prediction output, and if they don't make them 
path_prediction_output <- file.path(path_output_sherlock, sprintf("version%s", model_version), "anomAOD", "revisions", "predictions")
purrr::map(c("1km", "10km"), function(res){
  outdir = file.path(path_prediction_output, paste0(res,"_smoke_days"))
  if(!dir.exists(outdir)){ 
    dir.create(outdir, recursive = T)
    print(paste0("made directory ", outdir))
  }
  return(outdir)
})

all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(substr(all_dates, 1, 7))
year_months = gsub("-", "_", year_months)

# Load 1st stage model for predicting AOD at 1 km
# xgb_mod <- xgb.load(file.path(path_output_sherlock, sprintf("version%s", model_version), "anomAOD", "model", "aod_mod.xgb"))
# xgb.parameters(xgb_mod) <- list(nthread = xgb_nthread)

# Load crosswalk between 1 km grid and 10 km grid
crosswalk <- readRDS(file.path(path_data_sherlock, "1_grids", "grid_crosswalk_1km_10km.rds"))

#-------------------------------------------------------------------------------
#### Cross-sectional variables ####
# Latitude and longitude
print(paste("Longitude and latitude", "--------------------------------------------------"))
cell_cent = readRDS(file.path(path_data_sherlock, "1_grids", "1km_cell_centroids.rds"))

# Elevation
print(paste("Elevation", "--------------------------------------------------"))
start_time = get_start_time()
files = list.files(file.path(path_data_sherlock, "2_from_EE", "elevation_1km_subgrid"), 
                   full.names = TRUE)
files_filled = list.files(file.path(path_data_sherlock,"2_from_EE", "elevation_1km_subgrid_filled"), 
                          full.names = TRUE)
subgrids = gsub("^elevation_avg_sd_1km_subgrid_|\\.csv$", "", basename(files))
subgrids_filled = gsub("^elevation_avg_sd_1km_subgrid_|\\.csv$|_filled", "", basename(files_filled))
subgrids = setdiff(subgrids, subgrids_filled)
files = c(files_filled, 
          file.path(path_data_sherlock, "2_from_EE", "elevation_1km_subgrid", 
                    paste0("elevation_avg_sd_1km_subgrid_", subgrids, ".csv")))

elev <- files %>% 
  map_dfr(function(x) read.csv(x)) %>% 
  rename_with(function(x) {paste0("elevation_", x)}, .cols = !grid_id) %>% 
  rename(elevation_stdDev = elevation_stdDev_stdDev,
         grid_id_1km = grid_id)
print_time(start_time)

# NLCD
print(paste("NLCD", "--------------------------------------------------"))
start_time = get_start_time()
files = list.files(file.path(path_data_sherlock, "2_from_EE", "NLCD_1km_subgrid"), 
                   full.names = TRUE)
files_filled = list.files(file.path(path_data_sherlock, "2_from_EE", "NLCD_1km_subgrid_filled"), 
                          full.names = TRUE)
subgrids = gsub("^NLCD_areas_1km_subgrid_|\\.csv$", "", basename(files))
subgrids_filled = gsub("^NLCD_areas_1km_subgrid_|\\.csv$|_filled", "", basename(files_filled))
subgrids = setdiff(subgrids, subgrids_filled)
files = file.path(path_data_sherlock, "2_from_EE", "NLCD_1km_subgrid", 
                  paste0("NLCD_areas_1km_subgrid_", subgrids, ".csv"))

nlcd <- files %>% 
  map_dfr(function(x) read.csv(x)) %>%  #  
  mutate(groups = gsub("\\[|\\]", "", groups), # get rid of outer brackets
         groups = strsplit(groups, "\\}, \\{")) %>% # split on commas between brackets
  unnest(groups, keep_empty = TRUE) %>% # groups is now a list that we want to unnest (i.e. lengthen)
  mutate(groups = gsub("\\{|\\}", "", groups)) %>%  # drop the extra brackets left behind
  separate(groups, into = c("landcover", "area"), sep = ",") %>% # split in commas to get land cover class and area
  mutate(landcover = trimws(gsub("landcover=", "", landcover, fixed = TRUE)), # drop "landcover"
         area = trimws(gsub("sum=", "", area, fixed = TRUE)) %>% as.numeric, # drop "sum"
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
  rename(grid_id_1km = grid_id) %>% 
  select(-total) %>%
  bind_rows(map_dfr(files_filled, read.csv))
print_time(start_time)

# preprocess cross sectional variables 
crosswalk_processed = reduce(list(crosswalk, cell_cent, elev, nlcd), left_join, by = "grid_id_1km")
rm(crosswalk, cell_cent, elev, nlcd)
gc()
#-------------------------------------------------------------------------------
#### Time-varying variables ####

fire_dates_not_online = readRDS(file.path(path_data_sherlock, "fire", "fire_dates_auto", "fire_dates_not_online.rds"))
fire_dates_clusters_too_small = readRDS(file.path(path_data_sherlock, "fire", "fire_dates_auto", "fire_dates_clusters_too_small.rds"))

# out = vector("list", length(year_months))
registerDoParallel(num_par, num_cores)
log_file = paste0("log_", start_date, "_", end_date, ".txt")
writeLines(c(""), log_file)

out <- foreach(m = 1:length(year_months), 
               .combine = c, .inorder = FALSE, 
               .packages = c("lubridate", "magrittr", "dplyr",
                             "xgboost", "purrr")) %dopar% {
  
  sink(log_file, append = T)
  year_month = year_months[m]
  print(paste(year_month, "--------------------------------------------------"))
  start_time = get_start_time()
  
  xgb_mod <- xgb.load(file.path(path_output_sherlock, sprintf("version%s", model_version), "anomAOD", "model", "aod_mod.xgb"))
  xgb.parameters(xgb_mod) <- list(nthread = xgb_nthread)
  
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  dates_m = ymd(grep(paste0("^", y_str, m_str), all_dates_str, value = T))
  prev_m_str = as.integer(m_str) - 1
  prev_m_str = ifelse(prev_m_str == 0, "12", str_pad(prev_m_str, 2, "left", 0))
  prev_y_str = ifelse(prev_m_str == "12", as.character(as.integer(y_str) - 1), y_str)
  prev_ym_str = paste0(prev_y_str, "_", prev_m_str)
  
  # Filled fire and smoke
  filled_fire_file = file.path(path_data_sherlock, "3_intermediate", "filled_fire_auto", sprintf("filled_distance_to_fire_cluster_%s_%s.rds", y_str, m_str))
  if (file.exists(filled_fire_file)) filled_fire = readRDS(filled_fire_file)

  filled_smoke_days_file = file.path(path_data_sherlock, "3_intermediate", "filled_smoke", sprintf("filled_smoke_days_%s_%s.rds", y_str, m_str))
  if (file.exists(filled_smoke_days_file)) {
    filled_smoke_days = readRDS(filled_smoke_days_file)
  } else {
    filled_smoke_days = readRDS(file.path(path_data_sherlock, "smoke_days", sprintf("grid_smoke_day_%s_%s.rds", y_str, m_str))) %>% 
      select(id_grid, date, smoke_day) %>% 
      filter(smoke_day == 1)
  }
  
  # Fire
  fire_dist = file.path(path_data_sherlock, "distance_to_fire_cluster_auto", sprintf("grid_distance_to_fire_cluster_%s_%s.rds", y_str, m_str))
  if (file.exists(filled_fire_file)) {
    fire_dist = readRDS(fire_dist) %>% 
      left_join(filled_fire, by = c("id_grid", "date")) %>% 
      mutate(km_dist = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), km_dist.y, km_dist.x), 
             area = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), area.y, area.x), 
             num_points = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), num_points.y, num_points.x))
  } else{
    fire_dist = readRDS(fire_dist)
  }
  fire_dist = fire_dist %>% select(id_grid, date, km_dist, area, num_points) %>% 
    mutate(date = as.Date(date, format = "%Y%m%d"))
  
  
  # Anomalous AOT
  aot = file.path(path_data_sherlock, "3_intermediate", "aot_anom_auto", sprintf("aot_anom_%s_%s.rds", y_str, m_str))
  aot = readRDS(aot) %>% filter(year(date) == as.integer(y_str))
  if (format(min(dates_m), "%Y%m%d") != data_start_date) {
    print(paste0("loading previous month AOT for ", year_month))
    prev_aot = file.path(path_data_sherlock, "3_intermediate", "aot_anom_auto", sprintf("aot_anom_%s_%s.rds", prev_y_str, prev_m_str))
    prev_aot = readRDS(prev_aot) %>% filter(year(date) == as.integer(prev_y_str))
    aot = bind_rows(aot, prev_aot)
  }
  aot = aot %>% 
    mutate(orig = 1) %>%
    # add 3 lags of aot, filling with zeros on missing days
    {full_join(., 
               expand.grid(grid_id_10km = pull(., grid_id_10km) %>% unique,
                           date = seq.Date(pull(., date) %>% min, 
                                           pull(., date) %>% max, 
                                           by = "day")), 
               by = c("grid_id_10km", "date"))} %>% 
    replace_na(list(aot_anom = 0)) %>% 
    arrange(grid_id_10km, date) %>% 
    group_by(grid_id_10km) %>% 
    mutate(aot_anom_lag1 = lag(aot_anom, 1),
           aot_anom_lag2 = lag(aot_anom, 2),
           aot_anom_lag3 = lag(aot_anom, 3),
           aot_anom_lag1 = ifelse(is.na(aot_anom_lag1), aot_anom, aot_anom_lag1),
           aot_anom_lag2 = ifelse(is.na(aot_anom_lag2), aot_anom_lag1, aot_anom_lag2),
           aot_anom_lag3 = ifelse(is.na(aot_anom_lag3), aot_anom_lag2, aot_anom_lag3)) %>% 
    ungroup %>% 
    filter(orig == 1, # only keep the original set of days
           year(date) == as.integer(y_str),
           month(date) == as.integer(m_str)) %>% 
    select(-orig)
  
  # ERA5
  # load this month and next month to get leads of precipitation 
  next_ym_str <- seq.Date(ymd(paste0(year_month, "-01")),
                          by = "1 month", length.out = 2)[2] %>% 
    format("%Y-%m")
  era5_combined_grids <- c(year_month, next_ym_str) %>% 
    map_dfr(function(y_m) {
      y = substr(y_m, 1, 4)
      m = substr(y_m, 6, 7)
      x = readRDS(file.path(path_data_sherlock, "ERA5_variables", "automated", "joined_grids", sprintf("era5_joined_grid_%s_%s", y, m))) 
      return(x)
    })
  era5_combined_grids <- era5_combined_grids %>% filter(date <= ymd(paste0(next_ym_str, "-01")))
  
  setDT(era5_combined_grids)
  era5_combined_grids <- era5_combined_grids[, lapply(.SD, function(x) if (all(is.na(x))) as.numeric(NA) else x[which.max(!is.na(x))]), 
                                             by = .(id_grid, date)]
  # lead precipitation since its 1 day off (first make sure its ordered by date)
  setorder(era5_combined_grids, date)
  era5_combined_grids <- era5_combined_grids[, total_precipitation_daily_total := shift(total_precipitation_daily_total, 1, NA, "lead"),
                                             by = .(id_grid)]
  setDF(era5_combined_grids)
  
  era5_combined_grids <- era5_combined_grids %>% 
    filter(month(date) == as.numeric(m_str), 
           year(date) == as.numeric(y_str))
  
  print(paste0("starting on days for ", year_month))
  out_m = foreach(d = 1:length(dates_m), .combine = c) %do% {
    
    ymd_str = format(dates_m[d], "%Y%m%d")
    print(ymd_str)
    out_file_1km = file.path(path_prediction_output, "1km_smoke_days", 
                             paste0("anomAOD_predictions_1km_", ymd_str, ".rds"))
    out_file_10km = file.path(path_prediction_output, "10km_smoke_days", 
                              paste0("anomAOD_predictions_10km_", ymd_str, ".rds"))
    preexisting = file.exists(out_file_1km) & file.exists(out_file_10km)
    
    if (overwrite | !preexisting) {
      
      #### Predict AOD #####------------------------------------------------------
      smoke_days_d = filled_smoke_days %>% filter(date == dates_m[d], smoke_day == 1)
      if (nrow(smoke_days_d) == 0) return(1)
      aot_d = aot %>% filter(date == dates_m[d])
      era5_month_d = era5_combined_grids %>% filter(date == dates_m[d])
      fire_dist_d = fire_dist %>% filter(date == dates_m[d])
      
      # Combine 10 km predictors
      pred_data <- smoke_days_d %>% 
        select(grid_id_10km, date) %>% 
        left_join(aot_d, 
                  by = c("grid_id_10km", "date")) %>% 
        left_join(fire_dist_d %>% select(grid_id_10km = id_grid, 
                                         date, 
                                         fire_dist_km = km_dist, 
                                         closest_fire_area = area, 
                                         closest_fire_num_points = num_points), 
                  by = c("grid_id_10km", "date")) %>% 
        left_join(era5_month_d %>% select(grid_id_10km = id_grid, date, 
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
                  by = c("grid_id_10km", "date")) %>% 
        # add 1km predictors
        left_join(crosswalk_processed, by = "grid_id_10km")
      if (nrow(pred_data) == 0) return(2)
      
      # Get 1 km anomalous AOD predictions
      pred_data_mat = pred_data %>% 
        mutate(month = factor(as.integer(m_str), levels = 1:12)) %>%
        select(month, lat, lon, aot_anom, 
               aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
               fire_dist_km, closest_fire_area, closest_fire_num_points,
               pbl_min, pbl_max, pbl_mean,
               wind_u, wind_v, 
               dewpoint_temp_2m, temp_2m,  
               sea_level_pressure, surface_pressure, precip, 
               elevation_mean, elevation_stdDev, 
               developed, barren, forest, shrubland, cultivated, 
               wetlands, herbaceous, water)
      
      pred_data_mat <- model.matrix.lm(~.-1,
                                       data = pred_data_mat,
                                       na.action = "na.pass") %>%
        xgb.DMatrix(nthread = xgb_nthread)
      
      new_preds <- pred_data %>%
        {cbind(select(., grid_id_1km, date),
               aod_anom_pred = predict(xgb_mod, pred_data_mat, nthread = xgb_nthread))}
      
      # save the 1km predictions 
      saveRDS(new_preds, out_file_1km)
      print(paste(ymd_str, " - 1km predictions saved"))
      
      # save quantiles + mean aggregated to 10km grid cell
      preds_10km_feats <- new_preds %>% 
        left_join(crosswalk_processed %>% select(grid_id_1km, grid_id_10km), by = "grid_id_1km") %>% 
        group_by(grid_id_10km, date) %>% 
        summarise(aod_anom_pred_0.00 = quantile(aod_anom_pred, 0),
                  aod_anom_pred_0.25 = quantile(aod_anom_pred, 0.25),
                  aod_anom_pred_0.50 = quantile(aod_anom_pred, 0.5),
                  aod_anom_pred_0.75 = quantile(aod_anom_pred, 0.75),
                  aod_anom_pred_1.00 = quantile(aod_anom_pred, 1),
                  aod_anom_pred_mean = mean(aod_anom_pred)) %>% 
        ungroup()
      saveRDS(preds_10km_feats, out_file_10km)
      print(paste(ymd_str, " - 10km aggregates of predictions saved"))
      return(0)
    }
  }
  # out[[m]] = out_m
  print_time(start_time)
  return(year_month)
}
stopImplicitCluster()

#-#-----------------------------------------------------------------------------
# Run the line below uncommented on local machine to copy output folder from Oak 
# to local machine
# system(sprintf("rsync -azvP %s@dtn.sherlock.stanford.edu:%s  %s", SUNetID, path_output_sherlock, path_output))
#-#-----------------------------------------------------------------------------
