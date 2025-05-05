#-#-----------------------------------------------------------------------------
# Run the line below uncommented on local machine to copy data folder in Dropbox 
# repository from local machine to Oak
# system(sprintf("rsync -azvP %s %s@dtn.sherlock.stanford.edu:%s", path_data, SUNetID, path_data_sherlock))
#-#-----------------------------------------------------------------------------

# sherlock = T
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

source("./scripts/setup/00_02_load_functions.R")
source("./scripts/setup/00_03_load_paths.R")

if (Sys.getenv('SLURM_JOB_ID') != "") {
  num_cores = as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
  print(num_cores)
  xgb_nthread =  2
  num_par = floor(num_cores/xgb_nthread)
} else {
  num_cores = 2
  xgb_nthread = 2
  num_par = 1
}
print(paste0("Using ", num_cores, " CPUs with ", xgb_nthread, " threads per xgb, ", num_par, " parallel clusters"))

#-------------------------------------------------------------------------------
# Written by: Marissa Childs, Jessica Li
# Gets smoke PM2.5 predictions.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set date range to predict
start_date = "20060101" 
end_date = "20241231" 
data_start_date = "20060101" # when should the data start, used for determining whether to get previous month AOT
data_end_date = "20241231" # when does the data end, used for determining whether the get next month ERA5 data
# Set whether to overwrite preexisting files or not
overwrite = T

#-#-----------------------------------------------------------------------------
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(substr(all_dates, 1, 7))
year_months = gsub("-", "_", year_months)

#### Cross-sectional variables ####
# Latitude and longitude
print(paste("Longitude and latitude", "--------------------------------------------------"))
cell_cent = readRDS(file.path(path_data_sherlock, "1_grids", "10km_cell_centroids.rds"))

# Elevation
print(paste("Elevation", "--------------------------------------------------"))
elev <- file.path(path_data_sherlock, "2_from_EE", "elevation_avg_sd_10km_grid_filled.csv") %>% 
  read.csv() %>% 
  select(grid_id_10km = ID, elevation_mean = mean, elevation_stdDev = stdDev_stdDev)

# NLCD
print(paste("NLCD", "--------------------------------------------------"))
nlcd <- file.path(path_data_sherlock, "2_from_EE", "NLCD_areas_10km_grid_filled.csv") %>% 
  read.csv()

# join all cross sectional variables
cross_processed = reduce(list(cell_cent, elev, nlcd), left_join, by = "grid_id_10km")
rm(cell_cent, elev, nlcd)
gc()

#-------------------------------------------------------------------------------
#### Time-varying variables ####
fire_dates_not_online = readRDS(file.path(path_data_sherlock, "fire", "fire_dates_auto", "fire_dates_not_online.rds"))
fire_dates_clusters_too_small = readRDS(file.path(path_data_sherlock, "fire", "fire_dates_auto", "fire_dates_clusters_too_small.rds"))

registerDoParallel(num_par, num_cores)
log_file = paste0("log_smokePMpred_", start_date, "_", end_date, ".txt")
writeLines(c(""), log_file)
sink(log_file, append = T)

out <- foreach(
  m = 1:length(year_months), 
  .combine = c, .inorder = FALSE, 
  .packages = c("lubridate", "magrittr", "dplyr",
                "xgboost", "purrr")) %dopar% {
    
    
    year_month = year_months[m]
    print(paste(year_month, "--------------------------------------------------"))
    start_time = get_start_time()
    
    y_str = substr(year_month, 1, 4)
    m_str = substr(year_month, 6, 7)
    dates_m = ymd(grep(paste0("^", y_str, m_str), all_dates_str, value = T))
    prev_m_str = as.integer(m_str) - 1
    prev_m_str = ifelse(prev_m_str == 0, "12", str_pad(prev_m_str, 2, "left", 0))
    prev_y_str = ifelse(prev_m_str == "12", as.character(as.integer(y_str) - 1), y_str)
    prev_ym_str = paste0(prev_y_str, "_", prev_m_str)

    xgb_mod <-xgb.load(file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM", "revisions", "model","smokePM_mod_fold99_drop-aod_anom_pred.xgb"))
    xgb.parameters(xgb_mod) <- list(nthread = xgb_nthread)
    
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
    if(class(filled_smoke_days$date) == "character"){
      filled_smoke_days = filled_smoke_days %>% mutate(date = as.Date(date, format = "%Y%m%d"))
    }
    
    # Fire
    fire_dist = file.path(path_data_sherlock, "distance_to_fire_cluster_auto", 
                          sprintf("grid_distance_to_fire_cluster_%s_%s.rds", y_str, m_str))
    if (file.exists(filled_fire_file)) {
      fire_dist = readRDS(fire_dist) %>% 
        left_join(filled_fire, by = c("id_grid", "date")) %>% 
        mutate(km_dist = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), km_dist.y, km_dist.x), 
               area = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), area.y, area.x), 
               num_points = ifelse((date %in% fire_dates_not_online) | (date %in% fire_dates_clusters_too_small), num_points.y, num_points.x))
    }
    fire_dist = fire_dist %>% select(id_grid, date, km_dist, area, num_points) %>% 
      mutate(date = as.Date(date, format = "%Y%m%d"))
    
  # Anomalous AOT - aot_anom files dated 04/18/2024 are cleared of repeated observations
  aot = file.path(path_data_sherlock, "3_intermediate", "aot_anom_auto", sprintf("aot_anom_%s_%s.rds", y_str, m_str)) 
  aot = readRDS(aot) %>% filter(year(date) == as.integer(y_str))
  if (format(min(dates_m), "%Y%m%d") != data_start_date) {
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
  
    #ERA5
  # load this month and next month to get leads of precipitation 
  next_ym_str <- seq.Date(ymd(paste0(year_month, "-01")),
                          by = "1 month", length.out = 2)[2] %>% 
    format("%Y-%m")
  if (format(max(dates_m), "%Y%m%d") != data_end_date) {
    era5_months = c(year_month, next_ym_str)
  } else{ era5_months = year_month }
  
  era5_combined_grids <- era5_months %>% 
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
 
    #Interpolations
    interpolations =  file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM_interpolation", "revisions", "interpolations_full_model", 
                      sprintf("smokePM_interpolated_%s_%s.rds", y_str, m_str))
  
    smoke_interpolation = readRDS(interpolations) %>% 
    rename(grid_id_10km = id,
      smokePM_interp = interp)

  out_m = foreach(d = 1:length(dates_m), .combine = c) %do% { 
    ymd_str = format(dates_m[d], "%Y%m%d")
   out_file = file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM", "revisions", "predictions",
                     paste0("smokePM_predictions_10km_", ymd_str, ".rds"))
   
    preexisting = file.exists(out_file)
    
   if (overwrite | !preexisting) {
      #### Predict smokePM #####------------------------------------------------------
      smoke_days_d = filled_smoke_days %>% filter(date == dates_m[d], smoke_day == 1)
      if (nrow(smoke_days_d) == 0) return(1)
      aot_d = aot %>% filter(date == dates_m[d])
      era5_month_d = era5_combined_grids %>% filter(date == dates_m[d])
      fire_dist_d = fire_dist %>% filter(date == dates_m[d])
      interp_d = smoke_interpolation %>% filter(date == dates_m[d])
      
      # Combine time-varying predictors
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
        inner_join(era5_month_d %>% select(grid_id_10km = id_grid, date, 
                                           pbl_max = `boundary_layer_height_daily_maximum`,
                                           pbl_mean = `boundary_layer_height_daily_mean`,
                                           pbl_min = `boundary_layer_height_daily_minimum`,
                                           sea_level_pressure = `mean_sea_level_pressure_daily_mean`,
                                           wind_u = `10m_u_component_of_wind_daily_mean`,
                                           wind_v = `10m_v_component_of_wind_daily_mean`,
                                           dewpoint_temp_2m = `2m_dewpoint_temperature_daily_mean`,
                                           temp_2m = `2m_temperature_daily_mean`,
                                           surface_pressure = `surface_pressure_daily_mean`,
                                           precip = `total_precipitation_daily_total`) %>% 
                     drop_na(!precip), # drop except missing precip which happens when we don't have a lead of precip to use 
                   by = c("grid_id_10km", "date")) %>% 
        left_join(interp_d, 
                  by = c("grid_id_10km", "date")) %>% #joining interpolated values
        left_join(cross_processed, by = "grid_id_10km")
      if (nrow(pred_data) == 0) return(2)

      
      # Get smokePM predictions - THE VARIABLES NEED TO BE IN THIS EXACT ORDER AS THE MODEL WAS TRAINED
      pred_data_mat = pred_data %>% 
        mutate(month = factor(as.integer(m_str), levels = 1:12)) %>% 
        select(month, lat, lon, 
               smokePM_interp,
               aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
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
        {cbind(dplyr::select(., grid_id_10km, date), 
               smokePM_pred = predict(xgb_mod, pred_data_mat))}
      
      # save the 10km predictions 
      saveRDS(new_preds, out_file)
      print(paste(ymd_str, " - smokePM predictions saved"))
      return(0)
    }
  }
  print_time(start_time)
  return(year_month)
                }
sink()
stopImplicitCluster()

file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM", "revisions", "predictions") %>% 
  list.files(full.names = T, pattern ="smokePM_predictions_10km_\\d{8}.rds") %>% 
  purrr::map(readRDS) %>% 
  purrr::list_rbind() -> full_preds 
full_preds <- full_preds %>% mutate(smokePM_pred = pmax(0, smokePM_pred))

full_out_file = file.path(path_output_sherlock, sprintf("version%s", model_version), "smokePM", "revisions", "predictions", 
                          paste0("smokePM_predictions_10km_", format(min(full_preds$date), "%Y%m%d"), "_", format(max(full_preds$date), "%Y%m%d"), ".rds")) 

saveRDS(full_preds, full_out_file)



#-#-----------------------------------------------------------------------------
# Run the line below uncommented on local machine to copy output folder from Oak 
# to local machine
# system(sprintf("rsync -azvP %s@dtn.sherlock.stanford.edu:%s  %s", SUNetID, path_output_sherlock, path_output))
#-#-----------------------------------------------------------------------------
