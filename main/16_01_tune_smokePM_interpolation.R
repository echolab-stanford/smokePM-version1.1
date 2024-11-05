library(tidyverse)
library(magrittr)
library(gstat)
library(raster)
library(sf)  
library(fixest)  
library(lubridate)
library(rBayesianOptimization)

# parameters to set --------------------
n_bayes_init = 8
n_bayes_iter = 6
param_bounds = list(pow = c(0.5, 5), 
                    dist = c(3e5, 1e6), 
                    max_point = c(10L, 100L))

# Set months for sample to span
start_month = "2006-01" 
end_month = "2023-06"

# load data ----- 
source("scripts/setup/00_03_load_paths.R")

# read command line args which will be the fold which is treated as out-of-sample 
args <- commandArgs(TRUE)
out_folds <- na.omit(c(as.numeric(args)))  # run with 1-5 for the CV interpolations, and 99 all folds

# print informatiom about what will be run based on the command line args
print(paste0("Interpolation tuning via CV will be done over all fold(s) except ", 
             paste0(unique(out_folds), collapse = " and "), 
             "."))
if(any(out_folds == 99)){print("Predictions will be created for all folds.")
} else{print(paste0("Predictions will be created for fold(s) ",
                    paste0(unique(out_folds), collapse = " and "), "."))}

progress_file = file.path(path_output_sherlock, "smokePM_interpolation",
                          paste0("./interp_tune_progress_fold", paste0(out_folds, collapse = ""), ".txt"))

# if the folder for saving the output doesn't exist, make it 
if(!dir.exists(file.path(path_output_sherlock, "smokePM_interpolation"))){
  dir.create(file.path(path_output_sherlock, "smokePM_interpolation"), recursive = T)
}

# set crs for interpolation 
interp_crs = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"

# load full station PM2.5 panel 
smoke_missing_dates = readRDS(file.path(path_data_sherlock, "smoke", "smoke_dates_not_online.rds"))
smoke_missing_dates = ymd(smoke_missing_dates)
year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")
station_full_panel <- year_months %>% map(function(year_month) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)
  out = readRDS(file.path(path_data_sherlock, "3_intermediate", "station_smokePM_MM", sprintf("station_smokePM_%s_%s.rds", y, m))) %>% 
    dplyr::filter(!is.na(pm25) &  !is.na(smokePM) & !smoke_missing_date) # 
  return(out)
}) %>% list_rbind()
station_full_panel %<>% ungroup

# load 10km grid cell cetroids
grid_cent <- readRDS(file.path(path_data_sherlock, "1_grids", "10km_cell_centroids.rds"))

# load epa locations
epa_ll <- st_read(file.path(path_data_sherlock, "EPA", "epa_station_locations"))

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
                                  by = "id") %>% 
  left_join(grid_cent %>% dplyr::select(grid_id_10km, grid_x = COORDX, grid_y = COORDY), by = "grid_id_10km")
station_full_panel %<>% filter(!is.na(station_x) & !is.na(station_y)) 

# drop variable we don't need 
station_full_panel %<>% dplyr::select(id, fold, station_x, station_y, grid_x, grid_y, 
                                      year, date, smoke_day, smokePM)

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

# test <- interp_over_dates(station_full_panel %>% filter(fold %in% out_folds == FALSE), 
#                           station_full_panel %>% filter(fold %in% out_folds == TRUE), 
#                           list(idp = 2, maxdist = 1e6, nmax = 10))

# function to loop over folds, run interpolation for each fold held out of sample, then evaluation functions 
# returning best parameter set from parameter set options
interp_cv = function(param_set, # list of parameter sets 
                     interp_df){ # return eval metrics or interpolated values? 
  # print(paste0(names(param_set), ": ", unlist(param_set), collapse = ", "))
  write(paste0(names(param_set), ": ", unlist(param_set), collapse = ", "),
        file = progress_file,
        append = T)
  map(unique(interp_df$fold), function(j){
    print(paste0("CV with fold ", j, " held out."))
    interp_over_dates(interp_df %>% filter(fold != j), 
                      interp_df %>% filter(smoke_day == 1) %>% filter(fold == j),
                      param_set)  %>% 
      mutate(fold = j) %>% 
      return
  }) %>% list_rbind() -> interp_vals 
  
  full_join(interp_vals, 
            interp_df %>% 
              filter(smoke_day == 1) %>% 
              dplyr::select(id, date, smokePM),
            by = c("id", "date")) %>%
    mutate(interp_fill = replace_na(interp, 0)) %>% 
    # calculate eval metrics
    {c(r2 = r2(feols(smokePM ~ interp, data = .), "r2") %>% try %>% ifelse(class(.) == "try-error", 0, .), 
       wr2 = r2(feols(smokePM ~ interp | id, data = .), "wr2") %>% try %>% ifelse(class(.) == "try-error", 0, .), 
       rmse = mean(.$smokePM - .$interp, na.rm = T)^2 %>% sqrt, 
       r2_fill = r2(feols(smokePM ~ interp_fill, data = .), "r2") %>% try %>% ifelse(class(.) == "try-error", 0, .), 
       wr2_fill = r2(feols(smokePM ~ interp_fill | id, data = .), "wr2") %>% try %>% ifelse(class(.) == "try-error", 0, .), 
       rmse_fill = mean(.$smokePM - .$interp_fill)^2 %>% sqrt, 
       nobs_NA = sum(is.na(.$interp)))} -> eval_metrics
  write(paste0("R2 (with NAs filled) = ", eval_metrics["r2_fill"], 
               "\n", eval_metrics["nobs_NA"], " missing interpolations."),
        file = progress_file,
        append = T)
  return(list(Score = eval_metrics["r2_fill"],
              Pred = eval_metrics["nobs_NA"]))
} 

# tic <- Sys.time() 
# test <- interp_cv(list(idp = 2, maxdist = 1e6, nmax = 20),
#                   station_full_panel %>% filter(fold %in% out_folds == FALSE),
#                   T)
# print(Sys.time() - tic) # approximate timing 

# bayesian optimization over interpolation parameters power, max distance, and max points
print("beginning bayesian optimization")
write("", progress_file)
set.seed(10001)
bayes_opt_params <- BayesianOptimization(
  function(pow, dist, max_point){
    interp_cv(list(idp = pow, maxdist = dist, nmax = max_point),
              station_full_panel %>% filter(fold %in% out_folds == FALSE), 
              T)
    },
  bounds = param_bounds,
  init_points = n_bayes_init, 
  n_iter = n_bayes_iter)
# save bayesian optimization params
saveRDS(bayes_opt_params,
        file.path(path_output_sherlock, "smokePM_interpolation",
                  paste0("interp_tune_params_drop", paste0(out_folds, collapse = ""), ".rds")))

# run final interpolation with the tuned parameters
best_interp = interp_over_dates(station_full_panel %>% 
                                  filter(fold %in% out_folds == FALSE), 
                                station_full_panel %>% 
                                  filter(smoke_day == 1) %>% 
                                  filter(fold %in% out_folds == TRUE | any(out_folds == 99)),
                                list(idp = bayes_opt_params$Best_Par["pow"], 
                                     maxdist = bayes_opt_params$Best_Par["dist"], 
                                     nmax= bayes_opt_params$Best_Par["max_point"]))
# save interpolation values for the out_folds (or all obs if out_folds == 99)
saveRDS(best_interp,
        file.path(path_output_sherlock, "smokePM_interpolation",
                  paste0("interp_drop", paste0(out_folds, collapse = ""), ".rds")))


# print warnings 
warnings()

# future_map(1:10, ~rnorm(1), .options=furrr_options(seed = 1))