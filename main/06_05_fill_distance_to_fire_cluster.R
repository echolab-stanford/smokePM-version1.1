source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Fills in distance to fire cluster by month.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to fill
start_month = "2006-01" # "2003-04"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set months to use as available inputs
start_input = "2006-01" # "2003-04"
end_input = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

# Load dates
smoke_dates_not_online = readRDS(file.path(path_data, "smoke", "smoke_dates_not_online_update.rds"))
fire_dates_not_online = readRDS(file.path(path_data, "fire", "fire_dates_not_online.rds"))
fire_dates_clusters_too_small = readRDS(file.path(path_data, "fire", "fire_dates_clusters_too_small.rds"))

# Track runs since not every month needs to save filled data
previous_runs_file = file.path(path_data, "3_intermediate", "filled_fire", "previously_run.txt")
previous_runs_exist = file.exists(previous_runs_file)
if (previous_runs_exist) {
  previous_runs = read.table(previous_runs_file, header = T)
} else {
  previous_runs = data.frame(year = numeric(), 
                             month = numeric(), 
                             timestamp = character())
}

these_runs = vector("list", length(year_months))
for (i in 1:length(year_months)) {
  year_month = year_months[i]
  year = substr(year_month, 1, 4)
  month = substr(year_month, 6, 7)
  
  out_file = file.path(path_data, "3_intermediate", "filled_fire", sprintf("filled_distance_to_fire_cluster_%s_%s.rds", year, month))
  preexisting = file.exists(out_file)
  previously_run = previous_runs %>% 
    filter(year == as.numeric(!!year), 
           month == as.numeric(!!month)) %>% 
    nrow() %>% 
    as.logical()
  
  if (overwrite | (!preexisting & !previously_run)) {
    # Load gridded fire distance
    fire_dist = readRDS(file.path(path_data, "distance_to_fire_cluster", sprintf("grid_distance_to_fire_cluster_%s_%s.rds", year, month)))
    first_month = (year_month == start_input)
    if (!first_month) {
      prev_fire_dist = read_rds(file.path(path_data, "distance_to_fire_cluster", 
                                          sprintf("grid_distance_to_fire_cluster_%s.rds", format(ym(year_month) - months(1), "%Y_%m"))))
      prev_fire_dist = prev_fire_dist[ymd(prev_fire_dist$date) %within% ((ymd(paste0(year, month, "01")) - days(2)) %--% (ymd(paste0(year, month, "01")) - days(1))),]
      fire_dist = bind_rows(prev_fire_dist, fire_dist)
      rm(prev_fire_dist)
    }
    last_month = (year_month == end_input)
    if (!last_month) {
      foll_fire_dist = read_rds(file.path(path_data, "distance_to_fire_cluster", 
                                          sprintf("grid_distance_to_fire_cluster_%s.rds", format(ym(year_month) + months(1), "%Y_%m"))))
      foll_fire_dist = foll_fire_dist[ymd(foll_fire_dist$date) %within% ((ymd(paste0(year, month, "01")) + months(1)) %--% (ymd(paste0(year, month, "01")) + months(1) + days(1))),]
      fire_dist = bind_rows(fire_dist, foll_fire_dist)
      rm(foll_fire_dist)
    }
    
    # Classify and replace values
    fire_dist = fire_dist %>% 
      mutate(smoke_date_not_online = date %in% smoke_dates_not_online, 
             fire_date_not_online = date %in% fire_dates_not_online, 
             fire_date_clusters_too_small = date %in% fire_dates_clusters_too_small, 
             assumed_no_clusters = fire_date_not_online & !smoke_date_not_online, 
             km_dist = ifelse(fire_date_clusters_too_small | assumed_no_clusters, NA, km_dist), 
             area = ifelse(fire_date_clusters_too_small | assumed_no_clusters, 0, area), 
             num_points = ifelse(fire_date_clusters_too_small | assumed_no_clusters, 0, num_points))
    
    # Get fill values
    fire_fill = fire_dist %>%
      arrange(id_grid, ymd(date)) %>% 
      group_by(id_grid) %>%
      mutate(across(.cols = c(km_dist, area, num_points), 
                    .fns = list(lag1 = ~lag(.x, 1), 
                                lag2 = ~lag(.x, 2), 
                                lead1 = ~lead(.x, 1), 
                                lead2 = ~lead(.x, 2)))) %>% 
      filter(is.na(area) | is.na(num_points)) %>% 
      rowwise() %>% 
      mutate(km_dist = case_when(!is.na(km_dist_lag1) & !is.na(km_dist_lead1) ~ median(c(km_dist_lag1, km_dist_lead1), na.rm = T), 
                                 T ~ median(c(km_dist_lag2, km_dist_lag1, km_dist_lead1, km_dist_lead2), na.rm = T)),
             area = case_when(!is.na(area_lag1) & !is.na(area_lead1) ~ median(c(area_lag1, area_lead1)), 
                              T ~ median(c(area_lag2, area_lag1, area_lead1, area_lead2), na.rm = T)), 
             num_points = case_when(!is.na(num_points_lag1) & !is.na(num_points_lead1) ~ median(c(num_points_lag1, num_points_lead1), na.rm = T), 
                                    T ~ median(c(num_points_lag2, num_points_lag1, num_points_lead1, num_points_lead2), na.rm = T))) %>% 
      ungroup() %>% 
      select(id_grid, date, km_dist, area, num_points)
    
    fire_fill = fire_dist %>% 
      filter(fire_date_clusters_too_small | assumed_no_clusters) %>% 
      select(id_grid, date, km_dist, area, num_points) %>% 
      rbind(fire_fill)
    
    saveRDS(fire_fill, out_file)
  }
  this_run = data.frame(year = as.numeric(year), 
                        month = as.numeric(month), 
                        timestamp = as.character(Sys.time()))
  these_runs[[i]] = this_run
}

these_runs = bind_rows(these_runs)
latest_runs = previous_runs %>% 
  bind_rows(these_runs) %>% 
  group_by(year, month) %>% 
  slice_max(timestamp) %>% 
  ungroup() %>% 
  arrange(year, month)
write.table(latest_runs, previous_runs_file, row.names = F)
