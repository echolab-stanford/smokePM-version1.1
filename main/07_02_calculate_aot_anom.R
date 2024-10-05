source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs, Jessica Li
# Gets anomalous AOT (all days).
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to anomalize
start_month = "2006-01" # "1980-01"
end_month = "2023-12" # format(today() - months(2), "%Y%m")

# Set months to use as available inputs
start_input = "2006-01" # "1980-01"
end_input = "2023-12" # format(today() - months(2), "%Y%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")
input_range = (ym(start_input) %--% ym(end_input))

project_grid = grid = read_sf(file.path(path_data, "1_grids", "10km_grid"))
project_grid_ids = project_grid$ID

smoke_dates_not_online = readRDS(file.path(path_data, "smoke", "smoke_dates_not_online_update.rds"))

for (year_month in year_months) {
  y = substr(year_month, 1, 4)
  m = substr(year_month, 6, 7)

  dates_m_1 = ymd(paste(y, m, "01"))
  # all dates in the current month, same month in previous year, and same month in next year
  dates_m_all = c(seq.Date(dates_m_1 - years(1), dates_m_1 - years(1) + months(1) - days(1), by = "day"), 
                  seq.Date(dates_m_1, dates_m_1 + months(1) - days(1), by = "day"), 
                  seq.Date(dates_m_1 + years(1), dates_m_1 + years(1) + months(1) - days(1), by = "day")) %>% 
    # limited to dates in the input range
    intersect(seq.Date(ym(start_input), ym(end_input), by = "day"))
  
  project_grid_m = expand.grid(grid_id_10km = project_grid_ids, 
                               date = dates_m_all)

  # Load filled smoke days
  smoke <- -1:1 %>% map_dfr(function(i) {
    y = as.character(as.numeric(y) + i)
    file_name = file.path(path_data, "3_intermediate", "filled_smoke", sprintf("filled_smoke_days_%s_%s.rds", y, m))
    
    within_input_range = ym(paste(y, m)) %within% input_range
    file_exists = file.exists(file_name)
    if (within_input_range & file_exists) {
      out = readRDS(file_name) 
    } else {
      out = NULL
    }
    return(out)
  }) %>% mutate(date = as.double(date)) %>% 
    right_join(project_grid_m, by = c("grid_id_10km", "date")) %>% 
    mutate(smoke_day = ifelse(is.na(smoke_day) & !(date %in% smoke_dates_not_online), 0, smoke_day),
           date = as.Date(date)) 

  # Load AOT
  aot <- -1:1 %>% map_dfr(function(i) {
    y = as.character(as.numeric(y) + i)
    file_name = file.path(path_data, "MERRA2_AOT", "us_grid_daily_aot", sprintf("daily_grid_aot_%s_%s.RDS", y, m))
    
    within_input_range = ym(paste(y, m)) %within% input_range
    file_exists = file.exists(file_name)
    if (within_input_range & file_exists) {
      out = readRDS(file_name) %>% 
        mutate(date = ymd(date)) %>% 
        rename(grid_id_10km = grid_id)
    } else {
      out = NULL
    }
    return(out)
  }) %>% 
  left_join(smoke, by = c("date", "grid_id_10km")) %>% 
  mutate(month = month(date), 
         year = year(date))

  # Calculate background AOT
  aot_medians = nonsmoke_medians(filter(aot, !(format(date, "%Y%m%d") %in% smoke_dates_not_online)),
                               aot, smoke_day, grid_id_10km, month, year)
  saveRDS(aot_medians, file.path(path_data, "3_intermediate", "aot_nonsmoke_medians", sprintf("aot_nonsmoke_medians_%s_%s.rds", y, m)))

  aot_anom = aot %>% 
    left_join(aot_medians, by = c("grid_id_10km", "month", "year")) %>%
    transmute(grid_id_10km, 
              date,
              aot_anom = aot - aot_med_3yr) %>% 
    filter(year(date) == y )
  saveRDS(aot_anom, file.path(path_data, "3_intermediate", "aot_anom", sprintf("aot_anom_%s_%s.rds", y, m)))
}
