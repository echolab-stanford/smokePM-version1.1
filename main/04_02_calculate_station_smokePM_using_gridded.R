source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Calculates smokePM at stations using gridded smoke days.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months for sample to span
start_month = "2006-01" # "1999-01" TODO: check why 2023 is not runnning
end_month = "2023-12" # format(today() - days(1), "%Y%m")

# Set dates for sample to span
start_date = "20060101" # "19990101"
end_date = "20231231" # format(today() - days(1), "%Y%m%d")

# Make sure date range above does not exceed date range in 
# 02_02_get_epa_station_locations.R. If it does, first process EPA station 
# locations so that it does not. Otherwise, stations may be dropped from the 
# sample that should not be dropped.

# Set months to use as available inputs -here we can use the full date range that we have files for
start_input = "2006-01" # "1999-01"
end_input = "2023-12" # format(today() - days(1), "%Y%m")

# Set dates to filter dates to input range in full format
# start_input = "20220101" 
# end_input = "20231231" 

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------
year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")
input_range = (ym(start_input) %--% ym(end_input))

# Load EPA station locations
epa_ll <- read_sf(file.path(path_data, "EPA",  "epa_station_locations")) %>% #run 02_02
  dplyr::rename(id = stn_id, state_code = stat_cd, county_code = cnty_cd, cbsa_code = cbsa_cd, grid_id_10km = grid_10km) %>% 
  filter(!is.na(grid_id_10km)) # drop outside the contiguous US

grid_10km_epa = unique(epa_ll$grid_id_10km)

# Load smoke missing dates
smoke_missing_dates = readRDS(file.path(path_data, "smoke", "smoke_dates_not_online_update.rds")) 


for (year_month in year_months) {
  y <- substr(year_month, 1, 4)
  m <- substr(year_month, 6, 7)
  
  out_file = file.path(path_data, "3_intermediate", "station_smokePM_update" , sprintf("station_smokePM_%s_%s.rds", y, m)) #station_smokePM_update is the folder with the estimates from the API
  preexisting = file.exists(out_file)
  
  if (overwrite | !preexisting) {
    # Load PM2.5 from EPA stations
    epa_pm <- -1:1 %>% map_dfr(function(i) {
      y = as.character(as.numeric(y) + i) #change this back to i after troubleshooting
      file_name = file.path(path_data, "EPA", #"raw",
                            "station_level_pm25_data", sprintf("epa_station_level_pm25_data_%s_%s.rds", y, m))  #add "raw to file path to call data from API
 
      
      within_input_range = ym(paste(y, m)) %within% input_range
      file_exists = file.exists(file_name)
     

      if (within_input_range & file_exists) {
        out = readRDS(file_name) %>% 
          mutate(year = substr(date, 5,8),
                 month = substr(date, 1,2),
                 day = substr(date, 3,4),
                 date = mdy(date)) %>%
          filter(#date >= ymd(start_input), 
                 #date <= ymd(end_input), 
                 date >= ymd(start_date) - days(2), 
                 date <= ymd(end_date) + days(2), 
                 id %in% epa_ll$id) %>% 
          dplyr::select(id, year, month, day, pm25) %>% 
          group_by(id, year, month, day) %>% 
          dplyr::summarise(pm25 = mean(pm25)) %>% 
          unite(date, year, month, day, sep = "-", remove = F) %>% 
          mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% 
          ungroup() %>% 
          left_join(epa_ll %>% 
                      st_drop_geometry() %>% 
                      dplyr::select(id, grid_id_10km), 
                    by = "id") %>% 
          filter(!is.na(grid_id_10km))
      } else {
        out = NULL
      }
      return(out)
    })
    
    # Load gridded smoke days
    smoke_days <- -1:1 %>% map_dfr(function(i) {
      y = as.character(as.numeric(y) + i) 
      
      file_name = file.path(path_data, "3_intermediate", "filled_smoke", sprintf("filled_smoke_days_%s_%s.rds", y, m)) 
     
      within_input_range = ym(paste(y, m)) %within% input_range
      file_exists = file.exists(file_name)
      if (within_input_range & file_exists) {
        out = readRDS(file_name) %>% 
          filter(grid_id_10km %in% (grid_10km_epa)) %>% 
          mutate(date = ymd(date))
      } else {
        out = NULL
      }
      return(out)
    })
    
    # Calculate smoke PM
    smokePM <- epa_pm %>% 
      left_join(smoke_days, 
                by = c("date", "grid_id_10km")) %>% 
      replace_na(list(smoke_day = 0))%>%
    {left_join(., 
                 nonsmoke_medians(filter(., !(date %in% ymd(smoke_missing_dates))), 
                                  pm25, smoke_day, id, month, year), 
                 by = c("id", "month", "year"))} %>% 
      mutate(pm25_anom = pm25 - pm25_med_3yr, 
             smokePM = pmax(0, pm25_anom)*smoke_day, 
             smoke_missing_date = (date %in% ymd(smoke_missing_dates)))%>% 
      filter(year == y)
    
    # Save
    saveRDS(smokePM, out_file)
  }
}

#checking data generated
# smokePM_test<- year_months %>% map_dfr(function(year_month) {
#   y = substr(year_month, 1, 4)
#   m = substr(year_month, 6, 7)
#   out = readRDS(file.path(path_data, "3_intermediate", "station_smokePM_update", sprintf("station_smokePM_%s_%s.rds", y, m))) %>%
#     filter(smoke_day == 1 & !is.na(pm25) & !smoke_missing_date, lubridate::year(date) == as.integer(y))
#   return(out)
# })
# 
# smokePM_test %>% summary()
# 
# missings<- smokePM_test %>% filter(is.na(pm25_med_3yr))
# 
# missings %>% count(month, year) %>% view()
# 
# check<- smokePM_test %>%
#   filter(id %in% missings$id)
