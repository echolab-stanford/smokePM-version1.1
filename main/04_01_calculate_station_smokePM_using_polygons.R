library(lubridate)
library(sf)
#packageVersion("sf") make sure is version '0.9.8'
library(dplyr)
library(tidyr)
library(purrr)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs, Jessica Li
# Creates a panel data frame of daily EPA station total PM2.5 (2000-2023) and 
# smoke days and smoke PM2.5 (2006-2022) in CONUS using smoke plume polygons.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set dates for sample to span
start_date = "20060101" # "19990101"
end_date = "20231231" # format(today() - days(1), "%Y%m%d")

# Make sure date range above does not exceed date range in 
# 02_02_get_epa_station_locations.R. If it does, first process EPA station 
# locations so that it does not. Otherwise, stations may be dropped from the 
# sample that should not be dropped.

# Set dates to use as available inputs
start_input = "20060101" # "19990101"
end_input = "20231231" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

input_year_months = unique(format(seq.Date(ymd(start_input), ymd(end_input), by = "day"), "%Y%m"))

# Load station locations
epa_ll = read_sf(file.path(path_data, "EPA", "epa_station_locations")) %>%
  rename(id = stn_id, state_code = stat_cd, county_code = cnty_cd, cbsa_code = cbsa_cd) %>% 
  filter(!is.na(grid_10km)) # drop outside the contiguous US

# Load dates on which smoke data are not online
smoke_missing_dates = ymd(readRDS(file.path(path_data, "smoke", "smoke_dates_not_online_update.rds")))

# Load PM2.5 from EPA stations
epa_pm = input_year_months %>% map_dfr(function(year_month) {
  print(year_month)
  year = as.integer(substr(year_month, 1, 4))
  month = as.integer(substr(year_month, 5, 6))
  epa_pm = readRDS(file.path(path_data, "EPA", "station_level_pm25_data", sprintf("epa_station_level_pm25_data_%s_%s.rds", year, month)))
  epa_pm = epa_pm %>% 
    mutate(date = mdy(date)) %>% 
    filter(date >= ymd(start_input), 
           date <= ymd(end_input), 
           date >= ymd(start_date) - days(2), 
           date <= ymd(end_date) + days(2), 
           id %in% epa_ll$id)
  return(epa_pm)
})

# Expand to full station-day panel
epa_panel = expand.grid(id = epa_ll %>% filter(id %in% unique(epa_pm$id)) %>% pull(id), 
                        date = seq.Date(max(ymd(start_date) - days(2), ymd(start_input)), 
                                        min(ymd(end_date) + days(2), ymd(end_input)), 
                                        by = "day")) %>% 
  left_join(epa_pm, by = c("id", "date"))

# Load smoke data
smoke = input_year_months %>% map_dfr(function(year_month) {
  print(year_month)
  year = substr(year_month, 1, 4)
  month = substr(year_month, 5, 6)
  smoke_file = file.path(path_data, "smoke", "combined", sprintf("smoke_plumes_sfdf_%s_%s.RDS", year, month))
  if (file.exists(smoke_file)) {
    smoke = readRDS(smoke_file)
    smoke = smoke %>% 
      mutate(date = ymd(date)) %>% 
      filter(date >= ymd(start_input), 
             date <= ymd(end_input), 
             date >= ymd(start_date) - days(2), 
             date <= ymd(end_date) + days(2))
  } else {
    smoke = NULL
  }
  return(smoke)
})
plume_start = min(smoke$date)

# Get smoke days at station locations
# Takes 1-2 minutes
station_smoke <- st_intersects(epa_ll %>% filter(id %in% unique(epa_pm$id)), smoke) %>% 
  map(function(i) {
    smoke[i,] %>% pull(date) %>% unique()
  })
#double check NA's on station locations
station_smokedays <- epa_ll %>% 
  filter(id %in% unique(epa_pm$id)) %>% 
  st_drop_geometry() %>%
  select(id) %>% 
  mutate(date = station_smoke) %>% 
  unnest(date) %>% 
  mutate(plume = 1)

# Calculate smoke PM2.5
smoke_pm <- epa_panel %>% 
  mutate(year = year(date), month = month(date)) %>% 
  left_join(station_smokedays) %>% 
  # Replace missing plume info with 0s, except on missing smoke data days, or 
  # outside of plume date ranges
  replace_na(list(plume = 0)) %>% 
  mutate(plume = ifelse(date %in% smoke_missing_dates, NA, plume), 
         plume = ifelse((date > ymd(end_input)) | (date < ymd(start_input)) | (date < plume_start), NA, plume)) %>% 
  # Calculate non-smoke medians, i.e. background PM
  # Drop station-days not observed, dates before start of plume data, and dates 
  # where plume data were otherwise not online
  {left_join(., 
             nonsmoke_medians(filter(., !is.na(pm25), !is.na(plume)), 
                              pm25, plume, id, month, year), 
             by = c("id", "month", "year"))} %>% 
  # Fill in smoke days
  mutate(smoke_day = ifelse(date %in% smoke_missing_dates, 0, plume), 
         plume_lag1 = lag(plume, 1), 
         plume_lag2 = lag(plume, 2), 
         plume_lead1 = lead(plume, 1), 
         plume_lead2 = lead(plume, 2), 
         plume_tnn = ifelse(date %in% smoke_missing_dates, 
                            as.integer(ifelse(
                              (!is.na(plume_lag1)) & (!is.na(plume_lead1)),
                              (plume_lag1 == 1) & (plume_lead1 == 1),
                              ifelse(is.na(plume_lag1), T, plume_lag1 == 1) & 
                                ifelse(is.na(plume_lead1), T, plume_lead1 == 1) & 
                                ifelse(is.na(plume_lag2), T, plume_lag2 == 1) & 
                                ifelse(is.na(plume_lead2), T, plume_lead2 == 1)
                            )), 
                            plume), 
         filled_smoke_day = plume_tnn, 
         # Calculate PM anomalies and smokePM
         pm25_anom = pm25 - pm25_med_3yr, 
         smokePM = ifelse(smoke_day, pmax(pm25_anom, 0), 0), 
         filled_smokePM = ifelse(filled_smoke_day, pmax(pm25_anom, 0), 0)) %>% 
  filter(date >= ymd(start_date), 
         date <= ymd(end_date)) %>% 
  left_join(epa_ll %>% 
              filter(id %in% unique(epa_pm$id)) %>% 
              st_drop_geometry(), 
            by = "id") %>% 
  select(id, date, pm25, 
         smoke_day, smokePM, filled_smoke_day, filled_smokePM, 
         pm25_anom, nobs_3yr, pm25_med_3yr, 
         daily_obs_count, percent_complete, aqi, #frm, frm_like, 
         lon, lat, state, state_code, county, county_code, cbsa, cbsa_code, 
         grid_id_10km = grid_10km)

# Save
saveRDS(smoke_pm, 
        file.path(path_data, "EPA", "station_smokePM_panel", 
                  sprintf("epa_station_day_totalPM_smokePM_panel_%s-%s.rds", 
                          start_date, end_date))
)
