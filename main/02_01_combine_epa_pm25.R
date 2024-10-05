library(readr)
library(lubridate)
library(dplyr)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

# ------------------------------------------------------------------------------
# Written by: Sam Heft-Neal, Jessica Li - Updated by Mariana Martins Aug 2024 to follow the format of the API data
# Combines EPA station PM2.5 data by month.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to combine
start_month = "2006-01" # "1999-01"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

setwd(path_epa)

states <- c("01",  "04", "05", "06", "08", "09", "10",  "12", "13",  "16", "17", "18", "19", "20", "21", "22", "23", "24", 
            "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
            "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")

year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

for (year_month in year_months) {
  print(year_month) 
  year = as.numeric(substr(year_month, 1, 4))
  month = as.numeric(substr(year_month, 6, 7))
  
  monitor_file = file.path(path_data, "EPA", "monitor_level_pm25_data", sprintf("epa_monitor_level_pm25_data_%s_%s.rds", year, month))
  #monitor_file = file.path(path_epa, "monitor_level_pm25_data", sprintf("epa_monitor_level_pm25_data_%s_%s.rds", year, month))
  
  station_file = file.path(path_data, "EPA", "station_level_pm25_data", sprintf("epa_station_level_pm25_data_%s_%s.rds", year, month))
  #station_file = file.path(path_epa, "station_level_pm25_data", sprintf("epa_station_level_pm25_data_%s_%s.rds", year, month))
  
  monitor_preexisting = file.exists(monitor_file)
  station_preexisting = file.exists(station_file)
  
  if (overwrite | !monitor_preexisting | !station_preexisting) {
    
    if (overwrite | !monitor_preexisting) {
      first_loop = !("dat_m" %in% ls())
      same_year = !(first_loop)
      if (same_year) same_year = (year == unique(dat_m$year))
      if (first_loop | !same_year) {
        
        #### Combine across states at monitor level ####
        dat <- as.list(rep(NA, length(states)))
        
  
        for (j in 1:length(states)) {
          state = states[j]
          print(paste(year, state))
          dat[[j]] <- as.data.frame(readRDS(file.path(year, sprintf("epa_data_daily_%s_%s.rds", state, year))))
        }
        
        dat <- data.frame(data.table::rbindlist(dat, fill = TRUE)) 
        
        #add date variables
        dat$year <- as.numeric(substr(dat$date_local, 1, 4))
        dat$month <- as.numeric(substr(dat$date_local, 6, 7))
        dat$day <- as.numeric(substr(dat$date_local, 9,10))
        
        datmth <- dat$month
        datmth[nchar(datmth)==1]<-paste("0",datmth[nchar(datmth)==1], sep= "")
        datdy <- dat$day
        datdy[nchar(datdy)==1]<-paste("0",datdy[nchar(datdy)==1], sep= "")
        
        #change the format of the date variable
        dat$date <- paste(datmth, datdy, dat$year, sep = "")
        
        dat %>% 
          filter(is.na(state_code))
        #rename and select variables
        names(dat)[names(dat)=="latitude"]<-"lat"
        names(dat)[names(dat)=="longitude"]<-"lon"
        names(dat)[names(dat)=="arithmetic_mean"]<-"pm25"
        names(dat)[names(dat)=="observation_count"]<-"daily_obs_count"
        names(dat)[names(dat)=="observation_percent"]<-"percent_complete"
        names(dat)[names(dat)=="state_code"]<-"state_code"
        names(dat)[names(dat)=="state"]<-"state"
        names(dat)[names(dat)=="site_number"]<-"station_id"
        names(dat)[names(dat)=="poc"]<-"monitor_id"
        names(dat)[names(dat)=="aqi"]<-"aqi"
        names(dat)[names(dat)=="cbsa_code"]<-"cbsa_code"
        names(dat)[names(dat)=="cbsa"]<-"cbsa"
        names(dat)[names(dat)=="county_code"]<-"county_code"
        names(dat)[names(dat)=="county"]<-"county"
        
        dat <- dat %>% 
          select(date, station_id, monitor_id, year, month, day, lat, lon, 
                 state, state_code, 
                 county, county_code, 
                 cbsa, cbsa_code, 
                 pm25, aqi, aqs_code, 
                 daily_obs_count, percent_complete) %>% 
          mutate(station_id = case_when(!is.na(state_code) ~ paste0(state_code, county_code, station_id),
                                        is.na(state_code) ~ substr(aqs_code, 4, 12)))
      } 
      dat_m = dat %>% filter(year == !!year, month == !!month)
      
      write_rds(dat_m, file = monitor_file)
    }
    
    if (overwrite | !station_preexisting) {
      #### Aggregate to station level ####
      # Takes 5-6 minutes
      if (!(overwrite | !monitor_preexisting)) dat_m = readRDS(monitor_file)
      dat_s = dat_m %>% 
        select(id = station_id, date, pm25, aqi, 
               daily_obs_count, percent_complete) %>% 
        group_by(id, date) %>% 
        summarize(across(c(pm25, daily_obs_count, percent_complete, aqi), mean)) %>% 
        ungroup()
      
      write_rds(dat_s, file = station_file)
    }
  }
}

setwd(path_github)


#If needed:
#rename files when they are named with one digit only for months<10 / do it for monitor_level and station_level
# Set the directory where your files are located
# directory <- file.path(path_data, "EPA", "monitor_level_pm25_data")
# 
# # List all the files that match the pattern
# files <- list.files(directory, pattern = "^epa_monitor_level_pm25_data", full.names = TRUE)
# 
# # Loop through each file and rename it
# for (file in files) {
#   # Extract the year and month from the filename
#   file_name <- basename(file)
#   parts <- strsplit(file_name, "_|\\.")[[1]]
#   year <- parts[6]
#   month <- parts[7]
#   
#   new_month <- sprintf("%02d", as.numeric(month))
#   
#   # Create the new file name
#   new_file_name <- sprintf("epa_monitor_level_pm25_data_%s_%s.rds", year, new_month)
#   new_file_path <- file.path(directory, new_file_name)
#   
#   # Rename the file
#   file.rename(file, new_file_path)
# }
