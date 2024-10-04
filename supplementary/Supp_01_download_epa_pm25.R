library(tidyverse)
library(epair)
library(airnow)
setwd("~/Documents/smokePM-prediction")
source("./Local_github/scripts/setup/00_03_load_paths.R")

#------------------------------------------------------
#Written by Mariana Martins, August 2024
#using package epair and EPA AQS API to dowload station daily PM2.5 data by year.
#https://github.com/ropensci/epair/tree/master
#
#Register email to get the API key:
#1. run get_aqs_key("marianamartins0415@gmail.com") #You'll get an email with the key and a link to confirm the email.
#edit the R.environment file to include the email and key
#2. run usethis::edit_r_environ()
#3. copy the following to the R.environ file (withtou the #):
#aqs_api_key="baykit57"
#aqs_email="marianamartins0415@gmail.com"
#4. save and close that file, restart R session.
#---------------------------------------------------------------------
#epair::endpoints #this command shows a list of all datasets avalable with this API
#parameter for PM2.5: "88101" and "88502"
#https://aqs.epa.gov/aqsweb/documents/data_api.html#variables
#https://aqs.epa.gov/data/api/list/parametersByClass?email=test@aqs.api&key=test&pc=CRITERIA
#variables needed to call the daily summary: param, bdate, edate, state
#-----------------------------------------------------------------------------
#AirNow API allows us to download hourly estiamtes that can be used while AQS estimates are not avaialable - this should be double checked every 6 months
#after installing the package, run airnow::get_airnow_token(), it will give you the token and then you can either run airnow::set_airnow_token(token = "0084D31E-C203-414D-AA70-C045D45AA55F")
#or open the R.environ file and save it as AIRNOW_API_KEY = "0084D31E-C203-414D-AA70-C045D45AA55F" 
#AirNow API will only bring limited amount of data, we need to calibrate the area it's covering and the time, if increase the time, decrease the area and vice versa


#Time frame
bdate<- "20240101"
edate<- "20240630"

# Define the function to get PM data with EPA API
get_pm_data <- function(state, bdate, edate) {
  endpoint <- 'dailyData/byState'
  
  variable.list1 <- list("state" = state,  
                         "bdate" = bdate, 
                         "edate" = edate, 
                         "param" = '88101')  # parameter for PM2.5
  
  variable.list2 <- list("state" = state,  
                         "bdate" = bdate, 
                         "edate" = edate, 
                         "param" = '88502')  # another parameter for PM2.5
  
  result1 <- perform.call(endpoint = endpoint, variables = variable.list1)$Data
  result2 <- perform.call(endpoint = endpoint, variables = variable.list2)$Data
  
  if (length(result1) == 0 && length(result2) == 0) {
    message(paste("No data for state", state, "for parameters 88101 and 88502")) 
    return(list(status = "no_data", data = NULL))
  } 
  
  if (length(result1) > 0) {
    result1 <- result1 %>% 
      filter(observation_count == 1,
             pollutant_standard == "PM25 24-hour 2024") 
  }
  
  if (length(result2) > 0) {
    result2 <- result2 %>% 
      filter(observation_count == 1) 
  }
  
  if (length(result1) > 0 && length(result2) > 0) {
    combined_data <- bind_rows(result1, result2) 
    return(list(status = "data_found", data = combined_data))
  } else if (length(result1) > 0) {
    return(list(status = "data_found", data = result1))
  } else if (length(result2) > 0) {
    return(list(status = "data_found", data = result2))
  }
}



#not in contiguous US: "02","15", "11",
# Main script
states <- c("01",  "04", "05", "06", "08", "09", "10",  "12", "13",  "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")

start_year <- as.numeric(substr(bdate, 1, 4))
end_year <- as.numeric(substr(edate, 1, 4))
overwrite = FALSE
years = start_year:end_year

#create a folder for each year
for (i in 1:length(years)) {
  year = years[i]
  dir_name = file.path(path_epa, year)
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  } else if (overwrite) {
    unlink(dir_name, recursive = TRUE)
    dir.create(dir_name)
  }

  # save a file for each state 
  for (state in states) {
    pm_data_result <- get_pm_data(state, bdate = bdate, edate = edate) #run the function we created above
    
    if (pm_data_result$status == "no_data") { #trigger for the AirNow data
      # Load the previous year's data for the same state to get IDs and lat-lon box
      data23 <- readRDS(paste0(path_epa, "/", year - 1, "/epa_data_daily_", state, "_", year - 1, ".rds"))

      # Retrieve unique station IDs
      ID23 <- data23 %>% 
        mutate(aqs_code = paste0(840, state_code, county_code, site_number)) %>% 
        select(aqs_code) %>% unique() 
      
      # Define bounding box based on 2023 data - AirNow API used lat-lon box, not FIPS codes
      bbox <- c(
        min_longitude = min(data23$longitude, na.rm = TRUE) - 0.3, #add a buffer to make sure we cover all stations
        min_latitude = min(data23$latitude, na.rm = TRUE) - 0.4, #calibrate the buffer so the API doesn't break - if increasing box, gotta reduce time coverage
        max_longitude = max(data23$longitude, na.rm = TRUE) + 0.9,
        max_latitude = max(data23$latitude, na.rm = TRUE) + 0.4
      )
      
      # Retrieve data from AirNow API for the missing months
      all_months_data <- list()
      
      for (month in substr(bdate, 5, 6):substr(edate, 5, 6)) { 
        start_time <- sprintf("%d-%02d-01T00:00", year, month) #converts date to date-hour and define the first day of the month
        end_time <- sprintf("%d-%02d-%02dT23:00", year, month, as.integer(format(as.Date(paste0(year, "-", month, "-01")), "%d"))) #converts date to date-hour and define the first day of the following month
        
        tmp <- get_airnow_area(
          box = bbox, 
          parameters = "pm25",
          start_time = start_time, end_time = end_time,
          monitor_type = "both",
          data_type = "both",
          verbose = TRUE
        )
        
        data_month <- tmp %>% 
          filter(intl_aqs_code %in% ID23$aqs_code) %>% #Airnow brings more stations than AQS, here we filter based on previous AQS stations
          mutate(date = as.Date(datetime_observed),
                 time = format(datetime_observed, "%H:%M:%S")) %>% 
          group_by(intl_aqs_code, latitude, longitude, site_name, date, parameter, unit) %>% 
          summarise(arithmetic_mean = mean(value, na.rm = TRUE)) %>% #name the variable the same as AQS data
          rename(aqs_code = intl_aqs_code, 
                 local_site_name = site_name,
                 date_local = date,
                 units_of_measure = unit) %>% 
          ungroup() %>% 
          mutate(date_local = as.character(date_local))
        
        all_months_data[[month]] <- data_month
      }
      
      AirNow_data <- bind_rows(all_months_data)
      file_path <- paste0(path_epa,"/", year, "/epa_data_daily_", state, "_", year, ".rds")
      saveRDS(AirNow_data, file = file_path)
      message(paste("AirNow data for state", state, "in year", year, "saved to", file_path))
      
    } else {
      state_data <- pm_data_result$data
      file_path <- paste0(path_epa,"/", year, "/epa_data_daily_", state, "_", year, ".rds")
      saveRDS(state_data, file = file_path)
      message(paste("EPA data for state", state, "in year", year, "saved to", file_path))
    }
  }
}


