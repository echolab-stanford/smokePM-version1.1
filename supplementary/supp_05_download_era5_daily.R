library(stringr)
library(lubridate)
source("./scripts/setup/00_02_load_functions.R")
source("./scripts/setup/00_03_load_paths.R")

env_name = "era5"
if (!(env_name %in% reticulate::conda_list()$name)) {
  reticulate::conda_create(env_name)
}
reticulate::use_condaenv(env_name, required = T)
library(reticulate)
if (!py_module_available("cdsapi")) py_install("cdsapi", forge = T)
if (!py_module_available("tenacity")) py_install("tenacity", forge = T)
source_python(file.path(path_supplementary, "download_era5_daily.py"))

#-------------------------------------------------------------------------------
# Written by: Jessica Li
# Downloads ERA5 daily data using the "Daily statistics calculated from ERA5 
# data" application 
# (https://cds.climate.copernicus.eu/cdsapp#!/software/app-c3s-daily-era5-statistics?tab=app)
# accessed via the CDS Python API by month.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to download
start_month = "2024-01" # "1950-01"
end_month = "2024-06" # format(today() - months(2), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

# Output directory
path_out <- path_era5

# Time zone
# Contiguous US time zones range UTC-04:00 (EDT) to UTC-08:00 (PST), so we use
# UTC-06:00 (CST/MDT)
time_zone <- "UTC-06:00"

# Contiguous US
north <- 49.92
south <- 24.43
east <- -66.62
west <- -125.5

#-------------------------------------------------------------------------------
#### Retrieve data ####
#-------------------------------------------------------------------------------
retrieve_era5 <- function(variable, statistic, dataset, pressure_level = "-", path_log = "~/Desktop/") {
  if (variable == "total_precipitation") stopifnot(time_zone == "UTC+00:00")
  time_zone_dir <- gsub(":", "", time_zone)
  
  stopifnot(dataset %in% c("global", "land", "pressure"))
  dataset_dir <- str_to_title(dataset)
  area <- list(lat = c(south, north), lon = c(west, east))
  statistic_run <- statistic
  
  # Full dataset name
  if (dataset == "global") {
    dataset = "reanalysis-era5-single-levels"
  } else if (dataset == "land") {
    dataset = "reanalysis-era5-land"
  } else if (dataset == "pressure") {
    dataset = "reanalysis-era5-pressure-levels"
  }
  
  # Start logging output and messages to console
  warn_option <- getOption("warn")
  options(warn = 1)
  log_file_name <- file.path(path_log, sprintf("%s_%s_%s.log", dataset, variable, statistic))
  try(log_file <- file(log_file_name, open="at")) # open file for appending in text mode
  sink(log_file, type = "output", append = TRUE, split = TRUE)
  sink(log_file, type = "message", append = TRUE)
  
  year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")
  
  for (year_month in year_months) {
    # year and month must be passed in as integers, e.g. 2020L, 3L
    year = as.integer(substr(year_month, 1, 4))
    month = as.integer(substr(year_month, 6, 7))
    
    folder = file.path(path_out, dataset_dir, variable, "USA", "raw", time_zone_dir,
                       paste0(statistic, "_of_1-hourly"))
    file = sprintf("%s_%s_%s_%s_%s.nc", dataset, variable, statistic, 
                   str_pad(year, 4, "left", 0), str_pad(month, 2, "left", 0))
    preexisting = file.exists(file.path(folder, file))
    
    if (overwrite | !preexisting) {
      
      start_time <- get_start_time(paste("Started:", variable, year, month))
      retrieve_era5_daily(
        dataset = dataset,
        variable = variable,
        pressure_level = pressure_level,
        statistic = statistic_run,
        year = year,
        month = month,
        time_zone = time_zone,
        area = area,
        folder = folder
      )
      
      run_time <- print_time(start_time)
      
      # Wait a bit before next retrieval if current retrieval ran fast/from cache
      if (run_time < minutes(1)) Sys.sleep(10)
      
    }
  }
  
  # Stop logging
  sink(type = "output")
  sink(type = "message")
  close(log_file)
  options(warn = warn_option)
}

#-------------------------------------------------------------------------------
#### ERA5 ####
# Takes 2-5 minutes per month
retrieve_era5("boundary_layer_height", "daily_mean", "global")
retrieve_era5("boundary_layer_height", "daily_minimum", "global")
retrieve_era5("boundary_layer_height", "daily_maximum", "global")
retrieve_era5("mean_sea_level_pressure", "daily_mean", "global")

#### ERA5-Land ####
# Takes 15-90 minutes per month
retrieve_era5("2m_dewpoint_temperature", "daily_mean", "land")#done
retrieve_era5("2m_temperature", "daily_mean", "land")#done
retrieve_era5("surface_pressure", "daily_mean", "land")#done
retrieve_era5("10m_u_component_of_wind", "daily_mean", "land") #done
retrieve_era5("10m_v_component_of_wind", "daily_mean", "land") #done

# ERA5-Land total_precipitation is cumulative
time_zone <- "UTC+00:00"
retrieve_era5("total_precipitation", "daily_maximum", "land")#done
