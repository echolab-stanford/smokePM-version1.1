library(lubridate)
library(sf) #0.9-8
library(tibble)
library(dplyr)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Combines smoke plumes.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to combine
start_month = "2006-01" # "2005-08"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

setwd(path_smoke)

# Get file names
files <- list.files(pattern = "^hms_smoke[12][0-9]{3}[01][0-9][0-3][0-9]\\.shp$")
file_year_months = gsub("^hms_smoke|[0-3][0-9]\\.shp$", "", files)

# Get months to combine
year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y%m")

for (year_month in year_months) {
  start_time = get_start_time(paste("Started", year_month))
  year = substr(year_month, 1, 4)
  month = substr(year_month, 5, 6)
  
  # Check if output file already exists
  smoke_file = file.path(path_data, "smoke", "combined", sprintf("smoke_plumes_sfdf_%s_%s.RDS", year, month))
  preexisting = file.exists(smoke_file)
  
  if (overwrite | !preexisting) {
    
    # Get files for the month
    files_ym = files[file_year_months == year_month]
    
    # Create smoke list to populate
    smoke <- list()
    length(smoke) <- length(files_ym)
    names(smoke) <- gsub("^hms_smoke|\\.shp$", "", files_ym)
    
    # Loop through daily files
    # Takes ~15 minutes
    progress = txtProgressBar(min = 0, max = length(files_ym), style = 3)
    for (i in 1:length(files_ym)) {
      file = files_ym[i]
      df <- read_sf(file)
      date = gsub("^hms_smoke|\\.shp$", "", file)
      
      # Repair geometry by closing the polygon
      if (nrow(df) > 0 & all(is.na(st_is_valid(df)))) {
        for (j in 1:nrow(df)) {
          df[j,]$geometry[[1]][[1]] <- rbind(df[j,]$geometry[[1]][[1]], 
                                             df[j,]$geometry[[1]][[1]][1,])
        }
      }
      
      # Repair geometry by closing the polygon
      if (nrow(df) > 0 & any(!st_is_valid(df), na.rm = T)) {
        for (j in which(!st_is_valid(df))) {
          geo_vals = unlist(df[j,]$geometry)
          geo_vals = matrix(geo_vals, length(geo_vals)/2, 2)
          df[j,]$geometry[[1]] = st_polygon(list(rbind(geo_vals, 
                                                       geo_vals[nrow(geo_vals),])))
        }
      }
      
      # Drop invalid polygons
      df = df[!is.na(st_is_valid(df)),]
      df = st_cast(df, "POLYGON")
      
      # Append date
      df$date <- date
      
      # Append id
      if (nrow(df) > 0) {
        df = df %>% mutate(id = 1:nrow(df))
      } else {
        df = df %>% add_column(id = NA) %>% mutate(id = as.integer(id))
      }
      
      for (col in c("Start", "End", "Density", "Satellite")) {
        if (col %in% names(df)) {
          df = df %>% mutate(!!col := as.character(!!!syms(col)))
        } else {
          df = df %>% add_column(!!col := NA) %>% mutate(!!col := as.character(!!!syms(col)))
        }
      }
      
      # Append the day to the month
      df = df %>% select(date, id, Start, End, Density, Satellite, geometry)
      smoke[[date]] = df
      
      setTxtProgressBar(progress, i)
    }
    
    # Discard dates with no plumes
    not_NA = sapply(smoke, function(x){nrow(x) > 0})
    smoke = smoke[not_NA]
    
    # Turn the list of plumes into an sf data frame
    smoke_df = bind_rows(smoke) %>% select(date, Start, End, Density, Satellite, geometry)
    st_crs(smoke_df) = st_crs(smoke[[length(smoke)]])
    # st_crs(smoke_df) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
    
    # Save the parsed smoke data
    saveRDS(smoke_df, smoke_file)
  }
  print_time(start_time)
}

setwd(path_github)
