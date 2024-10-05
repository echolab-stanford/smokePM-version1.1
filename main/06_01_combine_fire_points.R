source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")


# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Combines fire points by month.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to combine
start_month = "2006-01" # "2003-04"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

setwd(path_fire)

files <- list.files(pattern = "^hms_fire[12][0-9]{3}[01][0-9][0-3][0-9]\\.shp$")
file_year_months = gsub("^hms_fire|[0-3][0-9]\\.shp$", "", files)
year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y%m")

for (year_month in year_months) {
  year = substr(year_month, 1, 4)
  month = substr(year_month, 5, 6)
  
  #Directory to save combined files
  fire_dir = file.path(path_data, "fire", "combined_from_shp")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(fire_dir)) {
    dir.create(fire_dir, recursive = TRUE)
  }
  
  fire_file = file.path(fire_dir, sprintf("hms_fires_%s_%s.RDS", year, month))
  preexisting = file.exists(fire_file)
  
  if (overwrite | !preexisting) {
    
    files_ym = files[file_year_months == year_month]
    
    # create fire list to populate
    fire <- list()
    length(fire) <- length(files_ym)
    names(fire) <- substr(files_ym, 9, 16)
    
    # loop through files and save
    for (i in 1:length(files_ym)) {
      try(fire[[i]] <- st_read(files_ym[i]))
      try(fire[[i]]$date <- as.numeric(substr(files_ym[i],9,16)))
    }
    
    saveRDS(fire, fire_file)
  }
}

# ------------------------------------------------------------------------------
# Only use shapefiles for smokePM-prediction to avoid duplication. Text files 
# are simply useful for FRP data

setwd(file.path(path_fire, "txt")) #create folder txt on path_fire

files <- list.files(pattern = "^hms(_fire)?[12][0-9]{3}[01][0-9][0-3][0-9]\\.txt$")
file_year_months = gsub("^hms(_fire)?|[0-3][0-9]\\.txt$", "", files)
year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y%m")

for (year_month in year_months) {
  year = substr(year_month, 1, 4)
  month = substr(year_month, 5, 6)
  
  
  fire_dir = file.path(path_data, "fire", "combined_from_txt")
  
  # Create the directory if it doesn't exist
  if (!dir.exists(fire_dir)) {
    dir.create(fire_dir, recursive = TRUE)
  }
  
  fire_file = file.path(fire_dir, sprintf("hms_fires_%s_%s.RDS", year, month))
  preexisting = file.exists(fire_file)
  
  if (overwrite | !preexisting) {
    
    files_ym = files[file_year_months == year_month]
    
    # create fire list to populate
    fire <- list()
    length(fire) <- length(files_ym)
    names(fire) <- gsub("^hms(_fire)?|\\.txt$", "", files_ym)
    
    # loop through files and save
    for (i in 1:length(files_ym)) {
      try(fire[[i]] <- read.csv(files_ym[i]))
      try(if (nrow(fire[[i]]) > 0) fire[[i]]$date <- gsub("^hms(_fire)?|\\.txt$", "", files_ym[i]))
    }
    
    saveRDS(fire, fire_file)
  }
}

setwd(path_github)
