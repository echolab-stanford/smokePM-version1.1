source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Jessica Li
# Gets dates on which fire data are empty, not online, or clusters are too small.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set dates to use as available inputs
start_input = "20060101" # "20030401"
end_input = "20231231" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

all_dates = format(seq.Date(ymd(start_input), ymd(end_input), by = "day"), "%Y%m%d")
input_year_months = format(seq.Date(ym(substr(start_input, 1, 6)), ym(substr(end_input, 1, 6)), by = "month"), "%Y_%m")

# Dates not online
fire_list = list.files(file.path(path_data, "fire", "combined_from_shp"))
file_year_months = gsub("^hms_fires_|\\.RDS$", "", fire_list)
fire_list = fire_list[file_year_months %in% input_year_months]

dates_online = unlist(lapply(file.path(path_data, "fire", "combined_from_shp", fire_list), function(file) {
  print(basename(file))
  fires = readRDS(file)
  fires = fires[ymd(names(fires)) %within% (ymd(start_input) %--% ymd(end_input))]

  dates = names(fires)

  return(dates)
}))
dates_not_online = setdiff(all_dates, dates_online)
saveRDS(dates_not_online, file.path(path_data, "fire", "fire_dates_not_online.rds"))

# Dates that are empty files
dates_empty_data = unlist(lapply(file.path(path_data, "fire", "combined_from_shp", fire_list), function(file) {
  print(basename(file))
  fires = readRDS(file)
  fires = fires[ymd(names(fires)) %within% (ymd(start_input) %--% ymd(end_input))]

  dates = names(fires)[which(sapply(fires, nrow) == 0)]

  return(dates)
}))
saveRDS(dates_empty_data, file.path(path_data, "fire", "fire_dates_empty_data.rds"))

# Dates when all clusters are too small
min_size <- 100.92
if (!("min_size" %in% ls())) {
  files <- list.files(file.path(path_data, "fire", "clusters"), pattern = "^clusters", full.names = TRUE)
  fire_clusters <- readRDS(files[1])
  min_size <- min(fire_clusters$area, na.rm = T) * 3
}

cluster_list = list.files(file.path(path_data, "fire", "clusters"))
file_year_months = gsub("^clusters_|\\.RDS$", "", cluster_list)
cluster_list = cluster_list[file_year_months %in% input_year_months]

dates_clusters_too_small = unlist(lapply(file.path(path_data, "fire", "clusters", cluster_list), function (file) {
  print(basename(file))
  clusters = readRDS(file)
  clusters = clusters@data %>% #had to add the @data here because the file is a spatial polygon data frame
    dplyr::filter(ymd(date) %within% (ymd(start_input) %--% ymd(end_input)))

  dates_downloaded = unique(clusters$date)
  dates_clusters_large_enough = unique(clusters[clusters$area > min_size,]$date)
  dates_clusters_too_small = setdiff(dates_downloaded, dates_clusters_large_enough)

  return(dates_clusters_too_small)
}))
saveRDS(dates_clusters_too_small, file.path(path_data, "fire", "fire_dates_clusters_too_small.rds"))
