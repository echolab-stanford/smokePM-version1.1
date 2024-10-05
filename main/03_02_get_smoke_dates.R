library(lubridate)
library(sf) #0.9-8
library(tibble)
library(dplyr)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Gets dates on which smoke data are empty, not online, repaired, or with Density 
# column.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set dates to use as available inputs
start_input = "20060101" # "20050805"
end_input = "20231231" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

all_dates = format(seq.Date(ymd(start_input), ymd(end_input), by = "day"), "%Y%m%d")
input_range = (ymd(start_input) %--% ymd(end_input))
files = list.files(path_smoke, pattern = "\\.shp$")
file_dates = gsub("^hms_smoke|\\.shp$", "", files)
files = files[ymd(file_dates) %within% input_range]
file_dates = gsub("^hms_smoke|\\.shp$", "", files)

# Dates not online
dates_not_online = setdiff(all_dates, file_dates)

saveRDS(dates_not_online, file.path(path_data, "smoke", "smoke_dates_not_online_update.rds"))

# Dates that are empty files
not_empty = vector("logical", length(files))

p = txtProgressBar(min = 0, max = length(files), style = 3)
for (i in 1:length(files)) {
  df = read_sf(file.path(path_smoke, files[i]))
  not_empty[i] = (nrow(df) > 0)

  setTxtProgressBar(p, i)
}
dates_empty_data = file_dates[!not_empty]

saveRDS(dates_empty_data, file.path(path_data, "smoke", "smoke_dates_empty_data_update.rds"))

# Dates with Density column
dates_density = vector("list", length(files))

p = txtProgressBar(min = 0, max = length(files), style = 3)
for (i in 1:length(files)) {
  df = read_sf(file.path(path_smoke, files[i]))
  dates_density[[i]] = names(df)

  setTxtProgressBar(p, i)
}
dates_density = sapply(dates_density, function(x) "Density" %in% x)
dates_density = file_dates[dates_density]

saveRDS(dates_density, file.path(path_data, "smoke", "smoke_dates_density_data_update.rds"))

# Dates with repaired geometry
dates_repaired_geometry = vector("list", length(files))

p = txtProgressBar(min = 0, max = length(files), style = 3)
for (i in 1:length(files)) {
  df = read_sf(file.path(path_smoke, files[i]))
  all_invalid = ((nrow(df) > 0) & all(is.na(st_is_valid(df))))
  any_invalid = ((nrow(df) > 0) & any(!st_is_valid(df), na.rm = T)) # this wasn't in there before...
  dates_repaired_geometry[[i]] = (all_invalid | any_invalid)

  setTxtProgressBar(p, i)
}
dates_repaired_geometry = file_dates[unlist(dates_repaired_geometry)]

saveRDS(dates_repaired_geometry, file.path(path_data, "smoke", "smoke_dates_repaired_geometry_update.rds"))
