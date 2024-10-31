source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("Local_github/scripts/setup/00_03_load_paths.R")
#source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Jessica Li
# Gets ERA5 variables over 10 km grid by month.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to grid
start_month = "2006-01" # "1950-01"
end_month = "2023-12" # format(today() - months(2), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

# Read in project grid
project_grid0 = read_sf("~/BurkeLab Dropbox/Projects/smokePM-prediction/data/1_grids/10km_grid/10km_grid.shp")

# Define function for getting an ERA5 variable over 10 km grid
get_over_grid <- function(dataset, variable, statistic, time_zone, path_in = path_era5, path_out = file.path(path_data, "ERA5_variables")) {
  # Confirm dataset
  stopifnot(dataset %in% c("global", "land"))
  dataset_dir <- str_to_title(dataset)
  time_zone_dir <- gsub(":", "", time_zone)
  
  # Load ERA5 data
  path_in <- file.path(path_in, dataset_dir, variable, "USA", "raw", time_zone_dir,
                       paste0(statistic, "_of_1-hourly"))
  file_names <- list.files(path_in, full.names = TRUE, pattern = "\\.nc$")
  
  file_year_months = gsub(sprintf("^reanalysis.*%s_|\\.nc$", statistic), "", basename(file_names))
  grid_year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y_%m")
  
  dat_variable = raster(file_names[file_year_months %in% grid_year_months][1], layer = 1)
  
  # Get project grid in same CRS
  project_grid <- spTransform(project_grid0 %>% as_Spatial(), crs(dat_variable))
  
  # Match project grid cell to overlapping ERA5 grid cell
  project_grid$cell_era5 <- cellFromXY(dat_variable, coordinates(project_grid))
  
  # Find nearest neighbor non-NA ERA5 grid cell
  cell_era5_nna <- which(!is.na(values(dat_variable)))
  nn <- get.knnx(coordinates(dat_variable)[cell_era5_nna,],
                 coordinates(project_grid),
                 k = 1)
  cell_era5_na <- which(is.na(values(dat_variable)))
  cell_proj_na <- project_grid$cell_era5 %in% cell_era5_na
  
  # Match to NN non-NA ERA5 grid cell in 1 degree if overlap value is NA
  project_grid$cell_era5 <- ifelse(cell_proj_na & nn$nn.dist <= 1,
                                   cell_era5_nna[nn$nn.index],
                                   project_grid$cell_era5)
  
  for (grid_year_month in grid_year_months) {
    file_out = file.path(
      path_out, dataset_dir, variable, "USA", "10km_grid", time_zone_dir, paste0(statistic, "_of_1-hourly"),
      paste0(paste("grid", variable, statistic, grid_year_month, sep = "_"), ".rds"))
    preexisting = file.exists(file_out)
    if (overwrite | !preexisting) {
      # Work on one month at a time
      print(paste("Working on:", variable, statistic, grid_year_month, Sys.time()))
      
      dat_variable = stack(file_names[file_year_months == grid_year_month])
      
      # Get daily value in each project grid cell
      start_time <- get_start_time()
      era5_values <- dat_variable[project_grid$cell_era5]
      print_time(start_time)
      
      # Reshape for merging later
      gridded_values <- data.frame(id_grid = project_grid$ID,
                                   era5_values) %>% 
        pivot_longer(cols = -id_grid,
                     names_to = "date",
                     values_to = variable) %>% 
        mutate(date = as.Date(date, format = "X%Y.%m.%d", tz = "UTC"))
      
      # Save daily gridded values
      saveRDS(gridded_values, file_out)
    }
  }
}

# Get variables over project grid
get_over_grid("global", "boundary_layer_height", "daily_mean", "UTC-06:00")
get_over_grid("global", "boundary_layer_height", "daily_minimum", "UTC-06:00")
get_over_grid("global", "boundary_layer_height", "daily_maximum", "UTC-06:00") 
get_over_grid("global", "mean_sea_level_pressure", "daily_mean", "UTC-06:00")
get_over_grid("land", "2m_temperature", "daily_mean", "UTC-06:00")
get_over_grid("land", "2m_dewpoint_temperature", "daily_mean", "UTC-06:00")
get_over_grid("land", "10m_u_component_of_wind", "daily_mean", "UTC-06:00")
get_over_grid("land", "10m_v_component_of_wind", "daily_mean", "UTC-06:00")
get_over_grid("land", "surface_pressure", "daily_mean", "UTC-06:00")
get_over_grid("land", "total_precipitation", "daily_maximum", "UTC+00:00")
