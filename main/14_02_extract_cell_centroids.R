source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Jessica Li
# Saves 1 km grid and 10 km grid cell centroids
#-------------------------------------------------------------------------------
# 1 km
start_time = get_start_time()
cell_cent <- st_read(file.path(path_data, "1_grids", "1km_aod_grid_wgs84")) %>%
  st_centroid %>%
  {cbind(.,
         st_coordinates(.))} %>%
  st_drop_geometry() %>%
  rename(grid_id_1km = grid_id,
         lat = Y, 
         lon = X)
print_time(start_time)

saveRDS(cell_cent, file.path(path_data, "1_grids", "1km_cell_centroids.rds"))

# 10 km
start_time = get_start_time()
cell_cent = st_read(file.path(path_data, "1_grids", "grid_10km_wgs84")) %>%
  st_centroid %>%
  {cbind(.,
         st_coordinates(.))} %>%
  st_drop_geometry() %>%
  rename(grid_id_10km = ID,
         lat = Y, 
         lon = X)
print_time(start_time)

saveRDS(cell_cent, file.path(path_data, "1_grids", "10km_cell_centroids.rds"))