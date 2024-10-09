source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("Local_github/scripts/setup/00_03_load_paths.R")
source("Local_github/scripts/setup/00_04_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Splits 10 km and 1 km grids into chunks.
# ------------------------------------------------------------------------------
# Set path to asset grids
gee_user = "SmokePM"
asset_folder_10km <- sprintf("users/%s/grid_10km/", gee_user)
asset_folder_1km <- sprintf("users/%s/grid_aod_1km/", gee_user)

grid_10km <- ee$FeatureCollection(paste0(asset_folder_10km, "grid_10km_wgs84"))
grid_1km <- ee$FeatureCollection(paste0(asset_folder_1km, "grid_aod_1km_wgs84_training"))

split_grids_10km <- 
  cut(1:grid_10km$size()$getInfo(), breaks = 10) %>% 
  as.numeric() %>% 
  table %>% 
  as.data.frame() %>% 
  set_colnames(c("group", "size")) %>% 
  mutate(offset = cumsum(lag(size, default = 0))) %>% 
  purrr::pmap(function(group, size, offset){
    subgrid <- grid_10km$toList(count = size, offset = offset) %>% 
      ee$FeatureCollection()
    subgrid_task <- ee_table_to_asset(collection = subgrid, 
                                      description = paste0("grid_10km_wgs84_", group),
                                      assetId = paste0(asset_folder_10km, "grid_10km_wgs84_", group))
    subgrid_task$start()
    return(subgrid_task)
  })


split_grids_1km <- 
  cut(1:grid_1km$size()$getInfo(), breaks = 50) %>% # should make subgrids about 480K obs each
  as.numeric() %>% 
  table %>% 
  as.data.frame() %>% 
  set_colnames(c("group", "size")) %>% 
  mutate(offset = cumsum(lag(size, default = 0))) %>% 
  purrr::pmap(function(group, size, offset){
    subgrid <- grid_1km$toList(count = size, offset = offset) %>% 
      ee$FeatureCollection()
    subgrid_task <- ee_table_to_asset(collection = subgrid, 
                                      description = paste0("grid_aod_1km_wgs84_", group),
                                      assetId = paste0(asset_folder_1km, "grid_aod_1km_wgs84_", group))
    subgrid_task$start()
    return(subgrid_task)
  })

#-#-----------------------------------------------------------------------------
# check all the tasks have a status of "COMPLETED" before proceeding
map_chr(split_grids_10km, function(x) x$status()$state)
retry::wait_until(
  expr = all(map_chr(split_grids_10km, function(x) x$status()$state) == "COMPLETED"), 
  interval = 60
  )

map_chr(split_grids_1km, function(x) x$status()$state)
retry::wait_until(
  expr = all(map_chr(split_grids_1km, function(x) x$status()$state) == "COMPLETED"), 
  interval = 60
  )
#-#-----------------------------------------------------------------------------
