source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Extracts percent land cover class over 10km and 1km grids.
#-------------------------------------------------------------------------------
nlcd <- ee$ImageCollection("USGS/NLCD_RELEASES/2016_REL") %>% 
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>% # use the 2013 land use classification
  ee$ImageCollection$first() %>% 
  ee$Image$divide(10) %>% # the tens digit has the class we want to use
  ee$Image$floor()
nlcd_crs <- nlcd$projection()
nlcd_res <- nlcd_crs$nominalScale()$getInfo()

# extraction on 10km grid ----
grid_10km <- ee$FeatureCollection(sprintf("users/%s/grid_10km/grid_10km_wgs84", gee_user))

# extract pct of each land cover in grid cell 
nlcd_10km_areas <- ee$Image$pixelArea()$addBands(nlcd)$reduceRegions( #add a band for pixel area
  collection = grid_10km, 
  reducer = ee$Reducer$sum()$group(groupField = 1, groupName = "landcover"), # sum over pixel areas, grouping by landcover
  scale = nlcd_res,
  crs = nlcd_crs
) %>% 
  ee$FeatureCollection$map(function(feat){
    feat$setGeometry(NULL) %>% return
  })

# in real version, would then drop geometry info from grid_areas to make file size smaller and export to google drive, then download to local
nlcd_10km_task <- ee_table_to_drive(
  collection = nlcd_10km_areas,
  description = "NLCD_areas_10km_grid",
  fileFormat = "CSV"
)

# start the task
nlcd_10km_task$start()

#-#-----------------------------------------------------------------------------
# Check the status of the task
nlcd_10km_task$status()$state

# Once task is "COMPLETED", download it to local folder
retry::wait_until(
  expr = nlcd_10km_task$status()$state == "COMPLETED", 
  interval = 60
  )
ee_drive_to_local(nlcd_10km_task, 
                  file.path(path_data, "2_from_EE", paste0(nlcd_10km_task$status()$description, ".csv")))
#-#-----------------------------------------------------------------------------
#alternative to download from google drive if task was completed out of a session
library(googledrive)

EE_folder<- drive_find(pattern = 'rgee', type = 'folder')
nlcd_file <- drive_ls(EE_folder, pattern = "NLCD_areas")

# Download file from the folder to local folder
drive_download(file = nlcd_file, path = file.path(path_data, "2_from_EE", "NLCD_areas_10km_grid_2024_11_01_14_26_17.csv"))

#-#-----------------------------------------------------------------------------


# extraction on 1km grid ----
subgrid_1km_tasks <- 1:50 %>% 
  purrr::map(function(subgrid_no){
    
    print(paste("Working on subgrid", subgrid_no))
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0(sprintf("users/%s/grid_aod_1km/grid_aod_1km_wgs84_", gee_user), subgrid_no))
    
    # for each day, calculate average for each grid cell
    nlcd_1km_areas <- ee$Image$pixelArea()$addBands(nlcd)$reduceRegions( #add a band for pixel area
      collection = subgrid, 
      reducer = ee$Reducer$sum()$group(groupField = 1, groupName = "landcover"), # sum over pixel areas, grouping by landcover
      scale = nlcd_res,
      crs = nlcd_crs
    )
    
    # make list of properties to export
    export_properties <- list("grid_id", "groups")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("NLCD_areas_1km_subgrid",
                         subgrid_no,
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = nlcd_1km_areas,
      description = output_name,
      fileFormat = "CSV",
      selectors = export_properties
    )
    
    # start the task
    print(paste("saving file as", output_name))
    grid_task$start()
    
    # save the task information for later retrieval of output
    return(grid_task) 
    
  })

#-#-----------------------------------------------------------------------------
# Check the status of the task
map_chr(subgrid_1km_tasks, function(x) x$status()$state)
retry::wait_until(
  expr = all(map_chr(subgrid_1km_tasks, function(x) x$status()$state) == "COMPLETED"), 
  interval = 60
  )

# Once task is "COMPLETED", download it to local folder
output_local <- file.path(path_data, "2_from_EE", "NLCD_1km_subgrid")
subgrid_1km_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    file.path(output_local, paste0(t$status()$description, ".csv")))
})
#-#-----------------------------------------------------------------------------
#alternative to download from google drive if task was completed out of a session
# Download each file from the folder to local folder
nlcd_subgrids<- drive_ls(EE_folder, pattern = "NLCD_areas_1km_subgrid_1")

for (file in nlcd_subgrids$name) {
  drive_download(file = file, path = file.path(path_data, "2_from_EE", file), overwrite = TRUE)
}
