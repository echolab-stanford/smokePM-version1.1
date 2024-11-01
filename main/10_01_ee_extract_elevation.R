source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("Local_github/scripts/setup/00_03_load_paths.R")
source("Local_github/scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Extracts elevation over 10km and 1km grids.
#-------------------------------------------------------------------------------
elev <- ee$Image("USGS/NED")

gee_user = "SmokePM"

# extraction for 10km grid ----
grid_10km <- ee$FeatureCollection(sprintf("users/%s/grid_10km/grid_10km_wgs84", gee_user))

#this didn't work
# comb_reducer <- ee$Reducer$combine(reducer1 = ee$Reducer$mean(),
#                                    reducer2 = ee$Reducer$stdDev(),
#                                    outputPrefix = "stdDev_",
#                                    sharedInputs = TRUE)

#ChatGPT fix
comb_reducer <- ee$Reducer$mean()$combine(
  reducer2 = ee$Reducer$stdDev(),
  sharedInputs = TRUE
)

# avg elevation 
grid_10km_elev <- elev$reduceRegions(collection = grid_10km, 
                                     reducer = comb_reducer, 
                                     scale = elev$projection()$nominalScale())

elev_10km_task <- ee_table_to_drive(
  collection = grid_10km_elev,
  description = "elevation_avg_sd_10km_grid",
  fileFormat = "CSV"
)

elev_10km_task$start()

#-#-----------------------------------------------------------------------------
# Check the status of the task
elev_10km_task$status()$state

# Once task is "COMPLETED", download it to local folder
retry::wait_until(
  expr = elev_10km_task$status()$state == "COMPLETED", 
  interval = 60)
ee_drive_to_local(elev_10km_task, 
                  file.path(path_data, "2_from_EE", paste0(elev_10km_task$status()$description, ".csv")))
#-#-----------------------------------------------------------------------------
#alternative to download from google drive if task was completed out of a session
library(googledrive)

EE_folder<- drive_find(pattern = 'rgee', type = 'folder')
elev_file <- drive_ls(EE_folder, pattern = "elevation_avg")

# Download file from the folder to local folder
drive_download(file = elev_file, path = file.path(path_data, "2_from_EE", "elevation_avg_sd_10km_grid_2024_10_31_17_05_00.csv"))

#-#-----------------------------------------------------------------------------

# extraction for 1km grid ----
subgrid_1km_tasks <- 1:50 %>% 
  purrr::map(function(subgrid_no){subgrid_no = 1
    
    print(paste("Working on subgrid", subgrid_no))
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0(sprintf("users/%s/grid_aod_1km/grid_aod_1km_wgs84_", gee_user), subgrid_no))
    
    # calculate average for each grid cell
    grid_1km_elev <- elev$reduceRegions(collection = subgrid,
                                        reducer = comb_reducer, 
                                        scale = elev$projection()$nominalScale())
    
    # make list of properties to export
    export_properties <- list("grid_id", "mean", "stdDev_stdDev")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("elevation_avg_sd_1km_subgrid",
                         subgrid_no,
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = grid_1km_elev,
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
output_local <- file.path(path_data, "2_from_EE", "elevation_1km_subgrid")
subgrid_1km_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    file.path(output_local, paste0(t$status()$description, ".csv")))
})
#-#-----------------------------------------------------------------------------
#alternative to download from google drive if task was completed out of a session
# Download each file from the folder to local folder
elev_subgrids<- drive_ls(EE_folder, pattern = "elevation_avg_sd_1km_subgrid")

for (file in elev_subgrids$name) {
  drive_download(file = file, path = file.path(path_data, "2_from_EE", file), overwrite = TRUE)
}
