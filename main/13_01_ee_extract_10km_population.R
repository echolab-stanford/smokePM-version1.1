source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Extracts population over 10km grid.
#-------------------------------------------------------------------------------
population <- ee$ImageCollection("WorldPop/GP/100m/pop") %>% 
  ee$ImageCollection$filterMetadata("country", "equals", "USA") %>% 
  ee$ImageCollection$filterMetadata("year", "equals", 2013) %>% 
  ee$ImageCollection$first() %>% 
  ee$Image$divide(ee$Image$pixelArea()) %>% # convert to population density
  ee$Image$unmask(0)

pop_scale <- population$projection()$nominalScale()$getInfo()

subgrid_10km_tasks <- 1:10 %>% 
  purrr::map(function(subgrid_no){
    
    print(paste("Working on subgrid", subgrid_no))
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0(sprintf("users/%s/grid_10km/grid_10km_wgs84_", gee_user), subgrid_no)) #"users/SmokePM/grid_10km/grid_10km_wgs84")
    
    # calculate population density, then average for each grid cell
    pop_10km <- population %>% #$divide(ee$Image$pixelArea()) %>% 
      ee$Image$reduceRegions( 
      collection = subgrid,
      reducer = ee$Reducer$mean(), 
      scale = pop_scale
    )
    
    # make list of properties to export
    export_properties <- list("ID", "mean")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("populationDensity_10km_subgrid",
                         subgrid_no,
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = pop_10km,
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
map_chr(subgrid_10km_tasks, function(x) x$status()$state)
retry::wait_until(
  expr = all(map_chr(subgrid_10km_tasks, function(x) x$status()$state) == "COMPLETED"), 
  interval = 60
  )

# Once task is "COMPLETED", download it to local folder
output_local <- file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid")
if (!dir.exists(output_local)) dir.create(output_local)
subgrid_10km_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    file.path(output_local, paste0(t$status()$description, ".csv")))
})
#-#-----------------------------------------------------------------------------
#alternative to download from google drive
library(googledrive)

EE_folder<- drive_find(pattern = 'rgee_backup', type = 'folder')
Pop_files<- drive_ls(EE_folder, pattern = 'populationDensity')

# Download each file from the folder to local folder
for (file in Pop_files$name) {
  drive_download(file = file, path = file.path(output_local, file), overwrite = TRUE)
}
