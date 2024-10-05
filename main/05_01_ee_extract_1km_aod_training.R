source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Extracts AOD over training 1km grid cells.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to extract
start_month = "2006-01" # "2000-03"
end_month = "2023-12" # "2023-01"

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

# set a null value 
null_value <- -999999

# earth engine location of the 1km grid
grid_train <- ee$FeatureCollection("users/SmokePM/grid_aod_1km/grid_aod_1km_wgs84_training") #Adjust path according to ee folder structure
#grid_train <- ee$FeatureCollection(sprintf("users/%s/grid_aod_1km/grid_aod_1km_wgs84_training", gee_user))

# google drive export location 
output_drive <- "maiac_AOD"

# local folder to save output to
output_local <- file.path(path_data, "2_from_EE", "maiac_AOD_training")

# set timezone, currently using GMT+6 to be consistent with ERA5 extractions
# matches mountain time in summer and central time in winter
timezone <- "Etc/GMT+6"

# Set up time range (end_date is exclusive) to run over
grid_year_months = seq.Date(ym(start_month), ym(end_month), by = "month")
date_grid <- data.frame(start_date = grid_year_months, 
                        end_date = grid_year_months + months(1)) %>% 
  mutate(across(c(start_date, end_date), format, "%Y-%m-%d"))
if (!overwrite) {
  for (i in 1:nrow(date_grid)) {
    row = date_grid[i,]
    file = file.path(output_local, sprintf(
      "aod_1km_grid_train_%s_%s.csv", 
      format(ymd(row$start_date), "%Y%m%d"), 
      format(ymd(row$end_date) - days(1), "%Y%m%d")))
    preexisting = file.exists(file)
    if (preexisting) date_grid = setdiff(date_grid, row)
  }
}

# Get MAIAC ----
maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_047") 

maiac_proj <- maiac$first()$projection()
pixel_res <- maiac_proj$nominalScale()$getInfo()

# for each date, calculate average AOD and export
grid_tasks <- date_grid %>%
  purrr::pmap(function(start_date, end_date){
    
    print(paste("Working on start date", start_date))
    
    # calculate how many days between start and end 
    length_date <- difftime(end_date, start_date, units = "days") %>% as.integer()
    
    # for each day, count available images, then make binary with 1's indicating no obs/missings ----
    maiac_daily <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
      im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date), 
                                 timezone)$advance(x, "day") 
      im_date_end <- im_date$advance(1, "day")
      im <- maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median())
      im <- ee$Algorithms$If(
        ee$Algorithms$IsEqual(im$bandNames()$size(), ee$Number(0)),
        im$addBands(ee$Image$constant(null_value)$rename("Optical_Depth_055_median")),
        im$unmask(null_value)
      ) %>% ee$Image$set("start_date", im_date$format("yMMdd"))
      return(im)
    })) %>%
      ee$ImageCollection$fromImages() 
  
    # for each day, calculate average for each grid cell
    grid_aod <- maiac_daily$map(function(daily_im){
      daily_im$reduceRegions(collection = grid_train, 
                             reducer = ee$Reducer$median(), 
                             crs = maiac_proj,
                             scale = pixel_res) %>% 
        ee$FeatureCollection$map(function(f){
          f$set("start_date", daily_im$get("start_date"))  %>%  # set the date of the feature
            return
        })
    }) %>% ee$FeatureCollection() %>% 
      ee$FeatureCollection$flatten()
    
    # make list of properties to export
    export_properties <- list("grid_id", "median", "start_date")
    
    # make useful file name with dates (inclusive)
    output_name <- paste("aod_1km_grid_train",
                         as.Date(start_date) %>% format("%Y_%m"),
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = grid_aod,
      folder = output_drive,
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
# Check the status of the tasks
grid_tasks %>% map_chr(function(x) x$status()$state)

# Once tasks are "COMPLETED", download them to local folder
retry::wait_until(
  expr = all(map_chr(grid_tasks, function(x) x$status()$state) == "COMPLETED"), 
  interval = 60
  )
grid_tasks %>% map(function(t){ #Issue here to download from drive to local
  ee_drive_to_local(t, 
                    file.path(output_local, paste0(t$status()$description, ".csv")))
})
#-#-----------------------------------------------------------------------------
#alternative to download from google drive
library(googledrive)

MAIAC_folder<- drive_find(pattern = 'maiac_AOD', type = 'folder')
MAIAC_files<- drive_ls(MAIAC_folder)

# Download each file from the folder to local folder
for (file in MAIAC_files$name) {
  drive_download(file = file, path = file.path(output_local, file), overwrite = TRUE)
}