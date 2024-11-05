source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#THIS SCRIPT STILL NEEDS FIXING

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Extracts percent AOD missing over 10km grid by month.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to grid
start_month = "2006-01" # "2000-03"
end_month = "2006-12" # "2023-01"

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------
#gee_user = "SmokePM"

# earth engine location of the subsets of the 10km grid
asset_folder <- sprintf("users/%s/grid_10km/", gee_user)

# google drive export location 
output_drive <- "maiac_AODmissings"

# local folder to save output to
output_local <- file.path(path_data, "2_from_EE", "maiac_AODmissings")

# set timezone, currently using GMT+6 to be consistent with ERA5 extractions
# matches mountain time in summer and central time in winter
timezone <- "Etc/GMT+6"

# Set up time range (end_date is exclusive) with subgrids to run over
grid_year_months = seq.Date(ym(start_month), ym(end_month), by = "month")
date_grid <- data.frame(start_date = grid_year_months, 
                        end_date = grid_year_months + months(1)) %>% 
  mutate(across(c(start_date, end_date), format, "%Y-%m-%d")) %>% 
  expand_grid(subgrid_no = 1:10)
if (!overwrite) {
  for (i in 1:nrow(date_grid)) {
    row = date_grid[i,]
    file = file.path(output_local, sprintf(
      "aod_pctMissing_10km_subgrid_%s_%s_%s.csv", 
      row$subgrid_no, 
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

# for each date and grid combination, calculate average missingness for the grid cells and export
subgrid_tasks <- date_grid %>%
  purrr::pmap(function(start_date, end_date, subgrid_no){#start_date = "2006-01-01", end_date = "2006-02-01", subgrid_no = 1
    
    print(paste("Working on start date", start_date, "for subgrid", subgrid_no))
    
    # calculate how many days between start and end 
    length_date <- difftime(end_date, start_date, units = "days") %>% as.integer()
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0(asset_folder, "grid_10km_wgs84_", subgrid_no)) 
    
    # for each day, count available images, then make binary with 1's indicating no obs/missings ----
    maiac_daily_missing <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
      im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date), 
                                 timezone)$advance(x, "day") 
      im_date_end <- im_date$advance(1, "day")
      # Filter the collection by date and count the number of images using size()
      im_count <- maiac$filterDate(im_date, im_date_end)$size()
      
      # Create an image with 1 if no observations/missings, otherwise 0
      im <- ee$Image(im_count$eq(0)) %>% 
        ee$Image$set("start_date", im_date$format("yMMdd")) # set date of the image
      
      #im <- maiac$filterDate(im_date, im_date_end)$count()$unmask(0) %>% 
       # ee$Image$Not() %>%
      #  ee$Image$set("start_date", im_date$format("yMMdd")) # set date of the image
      return(im)
    })) %>%
      ee$ImageCollection$fromImages() 
    
    # for each day, calculate average for each grid cell
    subgrid_missings <- maiac_daily_missing$map(function(daily_im){
      daily_im$reduceRegions(collection = subgrid, 
                             reducer = ee$Reducer$mean(), 
                             crs = maiac_proj,
                             scale = pixel_res) %>% 
        ee$FeatureCollection$map(function(f){
          f$set("start_date", daily_im$get("start_date"))  %>%  # set the date of the feature
            return
        })
    }) %>% ee$FeatureCollection() %>% 
      ee$FeatureCollection$flatten()
    
    # make list of properties to export
    export_properties <- list("ID", "mean", "start_date")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("aod_pctMissing_10km_subgrid",
                         subgrid_no,
                         as.Date(start_date) %>% format("%Y_%m"),
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = subgrid_missings,
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
subgrid_tasks %>% map_chr(function(x) x$status()$state)

# Once tasks are "COMPLETED", download them to local folder
# This took ~12 hours
retry::wait_until(
  expr = all(map_chr(subgrid_tasks, function(x) x$status()$state) == "COMPLETED"), 
  interval = 60
)
subgrid_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    file.path(output_local, paste0(t$status()$description, ".csv")))
})
#-#-----------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
#alternative to download from google drive
library(googledrive)

MAIAC_folder<- drive_find(pattern = 'maiac_AODmissings', type = 'folder')
MAIAC_files<- drive_ls(MAIAC_folder)

# Download each file from the folder to local folder
for (file in MAIAC_files$name) {
  drive_download(file = file, path = file.path(output_local, file), overwrite = TRUE)
}
