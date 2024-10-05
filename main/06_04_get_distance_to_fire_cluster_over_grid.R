source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")
install.packages("sf") #new version of sf needed here to run the conversion of the grid to spatial object
#-------------------------------------------------------------------------------
# Written by: Anne Driscoll, Jessica Li
# Gets distance (km) from each grid cell centroid to nearest fire cluster 
# centroid each day by month. Gets area and number of fire points composing each 
# fire cluster over daily grid as well.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to grid
start_month = "2006-01" # "2003-04"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

grid_year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

# Read in project grid
project_grid = read_sf(file.path(path_data, "1_grids", "10km_grid")) %>% as_Spatial()

# Set cutoff for minimum fire cluster size
min_size <- 100.92
files <- list.files(file.path(path_data, "fire", "clusters"), pattern = "^clusters", full.names = TRUE)
if (!("min_size" %in% ls())) {
  fire_clusters <- readRDS(files[1])
  min_size <- min(fire_clusters$area, na.rm = T) * 3
}

for (year_month in grid_year_months) {
  y <- substr(year_month, 1, 4)
  m <- substr(year_month, 6, 7)
  out_file = file.path(path_data, "distance_to_fire_cluster", 
                       sprintf("grid_distance_to_fire_cluster_%s_%s.rds", y, m))
  preexisting = file.exists(out_file)
  
  if (overwrite | !preexisting) {
    
    start_time = get_start_time()
    
    # Get number of days in the month
    dm <- days_in_month(as.numeric(m))
    if (leap_year(as.numeric(y)) & (m == "02")) dm <- dm + 1
    days <- format(seq.Date(ymd(paste0(year_month, "-01")), 
                            ymd(paste0(year_month, "-", dm)), 
                            "day"), "%Y%m%d")
    
    # Load fire clusters
    fire_clusters = readRDS(file.path(path_data, "fire", "clusters", sprintf("clusters_%s_%s.RDS", y, m)))
    fire_clusters = fire_clusters[fire_clusters$area > min_size,]
    
    # Get fire clusters into same CRS
    # crs(fire_clusters) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    fire_clusters <- spTransform(fire_clusters, crs(project_grid))
    
    # Get distance to nearest fire, area, and number of fire points by day
    out_month <- vector("list", length(days))
    for (d in 1:length(days)) {
      # Initialize output
      fire_distance <- data.frame(id_grid = project_grid$ID,
                                  date = days[d], 
                                  km_dist = NA, 
                                  area = NA,
                                  num_points = NA)
      
      # Limit to fire clusters that day
      cur_fire_clusters <- fire_clusters[fire_clusters$date == days[d], ]
      
      if (nrow(cur_fire_clusters) > 0) {
        # Find nearest fire cluster
        knn_dist <- get.knnx(coordinates(cur_fire_clusters), 
                             coordinates(project_grid), 
                             k = 1)
        
        # Get distance converted to km
        fire_distance$km_dist <- c(knn_dist$nn.dist)/1000
        
        # Set distance to 0 for grid cells intersecting a fire cluster
        fire_intersects <- over(project_grid, cur_fire_clusters)
        fire_intersects <- which(!is.na(fire_intersects$id))
        fire_distance$km_dist[fire_intersects] <- 0
        
        # Get area and number of fire points
        fire_distance$area <- cur_fire_clusters$area[knn_dist$nn.index]
        fire_distance$num_points <- cur_fire_clusters$num_points[knn_dist$nn.index]
      }
      # Append to list for the month
      out_month[[d]] <- fire_distance
    }
    # Save
    out_month <- out_month %>% bind_rows() %>% arrange(id_grid, date)
    saveRDS(out_month, out_file)
    
    print_time(start_time)
  }
}
