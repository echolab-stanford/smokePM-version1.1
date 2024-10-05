source("scripts/setup/00_01_load_packages.R") #installed most recent version of sf for this script to run, make sure to install old version back
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

gc()
#-------------------------------------------------------------------------------
# Written by: Anne Driscoll, Jessica Li
# Generates fire clusters from fire points by month.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to build clusters
start_month = "2006-01" # "2003-04"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set months to use as available inputs
start_input = "2006-01" # "2003-04"
end_input = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

# from https://www.ospo.noaa.gov/Products/land/hms.html#about
north = 72 + 20
south = 14.6 - 20
east = -50 + 40
west = -170 - 40

crs_use = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs_m = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

width_km = 2.9
width = width_km*1000

year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")
#year_month = "2024_01" #troubleshooting
# Takes ~1-2 minutes per month on average
for (year_month in year_months) { 
  year = substr(year_month, 1, 4)
  month = substr(year_month, 6, 7)
  
  cluster_file = file.path(path_data, "fire", "clusters", sprintf("clusters_%s_%s.RDS", year, month))
  preexisting = file.exists(cluster_file)
  
  if (overwrite | !preexisting) {
    # get the fires that happened during the month of interest or previous 3 days
    fire = read_rds(file.path(path_data, "fire", "combined_from_shp", 
                              sprintf("hms_fires_%s_%s.RDS", year, month)))
    
    first_month = (year_month == start_input)
    if (!first_month) {
      prev_fire = read_rds(file.path(path_data, "fire", "combined_from_shp", 
                                     sprintf("hms_fires_%s.RDS", format(ym(year_month) - months(1), "%Y_%m"))))
      prev_fire <- prev_fire[ymd(names(prev_fire)) %within% 
                               ((ymd(paste0(year, month, "01")) - days(3)) %--% 
                                  (ymd(paste0(year, month, "01")) - days(1)))]
      
      fire = c(prev_fire, fire)
      rm(prev_fire)
    }
    
    month_fire_sp = as.list(rep(NA, length(fire)))
    j = 1
    
    # loop through days in the month
    prog = txtProgressBar(min=0, max=length(month_fire_sp), initial=0, char="-", style=3)
    #k = 1 #troubleshooting
    for (k in which(names(fire) == format(floor_date(ym(paste(year, month)), "month"), "%Y%m%d")):length(month_fire_sp)) {
      # get the fires that happened that day or within the previous 3 days
      date = names(fire)[k]
      f = fire[names(fire) %in% format(seq.Date(ymd(date) - days(3), ymd(date), by = "day"), "%Y%m%d")]
      
      # skip in no fires happened that day
      f_nrow = sapply(f, nrow)
      if (f_nrow[length(f_nrow)] == 0) next # 2014-07-05 should be "not online"
      
      # remove dates on which no fires happened
      f = f[which(f_nrow > 0)]
      
      # combine fires across days
      f = lapply(f, "[", c("date", "geometry"))
      f = bind_rows(f)
      
      # get rid of points with lonlat that are beyond extent or empty
      st_crs(f) = crs_use
      sf_use_s2(FALSE)
      f = st_crop(f, xmin = west, xmax = east, ymin = south, ymax = north) 
      
      #packageVersion("sp")
      #install.packages("sf")
      #removing elements to open up space
      #rm(fire) #troubleshootin
      #library("sp")
      
       if (nrow(f[which(f$date == date), ])==0) next
     
      # convert f to an SP object
      # f = st_transform(f, crs_m)
      f = tryCatch({
        as_Spatial(f)
      }, error = function(e) {
        f = f[!is.na(st_is_valid(f)) & !st_is_empty(f),]
        f = st_cast(f, "POINT")
        as_Spatial(f)
      }) 
      
      if (nrow(f[which(f$date == date),]) == 0) next
      f = spTransform(f, crs_m)
      

      #buffer by 'width' and merge adjacent pixels
      f_buf = gBuffer(f, byid=T,  width=width, capStyle="SQUARE", quadsegs=1)
      f_buf = st_cast(st_union(st_as_sf(f_buf)), "POLYGON")
      f_buf = SpatialPolygonsDataFrame(as_Spatial(f_buf), data.frame(id=1:length(f_buf)), match.ID=F)
      
      #save metadata
      f_buf$area = gArea(f_buf, byid=T)/1000/1000
      f_buf$date = date
      num_points = over(f_buf, f, returnList = T)
      num_points = sapply(num_points, nrow)
      f_buf$num_points = num_points
      
      # filter to clusters with at least one fire point from that day
      f_buf = st_as_sf(f_buf)
      f = st_as_sf(f)
      f = f[which(f$date == date), "geometry"]
      f_buf_filtered = st_filter(f_buf, f)
      # if (nrow(f_buf_filtered) == 0) next
      f = as_Spatial(f_buf_filtered)
      
      # transform CRS back
      f = spTransform(f, crs_use)
      
      #create new ids for clustered fires so that they can all be merged at the end
      f$id = paste0(year, month, "-", j:(nrow(f)+j-1))
      row.names(f) = as.character(f$id)
      j = length(f$id)+j
      month_fire_sp[[k]] = f
      
      if (k %% 6 == 0) {setTxtProgressBar(prog, k)}
    }
    
    # combine across days in the month
    w = sapply(month_fire_sp, function(x){"SpatialPolygonsDataFrame" %in% class(x)})
    month_fire = month_fire_sp[w] #remove the ones that were empty or broken
    month_fire_df = rbindlist(lapply(month_fire, function(x){x@data}), fill=T)
    month_fire = unlist(lapply(month_fire, function(x){x@polygons}))
    
    row.names(month_fire_df) = month_fire_df$id
    month_fire = SpatialPolygonsDataFrame(SpatialPolygons(month_fire), month_fire_df)
    crs(month_fire) = crs_use
    
    saveRDS(month_fire, cluster_file)
    
    print(year_month)
  }
}
