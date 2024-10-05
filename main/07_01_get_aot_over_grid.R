source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Anne Driscoll
# Pulls MERRA daily grid cell means by month.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to grid
start_month = "2006-01" # "1980-01"
end_month = "2023-12" # format(today() - months(2), "%Y%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

# load in data and prep
files = list.files(path_merra2)
file_dates = unname(sapply(files, function(x) {substr(x, 28, 35)}))
grid_year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

grid_merra = raster(file.path(path_merra2, files[format(ymd(file_dates), "%Y-%m") %in% grid_year_months][1]))
grid = read_sf(file.path(path_data, "1_grids", "10km_grid")) %>% 
  as_Spatial() %>% 
  spTransform(crs(grid_merra))

# build crosswalk
grid$cell_m = cellFromXY(grid_merra, as.matrix(coordinates(grid)))

# get the daily aot
for (grid_year_month in grid_year_months) {
  
  year = substr(grid_year_month, 1, 4)
  month = substr(grid_year_month, 6, 7)
  out_file = file.path(path_data, "MERRA2_AOT", "us_grid_daily_aot", 
                       sprintf("daily_grid_aot_%s_%s.RDS", year, month))
  preexisting = file.exists(out_file)
  
  if (overwrite | !preexisting) {
    
    files_m = grep(sprintf("Nx\\.%s%s[0-3][0-9]\\.SUB\\.nc$", year, month), files, value = T)
    
    out = vector("list", length(files_m))
    for (d in 1:length(files_m)) {
      file = files_m[d]
      rast = raster(file.path(path_merra2, file))
      out[[d]] = data.frame(grid_id = grid$ID, 
                            date = substr(file, 28, 35), 
                            aot = rast[grid$cell_m])
    }
    out = rbindlist(out)
    
    saveRDS(out, out_file)
    
    print(grid_year_month)
  }
}
