source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Jessica Li
# Classifies MERRA-2 grid cells into spatial folds.
#-------------------------------------------------------------------------------
n_nest_cv_folds = 5

# Load MERRA-2 grid
grid_merra = raster(file.path(path_merra2, "MERRA2_300.tavg1_2d_aer_Nx.20060101.SUB.nc"), layer = 1)

# Load 10km grid
grid_10km = st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

# Get MERRA-2 grid cells numbers for cells overlapping 10km grid
merra_cells = unique(cellFromXY(grid_merra, st_coordinates(st_centroid(grid_10km))))

# Assign spatial folds
set.seed(98765)
folds = sample.int(length(merra_cells), length(merra_cells), replace = F)
folds = (folds %% n_nest_cv_folds) + 1
merra_folds = data.frame(merra_cell = merra_cells, 
                         fold = folds) %>% 
  full_join(data.frame(merra_cell = 1:ncell(grid_merra))) %>% 
  arrange(merra_cell)
values(grid_merra) = merra_folds$fold

writeRaster(grid_merra, file.path(path_data, "1_grids", "merra_folds.nc"))
