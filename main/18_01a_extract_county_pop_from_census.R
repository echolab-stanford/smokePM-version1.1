#checking new population files available
library(tidyverse)
library(arrow)
library(sf)
library(tigris)
library(doParallel)
library(foreach)
library(progressr)
library(future)
library(doFuture)
source("/scratch/users/mmarti04/smokePM-prediction/scripts/setup/00_03_load_paths.R")

# #create the crosswalk for each unit of aggregation, keep the intersection geometries, and retrieve the population weights from the 1km grids
#consistent_counties<- st_read("../Local_dropbox/data/1_grids/CONUS_consistent_counties.gpkg")

tigris_counties_2021<- tigris::counties(year = 2021) %>% 
  filter(!STATEFP %in% c("02","60","66","15","72","78","69"))

# read in the grid
#grid_10km <- st_read(file.path(path_data_sherlock, "1_grids", "grid_10km_wgs84")) %>%
#  st_transform(st_crs(tigris_counties_2021)) #adjust crs to the units shapefile


# # Setup parallel backend
# num_cores <- 15  # Leave one core for the OS
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)

# Set up future to use multisession (parallel)
#plan(multisession, workers = 15)

# Register doFuture as the backend for foreach
#registerDoFuture()


# Set up progress handler (this can be console, shiny, etc.)
#handlers(global = TRUE)
#handlers("progress")



#batch_size <- 5  # Number of rows per batch
#n_batches <- ceiling(nrow(tigris_counties_2021) / batch_size) #split the shapefile data

#with_progress({
  #p <- progressor(steps = n_batches)

  #unit_cross_parallel <- foreach(batch = 1:n_batches,
  #                               .combine = rbind,
 #                                .packages = c("sf", "dplyr", "progressr")) %dopar% {
#
   #                                # Calculate which rows belong to this batch
  #                                 start_idx <- (batch - 1) * batch_size + 1
 #                                  end_idx <- min(batch * batch_size, nrow(tigris_counties_2021))
#
 #                                  unit_batch <- tigris_counties_2021[start_idx:end_idx, , drop = FALSE]
#
  #                                 # Pre-filter grid to only overlapping cells
 #                                  grid_overlap <- st_filter(grid_10km, unit_batch)
#
    #                               # Perform intersection
   #                                intersection <- st_intersection(unit_batch, grid_overlap) %>%
  #                                   select(GEOID, grid_id_10km = ID) %>%
 #                                    mutate(area = st_area(.))
#
  #                                 # Progress message
 #                                  p(message = sprintf("Processing batch %d/%d (rows %d to %d)", batch, n_batches, start_idx, end_idx))
#
 #                                  return(intersection)
#                                 }
#})


# Stop the cluster after processing
#stopImplicitCluster()

# unit_cross_parallel %>%
#   ggplot()+
#   geom_sf()


# save the crosswalk since it takes a while to make
#saveRDS(unit_cross_parallel, file.path(path_data_sherlock, "1_grids", "county_grid_intersection_crosswalk.rds"))

unit_cross_parallel<- readRDS(file.path(path_data_sherlock, "1_grids", "county_grid_intersection_crosswalk.rds"))

years<- 2013:2023

#read in the 10 km grid
#grid_10km<- st_read(file.path(path_data_sherlock, "1_grids", "10km_grid"))

for (year in years){

#read in the pop file from Census Bureau
# test<- read_parquet(file.path(sprintf("https://www2.census.gov/ces/gridded_eif/gridded_eif_pop_ageracesex_%s.parquet", year)))
# test<- read_parquet("https://www2.census.gov/ces/gridded_eif/gridded_eif_pop_ageracesex_2009.parquet")
# 
# 
# # Define file URL and local path
# #url <- sprintf("https://www2.census.gov/ces/gridded_eif/gridded_eif_pop_ageracesex_%s.parquet", year)
# local_file <- file.path(path_data_sherlock, "US Census Bureau", sprintf("gridded_eif_pop_ageracesex_%s.parquet", year))
# 
# # Download the file with curl (handles big files and timeouts better)
# #curl::curl_download(url, destfile = local_file)
# 
# 
# # Now read it
# test<- read_parquet(local_file)
# 
# #change it to sf object
# pop_data <- test %>%
#   group_by(grid_lon, grid_lat) %>% 
#   summarise(total_pop = sum(n_noise_postprocessed)) %>% 
#   ungroup() %>% 
#   mutate(grid_lat = as.numeric(grid_lat),
#          grid_lon = as.numeric(grid_lon)) %>%
#   st_as_sf(coords = c("grid_lon", "grid_lat"), crs = 4326) %>%  # assuming WGS84
#   st_transform(crs = st_crs(consistent_counties))  # match the grid projection
# 
# saveRDS(pop_data, file.path(path_data_sherlock, "US Census Bureau", sprintf("centroids_pop_%s.rds", year)))
# 
# rm(test, pop_data)

# pop_data %>%
#   st_filter(unit_sf) %>% 
#   ggplot()+
#   geom_sf(aes(color = total_pop), size = 0.05)+
#   scale_color_gradient2(low = "lightblue", mid = "blue", high = "navyblue", midpoint = 20000)+
#   labs(title = "Census EIF pop 1km centroid points")+
#   theme_minimal()

#spatial join with the crosswalk intersection geometries
# pop_crosswalk<- unit_cross_parallel %>% 
#   st_join(pop_data, st_intersects)

#paralellized verison of the spatial join
plan(multisession, workers = 10)  # Adjust to your CPU
registerDoFuture()

# Set up progress handler (this can be console, shiny, etc.)
handlers(global = TRUE)
handlers("progress")


n_chunks <- 10  # adjust as needed
unit_chunks <- split(unit_cross_parallel, cut(seq_len(nrow(unit_cross_parallel)), breaks = n_chunks, labels = FALSE))
with_progress({
  p <- progressor(steps = length(unit_chunks))
  pop_crosswalk <- foreach(i = seq_along(unit_chunks), .combine = rbind,
                           .packages = c("sf", "dplyr")) %dopar% {
                             p(sprintf("Chunk %d of %d", i, length(unit_chunks)))
                             unit_chunk <- unit_chunks[[i]]
                             
                             # Load pop_data from file inside worker
                             pop_data_worker <- readRDS(file.path(path_data_sherlock, "US Census Bureau", sprintf("centroids_pop_%s.rds", year))) %>% st_transform(st_crs(unit_chunk))
                             
                             # Filter and join
                             pop_filtered <- st_filter(pop_data_worker, unit_chunk, .predicate = st_intersects)
                             st_join(unit_chunk, pop_filtered, join = st_intersects)
                           }
})

  stopImplicitCluster()

county_grid_pop_crosswalk<- pop_crosswalk %>% 
  mutate(total_pop = replace_na(total_pop, 0)) %>% 
  group_by(grid_id_10km, GEOID, geometry) %>% 
  summarise(pop = sum(total_pop, na.rm = TRUE)) %>% 
  ungroup()

# county_grid_pop_crosswalk %>% 
#   summary()
# 
# county_grid_pop_cross<- pop_cross %>% 
#   group_by(grid_id_10km, GEOID, geometry) %>% 
#   summarise(pop = sum(total_pop)) %>% 
#   ungroup()
# 
# county_grid_pop_cross %>% 
#   summary
# 
# county_grid_pop_crosswalk %>%
#   ggplot()+
#   geom_sf(aes(geometry = geom, fill = pop), color = NA)+
#   scale_fill_gradient2(low = "lightblue", mid = "blue", high = "navyblue", midpoint = 450000)+
#   theme_minimal()+
#   labs(title = "population contained at the county-grid intersection areas")

saveRDS(county_grid_pop_crosswalk, file.path(path_data_sherlock, "1_grids", "county_crosswalks", sprintf("county_grid_pop_crosswalk_%s.rds",  year)))

# pop_crosswalk %>% 
#   ggplot()+
#   geom_sf(aes(geometry = geometry, fill = total_pop), color = NA)
# 
# # pop_grid<- grid_10km %>% 
# #   st_join(pop_data, st_intersects)
# 
# 
# pop_GEOID<- pop_crosswalk %>% 
#   group_by(GEOID) %>% 
#   summarise(total_pop = sum(total_pop)) %>% 
# ungroup() %>% 
#   mutate(total_pop = replace_na(total_pop, 0))

# pop_10km %>% 
#   ggplot()+
#   geom_sf(aes(fill = total_pop), color = NA)+
#   scale_fill_gradient2(low = "lightblue", mid = "blue", high = "navyblue", midpoint = 650000)+
# #   theme_minimal()
# 
# saveRDS(pop_10km, file.path(path_data_sherlock, "US Census Bureau", "gridded_pop", sprintf("population_10km_%s.rds", year)))

print(paste0("saved 10km grid population crosswalk file for year ", year))

}
