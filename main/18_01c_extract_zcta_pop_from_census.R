library(tidyverse)
library(arrow)
library(data.table)
library(sf)
library(terra)
library(tigris)
library(doParallel)
library(foreach)
library(progressr)
library(future)
library(doFuture)
source("/scratch/users/mmarti04/smokePM-prediction/scripts/setup/00_03_load_paths.R")
options(tigris_use_cache = TRUE)

#create the crosswalk for each unit of aggregation, keep the intersection geometries, and retrieve the population weights from the 1km grids
# unit <- "zcta"
# nonContig_stateFIPS <- c("02","60","66","15","72","78","69")
# if (unit == "zcta") {
#     states_sf <- states() %>%
#       filter(STATEFP %in% nonContig_stateFIPS == F)
#     unit_sf<- zctas(cb = FALSE,  year = 2021) %>% select(ZCTA5CE20, GEOID20) %>% st_filter(states_sf)
#     }
# 
# 
# # # read in the grid
# grid_10km <- st_read(file.path(path_data_sherlock, "1_grids", "grid_10km_wgs84")) %>%
#   st_transform(st_crs(unit_sf))
# 
# #Set up future to use multisession (parallel)
# plan(multisession, workers = 9)
# 
# # Register doFuture as the backend for foreach
# registerDoFuture()
# 
# # Set up progress handler (this can be console, shiny, etc.)
# handlers(global = TRUE)
# handlers("progress")
# 
# batch_size <- 10  # Number of rows per batch
# n_batches <- ceiling(nrow(unit_sf) / batch_size)
# 
# with_progress({
#   p <- progressor(steps = n_batches)
# 
#   unit_cross_parallel <- foreach(batch = 1:n_batches,
#                                  .combine = rbind,
#                                  .packages = c("sf", "dplyr", "progressr")) %dopar% {
# 
#                                    # Calculate which rows belong to this batch
#                                    start_idx <- (batch - 1) * batch_size + 1
#                                    end_idx <- min(batch * batch_size, nrow(unit_sf))
# 
#                                    unit_batch <- unit_sf[start_idx:end_idx, , drop = FALSE]
# 
# 
#                                    # Pre-filter grid to only overlapping cells
#                                    grid_overlap <- st_filter(grid_10km, unit_batch)
# 
#                                    # Perform intersection
#                                    intersection <- st_intersection(unit_batch, grid_overlap) %>%
#                                      select(ZCTA5CE20, grid_id_10km = ID) %>%
#                                      mutate(area = st_area(.))
# 
#                                    # Progress message
#                                    p(message = sprintf("Processing batch %d/%d (rows %d to %d)", batch, n_batches, start_idx, end_idx))
# 
#                                    return(intersection)
#                                  }
# })
# 
# 
# # Stop the cluster after processing
# stopImplicitCluster()
# 
# # save the crosswalk since it takes a while to make
# saveRDS(unit_cross_parallel, file.path(path_data_sherlock, "1_grids", "tigris_zcta_grid_intersection_crosswalk.rds"))

unit_cross_parallel<- readRDS(file.path(path_data_sherlock, "1_grids", "tigris_zcta_grid_intersection_crosswalk.rds"))

#unit_cross_parallel %>% st_drop_geometry() %>% count(ZCTA5CE20)

years <- 2006:2023
for (year in years){ #year = 2006
  
  
  #paralellized verison of the spatial join
  plan(multisession, workers = 12)  # Adjust to your CPU
  registerDoFuture()
  
  # Set up progress handler (this can be console, shiny, etc.)
  handlers(global = TRUE)
  handlers("progress")
  
  n_chunks <- 20  # adjust as needed
  unit_chunks <- split(unit_cross_parallel, cut(seq_len(nrow(unit_cross_parallel)), breaks = n_chunks, labels = FALSE))
  with_progress({
    p <- progressor(steps = length(unit_chunks))
    pop_crosswalk <- foreach(i = seq_along(unit_chunks), .combine = rbind,
                             .packages = c("sf", "dplyr")) %dopar% {
                               p(sprintf("Chunk %d of %d", i, length(unit_chunks)))
                               unit_chunk <- unit_chunks[[i]]
                               
                               # Load pop_data from file inside worker
                               pop_data_worker <- readRDS(file.path(path_data_sherlock, "US Census Bureau", sprintf("centroids_pop_%s.rds", year)))%>%
                                 st_transform(st_crs(unit_chunk))
                               
                               # Filter and join
                               pop_filtered <- st_filter(pop_data_worker, unit_chunk, .predicate = st_intersects)
                               st_join(unit_chunk, pop_filtered, join = st_intersects)
                             }
  })
  
  stopImplicitCluster()
  
  
zcta_grid_pop_crosswalk<- pop_crosswalk %>% 
    mutate(total_pop = replace_na(total_pop, 0)) %>% 
    group_by(grid_id_10km, ZCTA5CE20, geometry) %>% 
    summarise(pop = sum(total_pop, na.rm = TRUE)) %>% 
    ungroup()
  
  saveRDS(zcta_grid_pop_crosswalk, file.path(path_data_sherlock, "1_grids", "zcta_crosswalks", sprintf("tigris_zcta_grid_pop_crosswalk_%s.rds",  year)))
  
  print(paste0("saved zcta-10km grid population crosswalk file for year ", year))
  
}