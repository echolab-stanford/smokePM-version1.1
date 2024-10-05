source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")


packageVersion("sf") #sf version HAS TO BE 0.9-8

# ------------------------------------------------------------------------------
# Written by: Jessica Li, Anne Driscoll
# Gets count of plumes overhead.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to grid
start_month = "2006-01" # "2005-08"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

grid_year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

# Read in project grid
project_grid = read_sf(file.path(path_data, "1_grids", "10km_grid")) %>% rename(id_grid = ID)

smoke = readRDS(file.path(path_data, "smoke", "combined", 
                          sprintf("smoke_plumes_sfdf_%s.RDS", gsub("-", "_", grid_year_months[1]))))

project_grid = project_grid %>% st_transform(st_crs(smoke))

# Load smoke dates
dates_not_online = readRDS(file.path(path_data, "smoke", "smoke_dates_not_online_update.rds"))
dates_empty_data = readRDS(file.path(path_data, "smoke", "smoke_dates_empty_data_update.rds"))
dates_repaired_geometry = readRDS(file.path(path_data, "smoke", "smoke_dates_repaired_geometry_update.rds"))
dates_density = readRDS(file.path(path_data, "smoke", "smoke_dates_density_data_update.rds"))


# Takes ~1-2 minutes per month
#year_month = "2024-01"
for (year_month in grid_year_months) { 
  y <- substr(year_month, 1, 4)
  m <- substr(year_month, 6, 7)
  
  # Load smoke plumes
  smoke = readRDS(file.path(path_data, "smoke", "combined", sprintf("smoke_plumes_sfdf_%s_%s.RDS", y, m))) %>% 
    select(-Start, -End) %>% 
    mutate(Density = case_when(Density == "Light" ~ 5,
                               Density == "Medium" ~ 16,
                               Density == "Dense" ~ 27),
           id_plume = row_number())
  
  out_file = file.path(path_data, "smoke_days", sprintf("grid_smoke_day_%s_%s.rds", y, m))
  preexisting = file.exists(out_file)
  
  if (overwrite | !preexisting) {
    
    # Get number of days in the month
    dm <- days_in_month(as.numeric(m))
    if (leap_year(as.numeric(y)) & (m == "02")) dm <- dm + 1
    days <- format(seq.Date(ymd(paste0(year_month, "-01")), 
                            ymd(paste0(year_month, "-", dm)), 
                            "day"), "%Y%m%d")
    
    # Get smoke day, plume count, and density if available
    out_month = vector("list", length(days))
    
    p = txtProgressBar(max = length(days), style = 3)
    for (d in 1:length(days)) {
      date = days[d]
      
      # Initialize output
      plumes <- data.frame(id_grid = project_grid$id_grid, 
                           date = date, 
                           smoke_day = NA, 
                           total = NA, 
                           light = NA, 
                           medium = NA, 
                           dense = NA)
      
      date_empty_data = (date %in% dates_empty_data)
      date_density = (date %in% dates_density)
      
      # Limit to plumes that day
      cur_smoke <- smoke[smoke$date == date,]
      
      if (nrow(cur_smoke) == 0) {
        plumes = plumes %>% 
          mutate(across(c(smoke_day, total)), ~ifelse(rep(date_empty_data, n()), 0, .x), 
                 across(c(light, medium, dense), ~ifelse(rep(date_empty_data, n()) & rep(date_density, n()), 0, .x)))
      } else if (nrow(cur_smoke) > 0) {
        # Find intersecting plumes and aggregate
        overlap = project_grid %>% 
          st_join(cur_smoke) %>% 
          st_drop_geometry() %>% 
          mutate(date = !!date) %>% 
          group_by(id_grid, date) %>% 
          summarize(total = sum(!is.na(id_plume)),
                    smoke_day = ifelse(total > 0, 1, 0),
                    light = sum(Density == 5, na.rm = T),
                    medium = sum(Density == 16, na.rm = T),
                    dense = sum(Density == 27, na.rm = T)) %>% 
          ungroup() %>% 
          mutate(across(c(light, medium, dense), 
                        ~ifelse(!rep(date_density, n()) | 
                                  (rep(date_density, n()) & 
                                     (total > 0) &
                                     (light == 0) &
                                     (medium == 0) &
                                     (dense == 0)), NA, .x)))
        plumes = plumes %>% 
          select(-smoke_day, -total, -light, -medium, -dense) %>% 
          left_join(overlap) %>% 
          select(id_grid, date, smoke_day, total, light, medium, dense)
      }
      # Append to list for the month
      out_month[[d]] <- plumes
      setTxtProgressBar(p, d)
    }
    # Save
    out_month <- out_month %>% bind_rows() %>% arrange(id_grid, date)
    saveRDS(out_month, out_file)
  }
}
