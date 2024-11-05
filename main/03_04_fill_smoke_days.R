source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Jessica Li, Marissa Childs
# Fill in missing smoke dates based on temporal nearest neighboring non-missing 
# smoke dates. Saves only smoke days. Not needed if there's no empty smoke days.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set months to fill
start_month = "2006-01" # "2005-08"
end_month = "2023-12" # format(today() - days(1), "%Y-%m")

# Set months to use as available inputs
start_input = "2006-01" # "2005-08"
end_input = "2023-12" # format(today() - days(1), "%Y-%m")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

year_months = format(seq.Date(ym(start_month), ym(end_month), by = "month"), "%Y-%m")

# Track runs since not every month needs to save filled data
previous_runs_file = file.path(path_data, "3_intermediate", "filled_smoke", "previously_run.txt")
previous_runs_exist = file.exists(previous_runs_file)
if (previous_runs_exist) {
  previous_runs = read.table(previous_runs_file, header = T)
} else {
  previous_runs = data.frame(year = numeric(), 
                             month = numeric(), 
                             timestamp = character())
}

these_runs = vector("list", length(year_months)) 
for (i in 1:length(year_months)) {
  year_month = year_months[i]
  year = substr(year_month, 1, 4)
  month = substr(year_month, 6, 7)
  
  out_file = file.path(path_data, "3_intermediate", "filled_smoke", sprintf("filled_smoke_days_%s_%s.rds", year, month))
  check<- readRDS(out_file)
  preexisting = file.exists(out_file)
  previously_run = previous_runs %>% 
    filter(year == as.numeric(!!year), 
           month == as.numeric(!!month)) %>% 
    nrow() %>% 
    as.logical()
  
  if (overwrite | (!preexisting & !previously_run)) {
    # Load gridded smoke days distance
    smoke_days = readRDS(file.path(path_data, "smoke_days", sprintf("grid_smoke_day_%s_%s.rds", year, month))) %>% 
      dplyr::select(-total, -light, -medium, -dense) %>% 
      dplyr::rename(grid_id_10km = id_grid)
    
   
    first_month = (year_month == start_input)
    if (!first_month) {
      prev_smoke_days = read_rds(file.path(path_data, "smoke_days", sprintf("grid_smoke_day_%s.rds", format(ym(year_month) - months(1), "%Y_%m")))) %>% 
        dplyr::rename(grid_id_10km = id_grid)
      prev_smoke_days = prev_smoke_days[ymd(prev_smoke_days$date) %within% ((ymd(paste0(year, month, "01")) - days(2)) %--% (ymd(paste0(year, month, "01")) - days(1))),]
      smoke_days = bind_rows(prev_smoke_days, smoke_days)
      rm(prev_smoke_days)
    }
    last_month = (year_month == end_input)
    if (!last_month) {
      foll_smoke_days = read_rds(file.path(path_data, "smoke_days", 
                                           sprintf("grid_smoke_day_%s.rds", format(ym(year_month) + months(1), "%Y_%m"))))%>% 
        dplyr::rename(grid_id_10km = id_grid)
      foll_smoke_days = foll_smoke_days[ymd(foll_smoke_days$date) %within% ((ymd(paste0(year, month, "01")) + months(1)) %--% (ymd(paste0(year, month, "01")) + months(1) + days(1))),]
      smoke_days = bind_rows(smoke_days, foll_smoke_days)
      rm(foll_smoke_days)
    }
    
    smoke_fill <- smoke_days %>%
      arrange(grid_id_10km, date) %>% 
      group_by(grid_id_10km) %>%
      mutate(lag1 = lag(smoke_day, 1), 
             lag2 = lag(smoke_day, 2), 
             lead1 = lead(smoke_day, 1), 
             lead2 = lead(smoke_day, 2)) %>% 
      filter(is.na(smoke_day)) %>% 
      rowwise() %>% 
      mutate(smoke_day_fill = case_when(!is.na(smoke_day) ~ list(c(smoke_day)),
                                        is.na(smoke_day) & !is.na(lag1) & !is.na(lead1) ~ list(c(lag1, lead1)), 
                                        T ~ list(c(lag2, lag1, lead1, lead2))),
             smoke_day_fill = mean(smoke_day_fill, na.rm = T)) #Here you'll get warnings of NA's being generated if there's no missing smoke days, this file can be empty
    
    # Default to zero if all neighbors weren't also smoke days. we only need to keep track of the 1s
    smoke_fill %<>% 
      ungroup() %>%
      mutate(smoke_day_fill = (smoke_day_fill == 1)*1) %>% 
      dplyr::select(grid_id_10km, date, smoke_day_fill) %>% 
      filter(smoke_day_fill == 1)
    
    # save RDS of all smoke days
    out = smoke_days %>% 
      left_join(smoke_fill) %>%  #change this if smoke_fill is not empty:", by = c("grid_id_10km", "date")) %>%"
      mutate(smoke_day = ifelse(is.na(smoke_day), smoke_day_fill, smoke_day)) %>% 
      filter(smoke_day == 1) %>% 
      dplyr::select(grid_id_10km, date, smoke_day)
    
    
    saveRDS(out, out_file) 
  }
  
  this_run = data.frame(year = as.numeric(year), 
                        month = as.numeric(month), 
                        timestamp = as.character(Sys.time()))
  these_runs[[i]] = this_run
}

these_runs = bind_rows(these_runs)
latest_runs = previous_runs %>% 
  bind_rows(these_runs) %>% 
  group_by(year, month) %>% 
  slice_max(timestamp) %>% 
  ungroup() %>% 
  arrange(year, month)
write.table(latest_runs, previous_runs_file, row.names = F)
