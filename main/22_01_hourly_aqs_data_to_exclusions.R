# install.packages("epair", repos = "https://ropensci.r-universe.dev")
pacman::p_load(fastverse, tidyverse, lubridate, fixest, epair, tictoc, arrow, future.apply)
setwd(here::here())

# usethis::edit_r_environ()
# aqs_api_key=API_KEY_HERE
# aqs_email=API_EMAIL_HERE

states <- c('01','04','05', '06', "08", "09", "10",  "12", "13",  "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41","42", "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56")

# Define the time periods (n-month windows, time-varying, from 2018 to 2023)
time_periods <- list(
  
  list("bdate" = '20160101', "edate" = '20160131'),
  list("bdate" = '20160201', "edate" = '20160301'),
  list("bdate" = '20160302', "edate" = '20160331'),
  list("bdate" = '20160401', "edate" = '20160430'),
  list("bdate" = '20160501', "edate" = '20160531'),
  list("bdate" = '20160601', "edate" = '20160630'),
  list("bdate" = '20160701', "edate" = '20160731'),
  list("bdate" = '20160801', "edate" = '20160831'),
  list("bdate" = '20160901', "edate" = '20160930'),
  list("bdate" = '20161001', "edate" = '20161031'),
  list("bdate" = '20161101', "edate" = '20161130'),
  list("bdate" = '20161201', "edate" = '20161231'),
  
  list("bdate" = '20170101', "edate" = '20170131'),
  list("bdate" = '20170201', "edate" = '20170301'),
  list("bdate" = '20170302', "edate" = '20170331'),
  list("bdate" = '20170401', "edate" = '20170430'),
  list("bdate" = '20170501', "edate" = '20170531'),
  list("bdate" = '20170601', "edate" = '20170630'),
  list("bdate" = '20170701', "edate" = '20170731'),
  list("bdate" = '20170801', "edate" = '20170831'),
  list("bdate" = '20170901', "edate" = '20170930'),
  list("bdate" = '20171001', "edate" = '20171031'),
  list("bdate" = '20171101', "edate" = '20171130'),
  list("bdate" = '20171201', "edate" = '20171231'),
  
  list("bdate" = '20180101', "edate" = '20180331'),
  list("bdate" = '20180401', "edate" = '20180630'),
  list("bdate" = '20180701', "edate" = '20180930'),
  list("bdate" = '20181001', "edate" = '20181231'),
  
  list("bdate" = '20190101', "edate" = '20190331'),
  list("bdate" = '20190401', "edate" = '20190630'),
  list("bdate" = '20190701', "edate" = '20190930'),
  list("bdate" = '20191001', "edate" = '20191231'),
  
  list("bdate" = '20200101', "edate" = '20200331'),
  list("bdate" = '20200401', "edate" = '20200630'),
  list("bdate" = '20200701', "edate" = '20200930'),
  list("bdate" = '20201001', "edate" = '20201231'),
  
  list("bdate" = '20210101', "edate" = '20210301'),
  list("bdate" = '20210302', "edate" = '20210430'),
  list("bdate" = '20210501', "edate" = '20210630'),
  list("bdate" = '20210701', "edate" = '20210831'),
  list("bdate" = '20210901', "edate" = '20211031'),
  list("bdate" = '20211101', "edate" = '20211231'),
  
  list("bdate" = '20220101', "edate" = '20220301'),
  list("bdate" = '20220302', "edate" = '20220430'),
  list("bdate" = '20220501', "edate" = '20220630'),
  list("bdate" = '20220701', "edate" = '20220831'),
  list("bdate" = '20220901', "edate" = '20221031'),
  list("bdate" = '20221101', "edate" = '20221231'),
  
  list("bdate" = '20230101', "edate" = '20230301'),
  list("bdate" = '20230302', "edate" = '20230430'),
  list("bdate" = '20230501', "edate" = '20230630'),
  list("bdate" = '20230701', "edate" = '20230831'),
  list("bdate" = '20230901', "edate" = '20231031'),
  list("bdate" = '20231101', "edate" = '20231231')
)

plan(multisession, workers = I(12))

this_state <- '06'
period <- time_periods[[35]]

# Loop through states and time periods
state_download <- function(this_state) {
  
  monitor_vars <- variable.list1 <- list(
    "state" = this_state,
    "bdate" = time_periods[[1]]$bdate,
    "edate" = time_periods[[length(time_periods)]]$edate,
    "param" = '88101'
  )
  monitors <- perform.call(endpoint = "monitors/byState", variables = monitor_vars)$Data %>% as.data.table()
  monitors[, station_id := paste0(state_code, county_code, site_number)]
  monitors %<>% select(station = station_id, 
                       monitor = poc, 
                       open_date,
                       close_date,
                       concurred_exclusions,
                       naaqs_primary_monitor) %>% 
    distinct(station, monitor, naaqs_primary_monitor, .keep_all = TRUE)
  # excluded <- monitors %>% filter(grepl('resent', concurred_exclusions)) %>% distinct(station_id)
  
  with_exclusions <- monitors %>% filter(!is.na(concurred_exclusions))
  
  exclusion_panel <- list()
  
  for (exclusion_row in 1:nrow(with_exclusions)) {
    
    periods <- str_count(with_exclusions$concurred_exclusions[exclusion_row], 'All')
    
    date_ranges <- str_extract_all(with_exclusions$concurred_exclusions[exclusion_row], "\\([0-9]{4}-[0-9]{2}-[0-9]{2} - ([0-9]{4}-[0-9]{2}-[0-9]{2}|Present)\\)")[[1]]
    
    start_dates <- c()
    end_dates <- c()
    
    # Loop over each date range
    for (range in date_ranges) {
      # Extract the start date for the current range
      start_date <- str_extract(range, "(?<=\\()[0-9]{4}-[0-9]{2}-[0-9]{2}")
      
      # Extract the end date or "Present" for the current range
      end_date <- str_extract(range, "(?<= - )([0-9]{4}-[0-9]{2}-[0-9]{2}|Present)")
      
      # If end date is "Present", replace it with the current date
      if (end_date == "Present") {
        end_date <- format(Sys.Date(), "%Y-%m-%d")
      }
      
      # Append the dates to the lists
      start_dates <- c(start_dates, start_date)
      end_dates <- c(end_dates, end_date)
    }
    
    for (this_period in periods) {
      
      exclusion_panel[[length(exclusion_panel)+1]] <- CJ(date = seq.Date(as.Date(start_dates[this_period]), as.Date(end_dates[this_period]), by = 'day'), station = with_exclusions$station[exclusion_row], monitor = with_exclusions$monitor[exclusion_row], exclusion_granted = 1)
      
    }
    
  }
  
  exclusion_panel <- rbindlist(exclusion_panel)
  
  # Initialize an empty data frame to store results
  final_summary <- list()
  
  for (period in time_periods) { 
    
    # Set up the API call variables
    variable.list1 <- list(
      "state" = this_state,
      "bdate" = period$bdate,
      "edate" = period$edate,
      "param" = '88101'
    )
    
    # Perform the API call 
    hourly_data <- perform.call(endpoint = "sampleData/byState", variables = variable.list1)$Data %>% as.data.table()
    
    # Process the hourly data #check if this filtering sample_duration_code is removing the number of monitors that we have on smokePM
    hourly_data[, station_id := paste0(state_code, county_code, site_number)][, hour := as.numeric(substr(time_local, 1, 2))]
    hourly_data %<>% select(station = station_id, 
                            monitor = poc, 
                            parameter, 
                            date = date_local, 
                            hour, 
                            pm25 = sample_measurement, 
                            sample_duration,
                            qualifier, 
                            method = method_type, 
                            lat = latitude, 
                            lon = longitude)
    complete <- hourly_data[sample_duration == '1 HOUR'][, .(data_complete = fifelse(sum(!is.na(pm25)) >= 18, T, F)), by = list(date, station, monitor)]
    completeness_spine_daily <- hourly_data %>% filter(sample_duration == '1 HOUR') %>% distinct(date, station, monitor)
    completeness_spine_hourly <- list()
    for (this_hour in 0:23) {
      completeness_spine_hourly[[length(completeness_spine_hourly)+1]] <- completeness_spine_daily %>% mutate(hour = this_hour)
    }
    completeness_spine_hourly <- rbindlist(completeness_spine_hourly)
    completeness_spine_hourly %<>% merge(hourly_data[sample_duration == '1 HOUR'], by = c('date','hour','station','monitor'))
    complete <- completeness_spine_hourly[, .(data_complete = fifelse(sum(!is.na(pm25)) >= 18, T, F)), by = list(date, station, monitor)]
    quasicomplete <- completeness_spine_hourly[, pm25 := fifelse(is.na(pm25), 0, pm25)][, .(data_quasicomplete = fifelse(mean(pm25) >= 35, T, F)), by = list(date, station, monitor)]
    full_complete <- complete %>% merge(quasicomplete)
    full_complete <- full_complete[, .(include = fifelse(data_complete | data_quasicomplete, T, F)), by = list(date, station, monitor)]
    hourly_data <- merge(hourly_data, full_complete, by = c('date', 'station', 'monitor'), all.x = T)[is.na(include) | include == TRUE]
    
    # Process flags
    hourly_data[, c('flag','description') := tstrsplit(sub(" - ", "<<SPLIT>>", qualifier), "<<SPLIT>>")]
    hourly_data[,`:=`(exclusion_requested = fifelse(flag %in% c("E", "RA", "RB", "RC", "RD", "RE", "RF", "RG", "RH", "RI", "RJ", "RK", "RM", "RN", "RO", "RP", "RQ", "RR", "RS", "RT", "RU"), 1, 0))][,
                                                                                                                                                                                                      `:=`(other_flags = fifelse(!is.na(flag) & exclusion_requested == 0, 1, 0),
                                                                                                                                                                                                           fire_exclusion = fifelse(flag %in% c("E", "RF", "RG", "RH", "RM", "RP", "RT", "RU"), 1, 0),
                                                                                                                                                                                                           fire_inform = fifelse(flag %in% c("F", "IF", "IG", "IH", "IM", "IP", "IT", "IU"), 1, 0),
                                                                                                                                                                                                           na_flags = fifelse(!is.na(flag) & is.na(pm25), 1, 0))]
    hourly_data[, date := ymd(date)]
    hourly_data %<>% merge(exclusion_panel, by = c('station','monitor','date'), all.x = T)
    hourly_data[, exclusion_granted := fifelse(is.na(exclusion_granted), 0, 1)]
    hourly_data %<>% mutate(across(exclusion_requested:exclusion_granted, ~ fifelse(sample_duration == '24 HOUR', .x*24, .x))) %>% as.data.table()
    
    # Summarize the data for the current state and period
    daily_summary <- hourly_data[, .(daily_avg_excl_flags = mean(pm25[exclusion_requested == 0], na.rm = T),
                                     daily_avg_incl_flags = mean(pm25, na.rm = T),
                                     excluded_hours = sum(exclusion_requested, na.rm = T),
                                     other_flags_hours = sum(other_flags, na.rm = T),
                                     fire_exclusion_hours = sum(fire_exclusion, na.rm = T),
                                     fire_inform_hours = sum(fire_inform, na.rm = T),
                                     na_flags = sum(na_flags, na.rm = T),
                                     exclusion_granted = sum(exclusion_granted, na.rm = T)), by = list(station, monitor, date, method)]
    
    #Merge daily data with monitor data
    merged <- daily_summary %>% 
      merge(monitors %>% select(-concurred_exclusions), by = c('station','monitor'), all.x = T)
    
    merged <- merged[, .(daily_avg_excl_flags =
                           fifelse(is.na(mean(daily_avg_excl_flags[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                                        mean(daily_avg_excl_flags[is.na(naaqs_primary_monitor)], na.rm = T),
                                                        mean(daily_avg_excl_flags[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T)),
                         
                         daily_avg_incl_flags =
                           fifelse(is.na(mean(daily_avg_incl_flags[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                                        mean(daily_avg_incl_flags[is.na(naaqs_primary_monitor)], na.rm = T),
                                                        mean(daily_avg_incl_flags[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T)),
                         
                         excluded_hours =
                           fifelse(is.na(mean(excluded_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                                  mean(excluded_hours[is.na(naaqs_primary_monitor)], na.rm = T),
                                                  mean(excluded_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T)),
                         
                         other_flags_hours =
                           fifelse(is.na(mean(other_flags_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                                     mean(other_flags_hours[is.na(naaqs_primary_monitor)], na.rm = T),
                                                     mean(other_flags_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T)),
                         
                         fire_exclusion_hours =
                           fifelse(is.na(mean(fire_exclusion_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                                        mean(fire_exclusion_hours[is.na(naaqs_primary_monitor)], na.rm = T),
                                                        mean(fire_exclusion_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T)),
                         
                         fire_inform_hours =
                           fifelse(is.na(mean(fire_inform_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                                     mean(fire_inform_hours[is.na(naaqs_primary_monitor)], na.rm = T),
                                                     mean(fire_inform_hours[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T)),
                         
                         na_flags =
                           fifelse(is.na(mean(na_flags[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                            mean(na_flags[is.na(naaqs_primary_monitor)], na.rm = T),
                                            mean(na_flags[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T)),
                         
                         exclusion_granted =
                           fifelse(is.na(mean(exclusion_granted[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)])),
                                                     mean(exclusion_granted[is.na(naaqs_primary_monitor)], na.rm = T),
                                                     mean(exclusion_granted[naaqs_primary_monitor == 'Y' & !is.na(naaqs_primary_monitor)], na.rm = T))),
                     
                     by = list(date, station)]
    
    merged[, month := month(date)][, `:=`(quarter = fcase(month < 4, 1, month < 7, 2, month < 9, 3, default = 4), year = year(date))]
    
    # daily_summary %>% filter(station_id %in% excluded)
    # setdiff(unique(merged$station_id[merged$naaqs_primary_monitor == 'Y' &!is.na(merged$naaqs_primary_monitor)]), unique(daily_summary$station_id))
    
    # Append the summary data to the final data frame
    final_summary[[length(final_summary)+1]] <- merged
    
    # Print progress
    print(paste("Finished state", this_state, "for period", period$bdate, "to", period$edate))
  }
  
  final_summary <- rbindlist(final_summary)
  
  p98_annual <- final_summary[, .(p98 = fnth(daily_avg_incl_flags, n = 0.98, ties = 3, na.rm = T)), by = list(station, year)]
  mean_annual <- final_summary[, .(quarter_mean = fmean(daily_avg_incl_flags, na.rm = T)), by = list(station, quarter, year)][, .(annual_mean = fmean(quarter_mean, na.rm = T)), by = list(station, year)]
  
  station_design_lagged_values <- list()
  
  for (this_year in 2018:2023) {
    
    station_design_lagged_values[[length(station_design_lagged_values)+1]] <- 
      rbind(p98_annual[year == this_year], p98_annual[year == this_year - 1], p98_annual[year == this_year - 2])[, .(p98 = mean(p98), obs = .N), by = station][obs > 2] %>% merge(rbind(mean_annual[year == this_year], mean_annual[year == this_year - 1], mean_annual[year == this_year - 2])[, .(annual_mean = mean(annual_mean), obs = .N), by = station][obs > 2]) %>% mutate(year = this_year) %>% select(-obs)
    
  }
  
  station_design_lagged_values <- rbindlist(station_design_lagged_values)
  
  final_summary %<>% merge(station_design_lagged_values, by = c('station','year'), all.x = T)
  
  #saving each state file
  arrow::write_parquet(final_summary, sink = paste0("hourly_data_bystate/", this_state, '.pq'))
  
}

future_lapply(states, state_download)

exclusion_files <- list.files(path = 'hourly_data_bystate/', pattern = '.pq', full.names = T)
exclusions <- arrow::open_dataset(sources = exclusion_files, format = 'parquet') %>% mutate(date = ymd(date)) %>% collect()

years <- 2016:2023
months <- str_pad(1:12, 2, 'left', '0')
smoke_pm <- list()
for (this_year in years) {
  for (this_month in months) {
    smoke_pm[[length(smoke_pm)+1]] <- readRDS(paste0('station_smokePM_update/station_smokePM_',this_year,'_',this_month,'.rds'))
  }
}
smoke_pm <- rbindlist(smoke_pm) %>% select(station = id, date, pm25_smokepm = pm25, smoke_day, smoke_pm = smokePM)

full_df <- exclusions %>% select(-year, -month, -quarter) %>% merge(smoke_pm, all = T, by = c('station','date'))
exclusion[, exclusion := fifelse(excluded_hours > 0, 1, 0)]
full_df[, fire_exclusion := fifelse(fire_exclusion_hours > 0, 1, 0)]
full_df[, `:=`(p98_close = factor(fcase(p98 < 20, 'x<20', p98 < 25, '20<=x<25', p98 < 30, '25<=x<30', p98 < 35, '30<=x<35', default = '35<x (noncompliant)'), levels = c('x<20','20<=x<25', '25<=x<30', '30<=x<35', '35<x (noncompliant)')),
               annual_close = factor(fcase(annual_mean < 8, 'x<8', annual_mean < 9, '8<=x<9', annual_mean < 10, '9<=x<10', annual_mean < 11, '10<=x<11', annual_mean < 12, '11<=x<12', default = '12<x (noncompliant)'), levels = c('x<8', '8<=x<9', '9<=x<10', '10<=x<11', '11<=x<12', '12<x (noncompliant)')))]

arrow::write_feather(full_df, sink = 'hourly_data_bystate/exclusions_smokePM.feather')