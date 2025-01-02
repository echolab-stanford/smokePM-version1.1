# create SI figure with fire season length 
library(tidyverse)
library(magrittr)
library(sf)
library(cowplot)

source("scripts/setup/00_03_load_paths.R")

# load the 10km grid, the final predictions, and the population density in each 10km grid cell
grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
smokePM_pred <- readRDS(file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101-20061231.rds"))
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"), 
                  full.names = TRUE) %>% 
  purrr::map(read.csv) %>% list_rbind

# calculate area in each grid 10km grid cell to get total population
grid %<>% mutate(area = st_area(geometry) %>% unclass)

# calculate the average population exposure for each day of the year
smokePM_pred %>% 
  left_join(pop %>% 
              left_join(grid) %>% 
              mutate(pop = mean*area) %>% 
              transmute(grid_id_10km = ID, 
                        pct_pop = pop/sum(pop), 
                        pop)) %>% 
  summarise(avg_smokePM = sum(pct_pop*smokePM_pred), 
            .by = date) -> pop_exposure

# calculate the "smoke season" for each year
pop_exposure %>% 
  # first join with full set of dates
  full_join(data.frame(date = seq.Date(as.Date("2006-01-01"), as.Date("2023-12-31"), "days"))) %>% 
  # fill missing dates with 0 since we didn't estimate smoke PM on those days
  mutate(across(c(avg_smokePM), 
                ~replace_na(.x, 0))) %>% 
  # add year and day of year information for plotting 
  mutate(year = year(date),
         y_color = ifelse(year < 2020, NA, year) %>% as.character, 
         # convert everything to days in 2000 to allow for plotting over the same x axis range
         day = paste0("2000-", month(date), "-", day(date)) %>% as.Date()) %>% 
  select(-date, -ends_with("_pop")) %>% 
  # leave the values for each year, and also calculate an average for each day of the year using 2006 - 2019
  {rbind(mutate(., 
                year = as.character(year), 
                type = "annual"),
         filter(., year <=2019) %>% 
           summarise(avg_smokePM = mean(avg_smokePM), 
                     .by = day) %>% 
           mutate(year = "2006-2019\naverage", 
                  type = "average", 
                  y_color = "2006-2019\naverage"))} %>%
  # rank the days by average smoke PM 
  mutate(date_rank = rank(desc(avg_smokePM))) %>% 
  # define the smoke seasons as days where pop-avg exposure to smoke PM2.5 is > 1ug 
  # calculate first and last day of the year where this happens
  mutate(smoke_season_day = avg_smokePM > 1,
         smoke_season_day_start = min(day[smoke_season_day]), 
         smoke_season_day_end = max(day[smoke_season_day]), 
         .by = year) %>%  
  # remove the 2006-2019 individual years and we'll just plot the average for visibility
  filter(!is.na(y_color)) %>%
  # plot a line for each year and one for the 2006-2019 average
  {ggplot(data = ., 
          aes(y = avg_smokePM, 
              color = y_color, 
              linewidth = I(case_when(type == "annual" ~ 0.6, 
                                      T ~ 1.2)),
              group = year)) + 
      geom_line(aes(x = day)) + 
      # add points for the top 10 days to match Fig 2 with population exposure
      geom_point(aes(alpha = I(ifelse(date_rank <= 10, 1, 0)), 
                     x = day)) + 
      # add line ranes from the first to the last day of the smoke season for each year
      geom_linerange(data = select(., year, y_color, type, smoke_season_day_start, smoke_season_day_end) %>% unique, 
                     aes(xmin = smoke_season_day_start, xmax = smoke_season_day_end, 
                         y = -0.5 + ifelse(type == "average", 0, (-as.numeric(year) + 2019)*0.5))) + 
      # label those lines with the year and season length 
      geom_text(data = filter(., !is.na(y_color)) %>% filter(day == max(day), .by = year) %>% 
                  mutate(season_length = difftime(smoke_season_day_end, smoke_season_day_start, units = "days") + 1),
                aes(x = day + as.difftime(-20, units = "days"), 
                    y = -0.5 + ifelse(type == "average", 0.1, (-as.numeric(year) + 2019)*0.5), 
                    label = paste0(year, ", ", season_length, " days")),
                hjust = 0, vjust = 0.5, lineheight = 0.65) +
      # adjust aesthetics 
      geom_hline(yintercept = 1, linetype = "dashed") + 
      coord_cartesian(clip = "off") + 
      scale_color_manual(values = c("black", MetBrewer::met.brewer("Juarez", 4))) + 
      scale_x_date(date_labels = "%b",
                   breaks = seq.Date(as.Date("2000-02-01"), as.Date("2000-12-31"), by = "2 month"),
                   expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0.03)), breaks = c(0, 1, 2, 5, 10, 20, 30), 
                         trans = "pseudo_log") + 
      ylab(expression(paste("Daily smoke exposure (", mu, "g/", m^3, ")"))) + 
      theme_classic() + 
      theme(legend.position = "none", 
            axis.title.x = element_blank(),
            plot.margin = unit(c(20.5, 80, 5.5, 5.5), "points"))} %>% 
  ggsave(filename = file.path(path_figures, "smoke_season_length.png"), 
         width = 6, height = 4.25)
