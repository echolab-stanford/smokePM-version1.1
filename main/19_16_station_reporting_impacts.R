library(magrittr)
library(tidyverse)
library(sf)
library(cowplot)
library(fixest)

source("scripts/setup/00_03_load_paths.R")

# load the 10km grid and the daily smoke PM2.5 predictions 
grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

smokePM_pred <- file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>% 
  mutate(smokePM_pred = pmax(0, smokePM_pred))

# load the station data used as input into the training 
stationPM <- list.files(file.path(path_data, "/3_intermediate/station_smokePM_auto/"),
                        full.names = T) %>% 
  map(readRDS) %>% list_rbind %>% 
  mutate(across(year:day, as.numeric))

obs_per_day <- stationPM %>% 
  summarise(n_station = n_distinct(id), 
            n_smoke_station = n_distinct(id[smoke_day == 1]),
            .by = date)

# calculate daily pop-exposure 
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"), 
                  full.names = TRUE) %>% 
  purrr::map(read.csv) %>% list_rbind

# calculate area in each grid 10km grid cell to get total population
grid %<>% mutate(area = st_area(geometry) %>% unclass)
smokePM_pred %>% 
  left_join(pop %>% 
              left_join(grid) %>% 
              mutate(pop = mean*area) %>% 
              transmute(grid_id_10km = ID, 
                        pct_pop = pop/sum(pop), 
                        pop)) %>% 
  summarise(avg_smokePM = sum(pct_pop*smokePM_pred), 
            above10_pop = sum(pop*(smokePM_pred > 10)),
            .by = date) -> pop_exposure

fixest::feols(avg_smokePM ~ n_station + as.factor(year) + as.factor(month), 
              data =  full_join(obs_per_day, 
                                pop_exposure) %>% 
                mutate(avg_smokePM = replace_na(avg_smokePM, 0), 
                       year = year(date), 
                       month = month(date))) -> mod

plot_grid(obs_per_day %>% 
            mutate(year = year(date), 
                   month = month(date)) %>% 
            summarise(.by = c(year, month), 
                      date = median(date),
                      min = min(n_station), 
                      max = max(n_station), 
                      avg = mean(n_station)) %>% 
            ggplot(aes(x = date)) + 
            geom_ribbon(aes(ymin = min, ymax = max), alpha = 0.2) + 
            geom_line(aes(y = avg)) + 
            facet_wrap(~year, scales = "free_x") + 
            scale_x_date(date_breaks = "3 month",
                         date_labels = "%b") + 
            xlab("month") +
            ylab("# of stations by month") + 
            theme_classic() + 
            theme(strip.background = element_blank(),
                  plot.margin = unit(c(15.5, 5.5, 5.5, 5.5), "points")), 
          full_join(obs_per_day, 
                    pop_exposure) %>% 
            mutate(avg_smokePM = replace_na(avg_smokePM, 0)) %>% 
            ggplot(aes(x = n_station, y = avg_smokePM)) + 
            geom_point(alpha = 0.2) + 
            theme_classic() + theme(plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points")) +
            annotate("text", label = paste0("smoke PM2.5 = ", 
                                            round(coef(mod)["n_station"], 5), 
                                            " x # stations + month + year"),
                     hjust = "left",
                     x = 350, y = 25) + 
            annotate("text", label = paste0("p-value = ", 
                                            round(pvalue(mod)["n_station"], 5)),
                     hjust = "left",
                     x = 350, y = 22) + 
            xlab("daily number of station-observations") + 
            ylab(expression(paste("pop-average daily smoke ", PM[2.5], "(", mu, "g", m^{-3}, ")"))), 
          ncol= 1, labels = c("a) station-observations by month", 
                              "b) impact of station reporting on average exposure"), 
          rel_heights = c(1, 0.7),
          vjust = 1,
          hjust = 0, label_x = 0.01) %>% 
  ggsave(filename = file.path(path_figures, "station_reporting.png"), 
         width = 6, height = 8)

          

