library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)

source("scripts/setup/00_03_load_paths.R")

stationPM <- list.files(file.path(path_data, "/3_intermediate/station_smokePM_auto/"),
                        full.names = T) %>% 
  map(readRDS) %>% list_rbind %>% 
  mutate(across(year:day, as.numeric))

stationPM %>% 
  ungroup %>% 
  summarise(n = n(), 
            .by = c(id, year)) %>% 
  arrange(n) %>% 
  mutate(cat = case_when(n >= 1 & n <= 50 ~ "1 - 50", 
                         n > 50 & n <= 100 ~ "51 - 100", 
                         n > 100 & n <= 200 ~ "101 - 200", 
                         n > 200 ~ ">200", 
                         T ~ "error"),
         cat = factor(cat, levels = unique(cat), ordered = T)) %>% 
  summarise(n = n(), 
            .by = c(year, cat)) %>% 
  arrange(year, desc(cat)) %>% 
  mutate(ymax = cumsum(n),
         ymin = cumsum(replace_na(lag(n, 1), 0)),
         .by = year) %>% 
  {ggplot(data = ., 
          aes(x = year, ymin = ymin, ymax = ymax, color = cat, fill = cat)) + 
      geom_ribbon() + 
      theme_classic() + 
      scale_color_manual(name = "number of days\nwith observations",
                         values = c("red", "blue", "black", "grey"), 
                         aesthetics = c("color", "fill")) + 
      scale_x_continuous(expand = expansion(mult = 0)) + 
      scale_y_continuous(expand = expansion(mult = 0)) + 
      ylab("number of stations")}  %>% 
  ggsave(filename = file.path(path_figures, "obs_per_station.png"), 
         width = 5, height = 3.5)
