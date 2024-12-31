library(tidyverse)
library(magrittr)
library(sf)
library(cowplot)

source("scripts/setup/00_03_load_paths.R")

# load the 10km grid and the daily smoke PM2.5 predictions 
grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
smokePM_pred <- readRDS(file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101-20061231.rds"))

# calculate annual average smoke PM2.5 in each year, by calculating sum then dividing by # of days in the year
annual_smokePM <- smokePM_pred %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(grid_id_10km, year) %>% 
  summarise(smokePM = sum(smokePM_pred), .groups = "drop") %>% 
  # pivot wide, then back to long, to fill any missing years with zeros
  pivot_wider(values_from = smokePM, names_from = year, names_prefix = "year_", values_fill = 0) %>% 
  pivot_longer(starts_with("year"), values_to = "smokePM", names_to = "year", names_prefix = "year_") %>% 
  # identify leap years for dividing by 365
  mutate(year = as.numeric(year)) %>% 
  mutate(y_days = 365 + lubridate::leap_year(year)*1, 
         smokePM = smokePM/y_days)

# join with the grid for plotting, then make small multiples 
right_join(grid,
           # top code the smoke PM to 8 for visibility
           annual_smokePM %>% 
             mutate(smokePM = pmin(smokePM, 8)),
           by = c("ID" = "grid_id_10km")) %>%
  {ggplot(data = ., aes(color = smokePM, fill = smokePM)) +
      geom_sf() + 
      facet_wrap(~year, nrow = 4, ncol = 5) +
      # customize the color gradient to show variation at the low end 
      scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                            name = expression(atop("Annual average",
                                                   paste(PM[2.5], " (", mu, "g/", m^3, ")"))),
                            aesthetics = c("fill", "color"), 
                            values = scales::rescale(sinh(seq(0, 3, length.out = 100))),
                            breaks = seq(0, 8, by = 2),
                            labels = c(seq(0, 6, by = 2), ">8"),
                            guide = guide_colorbar(barwidth = 12, 
                                                   direction = "horizontal",
                                                   title.theme = element_text(size = 16))) +
      theme_void() + 
      theme(text = element_text(size = 20), 
            legend.position = "inside", 
            legend.position.inside = c(0.8, 0.15))} %>% 
  ggsave(file.path(path_figures, "annual_small_multiples.png"), ., width = 15, height = 9.33)
