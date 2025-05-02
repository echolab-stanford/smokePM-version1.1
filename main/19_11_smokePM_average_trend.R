library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)

source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

nonContig_stateFIPS <- c("02","60","66","15","72","78","69")
conus <-read_sf("data/cb_2023_us_state_20m") %>% filter(!(STATEFP %in% nonContig_stateFIPS))

grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

smokePM_pred <- file.path(path_output, "version1.1", "smokePM", "predictions", 
                          "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>% 
  mutate(smokePM_pred = pmax(0, smokePM_pred)) 

trends  <- smokePM_pred %>% 
  mutate(year = lubridate::year(date)) %>% 
  # calculate annual avg contribution of smokePM to daily PM
  summarise(annual_total_smokePM = sum(smokePM_pred), 
            annual_days_over50 = sum(smokePM_pred > 50), 
            annual_days_over100 = sum(smokePM_pred > 100), 
            .by = c(year, grid_id_10km)) %>% 
  mutate(y_days = 365 + leap_year(year)*1, 
         annual_daily_smokePM = annual_total_smokePM/y_days) %>%
  select(-annual_total_smokePM) %>% 
  # join with full set of years and grid cells to fill in zeros
  {left_join(expand.grid(year = unique(.$year),
                         grid_id_10km = unique(.$grid_id_10km)), 
             .)} %>% 
  replace_na(list(annual_daily_smokePM = 0, 
                  annual_days_over50 = 0, 
                  annual_days_over100 = 0)) %>% 
  # fit trend lines
  select(-y_days) %>% 
  pivot_longer(starts_with("annual")) %>% 
  nest_by(grid_id_10km, name) %>% 
  transmute(trend = coef(lm(value ~ year, data = data))[2], 
            avg = mean(data$value)) 

# plot a map of the decadal change
purrr::pmap(data.frame(panel_name = c("annual_daily_smokePM", "annual_days_over50",
                                      "annual_daily_smokePM", "annual_days_over50"), 
                       panel_type = c("avg", "avg", "trend", "trend"),
                       min_bound = -Inf, 
                       max_bound = c(2, 3, 0.4, 1),
                       legend_title = I(list(expression(paste(mu, "g/", m^3)), 
                                             expression(paste("days >50", mu, "g/", m^3)), 
                                             expression(paste(Delta, mu, "g/", m^3)), 
                                             expression(paste(Delta, "days >50", mu, "g/", m^3)))),
                       color_breaks = I(list(seq(-0.5, 2, by = 0.5), 
                                             seq(0, 3, by = 1), 
                                             seq(0, 0.4, by = 0.1), 
                                             seq(-0.25, 1, by = 0.25))),
                       color_rescaler = I(list(scales::rescale, scales::rescale, mid_rescaler(0), mid_rescaler(0))),
                       color_values = I(list(cmocean::cmocean("matter",
                                                              start = 0, 
                                                              end = 1, 
                                                              direction = -1)(101), 
                                             cmocean::cmocean("tempo",
                                                              start = 0, 
                                                              end = 1)(101), 
                                             cmocean::cmocean("balance",
                                                              start = 0.05, 
                                                              end = 0.95)(101), 
                                             rev(colorRampPalette(RColorBrewer::brewer.pal(11, "BrBG"))(101))))), 
            function(panel_name, panel_type, min_bound, max_bound, 
                     legend_title, color_breaks, 
                     color_rescaler, color_values){
              color_labels = as.character(color_breaks)
              if(min_bound > -Inf){color_labels[1] = paste0("<", color_labels[1])}
              if(max_bound < Inf){color_labels[length(color_labels)] = paste0(">", color_labels[length(color_labels)])}
              grid %>% 
                left_join(trends %>% 
                            pivot_longer(c(trend, avg), names_to = "type") %>% 
                            filter(name == panel_name, type == panel_type), 
                          by = c("ID" = "grid_id_10km")) %>% 
                mutate(trend = pmax(pmin(max_bound, value), min_bound)) %>%
                {ggplot(data = .) + 
                    geom_sf(aes(color = trend, fill = trend)) + 
                    geom_sf(data = conus, 
                            color = "grey20", lwd = 0.1, fill = NA) + 
                    scale_color_gradientn(aesthetics = c("color", "fill"), 
                                          name = legend_title, 
                                          colors =  color_values,
                                          breaks = color_breaks, 
                                          labels = color_labels,
                                          values = scales::rescale(sinh(seq(-2, 2, length.out = 101))),
                                          rescaler = color_rescaler,
                                          guide = guide_colorbar(barheight = 5.5)
                    ) + 
                    theme_void() + 
                    theme(legend.position = "inside", 
                          plot.margin = unit(c(0, 5, 0, 0), "points"),
                          legend.position.inside = c(.83, 0.28),
                          legend.title = element_text(size = 9),
                          legend.justification = "left")} %>% return
            }) -> avg_trend_maps

plot_grid(plotlist = avg_trend_maps,
          labels = c("a) Annual average", 
                     "b) Number of extreme days",
                     "c) Trend in annual average", 
                     "d) Trend in number of extreme days"),
          hjust = -0.1, label_x = 0.01, 
          nrow = 2) %>% 
  ggsave(filenam = file.path(path_figures, "change_maps.png"), 
         width = 9.5, height = 6, bg = "white")
