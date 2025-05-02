library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

purple <- arrow::read_parquet("./data/PA_daily_topcoded.parquet") %>% 
  filter(nobs > 12)

# find 10km grid cell that each monitor is in
grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
purple_ll <- purple %>% select(sensor_index, lon, lat) %>% unique %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326, remove = FALSE)
# identify intersection grid cell, and add that information to the df
purple_ll$grid_10km = grid$ID[as.numeric(st_intersects(purple_ll, grid))]

list.files(file.path(path_data, "3_intermediate", "filled_smoke"), 
           full.names = T, pattern = "rds") %>% 
  # grep("2024", ., value = T, invert = T) %>% 
  purrr::map(function(x){
    out <- readRDS(x) %>% filter(grid_id_10km %in% purple_ll$grid_10km) 
    if(class(out$date) == "character"){
      out %<>% mutate(date = as.Date(date, format = "%Y%m%d"))
    }
    return(out)
  }) %>% list_rbind -> smoke_days

# use 10km grid cell id to merge in smoke day 
purple %<>% 
  mutate(date = date(date_local) + days(1)) %>%  
  left_join(st_drop_geometry(purple_ll)) %>% 
  filter(!is.na(grid_10km)) %>% 
  left_join(smoke_days, 
            by = c("date", "grid_10km" = "grid_id_10km")) %>% 
  mutate(smoke_day = replace_na(smoke_day, 0)) 
rm(smoke_days)

# calculate non-smoke median
purple %<>% 
  mutate(month = month(date), year = year(date), 
         loc_id = paste0(sensor_index, "_", lon, "_", lat)) %>% 
  {left_join(., 
             nonsmoke_medians(., 
                              daily_mean_pm, smoke_day, loc_id, month, year), 
             by = c("loc_id", "month", "year"))} 
# subtract off non-smoke median to leave behind anomalies on smoke days
purple %<>% mutate(smoke_pm_anom = (daily_mean_pm - daily_mean_pm_med_3yr)*smoke_day)

#quick plot to make sure it looks reasonable
# set.seed(1001)
# purple %>% filter(nobs_3yr > 30 & nobs > 12) %>% 
#   filter(., loc_id %in% (unique(.$loc_id) %>% sample(2))) %>% 
#   ggplot(aes(x = date)) + 
#   geom_line(aes(y = daily_mean_pm), color = "black") +
#   geom_line(aes(y = daily_mean_pm_med_3yr), color = "blue") +
#   geom_line(aes(y = pmax(smoke_pm_anom, 0)), color = "red") +
#   theme_classic() +
#   facet_wrap(~loc_id, scales = "free", nrow = 2)


# load full ML predictions and merge into the purple air data 
smokePM_pred <- file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

smokePM_pred_v1 <- 
  readRDS("output/version1.0/smokePM2pt5_predictions_daily_10km_20060101-20201231.rds") %>%
  mutate(smokePM_pred = pmax(smokePM_pred, 0))

purple %<>% 
  left_join(smokePM_pred %>% rename(pred_new = smokePM_pred), 
            by = c("date", "grid_10km" = "grid_id_10km"))  %>% 
  left_join(smokePM_pred_v1 %>% rename(pred_old = smokePM_pred), 
            by = c("date", "grid_10km" = "grid_id_10km")) 
rm(smokePM_pred, smokePM_pred_v1)

# filter to smoke days and plot comparison between purple air smoke PM2.5 and our predicted smoke PM2.5
library(fixest)
feols(smoke_pm_anom ~ pred_new,  
      data = purple %>% 
        mutate(smoke_pm_anom = pmax(0, smoke_pm_anom)) %>% 
        filter(smoke_day == 1) %>% 
        filter(nobs_3yr > 10 & nobs > 12)) %>% 
  r2("r2") -> r2_new_full

feols(smoke_pm_anom ~ pred_new,  
      data = purple %>% 
        mutate(smoke_pm_anom = pmax(0, smoke_pm_anom)) %>% 
        filter(year <= 2020) %>%
        filter(smoke_day == 1) %>% 
        filter(nobs_3yr > 10 & nobs > 12)) %>% 
  r2("r2") -> r2_new_2020

feols(smoke_pm_anom ~ pred_old,  
      data = purple %>% 
        mutate(smoke_pm_anom = pmax(0, smoke_pm_anom)) %>% 
        filter(year <= 2020) %>%
        filter(smoke_day == 1) %>% 
        filter(nobs_3yr > 10 & nobs > 12)) %>% 
  r2("r2") -> r2_old_2020

nonContig_stateFIPS <- c("02","60","66","15","72","78","69")
conus <-read_sf("data/cb_2023_us_state_20m") %>% filter(!(STATEFP %in% nonContig_stateFIPS))

plot_grid(
  plot_grid(ggplot() + 
              geom_sf(data = conus) + 
              geom_sf(data = purple_ll %>% filter(!is.na(grid_10km)) %>% 
                        left_join(purple %>% 
                                    filter(smoke_day == 1) %>% 
                                    filter(nobs_3yr > 10 & nobs > 12) %>%
                                    summarise(n = n(), 
                                              .by = c(sensor_index, lon, lat))) %>% 
                        filter(!is.na(n)), 
                      aes(color = n), cex = 0.1) + 
              scale_color_gradientn(colors = cmocean::cmocean('dense')(100),
                                    # cmocean::cmocean('thermal')(100),
                                    name = "# of obs",
                                    trans = "pseudo_log",
                                    guide = guide_colourbar(theme = theme(legend.direction = "horizontal", 
                                                                          legend.title.position = "top", 
                                                                          legend.key.height = unit(10, "points"),
                                                                          legend.key.width = unit(120, "points"),
                                    )),
                                    breaks = c(0, 1, 5, 20, 50, 200, 500)) +
              theme_void() + 
              theme(legend.position = "inside", 
                    legend.position.inside = c(0.25, 0.05)),
            ggplot(mapping = aes(x = date)) + 
              geom_histogram(data = purple %>% 
                               filter(nobs_3yr > 10 & nobs > 12), 
                             bins = ((as.integer(max(purple$date) - min(purple$date)) + 1) %/% 14) + 1, 
                             closed = "left", fill = "grey40", color = NA) + 
              geom_histogram(data = purple %>% 
                               filter(smoke_day == 1) %>% 
                               filter(nobs_3yr > 10 & nobs > 12), 
                             bins = ((as.integer(max(purple$date) - min(purple$date)) + 1) %/% 14) + 1, 
                             closed = "left", fill = "red3", color = NA) + 
              annotate("text", x = as.Date("2020-01-01"), y = 85000, color = "grey40", 
                       label = "all observations") + 
              annotate("text", x = as.Date("2018-06-01"), y = 32000, color = "red3", 
                       label = "smoke day\nobservations") + ylab("# of observations") + 
              theme_classic() + 
              theme(plot.margin = unit(c(25.5, 5.5, 5.5, 5.5), "points"),
                    axis.title.x = element_blank()), 
            ncol = 1, rel_heights = c(1, 1), hjust = -0.02,
            labels = c("a) locations of PurpleAir monitors",
                       "b) temporal distritbuion of obsevations")), 
  plot_grid(purple %>% mutate(smoke_pm_anom = pmax(0, smoke_pm_anom)) %>% 
              filter(smoke_day == 1) %>% 
              filter(nobs_3yr > 10 & nobs > 12) %>%
              ggplot(aes(x = pred_new, y = smoke_pm_anom)) + 
              geom_bin2d(bins = 60) + 
              geom_abline(intercept = 0, slope = 1, color = "grey30") +
              scale_fill_gradientn(colors = cmocean::cmocean('dense')(100),
                                   name = "# of obs",
                                   # cmocean::cmocean('thermal')(100),
                                   trans = "pseudo_log",
                                   breaks = c(0, 1, 10, 100, 1000, 10000)) +
              scale_x_continuous(name = expression(predicted~smokePM[2.5]),#"predicted PM2.5",
                                 trans = "pseudo_log",
                                 breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 500, 1000),
                                 expand = c(0, 0)) +
              scale_y_continuous(name = expression(observed~PurpleAir~smokePM[2.5]),#"observed PurpleAir PM2.5",
                                 trans = "pseudo_log",
                                 breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 500, 1000),
                                 expand = c(0, 0)) +
              annotate("text", x = 10, y = 350, #label = expression('R^2 =', round(r2, 3))) + 
                       label = paste('R^2 == ', round(r2_new_full, 3)), parse = T, size = 6) + 
              theme_classic() + theme(plot.margin = unit(c(20.5, -2, 1, 5.5), "points")), 
            purple %>% mutate(smoke_pm_anom = pmax(0, smoke_pm_anom)) %>% 
              filter(smoke_day == 1) %>% 
              filter(nobs_3yr > 10 & nobs > 12) %>%
              filter(year <= 2020) %>%
              pivot_longer(c(pred_new, pred_old)) %>% 
              mutate(name = ifelse(name == "pred_new", "updated predictions", "Childs et al. 2022 predictions")) %>%
              ggplot(aes(x = value, y = smoke_pm_anom)) + 
              geom_bin2d(bins = 60) + 
              geom_abline(intercept = 0, slope = 1, color = "grey30") +
              facet_wrap(~name) + 
              scale_fill_gradientn(colors = cmocean::cmocean('dense')(100),
                                   name = "# of obs",
                                   trans = "pseudo_log",
                                   breaks = c(0, 1, 10, 100, 1000, 10000)) +
              scale_x_continuous(name = expression(predicted~smokePM[2.5]), 
                                 trans = "pseudo_log",
                                 breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 500, 1000),
                                 expand = c(0, 0)) +
              scale_y_continuous(name = expression(observed~PurpleAir~smokePM[2.5]),#"observed PurpleAir PM2.5",
                                 trans = "pseudo_log",
                                 breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 500, 1000),
                                 expand = c(0, 0)) +
              geom_text(data = data.frame(name = c("updated predictions", "Childs et al. 2022 predictions"), 
                                          r2 = c(r2_new_2020, r2_old_2020)) %>% 
                          mutate(lab = paste0('R^2 == ', round(r2, 3))), 
                        mapping = aes(label = lab), 
                        x = 1.5, y = 5.5, parse = T, size = 4, inherit.aes = FALSE) + 
              # annotate("text", x = 10, y = 350, #label = expression('R^2 =', round(r2, 3))) + 
              #          label = paste('R^2 == ', round(r2, 3)), parse = T, size = 6) + 
              theme_classic() + 
              theme(plot.margin = unit(c(12.5, -2, 5.5, 5.5), "points"), 
                    text = element_text(size = 9.5),
                    strip.background = element_blank()), 
            rel_heights = c(1, 0.75),
            hjust = -0.02,
            labels = c("c) observed and predicted smoke PM2.5 (2016 - 2023)", 
                       "d) observed and predicted smoke PM2.5 (2016 - 2020)"), 
            nrow = 2, ncol = 1),
  nrow = 1, rel_widths = c(0.7, 1), 
  # labels = c("", "c) observed and predicted smoke PM2.5"), 
  hjust = -0.02) %>% 
  ggsave(filename = file.path(path_figures, "purpleair_comparison.png"), 
         width = 9, height = 6, bg = "white")
