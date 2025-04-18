library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)
library(ncdf4)

source("scripts/setup/00_03_load_paths.R")

grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

smokePM_pred <- file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>% 
  mutate(smokePM_pred = pmax(0, smokePM_pred))

nc_ll <- nc_open("data/SE prescribed fire/MEAN_Rx_BI_FUSED_PM25_TOT_AVG_12US2_2015.nc")

nc_rx_pm25 <- nc_open("data/SE prescribed fire/OneDrive_1_4-7-2025/MEAN_Rx_BI_FUSED_PM25_TOT_AVG_12US2_2015_2020.nc")

grid %>% 
  st_filter(st_as_sfc(st_bbox(c(range(as.vector(ncvar_get(nc_ll, "Lon"))), range(as.vector(ncvar_get(nc_ll, "Lat")))) %>% set_names(c("xmin", "xmax", "ymin", "ymax")),
                              crs = 4326))) -> se_grid
plot_states <- read_sf("data/cb_2023_us_state_20m") %>% st_transform(st_crs(se_grid)) %>% st_filter(se_grid)

my_date <- as.Date(c("2016-03-23", "2017-02-18", "2019-06-03")) #2016-03-23") #2019-06-03") #2015-07-01") #2019-06-03")
# good-ish match -- 2016-03-23 (also 2020-03-08)
# bad match -- "2017-02-18"
# wf and no prescribed fire -- "2019-06-03" 
date_ind <- year(my_date)*1000 + yday(my_date)
ll_df <- data.frame(x = as.vector(ncvar_get(nc_ll, "Lon")), 
                    y = as.vector(ncvar_get(nc_ll, "Lat")))

list(ncvar_get(nc_rx_pm25, "PM25_TOT_AVG")[,,which(ncvar_get(nc_rx_pm25, "TFLAG")[1,] %in% date_ind)] %>% 
       apply(3, function(x){data.frame(z = as.vector(x))}) %>% 
       imap(~mutate(.x, date = my_date[.y]) %>% cbind(ll_df)) %>% 
       list_rbind() %>% 
       mutate(date = format(date, "%b %d, %Y"),
              date = factor(date, levels = unique(date), ordered = T)) %>% 
       ggplot(aes(x = x, y = y, color = z)) + 
       geom_point() + 
       geom_sf(data = plot_states,
               fill = NA, color = "white",
               inherit.aes = FALSE) +
       scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                             aesthetics = c("fill", "color"), 
                             limits = c(0, 20),
                             oob = scales::squish,
                             values = scales::rescale(sinh(seq(0, 1, length.out = 100)))) +
       theme_void() + 
       xlim(range(as.vector(ncvar_get(nc_ll, "Lon")))) + 
       ylim(range(as.vector(ncvar_get(nc_ll, "Lat")))) + 
       facet_wrap(~date, ncol = 1), 
     cross_join(se_grid,
                data.frame(date = my_date)) %>% 
       left_join(smokePM_pred, by = c("ID" = "grid_id_10km","date")) %>%
       mutate(smokePM_pred = replace_na(smokePM_pred, 0)) %>%
       mutate(date = format(date, "%b %d, %Y"),
              date = factor(date, levels = unique(date), ordered = T)) %>% 
       {ggplot(data = ., aes(color = smokePM_pred, fill = smokePM_pred)) + 
           geom_sf() + 
           geom_sf(data = plot_states,
                   fill = NA, color = "white",
                   inherit.aes = FALSE) +
           scale_color_gradientn(name = expression(paste(PM[2.5], "(", mu, "g", m^{-3}, ")")), 
                                 colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                                 aesthetics = c("fill", "color"), 
                                 limits = c(0, 20),
                                 oob = scales::squish,
                                 values = scales::rescale(sinh(seq(0, 1.5, length.out = 100)))) +
           xlim(range(as.vector(ncvar_get(nc_ll, "Lon")))) + 
           ylim(range(as.vector(ncvar_get(nc_ll, "Lat")))) + 
           facet_wrap(~date, ncol = 1) + 
           theme_void()}) %>% 
  {plot_grid(plot_grid(.[[1]] + theme(legend.position = "none", panel.spacing = unit(1, "points")), 
                       .[[2]] + theme(legend.position = "none", panel.spacing = unit(1, "points")) + 
                         theme(plot.margin = unit(c(1.75, 0, 0, 0), "lines")),
                       ncol = 2, align = "h",
                       hjust = -0.1, label_x = 0.01, 
                       labels = c("a) Maji et al. Rx PM2.5", "b) smoke PM2.5")), 
             get_legend(.[[2]] + guides(color = guide_colorbar(direction = "horizontal"), 
                                        fill = guide_colorbar(direction = "horizontal"))), 
             nrow = 2, rel_heights = c(1, 0.1))} %>% 
  ggsave(filename = file.path(path_figures, "prescribed_burn_comparison_se.png"), 
         width = 5.5, height = 8, bg = "white")

full_join(data.frame(rx_pm25 = ncvar_get(nc_rx_pm25, "PM25_TOT_AVG") %>% 
                       apply(3, mean),
                     year_doy = ncvar_get(nc_rx_pm25, "TFLAG")[1,]) %>% 
            mutate(year = substr(as.character(year_doy), 1, 4) %>% as.numeric, 
                   doy = substr(as.character(year_doy), 5, 7) %>% as.numeric, 
                   date = as.Date(doy-1, origin=paste0(year, "-01-01"))), 
          smokePM_pred %>% 
            filter(grid_id_10km %in% se_grid$ID) %>% 
            summarise(smoke_pm25 = sum(smokePM_pred)/nrow(se_grid), 
                      .by = date)) %>% 
  mutate(month = as.factor(month(date)),
         year = year(date),
         across(c(smoke_pm25, rx_pm25), ~replace_na(.x, 0))) %>% 
  filter(year >= 2015) %>%
  summarise(across(c(smoke_pm25, rx_pm25), mean), 
            .by = c(month, year)) %>% 
  pivot_longer(ends_with("pm25")) %>% 
  {plot_grid(rbind(filter(., year <= 2020), 
                   filter(., year <= 2020) %>% 
                     summarise(value = mean(value), 
                               .by = c(month, name)) %>% 
                     mutate(year = Inf)) %>%
               ggplot(aes(x = as.Date(paste0("2020-", month, "-01")), 
                          y = value, color = name, 
                          lwd = I(ifelse(is.finite(year), 0.5, 1.5)),
                          group = interaction(year, name))) + 
               geom_line() + 
               scale_x_date(date_labels = "%b", date_breaks = "month") + 
               ylab(expression(paste(PM[2.5], "(", mu, "g", m^{-3}, ")"))) + 
               scale_color_manual(name = "type", 
                                  values = c("#7fa074", "#b695bc"),
                                  # "#748f46", "#f05b43"), 
                                  labels = function(x){ifelse(x == "rx_pm25", 
                                                              "prescribed burn", 
                                                              "smoke")}) + 
               theme_classic() + 
               theme(axis.title.x = element_blank(), 
                     plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points"),
                     legend.title = element_blank(),
                     legend.position = "inside", 
                     legend.position.inside = c(0.2, 0.8)), 
             filter(., name == "smoke_pm25") %>%
               ggplot(aes(x = as.Date(paste0("2020-", month, "-01")), 
                          y = value, color = (year > 2020), 
                          group = interaction(year, name))) + 
               geom_line() + 
               scale_x_date(date_labels = "%b", date_breaks = "month") +
               ylab(expression(paste("smoke ", PM[2.5], "(", mu, "g", m^{-3}, ")"))) + 
               scale_color_manual(name = "time period", 
                                  values = c("#b695bc", "#574571"),
                                  # "#f05b43", "#831818"), 
                                  labels = function(x){ifelse(x, "2021 - 2024", "2015 - 2020")}) + 
               theme_classic() + 
               theme(axis.title.x = element_blank(),
                     plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points"),
                     legend.title = element_blank(),
                     legend.position = "inside", 
                     legend.position.inside = c(0.2, 0.8)), 
             labels = c("a) PM2.5 by type, 2015 - 2020", 
                        "b) smoke PM2.5 by time period"),
             align = "h", hjust = -0.05, label_x = 0.01, 
             nrow = 2)} %>% 
  ggsave(filename = file.path(path_figures, "se_pm25_temporal_changes.png"), 
         width = 6*0.9, height = 7*0.9)

