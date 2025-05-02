library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)
library(fixest)

source("scripts/setup/00_03_load_paths.R")

stationPM <- list.files(file.path(path_data, "/3_intermediate/station_smokePM_auto/"),
                        full.names = T) %>% 
  map(readRDS) %>% list_rbind %>% 
  mutate(across(year:day, as.numeric))

report_schedule = stationPM %>% 
  ungroup %>% 
  filter(!is.na(pm25)) %>% 
  summarise(n = n(), 
            .by = c(date, year)) %>% 
  arrange(date) %>% 
  mutate(max_day = first(date[n == max(n)]), 
         on_day = (mod(lubridate::yday(date) - lubridate::yday(max_day), 3) == 0)*1,
         .by = year) 

smokePM_pred <- file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>% 
  mutate(smokePM_pred = pmax(0, smokePM_pred))

stationPM %>% 
  ungroup %>% 
  select(id, grid_id_10km) %>% 
  unique %>% 
  cross_join(data.frame(year = 2006:2023)) %>% 
  # use positive anomalies of station smoke PM to match the predictions 
  left_join(stationPM %>% 
              ungroup %>% 
              left_join(report_schedule) %>% 
              # calculate annual avg contribution of smokePM to daily PM
              summarise(annual_daily_smokePM_3day = sum(smokePM[on_day == 1])/sum(on_day == 1), 
                        annual_days_over50_3day = sum(smokePM[on_day == 1] > 50)/sum(on_day == 1)*365, 
                        annual_daily_smokePM_1day = sum(smokePM)/n(), 
                        annual_days_over50_1day = sum(smokePM > 50)/n()*365, 
                        n_1day = n(), 
                        n_3day = sum(on_day == 1),
                        .by = c(year, id, grid_id_10km))) %>% 
  left_join(smokePM_pred %>% 
              mutate(year = lubridate::year(date)) %>% 
              # calculate annual avg contribution of smokePM to daily PM
              summarise(annual_total_smokePM_pred = sum(smokePM_pred), 
                        annual_days_over50_pred = sum(smokePM_pred > 50),
                        .by = c(year, grid_id_10km)) %>% 
              mutate(y_days = 365 + leap_year(year)*1, 
                     annual_daily_smokePM_pred = annual_total_smokePM_pred/y_days) %>%
              select(-annual_total_smokePM_pred, -y_days)) -> annual_check


annual_check %>% 
  mutate(in_obs = n_3day > 100 & !is.na(n_3day)) %>% 
  # only include annual obs with at least 300 days
  mutate(across(intersect(starts_with("annual"), ends_with("3day")), 
                ~ifelse(in_obs, .x, NA))) %>% 
  select(-starts_with("n")) %>% 
  pivot_longer(starts_with("annual")) %>% 
  filter(!is.na(value)) %>% 
  nest_by(id, grid_id_10km, name) %>% 
  transmute(n_year = nrow(data),
            trend = coef(lm(value ~ year, data = data))[2], 
            n_obsyears = nrow(data %>% filter(in_obs)),
            trend_obsyears = if(n_obsyears > 1){coef(lm(value ~ year, data = data %>% filter(in_obs)))[2]}else{NA}) -> trend_check

plot_grid(annual_check %>% 
            filter(n_3day > 100) %>% 
            {ggplot(data = ., aes(y = annual_daily_smokePM_pred, x = annual_daily_smokePM_3day)) + 
                geom_bin2d(bins = 60) + 
                geom_abline(intercept = 0, slope = 1, color = "grey30") +
                annotate("text", 
                         label = feols(annual_daily_smokePM_3day ~ annual_daily_smokePM_pred, data = .) %>% r2("r2") %>% round(2), 
                         x = 15, y = 1, hjust = 0, vjust = 0.7) + 
                annotate("text", 
                         label = expression(paste(R^2, "= ")), 
                         x = 15, y = 1, hjust = 1) + 
                scale_fill_gradientn(colors = cmocean::cmocean('dense')(100),
                                     # cmocean::cmocean('thermal')(100),
                                     trans = "pseudo_log", 
                                     breaks = c(0, 1, 10, 100, 1000, 10000)) +
                scale_x_continuous(trans = "pseudo_log", 
                                   breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 500, 1000),
                                   expand = c(0, 0)) +
                scale_y_continuous(trans = "pseudo_log", 
                                   breaks = c(0, 1, 5, 10, 25, 50, 100, 250, 500, 1000), 
                                   expand = c(0, 0)) +
                theme_classic() + 
                xlab(expression(paste("observed annual average (", mu, "g/", m^3, ")"))) + 
                ylab(expression(paste("predicted annual average (", mu, "g/", m^3, ")"))) + 
                theme(legend.position = "inside", 
                      axis.title = element_text(size = 10),
                      legend.background = element_blank(),
                      plot.margin = unit(c(24.5, 5.5, 5.5, 5.5), "points"),
                      legend.position.inside = c(0.17, 0.72))},
          trend_check %>% 
            ungroup %>% 
            filter(n_obsyears > 9) %>% 
            pivot_wider(names_from = name, values_from = c(trend, trend_obsyears, n_year, n_obsyears)) %>% 
            {ggplot(data = ., 
                    aes(x = trend_obsyears_annual_daily_smokePM_3day, 
                        y = trend_obsyears_annual_daily_smokePM_pred)) + 
                geom_point(alpha = 0.2) + 
                geom_abline(intercept = 0, slope = 1, color = "red") +
                annotate("text", 
                         label = feols(trend_obsyears_annual_daily_smokePM_3day ~ trend_obsyears_annual_daily_smokePM_pred, data = .) %>% r2("r2") %>% round(2), 
                         x = 0.5, y = 0, hjust = 0, vjust = 0.7) + 
                annotate("text", 
                         label = expression(paste(R^2, "= ")), 
                         x = 0.5, y = 0, hjust = 1) + 
                xlab(expression(paste("observed trend (", mu, "g", m^-3, "/year)"))) + 
                ylab(expression(paste("predicted trend (", mu, "g", m^-3, "/year)"))) +
                theme_classic() + 
                theme(plot.margin = unit(c(24.5, 5.5, 5.5, 5.5), "points"), 
                      axis.title = element_text(size = 10))},
          trend_check %>% 
            ungroup %>% 
            filter(n_obsyears > 9) %>% 
            pivot_wider(names_from = name, values_from = c(trend, trend_obsyears, n_year, n_obsyears)) %>% 
            {ggplot(data = .,
                    aes(x = trend_obsyears_annual_days_over50_3day, 
                        y = trend_obsyears_annual_days_over50_pred)) +   
                geom_point(alpha = 0.2) + 
                geom_abline(intercept = 0, slope = 1, color = "red") +
                annotate("text", 
                         label = feols(trend_obsyears_annual_days_over50_3day ~ trend_obsyears_annual_days_over50_pred, data = .) %>% r2("r2") %>% round(2), 
                         x = 0.8, y = 0, hjust = 0, vjust = 0.7) + 
                annotate("text", 
                         label = expression(paste(R^2, "= ")), 
                         x = 0.8, y = 0, hjust = 1) + 
                xlab(expression(paste("observed trend (days >50", mu, "g", m^-3, "/year)"))) + 
                ylab(expression(paste("predicted trend (days >50", mu, "g", m^-3, "/year)"))) + 
                theme_classic() + 
                theme(plot.margin = unit(c(24.5, 5.5, 5.5, 5.5), "points"), 
                      axis.title = element_text(size = 10))}, 
          labels = c("a) annual averages", 
                     "b) trend in annual averages", 
                     "c) trend in extreme days"), 
          nrow = 1, 
          hjust = -0.1, label_x = 0.01, align = "v", axis = "b") %>% 
  ggsave(filename = file.path(path_figures, "pattern_trend_check.png"), 
         width = 9, height = 3)
