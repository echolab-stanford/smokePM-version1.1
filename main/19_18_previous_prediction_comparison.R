library(tidyverse)
library(magrittr)
library(cowplot)

source("scripts/setup/00_03_load_paths.R")

pred_comp <- rbind(full_join(file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
                               readRDS %>% 
                               mutate(smokePM_pred = pmax(0, smokePM_pred)) %>% 
                               rename(pred_new = smokePM_pred),
                             readRDS("./output/version1.0/smokePM2pt5_predictions_daily_10km_20060101-20201231.rds") %>%
                                 rename(pred_old = smokePM_pred)) %>%
                     mutate(pred_new = replace_na(pred_new, 0),
                            spatial = "grid", 
                            temporal = "day") %>% 
                     rename(id = grid_id_10km), 
                   full_join(file.path(path_output, "version1.1", "smokePM", "aggregations", "county_smokePM_predictions_20060101-20241231.rds") %>% 
                               readRDS %>%
                     # readRDS("./scratch/county_smokePM_v2_2006_2023.rds") %>% 
                               rename(pred_new = smokePM_pred),
                             readRDS("output/version1.0/smokePM2pt5_predictions_daily_county_20060101-20201231.rds") %>% 
                               rename(pred_old = smokePM_pred)) %>%  
                     mutate(pred_new = replace_na(pred_new, 0), 
                            spatial = "county", 
                            temporal = "day") %>% 
                     rename(id = GEOID))
pred_comp %<>% filter(year(date) <= 2020)
pred_comp %<>% mutate(month = month(date), year = year(date)) %>% 
  {rbind(., 
         summarise(., 
                   across(starts_with("pred"), sum), 
                   .by = c(id, spatial, month, year)) %>% 
           mutate(temporal = "month", 
                  date = NA))} 
pseudlog = function(x){ asinh(x / 2)}
inv_pseudlog = function(x){sinh(x)*2}

pred_comp %>% 
  mutate(panel = paste0(spatial, "-", temporal),
         panel = factor(panel, levels = unique(panel), 
                        ordered = T)) %>% 
  filter(!is.na(pred_old)) %>% 
  {ggplot(data = ., 
          aes(x = pred_old, y = pred_new, 
              group = panel)) + 
      geom_bin2d(bins = 70) + 
      geom_abline(intercept = 0, slope = 1, color = "grey30") +
      geom_text(data =  summarise(., 
                                  r2 = {lm(pred_new ~ pred_old)} %>% summary %>% extract2("r.squared") %>% round(2), 
                                  max_old = max(pred_old, na.rm = T), 
                                  max_new = max(pred_new, na.rm = T),
                                  .by = panel),
                aes(x = inv_pseudlog(pseudlog(max_old)*0.15), 
                    y = inv_pseudlog(pseudlog(max_new)*0.8), 
                    label = r2),
                # label = round(r2, 2)), 
                vjust = 0.7,
                hjust = 0) + 
      geom_text(data = summarise(., 
                                 r2 = {lm(pred_new ~ pred_old)} %>% summary %>% extract2("r.squared") %>% round(2), 
                                 max_old = max(pred_old, na.rm = T), 
                                 max_new = max(pred_new, na.rm = T),
                                 .by = panel), 
                aes(x = inv_pseudlog(pseudlog(max_old)*0.15), 
                    y = inv_pseudlog(pseudlog(max_new)*0.8)),
                label = expression(paste(R^2, "= ")),
                # label = round(r2, 2)), 
                hjust = 1) + 
      scale_fill_continuous(type = "viridis", 
                            trans = "pseudo_log", 
                            breaks = c(0, 1, 10, 100, 1000, 10000, 1e5, 1e6)) +
      scale_x_continuous(trans = "pseudo_log",  
                         breaks = c(0, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000),
                         expand = c(0, 0), limits = c(0, NA)) +
      scale_y_continuous(trans = "pseudo_log",
                         breaks = c(0, 1, 5, 10, 50, 100, 500, 1000, 5000, 10000),
                         expand = c(0, 0), limits = c(0, NA)) +
      xlab(expression(previous~smoke~PM[2.5]~estimates)) + ylab(expression(new~smoke~PM[2.5]~estimates)) +
      facet_wrap(~panel, 
                 scales = "free") +  
      theme_classic() + 
      theme(text = element_text(size = 15),
            axis.text = element_text(size = 10),
            plot.margin = unit(c(5.5, 10.5, 5.5, 5.5), "points"),
            strip.background = element_blank(),
            legend.position = "none")} %>% 
  ggsave(filename = file.path(path_figures, "old_new_pred_comparison.png"), 
         width = 8, height = 7)
