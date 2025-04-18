library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

# load full training df
training_data <- file.path(path_output, "version1.1", "smokePM", "smokePM_full_model_training.rds") %>% 
  readRDS %>% 
  select(id, date, grid_id_10km, month, smokePM, fold, contains("interp")) 

nonContig_stateFIPS <- c("02","60","66","15","72","78","69")
conus <-read_sf("data/cb_2023_us_state_20m") %>% filter(!(STATEFP %in% nonContig_stateFIPS))
epa_ll = read_sf(file.path(path_data, "EPA_automated", "epa_station_locations"))


# load CV predictions
list.files(file.path(path_output, "version1.1", "smokePM", "model"), 
           pattern = "smokePM_pred_", full.names = T) %>% 
  purrr::map(~readRDS(.x) %>% mutate(file = basename(.x))) %>% 
  list_rbind() -> all_cv_preds
all_cv_preds %<>% mutate(out_fold = str_split_i(file, "fold", 2) %>% str_split_i("_", 1) %>% as.numeric, 
                         drop_vars =  gsub(".*drop-|\\.rds", "", file), 
                         .keep = "unused") 

# observed vs predicted
{all_cv_preds %>%
    filter(drop_vars == "aod_anom_pred" & out_fold == fold) %>% 
    # filter(out_fold != 5 & fold != 5) %>%
    left_join(training_data %>% select(-contains("interp")) %>% distinct()) %>% 
    mutate(smokePM_pred = pmax(0, smokePM_pred)) %>% 
    ggplot(aes(x = smokePM_pred, y = smokePM)) + 
    geom_bin2d(bins = 60) + 
    geom_abline(intercept = 0, slope = 1, color = "grey30") +
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
    xlab(expression(predicted~smoke~PM[2.5])) + ylab(expression(observed~smoke~PM[2.5])) +
    theme_classic() + 
    theme(text = element_text(size = 16)) } -> obs_pred_comp_plot

# station-specific R2
all_cv_preds %>%
  filter(out_fold == fold) %>% 
  left_join(training_data %>% select(-contains("interp")) %>% distinct()) %>% 
  nest_by(id, drop_vars) %>% 
  mutate(n = nrow(data), 
         fold = unique(data$fold),
         n_unique_smokePM = length(unique(data$smokePM))) %>% 
  filter(n > 1 & n_unique_smokePM > 1) %>% 
  mutate(r2 = fixest::r2(fixest::feols(smokePM ~ smokePM_pred, 
                                       data = data), 
                         "r2") %>% unname) %>%
  select(-data) %>% 
  ungroup %>% 
  mutate(drop_vars = gsub("^-", "", drop_vars)) %>% 
  pivot_wider(values_from = r2, names_from = drop_vars) %>% 
  mutate(diff = aod_anom_pred - interp) %>% 
  left_join(epa_ll, by = c("id" = "stn_id")) -> station_r2

station_r2 %>% 
  st_as_sf %>% 
  filter(n > 50) %>%
  mutate(hist_col = cut(aod_anom_pred, breaks = seq(0, 1, length.out = 12))) %>% 
  ggplot() + 
  geom_sf(data = conus,
          color = "grey10", fill = "grey80") +
  geom_sf(aes(color = hist_col)) + 
  # scale_colour_gradientn(colors = RColorBrewer::brewer.pal(11, "RdYlBu"),
  #                        breaks = seq(0, 1, length.out = 12),
  #                        name = expression(R^2)) +
  scale_colour_manual(values = RColorBrewer::brewer.pal(11, "RdYlBu"),
                      name = expression(R^2), 
                      guide = "none") +
  # scale_color_gradient2(name = expression(R^2), 
  #                       breaks = seq(-0.2, 0.2, by = 0.1), 
  #                       labels = c("< -0.2", "-0.1", "0", "0.1", "> 0.2")) + 
  theme_void() + 
  theme(legend.position = "bottom") -> station_r2_plot 

station_r2 %>% 
  filter(n > 50) %>%
  mutate(hist_col = cut(aod_anom_pred, breaks = seq(0, 1, length.out = 12))) %>% 
  ggplot(aes(x = aod_anom_pred)) + 
  geom_histogram(aes(fill = hist_col), breaks = seq(0, 1, length.out = 12)) + 
  scale_colour_manual(values= RColorBrewer::brewer.pal(11, "RdYlBu"),
                      name = expression(R^2), aesthetics = c("color", "fill"), 
                      guide = "none") + 
  theme_classic() +
  xlab(expression(R^2)) + 
  theme(text = element_text(size = 12), 
        plot.background = element_blank()) -> station_r2_hist

# feature importance 
file.path(path_output, "version1.1", "smokePM", "model") %>%
  list.files(pattern = "importance.*aod_anom", 
           full.names = T) %>% 
  map(function(x){readRDS(x) %>% mutate(file = basename(x))}) %>% 
  list_rbind() -> var_imp

var_imp %>% 
  mutate(out_fold = gsub(".*fold", "", file)) %>% 
  separate(out_fold, sep = "_", into = c("out_fold", NA)) %>% 
  summarise(main = Gain[out_fold == "99"], 
            min = min(Gain), 
            max = max(Gain), 
            mid = mean(Gain),
            .by = Feature) %>% 
  arrange(desc(main)) %>% 
  slice_head(n = 10) %>% 
  mutate(group = case_when(grepl("aot", Feature) ~ "aerosols", 
                           grepl("fire", Feature) ~ "fire",
                           Feature %in% c("pbl_min", "dewpoint_temp_2m", "precip") ~ "meteorology", 
                           T ~ "other"), 
         Feature = factor(Feature, levels = rev(unique(Feature)))) %>% 
  ggplot(aes(y = Feature, color = group)) + 
  geom_point(aes(x = main), size = 3) + 
  geom_linerange(aes(xmin = min, xmax = max), linewidth = 2) + 
  xlab("Gain") + 
  scale_y_discrete(breaks = c("smokePM_interp",
                              "aot_anom",
                              "aot_anom_lag1",
                              "fire_dist_km",
                              "lon",
                              "closest_fire_area",
                              "precip",
                              "aot_anom_lag2",
                              "closest_fire_num_points",
                              "lat"),
                   labels = c(expression(paste("Interpolated smoke ", PM[2.5])),
                              "AOT anomaly (current)",
                              "AOT anomaly (1-day lag)",
                              "Distance to closest fire",
                              "Longitude",
                              "Area of closest fire",
                              "Precipitation",
                              "AOT anomaly (2-day lag)",
                              "Number of point in closest fire",
                              "Latitude")) +
  scale_color_manual(name = "variable type", 
                     values = c("grey10", "red3", "darkblue", "green3")) +
  theme_classic() + 
  theme(axis.title.y = element_blank(), 
        legend.position = "inside", 
        text = element_text(size = 16),
        legend.position.inside = c(0.7, 0.2)) -> var_imp_plot 

change_r2_plot <- station_r2 %>% 
  st_as_sf %>% 
  filter(!is.na(diff)) %>% 
  filter(n > 50) %>%
  ggplot() + 
  geom_sf(data = conus,
          color = "grey10", fill = "grey80") +
  geom_sf(aes(color = pmin(diff, 0.2) %>% pmax(-0.2))) + 
  scale_color_gradient2(name = expression(Delta~R^2), 
                        breaks = seq(-0.2, 0.2, by = 0.1), 
                        labels = c("< -0.2", "-0.1", "0", "0.1", "> 0.2")) + 
  theme_void() + 
  theme(legend.position = "bottom",
        legend.key.width = unit(2.5, "lines"))

plot_grid(obs_pred_comp_plot + 
            theme(plot.margin = unit(c(25.5, 5.5, 5.5, 5.5), "points")), 
          ggdraw() + 
            draw_plot(station_r2_plot, 0, 0.15, 1, 0.85) + 
            draw_plot(station_r2_hist, 0.0, 0.05, 0.4, 0.35),
          var_imp_plot + 
            theme(plot.margin = unit(c(25.5, 5.5, 5.5, 5.5), "points")), 
          change_r2_plot,
          nrow = 2, hjust = c(-0.1, -0.05, -0.1), label_x = 0.01, 
          # rel_widths = c(1.1, 1, 1),
          vjust = 1.3,
          label_size = 20, 
          labels = c("a) out of sample predictions", 
                     "b) spatial variation in performance",
                     "c) feature importance", 
                     "d) change in R2")) %>%
  ggsave(filename = file.path(path_figures, "model_performance.png"),  
         bg = "white",
         width = 11, height = 9)