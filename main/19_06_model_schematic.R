library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)

source("scripts/setup/00_03_load_paths.R")

fig1_date <- as.Date("2023-06-29")
nonContig_stateFIPS <- c("02","60","66","15","72","78","69")
conus <-read_sf("data/cb_2023_us_state_20m") %>% filter(!(STATEFP %in% nonContig_stateFIPS))
epa_ll = read_sf(file.path(path_data, "EPA_automated", "epa_station_locations"))

stationPM <- list.files(file.path(path_data, "/3_intermediate/station_smokePM_auto/"),
                        full.names = T, pattern = "2023") %>% 
  map(readRDS) %>% list_rbind %>% 
  mutate(across(year:day, as.numeric))

ggdraw() +
  draw_plot(stationPM %>% 
              # filter(id == "300490026") %>% 
              filter(id == "170313103") %>% 
              filter(date >= fig1_date - as.difftime(30, units = "days") & 
                       date <= fig1_date + as.difftime(27, units = "days")) %>% 
              group_by(id) %>% 
              mutate(maxPM = max(pm25)) %>% 
              {ggplot(data = ., aes(x = date)) + 
                  geom_line(aes(y = pm25), color = "black") + 
                  geom_line(aes(y = pm25_med_3yr), color = "blue") + 
                  geom_line(aes(y = smokePM), color = "red", lwd = 1.5) + 
                  geom_point(aes(alpha = I(smoke_day), y = -0.03*maxPM), 
                             color = "grey45", cex = 3) + 
                  # geom_vline(xintercept = fig1_date, color = "grey30", linetype = "dashed") +
                  geom_text(data = filter(., date == max(date)) %>% 
                              mutate(smoke_day = -0.03*maxPM) %>%
                              dplyr::select(id, date, pm25, pm25_med_3yr, smokePM, smoke_day) %>% 
                              pivot_longer(c(contains("pm"), smoke_day)) %>% 
                              mutate(name = recode(name, 
                                                   "pm25" = "total~PM[2.5]", #"total PM[2.5]",
                                                   "pm25_med_3yr" = "`non-smoke`~median", #"non-smoke median", 
                                                   "smokePM" = "smoke~PM[2.5]",
                                                   "smoke_day" = "smoke~day")),
                            aes(x = date + as.difftime(1, units = "days"), 
                                y = ifelse(name == "total~PM[2.5]", 1.2, 1)*value,
                                # ifelse(id == 482570005 & value > 0, 1.25, 1)*value + 
                                # ifelse(id == 482570005 & name == "total~PM[2.5]", 1.5, 0), 
                                label = name, 
                                color = name), 
                            size = 4.5,
                            parse = TRUE, 
                            hjust = 0) +
                  scale_color_manual(values = c("blue", "grey45", "red", "black"),
                                     guide = "none") +
                  theme_classic() + 
                  ylab(expression(paste(PM[2.5], " (", mu, "g/", m^3, ")"))) + 
                  xlab("") + 
                  scale_x_date(expand = expansion(add = c(0, 1)),
                               date_breaks = "1 month",
                               date_labels = "%b %Y") + 
                  coord_cartesian(clip = "off") +
                  theme(text = element_text(size = 16),
                        plot.margin = unit(c(0, 1.75, 0, 0), "in"))}, 
            0, 0, 1, 0.9) +
  draw_plot(stationPM %>%
              # filter(id == "170313103") %>%
              ungroup %>% 
              filter(date == fig1_date) %>% 
              left_join(epa_ll %>% select(id = stn_id)) %>% 
              st_as_sf %>% 
              ggplot() + 
              geom_sf(data = conus) + 
              geom_sf(aes(color = smokePM)) + 
              scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                                    aesthetics = c("fill", "color"), 
                                    limits = c(0, 100),
                                    oob = scales::squish,
                                    name = expression(paste(mu, "g/", m^3)),
                                    values = scales::rescale(sinh(seq(0, 3, length.out = 100))), 
                                    breaks = seq(0, 100, by = 20),
                                    labels = c(seq(0, 80, by = 20), ">100"), 
                                    guide = guide_colourbar(theme = theme(
                                      legend.key.width  = unit(0.75, "lines"),
                                      legend.key.height = unit(4, "lines")
                                    ))) + 
              theme_void() + 
              theme(legend.position = "inside", 
                    plot.margin = unit(c(35.5, 5.5, 5.5, 5.5), "points"),
                    legend.position.inside = c(0.9, 0.3)),
            0.45, 0.5, 0.55, 0.5) -> schematic_plot1

# from https://github.com/marcosci/layer/blob/main/R/tilt_maps.R since package will no longer install without rgeos
tilt_map <- function(data,
                     x_stretch = 2,
                     y_stretch = 1.2,
                     x_tilt = 0,
                     y_tilt = 1,
                     x_shift = 0,
                     y_shift = 0,
                     angle_rotate = pi/20,
                     boundary = NULL,
                     parallel = FALSE) {
  
  if (!any(class(data) %in% c("sf", "sfg"))) {
    data <- stars::st_as_stars(data)
    data <- sf::st_as_sf(data)
  }
  
  shear_matrix <- function(x) {
    matrix(c(x_stretch, y_stretch, x_tilt, y_tilt), 2, 2)
  }
  
  rotate_matrix <- function(x) {
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
  }
  
  if(!is.null(boundary)) data <- create_outline(boundary, data)
  
  if(parallel == TRUE){
    
    geom_func <- function(data, x_stretch, y_stretch, x_tilt, y_tilt, x_shift, y_shift){
      sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(angle_rotate) + c(x_shift, y_shift) 
      data <- data %>% sf::st_as_sf()
    }
    
    data <- data %>%
      dplyr::group_by(group = (dplyr::row_number()-1) %/% (dplyr::n()/10))%>%
      tidyr::nest() %>% 
      dplyr::pull(data) %>%
      furrr::future_map(~geom_func(data = .,
                                   x_stretch = x_stretch,
                                   y_stretch = y_stretch,
                                   x_tilt = x_tilt,
                                   y_tilt = y_tilt,
                                   x_shift = x_shift,
                                   y_shift = y_shift)) %>% 
      dplyr::bind_rows() %>% 
      sf::st_as_sf()
    
  } else {
    
    sf::st_geometry(data) <- sf::st_geometry(data) * shear_matrix() * rotate_matrix(angle_rotate) + c(x_shift, y_shift)
    
  }
  
  if(length(names(data)) > 1) names(data)[1] <- "value"
  
  return(data)
  
}

grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
tilted_grid <- tilt_map(grid, 
                        x_tilt = 0.1, y_tilt = 1) %>% 
  select(grid_id_10km = value, 
         geometry)

plumes <- st_read(file.path(path_data, "smoke", "raw"), 
                  layer = paste0("hms_smoke", format(fig1_date, "%Y%m%d"))) 
  # readRDS("./scratch/smoke_plumes_sfdf_2023_06.RDS") %>% 
  # filter(date == format(fig1_date, "%Y%m%d")) %>% 
  # st_set_crs(st_crs(grid))

contig_plumes <- st_intersection(plumes, conus  %>% st_union() %>% st_transform(crs = st_crs(plumes)))
contig_plumes_tilt <- tilt_map(contig_plumes, 
                               x_tilt = 0.1, y_tilt = 1)
# candidate features
features_map <- list(aot_anom = file.path(path_data, "3_intermediate", "aot_anom_auto", paste0("aot_anom_", format(fig1_date, "%Y_%m"), ".rds")) %>% 
                       readRDS %>% 
                       filter(date == fig1_date) %>%
                       select(grid_id_10km, 
                              value = aot_anom), 
                     pbl = file.path(path_data, "ERA5_variables", "raw", "joined_grids", paste0("era5_joined_grid_", format(fig1_date, "%Y_%m"))) %>% 
                       readRDS %>%
                       filter(date == fig1_date) %>% 
                       select(grid_id_10km = id_grid, 
                              value = `boundary_layer_height_daily_minimum`), 
                     fire_dist = file.path(path_data, "distance_to_fire_cluster_auto", paste0("grid_distance_to_fire_cluster_", format(fig1_date, "%Y_%m"), ".rds")) %>% 
                       readRDS %>% 
                       filter(date == format(fig1_date, "%Y%m%d")) %>% 
                       select(grid_id_10km = id_grid, 
                              value = km_dist), 
                     interp = file.path(path_output, "version1.1", "smokePM_interpolations", "interpolations_full_model", 
                                        paste0("smokePM_interpolated_", format(fig1_date, "%Y_%m"), ".rds")) %>% 
                       readRDS %>% 
                       filter(date == fig1_date) %>% 
                       select(grid_id_10km = id, value = interp),
                     elevation = file.path(path_data, "2_from_EE", "elevation_10km", "elevation_avg_sd_10km_grid_filled.csv") %>% 
                       read.csv() %>% 
                       select(grid_id_10km = ID, 
                              value = mean))
gg_mapLayers <- list(list(i = 0, 
                          data = features_map[["elevation"]], 
                          color = "Terrain 2",
                          name = "cross-sectional\nvariables", 
                          rev_pal = FALSE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 1,
                          data = features_map[["aot_anom"]],
                          color = "YlOrRd",
                          name = "AOT anomaly",
                          rev_pal = FALSE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 2, 
                          data = features_map[["fire_dist"]], 
                          color = "Red-Yellow", 
                          name = "fire variables",
                          rev_pal = FALSE,
                          scale_func = colorspace::scale_color_continuous_sequential),
                     list(i = 3,
                          data = features_map[["pbl"]],
                          color = "YlGnBu", 
                          name = "meterology",
                          rev_pal = FALSE,
                          scale_func = colorspace::scale_color_continuous_sequential), 
                     list(i = 4,
                          data = features_map[["interp"]],
                          color = "Lajolla",
                          name = "station\ninterpolations",
                          rev_pal = TRUE,
                          scale_func = colorspace::scale_color_continuous_sequential))  %>% 
  purrr::reduce(.f = function(prev_gg, layer_list){
    new_gg <- prev_gg + 
      {if(layer_list$i > 0) ggnewscale::new_scale_fill()} + 
      {if(layer_list$i > 0) ggnewscale::new_scale_color()} +
      geom_sf(data = tilted_grid %>% left_join(layer_list$data) %>% 
                mutate(geometry = geometry + layer_list$i*c(0, 20)), 
              aes(fill = value, color = value), 
              size = 0.5, alpha = 1) + 
      layer_list$scale_func(palette = layer_list$color, 
                            aesthetics = c("color", "fill"),
                            rev = layer_list$rev_pal,
                            name = layer_list$name, 
                            guide = guide_colorbar(ticks = FALSE,
                                                   label = FALSE,
                                                   title.position = "right",
                                                   barheight = 2.7,
                                                   theme = theme(legend.key.width = unit(0.9, "lines")),
                                                   title.theme = element_text(size = 11),
                                                   order = 10 - layer_list$i)) 
    return(new_gg)
  },  .init = ggplot()) 

# png("./figures/test.png",
#     width = 720, height = 880)
{gg_mapLayers +
    ggnewscale::new_scale_fill() +
    ggnewscale::new_scale_color() +
    geom_sf(data = tilted_grid %>%
              mutate(geometry = geometry + 5*c(0, 20)),
            color = "grey50", fill = "grey50", inherit.aes = F) + 
    geom_sf(data = contig_plumes_tilt %>% 
              mutate(geometry = geometry + 5*c(0, 20)), 
            aes(fill = "A", color = "A"), alpha = 0.5, 
            show.legend = "polygon", inherit.aes = F) + 
    scale_fill_manual(values = c("A" = "grey25"), 
                      labels = c("smoke plumes"), 
                      name = "", 
                      aesthetics = c("fill", "color"), 
                      guide = guide_legend(order = 1,
                                           label.theme = element_text(size = 11))) + 
    theme_void() + 
    theme(legend.spacing = unit(9, "points"), 
          plot.margin = unit(c(35.5, 0, 0, 0), "points"),
          legend.justification = c(0.5, 0.2))} -> schematic_plot2

file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>%
  mutate(smokePM_pred = pmax(smokePM_pred, 0)) %>% 
  filter(date == fig1_date) %>% 
  full_join(grid, by = c("grid_id_10km" = "ID")) %>% 
  st_as_sf %>%
  mutate(smokePM_pred = replace_na(smokePM_pred, 0)) %>% 
  {ggplot(data = ., 
          aes(fill = smokePM_pred, color = smokePM_pred)) + 
      geom_sf() + 
      scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                            aesthetics = c("fill", "color"), 
                            limits = c(0, 100),
                            oob = scales::squish,
                            name = expression(paste(mu, "g/", m^3)),
                            values = scales::rescale(sinh(seq(0, 3, length.out = 100))), 
                            breaks = seq(0, 100, by = 20),
                            labels = c(seq(0, 80, by = 20), ">100")) + 
      theme_void() + 
      theme(legend.position = "none", 
            plot.margin = unit(c(35.5, 0, 0, 10.5), "points"))} -> schematic_plot3

plot_grid(schematic_plot1, schematic_plot2, schematic_plot3, nrow = 1, 
          rel_widths = c(1.1, 1, 0.7),
          hjust = -0.1, label_x = 0.01, 
          label_size = 16, 
          labels = c("a) isolating smoke PM", "b) candidate features", 
                     "c) daily, 10km predictions")) %>% 
  ggsave(filename = file.path(path_figures, "model_schematic.png"), 
         bg = "white",
         width = 16, height = 5.5)
