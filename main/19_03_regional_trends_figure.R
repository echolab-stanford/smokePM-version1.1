# create figure showing the regional smoke and non-smoke PM2.5 trends in the CONUS
library(tidyverse)
library(magrittr)
library(sf)
library(cowplot)
library(geofacet)
library(gtable)
nonContig_stateFIPS <- c("02","60","66","15","72","78","69")

source("scripts/setup/00_03_load_paths.R")

# load EPA station PM2.5 data
stationPM <- list.files(file.path(path_data, "/3_intermediate/station_smokePM_update/"),
                        full.names = T, pattern = "rds") %>% 
  map(readRDS) %>% list_rbind %>% 
  ungroup %>%
  mutate(across(year:day, as.numeric))

# state to EPA region aggregation 
regions = read.csv("./us_climate_regions.csv") %>% 
  left_join(tigris::states() %>% 
              st_drop_geometry() %>% 
              select(state_code = STUSPS, state_num = GEOID))

# calculate annual average total PM2.5 and nonsmoke PM2.5 at stations
stationPM %>% 
  summarise(all_mean = sum(pm25)/n(), 
            nonsmoke_mean = sum(pm25 - pm25_anom*smoke_day)/n(), 
            n = n(),
            .by = c(id, year)) %>% 
  # only keep stations with over 50 obs in a year
  filter(n > 50) -> station_annual_avg

# construct a grid to work with geofacet for placing the different panels
region_grid <- data.frame(row = c(2, 1, 1, 1, 1, 
                                  2, 2, 3, 3, 3), 
                          col = c(1, 2, 3, 4, 5, 
                                  2, 5, 2, 3, 4), 
                          name = c("Contiguous US", "Northwest", 
                                   "Northen Rockies", "Upper Midwest", 
                                   "Northeast", "West", "Ohio Valley", 
                                   "Southwest", "South", "Southeast"), 
                          code = 1:10)

# grid_preview(region_grid)

# use the station annual averages to calculate regional annual averages
station_annual_avg %>%
  select(-n) %>%  
  filter(!is.na(all_mean) & !is.na(nonsmoke_mean)) %>% 
  mutate(state_num = substr(id, 1, 2)) %>% 
  # join in the region information
  left_join(regions %>% 
              mutate(climate_regions = stringr::str_to_title(climate_regions))) %>% 
  # also duplicate all the data for a CONUS-wide panel 
  rbind(., 
        mutate(., climate_regions = "Contiguous US")) %>% 
  # then summarise by calculating an average for each region-year 
  summarise(across(c(ends_with("mean")), mean), 
            .by = c(climate_regions, year)) %>%
  # clean up the region names for plotting
  mutate(climate_regions = gsub("\\(.*|And.*", "", climate_regions) %>% trimws()) %>% 
  # plot the total and nonsmoke PM2.5 as separate lines, with a panel for each region
  {ggplot(data = pivot_longer(., ends_with("mean")), 
          aes(x = year, y = value, color = name)) + 
      # fill the area between total and non-smoke PM2.5 as smoke 
      geom_ribbon(data = ., 
                  aes(ymax = all_mean, ymin = nonsmoke_mean, x = year, fill = "smoke"), 
                  color = NA, inherit.aes = FALSE) + 
      geom_line() +
      # arrange according to the grid created above
      facet_geo(~climate_regions, scales = "free",
                grid = region_grid, label = "name") + 
      # adjust aesthetics 
      theme_classic() + 
      ylab(expression(paste("Annual average ", PM[2.5], " (", mu, "g ", m^-3, ")"))) + 
      scale_color_manual(name = "name", 
                         values = c("black", "blue"), 
                         labels = c(expression(paste("observed ", PM[2.5])), 
                                    expression(paste("non-smoke ", PM[2.5])))) + 
      scale_fill_manual(name = "name", 
                        values = c("grey")) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.05))) + 
      guides(fill = guide_legend(order = 2), 
             colour = guide_legend(order = 1)) + 
      theme(axis.title.x = element_blank(), 
            legend.title = element_blank(),
            legend.spacing.y = unit(-11, "points"),
            strip.background = element_blank(), 
            plot.background = element_blank(),
            strip.text = element_text(face = "bold"))} -> region_plot

# grab the legend to use later 
region_legend = get_legend(region_plot)

# convert to gtable to make the CONUS panel bigger and shift bottom row to the middle
# extract gtable that has CONUS          
conus_plot = region_plot + theme(legend.position = "none")
conus_plot %<>% ggplot_build %>% ggplot_gtable
conus_plot <- gtable_filter(conus_plot,
                            "panel-2-1|axis-b-1-2|axis-l-2-1|strip-t-1-2|ylab-l")
conus_plot$heights[c(1, 6, 11)] <- unit(c(0.75, 1.25, 0.75), "null")
conus_plot %<>% gtable_add_grob(region_legend, t = 8, b = 11, r = 3, l = 3)
# pull gtable that has the sub-regions but no CONUS
region_plot = region_plot + theme(legend.position = "none", 
                                  axis.text = element_text(size = 7)) 
region_plot %<>% ggplot_build %>% ggplot_gtable
region_plot <- gtable_remove_grobs(region_plot,
                                   region_plot$layout %>% filter(r <= 7) %>% pull(name) %>%
                                     grep("guide", ., invert = T, value = T)) %>%
  gtable_squash_cols(1:7)
# grab the top two rows separate from the bottom one
plot_grid(conus_plot,
          plot_grid(gtable_remove_grobs(region_plot,
                                        region_plot$layout %>% filter(t >= 19) %>% pull(name) %>%
                                          grep("guide", ., invert = T, value = T)) %>% 
                      gtable_squash_rows(19:27),
                    gtable_remove_grobs(region_plot,
                                        region_plot$layout %>% filter(b < 19 | l > 23) %>% pull(name) %>%
                                          grep("guide", ., invert = T, value = T)) %>% 
                      gtable_squash_rows(1:18) %>%
                      gtable_squash_cols(22:24) %>% 
                      gtable_add_cols(widths = unit(0.5, "null"), pos = -1) %>% 
                      gtable_add_cols(widths = unit(0.5, "null"), pos = 0),
                    rel_heights = c(2, 0.95),
                    nrow = 2),
          rel_widths = c(1.75, 4), rel_heights = c(3, 1)) %>%
  ggsave(filename = file.path(path_figures, "region_averages.pdf"), 
         bg = "white",
         width = 8, height = 5)
