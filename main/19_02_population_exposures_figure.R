# create figure showing population exposures to smoke PM2.5
library(tidyverse)
library(magrittr)
library(sf)
library(cowplot)

source("scripts/setup/00_03_load_paths.R")

# load the 10km grid, the final predictions, and the population density in each 10km grid cell
grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
smokePM_pred <- readRDS(file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101-20061231.rds"))
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"), 
                  full.names = TRUE) %>% 
  purrr::map(read.csv) %>% list_rbind

# calculate area in each grid 10km grid cell to get total population
grid %<>% mutate(area = st_area(geometry) %>% unclass)

# calculate the average population exposure for each day of the year
# also the number of people living in locations with concentrations >10 ug for each day
smokePM_pred %>% 
  left_join(pop %>% 
              left_join(grid) %>% 
              mutate(pop = mean*area) %>% 
              transmute(grid_id_10km = ID, 
                        pct_pop = pop/sum(pop), 
                        pop)) %>% 
  summarise(avg_smokePM = sum(pct_pop*smokePM_pred), 
            above10_pop = sum(pop*(smokePM_pred > 10)),
            .by = date) -> pop_exposure

# panel a) cumulative exposure in each year
# fill the missing days as zeros, since they had no smoke PM prediction 
pop_exposure %>% 
  full_join(data.frame(date = seq.Date(as.Date("2006-01-01"), as.Date("2023-12-31"), "days"))) %>% 
  mutate(across(c(avg_smokePM), 
                ~replace_na(.x, 0))) %>% 
  # add variables for year, and color if the year > 2020
  mutate(year = year(date),
         y_color = ifelse(year < 2020, NA, year) %>% as.character, 
         # convert everything to days in 2000 to allow for plotting over the same x axis range 
         # (because of leap year, this will just set feb 29 to zero for non-leap years)
         day = paste0("2000-", month(date), "-", day(date)) %>% as.Date()) %>% 
  select(-date, -ends_with("_pop")) %>% 
  # add a 2006-2019 average as well, by taking the data from those years and 
  # calculating an average for each day of the year
  {rbind(mutate(., 
                year = as.character(year), 
                type = "annual"),
         filter(., year <=2019) %>% 
           summarise(avg_smokePM = mean(avg_smokePM), 
                     .by = day) %>% 
           mutate(year = "2006-2019\naverage", 
                  type = "average", 
                  y_color = "2006-2019\naverage"))} %>% 
  # rank the days in terms of worst exposure, to highlight the worst 10 days in the figure
  mutate(date_rank = rank(desc(avg_smokePM))) %>% 
  # then order by year and day to calculate cumulative sums over the year
  arrange(year, day) %>%
  mutate(cum_smokePM = cumsum(avg_smokePM), .by = year) %>% 
  # make a line for each year showing cumulative exposure throughout the year
  {ggplot(data = ., 
          aes(x = day, 
              y = cum_smokePM, 
              color = y_color, 
              group = year)) + 
      # make the 2006-2019 average thickest for visibility, 
      # and make the recent years (2020-2023) slight thicker so they can be seen as well
      geom_line(aes(linewidth = I(case_when(type == "annual" & is.na(y_color) ~ 0.35, 
                                            type == "annual" ~ 0.6, 
                                            T ~ 1.2)))) + 
      # show the specific dates if they are within the top 10 worst days
      geom_point(aes(alpha = I(ifelse(date_rank <= 10, 1, 0)))) + 
      # add labels to the right hand side for the average and recent years
      geom_text(data = filter(., !is.na(y_color)) %>% filter(day == max(day), .by = year),
                aes(x = day + as.difftime(1, units = "days"), y = cum_smokePM, label = year),
                hjust = 0, lineheight = 0.75) +
      coord_cartesian(clip = "off") + 
      # adjust plot aesthetics and labels
      scale_color_manual(values = c("black", MetBrewer::met.brewer("Juarez", 4))) + 
      scale_x_date(date_labels = "%b",
                   breaks = seq.Date(as.Date("2000-02-01"), as.Date("2000-12-31"), by = "2 month"),
                   expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0.01))) + 
      ylab(expression(paste("Cumulative smoke exposure (", mu, "g/", m^3, ")"))) + 
      theme_classic() + 
      theme(legend.position = "none", 
            axis.title.x = element_blank(),
            plot.margin = unit(c(20.5, 60, 5.5, 5.5), "points"))} -> cum_exposure

# panel b) small multiples of the daily predictions for the worst 10 days
# use the daily population average exposure estimates to identify the 10 worst days
pop_exposure %>% 
  arrange(desc(avg_smokePM)) %>% 
  mutate(day_rank = 1:n()) %>%
  slice_head(n = 10) %>% 
  select(date, day_rank, avg_smokePM, above10_pop) %>% 
  # join in the 10km grid to get a grid for each of those 10 days, then join the smoke PM predictions
  cross_join(grid %>% 
               rename(grid_id_10km = ID)) %>% 
  left_join(smokePM_pred) %>% 
  st_as_sf() %>%
  # fill the missing smoke PM predictions with zeros, 
  # and make panel labels for each day showing the date, the avg exposure, and the number of people exposed
  mutate(lab = paste0(day_rank, ". ", 
                      format(date, "%b %e, %Y"), "\n", 
                      round(avg_smokePM, 1), "\u03BCg/m³, ", 
                      round(above10_pop/1e6, 1), "M"),
         lab = factor(lab, levels = unique(lab), ordered = T),
         smokePM_pred = replace_na(smokePM_pred, 0)) %>% 
  # plot a panel for each date showing the map of predictions
  {ggplot(data = ., 
          aes(fill = smokePM_pred, color = smokePM_pred)) + 
      geom_sf() + 
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.075))) +
      facet_wrap(~lab, ncol = 2) + 
      scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 1, direction = 1),
                            aesthetics = c("fill", "color"), 
                            limits = c(0, 150),
                            oob = scales::squish,
                            name = expression(paste(mu, "g/", m^3)),
                            values = scales::rescale(sinh(seq(0, 3, length.out = 100))), 
                            guide = guide_colorbar(barwidth = 10, 
                                                   direction = "horizontal"),
                            breaks = seq(0, 150, by = 25),
                            labels = c(seq(0, 125, by = 25), ">150")) + 
      theme_void() + 
      theme(strip.text = element_text(hjust = 0),
            strip.clip = "off",
            plot.margin = unit(c(20.5, 0, 0, 0), "points"),
            legend.key.height = unit(0.8, "lines"),
            plot.background = element_rect(fill = "white", color = "white"),
            legend.position = "bottom")} -> worst_days_map 

# panel c) number of people seeing at least one extreme day each year
# calculate the number of people seeing an extreme day each year using different thresholds
extremes_by_year <- smokePM_pred %>%
  mutate(year = lubridate::year(date)) %>% 
  # for each grid-year check whether it saw an extreme day
  summarise(pop_over50 = any(smokePM_pred >50),
            pop_over100 = any(smokePM_pred > 100), 
            pop_over200 = any(smokePM_pred > 200),
            .by = c(year, grid_id_10km)) %>% 
  # join the population data and calculate number of people in that cell 
  left_join(pop %>% left_join(grid) %>%
              transmute(grid_id_10km = ID, pop = mean*area)) %>% 
  # then sum up the population with at least one extreme day each year
  summarise(across(starts_with("pop_over"), ~sum(.x*pop)), 
            .by = year)

# pivot longer so each threshold can be its own line 
extremes_by_year %>% 
  pivot_longer(starts_with("pop")) %>% 
  # sort the thresholds so they have a sensible order
  mutate(name = gsub("pop_over", "", name) %>% 
           factor(levels = sort(as.numeric(unique(.))))) %>% 
  ggplot(aes(x = year, ymax = value/1e6, fill = name)) + 
  # make each threshold a ribbon from zero to the value
  geom_ribbon(aes(ymin = 0)) + 
  # adjust aesthetics 
  theme_classic() + 
  ylab("population with at least\n1 extreme day") +
  scale_y_continuous(labels = function(x){paste0(x, " M")}, 
                     breaks = seq(0, 125, by = 25)) + 
  scale_fill_manual(values = c("#b9b9b8", "lightblue4", "red3"), 
                    labels = expression(paste(">50", mu, "g/", m^3),
                                        paste(">100", mu, "g/", m^3),
                                        paste(">200", mu, "g/", m^3))) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points"),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.7)) -> extreme_ts

# put the panels together and save
plot_grid(plot_grid(cum_exposure, extreme_ts, ncol = 1, 
                    align = "v", axis = "l",
                    hjust = -0.07, label_x = 0.01, 
                    labels = c("a) Population-weighted cumulative exposure", 
                               "c) Population exposure to extreme days")), 
          worst_days_map, 
          hjust = -0.15, label_x = 0.01, 
          nrow = 1, rel_widths = c(1, 0.65),
          labels = c("", "b) Worst days")) %>% 
  ggsave(filename = file.path(path_figures, "pop_exposure_over10.png"), bg = "white",
         height = 6, width = 8.5, units = "in")