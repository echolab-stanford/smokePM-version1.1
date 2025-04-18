library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)

source("scripts/setup/00_03_load_paths.R")

# burned area - smoke connection 
# reasons for divergence of pop-avg smoke and burned area 
# 1) where the people are vs where the fires burn --> non-pop avg shouldn't have same issue
# 2) what burns --> use an inventory? (GFED4 dry matter emissions is what Minghao used)

# both from https://gwis.jrc.ec.europa.eu/apps/country.profile/downloads
gfed <- read.csv("./data/emission_gfed_full_2002_2023.csv") %>% 
  filter(gid_0 %in% c("USA","CAN"))
BA <- read.csv("./data/MCD64A1_burned_area_full_dataset_2002-2023.csv") %>% 
  filter(gid_0 %in% c("USA","CAN"))

grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))
grid %<>% mutate(area = st_area(geometry) %>% unclass)

pop <- file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid") %>%
  list.files(full.names = TRUE) %>% 
  purrr::map(read.csv) %>% list_rbind

pop %<>% left_join(grid) %>% 
  mutate(pop = mean*area) %>% 
  transmute(grid_id_10km = ID, 
            pct_area = area/sum(area),
            pct_pop = pop/sum(pop), 
            pop)
# rm(grid); gc()
# gfed %>% 
#   filter(year >= 2006 ) %>% 
#   summarise(PM25 = sum(PM25), 
#             .by = c(year, country)) %>% 
#   ggplot(aes(x = year, y = PM25, color = country)) + 
#   geom_line() + 
#   theme_classic()

smokePM_pred <- file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>% 
  mutate(smokePM_pred = pmax(0, smokePM_pred))

smokePM_pred %>% 
  mutate(year = lubridate::year(date)) %>% 
  summarise(sum = sum(smokePM_pred)/365, 
            .by=c(year, grid_id_10km)) %>%
  left_join(pop %>% select(grid_id_10km, pct_pop, pct_area)) %>% 
  summarise(annual_avg = sum(sum*pct_area),
            annual_popavg = sum(sum*pct_pop),
            .by = year) %>%
  left_join(gfed %>% 
              filter(year >= 2006 ) %>% 
              summarise(PM25 = sum(PM25)/1e6, 
                        .by = c(year, gid_0)) %>% 
              pivot_wider(values_from = PM25, names_from = gid_0, 
                          names_prefix = "gfed_")) %>% 
  left_join(BA %>% 
              filter(year >= 2006 & gid_0 == "USA") %>% 
              summarise(across(forest:other, sum),
                        .by = c(year, gid_0)) %>% 
              mutate(total = pick(forest:other) %>% rowSums, 
                     across(forest:other, ~.x/total),
                     .keep = "unused")) -> test 
test %>%
  mutate(year_lab = ifelse(year >= 2020, as.character(year), "")) %>%
  filter(year <= 2023) %>% # limit to 2023 given availability of burned area/emissions data from GWIS
  {plot_grid(ggplot(.,aes(x = annual_avg, y = annual_popavg)) +
               geom_point() +
               geom_text(data = data.frame(lab = paste0("r = ", round(cor(.$annual_avg, .$annual_popavg), 2))),
                         aes(label = lab),
                         inherit.aes = FALSE, x = -Inf, y = Inf,
                         hjust = -0.57, vjust = 7.75) +
               geom_text(aes(label = year_lab), 
                         nudge_x = 0.1,
                         hjust = "inward", vjust = "top") +
               theme_classic() + 
               # xlab("Average PM2.5") + 
               # ylab("Population-weighted\naverage PM2.5") + 
               xlab(expression(paste("Average smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))) + 
               ylab(expression(atop("Population-weighted",
                                    paste("average smoke ", PM[2.5], " (", mu, "g/", m^3, ")")))),
             ggplot(.,aes(x = gfed_USA, y = annual_avg, color = gfed_CAN)) + 
               geom_point() + 
               geom_text(data = data.frame(lab = paste0("r = ", round(cor(.$gfed_USA, .$annual_avg), 2))), 
                         aes(label = lab),
                         inherit.aes = FALSE, x = -Inf, y = Inf, 
                         hjust = -0.57, vjust = 7) + 
               geom_text(data = data.frame(lab = paste0("r = ", round(cor(.$gfed_USA[.$year != 2023], 
                                                                          .$annual_avg[.$year != 2023]), 2),
                                                        " (without 2023)")), 
                         aes(label = lab),
                         inherit.aes = FALSE, x = -Inf, y = Inf, 
                         hjust = -0.16, vjust = 8.5) + 
               geom_text(aes(label = year_lab),
                         hjust = "inward", vjust = "inward") +
               scale_color_viridis_c(option = "rocket", 
                                     end = 0.7,
                                     name = "Biomass burning\nPM2.5 emissions\nin Canada\n(million tons)") + 
               theme_classic() + 
               theme(legend.position = "bottom") + 
               # ylab("Average PM2.5") + 
               ylab(expression(paste("Average smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))) + 
               # xlab("Biomass burning PM2.5 emissions\nin US (million tons)") + 
               xlab(expression(atop(paste("Biomass burning ", PM[2.5], " emissions"), 
                                    "in US (million tons)"))),
             ggplot(.,aes(y = gfed_USA, x = total/1e6, color = forest)) + 
               geom_point() + 
               geom_text(data = data.frame(lab = paste0("r = ", round(cor(.$gfed_USA, .$total), 2))), 
                         aes(label = lab),
                         inherit.aes = FALSE, x = -Inf, y = Inf,
                         hjust = -0.57, vjust = 7.75) + 
               geom_text(aes(label = year_lab), 
                         vjust = "inward") +
               scale_color_viridis_c(option = "mako", 
                                     end = 0.8,
                                     name = "% forest", 
                                     breaks = seq(0.05, 0.175, by = 0.04), 
                                     labels = scales::percent) + 
               # scale_color_continuous(name = "pct forest", 
               #                        breaks = seq(0.05, 0.175, by = 0.04), 
               #                        labels = scales::percent) + 
               theme_classic() + 
               scale_x_continuous(expand = expansion(mult = 0.1)) +
               theme(legend.position = "bottom") + 
               # ylab("Biomass burning PM2.5\n emissions in US (million tons)") + 
               ylab(expression(atop(paste("Biomass burning ", PM[2.5], " emissions"), 
                                    "in US (million tons)"))) + 
               xlab("Burned area (million hectares)"), 
             nrow = 1, align = "h", labels = "auto")} %>% 
  ggsave(filename = file.path(path_figures, "annual_burning_emissions.png"),
         width = 10, height = 4.5)
