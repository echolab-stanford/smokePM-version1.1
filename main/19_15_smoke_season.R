library(tidyverse)
library(magrittr)
library(cowplot)
library(sf)

source("scripts/setup/00_03_load_paths.R")

grid <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

smokePM_pred <- file.path(path_output, "version1.1", "smokePM", "predictions", "smokePM_predictions_10km_20060101_20241231.rds") %>% 
  readRDS %>% 
  mutate(smokePM_pred = pmax(0, smokePM_pred))

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

pop_exposure %>% 
  full_join(data.frame(date = seq.Date(as.Date("2006-01-01"), as.Date("2023-12-31"), "days"))) %>% 
  mutate(across(c(avg_smokePM), 
                ~replace_na(.x, 0)), 
         year = year(date),
         y_color = ifelse(year < 2020, NA, year) %>% as.character, 
         # convert everything to days in 2000 to allow for plotting over the same x axis range
         day = paste0("2000-", month(date), "-", day(date)) %>% as.Date()) %>% 
  select(-date, -ends_with("_pop")) %>% 
  {rbind(mutate(., 
                year = as.character(year), 
                type = "annual"),
         filter(., year <=2019) %>% 
           summarise(avg_smokePM = mean(avg_smokePM), 
                     .by = day) %>% 
           mutate(year = "2006-2019\naverage", 
                  type = "average", 
                  y_color = "2006-2019\naverage"))} %>% 
  mutate(date_rank = rank(desc(avg_smokePM))) %>% 
  arrange(year, day) %>% 
  # filter(!is.na(y_color)) %>% 
  mutate(smoke_season_day = avg_smokePM > 1,
         .by = year) %>% 
  {ggplot(data = filter(., !is.na(y_color)),
          aes(y = avg_smokePM, 
              color = y_color, 
              linewidth = I(case_when(type == "annual" & is.na(y_color) ~ 0.35, 
                                      type == "annual" ~ 0.6, 
                                      T ~ 1.2)),
              group = year)) + 
      geom_line(aes(x = day)) + 
      # geom_point(aes(alpha = I(ifelse(date_rank <= 10, 1, 0)), 
      #                x = day)) + 
      geom_text(data = rbind(filter(., as.numeric(year) >= 2020) %>% 
                               # filter(day == max(day), .by = year) %>% 
                               # mutate(season_length = difftime(smoke_season_day_end, smoke_season_day_start, units = "days") + 1),
                               summarise(season_length = sum(smoke_season_day), .by = c(year, y_color, type)), 
                             filter(., as.numeric(year) < 2020) %>% 
                               summarise(season_length = sum(smoke_season_day), .by = c(year, y_color, type)) %>% 
                               summarise(season_length = round(mean(season_length), 0)) %>% 
                               mutate(type = "average", year = "2006-2019\naverage") %>% 
                               mutate(y_color = year)),
                aes(x = as.Date("2000-12-31") + as.difftime(-20, units = "days"), 
                    y = sinh(-0.5 + ifelse(type == "average", 0.05, (-as.numeric(year) + 2019)*0.22))*2, 
                    label = paste0(year, ", ", season_length, " days")),
                hjust = 0, vjust = 0.1, lineheight = 0.65) +
      geom_hline(yintercept = 1, linetype = "dashed") + 
      # geom_linerange(data = select(., year, y_color, type, smoke_season_day_start, smoke_season_day_end) %>% unique, 
      #                aes(xmin = smoke_season_day_start, xmax = smoke_season_day_end, 
      #                    y = sinh(-0.5 + ifelse(type == "average", 0, (-as.numeric(year) + 2019)*0.22))*2)) + 
      geom_point(data = filter(., smoke_season_day == T) %>% filter(as.numeric(year) >= 2020) %>% select(year, day, y_color, type), 
                 aes(x = day, y = sinh(-0.5 + ifelse(type == "average", 0, (-as.numeric(year) + 2019)*0.22))*2), 
                 shape = 16) + 
      geom_point(data = filter(., smoke_season_day == T) %>% filter(as.numeric(year) < 2020) %>% select(year, day, y_color, type), 
                 aes(x = day, y = sinh(-0.55 + (-as.numeric(year) + 2019)*0.035)*2), alpha = 0.2, shape = 16) + 
      coord_cartesian(clip = "off") + 
      scale_color_manual(values = c("black", MetBrewer::met.brewer("Juarez", 5))) + # MetBrewer::met.brewer("Degas", 4)
      scale_x_date(date_labels = "%b",
                   breaks = seq.Date(as.Date("2000-02-01"), as.Date("2000-12-31"), by = "2 month"),
                   expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0.03)), breaks = c(0, 1, 2, 5, 10, 20, 30), 
                         trans = "pseudo_log") + 
      # xlab("Day of year") + 
      ylab(expression(paste("                  Daily smoke exposure (", mu, "g/", m^3, ")"))) + 
      theme_classic() + 
      theme(legend.position = "none", 
            axis.title.x = element_blank(),
            plot.margin = unit(c(20.5, 80, 5.5, 5.5), "points"))} %>%
  ggsave(filename = file.path(path_figures, "smoke_season_length.png"),
         width = 6, height = 4.25)


# mutate(smoke_season_day = avg_smokePM > 1) %>% 
#   summarise(smoke_season_day_start = min(day[smoke_season_day]), 
#             smoke_season_day_end = max(day[smoke_season_day]), 
#             nday = sum(smoke_season_day),
#             .by = year) %>% 