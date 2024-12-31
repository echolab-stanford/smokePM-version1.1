# create a figure showing the flags for smoke PM2.5  
library(tidyverse)
library(magrittr)
library(sf)
library(cowplot)

# load data on the flags and station PM2.5 data
flags <- arrow::read_feather("./exclusions_smokePM.feather")
stationPM <- list.files(file.path(path_data, "/3_intermediate/station_smokePM_update/"),
                        full.names = T, pattern = "rds") %>% 
  map(readRDS) %>% list_rbind %>% 
  ungroup %>%
  mutate(across(year:day, as.numeric))

# calculate the annual number of days in each smoke PM2.5 category for two time periods to show changes over time 
stationPM %>% 
  ungroup %>% 
  filter(!is.na(pm25)) %>% 
  # create the smoke PM bins and define the time periods
  mutate(smokePM_bin = cut(smokePM, breaks = c(-Inf, 0, 1, 2, 5, 10, 20, 50, 100, 200, Inf), 
                           include.lowest = T), 
         time_period = case_when(year <= 2018 ~ "2006-2018", 
                                 year > 2018 ~ "2019-2023")) %>% 
  # calculate number of days in each bin per station-year then average over the time period
  summarise(n_day_obs = n(), 
            bin_mean = mean(smokePM),
            .by = c(time_period, id, year, smokePM_bin)) %>% 
  # to ensure we account for different # of days with reporting, calculate pct of days in each bin for a station-year 
  # then multiply by 365 to get # of days
  mutate(n_day_365 = n_day_obs/sum(n_day_obs)*365, 
         .by = c(year, id, time_period)) %>% 
  # average the number of days (of 365) for each time period 
  summarise(n_day = mean(n_day_365), 
            bin_mean = mean(bin_mean),
            .by = c(time_period, smokePM_bin)) -> test

# look at the flags after 2019, with non-zero smoke PM 
flags %>% 
  filter(!is.na(smoke_pm)) %>% 
  filter(!is.na(annual_mean)) %>%
  filter(date >= as.Date("2019-01-01")) %>% 
  filter(smoke_pm > 0) %>% 
  # define relevance as observed PM2.5 > annual average that needs to be obtained to make this year + 2 previous years meet the regulatory limit (12ug)
  mutate(relevant = pm25_smokepm > (36 - 2*annual_mean),
         # classify smoke PM into the same bins as before
         smokePM_bin = cut(smoke_pm, breaks = c(-Inf, 0, 1, 2, 5, 10, 20, 50, 100, 200, Inf), 
                           include.lowest = F), 
         fire_inform_hours = pmax(fire_inform_hours, fire_exclusion_hours)) %>% 
  # for each smoke PM bin, calculate the % of days with any inform flags and exclusions requested
  # do for both relevant and all days
  summarise(across(c(fire_exclusion_hours, fire_inform_hours), 
                   list(all =~mean(.x > 0, na.rm = T), 
                        relevant =~ mean(.x[relevant] > 0, na.rm = T))),
            n_all = n(), 
            n_relevant = sum(relevant),
            .by = c(smokePM_bin)) %>% 
  # clean up the df for plotting, pivot longer and then clean up the names
  pivot_longer(starts_with("fire")) %>% 
  mutate(name = gsub("fire_|_hours", "", name)) %>% 
  separate(name, sep = "_", into = c("type", "relevant")) %>% 
  mutate(pct_relevant = n_relevant/n_all) %>% 
  # we'll only plot the statistics for relevant days, so filter to those
  filter(relevant == "relevant") %>% 
  # clean up the smoke PM bin labels for legibility in the plot 
  arrange(smokePM_bin) %>%
  mutate(smokePM_bin_lab = gsub("- Inf", "+", gsub("\\(|\\]", "", gsub(",", "-", smokePM_bin))), 
         smokePM_bin_lab = factor(smokePM_bin_lab, ordered = T, 
                                  levels = unique(smokePM_bin_lab))) %>% 
  mutate(type = factor(type, levels = c("inform", "exclusion"))) %>% 
  # plot with top panel showing % of days in each smoke PM2.5 bin that were flagged 
  # and bottom panel showing avg number of days in each bin in different time periods
  {plot_grid(ggplot(., aes(x = smokePM_bin_lab, y = value,
                           color = type)) +
               geom_point() +
               geom_line(aes(group = type,
                             linetype = type)) +
               scale_color_manual(name = "flag type", 
                                  values = c("black", "red")) +
               scale_linetype(name = "flag type") + 
               scale_y_continuous(labels = scales::percent_format()) + 
               ylab("percent of days flagged") + 
               theme_classic() +
               theme(legend.position = "inside", 
                     legend.position.inside = c(0.2, 0.6), 
                     axis.text.x = element_text(angle = 0), 
                     axis.title.x = element_blank()),
             test %>% 
               filter(bin_mean > 0) %>% 
               arrange(smokePM_bin) %>%
               mutate(smokePM_bin_lab = gsub("- Inf", "+", gsub("\\(|\\]", "", gsub(",", "-", smokePM_bin))), 
                      smokePM_bin_lab = factor(smokePM_bin_lab, ordered = T, 
                                               levels = unique(smokePM_bin_lab))) %>% 
               ggplot(aes(x = smokePM_bin_lab, y = n_day, color = time_period, fill = time_period)) + 
               geom_col(position = "dodge") + 
               theme_classic() + 
               scale_color_manual(values = c("grey20", "deepskyblue3"), 
                                  aesthetics = c("color", "fill")) + 
               ylab("# of days\nper year") + 
               #xlab("smoke PM2.5") + 
               xlab(expression(paste("smoke ", PM[2.5], " (", mu, "g/", m^{3}, ")"))) + 
               #xlab(expression(paste("smoke ", mu, "g/", m^3)
               theme(legend.position = "inside", 
                     legend.position.inside = c(0.8, 0.8), 
                     legend.title = element_blank(),
                     legend.key.size = unit(1, "lines")),
             nrow = 2, ncol = 1, align = "v", axis = "lr", 
             rel_heights = c(1, 0.5))} %>% 
  ggsave(filename = file.path(path_figures, "/exemption_flags.png"), 
         width = 5, height = 5)

