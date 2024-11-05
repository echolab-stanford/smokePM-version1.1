pacman::p_load(fastverse, tidyverse, lubridate, fixest, epair, tictoc, arrow)
setwd(here::here())

full_df <- arrow::read_feather('hourly_data_bystate/exclusions_smokePM.feather')

full_df[, `:=`(flag = fifelse((excluded_hours > 0 | other_flags_hours > 0), 1, 0),
               wildfire_inform_flag = fifelse(fire_inform_hours > 0, 1, 0),
               wildfire_exclusion_flag = fifelse(fire_exclusion_hours > 0, 1, 0))]
full_df[, nonsmoke_pm := pm25_smokepm - smoke_pm]
full_df[, `:=`(p98_relevant = fifelse(pm25_smokepm > (35*3) - (2*p98), 1, 0),
               annual_relevant = fifelse(pm25_smokepm > (12*3) - (2*annual_mean), 1, 0),
               p98_relevant_nonsmoke = fifelse(nonsmoke_pm > (35*3) - (2*p98), 1, 0),
               annual_relevant_nonsmoke = fifelse(nonsmoke_pm > (12*3) - (2*annual_mean), 1, 0))]
full_df[, `:=`(butfor_p98 = fifelse((p98_relevant == 1 & p98_relevant_nonsmoke == 0), 'Relevant because of smoke', fifelse(p98_relevant == 1, 'Relevant without smoke', 'Not relevant')),
               butfor_annual = fifelse((annual_relevant == 1 & annual_relevant_nonsmoke == 0), 'Relevant because of smoke', fifelse(annual_relevant == 1, 'Relevant without smoke', 'Not relevant')))]

bins <- c(-Inf,1,2,5,10,20,50,100,200,Inf)
annual_standard_bins <- full_df[!is.na(annual_relevant) & !is.na(smoke_pm)][, smoke_pm_group := cut(smoke_pm, breaks = bins, include_lowest = TRUE)][, .(exclusion = mean(exclusion, na.rm = TRUE),
                                                                                                                                                       exclusion_sd = sd(exclusion, na.rm = TRUE),
                                                                                                                                                       flag = mean(flag, na.rm = T),
                                                                                                                                                       flag_sd = sd(flag, na.rm = T),
                                                                                                                                                       wildfire_inform = mean(wildfire_inform_flag, na.rm = T),
                                                                                                                                                       wildfire_inform_sd = sd(wildfire_inform_flag, na.rm = T),
                                                                                                                                                       wildfire_exclusion = mean(wildfire_exclusion_flag, na.rm = TRUE),
                                                                                                                                                       wildfire_exclusion_sd = sd(wildfire_exclusion_flag, na.rm = TRUE),
                                                                                                                                                       bin_x = mean(smoke_pm, na.rm = TRUE)),
                                                                                                                                                   by = list(smoke_pm_group, annual_relevant)]
long_df <- merge(annual_standard_bins %>% pivot_longer(!c('smoke_pm_group','annual_relevant')) %>% filter(grepl('_sd', name)) %>% rename(sd = value) %>% mutate(name = gsub('_sd', '', name)),
                 annual_standard_bins %>% pivot_longer(!c('smoke_pm_group','annual_relevant')) %>% filter(!grepl('_sd', name) & !grepl('bin_x', name)) %>% rename(y = value)) %>%
                 merge(annual_standard_bins %>% select(smoke_pm_group, annual_relevant, bin_x))
binned_butfor_annual_plot <- ggplot(long_df %>% filter(name %in% c('wildfire_inform','wildfire_exclusion'), annual_relevant == 1), aes(x = bin_x, y = y, ymin = y - sd, ymax = y + sd, linetype = as.factor(name))) +
  geom_hline(yintercept = 0, color = 'blue', alpha = 0.2) + geom_hline(yintercept = 1, color = 'red', alpha = 0.2) +
  geom_line() +
  theme_minimal() + xlab('Smoke PM value (µg/m3)') + ylab('Percent of values flagged') +
  coord_cartesian(xlim = c(0,400)) + ggtitle("Percent of values flagged as wildfire by smoke PM level\nand relevance to annual PM2.5 standard") +
  scale_x_continuous(transform = 'log1p', breaks = c(0,bins[-1])) + scale_y_continuous(labels = scales::percent_format(), breaks = seq(0,1,0.25)) +
  scale_linetype(name = 'Flag type', labels = c('Exclusion requested','Informational')) +
  theme(panel.grid.minor = element_blank())
binned_butfor_annual_plot

p98_standard_bins <- full_df[!is.na(p98_relevant) & !is.na(smoke_pm)][, smoke_pm_group := cut(smoke_pm, breaks = bins, include_lowest = TRUE)][, .(exclusion = mean(exclusion, na.rm = TRUE),
                                                                                                                                                         exclusion_sd = sd(exclusion, na.rm = TRUE),
                                                                                                                                                         flag = mean(flag, na.rm = T),
                                                                                                                                                         flag_sd = sd(flag, na.rm = T),
                                                                                                                                                         wildfire_inform = mean(wildfire_inform_flag, na.rm = T),
                                                                                                                                                         wildfire_inform_sd = sd(wildfire_inform_flag, na.rm = T),
                                                                                                                                                         wildfire_exclusion = mean(wildfire_exclusion_flag, na.rm = TRUE),
                                                                                                                                                         wildfire_exclusion_sd = sd(wildfire_exclusion_flag, na.rm = TRUE),
                                                                                                                                                         bin_x = mean(smoke_pm, na.rm = TRUE)),
                                                                                                                                                     by = list(smoke_pm_group, p98_relevant)]
long_df <- merge(p98_standard_bins %>% pivot_longer(!c('smoke_pm_group','p98_relevant')) %>% filter(grepl('_sd', name)) %>% rename(sd = value) %>% mutate(name = gsub('_sd', '', name)),
                 p98_standard_bins %>% pivot_longer(!c('smoke_pm_group','p98_relevant')) %>% filter(!grepl('_sd', name) & !grepl('bin_x', name)) %>% rename(y = value)) %>%
  merge(p98_standard_bins %>% select(smoke_pm_group, p98_relevant, bin_x))
binned_butfor_p98_plot <- ggplot(long_df %>% filter(name %in% c('wildfire_inform','wildfire_exclusion'), p98_relevant == 1), aes(x = bin_x, y = y, ymin = y - sd, ymax = y + sd, linetype = as.factor(name))) +
  geom_hline(yintercept = 0, color = 'blue', alpha = 0.2) + geom_hline(yintercept = 1, color = 'red', alpha = 0.2) +
  geom_line() +
  theme_minimal() + xlab('Smoke PM value (µg/m3)') + ylab('Percent of values flagged') +
  coord_cartesian(xlim = c(0,400)) + ggtitle("Percent of values flagged as wildfire by smoke PM level\nand relevance to daily PM2.5 standard") +
  scale_x_continuous(transform = 'log1p', breaks = c(0,bins[-1])) + scale_y_continuous(labels = scales::percent_format(), breaks = seq(0,1,0.25)) +
  scale_linetype(name = 'Flag type', labels = c('Exclusion requested','Informational')) +
  theme(panel.grid.minor = element_blank())
binned_butfor_p98_plot
