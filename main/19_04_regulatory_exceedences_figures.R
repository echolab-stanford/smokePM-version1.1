# create two figures, showing which exceedences have been caused by smoke over time and what smoke days would have to be dropped to bring stations back into attainment 
library(tidyverse)
library(magrittr)
library(sf)
library(cowplot)

nonContig_stateFIPS <- c("02","60","66","15","72","78","69")

source("scripts/setup/00_03_load_paths.R")

# quantile function to match how the EPA calculates quantiles
my_quant = function(x, p){
  return(sort(x)[ceiling(length(x)*p)])
}

stationPM <- list.files(file.path(path_data, "/3_intermediate/station_smokePM_auto/"),
                        full.names = T, pattern = "rds") %>% 
  map(readRDS) %>% list_rbind %>% 
  mutate(across(year:day, as.numeric))

# for each smoke PM threshold, calculate mean and 98th percentile of total PM for each year and station
# this takes a few minutes to run 
stationPM %>% 
  filter(!is.na(smokePM)) %>%
  filter(n() > 50, .by = c(id, year)) %>%
  transmute(id, date, year, pm25, 
            smokePM = pm25_anom*smoke_day, 
            nonsmoke_pm25 = pm25 - pm25_anom*smoke_day) %>%
  arrange(id, year, smokePM) %>%
  mutate(denom = 1:n(),
         drop_N = n() - denom,
         drop_gte = lead(smokePM, 1, default = Inf),
         all_mean = cumsum(pm25)/denom,
         all_98th = slider::slide_dbl(pm25, my_quant, p = 0.98, .before = Inf),
         # run the nonsmoke calculations just for a sanity check
         nonsmoke_mean = cumsum(nonsmoke_pm25)/denom,
         nonsmoke_98th = slider::slide_dbl(nonsmoke_pm25, my_quant, p = 0.98, .before = Inf),
         .by = c(id, year), 
         .keep = "none") %>% 
  # limit to the last obs for each smoke PM level 
  filter(denom == max(denom),
         .by = c(id, year, drop_gte)) %>% 
  # remove any time you'd drop days with smoke PM = 0
  filter(drop_gte > 0) -> threshold_annual

# then calculate the 3 year averages, using present year and 2 lags 
last_per_year = function(x, year, summary_fn = mean){
  data.frame(x, year) %>% 
    slice_tail(n = 1, by = year) %>%
    # try slice_min? slice_min(drop_gte, n = 1, by = year) %>% 
    pull(x) %>% 
    summary_fn
}
# this can also be a little slow to run as we're calculating the averages for all possible values of dropped smoke PM2.5 
threshold_annual %>% 
  arrange(year) %>%
  mutate(drop_N_pct = drop_N/(drop_N + denom), 
         date = paste0(year, "-01-01") %>% as.Date) %>%
  {slider::slide_period_dfr(., .$date, "year", function(df_3yr){
    df_3yr %>%
      arrange(id, desc(drop_gte), year) %>% 
      # for each possible threshold to drop at, take the annual value for each of 
      # the 3 years that corresponds to striking days at or above that threshold 
      # (i.e., the last value for a give year above that threshold so slice_tail)
      mutate(across(c(all_mean, all_98th, nonsmoke_mean, nonsmoke_98th, drop_N_pct), 
                    ~slider::slide2_dbl(.x, year, last_per_year, summary_fn = mean, .before = Inf),
                    .names = "{.col}_3yr"), 
             denom_3yr = slider::slide2_dbl(denom, year, last_per_year, summary_fn = sum, .before = Inf),
             n_year = slider::slide2_dbl(all_mean, year, last_per_year, summary_fn = length, .before = Inf), 
             end_year = max(year),
             .by = id) %>% 
      mutate(check_year = max(year)) %>%
      filter(n_year == max(n_year), 
             .by = c(id, drop_gte)) %>% 
      filter(denom_3yr == min(denom_3yr), 
             .by = c(id, drop_gte)) %>% 
      select(id, end_year, check_year, drop_gte, n_year, ends_with("_3yr")) %>%
      return
  }, .before = 2, .complete = T)}  -> threshold_3yr
# saveRDS(list(yr3 = threshold_3yr, annual = threshold_annual), 
#         "./scratch/attainment_dfs_2025april14.rds")
threshold_3yr %>% 
  filter(check_year == end_year & n_year == 3) %>% 
  filter(drop_gte == Inf) %>%# take obs equivalent to dropping nothing
  filter(!is.na(all_mean_3yr)) %>%
  mutate(Daily_extremes = case_when(all_98th_3yr < 35 ~ "in_attainment",
                                    all_98th_3yr >= 35 & nonsmoke_98th_3yr >= 35 ~ "non_attainment",
                                    all_98th_3yr >= 35 & nonsmoke_98th_3yr < 35 ~ "non_from_smoke"),
         Annual_averages = case_when(all_mean_3yr < 9 ~ "in_attainment",
                                     all_mean_3yr >= 9 & nonsmoke_mean_3yr >= 9 ~ "non_attainment",
                                     all_mean_3yr >= 9 & nonsmoke_mean_3yr < 9 ~ "non_from_smoke"),
         .by = c(end_year, check_year, id)) -> attainment_cat

# epa_ll <- st_read("~/BurkeLab Dropbox/projects/smokePM-prediction/data/EPA/epa_station_locations/") 
epa_ll = read_sf(file.path(path_data, "EPA_automated", "epa_station_locations")) %>% 
  # stations with NA for grid cell are in HI or AK
  rename(grid_id_10km = grid_10km)
# states <- tigris::states(cb = TRUE)
# since tigris wasn't working, this shapefile is downloaded from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html --> states --> 1 : 20,000,000 (national)  shapefile 
states <- read_sf("data/cb_2023_us_state_20m")
nonContig_stateFIPS <- c("02","60","66","15","72","78","69")

station_cat <- attainment_cat %>% 
  filter(end_year == check_year) %>%
  filter(end_year >= 2020) %>%
  summarise(cat = case_when(all(Daily_extremes != "non_from_smoke") & 
                              all(Annual_averages != "non_from_smoke") ~ "unaffected", 
                            all(Daily_extremes != "non_from_smoke") & 
                              any(Annual_averages == "non_from_smoke") ~ "average",
                            any(Daily_extremes == "non_from_smoke") & 
                              all(Annual_averages != "non_from_smoke") ~ "extreme",
                            any(Daily_extremes == "non_from_smoke") & 
                              any(Annual_averages == "non_from_smoke") ~ "both",
                            T ~ "error"),
            .by = id) %>%
  arrange(desc(cat)) 
station_cat %>% 
  left_join(epa_ll %>% rename(id = stn_id)) %>% 
  st_as_sf %>%
  {ggplot(.) + 
      geom_sf(data = states %>% filter(STATEFP %in% nonContig_stateFIPS == FALSE),
              fill = "grey99") +
      geom_sf(aes(color = cat), size = 1.15) + 
      scale_color_manual(values = c("#ae3a4e","#3f2949", "#4885c1", "grey80")) + 
      theme_void() + 
      theme(legend.position = "none")} -> attain_map
biscale::bi_legend(pal = c("1-1" = "#cccccc", 
                           "2-1" = "#4885c1", 
                           "1-2" = "#ae3a4e", 
                           "2-2" = "#3f2949"), 
                   dim = 2) + 
  xlab("daily extremes") + ylab("annual averages") + 
  scale_x_continuous(breaks = c(1, 2), 
                     labels = c("status\nunchanged\nby smoke", "over limit\ndue to\nsmoke")) + 
  scale_y_continuous(breaks = c(1, 2), 
                     labels = c("status\nunchanged\nby smoke", "over limit\ndue to\nsmoke")) + 
  geom_text(data = mutate(station_cat, 
                          x = cat %in% c("extreme", "both") + 1, 
                          y = cat %in% c("average", "both") + 1) %>% 
              summarise(count = n(), 
                        .by = c(cat, x, y)) %>% 
              mutate(pct = count/sum(count)), 
            aes(x = x, y = y, label = scales::percent(pct, accuracy = 1), 
                color = I(ifelse(cat == "both", "white", "grey5"))), 
            size = 3) + 
  theme(axis.text.x = element_text(size = 6.5), 
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 6.5), 
        axis.title = element_text(face = "bold", size = 7)) -> attain_legend

attainment_cat %>% 
  filter(end_year == check_year) %>%
  select(end_year, Daily_extremes, Annual_averages) %>%
  pivot_longer(c(Daily_extremes, Annual_averages)) %>% 
  summarise(n = n(),
            .by = c(end_year, name, value)) %>% 
  pivot_wider(names_from = value, values_from = n) %>% 
  mutate(name = gsub("_", " ", name)) %>%
  ggplot(data = ., aes(x = end_year)) + 
  geom_ribbon(aes(ymin = 0, ymax = in_attainment),  
              color = NA, fill = "grey90") + 
  geom_ribbon(aes(ymin = in_attainment, ymax = in_attainment + non_attainment, 
                  fill = name),  
              color = NA, alpha = 0.4) + # "#ba8890"
  geom_ribbon(aes(ymin = in_attainment + non_attainment, 
                  ymax = in_attainment + non_attainment + non_from_smoke, 
                  fill = name),  
              color = NA) + 
  geom_text(data = data.frame(name = c("Annual averages", "Daily extremes")), 
            aes(x = c(2015, 2015), 
                y = c(300, 300)), 
            label = rep("under threshold", 2),
            size = 2.75,
            vjust = 0,
            lineheight = .75,
            color = "grey10",
            inherit.aes = FALSE) + 
  geom_text(data = data.frame(name = c("Annual averages", "Daily extremes")), 
            aes(x = c(2015, 2015), 
                y = c(290, 290)), 
            label = c(expression(paste("(<9", mu, "g/",m^3," annual average)")),
                      expression(paste("(98th percentile <35", mu, "g/",m^3,")"))),
            size = 2.75,
            vjust = 1,
            lineheight = .75,
            color = "grey10",
            inherit.aes = FALSE) + 
  geom_text(data = data.frame(x = c(2011, 2020, 
                                    2011, 2019),
                              y = c(800, 1275, 
                                    850, 800), 
                              label = c("over threshold\nwithout smoke", 
                                        "over threshold\ndue to smoke",
                                        "over threshold\nwithout smoke", 
                                        "over threshold\ndue to smoke"),
                              name = c(rep("Annual averages", 2),
                                       rep("Daily extremes", 2))), 
            aes(x = x, y = y, label = label), 
            size = 2.75,
            lineheight = .75,
            color = "grey10",
            inherit.aes = FALSE) + 
  geom_segment(data = data.frame(x = c(2011, 2019, 2020),
                                 xend = c(2011, 2020, 2021.5),
                                 y = c(950, 900, 1175),
                                 yend = c(1090, 1050, 1100),
                                 name = c("Daily extremes", 
                                          "Daily extremes", 
                                          "Annual averages")), 
               aes(x = x, xend = xend, y = y, yend = yend),
               color = "grey10",
               linewidth = 0.3,
               inherit.aes = FALSE) + 
  facet_wrap(~name) + 
  scale_fill_manual(values = c("#ae3a4e", "#4885c1")) + 
  theme_classic() + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                     breaks = c(0, 300, 600, 900, 1200)) + 
  scale_x_continuous(expand = expansion(mult = 0)) + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        strip.background = element_blank(), 
        strip.text = element_text(face = "bold")) + 
  ylab("number of stations") -> attain_ts

plot_grid(attain_ts + 
            theme(plot.margin = unit(c(15, 12, 5.5, 5.5), "points"), 
                  axis.text = element_text(size = 11*0.7),
                  panel.spacing.x = unit(1.3, "lines")), 
          ggdraw() +
            draw_plot(attain_map, 
                      -0.07, 0, 1, 1) +
            draw_plot(attain_legend + theme(plot.background = element_rect(fill = NA)), 
                      0.5, 0, 0.7, 0.7) + 
            theme(plot.margin = unit(c(10, 0, 0, 0), "points"), 
                  plot.background = element_rect(fill = "white", color = "white")), 
          rel_heights = c(0.9, 1),
          nrow = 2, 
          hjust = -0.02,
          vjust = 1.2,
          label_size = 13,
          labels = c("a) number of stations over regulatory threshold over time", 
                     "b) spatial distribution of smoke-affected thresholds (2020 - 2024)")) %>% 
  ggsave(filename = file.path(path_figures, "attainment.png"), 
         height = 5, width = 6)

rm(attain_legend, attain_ts, attain_map)

# finally, use the 3yr values to classify each station as in attianment always, 
# out of attainment even with smoke PM struck, or the # of days to strike for smkoe PM to be in attainment 
threshold_3yr %>%   
  filter(check_year == end_year & n_year == 3) %>% 
  mutate(under_mean = all_mean_3yr < 9,
         under_98th = all_98th_3yr < 35) %>% 
  summarise(all_mean = all_mean_3yr[drop_gte == Inf],
            nonsmoke_mean = nonsmoke_mean_3yr[drop_gte == Inf], 
            all_98th = all_98th_3yr[drop_gte == Inf], 
            nonsmoke_98th = nonsmoke_98th_3yr[drop_gte == Inf], 
            pct_smokeDay = (max(denom_3yr) - min(denom_3yr))/max(denom_3yr),
            thresh_smokePM_mean = max(drop_gte[under_mean]), 
            thresh_pctDays_mean = {if(any(under_mean)){drop_N_pct_3yr[drop_gte == thresh_smokePM_mean]}else{Inf}},
            thresh_smokePM_98th = max(drop_gte[under_98th]), 
            thresh_pctDays_98th = {if(any(under_98th)){drop_N_pct_3yr[drop_gte == thresh_smokePM_98th]}else{Inf}},
            thresh_smokePM_both = pmin(thresh_smokePM_mean, thresh_smokePM_98th),
            thresh_pctDays_both = pmax(thresh_pctDays_mean, thresh_pctDays_98th),
            .by = c(id, end_year, check_year)) %>% 
  mutate(n = n(), 
         .by = c(id, end_year, check_year))  -> station_year_thresh

plot_grid(station_year_thresh %>% 
            filter(thresh_smokePM_both > 0 & thresh_smokePM_both < Inf) %>%
            filter(end_year >= 2020) %>% 
            mutate(end_year = paste0(end_year, " (n = ", n(), ")"), 
                   .by = end_year) %>% 
            mutate(year = as.factor(end_year)) %>%
            ggplot(aes(y = thresh_pctDays_both, 
                       x = thresh_smokePM_both)) + 
            geom_point(size = 1, alpha = 0.5) + 
            xlab(expression(paste("strike days with smoke ", PM[2.5], " > X ", mu, "g/", m^3))) + 
            ylab("% of days to strike") + 
            theme_classic() + 
            scale_y_continuous(labels = scales::label_percent()) + 
            theme(legend.position = "inside", 
                  legend.position.inside = c(0.8, 0.7), 
                  plot.margin = unit(c(15.5, 0, 5.5, 5.5), "points")), 
          epa_ll %>% rename(id = stn_id) %>% 
            full_join(station_year_thresh %>% 
                        filter(end_year >= 2020) %>% 
                        summarise(final_pctDays = max(thresh_pctDays_both), 
                                  final_smokePM = min(thresh_smokePM_both), 
                                  class = case_when(final_pctDays <= 0 & final_smokePM == Inf ~ "in attainment\nwith smoke", 
                                                    final_pctDays == Inf & final_smokePM <= 0 ~ "out of attainment", 
                                                    final_pctDays > 0 & is.finite(final_pctDays) & final_smokePM > 0 & is.finite(final_smokePM) ~ "smoke affected", 
                                                    T ~ "error"),
                                  .by = id)) %>% 
            mutate(class_color = case_when(class == "out of attainment" ~ "black", 
                                           class == "in attainment\nwith smoke" ~ "grey75",  
                                           T ~ NA)) %>% 
            {ggplot() + 
                geom_sf(data = states %>% filter(STATEFP %in% nonContig_stateFIPS == FALSE), 
                        fill = "white") + 
                geom_sf(data = filter(., class != "smoke affected"), 
                        aes(color = I(class_color),
                            shape = class, 
                            size = I(ifelse(class_color == "black", 1, 0.7)))) + 
                geom_sf(data = filter(., class == "smoke affected"),
                        size = 1.5, shape = 21, alpha = 0.8,
                        aes(color = pmin(final_pctDays*365, 60), 
                            fill = pmin(final_pctDays*365, 60))) +
                scale_color_gradientn(name = "# of days\nto strike", 
                                      aesthetics = c("color", "fill"),
                                      colors = cmocean::cmocean("rain", start = 0.1)(100),
                                      # colors = cmocean::cmocean("tarn", end = 0.45, direction = -1)(100),
                                      breaks = c(15, 30, 45, 60),
                                      labels = c("15", "30", "45", ">60")) +
                scale_shape_manual(values = c(5, 4), 
                                   guide = guide_legend(theme = theme(legend.title = element_blank(), 
                                                                      legend.direction = "vertical",
                                                                      legend.position.inside = c(0.1, 0.1)), 
                                                        override.aes = list(color = c("grey75", "black")))) + 
                theme_void() + 
                theme(legend.position = "bottom", 
                      legend.position.inside = c(0.91, 0.3))}, 
          nrow = 1, hjust = -0.05, label_x = 0.01, 
          rel_widths = c(0.65, 1),
          label_size = 12.5,
          labels = c("a) days to exempt to be under threshold", 
                     "b) worst year for each station")) %>% 
  ggsave(filename = file.path(path_figures, "strike_smoke_days.png"), 
         bg = "white",
         width = 9, height = 4)

# SI figure with cdf
plot_pctDays_threshd <- station_year_thresh %>% 
  filter(end_year == check_year & check_year >= 2020) %>% 
  arrange(end_year, thresh_pctDays_both) %>%
  select(end_year, thresh_pctDays_both) %>%
  mutate(n_attain = 1:n(), 
         .by = end_year) %>% 
  filter(n_attain == max(n_attain),
         .by = c(end_year, thresh_pctDays_both)) %>%
  mutate(pct_attain = n_attain/max(n_attain), 
         .by = end_year) %>% 
  filter(thresh_pctDays_both < Inf) %>%
  mutate(end_year = as.factor(end_year)) %>% 
  ggplot(aes(y = pct_attain, 
             x = thresh_pctDays_both,
             group = end_year, 
             color = end_year)) + 
  geom_step() + 
  scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 9)[c(1, 2, 4, 7, 9)]), 
                     aesthetics = c("color", "fill")) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  scale_x_continuous(labels = scales::label_percent()) + 
  xlab("% of days struck") +
  ylab("% of stations in attainment") + 
  theme_classic() + 
  theme(legend.position = "none")

plot_smokePM_threshd <- station_year_thresh %>% 
  filter(end_year == check_year & check_year >= 2020) %>% 
  # arrange(end_year, thresh_pctDays_both) %>% 
  # select(end_year, thresh_pctDays_both) %>% 
  arrange(end_year, desc(thresh_smokePM_both)) %>% 
  select(end_year, thresh_smokePM_both) %>% 
  mutate(n_attain = 1:n(), 
         .by = end_year) %>% 
  filter(n_attain == max(n_attain),
         .by = c(end_year, thresh_smokePM_both)) %>% 
  # .by = c(end_year, thresh_pctDays_both)) %>%
  mutate(pct_attain = n_attain/max(n_attain), 
         .by = end_year) %>% 
  # filter(thresh_pctDays_both < Inf) %>%
  filter(thresh_smokePM_both > -Inf) %>% 
  # filter(end_year == 2023) %>% pull(pct_attain) %>% range
  # filter(end_year == 2023 & thresh_smokePM_both < 35) %>% 
  mutate(end_year = as.factor(end_year)) %>%
  ggplot(aes(y = pct_attain, 
             # x = thresh_pctDays_both, 
             x = thresh_smokePM_both,
             group = end_year, 
             color = end_year)) + 
  geom_step() + 
  scale_color_manual(name = "year", 
                     values = rev(MetBrewer::met.brewer("Hiroshige", 9)[c(1, 2, 4, 7, 9)]), 
                     aesthetics = c("color", "fill")) + 
  # xlim(0, 0.1) + 
  # xlab("% of days struck") + 
  scale_x_reverse() + 
  xlab(expression(paste("strike days with smoke ", PM[2.5], " > X ", mu, "g/", m^3))) + 
  ylab("% of stations in attainment") + 
  theme_classic() + 
  theme(legend.position = "inside", 
        legend.position.inside = c(0.2, 0.7))

plot_grid(
  ggdraw(plot_pctDays_threshd + 
           annotate("rect", xmin = 0 - 0.0025, xmax = 0.05 + 0.0025, ymin = 0.715 - 0.00825, ymax = 0.88 + 0.00825, fill = NA, 
                    color = "black")) +
    draw_plot(plot_pctDays_threshd +
                scale_y_continuous(labels = scales::label_percent(), 
                                   limits = c(0.715, 0.88)) + 
                scale_x_continuous(labels = scales::label_percent(), 
                                   limits = c(0, 0.05)) + 
                # xlim(0, 0.05) + #ylim(0.725, 0.88)  +
                theme(legend.position = "none",
                      plot.background = element_blank(),
                      panel.border = element_rect(colour = "black", fill=NA, size=0.75),
                      axis.title = element_blank()),
              0.4, 0.13, 0.55, 0.68),
  plot_smokePM_threshd + scale_y_continuous(labels = scales::label_percent()) , 
  nrow = 2) %>% 
  ggsave(filename = file.path(path_figures, "strike_days_cdf.png"), 
         width = 4.5, height = 6)

# attainment SI fig ---- 
station_year_thresh %>% 
  filter(thresh_pctDays_both < Inf & thresh_pctDays_both > 0) %>%
  mutate(smoke_mean = all_mean - nonsmoke_mean, 
         avg_smokeday_level = (smoke_mean/pct_smokeDay), 
         pct_smokedays_struck = thresh_pctDays_both/pct_smokeDay) %>% 
  pivot_longer(c(pct_smokeDay, nonsmoke_mean, avg_smokeday_level, pct_smokedays_struck)) %>% 
  mutate(value = ifelse(grepl("^pct", name), value*100, value), 
         name = case_when(name == "avg_smokeday_level" ~ "Average smoke concentration\n on smoke days", 
                          name == "nonsmoke_mean" ~ "Annual average non-smoke", 
                          name == "pct_smokeDay" ~ "% of days with smoke", 
                          name == "pct_smokedays_struck" ~ "% of possible smoke days struck",
                          T ~ name)) %>% 
  {ggplot(data = ., 
          aes(y = pmin(thresh_pctDays_both, 0.15),
              color = pmin(smoke_mean, 8),
              x = value)) + 
      geom_point(alpha = 0.6) + 
      # scale_color_viridis_c(option = "inferno") + 
      scale_color_gradientn(colors = viridis::inferno(100, begin = 0, end = 0.9, direction = 1),
                            name = expression(atop("Annual average",
                                                   paste("smoke ", PM[2.5], " (", mu, "g/", m^3, ")"))),
                            aesthetics = c("fill", "color"), 
                            values = scales::rescale(sinh(seq(0, 3, length.out = 100))),
                            breaks = seq(0, 8, by = 2),
                            labels = c(seq(0, 6, by = 2), ">8")) +
      scale_y_continuous(name = "% of days to strike", 
                         limits = c(0, 0.15), 
                         breaks = seq(0, 0.15, by = 0.05), 
                         labels = c("0%", "5%", "10%", ">15%")) + 
      facet_wrap(~name, scales = "free_x", 
                 strip.position = "bottom") + 
      theme_classic() + 
      theme(strip.background = element_blank(), 
            strip.text = element_text(size = 11),
            axis.title.x = element_blank(),
            strip.placement = "outside")} %>% 
  ggsave(filename = file.path(path_figures, "figS8_strike_explain.png"), 
         width = 9*0.8, height = 8*0.8)
