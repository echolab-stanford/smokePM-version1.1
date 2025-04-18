library(tidyverse)
library(magrittr)
library(cowplot)

source("scripts/setup/00_03_load_paths.R")

ed_comp <- readRDS("data/applications_comparison/coefficients-for-marissa_updatedApril2025.rds")
ed_comp %>% 
  mutate(outcome = gsub(" ED visits", "", outcome) %>% 
           factor(levels = unique(.), ordered = T)) %>%
  ggplot(aes(x = bin_plot-ifelse(data_version == "new", 0.25, -0.25), 
             y = est, ymin = lb, ymax = ub, 
             color = data_version)) + 
  geom_hline(yintercept = 0, color = "grey20", linewidth = 0.1) + 
  geom_linerange() + 
  geom_ribbon(aes(fill = data_version), color = NA, alpha = 0.3) + 
  geom_point() + 
  facet_wrap(~outcome, scales = "free", nrow = 3) + 
  xlab(expression(paste("smoke ", PM[2.5], "(", mu, "g", m^{-3}, ")"))) + ylab("change in visits per 100k") + 
  theme_classic() + 
  scale_color_manual(values = c("grey10", "#207ca0"), 
                     labels = function(x){paste0(x, " estimates")}, 
                     aesthetics = c("color", "fill")) + 
  theme(strip.background = element_blank(), 
        legend.title = element_blank(), 
        legend.position = "inside", 
        legend.background = element_blank(),
        legend.position.inside = c(0.2, 0.5)) -> fig_ed_comp

fire_comp <- rbind(readRDS("./scratch/applications_comparison/gfedDM_final_coef_wind_9region_90cone_gridmet_smokev1.rds") %>% 
                     mutate(data_version = "old"), 
                   readRDS("data/applications_comparison/gfedDM_final_coef_wind_9region_90cone_gridmet_smokev2_new.rds") %>% 
                     mutate(data_version = "new"))
fire_comp %>% 
  filter(model == "wind_all") %>% 
  separate(var, into = c("dist", "direction"), sep = "km_") %>% 
  filter(dist %in% c("within50", "above_2000"),  
         region %in% c("Northeast", "Northwest", "West", "Southeast")) %>% 
  mutate(direction = ifelse(direction != "other", paste0(direction, "wind"), direction),
         direction = factor(direction), 
         dist = case_when(dist == "above_2000" ~ ">2,000 km", 
                          dist == "within50" ~ "<50 km")) %>%
  {ggplot(., aes(y = as.numeric(direction) + ifelse(data_version == "new", 0.1, -0.1), 
                 x = 1e8*beta, xmax = 1e8*beta + 1e8*1.96*se, xmin = 1e8*beta - 1e8*1.96*se, 
                 color = data_version)) + 
      geom_vline(xintercept = 0, color = "grey20", linewidth = 0.1) + 
      geom_point() + 
      geom_linerange() + 
      facet_grid(region ~ dist, scales = "free") + 
      scale_color_manual(values = c("grey10", "#207ca0"), 
                         labels = function(x){paste0(x, " estimates")}) + 
      scale_y_continuous(breaks = 1:3, labels = levels(.$direction)) + 
      xlab(expression(paste(mu, "g smoke per 100,000 tons DM"))) + 
      theme_classic() + 
      theme(axis.title.y = element_blank(), 
            strip.background = element_blank(),
            strip.text = element_text(size = 10),
            panel.spacing = unit(10, "points"),
            legend.position = "none")} -> fig_fire_comp
plot_grid(fig_fire_comp + theme(plot.margin = unit(c(20, 20, 5, 5), "points")), 
          fig_ed_comp + theme(plot.margin = unit(c(20, 20, 5, 5), "points")), 
          hjust = 0, label_x = 0.01, nrow = 1,
          labels = c("a) Effect of fire on smoke PM2.5", "b) Effect of smoke PM2.5 on ED visits")) %>% 
  ggsave(filename = file.path(path_figures, "applications.png"), 
         width = 8, height = 6)
