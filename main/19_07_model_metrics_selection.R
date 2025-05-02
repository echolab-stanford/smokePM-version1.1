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

# contemporaneous predictions
list.files(file.path(path_output, "version1.1", "smokePM", "model"), 
           pattern = "smokePM_pred_", full.names = T) %>% 
  purrr::map(~readRDS(.x) %>% mutate(file = basename(.x))) %>% 
  list_rbind() -> all_cv_preds
all_cv_preds %<>% mutate(out_fold = str_split_i(file, "fold", 2) %>% str_split_i("_", 1) %>% as.numeric, 
                        drop_vars =  gsub(".*drop-|\\.rds", "", file), 
                        .keep = "unused") 

# "future" predictions (i.e., july 2023 - dec 2024 for models trained through june 2023)
all_cv_preds %<>% mutate(type = "current") %>% 
  rbind(file.path(path_output, "version1.1", "smokePM", "predictions_future", "smokePM_predictions_20230701_20241231.rds") %>% 
          readRDS %>%
          distinct %>%
          mutate(out_fold = as.numeric(out_fold), 
                 drop_vars = "aod_anom_pred", 
                 type = "future") %>% 
          # the future predictions are by grid id, so join on the epa station ids from the training data
          left_join(training_data %>% select(grid_id_10km, id, fold) %>% unique,
                    relationship = "many-to-many") %>% 
          select(id, date, fold, smokePM_pred, out_fold, drop_vars, type))

        # all_cv_preds %<>% 
        #   mutate(type = ifelse(grepl("2023", file), "future", "current")) %>% 
        #   mutate(out_fold = ifelse(type == "current", gsub(".*_fold|_drop.*", "", file) %>% as.numeric, out_fold),
        #          drop_vars = gsub(".*drop|\\.rds", "", file)) 
        
model_metrics <- all_cv_preds %>%
  rbind(training_data %>% 
          transmute(id, date, fold, 
                    smokePM_pred = fold99_interp %>% 
                      replace_na(0), 
                    out_fold = fold, 
                    drop_vars = "interp only") %>% 
          mutate(type = ifelse(date > as.Date("2023-06-30"), "future", "current"))) %>% 
  mutate(smokePM_pred = pmax(smokePM_pred, 0)) %>% 
  filter(!is.na(out_fold)) %>%
  left_join(training_data %>% select(id, date, smokePM)) %>% 
  mutate(test = out_fold == fold, 
         mod = paste0(type, "_", drop_vars)) %>% 
  filter(test == T, !is.na(smokePM)) %>% 
  eval_metrics(models = mod, test_tune = test, 
               obs = smokePM, pred = smokePM_pred, loc_id = id) %>%
  pivot_longer(contains("rank") | contains("value")) %>% 
  separate(name, into = c("metric", "rank_value"), sep = "_(?=rank|value)") %>% 
  pivot_wider(values_from = value, names_from = rank_value) %>%
  separate(metric, into = c("metric", "subset"), sep = "_", extra = "merge") 

model_metrics %>%
  separate(mod, into = c("time", "drop_vars"), sep = "_") %>% 
  filter(grepl("day", subset)) %>% 
  filter(metric %in% c("rmse", "r2", "wr2", "me")) %>% 
  filter(test == TRUE) %>%
  # add rows to force the y axes to 0 for r2 and within r2 
  mutate(alpha = 1) %>%
  rbind(data.frame(time = "current", 
                   drop_vars = "aod", 
                   test = TRUE, 
                   metric = c("r2", "wr2"), 
                   subset = "day", 
                   rank = NA, 
                   value = 0, 
                   alpha = 0)) %>%
  mutate(subset = recode_factor(subset, 
                                "day" = "all days", 
                                "day_sub50" = "days with\n< 50ug",
                                "day_over50" = "days with\n> 50ug", 
                                .ordered = T), 
         metric = recode_factor(metric, 
                                "r2" = "R^2", 
                                "wr2" = "within~R^2", 
                                "rmse" = "RMSE",
                                "me" = "Mean~error",
                                .ordered = TRUE)) %>%
  mutate(drop_vars = ifelse(drop_vars == "NONE", "All variables", drop_vars),
         drop_vars = ifelse(drop_vars %in% c("interp", "aod"), paste0("No ", drop_vars), drop_vars) %>% gsub("interp", "interpolations", .), 
         drop_vars = case_when(drop_vars == "interpolations only" ~ "Raw interpolations", 
                               drop_vars == "No aod" ~ "No predicted\nAOD anomaly\n(preferred model)", 
                               T ~ drop_vars)) %>% 
  
  mutate(time = ifelse(time == "future", "July 2023 - Dec 2024", "Jan 2006 - June 2023")) %>% 
  {plot_grid(ggplot(data = filter(., time == "Jan 2006 - June 2023"),  
                    aes(x = subset, y = value, color = drop_vars,
                        group = interaction(drop_vars, alpha), alpha = I(alpha))) + 
               geom_point() + 
               geom_line() + 
               scale_x_discrete(expand = expansion(mult = 0.04)) + 
               scale_color_manual(values = c("#a82203", "#208cc0",  "#637b31", "#f1af3a", "white")) +
               # ylim(0, NA) +
               facet_wrap(~metric, scales = "free", 
                          strip.position = "left",
                          labeller = label_parsed) +
               theme_classic() +
               theme(plot.margin = unit(c(15.5, 5.5, 5.5, 5.5), "points"), 
                     strip.placement = "outside",
                     strip.background = element_blank(),
                     axis.title = element_blank(),
                     panel.spacing.x = unit(1.25, "lines"),
                     legend.title = element_blank(),
                     legend.position = "right", 
                     legend.background = element_blank(),
                     legend.position.inside = c(0.25, 0.22),
                     strip.text = element_text(size = 12),
                     strip.switch.pad.wrap = unit(-0.25, "lines")), 
             ggplot(data = filter(., drop_vars == "No predicted\nAOD anomaly\n(preferred model)"),
                    aes(x = subset, y = value, color = time,
                        group = interaction(time, alpha), shape = time, linetype = time, 
                        alpha = I(alpha))) + 
               geom_point() + 
               geom_line() + 
               facet_wrap( ~metric, scales = "free", 
                           strip.position = "left",
                           labeller = label_parsed) + 
               scale_color_manual(values = c("grey50", "grey5")) +
               theme_classic() + 
               theme(plot.margin = unit(c(15.5, 5.5, 5.5, 5.5), "points"), 
                     strip.placement = "outside",
                     strip.background = element_blank(),
                     axis.title = element_blank(),
                     panel.spacing.x = unit(1.25, "lines"),
                     legend.title = element_blank(),
                     legend.position = "right", 
                     legend.background = element_blank(),
                     legend.position.inside = c(0.25, 0.22),
                     strip.text = element_text(size = 12),
                     strip.switch.pad.wrap = unit(-0.25, "lines")), 
             nrow = 2, align = "hv", axis = "bl", 
             vjust = 1.2,  hjust = -0.02, label_x = 0.01, 
             labels = c("a) CV model performance metrics for candidate models", 
                        "b) Out-of-temporal sample model performance metrics for preferred model"))} %>% 
  ggsave(filename = file.path(path_figures, "model_metrics_selection.png"), 
         width = 7*1.2, height = 7*1.2)
