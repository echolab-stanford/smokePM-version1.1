library(tidyverse)
source("scripts/setup/00_03_load_paths.R")
expand.grid(x = 1:5, y = 1:5) %>% 
  filter(x >= y) %>% 
  rbind(c(x = 99, y = 99)) %>%
  write.table(file.path(path_output_sherlock, 
                        sprintf("version%s", model_version), 
                        "smokePM_interpolation", "interpolation_tuning_jobs.csv"), 
             row.names = FALSE, col.names = FALSE, sep = ",")
  