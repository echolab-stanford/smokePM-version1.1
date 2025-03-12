library(tidyverse)
source("scripts/setup/00_03_load_paths.R")
# define the interpolation jobs with the interpolations used in the nested CV going through june 2023 
expand.grid(x = 1:5, y = 1:5) %>% 
  filter(x >= y) %>% 
  mutate(start_month = "200601", end_month = "202306") %>%
  # and the interpolations in the full model going through dec 2024
  rbind(data.frame(x = c(1:5, 99), 
                   y = c(1:5, 99),
                   start_month = "200601",
                   end_month = "202412")) %>% 
  write.table(file.path(path_output_sherlock, 
                        sprintf("version%s", model_version), 
                        "smokePM_interpolation", "interpolation_tuning_jobs.csv"), 
             row.names = FALSE, col.names = FALSE, sep = ",")