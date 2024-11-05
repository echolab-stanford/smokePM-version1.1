source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")
library(magrittr)
library(dplyr)
#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Defines smoke PM2.5 training jobs.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Edit the model version number in 16_03_train_smokePM_Sherlock.sh as well
#-#-----------------------------------------------------------------------------
# csv with jobs to submit 
expand.grid(cv_fold_num = c(1:5),
            cv_end_date = "2023-06-30",
            drop_vars = c("NONE", "aod_anom_pred", "interp")) %>%
  write.table(file.path(path_output, sprintf("version%s", model_version), "smokePM", "smokePM_training_jobs.csv"), 
              row.names = FALSE, col.names = FALSE, sep = ",")

