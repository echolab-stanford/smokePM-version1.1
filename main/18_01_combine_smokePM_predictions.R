source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

#-------------------------------------------------------------------------------
# Written by: Marissa Childs
# Combines smoke PM2.5 predictions.
#-------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set date range to combine
start_date = "20060101" # "20060101"
end_date = "20230715" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

smokePM_pred <- list.files(file.path(path_output, sprintf("version%s", model_version), "smokePM", "predictions", "10km_smoke_days"))
smokePM_pred = smokePM_pred[ymd(gsub("^smokePM_predictions_10km_|\\.rds$", "", smokePM_pred)) %within% (ymd(start_date) %--% ymd(end_date))]
smokePM_pred = smokePM_pred %>% map_dfr(readRDS)

smokePM_pred %<>% mutate(smokePM_pred = pmax(0, smokePM_pred))

saveRDS(smokePM_pred, 
        file.path(path_output, sprintf("version%s", model_version), "smokePM", "predictions", "combined", 
                  sprintf("smokePM_predictions_%s-%s.rds", start_date, end_date)))
