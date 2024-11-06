source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")
source("scripts/setup/00_04_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Saves smokePM aggregates in final folder.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set model version
model_version = "1.1"

# Set date range to finalize
start_date = "20060101" # "20060101"
end_date = "20230715" # format(today() - days(1), "%Y%m%d")
#-#-----------------------------------------------------------------------------

# Load predictions
preds = readRDS(file.path(path_output, sprintf("version%s", model_version), "smokePM", "predictions", "combined", sprintf("smokePM_predictions_%s-%s.rds", start_date, end_date)))

# Save
saveRDS(preds, file.path(path_final, sprintf("version%s", model_version), "10km_grid", sprintf("smokePM2pt5_predictions_daily_10km_%s-%s.rds", start_date, end_date)))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, sprintf("version%s", model_version), "10km_grid", sprintf("smokePM2pt5_predictions_daily_10km_%s-%s.csv", start_date, end_date)), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to county level
preds = readRDS(file.path(path_output, sprintf("version%s", model_version), "smokePM", "predictions", "combined", sprintf("county_smokePM_predictions_%s-%s.rds", start_date, end_date)))

# Save
saveRDS(preds, file.path(path_final, sprintf("version%s", model_version), "county", sprintf("smokePM2pt5_predictions_daily_county_%s-%s.rds", start_date, end_date)))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, sprintf("version%s", model_version), "county", sprintf("smokePM2pt5_predictions_daily_county_%s-%s.csv", start_date, end_date)), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to zip level
preds = readRDS(file.path(path_output, sprintf("version%s", model_version), "smokePM", "predictions", "combined", sprintf("zcta_smokePM_predictions_%s-%s.rds", start_date, end_date)))

# Save
saveRDS(preds, file.path(path_final, sprintf("version%s", model_version), "zcta", sprintf("smokePM2pt5_predictions_daily_zcta_%s-%s.rds", start_date, end_date)))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, sprintf("version%s", model_version), "zcta", sprintf("smokePM2pt5_predictions_daily_zcta_%s-%s.csv", start_date, end_date)), row.names = F)

# ------------------------------------------------------------------------------
# Load predictions aggregated to census tract level
preds = readRDS(file.path(path_output, sprintf("version%s", model_version), "smokePM", "predictions", "combined", sprintf("tract_smokePM_predictions_%s-%s.rds", start_date, end_date)))

# Save
saveRDS(preds, file.path(path_final, sprintf("version%s", model_version), "tract", sprintf("smokePM2pt5_predictions_daily_tract_%s-%s.rds", start_date, end_date)))

# Convert date to character
preds = preds %>% mutate(date = format(date, "%Y%m%d"))

# Save
write.csv(preds, file.path(path_final, sprintf("version%s", model_version), "tract", sprintf("smokePM2pt5_predictions_daily_tract_%s-%s.csv", start_date, end_date)), row.names = F)
