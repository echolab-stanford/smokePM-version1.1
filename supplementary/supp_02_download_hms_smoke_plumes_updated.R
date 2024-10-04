library(lubridate)
library(rvest)
library(R.utils)
library(tools)
library(readr)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R") #make sure is local for testing

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Downloads NOAA HMS smoke plumes by day.
# ------------------------------------------------------------------------------
#-#-----------------------------------------------------------------------------
# Set dates to download - same as supp_01
start_date = "20060101" # "20050805"
end_date = "20231231" # format(today() - days(1), "%Y%m%d")

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------

base_url = "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS"
year_months = unique(substr(seq.Date(ymd(start_date), ymd(end_date), by = "day"), 1, 7))

# Download as shapefiles
setwd(path_smoke)

for (year_month in year_months) {
  year = substr(year_month, 1, 4)
  month = substr(year_month, 6, 7)
  url = sprintf("%s/Smoke_Polygons/Shapefile/%s/%s/", base_url, year, month)
  files = url %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href")
  files = grep("^hms_smoke20[0-2][0-9][0-1][0-9][0-3][0-9]\\.zip$", files, value = T)
  dates = gsub("hms_smoke|\\.zip", "", files)
  files = files[which(ymd(dates) %within% interval(ymd(start_date), ymd(end_date)))]
  if (length(files) > 0) {
    for (f in files) {
      url_file = paste0(url, f)
      if (overwrite | !file.exists(gsub("\\.zip$", ".shp", f))) {
        download.file(url_file, f)
        unzip(f)
        file.remove(f)
      }
    }
  }
}

setwd(path_github)
