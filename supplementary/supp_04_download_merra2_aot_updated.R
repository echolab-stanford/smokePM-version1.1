library(lubridate)
library(rvest)
library(R.utils)
library(tools)
library(readr)
library(httr)
library(RSelenium)
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_paths.R")

#-#-----------------------------------------------------------------------------
# Written by: Mariana Martins, August 2024
# Automates the steps taken to downloads daily MERRA-2 AOT by day.
# The extraction of the files follows the script written by Jessica Li, 
# based on https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20Access%20GES%20DISC%20Data%20Using%20wget%20and%20curl.
#-----------------------------------------------------------------------
# R Selenium instructions
#------------------------------------------------------------------------
# 1. Install Mozilla Firefox browser (https://www.mozilla.org/en-US/firefox/new/)
# 2. Install the RSelenium package by running install.packages("RSelenium")
# 3. Go to https://selenium-release.storage.googleapis.com/index.html. Download 
#    selenium-server-standalone-4.0.0-alpha-2.jar (or whatever is the latest 
#    ‘selenium-server-standalone’ file)
# 4. Go to https://github.com/mozilla/geckodriver. Download the latest Mozilla 
#    geckodriver release, and place in same directory as the jar file
# 
# References:
#    https://www.r-bloggers.com/2021/05/r-selenium/
#    https://docs.ropensci.org/RSelenium/articles/basics.html
# 
# Known issues:
# 1. If running on Mac, you may receive the message that "geckodriver" can't be 
#    opened because Apple cannot check it for malicious software. To resolve 
#    this, open System Preferences > Security & Privacy immediately after trying 
#    to run Gecko Driver and click "Allow Anyway"
#    Reference: https://stackoverflow.com/questions/65972730/geckodriver-selenium-java-not-working-on-macos-big-sur
# ------------------------------------------------------------------------------
# Set dates to download - same as supp_01
start_date = "2024-01-01" # "2005-08-05"
end_date = "2024-06-30" # format needs to have hifens to paste on the search box

# Set whether to overwrite preexisting files or not
overwrite = T
#-#-----------------------------------------------------------------------------
#using RSelenium to dowload the list of files to be extracted with wget
rD = rsDriver(browser="firefox", port = 4444L)

remDr = rD[["client"]]

remDr$maxWindowSize()
remDr$navigate(url = "https://disc.gsfc.nasa.gov/datasets/M2T1NXAER_5.12.4/summary")
#click the corner to log in
remDr$findElement(using = 'xpath', value = "/html/body/div[3]/header/div/div/div[3]/ng-include/span/a/span/b")$clickElement()
#insert Login
remDr$findElement(using = 'name', value = "username")$sendKeysToElement(list("mariana.martins")) #individual login
#insert password
remDr$findElement(using = 'name', value = "password")$sendKeysToElement(list("cwz-YFP@cuy1uvg1tzu")) #individual password

#click "LOG IN"
remDr$findElement(using = 'name', value = "commit")$clickElement()

#Click "subset/get Data"
remDr$findElement(using = 'id', value = "intro-subset-1")$clickElement()

#Click the dropdown to select Download method
remDr$findElement(using = 'xpath', value = "/html/body/div[1]/div/div/div/div[2]/uib-accordion/div/form/div[3]/div/div[1]/h4/a/span/span/i")$clickElement()
 
#choose "Get File Subsets using the GES DISC Subsetter" 
remDr$findElement(using = 'name', value = "httpservice")$clickElement()

#Click the Dropdown to "Refine Date Range"
remDr$findElement(using = 'xpath', value = "/html/body/div[1]/div/div/div/div[2]/uib-accordion/div/form/div[5]/div/div[1]/h4/a/span/span/i")$clickElement()


# choose the desired dates to download 
#start date
remDr$findElement(using = 'name', value = "start")$clearElement()
remDr$findElement(using = 'name', value = "start")$sendKeysToElement(list(start_date))

#end date
remDr$findElement(using = 'name', value = "end")$clearElement()
remDr$findElement(using = 'name', value = "end")$sendKeysToElement(list(end_date))


#now to "Refine Region"
#Click the dropdown
remDr$findElement(using = 'xpath', value = "/html/body/div[1]/div/div/div/div[2]/uib-accordion/div/form/div[6]/div/div[1]/h4/a/span/span/span/span/i")$clickElement()

# enter "-125.313, 24.25, -66.562, 49.75"
remDr$findElement(using = 'name', value = "inputBox")$clearElement()
remDr$findElement(using = 'name', value = "inputBox")$sendKeysToElement(list("-125.313,24.25,-66.562,49.75")) #CONUS, this shouldn't change #Check for error messages at the website regarding the coordinates.

# Now to "Select Variables", 
#Dropdown
remDr$findElement(using = 'xpath', value = "/html/body/div[1]/div/div/div/div[2]/uib-accordion/div/form/div[8]/div/div[1]/h4/a/span/span/i")$clickElement()

#choose "TOTEXTTAU = total aerosol extinction aot [550 nm]"
remDr$findElement(using = 'name', value = "TOTEXTTAU = total aerosol extinction aot [550 nm]")$clickElement()

# go to "Select Time Range" 
#Dropdown
remDr$findElement(using = 'xpath', value = "/html/body/div[1]/div/div/div/div[2]/uib-accordion/div/form/div[9]/div/div[1]/h4/a/span/span/i")$clickElement()

#> "Apply Statistic", choose "Mean"
remDr$findElement(using = 'xpath', value = "/html/body/div[1]/div/div/div/div[2]/uib-accordion/div/form/div[9]/div/div[2]/div/div[3]/div[3]/select")$clickElement()

# Click "Get Data"
remDr$findElement(using = 'id', value = "intro-subset-modal-9")$clickElement()

# Click "Download Links List"
remDr$findElement(using = 'xpath', value ="/html/body/div[1]/div/div/div/div[2]/div[1]/div/div/div[1]/div[2]/p/a[2]")$clickElement()

#close remote access
remDr$close()

#manually get the path to the downloaded file
#urls = "~/Downloads/subset_M2T1NXAER_5.12.4_20240213_225114_.txt"  

#or get the latest .txt file in your Downloads folder
# Define the path to the Downloads folder
downloads_folder <- "~/Downloads"

# Get a list of all .txt files in the Downloads folder
txt_files <- list.files(path = downloads_folder, pattern = "\\.txt$", full.names = TRUE)

# Get the latest .txt file based on the modification time
urls = txt_files[which.max(file.info(txt_files)$mtime)]

#-#-----------------------------------------------------------------------------
rm(remDr, rD)
path_merra2 = "./Local_dropbox/data/MERRA2_AOT/total_aot/"
setwd(path_merra2)

urls = read.table(urls)[,1]
urls = urls[2:length(urls)]
for (url in urls) {
  file = gsub("^LABEL=", "", grep("^LABEL", unlist(strsplit(url, "&")), value = T))
  preexisting = file.exists(file)
  
  if (overwrite | !preexisting) {
    system(sprintf('/opt/homebrew/bin/wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition "%s"', url))
    if (preexisting) {
      file.rename(paste0(file, ".1"), file)
    }
  }
}

setwd(path_github)

library(raster)
check<- raster("./Local_dropbox/data/MERRA2_AOT/total_aot/MERRA2_400.tavg1_2d_aer_Nx.20240101.SUB.nc", layer = 1)
plot(check)
