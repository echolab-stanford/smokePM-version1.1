#-#-----------------------------------------------------------------------------
# Only packages marked "# Sherlock" need to be installed on Sherlock

# If installing sf on Sherlock, you will first need to load the appropriate modules:
# $ ml physics gdal/2.2.1 udunits/2.2.26 proj/4.9.3 geos/3.6.2
# $ ml R/4.2.0
# $ R
# > install.packages("devtools")
# > require(devtools)
# > install_version("sf", version = "0.9-8")

# Prioritize installing the same versions for spatial packages (esp. sf) and 
# ML/modeling packages

# If installation of sf version 0.9-8 is not possible, you may run into breaking 
# errors when running 03_01_combine_smoke_plumes.R, in which case you may need 
# to debug and implement shape-related adjustments plume by plume
#-#-----------------------------------------------------------------------------

install.packages("renv") # 0.17.3 # Sherlock
renv::install("remotes@2.4.2.1") 
renv::install("devtools@2.4.5")

# Attached base packages:
# grid
# splines
# parallel
# tools
# stats
# graphics
# grDevices
# datasets
# utils
# methods
# base

# Other attached packages:
renv::install("RSelenium@1.7.9")
renv::install("forcats@1.0.0")
renv::install("tidyverse@2.0.0") # Sherlock
renv::install("rsvg@2.4.0")
renv::install("DiagrammeRsvg@0.1")
renv::install("DiagrammeR@1.0.10")
renv::install("RColorBrewer@1.1-3")
renv::install("scales@1.2.1")
renv::install("cowplot@1.1.1")
renv::install("gridExtra@2.3")
renv::install("ggpubr@0.6.0")
renv::install("ggplot2@3.4.2")
renv::install("rBayesianOptimization@1.2.0") # Sherlock
renv::install("xgboost@1.7.5.1") # Sherlock
renv::install("fixest@0.11.1")
renv::install("gtable@0.3.3")
renv::install("purrr@1.0.1") # Sherlock
renv::install("tidyr@1.3.0") # Sherlock
renv::install("dplyr@1.1.2") # Sherlock
renv::install("plyr@1.8.8")
renv::install("tibble@3.2.1")
renv::install("magrittr@2.0.3") # Sherlock
renv::install("exactextractr@0.9.1") #error due to geos-config, worked after downloading geos on homebrew and using Sys.setenv(PATH = paste("/opt/homebrew/Cellar/geos/3.12.1/bin", Sys.getenv("PATH"), sep = ":"))
renv::install("SpatialPosition@2.1.1")
renv::install("rgeos@0.6-3")
renv::install("rgdal@1.6-6") #error, fixed gdal-config with Sys.setenv(PATH = paste("/opt/homebrew/bin", Sys.getenv("PATH"), sep = ":")), 
#now error with pkg-config proj not available; set PKG_CONFIG_PATH to the directory containing proj.pc, fized with export PKG_CONFIG_PATH=/opt/homebrew/Cellar/proj/9.3.1/lib/pkgconfig:$PKG_CONFIG_PATH on terminal
renv::install("raster@3.6-20")
renv::install("ncdf4@1.21")
renv::install("sf") #You'll have to switch between versions for specific scripts
renv::install("sf@0.9-8")
renv::install("sp@1.6-1")
renv::install("FNN@1.1.3.2")
renv::install("doParallel@1.0.17") # Sherlock
renv::install("iterators@1.0.14")
renv::install("foreach@1.5.2") # Sherlock
renv::install("lubridate@1.9.2") # Sherlock
renv::install("stringr@1.5.0") # Sherlock
renv::install("retry@0.1.0")
renv::install("readxl@1.4.2")
renv::install("readr@2.1.4")
renv::install("tidycensus@1.4")
renv::install("tigris@2.0.3") # Sherlock
renv::install("ff@4.0.9")
renv::install("bit@4.0.5")
renv::install("rlang@1.1.1") # Sherlock
renv::install("rvest@1.0.3")
renv::install("XML@3.99-0.14")
renv::install("R.utils@2.12.2")
renv::install("R.oo@1.25.0")
renv::install("R.methodsS3@1.8.2")
renv::install("RCurl@1.98-1.12")

# Loaded via a namespace (and not attached):
renv::install("later@1.3.1")
renv::install("bitops@1.0-7")
renv::install("cellranger@1.1.0")
renv::install("rpart@4.1.19")
renv::install("lifecycle@1.0.3")
renv::install("rstatix@0.7.2")
renv::install("rprojroot@2.0.3")
renv::install("MASS@7.3-60")
renv::install("processx@3.8.1")
renv::install("lattice@0.21-8")
renv::install("crosstalk@1.2.0")
renv::install("backports@1.4.1")
renv::install("rmarkdown@2.22")
renv::install("Hmisc@5.1-0")
renv::install("httpuv@1.6.11")
renv::install("sessioninfo@1.2.2")
renv::install("pkgbuild@1.4.2")
renv::install("reticulate@1.28")
renv::install("minqa@1.2.5") #error - fixed with gfortran-12.2 universal from https://mac.r-project.org/tools/
renv::install("DBI@1.1.3")
renv::install("abind@1.4-5")
renv::install("pkgload@1.3.2")
renv::install("nnet@7.3-18") 
renv::install("rappdirs@0.3.3")
renv::install("sandwich@3.0-2")
renv::install("dreamerr@1.2.3")
renv::install("gdata@2.19.0")
renv::install("terra@1.7-29")
renv::install("units@0.8-2")
renv::install("codetools@0.2-19") 
renv::install("xml2@1.3.4")
renv::install("shape@1.4.6")
renv::install("tidyselect@1.2.0")
renv::install("lme4@1.1-33") #gigantic error - installed most recent version 
#install.packages("lme4")
renv::install("base64enc@0.1-3")
renv::install("matrixStats@1.0.0")
renv::install("jsonlite@1.8.4")
renv::install("mitml@0.4-5")
renv::install("e1071@1.7-13")
renv::install("ellipsis@0.3.2")
renv::install("Formula@1.2-5")
renv::install("survival@3.5-5")
renv::install("ggnewscale@0.4.9")
renv::install("wdman@0.2.6")
renv::install("Rcpp@1.0.10")
renv::install("glue@1.6.2")
renv::install("pan@1.8")
renv::install("xfun@0.39")
renv::install("usethis@2.2.2")
renv::install("ggthemes@4.2.4")
renv::install("withr@2.5.0")
renv::install("numDeriv@2016.8-1.1")
renv::install("fastmap@1.1.1")
renv::install("boot@1.3-28.1")
renv::install("fansi@1.0.4")
renv::install("callr@3.7.3")
renv::install("caTools@1.18.2")
renv::install("digest@0.6.31")
renv::install("timechange@0.2.0")
renv::install("R6@2.5.1")
renv::install("mime@0.12")
renv::install("mice@3.16.0")
renv::install("colorspace@2.1-0")
renv::install("gtools@3.9.4")
renv::install("weights@1.0.4")
renv::install("utf8@1.2.3")
renv::install("generics@0.1.3")
renv::install("renv@0.17.3")
renv::install("data.table@1.14.8")
renv::install("class@7.3-22")
renv::install("prettyunits@1.1.1")
renv::install("httr@1.4.6")
renv::install("htmlwidgets@1.6.2")
renv::install("pkgconfig@2.0.3")
renv::install("htmltools@0.5.5")
renv::install("carData@3.0-5")
renv::install("profvis@0.3.8")
renv::install("png@0.1-8") 
renv::install("binman@0.1.3")
renv::install("knitr@1.43")
renv::install("rstudioapi@0.14")
renv::install("tzdb@0.4.0")
renv::install("uuid@1.1-0")
renv::install("nloptr@2.0.3")
renv::install("checkmate@2.2.0")
renv::install("visNetwork@2.1.2")
renv::install("nlme@3.1-162")
renv::install("curl@5.0.0")
renv::install("proxy@0.4-27")
renv::install("zoo@1.8-12")
renv::install("cachem@1.0.8")
renv::install("KernSmooth@2.23-21")
renv::install("miniUI@0.1.1.1")
renv::install("foreign@0.8-84")
renv::install("desc@1.4.2")
renv::install("pillar@1.9.0")
renv::install("vctrs@0.6.2")
renv::install("urlchecker@1.0.1")
renv::install("promises@1.2.0.1")
renv::install("jomo@2.7-6")
renv::install("car@3.1-2")
renv::install("cluster@2.1.4")
renv::install("xtable@1.8-4")
renv::install("htmlTable@2.4.1")
renv::install("evaluate@0.21")
renv::install("isoband@0.2.7")
renv::install("cli@3.6.1")
renv::install("compiler@4.3.0") #There are no packages to install.
renv::install("crayon@1.5.2")
renv::install("ggsignif@0.6.4")
renv::install("classInt@0.4-9")
renv::install("ps@1.7.5")
renv::install("fs@1.6.2")
renv::install("stringi@1.7.12")
renv::install("assertthat@0.2.1")
renv::install("munsell@0.5.0")
renv::install("leaflet@2.1.2")
renv::install("glmnet@4.1-7")
renv::install("V8@4.3.0")
renv::install("Matrix@1.5-4.1")
renv::install("hms@1.1.3")
renv::install("shiny@1.7.4.1")
renv::install("import@1.3.0")
renv::install("semver@0.2.0")
renv::install("broom@1.0.4")
renv::install("memoise@2.0.1")
renv::install('dotenv')

# Further tips on rgee setup: https://r-spatial.github.io/rgee/
renv::install("rgee@1.1.5")
rgee::ee_install()
#-#-----------------------------------------------------------------------------
# Choose yes to storing the environment variables EARTHENGINE_PYTHON and EARTHENGINE_ENV in your .Renviron file
# Choose 1 to restart your R session
#-#-----------------------------------------------------------------------------
reticulate::py_install('earthengine-api==0.1.323', envname='rgee')
rgee::ee_check()
#-#-----------------------------------------------------------------------------
# Install Google Cloud SDK and Google Drive tools. If using Homebrew, run the 
# following in Terminal:
# brew install --cask google-cloud-sdk
# brew install --cask google-drive
#-#-----------------------------------------------------------------------------
renv::install("googledrive@2.1.1")

# Custom packages
remotes::install_github("marcosci/layer") # version 0.0.2

remotes::install_github("zeehio/facetscales@archived")
# How to create your Github Personal Access Token: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
library(dotenv)
load_dot_env() #Error - create .env file
github_auth = Sys.getenv('CENSUS_TOOLS')
remotes::install_github("echolab-stanford/census.tools") # , auth_token = "xxxxxxx" - insert github token inside the parethesis (Classic)
