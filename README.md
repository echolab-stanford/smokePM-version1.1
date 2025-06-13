# smokePM-version1.1
This repo supports [Childs et al "Estimates of wildfire-derived \pmt for the contiguous US and implications for air quality regulation"]

## Use cases

Expand each drop-down to see script sequences to run by use case. For downloading raw data, some data are only downloaded once processed on a remote server and not listed here. For calculating EPA station daily smoke PM2.5, smoke days can be classified by intersection of EPA station location points either with smoke plume polygons or with 10 km grid cells intersecting smoke plume polygons.

<details>
<summary>Download EPA station daily PM2.5 and station locations</summary>
<ul><li>supp_01_download_epa_pm25.R</li>
<li>02_01_combine_epa_pm25.R</li>
<li>02_02_get_epa_station_locations.R</li></ul>
</details>

<details>
<summary>Download HMS smoke plumes</summary>
<ul><li>supp_02_download_hms_smoke_plumes.R</li></ul>
</details>

<details>
<summary>Download HMS fire points</summary>
<ul><li>supp_03_download_hms_fire_points.R</li></ul>
</details>

<details>
<summary>Download ERA5 daily atmospheric reanalysis</summary>
<ul><li>supp_05_download_era5_daily.R</li></ul>
</details>

<details>
<summary>Download MERRA-2 daily AOT</summary>
<ul><li>supp_04_download_merra2_aot.R</li></ul>
</details>

<details>
<summary>Create EPA station daily PM2.5 and smoke PM2.5 panel (using polygons)</summary>
<ul><li>supp_01_download_epa_pm25.R</li>
<li>supp_02_download_hms_smoke_plumes.R</li>
<li>02_01_combine_epa_pm25.R</li>
<li>02_02_get_epa_station_locations.R</li>
<li>03_01_combine_smoke_plumes.R</li>
<li>03_02_get_smoke_dates.R</li>
<li>04_01_calculate_station_smokePM_using_polygons.R</li></ul>
</details>

<details>
<summary>Create EPA station daily PM2.5 and smoke PM2.5 data frame (using gridded)</summary>
<ul><li>supp_01_download_epa_pm25.R</li>
<li>supp_02_download_hms_smoke_plumes.R</li>
<li>02_01_combine_epa_pm25.R</li>
<li>02_02_get_epa_station_locations.R</li>
<li>03_01_combine_smoke_plumes.R</li>
<li>03_02_get_smoke_dates.R</li>
<li>03_03_get_smoke_days_over_grid.R</li>
<li>03_04_fill_smoke_days.R</li>
<li>04_02_calculate_station_smokePM_using_gridded.R</li></ul>
</details>

<details>
<summary>Train anomalous AOD model and smoke PM2.5 model  (this step was removed after performance review) </summary>
<ul><li>supp_01_download_epa_pm25.R</li>
<li>supp_02_download_hms_smoke_plumes.R</li>
<li>supp_03_download_hms_fire_points.R</li>
<li>supp_04_download_merra2_aot.R</li>
<li>supp_05_download_era5_daily.R</li>
<li>02_01_combine_epa_pm25.R</li>
<li>02_02_get_epa_station_locations.R</li>
<li>03_01_combine_smoke_plumes.R</li>
<li>03_02_get_smoke_dates.R</li>
<li>03_03_get_smoke_days_over_grid.R</li>
<li>03_04_fill_smoke_days.R</li>
<li>04_02_calculate_station_smokePM_using_gridded.R</li>
<li>05_01_ee_extract_1km_aod_training.R</li>
<li>06_01_combine_fire_points.R</li>
<li>06_02_get_fire_clusters.R</li>
<li>06_03_get_fire_dates.R</li>
<li>06_04_get_distance_to_fire_cluster_over_grid.R</li>
<li>06_05_fill_distance_to_fire_cluster.R</li>
<li>07_01_get_aot_over_grid.R</li>
<li>07_02_calculate_aot_anom.R</li>
<li>08_01_ee_extract_aod_pct_missing.R</li>
<li>09_01_get_era5_over_grid.R</li>
<li>15_01_build_anomAOD_training.R</li>
<li>15_02_train_anomAOD_Sherlock.R</li>
<li>15_03_predict_anomAOD_Sherlock.R</li>
<li>17_02_build_smokePM_training.R</li>
<li>17_03_train_smokePM_Sherlock.R</li></ul>
</details>

<details>
<summary>Interpolate station smoke PM 2.5 and train smoke PM2.5 model (this step was added after performance review) </summary>
<ul><li>supp_01_download_epa_pm25.R</li>
<li>supp_02_download_hms_smoke_plumes.R</li>
<li>supp_03_download_hms_fire_points.R</li>
<li>supp_04_download_merra2_aot.R</li>
<li>supp_05_download_era5_daily.R</li>
<li>02_01_combine_epa_pm25.R</li>
<li>02_02_get_epa_station_locations.R</li>
<li>03_01_combine_smoke_plumes.R</li>
<li>03_02_get_smoke_dates.R</li>
<li>03_03_get_smoke_days_over_grid.R</li>
<li>03_04_fill_smoke_days.R</li>
<li>04_02_calculate_station_smokePM_using_gridded.R</li>
<li>16_00_define_interpolation_tuning_jobs.R</li>
<li>16_01_tune_smokePM_interpolation.R</li>
<li>16_01_tune_smokePM_interpolation.sh</li>
<li>16_02_interpolate_smokePM_to_grid.R</li>
<li>16_02_interpolate_smokePM_to_grid.sh</li>
<li>17_02_build_smokePM_training.R</li>
<li>17_03_train_smokePM_Sherlock.R</li></ul>
</details>

<details>
<summary>Predict smoke PM2.5 using trained models (with interpolation) </summary>
<ul><li>supp_02_download_hms_smoke_plumes.R</li>
<li>supp_03_download_hms_fire_points.R</li>
<li>supp_04_download_merra2_aot.R</li>
<li>supp_05_download_era5_daily.R</li>
<li>03_01_combine_smoke_plumes.R</li>
<li>03_02_get_smoke_dates.R</li>
<li>03_03_get_smoke_days_over_grid.R</li>
<li>03_04_fill_smoke_days.R</li>
<li>06_01_combine_fire_points.R</li>
<li>06_02_get_fire_clusters.R</li>
<li>06_03_get_fire_dates.R</li>
<li>06_04_get_distance_to_fire_cluster_over_grid.R</li>
<li>06_05_fill_distance_to_fire_cluster.R</li>
<li>07_01_get_aot_over_grid.R</li>
<li>07_02_calculate_aot_anom.R</li>
<li>08_01_ee_extract_aod_pct_missing.R</li>
<li>09_01_get_era5_over_grid.R</li>
<li>16_02_interpolate_smokePM_to_grid.R</li>
<li>16_02_interpolate_smokePM_to_grid.sh</li>
<li>17_04_predict_smokePM_Sherlock.R</li>
<li>18_02_combine_smokePM_predictions.R</li>
<li>18_03_aggregate_gridded_predictions_to_county.R</li>
<li>18_04_aggregate_gridded_predictions_to_zip.R</li>
<li>18_05_aggregate_gridded_predictions_to_tract.R</li>
<li>17_08_finalize_smokePM_predictions.R</li></ul>
</details>



## Data

As noted below, some datasets require setting up to access. Detailed instructions for downloading data are provided within each script as relevant.

* Environmental Protection Agency (EPA) PM2.5 data are available at the monitor-day level across the US (50 states, District of Columbia, and Puerto Rico) from January 1, 1999 to present (typically yesterday). Data are downloaded via the EPA AQS API (https://aqs.epa.gov/aqsweb/documents/data_api.html)  using `epair` and airnow (https://www.airnow.gov/) using `AirNow`.

  Data is initially provided as [AirNow](https://www.airnow.gov/) measurements and are later replaced by [Air Quality System](https://www.epa.gov/aqs) (AQS) upon validation, which may possibly take six months or more. So more recent data rely on AirNow measurements

  Negative PM2.5 values typically occur when the atmosphere is very clean and monitor measurements contain noise, and EPA considers negative measurements [valid](https://www.epa.gov/sites/default/files/2016-10/documents/pm2.5_continuous_monitoring.pdf) when reasonably small based on monitor specifications.

* National Oceanic and Atmospheric Administration (NOAA)/National Environmental Satellite, Data, and Information Service (NESDIS) Hazard Mapping System (HMS) provide smoke and fire data over North America. Smoke plumes are available as daily shapefiles from August 5, 2005 to present (typically yesterday), and fire points are available as daily shapefiles and text files from April 1, 2003 to present (typically yesterday). Data are scraped from the [HMS server](https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/) using `rvest`.

  Satellite observations and automated fire detections within a geographic domain extending from approximately 14.6&deg;N to 72&deg;N and from 50&deg;W to 170&deg;W are validated by expert image analysts. Smoke data typically begin around noon local time, and fire data typically begin at 6 UTC (9 UTC) for the east (west) coast. Smoke plume start and end times are determined from the satellite image sequence used to outline the smoke polygon. Smoke plume density is qualitatively described as light, medium, or heavy. Smoke plume delineation may not be possible when obscured by clouds or snow-covered ground. Further information can be found from [HMS Fire and Smoke Product Information](https://www.ospo.noaa.gov/Products/land/hms.html#about), [HMS Fire and Smoke Product Current Analysis Maps](https://www.ospo.noaa.gov/Products/land/hms.html#maps), and [Brey et al 2018](https://doi.org/10.5194/acp-18-1745-2018).

* Moderate Resolution Imaging Spectroradiometer (MODIS) Terra & Aqua Multi-Angle Implementation of Atmospheric Correction ([MAIAC](https://modis-land.gsfc.nasa.gov/MAIAC.html)) Land [Version 6](https://lpdaac.usgs.gov/documents/110/MCD19_User_Guide_V6.pdf) aerosol optical depth (AOD) data are available daily at 1 km resolution globally from February 26, 2000 to February 16, 2023. Data are downloaded from Google Earth Engine (ImageCollection `MODIS/006/MCD19A2_GRANULES`) using `rgee`.

  :exclamation: Version 6 has been superseded by [Version 6.1](https://modis-land.gsfc.nasa.gov/pdf/MCD19_UserGuide_C61_V3.1.pdf) (ImageCollection `MODIS/061/MCD19A2_GRANULES`).

* Modern-Era Retrospective Analysis for Research and Applications, version 2 (MERRA-2) aerosol optical thickness (AOT) [data](https://disc.gsfc.nasa.gov/datasets/M2T1NXAER_5.12.4/summary) are available daily at 5/8&deg; (lon) by 1/2&deg; (lat) resolution globally from January 1, 1980 to recent (typically one to two months ago). Data are downloaded from the National Aeronautics and Space Administration (NASA) Goddard Earth Sciences (GES) Data and Information Services Center (DISC) data archive after EarthData [registration](https://wiki.earthdata.nasa.gov/display/EL/How+To+Register+For+an+EarthData+Login+Profile), application [authorization](https://disc.gsfc.nasa.gov/earthdata-login), and wget [setup](https://disc.gsfc.nasa.gov/information/howto?title=How%20to%20Access%20GES%20DISC%20Data%20Using%20wget%20and%20curl).

* European Centre for Medium-Range Weather Forecasts (ECMWF) Reanalysis 5th Generation (ERA5) [Single Levels](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview) and [Land](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview) meteorological data are available daily at 0.25&deg; resolution and 0.1&deg; resolution, respectively, globally from January 1, 1950 to recent (typically one to two months ago). Data are [downloaded](https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5) from the Climate Data Store (CDS) via the [Daily statistics calculated from ERA5 data application](https://cds.climate.copernicus.eu/cdsapp#!/software/app-c3s-daily-era5-statistics?tab=app) using the CDS API after CDS [registration](https://cds.climate.copernicus.eu/user/register), data license [acceptance](https://confluence.ecmwf.int/display/CKB/How+to+download+ERA5#:~:text=accept%20the%20data%20licence%20in%20the%20Terms%20of%20use%20section%20(in%20case%20you%20had%20not%20yet%20accepted%20it).%20You%20will%20only%20see%20this%20section%20after%20you%20have%20logged%20in.) per dataset, and CDS API [installation](https://cds.climate.copernicus.eu/api-how-to).

* United States Geological Survey (USGS) National Elevation Dataset ([NED](https://www.usgs.gov/publications/national-elevation-dataset)) provides elevation data at 1/3 arc-second resolution over the conterminous US and parts of Alaska, Hawaii, and some territorial islands. Data are downloaded in processed form from Google Earth Engine (ImageCollection `USGS/NED`) using `rgee`.

  :exclamation: NED has been superseded by 3D Elevation Program ([3DEP](https://www.usgs.gov/3d-elevation-program)) (ImageCollection `USGS/3DEP/10m`).

* Multi-Resolution Land Characteristics Consortium (MRLC) National Land Cover Database (NLCD), [2016 release](https://www.mrlc.gov/data/nlcd-2016-land-cover-conus) provides land cover data at 30 m resolution over the continental US, Alaska, Hawaii, and Puerto Rico. Data are downloaded in processed form from Google Earth Engine (ImageCollection `USGS/NLCD_RELEASES/2016_REL`) using `rgee`.

  :exclamation: The 2016 release has been superseded by the [2019 release](https://www.mrlc.gov/data/nlcd-2019-land-cover-conus) (ImageCollection `USGS/NLCD_RELEASES/2019_REL/NLCD`)

* US Census Bureau American Community Survey (ACS) [5-Year estimates](https://www.census.gov/data/developers/data-sets/acs-5year.2021.html) of socioeconomic and demographic variables are available annually down to the block group level across the US from 2009 to recent (typically one to two years ago). Data are retrieved using `tidycensus` after US Census Bureau API [registration](https://api.census.gov/data/key_signup.html).

* Bureau of Labor Statistics (BLS) Consumer Price Index (CPI) data are available for the US city average monthly from December 1977 to recent (typically end of last year). Data are downloaded from the [R-CPI-U-RS homepage](https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm).

* WorldPop population count data are available at 100 m resolution globally. Data are downloaded in processed form from Google Earth Engine (ImageCollection `WorldPop/GP/100m/pop`) using `rgee`.

* US Census Bureau TIGER/Line state, county, zip code tabulation area (ZCTA), and tract shapefiles are available annually across the US from 2007 (2012 for ZCTA) to recent (typically last year). Data are retrieved using `tigris`.


* California Department of Forestry and Fire Protection (CAL FIRE) Fire and Resource Assessment Program (FRAP) fire perimeter data are available by incident. Data are downloaded from the CAL FIRE [website](https://frap.fire.ca.gov/frap-projects/fire-perimeters/). 

* EPA Chemical Speciation Network (CSN) and Interagency Monitor of PROtected Visual Environments (IMPROVE) speciated PM2.5 data

* CAGDP

## Computational environment

The following specifications are listed for the record and are not all necessary for reproduction.

* Local machine
  ```
  Model: MacBook Pro 16-inch 2019
  Processor: 2.6 GHz 6-Core Intel Core i7
  Memory: 16 GB 2667 MHz DDR4
  System Version: macOS Ventura 13.3.1 (a)
  ```

* [Homebrew](https://brew.sh/) version 4.0.26 (2023-04-28)

* Geospatial libraries
  ```
  GEOS 3.11.2
  GDAL 3.6.4
  PROJ 9.2.0
  ```

* [R](https://cran.r-project.org/bin/windows/base/) version 4.3.0 (2023-04-21)
  ```
  Platform: x86_64-apple-darwin20 (64-bit)
  Matrix products: default
  BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
  LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
  ```

* [RStudio](https://posit.co/download/rstudio-desktop/) version 2023.03.1+446 (2023.03.1+446)

* R packages are specified in `scripts/setup/00_00_install_packages.R`

* [Python](https://www.python.org/downloads/) version 3.9.12

* Python packages are installed as needed within relevant scripts

* [Conda](https://conda.io/projects/conda/en/latest/user-guide/install/index.html) version 4.12.0

* [Java Developer Kit](https://www.oracle.com/java/technologies/downloads/#jdk20-mac)

* [Selenium](https://selenium-release.storage.googleapis.com/index.html) selenium-server-standalone-4.0.0-alpha-2.jar (see `scripts/supplementary/supp_01_download_epa_pm25.R` for detailed instructions)

* [Mozilla Firefox](https://www.mozilla.org/en-US/firefox/new/)

* [geckodriver](https://github.com/mozilla/geckodriver) (see `scripts/supplementary/supp_01_download_epa_pm25.R` for detailed instructions)

* [Sherlock](https://www.sherlock.stanford.edu/) access for high-performance computing

* [Oak](https://uit.stanford.edu/service/oak-storage/about) access for high-performance computing storage

* [Google Earth Engine](https://earthengine.google.com/) (GEE) requires [registration](https://code.earthengine.google.com/register) to gain [access](https://developers.google.com/earth-engine/guides/access)


By default, code is run on local machine. Scripts prefixed `[##]_[##]_ee_` are run on local machine but perform data processing on Google Earth Engine via `rgee`. Scripts suffixed `_Sherlock.R` are run on Sherlock and accompanied by a job script of the same name.

In each script, lines requiring user attention are enclosed in the following form:
```
#-#-----------------------------------------------------------------------------
# comment with instructions
code that can be edited
#-#-----------------------------------------------------------------------------
```
Search for `#-#-` to quickly navigate to these sections.

Where temporal coverage must be set by the user, specify `start_` and `end_` dates `"[YYYYmmdd]"`, months `"[YYYY-mm]"`, or years `"[YYYY]"` as instructed in the comments. `_date`, `_month`, and `_year` refer to temporal coverage of script outputs, whereas `_input` refers to temporal coverage of script inputs. In other words, `_input` defines and can be used to restrict the temporal range throughout which data are considered available. This distinction becomes nontrivial when lagging or leading time series and when producing an output necessarily involves the entire temporal range.

`model_version` refers to the version of the smokePM model. For each model version `P.m`, `P` increments per publication, and `m` increments per smokePM model object released.

## Frequently asked questions ##

* What is the difference between `smokePM` and `filled_smokePM` in the EPA station-day smoke PM2.5 panel data frame?

  On dates where smoke data are missing, we either can treat those as non-smoke day (`smokePM`) or fill based on 1-2 days before and after (`filled_smokePM`).# SmokePM_real_time_interpolations
