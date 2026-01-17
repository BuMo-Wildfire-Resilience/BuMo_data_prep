# 17 weather_wrf_prep.r 
# WRF data prepared by Kiri Daust (BC government) Jan 2026
# using dates from 2014 - 2024 inclusive 
# April - Oct 

# note this script reads in wrf data and converts it from .nc files to usable grids. 
# 1) read in prepared files (.nc) 
# 2) format and downsample? STILL TO DO 
# 3) generate FWI metrics etc using the "cffdrs/cffdrs_r" functions - TEMPLATE FILTER AS OF AUGST 26

#WRF details 
#https://www2.mmm.ucar.edu/wrf/users/wrf_users_guide/build/html/output_variables.html

## requires: 
# - .nc files for WRF data 
# csffdr_utils.R to run 



library(terra)
library(dplyr)
library(fs)
library(sf)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')


# read in template and convert to points and coordinates
raster_template<- rast(file.path(spatialOutDir, "template_BuMo.tif"))
raster_template[raster_template ==1]<- 0

# read in weather data 
list.files(file.path(spatialDir,'weather_wrf'))

# set up years and months 
yrs <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")

# set up years and months 
#yrs <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")

################################################################################
# 1) accumulated precip - no correction for downscale
################################################################################

weather <- terra::rast(file.path(spatialDir,'weather_wrf','AccumPrecip_BuMo.nc'))
noi_full <- names(weather)
length(names(weather)) # 12814

numlist <- seq(as.Date("1989-09-01"), as.Date("2024-09-30"), by = "day")
numlist <- format(numlist, "%Y%m%d")
length(numlist)

# filter list of numbers per year 
# cycle through each yr 

all_years <- purrr::map(yrs, function(y) {
  
  print(y)

 # y <- yrs[1]
  
  # for each year filter the values from April - Oct
  moi <- paste(c( paste0(y, "04"), paste0(y, "05"),paste0(y, "06"),paste0(y, "07"),paste0(y, "08"),paste0(y, "09"), paste0(y, "10")), collapse = "|") #  , "2015*", "2016*", "2017*", "2018*", "2019*", "2020*", "2021*", "2022*", "2023*", "2024*"), collapse = "|")
  noi_tf1 <- stringr::str_starts(numlist, moi)
  
  yr_noi_full <- noi_full[which(noi_tf1 == TRUE)]
  
  # cycle through each file and projector
  yrmth = purrr::map(yr_noi_full, function(ii){
    
   # ii <- yr_noi_full[214]    #test
    
    # get names for output by matching the index on names list 
    names <- gsub("HPRECIPNC_", "", ii)
    names <- numlist[[as.numeric(names)]]
    
    # extract grid and reporject 
    ac <- weather[[ii]]
    acip <- project(ac, raster_template)
    acipi <- as.int(acip)
    
    # add names 
    names(acipi) <- names
    
    writeRaster(acipi, file.path(out_dir, paste0(names, '_acpr.tif')), overwrite=TRUE)
    
  }) # end of month cycle
  
})





#####################################################################################
#2) wind speed  - no correction for downscale
####################################################################################
#ws_1300_BuMo.nc
# note the valies here are * 10 to keep a single decimal place when converted to an integer
ws <- terra::rast(file.path(spatialDir,'weather_wrf','ws_1300_BuMo.nc'))
#length(names(ws)) #12804
#names(ws)
#tail(names(ws))
#ww <- ws[["ws_Times=20230929.875"]]


noi_full <- names(ws)
noi <- gsub("ws_Times=", "", noi_full)
noi <- gsub(".875", "", noi)

# cycle through each yr 

all_years <- purrr::map(yrs, function(y) {
  
  print(y)
  y <- yrs[10]
  noi_tf1 <- stringr::str_starts(noi, y)
  yr_noi_full <- noi_full[which(noi_tf1 == TRUE)]
  
  # subset months
  yr_noi  <- gsub("ws_Times=", "", yr_noi_full )
  yr_noi  <- gsub(".875", "", yr_noi )
  
  moi <- paste(c( paste0(y, "04"), paste0(y, "05"),paste0(y, "06"),paste0(y, "07"),paste0(y, "08"),paste0(y, "09"), paste0(y, "10")), collapse = "|") #  , "2015*", "2016*", "2017*", "2018*", "2019*", "2020*", "2021*", "2022*", "2023*", "2024*"), collapse = "|")
  
  ymoi_tf <- stringr::str_starts(yr_noi, moi)
  yrmt_noi <- yr_noi_full[which(ymoi_tf == TRUE)]

  # cycle through each file and projector
  
  yrmth = purrr::map(yrmt_noi, function(ii){
    #ii <- yrmt_noi[1]    #test
    
    # get names for output 
    names <- gsub("ws_Times=", "", ii )
    names <- gsub(".875", "", names)
    
    wsi <- ws[[ii]]
    wsip <- project(wsi, raster_template)
    wsipi <- wsip *10
    wsipi <- as.int(wsipi)
    names(wsipi) <- names

    writeRaster(wsipi, file.path(out_dir, paste0(names, '12_ws.tif')), overwrite=TRUE)

   }) # end of month cycle
  
})



####################################################################################
# Wind Direction - 1 pm - no correction for downscale
####################################################################################

#wdir_1300_BuMo
wd <-terra::rast(file.path(spatialDir,'weather_wrf','wdir_1300_BuMo.nc'))
#length(names(wd)) #12803
#tail(names(wd))
#ww <- wd[["wdir_Times=20230930.875"]]


noi_full <- names(wd)
noi <- gsub("wdir_Times=", "", noi_full)
noi <- gsub(".875", "", noi)

# cycle through each yr 
yrs <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")

all_years <- purrr::map(yrs, function(y) {
  
  print(y)
  #y <- yrs[1]
  
  noi_tf1 <- stringr::str_starts(noi, y)
  yr_noi_full <- noi_full[which(noi_tf1 == TRUE)]
  
  # subset months
  yr_noi  <- gsub("wdir_Times=", "", yr_noi_full )
  yr_noi  <- gsub(".875", "", yr_noi )
  
  moi <- paste(c( paste0(y, "04"), paste0(y, "05"),paste0(y, "06"),paste0(y, "07"),paste0(y, "08"),paste0(y, "09"),paste0(y, "10")), collapse = "|") #  , "2015*", "2016*", "2017*", "2018*", "2019*", "2020*", "2021*", "2022*", "2023*", "2024*"), collapse = "|")
  
  ymoi_tf <- stringr::str_starts(yr_noi, moi)
  yrmt_noi <- yr_noi_full[which(ymoi_tf == TRUE)]
  
  # cycle through each file and projector
  
  yrmth = purrr::map(yrmt_noi, function(ii){
    #ii <- yrmt_noi[1]    #test
    
    # get names for output 
    names <- gsub("wdir_Times=", "", ii )
    names <- gsub(".875", "", names)
    
    wdi <- wd[[ii]]
    wdip <- project(wdi, raster_template)
    wdipi <- as.int(wdip)
    names(wdipi) <- names
    
    writeRaster(wdipi, file.path(out_dir, paste0(names, '12_wd.tif')), overwrite=TRUE)
    
  }) # end of month cycle
  
})



###############################################################################
# "TQP_1300_BuMo.nc"
# How to calculate the relative humidity from temp, specific humidity and pressure 

# T2 is 2m temperature, PSFC is surface pressure, and Q2 is specific humidity.
# Need to correct temperature for altitude using : Environment canada methodology

# Elevation grid = DEM 
dem <- rast(file.path(spatialOutDir, "DEM3005_BuMo.tif"))

# read in stacked nc file
tqp <- terra::rast(file.path(spatialDir,'weather_wrf','TQP_1300_BuMo.nc'))
#names(tqp)
#tail(names(tqp))

#T2_Times=19900317.875 
#names(tqp)[13000]

###########################################################################
# 1) temperature - might not need these but they are available as needed.  
# outputs from WRF
#float T2(Time, south_north, west_east) ;
#T2:description = "TEMP at 2 M" ;
#T2:units = "K" ;


noi_full_all <- names(tqp)
noi_full <- noi_full_all[grep("T2_Times=", noi_full_all)]
noi <- gsub("T2_Times=", "", noi_full)
noi <- gsub(".875", "", noi)

# cycle through each yr 
yrs <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")

#yrs <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024")

all_years <- purrr::map(yrs, function(y) {
  
  print(y)
#  y <- yrs[1]
  
  noi_tf1 <- stringr::str_starts(noi, y)
  yr_noi_full <- noi_full[which(noi_tf1 == TRUE)]
  
  # subset months
  yr_noi  <- gsub("T2_Times=", "", yr_noi_full )
  yr_noi  <- gsub(".875", "", yr_noi )
  
  moi <- paste(c( paste0(y, "04"), paste0(y, "05"),paste0(y, "06"),paste0(y, "07"),paste0(y, "08"),paste0(y, "09"),paste0(y, "10")), collapse = "|") #  , "2015*", "2016*", "2017*", "2018*", "2019*", "2020*", "2021*", "2022*", "2023*", "2024*"), collapse = "|")
  
  ymoi_tf <- stringr::str_starts(yr_noi, moi)
  yrmt_noi <- yr_noi_full[which(ymoi_tf == TRUE)]
  
  # cycle through each file and projector
  
  yrmth = purrr::map(yrmt_noi, function(ii){
    #ii <- yrmt_noi[1]    #test
    
    # get names for output 
    names <- gsub("T2_Times=", "", ii )
    names <- gsub(".875", "", names)
    
    wdi <- tqp[[ii]]
    wdip <- project(wdi, raster_template)
    
    # convert from Kelvin to celcius 
    wdip <-  wdip -273.15
    
    # adjust for dem 
    wdip_alt <- wdip - (dem/1000)*6.5
    
    # convert to interger
    wdip_alt <-  wdip_alt *10
    wdip_alt <- as.int(wdip_alt)
    
    names(wdip_alt) <- names
    
    writeRaster(wdip_alt, file.path(out_dir, "temp2mcorrected", paste0(names, '14_temp.tif')), overwrite=TRUE)
    
  }) # end of month cycle
  
})



# Calculate relative humidity 
# requires T = temperature, Q2 = water vapour mixing ration, P = pressure.
# according to the Canadian FWI corrections, fro relative humidity the mixing ration (ratio of water vapour to dry air by weight)
# is assumed to be constant with elevation. The mixing ratio is calulated for each station and interpolated (hoevere we are using WRF data)
# The relative humidity is then calculated on a cell by cell basis using adjusted temperature grid. 

# need to adjust Temperature and Pressure (optional but encouraged)
# temperature is already adjusted 

#float T2(Time, south_north, west_east) ;
#T2:description = "TEMP at 2 M" ;
#T2:units = "K" ;

#float Q2(Time, south_north, west_east) ;
#Q2:description = "QV at 2 M" ;
#Q2:units = "kg kg-1" ;

#float PSFC(Time, south_north, west_east) ;Surface temperature 
#PSFC:description = "SFC PRESSURE" ;
#PSFC:units = "Pa" ;


# Elevation grid = DEM 
dem <- rast(file.path(spatialOutDir, "DEM3005_BuMo.tif"))

# Optional: use mean terrain as reference
#z_ref <- global(dem, "mean", na.rm = TRUE)[1] |> 
#  pull()

# read in stacked nc file
tqp <- terra::rast(file.path(spatialDir,'weather_wrf','TQP_1300_BuMo.nc'))
filesdate <- list.files(file.path(out_dir, "temp2mcorrected"))

# get list of files of adjusted temperatures 
loopfiles <- purrr::map(filesdate, function(ff){
  
 # ff <- filesdate[80]
  print(ff)
  #t <- rast(file.path(out_dir, "temp2mcorrected", ff))
  name <- gsub("14_temp.tif", "", ff)
  # convert temp back to  Kelvin to celcius 
  #t_ff <-  (t /10)
  #t_ff <- t + 273.15
  
  # get q2 
  q2_name <- paste0("Q2_Times=", name,".875")
  q2_ff <- tqp[[q2_name]]
  q2_ff <- project(q2_ff, raster_template)
  
  # read in raw temp also 
  t2_name <- paste0("T2_Times=", name,".875")
  t2_ff <- tqp[[t2_name]]
  t2raw_ff <- project(t2_ff, raster_template)
  
  # read in pressure
  p_ff <- paste0("PSFC_Times=", name,".875")
  p_ff  <- tqp[[p_ff]]
  p_ff <- project(p_ff, raster_template)
  
  # correct pressure for altitude 
  # g     <- 9.80665   # m s-2
  # Rd    <- 287.05    # J kg-1 K-1
   
   # correct temp for altitide
   t_corr <- t2raw_ff + (-0.0065 *(dem-0))
   
   #correct pressure for alititde
   p_corr <- p_ff * exp(-9.80665 * (dem / (87.05 * t2raw_ff)))
 
   # calculate the relative humidity 
    # Constants (Bolton / WRF)
    p <- p_corr #p_ff
    q <- q2_ff
    t <- t_corr #t2raw_ff # t2_ff 

    # vapour presure 
    e  <- (p * q)/ (0.622 + (1 - 0.622) * q)
    # saturation vapour pressure 
    es <-  611.2 * exp((17.67 * (t - 273.15)) / (t - 29.65))
    # relative humidity (%) 
    rh <- (100 * (e / es))
    rh <- clamp(rh, lower = 0, upper = 100, values = TRUE)
    rh <- as.int(rh)
    #plot(rh)
  
    writeRaster(rh, file.path(out_dir, "rh_12noon", paste0(name, '12_rh.tif')), overwrite=TRUE)
    
    # # calculate the relative humidity - USING UNCORRECTED VALUS 
    # # Constants (Bolton / WRF)
    # p <- p_ff #p_ff
    # q <- q2_ff
    # t <- t2raw_ff #t2raw_ff # t2_ff 
    # 
    # # vapour presure 
    # e  <- (p * q)/ (0.622 + (1 - 0.622) * q)
    # # saturation vapour pressure 
    # es <-  611.2 * exp((17.67 * (t - 273.15)) / (t - 29.65))
    # # relative humidity (%) 
    # rhr <- (100 * (e / es))
    # rhr <- clamp(rhr, lower = 0, upper = 100, values = TRUE)
    # plot(rhr)
    # 
    # dif <- rh - rhr
    # writeRaster(dif , file.path(out_dir, "rh_12noon", paste0(name, '12_rh.tif')), overwrite=TRUE)
    
    }) 



 
# #############################################################################################################3
# 
# # calculate the FWI using the CSSF tutorial
# 
# #remotes::install_github("cffdrs/cffdrs_r")
# library(cffdrs)
# library(lubridate)
# library(data.table)
# library(lutz)
# library(readr)
# library(sf)
# 
# # load in helper functions... not sure why these are not part of the package? Note these have
# # been manually moved and copied on August 18th as they were not accessible within the package directly 
# 
# source("17_csffdr_utils.R")
# 
# st_bumo <- st_read( fs::path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg")) |> 
#   select(STATION_NAME, LATITUDE, LONGITUDE) |> 
#   st_drop_geometry()
# 
# # id, lat, long, yr, mon, day, hr, temp, rh, ws, prec
# 
# # lets test this using the bc data stations # test on one station
# 
# wtest <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))
# 
# # join the lat and long cols
# wtest <- left_join(wtest, st_bumo)
# 
# wtest <- wtest |> 
#   #filter(STATION_NAME == "HOUSTON") |> 
#   rename(
#     "id" = STATION_CODE,
#     "lat" = LATITUDE,
#     "long" = LONGITUDE,
#     "temp" = HOURLY_TEMPERATURE ,
#     "rh" = HOURLY_RELATIVE_HUMIDITY,
#     "ws" = HOURLY_WIND_SPEED ,
#     "prec" = HOURLY_PRECIPITATION 
#   ) 
# 
# wtest <- wtest |> 
#   mutate(yr = as.numeric(substr(DATE_TIME, 1, 4)),
#          mon = as.numeric(substr(DATE_TIME, 5,6)),
#          day = as.numeric(substr(DATE_TIME, 7,8)),
#          hr = as.numeric(substr(DATE_TIME, 9,10))) 
# 
# wtest <- wtest |> select(id, lat, long, yr, mon, day, hr, temp, rh, ws, prec,DATE_TIME)
# wtest <- wtest |> 
#   filter(mon %in% c(4,5,6,7,8,9,10))
# 
# data <- wtest
# #wtest = NA
# 
# # checks for weather data ################
# # 1) RH 
# # replace rh = NA with rh = 0
# data <- data %>%
#   mutate(rh = ifelse(is.na(rh), 0, rh))
# 
# # 2) WS
# data <- data %>%
#   mutate(ws = ifelse(is.na(ws), 0, ws))
# 
# # 3) prec
# data <- data %>%
#   mutate(prec = ifelse(is.na(prec), 0, prec))
# 
# 
# # Print the column names, data should contain the following 11 columns:
# # id, lat, long, yr, mon, day, hr, temp, rh, ws, prec
# names(data)
# 
# ### Find the timezone ###
# # The 'lutz' library has functions to get the timezone of the weather station
# # based on latitude and longitude. First, make a dataframe of stations with
# # unique ID, latitude, and longitude.
# stations <- unique(data[c("id", "lat", "long")])
# 
# # Print the unique station IDs and coordinates. For this dataset the only station
# # is at Petawawa Research Forest (PRF).
# stations
# 
# 
# # Find the timezone based on latitude and longitude, this can take some time.
# # You may need to download the package 'sf' for method = "accurate".
# tz_loc <- tz_lookup_coords(stations$lat, stations$long, method = "accurate")
# # Print the timezone location. PRF is equivalent to "America/Toronto".
# tz_loc <- unique(tz_loc)
# 
# utc <- tz_offset("2014-01-01", tz_loc)[[5]]
# utc
# # 
# # 
# # # temporarty check a subset 
# # nas <- data |> filter(!is.na(rh))
# # 
# # data <- data |> 
# #   filter(!is.na(rh)) |> 
# #   filter(!is.na(ws)) |> 
# #   filter(!is.na(prec)) 
# 
# ### Calculate hourly FWI System outputs with FWI2025 ###
# # hFWI() is the function that calculates hourly FWI codes in FWI2025. It can
# # handle multiple stations and years/fire seasons (not shown in this tutorial).
# # Make sure to specify the corresponding UTC offsets for different stations.
# # Default starting FWI codes are: ffmc_old = 85, dmc_old = 6, dc_old = 15
# 
# 
# 
# data2014 <- data |> 
#   filter(yr == 2014) |> 
#   filter(id ==105)
# 
# # check that the difference in hr from one row to the next row is only 1 hour using lag
# data2014 <- data2014 |>
#   mutate(hr_diff = hr - lag(hr)) 
# 
# 
# 
# #select(id, lat, long, yr, mon, day, hr, temp, rh, ws, prec)
# 
# data_fwi <- hFWI(data, utc)
# 
# # Output is a data TABLE, with FWI calculations appended after the input columns.
# # Save the output as a .csv file (overrides any data in any preexisting file).
# write.csv(data_fwi, "wx_prf_fwi.csv")
# 
# # View a simple summary of the standard FWI components.
# standard_components <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi")
# View(summary(data_fwi[, ..standard_components]))
# 
# ### Calculate daily summaries ###
# # Calculate outputs like peak burn time and number of hours of spread potential.
# report <- generate_daily_summaries(data_fwi)
# 
# # View a simple summary of the daily report (convert values to numeric class first).
# daily_components <- c("peak_time", "duration", "peak_isi_smoothed", "dsr")
# View(summary(apply(report[daily_components], 2, as.numeric)))
# 
# # From here, the outputs can be converted to any datatype for further analysis or
# # plotted for visualization.
# 