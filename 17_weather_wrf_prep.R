# 17 weather_wrf_prep.r 

# note this script reads in wrf data and converts it from .nc files to usable grids. 
# 1) read in prepared files (.nc) 
# 2) format and downsample? STILL TO DO 
# 3) generate FWI metrics etc using the "cffdrs/cffdrs_r" functions - TEMPLATE FILTER AS OF AUGST 26

## requires: 
# - .nc files for WRF data 
# csffdr_utils.R to run 




#remotes::install_github("cffdrs/cffdrs_r")


#read in weather data 

library(terra)
library(dplyr)
library(fs)
library(sf)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

# read in weather data 

weather <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_ppt.nc'))

w1 <- weather[[1]]
writeRaster(w1, file.path(spatialDir,'weather_wrf','BuMo_ppt_11.tif'), overwrite=TRUE)

tmin <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_tmin.nc'))
w1 <- tmin[[1]]
writeRaster(w1, file.path(spatialDir,'weather_wrf','BuMo_tmin_1.tif'), overwrite=TRUE)


tmax <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_tmax.nc'))
w1 <- tmax[[1]]
writeRaster(w1, file.path(spatialDir,'weather_wrf','BuMo_tmax_1.tif'), overwrite=TRUE)
names(tmax)

# wind speed 
wind <- terra::rast(file.path(spatialDir,'weather_wrf','BuMo_wsmean.nc'))
w1 <- tmax[[1]]



tail(names(tmin))
tail(names(tmax))
length(names(weather))
length(names(tmin))
length(names(tmax))

"T2_Times=20230926.47916667" "T2_Times=20230927.47916667"
[3] "T2_Times=20230928.47916667" "T2_Times=20230929.47916667"
[5] "T2_Times=20230930.47916667" "T2_Times=20231001"  
names(weather
      )






#############################################################################################################3

# calculate the FWI using the CSSF tutorial

#remotes::install_github("cffdrs/cffdrs_r")
library(cffdrs)
library(lubridate)
library(data.table)
library(lutz)
library(readr)
library(sf)

# load in helper functions... not sure why these are not part of the package? Note these have
# been manually moved and copied on August 18th as they were not accessible within the package directly 

source("17_csffdr_utils.R")

st_bumo <- st_read( fs::path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg")) |> 
  select(STATION_NAME, LATITUDE, LONGITUDE) |> 
  st_drop_geometry()

# id, lat, long, yr, mon, day, hr, temp, rh, ws, prec

# lets test this using the bc data stations # test on one station

wtest <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

# join the lat and long cols
wtest <- left_join(wtest, st_bumo)

wtest <- wtest |> 
  #filter(STATION_NAME == "HOUSTON") |> 
  rename(
    "id" = STATION_CODE,
    "lat" = LATITUDE,
    "long" = LONGITUDE,
    "temp" = HOURLY_TEMPERATURE ,
    "rh" = HOURLY_RELATIVE_HUMIDITY,
    "ws" = HOURLY_WIND_SPEED ,
    "prec" = HOURLY_PRECIPITATION 
  ) 

wtest <- wtest |> 
  mutate(yr = as.numeric(substr(DATE_TIME, 1, 4)),
         mon = as.numeric(substr(DATE_TIME, 5,6)),
         day = as.numeric(substr(DATE_TIME, 7,8)),
         hr = as.numeric(substr(DATE_TIME, 9,10))) 

wtest <- wtest |> select(id, lat, long, yr, mon, day, hr, temp, rh, ws, prec,DATE_TIME)
wtest <- wtest |> 
  filter(mon %in% c(4,5,6,7,8,9,10))

data <- wtest
#wtest = NA

# checks for weather data ################
# 1) RH 
# replace rh = NA with rh = 0
data <- data %>%
  mutate(rh = ifelse(is.na(rh), 0, rh))

# 2) WS
data <- data %>%
  mutate(ws = ifelse(is.na(ws), 0, ws))

# 3) prec
data <- data %>%
  mutate(prec = ifelse(is.na(prec), 0, prec))


# Print the column names, data should contain the following 11 columns:
# id, lat, long, yr, mon, day, hr, temp, rh, ws, prec
names(data)

### Find the timezone ###
# The 'lutz' library has functions to get the timezone of the weather station
# based on latitude and longitude. First, make a dataframe of stations with
# unique ID, latitude, and longitude.
stations <- unique(data[c("id", "lat", "long")])

# Print the unique station IDs and coordinates. For this dataset the only station
# is at Petawawa Research Forest (PRF).
stations


# Find the timezone based on latitude and longitude, this can take some time.
# You may need to download the package 'sf' for method = "accurate".
tz_loc <- tz_lookup_coords(stations$lat, stations$long, method = "accurate")
# Print the timezone location. PRF is equivalent to "America/Toronto".
tz_loc <- unique(tz_loc)

utc <- tz_offset("2014-01-01", tz_loc)[[5]]
utc
# 
# 
# # temporarty check a subset 
# nas <- data |> filter(!is.na(rh))
# 
# data <- data |> 
#   filter(!is.na(rh)) |> 
#   filter(!is.na(ws)) |> 
#   filter(!is.na(prec)) 

### Calculate hourly FWI System outputs with FWI2025 ###
# hFWI() is the function that calculates hourly FWI codes in FWI2025. It can
# handle multiple stations and years/fire seasons (not shown in this tutorial).
# Make sure to specify the corresponding UTC offsets for different stations.
# Default starting FWI codes are: ffmc_old = 85, dmc_old = 6, dc_old = 15



data2014 <- data |> 
  filter(yr == 2014) |> 
  filter(id ==105)

# check that the difference in hr from one row to the next row is only 1 hour using lag
data2014 <- data2014 |>
  mutate(hr_diff = hr - lag(hr)) 



#select(id, lat, long, yr, mon, day, hr, temp, rh, ws, prec)

data_fwi <- hFWI(data, utc)

# Output is a data TABLE, with FWI calculations appended after the input columns.
# Save the output as a .csv file (overrides any data in any preexisting file).
write.csv(data_fwi, "wx_prf_fwi.csv")

# View a simple summary of the standard FWI components.
standard_components <- c("ffmc", "dmc", "dc", "isi", "bui", "fwi")
View(summary(data_fwi[, ..standard_components]))

### Calculate daily summaries ###
# Calculate outputs like peak burn time and number of hours of spread potential.
report <- generate_daily_summaries(data_fwi)

# View a simple summary of the daily report (convert values to numeric class first).
daily_components <- c("peak_time", "duration", "peak_isi_smoothed", "dsr")
View(summary(apply(report[daily_components], 2, as.numeric)))

# From here, the outputs can be converted to any datatype for further analysis or
# plotted for visualization.





