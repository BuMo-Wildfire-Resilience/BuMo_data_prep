# 17_weather_BCwildfire_stations


library(terra)
library(sf)
library(dplyr)
library(bcdata)
library(purrr)
library(fs)
library(readr)


DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')

# 1) prepare weather stations in and around the AOI 


# prepare files - only need to do this once
st <- bcdc_get_data("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_WEATHER_STATIONS_SP")
st_write(st, path(spatialDir, "weather", "weather_stations.gpkg"), delete_dsn = TRUE)

aoi <- st_read(file.path(spatialOutDir,"AOI.gpkg")) 

# select all the weather stations close by (within 10 KM of the actual BUMO admin)

st_bumo <- st |> st_intersection(aoi) |> 
  select(-c( "AOI","PERIMETER", "FCODE" , "WTSHD_CODE" , "WTSHD_SYS"          
             , "OBJECTID.1",  "AREA" ))

st_addition <- st |> 
  filter(STATION_NAME %in% c("BLACKPINE","MANSON", "BELL-IRVING"))

st_bumo <- rbind(st_bumo, st_addition)
st_write(st_bumo, path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"), delete_dsn = TRUE)

st_bumo_codes <- st_bumo$STATION_CODE


# now we have the list of potential weather stations we can download the daily weather from the datamart
# note hourly weather is available under a data sharing agreement, just starting with daily average to get 
# code started for ground truthing the data avavilabel and compare with WRF. 


# combine all the stations of interest and subset the times from April 1st to Oct 31st. 

wfl <- list.files(fs::path(spatialDir, "weather", "BuMo"), recursive = TRUE, pattern = "*BCWS_WX_OBS.csv")

mods <- purrr::map(wfl, function(x) {
  #x <- wfl[1]
  yr <- gsub("daily_measures/", "", x)
  yr <- gsub("_BCWS_WX_OBS.csv", "", yr)
  
  tfile <- read.csv(fs::path(spatialDir, "weather", "BuMo", x))
  tfilesub <- tfile |> filter(STATION_CODE %in% st_bumo_codes) |> 
    filter(DATE_TIME >= as.numeric(paste0(yr, 040100))) |> 
    filter(DATE_TIME <= as.numeric(paste0(yr, 103100))) 
  
  tfilesub
  # mapview::mapview(tsf_sub)
}) |> bind_rows()

write.csv(mods, file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"), row.names = FALSE)


mods <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))


# potential to extrapolate between stations? 


# convert to a raster 
# read in stations
st <- st_read(path(spatialDir, "weather", "weather_stations.gpkg"))

# read in temp raster
dem <- rast(fs::path(spatialOutDir, "DEM3005_BuMo.tif"))
# convert any values >0 to 1
dem[dem > 0] <- 1
dem[dem <0 ] <- 0

temp <- dem
# this can be our template raster 

raster_points <- as.points(temp, values = FALSE)



# calculate the FWI using the CSSF tutorial

#remotes::install_github("cffdrs/cffdrs_r")
library(cffdrs)
library(lubridate)
library(data.table)
library(lutz)
library(readr)

# load in helper functions... not sure why these are not part of the package? Note these have
# been manually moved and copied on August 18th as they were not accessible within the package directly 

source("17_csffdr_utils.r")

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



