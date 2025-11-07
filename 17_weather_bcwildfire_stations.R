## 17_weather_BCwildfire_stations

# this script will format bc data weather station information to generate raster layers for 
# weather and fire metrics across the Bumo Grid. 

# 1) Download the station meta information from bcdata package 
# 2) Format and combine weather station data. Relies on manual download of the data priori to this. 
# 3) For the raster template - determine which raster cell is closest to which station. note this 
      # has two outputs as station installed in Nov 2018
# 4) Generate weather data rasters for each time point ie 20140401 and for each metric (i.e FWI) 
 # by extracting the closest station inforamtion. Note there is a function to do do these. 


library(terra)
library(sf)
library(dplyr)
library(bcdata)
library(purrr)
library(fs)
library(readr)
library(lubridate)
library(gstat)

DataDir <- 'data'
spatialDir <- fs::path(DataDir,'spatial')

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
spatialOutDir <- file.path(OutDir,'spatial')
out_dir <- fs::path(spatialOutDir, "weather_stations_raster")


# 1) prepare weather stations in and around the AOI 

# 1 Download the station meta information from bcdata package 

# prepare files - only need to do this once
st <- bcdc_get_data("WHSE_LAND_AND_NATURAL_RESOURCE.PROT_WEATHER_STATIONS_SP")
st_write(st, path(spatialDir, "weather", "weather_stations.gpkg"), delete_dsn = TRUE)

aoi <- st_read(file.path(spatialOutDir,"template_bbox_poly.gpkg")) 

# select all the weather stations close by (within 10 KM of the actual BUMO admin)
st_bumo <- st |> st_intersection(aoi) 
st_write(st_bumo, path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"), delete_dsn = TRUE)


# now we have the list of potential weather stations we can download the daily weather from the datamart







# 2) Compile the weather data downloaded from the BCdata site 
# These were downloaded for annual years from 2014 - 2023 as annual zip files. 

# note hourly weather for fwi and fire metrics is available under a data sharing agreement, just starting with daily average to get 
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

# daily weather measures
mods <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

#Generate the fire weather information and calculate daily max, min, mean, sd for relevant metrics, 
# note included some wind variables also. 

#mods <- read_csv(file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- mods |> 
  mutate(DATE = substr(DATE_TIME, 1, 8)) |>
  mutate(ddate = as_date(DATE)) |> 
  mutate(jday = yday(ddate))

## summarise which stations have data 
# st_summary <- mods |> 
#   group_by(STATION_CODE) |> 
#   summarise(min(DATE_TIME),
#             max(DATE_TIME))
# potential to extrapolate between stations? 

# select FWI variables as these are generated daily at 12noon? 
st_fire_dailies <- mods |> 
  select(STATION_CODE, DATE_TIME, DATE, ddate, 
         PRECIPITATION, FINE_FUEL_MOISTURE_CODE, INITIAL_SPREAD_INDEX, 
         FIRE_WEATHER_INDEX, DUFF_MOISTURE_CODE, DROUGHT_CODE, BUILDUP_INDEX, 
         DANGER_RATING
         ) |>  
  mutate(hour = substr(DATE_TIME, 9, 10)) |> 
  filter(hour == 12) |> 
#  filter(!is.na(FIRE_WEATHER_INDEX)) |> 
  arrange(DATE)


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

# fire_st <- unique(st_fire_dailies$STATION_CODE)
# fire_dates <- unique(st_fire_dailies$DATE)

# generate daily averages for other temperature metrics
st_dailies <- mods |> 
  select(-c(PRECIPITATION, FINE_FUEL_MOISTURE_CODE, INITIAL_SPREAD_INDEX, 
          FIRE_WEATHER_INDEX, DUFF_MOISTURE_CODE, DROUGHT_CODE, BUILDUP_INDEX, 
          DANGER_RATING,"RN_1_PLUVIO1", "SNOW_DEPTH", "SNOW_DEPTH_QUALITY",
          "PRECIP_PLUVIO1_STATUS", "PRECIP_PLUVIO1_TOTAL" ,"RN_1_PLUVIO2",                  
          "PRECIP_PLUVIO2_STATUS","PRECIP_PLUVIO2_TOTAL", "RN_1_RIT",  "PRECIP_RIT_STATUS",             
          "PRECIP_RIT_TOTAL" ,"PRECIP_RGT","SOLAR_RADIATION_LICOR","SOLAR_RADIATION_CM3" )) 

st_dailies <- st_dailies|> 
  group_by(STATION_CODE, DATE) |> 
    summarise(
    dmax_precip = max(HOURLY_PRECIPITATION, na.rm = T),
    dmin_precip = min(HOURLY_PRECIPITATION, na.rm = T),
    dmean_precip = mean(HOURLY_PRECIPITATION, na.rm = T),
    dsd_precip = sd(HOURLY_PRECIPITATION, na.rm = T),
    dmax_temp = max(HOURLY_TEMPERATURE, na.rm = T),
    dmin_temp = min(HOURLY_TEMPERATURE, na.rm = T),
    dmean_temp = mean(HOURLY_TEMPERATURE, na.rm = T),
    dsd_temp = sd(HOURLY_TEMPERATURE, na.rm = T),
    dmax_rhm = max(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dmin_rhm = min(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dmean_rhm = mean(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dsd_rhm = sd(HOURLY_RELATIVE_HUMIDITY, na.rm = T),
    dmax_ws = max(HOURLY_WIND_SPEED, na.rm = T),
    dmin_ws = min(HOURLY_WIND_SPEED, na.rm = T),
    dmean_ws = mean(HOURLY_WIND_SPEED, na.rm = T),
    dsd_ws = sd(HOURLY_WIND_SPEED, na.rm = T),
    dmean_wdir = mean(HOURLY_WIND_DIRECTION, na.rm = T),
    dmax_wgust = max(HOURLY_WIND_GUST, na.rm = T),
    dmin_wgust = min(HOURLY_WIND_GUST, na.rm = T),
    dmean_wgust = mean(HOURLY_WIND_GUST, na.rm = T),
    dsd_wgust = sd(HOURLY_WIND_GUST, na.rm = T)
  ) 
      
# join the fire data and the daily summaries together

all_stat_weather <- left_join(st_fire_dailies, st_dailies) |> 
  select(-DATE_TIME)

write_csv(all_stat_weather , fs::path(spatialDir, "weather", "BuMo", "bumo_weather_daily_20142023.csv"))









# 3) interpolate the wind speed based on closest station (idw)
# read in the station locations

st <- st_read(path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"))
aoi_st <- unique(st$STATION_CODE)

# read in template and convert to points and coordinates
raster_template<- rast(file.path(spatialOutDir, "template_BuMo.tif"))
raster_template[raster_template ==1]<- 0

# daily weather measures at 12 noon timestamp 
mods <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- mods |> 
  select(STATION_CODE, STATION_NAME, DATE_TIME, HOURLY_WIND_SPEED, HOURLY_WIND_DIRECTION) |> 
  mutate(TIME = substr(DATE_TIME, nchar(DATE_TIME)-1 ,nchar(DATE_TIME))) |> 
  filter(TIME == 12) |> 
  mutate(month = substr(DATE_TIME, nchar(DATE_TIME)-5 ,nchar(DATE_TIME)-4)) |> 
  filter(month %in% c("04","05","06","07","08","09","10")) |> 
  mutate(year = substr(DATE_TIME, 1 ,4)) |> 
  select(-TIME)

st_test <- st |> 
  select(STATION_CODE,STATION_NAME,INSTALL_DATE)

# loop through each day to generate a windspeed raster based on interpolation
# shortlist the dates for each April 1st to 31st Oct

# type of metric to be analysed

# # create loop for the metrics 
# coi = "HOURLY_WIND_SPEED"
# Wind direction 
# ISI 
# BUI
# FWI

# get unique yr and months 
mod_years <- unique(mods$year)
mod_months <- unique(mods$month)

# loop through years # complete 2014[1]
yoi <- mod_years[2]

all_years <- purrr::map(mod_years, function(yy) {
  
  #yoi <- mod_years[yy]

    mods_df <- mods |> filter (year == yy) 
    
    # loop through months of year 
    mod_months <- mod_months
    
    all_months <- purrr::map(mod_months, function(m) {
      # test line
      #m <- mod_months[1]
      mods_dfm <- mods_df |> filter(month == m)
    
      # get list of all dates in the select month and year
      alldates <- unique(mods_dfm$DATE_TIME)
      #alldates <- alldates[1:2]
    
      # create a list of rasters to stack outputs
      all_out <- purrr::map(alldates, function(x) {
         #x <- alldates[1]
        print(x)
        mod_test <- mods_dfm |> filter(DATE_TIME == x)
    
        st_data <- left_join(st_test, mod_test, by = join_by(STATION_CODE, STATION_NAME))
        st_data <- st_data |>
          select(STATION_CODE, DATE_TIME, HOURLY_WIND_SPEED) |>
          filter(!is.na(HOURLY_WIND_SPEED))
    
        d <- data.frame(geom(vect(st_data))[, c("x", "y")], as.data.frame(st_data)) |>
          select(-geom)
    
        gs <- gstat(formula = HOURLY_WIND_SPEED ~ 1, locations = ~ x + y, data = d, set = list(idp = 2))
        idw <- interpolate(raster_template, gs, debug.level = 0)
        idw <- mask(idw, raster_template)
        names(idw) <- c(x, "drop")
        idw <- idw[[1]]
        fname <- paste0(x ,"_windsp.tif")
        writeRaster(idw, fs::path(spatialOutDir, "weather_stations_raster", fname), overwrite=TRUE)
        idw <- as.data.frame(idw, xy = TRUE)
        idw
        })
    
      # condense and save 
      aa <- all_out |> reduce(left_join, by = c("x", "y"))
      rm(all_out)
      fname <- paste0(yoi, m, "_windsp_df.rds")
      saveRDS(aa,  fs::path(out_dir, fname))
      # #write.csv(aa, fs::path(out_dir, fname))
      print(m)
      rm(aa)
    }) # end of month loop 
    
}) # end of yr loop 


#################################
## Wind direction 
###############################
# read in the station locations

st <- st_read(path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"))
aoi_st <- unique(st$STATION_CODE)

# read in template and convert to points and coordinates
raster_template<- rast(file.path(spatialOutDir, "template_BuMo.tif"))
raster_template[raster_template ==1]<- 0

# daily weather measures at 12 noon timestamp 
mods <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- mods |> 
  select(STATION_CODE, STATION_NAME, DATE_TIME, HOURLY_WIND_SPEED, HOURLY_WIND_DIRECTION) |> 
  mutate(TIME = substr(DATE_TIME, nchar(DATE_TIME)-1 ,nchar(DATE_TIME))) |> 
  filter(TIME == 12) |> 
  mutate(month = substr(DATE_TIME, nchar(DATE_TIME)-5 ,nchar(DATE_TIME)-4)) |> 
  filter(month %in% c("04","05","06","07","08","09","10")) |> 
  mutate(year = substr(DATE_TIME, 1 ,4)) |> 
  select(-TIME)

st_test <- st |> 
  select(STATION_CODE,STATION_NAME,INSTALL_DATE)


# get unique yr and months 
mod_years <- unique(mods$year)
mod_months <- unique(mods$month)

# loop through years # complete 2014[1]
#yoi <- mod_years[2]

#
all_years <- purrr::map(mod_years, function(yy) {
  
  #yy <- mod_years[1] # test line
  mods_df <- mods |> filter (year == yy) 
  
  # loop through months of year 
  #mod_months <- mod_months
  
  all_months <- purrr::map(mod_months, function(m) {
    # test line
    #m <- mod_months[1]
    mods_dfm <- mods_df |> filter(month == m)
    
    # get list of all dates in the select month and year
    alldates <- unique(mods_dfm$DATE_TIME)
    #alldates <- alldates[1:2]
    
    # create a list of rasters to stack outputs
    all_out <- purrr::map(alldates, function(x) {
      #x <- alldates[1]
      print(x)
      mod_test <- mods_dfm |> filter(DATE_TIME == x)
      
      st_data <- left_join(st_test, mod_test, by = join_by(STATION_CODE, STATION_NAME))
      st_data <- st_data |>
        select(STATION_CODE, DATE_TIME, HOURLY_WIND_DIRECTION) |>
        filter(!is.na(HOURLY_WIND_DIRECTION))
      
      d <- data.frame(geom(vect(st_data))[, c("x", "y")], as.data.frame(st_data)) |>
        select(-geom)
      
      gs <- gstat(formula = HOURLY_WIND_DIRECTION ~ 1, locations = ~ x + y, data = d, set = list(idp = 2))
      idw <- interpolate(raster_template, gs, debug.level = 0)
      idw <- mask(idw, raster_template)
      names(idw) <- c(x, "drop")
      idw <- idw[[1]]
      fname <- paste0(x ,"_winddir.tif")
      writeRaster(idw, fs::path(spatialOutDir, "weather_stations_raster", fname), overwrite=TRUE)
      idw <- as.data.frame(idw, xy = TRUE)
      idw
    })
    
    # condense and save 
    aa <- all_out |> reduce(left_join, by = c("x", "y"))
    rm(all_out)
    fname <- paste0(yoi, m, "_winddir_df.rds")
    saveRDS(aa,  fs::path(out_dir, fname))
    # #write.csv(aa, fs::path(out_dir, fname))
    print(m)
    rm(aa)
    
  }) # end of month loop 
  
  

}) #End of year loop 









  
# generate weather rasters 

# this example pulls the data from the closest weather station and generates raster based on daily weather of closest staion. 
# this can be updated to provide multiple weather variables over multiple dates. 
# note there is two station raster (pre2019 and post 2019)
# date is provided for the given date, even when the value is NA. Fire weather is based on daily 12 noon records. 

fwi_2014 <- generate_weather_raster(
                        weather_data = all_stat_weather,
                        station_raster = pre2019,
                        output_dir = fs::path(spatialOutDir, "weather_stations_raster"),
                        date_format = "%Y%m%d",
                        station_id_column = "STATION_CODE",
                        date_column = "DATE",
                        start_date = 20140401,
                        end_date =  20140430,
                        date_step = "day",
                        chunk_size = 100,
                        save_individual = FALSE,
                        return_stack = TRUE,
                        weather_variables = "FIRE_WEATHER_INDEX")

#fwi_2014 <- fwi_2014_2018
# convert to raster
fwi_2014 <- rast(fwi_2014)

output_dir = fs::path(spatialOutDir, "weather_stations_raster")
terra::writeRaster(fwi_2014, fs::path(output_dir, "fwi_201404.tif"))


