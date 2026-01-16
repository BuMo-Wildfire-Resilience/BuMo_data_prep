# comparison with weather station data and wrf model data 

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
out_dir_wrf <- fs::path(spatialOutDir, "weather_wrf_raster")

# READ IN THE weaher station data 
st <- st_read(path(spatialDir, "weather", "BuMo", "bumo_weather_stations.gpkg"))

# read in the daily data daily weather measures 2024 is seperate file

mods24 <- read_csv(fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142024.csv"))
mods <- read_csv(file = fs::path(spatialDir, "weather", "BuMo", "bumo_weather_obs_20142023.csv"))

mods <- rbind(mods, mods24)
names(mods)
#aoi_st <- unique(st$STATION_CODE)

# read in template and convert to points and coordinates
#raster_template<- rast(file.path(spatialOutDir, "template_BuMo.tif"))
#raster_template[raster_template ==1]<- 0

mods <- mods |> 
  select(STATION_CODE, STATION_NAME, DATE_TIME, HOURLY_WIND_SPEED, HOURLY_WIND_DIRECTION, HOURLY_TEMPERATURE, HOURLY_RELATIVE_HUMIDITY,PRECIPITATION) |> 
  mutate(TIME = substr(DATE_TIME, nchar(DATE_TIME)-1 ,nchar(DATE_TIME))) |> 
  filter(TIME == 12) |> 
  mutate(month = substr(DATE_TIME, nchar(DATE_TIME)-5 ,nchar(DATE_TIME)-4)) |> 
  filter(month %in% c("04","05","06","07","08","09","10")) |> 
  mutate(year = substr(DATE_TIME, 1 ,4)) |> 
  select(-TIME) |> 
  mutate(HOURLY_WIND_SPEED = as.integer(HOURLY_WIND_SPEED* 10), 
         HOURLY_WIND_DIRECTION = as.integer(HOURLY_WIND_DIRECTION))

st_test <- st |> 
  select(STATION_CODE,STATION_NAME,INSTALL_DATE)


# for each station (pull all the WRF data)

rasteroi <- list.files(out_dir_wrf, recursive = T, pattern = "*.tif$", full.names = T)

out <- purrr::map(st$WEATHER_STATIONS_ID, function(ii){
  
  #ii <- st$WEATHER_STATIONS_ID[1]
  
  st_ii <- st |>  filter(WEATHER_STATIONS_ID == ii) |> vect()
  
  valuesr <- purrr::map(rasteroi, function(raster_path){
    
    #raster_path <- rasteroi[1]
    print(raster_path)
    
    raster_ii <- terra::rast(raster_path)
    values <- terra::extract(raster_ii, st_ii) |> select(-ID) |> pull()
    
    tibble(
      raster_name = raster_path,
      values = values,
      id = ii
    )

  })
  
  return(valuesr)
  
}) |> bind_rows()



# format the data into id outputs 



# read in the raw data 



# compare the values 




# plot values. 








